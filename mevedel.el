;;; mevedel.el --- Instructed LLM programmer/assistant -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; Author: FrauH0lle
;; Version: 0.5.0
;; Keywords: convenience, tools, llm, gptel, gptel-agent
;; Package-Requires: ((emacs "30.1") (gptel-agent "0.0.1"))
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

(require 'gptel)
(require 'gptel-agent)

(require 'mevedel-workspace)
(require 'mevedel-instructions)
(require 'mevedel-restorer)
(require 'mevedel-tools)
(provide 'mevedel-presets)
(require 'mevedel-system)
(require 'mevedel-agents)
(require 'mevedel-compact)

;; `gptel'
(declare-function gptel-mode "ext:gptel" (&optional arg))
(defvar gptel-display-buffer-action)
(defvar gptel-use-header-line)

;; `gptel-request'
(declare-function gptel-request "ext:gptel-request")
(declare-function gptel-fsm-info "ext:gptel-request")
(defvar gptel-prompt-transform-functions)

;; `mevedel-presets'
(declare-function mevedel--define-presets "mevedel-presets")
(defvar mevedel-action-preset-alist)

;; `org-src'
(declare-function org-escape-code-in-string "ext:org-src" (s))


(defgroup mevedel nil
  "Customization group for Evedel."
  :group 'tools)

(defcustom mevedel-show-patch-buffer nil
  "Control if the mevedel patch buffer should be shown automatically.

If non-nil, the patch buffer will automatically be displayed after a
query completes."
  :type 'boolean
  :group 'mevedel)

(defvar mevedel--diff-preview-buffer-name "*mevedel-diff-preview*"
  "Name of the `diff' preview buffer.")

(defcustom mevedel-show-chat-buffer t
  "Control if the mevedel chat buffer should be shown automatically.

If non-nil, the chat buffer will automatically be displayed."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-plans-directory
  (expand-file-name (file-name-concat "mevedel" "plans") temporary-file-directory)
  "Directory where implementation plans are stored.

Defaults to a temporary directory. Set this to a permanent location if
you want to keep plans persistently."
  :type 'directory
  :group 'mevedel)

(defcustom mevedel-ov-dispatch-key "M-m"
  "Keybind to open overlay actions.
If nil, no keybinding is set for dispatch actions."
  :group 'mevedel
  :type '(choice (const :tag "No keybinding" nil)
          (string :tag "Key sequence"))
  :set (lambda (sym new-val)
         (let ((old-val (and (boundp sym) (symbol-value sym))))
           ;; Remove old binding if there was one and keymap exists
           (dolist (map mevedel--actions-maps)
             (when (and old-val (boundp map))
               (keymap-set (symbol-value map) old-val nil)))

           ;; Set the new value
           (set sym new-val)
           ;; Add new binding if new value is non-nil and keymap exists
           (dolist (map mevedel--actions-maps)
             (when (and new-val (boundp map))
               (keymap-set (symbol-value map) new-val #'mevedel--ov-actions-dispatch))))))

(defcustom mevedel-default-chat-preset 'implement
  "Default preset for the chat buffer from `mevedel' command.

Can be one of the symbols:
- \\='implement
- \\='discuss"
  :group 'mevedel
  :type '(choice
          (const :tag "Implement" implement)
          (const :tag "Discuss" discuss)))



;;
;;; Buffer management

(defun mevedel--chat-buffer (&optional create workspace)
  "Get or create the mevedel chat buffer for WORKSPACE.

This buffer is where LLM interactions occur. If CREATE is non-nil,
create the buffer if it doesn't exist. WORKSPACE should be a cons cell
\(TYPE . ID), or nil to use the current buffer's workspace."
  (let* ((workspace (or workspace (mevedel-workspace)))
         (buf (mevedel--get-buffer nil workspace create))
         (created-p (cdr buf))
         (buf (car buf)))
    (when created-p
      (mevedel--chat-buffer-setup buf workspace))
    buf))

(defun mevedel--tutor-buffer (&optional create workspace)
  "Get or create the mevedel tutor buffer for WORKSPACE.

This buffer is where LLM interactions occur. If CREATE is non-nil,
create the buffer if it doesn't exist. WORKSPACE should be a cons cell
\(TYPE . ID), or nil to use the current buffer's workspace."
  (let* ((workspace (or workspace (mevedel-workspace)))
         (buf (mevedel--get-buffer "tutor" workspace create))
         (created-p (cdr buf))
         (buf (car buf)))
    (when created-p
      (mevedel--chat-buffer-setup buf workspace))
    buf))

(defun mevedel--chat-buffer-setup (buf workspace)
  "Setup chat buffer BUF in WORKSPACE."
  (with-current-buffer buf
    ;; Use the global gptel default mode (e.g., markdown-mode)
    (funcall (or gptel-default-mode #'text-mode))
    ;; Enable `gptel-mode'
    (gptel-mode +1)
    ;; Right-align token count segment in gptel's header-line
    ;; HACK 2026-02-13: It is brittle and I do not like this approach but could
    ;;   not come up with something more robust. Let's hope `gptel' keeps it
    ;;   that way for some time.
    (when (and gptel-mode gptel-use-header-line header-line-format)
      (setq-local gptel--header-line-info
                  '(:eval
                    (let* ((base (eval (cadr (default-value 'gptel--header-line-info))))
                           (token (mevedel--token-header-segment)))
                      (if (string-empty-p token)
                          base
                        (setq base (copy-sequence base))
                        (let* ((disp (get-text-property 0 'display base))
                               (align-to (plist-get (cdr disp) :align-to))
                               (offset (nth 2 align-to)))
                          (put-text-property
                           0 1 'display
                           `(space :align-to (- right ,(+ offset 1 (length token))))
                           base)
                          (concat base token)))))))
    ;; Wrap lines
    (visual-line-mode +1)
    ;; Auto-scroll when at end of buffer
    (setq-local window-point-insertion-type t)
    ;; Set `default-directory' to workspace root
    (setq-local default-directory (mevedel-workspace--root workspace))
    ;; Make workspace-additional-roots buffer-local for session-specific access grants
    ;; Start with a copy of the global value so pre-configured roots are available
    (setq-local mevedel-workspace-additional-roots
                (copy-alist mevedel-workspace-additional-roots))
    (add-hook 'gptel-post-response-functions #'mevedel--clear-pending-access-requests nil t)))

(defun mevedel--patch-buffer (&optional create workspace)
  "Get or create the mevedel patch staging buffer for WORKSPACE.

This buffer shows diffs generated by the LLM that are awaiting review
and application. If CREATE is non-nil, create the buffer if it doesn't
exist. WORKSPACE should be a cons cell (TYPE . ID), or nil to use the
current buffer's workspace."
  (let* ((buf (mevedel--get-buffer "patch" workspace create))
         (created-p (cdr buf))
         (buf (car buf)))
    (when created-p
      (with-current-buffer buf
        (diff-mode)
        (setq buffer-read-only t)))
    buf))

(defun mevedel--get-buffer (&optional buffer-suffix workspace create-p)
  "Get or create a buffer for WORKSPACE with optional BUFFER-SUFFIX.

Returns (BUFFER . CREATED-P) where CREATED-P indicates if buffer was
created. When CREATE-P is non-nil and buffer doesn't exist, create it
with workspace."
  (let* ((workspace (or workspace (mevedel-workspace)))
         (workspace-type (car workspace))
         (workspace-name (mevedel-workspace--name workspace))
         (buf-name (format "*mevedel%s:%s@%s*"
                           (if buffer-suffix (format "-%s" buffer-suffix) "")
                           workspace-type
                           workspace-name))
         (target-buf (and buf-name (get-buffer buf-name)))
         created-p)
    (when (and (not target-buf) create-p buf-name)
      (setq target-buf (get-buffer-create buf-name)
            created-p t)
      (with-current-buffer target-buf
        ;; Set workspace for this buffer
        (setq-local mevedel--workspace workspace)))
    (when target-buf
      (cons target-buf created-p))))

(defun mevedel--generate-final-patch (&optional workspace)
  "Generate final diffs for all tracked files in current request.

Returns a unified diff string showing original → final state for each
file. Uses the `mevedel--request-file-snapshots' to compare original
states with current file contents in WORKSPACE."
  (let ((diffs "")
        (workspace-root (mevedel-workspace--root (or workspace (mevedel-workspace)))))
    (dolist (snapshot mevedel--request-file-snapshots)
      (let* ((filepath (car snapshot))
             (original (cdr snapshot))
             (current (when (file-exists-p filepath)
                        (with-temp-buffer
                          (insert-file-contents filepath)
                          (buffer-string))))
             (relpath (file-relative-name filepath workspace-root)))

        ;; Generate diff if file changed, was deleted, or was created
        (when (or
               ;; Modified
               (and original current (not (string= original current)))
               ;; Deleted
               (and original (not current))
               ;; Created
               (and (not original) current))
          (setq diffs (concat diffs
                              (format "diff --git a/%s b/%s\n" relpath relpath)
                              (cond
                               ((and (or (not original) (string-empty-p original))
                                     (and current (not (string-empty-p current))))
                                "new file mode 100644\n")
                               ((and (and original (not (string-empty-p original)))
                                     (or (not current) (string-empty-p current)))
                                "deleted file mode 100644\n"))
                              (mevedel-tools--generate-diff
                               (or original "")
                               (or current "")
                               relpath)
                              "\n")))))
    diffs))

(defun mevedel--replace-patch-buffer (patch-content)
  "Replace patch buffer contents with PATCH-CONTENT.
If PATCH-CONTENT is empty, does nothing."
  (when (and patch-content (> (length patch-content) 0))
    (with-current-buffer (mevedel--patch-buffer t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert patch-content)
        (diff-mode)
        (goto-char (point-min))))
    (mevedel--indicate-patch-ready)))

(defun mevedel--indicate-patch-ready ()
  "Provide visual feedback that a patch is ready for review."
  (message "Patch ready in *mevedel-patch* buffer")
  (when mevedel-show-patch-buffer
    (display-buffer (mevedel--patch-buffer))))

;;;###autoload
(defun mevedel-clear-patch-buffer ()
  "Clear the patch buffer."
  (interactive)
  (when-let ((buf (mevedel--patch-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (message "Patch buffer cleared")))


;;;###autoload
(defun mevedel-version (&optional here message)
  "Return the current version of mevedel.

Interactively, or when MESSAGE is non-nil, show it in echo area. With
prefix argument, or when HERE is non-nil, insert it at point."
  (interactive (list (or current-prefix-arg 'interactive)))
  (let ((version "v0.5.0"))
    (cond
     ((or message (called-interactively-p 'any)) (message "mevedel %s" version))
     (here (insert (format "mevedel %s" version)))
     (t version))))

(defun mevedel--implement-directive-prompt (content)
  "Generate an implementation prompt for CONTENT in the current buffer."
  (format
   "## TASK: Implement the following request.

### INSTRUCTIONS:

1. Read and understand the implementation request below
2. Read and understand all provided references
3. Use the references to complete the request
4. Use your tools as needed
5. Create working, complete code that fulfills the request

### IMPLEMENTATION REQUEST:

%s"
   content))

(defun mevedel--revise-directive-prompt (content &optional patch-buffer directive)
  "Generate a prompt for revising based on CONTENT (revision instructions).

The patch comes from either:
1. DIRECTIVE's stored patch (if provided and has one)
2. PATCH-BUFFER contents (defaulting to the mevedel patch buffer)

DIRECTIVE is the instruction overlay being revised."
  (let* ((directive-patch (when directive
                            (overlay-get directive 'mevedel-directive-patch)))
         (patch-buffer (or patch-buffer (mevedel--patch-buffer)))
         (patch-content
          (cond
           (directive-patch directive-patch)
           (patch-buffer
            (with-current-buffer patch-buffer
              (buffer-substring-no-properties (point-min) (point-max))))
           (t (user-error "No patch found for revision")))))
    (format
     "## TASK: Revise your previous implementation based on new feedback.

### INSTRUCTIONS:

1. Read the revision instructions below (if any)
2. Review your previous patch
3. Read and understand all provided references
4. Use the references to complete the request
5. Understand what needs to be changed or improved
6. Create a NEW implementation that addresses the feedback
7. Use your tools to make the changes
%s
==================================
YOUR PREVIOUS WORK (for reference)
==================================

%s"
     (if (and content (not (string-empty-p content)))
         (format "\n### REVISION INSTRUCTIONS:\n\n%s\n\n" content)
       "")
     patch-content)))

(defun mevedel--discuss-directive-prompt (content)
  "Generate a discussion prompt for CONTENT in the current buffer."
  (format
   "## TASK: Answer the following request.

### INSTRUCTIONS:

1. Read and understand the request below
2. Read and understand all provided references
3. Use the references to complete the request
4. Use your tools to access files as needed

### REQUEST:

%s"
   content))


;;
;;; Commands

;;;###autoload
(defun mevedel-implement-directive (&optional callback)
  "Propose a patch to implement directive at point.

If CALLBACK is provided, it will be called when the implementation
process completes. The callback will receive two arguments: ERROR (nil
on success, a string error description on failure, or the symbol
\\='abort if the request was aborted) and FSM (the gptel-fsm object for
the request)."
  (interactive)
  (if-let* ((directive (mevedel--topmost-instruction (mevedel--highest-priority-instruction
                                                      (mevedel--instructions-at (point) 'directive)
                                                      t)
                                                     'directive)))
      (progn
        (overlay-put directive 'mevedel-directive-action 'implement)
        (mevedel--process-directive directive (alist-get 'implement mevedel-action-preset-alist)
                                    #'mevedel--implement-directive-prompt callback))
    (user-error "No directive found at point")))

;;;###autoload
(defun mevedel-revise-directive (&optional callback)
  "Propose a revision to a patch based on the directive at point.

If CALLBACK is provided, it will be called when the implementation
process completes. The callback will receive two arguments: ERROR (nil
on success, a string error description on failure, or the symbol
\\='abort if the request was aborted) and FSM (the gptel-fsm object for
the request)."
  (interactive)
  (if-let* ((directive (mevedel--topmost-instruction (mevedel--highest-priority-instruction
                                                      (mevedel--instructions-at (point) 'directive)
                                                      t)
                                                     'directive)))
      (progn
        (overlay-put directive 'mevedel-directive-action 'revise)
        (mevedel--process-directive directive (alist-get 'revise mevedel-action-preset-alist)
                                    (lambda (content)
                                      (mevedel--revise-directive-prompt content nil directive))
                                    callback))
    (user-error "No directive found at point")))

;;;###autoload
(defun mevedel-discuss-directive (&optional callback)
  "Discuss the directive at point.

If CALLBACK is provided, it will be called when the implementation
process completes. The callback will receive two arguments: ERROR (nil
on success, a string error description on failure, or the symbol
\\='abort if the request was aborted) and FSM (the gptel-fsm object for
the request)."
  (interactive)
  (if-let* ((directive (mevedel--topmost-instruction (mevedel--highest-priority-instruction
                                                      (mevedel--instructions-at (point) 'directive)
                                                      t)
                                                     'directive)))
      (progn
        (overlay-put directive 'mevedel-directive-action 'discuss)
        (mevedel--process-directive directive (alist-get 'discuss mevedel-action-preset-alist)
                                    #'mevedel--discuss-directive-prompt callback))
    (user-error "No directive found at point")))

;;;###autoload
(defun mevedel-tutor-directive (&optional callback)
  "Guide user to solve directive through hints (no solutions).

If CALLBACK is provided, it will be called when the tutoring process
completes. The callback will receive two arguments: ERROR (nil on
success, a string error description on failure, or the symbol \\='abort
if the request was aborted) and FSM (the gptel-fsm object for the
request)."
  (interactive)
  (if-let* ((directive (mevedel--topmost-instruction (mevedel--highest-priority-instruction
                                                      (mevedel--instructions-at (point) 'directive)
                                                      t)
                                                     'directive)))
      (progn
        (overlay-put directive 'mevedel-directive-action 'tutor)
        (mevedel--process-directive directive (alist-get 'tutor mevedel-action-preset-alist)
                                    #'mevedel--discuss-directive-prompt callback))
    (user-error "No directive found at point")))

;;;###autoload
(defun mevedel-process-directives (&optional process-all)
  "Process multiple directives sequentially while auto-applying patches.

Collects directives based on context:

- If a region is selected, collect all directives in that region
- If no region is selected but point is on a directive, collect that
  directive
- If no region and no directive at point, collect all directives in
  buffer

Presents directives to user via `completing-read-multiple' for
ordering/filtering.

Without prefix argument, only selected directives are processed in the
order chosen.

With PROCESS-ALL or prefix argument (\\[universal-argument]), all
directives are processed: selected directives are processed first in the
chosen order, followed by unselected directives in their original order."
  (interactive "P")
  (let (found-directives)
    ;; Collect directives based on context
    (cond ((region-active-p)
           (when-let* ((toplevel-directives
                        (cl-remove-duplicates
                         (mapcar (lambda (instr)
                                   (mevedel--topmost-instruction instr 'directive))
                                 (mevedel--instructions-in (region-beginning)
                                                           (region-end)
                                                           'directive)))))
             (setq found-directives toplevel-directives)))
          (t
           (if-let* ((directive (mevedel--topmost-instruction (mevedel--highest-priority-instruction
                                                               (mevedel--instructions-at (point) 'directive)
                                                               t)
                                                              'directive)))
               (setq found-directives (list directive))
             (when-let* ((toplevel-directives (cl-remove-duplicates
                                               (mapcar (lambda (instr)
                                                         (mevedel--topmost-instruction instr 'directive))
                                                       (without-restriction
                                                         (mevedel--instructions-in (point-min)
                                                                                   (point-max)
                                                                                   'directive))))))
               (setq found-directives toplevel-directives)))))

    (if (null found-directives)
        (user-error "No directives found")
      ;; Create display strings and mapping for completing-read-multiple
      (let* ((ov-strings (cl-loop for ov in found-directives
                                  collect (format "#%d: %s"
                                                  (overlay-get ov 'mevedel-id)
                                                  (mevedel--directive-text ov))))
             (ov-map (cl-loop for str in ov-strings
                              for ov in found-directives
                              collect (cons str ov)))
             ;; Let user select and order directives
             (prompt (if process-all
                         "Order directives to process first (unselected will follow, leave empty for all): "
                       "Select directives to process (in order, leave empty for all): "))
             (selected-strings (cl-remove-duplicates
                                (or (completing-read-multiple prompt ov-strings)
                                    ov-strings)
                                :test #'equal))
             ;; Build final directive list based on mode
             (selected-directives (mapcar (lambda (str) (cdr (assoc str ov-map)))
                                          selected-strings))
             (directives-to-process
              (if process-all
                  ;; Process-all mode: selected first, then unselected
                  (let ((unselected-directives (cl-remove-if (lambda (ov)
                                                               (memq ov selected-directives))
                                                             found-directives)))
                    (append selected-directives unselected-directives))
                ;; Default mode: only process selected directives
                selected-directives))
             (total-count (length directives-to-process)))

        (if (zerop total-count)
            (message "No directives to process")
          ;; Process directives sequentially
          (message "Processing %d directive%s..." total-count (if (= total-count 1) "" "s"))
          (mevedel--process-directives-sequentially directives-to-process 1 total-count))))))

(defun mevedel--process-directives-sequentially (directives current total)
  "Process DIRECTIVES sequentially, showing progress.

CURRENT is the current directive number (1-indexed).
TOTAL is the total number of directives."
  (if (null directives)
      ;; All done - restore original setting
      (progn
        (message "Completed processing %d directive%s" total (if (= total 1) "" "s")))
    ;; Process next directive
    (let ((directive (car directives))
          (remaining (cdr directives)))
      (message "Processing directive %d/%d: #%d %s"
               current total
               (overlay-get directive 'mevedel-id)
               (mevedel--directive-text directive))
      ;; Set up callback to process next directive
      (let ((callback (lambda (err _fsm)
                        (if err
                            (progn
                              ;; Restore original setting and stop processing
                              (message "Stopped processing at directive %d/%d due to error: %s"
                                       current total err))
                          ;; Success - continue with next directive
                          (mevedel--process-directives-sequentially
                           remaining (1+ current) total)))))
        (overlay-put directive 'mevedel-directive-action 'implement)
        (mevedel--process-directive directive 'mevedel-implement
                                    #'mevedel--implement-directive-prompt callback)))))

(defvar-local mevedel--current-directive-uuid nil
  "UUID of the directive currently being processed.")

(defun mevedel--process-directive (directive preset prompt-fn callback)
  "Process DIRECTIVE using PRESET and PROMPT-FN, calling CALLBACK when complete.

DIRECTIVE is the instruction overlay to process.
PRESET is the gptel preset to use (mevedel-implement, mevedel-revise,
mevedel-discuss).
PROMPT-FN is a function that generates the prompt from the directive
content.
CALLBACK is called with (err fsm) when processing completes.

Updates directive status and overlay, handles success/failure states."
  ;; Save any unsaved buffers first
  (save-some-buffers nil (lambda () (and (buffer-file-name) (buffer-modified-p))))

  (let* ((directive-text (mevedel--directive-text directive))
         (content (mevedel--directive-llm-prompt directive))
         (prompt (funcall prompt-fn content))
         ;; Get chat buffer for the directive's buffer workspace
         (workspace (with-current-buffer (overlay-buffer directive)
                      (mevedel-workspace)))
         (chat-buffer (mevedel--chat-buffer t workspace))
         (callback-fn (lambda (err fsm)
                        (if err
                            (let ((reason (if (eq err 'abort) "aborted" (format "%s" err))))
                              (overlay-put directive 'mevedel-directive-status 'failed)
                              (overlay-put directive 'mevedel-directive-fail-reason reason)
                              (mevedel--update-instruction-overlay directive t)
                              (pulse-momentary-highlight-region (overlay-start directive) (overlay-end directive))
                              (setq mevedel--current-directive-uuid nil)
                              (when callback
                                (funcall callback err fsm)))

                          (overlay-put directive 'mevedel-directive-status 'succeeded)
                          (with-current-buffer (overlay-buffer directive)
                            ;; Delete any child directives of the top-level directive
                            (let ((child-directives (cl-remove-if-not #'mevedel--directivep
                                                                      (mevedel--child-instructions directive))))
                              (dolist (child-directive child-directives)
                                (mevedel--delete-instruction child-directive)))
                            (save-excursion
                              (goto-char (overlay-start directive))
                              (overlay-put directive 'evaporate t)))
                          (mevedel--update-instruction-overlay directive t)
                          (pulse-momentary-highlight-region (overlay-start directive) (overlay-end directive))
                          (setq mevedel--current-directive-uuid nil)
                          (when callback
                            (funcall callback err fsm))))))

    (with-current-buffer chat-buffer
      (setq mevedel--current-directive-uuid (overlay-get directive 'mevedel-uuid)))

    (overlay-put directive 'mevedel-directive-status 'processing)
    (mevedel--update-instruction-overlay directive t)
    (pulse-momentary-highlight-region (overlay-start directive) (overlay-end directive))

    ;; Display chat buffer if configured
    (when mevedel-show-chat-buffer
      (display-buffer chat-buffer gptel-display-buffer-action))

    ;; Execute with gptel-request
    (with-current-buffer chat-buffer
      (gptel--apply-preset
       (alist-get mevedel-default-chat-preset mevedel-action-preset-alist)
       (lambda (sym val) (set (make-local-variable sym) val)))

      (let* ((prompt prompt)
             (summary directive-text)
             (action (overlay-get directive 'mevedel-directive-action))
             (action-str (symbol-name action))
             (is-org-mode (derived-mode-p 'org-mode))
             (header-prefix
              (if is-org-mode
                  ""
                (format "`%s` " action-str)))
             (header-postfix
              (if is-org-mode
                  ;; Add the action as a tag at the end of the headline.
                  (format " :%s:" action-str)
                ""))
             ;; Extract the first non-whitespace line from the summary and
             ;; truncate to fill-column.
             (truncated-summary
              (let* ((lines (split-string summary "\n" t "[[:space:]]*"))
                     (first-line (or (car lines) ""))
                     ;; Calculate available space: total fill-column minus prefix, action, and spacing.
                     (prefix (or (alist-get major-mode gptel-prompt-prefix-alist) ""))
                     (used-length (+ (length prefix) (length header-prefix) (length header-postfix)))
                     (available-length (max 10 (- (or fill-column 70) used-length))))
                (truncate-string-to-width first-line available-length nil nil "...")))
             ;; Make the separation between prompt/response clearer using a
             ;; foldable block in org-mode
             (full-prompt-str
              (if is-org-mode
                  (progn
                    ;; Should already be required, but just for good measure.
                    (require 'org-src)
                    (concat (format ":PROMPT:\n") (org-escape-code-in-string prompt) "\n:END:\n"))
                (concat "``` prompt\n" prompt "\n```\n"))))

        (goto-char (point-max))

        ;; Insert the prefix if point isn't immediately preceded by it.
        (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
          (let ((prefix-length (length prefix)))
            (unless (and (>= (point) (+ (point-min) prefix-length))
                         (string=
                          (buffer-substring-no-properties (- (point) prefix-length) (point)) prefix))
              ;; Ensure prefix starts on its own line.
              (unless (bolp)
                (insert "\n"))
              (insert prefix))))

        ;; Header string.
        (insert (format "%s%s\n" header-prefix truncated-summary))
        ;; Add the demarcated prompt text.
        (let ((cur-pt (point)))
          (insert (if (derived-mode-p 'markdown-mode)
                      (propertize full-prompt-str 'gptel 'ignore 'keymap gptel--markdown-block-map)
                    (propertize full-prompt-str 'gptel 'ignore)))
          ;; Fold the prompt immediately.
          (ignore-errors
            (if (derived-mode-p 'org-mode)
                (save-excursion
                  (search-backward ":PROMPT:" cur-pt t)
                  (when (looking-at "^:PROMPT:")
                    (org-cycle)))
              (save-excursion
                (when (re-search-backward "^```" cur-pt t)
                  (gptel-markdown-cycle-block)))))))

      (gptel-with-preset preset
        (let* ((request-callback
                (lambda (exit-code fsm)
                  (let* ((state (gptel-fsm-state fsm))
                         (error
                          (cond
                           ;; If we have a non-nil exit code (i.e. 'abort), just
                           ;; use it as the error.
                           (exit-code)
                           ;; If the FSM is in an errored state, extract the
                           ;; error text.
                           ((eq state 'ERRS)
                            (let* ((info (gptel-fsm-info fsm))
                                   (error (plist-get info :error))
                                   (http-msg (plist-get info :status))
                                   (error-type (plist-get error :type))
                                   (error-msg (plist-get error :message)))
                              (or error-msg (format "%s: %s" error-type http-msg))))
                           ;; Otherwise, consider the request successful
                           (t
                            nil))))

                    ;; Call overlay callback, including the original callback if
                    ;; provided
                    (when (functionp callback-fn)
                      (funcall callback-fn error fsm)))))
               (fsm (gptel-request prompt
                      :buffer chat-buffer
                      ;; NOTE 2025-11-03: This seems not to be necessary?
                      ;; :position (point-max)
                      :stream gptel-stream
                      :transforms gptel-prompt-transform-functions
                      :fsm (gptel-make-fsm :handlers gptel-send--handlers)))
               ;; Extract the actual gptel callback for handling responses. By
               ;; default this will generally be `gptel--insert-response' or
               ;; `gptel-curl--stream-insert-response'.
               (info (gptel-fsm-info fsm))
               (fsm-callback (plist-get info :callback))
               (wrapped-callback
                (lambda (response &rest rest)
                  "Invoke the user-provided callback after the request is aborted.
Intercept tool results for patch capture. Then pass arguments through to
the original callback."
                  (when (eq response 'abort)
                    (funcall request-callback 'abort fsm))
                  ;; Always pass through to original callback for normal display
                  (apply fsm-callback response rest))))
          (setf (gptel-fsm-info fsm) (plist-put info :callback wrapped-callback))
          (setf (gptel-fsm-info fsm) (plist-put info :mevedel-request-callback request-callback))
          fsm)))))

(defun mevedel-abort (&optional buf)
  "Abort any active request associated with buffer BUF.

Thus, abort `gptel' requests running in the mevedel chat buffer
associated with the `mevedel-workspace' for BUF.

If a callback was provided to the original request, it will be called
with the \\='abort symbol as the error parameter.

BUF defaults to the current buffer if not specified."
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (when-let* ((chat-buffer (mevedel--chat-buffer))
                (_ (buffer-live-p chat-buffer)))
      (gptel-abort chat-buffer))))

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
            (message "No mevedel instructions currently loaded")
          (message "mevedel is showing %d instruction%s from %d buffer%s"
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

;;;###autoload
(defun mevedel ()
  "Start a chat session in the current project."
  (interactive)
  (let ((chat-buffer (mevedel--chat-buffer t)))
    (with-current-buffer chat-buffer
      (gptel--apply-preset
       (alist-get mevedel-default-chat-preset mevedel-action-preset-alist)
       (lambda (sym val) (set (make-local-variable sym) val))))
    (display-buffer chat-buffer gptel-display-buffer-action)))

;;;###autoload
(defun mevedel-tutoring ()
  "Start a tutoring chat session in the current project."
  (interactive)
  (let ((chat-buffer (mevedel--tutor-buffer t)))
    (with-current-buffer chat-buffer
      (gptel--apply-preset
       'mevedel-tutor
       (lambda (sym val) (set (make-local-variable sym) val))))
    (display-buffer chat-buffer gptel-display-buffer-action)))


;;
;;; Installation

;;;###autoload
(defun mevedel-install ()
  "Register `mevedel' presets, tools, and hooks."
  (interactive)

  ;; Define custom tools
  (mevedel--define-read-tools)
  (mevedel--define-edit-tools)

  ;; Define gptel presets
  (mevedel--define-presets)

  ;; Add @ref expansion to gptel transform functions and let it run early
  (add-hook 'gptel-prompt-transform-functions #'mevedel--transform-expand-refs -90)

  ;; Setup font-lock and completion for @ref mentions in gptel buffers
  (add-hook 'gptel-mode-hook #'mevedel--prettify-ref-mentions)

  (message "mevedel installed successfully"))

;;;###autoload
(defun mevedel-uninstall ()
  "Remove `mevedel' hooks and cleanup."
  (interactive)
  ;; Remove tools
  (setf (alist-get "mevedel" gptel--known-tools nil 'remove #'equal) nil)
  ;; Remove presets
  (dolist (preset '(mevedel-discuss mevedel-implement mevedel-revise))
    (setf (alist-get preset gptel--known-presets nil 'remove) nil))

  ;; Remove @ref expansion from gptel
  (remove-hook 'gptel-prompt-transform-functions #'mevedel--transform-expand-refs)

  ;; Remove font-lock and completion setup
  (remove-hook 'gptel-mode-hook #'mevedel--prettify-ref-mentions)

  (message "mevedel uninstalled successfully"))

(provide 'mevedel)

;;; mevedel.el ends here.
