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

;; Main entry point for mevedel.  Provides the `mevedel' and
;; `mevedel-tutoring' commands, installation/uninstallation of hooks
;; and presets, and the directive-processing commands
;; (`mevedel-implement-directive', `mevedel-revise-directive',
;; `mevedel-discuss-directive', `mevedel-tutor-directive').
;;
;; Acts as the top-level loader that `require's every mevedel module.
;; Downstream consumers need only `(require 'mevedel)'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'gptel)
(require 'gptel-agent)

(require 'mevedel-workspace)
(require 'mevedel-overlays)
(require 'mevedel-mentions)
(require 'mevedel-persistence)
(require 'mevedel-file-state)
(require 'mevedel-tools)
(require 'mevedel-system)
(require 'mevedel-agents)
(require 'mevedel-compact)
(require 'mevedel-reminders)
(require 'mevedel-skills)
(require 'mevedel-chat)

;; `gptel'
(declare-function gptel--apply-preset "ext:gptel" (preset setter))
(defvar gptel-display-buffer-action)
(defvar gptel-prompt-transform-functions)

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-install-tool-result-scrubber "mevedel-pipeline" ())
(declare-function mevedel-pipeline-uninstall-tool-result-scrubber "mevedel-pipeline" ())

;; `mevedel-presets'
(declare-function mevedel--define-presets "mevedel-presets")
(defvar mevedel-action-preset-alist)

;; `mevedel-view'
(declare-function mevedel-view-install-gptel-menu-advice "mevedel-view" ())
(declare-function mevedel-view-uninstall-gptel-menu-advice "mevedel-view" ())

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--kill-emacs-hook
                  "mevedel-session-persistence" ())

;; `mevedel-chat'
(declare-function mevedel--chat-buffer "mevedel-chat" (session-name &optional create workspace))
(defvar mevedel--view-buffer)
(declare-function mevedel--tutor-buffer "mevedel-chat" (&optional create workspace))
(declare-function mevedel--workspace-sessions "mevedel-chat" (workspace))
(declare-function mevedel--process-directive "mevedel-chat" (directive preset prompt-fn callback))
(declare-function mevedel--implement-directive-prompt "mevedel-chat" (content))
(declare-function mevedel--revise-directive-prompt "mevedel-chat" (content &optional patch-buffer directive))
(declare-function mevedel--discuss-directive-prompt "mevedel-chat" (content))


(defgroup mevedel nil
  "Customization group for Evedel."
  :group 'tools)

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
;;; Commands

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
(defun mevedel (&optional arg)
  "Start or switch to a chat session in the current project.

Without prefix ARG:
- No sessions exist: create \"main\" silently.
- One session exists: switch to it.
- Multiple sessions: prompt with `completing-read'.

With prefix ARG (\\[universal-argument]):
- Always prompt, allowing selection of an existing session or
  creation of a new one by typing a new name."
  (interactive "P")
  (let* ((workspace (mevedel-workspace))
         (sessions (mevedel--workspace-sessions workspace))
         (session-name
          (cond
           (arg (mevedel--pick-session sessions "main"))
           ((null sessions) "main")
           ((= (length sessions) 1) (caar sessions))
           (t (mevedel--pick-session sessions nil))))
         (chat-buffer (mevedel--chat-buffer session-name t workspace)))
    (with-current-buffer chat-buffer
      (gptel--apply-preset
       (alist-get mevedel-default-chat-preset mevedel-action-preset-alist)
       (lambda (sym val) (set (make-local-variable sym) val))))
    ;; Display the view buffer, not the data buffer
    (display-buffer (or (buffer-local-value 'mevedel--view-buffer chat-buffer)
                        chat-buffer)
                    gptel-display-buffer-action)))

;;;###autoload
(defun mevedel-tutoring ()
  "Start a tutoring chat session in the current project."
  (interactive)
  (let ((chat-buffer (mevedel--tutor-buffer t)))
    (with-current-buffer chat-buffer
      (gptel--apply-preset
       'mevedel-tutor
       (lambda (sym val) (set (make-local-variable sym) val))))
    ;; Display the view buffer, not the data buffer
    (display-buffer (or (buffer-local-value 'mevedel--view-buffer chat-buffer)
                        chat-buffer)
                    gptel-display-buffer-action)))

(defun mevedel--pick-session (sessions default)
  "Prompt for a session name via `completing-read'.

SESSIONS is an alist of (NAME . BUFFER) for the current workspace.
DEFAULT is the initial input; nil means no default.  Typing a name not
in SESSIONS creates a new session with that name."
  (let ((names (mapcar #'car sessions)))
    (completing-read "Session: " names nil nil nil nil default)))




;;
;;; Installation

;;;###autoload
(defun mevedel-install ()
  "Register `mevedel' presets, tools, and hooks."
  (interactive)

  ;; Define custom tools
  (mevedel-tool-web--register)
  (mevedel-tool-fs--register)
  (mevedel-tool-code--register)
  (mevedel-tool-plan--register)
  (mevedel-tool-tutor--register)
  (mevedel-tool-exec--register)
  (mevedel-tool-ui--register)
  (mevedel-tool-task--register)
  (mevedel-tool-introspect--register)
  (mevedel-skills--register)

  ;; Define gptel presets
  (mevedel--define-presets)

  ;; Expand @ref/@file mentions early in the gptel transform chain
  (add-hook 'gptel-prompt-transform-functions #'mevedel--transform-expand-mentions -90)

  ;; Inject system reminders after mention expansion but before the request fires
  (add-hook 'gptel-prompt-transform-functions #'mevedel-reminders--transform -80)

  ;; Strip render-data side-channel blocks on the LLM path only.  The
  ;; advice on `gptel--parse-tool-results' (the single chokepoint where
  ;; `:result' strings become API-shaped tool_result messages) catches
  ;; both tool-follow-up and user-initiated request paths while leaving
  ;; the chat-buffer display / view parser / persistence untouched.
  (require 'mevedel-pipeline)
  (mevedel-pipeline-install-tool-result-scrubber)

  ;; Install slash-command advice on `gptel-send'
  (mevedel-skills-install-slash-commands)

  ;; Proxy `gptel-menu' from view buffers to their data buffers
  (require 'mevedel-view)
  (mevedel-view-install-gptel-menu-advice)

  ;; Best-effort save of live sessions on Emacs exit.
  (require 'mevedel-session-persistence)
  (add-hook 'kill-emacs-hook
            #'mevedel-session-persistence--kill-emacs-hook)

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

  ;; Remove mention expansion from gptel
  (remove-hook 'gptel-prompt-transform-functions #'mevedel--transform-expand-mentions)

  ;; Remove reminder injection
  (remove-hook 'gptel-prompt-transform-functions #'mevedel-reminders--transform)

  ;; Remove render-data scrubber advice
  (when (featurep 'mevedel-pipeline)
    (mevedel-pipeline-uninstall-tool-result-scrubber))

  ;; Remove slash-command advice
  (mevedel-skills-uninstall-slash-commands)

  ;; Remove `gptel-menu' proxy advice
  (when (featurep 'mevedel-view)
    (mevedel-view-uninstall-gptel-menu-advice))

  ;; Remove session-persistence kill-emacs hook
  (when (featurep 'mevedel-session-persistence)
    (remove-hook 'kill-emacs-hook
                 #'mevedel-session-persistence--kill-emacs-hook))

  (message "mevedel uninstalled successfully"))

(provide 'mevedel)

;;; mevedel.el ends here.
