;;; mevedel.el --- Instructed LLM programmer/assistant -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; Author: FrauH0lle
;; Version: 0.5.0
;; Keywords: convenience, tools, llm, gptel, gptel-agent
;; Package-Requires: ((emacs "30.2") (gptel "0.9.9.5") (gptel-agent "0.0.1"))
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
(require 'mevedel-models)
(require 'mevedel-plan)
(require 'mevedel-sandbox)
(require 'mevedel-tools)
(require 'mevedel-tools-list)
(require 'mevedel-system)
(require 'mevedel-agents)
(require 'mevedel-turn)
(require 'mevedel-presets)
(require 'mevedel-transcript)
(require 'mevedel-transcript-restore)
(require 'mevedel-compact)
(require 'mevedel-view-agent)
(require 'mevedel-view-interaction)
(require 'mevedel-view-render)
(require 'mevedel-view-composer)
(require 'mevedel-view-stream)
(require 'mevedel-view-zone)
(require 'mevedel-reminders)
(require 'mevedel-skills-core)
(require 'mevedel-mention-bindings)
(require 'mevedel-skills-invoke)
(require 'mevedel-skills-plan)
(require 'mevedel-skills-prompt)
(require 'mevedel-skills-ui)
(require 'mevedel-cockpit)
(require 'mevedel-plugins)
(require 'mevedel-init)
(require 'mevedel-review)
(require 'mevedel-gptel-bridge)
(require 'mevedel-menu)
(require 'mevedel-hooks)
(require 'mevedel-chat)
(require 'mevedel-worktree)

;; `cl-seq'
(declare-function cl-remove-duplicates "cl-seq" (cl-seq &rest cl-keys))
(declare-function cl-remove-if "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `gptel'
(defvar gptel-display-buffer-action)

;; `gptel-request'
(defvar gptel-prompt-transform-functions)

;; `mevedel-chat'
(declare-function mevedel--discuss-directive-prompt "mevedel-chat" (content))
(declare-function mevedel--implement-directive-prompt "mevedel-chat" (content))
(declare-function mevedel--main-fsm-on-error "mevedel-chat" (fsm))
(declare-function mevedel--normalize-session-directory
                  "mevedel-chat" (directory workspace))
(declare-function mevedel--process-directive
                  "mevedel-chat" (directive preset prompt-fn callback))
(declare-function mevedel--read-session-directory "mevedel-chat" (workspace))
(declare-function mevedel--revise-directive-prompt
                  "mevedel-chat" (content &optional patch-buffer directive))
(declare-function mevedel--start-chat
                  "mevedel-chat"
                  (workspace working-directory prompt-session
                             &optional directory-scoped))
(declare-function mevedel--tutor-buffer
                  "mevedel-chat" (&optional create workspace))
(defvar mevedel--view-buffer)

;; `mevedel-compact'
(declare-function mevedel--compact-transform-auto
                  "mevedel-compact" (continue fsm))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-install-tool-result-scrubber
                  "mevedel-pipeline" ())
(declare-function mevedel-pipeline-uninstall-tool-result-scrubber
                  "mevedel-pipeline" ())

;; `mevedel-presets'
(declare-function mevedel--define-presets "mevedel-presets")
(declare-function mevedel-preset-apply
                  "mevedel-presets" (name &optional buffer))
(defvar mevedel-action-preset-alist)

;; `mevedel-skills-core'
(declare-function mevedel-skills-install-hot-reload
                  "mevedel-skills-core" ())
(declare-function mevedel-skills-uninstall-hot-reload
                  "mevedel-skills-core" ())

;; `mevedel-skills-invoke'
(declare-function mevedel-skills--transform-apply-model-override
                  "mevedel-skills-invoke" (fsm))
(declare-function mevedel-skills--transform-expand-inline-attachments
                  "mevedel-skills-invoke" (fsm))

;; `mevedel-structs'
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `mevedel-tool-repair'
(declare-function mevedel-tool-repair-install-shape-adapter
                  "mevedel-tool-repair" ())
(declare-function mevedel-tool-repair-uninstall-shape-adapter
                  "mevedel-tool-repair" ())

;; `mevedel-view-stream'
(declare-function mevedel-view-stream-install "mevedel-view-stream" ())
(declare-function mevedel-view-stream-uninstall "mevedel-view-stream" ())

;; `mevedel-worktree'
(declare-function mevedel-worktree-install-slash-command "mevedel-worktree" ())
(declare-function mevedel-worktree-uninstall-slash-command
                  "mevedel-worktree" ())

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

Interactively, or when MESSAGE is non-nil, show it in echo area.  With
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
process completes.  The callback will receive two arguments: ERROR (nil
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
process completes.  The callback will receive two arguments: ERROR (nil
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
process completes.  The callback will receive two arguments: ERROR (nil
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
completes.  The callback will receive two arguments: ERROR (nil on
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
                          ;; Success - continue with next directive after the
                          ;; terminal FSM handlers finish clearing the active
                          ;; request.
                          (run-at-time
                           0 nil
                           #'mevedel--process-directives-sequentially
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
- Prompt for a working directory under the current project.
- Prompt for a session in that directory, allowing selection of an
  existing session or creation of a new one by typing a new name."
  (interactive "P")
  (let* ((workspace (mevedel-workspace))
         (working-directory (if arg
                                (mevedel--read-session-directory workspace)
                              (mevedel-workspace-root workspace))))
    (mevedel--start-chat workspace working-directory arg arg)))

;;;###autoload
(defun mevedel-in-directory (directory &optional arg)
  "Start or switch to a chat session whose working directory is DIRECTORY.

DIRECTORY must be inside the current workspace root.  With prefix ARG,
always prompt for the session name."
  (interactive
   (let* ((workspace (mevedel-workspace))
          (directory (mevedel--read-session-directory workspace)))
     (list directory current-prefix-arg)))
  (let* ((workspace (mevedel-workspace))
         (working-directory
          (mevedel--normalize-session-directory directory workspace)))
    (mevedel--start-chat workspace working-directory arg t)))

;;;###autoload
(defun mevedel-tutoring ()
  "Start a tutoring chat session in the current project."
  (interactive)
  (let ((chat-buffer (mevedel--tutor-buffer t)))
    (with-current-buffer chat-buffer
      (mevedel-preset-apply 'mevedel-tutor))
    ;; Display the view buffer, not the data buffer
    (display-buffer (or (buffer-local-value 'mevedel--view-buffer chat-buffer)
                        chat-buffer)
                    gptel-display-buffer-action)))

;;
;;; Installation

;;;###autoload
(defun mevedel-install ()
  "Register `mevedel' presets, tools, and hooks."
  (interactive)

  ;; Define custom tools
  (mevedel-tools-register)

  ;; Define gptel presets
  (mevedel--define-presets)

  ;; Apply slash/inline skill model overrides before compaction so the
  ;; threshold uses the request's effective context window.  This only
  ;; mutates prompt-buffer locals, so no prompt text is lost if
  ;; compaction rebuilds the prompt buffer next.
  (add-hook 'gptel-prompt-transform-functions
            #'mevedel-skills--transform-apply-model-override -100)

  ;; Substitute view-derived text only in gptel's temporary request buffer.
  (add-hook 'gptel-prompt-transform-functions
            #'mevedel-view--transform-model-input -91)

  ;; Expand @ref/@file mentions early in the gptel transform chain
  (add-hook 'gptel-prompt-transform-functions #'mevedel--transform-expand-mentions -90)

  ;; Bare gptel buffers use their inline-attachment path.  Paired
  ;; mevedel views prepare complete plans before `gptel-send', so their stash
  ;; is empty and this transform is a no-op.
  (add-hook 'gptel-prompt-transform-functions
            #'mevedel-skills--transform-expand-inline-attachments -89)

  ;; Inject system reminders after mention expansion but before the request fires
  (add-hook 'gptel-prompt-transform-functions #'mevedel-reminders--transform -80)

  ;; Auto-compact after mevedel's synchronous prompt transforms so an
  ;; auto-compact send preserves the transformed pending prompt when it
  ;; rebuilds the temporary request buffer.
  (add-hook 'gptel-prompt-transform-functions
            #'mevedel--compact-transform-auto -70)

  ;; Strip render-data side-channel blocks on the LLM path only.  The
  ;; advice on `gptel--parse-tool-results' (the single chokepoint where
  ;; `:result' strings become API-shaped tool_result messages) catches
  ;; both tool-follow-up and user-initiated request paths while leaving
  ;; the chat-buffer display / view parser / persistence untouched.
  (require 'mevedel-pipeline)
  (mevedel-pipeline-install-tool-result-scrubber)

  ;; Preserve empty object versus null before gptel runs tool hooks.
  (require 'mevedel-tool-repair)
  (mevedel-tool-repair-install-shape-adapter)

  ;; Install slash-command advice on `gptel-send'
  (mevedel-worktree-install-slash-command)
  (mevedel-skills-install-slash-commands)

  ;; Install skill hot-reload hooks/watchers for active strategies
  (mevedel-skills-install-hot-reload)

  ;; Install view-specific gptel stream repair advice.
  (mevedel-view-stream-install)

  ;; Best-effort save of live sessions on Emacs exit.  The hook itself
  ;; is installed at `mevedel-session-persistence' file-load time so
  ;; it's active even when the user never calls `mevedel-install'
  ;; (e.g. only invokes `mevedel-resume').
  (require 'mevedel-session-persistence)

  ;; Terminate the session when the main agent errors out.  The main
  ;; turn is the load-bearing transcript; once gptel routes its FSM
  ;; through ERRS the conversation state can no longer roll forward,
  ;; so cancel any in-flight sub-agents and queued permissions.
  (advice-add 'gptel--handle-error :after #'mevedel--main-fsm-on-error)

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

  ;; Remove inline skill attachment expansion from gptel
  (remove-hook 'gptel-prompt-transform-functions
               #'mevedel-skills--transform-expand-inline-attachments)

  ;; Remove skill model override transform
  (remove-hook 'gptel-prompt-transform-functions
               #'mevedel-skills--transform-apply-model-override)

  ;; Remove view request-input substitution
  (remove-hook 'gptel-prompt-transform-functions
               #'mevedel-view--transform-model-input)

  ;; Remove reminder injection
  (remove-hook 'gptel-prompt-transform-functions #'mevedel-reminders--transform)

  ;; Remove auto-compaction transform
  (remove-hook 'gptel-prompt-transform-functions
               #'mevedel--compact-transform-auto)

  ;; Remove render-data scrubber advice
  (when (featurep 'mevedel-pipeline)
    (mevedel-pipeline-uninstall-tool-result-scrubber))

  ;; Remove lossless tool-argument shape restoration.
  (when (featurep 'mevedel-tool-repair)
    (mevedel-tool-repair-uninstall-shape-adapter))

  ;; Remove slash-command advice
  (mevedel-worktree-uninstall-slash-command)
  (mevedel-skills-uninstall-slash-commands)

  ;; Remove skill hot-reload hooks/watchers and registry state
  (mevedel-skills-uninstall-hot-reload)

  ;; Remove view-specific gptel stream repair advice
  (when (featurep 'mevedel-view-stream)
    (mevedel-view-stream-uninstall))

  ;; Remove main-agent error termination advice
  (advice-remove 'gptel--handle-error #'mevedel--main-fsm-on-error)

  (message "mevedel uninstalled successfully"))

(provide 'mevedel)

;;; mevedel.el ends here.
