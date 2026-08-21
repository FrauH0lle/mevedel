;;; mevedel.el --- Instructed LLM programmer/assistant -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; Author: FrauH0lle
;; Version: 0.5.0
;; Keywords: convenience, tools, llm, gptel, gptel-agent
;; Package-Requires: ((emacs "30.2") (gptel "0.9.9.5") (gptel-agent "0.0.1") (websocket "1.15"))
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

;; Main entry point for mevedel.  Provides the `mevedel' command,
;; installation/uninstallation of hooks and presets, and the
;; directive-processing commands (`mevedel-implement-directive',
;; `mevedel-discuss-directive', `mevedel-request-directive-changes',
;; and `mevedel-retry-directive').
;;
;; Acts as the top-level loader that `require's every mevedel module.
;; Downstream consumers need only `(require 'mevedel)'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'gptel)
(require 'gptel-agent)

(require 'mevedel-execution-target)
(require 'mevedel-workspace)
(require 'mevedel-directive)
(require 'mevedel-instruction-registry)
(require 'mevedel-overlays)
(require 'mevedel-directive-source)
(require 'mevedel-overlay-ui)
(require 'mevedel-mentions)
(require 'mevedel-directive-persistence)
(require 'mevedel-persistence)
(require 'mevedel-file-state)
(require 'mevedel-models)
(require 'mevedel-telemetry)
(require 'mevedel-context-summary)
(require 'mevedel-plan)
(require 'mevedel-plan-handoff)
(require 'mevedel-plan-mode)
(require 'mevedel-directive-plan)
(require 'mevedel-prompt-submission)
(require 'mevedel-permission-mode)
(require 'mevedel-permission-persistence)
(require 'mevedel-permission-rules)
(require 'mevedel-permissions)
(require 'mevedel-sandbox)
(require 'mevedel-execution)
(require 'mevedel-execution-transcript)
(require 'mevedel-buddy)
(require 'mevedel-buddy-note)
(require 'mevedel-tools)
(require 'mevedel-tools-list)
(require 'mevedel-system)
(require 'mevedel-agents)
(require 'mevedel-agent-conversation)
(require 'mevedel-agent-control)
(require 'mevedel-turn)
(require 'mevedel-presets)
(require 'mevedel-transcript)
(require 'mevedel-transcript-audit)
(require 'mevedel-transcript-restore)
(require 'mevedel-compact-estimation)
(require 'mevedel-compact-evidence)
(require 'mevedel-compact-target)
(require 'mevedel-compact-run)
(require 'mevedel-compact)
(require 'mevedel-side-conversation)
(require 'mevedel-view-agent)
(require 'mevedel-session-control-fs)
(require 'mevedel-session-durability)
(require 'mevedel-session-recovery)
(require 'mevedel-session-transfer)
(require 'mevedel-session-publication)
(require 'mevedel-session-codec)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-session-rewind)
(require 'mevedel-session-fork)
(require 'mevedel-session-save-as)
(require 'mevedel-session-control-transfer)
(require 'mevedel-view-control-transfer)
(require 'mevedel-view-interaction)
(require 'mevedel-view-disclosure)
(require 'mevedel-view-segments)
(require 'mevedel-view-render)
(require 'mevedel-view-composer)
(require 'mevedel-view-input-files)
(require 'mevedel-view-stream)
(require 'mevedel-gptel-stream-bridge)
(require 'mevedel-view-zone)
(require 'mevedel-directive-activity)
(require 'mevedel-reminders)
(require 'mevedel-skills-core)
(require 'mevedel-mention-bindings)
(require 'mevedel-skills-input)
(require 'mevedel-skills-invoke)
(require 'mevedel-skills-plan)
(require 'mevedel-skills-preparation)
(require 'mevedel-skills-prompt)
(require 'mevedel-skills-ui)
(require 'mevedel-cockpit)
(require 'mevedel-pending-inputs)
(require 'mevedel-executions-list)
(require 'mevedel-plugin-lifecycle)
(require 'mevedel-plugin-registry)
(require 'mevedel-plugin-ui)
(require 'mevedel-plugins)
(require 'mevedel-init)
(require 'mevedel-review)
(require 'mevedel-gptel-bridge)
(require 'mevedel-menu)
(require 'mevedel-hooks)
(require 'mevedel-chat)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration)
(require 'mevedel-worktree)

;; `cl-seq'
(declare-function cl-remove-duplicates "cl-seq" (cl-seq &rest cl-keys))
(declare-function cl-remove-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-remove-if-not "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `gptel'
(defvar gptel-display-buffer-action)

;; `gptel-request'
(defvar gptel-prompt-transform-functions)

;; `mevedel-chat'
(declare-function mevedel--active-chat-buffer
                  "mevedel-chat" (&optional workspace))
(declare-function mevedel--attach-directive-skills
                  "mevedel-chat" (prompt record chat-buffer))
(declare-function mevedel--directive-bound-session-buffer
                  "mevedel-chat" (record workspace))
(declare-function mevedel--discuss-directive-prompt "mevedel-chat" (content))
(declare-function mevedel--display-chat-buffer "mevedel-chat" (chat-buffer))
(declare-function mevedel--dispatch-directive-implementation
                  "mevedel-chat"
                  (directive record action prompt-fn callback))
(declare-function mevedel--implement-directive-prompt "mevedel-chat" (content))
(declare-function mevedel--implement-discussion "mevedel-chat"
                  (directive &optional callback))
(declare-function mevedel--implement-discussion-prompt "mevedel-chat"
                  (content directive))
(declare-function mevedel--normalize-session-directory
                  "mevedel-chat" (directory workspace))
(declare-function mevedel--process-directive
                  "mevedel-chat"
                  (directive preset prompt-fn callback &optional options))
(declare-function mevedel--read-session-directory "mevedel-chat" (workspace))
(declare-function mevedel--start-chat
                  "mevedel-chat"
                  (workspace working-directory prompt-session
                             &optional directory-scoped))
(defvar mevedel--view-buffer)

;; `mevedel-compact'
(declare-function mevedel--compact-transform-auto
                  "mevedel-compact" (continue fsm))

;; `mevedel-directive'
(declare-function mevedel-directive-actions "mevedel-directive" (directive))

;; `mevedel-gptel-stream-bridge'
(declare-function mevedel-gptel-stream-bridge-install
                  "mevedel-gptel-stream-bridge" ())
(declare-function mevedel-gptel-stream-bridge-uninstall
                  "mevedel-gptel-stream-bridge" ())

;; `mevedel-presets'
(declare-function mevedel--define-presets "mevedel-presets")
(declare-function mevedel-preset-apply
                  "mevedel-presets" (name &optional buffer))
(defvar mevedel-action-preset-alist)

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-choose-entry
                  "mevedel-session-persistence" (workspace))

;; `mevedel-skills-core'
(declare-function mevedel-skills-install-hot-reload
                  "mevedel-skills-core" ())
(declare-function mevedel-skills-uninstall-hot-reload
                  "mevedel-skills-core" ())

;; `mevedel-skills-input'
(declare-function mevedel-skills-input-transform-inline-attachments
                  "mevedel-skills-input" (fsm))

;; `mevedel-skills-invoke'
(declare-function mevedel-skills--transform-apply-request-model-policy
                  "mevedel-skills-invoke" (fsm))

;; `mevedel-structs'
(declare-function mevedel-directive-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-state "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-install-provider-adapter
                  "mevedel-tool-render-data" ())
(declare-function mevedel-tool-render-data-uninstall-provider-adapter
                  "mevedel-tool-render-data" ())

;; `mevedel-tool-repair'
(declare-function mevedel-tool-repair-install-shape-adapter
                  "mevedel-tool-repair" ())
(declare-function mevedel-tool-repair-uninstall-shape-adapter
                  "mevedel-tool-repair" ())

;; `mevedel-transport'
(declare-function mevedel-transport-install "mevedel-transport" ())
(declare-function mevedel-transport-uninstall "mevedel-transport" ())

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
        (unless (memq 'implement
                      (mevedel-directive-actions
                       (mevedel--directive-record directive)))
          (user-error "Implement requires a Ready directive"))
        (mevedel--dispatch-directive-implementation
         directive (mevedel--directive-record directive) 'implement
         #'mevedel--implement-directive-prompt callback))
    (user-error "No directive found at point")))

;;;###autoload
(defun mevedel-request-directive-changes ()
  "Open Request changes for the implemented directive at point."
  (interactive)
  (if-let* ((directive (mevedel--topmost-instruction (mevedel--highest-priority-instruction
                                                     (mevedel--instructions-at (point) 'directive)
                                                      t)
                                                     'directive)))
      (progn
        (unless (memq 'request-changes
                      (mevedel-directive-actions
                       (mevedel--directive-record directive)))
          (user-error "Request changes requires an implemented directive"))
        (mevedel-view-enter-directive-scope directive 'request-changes))
    (user-error "No directive found at point")))

;;;###autoload
(defun mevedel-retry-directive ()
  "Open Retry for the failed or aborted directive at point."
  (interactive)
  (if-let* ((directive (mevedel--topmost-instruction (mevedel--highest-priority-instruction
                                                      (mevedel--instructions-at (point) 'directive)
                                                      t)
                                                     'directive)))
      (progn
        (unless (memq 'retry
                      (mevedel-directive-actions
                       (mevedel--directive-record directive)))
          (user-error "Retry requires a failed or aborted directive"))
        (mevedel-view-enter-directive-scope directive 'retry))
    (user-error "No directive found at point")))

;;;###autoload
(defun mevedel-discuss-directive ()
  "Discuss the directive at point.
Submit a Ready directive immediately; otherwise focus its follow-up composer."
  (interactive)
  (if-let* ((directive (mevedel--topmost-instruction (mevedel--highest-priority-instruction
                                                      (mevedel--instructions-at (point) 'directive)
                                                      t)
                                                     'directive)))
      (progn
        (let ((actions
               (mevedel-directive-actions
                (mevedel--directive-record directive))))
          (unless (cl-intersection
                   actions '(discuss continue-discussion discuss-result))
            (user-error "Discussion is unavailable while processing"))
          (mevedel-view-enter-directive-scope directive 'discuss)
          (when (memq 'discuss actions)
            (mevedel--start-directive-discussion directive))))
    (user-error "No directive found at point")))

;;;###autoload
(defun mevedel-implement-discussion-directive (&optional callback)
  "Implement the directive at point using its complete local discussion.
CALLBACK receives the ordinary directive terminal arguments."
  (interactive)
  (if-let* ((directive
             (mevedel--topmost-instruction
              (mevedel--highest-priority-instruction
               (mevedel--instructions-at (point) 'directive) t)
              'directive)))
      (mevedel--implement-discussion directive callback)
    (user-error "No directive found at point")))

;;;###autoload
(defun mevedel-process-directives (&optional process-all)
  "Process initial directive implementations sequentially in source order.

Collects directives based on context:

- If a region is selected, collect all directives in that region
- If no region is selected but point is on a directive, collect that
  directive
- If no region and no directive at point, collect all directives in
  buffer

Presents directives to user via `completing-read-multiple' for filtering.

Without prefix argument, only selected directives are processed.

With PROCESS-ALL or prefix argument (\\[universal-argument]), all
top-level directives are processed without prompting.  Nested directives
remain details of their topmost parent."
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

    (setq found-directives
          (sort
           found-directives
           (lambda (a b)
             (let* ((a-record (mevedel--directive-record a))
                    (b-record (mevedel--directive-record b))
                    (a-anchor (mevedel-directive-anchor a-record))
                    (b-anchor (mevedel-directive-anchor b-record))
                    (a-order (or (plist-get a-anchor :source-order)
                                 (list (overlay-start a) (overlay-end a))))
                    (b-order (or (plist-get b-anchor :source-order)
                                 (list (overlay-start b) (overlay-end b)))))
               (or (< (car a-order) (car b-order))
                   (and (= (car a-order) (car b-order))
                        (or (< (cadr a-order) (cadr b-order))
                            (and (= (cadr a-order) (cadr b-order))
                                 (string-lessp
                                  (mevedel-directive-id a-record)
                                  (mevedel-directive-id b-record))))))))))
    (if (null found-directives)
        (user-error "No directives found")
      (let* ((ov-strings (cl-loop for ov in found-directives
                                  collect (format "#%d: %s"
                                                  (overlay-get ov 'mevedel-id)
                                                  (mevedel--directive-text ov))))
             (ov-map (cl-loop for str in ov-strings
                              for ov in found-directives
                              collect (cons str ov)))
             (selected-strings
              (unless process-all
                (completing-read-multiple
                 "Select directives to process (source order, leave empty for all): "
                 ov-strings)))
             (selected-directives
              (mapcar (lambda (str) (cdr (assoc str ov-map)))
                      selected-strings))
             (directives-to-process
              (if (or process-all (null selected-strings))
                  found-directives
                (cl-remove-if-not
                 (lambda (directive)
                   (memq directive selected-directives))
                 found-directives)))
             (workspace (mevedel-workspace))
             ;; Records without a live overlay (Source missing) are
             ;; invisible to the buffer scan above; a process-all batch
             ;; still owes them an attempt.
             (records (append
                       (mapcar #'mevedel--directive-record
                               directives-to-process)
                       (when (and workspace
                                  (or process-all (null selected-strings)))
                         (cl-remove-if-not
                          (lambda (record)
                            (eq 'source-missing
                                (plist-get (mevedel-directive-anchor record)
                                           :state)))
                          (mevedel-workspace-directives workspace)))))
             (total-count (length records)))

        (if (zerop total-count)
            (message "mevedel: no directives to process")
          (message "mevedel: processing %d directive%s..." total-count (if (= total-count 1) "" "s"))
          (mevedel--process-directives-sequentially
           records workspace 1 total-count))))))

(defun mevedel--process-directives-sequentially
    (records workspace current total)
  "Process directive RECORDS in WORKSPACE sequentially, showing progress.

CURRENT is the current directive number (1-indexed).
TOTAL is the total number of directives."
  (if (null records)
      (message "mevedel: completed processing %d directive%s"
               total (if (= total 1) "" "s"))
    (let* ((record (car records))
           (remaining (cdr records))
           (actions (mevedel-directive-actions record))
           (implementation-action
            (cond ((memq 'implement-this actions) 'implement-this)
                  ((memq 'implement actions) 'implement))))
      (cond
       ((mevedel-directive-attempts record)
        (message "mevedel: skipping directive %d/%d: existing implementation activity"
                 current total)
        (mevedel--process-directives-sequentially
         remaining workspace (1+ current) total))
       ((not implementation-action)
        (message "mevedel: skipping directive %d/%d: ineligible lifecycle state %s"
                 current total
                 (capitalize
                  (symbol-name
                   (or (mevedel-directive-state record) 'ready))))
        (mevedel--process-directives-sequentially
         remaining workspace (1+ current) total))
       (t
        (let ((context
               (condition-case err
                   (let ((context
                          (mevedel--directive-action-context
                           record workspace)))
                     (if (eq implementation-action 'implement-this)
                         (mevedel--implement-discussion-prompt
                          (plist-get context :prompt) record)
                       (mevedel--implement-directive-prompt
                        (plist-get context :prompt)))
                     ;; Pre-validate selected skills in the same session
                     ;; dispatch will use (bound first) so a stale
                     ;; selection skips this directive instead of
                     ;; stopping the batch at dispatch.  With no live
                     ;; session buffer, dispatch validates alone.
                     (when-let* (((mevedel-directive-skills record))
                                 (chat-buffer
                                  (or (mevedel--directive-bound-session-buffer
                                       record workspace)
                                      (mevedel--active-chat-buffer workspace)))
                                 ((buffer-live-p chat-buffer)))
                       (mevedel--attach-directive-skills
                        "" record chat-buffer))
                     context)
                 (error
                  (message "mevedel: skipping directive %d/%d: %s"
                           current total (error-message-string err))
                  nil))))
          (if (null context)
              (mevedel--process-directives-sequentially
               remaining workspace (1+ current) total)
            (let* ((directive (plist-get context :directive))
                   (callback
                    (lambda (err _fsm)
                      (if err
                          (message
                           "mevedel: stopped processing at directive %d/%d: implementation %s%s"
                           current total
                           (if (eq err 'abort) "aborted" "failed")
                           (if (eq err 'abort) "" (format ": %s" err)))
                        ;; Terminal handlers clear the active request before
                        ;; this zero-delay continuation runs.
                        (run-at-time
                         0 nil
                         #'mevedel--process-directives-sequentially
                         remaining workspace (1+ current) total)))))
              (message "mevedel: processing directive %d/%d: #%s %s"
                       current total
                       (or (overlay-get directive 'mevedel-id)
                           (mevedel-directive-id record))
                       (mevedel-directive-request record))
              (condition-case err
                  (if (eq implementation-action 'implement-this)
                      (mevedel--implement-discussion directive callback)
                    (mevedel--dispatch-directive-implementation
                     directive record 'implement
                     #'mevedel--implement-directive-prompt callback))
                (error
                 (message
                  "mevedel: stopped processing at directive %d/%d: %s"
                  current total (error-message-string err))))))))))))

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

Without prefix ARG, discover persisted workspace sessions first.  The entry
chooser offers a new session, ordinary resume, read-only inspection of an
active writer, or confirmed takeover of an expired lease.  With no persisted
sessions, retain the live-buffer behavior: create \"main\", switch to the sole
live session, or prompt among multiple live sessions.

With prefix ARG (\\[universal-argument]):
- Prompt for a working directory under the current project.
- Prompt for a session in that directory, allowing selection of an
  existing session or creation of a new one by typing a new name."
  (interactive "P")
  (let* ((workspace (mevedel-workspace))
         (working-directory (if arg
                                (mevedel--read-session-directory workspace)
                              (mevedel-workspace-root workspace)))
         (entry
          (unless arg
            (require 'mevedel-session-persistence)
            (mevedel-session-persistence-choose-entry workspace))))
    (cond
     ((bufferp entry)
      (mevedel--display-chat-buffer entry))
     ((eq entry 'new)
      (mevedel--start-chat workspace working-directory t nil))
     (t
      (mevedel--start-chat workspace working-directory arg arg)))))

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

;;
;;; Installation

;;;###autoload
(defun mevedel-install ()
  "Register `mevedel' presets, tools, and hooks."
  (interactive)

  (mevedel-transport-install)

  ;; Define custom tools
  (mevedel-tools-register)

  ;; Reflect managed Bash progress in the view and secure independent
  ;; completions in the original owner's mailbox.
  (add-hook 'mevedel-execution-event-functions
            #'mevedel-view-stream-handle-execution-event)
  (setq mevedel-execution-mailbox-delivery-function
        #'mevedel-tool-exec-handle-execution-event)

  ;; Define gptel presets
  (mevedel--define-presets)

  ;; Apply the root request's workload and skill overrides before
  ;; compaction so the threshold uses the effective context window.
  ;; This only mutates prompt-buffer locals, so no prompt text is lost
  ;; if compaction rebuilds the prompt buffer next.
  (add-hook 'gptel-prompt-transform-functions
            #'mevedel-skills--transform-apply-request-model-policy -100)

  ;; Substitute view-derived text only in gptel's temporary request buffer.
  (add-hook 'gptel-prompt-transform-functions
            #'mevedel-view--transform-model-input -91)

  ;; Expand @ref/@file mentions early in the gptel transform chain
  (add-hook 'gptel-prompt-transform-functions #'mevedel--transform-expand-mentions -90)

  ;; Bare gptel buffers use their inline-attachment path.  Paired
  ;; mevedel views prepare complete plans before `gptel-send', so their stash
  ;; is empty and this transform is a no-op.
  (add-hook 'gptel-prompt-transform-functions
            #'mevedel-skills-input-transform-inline-attachments -89)

  ;; Inject system reminders after mention expansion but before the request fires
  (add-hook 'gptel-prompt-transform-functions #'mevedel-reminders--transform -80)

  ;; Keep directive turns visible in the stored transcript while excluding
  ;; them from ordinary request copies before provider parsing.
  (add-hook 'gptel-prompt-transform-functions
            #'mevedel-transcript-exclude-directive-turns -79)

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
  (require 'mevedel-tool-render-data)
  (mevedel-tool-render-data-install-provider-adapter)

  ;; Preserve empty object versus null before gptel runs tool hooks.
  (require 'mevedel-tool-repair)
  (mevedel-tool-repair-install-shape-adapter)

  ;; Install slash-command advice on `gptel-send'
  (mevedel-worktree-install-slash-command)
  (mevedel-skills-install-slash-commands)

  ;; Install skill hot-reload hooks/watchers for active strategies
  (mevedel-skills-install-hot-reload)

  ;; Install the gptel stream compatibility bridge.
  (mevedel-gptel-stream-bridge-install)

  ;; Best-effort save and cleanup of live sessions on Emacs exit.  The hook
  ;; itself is installed at `mevedel-session-persistence' file-load time so
  ;; it's active even when the user never calls `mevedel-install' (e.g. only
  ;; invokes `mevedel-resume').
  (require 'mevedel-session-persistence)

  (message "mevedel installed successfully"))

;;;###autoload
(defun mevedel-uninstall ()
  "Remove `mevedel' hooks and cleanup."
  (interactive)
  (mevedel-transport-uninstall)
  (mevedel-execution-teardown-all)
  ;; Remove tools
  (setf (alist-get "mevedel" gptel--known-tools nil 'remove #'equal) nil)
  (remove-hook 'mevedel-execution-event-functions
               #'mevedel-view-stream-handle-execution-event)
  (when (eq mevedel-execution-mailbox-delivery-function
            #'mevedel-tool-exec-handle-execution-event)
    (setq mevedel-execution-mailbox-delivery-function nil))
  ;; Remove presets
  (dolist (preset '(mevedel-discuss mevedel-implement))
    (setf (alist-get preset gptel--known-presets nil 'remove) nil))

  ;; Remove mention expansion from gptel
  (remove-hook 'gptel-prompt-transform-functions #'mevedel--transform-expand-mentions)

  ;; Remove inline skill attachment expansion from gptel
  (remove-hook 'gptel-prompt-transform-functions
               #'mevedel-skills-input-transform-inline-attachments)

  ;; Remove root request model policy transform
  (remove-hook 'gptel-prompt-transform-functions
               #'mevedel-skills--transform-apply-request-model-policy)

  ;; Remove view request-input substitution
  (remove-hook 'gptel-prompt-transform-functions
               #'mevedel-view--transform-model-input)

  ;; Remove reminder injection
  (remove-hook 'gptel-prompt-transform-functions #'mevedel-reminders--transform)

  ;; Remove directive context projection
  (remove-hook 'gptel-prompt-transform-functions
               #'mevedel-transcript-exclude-directive-turns)

  ;; Remove auto-compaction transform
  (remove-hook 'gptel-prompt-transform-functions
               #'mevedel--compact-transform-auto)

  ;; Remove render-data scrubber advice
  (when (featurep 'mevedel-tool-render-data)
    (mevedel-tool-render-data-uninstall-provider-adapter))

  ;; Remove lossless tool-argument shape restoration.
  (when (featurep 'mevedel-tool-repair)
    (mevedel-tool-repair-uninstall-shape-adapter))

  ;; Remove slash-command advice
  (mevedel-worktree-uninstall-slash-command)
  (mevedel-skills-uninstall-slash-commands)

  ;; Remove skill hot-reload hooks/watchers and registry state
  (mevedel-skills-uninstall-hot-reload)

  ;; Remove the gptel stream compatibility bridge.
  (when (featurep 'mevedel-gptel-stream-bridge)
    (mevedel-gptel-stream-bridge-uninstall))

  (message "mevedel uninstalled successfully"))

(provide 'mevedel)

;;; mevedel.el ends here.
