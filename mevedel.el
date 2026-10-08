;;; mevedel.el --- Instructed LLM programmer/assistant -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; Author: FrauH0lle
;; Version: 0.5.0
;; Keywords: convenience, tools, llm, gptel
;; Package-Requires: ((emacs "31.1") (gptel "0.9.9.6") (acp "0.15.2") (yaml "1.2.0") (orderless "1.1") (websocket "1.15") (qrencode "1.4"))
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
;; Loads foundational data and exposes autoloaded feature entry points.
;; Installation registers the complete tool catalog and integration hooks.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'gptel)

(require 'mevedel-workspace)
(require 'mevedel-structs)
(require 'mevedel-instruction-registry)
(require 'mevedel-overlay-ui)
(require 'mevedel-persistence)
(require 'mevedel-models)
(require 'mevedel-claude-code-backend)

(mevedel-claude-code-register)

;; Top-level commands stay reachable through M-x as soon as the package is
;; loaded, also from a source checkout without generated autoloads.  Mode-local
;; and transient suffix commands load with the owner that binds them.
(autoload 'mevedel-abort "mevedel-chat" nil t)
(autoload 'mevedel-buddy-abort "mevedel-buddy" nil t)
(autoload 'mevedel-buddy-clear-changes "mevedel-buddy" nil t)
(autoload 'mevedel-buddy-dismiss-note "mevedel-buddy-note" nil t)
(autoload 'mevedel-buddy-dismiss-notes "mevedel-buddy-note" nil t)
(autoload 'mevedel-buddy-global-mode "mevedel-buddy" nil t)
(autoload 'mevedel-buddy-guide "mevedel-buddy" nil t)
(autoload 'mevedel-buddy-mode "mevedel-buddy" nil t)
(autoload 'mevedel-buddy-review "mevedel-buddy" nil t)
(autoload 'mevedel-claude-code-install-adapter "mevedel-claude-code" nil t)
(autoload 'mevedel-claude-code-recover-history "mevedel-claude-code-session" nil t)
(autoload 'mevedel-claude-code-setup "mevedel-claude-code" nil t)
(autoload 'mevedel-clear-patch-buffer "mevedel-chat" nil t)
(autoload 'mevedel-collaboration-status "mevedel-collaboration" nil t)
(autoload 'mevedel-collaboration-stop "mevedel-collaboration" nil t)
(autoload 'mevedel-collaboration-view "mevedel-collaboration" nil t)
(autoload 'mevedel-compact "mevedel-compact" nil t)
(autoload 'mevedel-diff-apply-buffer "mevedel-diff-apply" nil t)
(autoload 'mevedel-gptel-bridge-open "mevedel-gptel-bridge" nil t)
(autoload 'mevedel-hooks-list "mevedel-hooks" nil t)
(autoload 'mevedel-hooks-reload "mevedel-hooks" nil t)
(autoload 'mevedel-hooks-run-dry "mevedel-hooks" nil t)
(autoload 'mevedel-hooks-trust-project "mevedel-hooks" nil t)
(autoload 'mevedel-init "mevedel-init" nil t)
(autoload 'mevedel-inspect-effective-prompt "mevedel-system" nil t)
(autoload 'mevedel-journal-discard "mevedel-journal-jobs" nil t)
(autoload 'mevedel-journal-inspect "mevedel-journal-jobs" nil t)
(autoload 'mevedel-journal-jobs "mevedel-journal-jobs" nil t)
(autoload 'mevedel-journal-retry "mevedel-journal-jobs" nil t)
(autoload 'mevedel-list-archived-directives "mevedel-directive-activity" nil t)
(autoload 'mevedel-list-directives "mevedel-directive-activity" nil t)
(autoload 'mevedel-memory-list-open "mevedel-memory-list" nil t)
(autoload 'mevedel-menu "mevedel-menu" nil t)
(autoload 'mevedel-menu-open "mevedel-menu" nil t)
(autoload 'mevedel-open-directive-activity "mevedel-directive-activity" nil t)
(autoload 'mevedel-pending-inputs-clear "mevedel-pending-inputs" nil t)
(autoload 'mevedel-pending-inputs-edit "mevedel-pending-inputs" nil t)
(autoload 'mevedel-pending-inputs-open "mevedel-pending-inputs" nil t)
(autoload 'mevedel-plan-mode-enter "mevedel-plan-mode" nil t)
(autoload 'mevedel-plan-mode-exit "mevedel-plan-mode" nil t)
(autoload 'mevedel-redo "mevedel-session-rewind" nil t)
(autoload 'mevedel-refresh-session "mevedel-view-control-transfer" nil t)
(autoload 'mevedel-release-control "mevedel-view-control-transfer" nil t)
(autoload 'mevedel-remember "mevedel-memory-list" nil t)
(autoload 'mevedel-rename-session "mevedel-session-naming" nil t)
(autoload 'mevedel-retry-plan-implementation "mevedel-plan-handoff" nil t)
(autoload 'mevedel-retry-target-readiness "mevedel-chat" nil t)
(autoload 'mevedel-review "mevedel-review" nil t)
(autoload 'mevedel-rewind "mevedel-session-rewind" nil t)
(autoload 'mevedel-save-session "mevedel-session-persistence" nil t)
(autoload 'mevedel-session-debug "mevedel-telemetry" nil t)
(autoload 'mevedel-session-publication-abandon "mevedel-session-publication" nil t)
(autoload 'mevedel-session-publication-retry "mevedel-session-publication" nil t)
(autoload 'mevedel-side-conversation-close "mevedel-side-conversation" nil t)
(autoload 'mevedel-skills-rescan "mevedel-skills-core" nil t)
(autoload 'mevedel-subscription-usage-show "mevedel-subscription-usage" nil t)
(autoload 'mevedel-take-control "mevedel-view-control-transfer" nil t)
(autoload 'mevedel-telemetry-profiler-stop "mevedel-telemetry" nil t)
(autoload 'mevedel-toggle-follow "mevedel-view-control-transfer" nil t)
(autoload 'mevedel-verify "mevedel-review" nil t)
(autoload 'mevedel-view--transcript-gptel-send-blocked "mevedel-view-composer" nil t)
(autoload 'mevedel-view-abort "mevedel-view-composer" nil t)
(autoload 'mevedel-view-arm-conversation-fork "mevedel-view-composer" nil t)
(autoload 'mevedel-view-arm-worktree-fork "mevedel-view-composer" nil t)
(autoload 'mevedel-view-back-to-chat "mevedel-view-composer" nil t)
(autoload 'mevedel-view-close-agent-transcript "mevedel-view-agent" nil t)
(autoload 'mevedel-view-cycle-permission-mode "mevedel-view-composer" nil t)
(autoload 'mevedel-view-go-to-segment "mevedel-view-segments" nil t)
(autoload 'mevedel-view-next-display "mevedel-view-render" nil t)
(autoload 'mevedel-view-next-user-query "mevedel-view-render" nil t)
(autoload 'mevedel-view-open-agent-transcript-at-point "mevedel-view-agent" nil t)
(autoload 'mevedel-view-previous-display "mevedel-view-render" nil t)
(autoload 'mevedel-view-previous-user-query "mevedel-view-render" nil t)
(autoload 'mevedel-view-refresh-input-prompt "mevedel-view-composer" nil t)
(autoload 'mevedel-view-render-debug-clear "mevedel-view-render" nil t)
(autoload 'mevedel-view-render-debug-disable "mevedel-view-render" nil t)
(autoload 'mevedel-view-render-debug-enable "mevedel-view-render" nil t)
(autoload 'mevedel-view-render-debug-open "mevedel-view-render" nil t)
(autoload 'mevedel-view-return-to-latest-segment "mevedel-view-segments" nil t)
(autoload 'mevedel-view-rewind-at-point "mevedel-view-render" nil t)
(autoload 'mevedel-view-send "mevedel-view-composer" nil t)
(autoload 'mevedel-view-switch-conversation-variant-at-point "mevedel-view-render" nil t)
(autoload 'mevedel-view-toggle-section "mevedel-view-disclosure" nil t)
(autoload 'mevedel-view-toggle-transcript "mevedel-view-render" nil t)
(autoload 'mevedel-view-yank-dwim "mevedel-view-input-files" nil t)
(autoload 'mevedel-view-zone-next "mevedel-view-zone" nil t)
(autoload 'mevedel-view-zone-previous "mevedel-view-zone" nil t)

;; Integration callbacks load their owners at the corresponding lifecycle.
(autoload 'mevedel--active-chat-buffer "mevedel-chat")
(autoload 'mevedel--attach-directive-skills "mevedel-directive-request")
(autoload 'mevedel--compact-transform-auto "mevedel-compact")
(autoload 'mevedel--define-presets "mevedel-presets")
(autoload 'mevedel--directive-bound-session-buffer "mevedel-directive-request")
(autoload 'mevedel--dispatch-directive-implementation "mevedel-directive-request")
(autoload 'mevedel--display-chat-buffer "mevedel-chat")
(autoload 'mevedel--implement-directive-prompt "mevedel-directive-request")
(autoload 'mevedel--implement-discussion "mevedel-directive-request")
(autoload 'mevedel--implement-discussion-prompt "mevedel-directive-request")
(autoload 'mevedel--normalize-session-directory "mevedel-chat")
(autoload 'mevedel--read-session-directory "mevedel-chat")
(autoload 'mevedel--start-chat "mevedel-chat")
(autoload 'mevedel--start-directive-discussion "mevedel-directive-request")
(autoload 'mevedel--transform-expand-mentions "mevedel-mentions")
(autoload 'mevedel-execution-teardown-all "mevedel-execution")
(autoload 'mevedel-gptel-bridge-install "mevedel-gptel-bridge")
(autoload 'mevedel-gptel-bridge-uninstall "mevedel-gptel-bridge")
(autoload 'mevedel-gptel-stream-bridge-install "mevedel-gptel-stream-bridge")
(autoload 'mevedel-gptel-stream-bridge-uninstall "mevedel-gptel-stream-bridge")
(autoload 'mevedel-init-install-slash-command "mevedel-init")
(autoload 'mevedel-journal-idle-session-opened "mevedel-journal-idle")
(autoload 'mevedel-reminders--transform "mevedel-reminders")
(autoload 'mevedel-review-install-slash-command "mevedel-review")
(autoload 'mevedel-session-persistence-choose-entry "mevedel-session-persistence")
(autoload 'mevedel-shared-conversation-transform "mevedel-shared-conversation")
(autoload 'mevedel-skills--transform-apply-request-model-policy "mevedel-skills-invoke")
(autoload 'mevedel-skills-input-transform-inline-attachments "mevedel-skills-input")
(autoload 'mevedel-skills-install-hot-reload "mevedel-skills-core")
(autoload 'mevedel-skills-install-slash-commands "mevedel-skills-ui")
(autoload 'mevedel-skills-uninstall-hot-reload "mevedel-skills-core")
(autoload 'mevedel-skills-uninstall-slash-commands "mevedel-skills-ui")
(autoload 'mevedel-telemetry--lag-stop "mevedel-telemetry")
(autoload 'mevedel-telemetry-usage-install "mevedel-telemetry-usage")
(autoload 'mevedel-telemetry-usage-uninstall "mevedel-telemetry-usage")
(autoload 'mevedel-tool-exec-handle-execution-event "mevedel-tool-exec")
(autoload 'mevedel-tool-render-data-install-provider-adapter "mevedel-tool-render-data")
(autoload 'mevedel-tool-render-data-uninstall-provider-adapter "mevedel-tool-render-data")
(autoload 'mevedel-tool-repair-install-shape-adapter "mevedel-tool-repair-gptel")
(autoload 'mevedel-tool-repair-uninstall-shape-adapter "mevedel-tool-repair-gptel")
(autoload 'mevedel-tools-register "mevedel-tools")
(autoload 'mevedel-transcript-exclude-directive-turns "mevedel-transcript-audit")
(autoload 'mevedel-transport-install "mevedel-transport")
(autoload 'mevedel-transport-uninstall "mevedel-transport")
(autoload 'mevedel-view--refresh-animation-on-face "mevedel-view-stream")
(autoload 'mevedel-view--resume-attended-views "mevedel-view")
(autoload 'mevedel-view--transform-model-input "mevedel-view-composer")
(autoload 'mevedel-view-enter-directive-scope "mevedel-view-composer")
(autoload 'mevedel-view-stream-handle-execution-event "mevedel-view-stream")
(autoload 'mevedel-worktree-install-slash-command "mevedel-worktree")
(autoload 'mevedel-worktree-uninstall-slash-command "mevedel-worktree")

;; `cl-seq'
(declare-function cl-remove-duplicates "cl-seq" (cl-seq &rest cl-keys))
(declare-function cl-remove-if-not "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `gptel'
(defvar gptel-display-buffer-action)

;; `gptel-request'
(defvar gptel-prompt-transform-functions)

;; `mevedel-chat'
(declare-function mevedel--active-chat-buffer
                  "mevedel-chat" (&optional workspace))
(declare-function mevedel--display-chat-buffer "mevedel-chat" (chat-buffer))
(declare-function mevedel--normalize-session-directory
                  "mevedel-chat" (directory workspace))
(declare-function mevedel--read-session-directory "mevedel-chat" (workspace))
(declare-function mevedel--start-chat
                  "mevedel-chat"
                  (workspace working-directory prompt-session
                             &optional directory-scoped))
(defvar mevedel--view-buffer)

;; `mevedel-collaboration-lobby'
(declare-function mevedel-collaboration-lobby-restore
                  "mevedel-collaboration-lobby" ())
(autoload 'mevedel-collaboration-lobby-restore "mevedel-collaboration-lobby")

;; `mevedel-compact'
(declare-function mevedel--compact-transform-auto
                  "mevedel-compact" (continue fsm))

;; `mevedel-directive'
(declare-function mevedel-directive-actions "mevedel-directive" (directive))

;; `mevedel-directive-request'
(declare-function mevedel--attach-directive-skills
                  "mevedel-directive-request" (prompt record chat-buffer))
(declare-function mevedel--directive-bound-session-buffer
                  "mevedel-directive-request" (record workspace))
(declare-function mevedel--dispatch-directive-implementation
                  "mevedel-directive-request"
                  (directive record action prompt-fn callback))
(declare-function mevedel--implement-directive-prompt "mevedel-directive-request" (content))
(declare-function mevedel--implement-discussion "mevedel-directive-request"
                  (directive &optional callback))
(declare-function mevedel--implement-discussion-prompt "mevedel-directive-request"
                  (content directive))

;; `mevedel-gptel-bridge'
(declare-function mevedel-gptel-bridge-install "mevedel-gptel-bridge" ())
(declare-function mevedel-gptel-bridge-uninstall "mevedel-gptel-bridge" ())

;; `mevedel-gptel-stream-bridge'
(declare-function mevedel-gptel-stream-bridge-install
                  "mevedel-gptel-stream-bridge" ())
(declare-function mevedel-gptel-stream-bridge-uninstall
                  "mevedel-gptel-stream-bridge" ())

;; `mevedel-execution'
(defvar mevedel-execution-mailbox-delivery-function)

;; `mevedel-presets'
(declare-function mevedel--define-presets "mevedel-presets")
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

;; `mevedel-tool-repair-gptel'
(declare-function mevedel-tool-repair-install-shape-adapter
                  "mevedel-tool-repair-gptel" ())
(declare-function mevedel-tool-repair-uninstall-shape-adapter
                  "mevedel-tool-repair-gptel" ())

;; `mevedel-transport'
(declare-function mevedel-transport-install "mevedel-transport" ())
(declare-function mevedel-transport-uninstall "mevedel-transport" ())

;; `mevedel-view'
(declare-function mevedel-view--resume-attended-views "mevedel-view" (&rest _))

;; `mevedel-view-stream'
(declare-function mevedel-view--refresh-animation-on-face
                  "mevedel-view-stream" (face frame &rest attributes))

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
  (if-let* ((directive (mevedel--directive-at-point)))
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
  (if-let* ((directive (mevedel--directive-at-point)))
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
  (if-let* ((directive (mevedel--directive-at-point)))
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
  (if-let* ((directive (mevedel--directive-at-point)))
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
  (if-let* ((directive (mevedel--directive-at-point)))
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
           (if-let* ((directive (mevedel--directive-at-point)))
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
active writer, inert transcript inspection for incompatible sessions, or
confirmed takeover of an expired lease.  With no persisted sessions, retain
the live-buffer behavior: create \"main\", switch to the sole live session, or
prompt among multiple live sessions.

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
            (mevedel-session-persistence-choose-entry workspace))))
    (cond
     ((and (consp entry) (eq (plist-get entry :action) 'inspect))
      (display-buffer (plist-get entry :buffer) gptel-display-buffer-action))
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

(defun mevedel--resume-attended-views (&rest args)
  "Resume views on focus changes with ARGS after the view is loaded."
  (when (featurep 'mevedel-view)
    (apply #'mevedel-view--resume-attended-views args)))

(defun mevedel--refresh-animation-on-face (&rest args)
  "Refresh loaded view animation after face changes with ARGS."
  (when (featurep 'mevedel-view-stream)
    (apply #'mevedel-view--refresh-animation-on-face args)))

;;;###autoload
(defun mevedel-install ()
  "Register `mevedel' presets, tools, and hooks."
  (interactive)

  (mevedel-transport-install)
  (add-function :after after-focus-change-function
                #'mevedel--resume-attended-views)
  (advice-add 'set-face-attribute :after
              #'mevedel--refresh-animation-on-face)

  (add-hook 'mevedel-session-start-hook #'mevedel-journal-idle-session-opened)

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

  ;; Select item history before reminder retention is assessed; discarded room
  ;; turns must not suppress guidance the scoped request has never received.
  (add-hook 'gptel-prompt-transform-functions
            #'mevedel-shared-conversation-transform -92)

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
  (mevedel-tool-render-data-install-provider-adapter)

  ;; Preserve empty object versus null before gptel runs tool hooks.
  (mevedel-tool-repair-install-shape-adapter)

  ;; Install slash-command advice on `gptel-send'
  (mevedel-init-install-slash-command)
  (mevedel-review-install-slash-command)
  (mevedel-worktree-install-slash-command)
  (mevedel-skills-install-slash-commands)

  ;; Install skill hot-reload hooks/watchers for active strategies
  (mevedel-skills-install-hot-reload)

  ;; Install the gptel stream compatibility bridge.
  (mevedel-gptel-stream-bridge-install)
  (mevedel-telemetry-usage-install)
  (mevedel-gptel-bridge-install)

  ;; Restart the collaboration lobbies still running when Emacs last
  ;; exited, once startup has applied the user's relay configuration.
  (if after-init-time
      (mevedel-collaboration-lobby-restore)
    (add-hook 'emacs-startup-hook #'mevedel-collaboration-lobby-restore))

  (message "mevedel installed successfully"))

;;;###autoload
(defun mevedel-uninstall ()
  "Remove `mevedel' hooks and cleanup."
  (interactive)
  (when (featurep 'mevedel-transport)
    (mevedel-transport-uninstall))
  (when (featurep 'mevedel-execution)
    (mevedel-execution-teardown-all))
  ;; Shared periodic callbacks belong to mevedel; their host timer would
  ;; otherwise outlive it.  Stop owners that remember their timer first, so
  ;; a later install starts them again and the threshold is restored.
  (clrhash mevedel--gc-holds)
  (mevedel--gc-maintain)
  (when (featurep 'mevedel-telemetry)
    (mevedel-telemetry--lag-stop))
  (mapc #'mevedel--ui-timer-cancel (copy-sequence mevedel--coalesced-timers))
  (mevedel--ui-timer-cancel mevedel--coalesced-timer)
  (setq mevedel--coalesced-timer nil)
  ;; Remove tools
  (setf (alist-get "mevedel" gptel--known-tools nil 'remove #'equal) nil)
  (remove-hook 'mevedel-execution-event-functions
               #'mevedel-view-stream-handle-execution-event)
  (remove-function after-focus-change-function
                   #'mevedel--resume-attended-views)
  (advice-remove 'set-face-attribute
                 #'mevedel--refresh-animation-on-face)
  (when (eq (bound-and-true-p mevedel-execution-mailbox-delivery-function)
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
  (remove-hook 'gptel-prompt-transform-functions
               #'mevedel-shared-conversation-transform)

  ;; Remove auto-compaction transform
  (remove-hook 'gptel-prompt-transform-functions
               #'mevedel--compact-transform-auto)

  ;; Remove render-data scrubber advice
  (when (featurep 'mevedel-tool-render-data)
    (mevedel-tool-render-data-uninstall-provider-adapter))

  ;; Remove lossless tool-argument shape restoration.
  (when (featurep 'mevedel-tool-repair-gptel)
    (mevedel-tool-repair-uninstall-shape-adapter))

  ;; Remove slash-command advice
  (when (featurep 'mevedel-worktree)
    (mevedel-worktree-uninstall-slash-command))
  (when (featurep 'mevedel-skills-ui)
    (mevedel-skills-uninstall-slash-commands))

  ;; Remove skill hot-reload hooks/watchers and registry state
  (when (featurep 'mevedel-skills-core)
    (mevedel-skills-uninstall-hot-reload))

  ;; Remove the gptel stream compatibility bridge.
  (when (featurep 'mevedel-gptel-stream-bridge)
    (mevedel-gptel-stream-bridge-uninstall))
  (when (featurep 'mevedel-telemetry-usage)
    (mevedel-telemetry-usage-uninstall))
  (when (featurep 'mevedel-gptel-bridge)
    (mevedel-gptel-bridge-uninstall))

  ;; Stop event-loop lag watching and its timer advice.
  (when (featurep 'mevedel-telemetry)
    (mevedel-telemetry--lag-stop))

  (remove-hook 'emacs-startup-hook #'mevedel-collaboration-lobby-restore)

  (message "mevedel uninstalled successfully"))

(provide 'mevedel)

;;; mevedel.el ends here.
