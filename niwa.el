;;; niwa.el --- Instructed LLM programmer/assistant -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; Author: FrauH0lle
;; Version: 0.5.0
;; Keywords: convenience, tools, llm, gptel
;; Package-Requires: ((emacs "31.1") (gptel "0.9.9.6") (acp "0.15.2") (yaml "1.2.0") (orderless "1.1") (websocket "1.15") (qrencode "1.4"))
;; URL: https://github.com/FrauH0lle/niwa.el

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

;; Main entry point for niwa.  Provides the `niwa' command,
;; installation/uninstallation of hooks and presets.
;;
;; Loads foundational data and exposes autoloaded feature entry points.
;; Installation registers the complete tool catalog and integration hooks.

;;; Code:

(require 'niwa-utilities)

(eval-when-compile
  (require 'cl-lib))

(require 'gptel)

(require 'niwa-workspace)
(require 'niwa-structs)
(require 'niwa-instruction-registry)
(require 'niwa-overlay-ui)
(require 'niwa-persistence)
(require 'niwa-models)
(require 'niwa-claude-code-backend)

(defconst niwa--load-provenance
  (let ((niwa (niwa-library-provenance
                  (or load-file-name buffer-file-name)))
        (gptel (niwa-library-provenance (symbol-file 'gptel 'provide))))
    (list :loaded-at (format-time-string "%FT%T%z")
          :niwa-commit (plist-get niwa :commit)
          :niwa-version (plist-get niwa :version)
          :gptel-commit (plist-get gptel :commit)
          :gptel-version (plist-get gptel :version)))
  "When this Emacs loaded `niwa', and what it loaded of it and gptel.
A running Emacs keeps executing the code it loaded while its checkout moves
on; session telemetry records this so diagnoses compare the right revision.
See `niwa-library-provenance' for how installation methods are covered.")

(niwa-claude-code-register)

;; Top-level commands stay reachable through M-x as soon as the package is
;; loaded, also from a source checkout without generated autoloads.  Mode-local
;; and transient suffix commands load with the owner that binds them.
(autoload 'niwa-abort "niwa-chat" nil t)
(autoload 'niwa-artifacts "niwa-artifacts-list" nil t)
(autoload 'niwa-artifacts-new-shared-item "niwa-artifacts-list" nil t)
(autoload 'niwa-buddy-abort "niwa-buddy" nil t)
(autoload 'niwa-buddy-clear-changes "niwa-buddy" nil t)
(autoload 'niwa-buddy-dismiss-note "niwa-buddy-note" nil t)
(autoload 'niwa-buddy-keep-note "niwa-buddy-note" nil t)
(autoload 'niwa-buddy-dismiss-notes "niwa-buddy-note" nil t)
(autoload 'niwa-buddy-global-mode "niwa-buddy" nil t)
(autoload 'niwa-buddy-guide "niwa-buddy" nil t)
(autoload 'niwa-buddy-mode "niwa-buddy" nil t)
(autoload 'niwa-buddy-review "niwa-buddy" nil t)
(autoload 'niwa-buddy-set-level "niwa-buddy" nil t)
(autoload 'niwa-note-take "niwa-notes" nil t)
(autoload 'niwa-note-drop "niwa-notes" nil t)
(autoload 'niwa-claude-code-install-adapter "niwa-claude-code" nil t)
(autoload 'niwa-claude-code-recover-history "niwa-claude-code-session" nil t)
(autoload 'niwa-claude-code-setup "niwa-claude-code" nil t)
(autoload 'niwa-clear-patch-buffer "niwa-chat" nil t)
(autoload 'niwa-collaboration-revoke-artifact "niwa-collaboration-artifact-room" nil t)
(autoload 'niwa-collaboration-rotate-artifact "niwa-collaboration-artifact-room" nil t)
(autoload 'niwa-collaboration-share-artifact "niwa-collaboration-artifact-room" nil t)
(autoload 'niwa-remote-artifact-attach "niwa-remote-artifact" nil t)
(autoload 'niwa-remote-artifact-detach "niwa-remote-artifact" nil t)
(autoload 'niwa-collaboration-status "niwa-collaboration" nil t)
(autoload 'niwa-collaboration-stop "niwa-collaboration" nil t)
(autoload 'niwa-collaboration-stop-all "niwa-collaboration" nil t)
(autoload 'niwa-collaboration-view "niwa-collaboration" nil t)
(autoload 'niwa-compact "niwa-compact" nil t)
(autoload 'niwa-delete-all-instructions "niwa-overlays" nil t)
(autoload 'niwa-diff-apply-buffer "niwa-diff-apply" nil t)
(autoload 'niwa-evil-setup "niwa-evil" nil t)
(autoload 'niwa-gptel-bridge-open "niwa-gptel-bridge" nil t)
(autoload 'niwa-hooks-list "niwa-hooks" nil t)
(autoload 'niwa-hooks-reload "niwa-hooks" nil t)
(autoload 'niwa-hooks-run-dry "niwa-hooks" nil t)
(autoload 'niwa-hooks-trust-project "niwa-hooks" nil t)
(autoload 'niwa-init "niwa-init" nil t)
(autoload 'niwa-inspect-effective-prompt "niwa-system" nil t)
(autoload 'niwa-journal-discard "niwa-journal-jobs" nil t)
(autoload 'niwa-journal-inspect "niwa-journal-jobs" nil t)
(autoload 'niwa-journal-jobs "niwa-journal-jobs" nil t)
(autoload 'niwa-journal-retry "niwa-journal-jobs" nil t)
(autoload 'niwa-memory-list-open "niwa-memory-list" nil t)
(autoload 'niwa-schedules-list-open "niwa-schedules-list" nil t)
(autoload 'niwa-note-apply-change "niwa-note-change" nil t)
(autoload 'niwa-note-create "niwa-notes" nil t)
(autoload 'niwa-note-delete "niwa-notes" nil t)
(autoload 'niwa-note-dismiss "niwa-notes" nil t)
(autoload 'niwa-note-dismiss-agent-notes "niwa-notes" nil t)
(autoload 'niwa-note-done "niwa-notes" nil t)
(autoload 'niwa-note-edit "niwa-notes" nil t)
(autoload 'niwa-note-menu "niwa-notes" nil t)
(autoload 'niwa-note-merge-change "niwa-note-merge" nil t)
(autoload 'niwa-note-preview-prompt "niwa-note-conversation" nil t)
(autoload 'niwa-note-settings "niwa-note-settings" nil t)
(autoload 'niwa-note-send "niwa-note-conversation" nil t)
(autoload 'niwa-note-send-batch "niwa-note-conversation" nil t)
(autoload 'niwa-note-restore "niwa-notes" nil t)
(autoload 'niwa-note-revert-change "niwa-note-change" nil t)
(autoload 'niwa-note-set-tags "niwa-notes" nil t)
(autoload 'niwa-notes-list "niwa-notes-list" nil t)
(autoload 'niwa-notes-toggle-labels "niwa-notes" nil t)
(autoload 'niwa-menu "niwa-menu" nil t)
(autoload 'niwa-menu-open "niwa-menu" nil t)
(autoload 'niwa-pending-inputs-clear "niwa-pending-inputs" nil t)
(autoload 'niwa-pending-inputs-drop-at-point "niwa-pending-inputs" nil t)
(autoload 'niwa-pending-inputs-edit "niwa-pending-inputs" nil t)
(autoload 'niwa-pending-inputs-open "niwa-pending-inputs" nil t)
(autoload 'niwa-view-resume-pending "niwa-pending-inputs" nil t)
(autoload 'niwa-view-retract-pending "niwa-pending-inputs" nil t)
(autoload 'niwa-plan-mode-enter "niwa-plan-mode" nil t)
(autoload 'niwa-plan-mode-exit "niwa-plan-mode" nil t)
(autoload 'niwa-redo "niwa-session-rewind" nil t)
(autoload 'niwa-refresh-session "niwa-view-control-transfer" nil t)
(autoload 'niwa-release-control "niwa-view-control-transfer" nil t)
(autoload 'niwa-remember "niwa-memory-list" nil t)
(autoload 'niwa-remember-drain "niwa-memory-list" nil t)
(autoload 'niwa-rename-session "niwa-session-naming" nil t)
(autoload 'niwa-retry-session-naming "niwa-session-naming" nil t)
(autoload 'niwa-retry-plan-implementation "niwa-plan-handoff" nil t)
(autoload 'niwa-retry-target-readiness "niwa-chat" nil t)
(autoload 'niwa-review "niwa-review" nil t)
(autoload 'niwa-rewind "niwa-session-rewind" nil t)
(autoload 'niwa-save-session "niwa-session-persistence" nil t)
(autoload 'niwa-session-debug "niwa-telemetry" nil t)
(autoload 'niwa-session-publication-abandon "niwa-session-publication" nil t)
(autoload 'niwa-session-publication-retry "niwa-session-publication" nil t)
(autoload 'niwa-side-conversation-close "niwa-side-conversation" nil t)
(autoload 'niwa-skills-rescan "niwa-skills-core" nil t)
(autoload 'niwa-subscription-usage-show "niwa-subscription-usage" nil t)
(autoload 'niwa-take-control "niwa-view-control-transfer" nil t)
(autoload 'niwa-telemetry-profiler-stop "niwa-telemetry" nil t)
(autoload 'niwa-toggle-follow "niwa-view-control-transfer" nil t)
(autoload 'niwa-verify "niwa-review" nil t)
(autoload 'niwa-view--transcript-gptel-send-blocked "niwa-view-composer" nil t)
(autoload 'niwa-view-abort "niwa-view-composer" nil t)
(autoload 'niwa-view-arm-conversation-fork "niwa-view-composer" nil t)
(autoload 'niwa-view-arm-worktree-fork "niwa-view-composer" nil t)

(autoload 'niwa-view-close-agent-transcript "niwa-view-agent" nil t)
(autoload 'niwa-view-cycle-permission-mode "niwa-view-composer" nil t)
(autoload 'niwa-view-go-to-chat "niwa-view-segments" nil t)
(autoload 'niwa-view-go-to-segment "niwa-view-segments" nil t)
(autoload 'niwa-native-install "niwa-native" nil t)
(autoload 'niwa-view-next-display "niwa-view-render" nil t)
(autoload 'niwa-view-next-user-query "niwa-view-render" nil t)
(autoload 'niwa-view-open-agent-transcript-at-point "niwa-view-agent" nil t)
(autoload 'niwa-view-previous-display "niwa-view-render" nil t)
(autoload 'niwa-view-previous-user-query "niwa-view-render" nil t)
(autoload 'niwa-view-refresh-input-prompt "niwa-view-composer" nil t)
(autoload 'niwa-view-render-debug-clear "niwa-view-render" nil t)
(autoload 'niwa-view-render-debug-disable "niwa-view-render" nil t)
(autoload 'niwa-view-render-debug-enable "niwa-view-render" nil t)
(autoload 'niwa-view-render-debug-open "niwa-view-render" nil t)
(autoload 'niwa-view-return-to-latest-segment "niwa-view-segments" nil t)
(autoload 'niwa-view-rewind-at-point "niwa-view-render" nil t)
(autoload 'niwa-view-send "niwa-view-composer" nil t)
(autoload 'niwa-view-switch-conversation-variant-at-point "niwa-view-render" nil t)
(autoload 'niwa-view-toggle-section "niwa-view-disclosure" nil t)
(autoload 'niwa-view-toggle-transcript "niwa-view-render" nil t)
(autoload 'niwa-view-yank-dwim "niwa-view-input-files" nil t)
(autoload 'niwa-view-zone-next "niwa-view-zone" nil t)
(autoload 'niwa-view-zone-previous "niwa-view-zone" nil t)

;; A host asks this before restarting its Emacs.
(autoload 'niwa-restart-unsafe-p "niwa-turn")

;; Integration callbacks load their owners at the corresponding lifecycle.
(autoload 'niwa--active-chat-buffer "niwa-chat")

(autoload 'niwa--compact-transform-auto "niwa-compact")
(autoload 'niwa--define-presets "niwa-presets")


(autoload 'niwa--display-chat-buffer "niwa-chat")


(autoload 'niwa--normalize-session-directory "niwa-chat")
(autoload 'niwa--read-session-directory "niwa-chat")
(autoload 'niwa--start-chat "niwa-chat")

(autoload 'niwa--transform-expand-mentions "niwa-mentions")
;; The bundled artifact skill resolves the store path while it is invoked.
(autoload 'niwa-artifact-store-directory "niwa-artifact-store")
(autoload 'niwa-execution-teardown-all "niwa-execution")
(autoload 'niwa-gptel-bridge-install "niwa-gptel-bridge")
(autoload 'niwa-gptel-bridge-uninstall "niwa-gptel-bridge")
(autoload 'niwa-gptel-retry-install "niwa-gptel-retry")
(autoload 'niwa-gptel-retry-uninstall "niwa-gptel-retry")
(autoload 'niwa-gptel-stream-bridge-install "niwa-gptel-stream-bridge")
(autoload 'niwa-gptel-stream-bridge-uninstall "niwa-gptel-stream-bridge")
(autoload 'niwa-init-install-slash-command "niwa-init")
(autoload 'niwa-journal-idle-session-opened "niwa-journal-idle")
(autoload 'niwa-reminders--transform "niwa-reminders")
(autoload 'niwa-review-install-slash-command "niwa-review")
(autoload 'niwa-session-persistence-choose-entry "niwa-session-persistence")
(autoload 'niwa-shared-conversation-transform "niwa-shared-conversation")
(autoload 'niwa-skills--transform-apply-request-model-policy "niwa-skills-invoke")
(autoload 'niwa-skills-input-transform-inline-attachments "niwa-skills-input")
(autoload 'niwa-skills-install-hot-reload "niwa-skills-core")
(autoload 'niwa-skills-install-slash-commands "niwa-skills-ui")
(autoload 'niwa-skills-uninstall-hot-reload "niwa-skills-core")
(autoload 'niwa-skills-uninstall-slash-commands "niwa-skills-ui")
(autoload 'niwa-sleep-update "niwa-sleep")
(autoload 'niwa-telemetry--lag-stop "niwa-telemetry")
(autoload 'niwa-telemetry-usage-install "niwa-telemetry-usage")
(autoload 'niwa-telemetry-usage-uninstall "niwa-telemetry-usage")
(autoload 'niwa-tool-exec-handle-execution-event "niwa-tool-exec")
(autoload 'niwa-tool-render-data-install-provider-adapter "niwa-tool-render-data")
(autoload 'niwa-tool-render-data-uninstall-provider-adapter "niwa-tool-render-data")
(autoload 'niwa-tool-repair-install-shape-adapter "niwa-tool-repair-gptel")
(autoload 'niwa-tool-repair-uninstall-shape-adapter "niwa-tool-repair-gptel")
(autoload 'niwa-tools-register "niwa-tools")
(autoload 'niwa-transport-install "niwa-transport")
(autoload 'niwa-transport-uninstall "niwa-transport")
(autoload 'niwa-view--refresh-animation-on-face "niwa-view-stream")
(autoload 'niwa-view--resume-attended-views "niwa-view")
(autoload 'niwa-view--transform-model-input "niwa-view-composer")

(autoload 'niwa-view-stream-handle-execution-event "niwa-view-stream")
(autoload 'niwa-worktree-install-slash-command "niwa-worktree")
(autoload 'niwa-worktree-uninstall-slash-command "niwa-worktree")

;; `cl-seq'
(declare-function cl-remove-duplicates "cl-seq" (cl-seq &rest cl-keys))
(declare-function cl-remove-if-not "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `gptel'
(defvar gptel-display-buffer-action)

;; `gptel-request'
(defvar gptel-prompt-transform-functions)

;; `niwa-chat'
(declare-function niwa--active-chat-buffer
                  "niwa-chat" (&optional workspace))
(declare-function niwa--display-chat-buffer "niwa-chat" (chat-buffer))
(declare-function niwa--normalize-session-directory
                  "niwa-chat" (directory workspace))
(declare-function niwa--read-session-directory "niwa-chat" (workspace))
(declare-function niwa--start-chat
                  "niwa-chat"
                  (workspace working-directory prompt-session
                             &optional directory-scoped))
(defvar niwa--view-buffer)

;; `niwa-collaboration-artifact-room'
;; An Emacs daemon shares, revokes and rotates an artifact through emacsclient.
(declare-function niwa-collaboration-artifact-room-restore
                  "niwa-collaboration-artifact-room" ())
(autoload 'niwa-collaboration-artifact-room-restore
  "niwa-collaboration-artifact-room")
(autoload 'niwa-collaboration-artifact-room-revoke
  "niwa-collaboration-artifact-room")
(autoload 'niwa-collaboration-artifact-room-rotate
  "niwa-collaboration-artifact-room")
(autoload 'niwa-collaboration-artifact-room-share
  "niwa-collaboration-artifact-room")

;; `niwa-collaboration-lobby'
(declare-function niwa-collaboration-lobby-restore
                  "niwa-collaboration-lobby" ())
(autoload 'niwa-collaboration-lobby-restore "niwa-collaboration-lobby")

;; `niwa-compact'
(declare-function niwa--compact-transform-auto
                  "niwa-compact" (continue fsm))

;; `niwa-gptel-bridge'
(declare-function niwa-gptel-bridge-install "niwa-gptel-bridge" ())
(declare-function niwa-gptel-bridge-uninstall "niwa-gptel-bridge" ())

;; `niwa-gptel-retry'
(declare-function niwa-gptel-retry-install "niwa-gptel-retry" ())
(declare-function niwa-gptel-retry-uninstall "niwa-gptel-retry" ())

;; `niwa-gptel-stream-bridge'
(declare-function niwa-gptel-stream-bridge-install
                  "niwa-gptel-stream-bridge" ())
(declare-function niwa-gptel-stream-bridge-uninstall
                  "niwa-gptel-stream-bridge" ())

;; `niwa-execution'
(defvar niwa-execution-mailbox-delivery-function)

;; `niwa-presets'
(declare-function niwa--define-presets "niwa-presets")
(defvar niwa-action-preset-alist)

;; `niwa-schedule'
(declare-function niwa-schedule-restore "niwa-schedule" ())
(autoload 'niwa-schedule-restore "niwa-schedule")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-choose-entry
                  "niwa-session-persistence" (workspace))

;; `niwa-skills-core'
(declare-function niwa-skills-install-hot-reload
                  "niwa-skills-core" ())
(declare-function niwa-skills-uninstall-hot-reload
                  "niwa-skills-core" ())

;; `niwa-skills-input'
(declare-function niwa-skills-input-transform-inline-attachments
                  "niwa-skills-input" (fsm))

;; `niwa-skills-invoke'
(declare-function niwa-skills--transform-apply-request-model-policy
                  "niwa-skills-invoke" (fsm))

;; `niwa-sleep'
(declare-function niwa-sleep-stop "niwa-sleep" ())

;; `niwa-structs'
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-install-provider-adapter
                  "niwa-tool-render-data" ())
(declare-function niwa-tool-render-data-uninstall-provider-adapter
                  "niwa-tool-render-data" ())

;; `niwa-tool-repair-gptel'
(declare-function niwa-tool-repair-install-shape-adapter
                  "niwa-tool-repair-gptel" ())
(declare-function niwa-tool-repair-uninstall-shape-adapter
                  "niwa-tool-repair-gptel" ())

;; `niwa-transport'
(declare-function niwa-transport-install "niwa-transport" ())
(declare-function niwa-transport-uninstall "niwa-transport" ())

;; `niwa-view'
(declare-function niwa-view--resume-attended-views "niwa-view" (&rest _))

;; `niwa-view-stream'
(declare-function niwa-view--refresh-animation-on-face
                  "niwa-view-stream" (face frame &rest attributes))

;; `niwa-worktree'
(declare-function niwa-worktree-install-slash-command "niwa-worktree" ())
(declare-function niwa-worktree-uninstall-slash-command
                  "niwa-worktree" ())

(defgroup niwa nil
  "Customization group for Evedel."
  :group 'tools)

(defcustom niwa-ov-dispatch-key "M-m"
  "Keybind to open overlay actions.
If nil, no keybinding is set for dispatch actions."
  :group 'niwa
  :type '(choice (const :tag "No keybinding" nil)
          (string :tag "Key sequence"))
  :set (lambda (sym new-val)
         (let ((old-val (and (boundp sym) (symbol-value sym))))
           ;; Remove old binding if there was one and keymap exists
           (dolist (map niwa--actions-maps)
             (when (and old-val (boundp map))
               (keymap-set (symbol-value map) old-val nil)))

           ;; Set the new value
           (set sym new-val)
           ;; Add new binding if new value is non-nil and keymap exists
           (dolist (map niwa--actions-maps)
             (when (and new-val (boundp map))
               (keymap-set (symbol-value map) new-val #'niwa--ov-actions-dispatch))))))

(defcustom niwa-default-chat-preset 'implement
  "Default action preset for the chat buffer from `niwa' command."
  :group 'niwa
  :type 'symbol)


;;
;;; Commands

;;;###autoload
(defun niwa-instruction-count ()
  "Return the number of instructions currently loaded instructions.

If called interactively, it messages the number of instructions and
buffers."
  (interactive)
  (let ((count 0)
        (buffer-hash (make-hash-table :test 'eq)))
    (niwa--foreach-instruction instr count instr into instr-count
                                  do (puthash (overlay-buffer instr) t buffer-hash)
                                  finally (setf count instr-count))
    (let ((buffers (hash-table-count buffer-hash)))
      (when (called-interactively-p 'interactive)
        (if (= count 0)
            (message "No niwa instructions currently loaded")
          (message "niwa is showing %d instruction%s from %d buffer%s"
                   count (if (/= count 1) "s" "")
                   buffers (if (/= buffers 1) "s" ""))))
      count)))

;;;###autoload
(defun niwa (&optional arg)
  "Start or switch to a chat session in the current project.

Without prefix ARG, discover persisted workspace sessions first.  The entry
chooser offers a new session, ordinary resume, read-only inspection of an
active writer, inert transcript inspection for incompatible sessions, or
confirmed takeover of an expired lease.  Recovery-blocked sessions offer
Recover, Discard, and Cancel.  With no persisted sessions, retain
the live-buffer behavior: create \"main\", switch to the sole live session, or
prompt among multiple live sessions.

With prefix ARG (\\[universal-argument]):
- Prompt for a working directory under the current project.
- Prompt for a session in that directory, allowing selection of an
  existing session or creation of a new one by typing a new name."
  (interactive "P")
  (let ((workspace (niwa-workspace)))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
				    (let* ((working-directory (if arg
								  (niwa--read-session-directory workspace)
								(niwa-workspace-root workspace)))
					   (entry
					    (unless arg
					      (niwa-session-persistence-choose-entry workspace))))
				      (cond
				       ((and (consp entry) (eq (plist-get entry :action) 'inspect))
					(display-buffer (plist-get entry :buffer) gptel-display-buffer-action))
				       ((bufferp entry)
					(niwa--display-chat-buffer entry))
				       ((eq entry 'new)
					(niwa--start-chat workspace working-directory t nil))
				       (t
					(niwa--start-chat workspace working-directory arg arg)))))))

;;;###autoload
(defun niwa-in-directory (directory &optional arg)
  "Start or switch to a chat session whose working directory is DIRECTORY.

DIRECTORY must be inside the current workspace root.  With prefix ARG,
always prompt for the session name."
  (interactive
   (let* ((workspace (niwa-workspace))
          (directory (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
						     (niwa--read-session-directory workspace))))
     (list directory current-prefix-arg)))
  (let ((workspace (niwa-workspace)))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
				    (let ((working-directory
					   (niwa--normalize-session-directory directory workspace)))
				      (niwa--start-chat workspace working-directory arg t)))))

;;
;;; Installation

(defun niwa--resume-attended-views (&rest args)
  "Resume views on focus changes with ARGS after the view is loaded."
  (when (featurep 'niwa-view)
    (apply #'niwa-view--resume-attended-views args)))

(defun niwa--refresh-animation-on-face (&rest args)
  "Refresh loaded view animation after face changes with ARGS."
  (when (featurep 'niwa-view-stream)
    (apply #'niwa-view--refresh-animation-on-face args)))

(defun niwa--restore-schedules-when-idle ()
  "Restore saved wake-ups and workspace schedules once Emacs is idle.
Restore reads every indexed session's store, a round trip each on a remote
target, so it stays off the startup path."
  (cancel-function-timers #'niwa-schedule-restore)
  (run-with-idle-timer 0 nil #'niwa-schedule-restore))

;;;###autoload
(defun niwa-install ()
  "Register `niwa' presets, tools, and hooks."
  (interactive)

  (niwa-transport-install)
  (add-function :after after-focus-change-function
                #'niwa--resume-attended-views)
  (advice-add 'set-face-attribute :after
              #'niwa--refresh-animation-on-face)

  (add-hook 'niwa-session-start-hook #'niwa-journal-idle-session-opened)

  ;; Define custom tools
  (niwa-tools-register)

  ;; Reflect managed Bash progress in the view and secure independent
  ;; completions in the original owner's mailbox.
  (add-hook 'niwa-execution-event-functions
            #'niwa-view-stream-handle-execution-event)
  (setq niwa-execution-mailbox-delivery-function
        #'niwa-tool-exec-handle-execution-event)
  ;; A live command keeps the machine awake.
  (add-hook 'niwa-execution-state-change-hook #'niwa-sleep-update)

  ;; Define gptel presets
  (niwa--define-presets)

  ;; Apply the root request's workload and skill overrides before
  ;; compaction so the threshold uses the effective context window.
  ;; This only mutates prompt-buffer locals, so no prompt text is lost
  ;; if compaction rebuilds the prompt buffer next.
  (add-hook 'gptel-prompt-transform-functions
            #'niwa-skills--transform-apply-request-model-policy -100)

  ;; Select item history before reminder retention is assessed; discarded room
  ;; turns must not suppress guidance the scoped request has never received.
  (add-hook 'gptel-prompt-transform-functions
            #'niwa-shared-conversation-transform -92)

  ;; Substitute view-derived text only in gptel's temporary request buffer.
  (add-hook 'gptel-prompt-transform-functions
            #'niwa-view--transform-model-input -91)

  ;; Expand @ref/@file mentions early in the gptel transform chain
  (add-hook 'gptel-prompt-transform-functions #'niwa--transform-expand-mentions -90)

  ;; Bare gptel buffers use their inline-attachment path.  Paired
  ;; niwa views prepare complete plans before `gptel-send', so their stash
  ;; is empty and this transform is a no-op.
  (add-hook 'gptel-prompt-transform-functions
            #'niwa-skills-input-transform-inline-attachments -89)

  ;; Inject system reminders after mention expansion but before the request fires
  (add-hook 'gptel-prompt-transform-functions #'niwa-reminders--transform -80)

  ;; Auto-compact after niwa's synchronous prompt transforms so an
  ;; auto-compact send preserves the transformed pending prompt when it
  ;; rebuilds the temporary request buffer.
  (add-hook 'gptel-prompt-transform-functions
            #'niwa--compact-transform-auto -70)

  ;; Strip render-data side-channel blocks on the LLM path only.  The
  ;; advice on `gptel--parse-tool-results' (the single chokepoint where
  ;; `:result' strings become API-shaped tool_result messages) catches
  ;; both tool-follow-up and user-initiated request paths while leaving
  ;; the chat-buffer display / view parser / persistence untouched.
  (niwa-tool-render-data-install-provider-adapter)

  ;; Preserve empty object versus null before gptel runs tool hooks.
  (niwa-tool-repair-install-shape-adapter)

  ;; Install slash-command advice on `gptel-send'
  (niwa-init-install-slash-command)
  (niwa-review-install-slash-command)
  (niwa-worktree-install-slash-command)
  (niwa-skills-install-slash-commands)

  ;; Install skill hot-reload hooks/watchers for active strategies
  (niwa-skills-install-hot-reload)

  ;; Install the gptel stream compatibility bridge.
  (niwa-gptel-stream-bridge-install)
  (niwa-telemetry-usage-install)
  (niwa-gptel-retry-install)
  (niwa-gptel-bridge-install)

  ;; Restart the collaboration lobbies still running and the artifacts
  ;; still shared when Emacs last exited, once startup has applied the
  ;; user's relay configuration, and re-arm the pending wake-ups of saved
  ;; sessions once Emacs is idle.
  (if after-init-time
      (progn
        (niwa-collaboration-lobby-restore)
        (niwa-collaboration-artifact-room-restore)
        (niwa--restore-schedules-when-idle))
    (add-hook 'emacs-startup-hook #'niwa-collaboration-lobby-restore)
    (add-hook 'emacs-startup-hook #'niwa-collaboration-artifact-room-restore 90)
    (add-hook 'emacs-startup-hook #'niwa--restore-schedules-when-idle 90))

  (message "niwa installed successfully"))

;;;###autoload
(defun niwa-uninstall ()
  "Remove `niwa' hooks and cleanup."
  (interactive)
  (when (featurep 'niwa-transport)
    (niwa-transport-uninstall))
  (when (featurep 'niwa-execution)
    (niwa-execution-teardown-all))
  (remove-hook 'niwa-execution-state-change-hook #'niwa-sleep-update)
  (when (featurep 'niwa-sleep)
    (niwa-sleep-stop))
  ;; Shared periodic callbacks belong to niwa; their host timer would
  ;; otherwise outlive it.  Stop owners that remember their timer first, so
  ;; a later install starts them again and the threshold is restored.
  (clrhash niwa--gc-holds)
  (niwa--gc-maintain)
  (when (featurep 'niwa-telemetry)
    (niwa-telemetry--lag-stop))
  (mapc #'niwa--ui-timer-cancel (copy-sequence niwa--coalesced-timers))
  (niwa--ui-timer-cancel niwa--coalesced-timer)
  (setq niwa--coalesced-timer nil)
  ;; Remove tools
  (setf (alist-get "niwa" gptel--known-tools nil 'remove #'equal) nil)
  (remove-hook 'niwa-execution-event-functions
               #'niwa-view-stream-handle-execution-event)
  (remove-function after-focus-change-function
                   #'niwa--resume-attended-views)
  (advice-remove 'set-face-attribute
                 #'niwa--refresh-animation-on-face)
  (when (eq (bound-and-true-p niwa-execution-mailbox-delivery-function)
            #'niwa-tool-exec-handle-execution-event)
    (setq niwa-execution-mailbox-delivery-function nil))
  ;; Remove presets
  (dolist (preset '(niwa-implement))
    (setf (alist-get preset gptel--known-presets nil 'remove) nil))

  ;; Remove mention expansion from gptel
  (remove-hook 'gptel-prompt-transform-functions #'niwa--transform-expand-mentions)

  ;; Remove inline skill attachment expansion from gptel
  (remove-hook 'gptel-prompt-transform-functions
               #'niwa-skills-input-transform-inline-attachments)

  ;; Remove root request model policy transform
  (remove-hook 'gptel-prompt-transform-functions
               #'niwa-skills--transform-apply-request-model-policy)

  ;; Remove view request-input substitution
  (remove-hook 'gptel-prompt-transform-functions
               #'niwa-view--transform-model-input)

  ;; Remove reminder injection
  (remove-hook 'gptel-prompt-transform-functions #'niwa-reminders--transform)

  (remove-hook 'gptel-prompt-transform-functions
               #'niwa-shared-conversation-transform)

  ;; Remove auto-compaction transform
  (remove-hook 'gptel-prompt-transform-functions
               #'niwa--compact-transform-auto)

  ;; Remove render-data scrubber advice
  (when (featurep 'niwa-tool-render-data)
    (niwa-tool-render-data-uninstall-provider-adapter))

  ;; Remove lossless tool-argument shape restoration.
  (when (featurep 'niwa-tool-repair-gptel)
    (niwa-tool-repair-uninstall-shape-adapter))

  ;; Remove slash-command advice
  (when (featurep 'niwa-worktree)
    (niwa-worktree-uninstall-slash-command))
  (when (featurep 'niwa-skills-ui)
    (niwa-skills-uninstall-slash-commands))

  ;; Remove skill hot-reload hooks/watchers and registry state
  (when (featurep 'niwa-skills-core)
    (niwa-skills-uninstall-hot-reload))

  ;; Remove the gptel stream compatibility bridge.
  (when (featurep 'niwa-gptel-retry)
    (niwa-gptel-retry-uninstall))
  (when (featurep 'niwa-gptel-stream-bridge)
    (niwa-gptel-stream-bridge-uninstall))
  (when (featurep 'niwa-telemetry-usage)
    (niwa-telemetry-usage-uninstall))
  (when (featurep 'niwa-gptel-bridge)
    (niwa-gptel-bridge-uninstall))

  ;; Stop event-loop lag watching and its timer advice.
  (when (featurep 'niwa-telemetry)
    (niwa-telemetry--lag-stop))

  (remove-hook 'emacs-startup-hook #'niwa-collaboration-lobby-restore)
  (remove-hook 'emacs-startup-hook #'niwa-collaboration-artifact-room-restore)
  (remove-hook 'emacs-startup-hook #'niwa--restore-schedules-when-idle)
  (cancel-function-timers #'niwa-schedule-restore)

  (message "niwa uninstalled successfully"))

(provide 'niwa)

;;; niwa.el ends here.
