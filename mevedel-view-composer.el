;;; mevedel-view-composer.el -- View composer and send orchestration -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the editable composer, prompt submission, queued follow-ups, and
;; dispatch into the authoritative gptel data buffer.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `browse-url'
(declare-function browse-url "browse-url" (url &optional new-window))

;; `cl-seq'
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-position "cl-seq" (cl-item cl-seq &rest cl-keys))

;; `dnd'
(declare-function dnd-get-local-file-name "dnd"
		  (uri &optional must-exist))
(defvar dnd-protocol-alist)

;; `gptel'
(declare-function gptel--update-status "ext:gptel"
		  (msg &optional face))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)
(declare-function gptel-send "ext:gptel" (&optional arg))
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-root-waiting-p
		  "mevedel-agent-control" (session))
(declare-function mevedel-agent-control-wake-root-user
		  "mevedel-agent-control" (session))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))

;; `mevedel-chat'
(declare-function mevedel--directive-action-label "mevedel-chat" (action))
(declare-function mevedel--directive-session-buffer
                  "mevedel-chat" (directive workspace))
(declare-function mevedel--discuss-directive-turn
                  "mevedel-chat"
                  (directive message &optional attempt-index callback))
(declare-function mevedel--request-directive-changes
                  "mevedel-chat" (directive feedback &optional callback))
(declare-function mevedel--retry-directive
                  "mevedel-chat" (directive guidance &optional callback))
(declare-function mevedel-abort "mevedel-chat" (&optional buf))
(defvar mevedel--pending-model-input)

;; `mevedel-directive'
(declare-function mevedel-directive-actions "mevedel-directive" (directive))

;; `mevedel-goal'
(declare-function mevedel-goal-start "mevedel-goal"
		  (objective &optional prompt-submission))

;; `mevedel-hooks'
(declare-function mevedel-hooks-additional-context-string
		  "mevedel-hooks" (decision &optional event))
(declare-function mevedel-hooks-event-plist "mevedel-hooks"
		  (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-format-context "mevedel-hooks"
		  (entries))
(declare-function mevedel-hooks-record-session-context "mevedel-hooks"
		  (session decision &optional event))
(declare-function mevedel-hooks-run-event "mevedel-hooks"
		  (event event-plist callback &optional session
			 workspace request invocation))

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-copy-text
		  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-invalidate-edit
		  "mevedel-mention-bindings"
		  (start end minimum maximum))
(declare-function mevedel-mention-bindings-ranges
		  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-set
		  "mevedel-mention-bindings"
		  (start end binding &optional object))

;; `mevedel-mentions'
(declare-function mevedel-mentions--commit-expansion
		  "mevedel-mentions" (session expansion))
(declare-function mevedel-mentions-expand-user-input
		  "mevedel-mentions" (text session))
(declare-function mevedel-mentions-file-paths-in-text
                  "mevedel-mentions" (text))
(declare-function mevedel-mentions-file-token "mevedel-mentions"
		  (path))
(declare-function mevedel-mentions-install "mevedel-mentions" nil)
(declare-function mevedel-mentions-prepare-user-input
		  "mevedel-mentions" (text &optional session))
(defvar mevedel-mentions--agent-enabled-p)

;; `mevedel-menu'
(declare-function mevedel-menu "mevedel-menu" nil)

;; `mevedel-overlays'
(declare-function mevedel--directive-action-context
                  "mevedel-overlays" (record workspace))
(declare-function mevedel--directive-record
                  "mevedel-overlays" (directive))
(declare-function mevedel--instruction-buffer-workspace
                  "mevedel-overlays" (buffer))
(declare-function mevedel--topmost-instruction
                  "mevedel-overlays" (instruction type))

;; `mevedel-permissions'
(declare-function mevedel-permission-mode-effective
		  "mevedel-permissions"
		  (&optional session data-buffer surface-buffer))
(declare-function mevedel-permission-mode-label "mevedel-permissions"
		  (&optional mode))
(declare-function mevedel-permission-mode-transition
		  "mevedel-permissions" (mode))
(defvar mevedel-permission-mode)

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--render-data-blocks
		  "mevedel-pipeline" (string))

;; `mevedel-plan-handoff'
(declare-function mevedel-plan-handoff-reserved-goal-id
		  "mevedel-plan-handoff" (&optional session))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-mode--invalidate-proposal
		  "mevedel-plan-mode" (&optional session))

;; `mevedel-prompt-submission'
(declare-function mevedel-prompt-submission-audits
		  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-commit
		  "mevedel-prompt-submission" (submission))
(declare-function mevedel-prompt-submission-context
		  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-create
		  "mevedel-prompt-submission" (&rest args))
(declare-function mevedel-prompt-submission-display-text
		  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-input
		  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-outcome
		  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-reserve
		  "mevedel-prompt-submission" (submission))
(declare-function mevedel-prompt-submission-restore
		  "mevedel-prompt-submission" (submission))
(declare-function mevedel-prompt-submission-set-outcome
		  "mevedel-prompt-submission" (submission outcome))

;; `mevedel-resource-capf'
(declare-function mevedel-resource-capf "mevedel-resource-capf" ())

;; `mevedel-review'
(declare-function mevedel-review--mark-command-outcome
		  "mevedel-review" (outcome))
(declare-function mevedel-review-command-skill-p "mevedel-review"
		  (skill))
(declare-function mevedel-review-transform-outcome "mevedel-review"
		  (skill-name outcome))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--assert-stable-source
                  "mevedel-session-persistence"
                  (session buffer operation))
(declare-function mevedel-session-persistence--retarget-worktree-path
		  "mevedel-session-persistence" (session path))
(declare-function mevedel-session-persistence-conversation-fork
                  "mevedel-session-persistence" (buffer target))
(declare-function mevedel-session-persistence-worktree-fork
		  "mevedel-session-persistence" (buffer target))
(defvar mevedel-session--read-only-mode)

;; `mevedel-side-conversation'
(declare-function mevedel-side-conversation-send
                  "mevedel-side-conversation" ())

;; `mevedel-skills-core'
(declare-function mevedel-session-get-skill "mevedel-skills-core"
		  (session name))
(declare-function mevedel-skill-name "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-user-invocable-p "mevedel-skills-core"
		  (cl-x) t)

;; `mevedel-skills-invoke'
(declare-function mevedel-skills--insert-fork-result
		  "mevedel-skills-invoke" (outcome))
(declare-function mevedel-skills--parse-skill-line
		  "mevedel-skills-invoke" (text))
(declare-function mevedel-skills-commit-invoked-records
		  "mevedel-skills-invoke" (session records))
(declare-function mevedel-skills-dispatch-prepared-fork
		  "mevedel-skills-invoke" t t)
(declare-function mevedel-skills-prepare-user-input
		  "mevedel-skills-invoke" (text session))
(declare-function mevedel-skills-refresh-bound-input
		  "mevedel-skills-invoke" (text session))

;; `mevedel-skills-plan'
(declare-function mevedel-skill-invocation-plan-fork-p
		  "mevedel-skills-plan" (cl-x) t)
(declare-function mevedel-skill-invocation-plan-occurrences
		  "mevedel-skills-plan" (cl-x) t)
(declare-function mevedel-skills-plan-prepare "mevedel-skills-plan"
		  (plan callback &optional cancelled-p))
(declare-function mevedel-skills-plan-render-data
		  "mevedel-skills-plan" (plan expanded-prompt))
(declare-function mevedel-skills-plan-user-input "mevedel-skills-plan"
		  (text session))

;; `mevedel-skills-ui'
(declare-function mevedel-skills--parse-slash-line "mevedel-skills-ui"
		  (text))
(declare-function mevedel-skills--remaining-argument-hint
		  "mevedel-skills-ui" (skill arguments))
(declare-function mevedel-skills--slash-capf "mevedel-skills-ui"
		  (buffer session local-commands &optional input-start))
(declare-function mevedel-skills-install-font-lock "mevedel-skills-ui"
		  nil)
(declare-function mevedel-skills-local-command-active-request-p
		  "mevedel-skills-ui" (name args))
(defvar mevedel-slash-commands)

;; `mevedel-structs'
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-p "mevedel-structs" (cl-x))
(declare-function mevedel-directive-plan "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-assert-target-ready
                  "mevedel-structs" (session))
(declare-function mevedel-request-begin "mevedel-structs"
		  (session &optional directive-uuid))
(declare-function mevedel-request-end "mevedel-structs" nil)
(declare-function mevedel-request-fsm "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session--set-active-dropped-file-grants
                  "mevedel-structs" (session paths))
(declare-function mevedel-session--set-dropped-file-grants
                  "mevedel-structs" (session paths))
(declare-function mevedel-session-activate-dropped-file-grants
		  "mevedel-structs" (session paths))
(declare-function mevedel-session-active-dropped-file-grants
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-add-dropped-file-grant
		  "mevedel-structs" (session path))
(declare-function mevedel-session-clear-dropped-file-grants
		  "mevedel-structs" (session))
(declare-function mevedel-session-directive-planning
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-dropped-file-grants
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-enqueue-pending-input
                  "mevedel-structs" (session category entry))
(declare-function mevedel-session-forked-from-fork-point-id
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-hook-context-pending
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-follow-ups
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-input-delivery-paused-p
                  "mevedel-structs" (session))
(declare-function mevedel-session-pending-input-failure-paused
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-input-paused
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-plan-approval
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-steering
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pop-dropped-file-grants
		  "mevedel-structs" (session paths))
(declare-function mevedel-session-set-pending-inputs
                  "mevedel-structs" (session category entries))
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-state-dir "mevedel-structs"
		  (workspace))
(defvar mevedel--agent-invocation nil)
(defvar mevedel--compaction-in-flight nil)
(defvar mevedel--current-directive-uuid)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)
(defvar mevedel--workspace)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record "mevedel-telemetry"
		  (session event &rest props))

;; `mevedel-transcript'
(declare-function mevedel-transcript-prompt-transform-start
		  "mevedel-transcript" nil)

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
		  "mevedel-transcript-audit" (record))

;; `mevedel-utilities'
(declare-function mevedel--clear-user-turn-gptel-properties
		  "mevedel-utilities" (start end))
(declare-function mevedel--normalize-message-text "mevedel-utilities"
		  (text))

;; `mevedel-view'
(declare-function mevedel-view--abort-data-buffer
                  "mevedel-view" (data-buffer))
(defvar mevedel-view--interaction-marker)
(defvar mevedel-view--side-conversation-p)
(defvar mevedel-view--status-marker)

;; `mevedel-view-agent'
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-audit'
(declare-function mevedel-view--prompt-rewrite-audit-record
		  "mevedel-view-audit"
		  (event original submitted decision))

;; `mevedel-view-history'
(declare-function mevedel-view-history-add "mevedel-view-history"
		  (input))
(declare-function mevedel-view-history-beginning-of-line
		  "mevedel-view-history" (&optional arg))
(declare-function mevedel-view-history-browse "mevedel-view-history" ())
(declare-function mevedel-view-history-clear-input
		  "mevedel-view-history" ())
(declare-function mevedel-view-history-load "mevedel-view-history"
		  (&optional session))
(declare-function mevedel-view-history-next "mevedel-view-history" ())
(declare-function mevedel-view-history-previous "mevedel-view-history" ())
(declare-function mevedel-view-history-search "mevedel-view-history" ())

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-rebuild
		  "mevedel-view-interaction" nil)
(declare-function mevedel-view--interaction-register
		  "mevedel-view-interaction" (descriptor))
(declare-function mevedel-view--interaction-unregister
		  "mevedel-view-interaction" (id))

;; `mevedel-view-markdown'


(autoload 'mevedel-view--normalize-local-file-uri-path
  "mevedel-view-markdown")

;; `mevedel-view-render'
(declare-function mevedel-view--full-rerender "mevedel-view-render" ())
(declare-function mevedel-view--history-insertion-marker
                  "mevedel-view-render" ())
(declare-function mevedel-view--hook-context-events-from-text
                  "mevedel-view-render" (text))
(declare-function mevedel-view--inline-skill-prompt-summary-body
                  "mevedel-view-render" (render-data))
(declare-function mevedel-view--insert-rendered-tool
                  "mevedel-view-render" (rendering source))
(declare-function mevedel-view--insert-user-message
                  "mevedel-view-render"
                  (text &optional kind hook-context prompt-summary-body
                        prompt-summary-source hook-audits))
(declare-function mevedel-view--source-range
                  "mevedel-view-render" (data-buffer start end))
(declare-function mevedel-view-fork-point-at-point
                  "mevedel-view-render" ())
(declare-function mevedel-view-historical-segment-p
                  "mevedel-view-render" ())
(declare-function mevedel-view-reset-agent-ephemeral-state
                  "mevedel-view-render" (&optional data-buf))
(defvar mevedel-view--display-map)

;; `mevedel-view-stream'
(declare-function mevedel-view--stop-request-progress
                  "mevedel-view-stream" ())
(declare-function mevedel-view-stream-begin-turn
                  "mevedel-view-stream"
                  (view-turn-start data-turn-start &optional no-spinner))
(defvar mevedel-view--data-turn-start)
(defvar mevedel-view--in-flight-turn-start)

;; `mevedel-workspace'
(declare-function mevedel-workspace-ensure-generated-state-ignored
                  "mevedel-workspace" (workspace))

;; `mevedel-worktree'
(declare-function mevedel-worktree-fork-preflight
		  "mevedel-worktree" (session))
(declare-function mevedel-worktree-fork-reservation
		  "mevedel-worktree" (session &optional preflight))

;; `select'
(declare-function gui-get-selection "select" (selection-symbol target-type))

;; `seq'
(declare-function seq-take "seq" (sequence n))


;;
;;; Customization


(defcustom mevedel-view-clipboard-image-handlers
  (list
   (list (cons :command "wl-paste")
         (cons :save (lambda (file-path)
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (let ((coding-system-for-read 'binary)
                               (exit-code
                                (call-process "wl-paste" nil (list t nil)
                                              nil "--type" "image/png")))
                           (unless (zerop exit-code)
                             (error "Command wl-paste failed with exit code %d"
                                    exit-code))
                           (let ((coding-system-for-write 'binary))
                             (write-region (point-min) (point-max)
                                           file-path nil 'silent)))))))
   (list (cons :command "pngpaste")
         (cons :save (lambda (file-path)
                       (let ((exit-code
                              (call-process "pngpaste" nil nil nil
                                            file-path)))
                         (unless (zerop exit-code)
                           (error "Command pngpaste failed with exit code %d"
                                  exit-code))))))
   (list (cons :command "xclip")
         (cons :save (lambda (file-path)
                       (when-let* ((targets (and (eq (window-system) 'x)
                                                 (gui-get-selection
                                                  'CLIPBOARD 'TARGETS)))
                                   ((vectorp targets))
                                   ((not (cl-position 'image/png targets))))
                         (error "No image/png in clipboard"))
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (let ((exit-code
                                (call-process "xclip" nil t nil
                                              "-selection" "clipboard"
                                              "-t" "image/png" "-o")))
                           (unless (zerop exit-code)
                             (error "Command xclip failed with exit code %d"
                                    exit-code))
                           (let ((coding-system-for-write 'binary))
                             (write-region (point-min) (point-max)
                                           file-path nil 'silent)))))))
   (list (cons :command "powershell")
         (cons :save (lambda (file-path)
                       (let ((exit-code
                              (call-process
                               "powershell" nil nil nil
                               "-Command"
                               (format "& {(Get-Clipboard -Format image).Save(%s)}"
                                       (shell-quote-argument file-path)))))
                         (unless (zerop exit-code)
                           (error "Command powershell failed with exit code %d"
                                  exit-code)))))))
  "Handlers for saving a clipboard image to a file.
Each handler is an alist with `:command' and `:save'.  The first
handler whose command exists is used by `mevedel-view-yank-dwim'."
  :type '(repeat (alist :key-type keyword :value-type sexp))
  :group 'mevedel)



;;
;;; Input prompt

(defconst mevedel-view--input-prompt "> "
  "Read-only prefix rendered at the start of the input zone.")

(defface mevedel-view-directive-scope
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for directive-scoped composer chrome."
  :group 'mevedel)

(defface mevedel-view-directive-scope-banner
  '((((background light))
     :inherit mevedel-view-directive-scope :background "#eef3fe" :extend t)
    (((background dark))
     :inherit mevedel-view-directive-scope :background "#0d1b3d" :extend t)
    (t :inherit mevedel-view-directive-scope :extend t))
  "Face for the full-width directive-scope composer banner line."
  :group 'mevedel)

(defvar-local mevedel-view--composer-scope nil
  "Current directive composer scope, or nil for ordinary chat.")

(defvar-local mevedel-view--composer-drafts nil
  "Draft snapshots keyed by chat or directive composer scope.")

(defun mevedel-view-composer-scope-label (&optional scope)
  "Return a concise label for SCOPE or the current composer scope."
  (when-let* ((scope (or scope mevedel-view--composer-scope)))
    (format "directive %s/%s"
            (substring (plist-get scope :directive-id)
                       0 (min 8 (length (plist-get scope :directive-id))))
            (symbol-name (plist-get scope :action)))))

(defun mevedel-view--effective-permission-mode ()
  "Return the permission mode to apply to the current view buffer."
  (require 'mevedel-permissions)
  (mevedel-permission-mode-effective
   (and (boundp 'mevedel--session) mevedel--session)
   (and (boundp 'mevedel--data-buffer)
        (buffer-live-p mevedel--data-buffer)
        mevedel--data-buffer)
   (current-buffer)))

(defun mevedel-view--permission-mode-display (mode)
  "Return (LABEL FACE) for permission MODE."
  (require 'mevedel-permissions)
  (list
   (mevedel-permission-mode-label mode)
   (pcase mode
     ('edits 'mevedel-view-permission-mode-edits)
     ('full-auto 'mevedel-view-permission-mode-full-auto)
     (_ 'mevedel-view-permission-mode-ask))))

(defun mevedel-view--plan-mode-p ()
  "Return non-nil when the current view's session is in Plan mode."
  (and (boundp 'mevedel--session)
       mevedel--session
       (mevedel-session-plan-mode mevedel--session)))

(defconst mevedel-view--permission-mode-cycle
  '(ask edits full-auto)
  "Permission states cycled by `mevedel-view-cycle-permission-mode'.")

(defun mevedel-view--next-permission-mode (&optional mode)
  "Return the permission mode after MODE in the view cycle.
Nil and unknown modes are treated as `ask'."
  (let* ((current (if (memq mode mevedel-view--permission-mode-cycle)
                      mode
                    'ask))
         (tail (cdr (memq current mevedel-view--permission-mode-cycle))))
    (or (car tail)
        (car mevedel-view--permission-mode-cycle))))

(defun mevedel-view--input-prompt-string (&optional mode)
  "Return the read-only input prompt string for permission MODE.
The prompt starts with a blank separator line so status and interaction
rows remain visually distinct from the editable composer."
  (let ((mode (or mode (mevedel-view--effective-permission-mode))))
    (if mevedel-view--composer-scope
        (let* ((record (plist-get mevedel-view--composer-scope :record))
               (action (plist-get mevedel-view--composer-scope :action))
               (action-label
                (mevedel--directive-action-label
                 (if (eq action 'discuss)
                     (cond
                      ((plist-get mevedel-view--composer-scope :attempt-index)
                       'discuss-result)
                      ((and record
                            (memq 'continue-discussion
                                  (mevedel-directive-actions record)))
                       'continue-discussion)
                      (t 'discuss))
                   action)))
               (request
                (and record
                     (truncate-string-to-width
                      (replace-regexp-in-string
                       "[ \t\n\r]+" " "
                      (mevedel-directive-request record))
                      80 nil nil "…")))
               (request-label
                (or request
                    (plist-get mevedel-view--composer-scope :directive-id)))
               (permission-display
                (mevedel-view--permission-mode-display mode))
               (permission-label (car permission-display))
               (permission-face (cadr permission-display)))
          (concat
           "\n"
           (propertize (format "◆ %s · %s\n" action-label request-label)
                       'font-lock-face 'mevedel-view-directive-scope-banner)
           (propertize "  isolated from chat · " 'font-lock-face 'shadow)
           (propertize permission-label 'font-lock-face permission-face)
           (when (mevedel-view--plan-mode-p)
             (concat
              (propertize " · " 'font-lock-face 'shadow)
              (propertize "Plan paused"
                          'font-lock-face 'mevedel-view-plan-mode)))
           (propertize " · C-c C-k Back\n" 'font-lock-face 'shadow)
           (propertize "◆ > "
                       'font-lock-face 'mevedel-view-directive-scope)))
      (if mevedel-view--pending-input-edit
          (propertize
           (format "\n[Editing %s] %s"
                   (plist-get mevedel-view--pending-input-edit :label)
                   mevedel-view--input-prompt)
           'font-lock-face 'mevedel-view-input-prompt)
	(if (mevedel-view--plan-mode-p)
            (pcase-let* ((`(,label ,_) (mevedel-view--permission-mode-display mode))
			 (text (format "\n[Plan · %s] %s"
                                       label mevedel-view--input-prompt)))
              (add-text-properties
               0 (length text)
               '(font-lock-face mevedel-view-input-prompt)
               text)
              (add-text-properties
               2 (+ 9 (length label))
               '(font-lock-face mevedel-view-plan-mode)
               text)
              text)
	  (if (eq mode 'ask)
              (propertize (concat "\n" mevedel-view--input-prompt)
			  'font-lock-face 'mevedel-view-input-prompt)
            (pcase-let* ((`(,label ,face)
			  (mevedel-view--permission-mode-display mode))
			 (text (format "\n[%s] %s"
                                       label mevedel-view--input-prompt))
			 (label-start 2)
			 (label-end (+ label-start (length label))))
              (add-text-properties
               0 (length text)
               '(font-lock-face mevedel-view-input-prompt)
               text)
              (add-text-properties
               label-start label-end
               `(font-lock-face ,face)
               text)
              text)))))))


;;
;;; Composer state and redraw preservation

(defvar-local mevedel-view--input-marker nil
  "Marker separating request progress from the input zone.
Everything above this marker is read-only history/status/interaction
chrome; everything at or below it belongs to the input zone.  The input
zone starts with the read-only prompt prefix, followed by the editable
composer body.")

(defvar-local mevedel-view--armed-session-fork nil
  "Stable fork-point target armed for the next model-bound submission.")

(defvar-local mevedel-view--armed-session-fork-return-point nil
  "View position to restore when cancelling a historical session fork.")

(defvar-local mevedel-view--historical-composer-overlay nil
  "Overlay hiding the live composer during archived-segment inspection.")

(defun mevedel-view--set-historical-composer-visible (visible)
  "Show the live composer when VISIBLE, otherwise hide and lock it."
  (when (overlayp mevedel-view--historical-composer-overlay)
    (delete-overlay mevedel-view--historical-composer-overlay)
    (setq mevedel-view--historical-composer-overlay nil))
  (if visible
      (progn
        (remove-from-invisibility-spec 'mevedel-view-historical-composer)
        (setq buffer-read-only nil))
    (add-to-invisibility-spec 'mevedel-view-historical-composer)
    (setq mevedel-view--historical-composer-overlay
          (make-overlay
           (mevedel-view--input-marker-position) (point-max)
           (current-buffer) t t))
    (overlay-put mevedel-view--historical-composer-overlay
                 'invisible 'mevedel-view-historical-composer)
    (setq buffer-read-only t)))

(defvar-keymap mevedel-view--armed-session-fork-map
  :doc "Keymap for the armed session-fork interaction row."
  "RET" #'mevedel-view-cancel-session-fork
  "<mouse-1>" #'mevedel-view-cancel-session-fork)

(defun mevedel-view-cancel-session-fork (&optional _event)
  "Disarm the pending session fork without changing the composer draft."
  (interactive)
  (when mevedel-view--armed-session-fork
    (setq mevedel-view--armed-session-fork nil)
    (mevedel-view--interaction-unregister 'armed-session-fork)
    (when (mevedel-view-historical-segment-p)
      (mevedel-view--set-historical-composer-visible nil)
      (when mevedel-view--armed-session-fork-return-point
        (goto-char
         (min mevedel-view--armed-session-fork-return-point
              (mevedel-view--input-marker-position)))))
    (setq mevedel-view--armed-session-fork-return-point nil)
    t))

(defun mevedel-view-cancel-composer-state ()
  "Cancel the active composer mode, including directive scope."
  (interactive)
  (cond
   (mevedel-view--armed-session-fork
    (mevedel-view-cancel-session-fork))
   (mevedel-view--pending-input-edit
    (mevedel-pending-inputs-cancel-edit))
   (mevedel-view--composer-scope
    (mevedel-view-back-to-chat))))

(defun mevedel-view--arm-session-fork (fork-type)
  "Arm FORK-TYPE from the settled assistant response at point."
  (mevedel-view--ensure-interactive-chat-view)
  (let* ((target (mevedel-view-fork-point-at-point))
         (session (mevedel-view--session))
         (label (if (eq fork-type 'worktree) "worktree" "conversation"))
         reservation)
    (require 'mevedel-session-persistence)
    (when (equal
           (plist-get target :fork-point-id)
           (mevedel-session-forked-from-fork-point-id session))
      (user-error
       "Fork the inherited response from Source; switch variants first"))
    (mevedel-session-persistence--assert-stable-source
     session mevedel--data-buffer "forking")
    (when (eq fork-type 'worktree)
      (require 'mevedel-worktree)
      (let ((preflight (mevedel-worktree-fork-preflight session)))
        (setq reservation
              (mevedel-worktree-fork-reservation session preflight))))
    (setq target (copy-sequence target))
    (plist-put target :fork-type fork-type)
    (when reservation
      (plist-put target :worktree-reservation reservation))
    (when (mevedel-view-historical-segment-p)
      (setq mevedel-view--armed-session-fork-return-point (point))
      (mevedel-view--set-historical-composer-visible t))
    (mevedel-view--interaction-register
     (list :kind 'preview
           :id 'armed-session-fork
           :active-work-paused nil
           :body
           (format "Fork %s from Assistant turn %d  [Cancel]"
                   label (plist-get target :cum-turn))
           :keymap mevedel-view--armed-session-fork-map
           :help-echo (format "Cancel %s Fork"
                              (capitalize label))))
    (setq mevedel-view--armed-session-fork target)
    (goto-char (point-max))))

(defun mevedel-view--assert-live-tip (&optional allow-armed-fork)
  "Refuse live-tip actions during historical inspection.
ALLOW-ARMED-FORK permits submission of an already armed session fork."
  (when (and (mevedel-view-historical-segment-p)
             (not (and allow-armed-fork
                       mevedel-view--armed-session-fork)))
    (user-error
     "Viewing historical segment; return to latest or fork from an assistant response")))

(defun mevedel-view-arm-conversation-fork ()
  "Arm a Conversation Fork from the settled assistant response at point."
  (interactive)
  (mevedel-view--arm-session-fork 'conversation))

(defun mevedel-view-arm-worktree-fork ()
  "Arm a Worktree Fork from the settled assistant response at point."
  (interactive)
  (mevedel-view--arm-session-fork 'worktree))

(defvar-keymap mevedel-view--composer-keymap
  :doc "Keymap active over the editable composer body."
  "C-<tab>" #'mevedel-view-toggle-plan-mode
  "C-c RET" #'mevedel-view-send
  "C-c TAB" #'mevedel-view-send-follow-up
  "C-c C-c" #'mevedel-pending-inputs-save-edit
  "C-c C-e" #'mevedel-pending-inputs-open
  "C-c C-k" #'mevedel-view-cancel-composer-state
  "C-c C-l" #'mevedel-view-history-browse
  "C-c C-u" #'mevedel-view-history-clear-input
  "C-y" #'mevedel-view-yank-dwim
  "M-n" #'mevedel-view-history-next
  "M-p" #'mevedel-view-history-previous
  "M-r" #'mevedel-view-history-search
  "<backtab>" #'mevedel-view-cycle-permission-mode
  "S-TAB" #'mevedel-view-cycle-permission-mode)

(defvar-keymap mevedel-view--side-conversation-keymap
  :doc "Keymap active over an ephemeral side-conversation composer."
  "C-c RET" #'mevedel-view-send
  "C-c TAB" #'mevedel-view-send-follow-up
  "C-c C-k" #'mevedel-view-abort
  "C-y" #'mevedel-view-yank-dwim)

(define-key mevedel-view--composer-keymap
            [remap move-beginning-of-line]
            #'mevedel-view-history-beginning-of-line)

(defvar-local mevedel-view--composer-keymap-overlay nil
  "Overlay that gives the editable composer its local keymap.")


(defvar-local mevedel-view--skill-argument-hint-overlay nil
  "Zero-width overlay that displays skill argument guidance in the composer.")

(defvar-local mevedel-view--prompt-hook-pending nil
  "Non-nil while a `UserPromptSubmit' hook gate is pending for this view.
This covers the interval before the prompt has been accepted and before
`mevedel--current-request' exists in the data buffer.")

(defvar-local mevedel-view--pending-skill-submission nil
  "Cancellation token for skill-plan preparation before request dispatch.")

(defvar-local mevedel-view--pending-input-edit nil
  "Queue edit state active in this composer, or nil.")

(defun mevedel-view--cancel-pending-skill-submission ()
  "Cancel this view's pending skill-plan preparation, if any."
  (when mevedel-view--pending-skill-submission
    (setf (plist-get mevedel-view--pending-skill-submission :cancelled) t)
    (setq mevedel-view--pending-skill-submission nil)))



(defun mevedel-view--position-in-input-region-p (position)
  "Return non-nil when POSITION is in the editable composer."
  (and (boundp 'mevedel-view--input-marker)
       (markerp mevedel-view--input-marker)
       (marker-buffer mevedel-view--input-marker)
       (not (bound-and-true-p mevedel-view--agent-transcript-p))
       (ignore-errors
         (>= position (mevedel-view--input-start)))))

(defun mevedel-view--point-in-input-region-p ()
  "Return non-nil when point is in the editable composer."
  (mevedel-view--position-in-input-region-p (point)))

(defun mevedel-view--call-preserving-input-point (thunk)
  "Call THUNK, preserving point's offset inside the composer.
Redraws for spinners, agent status, and interaction prompts insert
or delete text above the editable input.  When the user is typing in
the composer, restore point by its input-relative offset after THUNK
finishes."
  (let* ((buffer (current-buffer))
         (preserve-p (mevedel-view--point-in-input-region-p))
         (offset (and preserve-p
                      (- (point) (mevedel-view--input-start))))
         result)
    (unwind-protect
        (setq result (funcall thunk))
      (when (and preserve-p
                 (buffer-live-p buffer))
        (with-current-buffer buffer
          (when (and (markerp mevedel-view--input-marker)
                     (marker-buffer mevedel-view--input-marker))
            (goto-char
             (min (point-max)
                  (+ (mevedel-view--input-start)
                     (max 0 offset))))))))
    result))

(defun mevedel-view--call-preserving-input-text (thunk)
  "Call THUNK without allowing it to mutate editable composer text.
History/status rendering should only change text above
`mevedel-view--input-marker'.  This guard restores the input body if a
late callback accidentally inserts transcript content below the prompt."
  (let* ((preserve-p
          (and (not (bound-and-true-p mevedel-view--agent-transcript-p))
               (markerp mevedel-view--input-marker)
               (marker-buffer mevedel-view--input-marker)))
         (text (and preserve-p
                    (progn
                      (require 'mevedel-mention-bindings)
                      (mevedel-mention-bindings-copy-text
                       (buffer-substring
                        (mevedel-view--input-start) (point-max))))))
         result)
    (let ((inhibit-modification-hooks t))
      (setq result (funcall thunk)))
    (when (and preserve-p
               (markerp mevedel-view--input-marker)
               (marker-buffer mevedel-view--input-marker))
      (let* ((start (mevedel-view--input-start))
             (current (mevedel-mention-bindings-copy-text
                       (buffer-substring start (point-max)))))
        (unless (equal-including-properties current text)
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (delete-region start (point-max))
            (goto-char start)
            (insert text)))))
    result))

(defun mevedel-view--call-preserving-window-state (thunk)
  "Call THUNK while preserving each displayed window's browsing state.
Preserves those values for every window displaying the current buffer.
Windows already following the bottom continue following new output;
windows browsing older content retain their point and start.

Used to wrap delete-and-re-render operations so the user's scroll
position and caret do not jump back to the edit site on every
progress tick.  Positions that are no longer valid after BODY (e.g.
point was inside the deleted region) are quietly clamped to the
buffer.  The buffer mark, active-region state, and selection direction
are preserved with point.  When either endpoint is in the editable
composer, preserve it by offset from `mevedel-view--input-start' so
streaming text inserted above the composer does not strand it in
rendered transcript text."
  (let* ((mevedel-view--pww-selected-window (selected-window))
          (mevedel-view--pww-current-buffer (current-buffer))
          (mevedel-view--pww-current-point (point))
          (mevedel-view--pww-current-mark (mark t))
          (mevedel-view--pww-mark-active mark-active)
          (mevedel-view--pww-deactivate-mark deactivate-mark)
          (mevedel-view--pww-current-input-offset
           (and (mevedel-view--point-in-input-region-p)
                (- (point) (mevedel-view--input-start))))
          (mevedel-view--pww-mark-input-offset
           (and mevedel-view--pww-current-mark
                (mevedel-view--position-in-input-region-p
                 mevedel-view--pww-current-mark)
                (- mevedel-view--pww-current-mark
                   (mevedel-view--input-start))))
          (mevedel-view--pww-saved
           (mapcar (lambda (w)
                     (with-current-buffer mevedel-view--pww-current-buffer
                       (let ((wp (window-point w))
                             (ws (window-start w)))
                         (list w
                               wp
                               ws
                               (and (mevedel-view--position-in-input-region-p wp)
                                    (- wp (mevedel-view--input-start)))
                               (= wp (point-max))))))
                   (get-buffer-window-list (current-buffer) nil t))))
     (prog1 (funcall thunk)
       (let ((restored-current-point
              (if (and mevedel-view--pww-current-input-offset
                       (markerp mevedel-view--input-marker)
                       (marker-buffer mevedel-view--input-marker))
                  (+ (mevedel-view--input-start)
                     (max 0 mevedel-view--pww-current-input-offset))
                mevedel-view--pww-current-point)))
         (goto-char (min (point-max) restored-current-point)))
       (dolist (entry mevedel-view--pww-saved)
         (pcase-let ((`(,w ,wp ,ws ,input-offset ,at-bottom) entry))
           (when (window-live-p w)
             (let ((restored-point
                    (if (and input-offset
                             (markerp mevedel-view--input-marker)
                             (marker-buffer mevedel-view--input-marker))
                        (+ (mevedel-view--input-start)
                           (max 0 input-offset))
                      wp)))
               (when restored-point
                 (set-window-point w (min (point-max) restored-point)))
               (when (eq w mevedel-view--pww-selected-window)
                 (goto-char (window-point w))))
             (when (and ws (<= ws (point-max)))
               (set-window-start w ws t))
             (when (and at-bottom (not input-offset))
               (save-selected-window
                 (select-window w)
                 (goto-char (point-max))
                 (recenter -1))))))
       (when mevedel-view--pww-current-mark
         (set-mark
          (min
           (point-max)
           (if (and mevedel-view--pww-mark-input-offset
                    (markerp mevedel-view--input-marker)
                    (marker-buffer mevedel-view--input-marker))
               (+ (mevedel-view--input-start)
                  (max 0 mevedel-view--pww-mark-input-offset))
             mevedel-view--pww-current-mark)))
         (setq mark-active mevedel-view--pww-mark-active
               deactivate-mark mevedel-view--pww-deactivate-mark)))))

(defmacro mevedel-view--preserving-window-state (&rest body)
  "Execute BODY while preserving point and window positions."
  (declare (indent 0) (debug t))
  `(mevedel-view--call-preserving-window-state
    (lambda () ,@body)))

(defun mevedel-view--call-with-render-boundaries-advancing (thunk)
  "Call THUNK while zone boundary markers advance across insertions."
  (let ((status-type (and (markerp mevedel-view--status-marker)
                          (marker-insertion-type mevedel-view--status-marker)))
        (interaction-type
         (and (markerp mevedel-view--interaction-marker)
              (marker-insertion-type mevedel-view--interaction-marker)))
        (input-type (and (markerp mevedel-view--input-marker)
                         (marker-insertion-type mevedel-view--input-marker))))
    (unwind-protect
        (progn
          (when (markerp mevedel-view--status-marker)
            (set-marker-insertion-type mevedel-view--status-marker t))
          (when (markerp mevedel-view--interaction-marker)
            (set-marker-insertion-type mevedel-view--interaction-marker t))
          (when (markerp mevedel-view--input-marker)
            (set-marker-insertion-type mevedel-view--input-marker t))
          (funcall thunk))
      (when (markerp mevedel-view--status-marker)
        (set-marker-insertion-type mevedel-view--status-marker status-type))
      (when (markerp mevedel-view--interaction-marker)
        (set-marker-insertion-type mevedel-view--interaction-marker
                                   interaction-type))
      (when (markerp mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker input-type)))))

(defmacro mevedel-view--with-render-boundaries-advancing (&rest body)
  "Execute BODY while zone boundary markers advance across insertions."
  (declare (indent 0) (debug t))
  `(mevedel-view--call-with-render-boundaries-advancing
    (lambda () ,@body)))

(defun mevedel-view--call-preserving-user-view-state (thunk)
  "Call THUNK without moving the user's live view cursor.
Async redraws may insert, delete, or reconcile view-owned text while the
user is typing in the composer or browsing transcript history.  Preserve
all displayed windows plus the editable composer text around THUNK."
  (mevedel-view--preserving-window-state
    (mevedel-view--call-preserving-input-text
     (lambda ()
       (mevedel-view--call-preserving-input-point thunk)))))


;;
;;; File input

(defun mevedel-view--insert-dropped-file-mentions (paths)
  "Insert @file mentions for dropped PATHS into the composer."
  (require 'mevedel-mention-bindings)
  (mevedel-view--ensure-interactive-chat-view)
  (let ((session (mevedel-view--session))
        tokens)
    (unless session
      (user-error "No active session for dropped files"))
    (dolist (path paths)
      (let* ((expanded (expand-file-name path))
             (token (mevedel-mentions-file-token expanded)))
        (mevedel-mention-bindings-set
         0 (length token)
         (list :kind 'file :token token :path expanded)
         token)
        (push token tokens)
        (mevedel-session-add-dropped-file-grant session expanded)))
    (setq tokens (nreverse tokens))
    (when tokens
      (when (< (point) (mevedel-view--input-start))
        (goto-char (point-max)))
      (unless (or (= (point) (mevedel-view--input-start))
                  (memq (char-before) '(?\s ?\t ?\n)))
        (insert " "))
      (insert (string-join tokens " "))
      (unless (or (eobp) (memq (char-after) '(?\s ?\t ?\n)))
        (insert " "))
      (font-lock-flush (mevedel-view--input-start) (point-max)))))

(defun mevedel-view--dnd-local-file-paths (uris)
  "Return existing regular local file paths from DND URIS.
Directories are ignored; directory-drop expansion is intentionally out
of scope for the composer."
  (let (paths)
    (dolist (uri (ensure-list uris))
      (let ((path (and (stringp uri)
                       (mevedel-view--normalize-local-file-uri-path
                        (dnd-get-local-file-name uri nil)))))
        (cond
         ((not path)
          (message "mevedel: ignored non-local drop: %s" uri))
         ((not (file-exists-p path))
          (message "mevedel: ignored missing dropped file: %s" path))
         ((file-directory-p path)
          (message "mevedel: ignored directory drop: %s" path))
         (t
          (push path paths)))))
    (nreverse paths)))

(defun mevedel-view--dnd-handle-files (uris action)
  "Handle dropped local file URIS with DND ACTION.
URIS may be a single URI string or a list of URI strings.  Some DND
paths call protocol handlers in the single-URL shape even when the
handler advertises `dnd-multiple-handler'."
  (let ((paths (mevedel-view--dnd-local-file-paths uris)))
    (when paths
      (mevedel-view--insert-dropped-file-mentions paths)
      (or action 'copy))))

(put 'mevedel-view--dnd-handle-files 'dnd-multiple-handler t)

(defun mevedel-view--media-dir ()
  "Return the workspace media directory for clipboard images."
  (let* ((session (mevedel-view--session))
         (workspace (and session (mevedel-session-workspace session))))
    (unless workspace
      (user-error "No active session for clipboard image"))
    (let ((dir (file-name-concat (mevedel-workspace-state-dir workspace)
                                 "media")))
      (make-directory dir t)
      (require 'mevedel-workspace)
      (mevedel-workspace-ensure-generated-state-ignored workspace)
      dir)))

(defun mevedel-view--clipboard-image-path (dir)
  "Return a fresh clipboard image path under DIR."
  (let* ((stamp (format-time-string "%Y%m%d-%H%M%S"))
         (base (file-name-concat dir (format "clipboard-%s" stamp)))
         (path (concat base ".png"))
         (n 1))
    (while (file-exists-p path)
      (setq path (format "%s-%d.png" base n))
      (cl-incf n))
    path))

(defun mevedel-view--save-clipboard-image (&optional no-error)
  "Save a clipboard image under `.mevedel/media/'.
Return the saved image path.  When NO-ERROR is non-nil, return nil
instead of signaling when no image is available."
  (condition-case err
      (let* ((dir (mevedel-view--media-dir))
             (file-path (mevedel-view--clipboard-image-path dir))
             (handler (cl-find-if
                       (lambda (entry)
                         (executable-find (alist-get :command entry)))
                       mevedel-view-clipboard-image-handlers)))
        (cond
         ((not handler)
          (unless no-error
            (error "No clipboard image utility found")))
         (t
          (condition-case err
              (funcall (alist-get :save handler) file-path)
            (error
             (when (file-exists-p file-path)
               (delete-file file-path))
             (unless no-error
               (signal (car err) (cdr err)))))
          (cond
           ((not (file-exists-p file-path))
            (unless no-error
              (error "Clipboard image file was not created")))
           ((zerop (nth 7 (file-attributes file-path)))
            (delete-file file-path)
            (unless no-error
              (error "No image found in clipboard")))
           (t file-path)))))
    (error
     (unless no-error
       (signal (car err) (cdr err))))))

(put 'mevedel-view-yank-dwim 'delete-selection 'yank)
(defun mevedel-view-yank-dwim (&optional arg)
  "Yank text, or save a clipboard image and insert it as an `@file'.
ARG is passed through from the interactive prefix."
  (interactive "*P")
  (if-let* (((window-system))
            (path (mevedel-view--save-clipboard-image t)))
      (mevedel-view--insert-dropped-file-mentions (list path))
    (yank arg)))

(defun mevedel-view--install-dnd ()
  "Install local file drag/drop support for the current view buffer."
  (require 'dnd)
  (let (rest)
    (dolist (entry dnd-protocol-alist)
      (unless (eq (cdr entry) 'mevedel-view--dnd-handle-files)
        (push entry rest)))
    (setq-local dnd-protocol-alist
                (cons '("^file:" . mevedel-view--dnd-handle-files)
                      (nreverse rest)))))

;;
;;; Initialization

(defun mevedel-view-composer-initialize ()
  "Initialize composer editing support in the current chat view."
  (unless mevedel-view--agent-transcript-p
    (setq mevedel-view--composer-scope nil
          mevedel-view--composer-drafts (make-hash-table :test #'equal))
    (require 'mevedel-mentions)
    (require 'mevedel-skills-ui)
    (require 'mevedel-transcript)
    (require 'mevedel-transcript-audit)
    (require 'mevedel-utilities)
    (require 'mevedel-view-history)
    (setq-local mevedel-mentions--agent-enabled-p
                (not mevedel-view--side-conversation-p))
    (mevedel-mentions-install)
    (mevedel-view--install-dnd)
    (require 'mevedel-resource-capf)
    (add-hook 'completion-at-point-functions
              #'mevedel-resource-capf nil t)
    (unless mevedel-view--side-conversation-p
      (mevedel-view-history-load mevedel--session)
      (add-hook 'completion-at-point-functions
                #'mevedel-view-slash-capf nil t)
      (mevedel-skills-install-font-lock)
      (add-hook 'post-command-hook
                #'mevedel-view--refresh-skill-argument-hint nil t)
      (add-hook 'after-change-functions
                #'mevedel-view--refresh-skill-argument-hint-after-change
                nil t))
    (add-hook 'kill-buffer-hook
              #'mevedel-view--cancel-pending-skill-submission nil t)
    (setq mevedel-view--composer-keymap-overlay
          (make-overlay
           (mevedel-view--input-start) (point-max)
           (current-buffer) nil t))
    (overlay-put mevedel-view--composer-keymap-overlay
                 'keymap
                 (if mevedel-view--side-conversation-p
                     mevedel-view--side-conversation-keymap
                   mevedel-view--composer-keymap))))

;;
;;; Input forwarding

(defun mevedel-view--ensure-interactive-chat-view ()
  "Signal when the current view buffer is not an editable chat view."
  (when mevedel-view--agent-transcript-p
    (user-error "Agent transcript views are read-only")))

(defun mevedel-view--transcript-gptel-send-blocked (&optional _arg)
  "Block `gptel-send' from transcript inspection views."
  (interactive "P")
  (user-error "Agent transcript views are read-only"))


(defun mevedel-view--begin-external-turn
    (display-text data-turn-start &optional kind hook-context no-spinner)
  "Begin a view turn initiated outside the editable input.

DISPLAY-TEXT is shown as the user-side turn in the view.
DATA-TURN-START is the data-buffer marker where the assistant
response for this turn begins.  KIND may be `directive'.  HOOK-CONTEXT
is model-visible hook context to summarize in the view.  When
NO-SPINNER is non-nil, render only the local user turn."
  (mevedel-view--ensure-interactive-chat-view)
  (let ((turn-start (mevedel-view--insert-user-message
                     display-text kind hook-context)))
    (when (eq kind 'directive)
      (when-let* ((drawer (mevedel-view--external-prompt-drawer
                           data-turn-start)))
        (save-excursion
          (goto-char (mevedel-view--history-insertion-marker))
          (mevedel-view--with-render-boundaries-advancing
            (let ((inhibit-read-only t)
                  (start (point)))
              (mevedel-view--insert-rendered-tool
               (list :header "Prompt"
                     :body (plist-get drawer :body)
                     :body-mode 'markdown-mode
                     :vtype 'prompt-summary
                     :initially-collapsed-p t)
               (cons (plist-get drawer :start)
                     (plist-get drawer :end)))
              (add-text-properties
               start (point)
               `(read-only t
                 keymap ,mevedel-view--display-map
                 front-sticky (read-only keymap)
                 rear-nonsticky (read-only keymap)))
              (setq turn-start (copy-marker (point) nil)))))))
    (mevedel-view-stream-begin-turn
     turn-start data-turn-start no-spinner)))

(defun mevedel-view--external-prompt-drawer (data-turn-start)
  "Return the prompt drawer ending before DATA-TURN-START, if any."
  (when-let* (((markerp data-turn-start))
              (data-buf (marker-buffer data-turn-start))
              ((buffer-live-p data-buf)))
    (with-current-buffer data-buf
      (save-excursion
        (goto-char data-turn-start)
        (when (re-search-backward "^:PROMPT:\n" nil t)
          (let ((drawer-start (match-beginning 0))
                (body-start (match-end 0)))
            (when (re-search-forward "^:END:[ \t]*\n?"
                                     data-turn-start t)
              (list :start drawer-start
                    :end (match-end 0)
                    :body (buffer-substring-no-properties
                           body-start (match-beginning 0))))))))))

(defun mevedel-view--prompt-start-position ()
  "Return the start of the read-only input prompt, or nil."
  (let* ((marker-pos
          (and (markerp mevedel-view--input-marker)
               (eq (marker-buffer mevedel-view--input-marker)
                   (current-buffer))
               (marker-position mevedel-view--input-marker)))
         (pos
          (if (and marker-pos
                   (< marker-pos (point-max))
                   (get-text-property marker-pos 'mevedel-view-prompt))
              marker-pos
            (text-property-any
             (point-min) (point-max) 'mevedel-view-prompt t))))
    (when pos
      (while (and (> pos (point-min))
                  (get-text-property (1- pos) 'mevedel-view-prompt))
        (setq pos (1- pos)))
      pos)))

(defun mevedel-view--input-marker-position ()
  "Return the recovered start position of the input prompt.
When prompt text properties survive but zone markers have drifted past
that prompt into the editable composer, repair the marker ordering so
later prompt refreshes do not operate on the draft body."
  (if-let* ((prompt-start (mevedel-view--prompt-start-position)))
      (progn
        (when (and (markerp mevedel-view--input-marker)
                   (marker-buffer mevedel-view--input-marker)
                   (not (= (marker-position mevedel-view--input-marker)
                           prompt-start)))
          (set-marker mevedel-view--input-marker prompt-start))
        (dolist (marker (list mevedel-view--status-marker
                              mevedel-view--interaction-marker))
          (when (and (markerp marker)
                     (marker-buffer marker)
                     (let ((pos (marker-position marker)))
                       (and pos (> pos prompt-start))))
            (set-marker marker prompt-start)))
        prompt-start)
    (and (markerp mevedel-view--input-marker)
         (marker-position mevedel-view--input-marker))))

(defun mevedel-view--input-start ()
  "Return the buffer position where the user's editable input begins.
This is the position immediately after the read-only `> ' prompt that
follows `mevedel-view--input-marker'."
  (save-excursion
    (goto-char (or (mevedel-view--input-marker-position)
                   mevedel-view--input-marker))
    (while (get-text-property (point) 'mevedel-view-prompt)
      (forward-char 1))
    (point)))

(defun mevedel-view-refresh-input-prompt ()
  "Refresh the input prompt to reflect the current permission mode."
  (interactive)
  (unless mevedel-view--agent-transcript-p
    (when (and (markerp mevedel-view--input-marker)
               (marker-buffer mevedel-view--input-marker))
      (mevedel-view--call-preserving-input-point
       (lambda ()
         (let* ((start (mevedel-view--input-marker-position))
                (end (mevedel-view--input-start))
                (status-type
                 (and (markerp mevedel-view--status-marker)
                      (marker-insertion-type mevedel-view--status-marker)))
                (interaction-type
                 (and (markerp mevedel-view--interaction-marker)
                      (marker-insertion-type mevedel-view--interaction-marker)))
                (input-type (marker-insertion-type mevedel-view--input-marker)))
           (save-excursion
             (goto-char start)
             (unwind-protect
                 (let ((inhibit-read-only t))
                   (when (markerp mevedel-view--status-marker)
                     (set-marker-insertion-type
                      mevedel-view--status-marker nil))
                   (when (markerp mevedel-view--interaction-marker)
                     (set-marker-insertion-type
                      mevedel-view--interaction-marker nil))
                   (set-marker-insertion-type mevedel-view--input-marker nil)
                   (delete-region start end)
                   (insert (mevedel-view--input-prompt-string))
                   (add-text-properties
                    start (point)
                    `(read-only t
                      mevedel-view-prompt t
                      front-sticky (read-only mevedel-view-prompt)
                      rear-nonsticky
                      (read-only mevedel-view-prompt font-lock-face))))
               (when (markerp mevedel-view--status-marker)
                 (set-marker-insertion-type
                  mevedel-view--status-marker status-type))
               (when (markerp mevedel-view--interaction-marker)
                 (set-marker-insertion-type
                  mevedel-view--interaction-marker interaction-type))
               (set-marker-insertion-type
                mevedel-view--input-marker input-type)))
           (when (overlayp mevedel-view--composer-keymap-overlay)
             (move-overlay
              mevedel-view--composer-keymap-overlay
              (mevedel-view--input-start) (point-max)))))))))

(defun mevedel-view-cycle-permission-mode ()
  "Cycle the current session's permission mode from the view buffer."
  (interactive)
  (let* ((data-buf (and (boundp 'mevedel--data-buffer)
                        mevedel--data-buffer
                        (buffer-live-p mevedel--data-buffer)
                        mevedel--data-buffer))
         (session (or (and (boundp 'mevedel--session) mevedel--session)
                      (and data-buf
                           (buffer-local-value 'mevedel--session data-buf)))))
    (unless (and data-buf session)
      (user-error "No mevedel session for permission mode cycling"))
    (let ((next
           (mevedel-view--next-permission-mode
            (or (mevedel-session-permission-mode session) 'ask))))
      (with-current-buffer data-buf
        (require 'mevedel-permissions)
        (mevedel-permission-mode-transition next))
      (mevedel-view-refresh-input-prompt)
      (message "mevedel: permission mode %s"
               (car (mevedel-view--permission-mode-display next)))
      next)))

(defun mevedel-view-toggle-plan-mode ()
  "Toggle Plan for the current session without changing permissions."
  (interactive)
  (let* ((data-buf (and (boundp 'mevedel--data-buffer)
                        mevedel--data-buffer
                        (buffer-live-p mevedel--data-buffer)
                        mevedel--data-buffer))
         (session (or (and (boundp 'mevedel--session) mevedel--session)
                      (and data-buf
                           (buffer-local-value 'mevedel--session data-buf)))))
    (unless (and data-buf session)
      (user-error "No mevedel session for Plan mode"))
    (require 'mevedel-plan-mode)
    (if (mevedel-session-plan-mode session)
        (mevedel-plan-mode-exit session)
      (mevedel-plan-mode-enter session))
    (mevedel-view-refresh-input-prompt)
    (message "mevedel: Plan mode %s"
             (if (mevedel-session-plan-mode session) "on" "off"))
    (mevedel-session-plan-mode session)))

(defun mevedel-view--input-text ()
  "Return the user's composer text, trimmed."
  (require 'mevedel-mention-bindings)
  (let ((text (mevedel-mention-bindings-copy-text
               (buffer-substring
                (mevedel-view--input-start) (point-max)))))
    (string-trim text)))

(defun mevedel-view--bind-input-mentions (session)
  "Bind known mentions in the live composer for SESSION and return input.
The visible text is unchanged.  Binding before asynchronous preparation
means a failed attempt leaves the exact source attached for a retry."
  (require 'mevedel-mention-bindings)
  (let* ((input-start (mevedel-view--input-start))
         (raw-input
          (mevedel-mention-bindings-copy-text
           (buffer-substring input-start (point-max))))
         (bound-input
          (with-current-buffer mevedel--data-buffer
            (mevedel-mentions-prepare-user-input
             (mevedel-skills-prepare-user-input raw-input session)
             session))))
    (with-silent-modifications
      (remove-text-properties
       input-start (point-max)
       '(mevedel-mention-binding nil rear-nonsticky nil))
      (dolist (range (mevedel-mention-bindings-ranges bound-input))
        (mevedel-mention-bindings-set
         (+ input-start (plist-get range :start))
         (+ input-start (plist-get range :end))
         (plist-get range :binding))))
    (string-trim bound-input)))

(defun mevedel-view--composer-snapshot (session)
  "Capture the editable composer and pending grants for SESSION."
  (let ((start (mevedel-view--input-start)))
    (list :text (buffer-substring start (point-max))
          :point-offset (- (point) start)
          :dropped-file-grants
          (copy-sequence (mevedel-session-dropped-file-grants session)))))

(defun mevedel-view--restore-composer-snapshot (snapshot session &optional force)
  "Restore SNAPSHOT for SESSION when its visible draft is still current.
When FORCE is non-nil, replace the current draft unconditionally."
  (let* ((start (mevedel-view--input-start))
         (text (plist-get snapshot :text)))
    (when (or force
              (equal (buffer-substring-no-properties start (point-max))
                     (substring-no-properties text)))
      (let ((inhibit-read-only t))
        (with-silent-modifications
          (delete-region start (point-max))
          (goto-char start)
          (insert text)))
      (mevedel-session--set-dropped-file-grants
       session
       (copy-sequence
        (plist-get snapshot :dropped-file-grants)))
      (goto-char
       (min (point-max)
            (+ start (plist-get snapshot :point-offset))))
      t)))

(defun mevedel-view--composer-scope-key (&optional scope)
  "Return the draft key for SCOPE or ordinary chat."
  (if-let* ((scope (or scope mevedel-view--composer-scope)))
      (list (plist-get scope :directive-id)
            (plist-get scope :action)
            (plist-get scope :attempt-index))
    'chat))

(defun mevedel-view--switch-composer-scope (scope)
  "Switch to SCOPE, preserving separate composer drafts."
  (let ((session (mevedel-view--session)))
    (unless session
      (user-error "No active session for composer scope"))
    (puthash (mevedel-view--composer-scope-key)
             (mevedel-view--composer-snapshot session)
             mevedel-view--composer-drafts)
    (setq mevedel-view--composer-scope scope)
    (mevedel-view-refresh-input-prompt)
    (mevedel-view--restore-composer-snapshot
     (or (gethash (mevedel-view--composer-scope-key scope)
                  mevedel-view--composer-drafts)
         '(:text "" :point-offset 0 :dropped-file-grants nil))
     session t)
    (force-mode-line-update t)
    scope))

(defun mevedel-view-back-to-chat ()
  "Leave directive scope and restore the ordinary chat draft."
  (interactive)
  (when mevedel-view--composer-scope
    (mevedel-view--switch-composer-scope nil)
    (message "mevedel: composer returned to chat scope")))

(defun mevedel-view--directive-record (directive workspace)
  "Resolve DIRECTIVE to a workspace record and WORKSPACE."
  (require 'mevedel-overlays)
  (cond
   ((overlayp directive)
    (let* ((owner (mevedel--topmost-instruction directive 'directive))
           (workspace
            (mevedel--instruction-buffer-workspace (overlay-buffer owner))))
      (list (mevedel--directive-record owner) workspace)))
   ((and workspace (mevedel-directive-p directive)
         (memq directive (mevedel-workspace-directives workspace)))
    (list directive workspace))
   (t (user-error "No directive selected"))))

(defun mevedel-view-enter-directive-scope
    (directive action &optional attempt-index workspace)
  "Open DIRECTIVE's shared session view in sticky ACTION scope."
  (unless (memq action '(discuss plan request-changes retry))
    (error "Unknown directive composer action: %S" action))
  (pcase-let* ((`(,record ,workspace)
                (mevedel-view--directive-record directive workspace))
               (actions (mevedel-directive-actions record)))
    (unless (pcase action
              ('discuss
               (or (memq 'discuss actions)
                   (memq 'continue-discussion actions)
                   (memq 'discuss-result actions)))
              ('plan
               (memq (plist-get (mevedel-directive-plan record) :status)
                     '(draft proposed)))
              (_ (memq action actions)))
      (user-error "Directive action is not available: %s"
                  (mevedel--directive-action-label action)))
    (let* ((data-buffer
            (car (mevedel--directive-session-buffer record workspace)))
           (view-buffer
            (buffer-local-value 'mevedel--view-buffer data-buffer))
           (scope
            (list :directive-id (mevedel-directive-id record)
                  :action action
                  :attempt-index
                  (or attempt-index
                      (and (eq action 'discuss)
                           (memq 'discuss-result actions)
                           (length (mevedel-directive-attempts record))))
                  :record record
                  :workspace workspace)))
      (unless (buffer-live-p view-buffer)
        (error "Directive session has no live view"))
      (with-current-buffer view-buffer
        (mevedel-view--switch-composer-scope scope)
        (goto-char (point-max)))
      (pop-to-buffer view-buffer)
      view-buffer)))

(defun mevedel-view--clear-input ()
  "Clear the user's composer text, leaving the prompt in place."
  (mevedel-view--ensure-interactive-chat-view)
  (when-let* ((session (mevedel-view--session)))
    (mevedel-session-clear-dropped-file-grants session))
  (delete-region (mevedel-view--input-start) (point-max))
  (mevedel-view--delete-skill-argument-hint))

(defun mevedel-view--session ()
  "Return the session associated with the current view buffer."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-view--pending-follow-ups (&optional session)
  "Return SESSION's pending follow-ups."
  (when-let* ((sess (or session (mevedel-view--session))))
    (mevedel-session-pending-follow-ups sess)))

(defun mevedel-view--set-pending-follow-ups (entries &optional session)
  "Set SESSION's pending follow-up ENTRIES."
  (when-let* ((sess (or session (mevedel-view--session))))
    (mevedel-session-set-pending-inputs sess 'follow-up entries)))

(defun mevedel-view--mentioned-file-paths (input)
  "Return expanded @file paths mentioned in INPUT."
  (require 'mevedel-mentions)
  (mevedel-mentions-file-paths-in-text input))

(defun mevedel-view--pop-dropped-file-grants-for-input (input session)
  "Consume SESSION's pending drag/drop grants referenced by INPUT."
  (when session
    (mevedel-session-pop-dropped-file-grants
     session
     (mevedel-view--mentioned-file-paths input))))

(defun mevedel-view--activate-dropped-file-grants (paths session)
  "Activate exact-file drag/drop grant PATHS for SESSION."
  (when (and session paths)
    (mevedel-session-activate-dropped-file-grants session paths)))

(defun mevedel-view--follow-up-auto-drain-blocked-p (&optional session)
  "Return non-nil when SESSION follow-ups should wait for user action."
  (when-let* ((sess (or session (mevedel-view--session))))
    (or (mevedel-session-pending-input-delivery-paused-p sess)
        (mevedel-session-pending-plan-approval sess)
        (when-let* ((workflow (mevedel-session-directive-planning sess)))
          (not
           (cl-find-if
            (lambda (entry)
              (let ((scope (plist-get entry :scope)))
                (and (eq (plist-get scope :action) 'plan)
                     (equal (plist-get scope :directive-id)
                            (plist-get workflow :directive-id)))))
            (mevedel-session-pending-follow-ups sess))))
        (plist-get (mevedel-session-plan-metadata sess)
                   :implementation-retry)
        (mevedel-view--reserved-goal-handoff-id sess)
        (when-let* ((goal (mevedel-session-goal sess))
                    ((memq (mevedel-goal-status goal)
                           '(paused blocked budget-limited)))
                    (entry (car (mevedel-session-pending-follow-ups sess))))
          (equal (mevedel-goal-id goal)
                 (plist-get entry :queued-at-goal-id))))))

(defun mevedel-view--reserved-goal-handoff-id (&optional session)
  "Return SESSION's Goal handoff reservation, or nil."
  (require 'mevedel-plan-handoff)
  (mevedel-plan-handoff-reserved-goal-id
   (or session (mevedel-view--session))))

(defun mevedel-view--occupied-root-workflow-p (session)
  "Return non-nil when SESSION owns work that makes later input a follow-up."
  (or (mevedel-session-pending-plan-approval session)
      (mevedel-session-directive-planning session)
      (plist-get (mevedel-session-plan-metadata session)
                 :implementation-retry)
      (mevedel-view--reserved-goal-handoff-id session)
      (when-let* ((goal (mevedel-session-goal session)))
        (not (eq (mevedel-goal-status goal) 'complete)))))

(defun mevedel-view--steerable-root-request-p (request)
  "Return non-nil when REQUEST is an ordinary root provider turn."
  (and request (mevedel-request-fsm request)))

(defun mevedel-view--pending-input-preview (input)
  "Return a one-line preview for pending INPUT."
  (let ((preview (string-trim
                  (replace-regexp-in-string "[ \t\n\r]+" " " input t t))))
    (if (> (length preview) 96)
        (concat (substring preview 0 93) "...")
      preview)))

(defun mevedel-view--pending-input-text (entry)
  "Return normalized input text for pending ENTRY."
  (mevedel--normalize-message-text (or (plist-get entry :input) "")))

(defvar-keymap mevedel-view--pending-inputs-map
  :doc "Keymap on the pending-input summary."
  "RET" #'mevedel-pending-inputs-open
  "<return>" #'mevedel-pending-inputs-open
  "<mouse-1>" #'mevedel-pending-inputs-open
  "<mouse-2>" #'mevedel-pending-inputs-open)

(defun mevedel-view--pending-input-category-body (label entries)
  "Return compact pending-input summary for LABEL and ENTRIES."
  (let ((index 0)
        lines)
    (dolist (entry (seq-take entries 3))
      (cl-incf index)
      (push (format "  %d. %s"
                    index
                    (concat
                     (when-let* ((scope (plist-get entry :scope)))
                       (format "[◆ %s] "
                               (mevedel-view-composer-scope-label scope)))
                     (mevedel-view--pending-input-preview
                      (mevedel-view--pending-input-text entry))))
            lines))
    (when (> (length entries) 3)
      (push (format "  %d more" (- (length entries) 3)) lines))
    (concat "\n" label "\n"
            (string-join (nreverse lines) "\n")
            "\n")))

(defun mevedel-view--pending-inputs-body (session)
  "Return the main-view pending-input summary for SESSION."
  (concat
   (when (mevedel-session-pending-input-failure-paused session)
     "\nPending-input delivery stopped after turn failure; review required\n")
   (when (mevedel-session-pending-input-paused session)
     "\nPending-input delivery paused\n")
   (when-let* ((entries (mevedel-session-pending-steering session)))
     (mevedel-view--pending-input-category-body "Steering" entries))
   (when-let* ((entries (mevedel-session-pending-follow-ups session)))
     (mevedel-view--pending-input-category-body "Follow-ups" entries))
   "\nRET or C-c C-e manage pending inputs\n"))

(defun mevedel-view--pending-inputs-render (&optional session)
  "Render SESSION pending input into the interaction zone."
  (when-let* ((session (or session (mevedel-view--session))))
    (let ((entries
           (append (mevedel-session-pending-steering session)
                   (mevedel-session-pending-follow-ups session))))
      (when (or entries
                (mevedel-session-pending-input-failure-paused session))
        (mevedel-view--interaction-register
         (list :kind 'pending-input
               :id 'pending-inputs
               :count (length entries)
               :body (mevedel-view--pending-inputs-body session)
               :keymap mevedel-view--pending-inputs-map
               :help-echo "Open Pending Inputs cockpit"))))))

(defun mevedel-view--queue-follow-up (input)
  "Queue INPUT to start a separate root turn."
  (setq input (mevedel--normalize-message-text input))
  (let ((session (mevedel-view--session)))
    (unless session
      (user-error "No active session for follow-up"))
    (let* ((dropped-file-grants
            (mevedel-view--pop-dropped-file-grants-for-input input session))
           (entry
            (mevedel-session-enqueue-pending-input
             session 'follow-up
             (list :input input
                   :scope (mevedel-view--queued-scope)
                   :dropped-file-grants dropped-file-grants
                   :queued-at-time (float-time)
                   :queued-at-goal-id
                   (or (and (mevedel-session-goal session)
                            (mevedel-goal-id
                             (mevedel-session-goal session)))
                       (mevedel-view--reserved-goal-handoff-id session))
                   :queued-at-turn
                   (or (mevedel-session-turn-count session) 0)))))
      (when (fboundp 'mevedel-telemetry-record)
        (mevedel-telemetry-record
         session 'user-message-queued
         :message-hash (secure-hash 'sha256 input)
         :message-chars (length input)
         :queue-depth (length (mevedel-view--pending-follow-ups session))
         :enqueue-goal-id (plist-get entry :queued-at-goal-id)))
      (mevedel-view-history-add input)
      (when (equal-including-properties (mevedel-view--input-text) input)
        (mevedel-view--clear-input))
      (mevedel-view--interaction-rebuild)
      (message "mevedel: queued follow-up for a separate turn")
      (mevedel-view--schedule-late-follow-up-drain)
      entry)))

(defun mevedel-view--steering-validation-expansion (text session)
  "Expand TEXT for steering validation without committing its effects."
  (let* ((paths (mevedel-view--mentioned-file-paths text))
         (pending (mevedel-session-dropped-file-grants session))
         (temporary-grants (cl-intersection paths pending :test #'equal))
         (active (mevedel-session-active-dropped-file-grants session)))
    (unwind-protect
        (progn
          (mevedel-session--set-active-dropped-file-grants
           session (append temporary-grants active))
          (with-current-buffer mevedel--data-buffer
            (require 'mevedel-mentions)
            (mevedel-mentions-expand-user-input text session)))
      (mevedel-session--set-active-dropped-file-grants session active))))

(defun mevedel-view--prepare-steering-entry (submission request)
  "Return a validated steering entry for SUBMISSION and REQUEST.
Return nil and leave the submission pending when the live request contract no
longer accepts the prepared input."
  (let* ((session (mevedel-view--session))
         (outcome (mevedel-prompt-submission-outcome submission))
         (request-context (plist-get outcome :request-context))
         (model-input (plist-get outcome :model-input))
         (fsm (and request (mevedel-request-fsm request)))
         (current-request
          (and (buffer-live-p mevedel--data-buffer)
               (buffer-local-value 'mevedel--current-request
                                   mevedel--data-buffer))))
    (cond
     ((plist-get outcome :fork-outcome)
      (message "mevedel: fork skills cannot steer; use C-c TAB")
      nil)
     ((not (mevedel-view--steering-request-context-supported-p
            request-context))
      (message "mevedel: skill policy cannot steer; use C-c TAB")
      nil)
     ((or (not (eq request current-request))
          (not fsm)
          (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT)))
      (message "mevedel: request can no longer be steered; use C-c TAB")
      nil)
     (t
      (let ((expansion
             (mevedel-view--steering-validation-expansion
              model-input session)))
        (if (plist-get expansion :media-contexts)
            (progn
              (message
               "mevedel: media cannot steer an active request; use C-c TAB")
              nil)
          (let* ((input
                  (mevedel-prompt-submission-display-text submission))
                 (dropped-file-grants
                  (mevedel-view--pop-dropped-file-grants-for-input
                   input session)))
            (mevedel-prompt-submission-reserve submission)
            (list
             :input input
             :model-input model-input
             :transcript-payload
             (concat (plist-get outcome :transcript-input)
                     (or (plist-get outcome :render-data) ""))
             :hook-audits (plist-get outcome :hook-audits)
             :request-context request-context
             :submission submission
             :dropped-file-grants dropped-file-grants
             :request-id (mevedel-request-id request)
             :queued-at-time (float-time)
             :queued-at-turn
             (or (mevedel-session-turn-count session) 0)))))))))

(defun mevedel-view--queue-prepared-steering (submission request)
  "Queue accepted prompt SUBMISSION as steering for REQUEST."
  (when-let* ((prepared
               (mevedel-view--prepare-steering-entry submission request))
              (session (mevedel-view--session))
              (entry
               (mevedel-session-enqueue-pending-input
                session 'steering prepared)))
    (let ((input (plist-get entry :input)))
      (mevedel-view-history-add input)
      (when (equal-including-properties
             (mevedel-view--input-text) input)
        (mevedel-view--clear-input))
      (mevedel-view--interaction-rebuild)
      (require 'mevedel-agent-control)
      (when (mevedel-agent-control-root-waiting-p session)
        (mevedel-agent-control-wake-root-user session))
      (message "mevedel: queued steering for this turn")
      entry)))

(defun mevedel-view--delete-skill-argument-hint ()
  "Remove the composer skill argument hint overlay."
  (when (overlayp mevedel-view--skill-argument-hint-overlay)
    (delete-overlay mevedel-view--skill-argument-hint-overlay))
  (setq mevedel-view--skill-argument-hint-overlay nil))

(defun mevedel-view--skill-argument-hint ()
  "Return display-only skill argument hint for the current composer."
  (when-let* ((session (mevedel-view--session))
              (input-start (and (markerp mevedel-view--input-marker)
                                (marker-buffer mevedel-view--input-marker)
                                (mevedel-view--input-start)))
              ((>= (point) input-start))
              (text (buffer-substring-no-properties input-start (point-max)))
              (parsed (mevedel-skills--parse-skill-line text))
              (name (nth 0 parsed))
              (skill (mevedel-session-get-skill session name))
              ((mevedel-skill-user-invocable-p skill)))
    (mevedel-skills--remaining-argument-hint skill (nth 1 parsed))))

(defun mevedel-view--refresh-skill-argument-hint ()
  "Refresh the display-only skill argument hint in the composer."
  (let* ((input-marker-pos (and (markerp mevedel-view--input-marker)
                                (marker-buffer mevedel-view--input-marker)
                                (marker-position mevedel-view--input-marker)))
         (marker-at-prompt-p
          (and input-marker-pos
               (< input-marker-pos (point-max))
               (get-text-property input-marker-pos 'mevedel-view-prompt))))
    (cond
     ((or mevedel-view--agent-transcript-p
          (not input-marker-pos))
      (mevedel-view--delete-skill-argument-hint))
     ((and marker-at-prompt-p (< (point) input-marker-pos))
      (mevedel-view--delete-skill-argument-hint))
     ((< (point) (mevedel-view--input-start))
      (mevedel-view--delete-skill-argument-hint))
     (t
      (let ((hint (mevedel-view--skill-argument-hint)))
        (if (and hint (not (string-empty-p hint)))
            (progn
              (unless (overlayp mevedel-view--skill-argument-hint-overlay)
                (setq mevedel-view--skill-argument-hint-overlay
                      (make-overlay (point) (point) (current-buffer) nil t))
                (overlay-put mevedel-view--skill-argument-hint-overlay
                             'priority 10))
              (move-overlay mevedel-view--skill-argument-hint-overlay
                            (point) (point) (current-buffer))
              (overlay-put
               mevedel-view--skill-argument-hint-overlay
               'after-string
               (propertize (concat " " hint) 'font-lock-face 'shadow)))
          (mevedel-view--delete-skill-argument-hint)))))))

(defun mevedel-view--refresh-skill-argument-hint-after-change
    (start end _old-length)
  "Invalidate edited bindings and refresh the composer skill hint."
  (when (and (markerp mevedel-view--input-marker)
             (marker-buffer mevedel-view--input-marker)
             (>= start (mevedel-view--input-start)))
    (require 'mevedel-mention-bindings)
    (mevedel-mention-bindings-invalidate-edit
     start end (mevedel-view--input-start) (point-max)))
  (mevedel-view--refresh-skill-argument-hint))

(defun mevedel-view-slash-capf ()
  "Completion-at-point for slash commands and `$' skills in the composer.
Offers local slash commands at `/name', session skills at `$name',
and command argument completion for commands with finite choices."
  (when (and mevedel--data-buffer
             (buffer-live-p mevedel--data-buffer)
             (>= (point) (mevedel-view--input-start)))
    (let ((session (buffer-local-value 'mevedel--session
                                       mevedel--data-buffer)))
      (mevedel-skills--slash-capf
       mevedel--data-buffer session mevedel-slash-commands
       (mevedel-view--input-start)))))

(defun mevedel-view--start-fork-skill-turn
    (input display-text &optional hook-context)
  "Render and record a fork skill INPUT without calling `gptel-send'.

DISPLAY-TEXT is shown in the view for the user turn.  INPUT is written
to the data buffer as the authoritative user prompt.  The data-turn
marker is anchored after that prompt so the eventual fork result can be
rendered by the normal post-response hook.  HOOK-CONTEXT is summarized
in the view when present."
  (let ((view-turn-start
         (mevedel-view--insert-user-message display-text nil hook-context)))
    (mevedel-view--clear-input)
    (with-current-buffer mevedel--data-buffer
      (when mevedel--session
        (mevedel-request-begin
         mevedel--session
         (and (boundp 'mevedel--current-directive-uuid)
              mevedel--current-directive-uuid)))
      (goto-char (point-max))
      (let ((user-turn-start (point)))
        (insert gptel-response-separator)
        (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
          (let ((prefix-length (length prefix)))
            (unless (and (>= (point) (+ (point-min) prefix-length))
                         (string= (buffer-substring-no-properties
                                   (- (point) prefix-length) (point))
                                  prefix))
              (unless (bolp) (insert "\n"))
              (insert prefix))))
        (insert input "\n")
        (mevedel--clear-user-turn-gptel-properties user-turn-start (point)))
      (let ((data-turn-start (copy-marker (point) nil)))
        (with-current-buffer mevedel--view-buffer
          (mevedel-view-stream-begin-turn
           view-turn-start data-turn-start))))))

(defun mevedel-view--finish-fork-skill-outcome
    (name outcome view-buffer data-buffer &optional skill)
  "Handle fork skill OUTCOME for NAME."
  (when (and (buffer-live-p view-buffer)
             (buffer-live-p data-buffer))
    (when (and (fboundp 'mevedel-review-command-skill-p)
               (mevedel-review-command-skill-p skill)
               (fboundp 'mevedel-review--mark-command-outcome))
      (setq outcome (mevedel-review--mark-command-outcome outcome)))
    (when (fboundp 'mevedel-review-transform-outcome)
      (setq outcome (mevedel-review-transform-outcome name outcome)))
    (pcase (plist-get outcome :status)
      ('ok
       (pcase (plist-get outcome :kind)
         ('fork
          (with-current-buffer data-buffer
            (mevedel-skills--insert-fork-result outcome)))
         (_
          (message "Skill '%s' returned unsupported outcome: %S"
                   name outcome))))
      (_
       (with-current-buffer view-buffer
         (mevedel-view--stop-request-progress)
         (message "Skill '%s' failed: %s"
                  name
                  (or (plist-get outcome :message)
                      "unknown error")))
       (with-current-buffer data-buffer
         (when (bound-and-true-p mevedel--current-request)
           (mevedel-request-end))
         (gptel--update-status " Ready" 'success))))))

(defun mevedel-view--skill-submission-active-p
    (token view-buffer data-buffer)
  "Return non-nil when TOKEN still owns a live skill submission."
  (and (buffer-live-p view-buffer)
       (buffer-live-p data-buffer)
       (not (plist-get token :cancelled))
       (with-current-buffer view-buffer
         (eq mevedel-view--pending-skill-submission token))))

(defun mevedel-view--finish-skill-submission (token)
  "Clear TOKEN when it still owns the current skill submission."
  (when (eq mevedel-view--pending-skill-submission token)
    (setq mevedel-view--pending-skill-submission nil)))

(defun mevedel-view--prepared-fork-outcome (prepared)
  "Return PREPARED's fork command outcome, or nil."
  (when-let* ((pair
               (cl-find-if
                (lambda (item)
                  (eq (plist-get (plist-get item :outcome) :kind) 'fork))
                (plist-get prepared :prepared-entries))))
    (plist-get pair :outcome)))

(defun mevedel-view--block-planned-submission (submission &optional prepared)
  "Finish SUBMISSION as blocked, optionally reporting failed PREPARED work."
  (let ((token (plist-get submission :token))
        (on-block (plist-get submission :on-block)))
    (mevedel-view--finish-skill-submission token)
    (when (and prepared
               (not (eq (plist-get prepared :reason) 'cancelled)))
      (message "mevedel: skill $%s failed: %s"
               (or (plist-get prepared :name) "unknown")
               (or (plist-get prepared :message) "unknown error")))
    (when on-block
      (funcall on-block))))

(defun mevedel-view--prepared-plan-outcome
    (submission prepared hook-input hook-context hook-audits)
  "Return the structured prepared outcome for SUBMISSION.
PREPARED is the skill planner result.  HOOK-INPUT, HOOK-CONTEXT, and
HOOK-AUDITS are the accepted `UserPromptSubmit' result."
  (let* ((plan (plist-get submission :plan))
         (input (plist-get submission :input))
         (prepared-input (plist-get prepared :model-input))
         (rewrite-preserves-plan-p
          (string-search prepared-input hook-input))
         (hook-input (if rewrite-preserves-plan-p
                         hook-input
                       prepared-input))
         (hook-audits (and rewrite-preserves-plan-p hook-audits))
         (model-input
          (if hook-context
              (concat hook-input "\n\n" hook-context)
            hook-input)))
    (list :model-input
          model-input
          :transcript-input
          (if hook-context
              (concat input "\n\n" hook-context)
            input)
          :hook-input hook-input
          :hook-context hook-context
          :hook-audits
          (append (plist-get prepared :hook-audits) hook-audits)
          :request-context (plist-get prepared :request-context)
          :render-data (mevedel-skills-plan-render-data plan hook-input)
          :fork-outcome
          (and (mevedel-skill-invocation-plan-fork-p plan)
               (mevedel-view--prepared-fork-outcome prepared)))))

(cl-defun mevedel-view--dispatch-prepared-outcome
    (submission data-buffer &key before-send after-insert on-block dispatch)
  "Dispatch accepted prompt SUBMISSION through DATA-BUFFER.
BEFORE-SEND runs at the dispatch boundary.  AFTER-INSERT runs once the prompt
is durably recorded.  ON-BLOCK runs after a dispatch error.  DISPATCH, when
non-nil, receives SUBMISSION instead of starting a request."
  (let* ((view-buffer (current-buffer))
         (outcome (mevedel-prompt-submission-outcome submission))
         (input (mevedel-prompt-submission-display-text submission))
         (model-input (plist-get outcome :model-input))
         (transcript-input (plist-get outcome :transcript-input))
         (hook-input (plist-get outcome :hook-input))
         (hook-context (plist-get outcome :hook-context))
         (all-audits (plist-get outcome :hook-audits))
         (request-context (plist-get outcome :request-context))
         (render-data (or (plist-get outcome :render-data) ""))
         (fork-outcome (plist-get outcome :fork-outcome))
         (view-context
          (and (not dispatch)
               (mevedel-view--join-hook-contexts
                (mevedel-hooks-format-context
                 (mevedel-view--hook-context-events-from-text hook-input))
                hook-context))))
    (condition-case err
        (progn
          (when before-send
            (funcall before-send))
          (cond
           (dispatch
            (funcall dispatch submission))
           (fork-outcome
            (let* ((skill (plist-get fork-outcome :skill))
                   (name (mevedel-skill-name skill)))
              (mevedel-view--start-fork-skill-turn
               (concat transcript-input render-data) input view-context)
              (mevedel-prompt-submission-commit submission)
              (when after-insert
                (funcall after-insert))
              (with-current-buffer data-buffer
                (mevedel-skills-dispatch-prepared-fork
                 fork-outcome
                 (lambda (result)
                   (mevedel-view--finish-fork-skill-outcome
                    name result view-buffer data-buffer skill))
                 :prompt model-input
                 :request-context request-context
                 :hook-audits all-audits))))
           (t
            (with-current-buffer data-buffer
              (setq-local mevedel-skills--pending-request-context
                          request-context))
            (mevedel-view--forward-input
             (concat transcript-input render-data)
             :display-text input
             :prompt-checked t
             :submission submission
             :after-insert after-insert
             :model-input (concat model-input render-data)))))
      (error
       (when (buffer-live-p data-buffer)
         (with-current-buffer data-buffer
           (setq-local mevedel-skills--pending-request-context nil)))
       (message "mevedel: skill dispatch failed: %s"
                (error-message-string err))
       (when on-block
         (funcall on-block))
       nil))))

(defun mevedel-view--dispatch-prepared-plan
    (plan-submission prepared prompt-submission)
  "Dispatch PREPARED plan for PLAN-SUBMISSION and PROMPT-SUBMISSION."
  (let* ((token (plist-get plan-submission :token))
         (view-buffer (plist-get plan-submission :view-buffer))
         (data-buffer (plist-get plan-submission :data-buffer)))
    (when (mevedel-view--skill-submission-active-p
           token view-buffer data-buffer)
      (with-current-buffer view-buffer
        (mevedel-view--finish-skill-submission token)
        (let* ((hook-input
                (mevedel-prompt-submission-input prompt-submission))
               (hook-context
                (mevedel-prompt-submission-context prompt-submission))
               (hook-audits
                (mevedel-prompt-submission-audits prompt-submission))
               (outcome
                (mevedel-view--prepared-plan-outcome
                 plan-submission prepared hook-input hook-context hook-audits)))
          (mevedel-prompt-submission-set-outcome
           prompt-submission outcome)
          (when-let* ((warnings (plist-get prepared :warnings)))
            (message "mevedel: %s" (string-join warnings "; ")))
          (mevedel-view--dispatch-prepared-outcome
           prompt-submission data-buffer
           :before-send (plist-get plan-submission :before-send)
           :after-insert (plist-get plan-submission :after-insert)
           :on-block (plist-get plan-submission :on-block)
           :dispatch (plist-get plan-submission :dispatch)))))))

(defun mevedel-view--handle-prepared-plan (submission prepared)
  "Continue SUBMISSION after PREPARED skill work settles."
  (let ((token (plist-get submission :token))
        (view-buffer (plist-get submission :view-buffer))
        (data-buffer (plist-get submission :data-buffer)))
    (when (mevedel-view--skill-submission-active-p
           token view-buffer data-buffer)
      (with-current-buffer view-buffer
        (if (not (eq (plist-get prepared :status) 'ok))
            (mevedel-view--block-planned-submission submission prepared)
          (mevedel-view--run-prompt-submit-hook
           (plist-get prepared :model-input)
           (plist-get submission :input)
           (lambda (prompt-submission)
             (mevedel-view--dispatch-prepared-plan
              submission prepared prompt-submission))
           (lambda ()
             (when (mevedel-view--skill-submission-active-p
                    token view-buffer data-buffer)
               (with-current-buffer view-buffer
                 (mevedel-view--block-planned-submission submission))))
           (plist-get prepared :hook-context)))))))

(defun mevedel-view--submit-planned-input
    (input &optional before-send on-block dispatch after-insert)
  "Plan, prepare, and submit atomically bound user INPUT.

BEFORE-SEND runs exactly once at the dispatch boundary.  ON-BLOCK runs when
planning, preparation, or `UserPromptSubmit' rejects the submission.  Derived
skill bodies and hook output are never scanned for additional invocations.
When DISPATCH is non-nil, call it with an accepted prompt submission instead of
starting a new request.  AFTER-INSERT runs once the prompt is durably recorded."
  (let ((view-buffer (current-buffer))
        (data-buffer mevedel--data-buffer)
        (session (mevedel-view--session)))
    (require 'mevedel-skills-plan)
    (let* ((plan
            (with-current-buffer data-buffer
              (mevedel-skills-refresh-bound-input input session)
              (mevedel-skills-plan-user-input input session))))
      (if (null (mevedel-skill-invocation-plan-occurrences plan))
          (if dispatch
              (mevedel-view--run-prompt-submit-hook
               input input
               (lambda (prompt-submission)
                 (let* ((hook-input
                         (mevedel-prompt-submission-input
                          prompt-submission))
                        (hook-context
                         (mevedel-prompt-submission-context
                          prompt-submission))
                        (prepared-input
                         (if hook-context
                             (concat hook-input "\n\n" hook-context)
                           hook-input)))
                   (mevedel-prompt-submission-set-outcome
                    prompt-submission
                    (list :model-input prepared-input
                          :transcript-input prepared-input
                          :hook-input hook-input
                          :hook-context hook-context
                          :hook-audits
                          (mevedel-prompt-submission-audits
                           prompt-submission)))
                   (mevedel-view--dispatch-prepared-outcome
                    prompt-submission data-buffer
                    :before-send before-send
                    :after-insert after-insert
                    :on-block on-block
                    :dispatch dispatch)))
               on-block)
            (mevedel-view--forward-input
             input :before-send before-send :after-insert after-insert
             :on-block on-block))
        (let* ((token (list :cancelled nil))
               (submission
                (list :token token
                      :input input
                      :plan plan
                      :view-buffer view-buffer
                      :data-buffer data-buffer
                      :before-send before-send
                      :after-insert after-insert
                      :dispatch dispatch
                      :on-block on-block)))
          (setq mevedel-view--pending-skill-submission token)
          (with-current-buffer data-buffer
            (mevedel-skills-plan-prepare
             plan
             (lambda (prepared)
               (mevedel-view--handle-prepared-plan submission prepared))
             (lambda ()
               (not (mevedel-view--skill-submission-active-p
                     token view-buffer data-buffer))))))))))

(defun mevedel-view--steering-request-context-supported-p (context)
  "Return non-nil when prepared skill CONTEXT can steer an active request."
  (cl-loop for (key value) on context by #'cddr
           always
           (pcase key
             (:invoked-skills t)
             ((or :permission-rules :hook-rules :model :effort)
              (null value))
             (_ nil))))

(defun mevedel-view-send-follow-up ()
  "Queue the composer as a follow-up, or send normally while idle."
  (interactive)
  (mevedel-view--ensure-interactive-chat-view)
  (when mevedel-view--side-conversation-p
    (user-error "/btw does not queue follow-ups; wait for the active response"))
  (mevedel-view--assert-live-tip)
  (when mevedel-view--pending-input-edit
    (user-error "Save or cancel the pending-input edit first"))
  (unless (and mevedel--data-buffer (buffer-live-p mevedel--data-buffer))
    (user-error "No live data buffer associated with this view"))
  (let* ((session (buffer-local-value 'mevedel--session
                                      mevedel--data-buffer))
         (occupied
          (or (buffer-local-value 'mevedel--current-request
                                  mevedel--data-buffer)
              (mevedel-session-pending-follow-ups session)
              (mevedel-view--occupied-root-workflow-p session)
              mevedel-view--prompt-hook-pending
              mevedel-view--pending-skill-submission
              (buffer-local-value 'mevedel--compaction-in-flight
                                  mevedel--data-buffer))))
    (if (not occupied)
        (mevedel-view-send)
      (when (buffer-local-value 'mevedel-session--read-only-mode
                                mevedel--data-buffer)
        (user-error "Session is open read-only (another host holds the lock)"))
      (let ((input (if mevedel-view--composer-scope
                       (mevedel-view--input-text)
                     (mevedel-view--bind-input-mentions session))))
        (when (string-empty-p input)
          (user-error "Nothing to send"))
        (when (mevedel-skills--parse-slash-line input)
          (user-error "Slash commands cannot be queued as follow-ups"))
        (mevedel-view--queue-follow-up input))))
  (goto-char (point-max)))

(defun mevedel-view--submit-armed-session-fork
    (source-view input target snapshot)
  "Publish TARGET from SOURCE-VIEW, then submit INPUT in its Child.
SNAPSHOT is the exact Source composer state transferred on publication."
  (let* ((fork-type (plist-get target :fork-type))
         (source-data
          (buffer-local-value 'mevedel--data-buffer source-view))
         (referenced-grants
          (cl-intersection
           (mevedel-view--mentioned-file-paths input)
           (plist-get snapshot :dropped-file-grants)
           :test #'equal))
         (child-data
          (progn
            (require 'mevedel-session-persistence)
            (pcase fork-type
              ('conversation
               (mevedel-session-persistence-conversation-fork
                source-data target))
              ('worktree
               (mevedel-session-persistence-worktree-fork
                source-data target))
              (_
               (error "Unknown session fork type: %S" fork-type)))))
         (child-view
          (buffer-local-value 'mevedel--view-buffer child-data))
         (child-session
          (buffer-local-value 'mevedel--session child-data)))
    (unless (buffer-live-p child-view)
      (error "Session Fork has no live view"))
    (with-current-buffer source-view
      (mevedel-view--full-rerender))
    (setq snapshot (copy-tree snapshot t))
    (when (eq fork-type 'worktree)
      (setq input
            (mevedel-view--retarget-worktree-mention-bindings
             input child-session)
            referenced-grants
            (mapcar
             (lambda (path)
               (mevedel-session-persistence--retarget-worktree-path
                child-session path))
             referenced-grants))
      (plist-put
       snapshot :text
       (mevedel-view--retarget-worktree-mention-bindings
        (plist-get snapshot :text) child-session)))
    (plist-put snapshot :dropped-file-grants referenced-grants)
    (with-current-buffer child-view
      (mevedel-view--restore-composer-snapshot
       snapshot child-session t))
    (with-current-buffer source-view
      (mevedel-view--clear-input)
      (mevedel-view-cancel-session-fork))
    (display-buffer child-view)
    (with-current-buffer child-view
      (mevedel-view--submit-planned-input
       input nil nil nil
       (lambda () (mevedel-view-history-add input))))
    child-data))

(defun mevedel-view--retarget-worktree-mention-bindings (text session)
  "Retarget repository-local mention bindings in TEXT for SESSION."
  (require 'mevedel-mention-bindings)
  (let ((copy (mevedel-mention-bindings-copy-text text)))
    (dolist (range (mevedel-mention-bindings-ranges copy))
      (let* ((binding (copy-tree (plist-get range :binding) t))
             (key
              (pcase (plist-get binding :kind)
                ('file :path)
                ('skill :source-file))))
        (when key
          (plist-put
           binding key
           (mevedel-session-persistence--retarget-worktree-path
            session (plist-get binding key)))
          (mevedel-mention-bindings-set
           (plist-get range :start)
           (plist-get range :end)
           binding copy))))
    copy))

(defun mevedel-view--queued-scope (&optional scope)
  "Return the serializable queue identity for directive SCOPE."
  (when-let* ((scope (or scope mevedel-view--composer-scope)))
    (list :directive-id (plist-get scope :directive-id)
          :action (plist-get scope :action)
          :attempt-index (plist-get scope :attempt-index))))

(defun mevedel-view--dispatch-directive-input (scope input)
  "Dispatch directive-scoped INPUT according to SCOPE."
  (let* ((session (mevedel-view--session))
         (workspace (mevedel-session-workspace session))
         (id (plist-get scope :directive-id))
         (record
          (or (and (mevedel-directive-p (plist-get scope :record))
                   (plist-get scope :record))
              (cl-find id (mevedel-workspace-directives workspace)
                       :key #'mevedel-directive-id :test #'equal))))
    (unless record
      (user-error "Directive no longer exists: %s" id))
    (let ((directive
           (plist-get
            (mevedel--directive-action-context record workspace)
            :directive)))
      (pcase (plist-get scope :action)
        ('discuss
         (mevedel--discuss-directive-turn
          directive input (plist-get scope :attempt-index)))
        ('plan
         (require 'mevedel-directive-plan)
         (mevedel-directive-plan-continue directive input))
        ('request-changes
         (mevedel--request-directive-changes directive input))
        ('retry
         (mevedel--retry-directive directive input))
        (_ (error "Unknown directive composer action: %S"
                  (plist-get scope :action)))))))

(defun mevedel-view--send-directive-input (input)
  "Send INPUT in the current sticky directive scope."
  (let ((session (mevedel-view--session)))
    (when (or (buffer-local-value 'mevedel--current-request
                                  mevedel--data-buffer)
              (and (mevedel-view--occupied-root-workflow-p session)
                   (not (eq (plist-get mevedel-view--composer-scope :action)
                            'plan))))
      (user-error "The workflow is occupied -- use C-c TAB to queue this directive follow-up"))
    (when (mevedel-skills--parse-slash-line input)
      (user-error "Slash commands are unavailable in directive scope"))
    (mevedel-view--dispatch-directive-input mevedel-view--composer-scope input)
    (mevedel-view-history-add input)
    (mevedel-view--clear-input)))

(defun mevedel-view-send ()
  "Send the current root or ephemeral side-conversation composer text."
  (interactive)
  (if mevedel-view--side-conversation-p
      (progn
        (require 'mevedel-side-conversation)
        (mevedel-side-conversation-send))
    (mevedel-view--send-root)))

(defun mevedel-view--send-root ()
  "Send the current composer text to the LLM via the data buffer.
Extracts text from the input zone, plans all bound `$skill' mentions,
renders the original text in the history region, and dispatches either
one coherent request or one leading fork command.  Slash commands retain
their local dispatch path."
  (mevedel-view--ensure-interactive-chat-view)
  (mevedel-view--assert-live-tip t)
  (when mevedel-view--pending-input-edit
    (user-error "Save or cancel the pending-input edit first"))
  (unless mevedel--data-buffer
    (user-error "No data buffer associated with this view"))
  (unless (buffer-live-p mevedel--data-buffer)
    (user-error "Data buffer has been killed"))
  (when mevedel-view--prompt-hook-pending
    (user-error "A prompt hook is still running -- wait or abort first"))
  (when mevedel-view--pending-skill-submission
    (user-error "Skill preparation is still running -- wait or abort first"))
  (when (buffer-local-value 'mevedel--compaction-in-flight mevedel--data-buffer)
    (message "mevedel: compacting, please wait...")
    (user-error "Compaction in progress"))
  (when (buffer-local-value 'mevedel-session--read-only-mode
                            mevedel--data-buffer)
    (user-error "Session is open read-only (another host holds the lock)"))
  (let* ((session (buffer-local-value 'mevedel--session
                                      mevedel--data-buffer))
         (snapshot (and session
                        (mevedel-view--composer-snapshot session)))
         (input (if (and session (not mevedel-view--composer-scope))
                    (mevedel-view--bind-input-mentions session)
                  (mevedel-view--input-text))))
    (when (string-empty-p input)
      (user-error "Nothing to send"))
    (if mevedel-view--composer-scope
        (mevedel-view--send-directive-input input)
      (let* ((slash-parsed (mevedel-skills--parse-slash-line input))
           (fork-target mevedel-view--armed-session-fork)
           (active-request
            (buffer-local-value 'mevedel--current-request
                                mevedel--data-buffer)))
      (when (and slash-parsed (mevedel-view-historical-segment-p))
        (user-error
         "Slash commands are unavailable while viewing a historical segment"))
      (when (and fork-target (not slash-parsed))
        (require 'mevedel-session-persistence)
        (mevedel-session-persistence--assert-stable-source
         session mevedel--data-buffer "forking"))
      (if (or active-request
              (and (not slash-parsed)
                   (mevedel-view--occupied-root-workflow-p session)))
          (let ((restore
                 (lambda ()
                   (when snapshot
                     (mevedel-view--restore-composer-snapshot
                      snapshot session)))))
            (cond
             ((and slash-parsed
                   (mevedel-skills-local-command-active-request-p
                    (nth 0 slash-parsed) (nth 1 slash-parsed)))
              (let ((result (with-current-buffer mevedel--data-buffer
                              (funcall (cdr (assoc (nth 0 slash-parsed)
                                                  mevedel-slash-commands))
                                       (nth 1 slash-parsed)))))
                (unless (eq result 'mevedel-view-sent)
                  (when (stringp result) (message "%s" result))
                  (mevedel-view-history-add input)
                  (mevedel-view--clear-input))))
             (slash-parsed
              (funcall restore)
              (user-error
               "A request is already active -- wait or abort first"))
             ((not active-request)
              (funcall restore)
              (user-error
               "The workflow is occupied -- use C-c TAB for a follow-up"))
             ((not (mevedel-view--steerable-root-request-p active-request))
              (funcall restore)
              (user-error
               "This workflow cannot be steered -- use C-c TAB"))
             (t
              (condition-case err
                  (mevedel-view--submit-planned-input
                   input nil
                   (lambda ()
                     (funcall restore)
                     (message
                      "mevedel: steering preparation failed; use C-c TAB"))
                   (lambda (submission)
                     (unless
                         (mevedel-view--queue-prepared-steering
                          submission active-request)
                       (funcall restore))))
                (error
                 (funcall restore)
                 (user-error "%s; use C-c TAB for a follow-up"
                             (error-message-string err)))))))
        (cond
         (slash-parsed
          (let* ((name (nth 0 slash-parsed))
                 (args (nth 1 slash-parsed))
                 (local (assoc name mevedel-slash-commands)))
            (cond
             ((and local
                   (string= name "goal")
                   args
                   (not (string-blank-p args))
                   (not (member
                         (car (split-string args "[ \t\n]+" t))
                         '("edit" "pause" "resume" "clear"))))
              (mevedel-view--send-local-goal input args))
             ((and local
                   (string= name "plan")
                   args
                   (not (string-blank-p args)))
              (let ((data-buffer mevedel--data-buffer))
                (mevedel-view--submit-planned-input
                 args
                 (lambda ()
                   (with-current-buffer data-buffer
                     (require 'mevedel-plan-mode)
                     (mevedel-plan-mode-enter))
                   (mevedel-view-history-add input)))))
             (local
              (let ((result (with-current-buffer mevedel--data-buffer
                              (funcall (cdr local) args))))
                ;; Most local slash commands don't send a turn.  A command may
                ;; return this sentinel when it took ownership of the input.
                (unless (eq result 'mevedel-view-sent)
                  (when (stringp result)
                    (message "%s" result))
                  (mevedel-view-history-add input)
                  (mevedel-view--clear-input))))
             (t
              (message "Unknown slash command: /%s" name)))))
         (t
          (let ((source-view (current-buffer)))
            (if fork-target
                (progn
                  ;; Parsing stays in Source so malformed skill syntax cannot
                  ;; publish a child.  Expansion and hooks belong to Child.
                  (require 'mevedel-skills-plan)
                  (with-current-buffer mevedel--data-buffer
                    (mevedel-skills-plan-user-input input session))
                  (mevedel-view--submit-armed-session-fork
                   source-view input fork-target snapshot))
              (mevedel-view--submit-planned-input
               input
               (lambda ()
                 (mevedel-view-history-add input)))))))))))
  ;; Accepted sends clear the draft and land at the new composer end.
  ;; Rejected sends preserve the exact input-relative point.
  (unless (mevedel-view--point-in-input-region-p)
    (goto-char (point-max))))

(defun mevedel-view--send-local-goal (input args)
  "Run pre-send check and start local `/goal' with ARGS.
INPUT is the original composer text, including the slash command."
  (let* ((view-buffer (current-buffer))
         (data-buffer mevedel--data-buffer)
         (objective args))
    (when (string-blank-p objective)
      (user-error "Goal objective must not be blank"))
    (mevedel-view--run-prompt-submit-hook
     objective input
     (lambda (submission)
       (when (and (buffer-live-p view-buffer)
                  (buffer-live-p data-buffer))
         (with-current-buffer view-buffer
           (mevedel-view-history-add input)
           (mevedel-view--clear-input))
         (with-current-buffer data-buffer
           (require 'mevedel-goal)
           (mevedel-goal-start
            (mevedel-prompt-submission-input submission) submission)))))))

(defun mevedel-view--safe-hook-decision (event decision)
  "Return plist-shaped hook DECISION for EVENT, or nil.

Prompt hook callbacks run from process sentinels and can be backed by
user/project code.  Treat malformed values as no decision so symbols
such as `passed' cannot escape into `plist-get' or `plist-member'."
  (if (and (listp decision)
           (or (null decision)
               (keywordp (car-safe decision))))
      decision
    (display-warning
     'mevedel
     (format "Ignoring malformed %s hook decision: %S" event decision)
     :warning)
    nil))

(defun mevedel-view--join-hook-contexts (&rest contexts)
  "Return CONTEXTS joined as separate hook context blocks."
  (let ((contexts (delq nil contexts)))
    (when contexts
      (mapconcat #'identity contexts "\n\n"))))

(defun mevedel-view--run-prompt-submit-hook
    (input display-text callback &optional blocked-callback prior-context)
  "Run `UserPromptSubmit' for INPUT, then call CALLBACK if accepted.
DISPLAY-TEXT is the user-facing prompt text.  CALLBACK receives
one `mevedel-prompt-submission'.  PRIOR-CONTEXT, when non-nil, is placed
after pending session context and before submit-hook context.  The consumer
commits the submission immediately after it durably records the accepted
input."
  (mevedel-view--ensure-interactive-chat-view)
  (when mevedel-view--prompt-hook-pending
    (user-error "A prompt hook is still running -- wait or abort first"))
  (let ((view-buffer (current-buffer))
        (data-buffer mevedel--data-buffer))
    (unless (and data-buffer (buffer-live-p data-buffer))
      (user-error "Data buffer has been killed"))
    (setq mevedel-view--prompt-hook-pending t)
    (condition-case err
        (with-current-buffer data-buffer
          (require 'mevedel-hooks)
          (let ((session mevedel--session)
                (workspace mevedel--workspace))
            (mevedel-hooks-run-event
             'UserPromptSubmit
             (mevedel-hooks-event-plist
              'UserPromptSubmit session workspace
              :prompt input
              :display-text display-text)
             (lambda (decision)
               (when (buffer-live-p view-buffer)
                 (with-current-buffer view-buffer
                   (setq mevedel-view--prompt-hook-pending nil)
                   (when (buffer-live-p data-buffer)
                     (setq decision
                           (mevedel-view--safe-hook-decision
                            'UserPromptSubmit decision))
                     (cond
                      ((and (plist-member decision :continue)
                            (not (plist-get decision :continue)))
                       (mevedel-hooks-record-session-context
                        session decision 'UserPromptSubmit)
                       (when blocked-callback
                         (funcall blocked-callback))
                       (message "mevedel: prompt blocked by hook: %s"
                                (or (plist-get decision :stop-reason)
                                    "no reason provided")))
                      (t
                       (when-let* ((msg (plist-get decision :system-message)))
                         (message "mevedel: %s" msg))
                       (let* ((submitted
                               (if (stringp (plist-get decision :updated-input))
                                   (plist-get decision :updated-input)
                                 input))
                              (pending-entries
                               (mevedel-session-hook-context-pending session))
                              (pending-context
                               (mevedel-hooks-format-context pending-entries))
                              (submit-context
                               (mevedel-hooks-additional-context-string
                                decision 'UserPromptSubmit))
                              (context
                               (mevedel-view--join-hook-contexts
                                pending-context
                                prior-context
                                submit-context))
                              (audit
                               (mevedel-view--prompt-rewrite-audit-record
                                'UserPromptSubmit input submitted decision)))
                         (when (fboundp 'mevedel-plan-mode--invalidate-proposal)
                           (mevedel-plan-mode--invalidate-proposal session))
                         (funcall
                          callback
                          (mevedel-prompt-submission-create
                           :input submitted
                           :display-text display-text
                           :context context
                           :audits (and audit (list audit))
                           :session session
                           :context-entries pending-entries)))))))))
             session workspace nil nil)))
      (error
       (setq mevedel-view--prompt-hook-pending nil)
       (signal (car err) (cdr err))))))

(cl-defun mevedel-view--forward-input
    (input &key display-text before-send after-insert prompt-checked on-block
           submission model-input)
  "Render INPUT in the history region, forward to the data buffer, and send.
Helper for `mevedel-view-send'.  When DISPLAY-TEXT is non-nil, show
that in the view instead of INPUT (e.g., compact skill invocation).
Optional BEFORE-SEND is called after prompt hooks allow the send but
before any user-visible prompt or data-buffer prompt is inserted.  When
PROMPT-CHECKED is non-nil, skip `UserPromptSubmit' because the caller
already ran it.  ON-BLOCK is called when a prompt hook blocks.
SUBMISSION carries hook context, audits, and commit ownership when
PROMPT-CHECKED is non-nil.  MODEL-INPUT, when non-nil, replaces INPUT only in
the temporary request prompt.

Anchors the incremental-render markers so progress hooks can redraw
the in-flight assistant turn as tool calls complete:
`mevedel-view--in-flight-turn-start' points into the view just above
the input zone (where the assistant turn will be rendered);
`mevedel-view--data-turn-start' points into the data buffer just
after the forwarded prompt, where the LLM's response will begin."
  (cl-labels
      ((send-now (stored-input view-text accepted request-input)
         (when before-send
           (funcall before-send))
         (mevedel-view--forward-input-now
          stored-input
          :display-text view-text
          :submission accepted
          :after-insert after-insert
          :model-input request-input)))
    (if prompt-checked
        (send-now input (or display-text input) submission model-input)
      (mevedel-view--run-prompt-submit-hook
       input display-text
       (lambda (accepted)
         (let ((hook-input
                (mevedel-prompt-submission-input accepted))
               (context
                (mevedel-prompt-submission-context accepted)))
           (send-now
            (if context
                (concat hook-input "\n\n" context)
              hook-input)
            (or display-text hook-input)
            accepted
            nil)))
       on-block))))

(cl-defun mevedel-view--forward-input-now
    (input &key display-text submission after-insert model-input)
  "Forward INPUT to gptel immediately, after prompt hooks have run.
DISPLAY-TEXT is shown in the view instead of INPUT when non-nil.  SUBMISSION
supplies hook context, audits, and commit ownership.  MODEL-INPUT, when non-nil,
replaces INPUT only in the temporary request prompt."
  (mevedel-view--ensure-interactive-chat-view)
  (when (buffer-local-value 'mevedel--compaction-in-flight mevedel--data-buffer)
    (message "mevedel: compacting, please wait...")
    (user-error "Compaction in progress"))
  (let* ((input (mevedel--normalize-message-text input))
         (display-text (and display-text
                            (mevedel--normalize-message-text display-text)))
         (hook-context
          (and submission
               (mevedel-prompt-submission-context submission)))
         (hook-audits
          (and submission
               (mevedel-prompt-submission-audits submission)))
         (prompt-summary-body
          (mevedel-view--inline-skill-prompt-summary-body
           (or model-input input)))
         (data-buffer mevedel--data-buffer)
         (session (mevedel-view--session))
         (dropped-file-grants
          (progn
            (mevedel-request-assert-target-ready session)
            (mevedel-view--pop-dropped-file-grants-for-input
             input session))))
    (let (data-turn-start
          hook-audits-with-source
          prompt-summary-source)
      ;; Forward to the data buffer first so immediate inline-skill
      ;; Prompt handles can expand through the same source-backed fold
      ;; path as a full rerender.
      (with-current-buffer mevedel--data-buffer
        (goto-char (point-max))
        (let ((user-turn-start (point))
              body-start)
          ;; Insert response separator
          (insert gptel-response-separator)
          ;; Insert prompt prefix if needed (e.g., org heading marker)
          (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
            (let ((prefix-length (length prefix)))
              (unless (and (>= (point) (+ (point-min) prefix-length))
                           (string= (buffer-substring-no-properties
                                     (- (point) prefix-length) (point))
                                    prefix))
                (unless (bolp) (insert "\n"))
                (insert prefix))))
          (setq body-start (point))
          (insert input "\n")
          (when-let* ((prompt-summary-body)
                      (block
                       (car (last
                             (mevedel-pipeline--render-data-blocks input)))))
            (setq prompt-summary-source
                  (mevedel-view--source-range
                   data-buffer
                   (+ body-start (car block))
                   (+ body-start (cadr block)))))
          (mevedel--clear-user-turn-gptel-properties
           user-turn-start (point)))
        (dolist (audit hook-audits)
          (let ((audit-start (point)))
            (insert (mevedel--format-hook-audit-record audit))
            (push (append audit
                          (list :source
                                (mevedel-view--source-range
                                 data-buffer audit-start (point))))
                  hook-audits-with-source)))
        (setq hook-audits-with-source (nreverse hook-audits-with-source))
        ;; Anchor the data-side marker after the forwarded prompt so
        ;; incremental renders extract only the in-flight assistant
        ;; segments from here forward.  Pushed onto the view buffer's
        ;; buffer-local so it is readable from `--render-incremental'
        ;; without switching buffers.
        (setq data-turn-start (copy-marker (point) nil)))
      (when submission
        (mevedel-prompt-submission-commit submission))
      (when after-insert
        (funcall after-insert))
      ;; Render the user's message in the view after the data source is
      ;; known, but before the model request starts.
      (let ((turn-start
             (mevedel-view--insert-user-message
              (or display-text input) nil hook-context
              prompt-summary-body prompt-summary-source
              hook-audits-with-source)))
        (mevedel-view-stream-begin-turn turn-start data-turn-start)
        ;; Clear composer text.
        (mevedel-view--clear-input))
      (with-current-buffer mevedel--data-buffer
        (mevedel-view--activate-dropped-file-grants
         dropped-file-grants session)
        (setq-local mevedel--pending-model-input model-input)
        (unwind-protect
            (gptel-send)
          (setq-local mevedel--pending-model-input nil))))))

(defun mevedel-view--transform-model-input (fsm)
  "Replace the latest stored prompt with its one-shot model input for FSM."
  (when-let* ((chat-buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p chat-buffer))
              (model-input
               (buffer-local-value 'mevedel--pending-model-input
                                   chat-buffer)))
    (with-current-buffer chat-buffer
      (setq-local mevedel--pending-model-input nil))
    (require 'mevedel-transcript)
    (goto-char (mevedel-transcript-prompt-transform-start))
    (delete-region (point) (point-max))
    (insert model-input)))

(defun mevedel-view--agent-fsm-p (info data-buffer)
  "Return non-nil when INFO or DATA-BUFFER belongs to an agent request."
  (or (and (fboundp 'mevedel-agent-invocation-p)
           (mevedel-agent-invocation-p
            (plist-get info :mevedel-agent-invocation)))
      (and (buffer-live-p data-buffer)
           (with-current-buffer data-buffer
             (bound-and-true-p mevedel--agent-invocation)))))

(defun mevedel-view--drain-follow-up (data-buffer)
  "Submit the next pending follow-up for DATA-BUFFER.

Each bound entry is planned and prepared as its own turn.  The queue entry is
removed only when the resulting prompt reaches its transcript commit boundary."
  (when (buffer-live-p data-buffer)
    (let* ((view-buffer (buffer-local-value 'mevedel--view-buffer data-buffer))
           (session (buffer-local-value 'mevedel--session data-buffer)))
      (when (and session
                 (buffer-live-p view-buffer)
                 (not (buffer-local-value 'mevedel--current-request
                                          data-buffer)))
        (with-current-buffer view-buffer
          (when (and (not mevedel-view--agent-transcript-p)
                     (not mevedel-view--prompt-hook-pending)
                     (not mevedel-view--pending-skill-submission)
                     (not (mevedel-view--follow-up-auto-drain-blocked-p
                           session))
                     (string-empty-p (mevedel-view--input-text)))
            (when-let* ((queue (mevedel-view--pending-follow-ups session)))
              (let* ((workflow (mevedel-session-directive-planning session))
                     (entry
                      (if workflow
                          (cl-find-if
                           (lambda (candidate)
                             (let ((scope (plist-get candidate :scope)))
                               (and (eq (plist-get scope :action) 'plan)
                                    (equal (plist-get scope :directive-id)
                                           (plist-get workflow
                                                      :directive-id)))))
                           queue)
                        (car queue)))
                     (input (mevedel-view--pending-input-text entry))
                     (scope (plist-get entry :scope))
                     (submission (plist-get entry :submission))
                     (dropped-file-grants
                      (plist-get entry :dropped-file-grants)))
                (let ((before-send
                       (lambda ()
                         (mevedel-view--activate-dropped-file-grants
                          dropped-file-grants session)))
                      (after-insert
                      (lambda ()
                         (when (fboundp 'mevedel-telemetry-record)
                           (mevedel-telemetry-record
                            session 'user-message-dequeued
                            :message-hash (secure-hash 'sha256 input)
                            :queue-depth-before
                            (length (mevedel-view--pending-follow-ups session))
                            :queue-duration-ms
                            (and (numberp (plist-get entry :queued-at-time))
                                 (round
                                  (* 1000.0
                                     (- (float-time)
                                        (plist-get entry
                                                   :queued-at-time)))))
                            :enqueue-goal-id
                            (plist-get entry :queued-at-goal-id)
                            :dequeue-goal-id
                            (and (mevedel-session-goal session)
                                 (mevedel-goal-id
                                  (mevedel-session-goal session)))))
                         (mevedel-view--set-pending-follow-ups
                          (delq entry
                                (mevedel-view--pending-follow-ups session))
                          session)
                         (mevedel-view--interaction-rebuild))))
                  (cond
                   (scope
                    (condition-case err
                        (progn
                          (funcall before-send)
                          (mevedel-view--dispatch-directive-input scope input)
                          (funcall after-insert))
                      (error
                       (mevedel-view--interaction-rebuild)
                       (message
                        "mevedel: queued directive follow-up failed: %s"
                        (error-message-string err)))))
                   (submission
                    (mevedel-view--dispatch-prepared-outcome
                     submission data-buffer
                     :before-send before-send
                     :after-insert after-insert
                     :on-block (lambda ()
                                 (mevedel-view--interaction-rebuild))))
                   (t
                    (mevedel-view--submit-planned-input
                     input before-send
                     (lambda ()
                       (mevedel-view--interaction-rebuild))
                     nil after-insert))))))))))))

(defun mevedel-view--run-follow-up-drain (data-buffer)
  "Drain one pending follow-up for DATA-BUFFER if it is live."
  (when (buffer-live-p data-buffer)
    (mevedel-view--drain-follow-up data-buffer)))

(defun mevedel-view--schedule-late-follow-up-drain ()
  "Schedule a fallback follow-up drain after request cleanup."
  (when-let* ((data-buffer mevedel--data-buffer)
              ((buffer-live-p data-buffer))
              ((not (buffer-local-value 'mevedel--current-request
                                        data-buffer))))
    (run-at-time 0 nil
                 #'mevedel-view--run-follow-up-drain
                 data-buffer)))

(defun mevedel-view--schedule-follow-up-drain (fsm)
  "Schedule the next follow-up after FSM completes successfully."
  (when-let* ((info (and fsm (fboundp 'gptel-fsm-info)
                         (gptel-fsm-info fsm)))
              (data-buffer (plist-get info :buffer))
              ((buffer-live-p data-buffer)))
    (run-at-time 0 nil
                 #'mevedel-view--run-follow-up-drain
                 data-buffer)))

(defun mevedel-view-abort ()
  "Abort the active request from the view buffer."
  (interactive)
  (mevedel-view--ensure-interactive-chat-view)
  (mevedel-view--cancel-pending-skill-submission)
  (mevedel-view--stop-request-progress)
  (when-let* ((data-buf mevedel--data-buffer)
              (_ (buffer-live-p data-buf)))
    (require 'mevedel-view)
    (mevedel-view--abort-data-buffer data-buf)))




(provide 'mevedel-view-composer)
;;; mevedel-view-composer.el ends here
