;;; mevedel-view.el -- Compact view buffer for chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Coordinates the user-facing view mode, session lifecycle, and managed
;; zones.  `mevedel-view-composer' owns editable input and submission;
;; `mevedel-view-render' owns the transcript projection.  The gptel data
;; buffer remains the authoritative conversation.
;;
;; Architecture:
;;   data buffer (org-mode, gptel) <--- authoritative
;;     |
;;     +---> view buffer (mevedel-view-mode) <--- user-facing
;;
;; The view buffer is ephemeral and always reconstructable from the
;; data buffer.

;;; Code:

(require 'cl-lib)
(require 'mevedel-utilities)
(require 'mevedel-transcript)

;; `browse-url'
(declare-function browse-url "browse-url" (url &optional new-window))

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-teardown-session
                  "mevedel-agent-control" (session))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(defvar mevedel--agent-invocation)

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))

;; `mevedel-execution'
(declare-function mevedel-execution-count-user "mevedel-execution" (session))
(declare-function mevedel-execution-teardown-session
                  "mevedel-execution" (session))
(defvar mevedel-execution-state-change-hook)

;; `mevedel-executions-list'
(declare-function mevedel-executions-list-open
                  "mevedel-executions-list" (&optional context))

;; `mevedel-goal'
(declare-function mevedel-goal-approval-status
                  "mevedel-goal" (&optional session))
(declare-function mevedel-goal-cycle-record "mevedel-goal" (goal))
(declare-function mevedel-plan-queue-abort-all
                  "mevedel-goal" (&optional session))

;; `mevedel-menu'
(declare-function mevedel-menu "mevedel-menu" ())
(declare-function mevedel-menu-open "mevedel-menu" (area))

;; `mevedel-models'
(declare-function mevedel-model-current-label "mevedel-models"
                  (&optional buffer))

;; `mevedel-permission-queue'
(declare-function mevedel-permission-queue-abort-all
                  "mevedel-permission-queue" (&optional session))

;; `mevedel-sandbox'
(declare-function mevedel-sandbox-status-text "mevedel-sandbox" (facts))
(declare-function mevedel-sandbox-visible-facts
                  "mevedel-sandbox" (&optional session))
(defvar mevedel-sandbox-state-change-hook)

;; `mevedel-structs'
(declare-function mevedel-goal-phase "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-state-label "mevedel-structs"
                  (&optional buffer))
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-display-string "mevedel-tool-registry" (tool-name args))

;; `mevedel-tool-task'
(declare-function mevedel-tool-task--display-string
                  "mevedel-tool-task" (session show-completed))
(declare-function mevedel-tool-task--session-has-active-p
                  "mevedel-tool-task" (session))
(declare-function mevedel-toggle-tasks "mevedel-tool-task" ())
(defvar mevedel-tool-task--status-keymap)

;; `mevedel-tools'
(declare-function mevedel-tools-active-count "mevedel-tools"
                  (&optional buffer))

;; `mevedel-view-agent'
(declare-function mevedel-view--on-agent-transcript-data-killed
                  "mevedel-view-agent" ())
(declare-function mevedel-view-agent-cleanup-parent
                  "mevedel-view-agent" (parent-view))
(declare-function mevedel-view-agent-handle-view-kill
                  "mevedel-view-agent" ())
(declare-function mevedel-view-agent-initialize
                  "mevedel-view-agent" (options data-buffer))
(declare-function mevedel-view-agent-status-fragment
                  "mevedel-view-agent" ())
(declare-function mevedel-view-close-agent-transcript
                  "mevedel-view-agent" ())
(declare-function mevedel-view-open-agent-transcript-at-point
                  "mevedel-view-agent" (&optional event))
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-composer'
(declare-function mevedel-view--effective-permission-mode
                  "mevedel-view-composer" ())
(declare-function mevedel-view--input-marker-position
                  "mevedel-view-composer" ())
(declare-function mevedel-view--input-prompt-string
                  "mevedel-view-composer" (&optional mode))
(declare-function mevedel-view--permission-mode-display
                  "mevedel-view-composer" (mode))
(declare-function mevedel-view--position-in-input-region-p
                  "mevedel-view-composer" (position))
(declare-function mevedel-view-abort "mevedel-view-composer" ())
(declare-function mevedel-view-clear-queued-messages
                  "mevedel-view-composer" ())
(declare-function mevedel-view-composer-initialize
                  "mevedel-view-composer" ())
(declare-function mevedel-view-cycle-permission-mode
                  "mevedel-view-composer" ())
(declare-function mevedel-view-edit-last-queued-message
                  "mevedel-view-composer" ())
(declare-function mevedel-view-send "mevedel-view-composer" ())
(declare-function mevedel-view-yank-dwim
                  "mevedel-view-composer" (&optional arg))
(defvar mevedel-view--input-marker)

;; `mevedel-view-history'
(declare-function mevedel-view-history-beginning-of-line
                  "mevedel-view-history" (&optional arg))
(declare-function mevedel-view-history-browse "mevedel-view-history" ())
(declare-function mevedel-view-history-clear-input
                  "mevedel-view-history" ())
(declare-function mevedel-view-history-next "mevedel-view-history" ())
(declare-function mevedel-view-history-previous "mevedel-view-history" ())
(declare-function mevedel-view-history-save "mevedel-view-history"
                  (&optional view-buffer))
(declare-function mevedel-view-history-search "mevedel-view-history" ())

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-clear
                  "mevedel-view-interaction" ())
(declare-function mevedel-view-interaction-initialize
                  "mevedel-view-interaction" ())

;; `mevedel-view-markdown'
(autoload 'mevedel-view--normalize-local-file-uri-path
  "mevedel-view-markdown")

;; `mevedel-view-render'
(declare-function mevedel-view--after-header-position
                  "mevedel-view-render" ())
(declare-function mevedel-view--full-rerender
                  "mevedel-view-render" ())
(declare-function mevedel-view--history-tail-position
                  "mevedel-view-render" ())
(declare-function mevedel-view--non-history-view-position-p
                  "mevedel-view-render" (pos))
(declare-function mevedel-view-next-display "mevedel-view-render" ())
(declare-function mevedel-view-previous-display "mevedel-view-render" ())
(declare-function mevedel-view-render-initialize
                  "mevedel-view-render" ())
(declare-function mevedel-view-toggle-section "mevedel-view-render" ())
(declare-function mevedel-view-toggle-transcript "mevedel-view-render" ())

;; `mevedel-view-stream'
(declare-function mevedel-view--render-stream-update
                  "mevedel-view-stream" (data-buf))

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-collapse-state
                  "mevedel-view-zone" (key &optional default))
(declare-function mevedel-view-zone-reconcile
                  "mevedel-view-zone" (zone start end fragments))

;; `org'
(declare-function org-mode "ext:org" ())


;;
;;; Customization

(defcustom mevedel-view-inline-image-max-width 600
  "Maximum pixel width for inline images rendered in the view."
  :type 'integer
  :group 'mevedel)


(defface mevedel-view-separator
  '((t :inherit shadow :extend t))
  "Face for separator lines in the view buffer."
  :group 'mevedel)

(defface mevedel-view-header
  '((t :inherit (bold shadow) :overline t :extend t))
  "Face for the session header at the top of the view buffer."
  :group 'mevedel)

(defface mevedel-view-user-header
  '((t :inherit bold :overline t :extend t))
  "Face for user message headers in the view buffer."
  :group 'mevedel)

(defface mevedel-view-directive-action
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for directive action labels in the view buffer."
  :group 'mevedel)

(defface mevedel-view-assistant-header
  '((t :inherit (bold font-lock-function-name-face) :overline t :extend t))
  "Face for assistant message headers in the view buffer."
  :group 'mevedel)

(defface mevedel-view-tool-summary
  '((t :inherit default))
  "Face for collapsed tool call summaries."
  :group 'mevedel)

(defface mevedel-view-tool-marker
  '((t :inherit success :weight bold))
  "Face for successful tool summary markers."
  :group 'mevedel)

(defface mevedel-view-tool-name
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for tool names in collapsed summaries."
  :group 'mevedel)

(defface mevedel-view-tool-argument
  '((t :inherit font-lock-string-face))
  "Face for primary tool arguments in collapsed summaries."
  :group 'mevedel)

(defface mevedel-view-tool-metadata
  '((t :inherit shadow))
  "Face for line counts and secondary metadata in summaries."
  :group 'mevedel)

(defface mevedel-view-tool-diff-added
  '((t :inherit success :weight bold))
  "Face for added-line counts in Edit and Write summaries."
  :group 'mevedel)

(defface mevedel-view-tool-diff-removed
  '((t :inherit error :weight bold))
  "Face for removed-line counts in Edit and Write summaries."
  :group 'mevedel)

(defface mevedel-view-tool-warning
  '((t :inherit warning :weight bold))
  "Face for blocked or warning tool summary markers."
  :group 'mevedel)

(defface mevedel-view-hook-context
  '((t :inherit (shadow italic)))
  "Face for hook context indicators in user turns."
  :group 'mevedel)

(defface mevedel-view-hook-audit
  '((t :inherit (shadow italic)))
  "Face for hook audit indicators in transcript turns."
  :group 'mevedel)

(defface mevedel-view-thinking-summary
  '((t :inherit (shadow italic)))
  "Face for collapsed thinking/reasoning summaries."
  :group 'mevedel)

(defface mevedel-view-system-reminder
  '((t :inherit shadow))
  "Face for collapsed system reminder summaries."
  :group 'mevedel)

(defface mevedel-view-thinking-marker
  '((t :inherit (shadow italic)))
  "Face for thinking/reasoning summary markers."
  :group 'mevedel)

(defface mevedel-view-response-summary
  '((t :inherit shadow))
  "Face for collapsed response summaries."
  :group 'mevedel)

(defface mevedel-view-response-marker
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for collapsed response summary markers."
  :group 'mevedel)

(defface mevedel-view-source-block
  '((t :inherit org-block :foreground unspecified :extend t))
  "Face for rendered Markdown source block panels."
  :group 'mevedel)

(defface mevedel-view-source-block-language
  '((t :inherit (italic font-lock-type-face mevedel-view-source-block)))
  "Face for rendered Markdown source block language labels."
  :group 'mevedel)

(defface mevedel-view-spinner
  '((t :inherit (bold font-lock-comment-face)))
  "Face for the spinner status line."
  :group 'mevedel)

(defface mevedel-view-turn-rule
  '((t :inherit shadow :overline t :extend t))
  "Face for the horizontal rule that closes an assistant turn."
  :group 'mevedel)

(defface mevedel-view-activity-rule
  '((t :inherit shadow :overline t :extend t))
  "Face for the separator before assistant activity rows."
  :group 'mevedel)

(defface mevedel-view-input-prompt
  '((t :inherit shadow :weight bold))
  "Face for the read-only `> ' prompt in the input zone."
  :group 'mevedel)

(defface mevedel-view-permission-mode-ask
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the ask permission mode prompt label."
  :group 'mevedel)

(defface mevedel-view-permission-mode-auto
  '((t :inherit success :weight bold))
  "Face for the auto permission mode prompt label."
  :group 'mevedel)

(defface mevedel-view-permission-mode-full-auto
  '((t :inherit error :weight bold))
  "Face for the full-auto permission mode prompt label."
  :group 'mevedel)

(defface mevedel-view-zone-separator
  '((t :inherit shadow))
  "Face for status / interaction zone separator lines."
  :group 'mevedel)

(defface mevedel-view-ephemeral
  '((t :inherit shadow))
  "Face for ephemeral live-tail lines (spinner, \"Calling X…\")."
  :group 'mevedel)

(defface mevedel-view-attribution
  '((t :inherit (link shadow)))
  "Face for the `from <type>--<idshort>' fragment.
Click target on handles, mailbox blocks, plan summaries, and
permission prompts."
  :group 'mevedel)

(defface mevedel-view-mailbox-gutter
  '((t :inherit mevedel-view-tool-metadata))
  "Face for the gutter prefix on expanded mailbox deliveries."
  :group 'mevedel)

(defface mevedel-view-mailbox-body
  '((t :inherit default))
  "Face for expanded mailbox delivery body text."
  :group 'mevedel)

(defface mevedel-view-handle-running
  '((t :inherit bold))
  "Face for the `[running · N calls]' handle badge."
  :group 'mevedel)

(defface mevedel-view-agent-running
  '((t :inherit (font-lock-escape-face bold)))
  "Face for active running agent handle rows.
This follows gptel-agent's own active-agent status styling."
  :group 'mevedel)

(defface mevedel-view-handle-blocked
  '((t :inherit warning))
  "Face for the `[blocked · awaiting …]' handle badge."
  :group 'mevedel)

(defface mevedel-view-handle-done
  '((t :inherit success))
  "Face for the `✓ done · …' handle badge."
  :group 'mevedel)

(defface mevedel-view-handle-error
  '((t :inherit error))
  "Face for the `✗ error · …' / `✗ aborted' handle badges."
  :group 'mevedel)

(defcustom mevedel-view-pending-tools-visible-max 5
  "Maximum number of `Calling X…' lines shown in the live tail.
When more tools are in flight than this cap, the visible lines are
the most recent and the rest are summarised in a single tail line."
  :type 'integer
  :group 'mevedel)

(defvar-local mevedel-view--status-marker nil
  "Marker separating the history region from the status zone.
Insertion-type t so history-content insertion advances it; status-zone
content renders here as read-only text.")

(defvar-local mevedel-view--interaction-marker nil
  "Marker separating the status zone from the interaction zone.
Insertion-type t so status content above advances it; interaction-zone
overlays anchor here.  Permission queue head, plan confirmation, and
preview overlays render against this marker.")

(defconst mevedel-view--status-task-collapse-key '(status tasks)
  "Stable fragment collapse key for the task status block.")


(defcustom mevedel-view-spinner-animate t
  "Non-nil means animate view buffer spinner glyphs."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-view-spinner-interval 0.12
  "Seconds between view buffer spinner frame updates."
  :type 'number
  :group 'mevedel)

(defconst mevedel-view-spinner-braille-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Braille Pattern frames for animated view buffer spinners.")

(defconst mevedel-view-spinner-ascii-frames
  '("-" "\\" "|" "/")
  "ASCII fallback frames for animated view buffer spinners.")

(defcustom mevedel-view-spinner-frames
  mevedel-view-spinner-braille-frames
  "Frames used for animated view buffer spinners.
The default frames are Braille Pattern Unicode code points U+280B,
U+2819, U+2839, U+2838, U+283C, U+2834, U+2826, U+2827, U+2807,
and U+280F.  If your font does not render these glyphs, set this to
`mevedel-view-spinner-ascii-frames'."
  :type '(repeat string)
  :group 'mevedel)


;;
;;; Major mode

(defvar-keymap mevedel-view--display-map
  :doc "Keymap active in the read-only history/status/interaction area.
Applied via the `keymap' text property so these bindings only fire
above `mevedel-view--input-marker'."
  "TAB" #'mevedel-view-toggle-section
  "RET" #'mevedel-view-activate-at-point
  "<mouse-1>" #'mevedel-view-activate-at-point
  "<mouse-2>" #'mevedel-view-activate-at-point
  "n" #'mevedel-view-next-display
  "p" #'mevedel-view-previous-display
  "t" #'mevedel-view-toggle-transcript
  "q" #'mevedel-view-close-agent-transcript)

(defvar-keymap mevedel-view-mode-map
  :doc "Keymap for `mevedel-view-mode'."
  "C-c RET" #'mevedel-view-send
  "C-c C-k" #'mevedel-view-abort
  "C-c C-o" #'mevedel-menu
  "C-c C-l" #'mevedel-view-history-browse
  "C-c C-u" #'mevedel-view-history-clear-input
  "C-c C-e" #'mevedel-view-edit-last-queued-message
  "C-c C-q" #'mevedel-view-clear-queued-messages
  "C-y" #'mevedel-view-yank-dwim
  "M-p" #'mevedel-view-history-previous
  "M-n" #'mevedel-view-history-next
  "M-r" #'mevedel-view-history-search)

(defun mevedel-view--display-fragment-keymap (&rest maps)
  "Return a composed display-fragment keymap from MAPS.
MAPS take precedence, with `mevedel-view--display-map' providing shared
navigation and activation fallbacks."
  (make-composed-keymap
   (delq nil (append maps (list mevedel-view--display-map)))))

(defun mevedel-view--status-task-keymap ()
  "Return the `view-buffer' keymap for the task status fragment."
  (mevedel-view--display-fragment-keymap
   (define-keymap
     "<tab>" #'mevedel-view-toggle-section
     "TAB" #'mevedel-view-toggle-section
     "<return>" #'mevedel-view-activate-at-point
     "RET" #'mevedel-view-activate-at-point)
   mevedel-tool-task--status-keymap))

(define-key mevedel-view-mode-map
            [remap move-beginning-of-line]
            #'mevedel-view-history-beginning-of-line)
(define-key mevedel-view-mode-map (kbd "<backtab>")
            #'mevedel-view-cycle-permission-mode)
(define-key mevedel-view-mode-map (kbd "S-TAB")
            #'mevedel-view-cycle-permission-mode)

(defun mevedel-view--enforce-ephemeral (&rest _)
  "Keep the current view buffer out of Emacs save machinery."
  (setq buffer-file-name nil
        buffer-file-truename nil
        buffer-file-number nil)
  (setq-local buffer-offer-save nil)
  (setq-local buffer-auto-save-file-name nil)
  (setq-local buffer-save-without-query nil)
  (setq-local auto-save-default nil)
  (setq-local make-backup-files nil)
  (setq-local create-lockfiles nil)
  (set-buffer-modified-p nil))

(define-derived-mode mevedel-view-mode text-mode "MevView"
  "Major mode for the mevedel chat view buffer.

Displays a compact rendering of the gptel data buffer.  Interactive view
buffers are ordered as history region, status zone, interaction zone,
request progress row, and input zone.  The input zone starts at
`mevedel-view--input-marker' with a read-only prompt prefix followed by
the editable composer body.

\\{mevedel-view-mode-map}"
  (visual-line-mode +1)
  (setq-local window-point-insertion-type t)
  (auto-save-mode -1)
  (mevedel-view--enforce-ephemeral)
  (add-hook 'after-change-functions
            #'mevedel-view--enforce-ephemeral nil t)
  (add-hook 'post-command-hook
            #'mevedel-view--enforce-ephemeral nil t))


;;
;;; Header helper

(defun mevedel-view--header-string (data-buf)
  "Return the read-only session-header string for DATA-BUF.
The line shows \"SESSION @ WORKSPACE\" using the `mevedel-view-header'
face and carries the display-region text properties so it participates
in read-only enforcement and section navigation."
  (let* ((session (buffer-local-value 'mevedel--session data-buf))
         (ws (and session (mevedel-session-workspace session)))
         (label (if session
                    (format "%s @ %s"
                            (mevedel-session-name session)
                            (or (and ws (mevedel-workspace-name ws))
                                "mevedel"))
                  "mevedel")))
    (propertize (concat label "\n")
                'read-only t
                'keymap mevedel-view--display-map
                'front-sticky '(read-only keymap)
                'rear-nonsticky '(read-only keymap)
                'font-lock-face 'mevedel-view-header)))

(defun mevedel-view--setup (view-buf data-buf &optional options)
  "Initialize VIEW-BUF as the view buffer for DATA-BUF.
Activates `mevedel-view-mode', wires the cross-references, and
inserts the initial separator with input marker.

OPTIONS is a plist.  When `:agent-transcript-p' is non-nil, create
a read-only transcript inspection view instead of an interactive chat
view.  When `:preserve-data-view-buffer' is non-nil, leave DATA-BUF's
  existing `mevedel--view-buffer' binding untouched."
  (require 'mevedel-view-composer)
  (require 'mevedel-view-agent)
  (require 'mevedel-view-history)
  (require 'mevedel-view-interaction)
  (require 'mevedel-view-render)
  (require 'mevedel-view-stream)
  (with-current-buffer view-buf
    (mevedel-view-mode)
    (mevedel-view--enforce-ephemeral)
    (setq-local mevedel--data-buffer data-buf)
    (setq-local mevedel--session
                (and (buffer-live-p data-buf)
                     (buffer-local-value 'mevedel--session data-buf)))
    (mevedel-view-agent-initialize options data-buf)
    (mevedel-view-render-initialize)
    (mevedel-view-interaction-initialize)
    ;; Copy workspace directory so relative paths resolve correctly
    (setq-local default-directory
                (buffer-local-value 'default-directory data-buf))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if mevedel-view--agent-transcript-p
          (let ((start (point)))
            (setq mevedel-view--status-marker (copy-marker start t))
            (setq mevedel-view--interaction-marker (copy-marker start t))
            (setq mevedel-view--input-marker (copy-marker start nil)))
        ;; Insert session header and set up zone markers.
        ;;
        ;; Three markers carve the buffer above the input prompt into
        ;; four zones (history / status / interaction / input).
        (insert (mevedel-view--header-string data-buf))
        ;; Insert the prompt first, then place all three zone markers
        ;; at start-of-prompt.  Order matters because the status and
        ;; interaction markers have insertion-type t.
        (let ((start (point)))
          (insert (mevedel-view--input-prompt-string))
          (add-text-properties
           start (point)
           `(read-only t
             mevedel-view-prompt t
             front-sticky (read-only mevedel-view-prompt)
             rear-nonsticky (read-only mevedel-view-prompt font-lock-face)))
          (setq mevedel-view--status-marker (copy-marker start t))
          (setq mevedel-view--interaction-marker (copy-marker start t))
          (setq mevedel-view--input-marker (copy-marker start nil)))))
    (mevedel-view-composer-initialize)
    ;; Kill-buffer lifecycle: view killed -> clear ref on data buffer
    (add-hook 'kill-buffer-hook #'mevedel-view--on-view-killed nil t)
    (add-hook 'kill-buffer-hook #'mevedel-view-history-save nil t)
    (unless mevedel-view--agent-transcript-p
      (setq header-line-format '(:eval (mevedel-view--status-strip)))))
  (unless (plist-get options :preserve-data-view-buffer)
    (with-current-buffer data-buf
      (setq-local mevedel--view-buffer view-buf)
      (use-local-map
       (copy-keymap (or (current-local-map) (make-sparse-keymap))))
      (local-set-key (kbd "C-c C-o") #'mevedel-menu)
      ;; Kill-buffer lifecycle: data killed -> kill view buffer
      (add-hook 'kill-buffer-hook
                (if (plist-get options :agent-transcript-p)
                    #'mevedel-view--on-agent-transcript-data-killed
                  #'mevedel-view--on-data-killed)
                nil t))))

(defun mevedel-view--ensure (data-buf &optional view-name options)
  "Return the view buffer for DATA-BUF, creating it if needed.
VIEW-NAME and OPTIONS are forwarded to `mevedel-view--setup' when a
new view buffer is created."
  (or (let ((vb (buffer-local-value 'mevedel--view-buffer data-buf)))
        (and vb
             (buffer-live-p vb)
             (with-current-buffer vb
               (eq (and mevedel-view--agent-transcript-p t)
                   (and (plist-get options :agent-transcript-p) t)))
             vb))
      (let* ((data-name (buffer-name data-buf))
             ;; Derive view buffer name from data buffer name:
             ;; *mevedel:main@proj* -> *mevedel:main@proj:view*
             (derived-name (if (string-match "\\*$" data-name)
                               (replace-match ":view*" t t data-name)
                             (concat data-name ":view")))
             (view-buf (get-buffer-create (or view-name derived-name))))
        (mevedel-view--setup view-buf data-buf options)
        view-buf)))


;;
;;; Lifecycle

(defun mevedel-view--abort-data-buffer (data-buffer)
  "Abort active work owned by DATA-BUFFER."
  (when (and (buffer-live-p data-buffer)
             (buffer-local-value 'mevedel--session data-buffer)
             (fboundp 'mevedel-abort))
    (condition-case err
        (mevedel-abort data-buffer)
      (error
       (display-warning
        'mevedel
        (format "Could not abort session during buffer cleanup: %S" err)
        :warning)))))

(defun mevedel-view--on-view-killed ()
  "Hook run when the view buffer is killed.
Clears `mevedel--view-buffer' on the associated data buffer and kills
it.  The reference is cleared before killing so the data buffer's own
kill hook sees nil and exits without re-entering this function."
  (unless (mevedel-view-agent-handle-view-kill)
    (let ((view-buffer (current-buffer)))
      (mevedel-view--interaction-clear)
      (when-let* ((db mevedel--data-buffer)
                  (_ (buffer-live-p db)))
        (mevedel-view--abort-data-buffer db)
        (mevedel-view-agent-cleanup-parent view-buffer)
        (with-current-buffer db
          (when (fboundp 'mevedel-permission-queue-abort-all)
            (mevedel-permission-queue-abort-all mevedel--session))
          (when (fboundp 'mevedel-plan-queue-abort-all)
            (mevedel-plan-queue-abort-all mevedel--session))
          (setq mevedel--view-buffer nil))
        (kill-buffer db)))))

(defun mevedel-view--on-data-killed ()
  "Hook run when the data buffer is killed.
Kills the associated view buffer."
  (mevedel-view--abort-data-buffer (current-buffer))
  (when (and mevedel--session
             (fboundp 'mevedel-execution-teardown-session))
    (mevedel-execution-teardown-session mevedel--session))
  (when mevedel--session
    (require 'mevedel-agent-control)
    (mevedel-agent-control-teardown-session mevedel--session))
  (when (fboundp 'mevedel-permission-queue-abort-all)
    (mevedel-permission-queue-abort-all mevedel--session))
  (when (fboundp 'mevedel-plan-queue-abort-all)
    (mevedel-plan-queue-abort-all mevedel--session))
  (when-let* ((vb mevedel--view-buffer)
              (_ (buffer-live-p vb)))
    (with-current-buffer vb
      (mevedel-view-agent-cleanup-parent vb)
      (mevedel-view--interaction-clear))
    (kill-buffer vb)))

(defun mevedel-view--status-strip-button (label area help)
  "Return clickable status strip LABEL for cockpit AREA with HELP."
  (let* ((map (make-sparse-keymap))
         (command (lambda (&optional _event)
                    (interactive "e")
                    (mevedel-menu-open area))))
    (define-key map [header-line mouse-1] command)
    (propertize label
                'face 'link
                'mouse-face 'highlight
                'help-echo help
                'local-map map
                'mevedel-view-cockpit-area area)))

(defun mevedel-view--status-strip-width ()
  "Return display columns available for the status strip."
  (let* ((buffer (current-buffer))
         (selected (selected-window))
         (windows (get-buffer-window-list buffer nil t))
         (width (cond
                 ((eq (window-buffer selected) buffer)
                  (window-body-width selected))
                 (windows
                  (apply #'min (mapcar #'window-body-width windows)))
                 (t
                  (window-body-width)))))
    (max 20 (1- width))))

(defun mevedel-view--status-strip-root-label (root max-width)
  "Return ROOT shortened to fit MAX-WIDTH display columns."
  (cond
   ((<= max-width 0) "")
   ((<= (string-width root) max-width) root)
   (t
    (let* ((base (file-name-nondirectory (directory-file-name root)))
           (tail (concat "…/" base "/")))
      (if (<= (string-width tail) max-width) tail "")))))

(defun mevedel-view--status-strip-spacer (rhs)
  "Return a spacer that right-aligns RHS in the header line."
  (propertize
   " " 'display
   (if (and (fboundp 'string-pixel-width)
            (display-graphic-p))
       `(space :align-to (- right (,(string-pixel-width rhs))))
     `(space :align-to (- right ,(string-width rhs))))))

(defun mevedel-view--status-strip ()
  "Return a mevedel-owned clickable status strip for the view buffer."
  (when (and (boundp 'mevedel--data-buffer)
             (buffer-live-p mevedel--data-buffer))
    (require 'mevedel-models)
    (require 'mevedel-tools)
    (let* ((data-buffer mevedel--data-buffer)
           (session (with-current-buffer data-buffer
                      (and (boundp 'mevedel--session) mevedel--session)))
           (workspace (and session (mevedel-session-workspace session)))
           (session-name (or (and session (mevedel-session-name session))
                             "unknown"))
           (root (abbreviate-file-name
                  (file-name-as-directory
                   (or (and workspace (mevedel-workspace-root workspace))
                       (with-current-buffer data-buffer default-directory)))))
           (mode (car (mevedel-view--permission-mode-display
                       (mevedel-view--effective-permission-mode))))
           (state (mevedel-request-state-label data-buffer))
           (model-label (mevedel-model-current-label data-buffer))
           (model (if (string= model-label "none")
                      "model none"
                    model-label))
           (goal (and session (mevedel-session-goal session)))
           (goal-phase (and goal (mevedel-goal-phase goal)))
           (approval-status
            (and goal
                 (mevedel-goal-approval-status session)))
           (goal-workload
            (or (plist-get approval-status :workload)
                (pcase goal-phase
                  ('implementing 'implementation)
                  ('reviewing 'review)
                  ('awaiting-approval 'goal-guardian)
                  (_ goal-phase))))
           (cycle-record (and goal (mevedel-goal-cycle-record goal)))
           (actual (and goal-workload
                        (alist-get goal-workload
                                   (plist-get cycle-record :providers))))
           (phase-model
            (cond
             ((and goal (eq (mevedel-goal-status goal) 'complete))
              (format "complete · %s" model))
             ((and approval-status actual)
              (format "%s · %s/%s"
                      (plist-get approval-status :label)
                      (plist-get actual :provider)
                      (or (plist-get actual :effort) "default")))
             (approval-status
              (format "%s · %s" (plist-get approval-status :label) model))
             (actual
              (format "%s · %s/%s"
                      goal-phase (plist-get actual :provider)
                      (or (plist-get actual :effort) "default")))
             (t model)))
           (preset-name (and session (mevedel-session-preset-name session)))
           (tool-count (mevedel-tools-active-count data-buffer))
           (tools (format "%d tool%s"
                          tool-count
                          (if (= tool-count 1) "" "s")))
           (rhs
            (mapconcat
             #'identity
             (delq nil
                   (list
              (mevedel-view--status-strip-button
               mode 'mode "Open mode cockpit")
              (propertize state 'face (cond ((string= state "running") 'success)
                                            (t 'shadow)))
              (mevedel-view--status-strip-button
               phase-model (if goal 'goal 'model)
               (if goal "Open Goal cockpit" "Open model cockpit"))
              (and preset-name
                   (mevedel-view--status-strip-button
                    (format "preset %s" preset-name)
                    'preset "Open Preset cockpit"))
              (mevedel-view--status-strip-button
               tools 'tools "Open tools cockpit")))
             " · "))
           (root-max
            (- (mevedel-view--status-strip-width)
               (string-width session-name)
               (string-width rhs)
               3))
           (root-label
            (mevedel-view--status-strip-root-label root root-max))
           (lhs
            (if (string-empty-p root-label)
                session-name
              (format "%s  %s" session-name root-label))))
      (concat
       (mevedel-view--status-strip-button
        lhs
        'top "Open session cockpit")
       (mevedel-view--status-strip-spacer rhs)
       rhs))))


(defun mevedel-view--tool-status-string (tool-name args)
  "Build a short status string for TOOL-NAME with ARGS."
  (if (equal tool-name "WaitAgent")
      "Waiting for agents"
    (let ((primary-arg (mevedel-tool-display-string tool-name args)))
      (if primary-arg
          (format "Calling %s: %s..." tool-name primary-arg)
        (format "Calling %s..." tool-name)))))


;;
;;; Rerender coordination


(defvar-local mevedel-view--render-timer nil
  "Timer for the next coalesced transcript render.")

(defvar-local mevedel-view--pending-render-kind nil
  "Pending transcript render kind, either `incremental' or `full'.")

(defvar-local mevedel-view--pending-render-data-buffer nil
  "Authoritative data buffer for the pending transcript render.")

(defcustom mevedel-view-rerender-debounce 0.15
  "Seconds to wait before a queued full transcript refresh.
Requests arriving while any transcript refresh is pending join that
refresh; a full request upgrades a pending incremental refresh."
  :type 'number
  :group 'mevedel)

(defun mevedel-view--cancel-scheduled-render ()
  "Cancel the current view buffer's pending transcript render."
  (when (timerp mevedel-view--render-timer)
    (cancel-timer mevedel-view--render-timer))
  (setq mevedel-view--render-timer nil
        mevedel-view--pending-render-kind nil
        mevedel-view--pending-render-data-buffer nil))

(defun mevedel-view--flush-scheduled-render (view-buffer)
  "Run VIEW-BUFFER's pending transcript render once."
  (when (buffer-live-p view-buffer)
    (with-current-buffer view-buffer
      (let ((kind mevedel-view--pending-render-kind)
            (data-buffer mevedel-view--pending-render-data-buffer))
        (setq mevedel-view--render-timer nil
              mevedel-view--pending-render-kind nil
              mevedel-view--pending-render-data-buffer nil)
        (condition-case _
            (pcase kind
              ('full (mevedel-view--full-rerender))
              ('incremental
               (when (buffer-live-p data-buffer)
                 (mevedel-view--render-stream-update data-buffer))))
          (error nil))))))

(defun mevedel-view--schedule-render (kind data-buffer delay)
  "Coalesce a KIND render of DATA-BUFFER after DELAY seconds.
`full' supersedes `incremental'.  Once scheduled, later requests join
the same refresh instead of creating independent stream, tool, and full
render timers."
  (unless (memq kind '(incremental full))
    (error "Unknown render kind: %S" kind))
  (when (buffer-live-p data-buffer)
    (setq mevedel-view--pending-render-data-buffer data-buffer)
    (when (or (eq kind 'full)
              (null mevedel-view--pending-render-kind))
      (setq mevedel-view--pending-render-kind kind))
    (if (and (numberp delay) (> delay 0))
        (unless mevedel-view--render-timer
          (let ((view-buffer (current-buffer)))
            (setq mevedel-view--render-timer
                  (run-at-time
                   delay nil #'mevedel-view--flush-scheduled-render
                   view-buffer))))
      (when (timerp mevedel-view--render-timer)
        (cancel-timer mevedel-view--render-timer))
      (mevedel-view--flush-scheduled-render (current-buffer)))))

(defun mevedel-view-rerender (&optional buffer)
  "Schedule a coalesced full re-render of BUFFER.
Default to the current buffer.  Full, stream, and tool-boundary requests
share one timer, and a full request upgrades an already pending
incremental refresh."
  (let ((view-buffer (or buffer (current-buffer))))
    (when (buffer-live-p view-buffer)
      (with-current-buffer view-buffer
        (when (and (boundp 'mevedel--data-buffer)
                   (buffer-live-p mevedel--data-buffer))
          (mevedel-view--schedule-render
           'full mevedel--data-buffer mevedel-view-rerender-debounce))))))


;;
;;; Sub-agent transcript open command

(defun mevedel-view--event-position (&optional event)
  "Return buffer position referenced by mouse EVENT, or nil."
  (and event
       (eventp event)
       (let ((pos (posn-point (event-end event))))
         (and (integer-or-marker-p pos) pos))))

(defun mevedel-view-activate-at-point (&optional event)
  "Activate actionable display or fragment text at point or EVENT.
This command is installed only on display text keymaps; direct calls from
the editable composer signal instead of settling queued interactions."
  (interactive (list last-nonmenu-event))
  (let* ((event-pos (mevedel-view--event-position event))
         (pos (or event-pos (point)))
         (activate (get-text-property pos 'mevedel-view-zone-activate)))
    (when event-pos
      (mouse-set-point event))
    (cond
     ((mevedel-view--position-in-input-region-p pos)
      (user-error "No actionable fragment at point"))
     ((get-text-property pos 'mevedel-view-agent-path)
      (mevedel-view-open-agent-transcript-at-point event))
     ((and activate
           (not (get-text-property pos 'mevedel-view-interaction-overlay)))
      (funcall activate))
     ((get-text-property pos 'mevedel-tool-task)
      (mevedel-toggle-tasks))
     ((and event (eventp event))
      nil)
     (t
      (user-error "No actionable fragment at point")))))

(defun mevedel-view--indent-region-lines (start end prefix)
  "Insert PREFIX before every non-empty line between START and END."
  (save-excursion
    (goto-char start)
    (let ((end-marker (copy-marker end t)))
      (unwind-protect
          (while (< (point) end-marker)
            (unless (looking-at-p "[ \t]*$")
              (insert prefix))
            (forward-line 1))
        (set-marker end-marker nil)))))

(defun mevedel-view--current-buffer-marker-position (marker)
  "Return MARKER's position when it belongs to the current buffer."
  (and (markerp marker)
       (eq (marker-buffer marker) (current-buffer))
       (marker-position marker)))

(defun mevedel-view--status-anchor ()
  "Return the recovered start of fragment-managed status text."
  (let* ((input-pos (mevedel-view--input-marker-position))
         (status-pos (mevedel-view--current-buffer-marker-position
                      mevedel-view--status-marker))
         (history-tail (mevedel-view--history-tail-position))
         (status-valid-p
          (and status-pos
               (or (not input-pos) (<= status-pos input-pos))
               (>= status-pos history-tail)
               (not (mevedel-view--non-history-view-position-p
                     status-pos))
               (not (and (> status-pos (mevedel-view--after-header-position))
                         (mevedel-view--non-history-view-position-p
                          (1- status-pos)))))))
    (or (and status-valid-p status-pos)
        history-tail)))

(defun mevedel-view--status-trailing-newline-suffix (body)
  "Return the suffix needed to preserve BODY's trailing newlines."
  (let ((pos (length body))
        (count 0))
    (while (and (> pos 0) (eq (aref body (1- pos)) ?\n))
      (setq pos (1- pos)
            count (1+ count)))
    (when (> count 1)
      (make-string (1- count) ?\n))))

(defun mevedel-view--status-task-show-completed-p ()
  "Return non-nil when task status should show completed rows."
  (require 'mevedel-view-zone)
  (not (mevedel-view-zone-collapse-state
        mevedel-view--status-task-collapse-key t)))

(defun mevedel-view--status-task-body (session show-completed)
  "Return propertized status-zone task text for SESSION and SHOW-COMPLETED."
  (let ((body (mevedel-tool-task--display-string session show-completed)))
    (add-text-properties 0 (length body) '(mevedel-tool-task t) body)
    body))

(defun mevedel-view--status-session (&optional data-buf)
  "Return DATA-BUF session used for status rendering."
  (or (and data-buf
           (buffer-live-p data-buf)
           (buffer-local-value 'mevedel--session data-buf))
      (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-view--status-model (&optional data-buf)
  "Return the authoritative status-zone model for DATA-BUF."
  (let* ((session (mevedel-view--status-session data-buf))
         (show-completed (mevedel-view--status-task-show-completed-p))
         (task-active-p (and session
                             (require 'mevedel-tool-task nil t)
                             (mevedel-tool-task--session-has-active-p
                              session)))
         (task-body (and task-active-p
                         (mevedel-view--status-task-body
                          session show-completed))))
    (list :session session
          :show-completed show-completed
          :task-active-p task-active-p
          :task-body task-body)))

(defun mevedel-view--status-fragments (model)
  "Return status fragments for MODEL."
  (let (fragments)
    (require 'mevedel-sandbox)
    (let* ((facts (mevedel-sandbox-visible-facts
                   (plist-get model :session)))
           (body (concat (mevedel-sandbox-status-text facts) "\n")))
      (add-text-properties 0 (length body) '(font-lock-face shadow) body)
      (push (list :namespace 'status
                  :id 'sandbox
                  :priority 200
                  :body body)
            fragments))
    (when-let* ((body (plist-get model :task-body)))
      (let ((fragment (list :namespace 'status
                            :id 'tasks
                            :priority 100
                            :body body
                            :keymap (mevedel-view--status-task-keymap)
                            :navigatable t
                            :activate #'mevedel-toggle-tasks
                            :entry 'tasks
                            :collapsible t
                            :collapse-key mevedel-view--status-task-collapse-key
                            :collapsed (not (plist-get model
                                                        :show-completed))))
            (suffix (mevedel-view--status-trailing-newline-suffix body)))
        (when suffix
          (setq fragment (plist-put fragment :body-suffix suffix)))
        (push fragment fragments)))
    (when-let* ((session (plist-get model :session))
                (count (progn
                         (require 'mevedel-execution)
                         (mevedel-execution-count-user session)))
                ((> count 0)))
      (let ((body (format "Executions: %d live\n" count)))
        (add-text-properties 0 (length body)
                             '(font-lock-face shadow) body)
        (push (list :namespace 'status
                    :id 'executions
                    :priority 50
                    :body body
                    :keymap (mevedel-view--display-fragment-keymap)
                    :navigatable t
                    :activate #'mevedel-view-open-executions
                    :entry 'executions)
              fragments)))
    (when-let* ((fragment (mevedel-view-agent-status-fragment)))
      (push fragment fragments))
    (nreverse fragments)))

(defun mevedel-view--sandbox-state-changed (session)
  "Refresh main views owned by SESSION after its child boundary changes."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'mevedel-view-mode)
                 (not mevedel-view--agent-transcript-p)
                 (eq session (mevedel-view--status-session)))
        (mevedel-view--render-status)))))

(add-hook 'mevedel-sandbox-state-change-hook
          #'mevedel-view--sandbox-state-changed)

(defun mevedel-view-open-executions ()
  "Open the current session's live execution cockpit."
  (interactive)
  (require 'mevedel-executions-list)
  (mevedel-executions-list-open))

(defun mevedel-view--execution-state-changed (session data-buffer)
  "Refresh main views owned by SESSION after live executions change."
  (when (buffer-live-p data-buffer)
    (let* ((invocation
            (buffer-local-value 'mevedel--agent-invocation data-buffer))
           (main-data-buffer
            (if invocation
                (mevedel-agent-invocation-parent-data-buffer invocation)
              data-buffer))
           (view-buffer
            (and (buffer-live-p main-data-buffer)
                 (buffer-local-value 'mevedel--view-buffer
                                     main-data-buffer))))
      (when (buffer-live-p view-buffer)
        (with-current-buffer view-buffer
          (when (and (derived-mode-p 'mevedel-view-mode)
                     (not mevedel-view--agent-transcript-p)
                     (eq session (mevedel-view--status-session)))
            (mevedel-view--render-status)))))))

(add-hook 'mevedel-execution-state-change-hook
          #'mevedel-view--execution-state-changed)

(defun mevedel-view--render-status (&optional data-buf)
  "Render sandbox, task, and aggregate agent status for DATA-BUF."
  (unless mevedel-view--agent-transcript-p
    (require 'mevedel-view-zone)
    (let* ((model (mevedel-view--status-model data-buf))
           (fragments (mevedel-view--status-fragments model))
           (start (mevedel-view--status-anchor))
           (input-pos (mevedel-view--input-marker-position))
           (interaction-pos (mevedel-view--current-buffer-marker-position
                             mevedel-view--interaction-marker))
           (end (if (and interaction-pos
                         (<= start interaction-pos)
                         (or (not input-pos) (<= interaction-pos input-pos)))
                    interaction-pos
                  start)))
      (mevedel-view-zone-reconcile 'status start end fragments))))

(defun mevedel-view--zone-separator (label)
  "Return a propertized zone separator line for LABEL.
Format: ` ─── LABEL ─── ' followed by enough box-drawing dashes
to reach `(max 4 (min 60 (- (window-width) 4)))' total length,
then a trailing newline.  Width is clamped so very narrow windows
don't produce zero-length rules.

LABEL is a single string like \"tasks\", \"1 permission pending\",
\"2 previews · 1 plan pending\".  Caller is responsible for
constructing composite count labels via concatenation.

The returned string carries `mevedel-view-zone-separator' face on
the dash runs and inherits the same face on the label text so
tweaks via `customize-face' apply uniformly."
  (let* ((win-widths
          ;; Use the widest window currently displaying this buffer
          ;; if any; fall back to the selected window.  Bare
          ;; (window-width) returned the selected window's width
          ;; even when the call originated from an unrelated
          ;; context (e.g. a sub-agent FSM hook), producing a
          ;; mis-sized rule for split-window setups.
          (or (mapcar #'window-width
                      (get-buffer-window-list (current-buffer) nil t))
              (list (window-width))))
         (target-width (max 4 (min 60 (- (apply #'max win-widths) 4))))
         (pre " ─── ")
         (post " ")
         (decorated-label (or label ""))
         (used (+ (length pre) (length decorated-label) (length post)))
         (tail-len (max 3 (- target-width used)))
         (tail (concat (make-string tail-len ?─))))
    (concat
     (propertize (concat pre decorated-label post tail "\n")
                 'face 'mevedel-view-zone-separator
                 'font-lock-face 'mevedel-view-zone-separator))))

(defun mevedel-view--header-end-position ()
  "Return the position after the current view header, when recognized."
  (or
   (when (eq (get-text-property (point-min) 'font-lock-face)
             'mevedel-view-header)
     (save-excursion
       (goto-char (point-min))
       (let ((end (line-end-position)))
         (if (and (< end (point-max))
                  (eq (char-after end) ?\n))
             (1+ end)
           end))))
   (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                              mevedel--data-buffer))
               ((buffer-live-p data-buf))
               (header (substring-no-properties
                        (mevedel-view--header-string data-buf)))
               (end (+ (point-min) (length header)))
               ((<= end (point-max)))
               ((equal header
                       (buffer-substring-no-properties (point-min) end))))
     end)))

(provide 'mevedel-view)

;;; mevedel-view.el ends here
