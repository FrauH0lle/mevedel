;;; mevedel-pending-inputs.el --- Pending Inputs cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Session-owned queueing, steering, automatic follow-up delivery, inspection,
;; and composer editing.  Opening the cockpit pauses automatic delivery, and
;; closing or killing it resumes delivery and releases a turn parked at the
;; pending-input boundary; queue edits retain the selected entry until a
;; validated replacement is ready.  Delivery leaves a composer draft alone and
;; resumes once an edit empties it.  When something else that held an idle
;; session clears, such as its last interaction, the held work is offered
;; again: queued follow-ups first, then a turn for background work that
;; reported (an agent report, finished command or Monitor event), otherwise an
;; active Goal's continuation.

;;; Code:

(require 'mevedel-recovery)

(require 'mevedel-engine)

(require 'mevedel-report)

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

;; The follow-up drain shares the session durability transaction used by
;; composer dispatch, so the macro must expand for interpreted loads too.
(require 'mevedel-cockpit)
(require 'mevedel-prompt-submission)
(require 'mevedel-session-durability)

;; `gptel-request'
(declare-function gptel--fsm-transition "ext:gptel-request"
                  (machine &optional new-state))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-root-waiting-p
                  "mevedel-agent-control" (session))
(declare-function mevedel-agent-control-wake-root-user
                  "mevedel-agent-control" (session))
(autoload 'mevedel-agent-control-root-waiting-p "mevedel-agent-control")
(autoload 'mevedel-agent-control-wake-root-user "mevedel-agent-control")

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-data-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-for-buffer
                  "mevedel-cockpit" (buffer))
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-view-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context "mevedel-cockpit" ())
(declare-function mevedel-cockpit-open-surface
                  "mevedel-cockpit" (surface &optional context))
(declare-function mevedel-cockpit-quit "mevedel-cockpit" (&optional label))
(declare-function mevedel-cockpit-setup-tabulated-surface
                  "mevedel-cockpit" (surface))
(defvar mevedel-cockpit--context)
(declare-function mevedel-cockpit-surface-context
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-refresh
                  "mevedel-cockpit" (&optional selected-id))
(declare-function mevedel-cockpit-surface-selected
                  "mevedel-cockpit" (&optional no-error))

;; `mevedel-collaboration'
(defvar mevedel-collaboration-guest-skills)

;; `mevedel-collaboration'
(defvar mevedel-collaboration-needs-host-message)
(declare-function mevedel-collaboration-notify-guest
                  "mevedel-collaboration" (session guest-id message))

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--guest-skills-admitted-p
                  "mevedel-collaboration-guest" (names role session))

;; `mevedel-compact-run'
(defvar mevedel-compact-run-in-flight)

;; `mevedel-goal'
(declare-function mevedel-goal-schedule-continuation
                  "mevedel-goal"
                  (&optional session buffer prompt-submission delay))
(autoload 'mevedel-goal-schedule-continuation "mevedel-goal")

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-copy-text
                  "mevedel-mention-bindings" (text))
(autoload 'mevedel-mention-bindings-copy-text "mevedel-mention-bindings")

;; `mevedel-mentions'
(declare-function mevedel-mentions-file-token "mevedel-mentions" (path))
(autoload 'mevedel-mentions-file-token "mevedel-mentions")

;; `mevedel-permission-queue'
(declare-function mevedel-permission-absence-clear
                  "mevedel-permission-queue" (session))
(declare-function mevedel-permission-absence-held-p
                  "mevedel-permission-queue" (session))
(autoload 'mevedel-permission-absence-clear "mevedel-permission-queue")
(autoload 'mevedel-permission-absence-held-p "mevedel-permission-queue")

;; `mevedel-prompt-submission'
(declare-function mevedel-prompt-submission-display-text
                  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-outcome
                  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-reserve
                  "mevedel-prompt-submission" (submission))
(declare-function mevedel-prompt-submission-restore
                  "mevedel-prompt-submission" (submission))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-new-mutation-authority
                  "mevedel-session-artifacts" (session))
(autoload 'mevedel-session-artifacts-assert-new-mutation-authority
  "mevedel-session-artifacts")

;; `mevedel-session-persistence'
(defvar mevedel-session--read-only-mode)

;; `mevedel-skills-ui'
(declare-function mevedel-skills-parse-slash-line
                  "mevedel-skills-ui" (text))

;; `mevedel-structs'
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-fsm "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session--set-active-dropped-file-grants
                  "mevedel-structs" (session paths))
(declare-function mevedel-session--set-dropped-file-grants
                  "mevedel-structs" (session paths))
(declare-function mevedel-session-active-dropped-file-grants
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-add-dropped-file-grant
                  "mevedel-structs" (session path))
(declare-function mevedel-session-directive-planning
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-dropped-file-grants
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-enqueue-pending-input
                  "mevedel-structs" (session category entry))
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-follow-ups
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-input-delivery-paused-p
                  "mevedel-structs" (session))
(declare-function mevedel-session-pending-input-failure-paused
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-input-paused
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-inputs
                  "mevedel-structs" (session category))
(declare-function mevedel-session-pending-plan-approval
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-steering
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-metadata
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-root-buffer "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-set-pending-input-failure-paused
                  "mevedel-structs" (session paused))
(declare-function mevedel-session-set-pending-input-paused
                  "mevedel-structs" (session paused))
(declare-function mevedel-session-set-pending-inputs
                  "mevedel-structs" (session category entries))
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-transport'
(declare-function mevedel-transport-run-when-idle
                  "mevedel-transport" (key path thunk))
(autoload 'mevedel-transport-run-when-idle "mevedel-transport")

;; `mevedel-turn'
(declare-function mevedel-turn-busy-p "mevedel-turn" (&optional buffer))
(autoload 'mevedel-turn-busy-p "mevedel-turn")

;; `mevedel-utilities'
(declare-function mevedel--normalize-message-text
                  "mevedel-utilities" (text))
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))
(autoload 'mevedel--normalize-message-text "mevedel-utilities")

;; `mevedel-view'
(defvar mevedel-view--side-conversation-p)

;; `mevedel-view-composer'
(declare-function mevedel-view--assert-live-tip
                  "mevedel-view-composer" (&optional allow-armed-fork))
(declare-function mevedel-view--bind-input-mentions
                  "mevedel-view-composer" (session))
(declare-function mevedel-view--clear-input "mevedel-view-composer" ())
(declare-function mevedel-view--composer-snapshot
                  "mevedel-view-composer" (session))
(declare-function mevedel-view--dispatch-directive-input
                  "mevedel-view-composer" (scope input))
(declare-function mevedel-view--dispatch-prepared-outcome
                  "mevedel-view-composer" (submission data-buffer &rest keys))
(declare-function mevedel-view--ensure-interactive-chat-view
                  "mevedel-view-composer" ())
(declare-function mevedel-view--input-start "mevedel-view-composer" ())
(declare-function mevedel-view--input-text "mevedel-view-composer" ())
(declare-function mevedel-view--occupied-root-workflow
                  "mevedel-view-composer" (session))
(declare-function mevedel-view--queued-scope
                  "mevedel-view-composer" (&optional scope))
(declare-function mevedel-view--reserved-goal-handoff-id
                  "mevedel-view-composer" (&optional session))
(declare-function mevedel-view--restore-composer-snapshot
                  "mevedel-view-composer" (snapshot session &optional force))
(declare-function mevedel-view--session "mevedel-view-composer" ())
(declare-function mevedel-view--submit-planned-input
                  "mevedel-view-composer"
                  (input &optional before-send on-block dispatch after-insert
                         inert-skills selected-skills))
(declare-function mevedel-view--switch-composer-scope
                  "mevedel-view-composer" (scope))
(declare-function mevedel-view--visible-draft "mevedel-view-composer" ())
(declare-function mevedel-view-composer-scope-label
                  "mevedel-view-composer" (&optional scope))
(declare-function mevedel-view-refresh-input-prompt
                  "mevedel-view-composer" ())
(declare-function mevedel-view-run-invocation
                  "mevedel-view-composer" (name args &rest keys))
(declare-function mevedel-view-send "mevedel-view-composer" ())
(defvar mevedel-view--composer-drafts)
(defvar mevedel-view--composer-scope)
(defvar mevedel-view--input-marker)
(defvar mevedel-view--pending-skill-submission)
(defvar mevedel-view--prompt-hook-pending)

;; `mevedel-view-history'
(declare-function mevedel-view-history-add "mevedel-view-history" (input))

;; `mevedel-view-input-files'
(declare-function mevedel-view--activate-dropped-file-grants
                  "mevedel-view-input-files" (paths session))
(declare-function mevedel-view--pop-dropped-file-grants-for-input
                  "mevedel-view-input-files" (input session))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" ())
(declare-function mevedel-view--interaction-register
                  "mevedel-view-interaction" (descriptor))
(defvar mevedel-view--interaction-overlays)

;; `seq'
(declare-function seq-take "seq" (sequence n))

;; `tabulated-list'
(declare-function tabulated-list-get-id "tabulated-list" ())
(declare-function tabulated-list-mode "tabulated-list" ())
(declare-function tabulated-list-put-tag
                  "tabulated-list" (tag &optional advance))

(defconst mevedel-pending-inputs-buffer-name "*mevedel pending inputs*"
  "Name of the Pending Inputs cockpit buffer.")

(defvar-local mevedel-view--pending-input-edit nil
  "Queue edit state active in this composer, or nil.")

(defvar-local mevedel-view--pending-resume-entry nil
  "Steering entry owned by this composer's unfinished Resume preparation.")

(defun mevedel-pending-inputs-cancel-resume ()
  "Release this composer's unfinished Resume preparation, if any."
  (when mevedel-view--pending-resume-entry
    (plist-put mevedel-view--pending-resume-entry :delivering nil)
    (setq mevedel-view--pending-resume-entry nil)))

(defvar-local mevedel-view--pending-guest-attribution nil
  "Guest attribution audit record for the follow-up being submitted, or nil.

Set by the follow-up drain when it dispatches a collaboration guest's
entry and consumed exactly once where the prompt and its hook audits are
inserted, so attribution lands inside the user turn before the response
marker exists regardless of asynchronous submit hooks.  Blocked or
failed submissions clear it so the next host prompt is never
mis-attributed.")

(defvar-local mevedel-view--turn-guest-attribution nil
  "Guest attribution of the turn this data buffer is running, or nil.
Set where a prompt is inserted, from its guest attribution or nil, so it
names the running turn; read and cleared when that turn settles.")

(defvar-local mevedel-pending-inputs--marked-ids nil
  "Pending-input identities marked for deletion.")

(defvar-local mevedel-pending-inputs--converting-id nil
  "Pending-input identity undergoing asynchronous conversion.")


;;
;;; Queue state and summary

(defun mevedel-view--pending-follow-ups (&optional session)
  "Return SESSION's pending follow-ups."
  (when-let* ((sess (or session (mevedel-view--session))))
    (mevedel-session-pending-follow-ups sess)))

(defun mevedel-pending-inputs-follow-up-changed (session)
  "Persist SESSION's changed follow-up queue soon and notify observers."
  (mevedel-recovery-save session)
  (when (fboundp 'mevedel-collaboration-notify-queue-changed)
    (mevedel-collaboration-notify-queue-changed session)))

(defun mevedel-pending-inputs--set-queues (session &rest replacements)
  "Replace SESSION queue CATEGORY ENTRIES pairs.
Persist and notify guests once, and only when a queue changed."
  (let (changed follow-up-changed)
    (while replacements
      (let ((category (pop replacements))
            (entries (pop replacements)))
        (unless (eq entries (mevedel-session-pending-inputs session category))
          (setq changed t)
          (when (eq category 'follow-up)
            (setq follow-up-changed t))
          (mevedel-session-set-pending-inputs session category entries))))
    (cond
     (follow-up-changed (mevedel-pending-inputs-follow-up-changed session))
     (changed (mevedel-recovery-save session)))))

(defun mevedel-view--next-follow-up (session)
  "Return SESSION's first follow-up eligible for its owning workflow.
Directive planning admits only Plan input for its directive; otherwise
follow-ups retain their queue order.  Delivery holds are checked separately."
  (let ((queue (mevedel-session-pending-follow-ups session))
        (workflow (mevedel-session-directive-planning session)))
    (if workflow
        (cl-find-if
         (lambda (entry)
           (let ((scope (plist-get entry :scope)))
             (and (eq (plist-get scope :action) 'plan)
                  (equal (plist-get scope :directive-id)
                         (plist-get workflow :directive-id)))))
         queue)
      (car queue))))


(defun mevedel-view--follow-up-auto-drain-blocked-p
    (&optional session ignore-failure-latch)
  "Return non-nil when SESSION follow-ups should wait for user action.
With IGNORE-FAILURE-LATCH, the failure-recovery latch alone does not hold."
  (when-let* ((sess (or session (mevedel-view--session))))
    (or (mevedel-recovery-blocker sess)
        (mevedel-session-pending-steering sess)
        (mevedel-permission-absence-held-p sess)
        (cl-find 'failed-turn (mevedel-session-pending-follow-ups sess)
                 :key (lambda (entry) (plist-get entry :state)))
        (if ignore-failure-latch
            (mevedel-session-pending-input-paused sess)
          (mevedel-session-pending-input-delivery-paused-p sess))
        (mevedel-session-pending-plan-approval sess)
        (and (mevedel-session-directive-planning sess)
             (not (mevedel-view--next-follow-up sess)))
        (plist-get (mevedel-session-plan-metadata sess)
                   :implementation-retry)
        (mevedel-view--reserved-goal-handoff-id sess)
        (when-let* ((goal (mevedel-session-goal sess))
                    ((memq (mevedel-goal-status goal)
                           '(paused blocked budget-limited)))
                    (entry (car (mevedel-session-pending-follow-ups sess))))
          (equal (mevedel-goal-id goal)
                 (plist-get entry :queued-at-goal-id))))))

(defun mevedel-view--pending-input-preview (input)
  "Return a one-line, 96-column preview from a bounded prefix of INPUT."
  (let* ((prefix (substring-no-properties input 0 (min (length input) 1024)))
         (preview (string-trim
                   (replace-regexp-in-string
                    "[ \t\n\r]+" " "
                    (mevedel--normalize-message-text prefix) t t))))
    (if (or (> (length input) (length prefix)) (> (string-width preview) 96))
        (concat (truncate-string-to-width preview 93) "...")
      preview)))

(defun mevedel-view--pending-input-text (entry)
  "Return normalized input text for pending ENTRY."
  (mevedel--normalize-message-text (or (plist-get entry :input) "")))

(defvar-keymap mevedel-view--pending-inputs-map
  :doc "Keymap on the pending-input summary."
  "RET" #'mevedel-pending-inputs-preview-activate
  "d" #'mevedel-pending-inputs-drop-at-point
  "DEL" #'mevedel-pending-inputs-drop-at-point
  "<delete>" #'mevedel-pending-inputs-drop-at-point
  "<return>" #'mevedel-pending-inputs-preview-activate
  "<mouse-1>" #'mevedel-pending-inputs-preview-activate
  "<mouse-2>" #'mevedel-pending-inputs-preview-activate)

(defun mevedel-pending-inputs--preview-selection (session entries)
  "Return SESSION's expanded visible entry identity among ENTRIES, or nil.
The disposable interaction overlay owns this presentation state.  Removing
the summary removes its state; rebuilding an unchanged summary retains it."
  (when-let* ((overlay (and (hash-table-p mevedel-view--interaction-overlays)
                          (gethash 'pending-inputs
                                   mevedel-view--interaction-overlays)))
              ((overlay-buffer overlay)))
    (let ((selection (overlay-get overlay 'mevedel-pending-input-preview)))
      (if (and (eq (car selection) session)
               (cdr selection)
               (cl-find (cdr selection) entries
                        :key (lambda (entry) (plist-get entry :id))))
          (cdr selection)
        (overlay-put overlay 'mevedel-pending-input-preview nil)
        nil))))

(defun mevedel-pending-inputs-preview-activate (&optional event)
  "Inspect the pending input at point or mouse EVENT without holding delivery.
Only the explicitly labeled management action opens the queue cockpit."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (when-let* ((target (get-text-property (point) 'mevedel-pending-input-target))
              (session (mevedel-view--session))
              ((eq (car target) session))
              (overlay (get-text-property
                        (point) 'mevedel-view-interaction-overlay))
              ((overlayp overlay))
              ((eq (overlay-buffer overlay) (current-buffer)))
              ((<= (overlay-start overlay) (point)))
              ((< (point) (overlay-end overlay))))
    (if (eq (cdr target) 'manage)
        (mevedel-pending-inputs-open)
      (when-let* ((id (cdr target))
                  ((cl-find id
                            (append (mevedel-session-pending-steering session)
                                    (mevedel-session-pending-follow-ups session))
                            :key (lambda (entry) (plist-get entry :id)))))
        (overlay-put overlay 'mevedel-pending-input-preview
                     (unless (equal target (overlay-get
                                            overlay 'mevedel-pending-input-preview))
                       target))
        (mevedel-view--pending-inputs-render session)))))

(defun mevedel-pending-inputs--preview-detail (entry)
  "Return ENTRY's plain input, bounded to ten width-limited display lines.
Long inputs are explicitly labeled partial.  Text is never interpreted as
buttons, Markdown, or interaction authority."
  (let* ((width (max 1 (min 80 (- (window-body-width
                                  (get-buffer-window (current-buffer) t))
                                 6))))
         (input (or (plist-get entry :input) ""))
         ;; Bound normalization and property copying as well as line layout.
         ;; Async queue refresh must not rescan a pasted megabyte-long input.
         (prefix (substring-no-properties
                  input 0 (min (length input) (1+ (* 20 width)))))
         (partial (> (length input) (length prefix)))
         (text (replace-regexp-in-string
                "[\t\r]" " " (mevedel--normalize-message-text prefix) t t))
         (position 0)
         (lines 0)
         parts)
    (while (and (< position (length text)) (< lines 10))
      ;; Inspect only a bounded prefix, even for a single enormous line.
      (let* ((sample (substring text position
                                (min (length text) (+ position (* 2 width)))))
             (newline (string-search "\n" sample))
             (line (substring sample 0 newline))
             (visible (truncate-string-to-width line width))
             (count (length visible)))
        (when (and (= count 0) (not (string-empty-p line)))
          (setq visible (substring line 0 1) count 1))
        (push (concat "    " visible "\n") parts)
        (cl-incf position count)
        (when (and newline (= count newline)) (cl-incf position))
        (cl-incf lines)))
    (concat (if parts (apply #'concat (nreverse parts)) "    (Empty input)\n")
            (when (or partial (< position (length text)))
              "    [Partial preview: 10 lines maximum; Manage queue for full input.]\n"))))

(defun mevedel-view--pending-input-category-body (label entries &optional session selected)
  "Return compact pending-input summary for LABEL and ENTRIES.
SESSION and SELECTED identify the locally expanded entry.  Only an entry
whose summary line cuts its input short can expand: expanding a fully
shown input would repeat it.  Expanded headers omit the abbreviated input
because the detail body already shows it."
  (let ((index 0)
        lines)
    (dolist (entry (seq-take entries 3))
      (cl-incf index)
      (let* ((id (plist-get entry :id))
             (input (or (plist-get entry :input) ""))
             (preview (mevedel-view--pending-input-preview input))
             ;; Compare within the preview's own bounded prefix: a pasted
             ;; megabyte must not be normalized on every render.
             (prefix (substring-no-properties input 0 (min (length input) 1024)))
             (partial (or (> (length input) (length prefix))
                          (not (equal preview
                                      (string-trim
                                       (mevedel--normalize-message-text prefix))))))
             (expanded (and partial id (equal id selected)))
             (text (concat
                    (format "%d. %s" index
                            (cond (expanded "[-]") (partial "[+]") (t "   ")))
                    (when-let* ((scope (plist-get entry :scope)))
                      (format " [◆ %s]"
                              (mevedel-view-composer-scope-label scope)))
                    (unless expanded (concat " " preview)))))
        (when (and session id)
          (add-text-properties
           0 (length text)
           (list 'mevedel-pending-input-row (cons session id)) text))
        (when (and session id partial)
          (add-text-properties
           0 (length text)
           (list 'mevedel-pending-input-target (cons session id)
                 'mouse-face 'highlight
                 'help-echo "Expand or collapse input preview; delivery continues")
           text))
        (push (concat "  " text
                      (when expanded
                        (concat "\n" (string-remove-suffix
                                      "\n" (mevedel-pending-inputs--preview-detail entry)))))
              lines)))
    (when (> (length entries) 3)
      (push (format "  %d more" (- (length entries) 3)) lines))
    (concat "\n" label "\n"
            (string-join (nreverse lines) "\n")
            "\n")))

(defun mevedel-view--pending-inputs-body (session)
  "Return the main-view pending-input summary for SESSION."
  (let ((selected
         (mevedel-pending-inputs--preview-selection
          session (append (seq-take (mevedel-session-pending-steering session) 3)
                          (seq-take (mevedel-session-pending-follow-ups session) 3)))))
    (concat
     "\nC-c C-w retract newest"
     (when (and (buffer-live-p mevedel--data-buffer)
                (not (mevedel-turn-busy-p mevedel--data-buffer)))
       " · C-c C-r resume")
     " · d/DEL drop row\n"
     (when (mevedel-session-pending-input-failure-paused session)
       "\nPending-input delivery stopped after turn failure; review required\n")
     (when (mevedel-session-pending-input-paused session)
       (if (and (not (mevedel-turn-busy-p mevedel--data-buffer))
                (not (mevedel-pending-inputs-cockpit-holds-p session)))
           "\nPending input waiting for Resume (C-c C-r)\n"
         "\nPending-input delivery paused\n"))
     (when-let* ((entries (mevedel-session-pending-steering session)))
       (mevedel-view--pending-input-category-body "Steering" entries session selected))
     (when-let* ((entries (mevedel-session-pending-follow-ups session)))
       (mevedel-view--pending-input-category-body "Follow-ups" entries session selected))
     "\n"
     (propertize "Manage queue…" 'mevedel-pending-input-target (cons session 'manage)
                 'mouse-face 'highlight 'face 'link)
     " (pauses delivery; C-c C-e)\n")))

(defun mevedel-view--pending-inputs-render (&optional session)
  "Render SESSION pending input into the interaction zone."
  (when-let* ((session (or session (mevedel-view--session))))
    (let ((entries
           (append (mevedel-session-pending-steering session)
                   (mevedel-session-pending-follow-ups session))))
      (when entries
        (mevedel-view--interaction-register
         (list :kind 'pending-input
               :id 'pending-inputs
               :count (length entries)
               :body (lambda () (mevedel-view--pending-inputs-body session))
               :keymap mevedel-view--pending-inputs-map))))))

(cl-defun mevedel-view-enqueue-external-follow-up
    (data-buffer text &key guest-name guest-id paths directive-id invoke
                 guest-role shared-question skills)
  "Queue TEXT as a follow-up that originated outside this Emacs.

DATA-BUFFER owns the session.  GUEST-NAME attributes the entry to a
collaboration guest; GUEST-ID is that guest's stable identity, which
the retract seam checks so only the sender can take the entry back.
PATHS are files to mention as @file tokens with read grants, like an
Emacs-side drop; the entry remembers them so a retraction can delete
them.  DIRECTIVE-ID, when given, scopes the entry to that directive's
discussion; the caller has already checked that the directive exists.

Skill tokens in TEXT stay literal at submission: external free text
carries prompting authority only, never skill invocation.  The one
exception is INVOKE, a command or skill name the caller has already
validated against its own allowlist for the guest's link tier
GUEST-ROLE: TEXT is then that invocation's arguments, dispatched
through `mevedel-view-run-invocation' at delivery and rechecked
against the same allowlist and tier first.  Return the queued entry,
or nil without a live session view.  SHARED-QUESTION is host-validated
shared-item attribution retained with the delivered prompt.  SKILLS names
explicitly selected skills applied together, with the same admission recheck."
  (when-let* (((buffer-live-p data-buffer))
              (view-buffer (buffer-local-value 'mevedel--view-buffer
                                               data-buffer))
              ((buffer-live-p view-buffer))
              (session (buffer-local-value 'mevedel--session data-buffer)))
    (with-current-buffer view-buffer
      (let ((input (mevedel--normalize-message-text text)))
        (when paths
          (setq input (concat input " "
                              (mapconcat #'mevedel-mentions-file-token
                                         paths " ")))
          (dolist (path paths)
            (mevedel-session-add-dropped-file-grant session path)))
        (let ((entry (mevedel-session-enqueue-pending-input
                      session 'follow-up
                      (list :input input
                            :guest-name guest-name
                            :guest-id guest-id
                            :guest-paths paths
                            :guest-invoke invoke
                            :guest-skills skills
                            :guest-role guest-role
                            :shared-question shared-question
                            ;; An invocation is dispatched by name at
                            ;; delivery; its arguments stay inert text.
                            :inert-skills t
                            ;; External input can only discuss.  The
                            ;; sender knows a directive id and nothing
                            ;; else, and discussion is the one directive
                            ;; action that mutates nothing.  Delivery
                            ;; takes the scope branch, which does not
                            ;; consult `:inert-skills'; discussion stays
                            ;; skill-inert because directive dispatch
                            ;; never plans skills and already refuses
                            ;; slash lines.  A scoped action that did
                            ;; would have to honour the flag there.
                            :scope (when directive-id
                                     (list :directive-id directive-id
                                           :action 'discuss))
                            :dropped-file-grants
                            (mevedel-view--pop-dropped-file-grants-for-input
                             input session)
                            :queued-at-time (float-time)
                            :queued-at-turn
                            (or (mevedel-session-turn-count session) 0)))))
          ;; A guest's message is a human instruction: it ends the wait.
          (mevedel-permission-absence-clear session)
          (mevedel-pending-inputs-follow-up-changed session)
          (mevedel-view--interaction-rebuild)
          (mevedel-pending-inputs-schedule-held-input-delivery)
          entry)))))

(defun mevedel-view--queue-follow-up (input)
  "Queue INPUT to start a separate root turn."
  (setq input (mevedel--normalize-message-text input))
  (let ((session (mevedel-view--session)))
    (unless session
      (user-error "No active session for follow-up"))
    (mevedel-permission-absence-clear session)
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
      (mevedel-pending-inputs-follow-up-changed session)
      (mevedel-view--interaction-rebuild)
      (message "mevedel: queued follow-up for a separate turn")
      (mevedel-pending-inputs-schedule-held-input-delivery)
      entry)))

(defun mevedel-view--steering-request-context-supported-p (context)
  "Return non-nil when prepared skill CONTEXT can steer an active request."
  (cl-loop for (key value) on context by #'cddr
           always
           (pcase key
             (:invoked-skills t)
             (:ptc-primitives (eq value :unrestricted))
             ((or :permission-rules :hook-rules :model :effort)
              (null value))
             (_ nil))))

(defun mevedel-view--steerable-root-request-p (request)
  "Return non-nil when REQUEST is a root turn that can still take steering."
  (and request (mevedel-engine-steerable-p (mevedel-engine-owner request))))

(defun mevedel-view--prepare-steering-entry (submission request)
  "Return a validated steering entry for SUBMISSION and REQUEST.
Return nil and leave the submission pending when the live request contract no
longer accepts the prepared input."
  (let* ((session (mevedel-view--session))
         (outcome (mevedel-prompt-submission-outcome submission))
         (request-context (plist-get outcome :request-context))
         (model-input (plist-get outcome :model-input))
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
          (not (mevedel-view--steerable-root-request-p request)))
      (message "mevedel: request can no longer be steered; use C-c TAB")
      nil)
     (t
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
         (or (mevedel-session-turn-count session) 0)))))))

(defun mevedel-view--queue-prepared-steering
    (submission request &optional preserve-draft)
  "Queue accepted prompt SUBMISSION as steering for REQUEST.
When PRESERVE-DRAFT is non-nil, leave the composer untouched."
  (when-let* ((prepared
               (mevedel-view--prepare-steering-entry submission request))
              (session (mevedel-view--session))
              (entry
               (mevedel-session-enqueue-pending-input
                session 'steering prepared)))
    (mevedel-recovery-save session)
    ;; Queued steering is a user message: it ends a wait after expiry.
    (mevedel-permission-absence-clear session)
    (let ((input (plist-get entry :input)))
      (mevedel-view-history-add input)
      (when (and (not preserve-draft)
                 (equal-including-properties
                  (mevedel-view--input-text) input))
        (mevedel-view--clear-input))
      (mevedel-view--interaction-rebuild)
      (when (mevedel-agent-control-root-waiting-p session)
        (mevedel-agent-control-wake-root-user session))
      (message "mevedel: queued steering for this turn")
      entry)))

(defun mevedel-pending-inputs--assert-removable (session entry)
  "Refuse removal of SESSION's ENTRY when delivery or editing owns it."
  (unless (memq entry (append (mevedel-session-pending-steering session)
                             (mevedel-session-pending-follow-ups session)))
    (user-error "Pending input is no longer queued"))
  (when (or (eq (plist-get entry :state) 'dispatching)
            (plist-get entry :delivering))
    (user-error "Pending input is being dispatched; it can no longer be retracted or deleted"))
  (when-let* ((data (mevedel-session-root-buffer session))
              ((buffer-live-p data))
              (view (buffer-local-value 'mevedel--view-buffer data))
              ((buffer-live-p view))
              (edit (buffer-local-value 'mevedel-view--pending-input-edit view))
              ((equal (plist-get edit :id) (plist-get entry :id))))
    (user-error "Save or cancel the pending-input edit first"))
  (when-let* ((cockpit (get-buffer mevedel-pending-inputs-buffer-name))
              ((mevedel-pending-inputs-cockpit-holds-p session))
              ((equal (buffer-local-value 'mevedel-pending-inputs--converting-id cockpit)
                      (plist-get entry :id))))
    (user-error "Pending input conversion is still running")))

(defun mevedel-pending-inputs--remove (session entry &optional validate)
  "Remove SESSION's ENTRY after checking authority and delivery ownership.
Call VALIDATE, when non-nil, immediately before removal, after yielding checks."
  (mevedel-session-artifacts-assert-new-mutation-authority session)
  ;; Authority checks can yield to a transport.  Recheck the live entry last.
  (mevedel-pending-inputs--assert-removable session entry)
  (when validate (funcall validate))
  (let ((category (plist-get entry :category)))
    (mevedel-pending-inputs--set-queues
     session category (remq entry (mevedel-session-pending-inputs session category))))
  (mevedel-pending-inputs--discard (list entry)))

;;;###autoload
(defun mevedel-pending-inputs-drop-at-point ()
  "Delete the pending entry on this summary row without pausing delivery."
  (interactive)
  (require 'mevedel-view-composer)
  (mevedel-view--ensure-interactive-chat-view)
  (let* ((row (text-property-not-all (line-beginning-position) (line-end-position)
                                     'mevedel-pending-input-row nil))
         (target (and row (get-text-property row 'mevedel-pending-input-row)))
         (session (mevedel-view--session))
         (entry (and (eq (car-safe target) session)
                     (cl-find (cdr target)
                              (append (mevedel-session-pending-steering session)
                                      (mevedel-session-pending-follow-ups session))
                              :key (lambda (item) (plist-get item :id))
                              :test #'equal))))
    (unless entry (user-error "No queued pending input on this row"))
    (mevedel-view--assert-live-tip)
    (mevedel-pending-inputs--remove session entry)
    (mevedel-view--interaction-rebuild)))

(defun mevedel-pending-inputs--assert-retract-composer (scope)
  "Refuse withdrawal into a busy composer or a saved draft for SCOPE."
  (when (or mevedel-view--pending-input-edit
            mevedel-view--prompt-hook-pending mevedel-view--pending-skill-submission)
    (user-error "Finish the current composer submission or edit first"))
  (unless (string-empty-p (or (mevedel-view--visible-draft) ""))
    (user-error "Composer holds a draft; clear or send it before retracting"))
  (unless (equal scope mevedel-view--composer-scope)
    (when-let* ((saved (gethash (if scope
                                  (list (plist-get scope :directive-id)
                                        (plist-get scope :action)
                                        (plist-get scope :attempt-index))
                                'chat)
                              mevedel-view--composer-drafts))
                ((not (string-empty-p (plist-get saved :text)))))
      (user-error "The input's composer scope holds a draft; clear or send it first"))))

;;;###autoload
(defun mevedel-view-retract-pending ()
  "Withdraw the newest pending input into an empty composer.
Restore its bound mentions, attachment grants, scope and reserved context.
Delivery is not paused; an entry already being dispatched cannot be withdrawn."
  (interactive)
  (require 'mevedel-view-composer)
  (mevedel-view--ensure-interactive-chat-view)
  (mevedel-view--assert-live-tip)
  (when mevedel-view--side-conversation-p
    (user-error "/btw has no pending input queue"))
  (let* ((session (mevedel-view--session))
         ;; Ids record acceptance order even after reordering or conversion.
         (entry (cl-reduce (lambda (a b)
                             (if (or (null a) (> (plist-get b :id) (plist-get a :id))) b a))
                           (append (mevedel-session-pending-steering session)
                                   (mevedel-session-pending-follow-ups session))
                           :initial-value nil))
         (scope (plist-get entry :scope))
         (composer-scope mevedel-view--composer-scope)
         (text (and entry (mevedel-mention-bindings-copy-text (plist-get entry :input)))))
    (unless entry (user-error "No pending input to retract"))
    (mevedel-pending-inputs--assert-retract-composer scope)
    (mevedel-pending-inputs--remove
     session entry
     (lambda ()
       (mevedel-pending-inputs--assert-retract-composer scope)
       (unless (equal composer-scope mevedel-view--composer-scope)
         (user-error "Composer scope changed; pending input was not retracted"))))
    (unless (equal scope mevedel-view--composer-scope)
      (mevedel-view--switch-composer-scope scope))
    (mevedel-view--restore-composer-snapshot
     (list :text text :point-offset (length text)
           :dropped-file-grants
           (delete-dups (append (plist-get entry :dropped-file-grants)
                                (mevedel-session-dropped-file-grants session))))
     session t)
    (mevedel-view-refresh-input-prompt)
    (mevedel-view--interaction-rebuild)
    (message "mevedel: retracted pending input into the composer")))

(defun mevedel-pending-inputs--resume-entry (session entries)
  "Combine SESSION's steering ENTRIES into one ordered follow-up.
Live preparations retain their accepted outcomes without rerunning hooks.
After restore only authored text remains, so normal submission prepares it."
  (let* ((input (mapconcat #'mevedel-pending-inputs--copy-input entries "\n\n"))
         (submissions (mapcar (lambda (entry) (plist-get entry :submission)) entries))
         (prepared (cl-every #'identity submissions))
         (entry (list :id (apply #'max (mapcar (lambda (item) (plist-get item :id)) entries))
                      :category 'follow-up :input input :state 'pending
                      :queued-at-time (plist-get (car entries) :queued-at-time)
                      :dropped-file-grants
                      (delete-dups
                       (cl-loop for item in entries append
                                (copy-sequence (plist-get item :dropped-file-grants)))))))
    (if prepared
        (let* ((audits (cl-loop for item in entries append (plist-get item :hook-audits)))
               (skills (cl-loop for item in entries append
                                (plist-get (plist-get item :request-context) :invoked-skills)))
               (submission
                (mevedel-prompt-submission-create
                 :session session :state 'reserved :input input :display-text input
                 :context-entries
                 (cl-loop for item in submissions append
                          (mevedel-prompt-submission-context-entries item))
                 :audits audits
                 :outcome
                 (list :hook-input input :hook-audits audits
                       :model-input (mapconcat (lambda (item) (plist-get item :model-input))
                                               entries "\n\n")
                       :transcript-input (mapconcat (lambda (item) (plist-get item :transcript-payload))
                                                    entries "\n\n")
                       :request-context (list :invoked-skills skills)))))
          (setq entry (plist-put entry :submission submission)))
      (when (cl-some #'identity submissions)
        (error "Prepare restored steering before combining it with live submissions")))
    entry))

(defun mevedel-pending-inputs--prepare-restored-steering (session entry)
  "Prepare restored ENTRY in SESSION without rerunning live submissions.
Continue Resume after accepting this part.  Until then ENTRY remains queued
and claimed, and every existing failure or interruption hold stays intact."
  (let* ((view (current-buffer))
         (release (lambda ()
                    (when (buffer-live-p view)
                      (with-current-buffer view
                        (when (eq entry mevedel-view--pending-resume-entry)
                          (mevedel-pending-inputs-cancel-resume)))))))
    (setq mevedel-view--pending-resume-entry entry)
    (plist-put entry :delivering t)
    (condition-case err
        (mevedel-view--submit-planned-input
         (mevedel-pending-inputs--copy-input entry) nil release
         (lambda (submission)
           (with-current-buffer view
             (mevedel-session-artifacts-assert-new-mutation-authority session)
             (unless (memq entry (mevedel-session-pending-steering session))
               (user-error "Pending steering changed while Resume was preparing it"))
             (let* ((outcome (mevedel-prompt-submission-outcome submission))
                    (context (plist-get outcome :request-context))
                    (prepared (copy-sequence entry)))
               (when (or (plist-get outcome :fork-outcome)
                         (not (mevedel-view--steering-request-context-supported-p context)))
                 (user-error "Restored skill policy cannot combine with live steering; edit the input first"))
               (mevedel-prompt-submission-reserve submission)
               (dolist (pair (list (cons :submission submission)
                                   (cons :model-input (plist-get outcome :model-input))
                                   (cons :transcript-payload
                                         (concat (plist-get outcome :transcript-input)
                                                 (plist-get outcome :render-data)))
                                   (cons :hook-audits (plist-get outcome :hook-audits))
                                   (cons :request-context context)
                                   (cons :delivering nil)))
                 (setq prepared (plist-put prepared (car pair) (cdr pair))))
               (mevedel-pending-inputs--replace
                session 'steering (plist-get entry :id) prepared))
             (funcall release)
             (mevedel-view-resume-pending))))
      ((error quit)
       (funcall release)
       (signal (car err) (cdr err))))))

(defun mevedel-pending-inputs-cockpit-holds-p (session)
  "Return non-nil when SESSION's Pending Inputs cockpit is open."
  (when-let* ((cockpit (get-buffer mevedel-pending-inputs-buffer-name)))
    (eq session (mevedel-cockpit-context-session
                 (buffer-local-value 'mevedel-cockpit--context cockpit)))))

;;;###autoload
(defun mevedel-view-resume-pending ()
  "Resume idle pending input, starting one turn for leftover steering.
Steering is combined in queue order into the first follow-up.  Clear the
interrupt and failure holds, then let ordinary queued follow-ups drain.
An unaccepted dispatch retains the combined entry for retry or retraction."
  (interactive)
  (require 'mevedel-view-composer)
  (mevedel-view--ensure-interactive-chat-view)
  (mevedel-view--assert-live-tip)
  (when mevedel-view--side-conversation-p
    (user-error "/btw has no pending input queue"))
  (when (or (mevedel-turn-busy-p mevedel--data-buffer)
            mevedel-view--prompt-hook-pending mevedel-view--pending-skill-submission
            mevedel-view--pending-input-edit
            (buffer-local-value 'mevedel-compact-run-in-flight mevedel--data-buffer))
    (user-error "Wait for or interrupt the active turn or composer submission first"))
  (unless (string-empty-p (or (mevedel-view--visible-draft) ""))
    (user-error "Composer holds a draft; clear or send it before resuming"))
  (let* ((session (mevedel-view--session))
         (steering (mevedel-session-pending-steering session))
         (follow-ups (mevedel-session-pending-follow-ups session)))
    (unless (or steering follow-ups) (user-error "No pending input to resume"))
    (when (mevedel-pending-inputs-cockpit-holds-p session)
      (user-error "Close the Pending Inputs cockpit before resuming"))
    (mevedel-session-artifacts-assert-new-mutation-authority session)
    (mevedel-recovery-assert-ready session)
    ;; Authority checks can process callbacks or new composer input.
    (when (or (mevedel-turn-busy-p mevedel--data-buffer)
              (not (string-empty-p (or (mevedel-view--visible-draft) "")))
              mevedel-view--prompt-hook-pending mevedel-view--pending-skill-submission
              mevedel-view--pending-input-edit)
      (user-error "Session or composer changed before Resume; pending input remains queued"))
    (when (mevedel-pending-inputs-cockpit-holds-p session)
      (user-error "Close the Pending Inputs cockpit before resuming"))
    (setq steering (mevedel-session-pending-steering session)
          follow-ups (mevedel-session-pending-follow-ups session))
    (unless (or steering follow-ups) (user-error "No pending input to resume"))
    (dolist (entry (append steering follow-ups))
      (mevedel-pending-inputs--assert-removable session entry))
    (if (and (cl-some (lambda (entry) (plist-get entry :submission)) steering)
             (cl-some (lambda (entry) (not (plist-get entry :submission))) steering))
        (mevedel-pending-inputs--prepare-restored-steering
         session (cl-find-if (lambda (entry) (not (plist-get entry :submission))) steering))
      ;; Keep the existing drain's durable dispatch intent and commit fence.
      ;; Resume is explicit consent to retry failed follow-ups as well.
      (mevedel-pending-inputs--set-queues
       session 'steering nil
       'follow-up (append (when steering
                            (list (mevedel-pending-inputs--resume-entry session steering)))
                          (mapcar (lambda (entry)
                                    (if (eq (plist-get entry :state) 'failed-turn)
                                        (plist-put (copy-sequence entry) :state 'pending)
                                      entry))
                                  follow-ups)))
      (mevedel-session-set-pending-input-failure-paused session nil)
      (mevedel-session-set-pending-input-paused session nil)
      (mevedel-permission-absence-clear session)
      (mevedel-recovery-save session)
      (mevedel-view--interaction-rebuild)
      (mevedel-view--drain-follow-up mevedel--data-buffer))))

(defun mevedel-view-send-follow-up ()
  "Queue the composer as a follow-up, or send normally while idle."
  (interactive)
  (require 'mevedel-compact-run)
  (mevedel-view--ensure-interactive-chat-view)
  (when mevedel-view--side-conversation-p
    (user-error "/btw does not queue follow-ups; wait for the active response"))
  (mevedel-view--assert-live-tip)
  (when mevedel-view--pending-input-edit
    (user-error "Save or cancel the pending-input edit first"))
  (unless (and mevedel--data-buffer (buffer-live-p mevedel--data-buffer))
    (user-error "No live data buffer associated with this view"))
  (mevedel-session-durability-with-transaction
   (let* ((session (buffer-local-value 'mevedel--session
                                       mevedel--data-buffer))
          (occupied
           (or (buffer-local-value 'mevedel--current-request
                                   mevedel--data-buffer)
               (mevedel-session-pending-follow-ups session)
               (mevedel-view--occupied-root-workflow session)
               mevedel-view--prompt-hook-pending
               mevedel-view--pending-skill-submission
               (buffer-local-value 'mevedel-compact-run-in-flight
                                   mevedel--data-buffer))))
     (mevedel-session-artifacts-assert-new-mutation-authority session)
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
         (when (mevedel-skills-parse-slash-line input)
           (user-error "Slash commands cannot be queued as follow-ups"))
         (mevedel-view--queue-follow-up input)))))
  (goto-char (point-max)))


;;
;;; Automatic delivery

(defun mevedel-view--drop-disallowed-guest-skills (session)
  "Drop SESSION's queued guest invocations the allowlist no longer admits.
A guest invocation is validated when its frame arrives and again here
at delivery, against the link tier the entry was queued under, so
shrinking `mevedel-collaboration-guest-skills' or adding a name to the
unsafe list takes effect for entries already waiting.  Return the
remaining queue."
  (let ((entries (mevedel-session-pending-follow-ups session))
        dropped)
    (dolist (entry entries)
      (let ((name (plist-get entry :guest-invoke))
            (skills (plist-get entry :guest-skills))
            (role (plist-get entry :guest-role)))
        (when (or (and name
                       (not (and (fboundp 'mevedel-collaboration--guest-invocable-p)
                                 (mevedel-collaboration--guest-invocable-p name role))))
                  (and skills
                       (not (and (fboundp 'mevedel-collaboration--guest-skills-admitted-p)
                                 (mevedel-collaboration--guest-skills-admitted-p
                                  skills role session)))))
          (push entry dropped))))
    (when dropped
      (mevedel-pending-inputs--set-queues
       session 'follow-up
       (cl-remove-if (lambda (entry) (memq entry dropped)) entries))
      (mevedel-view--interaction-rebuild)
      (message
       "mevedel: dropped %d queued guest invocation%s no longer allowed"
       (length dropped) (if (= 1 (length dropped)) "" "s")))
    (mevedel-session-pending-follow-ups session)))

(defun mevedel-view--pending-follow-up-kind (entry)
  "Return the delivery kind derived from pending follow-up ENTRY."
  (cond
   ((plist-get entry :scope) 'directive)
   ((plist-get entry :guest-invoke) 'invocation)
   ((plist-get entry :guest-skills) 'skills)
   ((plist-get entry :submission) 'prepared)
   (t 'prompt)))

(defun mevedel-view--dispatch-directive-follow-up
    (entry input session before-send after-insert release)
  "Dispatch directive follow-up ENTRY with INPUT in SESSION.
Call BEFORE-SEND before dispatch and AFTER-INSERT after acceptance.
Call RELEASE on failure, dropping a permanently stale directive entry."
  (let ((scope (plist-get entry :scope)))
    (condition-case scope-err
        (progn
          (funcall before-send)
          (mevedel-view--dispatch-directive-input scope input)
          (funcall after-insert))
      (error
       (funcall release)
       ;; A directive that no longer exists can never deliver, and the drain
       ;; always picks the head: keeping the entry would wedge every follow-up
       ;; queued behind it.  Transient failures keep it for a later drain.
       (when-let* ((directive-id (plist-get scope :directive-id))
                   (workspace (mevedel-session-workspace session))
                   ((not (cl-find directive-id
                                  (mevedel-workspace-directives workspace)
                                  :key #'mevedel-directive-id
                                  :test #'equal))))
         (mevedel-pending-inputs--set-queues
          session 'follow-up
          (delq entry (mevedel-view--pending-follow-ups session)))
         (mevedel-view--interaction-rebuild)
         (mevedel-pending-inputs-schedule-held-input-delivery))
       (message "mevedel: queued directive follow-up failed: %s"
                (error-message-string scope-err))))))

(defun mevedel-view--dispatch-follow-up-entry
    (kind entry input session data-buffer before-send after-insert release)
  "Dispatch follow-up ENTRY of KIND with INPUT for SESSION.
DATA-BUFFER owns the root transcript.  BEFORE-SEND, AFTER-INSERT, and RELEASE
implement the drain's shared delivery transaction."
  (pcase kind
    ('directive
     (mevedel-view--dispatch-directive-follow-up
      entry input session before-send after-insert release))
    ('invocation
     ;; A guest invocation is a command or skill name, not a prompt: it has
     ;; to reach the same dispatch typing the line in the composer would.
     (funcall before-send)
     (mevedel-view-run-invocation
      (plist-get entry :guest-invoke) input
      :on-quiet after-insert
      :on-sent after-insert))
    ('skills
     (mevedel-view--submit-planned-input
      input before-send release nil after-insert nil
      (plist-get entry :guest-skills)))
    ('prepared
     (mevedel-view--dispatch-prepared-outcome
      (plist-get entry :submission) data-buffer
      :before-send before-send
      :after-insert after-insert
      :on-block release))
    ('prompt
     (mevedel-view--submit-planned-input
      input before-send release nil after-insert
      (plist-get entry :inert-skills)))
    (_ (error "Unknown pending follow-up kind: %S" kind))))

(defun mevedel-view--drain-follow-up (data-buffer)
  "Submit the next pending follow-up for DATA-BUFFER.

Each bound entry is planned and prepared as its own turn.  The queue entry is
removed only when the resulting prompt reaches its transcript commit boundary."
  (when (buffer-live-p data-buffer)
    (mevedel-session-durability-with-transaction
     (let* ((view-buffer (buffer-local-value 'mevedel--view-buffer data-buffer))
            (session (buffer-local-value 'mevedel--session data-buffer)))
       ;; A settling turn or a running compaction refuses dispatch only for
       ;; now; the entry waits for the drain that follows it.
       (when (and session
                  (buffer-live-p view-buffer)
                  (not (mevedel-turn-busy-p data-buffer))
                  (not (buffer-local-value 'mevedel-compact-run-in-flight
                                           data-buffer)))
         (with-current-buffer view-buffer
           (when (and (not mevedel-view--agent-transcript-p)
                      (not mevedel-view--prompt-hook-pending)
                      (not mevedel-view--pending-skill-submission)
                      (not (mevedel-view--follow-up-auto-drain-blocked-p
                            session))
                      (string-empty-p (mevedel-view--input-text)))
             (mevedel-session-artifacts-assert-new-mutation-authority
              session)
             (when (mevedel-view--drop-disallowed-guest-skills session)
               (let* ((entry (mevedel-view--next-follow-up session))
                      (kind (mevedel-view--pending-follow-up-kind entry))
                      (input (mevedel-view--pending-input-text entry))
                      (dropped-file-grants
                       (plist-get entry :dropped-file-grants)))
                 (let* ((active-grants-before
                         (copy-sequence
                          (mevedel-session-active-dropped-file-grants
                           session)))
                        (delivered nil)
                        (transcript-tick (buffer-chars-modified-tick data-buffer))
                        (before-send
                         (lambda ()
                           (mevedel-view--activate-dropped-file-grants
                            dropped-file-grants session)))
                        ;; The entry needs its grant active to expand its own
                        ;; mentions, so an attempt that never reaches the
                        ;; transcript gives that authority back.  Once the
                        ;; prompt is inserted and the entry dequeued it is
                        ;; delivered, and a later failure keeps the grant.
                        (release
                         (lambda ()
                           (plist-put entry :delivering nil)
                           ;; An attempt that touched the transcript or
                           ;; started a turn may have been delivered: it
                           ;; waits for review instead of running again.
                           (unless delivered
                             (if (and (= transcript-tick (buffer-chars-modified-tick data-buffer))
                                      (not (mevedel-turn-busy-p data-buffer)))
                                 (plist-put entry :state nil)
                               (plist-put entry :state 'failed-turn)
                               (mevedel-session-set-pending-input-failure-paused session t))
                             (mevedel-recovery-save session))
                           (unless delivered
                             (mevedel-session--set-active-dropped-file-grants
                              session active-grants-before))
                           (when (buffer-live-p data-buffer)
                             (with-current-buffer data-buffer
                               (setq mevedel-view--pending-guest-attribution nil)))
                           (mevedel-view--interaction-rebuild)))
                        (after-insert
                         (lambda ()
                           (when (fboundp 'mevedel-telemetry-record)
                             (mevedel-telemetry-record
                              session 'user-message-dequeued
                              :message-hash (secure-hash 'sha256 input)
                              :queue-depth-before
                              (length
                               (mevedel-view--pending-follow-ups session))
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
                           (setq delivered t)
                           ;; The turn's own checkpoints persist the removal;
                           ;; until then a restart reloads the durable
                           ;; `dispatching' mark as input requiring review.
                           (mevedel-session-set-pending-inputs
                            session 'follow-up
                            (remq entry (mevedel-view--pending-follow-ups session)))
                           (when (fboundp 'mevedel-collaboration-notify-queue-changed)
                             (mevedel-collaboration-notify-queue-changed session))
                           (mevedel-view--interaction-rebuild))))
                   ;; Consumed where the prompt and its hook audits are
                   ;; inserted; cleared on every blocked or failed path.
                   (with-current-buffer data-buffer
                     (setq mevedel-view--pending-guest-attribution
                           (when-let* ((name (plist-get entry :guest-name)))
                             (append (list :type 'guest-prompt :name name)
                                     (when-let* ((shared (plist-get entry :shared-question)))
                                       (list :shared shared))))))
                   ;; Delivery is asynchronous and the entry stays queued
                   ;; until the transcript commit; the flag keeps a guest
                   ;; retraction from deleting attachment files an
                   ;; in-flight prompt is about to read.
                   (plist-put entry :state 'dispatching)
                   (mevedel-recovery-save-now session)
                   (plist-put entry :delivering t)
                   ;; A blocked dispatch reports through its own callback,
                   ;; but preparation and the submit hook re-signal instead,
                   ;; so both exits have to release what `before-send' took.
                   ;; A guest may be the only one there: its turn must not
                   ;; wait on a question in Emacs that nobody can answer.
                   (condition-case err
                       (let ((inhibit-interaction
                              (or inhibit-interaction
                                  (and (plist-get entry :guest-id) t))))
                         (mevedel-view--dispatch-follow-up-entry
                          kind entry input session data-buffer
                          before-send after-insert release))
                     (inhibited-interaction
                      (funcall release)
                      (mevedel-view--refuse-guest-follow-up entry session err))
                     ((error quit)
                      (funcall release)
                      ;; A refused guest entry left at the head would block
                      ;; every later message; drop it and tell its sender.
                      (if (and (plist-get entry :guest-id) (not (eq 'quit (car err))))
                          (mevedel-view--refuse-guest-follow-up entry session err)
                        (signal (car err) (cdr err)))))))))))))))

(defun mevedel-view--refused-question (err)
  "Return \": QUESTION\" for an `inhibited-interaction' ERR naming one, else \"\"."
  (let ((question (cadr err)))
    (if (and (stringp question) (not (string-empty-p question)))
        (concat ": " question)
      "")))

(defun mevedel-view--refuse-guest-follow-up (entry session &optional err)
  "Drop or hold guest ENTRY of SESSION, whose turn could not run.
ERR is the `inhibited-interaction' signal, which may carry the question, or
the error that refused the turn.  Its attachment files leave with it, as on
a retraction; the host is warned and the guest told, rather than the queue
retrying it forever.  An attempt that may have been delivered is kept for
review instead.  A refusal that left a blocking recovery issue, such as an
expired provider login, keeps the entry queued: the queue waits for the
issue, and the guest learns its public reason.  Prompts and errors can name
hosts and paths, so only the host sees the question or the error."
  (let* ((inhibited (or (null err) (eq 'inhibited-interaction (car err))))
         (review (eq 'failed-turn (plist-get entry :state)))
         (blocker (and (not (or inhibited review)) (mevedel-recovery-blocker session))))
    (unless (or review blocker)
      (mevedel-pending-inputs--set-queues
       session 'follow-up (remq entry (mevedel-view--pending-follow-ups session)))
      (dolist (path (plist-get entry :guest-paths))
        (when (file-exists-p path)
          (ignore-errors (delete-file path)))))
    (mevedel-view--interaction-rebuild)
    (display-warning
     'mevedel
     (format "A message from %s was %s: %s"
             (or (plist-get entry :guest-name) "a guest")
             (cond (review "interrupted and needs review")
                   (blocker "held")
                   (t "not sent"))
             (if inhibited
                 (concat "its turn needed a decision in Emacs"
                         (mevedel-view--refused-question err))
               (error-message-string err))))
    (when (fboundp 'mevedel-collaboration-notify-guest)
      (mevedel-collaboration-notify-guest
       session (plist-get entry :guest-id)
       (cond
        (review "Your message was interrupted; the owner can review it")
        (blocker (concat (mevedel-recovery-public-message blocker)
                         "; your message will be sent once that is resolved"))
        (inhibited (concat mevedel-collaboration-needs-host-message
                           "; your message was not sent"))
        (t "The host could not run your message; it was not sent"))))))

(defun mevedel-view--run-follow-up-drain (data-buffer)
  "Drain one pending follow-up for DATA-BUFFER if it is live.

The drain admits a new mutation, which is target I/O.  It is scheduled from a
timer, and a timer fires wherever the main loop happens to be waiting -- inside
an unrelated remote operation started by redisplay or another package included
-- so it waits for an idle transport rather than nesting."
  (when (buffer-live-p data-buffer)
    (mevedel-transport-run-when-idle
     (list 'follow-up-drain data-buffer)
     (buffer-local-value 'default-directory data-buffer)
     (lambda ()
       (when (buffer-live-p data-buffer)
         (mevedel-view--drain-follow-up data-buffer))))))

(defun mevedel-pending-inputs-schedule-held-input-delivery (&optional data-buffer)
  "Offer held follow-ups or background mail after a hold clears.
DATA-BUFFER defaults to this view's data buffer.  Recheck at dispatch so a
consumed input does not start a Goal continuation instead."
  (let ((buffer (or data-buffer mevedel--data-buffer)))
    (when (memq (mevedel-pending-inputs--idle-work buffer) '(follow-up event))
      (run-at-time 0 nil #'mevedel-pending-inputs--offer-idle-work-now buffer t))))

(defun mevedel-pending-inputs--idle-work (data-buffer)
  "Return the kind of idle work DATA-BUFFER's session holds, or nil.
The result is `follow-up' while follow-ups are queued, else `event' while
background work has unread waking mail, else `goal' for an active Goal.
Only an idle data buffer with a live root view qualifies."
  (when-let* (((buffer-live-p data-buffer))
              (session (buffer-local-value 'mevedel--session data-buffer))
              ((buffer-live-p
                (buffer-local-value 'mevedel--view-buffer data-buffer)))
              ((not (buffer-local-value 'mevedel--current-request
                                        data-buffer))))
    (cond
     ((mevedel-session-pending-follow-ups session) 'follow-up)
     ((mevedel-agent-control-wake-pending-p session) 'event)
     ((when-let* ((goal (mevedel-session-goal session)))
        (eq (mevedel-goal-status goal) 'active))
      'goal))))

(defun mevedel-pending-inputs-offer-root-event (session &optional data-buffer)
  "Offer SESSION's idle root a turn for committed background mail.
Use DATA-BUFFER when it belongs to the root, including an unsaved session;
otherwise resolve the session's root buffer.  Failure to offer cannot undo
mail delivery: the unread records remain available to the next turn."
  (condition-case err
      (when-let* ((buffer
                   (if (and (buffer-live-p data-buffer)
                            (eq session (buffer-local-value
                                         'mevedel--session data-buffer))
                            (not (buffer-local-value
                                  'mevedel--agent-invocation data-buffer)))
                       data-buffer
                     (mevedel-session-root-buffer session)))
                  ((buffer-live-p buffer)))
        (mevedel-pending-inputs-offer-idle-work buffer))
    (error
     (mevedel--warn-once
      'background-wake "Could not offer a background event turn: %s"
      (error-message-string err)))))

(defun mevedel-pending-inputs-offer-idle-work (data-buffer)
  "Offer DATA-BUFFER's queued idle work again after a hold on it cleared.
Queued follow-ups go first, then a turn for background work that reported;
the turn each one starts continues an active Goal when it settles.  With
neither waiting, an active Goal continues directly.
The decision waits for the current command to finish, and the offered work
re-checks its own gates when it runs, so an offer made while the session is
still occupied does nothing."
  (when (mevedel-pending-inputs--idle-work data-buffer)
    (run-at-time 0 nil #'mevedel-pending-inputs--offer-idle-work-now
                 data-buffer)))

(defun mevedel-pending-inputs--offer-idle-work-now (data-buffer &optional inputs-only)
  "Start the idle work `mevedel-pending-inputs-offer-idle-work' offered.
DATA-BUFFER is checked again, because the offering command may have
started a turn or settled the work itself.  INPUTS-ONLY excludes Goal work."
  (pcase (mevedel-pending-inputs--idle-work data-buffer)
    ('follow-up (mevedel-view--run-follow-up-drain data-buffer))
    ('event (mevedel-pending-inputs--run-event-wake data-buffer))
    ('goal (unless inputs-only
             (mevedel-goal-schedule-continuation
              (buffer-local-value 'mevedel--session data-buffer)
              data-buffer)))))

(defconst mevedel-pending-inputs--event-trigger
  (concat "Background work has reported; its messages follow. "
          "This is not a message from the user.")
  "Fixed prompt of a turn started for background work that reported.
The reports themselves reach the model as mailbox records, never as this
prompt, so their text cannot pose as the user.")

(defun mevedel-pending-inputs--interaction-pending-p (session view-buffer)
  "Return non-nil while SESSION or VIEW-BUFFER awaits a user decision."
  (or (mevedel-session-permission-queue session)
      (mevedel-session-pending-plan-approval session)
      (mevedel-view-interaction-pending-p view-buffer)))

(defvar-local mevedel-pending-inputs--wake-refusal nil
  "Reason the data buffer's last background wake was refused, or nil.
Telemetry records a refusal only when this reason changes.")

(defun mevedel-pending-inputs--event-wake-refusal (session data-buffer view-buffer)
  "Return the gate refusing SESSION's background wake in DATA-BUFFER, or nil.
VIEW-BUFFER is the live root view.  Gates are checked in admission order."
  (cond
   ((or (mevedel-turn-busy-p data-buffer)
        (buffer-local-value 'mevedel-compact-run-in-flight data-buffer))
    'busy)
   ((mevedel-session-pending-follow-ups session) 'drain-blocked-queued)
   ((with-current-buffer view-buffer
      (or mevedel-view--prompt-hook-pending
          mevedel-view--pending-skill-submission))
    'prompt-hook)
   ((mevedel-recovery-blocker session) 'recovery)
   ((mevedel-session-pending-steering session)
    (if (mevedel-session-pending-input-failure-paused session)
        'drain-blocked-latch
      'drain-blocked-queued))
   ;; Background mail is not user input: a failure latch that holds no
   ;; queued input must not strand it.
   ((with-current-buffer view-buffer
      (mevedel-view--follow-up-auto-drain-blocked-p
       session (null (mevedel-session-pending-steering session))))
    (if (with-current-buffer view-buffer
          (mevedel-view--follow-up-auto-drain-blocked-p session t))
        'drain-blocked-queued
      'drain-blocked-latch))
   ((mevedel-pending-inputs--interaction-pending-p session view-buffer)
    'interaction)
   ((not (string-empty-p
          (with-current-buffer view-buffer (mevedel-view--input-text))))
    'composer-draft)))

(defun mevedel-pending-inputs--note-wake-refusal (session data-buffer reason)
  "Record REASON for SESSION's DATA-BUFFER when it differs from the last one."
  (unless (eq reason (buffer-local-value 'mevedel-pending-inputs--wake-refusal
                                         data-buffer))
    (with-current-buffer data-buffer
      (setq mevedel-pending-inputs--wake-refusal reason))
    (when (and reason (fboundp 'mevedel-telemetry-record))
      (mevedel-telemetry-record session 'event-wake-refused :reason reason))))

(defun mevedel-pending-inputs--event-wake-view (data-buffer)
  "Return DATA-BUFFER's live root view when no background wake hold remains.
A refused wake records its gate once per change of reason."
  (when (buffer-live-p data-buffer)
    (let ((view-buffer (buffer-local-value 'mevedel--view-buffer data-buffer))
          (session (buffer-local-value 'mevedel--session data-buffer)))
      (when (and session
                 (buffer-live-p view-buffer)
                 (not (buffer-local-value 'mevedel-view--agent-transcript-p
                                          view-buffer))
                 (mevedel-agent-control-wake-pending-p session))
        (let ((reason (mevedel-pending-inputs--event-wake-refusal
                       session data-buffer view-buffer)))
          (mevedel-pending-inputs--note-wake-refusal session data-buffer reason)
          (unless reason view-buffer))))))

(defun mevedel-pending-inputs--wake-for-event (data-buffer)
  "Start one background-event turn in DATA-BUFFER when no hold remains.
The holds are those of the follow-up drain, plus queued follow-ups, which
go first, and pending user decisions.  The turn is a new request, so it
runs under the session's current permission settings."
  (when-let* ((view-buffer (mevedel-pending-inputs--event-wake-view data-buffer))
              (session (buffer-local-value 'mevedel--session data-buffer)))
    (mevedel-session-durability-with-transaction
     (mevedel-session-artifacts-assert-new-mutation-authority session)
     ;; Authority admission can yield during target I/O.  A user draft,
     ;; another turn, or a consumed wake must win over this stale offer.
     (when (and (eq view-buffer
                    (mevedel-pending-inputs--event-wake-view data-buffer))
                (eq session (buffer-local-value 'mevedel--session data-buffer)))
       (with-current-buffer data-buffer
         (setq mevedel-view--pending-guest-attribution
               (list :type 'background-event))
         (unwind-protect
             (mevedel--submit-generated-turn
              mevedel-pending-inputs--event-trigger
              mevedel-pending-inputs--event-trigger)
           ;; Consumed where the prompt is inserted; a start that
           ;; failed before that must not label a later prompt.
           (setq mevedel-view--pending-guest-attribution nil)))))))

(defun mevedel-pending-inputs--run-event-wake (data-buffer)
  "Start a background-event turn for DATA-BUFFER once the transport is idle.
Like the follow-up drain this admits target I/O from a timer, so it waits
rather than nesting inside a remote operation.  A failure leaves the waking
mail unread for the next turn."
  (when (buffer-live-p data-buffer)
    (mevedel-transport-run-when-idle
     (list 'event-wake data-buffer)
     (buffer-local-value 'default-directory data-buffer)
     (lambda ()
       (condition-case err
           (mevedel-pending-inputs--wake-for-event data-buffer)
         (error
          (message "mevedel: background event turn failed: %s"
                   (error-message-string err))))))))

(defun mevedel-pending-inputs-offer-event-after-turn (fsm)
  "Offer FSM's idle session a turn for background work that reported.
Only a successful turn calls this; a failed one does not wake again, so a
failing wake cannot loop."
  (when-let* ((info (and fsm (mevedel-engine-info fsm)))
              (data-buffer (plist-get info :buffer))
              ((buffer-live-p data-buffer))
              (session (buffer-local-value 'mevedel--session data-buffer))
              ((mevedel-agent-control-wake-pending-p session)))
    (mevedel-pending-inputs-offer-idle-work data-buffer)))

(defun mevedel-view--offer-follow-up-after-draft-change
    (start _end old-length)
  "Offer held idle work once an edit from START empties the composer.
OLD-LENGTH is the length of the replaced text.  Automatic delivery leaves
a composer draft alone, so a follow-up or a background-event turn holds
until the user sends, clears, or deletes that draft.  A draft never holds a
Goal continuation, so its deletion never starts one.  Only a deletion can
empty the composer, and the whitespace scan stops at the first character of
a real draft."
  (when (and (> old-length 0)
             (markerp mevedel-view--input-marker)
             (eq (marker-buffer mevedel-view--input-marker) (current-buffer))
             (>= start (mevedel-view--input-start))
             (or (mevedel-view--pending-follow-ups)
                 (mevedel-agent-control-wake-pending-p (mevedel-view--session)))
             (save-excursion
               (goto-char (mevedel-view--input-start))
               (skip-chars-forward " \t\n\r")
               (= (point) (point-max))))
    (mevedel-pending-inputs-schedule-held-input-delivery)))

(defun mevedel-view--schedule-follow-up-drain (fsm)
  "Schedule the next follow-up after FSM completes successfully."
  (when-let* ((info (and fsm (fboundp 'mevedel-engine-info)
                         (mevedel-engine-info fsm)))
              (data-buffer (plist-get info :buffer))
              ((buffer-live-p data-buffer)))
    (run-at-time 0 nil
                 #'mevedel-view--run-follow-up-drain
                 data-buffer)))

(defun mevedel-pending-inputs--session (&optional context)
  "Return the Pending Inputs session for CONTEXT."
  (or (mevedel-cockpit-context-session
       (or context (mevedel-cockpit-surface-context)))
      (user-error "No mevedel session in this buffer")))

(defun mevedel-pending-inputs--collect (context)
  "Collect pending-input row projections for CONTEXT."
  (let ((session (mevedel-pending-inputs--session context))
        rows)
    (dolist (category '(steering follow-up))
      (let ((position 0)
            (entries (mevedel-session-pending-inputs session category)))
        (dolist (entry entries)
          (push (list :id (plist-get entry :id)
                      :category category
                      :position (cl-incf position)
                      :state (or (plist-get entry :state) 'pending)
                      :entry entry)
                rows))))
    (nreverse rows)))

(defun mevedel-pending-inputs--entry (item _context)
  "Return the tabulated row for pending-input ITEM."
  (let* ((entry (plist-get item :entry))
         (category (plist-get item :category))
         (id (plist-get item :id)))
    (list
     id
     (vector
      (if (eq category 'steering) "Steering" "Follow-up")
      (number-to-string (plist-get item :position))
      (pcase (plist-get item :state)
        ('failed-turn "Needs review")
        (state (capitalize (symbol-name state))))
      (concat
       (when-let* ((scope (plist-get entry :scope)))
         (format "[◆ %s] " (mevedel-view-composer-scope-label scope)))
       (when-let* ((guest (plist-get entry :guest-name)))
         (format "[⇄ %s] " guest))
       (mevedel-view--pending-input-preview
        (mevedel-view--pending-input-text entry)))))))

(defun mevedel-pending-inputs--header (items context)
  "Return the cockpit header for ITEMS and CONTEXT."
  (format "%d pending input%s%s"
          (length items)
          (if (= 1 (length items)) "" "s")
          (let ((session (mevedel-pending-inputs--session context)))
            (concat
             (when (mevedel-session-pending-input-failure-paused session)
               " · failure recovery required")
             (when (mevedel-session-pending-input-paused session)
               " · delivery paused")))))

(defun mevedel-pending-inputs--details (item _context)
  "Return the complete information report for pending-input ITEM."
  (list :title "Pending input"
        :sections (list (list :id 'input :title "Full input"
                              :body (mevedel-view--pending-input-text (plist-get item :entry))))))

(defun mevedel-pending-inputs--setup (context)
  "Pause delivery before rendering the cockpit for CONTEXT."
  (let ((session (mevedel-pending-inputs--session context))
        (view (mevedel-cockpit-context-view-buffer context)))
    (mevedel-session-set-pending-input-paused session t)
    (mevedel-recovery-save session)
    (when (buffer-live-p view)
      (with-current-buffer view
        (mevedel-view--interaction-rebuild)))))

(defun mevedel-pending-inputs--selected ()
  "Return the selected pending-input row projection."
  (mevedel-cockpit-surface-selected))

(defun mevedel-pending-inputs--refresh (&optional selected-id)
  "Refresh live pending-input UI, preserving SELECTED-ID and deletion marks."
  (let* ((context (mevedel-cockpit-surface-context))
         (view (mevedel-cockpit-context-view-buffer context)))
    (when (buffer-live-p view)
      (with-current-buffer view
        (mevedel-view--interaction-rebuild)))
    (mevedel-cockpit-surface-refresh selected-id)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (member (tabulated-list-get-id)
                      mevedel-pending-inputs--marked-ids)
          (tabulated-list-put-tag "D"))
        (forward-line 1)))))

(defun mevedel-pending-inputs-refresh ()
  "Refresh the live Pending Inputs cockpit."
  (interactive)
  (mevedel-pending-inputs--refresh (tabulated-list-get-id)))

(defun mevedel-pending-inputs--replace
    (session category id replacement)
  "Replace pending entry ID in SESSION CATEGORY with REPLACEMENT."
  (let* ((entries (mevedel-session-pending-inputs session category))
         (position
          (cl-position-if
           (lambda (entry) (equal id (plist-get entry :id)))
           entries)))
    (unless position
      (user-error "Pending input is no longer queued"))
    (let ((updated (copy-sequence entries)))
      (setcar (nthcdr position updated) replacement)
      (mevedel-pending-inputs--set-queues session category updated))
    replacement))

(defun mevedel-pending-inputs--move-between
    (session source target id replacement)
  "Move ID from SESSION SOURCE to TARGET tail as REPLACEMENT."
  (let ((source-entries (mevedel-session-pending-inputs session source)))
    (unless (cl-find id source-entries
                     :key (lambda (entry) (plist-get entry :id))
                     :test #'equal)
      (user-error "Pending input is no longer queued"))
    (mevedel-pending-inputs--set-queues
     session
     source (cl-remove id source-entries
                       :key (lambda (entry) (plist-get entry :id))
                       :test #'equal)
     target (append (mevedel-session-pending-inputs session target)
                    (list replacement)))
    replacement))

(defun mevedel-pending-inputs--restore-entry-metadata
    (prepared original)
  "Copy stable queue metadata from ORIGINAL to PREPARED."
  (dolist (key '(:id :category :queued-at-time :queued-at-turn
                 :queued-at-goal-id :state))
    (when (plist-member original key)
      (setq prepared (plist-put prepared key (plist-get original key)))))
  prepared)

(defun mevedel-pending-inputs--return-to-cockpit (state)
  "Restore the suspended draft and return to the cockpit for STATE."
  (let* ((context (plist-get state :context))
         (session (mevedel-pending-inputs--session context))
         (view (mevedel-cockpit-context-view-buffer context))
         (cockpit (plist-get state :cockpit-buffer))
         (id (plist-get state :id)))
    (when (buffer-live-p view)
      (with-current-buffer view
        (setq mevedel-view--pending-input-edit nil)
        (mevedel-view--restore-composer-snapshot
         (plist-get state :composer-snapshot) session t)
        (mevedel-view-refresh-input-prompt)
        (mevedel-view--interaction-rebuild)))
    (if (buffer-live-p cockpit)
        (progn
          (with-current-buffer cockpit
            (mevedel-cockpit-surface-refresh id))
          (when-let* ((window (display-buffer cockpit)))
            (select-window window)))
      (when (buffer-live-p view)
        (when-let* ((window (display-buffer view)))
          (select-window window))))))

(defun mevedel-pending-inputs-edit ()
  "Edit the selected pending input in its owning view composer."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (view (mevedel-cockpit-context-view-buffer context))
         (item (mevedel-pending-inputs--selected))
         (entry (plist-get item :entry))
         (category (plist-get item :category))
         (position (plist-get item :position))
         (cockpit (current-buffer)))
    (unless (buffer-live-p view)
      (user-error "No live owning view"))
    (with-current-buffer view
      (when mevedel-view--pending-input-edit
        (user-error "A pending-input edit is already active"))
      (let* ((snapshot (mevedel-view--composer-snapshot session))
             (input (mevedel-mention-bindings-copy-text
                     (plist-get entry :input)))
             (state
              (list
               :id (plist-get entry :id)
               :category category
               :label
               (format "%s %d"
                       (if (eq category 'steering)
                           "steering"
                         "follow-up")
                       position)
               :entry entry
               :context context
               :cockpit-buffer cockpit
               :saving nil
               :composer-snapshot snapshot)))
        (setq mevedel-view--pending-input-edit state)
        (mevedel-view--restore-composer-snapshot
         (list :text input
               :point-offset (length input)
               :dropped-file-grants
               (copy-sequence
                (plist-get entry :dropped-file-grants)))
         session t)
        (mevedel-view-refresh-input-prompt)))
    (when-let* ((window (display-buffer view)))
      (select-window window))))

(defun mevedel-pending-inputs--finish-replacement
    (state replacement)
  "Replace STATE's entry with REPLACEMENT and finish the edit."
  (let* ((context (plist-get state :context))
         (session (mevedel-pending-inputs--session context))
         (original (plist-get state :entry))
         (replacement
          (mevedel-pending-inputs--restore-entry-metadata
           replacement original)))
    (mevedel-pending-inputs--replace
     session
     (plist-get state :category)
     (plist-get state :id)
     replacement)
    (mevedel-pending-inputs--return-to-cockpit state)
    replacement))

(defun mevedel-pending-inputs--save-follow-up (state input session)
  "Save follow-up edit STATE using bound INPUT in SESSION."
  (let* ((original (copy-sequence (plist-get state :entry)))
         (submission (plist-get original :submission))
         (grants
          (mevedel-view--pop-dropped-file-grants-for-input input session))
         (replacement (plist-put original :input input)))
    (when-let* ((shared (plist-get original :shared-question))
                ((not (equal input (plist-get (plist-get state :entry) :input)))))
      ;; Keep receipt identity, but stop presenting the original frozen context
      ;; as the input after the host has edited the queued prompt.
      (setq shared (copy-sequence shared))
      (setq shared (plist-put shared :edited t)
            shared (plist-put shared :text input)
            shared (plist-put shared :quote nil)
            shared (plist-put shared :revision nil)
            shared (plist-put shared :scope nil))
      (setq replacement (plist-put replacement :shared-question shared)))
    (setq replacement
          (plist-put replacement :dropped-file-grants grants)
          replacement (plist-put replacement :submission nil))
    (when submission (mevedel-prompt-submission-restore submission))
    (mevedel-pending-inputs--finish-replacement state replacement)))

(defun mevedel-pending-inputs--save-failed-steering (state input session)
  "Save failed steering edit STATE as review-only INPUT in SESSION."
  (let* ((original (plist-get state :entry))
         (grants
          (mevedel-view--pop-dropped-file-grants-for-input input session))
         replacement)
    (when-let* ((submission (plist-get original :submission)))
      (mevedel-prompt-submission-restore submission))
    (cl-loop for (key value) on original by #'cddr
             unless (memq key '(:model-input :transcript-payload
                                :hook-audits :request-context :submission))
             do (setq replacement
                      (append replacement (list key value))))
    (setq replacement
          (plist-put replacement :input input)
          replacement
          (plist-put replacement :dropped-file-grants grants))
    (mevedel-pending-inputs--finish-replacement state replacement)))

(defun mevedel-pending-inputs--save-steering (state input request)
  "Prepare and save steering edit STATE from INPUT for REQUEST."
  (setf (plist-get state :saving) t)
  (mevedel-view--submit-planned-input
   input nil
   (lambda ()
     (when (eq state mevedel-view--pending-input-edit)
       (setf (plist-get state :saving) nil)
       (message "mevedel: pending-input edit was not accepted")))
   (lambda (submission)
     (when (eq state mevedel-view--pending-input-edit)
       (if-let* ((prepared
                  (mevedel-view--prepare-steering-entry
                   submission request)))
           (mevedel-pending-inputs--finish-replacement state prepared)
         (setf (plist-get state :saving) nil))))))

(defun mevedel-pending-inputs-save-edit ()
  "Validate and save the active pending-input composer edit."
  (interactive)
  (unless mevedel-view--pending-input-edit
    (user-error "No pending-input edit is active"))
  (let* ((state mevedel-view--pending-input-edit)
         (context (plist-get state :context))
         (session (mevedel-pending-inputs--session context))
         (category (plist-get state :category)))
    (mevedel-session-artifacts-assert-new-mutation-authority session)
    (when (plist-get state :saving)
      (user-error "Pending-input preparation is still running"))
    (condition-case err
        (let ((input (mevedel-view--bind-input-mentions session)))
          (when (string-empty-p input)
            (user-error "Pending input must not be empty"))
          (when (string-match-p "\\`[ \t]*[/]" input)
            (user-error "Slash commands cannot be pending input"))
          (cond
           ((eq category 'follow-up)
            (mevedel-pending-inputs--save-follow-up
             state input session))
           ((eq (plist-get (plist-get state :entry) :state)
                'failed-turn)
            (mevedel-pending-inputs--save-failed-steering
             state input session))
           (t
            (let ((request
                   (and (buffer-live-p mevedel--data-buffer)
                        (buffer-local-value
                         'mevedel--current-request mevedel--data-buffer))))
              (unless (and request
                           (equal (mevedel-request-id request)
                                  (plist-get
                                   (plist-get state :entry)
                                   :request-id)))
                (user-error "Steering turn is no longer active"))
              (mevedel-pending-inputs--save-steering
               state input request)))))
      (error
       (message "mevedel: pending-input edit failed: %s"
                (error-message-string err))))))

(defun mevedel-pending-inputs-cancel-edit ()
  "Cancel the active pending-input composer edit."
  (interactive)
  (unless mevedel-view--pending-input-edit
    (user-error "No pending-input edit is active"))
  (let ((state mevedel-view--pending-input-edit))
    (mevedel-pending-inputs--return-to-cockpit state)
    (message "mevedel: pending-input edit cancelled")))

(defun mevedel-pending-inputs--move (offset)
  "Move selected pending input by OFFSET inside its category."
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (item (mevedel-pending-inputs--selected))
         (category (plist-get item :category))
         (id (plist-get item :id))
         (entries (copy-sequence
                   (mevedel-session-pending-inputs session category)))
         (index (cl-position id entries
                             :key (lambda (entry) (plist-get entry :id))
                             :test #'equal))
         (target (and index (+ index offset))))
    (mevedel-session-artifacts-assert-new-mutation-authority session)
    (unless (and target (>= target 0) (< target (length entries)))
      (user-error "Pending input is already at the category boundary"))
    (cl-rotatef (nth index entries) (nth target entries))
    (mevedel-pending-inputs--set-queues session category entries)
    (mevedel-pending-inputs--refresh id)))

(defun mevedel-pending-inputs-move-up ()
  "Move the selected pending input up within its category."
  (interactive)
  (mevedel-pending-inputs--move -1))

(defun mevedel-pending-inputs-move-down ()
  "Move the selected pending input down within its category."
  (interactive)
  (mevedel-pending-inputs--move 1))

(defun mevedel-pending-inputs--current-request (context)
  "Return CONTEXT's current root request, or nil."
  (when-let* ((data-buffer
               (mevedel-cockpit-context-data-buffer context))
              ((buffer-live-p data-buffer)))
    (buffer-local-value 'mevedel--current-request data-buffer)))

(defun mevedel-pending-inputs--copy-input (entry)
  "Return ENTRY's propertized input copy."
  (mevedel-mention-bindings-copy-text (plist-get entry :input)))

(defun mevedel-pending-inputs--conversion-finished
    (cockpit context selected-id)
  "Refresh COCKPIT and CONTEXT after converting SELECTED-ID."
  (if (buffer-live-p cockpit)
      (with-current-buffer cockpit
        (setq mevedel-pending-inputs--converting-id nil)
        (mevedel-pending-inputs--refresh selected-id))
    (when-let* ((view (mevedel-cockpit-context-view-buffer context)))
      (with-current-buffer view
        (mevedel-view--interaction-rebuild)))))

(defun mevedel-pending-inputs-make-steering ()
  "Convert the selected follow-up to steering after full preparation."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (item (mevedel-pending-inputs--selected))
         (entry (plist-get item :entry))
         (id (plist-get item :id))
         (request (mevedel-pending-inputs--current-request context))
         (view (mevedel-cockpit-context-view-buffer context))
         (cockpit (current-buffer))
         (original-grants
          (copy-sequence
           (mevedel-session-dropped-file-grants session)))
         (entry-grants
          (copy-sequence (plist-get entry :dropped-file-grants)))
         (staged-grants
          (cl-set-difference entry-grants original-grants :test #'equal))
         (restore-grants
          (lambda ()
            (let ((current
                   (mevedel-session-dropped-file-grants session)))
              (mevedel-session--set-dropped-file-grants
               session
               (append
                original-grants
                (cl-set-difference
                 current
                 (append original-grants staged-grants)
                 :test #'equal)))))))
    (mevedel-session-artifacts-assert-new-mutation-authority session)
    (unless (eq (plist-get item :category) 'follow-up)
      (user-error "Pending input is already steering"))
    (when (plist-get entry :scope)
      (user-error "Directive follow-ups cannot be converted to steering"))
    ;; The follow-up drain owns both the delivery-time allowlist recheck
    ;; and the command dispatch a guest invocation needs; steering
    ;; delivery has neither.
    (when (or (plist-get entry :guest-invoke) (plist-get entry :guest-skills))
      (user-error "Guest invocations cannot be converted to steering"))
    (when mevedel-pending-inputs--converting-id
      (user-error "Pending-input conversion is still running"))
    (unless (mevedel-view--steerable-root-request-p request)
      (user-error "No steerable root turn is active"))
    (unless (buffer-live-p view)
      (user-error "No live owning view"))
    (setq mevedel-pending-inputs--converting-id id)
    (mevedel-session--set-dropped-file-grants
     session (append staged-grants original-grants))
    (with-current-buffer view
      (condition-case err
          (mevedel-view--submit-planned-input
           (mevedel-pending-inputs--copy-input entry)
           nil
           (lambda ()
             (funcall restore-grants)
             (when (buffer-live-p cockpit)
               (with-current-buffer cockpit
                 (setq mevedel-pending-inputs--converting-id nil)))
             (message "mevedel: follow-up remains unchanged"))
           (lambda (submission)
             (unwind-protect
                 (if-let* ((prepared
                            (mevedel-view--prepare-steering-entry
                             submission request)))
                     (condition-case conversion-err
                         (let ((replacement
                                (mevedel-pending-inputs--restore-entry-metadata
                                 prepared entry)))
                           (setq replacement
                                 (plist-put replacement :category 'steering)
                                 replacement
                                 (plist-put replacement :state 'pending))
                           (mevedel-pending-inputs--move-between
                            session 'follow-up 'steering id replacement)
                           (mevedel-pending-inputs--conversion-finished
                            cockpit context id))
                       (error
                        (mevedel-prompt-submission-restore submission)
                        (when (buffer-live-p cockpit)
                          (with-current-buffer cockpit
                            (setq mevedel-pending-inputs--converting-id nil)))
                        (message "mevedel: follow-up remains unchanged: %s"
                                 (error-message-string conversion-err))))
                   (when (buffer-live-p cockpit)
                     (with-current-buffer cockpit
                       (setq mevedel-pending-inputs--converting-id nil))))
               (funcall restore-grants)))
           nil
           ;; External input stays skill-inert through conversion: the
           ;; entry's own flag rides along instead of defaulting to live
           ;; skill planning.
           (plist-get entry :inert-skills))
        (error
         (funcall restore-grants)
         (setq mevedel-pending-inputs--converting-id nil)
         (signal (car err) (cdr err)))))))

(defun mevedel-pending-inputs-make-follow-up ()
  "Convert steering or requeue a failed follow-up at the follow-up tail."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (item (mevedel-pending-inputs--selected))
         (entry (plist-get item :entry))
         (id (plist-get item :id)))
    (unless (or (eq (plist-get item :category) 'steering)
                (eq (plist-get entry :state) 'failed-turn))
      (user-error "Pending input is already a follow-up"))
    (mevedel-session-artifacts-assert-new-mutation-authority session)
    (mevedel-pending-inputs--set-queues
     session 'steering (remq entry (mevedel-session-pending-steering session))
     'follow-up (append (remq entry (mevedel-session-pending-follow-ups session))
                        (list (mevedel-pending-inputs--as-follow-up session entry))))
    (mevedel-pending-inputs--refresh id)))

(defun mevedel-pending-inputs--as-follow-up (session entry)
  "Return pending ENTRY of SESSION as a fresh follow-up with the same id.
Restore its reserved prompt submission, which the follow-up turn prepares
again."
  (when-let* ((submission (plist-get entry :submission)))
    (mevedel-prompt-submission-restore submission))
  (append
   (list
    :id (plist-get entry :id)
    :category 'follow-up
    :input (mevedel-pending-inputs--copy-input entry)
    :dropped-file-grants
    (copy-sequence (plist-get entry :dropped-file-grants))
    :queued-at-time (float-time)
    :queued-at-goal-id
    (when-let* ((goal (mevedel-session-goal session)))
      (mevedel-goal-id goal))
    :queued-at-turn
    (or (mevedel-session-turn-count session) 0)
    :state 'pending)
   ;; Keep guest attribution, attachment ownership, directive scope,
   ;; and skill restrictions when retrying retained input.
   (cl-loop for key in mevedel-recovery--input-keys
            unless (memq key '(:id :category :input :dropped-file-grants
                               :queued-at-time :queued-at-goal-id
                               :queued-at-turn :state :request-id :blocked))
            when (plist-member entry key)
            append (list key (copy-tree (plist-get entry key))))))

(defun mevedel-pending-inputs-requeue-steering (session request-id)
  "Move SESSION's steering left by REQUEST-ID's settled turn to the follow-up head.
An external turn cannot park at a held boundary the way a gptel turn waits, so
input it could not take becomes the next turns, in submission order."
  (when-let* ((entries (cl-remove-if-not
                        (lambda (entry) (equal request-id (plist-get entry :request-id)))
                        (mevedel-session-pending-steering session))))
    (mevedel-pending-inputs--set-queues
     session 'steering (cl-remove-if (lambda (entry) (memq entry entries))
                                   (mevedel-session-pending-steering session))
     'follow-up (append (mapcar (lambda (entry) (mevedel-pending-inputs--as-follow-up session entry))
                                entries)
                        (mevedel-session-pending-follow-ups session)))
    (when-let* ((cockpit (get-buffer mevedel-pending-inputs-buffer-name))
                ((eq session (mevedel-cockpit-context-session
                              (buffer-local-value 'mevedel-cockpit--context cockpit)))))
      (with-current-buffer cockpit
        (mevedel-pending-inputs--refresh (tabulated-list-get-id))))
    entries))

(defun mevedel-pending-inputs-mark-delete ()
  "Mark the selected pending input for deletion."
  (interactive)
  (let ((id (plist-get (mevedel-pending-inputs--selected) :id)))
    (cl-pushnew id mevedel-pending-inputs--marked-ids :test #'equal)
    (tabulated-list-put-tag "D" t)))

(defun mevedel-pending-inputs-unmark ()
  "Remove the deletion mark from the selected pending input."
  (interactive)
  (let ((id (plist-get (mevedel-pending-inputs--selected) :id)))
    (setq mevedel-pending-inputs--marked-ids
          (delete id mevedel-pending-inputs--marked-ids))
    (tabulated-list-put-tag " " t)))

(defun mevedel-pending-inputs--discard (entries)
  "Release reserved prompt context owned by ENTRIES."
  (dolist (entry entries)
    (when-let* ((submission (plist-get entry :submission)))
      (mevedel-prompt-submission-restore submission))))

(defun mevedel-pending-inputs-execute-deletions ()
  "Confirm and delete every marked pending input."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (steering
          (cl-remove-if-not
           (lambda (entry)
             (member (plist-get entry :id)
                     mevedel-pending-inputs--marked-ids))
           (mevedel-session-pending-steering session)))
         (follow-ups
          (cl-remove-if-not
           (lambda (entry)
             (member (plist-get entry :id)
                     mevedel-pending-inputs--marked-ids))
           (mevedel-session-pending-follow-ups session))))
    (unless (or steering follow-ups)
      (user-error "No pending input is marked for deletion"))
    (when (yes-or-no-p
           (format "Delete %d steering and %d follow-up pending input%s? "
                   (length steering) (length follow-ups)
                   (if (= 1 (+ (length steering) (length follow-ups)))
                       ""
                     "s")))
      (mevedel-session-artifacts-assert-new-mutation-authority session)
      (dolist (entry (append steering follow-ups))
        (mevedel-pending-inputs--assert-removable session entry))
      (mevedel-pending-inputs--discard (append steering follow-ups))
      (mevedel-pending-inputs--set-queues
       session
       'steering (cl-remove-if
                  (lambda (entry) (memq entry steering))
                  (mevedel-session-pending-steering session))
       'follow-up (cl-remove-if
                   (lambda (entry) (memq entry follow-ups))
                   (mevedel-session-pending-follow-ups session)))
      (setq mevedel-pending-inputs--marked-ids nil)
      (mevedel-pending-inputs--refresh))))

(defun mevedel-pending-inputs-clear ()
  "Confirm and clear all pending input in the owning root session."
  (interactive)
  (let* ((context (mevedel-cockpit-current-context))
         (session (mevedel-pending-inputs--session context))
         (steering (mevedel-session-pending-steering session))
         (follow-ups (mevedel-session-pending-follow-ups session))
         (view (mevedel-cockpit-context-view-buffer context)))
    (unless (or steering follow-ups)
      (user-error "No pending input"))
    (when (and (buffer-live-p view)
               (buffer-local-value 'mevedel-view--pending-input-edit view))
      (user-error "Save or cancel the pending-input edit first"))
    (when (yes-or-no-p
           (format "Clear %d steering and %d follow-up pending input%s? "
                   (length steering) (length follow-ups)
                   (if (= 1 (+ (length steering) (length follow-ups)))
                       ""
                     "s")))
      (mevedel-session-artifacts-assert-new-mutation-authority session)
      (dolist (entry (append steering follow-ups))
        (mevedel-pending-inputs--assert-removable session entry))
      (mevedel-pending-inputs--discard (append steering follow-ups))
      (mevedel-pending-inputs--set-queues
       session 'steering nil 'follow-up nil)
      (when (buffer-live-p view)
        (with-current-buffer view
          (mevedel-view--interaction-rebuild)))
      (when (derived-mode-p 'mevedel-pending-inputs-mode)
        (setq mevedel-pending-inputs--marked-ids nil)
        (mevedel-cockpit-surface-refresh))
      (message "mevedel: cleared pending input"))))

(defun mevedel-pending-inputs-resume-after-failure ()
  "Clear failure pause after all failed-turn input is resolved."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (failed
          (cl-remove-if-not
           (lambda (entry)
             (eq (plist-get entry :state) 'failed-turn))
           (append (mevedel-session-pending-steering session)
                   (mevedel-session-pending-follow-ups session)))))
    (unless (mevedel-session-pending-input-failure-paused session)
      (user-error "Pending input is not paused after a turn failure"))
    (when failed
      (user-error "Resolve %d failed-turn message%s first"
                  (length failed) (if (= 1 (length failed)) "" "s")))
    (mevedel-session-artifacts-assert-new-mutation-authority session)
    (mevedel-session-set-pending-input-failure-paused session nil)
    (mevedel-recovery-save session)
    (mevedel-pending-inputs--refresh)
    (message "mevedel: pending-input failure pause cleared")))

(defun mevedel-pending-inputs-quit ()
  "Close the cockpit and resume eligible automatic delivery."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (view (mevedel-cockpit-context-view-buffer context)))
    (when (and (buffer-live-p view)
               (buffer-local-value 'mevedel-view--pending-input-edit view))
      (user-error "Save or cancel the pending-input edit first"))
    (mevedel-pending-inputs--resume-delivery context)
    (mevedel-cockpit-quit "Pending Inputs cockpit")))

(defun mevedel-pending-inputs--resume-delivery (context)
  "Resume eligible automatic delivery for CONTEXT's session.

Clearing the pause is not enough on its own: a steering entry that
matched the live request parked its turn at the pending-input boundary,
and only a transition back to WAIT releases it."
  (let* ((session (mevedel-pending-inputs--session context))
         (view (mevedel-cockpit-context-view-buffer context))
         (failure-paused
          (mevedel-session-pending-input-failure-paused session)))
    (mevedel-session-set-pending-input-paused session nil)
    (mevedel-recovery-save session)
    (when (buffer-live-p view)
      (with-current-buffer view
        (mevedel-view--interaction-rebuild)))
    (unless failure-paused
      (when-let* ((data-buffer
                   (and (buffer-live-p view)
                        (buffer-local-value 'mevedel--data-buffer view)))
                  ((buffer-live-p data-buffer))
                  (request
                   (buffer-local-value
                    'mevedel--current-request data-buffer))
                  (fsm (and request (mevedel-request-fsm request)))
                  ((plist-get (gptel-fsm-info fsm)
                              :mevedel-pending-input-hold)))
        (gptel--fsm-transition fsm 'WAIT)))
    ;; A failure latch that holds no queued input no longer holds background
    ;; mail, so offer it; latched input waits for explicit recovery.
    (when (and (buffer-live-p view)
               (or (not failure-paused)
                   (not (or (mevedel-session-pending-steering session)
                            (mevedel-session-pending-follow-ups session)))))
      (with-current-buffer view
        (mevedel-pending-inputs-schedule-held-input-delivery)))))

(defun mevedel-pending-inputs--on-kill-buffer ()
  "Resume delivery when the cockpit is killed while it still owns a pause.

The pause is stored on the session but owned by this buffer's lifetime.
A kill cannot refuse the way `mevedel-pending-inputs-quit' does, so the
entry edit that would have blocked the quit is cancelled instead: its
text would otherwise sit in the composer, where the drain refuses to
deliver anything, and the resume would be dead on arrival.  Errors are
demoted because a signal here would abort the kill and leave a live
cockpit whose banner and session disagree."
  (with-demoted-errors "mevedel: pending-input teardown failed: %S"
    (when-let* ((context (mevedel-cockpit-context-for-buffer (current-buffer)))
                (session (mevedel-cockpit-context-session context))
                ((mevedel-session-pending-input-paused session)))
      (when-let* ((view (mevedel-cockpit-context-view-buffer context))
                  ((buffer-live-p view))
                  (edit (buffer-local-value 'mevedel-view--pending-input-edit
                                            view)))
        ;; This cockpit is on its way out, so the restored draft returns
        ;; to the view rather than to a buffer about to die.
        (mevedel-pending-inputs--return-to-cockpit
         (plist-put (copy-sequence edit) :cockpit-buffer nil)))
      (mevedel-pending-inputs--resume-delivery context))))

(defconst mevedel-pending-inputs--surface
  `(:buffer-name ,mevedel-pending-inputs-buffer-name
    :label "Pending Inputs cockpit"
    :row-label "pending input"
    :mode mevedel-pending-inputs-mode
    :format [("Category" 12 nil)
             ("#" 4 nil)
             ("State" 12 nil)
             ("Preview" 0 nil)]
    :require-session t
    :collect mevedel-pending-inputs--collect
    :entry mevedel-pending-inputs--entry
    :header mevedel-pending-inputs--header
    :details mevedel-pending-inputs--details
    :details-buffer "*mevedel pending input*"
    :setup mevedel-pending-inputs--setup
    :keys (("RET" "Edit selected pending input" mevedel-pending-inputs-edit)
           ("e" "Edit selected pending input" mevedel-pending-inputs-edit)
           ("M-<up>" "Move up within category" mevedel-pending-inputs-move-up)
           ("M-<down>" "Move down within category"
            mevedel-pending-inputs-move-down)
           ("s" "Convert to steering" mevedel-pending-inputs-make-steering)
           ("f" "Convert or requeue as follow-up" mevedel-pending-inputs-make-follow-up)
           ("d" "Mark for deletion" mevedel-pending-inputs-mark-delete)
           ("u" "Unmark deletion" mevedel-pending-inputs-unmark)
           ("x" "Delete marked pending input"
            mevedel-pending-inputs-execute-deletions)
           ("R" "Resume after failure" mevedel-pending-inputs-resume-after-failure)
           ("C-c C-q" "Clear all pending input" mevedel-pending-inputs-clear)
           ("g" "Refresh live pending input" mevedel-pending-inputs-refresh)
           ("q" "Close and resume eligible delivery" mevedel-pending-inputs-quit)))
  "Cockpit surface spec for pending input.")

(define-derived-mode mevedel-pending-inputs-mode tabulated-list-mode
  "mevedel-pending-inputs"
  "Major mode for inspecting and editing pending input."
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-pending-inputs--surface)
  (add-hook 'kill-buffer-hook #'mevedel-pending-inputs--on-kill-buffer nil t))

(defun mevedel-pending-inputs-open (&optional context-or-event)
  "Open the Pending Inputs cockpit for CONTEXT-OR-EVENT."
  (interactive
   (list (and (mouse-event-p last-nonmenu-event)
              last-nonmenu-event)))
  (when (mouse-event-p context-or-event)
    (mouse-set-point context-or-event)
    (setq context-or-event nil))
  (let* ((context (or context-or-event
                      (mevedel-cockpit-current-context)))
         (session (mevedel-pending-inputs--session context)))
    (unless (or (mevedel-session-pending-steering session)
                (mevedel-session-pending-follow-ups session)
                (mevedel-session-pending-input-failure-paused session))
      (user-error "No pending input"))
    (mevedel-cockpit-open-surface
     mevedel-pending-inputs--surface context)))

(provide 'mevedel-pending-inputs)
;;; mevedel-pending-inputs.el ends here
