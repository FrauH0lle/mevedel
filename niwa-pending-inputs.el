;;; niwa-pending-inputs.el --- Pending Inputs cockpit -*- lexical-binding: t -*-

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

(require 'niwa-telemetry)
(require 'niwa-utilities)

(require 'niwa-recovery)

(require 'niwa-engine)

(require 'niwa-report)

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

;; The follow-up drain shares the session durability transaction used by
;; composer dispatch, so the macro must expand for interpreted loads too.
(require 'niwa-cockpit)
(require 'niwa-prompt-submission)
(require 'niwa-session-durability)

;; `gptel-request'
(declare-function gptel--fsm-transition "ext:gptel-request"
                  (machine &optional new-state))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)

;; `niwa-agent-control'
(declare-function niwa-agent-control-context-mailbox
                  "niwa-agent-control" (context))
(declare-function niwa-agent-control-root-waiting-p
                  "niwa-agent-control" (session))
(declare-function niwa-agent-control-wake-root-user
                  "niwa-agent-control" (session))
(autoload 'niwa-agent-control-root-waiting-p "niwa-agent-control")
(autoload 'niwa-agent-control-wake-root-user "niwa-agent-control")

;; `niwa-cockpit'
(declare-function niwa-cockpit-context-data-buffer
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-for-buffer
                  "niwa-cockpit" (buffer))
(declare-function niwa-cockpit-context-session
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-view-buffer
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-current-context "niwa-cockpit" ())
(declare-function niwa-cockpit-open-surface
                  "niwa-cockpit" (surface &optional context))
(declare-function niwa-cockpit-quit "niwa-cockpit" (&optional label))
(declare-function niwa-cockpit-setup-tabulated-surface
                  "niwa-cockpit" (surface))
(defvar niwa-cockpit--context)
(declare-function niwa-cockpit-surface-context
                  "niwa-cockpit" (&optional surface))
(declare-function niwa-cockpit-surface-refresh
                  "niwa-cockpit" (&optional selected-id))
(declare-function niwa-cockpit-surface-selected
                  "niwa-cockpit" (&optional no-error))

;; `niwa-collaboration'
(defvar niwa-collaboration-guest-skills)

;; `niwa-collaboration'
(defvar niwa-collaboration-needs-host-message)
(declare-function niwa-collaboration-notify-guest
                  "niwa-collaboration" (session guest-id message))

;; `niwa-collaboration-guest'
(declare-function niwa-collaboration--guest-skills-admitted-p
                  "niwa-collaboration-guest" (names role session))

;; `niwa-compact-run'
(defvar niwa-compact-run-in-flight)

;; `niwa-schedule'
(declare-function niwa-schedule-wake-request-context "niwa-schedule" (session))
(autoload 'niwa-schedule-settle-wake "niwa-schedule")
(autoload 'niwa-schedule-wake-request-context "niwa-schedule")

;; `niwa-skills-invoke'
(defvar niwa-skills--pending-request-context)

;; `niwa-goal'
(declare-function niwa-goal-schedule-continuation
                  "niwa-goal"
                  (&optional session buffer prompt-submission delay))
(autoload 'niwa-goal-schedule-continuation "niwa-goal")

;; `niwa-gptel-retry'
(declare-function niwa-gptel-retry-refresh-availability "niwa-gptel-retry" (buffer))
(autoload 'niwa-gptel-retry-refresh-availability "niwa-gptel-retry")

;; `niwa-mention-bindings'
(declare-function niwa-mention-bindings-copy-text
                  "niwa-mention-bindings" (text))
(autoload 'niwa-mention-bindings-copy-text "niwa-mention-bindings")

;; `niwa-mentions'
(declare-function niwa-mentions-file-token "niwa-mentions" (path))
(autoload 'niwa-mentions-file-token "niwa-mentions")

;; `niwa-permission-queue'
(declare-function niwa-permission-absence-clear
                  "niwa-permission-queue" (session))
(declare-function niwa-permission-absence-held-p
                  "niwa-permission-queue" (session))
(autoload 'niwa-permission-absence-clear "niwa-permission-queue")
(autoload 'niwa-permission-absence-held-p "niwa-permission-queue")

;; `niwa-prompt-submission'
(declare-function niwa-prompt-submission-display-text
                  "niwa-prompt-submission" (cl-x) t)
(declare-function niwa-prompt-submission-outcome
                  "niwa-prompt-submission" (cl-x) t)
(declare-function niwa-prompt-submission-reserve
                  "niwa-prompt-submission" (submission))
(declare-function niwa-prompt-submission-restore
                  "niwa-prompt-submission" (submission))

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-assert-new-mutation-authority
                  "niwa-session-artifacts" (session))
(autoload 'niwa-session-artifacts-assert-new-mutation-authority
  "niwa-session-artifacts")

;; `niwa-session-persistence'
(defvar niwa-session--read-only-mode)

;; `niwa-skills-ui'
(declare-function niwa-skills-parse-slash-line
                  "niwa-skills-ui" (text))

;; `niwa-structs'
(declare-function niwa-goal-id "niwa-structs" (cl-x) t)
(declare-function niwa-goal-status "niwa-structs" (cl-x) t)
(declare-function niwa-request-fsm "niwa-structs" (cl-x) t)
(declare-function niwa-request-id "niwa-structs" (cl-x) t)
(declare-function niwa-session--set-active-dropped-file-grants
                  "niwa-structs" (session paths))
(declare-function niwa-session--set-dropped-file-grants
                  "niwa-structs" (session paths))
(declare-function niwa-session-active-dropped-file-grants
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-add-dropped-file-grant
                  "niwa-structs" (session path))
(declare-function niwa-session-dropped-file-grants
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-enqueue-pending-input
                  "niwa-structs" (session category entry))
(declare-function niwa-session-goal "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-follow-ups
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-input-delivery-paused-p
                  "niwa-structs" (session))
(declare-function niwa-session-pending-input-failure-paused
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-input-paused
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-inputs
                  "niwa-structs" (session category))
(declare-function niwa-session-pending-plan-approval
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-steering
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-plan-metadata
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-root-buffer "niwa-structs" (cl-x) t)
(declare-function niwa-session-set-pending-input-failure-paused
                  "niwa-structs" (session paused))
(declare-function niwa-session-set-pending-input-paused
                  "niwa-structs" (session paused))
(declare-function niwa-session-set-pending-inputs
                  "niwa-structs" (session category entries))
(declare-function niwa-session-turn-count "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-telemetry'
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))

;; `niwa-transport'
(declare-function niwa-transport-run-when-idle
                  "niwa-transport" (key path thunk))
(autoload 'niwa-transport-run-when-idle "niwa-transport")

;; `niwa-turn'
(declare-function niwa-turn-busy-p "niwa-turn" (&optional buffer))
(autoload 'niwa-turn-busy-p "niwa-turn")

;; `niwa-utilities'
(declare-function niwa--normalize-message-text
                  "niwa-utilities" (text))
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))
(autoload 'niwa--normalize-message-text "niwa-utilities")

;; `niwa-view'
(defvar niwa-view--side-conversation-p)

;; `niwa-view-composer'
(defvar niwa-composer--view)
(declare-function niwa-view-composer-display "niwa-view-pair" (view-window))
(declare-function niwa-view--assert-live-tip
                  "niwa-view-composer" (&optional allow-armed-fork))
(declare-function niwa-view--bind-input-mentions
                  "niwa-view-composer" (session))
(declare-function niwa-view--clear-input "niwa-view-composer" ())
(declare-function niwa-view--clear-submitted-input "niwa-view-composer"
                  (submitted-draft))
(declare-function niwa-view-composer-owner "niwa-view-composer" ())
(declare-function niwa-view--composer-snapshot
                  "niwa-view-composer" (session))

(declare-function niwa-view--dispatch-prepared-outcome
                  "niwa-view-composer" (submission data-buffer &rest keys))
(declare-function niwa-view--ensure-interactive-chat-view
                  "niwa-view-composer" ())
(declare-function niwa-view--input-text "niwa-view-composer" ())
(declare-function niwa-view--occupied-root-workflow
                  "niwa-view-composer" (session))

(declare-function niwa-view--reserved-goal-handoff-id
                  "niwa-view-composer" (&optional session))
(declare-function niwa-view--restore-composer-snapshot
                  "niwa-view-composer" (snapshot session &optional force))
(declare-function niwa-view--session "niwa-view-composer" ())
(declare-function niwa-view--submit-planned-input
                  "niwa-view-composer"
                  (input &optional before-send on-block dispatch after-insert
                         inert-skills selected-skills))

(declare-function niwa-view-composer-buffer "niwa-view-composer" (&optional view))
(declare-function niwa-view-composer-draft-end "niwa-view-composer" (&optional view))
(declare-function niwa-view-composer-draft-start "niwa-view-composer" (&optional view))
(declare-function niwa-view-composer-draft-text "niwa-view-composer" (&optional view without-properties))
(declare-function niwa-view-refresh-input-prompt
                  "niwa-view-composer" ())
(declare-function niwa-view-run-invocation
                  "niwa-view-composer" (name args &rest keys))
(declare-function niwa-view-send "niwa-view-composer" ())


(defvar niwa-view--pending-skill-submission)
(defvar niwa-view--prompt-hook-pending)

;; `niwa-view-history'
(declare-function niwa-view-history-add "niwa-view-history" (input))

;; `niwa-view-input-files'
(declare-function niwa-view--activate-dropped-file-grants
                  "niwa-view-input-files" (paths session))
(declare-function niwa-view--pop-dropped-file-grants-for-input
                  "niwa-view-input-files" (input session))

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-rebuild
                  "niwa-view-interaction" ())
(declare-function niwa-view--interaction-register
                  "niwa-view-interaction" (descriptor))
(defvar niwa-view--interaction-overlays)

;; `seq'
(declare-function seq-take "seq" (sequence n))

;; `tabulated-list'
(declare-function tabulated-list-get-id "tabulated-list" ())
(declare-function tabulated-list-mode "tabulated-list" ())
(declare-function tabulated-list-put-tag
                  "tabulated-list" (tag &optional advance))

(defconst niwa-pending-inputs-buffer-name "*niwa pending inputs*"
  "Name of the Pending Inputs cockpit buffer.")

(defvar-local niwa-view--pending-input-edit nil
  "Queue edit state for this view's composer, or nil.")

(defvar-local niwa-view--pending-resume-entry nil
  "Steering entry owned by this composer's unfinished Resume preparation.")

(defun niwa-pending-inputs-cancel-resume ()
  "Release this composer's unfinished Resume preparation, if any."
  (when niwa-view--pending-resume-entry
    (plist-put niwa-view--pending-resume-entry :delivering nil)
    (setq niwa-view--pending-resume-entry nil)))

(defvar-local niwa-view--pending-guest-attribution nil
  "Guest attribution audit record for the follow-up being submitted, or nil.

Set by the follow-up drain when it dispatches a collaboration guest's
entry and consumed exactly once where the prompt and its hook audits are
inserted, so attribution lands inside the user turn before the response
marker exists regardless of asynchronous submit hooks.  Blocked or
failed submissions clear it so the next host prompt is never
mis-attributed.")

(defvar-local niwa-view--turn-guest-attribution nil
  "Guest attribution of the turn this data buffer is running, or nil.
Set where a prompt is inserted, from its guest attribution or nil, so it
names the running turn; read and cleared when that turn settles.")

(defvar-local niwa-pending-inputs--marked-ids nil
  "Pending-input identities marked for deletion.")

(defvar-local niwa-pending-inputs--converting-id nil
  "Pending-input identity undergoing asynchronous conversion.")


;;
;;; Queue state and summary

(defun niwa-view--pending-follow-ups (&optional session)
  "Return SESSION's pending follow-ups."
  (when-let* ((sess (or session (niwa-view--session))))
    (niwa-session-pending-follow-ups sess)))

(defun niwa-pending-inputs-follow-up-changed (session)
  "Persist SESSION's changed follow-up queue soon and notify observers."
  (niwa-recovery-save session)
  (niwa-gptel-retry-refresh-availability (niwa-session-root-buffer session))
  (when (fboundp 'niwa-collaboration-notify-queue-changed)
    (niwa-collaboration-notify-queue-changed session)))

(defun niwa-pending-inputs--set-queues (session &rest replacements)
  "Replace SESSION queue CATEGORY ENTRIES pairs.
Persist and notify guests once, and only when a queue changed."
  (let (changed follow-up-changed)
    (while replacements
      (let ((category (pop replacements))
            (entries (pop replacements)))
        (unless (eq entries (niwa-session-pending-inputs session category))
          (setq changed t)
          (when (eq category 'follow-up)
            (setq follow-up-changed t))
          (niwa-session-set-pending-inputs session category entries))))
    (cond
     (follow-up-changed (niwa-pending-inputs-follow-up-changed session))
     (changed
      (niwa-recovery-save session)
      (niwa-gptel-retry-refresh-availability (niwa-session-root-buffer session))))))

(defun niwa-view--next-follow-up (session)
  "Return SESSION's first queued follow-up."
  (car (niwa-session-pending-follow-ups session)))


(defun niwa-view--follow-up-auto-drain-blocked-p
    (&optional session ignore-failure-latch)
  "Return non-nil when SESSION follow-ups should wait for user action.
With IGNORE-FAILURE-LATCH, the failure-recovery latch alone does not hold."
  (when-let* ((sess (or session (niwa-view--session))))
    (or (niwa-recovery-blocker sess)
        (niwa-session-pending-steering sess)
        (niwa-permission-absence-held-p sess)
        (cl-find 'failed-turn (niwa-session-pending-follow-ups sess)
                 :key (lambda (entry) (plist-get entry :state)))
        (if ignore-failure-latch
            (niwa-session-pending-input-paused sess)
          (niwa-session-pending-input-delivery-paused-p sess))
        (niwa-session-pending-plan-approval sess)
        (plist-get (niwa-session-plan-metadata sess)
                   :implementation-retry)
        (niwa-view--reserved-goal-handoff-id sess)
        (when-let* ((goal (niwa-session-goal sess))
                    ((memq (niwa-goal-status goal)
                           '(paused blocked budget-limited)))
                    (entry (car (niwa-session-pending-follow-ups sess))))
          (equal (niwa-goal-id goal)
                 (plist-get entry :queued-at-goal-id))))))

(defun niwa-view--pending-input-preview (input)
  "Return a one-line, 96-column preview from a bounded prefix of INPUT."
  (let* ((prefix (substring-no-properties input 0 (min (length input) 1024)))
         (preview (string-trim
                   (replace-regexp-in-string
                    "[ \t\n\r]+" " "
                    (niwa--normalize-message-text prefix) t t))))
    (if (or (> (length input) (length prefix)) (> (string-width preview) 96))
        (concat (truncate-string-to-width preview 93) "...")
      preview)))

(defun niwa-view--pending-input-text (entry)
  "Return normalized input text for pending ENTRY."
  (niwa--normalize-message-text (or (plist-get entry :input) "")))

(defvar-keymap niwa-view--pending-inputs-map
  :doc "Keymap on the pending-input summary."
  "RET" #'niwa-pending-inputs-preview-activate
  "d" #'niwa-pending-inputs-drop-at-point
  "DEL" #'niwa-pending-inputs-drop-at-point
  "<delete>" #'niwa-pending-inputs-drop-at-point
  "<return>" #'niwa-pending-inputs-preview-activate
  "<mouse-1>" #'niwa-pending-inputs-preview-activate
  "<mouse-2>" #'niwa-pending-inputs-preview-activate)

(defun niwa-pending-inputs--preview-selection (session entries)
  "Return SESSION's expanded visible entry identity among ENTRIES, or nil.
The disposable interaction overlay owns this presentation state.  Removing
the summary removes its state; rebuilding an unchanged summary retains it."
  (when-let* ((overlay (and (hash-table-p niwa-view--interaction-overlays)
                          (gethash 'pending-inputs
                                   niwa-view--interaction-overlays)))
              ((overlay-buffer overlay)))
    (let ((selection (overlay-get overlay 'niwa-pending-input-preview)))
      (if (and (eq (car selection) session)
               (cdr selection)
               (cl-find (cdr selection) entries
                        :key (lambda (entry) (plist-get entry :id))))
          (cdr selection)
        (overlay-put overlay 'niwa-pending-input-preview nil)
        nil))))

(defun niwa-pending-inputs-preview-activate (&optional event)
  "Inspect the pending input at point or mouse EVENT without holding delivery.
Only the explicitly labeled management action opens the queue cockpit."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (when-let* ((target (get-text-property (point) 'niwa-pending-input-target))
              (session (niwa-view--session))
              ((eq (car target) session))
              (overlay (get-text-property
                        (point) 'niwa-view-interaction-overlay))
              ((overlayp overlay))
              ((eq (overlay-buffer overlay) (current-buffer)))
              ((<= (overlay-start overlay) (point)))
              ((< (point) (overlay-end overlay))))
    (if (eq (cdr target) 'manage)
        (niwa-pending-inputs-open)
      (when-let* ((id (cdr target))
                  ((cl-find id
                            (append (niwa-session-pending-steering session)
                                    (niwa-session-pending-follow-ups session))
                            :key (lambda (entry) (plist-get entry :id)))))
        (overlay-put overlay 'niwa-pending-input-preview
                     (unless (equal target (overlay-get
                                            overlay 'niwa-pending-input-preview))
                       target))
        (niwa-view--pending-inputs-render session)))))

(defun niwa-pending-inputs--preview-detail (entry)
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
                "[\t\r]" " " (niwa--normalize-message-text prefix) t t))
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

(defun niwa-view--pending-input-category-body (label entries &optional session selected)
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
             (preview (niwa-view--pending-input-preview input))
             ;; Compare within the preview's own bounded prefix: a pasted
             ;; megabyte must not be normalized on every render.
             (prefix (substring-no-properties input 0 (min (length input) 1024)))
             (partial (or (> (length input) (length prefix))
                          (not (equal preview
                                      (string-trim
                                       (niwa--normalize-message-text prefix))))))
             (expanded (and partial id (equal id selected)))
             (text (concat
                    (format "%d. %s" index
                            (cond (expanded "[-]") (partial "[+]") (t "   ")))
                    (unless expanded (concat " " preview)))))
        (when (and session id)
          (add-text-properties
           0 (length text)
           (list 'niwa-pending-input-row (cons session id)) text))
        (when (and session id partial)
          (add-text-properties
           0 (length text)
           (list 'niwa-pending-input-target (cons session id)
                 'mouse-face 'highlight
                 'help-echo "Expand or collapse input preview; delivery continues")
           text))
        (push (concat "  " text
                      (when expanded
                        (concat "\n" (string-remove-suffix
                                      "\n" (niwa-pending-inputs--preview-detail entry)))))
              lines)))
    (when (> (length entries) 3)
      (push (format "  %d more" (- (length entries) 3)) lines))
    (concat "\n" label "\n"
            (string-join (nreverse lines) "\n")
            "\n")))

(defun niwa-view--pending-inputs-body (session)
  "Return the main-view pending-input summary for SESSION."
  (let ((selected
         (niwa-pending-inputs--preview-selection
          session (append (seq-take (niwa-session-pending-steering session) 3)
                          (seq-take (niwa-session-pending-follow-ups session) 3)))))
    (concat
     "\nC-c C-w retract newest"
     (when (and (buffer-live-p niwa--data-buffer)
                (not (niwa-turn-busy-p niwa--data-buffer)))
       " · C-c C-r resume")
     " · d/DEL drop row\n"
     (when (niwa-session-pending-input-failure-paused session)
       "\nPending-input delivery stopped after turn failure; review required\n")
     (when (niwa-session-pending-input-paused session)
       (if (and (not (niwa-turn-busy-p niwa--data-buffer))
                (not (niwa-pending-inputs-cockpit-holds-p session)))
           "\nPending input waiting for Resume (C-c C-r)\n"
         "\nPending-input delivery paused\n"))
     (when-let* ((entries (niwa-session-pending-steering session)))
       (niwa-view--pending-input-category-body "Steering" entries session selected))
     (when-let* ((entries (niwa-session-pending-follow-ups session)))
       (niwa-view--pending-input-category-body "Follow-ups" entries session selected))
     "\n"
     (propertize "Manage queue…" 'niwa-pending-input-target (cons session 'manage)
                 'mouse-face 'highlight 'face 'link)
     " (pauses delivery; C-c C-e)\n")))

(defun niwa-view--pending-inputs-render (&optional session)
  "Render SESSION pending input into the interaction zone."
  (when-let* ((session (or session (niwa-view--session))))
    (let ((entries
           (append (niwa-session-pending-steering session)
                   (niwa-session-pending-follow-ups session))))
      (when entries
        (niwa-view--interaction-register
         (list :kind 'pending-input
               :id 'pending-inputs
               :count (length entries)
               :body (lambda () (niwa-view--pending-inputs-body session))
               :keymap niwa-view--pending-inputs-map))))))

(cl-defun niwa-view-enqueue-external-follow-up
    (data-buffer text &key guest-name guest-id paths invoke
                 guest-role shared-question skills)
  "Queue TEXT as a follow-up that originated outside this Emacs.

DATA-BUFFER owns the session.  GUEST-NAME attributes the entry to a
collaboration guest; GUEST-ID is that guest's stable identity, which
the retract seam checks so only the sender can take the entry back.
PATHS are files to mention as @file tokens with read grants, like an
Emacs-side drop; the entry remembers them so a retraction can delete
them.

Skill tokens in TEXT stay literal at submission: external free text
carries prompting authority only, never skill invocation.  The one
exception is INVOKE, a command or skill name the caller has already
validated against its own allowlist for the guest's link tier
GUEST-ROLE: TEXT is then that invocation's arguments, dispatched
through `niwa-view-run-invocation' at delivery and rechecked
against the same allowlist and tier first.  Return the queued entry,
or nil without a live session view.  SHARED-QUESTION is host-validated
shared-item attribution retained with the delivered prompt.  SKILLS names
explicitly selected skills applied together, with the same admission recheck."
  (when-let* (((buffer-live-p data-buffer))
              (view-buffer (buffer-local-value 'niwa--view-buffer
                                               data-buffer))
              ((buffer-live-p view-buffer))
              (session (buffer-local-value 'niwa--session data-buffer)))
    (with-current-buffer view-buffer
      (let ((input (niwa--normalize-message-text text)))
        (when paths
          (setq input (concat input " "
                              (mapconcat #'niwa-mentions-file-token
                                         paths " ")))
          (dolist (path paths)
            (niwa-session-add-dropped-file-grant session path)))
        (let ((entry (niwa-session-enqueue-pending-input
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
                            :dropped-file-grants
                            (niwa-view--pop-dropped-file-grants-for-input
                             input session)
                            :queued-at-time (float-time)
                            :queued-at-turn
                            (or (niwa-session-turn-count session) 0)))))
          ;; A guest's message is a human instruction: it ends the wait.
          (niwa-permission-absence-clear session)
          (niwa-pending-inputs-follow-up-changed session)
          (niwa-view--interaction-rebuild)
          (niwa-pending-inputs-schedule-held-input-delivery)
          entry)))))

(defun niwa-view--queue-follow-up (input)
  "Queue INPUT to start a separate root turn."
  (setq input (niwa--normalize-message-text input))
  (let ((session (niwa-view--session)))
    (unless session
      (user-error "No active session for follow-up"))
    (niwa-permission-absence-clear session)
    (let* ((dropped-file-grants
            (niwa-view--pop-dropped-file-grants-for-input input session))
           (entry
            (niwa-session-enqueue-pending-input
             session 'follow-up
             (list :input input
                   :dropped-file-grants dropped-file-grants
                   :queued-at-time (float-time)
                   :queued-at-goal-id
                   (or (and (niwa-session-goal session)
                            (niwa-goal-id
                             (niwa-session-goal session)))
                       (niwa-view--reserved-goal-handoff-id session))
                   :queued-at-turn
                   (or (niwa-session-turn-count session) 0)))))
      (niwa-telemetry-record
       session 'user-message-queued
       :message-hash (secure-hash 'sha256 input)
       :message-chars (length input)
       :queue-depth (length (niwa-view--pending-follow-ups session))
       :enqueue-goal-id (plist-get entry :queued-at-goal-id))
      (niwa-view-history-add input)
      (when (equal-including-properties (niwa-view--input-text) input)
        (niwa-view--clear-input))
      (niwa-pending-inputs-follow-up-changed session)
      (niwa-view--interaction-rebuild)
      (message "niwa: queued follow-up for a separate turn")
      (niwa-pending-inputs-schedule-held-input-delivery)
      entry)))

(defun niwa-view--steering-request-context-supported-p (context)
  "Return non-nil when prepared skill CONTEXT can steer an active request."
  (cl-loop for (key value) on context by #'cddr
           always
           (pcase key
             (:invoked-skills t)
             (:ptc-primitives (eq value :unrestricted))
             ((or :permission-rules :hook-rules :model :effort)
              (null value))
             (_ nil))))

(defun niwa-view--steerable-root-request-p (request)
  "Return non-nil when REQUEST is a root turn that can still take steering."
  (and request (niwa-engine-steerable-p (niwa-engine-owner request))))

(defun niwa-view--prepare-steering-entry (submission request)
  "Return a validated steering entry for SUBMISSION and REQUEST.
Return nil and leave the submission pending when the live request contract no
longer accepts the prepared input."
  (let* ((session (niwa-view--session))
         (outcome (niwa-prompt-submission-outcome submission))
         (request-context (plist-get outcome :request-context))
         (model-input (plist-get outcome :model-input))
         (current-request
          (and (buffer-live-p niwa--data-buffer)
               (buffer-local-value 'niwa--current-request
                                   niwa--data-buffer))))
    (cond
     ((plist-get outcome :fork-outcome)
      (niwa-diagnostic-message (niwa-diagnostic-scope session)
       "Fork skills cannot steer; use C-c TAB")
      nil)
     ((not (niwa-view--steering-request-context-supported-p
            request-context))
      (niwa-diagnostic-message (niwa-diagnostic-scope session)
       "Skill policy cannot steer; use C-c TAB")
      nil)
     ((or (not (eq request current-request))
          (not (niwa-view--steerable-root-request-p request)))
      (niwa-diagnostic-message (niwa-diagnostic-scope session)
       "Request can no longer be steered; use C-c TAB")
      nil)
     (t
      (let* ((input
              (niwa-prompt-submission-display-text submission))
             (dropped-file-grants
              (niwa-view--pop-dropped-file-grants-for-input
               input session)))
        (niwa-prompt-submission-reserve submission)
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
         :request-id (niwa-request-id request)
         :queued-at-time (float-time)
         :queued-at-turn
         (or (niwa-session-turn-count session) 0)))))))

(defun niwa-view--queue-prepared-steering
    (submission request &optional submitted-draft)
  "Queue accepted prompt SUBMISSION as steering for REQUEST.
Clear SUBMITTED-DRAFT only if the composer still holds that exact text."
  (when-let* ((prepared
               (niwa-view--prepare-steering-entry submission request))
              (session (niwa-view--session))
              (entry
               (niwa-session-enqueue-pending-input
                session 'steering prepared)))
    (niwa-recovery-save session)
    ;; Queued steering is a user message: it ends a wait after expiry.
    (niwa-permission-absence-clear session)
    (let ((input (plist-get entry :input)))
      (niwa-view-history-add input)
      (when submitted-draft
        (niwa-view--clear-submitted-input submitted-draft))
      (niwa-view--interaction-rebuild)
      (when (niwa-agent-control-root-waiting-p session)
        (niwa-agent-control-wake-root-user session))
      (message "niwa: queued steering for this turn")
      entry)))

(defun niwa-pending-inputs--assert-removable (session entry)
  "Refuse removal of SESSION's ENTRY when delivery or editing owns it."
  (unless (memq entry (append (niwa-session-pending-steering session)
                             (niwa-session-pending-follow-ups session)))
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Pending input is no longer queued"))
  (when (or (eq (plist-get entry :state) 'dispatching)
            (plist-get entry :delivering))
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Pending input is being dispatched; it can no longer be retracted or deleted"))
  (when-let* ((data (niwa-session-root-buffer session))
              ((buffer-live-p data))
              (view (buffer-local-value 'niwa--view-buffer data))
              ((buffer-live-p view))
              (edit (buffer-local-value 'niwa-view--pending-input-edit view))
              ((equal (plist-get edit :id) (plist-get entry :id))))
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Save or cancel the pending-input edit first"))
  (when-let* ((cockpit (get-buffer niwa-pending-inputs-buffer-name))
              ((niwa-pending-inputs-cockpit-holds-p session))
              ((equal (buffer-local-value 'niwa-pending-inputs--converting-id cockpit)
                      (plist-get entry :id))))
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Pending input conversion is still running")))

(defun niwa-pending-inputs--remove (session entry &optional validate)
  "Remove SESSION's ENTRY after checking authority and delivery ownership.
Call VALIDATE, when non-nil, immediately before removal, after yielding checks."
  (niwa-session-artifacts-assert-new-mutation-authority session)
  ;; Authority checks can yield to a transport.  Recheck the live entry last.
  (niwa-pending-inputs--assert-removable session entry)
  (when validate (funcall validate))
  (let ((category (plist-get entry :category)))
    (niwa-pending-inputs--set-queues
     session category (remq entry (niwa-session-pending-inputs session category))))
  (niwa-pending-inputs--discard (list entry)))

;;;###autoload
(defun niwa-pending-inputs-drop-at-point ()
  "Delete the pending entry on this summary row without pausing delivery."
  (interactive)
  (require 'niwa-view-composer)
  (niwa-view--ensure-interactive-chat-view)
  (let* ((row (text-property-not-all (line-beginning-position) (line-end-position)
                                     'niwa-pending-input-row nil))
         (target (and row (get-text-property row 'niwa-pending-input-row)))
         (session (niwa-view--session))
         (entry (and (eq (car-safe target) session)
                     (cl-find (cdr target)
                              (append (niwa-session-pending-steering session)
                                      (niwa-session-pending-follow-ups session))
                              :key (lambda (item) (plist-get item :id))
                              :test #'equal))))
    (unless entry (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No queued pending input on this row"))
    (niwa-view--assert-live-tip)
    (niwa-pending-inputs--remove session entry)
    (niwa-view--interaction-rebuild)))

(defun niwa-pending-inputs--assert-retract-composer ()
  "Refuse withdrawal into a busy composer or a nonempty draft."
  (when (or niwa-view--pending-input-edit
            niwa-view--prompt-hook-pending niwa-view--pending-skill-submission)
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Finish the current composer submission or edit first"))
  (unless (string-empty-p (or (niwa-view-composer-draft-text nil t) ""))
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Composer holds a draft; clear or send it before retracting")))

;;;###autoload
(defun niwa-view-retract-pending ()
  "Withdraw the newest pending input into an empty composer.
Restore its bound mentions, attachment grants, scope and reserved context.
Delivery is not paused; an entry already being dispatched cannot be withdrawn."
  (interactive)
  (require 'niwa-view-composer)
  (with-current-buffer (niwa-view-composer-owner)
    (niwa-pending-inputs--retract)))

(defun niwa-pending-inputs--retract ()
  "Withdraw the newest pending input from the owning view."
  (niwa-view--ensure-interactive-chat-view)
  (niwa-view--assert-live-tip)
  (when niwa-view--side-conversation-p
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "/btw has no pending input queue"))
  (let* ((session (niwa-view--session))
         ;; Ids record acceptance order even after reordering or conversion.
         (entry (cl-reduce (lambda (a b)
                             (if (or (null a) (> (plist-get b :id) (plist-get a :id))) b a))
                           (append (niwa-session-pending-steering session)
                                   (niwa-session-pending-follow-ups session))
                           :initial-value nil))
         (text (and entry (niwa-mention-bindings-copy-text (plist-get entry :input)))))
    (unless entry (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No pending input to retract"))
    (niwa-pending-inputs--assert-retract-composer)
    (niwa-pending-inputs--remove
     session entry
     #'niwa-pending-inputs--assert-retract-composer)
    (niwa-view--restore-composer-snapshot
     (list :text text :point-offset (length text)
           :dropped-file-grants
           (delete-dups (append (plist-get entry :dropped-file-grants)
                                (niwa-session-dropped-file-grants session))))
     session t)
    (niwa-view-refresh-input-prompt)
    (niwa-view--interaction-rebuild)
    (message "niwa: retracted pending input into the composer")))

(defun niwa-pending-inputs--resume-entry (session entries)
  "Combine SESSION's steering ENTRIES into one ordered follow-up.
Live preparations retain their accepted outcomes without rerunning hooks.
After restore only authored text remains, so normal submission prepares it."
  (let* ((input (mapconcat #'niwa-pending-inputs--copy-input entries "\n\n"))
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
                (niwa-prompt-submission-create
                 :session session :state 'reserved :input input :display-text input
                 :context-entries
                 (cl-loop for item in submissions append
                          (niwa-prompt-submission-context-entries item))
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

(defun niwa-pending-inputs--prepare-restored-steering (session entry)
  "Prepare restored ENTRY in SESSION without rerunning live submissions.
Continue Resume after accepting this part.  Until then ENTRY remains queued
and claimed, and every existing failure or interruption hold stays intact."
  (let* ((view (current-buffer))
         (release (lambda ()
                    (when (buffer-live-p view)
                      (with-current-buffer view
                        (when (eq entry niwa-view--pending-resume-entry)
                          (niwa-pending-inputs-cancel-resume)))))))
    (setq niwa-view--pending-resume-entry entry)
    (plist-put entry :delivering t)
    (condition-case err
        (niwa-view--submit-planned-input
         (niwa-pending-inputs--copy-input entry) nil release
         (lambda (submission)
           (with-current-buffer view
             (niwa-session-artifacts-assert-new-mutation-authority session)
             (unless (memq entry (niwa-session-pending-steering session))
               (user-error "Pending steering changed while Resume was preparing it"))
             (let* ((outcome (niwa-prompt-submission-outcome submission))
                    (context (plist-get outcome :request-context))
                    (prepared (copy-sequence entry)))
               (when (or (plist-get outcome :fork-outcome)
                         (not (niwa-view--steering-request-context-supported-p context)))
                 (user-error "Restored skill policy cannot combine with live steering; edit the input first"))
               (niwa-prompt-submission-reserve submission)
               (dolist (pair (list (cons :submission submission)
                                   (cons :model-input (plist-get outcome :model-input))
                                   (cons :transcript-payload
                                         (concat (plist-get outcome :transcript-input)
                                                 (plist-get outcome :render-data)))
                                   (cons :hook-audits (plist-get outcome :hook-audits))
                                   (cons :request-context context)
                                   (cons :delivering nil)))
                 (setq prepared (plist-put prepared (car pair) (cdr pair))))
               (niwa-pending-inputs--replace
                session 'steering (plist-get entry :id) prepared))
             (funcall release)
             (niwa-view-resume-pending))))
      ((error quit)
       (funcall release)
       (signal (car err) (cdr err))))))

(defun niwa-pending-inputs-cockpit-holds-p (session)
  "Return non-nil when SESSION's Pending Inputs cockpit is open."
  (when-let* ((cockpit (get-buffer niwa-pending-inputs-buffer-name)))
    (eq session (niwa-cockpit-context-session
                 (buffer-local-value 'niwa-cockpit--context cockpit)))))

;;;###autoload
(defun niwa-view-resume-pending ()
  "Resume idle pending input, starting one turn for leftover steering.
Steering is combined in queue order into the first follow-up.  Clear the
interrupt and failure holds, then let ordinary queued follow-ups drain.
An unaccepted dispatch retains the combined entry for retry or retraction."
  (interactive)
  (require 'niwa-view-composer)
  (with-current-buffer (niwa-view-composer-owner)
    (niwa-pending-inputs--resume)))

(defun niwa-pending-inputs--resume ()
  "Resume idle pending input from the owning view."
  (niwa-view--ensure-interactive-chat-view)
  (niwa-view--assert-live-tip)
  (when niwa-view--side-conversation-p
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "/btw has no pending input queue"))
  (when (or (niwa-turn-busy-p niwa--data-buffer)
            niwa-view--prompt-hook-pending niwa-view--pending-skill-submission
            niwa-view--pending-input-edit
            (buffer-local-value 'niwa-compact-run-in-flight niwa--data-buffer))
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Wait for or interrupt the active turn or composer submission first"))
  (unless (string-empty-p (or (niwa-view-composer-draft-text nil t) ""))
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Composer holds a draft; clear or send it before resuming"))
  (let* ((session (niwa-view--session))
         (steering (niwa-session-pending-steering session))
         (follow-ups (niwa-session-pending-follow-ups session)))
    (unless (or steering follow-ups) (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No pending input to resume"))
    (when (niwa-pending-inputs-cockpit-holds-p session)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Close the Pending Inputs cockpit before resuming"))
    (niwa-session-artifacts-assert-new-mutation-authority session)
    (niwa-recovery-assert-ready session)
    ;; Authority checks can process callbacks or new composer input.
    (when (or (niwa-turn-busy-p niwa--data-buffer)
              (not (string-empty-p (or (niwa-view-composer-draft-text nil t) "")))
              niwa-view--prompt-hook-pending niwa-view--pending-skill-submission
              niwa-view--pending-input-edit)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Session or composer changed before Resume; pending input remains queued"))
    (when (niwa-pending-inputs-cockpit-holds-p session)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Close the Pending Inputs cockpit before resuming"))
    (setq steering (niwa-session-pending-steering session)
          follow-ups (niwa-session-pending-follow-ups session))
    (unless (or steering follow-ups) (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No pending input to resume"))
    (dolist (entry (append steering follow-ups))
      (niwa-pending-inputs--assert-removable session entry))
    (if (and (cl-some (lambda (entry) (plist-get entry :submission)) steering)
             (cl-some (lambda (entry) (not (plist-get entry :submission))) steering))
        (niwa-pending-inputs--prepare-restored-steering
         session (cl-find-if (lambda (entry) (not (plist-get entry :submission))) steering))
      ;; Keep the existing drain's durable dispatch intent and commit fence.
      ;; Resume is explicit consent to retry failed follow-ups as well.
      (niwa-pending-inputs--set-queues
       session 'steering nil
       'follow-up (append (when steering
                            (list (niwa-pending-inputs--resume-entry session steering)))
                          (mapcar (lambda (entry)
                                    (if (eq (plist-get entry :state) 'failed-turn)
                                        (plist-put (copy-sequence entry) :state 'pending)
                                      entry))
                                  follow-ups)))
      (niwa-session-set-pending-input-failure-paused session nil)
      (niwa-session-set-pending-input-paused session nil)
      (niwa-permission-absence-clear session)
      (niwa-recovery-save session)
      (niwa-view--interaction-rebuild)
      (niwa-view--drain-follow-up niwa--data-buffer))))

(defun niwa-view-send-follow-up ()
  "Queue the composer as a follow-up, or send normally while idle."
  (interactive)
  (require 'niwa-view-composer)
  (with-current-buffer (niwa-view-composer-owner)
    (niwa-pending-inputs--send-follow-up)))

(defun niwa-pending-inputs--send-follow-up ()
  "Submit the draft as a follow-up from the owning view."
  (require 'niwa-compact-run)
  (niwa-view--ensure-interactive-chat-view)
  (when niwa-view--side-conversation-p
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "/btw does not queue follow-ups; wait for the active response"))
  (niwa-view--assert-live-tip)
  (when niwa-view--pending-input-edit
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Save or cancel the pending-input edit first"))
  (unless (and niwa--data-buffer (buffer-live-p niwa--data-buffer))
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No live data buffer associated with this view"))
  (niwa-session-durability-with-transaction
   (let* ((session (buffer-local-value 'niwa--session
                                       niwa--data-buffer))
          (occupied
           (or (niwa-turn-busy-p niwa--data-buffer)
               (niwa-session-pending-follow-ups session)
               (niwa-view--occupied-root-workflow session)
               niwa-view--prompt-hook-pending
               niwa-view--pending-skill-submission
               (buffer-local-value 'niwa-compact-run-in-flight
                                   niwa--data-buffer))))
     (niwa-session-artifacts-assert-new-mutation-authority session)
     (if (not occupied)
         (niwa-view-send)
       (when (buffer-local-value 'niwa-session--read-only-mode
                                 niwa--data-buffer)
         (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Session is open read-only (another host holds the lock)"))
       (let ((input (niwa-view--bind-input-mentions session)))
         (when (string-empty-p input)
           (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Nothing to send"))
         (when (niwa-skills-parse-slash-line input)
           (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Slash commands cannot be queued as follow-ups"))
         (niwa-view--queue-follow-up input)))))
  (let ((view (current-buffer)))
    (with-current-buffer (niwa-view-composer-buffer view)
      (goto-char (niwa-view-composer-draft-end view)))))


;;
;;; Automatic delivery

(defun niwa-view--drop-disallowed-guest-skills (session)
  "Drop SESSION's queued guest invocations the allowlist no longer admits.
A guest invocation is validated when its frame arrives and again here
at delivery, against the link tier the entry was queued under, so
shrinking `niwa-collaboration-guest-skills' or adding a name to the
unsafe list takes effect for entries already waiting.  Return the
remaining queue."
  (let ((entries (niwa-session-pending-follow-ups session))
        dropped)
    (dolist (entry entries)
      (let ((name (plist-get entry :guest-invoke))
            (skills (plist-get entry :guest-skills))
            (role (plist-get entry :guest-role)))
        (when (or (and name
                       (not (and (fboundp 'niwa-collaboration--guest-invocable-p)
                                 (niwa-collaboration--guest-invocable-p name role))))
                  (and skills
                       (not (and (fboundp 'niwa-collaboration--guest-skills-admitted-p)
                                 (niwa-collaboration--guest-skills-admitted-p
                                  skills role session)))))
          (push entry dropped))))
    (when dropped
      (niwa-pending-inputs--set-queues
       session 'follow-up
       (cl-remove-if (lambda (entry) (memq entry dropped)) entries))
      (niwa-view--interaction-rebuild)
      (niwa-diagnostic-message (niwa-diagnostic-scope session)
       "Dropped %d queued guest invocation%s no longer allowed"
       (length dropped) (if (= 1 (length dropped)) "" "s")))
    (niwa-session-pending-follow-ups session)))

(defun niwa-view--pending-follow-up-kind (entry)
  "Return the delivery kind derived from pending follow-up ENTRY."
  (cond
   ((plist-get entry :guest-invoke) 'invocation)
   ((plist-get entry :guest-skills) 'skills)
   ((plist-get entry :submission) 'prepared)
   (t 'prompt)))


(defun niwa-view--dispatch-follow-up-entry
    (kind entry input data-buffer before-send after-insert release)
  "Dispatch follow-up ENTRY of KIND with INPUT.
DATA-BUFFER owns the root transcript.  BEFORE-SEND, AFTER-INSERT, and RELEASE
implement the drain's shared delivery transaction."
  (pcase kind
    ('invocation
     ;; A guest invocation is a command or skill name, not a prompt: it has
     ;; to reach the same dispatch typing the line in the composer would.
     (funcall before-send)
     (niwa-view-run-invocation
      (plist-get entry :guest-invoke) input
      :on-quiet after-insert
      :on-sent after-insert))
    ('skills
     (niwa-view--submit-planned-input
      input before-send release nil after-insert nil
      (plist-get entry :guest-skills)))
    ('prepared
     (niwa-view--dispatch-prepared-outcome
      (plist-get entry :submission) data-buffer
      :before-send before-send
      :after-insert after-insert
      :on-block release))
    ('prompt
     (niwa-view--submit-planned-input
      input before-send release nil after-insert
      (plist-get entry :inert-skills)))
    (_ (error "Unknown pending follow-up kind: %S" kind))))

(defun niwa-view--drain-follow-up (data-buffer)
  "Submit the next pending follow-up for DATA-BUFFER.

Each bound entry is planned and prepared as its own turn.  The queue entry is
removed only when the resulting prompt reaches its transcript commit boundary."
  (when (buffer-live-p data-buffer)
    (niwa-session-durability-with-transaction
     (let* ((view-buffer (buffer-local-value 'niwa--view-buffer data-buffer))
            (session (buffer-local-value 'niwa--session data-buffer)))
       ;; A settling turn or a running compaction refuses dispatch only for
       ;; now; the entry waits for the drain that follows it.
       (when (and session
                  (buffer-live-p view-buffer)
                  (not (niwa-turn-busy-p data-buffer))
                  (not (memq session (bound-and-true-p niwa-claude-code-fork--sessions)))
                  (not (buffer-local-value 'niwa-compact-run-in-flight
                                           data-buffer)))
         (with-current-buffer view-buffer
           (when (and (not niwa-view--agent-transcript-p)
                      (not niwa-view--prompt-hook-pending)
                      (not niwa-view--pending-skill-submission)
                      (not (niwa-view--follow-up-auto-drain-blocked-p
                            session))
                      (string-empty-p (niwa-view--input-text)))
             (niwa-session-artifacts-assert-new-mutation-authority
              session)
             (when (niwa-view--drop-disallowed-guest-skills session)
               (let* ((entry (niwa-view--next-follow-up session))
                      (kind (niwa-view--pending-follow-up-kind entry))
                      (input (niwa-view--pending-input-text entry))
                      (dropped-file-grants
                       (plist-get entry :dropped-file-grants)))
                 (let* ((active-grants-before
                         (copy-sequence
                          (niwa-session-active-dropped-file-grants
                           session)))
                        (delivered nil)
                        (transcript-tick (buffer-chars-modified-tick data-buffer))
                        (before-send
                         (lambda ()
                           (niwa-view--activate-dropped-file-grants
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
                                      (not (niwa-turn-busy-p data-buffer)))
                                 (plist-put entry :state nil)
                               (plist-put entry :state 'failed-turn)
                               (niwa-session-set-pending-input-failure-paused session t))
                             (niwa-recovery-save session))
                           (unless delivered
                             (niwa-session--set-active-dropped-file-grants
                              session active-grants-before))
                           (when (buffer-live-p data-buffer)
                             (with-current-buffer data-buffer
                               (setq niwa-view--pending-guest-attribution nil)))
                           (niwa-view--interaction-rebuild)))
                        (after-insert
                         (lambda ()
                           (niwa-telemetry-record
                            session 'user-message-dequeued
                            :message-hash (secure-hash 'sha256 input)
                            :queue-depth-before
                            (length
                             (niwa-view--pending-follow-ups session))
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
                            (and (niwa-session-goal session)
                                 (niwa-goal-id
                                  (niwa-session-goal session))))
                           (setq delivered t)
                           ;; The turn's own checkpoints persist the removal;
                           ;; until then a restart reloads the durable
                           ;; `dispatching' mark as input requiring review.
                           (niwa-session-set-pending-inputs
                            session 'follow-up
                            (remq entry (niwa-view--pending-follow-ups session)))
                           (when (fboundp 'niwa-collaboration-notify-queue-changed)
                             (niwa-collaboration-notify-queue-changed session))
                           (niwa-view--interaction-rebuild))))
                   ;; Consumed where the prompt and its hook audits are
                   ;; inserted; cleared on every blocked or failed path.
                   (with-current-buffer data-buffer
                     (setq niwa-view--pending-guest-attribution
                           (when-let* ((name (plist-get entry :guest-name)))
                             (append (list :type 'guest-prompt :name name)
                                     (when-let* ((shared (plist-get entry :shared-question)))
                                       (list :shared shared))))))
                   ;; Delivery is asynchronous and the entry stays queued
                   ;; until the transcript commit; the flag keeps a guest
                   ;; retraction from deleting attachment files an
                   ;; in-flight prompt is about to read.
                   (plist-put entry :state 'dispatching)
                   (niwa-recovery-save-now session)
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
                         (niwa-view--dispatch-follow-up-entry
                          kind entry input data-buffer
                          before-send after-insert release))
                     (inhibited-interaction
                      (funcall release)
                      (niwa-view--refuse-guest-follow-up entry session err))
                     ((error quit)
                      (funcall release)
                      ;; A refused guest entry left at the head would block
                      ;; every later message; drop it and tell its sender.
                      (cond
                       ;; Preparation can begin while a readiness check yields.
                       ((and (not (eq 'quit (car err)))
                             (memq session (bound-and-true-p niwa-claude-code-fork--sessions))))
                       ((and (plist-get entry :guest-id) (not (eq 'quit (car err))))
                        (niwa-view--refuse-guest-follow-up entry session err))
                       (t (signal (car err) (cdr err))))))))))))))))

(defun niwa-view--refused-question (err)
  "Return \": QUESTION\" for an `inhibited-interaction' ERR naming one, else \"\"."
  (let ((question (cadr err)))
    (if (and (stringp question) (not (string-empty-p question)))
        (concat ": " question)
      "")))

(defun niwa-view--refuse-guest-follow-up (entry session &optional err)
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
         (blocker (and (not (or inhibited review)) (niwa-recovery-blocker session))))
    (unless (or review blocker)
      (niwa-pending-inputs--set-queues
       session 'follow-up (remq entry (niwa-view--pending-follow-ups session)))
      (dolist (path (plist-get entry :guest-paths))
        (when (file-exists-p path)
          (ignore-errors (delete-file path)))))
    (niwa-view--interaction-rebuild)
    (niwa-diagnostic-warning (niwa-diagnostic-scope session)
     'niwa
     (format "A message from %s was %s: %s"
             (or (plist-get entry :guest-name) "a guest")
             (cond (review "interrupted and needs review")
                   (blocker "held")
                   (t "not sent"))
             (if inhibited
                 (concat "its turn needed a decision in Emacs"
                         (niwa-view--refused-question err))
               (error-message-string err))))
    (when (fboundp 'niwa-collaboration-notify-guest)
      (niwa-collaboration-notify-guest
       session (plist-get entry :guest-id)
       (cond
        (review "Your message was interrupted; the owner can review it")
        (blocker (concat (niwa-recovery-public-message blocker)
                         "; your message will be sent once that is resolved"))
        (inhibited (concat niwa-collaboration-needs-host-message
                           "; your message was not sent"))
        (t "The host could not run your message; it was not sent"))))))

(defun niwa-view--run-follow-up-drain (data-buffer)
  "Drain one pending follow-up for DATA-BUFFER if it is live.

The drain admits a new mutation, which is target I/O.  It is scheduled from a
timer, and a timer fires wherever the main loop happens to be waiting -- inside
an unrelated remote operation started by redisplay or another package included
-- so it waits for an idle transport rather than nesting."
  (when (buffer-live-p data-buffer)
    (niwa-transport-run-when-idle
     (list 'follow-up-drain data-buffer)
     (buffer-local-value 'default-directory data-buffer)
     (lambda ()
       (when (buffer-live-p data-buffer)
         (niwa-view--drain-follow-up data-buffer))))))

(defun niwa-pending-inputs-schedule-held-input-delivery (&optional data-buffer)
  "Offer held follow-ups or background mail after a hold clears.
DATA-BUFFER defaults to this view's data buffer.  Recheck at dispatch so a
consumed input does not start a Goal continuation instead."
  (let ((buffer (or data-buffer niwa--data-buffer)))
    (when (memq (niwa-pending-inputs--idle-work buffer) '(follow-up event))
      (run-at-time 0 nil #'niwa-pending-inputs--offer-idle-work-now buffer t))))

(defun niwa-pending-inputs--idle-work (data-buffer)
  "Return the kind of idle work DATA-BUFFER's session holds, or nil.
The result is `follow-up' while follow-ups are queued, else `event' while
background work has unread waking mail, else `goal' for an active Goal.
Only an idle data buffer with a live root view qualifies."
  (when-let* (((buffer-live-p data-buffer))
              (session (buffer-local-value 'niwa--session data-buffer))
              ((buffer-live-p
                (buffer-local-value 'niwa--view-buffer data-buffer)))
              ((not (buffer-local-value 'niwa--current-request
                                        data-buffer))))
    (cond
     ((niwa-session-pending-follow-ups session) 'follow-up)
     ((niwa-agent-control-wake-pending-p session) 'event)
     ((when-let* ((goal (niwa-session-goal session)))
        (eq (niwa-goal-status goal) 'active))
      'goal))))

(defun niwa-pending-inputs-offer-root-event (session &optional data-buffer)
  "Offer SESSION's idle root a turn for committed background mail.
Use DATA-BUFFER when it belongs to the root, including an unsaved session;
otherwise resolve the session's root buffer.  Failure to offer cannot undo
mail delivery: the unread records remain available to the next turn."
  (condition-case err
      (when-let* ((buffer
                   (if (and (buffer-live-p data-buffer)
                            (eq session (buffer-local-value
                                         'niwa--session data-buffer))
                            (not (buffer-local-value
                                  'niwa--agent-invocation data-buffer)))
                       data-buffer
                     (niwa-session-root-buffer session)))
                  ((buffer-live-p buffer)))
        (niwa-pending-inputs-offer-idle-work buffer))
    (error
     (niwa--warn-once
      (niwa-diagnostic-scope session)
      'background-wake "Could not offer a background event turn: %s"
      (error-message-string err)))))

(defun niwa-pending-inputs-offer-idle-work (data-buffer)
  "Offer DATA-BUFFER's queued idle work again after a hold on it cleared.
Queued follow-ups go first, then a turn for background work that reported.
Only once neither is waiting does an active Goal get a continuation turn.
The decision waits for the current command to finish, and the offered work
re-checks its own gates when it runs, so an offer made while the session is
still occupied does nothing."
  (when (niwa-pending-inputs--idle-work data-buffer)
    (run-at-time 0 nil #'niwa-pending-inputs--offer-idle-work-now
                 data-buffer)))

(defun niwa-pending-inputs--offer-idle-work-now (data-buffer &optional inputs-only)
  "Start the idle work `niwa-pending-inputs-offer-idle-work' offered.
DATA-BUFFER is checked again, because the offering command may have
started a turn or settled the work itself.  INPUTS-ONLY excludes Goal work."
  (pcase (niwa-pending-inputs--idle-work data-buffer)
    ('follow-up (niwa-view--run-follow-up-drain data-buffer))
    ('event (niwa-pending-inputs--run-event-wake data-buffer))
    ('goal (unless inputs-only
             (niwa-goal-schedule-continuation
              (buffer-local-value 'niwa--session data-buffer)
              data-buffer)))))

(defconst niwa-pending-inputs--event-trigger
  (concat "Background work has reported; its messages follow. "
          "This is not a message from the user.")
  "Fixed prompt of a turn started for background work that reported.
The reports themselves reach the model as mailbox records, never as this
prompt, so their text cannot pose as the user.")

(defun niwa-pending-inputs--interaction-pending-p (session view-buffer)
  "Return non-nil while SESSION or VIEW-BUFFER awaits a user decision."
  (or (niwa-session-permission-queue session)
      (niwa-session-pending-plan-approval session)
      (niwa-view-interaction-pending-p view-buffer)))

(defvar-local niwa-pending-inputs--wake-refusal nil
  "Reason the data buffer's last background wake was refused, or nil.
Telemetry records a refusal only when this reason changes.")

(defun niwa-pending-inputs--event-wake-refusal (session data-buffer view-buffer)
  "Return the gate refusing SESSION's background wake in DATA-BUFFER, or nil.
VIEW-BUFFER is the live root view.  Gates are checked in admission order."
  (cond
   ((or (niwa-turn-busy-p data-buffer)
        (buffer-local-value 'niwa-compact-run-in-flight data-buffer))
    'busy)
   ((niwa-session-pending-follow-ups session) 'drain-blocked-queued)
   ((with-current-buffer view-buffer
      (or niwa-view--prompt-hook-pending
          niwa-view--pending-skill-submission))
    'prompt-hook)
   ((niwa-recovery-blocker session) 'recovery)
   ((niwa-session-pending-steering session)
    (if (niwa-session-pending-input-failure-paused session)
        'drain-blocked-latch
      'drain-blocked-queued))
   ;; Background mail is not user input: a failure latch that holds no
   ;; queued input must not strand it.
   ((with-current-buffer view-buffer
      (niwa-view--follow-up-auto-drain-blocked-p
       session (null (niwa-session-pending-steering session))))
    (if (with-current-buffer view-buffer
          (niwa-view--follow-up-auto-drain-blocked-p session t))
        'drain-blocked-queued
      'drain-blocked-latch))
   ((niwa-pending-inputs--interaction-pending-p session view-buffer)
    'interaction)
   ((not (string-empty-p
          (string-trim (or (niwa-view-composer-draft-text view-buffer t) ""))))
    'composer-draft)))

(defun niwa-pending-inputs--note-wake-refusal (session data-buffer reason)
  "Record REASON for SESSION's DATA-BUFFER when it differs from the last one."
  (unless (eq reason (buffer-local-value 'niwa-pending-inputs--wake-refusal
                                         data-buffer))
    (with-current-buffer data-buffer
      (setq niwa-pending-inputs--wake-refusal reason))
    (when reason
      (niwa-telemetry-record session 'event-wake-refused :reason reason))))

(defun niwa-pending-inputs--event-wake-view (data-buffer)
  "Return DATA-BUFFER's live root view when no background wake hold remains.
A refused wake records its gate once per change of reason."
  (when (buffer-live-p data-buffer)
    (let ((view-buffer (buffer-local-value 'niwa--view-buffer data-buffer))
          (session (buffer-local-value 'niwa--session data-buffer)))
      (when (and session
                 (buffer-live-p view-buffer)
                 (not (buffer-local-value 'niwa-view--agent-transcript-p
                                          view-buffer))
                 (niwa-agent-control-wake-pending-p session))
        (let ((reason (niwa-pending-inputs--event-wake-refusal
                       session data-buffer view-buffer)))
          (niwa-pending-inputs--note-wake-refusal session data-buffer reason)
          (unless reason view-buffer))))))

(defun niwa-pending-inputs--wake-for-event (data-buffer)
  "Start one background-event turn in DATA-BUFFER when no hold remains.
The holds are those of the follow-up drain, plus queued follow-ups, which
go first, and pending user decisions.  The turn is a new request, so it
runs under the session's current permission settings."
  (when-let* ((view-buffer (niwa-pending-inputs--event-wake-view data-buffer))
              (session (buffer-local-value 'niwa--session data-buffer)))
    (niwa-session-durability-with-transaction
     (niwa-session-artifacts-assert-new-mutation-authority session)
     ;; Authority admission can yield during target I/O.  A user draft,
     ;; another turn, or a consumed wake must win over this stale offer.
     (when (and (eq view-buffer
                    (niwa-pending-inputs--event-wake-view data-buffer))
                (eq session (buffer-local-value 'niwa--session data-buffer)))
       (with-current-buffer data-buffer
         (when-let* ((context (niwa-schedule-wake-request-context session)))
           (setq-local niwa-skills--pending-request-context context))
         (setq niwa-view--pending-guest-attribution
               (list :type 'background-event))
         (let* ((records (cl-remove-if-not (lambda (r) (eq 'WAKE_UP (plist-get r :type)))
                                           (niwa-agent-control-context-mailbox session)))
                (trigger (if records (list :turn-trigger 'scheduled-wake :wake-runs records)
                           'background-event)))
           (unwind-protect
             (condition-case err
                 (niwa--submit-generated-turn
                  niwa-pending-inputs--event-trigger
                  niwa-pending-inputs--event-trigger nil nil trigger)
               ;; Admission drains the override; a start that failed must
               ;; not leave it for a later turn.
               (error (setq-local niwa-skills--pending-request-context nil)
                      (when (and records (plist-get trigger :wake-runs))
                        (niwa-schedule-settle-wake session data-buffer trigger 'error
                                                      (error-message-string err)))
                      (signal (car err) (cdr err))))
           ;; Consumed where the prompt is inserted; a start that
           ;; failed before that must not label a later prompt.
           (setq niwa-view--pending-guest-attribution nil))))))))

(defun niwa-pending-inputs--run-event-wake (data-buffer)
  "Start a background-event turn for DATA-BUFFER once the transport is idle.
Like the follow-up drain this admits target I/O from a timer, so it waits
rather than nesting inside a remote operation.  A failure leaves the waking
mail unread for the next turn."
  (when (buffer-live-p data-buffer)
    (let ((diagnostic-scope (niwa-diagnostic-scope data-buffer)))
      (niwa-transport-run-when-idle
       (list 'event-wake data-buffer)
       (buffer-local-value 'default-directory data-buffer)
       (lambda ()
         (condition-case err
             (niwa-pending-inputs--wake-for-event data-buffer)
           (error
            (niwa-diagnostic-message diagnostic-scope
             "%s (background event turn failed)"
             (error-message-string err)))))))))

(defun niwa-pending-inputs-offer-event-after-turn (fsm)
  "Offer FSM's idle session a turn for background work that reported.
Only a successful turn calls this; a failed one does not wake again, so a
failing wake cannot loop."
  (when-let* ((info (and fsm (niwa-engine-info fsm)))
              (data-buffer (plist-get info :buffer))
              ((buffer-live-p data-buffer))
              (session (buffer-local-value 'niwa--session data-buffer))
              ((niwa-agent-control-wake-pending-p session)))
    (niwa-pending-inputs-offer-idle-work data-buffer)))

(defvar-local niwa-view--removing-draft-text nil
  "Whether the current edit removes nonblank composer text.")

(defun niwa-pending-inputs--root-view-for-draft-edit ()
  "Return the current composer's owning view if it is the root."
  (when (and (bound-and-true-p niwa-composer--view)
             (buffer-live-p niwa--data-buffer)
             (eq niwa-composer--view
                 (buffer-local-value 'niwa--view-buffer niwa--data-buffer)))
    niwa-composer--view))

(defun niwa-view--note-draft-deletion (start end)
  "Record whether START to END removes any nonblank composer text."
  (setq niwa-view--removing-draft-text
        (when-let* (((< start end))
                    (view (niwa-pending-inputs--root-view-for-draft-edit))
                    ((<= (niwa-view-composer-draft-start view) start end
                         (niwa-view-composer-draft-end view))))
          (save-excursion
            (goto-char start)
            (skip-chars-forward " \t\n\r" end)
            (< (point) end)))))

(defun niwa-view--offer-follow-up-after-draft-change (&rest _)
  "Offer held follow-ups or wakes when an edit empties the root composer."
  (when niwa-view--removing-draft-text
    (setq niwa-view--removing-draft-text nil)
    (when-let* ((view (niwa-pending-inputs--root-view-for-draft-edit))
                (end (niwa-view-composer-draft-end view))
                ((save-excursion
                   (save-restriction
                     (widen)
                     (goto-char (niwa-view-composer-draft-start view))
                     (skip-chars-forward " \t\n\r" end)
                     (= (point) end))))
                ((or (with-current-buffer view (niwa-view--pending-follow-ups))
                     (eq 'composer-draft
                         (buffer-local-value 'niwa-pending-inputs--wake-refusal
                                             niwa--data-buffer)))))
      (niwa-pending-inputs-schedule-held-input-delivery))))

(defun niwa-view--schedule-follow-up-drain (fsm)
  "Schedule the next follow-up after FSM completes successfully."
  (when-let* ((info (and fsm (fboundp 'niwa-engine-info)
                         (niwa-engine-info fsm)))
              (data-buffer (plist-get info :buffer))
              ((buffer-live-p data-buffer)))
    (run-at-time 0 nil
                 #'niwa-view--run-follow-up-drain
                 data-buffer)))

(defun niwa-pending-inputs--session (&optional context)
  "Return the Pending Inputs session for CONTEXT."
  (or (niwa-cockpit-context-session
       (or context (niwa-cockpit-surface-context)))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No niwa session in this buffer")))

(defun niwa-pending-inputs--collect (context)
  "Collect pending-input row projections for CONTEXT."
  (let ((session (niwa-pending-inputs--session context))
        rows)
    (dolist (category '(steering follow-up))
      (let ((position 0)
            (entries (niwa-session-pending-inputs session category)))
        (dolist (entry entries)
          (push (list :id (plist-get entry :id)
                      :category category
                      :position (cl-incf position)
                      :state (or (plist-get entry :state) 'pending)
                      :entry entry)
                rows))))
    (nreverse rows)))

(defun niwa-pending-inputs--entry (item _context)
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
       (when-let* ((guest (plist-get entry :guest-name)))
         (format "[⇄ %s] " guest))
       (niwa-view--pending-input-preview
        (niwa-view--pending-input-text entry)))))))

(defun niwa-pending-inputs--header (items context)
  "Return the cockpit header for ITEMS and CONTEXT."
  (format "%d pending input%s%s"
          (length items)
          (if (= 1 (length items)) "" "s")
          (let ((session (niwa-pending-inputs--session context)))
            (concat
             (when (niwa-session-pending-input-failure-paused session)
               " · failure recovery required")
             (when (niwa-session-pending-input-paused session)
               " · delivery paused")))))

(defun niwa-pending-inputs--details (item _context)
  "Return the complete information report for pending-input ITEM."
  (list :title "Pending input"
        :sections (list (list :id 'input :title "Full input"
                              :body (niwa-view--pending-input-text (plist-get item :entry))))))

(defun niwa-pending-inputs--setup (context)
  "Pause delivery before rendering the cockpit for CONTEXT."
  (let ((session (niwa-pending-inputs--session context))
        (view (niwa-cockpit-context-view-buffer context)))
    (niwa-session-set-pending-input-paused session t)
    (niwa-recovery-save session)
    (when (buffer-live-p view)
      (with-current-buffer view
        (niwa-view--interaction-rebuild)))))

(defun niwa-pending-inputs--selected ()
  "Return the selected pending-input row projection."
  (niwa-cockpit-surface-selected))

(defun niwa-pending-inputs--refresh (&optional selected-id)
  "Refresh live pending-input UI, preserving SELECTED-ID and deletion marks."
  (let* ((context (niwa-cockpit-surface-context))
         (view (niwa-cockpit-context-view-buffer context)))
    (when (buffer-live-p view)
      (with-current-buffer view
        (niwa-view--interaction-rebuild)))
    (niwa-cockpit-surface-refresh selected-id)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (member (tabulated-list-get-id)
                      niwa-pending-inputs--marked-ids)
          (tabulated-list-put-tag "D"))
        (forward-line 1)))))

(defun niwa-pending-inputs-refresh ()
  "Refresh the live Pending Inputs cockpit."
  (interactive)
  (niwa-pending-inputs--refresh (tabulated-list-get-id)))

(defun niwa-pending-inputs--replace
    (session category id replacement)
  "Replace pending entry ID in SESSION CATEGORY with REPLACEMENT."
  (let* ((entries (niwa-session-pending-inputs session category))
         (position
          (cl-position-if
           (lambda (entry) (equal id (plist-get entry :id)))
           entries)))
    (unless position
      (user-error "Pending input is no longer queued"))
    (let ((updated (copy-sequence entries)))
      (setcar (nthcdr position updated) replacement)
      (niwa-pending-inputs--set-queues session category updated))
    replacement))

(defun niwa-pending-inputs--move-between
    (session source target id replacement)
  "Move ID from SESSION SOURCE to TARGET tail as REPLACEMENT."
  (let ((source-entries (niwa-session-pending-inputs session source)))
    (unless (cl-find id source-entries
                     :key (lambda (entry) (plist-get entry :id))
                     :test #'equal)
      (user-error "Pending input is no longer queued"))
    (niwa-pending-inputs--set-queues
     session
     source (cl-remove id source-entries
                       :key (lambda (entry) (plist-get entry :id))
                       :test #'equal)
     target (append (niwa-session-pending-inputs session target)
                    (list replacement)))
    replacement))

(defun niwa-pending-inputs--restore-entry-metadata
    (prepared original)
  "Copy stable queue metadata from ORIGINAL to PREPARED."
  (dolist (key '(:id :category :queued-at-time :queued-at-turn
                 :queued-at-goal-id :state))
    (when (plist-member original key)
      (setq prepared (plist-put prepared key (plist-get original key)))))
  prepared)

(defun niwa-pending-inputs--return-to-cockpit (state)
  "Restore the suspended draft and return to the cockpit for STATE."
  (let* ((context (plist-get state :context))
         (session (niwa-pending-inputs--session context))
         (view (niwa-cockpit-context-view-buffer context))
         (cockpit (plist-get state :cockpit-buffer))
         (id (plist-get state :id)))
    (when (buffer-live-p view)
      (with-current-buffer view
        (setq niwa-view--pending-input-edit nil)
        (niwa-view--restore-composer-snapshot
         (plist-get state :composer-snapshot) session t)
        (niwa-view-refresh-input-prompt)
        (niwa-view--interaction-rebuild)))
    (if (buffer-live-p cockpit)
        (progn
          (with-current-buffer cockpit
            (niwa-cockpit-surface-refresh id))
          (when-let* ((window (display-buffer cockpit)))
            (select-window window)))
      (when (buffer-live-p view)
        (when-let* ((window (display-buffer view)))
          (select-window window))))))

(defun niwa-pending-inputs-edit ()
  "Edit the selected pending input in its owning view composer."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (session (niwa-pending-inputs--session context))
         (view (niwa-cockpit-context-view-buffer context))
         (item (niwa-pending-inputs--selected))
         (entry (plist-get item :entry))
         (category (plist-get item :category))
         (position (plist-get item :position))
         (cockpit (current-buffer)))
    (unless (buffer-live-p view)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No live owning view"))
    (with-current-buffer view
      (when niwa-view--pending-input-edit
        (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "A pending-input edit is already active"))
      (let* ((snapshot (niwa-view--composer-snapshot session))
             (input (niwa-mention-bindings-copy-text
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
        (setq niwa-view--pending-input-edit state)
        (niwa-view--restore-composer-snapshot
         (list :text input
               :point-offset (length input)
               :dropped-file-grants
               (copy-sequence
                (plist-get entry :dropped-file-grants)))
         session t)
        (niwa-view-refresh-input-prompt)))
    (when-let* ((window (display-buffer view)))
      (select-window (or (niwa-view-composer-display window) window)))))

(defun niwa-pending-inputs--finish-replacement
    (state replacement)
  "Replace STATE's entry with REPLACEMENT and finish the edit."
  (let* ((context (plist-get state :context))
         (session (niwa-pending-inputs--session context))
         (original (plist-get state :entry))
         (replacement
          (niwa-pending-inputs--restore-entry-metadata
           replacement original)))
    (niwa-pending-inputs--replace
     session
     (plist-get state :category)
     (plist-get state :id)
     replacement)
    (niwa-pending-inputs--return-to-cockpit state)
    replacement))

(defun niwa-pending-inputs--save-follow-up (state input session)
  "Save follow-up edit STATE using bound INPUT in SESSION."
  (let* ((original (copy-sequence (plist-get state :entry)))
         (submission (plist-get original :submission))
         (grants
          (niwa-view--pop-dropped-file-grants-for-input input session))
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
    (when submission (niwa-prompt-submission-restore submission))
    (niwa-pending-inputs--finish-replacement state replacement)))

(defun niwa-pending-inputs--save-failed-steering (state input session)
  "Save failed steering edit STATE as review-only INPUT in SESSION."
  (let* ((original (plist-get state :entry))
         (grants
          (niwa-view--pop-dropped-file-grants-for-input input session))
         replacement)
    (when-let* ((submission (plist-get original :submission)))
      (niwa-prompt-submission-restore submission))
    (cl-loop for (key value) on original by #'cddr
             unless (memq key '(:model-input :transcript-payload
                                :hook-audits :request-context :submission))
             do (setq replacement
                      (append replacement (list key value))))
    (setq replacement
          (plist-put replacement :input input)
          replacement
          (plist-put replacement :dropped-file-grants grants))
    (niwa-pending-inputs--finish-replacement state replacement)))

(defun niwa-pending-inputs--save-steering (state input request)
  "Prepare and save steering edit STATE from INPUT for REQUEST."
  (setf (plist-get state :saving) t)
  (let ((diagnostic-scope
         (niwa-diagnostic-scope (niwa-request-session request))))
    (niwa-view--submit-planned-input
     input nil
     (lambda ()
       (when (eq state niwa-view--pending-input-edit)
         (setf (plist-get state :saving) nil)
         (niwa-diagnostic-message
          diagnostic-scope "Pending-input edit was not accepted")))
     (lambda (submission)
       (when (eq state niwa-view--pending-input-edit)
         (if-let* ((prepared
                    (niwa-view--prepare-steering-entry
                     submission request)))
             (niwa-pending-inputs--finish-replacement state prepared)
           (setf (plist-get state :saving) nil)))))))

(defun niwa-pending-inputs-save-edit ()
  "Validate and save the active pending-input composer edit."
  (interactive)
  (require 'niwa-view-composer)
  (with-current-buffer (niwa-view-composer-owner)
    (niwa-pending-inputs--save-edit)))

(defun niwa-pending-inputs--save-edit ()
  "Validate and save the pending-input edit in the owning view."
  (unless niwa-view--pending-input-edit
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No pending-input edit is active"))
  (let* ((state niwa-view--pending-input-edit)
         (context (plist-get state :context))
         (session (niwa-pending-inputs--session context))
         (category (plist-get state :category)))
    (niwa-session-artifacts-assert-new-mutation-authority session)
    (when (plist-get state :saving)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Pending-input preparation is still running"))
    (condition-case err
        (let ((input (niwa-view--bind-input-mentions session)))
          (when (string-empty-p input)
            (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Pending input must not be empty"))
          (when (string-match-p "\\`[ \t]*[/]" input)
            (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Slash commands cannot be pending input"))
          (cond
           ((eq category 'follow-up)
            (niwa-pending-inputs--save-follow-up
             state input session))
           ((eq (plist-get (plist-get state :entry) :state)
                'failed-turn)
            (niwa-pending-inputs--save-failed-steering
             state input session))
           (t
            (let ((request
                   (and (buffer-live-p niwa--data-buffer)
                        (buffer-local-value
                         'niwa--current-request niwa--data-buffer))))
              (unless (and request
                           (equal (niwa-request-id request)
                                  (plist-get
                                   (plist-get state :entry)
                                   :request-id)))
                (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Steering turn is no longer active"))
              (niwa-pending-inputs--save-steering
               state input request)))))
      (error
       (niwa-diagnostic-message (niwa-diagnostic-scope session)
                "%s (pending-input edit failed)"
                (error-message-string err))))))

(defun niwa-pending-inputs-cancel-edit ()
  "Cancel the active pending-input composer edit."
  (interactive)
  (require 'niwa-view-composer)
  (with-current-buffer (niwa-view-composer-owner)
    (unless niwa-view--pending-input-edit
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No pending-input edit is active"))
    (niwa-pending-inputs--return-to-cockpit niwa-view--pending-input-edit)
    (message "niwa: pending-input edit cancelled")))

(defun niwa-pending-inputs--move (offset)
  "Move selected pending input by OFFSET inside its category."
  (let* ((context (niwa-cockpit-surface-context))
         (session (niwa-pending-inputs--session context))
         (item (niwa-pending-inputs--selected))
         (category (plist-get item :category))
         (id (plist-get item :id))
         (entries (copy-sequence
                   (niwa-session-pending-inputs session category)))
         (index (cl-position id entries
                             :key (lambda (entry) (plist-get entry :id))
                             :test #'equal))
         (target (and index (+ index offset))))
    (niwa-session-artifacts-assert-new-mutation-authority session)
    (unless (and target (>= target 0) (< target (length entries)))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Pending input is already at the category boundary"))
    (cl-rotatef (nth index entries) (nth target entries))
    (niwa-pending-inputs--set-queues session category entries)
    (niwa-pending-inputs--refresh id)))

(defun niwa-pending-inputs-move-up ()
  "Move the selected pending input up within its category."
  (interactive)
  (niwa-pending-inputs--move -1))

(defun niwa-pending-inputs-move-down ()
  "Move the selected pending input down within its category."
  (interactive)
  (niwa-pending-inputs--move 1))

(defun niwa-pending-inputs--current-request (context)
  "Return CONTEXT's current root request, or nil."
  (when-let* ((data-buffer
               (niwa-cockpit-context-data-buffer context))
              ((buffer-live-p data-buffer)))
    (buffer-local-value 'niwa--current-request data-buffer)))

(defun niwa-pending-inputs--copy-input (entry)
  "Return ENTRY's propertized input copy."
  (niwa-mention-bindings-copy-text (plist-get entry :input)))

(defun niwa-pending-inputs--conversion-finished
    (cockpit context selected-id)
  "Refresh COCKPIT and CONTEXT after converting SELECTED-ID."
  (if (buffer-live-p cockpit)
      (with-current-buffer cockpit
        (setq niwa-pending-inputs--converting-id nil)
        (niwa-pending-inputs--refresh selected-id))
    (when-let* ((view (niwa-cockpit-context-view-buffer context)))
      (with-current-buffer view
        (niwa-view--interaction-rebuild)))))

(defun niwa-pending-inputs-make-steering ()
  "Convert the selected follow-up to steering after full preparation."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (session (niwa-pending-inputs--session context))
         (item (niwa-pending-inputs--selected))
         (entry (plist-get item :entry))
         (id (plist-get item :id))
         (request (niwa-pending-inputs--current-request context))
         (view (niwa-cockpit-context-view-buffer context))
         (cockpit (current-buffer))
         (original-grants
          (copy-sequence
           (niwa-session-dropped-file-grants session)))
         (entry-grants
          (copy-sequence (plist-get entry :dropped-file-grants)))
         (staged-grants
          (cl-set-difference entry-grants original-grants :test #'equal))
         (restore-grants
          (lambda ()
            (let ((current
                   (niwa-session-dropped-file-grants session)))
              (niwa-session--set-dropped-file-grants
               session
               (append
                original-grants
                (cl-set-difference
                 current
                 (append original-grants staged-grants)
                 :test #'equal)))))))
    (niwa-session-artifacts-assert-new-mutation-authority session)
    (unless (eq (plist-get item :category) 'follow-up)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Pending input is already steering"))
    ;; The follow-up drain owns both the delivery-time allowlist recheck
    ;; and the command dispatch a guest invocation needs; steering
    ;; delivery has neither.
    (when (or (plist-get entry :guest-invoke) (plist-get entry :guest-skills))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Guest invocations cannot be converted to steering"))
    (when niwa-pending-inputs--converting-id
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Pending-input conversion is still running"))
    (unless (niwa-view--steerable-root-request-p request)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No steerable root turn is active"))
    (unless (buffer-live-p view)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No live owning view"))
    (setq niwa-pending-inputs--converting-id id)
    (niwa-session--set-dropped-file-grants
     session (append staged-grants original-grants))
    (with-current-buffer view
      (condition-case err
          (niwa-view--submit-planned-input
           (niwa-pending-inputs--copy-input entry)
           nil
           (lambda ()
             (funcall restore-grants)
             (when (buffer-live-p cockpit)
               (with-current-buffer cockpit
                 (setq niwa-pending-inputs--converting-id nil)))
             (message "niwa: follow-up remains unchanged"))
           (lambda (submission)
             (unwind-protect
                 (if-let* ((prepared
                            (niwa-view--prepare-steering-entry
                             submission request)))
                     (condition-case conversion-err
                         (let ((replacement
                                (niwa-pending-inputs--restore-entry-metadata
                                 prepared entry)))
                           (setq replacement
                                 (plist-put replacement :category 'steering)
                                 replacement
                                 (plist-put replacement :state 'pending))
                           (niwa-pending-inputs--move-between
                            session 'follow-up 'steering id replacement)
                           (niwa-pending-inputs--conversion-finished
                            cockpit context id))
                       (error
                        (niwa-prompt-submission-restore submission)
                        (when (buffer-live-p cockpit)
                          (with-current-buffer cockpit
                            (setq niwa-pending-inputs--converting-id nil)))
                        (niwa-diagnostic-message (niwa-diagnostic-scope session)
                                 "%s (follow-up remains unchanged)"
                                 (error-message-string conversion-err))))
                   (when (buffer-live-p cockpit)
                     (with-current-buffer cockpit
                       (setq niwa-pending-inputs--converting-id nil))))
               (funcall restore-grants)))
           nil
           ;; External input stays skill-inert through conversion: the
           ;; entry's own flag rides along instead of defaulting to live
           ;; skill planning.
           (plist-get entry :inert-skills))
        (error
         (funcall restore-grants)
         (setq niwa-pending-inputs--converting-id nil)
         (signal (car err) (cdr err)))))))

(defun niwa-pending-inputs-make-follow-up ()
  "Convert steering or requeue a failed follow-up at the follow-up tail."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (session (niwa-pending-inputs--session context))
         (item (niwa-pending-inputs--selected))
         (entry (plist-get item :entry))
         (id (plist-get item :id)))
    (unless (or (eq (plist-get item :category) 'steering)
                (eq (plist-get entry :state) 'failed-turn))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Pending input is already a follow-up"))
    (niwa-session-artifacts-assert-new-mutation-authority session)
    (niwa-pending-inputs--set-queues
     session 'steering (remq entry (niwa-session-pending-steering session))
     'follow-up (append (remq entry (niwa-session-pending-follow-ups session))
                        (list (niwa-pending-inputs--as-follow-up session entry))))
    (niwa-pending-inputs--refresh id)))

(defun niwa-pending-inputs--as-follow-up (session entry)
  "Return pending ENTRY of SESSION as a fresh follow-up with the same id.
Restore its reserved prompt submission, which the follow-up turn prepares
again."
  (when-let* ((submission (plist-get entry :submission)))
    (niwa-prompt-submission-restore submission))
  (append
   (list
    :id (plist-get entry :id)
    :category 'follow-up
    :input (niwa-pending-inputs--copy-input entry)
    :dropped-file-grants
    (copy-sequence (plist-get entry :dropped-file-grants))
    :queued-at-time (float-time)
    :queued-at-goal-id
    (when-let* ((goal (niwa-session-goal session)))
      (niwa-goal-id goal))
    :queued-at-turn
    (or (niwa-session-turn-count session) 0)
    :state 'pending)
   ;; Keep guest attribution, attachment ownership, and skill restrictions
   ;; when retrying retained input.
   (cl-loop for key in niwa-recovery--input-keys
            unless (memq key '(:id :category :input :dropped-file-grants
                               :queued-at-time :queued-at-goal-id
                               :queued-at-turn :state :request-id :blocked))
            when (plist-member entry key)
            append (list key (copy-tree (plist-get entry key))))))

(defun niwa-pending-inputs-requeue-steering (session request-id)
  "Move SESSION's steering left by REQUEST-ID's settled turn to the follow-up head.
An external turn cannot park at a held boundary the way a gptel turn waits, so
input it could not take becomes the next turns, in submission order."
  (when-let* ((entries (cl-remove-if-not
                        (lambda (entry) (equal request-id (plist-get entry :request-id)))
                        (niwa-session-pending-steering session))))
    (niwa-pending-inputs--set-queues
     session 'steering (cl-remove-if (lambda (entry) (memq entry entries))
                                   (niwa-session-pending-steering session))
     'follow-up (append (mapcar (lambda (entry) (niwa-pending-inputs--as-follow-up session entry))
                                entries)
                        (niwa-session-pending-follow-ups session)))
    (when-let* ((cockpit (get-buffer niwa-pending-inputs-buffer-name))
                ((eq session (niwa-cockpit-context-session
                              (buffer-local-value 'niwa-cockpit--context cockpit)))))
      (with-current-buffer cockpit
        (niwa-pending-inputs--refresh (tabulated-list-get-id))))
    entries))

(defun niwa-pending-inputs-mark-delete ()
  "Mark the selected pending input for deletion."
  (interactive)
  (let ((id (plist-get (niwa-pending-inputs--selected) :id)))
    (cl-pushnew id niwa-pending-inputs--marked-ids :test #'equal)
    (tabulated-list-put-tag "D" t)))

(defun niwa-pending-inputs-unmark ()
  "Remove the deletion mark from the selected pending input."
  (interactive)
  (let ((id (plist-get (niwa-pending-inputs--selected) :id)))
    (setq niwa-pending-inputs--marked-ids
          (delete id niwa-pending-inputs--marked-ids))
    (tabulated-list-put-tag " " t)))

(defun niwa-pending-inputs--discard (entries)
  "Release reserved prompt context owned by ENTRIES."
  (dolist (entry entries)
    (when-let* ((submission (plist-get entry :submission)))
      (niwa-prompt-submission-restore submission))))

(defun niwa-pending-inputs-execute-deletions ()
  "Confirm and delete every marked pending input."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (session (niwa-pending-inputs--session context))
         (steering
          (cl-remove-if-not
           (lambda (entry)
             (member (plist-get entry :id)
                     niwa-pending-inputs--marked-ids))
           (niwa-session-pending-steering session)))
         (follow-ups
          (cl-remove-if-not
           (lambda (entry)
             (member (plist-get entry :id)
                     niwa-pending-inputs--marked-ids))
           (niwa-session-pending-follow-ups session))))
    (unless (or steering follow-ups)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No pending input is marked for deletion"))
    (when (yes-or-no-p
           (format "Delete %d steering and %d follow-up pending input%s? "
                   (length steering) (length follow-ups)
                   (if (= 1 (+ (length steering) (length follow-ups)))
                       ""
                     "s")))
      (niwa-session-artifacts-assert-new-mutation-authority session)
      (dolist (entry (append steering follow-ups))
        (niwa-pending-inputs--assert-removable session entry))
      (niwa-pending-inputs--discard (append steering follow-ups))
      (niwa-pending-inputs--set-queues
       session
       'steering (cl-remove-if
                  (lambda (entry) (memq entry steering))
                  (niwa-session-pending-steering session))
       'follow-up (cl-remove-if
                   (lambda (entry) (memq entry follow-ups))
                   (niwa-session-pending-follow-ups session)))
      (setq niwa-pending-inputs--marked-ids nil)
      (niwa-pending-inputs--refresh))))

(defun niwa-pending-inputs-clear ()
  "Confirm and clear all pending input in the owning root session."
  (interactive)
  (let* ((context (niwa-cockpit-current-context))
         (session (niwa-pending-inputs--session context))
         (steering (niwa-session-pending-steering session))
         (follow-ups (niwa-session-pending-follow-ups session))
         (view (niwa-cockpit-context-view-buffer context)))
    (unless (or steering follow-ups)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No pending input"))
    (when (and (buffer-live-p view)
               (buffer-local-value 'niwa-view--pending-input-edit view))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Save or cancel the pending-input edit first"))
    (when (yes-or-no-p
           (format "Clear %d steering and %d follow-up pending input%s? "
                   (length steering) (length follow-ups)
                   (if (= 1 (+ (length steering) (length follow-ups)))
                       ""
                     "s")))
      (niwa-session-artifacts-assert-new-mutation-authority session)
      (dolist (entry (append steering follow-ups))
        (niwa-pending-inputs--assert-removable session entry))
      (niwa-pending-inputs--discard (append steering follow-ups))
      (niwa-pending-inputs--set-queues
       session 'steering nil 'follow-up nil)
      (when (buffer-live-p view)
        (with-current-buffer view
          (niwa-view--interaction-rebuild)))
      (when (derived-mode-p 'niwa-pending-inputs-mode)
        (setq niwa-pending-inputs--marked-ids nil)
        (niwa-cockpit-surface-refresh))
      (message "niwa: cleared pending input"))))

(defun niwa-pending-inputs-resume-after-failure ()
  "Clear failure pause after all failed-turn input is resolved."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (session (niwa-pending-inputs--session context))
         (failed
          (cl-remove-if-not
           (lambda (entry)
             (eq (plist-get entry :state) 'failed-turn))
           (append (niwa-session-pending-steering session)
                   (niwa-session-pending-follow-ups session)))))
    (unless (niwa-session-pending-input-failure-paused session)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Pending input is not paused after a turn failure"))
    (when failed
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Resolve %d failed-turn message%s first"
                  (length failed) (if (= 1 (length failed)) "" "s")))
    (niwa-session-artifacts-assert-new-mutation-authority session)
    (niwa-session-set-pending-input-failure-paused session nil)
    (niwa-recovery-save session)
    (niwa-pending-inputs--refresh)
    (message "niwa: pending-input failure pause cleared")))

(defun niwa-pending-inputs-quit ()
  "Close the cockpit and resume eligible automatic delivery."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (view (niwa-cockpit-context-view-buffer context)))
    (when (and (buffer-live-p view)
               (buffer-local-value 'niwa-view--pending-input-edit view))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Save or cancel the pending-input edit first"))
    (niwa-pending-inputs--resume-delivery context)
    (niwa-cockpit-quit "Pending Inputs cockpit")))

(defun niwa-pending-inputs--resume-delivery (context)
  "Resume eligible automatic delivery for CONTEXT's session.

Clearing the pause is not enough on its own: a steering entry that
matched the live request parked its turn at the pending-input boundary,
and only a transition back to WAIT releases it."
  (let* ((session (niwa-pending-inputs--session context))
         (view (niwa-cockpit-context-view-buffer context))
         (failure-paused
          (niwa-session-pending-input-failure-paused session)))
    (niwa-session-set-pending-input-paused session nil)
    (niwa-recovery-save session)
    (when (buffer-live-p view)
      (with-current-buffer view
        (niwa-view--interaction-rebuild)))
    (unless failure-paused
      (when-let* ((data-buffer
                   (and (buffer-live-p view)
                        (buffer-local-value 'niwa--data-buffer view)))
                  ((buffer-live-p data-buffer))
                  (request
                   (buffer-local-value
                    'niwa--current-request data-buffer))
                  (fsm (and request (niwa-request-fsm request)))
                  ((plist-get (gptel-fsm-info fsm)
                              :niwa-pending-input-hold)))
        (gptel--fsm-transition fsm 'WAIT)))
    ;; A failure latch that holds no queued input no longer holds background
    ;; mail, so offer it; latched input waits for explicit recovery.
    (when (and (buffer-live-p view)
               (or (not failure-paused)
                   (not (or (niwa-session-pending-steering session)
                            (niwa-session-pending-follow-ups session)))))
      (with-current-buffer view
        (niwa-pending-inputs-schedule-held-input-delivery)))))

(defun niwa-pending-inputs--on-kill-buffer ()
  "Resume delivery when the cockpit is killed while it still owns a pause.

The pause is stored on the session but owned by this buffer's lifetime.
A kill cannot refuse the way `niwa-pending-inputs-quit' does, so the
entry edit that would have blocked the quit is cancelled instead: its
text would otherwise sit in the composer, where the drain refuses to
deliver anything, and the resume would be dead on arrival.  Errors are
demoted because a signal here would abort the kill and leave a live
cockpit whose banner and session disagree."
  (let ((diagnostic-scope (niwa-cockpit-diagnostic-scope)))
    (condition-case-unless-debug err
        (when-let* ((context (niwa-cockpit-context-for-buffer (current-buffer)))
                    (session (niwa-cockpit-context-session context))
                    ((niwa-session-pending-input-paused session)))
          (when-let* ((view (niwa-cockpit-context-view-buffer context))
                      ((buffer-live-p view))
                      (edit (buffer-local-value 'niwa-view--pending-input-edit
                                                view)))
            ;; This cockpit is on its way out, so the restored draft returns
            ;; to the view rather than to a buffer about to die.
            (niwa-pending-inputs--return-to-cockpit
             (plist-put (copy-sequence edit) :cockpit-buffer nil)))
          (niwa-pending-inputs--resume-delivery context))
      (error
       (niwa-diagnostic-message diagnostic-scope
        "%s (pending-input teardown failed)" (error-message-string err))))))

(defconst niwa-pending-inputs--surface
  `(:buffer-name ,niwa-pending-inputs-buffer-name
    :label "Pending Inputs cockpit"
    :row-label "pending input"
    :mode niwa-pending-inputs-mode
    :format [("Category" 12 nil)
             ("#" 4 nil)
             ("State" 12 nil)
             ("Preview" 0 nil)]
    :require-session t
    :collect niwa-pending-inputs--collect
    :entry niwa-pending-inputs--entry
    :header niwa-pending-inputs--header
    :details niwa-pending-inputs--details
    :details-buffer "*niwa pending input*"
    :setup niwa-pending-inputs--setup
    :keys (("RET" "Edit selected pending input" niwa-pending-inputs-edit)
           ("e" "Edit selected pending input" niwa-pending-inputs-edit)
           ("M-<up>" "Move up within category" niwa-pending-inputs-move-up)
           ("M-<down>" "Move down within category"
            niwa-pending-inputs-move-down)
           ("s" "Convert to steering" niwa-pending-inputs-make-steering)
           ("f" "Convert or requeue as follow-up" niwa-pending-inputs-make-follow-up)
           ("d" "Mark for deletion" niwa-pending-inputs-mark-delete)
           ("u" "Unmark deletion" niwa-pending-inputs-unmark)
           ("x" "Delete marked pending input"
            niwa-pending-inputs-execute-deletions)
           ("R" "Resume after failure" niwa-pending-inputs-resume-after-failure)
           ("C-c C-q" "Clear all pending input" niwa-pending-inputs-clear)
           ("g" "Refresh live pending input" niwa-pending-inputs-refresh)
           ("q" "Close and resume eligible delivery" niwa-pending-inputs-quit)))
  "Cockpit surface spec for pending input.")

(define-derived-mode niwa-pending-inputs-mode tabulated-list-mode
  "niwa-pending-inputs"
  "Major mode for inspecting and editing pending input."
  (niwa-cockpit-setup-tabulated-surface
   niwa-pending-inputs--surface)
  (add-hook 'kill-buffer-hook #'niwa-pending-inputs--on-kill-buffer nil t))

(defun niwa-pending-inputs-open (&optional context-or-event)
  "Open the Pending Inputs cockpit for CONTEXT-OR-EVENT."
  (interactive
   (list (and (mouse-event-p last-nonmenu-event)
              last-nonmenu-event)))
  (when (mouse-event-p context-or-event)
    (mouse-set-point context-or-event)
    (setq context-or-event nil))
  (let* ((context (or context-or-event
                      (niwa-cockpit-current-context)))
         (session (niwa-pending-inputs--session context)))
    (unless (or (niwa-session-pending-steering session)
                (niwa-session-pending-follow-ups session)
                (niwa-session-pending-input-failure-paused session))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No pending input"))
    (niwa-cockpit-open-surface
     niwa-pending-inputs--surface context)))

(provide 'niwa-pending-inputs)
;;; niwa-pending-inputs.el ends here
