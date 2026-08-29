;;; mevedel-pending-inputs.el --- Pending Inputs cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Session-owned queueing, steering, automatic follow-up delivery, inspection,
;; and composer editing.  Opening the cockpit pauses automatic delivery, and
;; closing or killing it resumes delivery and releases a turn parked at the
;; pending-input boundary; queue edits retain the selected entry until a
;; validated replacement is ready.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

;; The follow-up drain shares the session durability transaction used by
;; composer dispatch, so the macro must expand for interpreted loads too.
(require 'mevedel-cockpit)
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
(declare-function mevedel-cockpit-surface-context
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-refresh
                  "mevedel-cockpit" (&optional selected-id))
(declare-function mevedel-cockpit-surface-selected
                  "mevedel-cockpit" (&optional no-error))

;; `mevedel-compact-run'
(defvar mevedel-compact-run-in-flight)

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-copy-text
                  "mevedel-mention-bindings" (text))
(autoload 'mevedel-mention-bindings-copy-text "mevedel-mention-bindings")

;; `mevedel-mentions'
(declare-function mevedel-mentions-expand-user-input
                  "mevedel-mentions" (text session))
(declare-function mevedel-mentions-file-token "mevedel-mentions" (path))
(autoload 'mevedel-mentions-expand-user-input "mevedel-mentions")
(autoload 'mevedel-mentions-file-token "mevedel-mentions")

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
(declare-function mevedel-session-set-pending-input-failure-paused
                  "mevedel-structs" (session paused))
(declare-function mevedel-session-set-pending-input-paused
                  "mevedel-structs" (session paused))
(declare-function mevedel-session-set-pending-inputs
                  "mevedel-structs" (session category entries))
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
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

;; `mevedel-utilities'
(declare-function mevedel--normalize-message-text
                  "mevedel-utilities" (text))
(autoload 'mevedel--normalize-message-text "mevedel-utilities")

;; `mevedel-view'
(defvar mevedel-view--side-conversation-p)

;; `mevedel-view-agent'
(defvar mevedel-view--agent-transcript-p)

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
                         inert-skills))
(declare-function mevedel-view-composer-scope-label
                  "mevedel-view-composer" (&optional scope))
(declare-function mevedel-view-refresh-input-prompt
                  "mevedel-view-composer" ())
(declare-function mevedel-view-send "mevedel-view-composer" ())
(defvar mevedel-view--composer-scope)
(defvar mevedel-view--pending-skill-submission)
(defvar mevedel-view--prompt-hook-pending)

;; `mevedel-view-history'
(declare-function mevedel-view-history-add "mevedel-view-history" (input))

;; `mevedel-view-input-files'
(declare-function mevedel-view--activate-dropped-file-grants
                  "mevedel-view-input-files" (paths session))
(declare-function mevedel-view--mentioned-file-paths
                  "mevedel-view-input-files" (input))
(declare-function mevedel-view--pop-dropped-file-grants-for-input
                  "mevedel-view-input-files" (input session))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" ())
(declare-function mevedel-view--interaction-register
                  "mevedel-view-interaction" (descriptor))

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

(defvar mevedel-view--pending-guest-attribution nil
  "Guest name owning the follow-up currently being submitted, or nil.

Set by the follow-up drain when it dispatches a collaboration guest's
entry and consumed exactly once where the prompt and its hook audits are
inserted, so attribution lands inside the user turn before the response
marker exists regardless of asynchronous submit hooks.  Blocked or
failed submissions clear it so the next host prompt is never
mis-attributed.")

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

(defun mevedel-view--set-pending-follow-ups (entries &optional session)
  "Set SESSION's pending follow-up ENTRIES."
  (when-let* ((sess (or session (mevedel-view--session))))
    (mevedel-session-set-pending-inputs sess 'follow-up entries)))

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

(cl-defun mevedel-view-enqueue-external-follow-up
    (data-buffer text &key guest-name paths directive-id)
  "Queue TEXT as a follow-up that originated outside this Emacs.

DATA-BUFFER owns the session.  GUEST-NAME attributes the entry to a
collaboration guest.  PATHS are files to mention as @file tokens with
read grants, like an Emacs-side drop.  DIRECTIVE-ID, when given, scopes
the entry to that directive's discussion; the caller has already checked
that the directive exists.  Skill tokens in TEXT stay literal at
submission: external input carries prompting authority only, never skill
invocation.  Return the queued entry, or nil without a live session
view."
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
          (mevedel-view--interaction-rebuild)
          (mevedel-view--schedule-late-follow-up-drain)
          entry)))))

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
            (mevedel-mentions-expand-user-input text session)))
      (mevedel-session--set-active-dropped-file-grants session active))))

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
      (when (mevedel-agent-control-root-waiting-p session)
        (mevedel-agent-control-wake-root-user session))
      (message "mevedel: queued steering for this turn")
      entry)))

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

(defun mevedel-view--drain-follow-up (data-buffer)
  "Submit the next pending follow-up for DATA-BUFFER.

Each bound entry is planned and prepared as its own turn.  The queue entry is
removed only when the resulting prompt reaches its transcript commit boundary."
  (when (buffer-live-p data-buffer)
    (mevedel-session-durability-with-transaction
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
             (mevedel-session-artifacts-assert-new-mutation-authority
              session)
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
                 (let* ((active-grants-before
                         (copy-sequence
                          (mevedel-session-active-dropped-file-grants
                           session)))
                        (delivered nil)
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
                           (unless delivered
                             (mevedel-session--set-active-dropped-file-grants
                              session active-grants-before))
                           (setq mevedel-view--pending-guest-attribution nil)
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
                           (mevedel-view--set-pending-follow-ups
                            (delq entry
                                  (mevedel-view--pending-follow-ups session))
                            session)
                           (mevedel-view--interaction-rebuild))))
                   ;; Consumed where the prompt and its hook audits are
                   ;; inserted; cleared on every blocked or failed path.
                   (setq mevedel-view--pending-guest-attribution
                         (plist-get entry :guest-name))
                   ;; A blocked dispatch reports through its own callback,
                   ;; but preparation and the submit hook re-signal instead,
                   ;; so both exits have to release what `before-send' took.
                   (condition-case err
                       (cond
                        (scope
                         (condition-case scope-err
                             (progn
                               (funcall before-send)
                               (mevedel-view--dispatch-directive-input
                                scope input)
                               (funcall after-insert))
                           (error
                            (funcall release)
                            (message
                             "mevedel: queued directive follow-up failed: %s"
                             (error-message-string scope-err)))))
                        (submission
                         (mevedel-view--dispatch-prepared-outcome
                          submission data-buffer
                          :before-send before-send
                          :after-insert after-insert
                          :on-block release))
                        (t
                         (mevedel-view--submit-planned-input
                          input before-send release
                          nil after-insert
                          (plist-get entry :inert-skills))))
                     ((error quit)
                      (funcall release)
                      (signal (car err) (cdr err))))))))))))))

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
  "Return full text for pending-input ITEM."
  (mevedel-view--pending-input-text (plist-get item :entry)))

(defun mevedel-pending-inputs--setup (context)
  "Pause delivery before rendering the cockpit for CONTEXT."
  (let ((session (mevedel-pending-inputs--session context))
        (view (mevedel-cockpit-context-view-buffer context)))
    (mevedel-session-set-pending-input-paused session t)
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
      (mevedel-session-set-pending-inputs session category updated))
    replacement))

(defun mevedel-pending-inputs--move-between
    (session source target id replacement)
  "Move ID from SESSION SOURCE to TARGET tail as REPLACEMENT."
  (let ((source-entries (mevedel-session-pending-inputs session source)))
    (unless (cl-find id source-entries
                     :key (lambda (entry) (plist-get entry :id))
                     :test #'equal)
      (user-error "Pending input is no longer queued"))
    (mevedel-session-set-pending-inputs
     session source
     (cl-remove id source-entries
                :key (lambda (entry) (plist-get entry :id))
                :test #'equal))
    (mevedel-session-set-pending-inputs
     session target
     (append (mevedel-session-pending-inputs session target)
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
         (grants
          (mevedel-view--pop-dropped-file-grants-for-input input session))
         (replacement (plist-put original :input input)))
    (setq replacement
          (plist-put replacement :dropped-file-grants grants)
          replacement (plist-put replacement :submission nil))
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
    (mevedel-session-set-pending-inputs session category entries)
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
         (fsm (and request (mevedel-request-fsm request)))
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
    (when mevedel-pending-inputs--converting-id
      (user-error "Pending-input conversion is still running"))
    (unless (and fsm
                 (not (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT))))
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
               (funcall restore-grants))))
        (error
         (funcall restore-grants)
         (setq mevedel-pending-inputs--converting-id nil)
         (signal (car err) (cdr err)))))))

(defun mevedel-pending-inputs-make-follow-up ()
  "Convert the selected steering entry to a follow-up at the target tail."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (item (mevedel-pending-inputs--selected))
         (entry (plist-get item :entry))
         (id (plist-get item :id)))
    (unless (eq (plist-get item :category) 'steering)
      (user-error "Pending input is already a follow-up"))
    (mevedel-session-artifacts-assert-new-mutation-authority session)
    (when-let* ((submission (plist-get entry :submission)))
      (mevedel-prompt-submission-restore submission))
    (let ((replacement
           (list
            :id id
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
            :state 'pending)))
      (mevedel-pending-inputs--move-between
       session 'steering 'follow-up id replacement)
      (mevedel-pending-inputs--refresh id))))

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
      (mevedel-pending-inputs--discard (append steering follow-ups))
      (mevedel-session-set-pending-inputs
       session 'steering
       (cl-remove-if
        (lambda (entry) (memq entry steering))
        (mevedel-session-pending-steering session)))
      (mevedel-session-set-pending-inputs
       session 'follow-up
       (cl-remove-if
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
      (mevedel-pending-inputs--discard (append steering follow-ups))
      (mevedel-session-set-pending-inputs session 'steering nil)
      (mevedel-session-set-pending-inputs session 'follow-up nil)
      (when (buffer-live-p view)
        (with-current-buffer view
          (mevedel-view--interaction-rebuild)))
      (when (derived-mode-p 'mevedel-pending-inputs-mode)
        (setq mevedel-pending-inputs--marked-ids nil)
        (mevedel-cockpit-surface-refresh))
      (message "mevedel: cleared pending input"))))

(defun mevedel-pending-inputs-resume-after-failure ()
  "Clear failure pause after all failed-turn steering is resolved."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (failed
          (cl-remove-if-not
           (lambda (entry)
             (eq (plist-get entry :state) 'failed-turn))
           (mevedel-session-pending-steering session))))
    (unless (mevedel-session-pending-input-failure-paused session)
      (user-error "Pending input is not paused after a turn failure"))
    (when failed
      (user-error "Resolve %d failed-turn steering message%s first"
                  (length failed) (if (= 1 (length failed)) "" "s")))
    (mevedel-session-artifacts-assert-new-mutation-authority session)
    (mevedel-session-set-pending-input-failure-paused session nil)
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
        (gptel--fsm-transition fsm 'WAIT))
      (when (buffer-live-p view)
        (with-current-buffer view
          (mevedel-view--schedule-late-follow-up-drain))))))

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
           ("f" "Convert to follow-up" mevedel-pending-inputs-make-follow-up)
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
