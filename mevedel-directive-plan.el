;;; mevedel-directive-plan.el -- Directive planning workflow -*- lexical-binding: t -*-

;;; Commentary:

;; Durable Plan-before-implementation state and approval handoff for
;; workspace-owned directives.  Planning uses the ordinary directive request
;; path and the shared Plan approval interaction, without entering sticky Plan
;; mode or creating standalone Plan artifacts.

;;; Code:

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `cl-seq'
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-remove-duplicates "cl-seq" (cl-seq &rest cl-keys))

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request")

;; `mevedel-chat'
(declare-function mevedel--directive-action-context
                  "mevedel-chat" (directive workspace))
(declare-function mevedel--directive-model-policy
                  "mevedel-chat" (directive))
(declare-function mevedel--process-directive
                  "mevedel-chat"
                  (directive preset prompt-fn callback &optional options))

;; `mevedel-models'
(declare-function mevedel-model-resolve-provider
                  "mevedel-models" (spec &optional noerror))
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))
(declare-function mevedel-model-validate-effort
                  "mevedel-models" (model effort))

;; `mevedel-overlays'
(declare-function mevedel--directive-record "mevedel-overlays" (directive))
(declare-function mevedel--update-instruction-overlay
                  "mevedel-overlays" (instruction &optional refresh))

;; `mevedel-pending-inputs'
(declare-function mevedel-view--schedule-late-follow-up-drain
                  "mevedel-pending-inputs" ())

;; `mevedel-plan'
(declare-function mevedel-plan-extract-proposed "mevedel-plan" (text))

;; `mevedel-plan-handoff'
(declare-function mevedel-plan-handoff--append-implementation-input
                  "mevedel-plan-handoff" (prompt selection))
(declare-function mevedel-plan-handoff--validate-skill-bindings
                  "mevedel-plan-handoff" (prompt session))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-approval-present
                  "mevedel-plan-mode" (entry &optional session))
(declare-function mevedel-plan-mode--default-selection
                  "mevedel-plan-mode" (session))
(declare-function mevedel-plan-mode--render-approval
                  "mevedel-plan-mode" (entry))

;; `mevedel-presets'
(defvar mevedel-action-preset-alist)

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence" (session buffer &optional settled))

;; `mevedel-skills-invoke'
(declare-function mevedel-skills-prepare-user-input
                  "mevedel-skills-invoke" (text session))

;; `mevedel-structs'
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-plan "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-planning "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-directive-planning
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-follow-ups
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)

;; `mevedel-view-composer'
(declare-function mevedel-view-back-to-chat "mevedel-view-composer" ())
(declare-function mevedel-view-enter-directive-scope
                  "mevedel-view-composer"
                  (directive action &optional attempt-index workspace))

(defun mevedel-directive-plan-put (record &rest kvs)
  "Merge KVS into RECORD's plan plist and return the stored plan."
  (let ((plan (copy-tree (mevedel-directive-plan record))))
    (while kvs
      (setq plan (plist-put plan (pop kvs) (pop kvs))))
    (setf (mevedel-directive-plan record) plan)
    plan))

(defun mevedel-directive-plan--live-p (plan &rest statuses)
  "Return non-nil when PLAN is in STATUSES and neither cancelled nor invalidated."
  (and plan
       (memq (plist-get plan :status) statuses)
       (not (plist-get plan :cancelled))
       (not (plist-get plan :invalidated))))

(defun mevedel-directive-plan--refresh (directive)
  "Refresh DIRECTIVE when it still has a presentation."
  (when (overlay-buffer directive)
    (mevedel--update-instruction-overlay directive t)))

(defun mevedel-directive-plan--restore-chat-scope (plan &optional drain)
  "Restore PLAN's ordinary composer draft and optionally DRAIN queued input."
  (when-let* ((chat-buffer (plist-get plan :chat-buffer))
              ((buffer-live-p chat-buffer))
              (view-buffer (buffer-local-value 'mevedel--view-buffer
                                               chat-buffer))
              ((buffer-live-p view-buffer)))
    (with-current-buffer view-buffer
      (mevedel-view-back-to-chat)
      (when drain
        (require 'mevedel-pending-inputs)
        (mevedel-view--schedule-late-follow-up-drain)))))

(defun mevedel-directive-plan--persist (session chat-buffer)
  "Persist SESSION from CHAT-BUFFER."
  (require 'mevedel-session-persistence)
  (mevedel-session-persistence-save session chat-buffer))

(defun mevedel-directive-plan--planning-prompt
    (implementation-prompt &optional feedback proposal)
  "Return a planning request for IMPLEMENTATION-PROMPT.
FEEDBACK and PROPOSAL describe a requested replacement proposal."
  (concat
   "Create a concrete implementation plan for the directive request below. "
   "Investigate as needed, but do not modify the project. The eventual "
   "implementation receives the original request and your accepted plan."
   (when feedback
     (format "\n\nPlan feedback:\n%s" feedback))
   (when proposal
     (format "\n\nCurrent proposal (reference only; replace it completely):\n%s"
             proposal))
   "\n\nDirective implementation request:\n"
   implementation-prompt))

(defun mevedel-directive-plan--planning-model-policy (directive)
  "Resolve DIRECTIVE's explicit model or the planning workload."
  (require 'mevedel-models)
  (or (mevedel--directive-model-policy directive)
      (mevedel-model-resolve-workload 'planning)))

(defun mevedel-directive-plan--selection (session plan record)
  "Return PLAN's retained selection or defaults from SESSION.
RECORD's directive-selected skills seed a fresh default selection; the
card's selection stays authoritative once retained."
  (or (copy-tree (plist-get plan :selection))
      (let ((selection
             (with-current-buffer (plist-get plan :chat-buffer)
               (mevedel-plan-mode--default-selection session))))
        (when-let* ((skills (mevedel-directive-skills record)))
          (setq selection
                (plist-put selection :skills
                           (cl-remove-duplicates
                            (append (plist-get selection :skills)
                                    (copy-tree skills))
                            :key (lambda (skill)
                                   (plist-get skill :source-file))
                            :test #'equal))))
        selection)))

(defun mevedel-directive-plan--model-policy (selection)
  "Return request-local model policy from approval SELECTION."
  (require 'mevedel-models)
  (let* ((provider (plist-get selection :model-provider))
         (policy (mevedel-model-resolve-provider provider))
         (effort (plist-get selection :reasoning-effort)))
    (mevedel-model-validate-effort (plist-get policy :model) effort)
    (plist-put policy :effort effort)))

(defun mevedel-directive-plan--implementation-prompt (plan selection)
  "Return PLAN's accepted directive prompt with SELECTION additions."
  (require 'mevedel-plan-handoff)
  (or (plist-get plan :accepted-prompt)
      (mevedel-plan-handoff--append-implementation-input
       (format
        "%s\n\n### ACCEPTED DIRECTIVE PLAN:\n\n%s\n\nImplement the directive request according to this accepted plan. Preserve its stated outcomes and acceptance criteria; any implementation instructions below supplement the accepted plan and never replace its outcomes or accepted constraints."
        (plist-get plan :implementation-prompt)
        (plist-get plan :proposal))
       selection)))

(defun mevedel-directive-plan--clear-session (session record)
  "Release SESSION when its active workflow belongs to RECORD."
  (when (equal (plist-get (mevedel-session-directive-planning session)
                          :directive-id)
               (mevedel-directive-id record))
    (setf (mevedel-session-directive-planning session) nil)))

(defun mevedel-directive-plan--implement
    (directive record session selection callback)
  "Implement accepted plan for DIRECTIVE and RECORD in SESSION."
  (let* ((plan (copy-tree (mevedel-directive-plan record)))
         (action (plist-get plan :action))
         (execution-action (if (eq action 'implement-this)
                               'implement
                             action))
         (implementation-input
          (mevedel-directive-plan--implementation-prompt plan selection)))
    (with-current-buffer (plist-get plan :chat-buffer)
      (require 'mevedel-skills-invoke)
      (setq implementation-input
            (mevedel-skills-prepare-user-input implementation-input session))
      (mevedel-plan-handoff--validate-skill-bindings
       implementation-input session))
    (plist-put plan :status 'accepted)
    (plist-put plan :selection (copy-tree selection))
    (plist-put plan :accepted-prompt implementation-input)
    (setf (mevedel-directive-plan record) plan)
    (mevedel-directive-plan--refresh directive)
    (overlay-put directive 'mevedel-directive-action execution-action)
    (setf (mevedel-session-directive-planning session)
          (list :directive-id (mevedel-directive-id record)
                :action action :phase 'implementation))
    (mevedel-directive-plan--persist
     session (plist-get plan :chat-buffer))
    ;; Mark implementing before dispatching: the settle callback may
    ;; write :status 'settled, which a post-dispatch write from this
    ;; stale local would clobber.
    (mevedel-directive-plan-put record :status 'implementing)
    (mevedel-directive-plan--refresh directive)
    (condition-case err
        (mevedel--process-directive
         directive (alist-get 'implement mevedel-action-preset-alist)
         (lambda (_content) implementation-input)
         (lambda (request-error terminal-fsm)
           (mevedel-directive-plan-put record :status 'settled)
           (mevedel-directive-plan--clear-session session record)
           (mevedel-directive-plan--refresh directive)
           (when callback
             (funcall callback request-error terminal-fsm)))
         (list :permission-mode (plist-get selection :mode)
               :model-policy
               (mevedel-directive-plan--model-policy selection)
               :plan (plist-get plan :proposal)
               :plan-selection selection))
      (error
       (setf (mevedel-session-directive-planning session)
             (list :directive-id (mevedel-directive-id record)
                   :action action :phase 'approval))
       (plist-put plan :status 'accepted)
       (setf (mevedel-directive-plan record) plan)
       (mevedel-directive-plan--refresh directive)
       (signal (car err) (cdr err))))))

(defun mevedel-directive-plan--approval-outcome
    (directive record session callback outcome)
  "Handle directive approval OUTCOME for DIRECTIVE and RECORD."
  (let ((plan (copy-tree (mevedel-directive-plan record))))
    (cond
     ((and (proper-list-p outcome) (plist-get outcome :accept))
      (mevedel-directive-plan--restore-chat-scope plan)
      (mevedel-directive-plan--implement
       directive record session (plist-get outcome :selection) callback))
     ((eq outcome 'feedback-draft)
      (mevedel-directive-plan-put record :status 'draft)
      (mevedel-directive-plan--persist
       session (plist-get plan :chat-buffer))
      (mevedel-directive-plan--refresh directive)
      (mevedel-view-enter-directive-scope directive 'plan))
     ((eq outcome 'aborted)
      (mevedel-directive-plan-put record :status 'draft :cancelled t)
      (mevedel-directive-plan--clear-session session record)
      (mevedel-directive-plan--persist
       session (plist-get plan :chat-buffer))
      (mevedel-directive-plan--refresh directive)
      (mevedel-directive-plan--restore-chat-scope plan t)
      (when callback (funcall callback nil nil)))
     ((eq outcome 'invalidated)
      (mevedel-directive-plan--clear-session session record)
      (mevedel-directive-plan--persist
       session (plist-get plan :chat-buffer))
      (mevedel-directive-plan--refresh directive)
      (mevedel-directive-plan--restore-chat-scope plan t)
      ;; Settle the caller so a running batch pauses with a report
      ;; instead of hanging on a proposal invalidated by a request edit.
      (when callback
        (funcall callback "directive plan invalidated by a request edit"
                 nil)))
     ((eq outcome 'superseded) nil)
     (t (message "mevedel: unknown directive Plan outcome %S" outcome)))))

(defun mevedel-directive-plan--present
    (directive record session chat-buffer callback)
  "Present RECORD's proposed plan for DIRECTIVE in CHAT-BUFFER."
  (let* ((plan (mevedel-directive-plan record))
         (selection (mevedel-directive-plan--selection session plan record))
         (entry
          (list :body (plist-get plan :proposal)
                :chat-buffer chat-buffer
                :origin "/root"
                :session session
                :selection selection
                :directive t
                :directive-id (mevedel-directive-id record)
                :interaction-id (list :directive-plan (gensym "plan-"))
                :renderer #'mevedel-plan-mode--render-approval)))
    (plist-put
     entry :selection-changed
     (lambda (changed)
       (mevedel-directive-plan-put record :selection (copy-tree changed))
       (mevedel-directive-plan--persist session chat-buffer)))
    (plist-put
     entry :callback
     (lambda (outcome)
       (mevedel-directive-plan--approval-outcome
        directive record session callback outcome)))
    (plist-put plan :selection selection)
    (setf (mevedel-directive-plan record) plan)
    (mevedel-plan-approval-present entry session)))

(defun mevedel-directive-plan--settle-planning
    (directive record callback err fsm)
  "Settle DIRECTIVE planning request for RECORD after ERR and FSM."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (chat-buffer (and info (plist-get info :buffer)))
         (session (and (buffer-live-p chat-buffer)
                       (buffer-local-value 'mevedel--session chat-buffer)))
         (turn (car (last (mevedel-directive-planning record))))
         (proposal (and (not err) turn
                        (mevedel-plan-extract-proposed
                         (plist-get turn :result))))
         (plan (copy-tree (mevedel-directive-plan record))))
    (when turn
      (plist-put turn :proposal proposal))
    (when session
      (setf (mevedel-session-directive-planning session)
            (list :directive-id (mevedel-directive-id record)
                  :action (plist-get plan :action) :phase 'approval
                  :callback callback)))
    (if (and proposal session)
        (let ((status
               (if (cl-some
                    (lambda (queued)
                      (eq (plist-get (plist-get queued :scope) :action)
                          'plan))
                    (mevedel-session-pending-follow-ups session))
                   'draft
                 'proposed)))
          (mevedel-directive-plan-put
           record :status status :proposal proposal :chat-buffer chat-buffer)
          (mevedel-directive-plan--refresh directive)
          (when (eq status 'proposed)
            (mevedel-directive-plan--present
             directive record session chat-buffer callback)))
      ;; No presentable outcome: request error, a successful turn
      ;; without a proposed plan, or a dead chat buffer.  Settle the
      ;; callback so batch processing pauses instead of stalling.
      (apply #'mevedel-directive-plan-put
             record :status 'draft
             (and proposal (list :proposal proposal)))
      (mevedel-directive-plan--refresh directive)
      (when callback
        (funcall callback
                 (or err
                     (if proposal
                         "planning chat buffer is no longer live"
                       "planning turn produced no proposed plan"))
                 fsm)))))

(defun mevedel-directive-plan-start (directive action prompt-fn callback)
  "Plan DIRECTIVE ACTION before running PROMPT-FN.
CALLBACK runs after implementation settles."
  (let* ((record (mevedel--directive-record directive))
         (prior (mevedel-directive-plan record))
         implementation-prompt)
    (setf (mevedel-directive-plan record)
          (list :status 'planning :action action))
    (overlay-put directive 'mevedel-directive-action 'plan)
    (condition-case err
        (mevedel--process-directive
         directive (alist-get 'discuss mevedel-action-preset-alist)
         (lambda (content)
           (setq implementation-prompt (funcall prompt-fn content))
           (mevedel-directive-plan-put
            record :implementation-prompt implementation-prompt)
           (mevedel-directive-plan--planning-prompt implementation-prompt))
         (lambda (err fsm)
           (mevedel-directive-plan--settle-planning
            directive record callback err fsm))
         (list :planned-action action
               :model-policy
               (mevedel-directive-plan--planning-model-policy directive)))
      (error
       (setf (mevedel-directive-plan record) prior)
       (mevedel-directive-plan--refresh directive)
       (signal (car err) (cdr err))))))

(defun mevedel-directive-plan-continue (directive feedback)
  "Continue DIRECTIVE planning with FEEDBACK."
  (let* ((record (mevedel--directive-record directive))
         (plan (copy-tree (mevedel-directive-plan record)))
         (chat-buffer (plist-get plan :chat-buffer))
         (session (and (buffer-live-p chat-buffer)
                       (buffer-local-value 'mevedel--session chat-buffer)))
         (callback
          (plist-get (and session
                          (mevedel-session-directive-planning session))
                     :callback)))
    (unless (mevedel-directive-plan--live-p plan 'draft 'proposed)
      (user-error "Directive has no planning conversation to continue"))
    (mevedel-directive-plan-put record :status 'planning)
    (overlay-put directive 'mevedel-directive-action 'plan)
    (mevedel--process-directive
     directive (alist-get 'discuss mevedel-action-preset-alist)
     (lambda (_content)
       (mevedel-directive-plan--planning-prompt
        (plist-get plan :implementation-prompt)
        feedback (plist-get plan :proposal)))
     (lambda (err fsm)
       (mevedel-directive-plan--settle-planning
        directive record callback err fsm))
     (list :planned-action (plist-get plan :action) :message feedback
           :plan-continuation t
           :model-policy
           (mevedel-directive-plan--planning-model-policy directive)))))

(defun mevedel-directive-plan-restore-pending (session chat-buffer)
  "Restore SESSION's directive planning workflow in CHAT-BUFFER."
  (unless (mevedel-session-plan-mode session)
    (when-let* ((record
                 (cl-find-if
                  (lambda (candidate)
                    (and (equal (mevedel-directive-session-id candidate)
                                (mevedel-session-session-id session))
                         (mevedel-directive-plan--live-p
                          (mevedel-directive-plan candidate)
                          'planning 'draft 'proposed 'accepted)))
                  (mevedel-workspace-directives
                   (mevedel-session-workspace session))))
                (context
                 (mevedel--directive-action-context
                  record (mevedel-session-workspace session)))
                (directive (plist-get context :directive)))
      (let ((plan (mevedel-directive-plan record)))
        (when (eq (plist-get plan :status) 'planning)
          (setq plan (mevedel-directive-plan-put record :status 'draft)))
        (setf (mevedel-session-directive-planning session)
              (list :directive-id (mevedel-directive-id record)
                    :action (plist-get plan :action)
                    :phase 'approval))
        (when (memq (plist-get plan :status) '(proposed accepted))
          (mevedel-directive-plan-put record :chat-buffer chat-buffer)
          (mevedel-directive-plan--present
           directive record session chat-buffer nil))
        t))))

(provide 'mevedel-directive-plan)
;;; mevedel-directive-plan.el ends here
