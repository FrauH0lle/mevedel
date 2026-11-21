;;; niwa-claude-code-session.el --- Subscription session dispatch -*- lexical-binding: t -*-

;;; Commentary:
;; Connects normal session selection to the request-owned ACP runner.  Root
;; native history is resumed, including in note sessions; each shared-item turn
;; starts an isolated native conversation from its selected prompt.  The local
;; installed agent owns those histories; session metadata stores references,
;; never credentials or a replay queue for tool effects.

;;; Code:

(require 'niwa-acp-turn)
(require 'niwa-claude-code)
(require 'niwa-claude-code-context)
(require 'niwa-claude-code-history)
(require 'niwa-context-delivery)
(require 'niwa-implementation-policy)
(require 'niwa-presets)
(require 'niwa-reminders)
(require 'niwa-session-artifacts)
(require 'niwa-shared-conversation)
(require 'niwa-skills-core)
(require 'niwa-tool-render-data)
(require 'niwa-tools)
(require 'niwa-transcript)
(require 'niwa-turn)

;; `niwa-note-conversation'
(declare-function niwa-note-conversation-record-input
                  "niwa-note-conversation" (session data))
(autoload 'niwa-note-conversation-record-input "niwa-note-conversation")

;; `niwa-skills-invoke'
(declare-function niwa-skills-request-model-policy "niwa-skills-invoke" ())
(autoload 'niwa-skills-request-model-policy "niwa-skills-invoke")

;; `niwa-structs'
(defun niwa-claude-code--forget-instructions (session owner)
  "Forget SESSION's path-instruction acknowledgements for conversation OWNER."
  (setf (niwa-session-workspace-instruction-hashes session)
        (mapcar (lambda (entry)
                  (cons (car entry) (unless (equal owner (caar entry)) (cdr entry))))
                (niwa-session-workspace-instruction-hashes session))))

(defun niwa-claude-code-reset-root-delivery (session)
  "Forget root context acknowledgements when SESSION changes engines.
Child conversations keep their independent path-instruction acknowledgements."
  (niwa-claude-code--forget-instructions session "/root")
  (setf (niwa-session-mentions-shown session) (make-hash-table :test #'equal))
  (niwa-reminders-rearm-plan-reference session))

(defun niwa-claude-code-release-history (session &optional scope at-turn)
  "Detach SESSION's native history in SCOPE for transcript continuation.
SCOPE defaults to root; a child keeps an unstarted marker for excerpt recovery.
Persist detachment before dispatch so returning to Claude cannot resume an
external conversation missing intervening gptel turns.  Selection alone does
not call this operation.  Edited histories discard prior native coordinates.
AT-TURN defaults to the active or next admitted turn; explicit recovery can
detach immediately at the committed turn instead."
  (setq scope (or scope "root"))
  (when (assoc scope (niwa-session-external-conversations session))
    (let* ((previous (niwa-session-external-conversations session))
           (hashes (niwa-session-workspace-instruction-hashes session))
           (mentions (niwa-session-mentions-shown session))
           (reminder (cl-find 'plan-reference (niwa-session-reminders session)
                              :key #'niwa-reminder-type))
           (fired (and reminder (niwa-reminder-last-fired reminder))))
      (when-let* ((history (alist-get scope previous nil nil #'equal))
                  ((plist-get history :id)))
        (let* ((turn (or at-turn (niwa-current-turn session)))
               (boundaries (unless (eq 'diverged (plist-get history :state))
                             (cl-remove turn (copy-tree (plist-get history :boundaries))
                                        :key (lambda (entry) (plist-get entry :turn))))))
          (setf (niwa-session-external-conversations session)
                (cons (list scope :engine 'claude-code :state 'unstarted
                            :boundaries (append boundaries (list (list :turn turn :detached t))))
                      (cl-remove scope previous :key #'car :test #'equal)))))
      (if (equal scope "root")
          (niwa-claude-code-reset-root-delivery session)
        (niwa-claude-code--forget-instructions session scope))
      (condition-case err
          (niwa-session-artifacts-save session (current-buffer))
        (error
         (setf (niwa-session-external-conversations session) previous
               (niwa-session-workspace-instruction-hashes session) hashes
               (niwa-session-mentions-shown session) mentions)
         (when reminder (setf (niwa-reminder-last-fired reminder) fired))
         (signal (car err) (cdr err)))))
    (message "niwa: %s will continue from its retained transcript on the next send" scope)))

;;;###autoload
(defun niwa-claude-code-recover-history (&optional scope)
  "Recover SCOPE from its retained transcript on the next send.
SCOPE is root (the default) or a retained child's canonical path.
Detach the native reference without deleting the transcript or replaying
effects.
The next Claude turn starts a labelled excerpt continuation, rather than
an exact resume.
This command does not start a model request or resume a paused Goal."
  (interactive
   (list (completing-read "Recover Claude conversation: "
                          (and (bound-and-true-p niwa--session)
                               (cl-loop for (scope . _) in (niwa-session-external-conversations niwa--session)
                                        when (or (equal scope "root")
                                                 (assoc scope (niwa-session-agent-registry niwa--session)))
                                        collect scope))
                          nil t nil nil "root")))
  (setq scope (or scope "root"))
  (let* ((session (bound-and-true-p niwa--session))
         (buffer (and session (niwa-session-root-buffer session))))
    (unless (buffer-live-p buffer) (user-error "No live niwa session here"))
    (with-current-buffer buffer
      (when (or buffer-read-only (bound-and-true-p niwa-session--read-only-mode))
        (user-error "This session is open read-only"))
      (niwa-session-artifacts-assert-new-mutation-authority session)
      (when (or (niwa-turn-busy-p buffer)
                (niwa-agent-control-active-turn-p session))
        (user-error "Wait for or interrupt running turns before recovering Claude history"))
      (when-let* ((goal (niwa-session-goal session))
                  ((eq 'active (niwa-goal-status goal))))
        (user-error "Pause the Goal before recovering Claude history"))
      (unless (and (assoc scope (niwa-session-external-conversations session))
                   (or (equal scope "root")
                       (assoc scope (niwa-session-agent-registry session))))
        (user-error "No retained Claude conversation at %s" scope))
      (niwa-claude-code-release-history session scope (or (niwa-session-turn-count session) 0))
      (when (niwa-recovery-clear session "request")
        (niwa-recovery-save session)))))

;;;###autoload
(defun niwa-claude-code-send (&optional model-input model-policy prepared-input trigger)
  "Send MODEL-INPUT through the effective Claude subscription MODEL-POLICY.
When input is omitted, read the latest submitted canonical transcript prompt.
An omitted policy resolves through the common request model resolver.
PREPARED-INPUT reuses accepted ACP content, including the initial excerpt and
original image bytes, for an explicit retry from its pre-failure boundary.
TRIGGER names the initiator of an automatic root turn."
  (let* ((niwa-implementation-policy--admission-preparation
          (niwa-implementation-policy-capture-preparation))
         (model-policy (or model-policy (niwa-skills-request-model-policy)))
         (backend (plist-get model-policy :backend))
         (model-id (plist-get model-policy :model))
         (model (gptel--model-name model-id))
         (effort (plist-get model-policy :effort))
         (session (or (and niwa--session
                           (niwa-claude-code-backend-p backend)
                           niwa--session)
                      (user-error "Select Claude Code in a niwa session first")))
         (buffer (current-buffer))
         ;; A shared-item question keeps its own context, apart from the room.
         (shared (niwa-shared-conversation-current))
         ;; Item prompts carry their precisely selected durable
         ;; history.  Resuming hidden history would reintroduce excluded turns,
         ;; and their turns must not enter the root's native history.
         (isolated shared)
         (history (and (not isolated)
                       (alist-get "root" (niwa-session-external-conversations session)
                                  nil nil #'equal)))
         (excerpt (progn
                    (niwa-claude-code-history-assert-current history)
                    (and (not isolated)
                         (not prepared-input)
                         (not (plist-get history :id))
                         (niwa-engine-root-history-p session)
                         (niwa-claude-code-history-excerpt session))))
         (transcript-input (buffer-substring-no-properties
                            (niwa-transcript-prompt-transform-start) (point-max)))
         (prompt (concat
                  (when shared (niwa-shared-conversation-prefix session shared t))
                  (niwa-tool-render-data-strip-non-media
                   (or model-input
                       (buffer-substring
                        (niwa-transcript-prompt-transform-start) (point-max))))))
         (request (or niwa--current-request
                      (niwa-request-begin session)))
         (system-prompt gptel-system-prompt)
         tools record selected)
    (condition-case err
        (progn
          (unless (and (eq session (niwa-request-session request))
                       (eq request niwa--current-request))
            (error "Subscription send lost its admitted request"))
          (when excerpt
            (niwa-claude-code-reset-root-delivery session)
            (message "niwa: starting Claude with an excerpt continuation"))
          (setf (niwa-engine-info request)
                (append (niwa-engine-info request)
                        (list :context (if (listp trigger) trigger (list :turn-trigger trigger))
                              :backend backend :model model-id :reasoning-effort effort
                              :niwa-native-before-history (copy-tree history)
                              :external-history t :niwa-native-isolated isolated)))
          ;; UpdateGoal visibility depends on the request's Goal attribution.
          (niwa-goal-capture-request request)
          ;; Resolve the selected preset's current tools, extras and local
          ;; Agent schema before freezing the external turn's roster.
          ;; A request-only preset has already installed its own scope.
          (unless niwa-preset--temporary-p
            (when-let* ((preset gptel--preset)
                        ((assq preset niwa-preset--registry)))
              (niwa-preset--refresh-tools preset)
              (niwa-preset--setup-scope preset)))
          (setq tools (niwa-tools-native-roster request))
          ;; Preparation can settle before the agent returns an identity.  Record
          ;; that startup state without inventing a resumable native history.
          (unless (or history isolated)
            (setf (alist-get "root" (niwa-session-external-conversations session)
                             nil nil #'equal)
                  (list :engine 'claude-code :state 'unstarted))))
      (error
       ;; ACP has not installed its terminal owner yet.  Admission still owns
       ;; the failed turn, including its reservation and busy/sleep state.
       (when (eq request niwa--current-request)
         (setf (niwa-engine-info request)
               (plist-put (niwa-engine-info request) :error
                          (error-message-string err)))
         (niwa-preset--settle-terminal request nil 'error))
       (signal (car err) (cdr err))))
    (niwa-acp-turn-start
     request
     (lambda (mcp hook-command)
       (niwa-claude-code-launch
        (with-current-buffer buffer
          (niwa-claude-code-context-system request system-prompt selected (plist-get history :id)))
        mcp model effort (plist-get history :id) hook-command))
     (lambda ()
       (let* ((user-input (vconcat (when excerpt (vector `((type . "text") (text . ,excerpt))))
                                   (or prepared-input (niwa-claude-code-context-input request prompt))))
              (input (niwa-claude-code-context-prompt
                      request user-input history)))
         (setf (niwa-engine-info request)
               (append (niwa-engine-info request)
                       (list :niwa-retry-prompt (copy-sequence prompt)
                             :niwa-retry-transcript-input transcript-input
                             :niwa-retry-input (copy-tree user-input t)
                             :niwa-retry-model-policy (copy-tree model-policy))))
         (when (niwa-session-note-conversation session)
           (niwa-note-conversation-record-input
            session (list :system system-prompt :input input)))
         input))
     tools
     (lambda (id)
       (niwa-claude-code-history-assert-current history)
       ;; Each item turn starts an isolated native conversation
       ;; that is never resumed, so its identity needs no durable reference.
       (unless isolated
         ;; A submitted prompt belongs to native history even if interruption
         ;; precedes all output.  Exclude mutable leading Org metadata offsets.
         (setq record (niwa-claude-code-history-open
                       request id
                       (cons (niwa-session-current-segment session)
                             (- (marker-position (plist-get (niwa-engine-info request) :position))
                                (niwa-session-artifacts-content-start buffer)))
                       history))
         (setf (alist-get "root" (niwa-session-external-conversations session)
                          nil nil #'equal) record)
         ;; A crash after dispatch leaves an explicit uncertain turn.  The
         ;; retained ID is durable before any model-triggered effect can occur.
         (niwa-session-artifacts-save session buffer)
         ;; Publishing the first metadata drawer at an empty buffer's start
         ;; leaves a non-advancing submission marker before that drawer.
         (let ((position (plist-get (niwa-engine-info request) :position)))
           (when (< position (niwa-session-artifacts-content-start buffer))
             (set-marker position (niwa-session-artifacts-content-start buffer))))))
     (lambda (outcome)
       (niwa-claude-code-history-settle request history record outcome))
     nil
     (lambda (done)
       (niwa-preset-prepare-request request)
       (when (functionp system-prompt)
         (setq system-prompt (funcall system-prompt)))
       (niwa-claude-code-context-selected
        (lambda (value)
          (setq selected value)
          ;; Edited-file diffs are helper processes, as on gptel's dispatch;
          ;; prepare them before the prompt collects configured reminders.
          (when-let* ((cancel (niwa-reminders-retain-edited-files
                               request done)))
            (niwa-request-push-canceller request cancel))))))
    request))

(provide 'niwa-claude-code-session)
;;; niwa-claude-code-session.el ends here
