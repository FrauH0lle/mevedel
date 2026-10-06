;;; mevedel-claude-code-session.el --- Subscription session dispatch -*- lexical-binding: t -*-

;;; Commentary:
;; Connects normal session selection to the request-owned ACP runner.  Root
;; and directive conversations retain independent native histories.  The local
;; installed agent owns those histories; session metadata stores references,
;; never credentials or a replay queue for tool effects.

;;; Code:

(require 'mevedel-acp-turn)
(require 'mevedel-claude-code)
(require 'mevedel-claude-code-context)
(require 'mevedel-claude-code-history)
(require 'mevedel-context-delivery)
(require 'mevedel-session-artifacts)
(require 'mevedel-skills-core)
(require 'mevedel-tool-render-data)
(require 'mevedel-tools)
(require 'mevedel-transcript)

;; `gptel'
(defvar gptel-reasoning-effort)

;; `mevedel-structs'
(defvar mevedel--current-directive-uuid)

(defun mevedel-claude-code-reset-root-delivery (session)
  "Forget root context acknowledgements when SESSION changes engines.
Child conversations keep their independent path-instruction acknowledgements."
  (setf (mevedel-session-workspace-instruction-hashes session)
        (mapcar (lambda (entry)
                  (cons (car entry) (unless (equal "/root" (caar entry)) (cdr entry))))
                (mevedel-session-workspace-instruction-hashes session))
        (mevedel-session-mentions-shown session) (make-hash-table :test #'equal))
  (mevedel-reminders-rearm-plan-reference session))

(defun mevedel-claude-code-release-history (session &optional scope)
  "Detach SESSION's native history in SCOPE for transcript continuation.
SCOPE defaults to root; a child keeps an unstarted marker for excerpt recovery.
Persist detachment before dispatch so returning to Claude cannot resume an
external conversation missing intervening gptel turns.  Selection alone does
not call this operation."
  (setq scope (or scope "root"))
  (when (assoc scope (mevedel-session-external-conversations session))
    (let* ((previous (mevedel-session-external-conversations session))
           (hashes (mevedel-session-workspace-instruction-hashes session))
           (mentions (mevedel-session-mentions-shown session))
           (reminder (cl-find 'plan-reference (mevedel-session-reminders session)
                              :key #'mevedel-reminder-type))
           (fired (and reminder (mevedel-reminder-last-fired reminder))))
      (setf (mevedel-session-external-conversations session)
            (cl-remove scope previous :key #'car :test #'equal))
      (if (equal scope "root")
          (mevedel-claude-code-reset-root-delivery session)
        (push (list scope :engine 'claude-code :state 'unstarted)
              (mevedel-session-external-conversations session))
        (setf (mevedel-session-workspace-instruction-hashes session)
              (mapcar (lambda (entry)
                        (cons (car entry) (unless (equal scope (caar entry)) (cdr entry))))
                      hashes)))
      (condition-case err
          (mevedel-session-artifacts-save session (current-buffer))
        (error
         (setf (mevedel-session-external-conversations session) previous
               (mevedel-session-workspace-instruction-hashes session) hashes
               (mevedel-session-mentions-shown session) mentions)
         (when reminder (setf (mevedel-reminder-last-fired reminder) fired))
         (signal (car err) (cdr err)))))
    (message "mevedel: %s will continue from its retained transcript on the next send" scope)))

;;;###autoload
(defun mevedel-claude-code-recover-history (&optional scope)
  "Recover SCOPE from its retained transcript on the next send.
SCOPE is root (the default) or a retained child's canonical path.
Detach the native reference without deleting the transcript or replaying
effects.
The next Claude turn starts a labelled excerpt continuation, rather than
an exact resume.
This command does not start a model request or resume a paused Goal."
  (interactive
   (list (completing-read "Recover Claude conversation: "
                          (and (bound-and-true-p mevedel--session)
                               (cl-loop for (scope . _) in (mevedel-session-external-conversations mevedel--session)
                                        when (or (equal scope "root")
                                                 (assoc scope (mevedel-session-agent-registry mevedel--session)))
                                        collect scope))
                          nil t nil nil "root")))
  (setq scope (or scope "root"))
  (let* ((session (bound-and-true-p mevedel--session))
         (buffer (and session (mevedel-session-root-buffer session))))
    (unless (buffer-live-p buffer) (user-error "No live mevedel session here"))
    (with-current-buffer buffer
      (when (or buffer-read-only (bound-and-true-p mevedel-session--read-only-mode))
        (user-error "This session is open read-only"))
      (mevedel-session-artifacts-assert-new-mutation-authority session)
      (when (or (mevedel-turn-busy-p buffer)
                (mevedel-agent-control-active-turn-p session))
        (user-error "Wait for or interrupt running turns before recovering Claude history"))
      (when-let* ((goal (mevedel-session-goal session))
                  ((eq 'active (mevedel-goal-status goal))))
        (user-error "Pause the Goal before recovering Claude history"))
      (unless (and (assoc scope (mevedel-session-external-conversations session))
                   (or (equal scope "root")
                       (assoc scope (mevedel-session-agent-registry session))))
        (user-error "No retained Claude conversation at %s" scope))
      (mevedel-claude-code-release-history session scope))))

;;;###autoload
(defun mevedel-claude-code-send (&optional model-input)
  "Send MODEL-INPUT through the session's selected Claude subscription model.
When omitted, read the latest submitted prompt from the canonical transcript."
  (unless (and mevedel--session (mevedel-claude-code-backend-p gptel-backend))
    (user-error "Select Claude Code in a mevedel session first"))
  (unless mevedel--current-directive-uuid
    (mevedel-claude-code-history-assert-current
     (cdr (assoc "root" (mevedel-session-external-conversations mevedel--session)))))
  (let* ((session mevedel--session)
         (buffer (current-buffer))
         (scope (or mevedel--current-directive-uuid "root"))
         ;; Directive prompts already contain their precisely selected durable
         ;; history. Resuming hidden history would reintroduce excluded turns.
         (history (and (not mevedel--current-directive-uuid)
                       (alist-get scope (mevedel-session-external-conversations session)
                                  nil nil #'equal)))
         (excerpt (and (not mevedel--current-directive-uuid)
                       (not (plist-get history :id))
                       (mevedel-engine-root-history-p session)
                       (mevedel-claude-code-history-excerpt session)))
         (directory (expand-file-name mevedel-claude-code-directory))
         (backend gptel-backend)
         (model-id gptel-model)
         (model (gptel--model-name gptel-model))
         (effort gptel-reasoning-effort)
         (prompt (mevedel-tool-render-data-strip-non-media
                  (or model-input
                      (buffer-substring
                       (mevedel-transcript-prompt-transform-start) (point-max)))))
         (tools (mapcar
                 (lambda (tool)
                   (or (cl-find tool (mevedel-tool-all)
                                :key #'mevedel-tool-gptel-tool :test #'eq)
                       (error "Tool %s is not registered with mevedel" (gptel-tool-name tool))))
                 gptel-tools))
         (request (or mevedel--current-request
                      (mevedel-request-begin session mevedel--current-directive-uuid)))
         (system-prompt gptel-system-prompt)
         record selected)
    (unless (and (eq session (mevedel-request-session request))
                 (eq request mevedel--current-request))
      (error "Subscription send lost its admitted request"))
    (when excerpt
      (mevedel-claude-code-reset-root-delivery session)
      (message "mevedel: starting Claude with an excerpt continuation"))
    (setf (mevedel-engine-info request)
          (append (mevedel-engine-info request)
                  (list :backend backend :model model-id :reasoning-effort effort
                        :external-history t)))
    ;; Preparation can settle before the agent returns an identity.  Record
    ;; that startup state without inventing a resumable native history.
    (unless (or history mevedel--current-directive-uuid)
      (setf (alist-get scope (mevedel-session-external-conversations session)
                       nil nil #'equal)
            '(:engine claude-code :state unstarted)))
    (mevedel-acp-turn-start
     request
     (lambda (mcp hook-command)
       (let ((gptel-backend backend) (gptel-model model-id)
             (gptel-reasoning-effort effort))
         (mevedel-claude-code-history-assert-current history)
         (when (and (plist-get history :id)
                    (not (and (eq 'claude-code (plist-get history :engine))
                              (equal (system-name) (plist-get history :host))
                              (equal directory (plist-get history :directory)))))
           (user-error "This Claude history belongs to another installation; its transcript remains readable"))
         (plist-put
          (mevedel-claude-code-launch
           (with-current-buffer buffer
             (mevedel-claude-code-context-system
              request (if (functionp system-prompt) (funcall system-prompt) system-prompt)
              selected))
           mcp model effort (plist-get history :id) hook-command)
          :admit-tool #'mevedel-claude-code-history-admit)))
     (lambda ()
       (let ((gptel-backend backend) (gptel-model model-id)
             (gptel-reasoning-effort effort))
         (mevedel-claude-code-context-prompt
          request (vconcat (when excerpt (vector `((type . "text") (text . ,excerpt))))
                           (mevedel-claude-code-context-input request prompt)) history)))
     tools
     (lambda (id)
       (mevedel-claude-code-history-assert-current history)
       (setq record (list :engine 'claude-code :id id :host (system-name)
                          :directory directory :state 'in-flight
                          :tool-calls (copy-tree (plist-get history :tool-calls))))
       (unless (mevedel-request-directive-uuid request)
         ;; A submitted prompt belongs to native history even if interruption
         ;; precedes all output.  Exclude mutable leading Org metadata offsets.
         (plist-put record :input-boundary
                    (cons (mevedel-session-current-segment session)
                          (- (marker-position (plist-get (mevedel-engine-info request) :position))
                             (mevedel-session-artifacts-content-start buffer)))))
       (setf (mevedel-engine-info request)
             (plist-put (mevedel-engine-info request) :mevedel-claude-history record))
       (setf (alist-get scope (mevedel-session-external-conversations session)
                        nil nil #'equal) record)
       ;; A crash after dispatch leaves an explicit uncertain turn.  The
       ;; retained ID is durable before any model-triggered effect can occur.
       (if (mevedel-request-directive-uuid request)
           (mevedel-session-artifacts-publish-sidecar-state session buffer)
         (mevedel-session-artifacts-save session buffer)
         ;; Publishing the first metadata drawer at an empty buffer's start
         ;; leaves a non-advancing submission marker before that drawer.
         (let ((position (plist-get (mevedel-engine-info request) :position)))
           (when (< position (mevedel-session-artifacts-content-start buffer))
             (set-marker position (mevedel-session-artifacts-content-start buffer))))))
     (lambda (outcome)
       (when (and (plist-get history :id) (not record))
         (mevedel-claude-code-history-unavailable request))
       (when (and record (not (eq 'diverged (plist-get record :state))))
         (plist-put record :state
                    (if (eq 'success (plist-get outcome :status))
                        'ready 'uncertain))))
     nil
     (lambda (done)
       (let ((gptel-backend backend) (gptel-model model-id)
             (gptel-reasoning-effort effort))
         (mevedel-preset-prepare-request request)
         (when (functionp system-prompt)
           (setq system-prompt (funcall system-prompt)))
         (mevedel-claude-code-context-selected
          (lambda (value) (setq selected value) (funcall done))))))
    request))

(provide 'mevedel-claude-code-session)
;;; mevedel-claude-code-session.el ends here
