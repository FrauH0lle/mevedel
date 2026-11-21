;;; niwa-claude-code-agent.el --- Retained subscription agents -*- lexical-binding: t -*-

;;; Commentary:
;; Runs a retained child through the same ACP/MCP transport as root turns.
;; The existing agent runtime owns terminal publication, capacity and results;
;; native model history is scoped to the child's canonical path.

;;; Code:

(require 'niwa-acp-turn)
(require 'niwa-agent-conversation)
(require 'niwa-claude-code)
(require 'niwa-claude-code-context)
(require 'niwa-claude-code-history)
(require 'niwa-context-delivery)
(require 'niwa-session-artifacts)
(require 'niwa-tools)

;; `gptel'
(defvar gptel-reasoning-effort)

;; `niwa-turn'
(declare-function niwa-request-begin "niwa-turn" (session &optional options))

(defun niwa-claude-code-agent-next-sample (invocation &optional boundary)
  "Prepare INVOCATION's next sample, or stop at BOUNDARY when its cap is spent.
The initial prompt and each completed tool batch precede one model sample.
Tools in the last allowed sample settle before the boundary ends the turn."
  (let* ((count (niwa-agent-invocation-turn-count invocation))
         (limit (niwa-agent-max-turns (niwa-agent-invocation-agent invocation)))
         (buffer (niwa-agent-invocation-buffer invocation)))
    (if (and boundary limit (>= count limit))
        (when (niwa-turn-end-at-boundary invocation 'agent-turn-limit)
          (setf (niwa-engine-info invocation)
                (plist-put (niwa-engine-info invocation) :niwa-agent-stop-note
                           (format "[Stopped before a final answer: the %d-turn limit was reached.]" limit))))
      (setq count (cl-incf (niwa-agent-invocation-turn-count invocation)))
      (cond
       ((and limit (>= count limit))
        (niwa-reminders-queue-turn-event
         buffer 'max-turns-limit
         (niwa-reminders-agent-turn-limit-context invocation)))
       (boundary
        (let* ((batch (niwa-reminders-agent-turn-warnings invocation count))
               (commits (plist-get batch :commits)))
          (when (plist-get batch :entries)
            (niwa-reminders-queue-turn-event
             buffer 'max-turns-warning
             (mapconcat (lambda (entry) (plist-get entry :body)) (plist-get batch :entries) "\n")
             (lambda () (dolist (commit commits) (funcall commit)))))))))))

(defun niwa-claude-code-agent-run (invocation callback)
  "Run INVOCATION with its frozen configuration and terminal CALLBACK.
CALLBACK follows the existing agent bookkeeping contract.  Return INVOCATION;
its runtime canceller owns the transport without creating a gptel FSM."
  (let* ((buffer (niwa-agent-invocation-buffer invocation))
         (session (niwa-agent-invocation-parent-session invocation))
         (scope (niwa-agent-invocation-require-path invocation))
         (history (alist-get scope (niwa-session-external-conversations session)
                             nil nil #'equal))
         record selected)
    (niwa-claude-code-history-assert-current history)
    (niwa-agent-conversation-configure invocation buffer)
    (with-current-buffer buffer
      (unless (niwa-claude-code-backend-p gptel-backend)
        (error "Unsupported external agent provider"))
      ;; Reuse request-owned interaction and pipeline cleanup.  The invocation
      ;; still owns its retained history and terminal publication; no FSM is
      ;; needed to admit the child request.
      (setf (niwa-engine-info invocation)
            (plist-put (niwa-engine-info invocation) :niwa-request
                       (niwa-request-begin session)))
      (setf (niwa-engine-info invocation)
            (append (niwa-engine-info invocation)
                    (list :buffer buffer :stream t :backend gptel-backend
                          :model gptel-model :external-history t
                          :niwa-native-before-history (copy-tree history)
                          :niwa-agent-invocation invocation)))
      (let ((model (gptel--model-name gptel-model))
            (effort gptel-reasoning-effort)
            (system gptel-system-prompt)
            (tools (niwa-tools-native-roster invocation)))
        (niwa-acp-turn-start
         invocation
         (lambda (mcp hook)
           (niwa-claude-code-launch
            (niwa-claude-code-context-system invocation system selected (plist-get history :id))
            mcp model effort (plist-get history :id) hook))
         (lambda ()
           (niwa-claude-code-agent-next-sample invocation)
           (niwa-claude-code-context-prompt
            invocation
            (vconcat
             (when-let* ((excerpt (and history (not (plist-get history :id))
                                      (niwa-claude-code-history-excerpt
                                       session t
                                       (plist-get (niwa-engine-info invocation)
                                                  :niwa-agent-prompt-start)))))
               (vector `((type . "text") (text . ,excerpt))))
             (niwa-claude-code-context-input
              invocation (niwa-tool-render-data-strip-non-media
                          (plist-get (niwa-engine-info invocation) :niwa-agent-prompt))))
            history))
         tools
         (lambda (id)
           (niwa-claude-code-history-assert-current history)
           ;; Children keep one canonical transcript across compactions, rather
           ;; than numbered root segments.  Zero identifies that private body.
           (setq record (niwa-claude-code-history-open
                         invocation id
                         (cons 0
                               (- (marker-position
                                   (plist-get (niwa-engine-info invocation) :position))
                                  (niwa-session-artifacts-content-start buffer)))
                         history))
           (setf (alist-get scope (niwa-session-external-conversations session)
                            nil nil #'equal) record)
           (niwa-session-artifacts-publish-sidecar-state
            session (niwa-session-root-buffer session)))
         (lambda (outcome)
           (niwa-claude-code-history-settle invocation history record outcome))
         (lambda (status)
           (niwa-goal-charge-agent-progress invocation t)
           (funcall callback (pcase status ('success t) ('aborted 'abort) (_ nil))
                    (niwa-engine-info invocation)))
         (lambda (done)
           (niwa-claude-code-context-selected
            (lambda (value) (setq selected value) (funcall done)))))))
    invocation))

(provide 'niwa-claude-code-agent)
;;; niwa-claude-code-agent.el ends here
