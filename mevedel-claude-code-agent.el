;;; mevedel-claude-code-agent.el --- Retained subscription agents -*- lexical-binding: t -*-

;;; Commentary:
;; Runs a retained child through the same ACP/MCP transport as root turns.
;; The existing agent runtime owns terminal publication, capacity and results;
;; native model history is scoped to the child's canonical path.

;;; Code:

(require 'mevedel-acp-turn)
(require 'mevedel-agent-conversation)
(require 'mevedel-claude-code)
(require 'mevedel-claude-code-context)
(require 'mevedel-claude-code-history)
(require 'mevedel-context-delivery)
(require 'mevedel-session-artifacts)

;; `gptel'
(defvar gptel-reasoning-effort)

;; `mevedel-turn'
(declare-function mevedel-request-begin "mevedel-turn" (session &optional directive-uuid))

(defun mevedel-claude-code-agent-next-sample (invocation &optional boundary)
  "Prepare INVOCATION's next sample, or stop at BOUNDARY when its cap is spent.
The initial prompt and each completed tool batch precede one model sample.
Tools in the last allowed sample settle before the boundary ends the turn."
  (let* ((count (mevedel-agent-invocation-turn-count invocation))
         (limit (mevedel-agent-max-turns (mevedel-agent-invocation-agent invocation)))
         (buffer (mevedel-agent-invocation-buffer invocation)))
    (if (and boundary limit (>= count limit))
        (when (mevedel-turn-end-at-boundary invocation 'agent-turn-limit)
          (setf (mevedel-engine-info invocation)
                (plist-put (mevedel-engine-info invocation) :mevedel-agent-stop-note
                           (format "[Stopped before a final answer: the %d-turn limit was reached.]" limit))))
      (setq count (cl-incf (mevedel-agent-invocation-turn-count invocation)))
      (cond
       ((and limit (>= count limit))
        (mevedel-reminders-queue-turn-event
         buffer 'max-turns-limit
         (mevedel-reminders-agent-turn-limit-context invocation)))
       (boundary
        (let* ((batch (mevedel-reminders--collect-from
                       (seq-filter (lambda (reminder) (eq 'max-turns-warning (mevedel-reminder-type reminder)))
                                   (mevedel-agent-invocation-reminders invocation))
                       count invocation))
               (commits (plist-get batch :commits)))
          (when (plist-get batch :entries)
            (mevedel-reminders-queue-turn-event
             buffer 'max-turns-warning
             (mapconcat (lambda (entry) (plist-get entry :body)) (plist-get batch :entries) "\n")
             (lambda () (dolist (commit commits) (funcall commit)))))))))))

(defun mevedel-claude-code-agent-run (invocation callback)
  "Run INVOCATION with its frozen configuration and terminal CALLBACK.
CALLBACK follows the existing agent bookkeeping contract.  Return INVOCATION;
its runtime canceller owns the transport without creating a gptel FSM."
  (let* ((buffer (mevedel-agent-invocation-buffer invocation))
         (session (mevedel-agent-invocation-parent-session invocation))
         (scope (mevedel-agent-invocation-require-path invocation))
         (history (alist-get scope (mevedel-session-external-conversations session)
                             nil nil #'equal))
         (directory (expand-file-name mevedel-claude-code-directory))
         record selected)
    (mevedel-agent-conversation-configure invocation buffer)
    (with-current-buffer buffer
      (unless (mevedel-claude-code-backend-p gptel-backend)
        (error "Unsupported external agent provider"))
      ;; Reuse request-owned interaction and pipeline cleanup.  The invocation
      ;; still owns its retained history and terminal publication; no FSM is
      ;; needed to admit the child request.
      (setf (mevedel-engine-info invocation)
            (plist-put (mevedel-engine-info invocation) :mevedel-request
                       (mevedel-request-begin session)))
      (setf (mevedel-engine-info invocation)
            (append (mevedel-engine-info invocation)
                    (list :buffer buffer :stream t :backend gptel-backend
                          :model gptel-model :external-history t
                          :mevedel-agent-invocation invocation)))
      (let ((model (gptel--model-name gptel-model))
            (effort gptel-reasoning-effort)
            (system gptel-system-prompt)
            (tools (mapcar
                    (lambda (tool)
                      (or (cl-find tool (mevedel-tool-all)
                                   :key #'mevedel-tool-gptel-tool :test #'eq)
                          (error "Tool %s is not registered with mevedel" (gptel-tool-name tool))))
                    gptel-tools)))
        (mevedel-acp-turn-start
         invocation
         (lambda (mcp hook)
           (when (and (plist-get history :id)
                      (not (and (eq 'claude-code (plist-get history :engine))
                                (equal (system-name) (plist-get history :host))
                                (equal directory (plist-get history :directory)))))
             (user-error "This Claude child history belongs to another installation; its transcript remains readable"))
           (plist-put
            (mevedel-claude-code-launch
             (mevedel-claude-code-context-system invocation system selected)
             mcp model effort (plist-get history :id) hook)
            :admit-tool #'mevedel-claude-code-history-admit))
         (lambda ()
           (mevedel-claude-code-agent-next-sample invocation)
           (mevedel-claude-code-context-prompt
            invocation
            (vconcat
             (when-let* ((excerpt (and history (not (plist-get history :id))
                                      (mevedel-claude-code-history-excerpt session t))))
               (vector `((type . "text") (text . ,excerpt))))
             (mevedel-claude-code-context-input
              invocation (mevedel-tool-render-data-strip-non-media
                          (plist-get (mevedel-engine-info invocation) :mevedel-agent-prompt))))
            history))
         tools
         (lambda (id)
           (setq record (list :engine 'claude-code :id id :host (system-name)
                              :directory directory :state 'in-flight
                              :tool-calls (copy-tree (plist-get history :tool-calls))))
           (setf (mevedel-engine-info invocation)
                 (plist-put (mevedel-engine-info invocation) :mevedel-claude-history record))
           (setf (alist-get scope (mevedel-session-external-conversations session)
                            nil nil #'equal) record)
           (mevedel-session-artifacts-publish-sidecar-state
            session (mevedel-session-root-buffer session)))
         (lambda (outcome)
           (when (and (plist-get history :id) (not record))
             (mevedel-claude-code-history-unavailable invocation))
           (when record
             (plist-put record :state
                        (if (eq 'success (plist-get outcome :status)) 'ready 'uncertain))))
         (lambda (status)
           (mevedel-goal-charge-agent-progress invocation)
           (funcall callback (pcase status ('success t) ('aborted 'abort) (_ nil))
                    (mevedel-engine-info invocation)))
         (lambda (done)
           (mevedel-claude-code-context-selected
            (lambda (value) (setq selected value) (funcall done)))))))
    invocation))

(provide 'mevedel-claude-code-agent)
;;; mevedel-claude-code-agent.el ends here
