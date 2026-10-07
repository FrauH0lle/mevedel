;;; mevedel-acp-turn.el --- ACP turns owned by admitted requests -*- lexical-binding: t -*-

;;; Commentary:
;; Bridges an admitted request to an external conversation and the ordinary
;; tool, transcript and terminal transactions.  Each turn owns its processes
;; and private tool endpoint; a retained conversation ID grants no authority.

;;; Code:

(require 'gptel)
(require 'mevedel-acp)
(require 'mevedel-acp-compaction)
(require 'mevedel-engine)
(require 'mevedel-gptel-stream-bridge)
(require 'mevedel-mcp)
(require 'mevedel-mcp-tools)
(require 'mevedel-presets)
(require 'mevedel-view-stream)

;; `mevedel-goal'
(declare-function mevedel-goal-capture-request "mevedel-goal" (request))

(defun mevedel-acp-turn--close-reasoning (request)
  "Close REQUEST's displayed reasoning block, if one is open."
  (let ((info (mevedel-engine-info request))
        (inhibit-read-only t))
    (when (plist-get info :mevedel-acp-reasoning)
      (plist-put info :mevedel-acp-reasoning nil)
      (gptel-curl--stream-insert-response '(reasoning . t) info))))

(defun mevedel-acp-turn--event (request notification)
  "Publish owned ACP NOTIFICATION for REQUEST once.
Tool notifications are observations; only the MCP endpoint executes tools."
  (let* ((inhibit-read-only t)
         (info (mevedel-engine-info request))
         (update (alist-get 'update (alist-get 'params notification)))
         (kind (alist-get 'sessionUpdate update))
         (content (alist-get 'content update)))
    (when (member kind '("compaction_update" "compaction_summary_chunk"))
      (when (equal kind "compaction_update")
        (mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch info)
        (mevedel-acp-turn--close-reasoning request))
      (mevedel-acp-compaction-observe request update))
    (when (and (member kind '("agent_message_chunk" "agent_thought_chunk"))
               (equal "text" (alist-get 'type content)))
      (let ((text (alist-get 'text content)))
        (when (stringp text)
          (if (equal kind "agent_thought_chunk")
              (progn
                (setf (mevedel-engine-info request)
                      (plist-put info :mevedel-acp-reasoning t))
                (gptel-curl--stream-insert-response (cons 'reasoning text) info))
            (mevedel-acp-turn--close-reasoning request)
            (gptel-curl--stream-insert-response text info)))))))

(defun mevedel-acp-turn--run-tool-hooks (hook request call)
  "Run the current buffer's tool-call HOOK for REQUEST's CALL, as gptel does.
Views, collaboration rooms and agent activity observe native calls through
the same hooks.  Tool repair already ran inside the MCP pipeline.  Return
the first hook result that stops the turn or blocks the call."
  (let ((info (mevedel-engine-info request))
        verdict)
    (setq call (append call (list :buffer (buffer-name)
                                  :backend (plist-get info :backend)
                                  :model (plist-get info :model))))
    (run-hook-wrapped
     hook
     (lambda (function)
       (unless (memq function '(mevedel-tool-repair-pre-tool-call
                                mevedel-tool-repair-post-tool-call))
         (let ((result (with-demoted-errors "Tool hook error: %S"
                         (funcall function (copy-sequence call)))))
           (when (and (not verdict)
                      (or (plist-get result :stop) (plist-get result :block)))
             (setq verdict result))))
       nil))
    verdict))

(defun mevedel-acp-turn--tool-result (request tool args id outcome)
  "Publish TOOL's ARGS and OUTCOME with native ID in REQUEST's transcript."
  (mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch
   (mevedel-engine-info request))
  (mevedel-acp-turn--close-reasoning request)
  (let* ((info (mevedel-engine-info request))
         ;; The gptel renderer looks up IDs by name.  Give it exactly this
         ;; completed call, so identical parallel calls retain their own IDs.
         (render-info (copy-sequence info))
         (result (gptel--to-string (plist-get outcome :result)))
         (gptel-include-tool-results t))
    (setq render-info
          (plist-put render-info :tool-use
                     (list (list :name (mevedel-tool-name tool) :id id
                                 :args args :result result))))
    (gptel--display-tool-results
     (list (list (mevedel-tool-gptel-tool tool) args result))
     render-info)
    (dolist (key '(:tracking-marker :tool-marker))
      (setq info (plist-put info key (plist-get render-info key))))
    (setf (mevedel-engine-info request) info)
    (mevedel-acp-turn--run-tool-hooks
     'gptel-post-tool-call-functions request
     (list :id id :name (mevedel-tool-name tool) :args args :result result))))

(defun mevedel-acp-turn-start (request launch content tools &optional ready terminal settle prepare)
  "Run admitted REQUEST through ACP with CONTENT and registered TOOLS.
CONTENT is an ACP vector or a function returning one in the owning buffer
after READY completes, immediately before dispatch.
LAUNCH receives the generated MCP server vector and native hook command,
and returns an ACP launch
plist.  Its :tool-id-field selects the adapter's native call identity from
MCP metadata.  Its optional :check-context callback validates REQUEST before
tool execution and successful settlement; failures end the turn as errors.
Its optional :complete-prompt callback receives REQUEST and each normalized
prompt outcome, returning that outcome with cumulative usage and optionally
:next-prompt content.  A successful continuation keeps this admission open;
only the final prompt settles the turn.  Cancellation and boundary stops win.
READY receives the retained conversation ID before the prompt
is sent; it may persist that identity or signal to prevent dispatch.
TERMINAL receives the normalized outcome before the terminal transaction saves
the session, allowing the conversation owner to record its last known state.
REQUEST may also be a retained-agent invocation; SETTLE then receives its
terminal status and owns the child terminal transaction.  Root requests use
ordinary request settlement.  No gptel state machine is created.
PREPARE, when non-nil, receives a zero-argument continuation.  Startup waits
for it; duplicate or late continuations cannot start a cancelled turn."
  (let* ((info (mevedel-engine-info request))
         (buffer (plist-get info :buffer))
         (calls (make-hash-table :test #'equal))
         (root-p (mevedel-request-p request))
         (admission (if root-p request (plist-get info :mevedel-request)))
         (dispatch-key (list 'acp-prompt (if root-p (mevedel-request-id request)
                                           (gensym "child"))))
         ;; Resolved once: every event and tool asks whether its target is busy.
         (dispatch-target (and (buffer-live-p buffer)
                               (mevedel-transport-target
                                (buffer-local-value 'default-directory buffer))))
         (event-key (list 'acp-events dispatch-key))
         pending draining server connection configuration finished cancellation-held started terminal-pending)
    (unless (and (buffer-live-p buffer)
                 (if root-p
                     (and (eq request (buffer-local-value 'mevedel--current-request buffer))
                          (not (mevedel-request-cancelled-p request)))
                   (and settle
                        (eq admission (buffer-local-value 'mevedel--current-request buffer))
                        (not (mevedel-request-cancelled-p admission))
                        (eq request (buffer-local-value 'mevedel--agent-invocation buffer))
                        (not (mevedel-agent-invocation-runtime-settled-p request)))))
      (error "External turn requires the current admitted request"))
    (with-current-buffer buffer
      (setf (mevedel-engine-info request)
            (append info
                    (list :position (copy-marker (point-max))
                          :include-reasoning t
                          :callback #'gptel-curl--stream-insert-response
                          :tools (mapcar #'mevedel-tool-gptel-tool tools))))
      (when root-p (mevedel-goal-capture-request request)))
    (cl-labels
        ((owned ()
           (and (buffer-live-p buffer)
                (eq admission (buffer-local-value 'mevedel--current-request buffer))
                (eq request (buffer-local-value
                             (if root-p 'mevedel--current-request 'mevedel--agent-invocation)
                             buffer))))
         (cancelled ()
           (if root-p (mevedel-request-cancelled-p request)
             (or (and admission (mevedel-request-cancelled-p admission))
                 (plist-get (mevedel-engine-info request) :mevedel-cancelled))))
         (enqueue (operation &optional terminal reject)
           (if (or finished (and (not terminal) (cancelled)))
               (when reject (funcall reject))
             (setq pending (nconc pending (list (list terminal operation reject))))
             (unless draining
               (unless (mevedel-transport-run-when-idle
                        event-key dispatch-target #'drain
                        (lambda () (finish '(:status interrupted))))
                 (kill-owner)))))
         (enqueue-later (operation &optional terminal reject)
           ;; Socket work is handled synchronously while ACP frames wait for
           ;; the library's drain timer.  Read ACP output that is already
           ;; available first, so its drain is armed before this timer.
           (when-let* ((process (and connection
                                     (alist-get :process (mevedel-acp-client connection))))
                       ((process-live-p process)))
             (accept-process-output process 0 nil 1))
           (mevedel-transport-run-at-time
            0 (lambda () (enqueue operation terminal reject))))
         (drain ()
           ;; Only `mevedel-transport-run-when-idle' calls this, after its
           ;; busy check; operations cannot leave the transport busy.
           (unless draining
             (setq draining t)
             (unwind-protect
                 (while (and pending (not finished) (owned))
                   (condition-case err
                       (let ((operation (pop pending)))
                         (if (or (car operation) (not (cancelled)))
                             (with-current-buffer buffer (funcall (cadr operation)))
                           (when-let* ((reject (caddr operation)))
                             (funcall reject))))
                     (error (finish (list :status 'error :message (error-message-string err))))))
               (setq draining nil))
             ;; The loop leaves work behind only for a turn it no longer owns.
             (when (and pending (not finished))
               (finish '(:status interrupted)))))
         (prompt (input)
           (setf (mevedel-engine-info request)
                 (plist-put (mevedel-engine-info request) :mevedel-acp-prompted t))
           (mevedel-acp-prompt
            connection input
            (lambda (event)
              ;; Events describe native history that already happened, so
              ;; they survive cancellation; only new work is rejected.
              (enqueue
               (lambda ()
                 (when-let* ((observe (plist-get configuration :observe)))
                   (funcall observe request event))
                 (mevedel-acp-turn--event request event))
               t))
            (lambda (outcome)
              (setq terminal-pending t)
              (enqueue
               (lambda ()
                 (setq terminal-pending nil)
                 (when (cancelled)
                   (setq outcome (plist-put outcome :status 'interrupted)))
                 (when-let* ((complete (plist-get configuration :complete-prompt)))
                   (setq outcome (funcall complete request outcome)))
                 (if (and (eq 'success (plist-get outcome :status))
                          (plist-get outcome :next-prompt)
                          (not (cancelled))
                          (not (plist-get (mevedel-engine-info request) :mevedel-end-turn)))
                     (prompt (plist-get outcome :next-prompt))
                   (finish outcome))) t))))
         (finish (outcome)
           (when (and (not finished) (eq 'success (plist-get outcome :status)))
             (when-let* ((check (plist-get configuration :check-context)))
               (condition-case err
                   (funcall check request)
                 (error (setq outcome (list :status 'error :message (error-message-string err)))))))
           (unless finished
             (setq finished t)
             (setq pending nil)
             (mevedel-transport-cancel-pending event-key)
             (when cancellation-held
               (setq cancellation-held nil)
               (mevedel--turn-release admission))
             (mevedel-transport-cancel-pending dispatch-key)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (remove-hook 'kill-buffer-hook #'kill-owner t)))
             (when connection (mevedel-acp-close connection))
             (when server (mevedel-mcp-stop server))
             (when (owned)
               (with-current-buffer buffer
                 (mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch
                  (mevedel-engine-info request))
                 (mevedel-acp-turn--close-reasoning request)
                 (let* ((info (mevedel-engine-info request))
                        (status (if (or (cancelled)
                                        (eq 'interrupted (plist-get outcome :status)))
                                    'aborted (plist-get outcome :status))))
                   (setq info (plist-put info :mevedel-acp-outcome status))
                   (when-let* ((message (plist-get outcome :message)))
                     (setq info (plist-put info :error
                                           (if (plist-get outcome :code)
                                               (list :message message :code (plist-get outcome :code))
                                             message))))
                   (setf (mevedel-engine-info request) info)
                   (when terminal
                     (condition-case err
                         (funcall terminal (plist-put (copy-sequence outcome) :status status))
                       (error
                        (setq status 'error)
                        (setf (mevedel-engine-info request)
                              (plist-put (mevedel-engine-info request)
                                         :error (error-message-string err))))))
                   (condition-case err
                       (run-hook-with-args 'gptel-post-response-functions
                                           (plist-get info :position)
                                           (or (plist-get info :tracking-marker)
                                               (plist-get info :position)))
                     (error
                      (display-warning
                       'mevedel (format "Post-response hook failed: %s"
                                        (error-message-string err)) :warning)))
                   (setf (mevedel-engine-info request)
                         (plist-put (mevedel-engine-info request) :mevedel-response-end
                                    (copy-marker (point-max) nil)))
                   (if settle
                       (funcall settle status)
                     (mevedel-preset--final-patch-handler
                      request
                      (lambda (owner)
                        (mevedel-preset--settle-terminal
                         owner (mevedel-request-cancelled-p owner) status)))))))))
         (cancel ()
           (unless finished
             (if (and connection
                      (or (mevedel-acp-active connection) terminal-pending))
                 (progn
                   ;; Public abort may otherwise clear the request while its
                   ;; native cancellation reply is still in flight.
                   (unless cancellation-held
                     (setq cancellation-held t)
                     (mevedel--turn-hold admission))
                   (mevedel-acp-cancel connection))
               (finish '(:status interrupted)))))
         (kill-owner ()
           (if root-p
               (mevedel-request-cancel request)
             (setf (mevedel-engine-info request)
                   (plist-put (mevedel-engine-info request) :mevedel-cancelled t))
             (when admission (mevedel-request-cancel admission)))
           (finish '(:status interrupted)))
         (dispatch (name args metadata complete)
           (let (cancel-call cancelled-call)
             (enqueue-later
              (lambda ()
                (unless cancelled-call
                  (condition-case err
                      (setq cancel-call (dispatch-now name args metadata complete))
                    (error
                     (funcall complete
                              (list :isError t :content
                                    (vector (list :type "text" :text (error-message-string err)))))))))
              nil
              (lambda ()
                (funcall complete
                         (list :isError t :content
                               (vector (list :type "text" :text "The owning turn was cancelled"))))))
             (lambda ()
               (setq cancelled-call t)
               (when (functionp cancel-call) (funcall cancel-call)))))
         (dispatch-now (name args metadata complete)
           (unless (and (owned) (not finished)
                        (not (cancelled))
                        (not (plist-get (mevedel-engine-info request) :mevedel-end-turn)))
             (error "The owning turn no longer accepts tools"))
           (when-let* ((check (plist-get configuration :check-context)))
             (condition-case err
                 (funcall check request)
               (error
                (finish (list :status 'error :message (error-message-string err)))
                (signal (car err) (cdr err)))))
           (let ((tool (cl-find name tools :test #'equal :key #'mevedel-tool-name))
                 (id (plist-get metadata (plist-get configuration :tool-id-field))))
             (unless (and tool (stringp id) (not (equal id "")))
               (error "Tool call lacks its admitted tool or native identity"))
             (when (gethash id calls) (error "Tool call identity was already admitted"))
             (puthash id t calls)
             (with-current-buffer buffer
               (let ((verdict (mevedel-acp-turn--run-tool-hooks
                               'gptel-pre-tool-call-functions request
                               (list :id id :name name :args args))))
                 (when (plist-get verdict :stop)
                   (finish (list :status 'error
                                 :message (or (plist-get verdict :stop-reason)
                                              (format "Tool %s stopped the turn" name))))
                   (error "The owning turn no longer accepts tools"))
                 (when-let* ((block (plist-get verdict :block)))
                   (let ((reason (if (stringp block) block (format "Tool %s blocked" name))))
                     ;; Observers that saw the call start also see it end.
                     (mevedel-acp-turn--run-tool-hooks
                      'gptel-post-tool-call-functions request
                      (list :id id :name name :args args :result reason))
                     (error "%s" reason))))
               (mevedel-mcp-tools-call
                request buffer tool args id
                (lambda (result outcome)
                  (unwind-protect
                      (when (and (owned) (not finished))
                        (with-current-buffer buffer
                          (mevedel-acp-turn--tool-result request tool args id outcome)))
                    (funcall complete result))))))))
      (when admission (mevedel-request-push-canceller admission #'cancel))
      (unless root-p
        (setf (mevedel-agent-invocation-runtime-cancel request)
              (lambda ()
                (setf (mevedel-engine-info request)
                      (plist-put (mevedel-engine-info request) :mevedel-cancelled t))
                (when admission (mevedel-request-cancel admission))
                (cancel)
                (unless finished 'deferred))))
      (with-current-buffer buffer (add-hook 'kill-buffer-hook #'kill-owner nil t))
      (cl-labels ((start ()
                    (when (and (not started) (not finished) (owned) (not (cancelled)))
                      (setq started t)
                      (with-current-buffer buffer
                        (condition-case err
                            (progn
                              (setq server
                                    (mevedel-mcp-start
                                     (lambda () (mevedel-mcp-tools-schemas
                                                 (mapcar #'mevedel-tool-gptel-tool tools)))
                                     #'dispatch
                                     (lambda (event complete)
                                       (let (abandoned)
                                         (enqueue-later
                                          (lambda ()
                                            ;; A hook client that gave up never
                                            ;; delivers what this control prepares.
                                            (unless abandoned
                                              (condition-case err
                                                  (funcall complete
                                                           (funcall (or (plist-get configuration :control)
                                                                        (error "Agent has no native hook handler"))
                                                                    request event))
                                                (error
                                                 (funcall complete nil (error-message-string err))
                                                 (finish (list :status 'error :message (error-message-string err)))))))
                                          nil
                                          (lambda ()
                                            ;; A native hook may block the peer
                                            ;; that must acknowledge cancellation.
                                            ;; Reject its authority without running
                                            ;; the queued control transaction.
                                            (funcall complete nil "The owning turn was cancelled")))
                                         (lambda () (setq abandoned t))))))
                              (setq configuration
                                    (funcall launch (vector (mevedel-mcp-configuration server))
                                             (mevedel-mcp-hook-command server)))
                              (unless (plist-get (mevedel-engine-info request) :mevedel-native-isolated)
                                (setq configuration (plist-put configuration :compaction t)))
                              (unless (or finished (not (owned)))
                                (setq connection
                                      (mevedel-acp-open
                                       configuration
                                       (lambda (active)
                                         (condition-case err
                                             (unless
                                                 (mevedel-transport-run-when-idle
                                                  dispatch-key dispatch-target
                                                  (lambda ()
                                                    (condition-case err
                                                        (if (or finished (not (owned)))
                                                            (mevedel-acp-close active)
                                                          (with-current-buffer buffer
                                                            (when ready
                                                              (funcall ready (mevedel-acp-session-id active))))
                                                          (when (and (not finished) (owned)
                                                                     (not (cancelled)))
                                                            (prompt (if (functionp content)
                                                                        (with-current-buffer buffer (funcall content))
                                                                      content))))
                                                      (error (finish (list :status 'error :message (error-message-string err))))))
                                                  (lambda () (finish '(:status interrupted))))
                                               (finish '(:status interrupted)))
                                           (error (finish (list :status 'error :message (error-message-string err))))))
                                       (lambda (message) (finish (list :status 'error :message message)))))))
                          (error (finish (list :status 'error :message (error-message-string err))))
                          (quit (finish '(:status interrupted)) (signal (car err) (cdr err))))))))
        (condition-case err
            (if prepare
                (with-current-buffer buffer (funcall prepare #'start))
              (start))
          (error (finish (list :status 'error :message (error-message-string err))))
          (quit (finish '(:status interrupted)) (signal (car err) (cdr err)))))
      connection)))

(provide 'mevedel-acp-turn)
;;; mevedel-acp-turn.el ends here
