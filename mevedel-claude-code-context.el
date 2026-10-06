;;; mevedel-claude-code-context.el --- Acknowledged native context -*- lexical-binding: t -*-

;;; Commentary:
;; Submitted prompt or hook mail stays queued until the SDK echoes that exact
;; complete text in its user-message or successful hook receipt.  At native
;; tool boundaries, changed selected observations use the same receipt boundary
;; before further effects.  Native compaction retains the full system baseline
;; and restores its current differences, even if delivered before compaction.

;;; Code:

(require 'mevedel-agent-control)
(require 'mevedel-agents)
(require 'mevedel-engine)
(require 'mevedel-context-delivery)
(require 'mevedel-gptel-stream-bridge)
(require 'mevedel-tools)
(require 'mevedel-reminders)
(require 'mevedel-tool-fs-read)
(require 'mevedel-mentions)
(require 'gptel-context)

;; `mevedel-acp-turn'
(declare-function mevedel-acp-turn--close-reasoning "mevedel-acp-turn" (request))

(defun mevedel-claude-code-context-selected (callback)
  "Format this conversation's selected gptel context and call CALLBACK.
Reuse the synchronous or asynchronous formatter without starting a model.
CALLBACK receives a plist containing placement, text and media sources."
  (if (not (and gptel-use-context gptel-context))
      (funcall callback nil)
    (let* ((sources (gptel-context--collect))
           (mode gptel-use-context)
           (media (seq-remove
                   (lambda (part) (string-prefix-p "text/" (plist-get part :mime)))
                   (and sources (gptel-context--collect-media sources))))
           (done (lambda (text) (funcall callback (list :mode mode :text text :media media)))))
      (if (= (car (func-arity gptel-context-string-function)) 2)
          (funcall gptel-context-string-function done sources)
        (funcall done (funcall gptel-context-string-function sources))))))

(defun mevedel-claude-code-context-input (owner text)
  "Prepare OWNER's selected TEXT and accepted media as ACP content.
Reuse mention resolution and permission checks.  Keep media in this request,
without adding it to another conversation's persistent gptel context."
  (let* ((child (mevedel-agent-invocation-p owner))
         (session (if child (mevedel-agent-invocation-parent-session owner)
                    (mevedel-request-session owner)))
         (fresh (or child (mevedel-request-directive-uuid owner)))
         (selected (plist-get (mevedel-engine-info owner) :mevedel-claude-selected-context))
         (context-text (and (eq 'user (plist-get selected :mode)) (plist-get selected :text)))
         (expansion (mevedel-mentions-expand-user-input text session fresh))
         (contexts (cl-delete-duplicates
                    (append (plist-get expansion :media-contexts)
                            (mapcar (lambda (part)
                                      (list (plist-get part :media) (plist-get part :mime)))
                                    (plist-get selected :media)))
                    :key #'car :test #'equal :from-end t))
         (content (list (list (cons 'type "text") (cons 'text (plist-get expansion :text))))))
    (when (and context-text (not (string-empty-p context-text)))
      (setq content (append content (list `((type . "text") (text . ,context-text))))))
    (dolist (context contexts)
      (pcase-let ((`(,path ,mime . ,rest) context))
        (unless (and (string-prefix-p "image/" mime) (gptel--model-mime-capable-p mime))
          (user-error "Claude ACP input does not support %s; use Read for this resource" mime))
        (setq content
              (append content
                      (list (list (cons 'type "image") (cons 'mimeType mime)
                                  (cons 'data (if (car rest) (base64-encode-string (car rest) t)
                                                (gptel--base64-encode path)))))))))
    (dolist (item (plist-get expansion :reminder-items))
      (mevedel-reminders-queue-turn-event
       (current-buffer) (cons 'mention (plist-get item :key)) (plist-get item :body)))
    (when (or context-text contexts (plist-get expansion :dedup-updates) (plist-get expansion :reminder-items))
      (setf (mevedel-engine-info owner)
            (plist-put (mevedel-engine-info owner) :mevedel-claude-input-commit
                       (lambda ()
                         (unless fresh (mevedel-mentions-commit-expansion session expansion))))))
    (vconcat content)))

(defun mevedel-claude-code-context--input-received-p (content message)
  "Return non-nil when native user MESSAGE echoes all required CONTENT.
Image order, MIME types and complete base64 bytes must match the submitted
images; tool-result messages cannot acknowledge a submitted user attachment."
  (let* ((parts (alist-get 'content (alist-get 'message message)))
         (images (seq-filter (lambda (part) (equal "image" (alist-get 'type part))) content))
         (received (and (sequencep parts) (not (stringp parts))
                        (seq-filter (lambda (part) (equal "image" (alist-get 'type part))) parts))))
    (and (equal "user" (alist-get 'type message))
         (null (alist-get 'parent_tool_use_id message))
         (equal "user" (alist-get 'role (alist-get 'message message)))
         (sequencep parts) (not (stringp parts))
         (= (length images) (length received))
         (cl-every (lambda (expected actual)
                     (let ((source (alist-get 'source actual)))
                       (and (equal "base64" (alist-get 'type source))
                            (equal (alist-get 'data expected) (alist-get 'data source))
                            (equal (alist-get 'mimeType expected) (alist-get 'media_type source)))))
                   images received)
         (seq-every-p
          (lambda (part)
            (or (equal "image" (alist-get 'type part))
                (seq-some (lambda (actual)
                            (and (equal "text" (alist-get 'type actual))
                                 (equal (alist-get 'text part) (alist-get 'text actual))))
                          parts)))
          content))))

(defun mevedel-claude-code-context--observations (owner)
  "Render OWNER's currently selected observations through their shared owner."
  (let* ((child (mevedel-agent-invocation-p owner))
         (session (if child (mevedel-agent-invocation-parent-session owner)
                    (mevedel-request-session owner)))
         (names (if child
                    (mevedel-agent-context-components (mevedel-agent-invocation-agent owner))
                  mevedel-system-retained-components)))
    (mevedel-context-delivery--observations
     names (mevedel-system--make-context
            (mevedel-session-workspace session)
            (mevedel-session-working-directory session) session (current-buffer)))))

(defun mevedel-claude-code-context-system (owner system &optional selected)
  "Compose OWNER's SYSTEM with its complete selected observation baseline.
The native system prompt is re-sent by Claude after compaction.  Only changed
observations need the bounded restoration hook, never a truncated baseline.
SELECTED is this turn's formatted gptel context."
  (unless (or (null (plist-get selected :text)) (stringp (plist-get selected :text)))
    (error "Context formatter must return text"))
  (let ((observations (mevedel-claude-code-context--observations owner)))
    (setf (mevedel-engine-info owner)
          (plist-put (mevedel-engine-info owner) :mevedel-claude-selected-context selected))
    (setf (mevedel-engine-info owner)
          (plist-put
           (plist-put (mevedel-engine-info owner) :mevedel-claude-system-observations observations)
           :mevedel-claude-observations observations))
    (string-join (append (when (eq 'system (plist-get selected :mode))
                           (list (or (plist-get selected :text) "")))
                         (cons (or system "") (mapcar #'cdr observations))) "\n\n")))

(defun mevedel-claude-code-context--workspace-context (owner)
  "Prepare the path instructions learned by OWNER's conversation."
  (mevedel-tool-fs-read-workspace-context
   (cond ((mevedel-agent-invocation-p owner) (mevedel-agent-invocation-parent-session owner))
         ((mevedel-request-directive-uuid owner) owner)
         (t (mevedel-request-session owner)))
   (if (mevedel-agent-invocation-p owner) (mevedel-agent-invocation-path owner) "/root")))

(defun mevedel-claude-code-context-restore (owner)
  "Restore OWNER's selected context, observations and guidance after compaction.
An oversized update requires a prompt continuation, never a preview accepted
as full restoration.  The native system already retains the complete baseline."
  (let* ((info (mevedel-engine-info owner))
         (selected (plist-get info :mevedel-claude-selected-context))
         (selected-text (and (eq 'user (plist-get selected :mode))
                             (plist-get selected :text)))
         (baseline (plist-get info :mevedel-claude-system-observations))
         (observations (mevedel-claude-code-context--observations owner))
         (roster (unless (and (mevedel-request-p owner) (mevedel-request-directive-uuid owner))
                   (mevedel-tools-agent-roster owner t)))
         (instructions (mevedel-claude-code-context--workspace-context owner))
         (limit (when (mevedel-agent-invocation-p owner)
                  (mevedel-reminders-agent-turn-limit-context owner)))
         (plans (when (mevedel-request-p owner)
                  (let ((session (mevedel-request-session owner)))
                    (cl-loop for reminder in (list (mevedel-reminders-make-plan-mode)
                                                   (mevedel-reminders-make-plan-reference))
                             when (funcall (mevedel-reminder-trigger reminder) session)
                             collect (list :type (mevedel-reminder-type reminder)
                                           :body (funcall (mevedel-reminder-content reminder) session))))))
         (changed (cl-remove-if
                   (lambda (item) (equal (cdr item) (alist-get (car item) baseline)))
                   observations)))
    ;; Earlier hook updates may have been retired by native compaction.
    (setf (mevedel-engine-info owner)
          (setq info (plist-put info :mevedel-claude-observations baseline)))
    (when (or selected-text changed roster plans limit (plist-get instructions :entries))
      (let* ((entries (append (when roster (list (list :type 'agent-roster :body (plist-get roster :body))))
                              (when limit (list (list :type 'max-turns-warning :body limit)))
                              plans (plist-get instructions :entries)))
             (context-text (concat (mapconcat #'cdr changed "\n\n")
                                   (when selected-text (concat "\n" selected-text))))
             (body (concat (format "<!-- mevedel-restoration:%s -->\n"
                                   (md5 (format "%s:%s:%s" (emacs-pid) (current-time) (random))))
                           context-text
                           (mapconcat (lambda (entry)
                                        (concat "\n" (mevedel-reminders-format-block (plist-get entry :body))))
                                      entries "\n")))
             (transcript (if entries
                             (concat context-text
                                     (mevedel--format-hook-audit-record
                                      (mevedel-reminders--injection-record entries 'mid-turn)))
                           body)))
        (setf (mevedel-engine-info owner)
              (plist-put info :mevedel-claude-restoration-pending
                         (list :route (if (> (/ (string-bytes (encode-coding-string body 'utf-16-le)) 2) 10000)
                                          'continuation 'hook)
                               :event "SessionStart" :body body :transcript transcript
                               :observations observations
                               :commits (append (when roster (list (plist-get roster :commit)))
                                                (plist-get instructions :commits)))))
        body))))

(defun mevedel-claude-code-context-check (owner)
  "Reject tool work for OWNER until required context is acknowledged."
  (let ((info (mevedel-engine-info owner)))
    (when (or (plist-get info :mevedel-claude-input-pending)
              (plist-get info :mevedel-claude-restoration-pending)
              (plist-get info :mevedel-claude-recovery-pending)
              (eq 'continuation (plist-get (plist-get info :mevedel-claude-context-pending) :route))
              (plist-get (plist-get info :mevedel-claude-context-pending) :observations)
              (plist-get (plist-get info :mevedel-claude-context-pending) :commits))
      (error "Claude did not acknowledge required context before continuing"))))

(defun mevedel-claude-code-context-prepare (owner &optional prompt-p instructions)
  "Prepare OWNER's observations, turn events, child roster and mail for delivery.
Never dequeue on send.  A pending batch is not retransmitted before its receipt.
Oversized whole messages remain queued; no preview counts as full delivery.
Oversized observation changes require a prompt continuation before more effects.
With PROMPT-P, also collect configured reminders and pending hook context, and
include complete mail without the hook limit.  The selected observation baseline
is already in the system prompt.  INSTRUCTIONS is an optional captured batch of
path instructions restored before an uncertain conversation resumes."
  (let* ((info (mevedel-engine-info owner))
         (context (if (mevedel-agent-invocation-p owner) owner
                    (unless (mevedel-request-directive-uuid owner)
                      (mevedel-request-session owner))))
         (observations (unless prompt-p (mevedel-claude-code-context--observations owner)))
         (previous (plist-get info :mevedel-claude-observations))
         (changed (cl-remove-if
                   (lambda (item) (equal (cdr item) (alist-get (car item) previous)))
                   observations))
         (events (mevedel-reminders--stage-turn-events (plist-get info :buffer)))
         (roster (and context (mevedel-tools-agent-roster owner)))
         (reminders (when prompt-p (mevedel-reminders-collect owner)))
         (entries (append (plist-get events :entries)
                          (plist-get instructions :entries)
                          (plist-get reminders :entries)
                          (when roster (list (list :type 'agent-roster :body (plist-get roster :body)))))))
    (when (and (plist-get info :mevedel-claude-context-pending) (or changed entries))
      (error "Claude's unacknowledged delivery prevents required context updates"))
    (unless (plist-get info :mevedel-claude-context-pending)
      (let* ((messages (and context (mevedel-agent-control-context-mailbox context)))
             (id (md5 (format "%s:%s:%s" (emacs-pid) (current-time) (random))))
             (prefix (concat (format "<!-- mevedel-delivery:%s -->\n" id)
                             (when changed (concat (mapconcat #'cdr changed "\n\n") "\n"))))
             ;; Preserve typed entries in the canonical transcript.  The receipt
             ;; marker is only needed to identify the native wire message.
             (transcript (concat (if entries
                                     (when changed (concat (mapconcat #'cdr changed "\n\n") "\n"))
                                   prefix)
                                 (when entries
                                   (mevedel--format-hook-audit-record
                                    (mevedel-reminders--injection-record
                                     entries (if prompt-p 'turn-start 'mid-turn))))))
             (body (concat prefix
                           (when entries
                             (concat (mapconcat
                                      (lambda (entry)
                                        (mevedel-reminders-format-block (plist-get entry :body)))
                                      entries "\n") "\n"))))
             (overflow (and (not prompt-p)
                            (> (/ (string-bytes (encode-coding-string body 'utf-16-le)) 2) 10000)))
             selected)
        (while (and messages
                    (let ((candidate (concat body (mevedel-tools--message-delivery-block
                                                   (car messages)) "\n")))
                      ;; Claude measures JavaScript string units, not Unicode
                      ;; codepoints.  Emoji can occupy two UTF-16 units each.
                      (when (or prompt-p overflow
                                (<= (/ (string-bytes (encode-coding-string candidate 'utf-16-le)) 2)
                                    10000))
                        (setq transcript
                              (concat transcript (substring candidate (length body)))
                              body candidate)
                        t)))
          (push (pop messages) selected))
        (when (or changed entries selected (plist-get reminders :commits))
          (setf (mevedel-engine-info owner)
                (plist-put info :mevedel-claude-context-pending
                           (list :context context :messages (nreverse selected) :body body
                                 :transcript transcript
                                 :observations (and changed observations)
                                 :commits (append (and entries (plist-get events :commits))
                                                  (plist-get instructions :commits)
                                                  (plist-get reminders :commits)
                                                  (when roster (list (plist-get roster :commit))))
                                 :route (cond (prompt-p 'prompt) (overflow 'continuation) (t 'hook))
                                 :event "PostToolBatch")))
          body)))))

(defun mevedel-claude-code-context-next-prompt (owner)
  "Return OWNER's full overflow context for a clean native prompt continuation.
Delivery still requires the exact SDK user receipt.  A user boundary stop
discards only these unsubmitted batches; their underlying events remain queued."
  (let ((info (mevedel-engine-info owner)) content)
    (dolist (key '(:mevedel-claude-context-pending :mevedel-claude-restoration-pending))
      (when-let* ((pending (plist-get info key))
                  ((eq 'continuation (plist-get pending :route))))
        (if (plist-get info :mevedel-end-turn)
            (setq info (plist-put info key nil))
          (plist-put pending :route 'prompt)
          (push `((type . "text") (text . ,(plist-get pending :body))) content))))
    (setf (mevedel-engine-info owner) info)
    (when content
      ;; Restoration occupies its own receipt slot.  Capture prompt reminders
      ;; too, including a child's newly reserved final-sample warning.
      (unless (plist-get info :mevedel-claude-context-pending)
        (when-let* ((body (mevedel-claude-code-context-prepare owner t)))
          (push `((type . "text") (text . ,body)) content)))
      (vconcat [((type . "text")
                 (text . "Continue the current task after applying the complete context updates below. The prior native prompt stopped only to deliver this context in full. Earlier tool effects remain applied; do not replay them."))]
               (nreverse content)))))

(defun mevedel-claude-code-context-prompt (owner content &optional history)
  "Append OWNER's initial context deliveries to ACP CONTENT.
Uncertain HISTORY requires an acknowledged recovery notice before more tools.
The native user-message echo, not submission, confirms delivery."
  (when (and (mevedel-request-p owner) (memq (plist-get history :state) '(uncertain in-flight)))
    ;; Root recovery is a shared pending event.  Use its normal receipt/commit
    ;; instead of adding a second copy beside configured session reminders.
    (let ((session (mevedel-request-session owner)))
      (unless (member mevedel-reminders-reconciliation-body (mevedel-session-pending-reminders session))
        (mevedel-session-enqueue-pending-reminder session mevedel-reminders-reconciliation-body))
      (mevedel-session-ensure-reminder session (mevedel-reminders-make-pending-events))))
  (when-let* ((body (mevedel-claude-code-context-prepare
                     owner t (mevedel-claude-code-context--workspace-context owner))))
    (setq content (vconcat content (vector `((type . "text") (text . ,body))))))
  (when (and (mevedel-agent-invocation-p owner) (memq (plist-get history :state) '(uncertain in-flight)))
    (let ((body (concat (format "<!-- mevedel-recovery:%s -->\n"
                                (md5 (format "%s:%s:%s" (emacs-pid) (current-time) (random))))
                        mevedel-reminders-reconciliation-body
                        " Never replay a prior tool call merely because its result is missing.")))
      (setf (mevedel-engine-info owner)
            (plist-put (mevedel-engine-info owner) :mevedel-claude-recovery-pending
                       (list :route 'prompt :body body)))
      (setq content (vconcat content (vector `((type . "text") (text . ,body)))))))
  (when-let* ((commit (plist-get (mevedel-engine-info owner) :mevedel-claude-input-commit)))
    (setf (mevedel-engine-info owner)
          (plist-put
           (plist-put (mevedel-engine-info owner) :mevedel-claude-input-commit nil)
           :mevedel-claude-input-pending (list :content content :commit commit))))
  content)

(defun mevedel-claude-code-context-observe (owner notification)
  "Acknowledge OWNER's captured context from an exact SDK NOTIFICATION.
The ACP connection has already checked the session identity.  Ordinary text,
tool notifications, malformed output and duplicate receipts cannot consume mail."
  (let* ((info (mevedel-engine-info owner))
         (message (alist-get 'message (alist-get 'params notification)))
         (keys '(:mevedel-claude-context-pending :mevedel-claude-restoration-pending
                                                 :mevedel-claude-recovery-pending)))
    (when-let* ((pending (plist-get info :mevedel-claude-input-pending))
                ((equal "_claude/sdkMessage" (alist-get 'method notification)))
                ((mevedel-claude-code-context--input-received-p (plist-get pending :content) message)))
      (setf (mevedel-engine-info owner)
            (plist-put (mevedel-engine-info owner) :mevedel-claude-input-pending nil))
      (funcall (plist-get pending :commit)))
    (dolist (key keys)
      (let ((pending (plist-get (mevedel-engine-info owner) key)))
        (when (and pending
                   (equal "_claude/sdkMessage" (alist-get 'method notification))
                   (pcase (plist-get pending :route)
                     ('prompt
                      (and (equal "user" (alist-get 'type message))
                           (null (alist-get 'parent_tool_use_id message))
                           (equal "user" (alist-get 'role (alist-get 'message message)))
                           (let ((content (alist-get 'content (alist-get 'message message))))
                             (and (sequencep content) (not (stringp content))
                                  (seq-some
                                   (lambda (block)
                                     (and (equal "text" (alist-get 'type block))
                                          (equal (plist-get pending :body) (alist-get 'text block))))
                                   content)))))
                     ('hook
                      (and (equal "system" (alist-get 'type message))
                           (equal "hook_response" (alist-get 'subtype message))
                           (equal (plist-get pending :event) (alist-get 'hook_event message))
                           (equal "success" (alist-get 'outcome message))
                           (equal 0 (alist-get 'exit_code message))
                           (let* ((output (ignore-errors
                                            (json-parse-string (alist-get 'stdout message)
                                                               :object-type 'plist)))
                                  (specific (plist-get output :hookSpecificOutput)))
                             (and (equal (plist-get pending :event) (plist-get specific :hookEventName))
                                  (equal (plist-get pending :body)
                                         (plist-get specific :additionalContext))))))))
          (let ((buffer (plist-get info :buffer)))
            (with-current-buffer buffer
              (mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch info)
              (mevedel-acp-turn--close-reasoning owner)
              (mevedel--insert-user-role-block-at-marker
               (or (plist-get pending :transcript) (plist-get pending :body))
               (mevedel--active-response-marker info buffer))
              (setf (mevedel-engine-info owner)
                    (plist-put (mevedel-engine-info owner) key nil))
              (when (plist-get pending :observations)
                (setf (mevedel-engine-info owner)
                      (plist-put (mevedel-engine-info owner) :mevedel-claude-observations
                                 (plist-get pending :observations))))
              (dolist (commit (plist-get pending :commits))
                (funcall commit))
              (when (plist-get pending :context)
                (mevedel-agent-control-acknowledge-mail
                 (plist-get pending :context) (plist-get pending :messages))))))))))

(provide 'mevedel-claude-code-context)
;;; mevedel-claude-code-context.el ends here
