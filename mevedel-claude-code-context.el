;;; mevedel-claude-code-context.el --- Acknowledged native context -*- lexical-binding: t -*-

;;; Commentary:
;; Submitted prompt or hook mail stays queued until the SDK echoes that exact
;; complete text in its user-message or successful hook receipt.  At native
;; tool boundaries, changed selected observations use the same receipt boundary
;; before further effects.  Native compaction retains the full system baseline
;; and restores its current differences, even if delivered before compaction.
;; Receipts reach the transcript as the shared typed injection record and mail
;; blocks, never as wire markers.

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

(defun mevedel-claude-code-context--session (owner)
  "Return the session that owns OWNER's conversation."
  (if (mevedel-agent-invocation-p owner)
      (mevedel-agent-invocation-parent-session owner)
    (mevedel-request-session owner)))

(defun mevedel-claude-code-context--images (contexts)
  "Return ACP image parts for media CONTEXTS of (PATH MIME [BYTES])."
  (mapcar
   (lambda (context)
     (pcase-let ((`(,path ,mime . ,rest) context))
       (unless (and (string-prefix-p "image/" mime) (gptel--model-mime-capable-p mime))
         (user-error "Claude ACP input does not support %s; use Read for this resource" mime))
       `((type . "image") (mimeType . ,mime)
         (data . ,(if (car rest) (base64-encode-string (car rest) t)
                    (gptel--base64-encode path))))))
   contexts))

(defun mevedel-claude-code-context-input (owner text)
  "Prepare OWNER's TEXT and accepted mention media as ACP content.
Reuse mention resolution and permission checks.  Keep media in this request,
without adding it to another conversation's persistent gptel context."
  (let* ((child (mevedel-agent-invocation-p owner))
         (session (mevedel-claude-code-context--session owner))
         (fresh (or child (mevedel-request-directive-uuid owner)
                    (plist-get (mevedel-engine-info owner) :mevedel-native-isolated)))
         (expansion (mevedel-mentions-expand-user-input text session fresh))
         (images (mevedel-claude-code-context--images (plist-get expansion :media-contexts))))
    (dolist (item (plist-get expansion :reminder-items))
      (mevedel-reminders-queue-turn-event
       (current-buffer) (cons 'mention (plist-get item :key)) (plist-get item :body)))
    (when (or images (plist-get expansion :dedup-updates) (plist-get expansion :reminder-items))
      (setf (mevedel-engine-info owner)
            (plist-put (mevedel-engine-info owner) :mevedel-claude-input-commit
                       (lambda ()
                         (unless fresh (mevedel-mentions-commit-expansion session expansion))))))
    (vconcat (list `((type . "text") (text . ,(plist-get expansion :text)))) images)))

(defun mevedel-claude-code-context--user-parts (message)
  "Return the content parts of top-level native user MESSAGE, or nil.
Tool-result messages cannot acknowledge submitted input."
  (let ((parts (alist-get 'content (alist-get 'message message))))
    (and (equal "user" (alist-get 'type message))
         (null (alist-get 'parent_tool_use_id message))
         (equal "user" (alist-get 'role (alist-get 'message message)))
         (sequencep parts) (not (stringp parts))
         parts)))

(defun mevedel-claude-code-context--input-received-p (content message)
  "Return non-nil when native user MESSAGE echoes all required CONTENT.
Image order, MIME types and complete base64 bytes must match the submitted
images; tool-result messages cannot acknowledge a submitted user attachment."
  (let* ((parts (mevedel-claude-code-context--user-parts message))
         (images (seq-filter (lambda (part) (equal "image" (alist-get 'type part))) content))
         (received (seq-filter (lambda (part) (equal "image" (alist-get 'type part))) parts)))
    (and parts
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
         (session (mevedel-claude-code-context--session owner))
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

(defun mevedel-claude-code-context--workspace-context (owner &optional changed)
  "Prepare the path instructions learned by OWNER's conversation.
With CHANGED, omit files whose current contents match their acknowledged hash:
retained native history already holds that delivery."
  (let* ((child (mevedel-agent-invocation-p owner))
         (scope (cond (child (mevedel-agent-invocation-parent-session owner))
                      ((or (mevedel-request-directive-uuid owner)
                           (plist-get (mevedel-engine-info owner) :mevedel-native-isolated)) owner)
                      (t (mevedel-request-session owner))))
         (path (if child (mevedel-agent-invocation-path owner) "/root"))
         (all (mevedel-tool-fs-read-workspace-context scope path)))
    (if (not changed)
        all
      (let ((hashes (mevedel-tool-fs-read--instruction-hashes scope)) entries commits)
        (cl-mapc (lambda (entry commit)
                   (let ((file (cdr (plist-get entry :type))))
                     (unless (equal (alist-get (list path file) hashes nil nil #'equal)
                                    (plist-get entry :hash))
                       (push entry entries)
                       (push commit commits))))
                 (plist-get all :entries) (plist-get all :commits))
        (list :entries (nreverse entries) :commits (nreverse commits))))))

(defun mevedel-claude-code-context-hook-fits-p (text)
  "Return non-nil when TEXT fits Claude's inline hook output limit.
Claude measures JavaScript string units, not codepoints: emoji count twice."
  (<= (/ (string-bytes (encode-coding-string text 'utf-16le)) 2) 10000))

(defun mevedel-claude-code-context--batch (entries phase)
  "Return (BODY . TRANSCRIPT) delivering typed ENTRIES in PHASE.
BODY carries a unique receipt marker for the native wire only.  TRANSCRIPT is
the shared hidden injection record, empty without ENTRIES."
  (cons (concat (format "<!-- mevedel-delivery:%s -->\n"
                        (md5 (format "%s:%s:%s" (emacs-pid) (current-time) (random))))
                (mapconcat (lambda (entry)
                             (concat (mevedel-reminders-format-block (plist-get entry :body)) "\n"))
                           entries))
        (if entries
            (mevedel--format-hook-audit-record (mevedel-reminders--injection-record entries phase))
          "")))

(defun mevedel-claude-code-context--observation-entries (observations)
  "Return typed reminder entries for selected OBSERVATIONS."
  (mapcar (lambda (item) (list :type (car item) :body (cdr item))) observations))

(defun mevedel-claude-code-context--selected-key (owner)
  "Return OWNER's selected-context dedup key in its native conversation, or nil."
  (when-let* ((id (plist-get (plist-get (mevedel-engine-info owner) :mevedel-claude-history) :id)))
    (cons 'claude-selected-context id)))

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
         (roster (unless (and (mevedel-request-p owner)
                              (or (mevedel-request-directive-uuid owner)
                                  (plist-get info :mevedel-native-isolated)))
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
         (entries (append (mevedel-claude-code-context--observation-entries
                           (cl-remove-if
                            (lambda (item) (equal (cdr item) (alist-get (car item) baseline)))
                            observations))
                          (when (and selected-text (not (string-empty-p selected-text)))
                            (list (list :type 'selected-context :body selected-text)))
                          (when roster (list (list :type 'agent-roster :body (plist-get roster :body))))
                          (when limit (list (list :type 'max-turns-warning :body limit)))
                          plans (plist-get instructions :entries))))
    ;; Earlier hook updates may have been retired by native compaction.
    (setf (mevedel-engine-info owner)
          (setq info (plist-put info :mevedel-claude-observations baseline)))
    ;; Restoration cannot carry selected images; the next prompt re-sends them.
    (when-let* ((key (mevedel-claude-code-context--selected-key owner))
                (shown (mevedel-session-mentions-shown (mevedel-claude-code-context--session owner))))
      (remhash key shown))
    (when entries
      (let ((batch (mevedel-claude-code-context--batch entries 'mid-turn)))
        (setf (mevedel-engine-info owner)
              (plist-put info :mevedel-claude-restoration-pending
                         (list :route (if (mevedel-claude-code-context-hook-fits-p (car batch))
                                          'hook 'continuation)
                               :event "SessionStart" :body (car batch) :transcript (cdr batch)
                               :observations observations
                               :commits (append (when roster (list (plist-get roster :commit)))
                                                (plist-get instructions :commits)))))
        (car batch)))))

(defun mevedel-claude-code-context-check (owner)
  "Reject tool work for OWNER until required context is acknowledged."
  (let ((info (mevedel-engine-info owner)))
    (when (or (plist-get info :mevedel-claude-input-pending)
              (plist-get info :mevedel-claude-restoration-pending)
              (eq 'continuation (plist-get (plist-get info :mevedel-claude-context-pending) :route))
              (plist-get (plist-get info :mevedel-claude-context-pending) :observations)
              (plist-get (plist-get info :mevedel-claude-context-pending) :commits))
      (error "Claude did not acknowledge required context before continuing"))))

(defun mevedel-claude-code-context-prepare (owner &optional prompt-p extra)
  "Prepare OWNER's observations, turn events, child roster and mail for delivery.
Never dequeue on send.  A hook receipt can trail the next boundary; until it
arrives, everything else stays queued for a later one.  A prompt echo precedes
every tool, so its absence fails closed.  Oversized whole messages and changes
require a prompt continuation before more effects; no preview counts as full
delivery.  With PROMPT-P, also collect configured reminders and pending hook
context, and include complete mail without the hook limit.  The selected
observation baseline is already in the system prompt.  EXTRA is an optional
plist of further typed `:entries' and their `:commits'."
  (let* ((info (mevedel-engine-info owner))
         (pending (plist-get info :mevedel-claude-context-pending)))
    (unless (eq 'hook (plist-get pending :route))
      (let* ((context (if (mevedel-agent-invocation-p owner) owner
                        (unless (or (mevedel-request-directive-uuid owner)
                                    (plist-get info :mevedel-native-isolated))
                          (mevedel-request-session owner))))
             (observations (unless prompt-p (mevedel-claude-code-context--observations owner)))
             (previous (plist-get info :mevedel-claude-observations))
             (changed (cl-remove-if
                       (lambda (item) (equal (cdr item) (alist-get (car item) previous)))
                       observations))
             (events (mevedel-reminders--stage-turn-events (plist-get info :buffer)))
             (roster (and context (mevedel-tools-agent-roster owner)))
             (reminders (when prompt-p (mevedel-reminders-collect owner)))
             (entries (append (mevedel-claude-code-context--observation-entries changed)
                              (plist-get events :entries)
                              (plist-get extra :entries)
                              (plist-get reminders :entries)
                              (when roster (list (list :type 'agent-roster :body (plist-get roster :body)))))))
        (when (and pending entries)
          (error "Claude's unacknowledged delivery prevents required context updates"))
        (unless pending
          (let* ((batch (mevedel-claude-code-context--batch entries (if prompt-p 'turn-start 'mid-turn)))
                 (body (car batch))
                 (transcript (cdr batch))
                 (overflow (and (not prompt-p) (not (mevedel-claude-code-context-hook-fits-p body))))
                 (messages (and context (mevedel-agent-control-context-mailbox context)))
                 selected)
            (while (and messages
                        (let ((block (concat (mevedel-tools--message-delivery-block (car messages)) "\n")))
                          (when (or prompt-p overflow
                                    (mevedel-claude-code-context-hook-fits-p (concat body block))
                                    ;; A whole message beyond the hook limit takes
                                    ;; the continuation prompt, never a preview.
                                    (unless selected (setq overflow t)))
                            (setq body (concat body block)
                                  transcript (concat transcript block))
                            t)))
              (push (pop messages) selected))
            (when (or entries selected (plist-get reminders :commits))
              (setf (mevedel-engine-info owner)
                    (plist-put info :mevedel-claude-context-pending
                               (list :context context :messages (nreverse selected) :body body
                                     :transcript transcript
                                     :observations (and changed observations)
                                     :commits (append (and entries (plist-get events :commits))
                                                      (plist-get extra :commits)
                                                      (plist-get reminders :commits)
                                                      (when roster (list (plist-get roster :commit))))
                                     :route (cond (prompt-p 'prompt) (overflow 'continuation) (t 'hook))
                                     :event "PostToolBatch")))
              body)))))))

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
  "Append OWNER's selected context and initial context deliveries to ACP CONTENT.
Selected user-placement context and media already accepted by this native
HISTORY are not repeated, nor are acknowledged unchanged path instructions.
Uncertain HISTORY requires an acknowledged recovery notice before more tools.
The native user-message echo, not submission, confirms delivery."
  (let* ((child (mevedel-agent-invocation-p owner))
         (uncertain (memq (plist-get history :state) '(uncertain in-flight)))
         (session (mevedel-claude-code-context--session owner))
         (selected (plist-get (mevedel-engine-info owner) :mevedel-claude-selected-context))
         (text (and (eq 'user (plist-get selected :mode)) (plist-get selected :text)))
         (parts (append (when (and text (not (string-empty-p text)))
                          (list `((type . "text") (text . ,text))))
                        (mevedel-claude-code-context--images
                         (mapcar (lambda (part) (list (plist-get part :media) (plist-get part :mime)))
                                 (plist-get selected :media)))))
         (hash (and parts (secure-hash 'sha256 (prin1-to-string parts))))
         (shown (mevedel-session-mentions-shown session))
         (extra (mevedel-claude-code-context--workspace-context owner t)))
    (when (and hash (not (and shown (plist-get history :id)
                              (equal hash (cdr (gethash (cons 'claude-selected-context (plist-get history :id))
                                                        shown))))))
      (setq content (vconcat content (cl-remove-if (lambda (part) (seq-contains-p content part)) parts)))
      (let ((commit (plist-get (mevedel-engine-info owner) :mevedel-claude-input-commit)))
        (setf (mevedel-engine-info owner)
              (plist-put (mevedel-engine-info owner) :mevedel-claude-input-commit
                         (lambda ()
                           (when commit (funcall commit))
                           (when-let* ((key (mevedel-claude-code-context--selected-key owner))
                                       (shown (mevedel-session-mentions-shown session)))
                             (puthash key (cons (mevedel-session-turn-count session) hash) shown)))))))
    (when (and (not child) uncertain)
      ;; Root recovery is a shared pending event.  Use its normal receipt/commit
      ;; instead of adding a second copy beside configured session reminders.
      (unless (member mevedel-reminders-reconciliation-body (mevedel-session-pending-reminders session))
        (mevedel-session-enqueue-pending-reminder session mevedel-reminders-reconciliation-body))
      (mevedel-session-ensure-reminder session (mevedel-reminders-make-pending-events)))
    (when (and child uncertain)
      (setq extra (list :entries (append (plist-get extra :entries)
                                         (list (list :type 'pending-events
                                                     :body (concat mevedel-reminders-reconciliation-body
                                                                   " Never replay a prior tool call merely because its result is missing."))))
                        ;; Nothing is consumed, but tools still wait for receipt.
                        :commits (cons #'ignore (plist-get extra :commits)))))
    (when-let* ((body (mevedel-claude-code-context-prepare owner t extra)))
      (setq content (vconcat content (vector `((type . "text") (text . ,body))))))
    (when-let* ((commit (plist-get (mevedel-engine-info owner) :mevedel-claude-input-commit)))
      (setf (mevedel-engine-info owner)
            (plist-put
             (plist-put (mevedel-engine-info owner) :mevedel-claude-input-commit nil)
             :mevedel-claude-input-pending (list :content content :commit commit))))
    content))

(defun mevedel-claude-code-context-observe (owner notification)
  "Acknowledge OWNER's captured context from an exact SDK NOTIFICATION.
The ACP connection has already checked the session identity.  Ordinary text,
tool notifications, malformed output and duplicate receipts cannot consume mail."
  (let* ((info (mevedel-engine-info owner))
         (message (alist-get 'message (alist-get 'params notification))))
    (when-let* ((pending (plist-get info :mevedel-claude-input-pending))
                ((equal "_claude/sdkMessage" (alist-get 'method notification)))
                ((mevedel-claude-code-context--input-received-p (plist-get pending :content) message)))
      (setf (mevedel-engine-info owner)
            (plist-put (mevedel-engine-info owner) :mevedel-claude-input-pending nil))
      (funcall (plist-get pending :commit)))
    (dolist (key '(:mevedel-claude-context-pending :mevedel-claude-restoration-pending))
      (let ((pending (plist-get (mevedel-engine-info owner) key)))
        (when (and pending
                   (equal "_claude/sdkMessage" (alist-get 'method notification))
                   (pcase (plist-get pending :route)
                     ('prompt
                      (seq-some (lambda (block)
                                  (and (equal "text" (alist-get 'type block))
                                       (equal (plist-get pending :body) (alist-get 'text block))))
                                (mevedel-claude-code-context--user-parts message)))
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
               (plist-get pending :transcript)
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
