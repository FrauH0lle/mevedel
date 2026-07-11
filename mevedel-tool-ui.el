;;; mevedel-tool-ui.el -- User interaction tools -*- lexical-binding: t -*-

;;; Commentary:

;; User-facing tool UI: directory access requests, agent delegation,
;; todo list display, and the Ask questionnaire.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'gptel-request)
  (require 'mevedel-tool-registry)
  (require 'mevedel-agents)
  (require 'mevedel-reminders))

(require 'mevedel-models)
(require 'mevedel-permission-log)
(require 'mevedel-queue)
(require 'subr-x)

;; `gptel-agent-tools'
(declare-function mevedel-tool-truthy-p "mevedel-tool-registry" (value))

;; `mevedel-queue'
(declare-function mevedel-queue--entry-metadata-get "mevedel-queue"
                  (entry key))
(declare-function mevedel-queue--entry-metadata-put "mevedel-queue"
                  (entry key value))

(declare-function gptel-agent--task "ext:gptel-agent-tools" (main-cb agent-type description prompt))
(declare-function gptel-agent--block-bg "ext:gptel-agent-tools" ())

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-handlers "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-table "ext:gptel-request" (cl-x) t)
(declare-function gptel--fsm-transition "ext:gptel-request"
                  (machine &optional new-state))
(declare-function gptel-request "ext:gptel-request" (&optional prompt &rest args))
(declare-function gptel--inject-prompt "ext:gptel-request"
                  (backend data new-prompt &optional position))
(declare-function gptel-abort "ext:gptel-request" (&optional buf))

;; `mevedel-agents'
(declare-function mevedel-agent-get "mevedel-agents" (name))
(declare-function mevedel-agent-invocation-activity
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-background-result-reported-p
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-call-count
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-create "mevedel-agents" (agent))
(declare-function mevedel-agent-invocation-description
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-foreground-result-reported-p
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-hook-audits
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-model-tier-override
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-context
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-fsm
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-tool-callback
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-turn
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-sidecar-dirty
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-verdict
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)

;; `mevedel-models'
(declare-function mevedel-model-tier-selector "mevedel-models" (tier))
(declare-function mevedel-model-normalize-tier "mevedel-models" (value))
(declare-function mevedel-model-agent-tool-description "mevedel-models" ())

;; `mevedel-agent-exec'
(declare-function mevedel-agent-exec--allocate-agent-buffer
                  "mevedel-agent-exec" (invocation parent-data-buffer))
(declare-function mevedel-agent-exec--save-transcript-buffer
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec--insert-injected-prompt
                  "mevedel-agent-exec" (invocation block &optional position))
(declare-function mevedel-agent-exec--record-activity
                  "mevedel-agent-exec"
                  (invocation item &optional reserved))
(declare-function mevedel-agent-exec--handle-update
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec--final-response-text
                  "mevedel-agent-exec" (invocation))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--shallow-ensure-files
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence--record-running-transcript
                  "mevedel-session-persistence" (session entry))
(declare-function mevedel-session-persistence--validate-transcript-path
                  "mevedel-session-persistence" (path save-path))
(declare-function mevedel-session-persistence--write-sidecar-now
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence--update-transcript-entry
                  "mevedel-session-persistence" (session agent-id updates))
(declare-function mevedel-session-p "mevedel-structs" (cl-x))
(declare-function mevedel-request-push-canceller
                  "mevedel-structs" (request canceller))

;; `mevedel-view'
(declare-function mevedel-view-open-agent-transcript
                  "mevedel-view" (agent-id))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-queue
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-queue
                  "mevedel-structs" (cl-x) t)
(defvar mevedel-session-persistence)
(defvar mevedel-session--read-only-mode)

;; `mevedel-reminders'
(declare-function mevedel-reminders--collect-from "mevedel-reminders"
                  (reminders turn-count ctx))

;; `mevedel-tool-plan'
(declare-function mevedel-plan-mode-clear-verification-pending
                  "mevedel-tool-plan" (&optional session))

;; `gptel'
(defvar gptel--fsm-last)
(defvar gptel-stream)
(declare-function gptel--update-status "ext:gptel" (msg &optional face))
(defvar gptel--request-alist)

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))

;; `mevedel-agent-runtime'
(declare-function mevedel-agent-runtime--stop-agent-result-string
                  "mevedel-agent-runtime" (result))
(declare-function mevedel-agent-runtime-dispatch
                  "mevedel-agent-runtime" t t)
(declare-function mevedel-agent-runtime-stop
                  "mevedel-agent-runtime"
                  (agent-id &optional reason parent-buffer))

;; `mevedel-tools'
(declare-function mevedel-tools--tool-search "mevedel-tools" (callback query &optional load))
(declare-function mevedel-tools--send-message "mevedel-tools" (args))
(declare-function mevedel-tools--handle-message-inject "mevedel-tools" (fsm))
(declare-function mevedel-tools--handle-terminal-mailbox "mevedel-tools" (fsm))
(declare-function mevedel-tools--current-deferred-context "mevedel-tools" ())
(declare-function mevedel-tools--deferred-context-for "mevedel-tools" (fsm))
(defvar mevedel-tools--current-fsm)

;; `mevedel-structs'
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-view'
(defvar mevedel-view--input-marker)
(defvar mevedel-view--interaction-marker)
(defvar mevedel-view--interaction-overlays)
(declare-function mevedel-view--interaction-anchor "mevedel-view" ())
(declare-function mevedel-view--interaction-register "mevedel-view"
                  (descriptor))
(declare-function mevedel-view--interaction-unregister "mevedel-view"
                  (id))
(declare-function mevedel-view--interaction-target-buffer "mevedel-view"
                  (&optional data-buffer))
(declare-function mevedel-view-collapse-by-height-p "mevedel-view" (body))
(declare-function mevedel-view-data-buffer-major-mode "mevedel-view" ())
(declare-function mevedel-view--insert-attribution "mevedel-view"
                  (agent-id &optional live-click-p calls))
(declare-function mevedel-session-agent-transcripts
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-enqueue-pending-reminder
                  "mevedel-structs" (session body))

;; `mevedel-workspace'
(declare-function mevedel-workspace--file-in-allowed-roots-p "mevedel-workspace" (file &optional buffer))
(declare-function mevedel-add-project-root "mevedel-workspace" (directory))

;;
;;; Directory access dedup

(defvar-local mevedel--pending-access-requests nil
  "Alist of (ROOT . (STATUS . WAITERS)) for in-flight access requests.

STATUS is one of `pending', `granted', `denied'.  WAITERS is a list
of callback thunks accumulated while STATUS is `pending'; they fire
once when the first prompt resolves.

Buffer-local per chat buffer to deduplicate access prompts within a
session.  Kept across overlay primitive callbacks via the data
buffer's binding."
  )

(defun mevedel-tools--request-access--collapse (ui-outcome)
  "Collapse a UI-OUTCOME symbol or cons into a cache status.

`approve' / `deny' / `aborted' pass through; `(feedback . TEXT)'
collapses to `deny' since later cache hits cannot replay the
feedback text.  Anything unrecognized collapses to `deny' as the
safest default."
  (pcase ui-outcome
    ('approve 'approve)
    ('aborted 'aborted)
    ((or 'deny `(feedback . ,_)) 'deny)
    (_ 'deny)))

(defun mevedel-tools--request-access--log
    (event root reason &optional buffer &rest props)
  "Persist RequestAccess EVENT for ROOT, REASON, BUFFER, and PROPS."
  (when-let* ((session (mevedel-permission-log-current-session buffer)))
    (apply #'mevedel-permission-log
           session event
           (append
            (list :origin (mevedel-permission-log-origin buffer)
                  :directory root
                  :reason reason)
            props))))

(defun mevedel-tools--request-access (root reason callback &optional buffer)
  "Request access to ROOT with REASON, delivering UI outcome to CALLBACK.

CALLBACK is invoked exactly once with one of `approve', `deny',
`(feedback . TEXT)', or `aborted' -- the same outcome vocabulary
`mevedel--prompt-user-for-access' produces.  Callers translate the
outcome into their tool-result string (e.g. preserving feedback in a
denial message, or `\"Error: aborted\"' on cancel).

Concurrent calls for the same ROOT collapse onto one prompt: the
first call shows the overlay, later calls register as waiters; the
prompt's resolution fans out the same outcome to every waiter so the
LLM-visible string for each tool call is consistent.

The cache stores only a collapsed status (`approve' / `deny' /
`aborted') for future hits in the same batch -- feedback text from
the original prompt is per-call and not replayed.

BUFFER is the chat buffer for buffer-local state (defaults to
current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((allowed-root
            (and (fboundp 'mevedel-workspace--file-in-allowed-roots-p)
                 (condition-case nil
                     (mevedel-workspace--file-in-allowed-roots-p
                      root (current-buffer))
                   (error nil))))
           (entry (assoc root mevedel--pending-access-requests
                         #'string=))
           (status (and entry (car (cdr entry)))))
      (pcase (if allowed-root 'approve status)
        ('approve
         (mevedel-tools--request-access--log
          (if allowed-root
              'request-access-bypassed
            'request-access-cache-hit)
          root reason (current-buffer)
          :outcome 'approve)
         (funcall callback 'approve))
        ((or 'deny 'aborted)
         (mevedel-tools--request-access--log
          'request-access-cache-hit root reason (current-buffer)
          :outcome status)
         (funcall callback status))
        ('pending
         ;; Append our callback to the waiters list -- first prompt's
         ;; resolution fans out the same outcome to all of us.
         (mevedel-tools--request-access--log
          'request-access-joined root reason (current-buffer))
         (setcdr entry (cons 'pending
                             (append (cdr (cdr entry))
                                     (list callback)))))
        (_
         ;; New request: install the entry and drive the prompt.
         (let ((new-entry (cons root (cons 'pending nil)))
               (chat-buf (current-buffer))
               interaction-id)
           (push new-entry mevedel--pending-access-requests)
           (let ((ov
                  (mevedel--prompt-user-for-access
                   root reason
                   (lambda (ui-outcome)
                     (when (buffer-live-p chat-buf)
                       (with-current-buffer chat-buf
                         (let ((entry
                                (assoc root
                                       mevedel--pending-access-requests
                                       #'string=))
                               (cached
                                (mevedel-tools--request-access--collapse
                                 ui-outcome)))
                           (mevedel-tools--request-access--log
                            'request-access-resolved
                            root reason chat-buf
                            :outcome ui-outcome
                            :collapsed cached
                            :interaction-id interaction-id)
                           (when (eq ui-outcome 'approve)
                             (mevedel-add-project-root root))
                           (when entry
                             (let ((waiters (cdr (cdr entry))))
                               (setcdr entry (cons cached nil))
                               ;; Fire the original caller, then any waiters,
                               ;; with the full UI outcome (preserves feedback
                               ;; text and the abort sentinel).
                               (ignore-errors
                                 (funcall callback ui-outcome))
                               (dolist (w waiters)
                                 (ignore-errors
                                   (funcall w ui-outcome))))))))))))
             (setq interaction-id
                   (and (overlayp ov)
                        (overlay-get ov 'mevedel-view-interaction-id)))
             (mevedel-tools--request-access--log
              'request-access-created root reason chat-buf
              :interaction-id interaction-id))))))))


;;
;;; Async prompt overlay primitive

(defvar-local mevedel--prompt-overlays nil
  "List of pending mevedel-user-request overlays in this buffer.
Each carries a `mevedel--callback' overlay property -- a one-arg
thunk receiving `approve' / `deny' / (feedback . TEXT) / `aborted'.")

(defvar-local mevedel--prompt-canceller-registered-for nil
  "The `mevedel-request' structs we registered dismiss cancellers onto.
Mirrors preview-mode's pattern: only the first overlay per request
pushes a canceller onto that request's cancellers list.")

(defun mevedel--prompt--data-buffer (&optional buffer)
  "Return the data buffer reachable from `current-buffer', else nil.
Prefer the `mevedel--data-buffer' back-pointer set on view and derived
buffers before accepting the current buffer.  View buffers also carry
`mevedel--session', but their agent registry and active request state
live on the data buffer.

When BUFFER is non-nil, resolve from that buffer instead of the
current one."
  (let* ((cur (or buffer (current-buffer)))
         (db (and (buffer-live-p cur)
                  (ignore-errors
                    (buffer-local-value 'mevedel--data-buffer cur)))))
    (or (and db (buffer-live-p db)
             (buffer-local-value 'mevedel--session db)
             db)
        (and (buffer-live-p cur)
             (ignore-errors
               (buffer-local-value 'mevedel--session cur))
             cur))))

(defun mevedel--prompt--registered-for-p (request)
  "Return non-nil when this buffer already registered REQUEST."
  (if (listp mevedel--prompt-canceller-registered-for)
      (memq request mevedel--prompt-canceller-registered-for)
    (eq request mevedel--prompt-canceller-registered-for)))

(defun mevedel--prompt--mark-registered-for (request)
  "Record that this buffer has registered a canceller for REQUEST."
  (unless (mevedel--prompt--registered-for-p request)
    (setq mevedel--prompt-canceller-registered-for
          (cons request
                (if (listp mevedel--prompt-canceller-registered-for)
                    mevedel--prompt-canceller-registered-for
                  (and mevedel--prompt-canceller-registered-for
                       (list mevedel--prompt-canceller-registered-for)))))))

(defun mevedel--prompt--register-canceller (&optional source-buffer overlay)
  "Push the prompt-dismiss thunk onto the active request's cancellers list.

Idempotent per request: subsequent overlays in the same request do
not push a duplicate.  Also installs `mevedel--prompt-dismiss-all' on
the buffer's `kill-buffer-hook' so killing the chat buffer settles
every pending overlay with `aborted'.

SOURCE-BUFFER, when non-nil, is used to find the owning request.
This matters when a sub-agent prompt is rendered in the parent view:
the overlay lives in the parent view, but the active request belongs
to the agent data buffer.  OVERLAY, when non-nil, is tagged with the
owning request so shared view buffers only cancel request-local
prompts during request teardown."
  (let ((prompt-buffer (current-buffer))
        (source-buffer (or source-buffer (current-buffer))))
    (when-let* ((data-buf (mevedel--prompt--data-buffer source-buffer))
                (request (buffer-local-value 'mevedel--current-request
                                             data-buf)))
      (with-current-buffer prompt-buffer
        (when (overlayp overlay)
          (overlay-put overlay 'mevedel--owning-request request))
        (unless (mevedel--prompt--registered-for-p request)
          (let ((buf prompt-buffer))
            (mevedel-request-push-canceller
             request
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (mevedel--prompt-dismiss-request request))))))
          (mevedel--prompt--mark-registered-for request)))))
  (add-hook 'kill-buffer-hook #'mevedel--prompt-dismiss-all nil t))

(defun mevedel--prompt--settle (overlay outcome)
  "Settle OVERLAY's callback exactly once with OUTCOME.

`mevedel-settled' overlay property gates this -- first call sets it
and proceeds; second call is a no-op (defense against duplicate
keypresses or aborts during user action).  Removes OVERLAY from the
buffer's pending list, deletes the overlay text/region so the user
sees it disappear, and finally fires the stored callback."
  (when (and (overlayp overlay)
             (not (overlay-get overlay 'mevedel-settled)))
    (overlay-put overlay 'mevedel-settled t)
    (let ((cb (overlay-get overlay 'mevedel--callback))
          (interaction-id (overlay-get overlay 'mevedel-view-interaction-id))
          (buf (overlay-buffer overlay))
          (s (overlay-start overlay))
          (e (overlay-end overlay)))
      (unless (buffer-live-p buf)
        (setq cb nil)
        (display-warning
         'mevedel
         "Stale interaction prompt activation ignored"
         :warning))
      (when (and interaction-id
                 (boundp 'mevedel-view--interaction-overlays)
                 (hash-table-p mevedel-view--interaction-overlays)
                 (not (eq overlay
                          (gethash interaction-id
                                   mevedel-view--interaction-overlays))))
        (setq cb nil)
        (display-warning
         'mevedel
         "Stale interaction prompt activation ignored"
         :warning))
      (when (buffer-live-p buf)
        (with-current-buffer buf
	  (setq mevedel--prompt-overlays
	        (delq overlay mevedel--prompt-overlays))
          (when (and interaction-id
                     (fboundp 'mevedel-view--interaction-unregister))
            (mevedel-view--interaction-unregister interaction-id))
          (let ((inhibit-read-only t))
            (delete-overlay overlay)
            (when (and (not interaction-id)
                       s e (>= e s) (not (eq s e)))
              (ignore-errors (delete-region s e))))))
      (when cb (funcall cb outcome)))))

(defun mevedel--prompt--overlay-at-point (property)
  "Return prompt overlay at point carrying PROPERTY.
Falls back to the `mevedel-view-interaction-overlay' text property
set on materialized interaction-zone descriptor text."
  (or (cdr (get-char-property-and-overlay (point) property))
      (let ((ov (get-text-property (point) 'mevedel-view-interaction-overlay)))
        (and (overlayp ov) (overlay-get ov property) ov))))

(defun mevedel--prompt-dismiss-request (request)
  "Settle pending prompt overlays owned by REQUEST with `aborted'."
  (let ((overlays (copy-sequence mevedel--prompt-overlays)))
    (dolist (ov overlays)
      (when (eq request (overlay-get ov 'mevedel--owning-request))
        (mevedel--prompt--settle ov 'aborted)))))

(defun mevedel--prompt-dismiss-all ()
  "Settle every pending prompt overlay in this buffer with `aborted'.

Drains the buffer's `mevedel--prompt-overlays' list; each overlay's
callback fires with `aborted' through `mevedel--prompt--settle'.

Used as the buffer-local `kill-buffer-hook' entry installed when the
first overlay is created, so killing the chat/view buffer settles
stranded callbacks and lets FSMs parked on a TOOL state advance out
via the tool callback.  Request-local teardown uses
`mevedel--prompt-dismiss-request' instead."
  (let ((overlays (copy-sequence mevedel--prompt-overlays)))
    (dolist (ov overlays)
      (mevedel--prompt--settle ov 'aborted))))

(defun mevedel--approve-request ()
  "Approve the prompt overlay at point."
  (interactive)
  (when-let* ((ov (mevedel--prompt--overlay-at-point 'mevedel-user-request)))
    (mevedel--prompt--settle ov 'approve)))

(defun mevedel--deny-request ()
  "Deny the prompt overlay at point.

Settles the overlay's callback with `deny'.  Does NOT call
`mevedel-abort' -- deny is a scoped per-tool outcome (the LLM sees
one failed tool call and may try alternatives), not a request-wide
teardown.  Earlier behavior tore down the whole request and is
no longer used."
  (interactive)
  (when-let* ((ov (mevedel--prompt--overlay-at-point 'mevedel-user-request)))
    (mevedel--prompt--settle ov 'deny)))

(defun mevedel--feedback-request ()
  "Settle the prompt overlay at point with feedback text."
  (interactive)
  (when-let* ((ov (mevedel--prompt--overlay-at-point 'mevedel-user-request)))
    (let ((feedback (read-string "What should be changed? ")))
      (mevedel--prompt--settle ov (cons 'feedback feedback)))))

(defun mevedel--prompt-framed-body (content face)
  "Return CONTENT inside the standard interaction prompt frame.
FACE is the face inherited by the top and bottom rule lines.  The
same background face used by permission prompts is applied to the
whole body so domain-specific prompts share one visual container."
  (let ((body
         (concat
          "\n"
          (propertize "\n" 'font-lock-face
                      `(:inherit ,face :underline t :extend t))
          content
          (propertize "\n" 'font-lock-face
                      `(:inherit ,face :underline t :extend t)))))
    (font-lock-append-text-property
     0 (length body) 'font-lock-face (gptel-agent--block-bg) body)
    body))

(defun mevedel--prompt-key (key)
  "Return propertized KEY for prompt key-help rows."
  (propertize key 'font-lock-face 'help-key-binding))

(defun mevedel--prompt-user-with-overlay
    (title content question help-echo-text callback)
  "Display a confirmation overlay; settle CALLBACK exactly once.

CALLBACK is invoked with one of:
  `approve'             user accepted
  `deny'                user denied (no abort, scoped per-tool)
  (feedback . TEXT)     user provided feedback (treated as denial
                        by callers that map this to a scoped error)
  `aborted'             primitive torn down via the request's
                        cancellers list or the chat-buffer kill hook

Multiple concurrent calls produce multiple independent overlays that
settle in user-chosen order; no `recursive-edit', no nesting, no
queue serialization.  The first overlay per request registers a
dismiss thunk onto the request's cancellers list and a
`kill-buffer-hook' entry; subsequent overlays in the same request
piggyback on those registrations.

TITLE is the heading text (bold + warning).  CONTENT is the body
describing the request.  QUESTION is the bold final question.
HELP-ECHO-TEXT is optional hover text."
  (let* ((source-buffer (current-buffer))
         (target-buf
	          (if (fboundp 'mevedel-view--interaction-target-buffer)
	              (mevedel-view--interaction-target-buffer
	               (mevedel--prompt--data-buffer source-buffer))
	            (error "No live view for queued prompt")))
         (id (list :request (gensym "request-")))
         (body
          (mevedel--prompt-framed-body
           (concat
            (propertize (format "%s\n" title)
                        'font-lock-face '(:inherit bold :inherit warning))
            "\n"
            content
            "\n\n"
            (propertize (format "%s\n\n" question) 'font-lock-face 'bold)
            (propertize "Keys: " 'font-lock-face 'help-key-binding)
            (mevedel--prompt-key "a")
            " approve  "
            (mevedel--prompt-key "d")
            " deny  "
            (mevedel--prompt-key "f")
            " feedback\n")
           'warning))
         (keymap
          (define-keymap
            "y"        #'mevedel--approve-request
            "a"        #'mevedel--approve-request
            "RET"      #'mevedel--approve-request
            "<return>" #'mevedel--approve-request
            "C-c C-c"  #'mevedel--approve-request
            "n"        #'mevedel--deny-request
            "d"        #'mevedel--deny-request
            "q"        #'mevedel--deny-request
            "C-c C-k"  #'mevedel--deny-request
            "C-g"      #'mevedel--deny-request
            "f"        #'mevedel--feedback-request))
         ov)
    (with-current-buffer target-buf
      (setq ov
            (mevedel-view--interaction-register
             (list :kind 'request
                   :id id
                   :count 0
                   :body body
                   :priority 150
                   :keymap keymap
                   :help-echo
                   (or help-echo-text
                       (concat title ": "
                               (propertize "Keys: C-c C-c approve  \
C-c C-k deny  f feedback"
                                           'face 'help-key-binding)))
                   :activate callback)))
      (overlay-put ov 'mevedel-user-request t)
      (overlay-put ov 'mevedel--callback callback)
      (cl-pushnew ov mevedel--prompt-overlays :test #'eq)
      (mevedel--prompt--register-canceller source-buffer ov))
    ov))

(defun mevedel--prompt-user-for-access (root reason callback)
  "Display access prompt for ROOT and REASON; deliver outcome to CALLBACK.

CALLBACK receives the bare overlay outcome: `approve', `deny', a
feedback cons, or `aborted'.  The caller is responsible for mapping that
to its tool-result string and any rule storage.

Used by `mevedel-tools--request-access' to drive the dedup wrapper --
non-grant outcomes (deny, feedback, abort) all collapse to \"not
granted\" at that layer."
  (let ((content (concat
                  "The LLM is requesting access to a directory outside the current workspace.\n\n"
                  (propertize "Directory: "
                              'font-lock-face 'font-lock-escape-face)
                  (propertize (format "%s\n" root)
                              'font-lock-face 'font-lock-constant-face)
                  (propertize "Reason: "
                              'font-lock-face 'font-lock-escape-face)
                  (format "%s" reason))))
    (mevedel--prompt-user-with-overlay
     "Directory Access Request"
     content
     "Grant access to this directory?"
     (concat "Directory access request: "
             (propertize "Keys: C-c C-c approve  C-c C-k deny  f feedback"
                         'face 'help-key-binding))
     callback)))

(defun mevedel--clear-pending-access-requests (&rest _)
  "Clear the pending access requests cache.
Should be called after each LLM response completes."
  (setq mevedel--pending-access-requests nil))

(defun mevedel-tools--request-access--format-result (path ui-outcome)
  "Translate UI-OUTCOME into the LLM-facing tool-result string for PATH.

`approve'              -> grant string.
`deny'                 -> \"Access denied to PATH...\".
`(feedback . TEXT)'    -> denial string with the user's feedback.
`aborted'              -> \"Error: aborted\" (canceller path).
Anything else collapses to a plain denial string."
  (pcase ui-outcome
    ('approve
     (format "Access granted to %s. You can now read and write files in this directory."
             path))
    ('aborted "Error: aborted")
    (`(feedback . ,text)
     (format "Access denied to %s. Feedback: %s" path text))
    ('deny
     (format "Access denied to %s. You cannot access files in this directory."
             path))
    (_
     (format "Access denied to %s. You cannot access files in this directory."
             path))))

(cl-defun mevedel--tools-request-dir-access (callback directory reason)
  "Request user permission to access a directory.

CALLBACK is the tool's async callback; receives a tool-result string.
DIRECTORY is the path to grant access to; REASON explains why.
Routes the user's choice through the dedup wrapper
`mevedel-tools--request-access' and overlay prompt
`mevedel--prompt-user-for-access'.  Feedback denials carry the user's
text into the LLM-visible result; canceller teardown produces
`\"Error: aborted\"' so a parked sub-agent FSM can
advance out of TOOL."
  (mevedel-tools--validate-params callback mevedel--tools-request-dir-access
    (directory stringp)
    (reason stringp))
  (unless (and (file-readable-p directory) (file-directory-p directory))
    (cl-return-from mevedel--tools-request-dir-access
      (funcall callback
               (format "Error: directory '%s' is not readable" directory))))
  (let ((expanded (expand-file-name directory)))
    (mevedel-tools--request-access
     expanded reason
     (lambda (ui-outcome)
       (funcall callback
                (mevedel-tools--request-access--format-result
                 expanded ui-outcome))))))


;;
;;; Agent tool

(defcustom mevedel-tool-ui-agent-description-width 96
  "Maximum display width for the task text in Agent handle headers.
Agent prompts can be long; handles keep the status zone scannable by
showing a single normalized line and leaving the full task text in the
agent transcript."
  :type 'integer
  :group 'mevedel)

(defun mevedel-tool-ui--compact-agent-description (description &optional width)
  "Return DESCRIPTION normalized to one truncated display line.
WIDTH defaults to `mevedel-tool-ui-agent-description-width'."
  (let ((text (string-trim
               (replace-regexp-in-string
                "[\n\r\t ]+" " " (or description ""))))
        (width (or width mevedel-tool-ui-agent-description-width)))
    (cond
     ((<= width 0) "")
     ((<= width (string-width "..."))
      (truncate-string-to-width "..." width))
     (t
      (truncate-string-to-width text width nil nil "...")))))

(defun mevedel-tool-ui--handle-badge (render-data)
  "Return a propertized state-badge string for RENDER-DATA, or empty.
Maps `:status' to a visible badge with an appropriate face."
  (let* ((status (plist-get render-data :status))
         (blocked-reason (plist-get render-data :blocked-reason))
         (calls (plist-get render-data :calls))
         (elapsed (plist-get render-data :elapsed))
         (reason (plist-get render-data :reason))
         (verdict (plist-get render-data :verdict))
         ;; Suppress meaningless zeros when underlying data is
         ;; absent.  `:elapsed' is omitted from render-data when
         ;; `started-at' was missing; treat 0/nil identically.
         (calls-suffix (if (and calls (> calls 0))
                           (format " · %d calls" calls)
                         ""))
         (elapsed-suffix (if (and elapsed (> elapsed 0))
                             (format " · %.1fs" elapsed)
                           "")))
    (if blocked-reason
        (propertize (format "[blocked · awaiting %s]" blocked-reason)
                    'font-lock-face 'mevedel-view-handle-blocked)
      (pcase status
        ('running
         (propertize (format "[running%s]" calls-suffix)
                     'font-lock-face 'mevedel-view-handle-running))
        ('completed
         (pcase verdict
           ('fail
            (propertize (format "✗ verdict FAIL%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-error))
           ('partial
            (propertize (format "○ verdict PARTIAL%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-error))
           ('pass
            (propertize (format "✓ verdict PASS%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-done))
           (_
            (propertize (format "✓ done%s%s" elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-done))))
        ('error
         (propertize (format "✗ error%s"
                             (if reason (format " · %s" reason) ""))
                     'font-lock-face 'mevedel-view-handle-error))
        ('aborted
         (propertize "✗ aborted"
                     'font-lock-face 'mevedel-view-handle-error))
        ('incomplete
         (propertize "○ incomplete"
                     'font-lock-face 'mevedel-view-handle-error))
        (_ "")))))

(defun mevedel-tool-ui--agent-blocked-reason (agent-id session)
  "Return the visible blocked reason for AGENT-ID in SESSION, or nil."
  (when (and agent-id session)
    (cond
     ((cl-some (lambda (entry)
                 (equal (plist-get entry :origin) agent-id))
               (mevedel-session-permission-queue session))
      "permission")
     ((cl-some (lambda (entry)
                 (equal (plist-get entry :origin) agent-id))
               (mevedel-session-plan-queue session))
      "plan"))))


;;
;;; Ask User

(defconst mevedel-tools--ask-recommended-suffix " (Recommended)"
  "Suffix marking a recommended Ask option.")

(defun mevedel-tools--ask-option-field (option key)
  "Return OPTION field KEY from supported option object shapes."
  (let ((string-key (substring (symbol-name key) 1)))
    (cond
     ((hash-table-p option)
      (or (gethash key option)
          (gethash string-key option)
          (gethash (intern string-key) option)))
     ((and (listp option) (plist-member option key))
      (plist-get option key))
     ((listp option)
      (or (cdr (assq key option))
          (cdr (assq (intern string-key) option))
          (cdr (assoc string-key option)))))))

(defun mevedel-tools--ask-option-label (option)
  "Return the answer label for OPTION."
  (let ((label (if (stringp option)
                   option
                 (mevedel-tools--ask-option-field option :label))))
    (cond
     ((stringp label) label)
     (label (format "%s" label))
     (t (format "%s" option)))))

(defun mevedel-tools--ask-option-description (option)
  "Return OPTION's description string, or nil."
  (let ((description
         (and (not (stringp option))
              (mevedel-tools--ask-option-field option :description))))
    (cond
     ((and (stringp description)
           (not (string-blank-p description)))
      description)
     (description (format "%s" description)))))

(defun mevedel-tools--ask-option-preview (option)
  "Return OPTION's preview string, or nil."
  (let ((preview (and (not (stringp option))
                      (mevedel-tools--ask-option-field option :preview))))
    (cond
     ((and (stringp preview)
           (not (string-blank-p preview)))
      preview)
     (preview (format "%s" preview)))))

(defun mevedel-tools--ask-option-labels (options)
  "Return display labels for OPTIONS."
  (mapcar #'mevedel-tools--ask-option-label options))

(defun mevedel-tools--ask-completion-table (choices)
  "Return a completion table that preserves CHOICES order."
  (let ((choices (copy-sequence choices)))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          '(metadata
            (category . mevedel-ask)
            (display-sort-function . identity)
            (cycle-sort-function . identity))
        (complete-with-action action choices string predicate)))))

(defun mevedel-tools--ask-option-by-label (label options)
  "Return first option in OPTIONS whose label equals LABEL."
  (cl-find label options
           :test #'equal
           :key #'mevedel-tools--ask-option-label))

(defun mevedel-tools--ask-format-option (option)
  "Return OPTION formatted for display in an Ask prompt."
  (let ((label (mevedel-tools--ask-option-label option)))
    (if (string-suffix-p mevedel-tools--ask-recommended-suffix label)
        (let ((base (substring label 0
                               (- (length label)
                                  (length mevedel-tools--ask-recommended-suffix)))))
          (concat base
                  (propertize mevedel-tools--ask-recommended-suffix
                              'font-lock-face 'success)))
      label)))

(defun mevedel-tools--ask-format-option-line (option)
  "Return a rendered option line for OPTION."
  (let ((description (mevedel-tools--ask-option-description option)))
    (concat
     (format "  - %s" (mevedel-tools--ask-format-option option))
     (when description
       (concat "\n    "
               (propertize description 'font-lock-face 'shadow))))))

(defun mevedel-tools--ask-format-preview (preview)
  "Return a rendered PREVIEW block."
  (when (and (stringp preview)
             (not (string-blank-p preview)))
    (concat
     (propertize "Preview:\n" 'font-lock-face 'font-lock-constant-face)
     (mapconcat (lambda (line) (concat "    " line))
                (split-string preview "\n")
                "\n")
     "\n\n")))

(defun mevedel-tools--ask-format-selected-preview (answer options)
  "Return the preview block for ANSWER selected from OPTIONS."
  (when-let* ((option (mevedel-tools--ask-option-by-label answer options))
              (preview (mevedel-tools--ask-option-preview option)))
    (mevedel-tools--ask-format-preview preview)))

(cl-defun mevedel-tools--ask-user (callback questions)
  "Ask user multiple questions with navigation support using overlays.

CALLBACK is the async callback function to call with results.
QUESTIONS is an array of question plists, each with :question and :options keys."
  (mevedel-tools--validate-params callback mevedel-tools--ask-user
    (questions (vectorp . "array")))

	  (let* ((source-buffer (current-buffer))
	         (questions-list (append questions nil)) ; Convert vector to list
	         (answers (make-vector (length questions-list) nil))
	         (chat-buffer
	          (if (fboundp 'mevedel-view--interaction-target-buffer)
	              (mevedel-view--interaction-target-buffer
	               (with-current-buffer source-buffer
	                 (mevedel--prompt--data-buffer)))
	            (error "No live view for Ask prompt")))
	         (interaction-id (list :ask (gensym "ask-")))
	         (overlay nil)
	         (current-index 0))

    (cl-labels
        ((answer-question
           ()
           "Prompt user to answer current question."
           (let* ((q (nth current-index questions-list))
                  (question-text (plist-get q :question))
                  (options (append (plist-get q :options) nil))
                  (all-choices (append (mevedel-tools--ask-option-labels
                                        options)
                                       '("Custom input")))
                  (prev-answer (aref answers current-index))
                  (choice (completing-read
                           (format "[Q%d/%d] %s: "
                                   (1+ current-index)
                                   (length questions-list)
                                   question-text)
                           (mevedel-tools--ask-completion-table
                            all-choices)
                           nil nil
                           prev-answer))
                  (answer (if (equal choice "Custom input")
                              (read-string (concat question-text " (custom): ")
                                           prev-answer)
                            choice)))
             (aset answers current-index answer)
             (cycle-forward)))

         (cycle-forward
           ()
           "Cycle to next question or confirmation screen."
           (interactive)
           (if (eq current-index 'confirm)
               ;; From confirm, go to first question
               (progn
                 (setq current-index 0)
                 (update-overlay current-index))
             ;; From a question
             (if (< current-index (1- (length questions-list)))
                 ;; Go to next question
                 (progn
                   (setq current-index (1+ current-index))
                   (update-overlay current-index))
               ;; At last question, go to confirmation
               (progn
                 (setq current-index 'confirm)
                 (show-confirmation)))))

         (cycle-backward
           ()
           "Cycle to previous question or confirmation screen."
           (interactive)
           (if (eq current-index 'confirm)
               ;; From confirm, go to last question
               (progn
                 (setq current-index (1- (length questions-list)))
                 (update-overlay current-index))
             ;; From a question
             (if (> current-index 0)
                 ;; Go to previous question
                 (progn
                   (setq current-index (1- current-index))
                   (update-overlay current-index))
               ;; At first question, go to confirmation
               (progn
                 (setq current-index 'confirm)
                 (show-confirmation)))))

         (edit-answer
           ()
           "Edit current question's answer."
           (interactive)
           (answer-question))

         (confirm-all
           ()
           "Skip to confirmation screen."
           (interactive)
           (setq current-index 'confirm)
           (show-confirmation))

         (quit-questionnaire
           ()
           "Cancel questionnaire and abort execution."
           (interactive)
           (when overlay
             (when (fboundp 'mevedel-view--interaction-unregister)
               (mevedel-view--interaction-unregister interaction-id))
             (delete-overlay overlay))
           (mevedel-abort))  ; Abort entire execution

         (ask-keymap
           (&optional confirm)
           "Return keymap for the Ask prompt.
When CONFIRM is non-nil, bind submit/edit commands for the review screen."
           (let ((keymap (make-sparse-keymap)))
             (define-key keymap (kbd "TAB") #'cycle-forward)
             (define-key keymap (kbd "<tab>") #'cycle-forward)
             (define-key keymap (kbd "S-TAB") #'cycle-backward)
             (define-key keymap (kbd "<backtab>") #'cycle-backward)
             (define-key keymap (kbd "RET")
                         (if confirm #'submit-answers #'edit-answer))
             (define-key keymap (kbd "<return>")
                         (if confirm #'submit-answers #'edit-answer))
             (when confirm
               (define-key keymap (kbd "C-c C-c") #'submit-answers)
               (define-key keymap (kbd "C-c C-e")
                           #'edit-specific-question)
               (define-key keymap (kbd "e") #'edit-specific-question))
             (define-key keymap (kbd "C-c C-k") #'quit-questionnaire)
             (define-key keymap (kbd "q") #'quit-questionnaire)
             (define-key keymap (kbd "C-g") #'quit-questionnaire)
             keymap))

         (render-ask-body
           (body keymap)
           "Render Ask BODY with KEYMAP through the interaction painter."
           (with-current-buffer chat-buffer
             (setq overlay
                   (mevedel-view--interaction-register
                    (list :kind 'ask
                          :id interaction-id
                          :count 0
                          :body body
                          :priority 150
                          :keymap keymap
                          :help-echo "Ask prompt")))
             (overlay-put overlay 'mevedel-user-request t)
             (overlay-put overlay 'mevedel--callback callback)
             (cl-pushnew overlay mevedel--prompt-overlays :test #'eq)
             (mevedel--prompt--register-canceller source-buffer overlay)))

         (update-overlay
           (index)
           "Update overlay to show question at INDEX."
           (let* ((q (nth index questions-list))
                  (question-text (plist-get q :question))
                  (options (append (plist-get q :options) nil))
                  (prev-answer (aref answers index))
                  (body
                   (concat
                    "\n"
                    (propertize (format "Question %d/%d"
                                        (1+ index)
                                        (length questions-list))
                                'font-lock-face 'font-lock-string-face)
                    (propertize "\n" 'font-lock-face
                                '(:inherit font-lock-string-face
                                  :underline t :extend t))
                    "\n"
                    (propertize question-text
                                'font-lock-face 'font-lock-escape-face)
                    "\n\n"
                    (propertize "Available options:\n"
                                'font-lock-face
                                'font-lock-constant-face)
                    (mapconcat
                     (lambda (opt)
                       (mevedel-tools--ask-format-option-line opt))
                     options "\n")
                    "\n  - Custom input\n\n"
                    (when prev-answer
                      (concat
                       (propertize "Current answer: "
                                   'font-lock-face 'warning)
                       (propertize prev-answer 'font-lock-face 'bold)
                       "\n"
                       (or (mevedel-tools--ask-format-selected-preview
                            prev-answer options)
                           "\n")))
                    (propertize "Keys: "
                                'font-lock-face 'help-key-binding)
                    (propertize "TAB"
                                'font-lock-face 'help-key-binding)
                    " cycle  "
                    (propertize "RET"
                                'font-lock-face 'help-key-binding)
                    " answer next  "
                    (propertize "q"
                                'font-lock-face 'help-key-binding)
                    " cancel\n"
                    (propertize "\n" 'font-lock-face
                                '(:inherit font-lock-string-face
                                  :underline t :extend t)))))
             (render-ask-body body (ask-keymap))))

         (submit-answers
           ()
           "Submit all answers to LLM."
           (interactive)
           (let ((result (with-temp-buffer
                           (insert "User answered the following questions:\n\n")
                           (dotimes (i (length questions-list))
                             (let ((q (nth i questions-list))
                                   (a (aref answers i)))
                               (insert (format "Q%d: %s\n" (1+ i) (plist-get q :question)))
                               (insert (format "A%d: %s\n\n" (1+ i) a))))
                           (buffer-string))))
             (cleanup-and-return result)))

         (edit-specific-question
           ()
           "Edit a specific question by number."
           (interactive)
           (let* ((default-qnum (if (eq current-index 'confirm) 1 (1+ current-index)))
                  (qnum (read-number "Edit question number: "
                                     default-qnum)))
             (when (and (>= qnum 1) (<= qnum (length questions-list)))
               (setq current-index (1- qnum))
               (update-overlay current-index))))

         (show-confirmation
           ()
           "Show all answers in overlay and ask for final confirmation."
           (let ((body
                  (concat
                   "\n"
                   (propertize "Review Your Answers"
                               'font-lock-face 'font-lock-string-face)
                   (propertize "\n" 'font-lock-face
                               '(:inherit font-lock-string-face
                                 :underline t :extend t))
                   "\n"
                   (mapconcat
                    (lambda (i)
                      (let ((q (nth i questions-list))
                            (a (aref answers i)))
                        (concat
                         (propertize (format "%d. " (1+ i))
                                     'font-lock-face 'bold)
                         (plist-get q :question)
                         "\n"
                         (propertize "   -> "
                                     'font-lock-face 'shadow)
                         (if a
                             (concat
                              (propertize a 'font-lock-face 'success)
                              (when-let* ((preview
                                           (mevedel-tools--ask-format-selected-preview
                                            a (append (plist-get q :options) nil))))
                                (concat "\n" preview)))
                           (propertize "(not answered)"
                                       'font-lock-face 'shadow)))))
                    (number-sequence 0 (1- (length questions-list)))
                    "\n\n")
                   "\n\n"
                   (propertize "Keys: "
                               'font-lock-face 'help-key-binding)
                   (propertize "TAB"
                               'font-lock-face 'help-key-binding)
                   " cycle  "
                   (propertize "RET"
                               'font-lock-face 'help-key-binding)
                   " submit  "
                   (propertize "e"
                               'font-lock-face 'help-key-binding)
                   " edit  "
                   (propertize "q"
                               'font-lock-face 'help-key-binding)
                   " cancel\n"
                   (propertize "\n" 'font-lock-face
                               '(:inherit font-lock-string-face
                                 :underline t :extend t)))))
             (render-ask-body body (ask-keymap t))))

         (cleanup-and-return
           (result)
           "Clean up overlay and return RESULT."
           (when overlay
             (when (fboundp 'mevedel-view--interaction-unregister)
               (mevedel-view--interaction-unregister interaction-id))
             (delete-overlay overlay))
           (funcall callback result)))

      ;; Start the questionnaire - show first question
      (update-overlay 0))))


;;
;;; Pipeline-compatible handlers

(defun mevedel-tool-ui--deliver-result (callback value &rest args)
  "Deliver VALUE to CALLBACK as a canonical tool result."
  (apply callback
         (if (and (proper-list-p value) (plist-member value :result))
             value
           (list :result value))
         args))

(defun mevedel-tool-ui--ask (callback args)
  "Ask the user questions.
CALLBACK receives the formatted answers.  ARGS is a plist with :questions."
  (let ((questions (plist-get args :questions)))
    (unless questions
      (error "Parameter questions is required"))
    (mevedel-tools--ask-user
     (apply-partially #'mevedel-tool-ui--deliver-result callback)
     questions)))

(defun mevedel-tool-ui--request-access (callback args)
  "Request access to a directory outside the workspace.
CALLBACK receives the result.  ARGS is a plist with :directory and :reason."
  (let ((directory (plist-get args :directory))
        (reason (plist-get args :reason)))
    (unless (stringp directory)
      (error "Parameter directory is required"))
    (unless (stringp reason)
      (error "Parameter reason is required"))
    (if (not (and (file-readable-p directory) (file-directory-p directory)))
        (mevedel-tool-ui--deliver-result
         callback
         (format "Error: Directory '%s' is not readable" directory))
      (let ((expanded (expand-file-name directory)))
        (mevedel-tools--request-access
         expanded reason
         (lambda (ui-outcome)
           (mevedel-tool-ui--deliver-result
            callback
            (mevedel-tools--request-access--format-result
             expanded ui-outcome))))))))

(defun mevedel-tool-ui--agent (callback args)
  "Launch a specialized agent.
CALLBACK receives the agent result.  ARGS is a plist with :subagent_type,
:description, :prompt, optional :run_in_background, and optional :model."
  (let ((agent-type (plist-get args :subagent_type))
        (description (plist-get args :description))
        (prompt (plist-get args :prompt))
        (model (plist-get args :model))
        (background (mevedel-tool-truthy-p
                     (plist-get args :run_in_background))))
    (unless (stringp agent-type)
      (error "Parameter subagent_type is required"))
    (unless (stringp description)
      (error "Parameter description is required"))
    (unless (stringp prompt)
      (error "Parameter prompt is required"))
    (when (and model (not (mevedel-model-normalize-tier model)))
      (mevedel-tool-ui--deliver-result
       callback
       (format "Error: Unknown model tier: %s. Available: fast, balanced, strong"
               model))
      (setq agent-type nil))
    (when agent-type
      (require 'mevedel-agent-runtime)
      (when (equal agent-type "verifier")
        (mevedel-plan-mode-clear-verification-pending))
      (if-let* ((agent (mevedel-agent-get agent-type)))
          (mevedel-agent-runtime-dispatch
           (apply-partially #'mevedel-tool-ui--deliver-result callback)
           agent description prompt
           :background background
           :parent-context (mevedel-tools--current-deferred-context)
           :parent-fsm mevedel-tools--current-fsm
           :message-handler #'mevedel-tools--handle-message-inject
           :terminal-handler #'mevedel-tools--handle-terminal-mailbox
           :model-tier (and model (mevedel-model-normalize-tier model)))
        (mevedel-tool-ui--deliver-result
         callback
         (format "Error: Unknown agent type: %s" agent-type))))))

(defun mevedel-tool-ui--stop-agent (args)
  "Stop a running sub-agent.
ARGS is a plist with :agent_id and optional :reason."
  (require 'mevedel-agent-runtime)
  (let ((agent-id (plist-get args :agent_id))
        (reason (plist-get args :reason)))
    (unless (stringp agent-id)
      (error "Parameter agent_id is required"))
    (list :result
          (mevedel-agent-runtime--stop-agent-result-string
           (mevedel-agent-runtime-stop agent-id reason)))))

(defun mevedel-tool-ui--tool-search (callback args)
  "Search for and load deferred tools.
CALLBACK receives the search results.  ARGS is a plist with :query
and optional :load."
  (let ((query (plist-get args :query))
        (load (plist-get args :load)))
    (unless (stringp query)
      (error "Parameter query is required"))
    (mevedel-tools--tool-search
     (apply-partially #'mevedel-tool-ui--deliver-result callback)
     query load)))

(defun mevedel-tool-ui--send-message (args)
  "Dispatch SendMessage to the mevedel-tools implementation.
ARGS is a plist with :to and :message."
  (list :result (mevedel-tools--send-message args)))


;;
;;; Renderers

(defun mevedel-tool-ui--render-agent (name args result render-data)
  "Return rendering plist for Agent NAME, ARGS, RESULT, and RENDER-DATA.
Header shows the subagent type, its short task description, the
state badge (running / done / error / aborted / incomplete),
and -- when RENDER-DATA carries transcript metadata that passes path
hygiene -- a clickable transcript-open button.  Body fontifies in
the data buffer's major mode."
  (when (stringp result)
    (let* ((agent-id (and (consp render-data)
                          (plist-get render-data :agent-id)))
           (session (and (boundp 'mevedel--session) mevedel--session))
           (sidecar-entry
            (and agent-id session
                 (cdr (assoc agent-id
                             (mevedel-session-agent-transcripts
                              session)))))
           (effective-render-data
            (if sidecar-entry
                (append (list :status (plist-get sidecar-entry :status)
                              :transcript-relative-path
                              (plist-get sidecar-entry :path))
                        render-data)
              render-data))
           (blocked-reason
            (and (eq (plist-get effective-render-data :status) 'running)
                 (mevedel-tool-ui--agent-blocked-reason
                  agent-id session)))
           (progress-p (plist-get effective-render-data :progress-handle))
           (agent-type (or (plist-get args :subagent_type) "?"))
           ;; Empty when render-data lacks a recognized :status.
           (badge (mevedel-tool-ui--handle-badge
                   (if blocked-reason
                       (plist-put (copy-sequence effective-render-data)
                                  :blocked-reason blocked-reason)
                     effective-render-data)))
           (badge-suffix (if (string-empty-p badge) "" (concat "  " badge)))
           (description (or (plist-get args :description) ""))
           (header-width (plist-get effective-render-data :header-width))
           (description-width
            (when header-width
              (let* ((base (if progress-p "" (format "%s -- " agent-type)))
                     (fixed (format "%s: %s%s"
                                    (or name "Agent") base
                                    badge-suffix)))
                (max 0 (- header-width (string-width fixed))))))
           (compact-description
            (mevedel-tool-ui--compact-agent-description
             description description-width))
           (shown (cond
                   ((and progress-p (not (string-empty-p compact-description)))
                    compact-description)
                   ((string-empty-p compact-description)
                    agent-type)
                   (t
                    (format "%s -- %s" agent-type compact-description)))))
      (list :header (format "%s: %s%s"
                            (or name "Agent") shown
                            badge-suffix)
            :body result
            :body-mode (mevedel-view-data-buffer-major-mode)
            :vtype 'agent-handle
            :agent-id agent-id
            :agent-status (plist-get effective-render-data :status)
            :agent-description description
            :hook-audits (plist-get effective-render-data :hook-audits)
            :initially-collapsed-p t))))

(defun mevedel-tool-ui--result-status (result)
  "Return a renderer status for RESULT."
  (and (stringp result)
       (or (string-prefix-p "Error:" result)
           (string-prefix-p "Access denied" result))
       'error))

(defun mevedel-tool-ui--line-count (result)
  "Return non-empty line count for RESULT."
  (if (stringp result)
      (length (split-string result "\n" t))
    0))

(defun mevedel-tool-ui--question-count (questions)
  "Return the number of QUESTIONS in an Ask call."
  (cond
   ((vectorp questions) (length questions))
   ((listp questions) (length questions))
   (questions 1)
   (t 0)))

(defun mevedel-tool-ui--render-ask (name args result _render-data)
  "Return rendering plist for Ask NAME, ARGS, and RESULT."
  (when (stringp result)
    (let ((count (mevedel-tool-ui--question-count
                  (plist-get args :questions))))
      (list :header (format "%s: %d %s"
                            (or name "Ask")
                            count
                            (if (= count 1) "question" "questions"))
            :body result
            :body-mode nil
            :status (mevedel-tool-ui--result-status result)
            :initially-collapsed-p t))))

(defun mevedel-tool-ui--render-request-access (name args result _render-data)
  "Return rendering plist for RequestAccess NAME, ARGS, and RESULT."
  (when (stringp result)
    (let* ((directory (or (plist-get args :directory) "?"))
           (status (cond
                    ((string-prefix-p "Access granted" result) "granted")
                    ((string-prefix-p "Access denied" result) "denied")
                    ((string-prefix-p "Error:" result) "error")
                    (t "done"))))
      (list :header (format "%s: %s (%s)"
                            (or name "RequestAccess") directory status)
            :body result
            :body-mode nil
            :status (mevedel-tool-ui--result-status result)
            :initially-collapsed-p t))))

(defun mevedel-tool-ui--render-stop-agent (name args result _render-data)
  "Return compact rendering plist for StopAgent NAME, ARGS, and RESULT."
  (when (stringp result)
    (let ((agent-id (or (plist-get args :agent_id) "?")))
      (list :header (format "%s: %s"
                            (or name "StopAgent") agent-id)
            :status (mevedel-tool-ui--result-status result)
            :expandable-p nil))))

(defun mevedel-tool-ui--render-tool-search (name args result _render-data)
  "Return rendering plist for ToolSearch NAME, ARGS, and RESULT."
  (when (stringp result)
    (let* ((query (or (plist-get args :query) ""))
           (load (mevedel-tool-truthy-p (plist-get args :load)))
           (count (mevedel-tool-ui--line-count result)))
      (list :header (format "%s: %s (%s, %d %s)"
                            (or name "ToolSearch")
                            query
                            (if load "load" "search")
                            count
                            (if (= count 1) "line" "lines"))
            :body result
            :body-mode nil
            :status (mevedel-tool-ui--result-status result)
            :initially-collapsed-p t))))

(defun mevedel-tool-ui--render-send-message (name args result _render-data)
  "Return compact rendering plist for SendMessage NAME, ARGS, and RESULT."
  (when (stringp result)
    (let ((to (or (plist-get args :to) "?")))
      (list :header (format "%s: %s"
                            (or name "SendMessage") to)
            :status (mevedel-tool-ui--result-status result)
            :expandable-p nil))))


;;
;;; Register Tools

(defun mevedel-tool-ui--register ()
  "Register user interaction tools."

  (mevedel-define-tool
    :name "Ask"
    :description "Ask the user one or more questions and wait for their responses."
    :prompt-file "tools/ask.md"
    :handler #'mevedel-tool-ui--ask
    :args ((questions array :required
                      "Array of question objects. Each question must have predefined answer options. Options may be strings or objects with label, description, and preview fields. Mark exactly one option per question by appending ` (Recommended)` to that option label."
                      :items (:type object)
                      :minItems 1))
    :async-p t
    :max-result-size 30000
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-ui--render-ask)

  (mevedel-define-tool
    :name "RequestAccess"
    :description "Request access to a directory outside the current allowed project roots."
    :prompt-file "tools/requestaccess.md"
    :handler #'mevedel-tool-ui--request-access
    :args ((directory path :required
                      "Absolute or relative path to the directory you need to access.")
           (reason string :required
                   "Clear explanation of why you need access to this directory."))
    :async-p t
    :groups (util)
    ;; RequestAccess is itself the user-facing permission prompt for
    ;; expanding workspace access.  Let the tool run so its dedicated
    ;; directory-access UI is the only normal prompt; explicit deny
    ;; rules and protected-path checks still run before this slot.
    :check-permission (lambda (_tool _args) 'allow)
    :get-path (lambda (args) (plist-get args :directory))
    :read-only-p t
    :renderer #'mevedel-tool-ui--render-request-access)

  (mevedel-define-tool
    :name "Agent"
    :description "Launch a specialized agent to handle complex, multi-step tasks autonomously."
    :prompt-file "tools/agent.md"
    :handler #'mevedel-tool-ui--agent
    :args ((subagent_type string :required
                          "The type of specialized agent to use for this task.")
           (description string :required
                        "A short (3-5 word) description of the task.")
           (prompt string :required
                   "The detailed task for the agent to perform autonomously.")
           (model string :optional
                  "Optional model tier for this agent invocation."
                  :enum ["fast" "balanced" "strong"])
           (run_in_background boolean :optional
                              "Set to true to run this agent in the background. The tool returns immediately with the agent ID; the agent's result is delivered to your mailbox when it finishes."))
    :async-p t
    :max-result-size 50000
    :groups (util)
    :get-name (lambda (args) (plist-get args :subagent_type))
    :read-only-p t
    :renderer #'mevedel-tool-ui--render-agent)

  (mevedel-define-tool
    :name "StopAgent"
    :description "Stop a running sub-agent owned by the current session."
    :prompt-file "tools/stopagent.md"
    :handler #'mevedel-tool-ui--stop-agent
    :args ((agent_id string :required
                     "Full agent id, or an unambiguous displayed short id such as reviewer--73512314.")
           (reason string :optional
                   "Short reason for stopping the agent."))
    :groups (util)
    :read-only-p t
    :renderer #'mevedel-tool-ui--render-stop-agent)

  (mevedel-define-tool
    :name "ToolSearch"
    :description "Search for and load deferred tools before using them."
    :prompt-file "tools/toolsearch.md"
    :handler #'mevedel-tool-ui--tool-search
    :args ((query string :required
                  "Search query: tool name or capability description.")
	   (load boolean :optional
		 "Set true when you intend to call the matched tool; it becomes available now for your next tool call."))
    :async-p t
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-ui--render-tool-search)

  (mevedel-define-tool
    :name "SendMessage"
    :description "Send an asynchronous message to a running agent or back to the main chat."
    :prompt-file "tools/sendmessage.md"
    :handler #'mevedel-tool-ui--send-message
    :args ((to string :required
               "Recipient: exact agent id, \"main\", or \"coordinator\".")
           (message string :required
                    "Message body to deliver."))
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-ui--render-send-message))


;;
;;; Permission prompt

(defun mevedel-permission--prompt-self-insert ()
  "Insert the typed permission key when no permission prompt is active."
  (when (and (characterp last-command-event)
             (not buffer-read-only)
             (not (get-char-property (point) 'read-only)))
    (self-insert-command 1)))

(defun mevedel-permission--prompt-finish-or-self-insert (result)
  "Settle the permission prompt at point with RESULT, or insert the key.
This is used by prompt overlay keymaps so shortcuts work only when
point is on the prompt text."
  (unless (mevedel-permission--prompt-finish result)
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-approve-once ()
  "Allow this tool invocation once."
  (interactive)
  (mevedel-permission--prompt-finish-or-self-insert 'allow-once))

(defun mevedel-permission--prompt-approve-session ()
  "Allow this tool for the rest of the session."
  (interactive)
  (if-let ((ov (mevedel--prompt--overlay-at-point
                'mevedel-permission-prompt)))
      (if (overlay-get ov 'mevedel-permission-suppress-allow-session)
          (message "Session allow is not available for this prompt")
        (mevedel-permission--prompt-finish 'allow-session))
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-approve-always ()
  "Always allow this tool (persisted to disk)."
  (interactive)
  (if-let ((ov (mevedel--prompt--overlay-at-point
                'mevedel-permission-prompt)))
      (if (not (overlay-get ov 'mevedel-permission-include-always))
          (message "Persistent allow is not available for this prompt")
        (mevedel-permission--prompt-finish 'always-allow))
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-deny-once ()
  "Deny this tool invocation once."
  (interactive)
  (mevedel-permission--prompt-finish-or-self-insert 'deny-once))

(defun mevedel-permission--prompt-deny-session ()
  "Deny this tool for the rest of the session."
  (interactive)
  (mevedel-permission--prompt-finish-or-self-insert 'deny-session))

(defun mevedel-permission--prompt-feedback ()
  "Deny this tool invocation and pass back free-form feedback to the LLM.
Reads a feedback string from the minibuffer, then settles the
prompt with `(feedback . TEXT)' so the pipeline's `:fail'
continuation surfaces the message as a `Permission denied: TEXT'
tool-result the model sees verbatim."
  (interactive)
  (if (mevedel--prompt--overlay-at-point 'mevedel-permission-prompt)
      (let ((text (read-string "Feedback: ")))
        (when (and text (not (string-empty-p (string-trim text))))
          (mevedel-permission--prompt-finish
           (cons 'feedback (string-trim text)))))
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-finish (result)
  "Settle the permission prompt overlay at point with RESULT.

RESULT is one of the 6-button vocabulary symbols (`allow-once' etc.,
or a `(feedback . TEXT)' cons) or `aborted' from the canceller
path.  Routes through `mevedel--prompt--settle' so the overlay's
callback fires exactly once and the overlay text/region is removed
atomically."
  (when-let* ((ov (mevedel--prompt--overlay-at-point
                   'mevedel-permission-prompt)))
    (mevedel--prompt--settle ov result)
    t))

(defun mevedel-permission--prompt-body
    (content include-always &optional suppress-allow-session)
  "Return prompt body for CONTENT, INCLUDE-ALWAYS, and SUPPRESS-ALLOW-SESSION."
  (mevedel--prompt-framed-body
   (concat
    content
    (propertize "Keys: " 'font-lock-face 'help-key-binding)
    (mevedel--prompt-key "RET")
    " allow-once  "
    (unless suppress-allow-session
      (concat
       (mevedel--prompt-key "s")
       " allow-session  "))
    (when include-always
      (concat
       (mevedel--prompt-key "A")
       " always-allow  "))
    (mevedel--prompt-key "d")
    " deny-once  "
    (mevedel--prompt-key "D")
    " deny-session  "
    (mevedel--prompt-key "f")
    " feedback\n")
   'warning))

(defun mevedel-permission--prompt-async-with-content
    (content include-always cont &optional count entry suppress-allow-session)
  "Display a 5-button permission prompt with a caller-built CONTENT block.
Shared engine for the generic permission prompt and the Bash
prompt.  CONTENT is a propertized string forming the body
between the upper and lower warning rules; INCLUDE-ALWAYS gates
the \"always-allow\" key; COUNT and ENTRY carry queue metadata;
SUPPRESS-ALLOW-SESSION hides the \"allow-session\" key.  CONT receives
the queue-vocabulary outcome (`allow-once' / `allow-session' /
`always-allow' / `deny-once' / `deny-session' / `aborted')."
  (let* ((source-buffer (current-buffer))
         (target-buf
          (if (fboundp 'mevedel-view--interaction-target-buffer)
              (mevedel-view--interaction-target-buffer
               (mevedel--prompt--data-buffer source-buffer))
            (error "No live view for queued prompt")))
         (interaction-id (or (and entry
                                  (mevedel-queue--entry-metadata-get
                                   entry :interaction-id))
                             (let ((id (list :permission
                                             (gensym "permission-"))))
                               (when entry
                                 (mevedel-queue--entry-metadata-put
                                  entry :interaction-id id))
                               id)))
         (ov nil))
    (when entry
      (mevedel-queue--entry-metadata-put entry :view-buffer target-buf))
    (with-current-buffer target-buf
      (let ((map (make-sparse-keymap)))
        (define-key map "a" #'mevedel-permission--prompt-approve-once)
        (define-key map (kbd "RET") #'mevedel-permission--prompt-approve-once)
        (define-key map (kbd "<return>") #'mevedel-permission--prompt-approve-once)
        (unless suppress-allow-session
          (define-key map "s" #'mevedel-permission--prompt-approve-session))
        (when include-always
          (define-key map "A" #'mevedel-permission--prompt-approve-always))
        (define-key map "d" #'mevedel-permission--prompt-deny-once)
        (define-key map "D" #'mevedel-permission--prompt-deny-session)
        (define-key map "f" #'mevedel-permission--prompt-feedback)
        (define-key map [?q] #'mevedel-permission--prompt-deny-once)
        (define-key map (kbd "C-g") #'mevedel-permission--prompt-deny-once)
        (setq ov
              (mevedel-view--interaction-register
               (list :kind 'permission
                     :id interaction-id
                     :count (or count 1)
                     :body (mevedel-permission--prompt-body
                            content include-always suppress-allow-session)
                     :priority 100
                     :keymap map
                     :help-echo "Permission prompt"
                     :entry entry
                     :activate cont)))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-permission-suppress-allow-session
                     suppress-allow-session)
        (overlay-put ov 'mevedel-permission-include-always include-always)
        (overlay-put ov 'mevedel--callback cont)
        (overlay-put ov 'mevedel-user-request t)
        (unless entry
          (cl-pushnew ov mevedel--prompt-overlays :test #'eq)
          (mevedel--prompt--register-canceller source-buffer ov))))
    ov))

(defun mevedel-permission--build-attribution-line (origin)
  "Return a ` from <type>--<idshort>\n' line for ORIGIN, or empty string.
Reuses `mevedel-view--insert-attribution' so the click target,
display-label derivation, and terminal-status gating are
consistent across handle / mailbox / plan / permission elements."
  (cond
   ((null origin) "")
   ((equal origin "main") "")
   ((fboundp 'mevedel-view--insert-attribution)
    (concat (mevedel-view--insert-attribution origin) "\n"))
   (t "")))

(defun mevedel-permission--prompt-async-attributed
    (tool-name path include-always origin cont &optional count entry)
  "Display permission prompt for TOOL-NAME, PATH, and INCLUDE-ALWAYS.
Permission prompts originated by sub-agents carry a
`from <type>--<idshort>' fragment so the user can see which agent
is asking.  When ORIGIN is nil or \"main\", the attribution line
is suppressed.  CONT receives the outcome.  COUNT and ENTRY carry queue
metadata."
  (let ((content (concat
                  (propertize "Permission Request\n"
                              'font-lock-face '(:inherit bold :inherit warning))
                  (mevedel-permission--build-attribution-line origin)
                  "\n"
                  (propertize "Tool: " 'font-lock-face 'font-lock-escape-face)
                  (propertize (format "%s\n" tool-name)
                              'font-lock-face 'font-lock-constant-face)
                  (when path
                    (concat
                     (propertize "Path: " 'font-lock-face 'font-lock-escape-face)
                     (propertize (format "%s\n" path)
                                 'font-lock-face 'font-lock-string-face)))
                  "\n")))
    (mevedel-permission--prompt-async-with-content
     content include-always cont count entry)))

(defun mevedel-permission--bash-guardian-label (value)
  "Return a display label for Bash guardian VALUE."
  (capitalize
   (replace-regexp-in-string "-" " " (format "%s" value))))

(defun mevedel-permission--bash-guardian-face (risk)
  "Return face for Bash guardian RISK."
  (pcase risk
    ('low 'success)
    ('medium 'warning)
    ((or 'high 'critical) 'error)
    (_ 'font-lock-comment-face)))

(defun mevedel-permission--format-bash-guardian (guardian &optional status)
  "Return formatted Bash GUARDIAN guidance.
When STATUS is `pending' and GUARDIAN is nil, return the
in-flight placeholder shown while advisory guidance is loading.
When STATUS is `unavailable', return a short unavailable note."
  (cond
   (guardian
    (let ((risk (plist-get guardian :risk))
          (recommendation (plist-get guardian :recommendation))
          (reason (plist-get guardian :reason)))
      (concat
       "\n"
       (propertize "Guardian guidance\n"
                   'font-lock-face '(:inherit bold))
       (propertize "Risk: " 'font-lock-face 'font-lock-escape-face)
       (propertize (format "%s\n" (mevedel-permission--bash-guardian-label risk))
                   'font-lock-face
                   (mevedel-permission--bash-guardian-face risk))
       (propertize "Recommendation: "
                   'font-lock-face 'font-lock-escape-face)
       (propertize
        (format "%s\n" (mevedel-permission--bash-guardian-label
                        recommendation))
        'font-lock-face 'font-lock-constant-face)
       (propertize "Reason: " 'font-lock-face 'font-lock-escape-face)
       (propertize (format "%s\n" reason)
                   'font-lock-face 'font-lock-comment-face))))
   ((eq status 'pending)
    (concat
     "\n"
     (propertize "Guardian guidance\n"
                 'font-lock-face '(:inherit bold))
     (propertize "Status: " 'font-lock-face 'font-lock-escape-face)
     (propertize "Analyzing command risk...\n"
                 'font-lock-face 'font-lock-comment-face)))
   ((eq status 'unavailable)
    (concat
     "\n"
     (propertize "Guardian guidance\n"
                 'font-lock-face '(:inherit bold))
     (propertize "Unavailable\n"
                 'font-lock-face 'font-lock-comment-face)))))

(defun mevedel-permission--prompt-async-bash
    (command dangerous include-always origin cont &optional count entry)
  "Display a Bash-specific permission prompt.
COMMAND is the parsed bash command string.  DANGEROUS is non-nil
when the command contains a dangerous binary per
`mevedel-bash-dangerous-commands' (renders prominently to warn
the user).  Dangerous prompts suppress session/permanent allow
choices.  INCLUDE-ALWAYS gates the always-allow key for
non-dangerous prompts the same way as the generic prompt.  ORIGIN
is the canonical agent-id that issued the request; renders the
attribution line when non-nil and not \"main\".  COUNT and ENTRY carry
queue metadata.  CONT receives the
queue-vocabulary outcome.

Routes Bash through the same prompt machinery as generic
permissions; when available, `allow-session' / `always-allow'
produce session / persistent pattern rules via the Bash slot
adapter."
  (let* ((commands (and entry (plist-get entry :commands)))
         (commands-summary
          (and entry
               (or (plist-get entry :commands-summary)
                   (and commands (mapconcat #'identity commands ", ")))))
         (unparseable (and entry (plist-get entry :unparseable)))
         (allow-patterns (and entry (plist-get entry :allow-patterns)))
         (guardian-cell (and entry (plist-get entry :guardian-cell)))
         (guardian (and entry
                        (or (plist-get entry :guardian)
                            (car guardian-cell))))
         (guardian-status (and guardian-cell (cadr guardian-cell)))
         (content
          (concat
          (propertize (if dangerous
                          "Bash Command Execution Request — DANGEROUS\n"
                        "Bash Command Execution Request\n")
                      'font-lock-face
                      (if dangerous
                          '(:inherit bold :inherit error)
                        '(:inherit bold :inherit warning)))
          (mevedel-permission--build-attribution-line origin)
          "\n"
          (propertize "Command: " 'font-lock-face 'font-lock-escape-face)
          (propertize (format "%s\n" command)
                      'font-lock-face 'font-lock-string-face)
          (mevedel-permission--format-bash-guardian
           guardian guardian-status)
          (when commands-summary
            (concat
             "\n"
             (propertize "Detected commands: "
                         'font-lock-face 'font-lock-escape-face)
             (propertize commands-summary
                         'font-lock-face 'font-lock-constant-face)
             "\n"))
          (when (and allow-patterns (not dangerous))
            (concat
             (propertize "Session/always allow will add: "
                         'font-lock-face 'font-lock-escape-face)
             (propertize (mapconcat
                          (lambda (pattern) (format "`%s'" pattern))
                          allow-patterns ", ")
                         'font-lock-face 'font-lock-constant-face)
             "\n"))
          (when dangerous
            (concat
             (propertize "⚠ " 'font-lock-face 'error)
             (propertize "Contains a binary on `mevedel-bash-dangerous-commands'.\n"
                         'font-lock-face 'font-lock-comment-face)
             (propertize
              "Session/permanent allow is disabled for dangerous Bash commands.\n"
              'font-lock-face 'font-lock-comment-face)))
          (when unparseable
            (propertize
             "Warning: Command contains complex syntax that could not be fully parsed.\n"
             'font-lock-face 'warning))
          "\n")))
    (mevedel-permission--prompt-async-with-content
     content (and include-always (not dangerous)) cont count entry dangerous)))

(defun mevedel-permission--prompt-async-eval
    (content cont &optional count entry)
  "Display an Eval permission prompt from caller-built CONTENT.
COUNT and ENTRY carry queue metadata.  CONT receives `allow-once',
`deny-once', a feedback cons, or `aborted'."
  (let* ((source-buffer (current-buffer))
         (target-buf
          (if (fboundp 'mevedel-view--interaction-target-buffer)
              (mevedel-view--interaction-target-buffer
               (mevedel--prompt--data-buffer source-buffer))
            (error "No live view for queued prompt")))
         (interaction-id (or (and entry
                                  (mevedel-queue--entry-metadata-get
                                   entry :interaction-id))
                             (let ((id (list :permission
                                             (gensym "eval-permission-"))))
                               (when entry
                                 (mevedel-queue--entry-metadata-put
                                  entry :interaction-id id))
                               id)))
         (body (concat
                "\n"
                (propertize "\n" 'font-lock-face
                            '(:inherit warning :underline t :extend t))
                content
                (propertize "Keys: " 'font-lock-face 'help-key-binding)
                (propertize "RET" 'font-lock-face 'help-key-binding)
                " allow-once  "
                (propertize "d" 'font-lock-face 'help-key-binding)
                " deny-once  "
                (propertize "f" 'font-lock-face 'help-key-binding)
                " feedback\n"
                (propertize "\n" 'font-lock-face
                            '(:inherit warning :underline t :extend t))))
         ov)
    (when entry
      (mevedel-queue--entry-metadata-put entry :view-buffer target-buf))
    (font-lock-append-text-property
     0 (length body) 'font-lock-face (gptel-agent--block-bg) body)
    (with-current-buffer target-buf
      (let ((map (make-sparse-keymap)))
        (define-key map "a" #'mevedel-permission--prompt-approve-once)
        (define-key map (kbd "RET") #'mevedel-permission--prompt-approve-once)
        (define-key map (kbd "<return>") #'mevedel-permission--prompt-approve-once)
        (define-key map "d" #'mevedel-permission--prompt-deny-once)
        (define-key map "f" #'mevedel-permission--prompt-feedback)
        (define-key map [?q] #'mevedel-permission--prompt-deny-once)
        (define-key map (kbd "C-g") #'mevedel-permission--prompt-deny-once)
        (setq ov
              (mevedel-view--interaction-register
               (list :kind 'permission
                     :id interaction-id
                     :count (or count 1)
                     :body body
                     :priority 100
                     :keymap map
                     :help-echo "Eval permission prompt"
                     :entry entry
                     :activate cont)))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel--callback cont)
        (overlay-put ov 'mevedel-user-request t)
        (unless entry
          (cl-pushnew ov mevedel--prompt-overlays :test #'eq)
          (mevedel--prompt--register-canceller source-buffer ov))))
    ov))

(provide 'mevedel-tool-ui)

;;; mevedel-tool-ui.el ends here
