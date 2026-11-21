;;; niwa-engine.el --- Turn engine context -*- lexical-binding: t -*-

;;; Commentary:
;; Shared workflow consumers access turn context without assuming a provider
;; state-machine representation.  Native gptel calls retain their real FSM;
;; externally managed conversations carry context on their admitted request.

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'niwa-structs)

;; `niwa-agents'
(declare-function niwa-agent-invocation-require-path "niwa-agents" (invocation))
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-agent-id "niwa-agents" (cl-x) t)
(defvar niwa--agent-invocation)

;; `gptel-openai-oauth'
(declare-function gptel-openai-oauth-p "ext:gptel-openai-oauth" (cl-x) t)

;; `niwa-auth'
(declare-function niwa-auth-assert-ready "niwa-auth" (backend))
(declare-function niwa-auth-unready-reason "niwa-auth" (backend))
(autoload 'niwa-auth-assert-ready "niwa-auth")
(autoload 'niwa-auth-unready-reason "niwa-auth")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-content-start "niwa-session-artifacts" (buffer))
(autoload 'niwa-session-artifacts-content-start "niwa-session-artifacts")

;; `niwa-transcript'
(declare-function niwa-transcript--skip-leading-summary-block "niwa-transcript" (pos))
(autoload 'niwa-transcript--skip-leading-summary-block "niwa-transcript")

(defun niwa-engine--codex-cache-key (fsm)
  "Set the Codex cache key in FSM's prompt buffer before serialization.
Normal sends and compaction reach this boundary after applying their
effective backend and request parameters.  Use the source session id,
with a separate key for each agent's distinct conversation prefix."
  (let* ((info (gptel-fsm-info fsm))
         (source (plist-get info :buffer))
         (prompt (plist-get info :data)))
    (when (and (buffer-live-p source) (buffer-live-p prompt))
      (with-current-buffer prompt
        (when (and (fboundp 'gptel-openai-oauth-p)
                   (gptel-openai-oauth-p gptel-backend))
          (when-let* ((session (buffer-local-value 'niwa--session source))
                      (id (niwa-session-session-id session))
                      ((stringp id)))
            (let ((inv (buffer-local-value 'niwa--agent-invocation source)))
              (setq-local gptel--request-params
                          (plist-put (copy-sequence gptel--request-params)
                                     :prompt_cache_key
                                     (if (and inv (niwa-agent-invocation-p inv))
                                         (format "%s:%s" id (niwa-agent-invocation-agent-id inv))
                                       id))))))))))

(advice-add 'gptel--realize-query :before #'niwa-engine--codex-cache-key)

(cl-defgeneric niwa-engine-info (owner)
  "Return the workflow context plist for turn OWNER.
OWNER is an admitted niwa request, a retained-agent invocation, or a
native gptel state machine.")

(cl-defmethod niwa-engine-info ((owner niwa-request))
  (niwa-request-context owner))

(cl-defmethod niwa-engine-info ((owner gptel-fsm))
  (gptel-fsm-info owner))

(cl-defgeneric (setf niwa-engine-info) (value owner)
  "Replace turn OWNER's workflow context with VALUE.")

(cl-defmethod (setf niwa-engine-info) (value (owner niwa-request))
  (setf (niwa-request-context owner) value))

(cl-defmethod (setf niwa-engine-info) (value (owner gptel-fsm))
  (setf (gptel-fsm-info owner) value))

(defun niwa-engine-owner (request)
  "Return REQUEST's native engine owner or its external request context."
  (and request (or (niwa-request-fsm request) request)))

(cl-defgeneric niwa-engine-steerable-p (owner)
  "Return whether turn OWNER can still take steering at a later boundary.")

(cl-defmethod niwa-engine-steerable-p ((owner gptel-fsm))
  (not (memq (gptel-fsm-state owner) '(DONE ERRS ABRT))))

(cl-defmethod niwa-engine-steerable-p ((owner niwa-request))
  ;; Its tool-batch hook and prompt continuation deliver steering until the
  ;; native turn settles or stops at a boundary.
  (let ((info (niwa-engine-info owner)))
    (and (plist-get info :external-history)
         (not (plist-get info :niwa-acp-outcome))
         (not (plist-get info :niwa-end-turn)))))

(cl-defgeneric niwa-engine-external-p (backend)
  "Return whether BACKEND owns retained history outside niwa.")

(cl-defmethod niwa-engine-external-p ((_backend t)) nil)

(defun niwa-engine-root-history-p (session)
  "Return whether SESSION has completed root conversational history.
An external root identity also counts, including interrupted native history."
  (and session
       (or (assoc "root" (niwa-session-external-conversations session))
           (cl-some
            (lambda (segment)
              (cl-some (lambda (prompt)
                         (<= (plist-get prompt :cum-turn)
                             (or (niwa-session-turn-count session) 0)))
                       (cdr segment)))
            (niwa-session-prompt-index session)))))

(defun niwa-engine-external-history-p (session &optional root-only)
  "Return whether SESSION retains a native external conversation identity.
Unstarted and released markers hold no native history.  ROOT-ONLY ignores
child conversations, whose native histories are independent of the root."
  (and session
       (cl-some (lambda (entry)
                  (and (or (not root-only) (equal "root" (car entry)))
                       (plist-get (cdr entry) :id)))
                (niwa-session-external-conversations session))))

(defun niwa-engine-assert-local-history (session operation &optional backend root-only)
  "Reject OPERATION when SESSION or BACKEND has external model history.
ROOT-ONLY considers only SESSION's root conversation."
  (when (or (niwa-engine-external-history-p session root-only)
            (niwa-engine-external-p backend))
    (user-error "%s is unavailable for external conversations; their transcript remains readable"
                operation)))

(defun niwa-engine-record-history-edit (begin end)
  "Record divergence when BEGIN through END changes native transcript evidence.
The unsent draft after the last model response remains editable.  Harness
publication and restoration bind `inhibit-read-only' for owned changes.
Continuation must reconcile divergence instead of resuming hidden history."
  (when (and (not inhibit-read-only)
             (bound-and-true-p niwa--session)
             (niwa-session-external-conversations niwa--session))
    (let* ((child niwa--agent-invocation)
           (scope (if child (niwa-agent-invocation-require-path child) "root"))
           (record (alist-get scope (niwa-session-external-conversations niwa--session)
                              nil nil #'equal)))
      (when (or (plist-get record :id)
                (cl-some (lambda (entry) (plist-get entry :id))
                         (plist-get record :boundaries)))
        (save-match-data
          (save-restriction
            (widen)
            (let* ((boundary
                    (or (plist-get record :input-boundary)
                        (plist-get
                         (car (last (cl-remove-if-not
                                     (lambda (entry) (plist-get entry :id))
                                     (plist-get record :boundaries))))
                         :input-boundary)))
                   (content-start (niwa-session-artifacts-content-start (current-buffer)))
                   (submitted-end
                    (when boundary
                      (if (or child
                              (= (car boundary) (niwa-session-current-segment niwa--session)))
                          (+ content-start (cdr boundary))
                        ;; Root rotation replaced the submitted prefix with a summary.
                        (niwa-transcript--skip-leading-summary-block content-start))))
                   ;; Metadata belongs to the editor, not native model history.
                   (body-change (or (>= begin content-start) (> end content-start)))
                   (position (if body-change (point-max) begin))
                   (found (and body-change submitted-end (< begin submitted-end))))
              (while (and (> position begin) (not found))
                (let ((property (get-text-property (1- position) 'gptel)))
                  (if (memq (if (consp property) (car property) property)
                            '(response reasoning tool))
                      (setq found t)
                    (setq position (previous-single-property-change
                                    position 'gptel nil (point-min))))))
              (when found
                (if (plist-get record :id)
                    (plist-put record :state 'diverged)
                  ;; A detached excerpt cannot vouch for old native coordinates.
                  (plist-put record :boundaries
                             (cl-remove-if-not
                              (lambda (entry) (plist-get entry :detached))
                              (plist-get record :boundaries))))))))))))

(cl-defgeneric niwa-engine-request-text
    (backend prompt system callback &optional stream context ready)
  "Generate isolated text through BACKEND in the current request buffer.
PROMPT and SYSTEM are explicit strings; no conversation or tools are selected.
CALLBACK follows gptel's text response/INFO convention.  STREAM selects chunk
delivery.  CONTEXT carries workload telemetry ownership.  READY, a
zero-argument function, runs at most once, after the provider started and the
prompt was submitted, so callers can time the response alone.  CALLBACK may
settle before READY runs, or without it.  Return a canceller.
Callers configure model/effort on the request buffer and own output validation,
timeouts and buffer cleanup.")

(cl-defmethod niwa-engine-request-text
  ((_backend gptel-backend) prompt system callback &optional stream context ready)
  (when gptel-tools (error "Isolated text requests cannot use tools"))
  (let ((buffer (current-buffer)))
    (niwa-auth-assert-ready gptel-backend)
    (gptel-request prompt :buffer buffer :system system :stream stream
		   :transforms nil :context context :callback callback)
    (when ready (funcall ready))
    (lambda () (when (buffer-live-p buffer) (gptel-abort buffer)))))

(cl-defgeneric niwa-engine-text-unavailable (_backend)
  "Return why BACKEND cannot answer an isolated text request now, or nil.
A reason is a credential-free diagnostic and positive knowledge that the
provider is unusable here: missing sign-in, credentials or runtime.  Nil
promises nothing; the request itself can still fail.  Checking may begin
asynchronous credential renewal."
  nil)

(cl-defmethod niwa-engine-text-unavailable ((backend gptel-backend))
  (niwa-auth-unready-reason backend))

(provide 'niwa-engine)
;;; niwa-engine.el ends here
