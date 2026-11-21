;;; niwa-telemetry-usage.el --- Per-provider-call usage diagnostics -*- lexical-binding: t -*-

;;; Commentary:

;; Observe gptel's actual dispatch, completion callback, and response boundary.
;; One FSM can make many HTTP calls, so neither whole-turn settlement nor
;; cumulative usage is a per-call measurement.  Retain only normalized counts
;; and frozen identity; never inspect the request/response payload.
;; Ordinary gptel and ephemeral
;; side conversations are left alone.

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'niwa-structs)
(require 'niwa-telemetry)

;; `gptel'
(declare-function gptel--insert-response "ext:gptel" (response info &optional raw))
(declare-function gptel-backend-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-curl--stream-insert-response "ext:gptel-request"
                  (response info &optional raw))
(defvar gptel--request-alist)
(defvar gptel-use-curl)

;; `niwa-agents'
(declare-function niwa-agent-invocation-path "niwa-agents" (cl-x) t)
(defvar niwa--agent-invocation)

(defvar niwa-telemetry-usage--completion-owner nil
  "Dynamically bound (INFO . CALL) for the transport currently completing.
An older cleanup may reenter the same FSM and invoke its replacement callback;
that notification must not settle the replacement provider call.")

(defun niwa-telemetry-usage-normalize (tokens)
  "Return allowlisted usage fields from gptel's normalized TOKENS.
Input already includes cache writes.  Missing/invalid fields remain nil;
cache writes are an optional subset, not another input category to add."
  (let (result invalid (known 0))
    (unless (and (proper-list-p tokens) (zerop (% (length tokens) 2)))
      (setq tokens nil invalid t))
    (dolist (pair '((:input . :input-tokens) (:cached . :cached-tokens)
                   (:cache . :cache-write-tokens) (:output . :output-tokens)))
      (let ((value (and (listp tokens) (plist-get tokens (car pair)))))
        (cond
         ((and (integerp value) (>= value 0))
          (unless (eq (car pair) :cache) (cl-incf known)))
         (value (setq value nil invalid t)))
        (setq result (append result (list (cdr pair) value)))))
    (when (and (plist-get result :cache-write-tokens)
               (plist-get result :input-tokens)
               (> (plist-get result :cache-write-tokens)
                  (plist-get result :input-tokens)))
      (setq invalid t)
      (setq result (plist-put result :cache-write-tokens nil)))
    (append result
            (list :usage-status (cond (invalid 'invalid)
                                      ((= known 3) 'complete)
                                      ((zerop known) 'missing)
                                      (t 'partial))))))

(defun niwa-telemetry-usage--owner (info)
  "Return the durable session owning INFO, otherwise nil."
  (let* ((context (let ((value (plist-get info :context)))
                    (and (proper-list-p value) value)))
         (session (or (and (listp context)
                           (plist-get context :niwa-telemetry-session))
                      (when (buffer-live-p (plist-get info :buffer))
                        (niwa-telemetry-current-session
                         (plist-get info :buffer))))))
    (when (and session (not (niwa-session-audit-session session)))
      session)))

(defun niwa-telemetry-usage--emit (call stage &rest props)
  "Emit frozen CALL at STAGE with PROPS, without affecting request execution."
  (when-let* ((observer (plist-get call :observer)))
    (funcall observer stage props))
  (let ((niwa-telemetry--event-turn (plist-get call :turn)))
    (apply (if (plist-get call :workspace)
               #'niwa-telemetry-record-workspace #'niwa-telemetry-record)
           (or (plist-get call :workspace) (plist-get call :session)) 'provider-call
           :stage stage
           (append (plist-get call :identity) props))))

(defun niwa-telemetry-usage--finish (call info outcome)
  "Settle CALL once with INFO's per-call counts and OUTCOME."
  (unless (or (plist-get call :finished)
              (and niwa-telemetry-usage--completion-owner
                   (not (eq outcome 'aborted))
                   (eq info (car niwa-telemetry-usage--completion-owner))
                   (not (eq call (cdr niwa-telemetry-usage--completion-owner)))))
    ;; Mark before append: an emission failure must not create another finish.
    (setf (plist-get call :finished) t)
    (apply #'niwa-telemetry-usage--emit call 'finish
           :outcome outcome
           :duration-ms (max 0 (round (* 1000 (- (niwa-telemetry--monotonic-now)
                                               (plist-get call :started-at)))))
           ;; Status text/error payloads are deliberately not forwarded.
           (niwa-telemetry-usage-normalize (plist-get info :tokens)))))

(defun niwa-telemetry-usage--callback (call callback stream response info &rest args)
  "Observe completion of CALL, then invoke CALLBACK with RESPONSE and INFO.
STREAM means the selected transport actually streams.  Chunks, reasoning,
and tool notifications do not complete a provider call."
  (when (and (not (car args))
             (or (eq response 'abort)
                 (eq response t)
                 (and (not stream) (or (null response) (stringp response)))
                 (and stream (null response))))
    (niwa-telemetry-usage--finish
     call info (cond ((eq response 'abort) 'aborted)
                     ((or (plist-get info :error)
                          (and (plist-get info :http-status)
                               (not (member (plist-get info :http-status)
                                            '("200" "100")))))
                      'error)
                     (t 'success))))
  (apply callback response info args))

(defun niwa-telemetry-usage--parse-stream (original backend info)
  "Observe a provider error normalized by ORIGINAL for BACKEND and INFO.
Keep only a boolean on the current call, never the error payload.  An error
inherited from an older transport's cleanup is not this parser's evidence."
  (let ((call (plist-get info :niwa-usage-call))
        (previous-error (plist-get info :error)))
    (condition-case err
        (prog1 (funcall original backend info)
          (when (and call (plist-get info :error)
                     (not (eq previous-error (plist-get info :error))))
            (setf (plist-get call :provider-error) t)))
      (error
       (when call (setf (plist-get call :provider-error) t))
       (signal (car err) (cdr err))))))

(defun niwa-telemetry-usage--stream-cleanup (original process status)
  "Observe PROCESS completion before ORIGINAL handles STATUS.
Freeze completion ownership across reentrant tool-loop transitions.  gptel
before `ec25a41' transitions a failed curl cleanup before assigning its error,
so a pending tool call can dispatch the next call from inside ORIGINAL; capture
this call's usage before that dispatch replaces the callback and resets its
counts."
  (let* ((fsm (car (alist-get process gptel--request-alist)))
         (info (and fsm (gptel-fsm-info fsm)))
         (call (plist-get info :niwa-usage-call))
         (niwa-telemetry-usage--completion-owner
          (and call (cons info call))))
    (when call
      (niwa-telemetry-usage--finish
       call info (if (and (zerop (process-exit-status process))
                          (member (plist-get info :http-status) '("200" "100"))
                          (not (plist-get call :provider-error)))
                     'success 'error)))
    (funcall original process status)))

(defun niwa-telemetry-usage--parse-response (original &rest args)
  "Observe ORIGINAL's non-streaming transport completion with ARGS.
Both curl and URL parsers take INFO last and return response, HTTP status,
status text and optional error.  Use only their completion metadata and INFO's
normalized counts, never the returned text.  This also covers tool-only calls
whose response callback gptel skips."
  (let* ((info (car (last args)))
         (call (plist-get info :niwa-usage-call))
         (niwa-telemetry-usage--completion-owner
          (and call (cons info call)))
         (result (condition-case err
                     (apply original args)
                   (error
                    (when call
                      (niwa-telemetry-usage--finish call info 'error))
                    (signal (car err) (cdr err))))))
    (when call
      (niwa-telemetry-usage--finish
       call info (if (and (member (nth 1 result) '("200" "100"))
                          (not (nth 3 result)))
                     'success 'error)))
    result))

(defun niwa-telemetry-usage--abort (fsm &optional new-state)
  "Capture explicit NEW-STATE ABRT for FSM, even if its callback was muted.
Retained-agent interruption intentionally replaces the response callback while
cancelling transport.  Ordinary abort callbacks may already have settled it."
  (when (eq new-state 'ABRT)
    (let* ((info (gptel-fsm-info fsm))
           (call (plist-get info :niwa-usage-call)))
      (when call (niwa-telemetry-usage--finish call info 'aborted)))))

(defun niwa-telemetry-usage--dispatch (original fsm)
  "Observe one actual dispatch by ORIGINAL using FSM.
This seam is downstream of niwa's compaction/admission gates."
  (let* ((info (gptel-fsm-info fsm))
         (context (let ((value (plist-get info :context)))
                    (and (proper-list-p value) value)))
         (workspace (and niwa-telemetry-enabled
                         (plist-get context :niwa-telemetry-workspace)))
         (session (and niwa-telemetry-enabled
                       (niwa-telemetry-usage--owner info))))
    (if (not (or workspace session))
        (funcall original fsm)
      (let* ((invocation (or (plist-get info :niwa-agent-invocation)
                             ;; Agent request transforms may dispatch before
                             ;; gptel-request returns to install top-level keys.
                             (let ((buffer (plist-get info :buffer)))
                               (when (and (buffer-live-p buffer)
                                          (buffer-local-boundp
                                           'niwa--agent-invocation buffer))
                                 (buffer-local-value 'niwa--agent-invocation
                                                     buffer)))))
             (request (plist-get info :niwa-request))
             (backend (plist-get info :backend))
             (stream (and gptel-use-curl (plist-get info :stream)))
             ;; Reuse gptel's default callbacks, without accumulating wrappers
             ;; when the same FSM dispatches its next tool-loop round.
             (callback (or (if (eq (plist-get info :callback)
                                  (plist-get info :niwa-usage-wrapper))
                               (plist-get info :niwa-usage-original)
                             (plist-get info :callback))
                           (if stream #'gptel-curl--stream-insert-response
                             #'gptel--insert-response)))
             (call
              (list :session session :workspace workspace
                    :observer (plist-get context :niwa-telemetry-observer)
                    :turn (when session
                            (if request (niwa-request-turn request)
                              (niwa-session-turn-count session)))
                    :started-at (niwa-telemetry--monotonic-now)
                    :finished nil
                    :identity
                    (list :call-id (niwa-telemetry--span-id 'provider-call)
                          :pass-id (plist-get context :niwa-telemetry-pass-id)
                          :request-id (plist-get info :niwa-request-id)
                          :agent-path (when invocation
                                        (niwa-agent-invocation-path invocation))
                          :backend (when backend (gptel-backend-name backend))
                          :backend-type (when backend (type-of backend))
                          :model (plist-get info :model)
                          :workload (or (plist-get context :niwa-telemetry-workload)
                                        (if invocation 'agent 'root))
                          :purpose (plist-get context :purpose))))
             (wrapper (apply-partially #'niwa-telemetry-usage--callback
                                       call callback stream))
             (registered-transports (mapcar #'cdr gptel--request-alist)))
        (setq info (plist-put info :niwa-usage-original callback))
        (setq info (plist-put info :niwa-usage-wrapper wrapper))
        (setq info (plist-put info :niwa-usage-call call))
        (setf (gptel-fsm-info fsm) (plist-put info :callback wrapper))
        (niwa-telemetry-usage--emit call 'start)
        (condition-case err
            (progn
              ;; Workspace appends can yield before transport registration.
              ;; Refuse a cancelled pass without launching an orphan request.
              (when-let* ((currentp (plist-get context :niwa-telemetry-currentp)))
                (unless (funcall currentp)
                  (niwa-telemetry-usage--finish call nil 'aborted)
                  (error "Memory provider dispatch cancelled")))
              (funcall original fsm))
          ((error quit)
           ;; handle-wait runs post-request hooks after registering transport.
           ;; A hook failure must propagate, but that active call can still
           ;; report usage later.  Only a failed transport start settles here.
           (unless (cl-some (lambda (entry)
                              (and (eq (cadr entry) fsm)
                                   (not (memq (cdr entry) registered-transports))))
                            gptel--request-alist)
             (niwa-telemetry-usage--finish call nil 'error))
           (signal (car err) (cdr err))))))))

(defun niwa-telemetry-usage-install ()
  "Install sparse usage observation for niwa-owned provider calls."
  (unless (advice-member-p #'niwa-telemetry-usage--dispatch
                           'gptel--handle-wait)
    (advice-add 'gptel--handle-wait :around #'niwa-telemetry-usage--dispatch))
  (dolist (parser '(gptel-curl--parse-response gptel--url-parse-response))
    (unless (advice-member-p #'niwa-telemetry-usage--parse-response parser)
      (advice-add parser :around #'niwa-telemetry-usage--parse-response)))
  (unless (advice-member-p #'niwa-telemetry-usage--stream-cleanup
                           'gptel-curl--stream-cleanup)
    (advice-add 'gptel-curl--stream-cleanup :around
                #'niwa-telemetry-usage--stream-cleanup))
  (unless (advice-member-p #'niwa-telemetry-usage--parse-stream
                           'gptel-curl--parse-stream)
    (advice-add 'gptel-curl--parse-stream :around
                #'niwa-telemetry-usage--parse-stream))
  (unless (advice-member-p #'niwa-telemetry-usage--abort
                           'gptel--fsm-transition)
    (advice-add 'gptel--fsm-transition :before #'niwa-telemetry-usage--abort)))

(defun niwa-telemetry-usage-uninstall ()
  "Stop observing new dispatches; already dispatched calls may still settle."
  (advice-remove 'gptel--handle-wait #'niwa-telemetry-usage--dispatch)
  (dolist (parser '(gptel-curl--parse-response gptel--url-parse-response))
    (advice-remove parser #'niwa-telemetry-usage--parse-response))
  (advice-remove 'gptel-curl--stream-cleanup #'niwa-telemetry-usage--stream-cleanup)
  (advice-remove 'gptel-curl--parse-stream #'niwa-telemetry-usage--parse-stream)
  (advice-remove 'gptel--fsm-transition #'niwa-telemetry-usage--abort))

(provide 'niwa-telemetry-usage)
;;; niwa-telemetry-usage.el ends here
