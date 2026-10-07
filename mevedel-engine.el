;;; mevedel-engine.el --- Turn engine context -*- lexical-binding: t -*-

;;; Commentary:
;; Shared workflow consumers access turn context without assuming a provider
;; state-machine representation.  Native gptel calls retain their real FSM;
;; externally managed conversations carry context on their admitted request.

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'mevedel-structs)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-require-path "mevedel-agents" (invocation))
(defvar mevedel--agent-invocation)

;; `mevedel-auth'
(declare-function mevedel-auth-assert-ready "mevedel-auth" (backend))
(autoload 'mevedel-auth-assert-ready "mevedel-auth")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-content-start "mevedel-session-artifacts" (buffer))
(autoload 'mevedel-session-artifacts-content-start "mevedel-session-artifacts")

;; `mevedel-transcript'
(declare-function mevedel-transcript--skip-leading-summary-block "mevedel-transcript" (pos))
(autoload 'mevedel-transcript--skip-leading-summary-block "mevedel-transcript")

(cl-defgeneric mevedel-engine-info (owner)
  "Return the workflow context plist for turn OWNER.
OWNER is an admitted mevedel request, a retained-agent invocation, or a
native gptel state machine.")

(cl-defmethod mevedel-engine-info ((owner mevedel-request))
  (mevedel-request-context owner))

(cl-defmethod mevedel-engine-info ((owner gptel-fsm))
  (gptel-fsm-info owner))

(cl-defgeneric (setf mevedel-engine-info) (value owner)
  "Replace turn OWNER's workflow context with VALUE.")

(cl-defmethod (setf mevedel-engine-info) (value (owner mevedel-request))
  (setf (mevedel-request-context owner) value))

(cl-defmethod (setf mevedel-engine-info) (value (owner gptel-fsm))
  (setf (gptel-fsm-info owner) value))

(defun mevedel-engine-owner (request)
  "Return REQUEST's native engine owner or its external request context."
  (and request (or (mevedel-request-fsm request) request)))

(cl-defgeneric mevedel-engine-external-p (backend)
  "Return whether BACKEND owns retained history outside mevedel.")

(cl-defmethod mevedel-engine-external-p ((_backend t)) nil)

(defun mevedel-engine-root-history-p (session)
  "Return whether SESSION has completed root conversational history.
Directive turns share execution chronology but do not establish root history.
An external root identity also counts, including interrupted native history."
  (and session
       (or (assoc "root" (mevedel-session-external-conversations session))
           (cl-some
            (lambda (segment)
              (cl-some (lambda (prompt)
                         (and (not (eq 'directive (plist-get prompt :kind)))
                              (<= (plist-get prompt :cum-turn)
                                  (or (mevedel-session-turn-count session) 0))))
                       (cdr segment)))
            (mevedel-session-prompt-index session)))))

(defun mevedel-engine-external-history-p (session &optional root-only)
  "Return whether SESSION retains a native external conversation identity.
Unstarted and released markers hold no native history.  ROOT-ONLY ignores
child conversations, whose native histories are independent of the root."
  (and session
       (cl-some (lambda (entry)
                  (and (or (not root-only) (equal "root" (car entry)))
                       (plist-get (cdr entry) :id)))
                (mevedel-session-external-conversations session))))

(defun mevedel-engine-assert-local-history (session operation &optional backend root-only)
  "Reject OPERATION when SESSION or BACKEND has external model history.
ROOT-ONLY considers only SESSION's root conversation."
  (when (or (mevedel-engine-external-history-p session root-only)
            (mevedel-engine-external-p backend))
    (user-error "%s is unavailable for external conversations; their transcript remains readable"
                operation)))

(defun mevedel-engine-record-history-edit (begin end)
  "Record divergence when BEGIN through END changes native transcript evidence.
The unsent draft after the last model response remains editable.  Harness
publication and restoration bind `inhibit-read-only' for owned changes.
Continuation must reconcile divergence instead of resuming hidden history."
  (when (and (not inhibit-read-only)
             (bound-and-true-p mevedel--session)
             (mevedel-session-external-conversations mevedel--session))
    (let* ((child (bound-and-true-p mevedel--agent-invocation))
           (scope (if child (mevedel-agent-invocation-require-path child) "root"))
           (record (alist-get scope (mevedel-session-external-conversations mevedel--session)
                              nil nil #'equal)))
      (when (plist-get record :id)
        (save-match-data
          (save-restriction
            (widen)
            (let* ((boundary (plist-get record :input-boundary))
                   (content-start (mevedel-session-artifacts-content-start (current-buffer)))
                   (submitted-end
                    (when boundary
                      (if (or child
                              (= (car boundary) (mevedel-session-current-segment mevedel--session)))
                          (+ content-start (cdr boundary))
                        ;; Root rotation replaced the submitted prefix with a summary.
                        (mevedel-transcript--skip-leading-summary-block content-start))))
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
              (when found (plist-put record :state 'diverged)))))))))

(cl-defgeneric mevedel-engine-request-text
    (backend prompt system callback &optional stream context)
  "Generate isolated text through BACKEND in the current request buffer.
PROMPT and SYSTEM are explicit strings; no conversation or tools are selected.
CALLBACK follows gptel's text response/INFO convention.  STREAM selects chunk
delivery.  CONTEXT carries workload telemetry ownership.  Return a canceller.
Callers configure model/effort on the request buffer and own output validation,
timeouts and buffer cleanup.")

(cl-defmethod mevedel-engine-request-text
  ((_backend gptel-backend) prompt system callback &optional stream context)
  (when gptel-tools (error "Isolated text requests cannot use tools"))
  (let ((buffer (current-buffer)))
    (mevedel-auth-assert-ready gptel-backend)
    (gptel-request prompt :buffer buffer :system system :stream stream
		   :transforms nil :context context :callback callback)
    (lambda () (when (buffer-live-p buffer) (gptel-abort buffer)))))

(provide 'mevedel-engine)
;;; mevedel-engine.el ends here
