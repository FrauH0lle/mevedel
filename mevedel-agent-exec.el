;;; mevedel-agent-exec.el -- Sub-agent task runner -*- lexical-binding: t -*-

;;; Commentary:

;; Mevedel-owned sub-agent runtime.  Extracted from `gptel-agent-tools'
;; so mevedel controls the surface where private coupling used to
;; concentrate: the task dispatch function, the sub-agent FSM handler
;; table, the agent registry, the task overlay, and the status
;; indicators that ride on it.
;;
;; The extraction fixes a streaming-truncation bug present in the
;; upstream `gptel-agent--task': that function's `(pred stringp)'
;; branch fires its main callback on every streamed chunk and has no
;; `t' branch, so gptel's stream-complete signal (`gptel-request.el'
;; line 2864) is silently dropped.  Since gptel's tool-call commit
;; path locks in the first delivered value, the parent agent only
;; ever sees the first chunk of a sub-agent's final response.  The
;; runner here accumulates on string chunks and fires exactly once on
;; `t', so the full response reaches the parent.
;;
;; See specs/archive/18-sub-agent-runtime.md for the full extraction
;; rationale, scope, and regression-test plan.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel)
  ;; Required for the cl-defstruct `setf' expanders of `gptel-fsm-*' slots.
  ;; Without this, `(setf (gptel-fsm-handlers ...) ...)' below does not expand
  ;; to its slot-setter at compile time and falls back to looking up a
  ;; nonexistent `(setf gptel-fsm-handlers)' function at runtime.
  (require 'gptel-request))

;; `gptel-request'
(declare-function gptel-request "ext:gptel-request"
                  (&optional prompt &rest args))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-handlers "ext:gptel-request" (cl-x) t)
(declare-function gptel-make-fsm "ext:gptel-request" (&rest args))
(declare-function gptel--fsm-transition "ext:gptel-request"
                  (machine &optional new-state))
(declare-function gptel--transform-add-context "ext:gptel-request" (fsm))
(declare-function gptel--display-tool-calls "ext:gptel-request" (calls info))
(defvar gptel-send--transitions)

;; `gptel'
(declare-function gptel-with-preset "ext:gptel" (name &rest body))
(declare-function gptel-get-preset "ext:gptel" (name))
(declare-function gptel--update-status "ext:gptel"
                  (msg &optional face))
(defvar gptel--fsm-last)
(defvar gptel-agent-preset)
(defvar gptel-stream)

;; `mevedel-tool-ui' -- static cycle (tool-ui requires this module at
;; compile time for symbols declared below; we declare tool-ui helpers
;; here for the runtime call sites.  Both modules are fully loaded by
;; the time `mevedel-agent-exec--run' is reachable.)
(declare-function mevedel-tools--augment-agent-handlers "mevedel-tool-ui"
                  (handlers &rest rest))
(declare-function mevedel-tools--handle-message-inject "mevedel-tool-ui" (fsm))
(declare-function mevedel-tools--handle-wait-inject "mevedel-tool-ui" (fsm))
(declare-function mevedel-tools--inject-bwait-transition "mevedel-tool-ui"
                  (fsm))

;; `gptel' core (stable)
(declare-function gptel--model-name "ext:gptel" (&optional model))
(declare-function gptel--format-tool-call "ext:gptel-request" (name args))
(declare-function gptel--handle-wait "ext:gptel-request" (fsm))
(declare-function gptel--handle-pre-tool "ext:gptel-request" (fsm))
(declare-function gptel--handle-tool-use "ext:gptel-request" (fsm))
(declare-function gptel--handle-post-tool "ext:gptel-request" (fsm))
(declare-function gptel--handle-tool-result "ext:gptel-request" (fsm))
(defvar gptel-model)
(defvar gptel--tool-preview-alist)

;; `gptel-agent-tools' — still external for the helpers we have not
;; internalized yet (confirm-overlay used by the preview renderer).
(declare-function gptel-agent--confirm-overlay "ext:gptel-agent-tools"
                  (from to &optional no-hide))


;;
;;; Agent registry

(defvar-local mevedel-agent-exec--agents nil
  "Buffer-local list of sub-agent specs available in this chat buffer.

Each element is a cons `(NAME . PLIST)' where PLIST is a gptel preset
keyword list describing the sub-agent (tools, prompt, max-turns, ...).
The list is populated by `mevedel-agents--setup-for-request' from
`mevedel-agent--registry' at preset-install time and consumed by
`mevedel-agent-exec--run' when dispatching sub-agent tasks.")



;;
;;; Task overlay + status indicators

(defconst mevedel-agent-exec--hrule
  (propertize "\n" 'face '(:inherit shadow :underline t :extend t))
  "Horizontal rule inserted in the task overlay message.")

(defun mevedel-agent-exec--task-overlay (where &optional agent-type description)
  "Create a task overlay at WHERE for AGENT-TYPE with DESCRIPTION.

The overlay is tagged with the `mevedel-agent' property so other
mevedel layers (chat spinner, tutor hint filtering, preview mode
coexistence) can detect sub-agent runs.  Its `msg' property holds the
banner used by the status indicators, and `count' tracks how many
tool calls have been observed — the indicators compose on top of this
state."
  (let* ((bounds                    ;; Place the overlay, handle edge cases.
          (save-excursion
            (goto-char where)
            (when (bobp) (insert "\n"))
            (if (and (bolp) (eolp))
                (cons (1- (point)) (point))
              (cons (line-beginning-position) (line-end-position)))))
         (ov (make-overlay (car bounds) (cdr bounds) nil t))
         (model
          (propertize (concat (gptel--model-name gptel-model))
                      'face 'font-lock-comment-face))
         (msg (concat
               (unless (eq (char-after (car bounds)) ?\n) "\n")
               "\n" mevedel-agent-exec--hrule
               (propertize (concat (capitalize agent-type) " Task: ")
                           'face 'font-lock-escape-face)
               (propertize description 'face 'font-lock-doc-face)
               (propertize
                " " 'display
                (if (fboundp 'string-pixel-width)
                    `(space :align-to (- right (,(string-pixel-width model))))
                  `(space :align-to (- right ,(+ 5 (string-width model))))))
               model "\n")))
    (prog1 ov
      (overlay-put ov 'mevedel-agent t)
      (overlay-put ov 'count 0)
      (overlay-put ov 'msg msg)
      (overlay-put ov 'line-prefix "")
      (overlay-put
       ov 'after-string
       (concat msg (propertize "Waiting..." 'face 'warning) "\n"
               mevedel-agent-exec--hrule)))))

(defun mevedel-agent-exec--indicate-wait (fsm)
  "Display the waiting indicator for sub-agent FSM."
  (when-let* ((info (gptel-fsm-info fsm))
              (info-ov (plist-get info :context))
              (count (overlay-get info-ov 'count)))
    (run-at-time
     1.5 nil
     (lambda (ov count)
       (when (and (overlay-buffer ov)
                  (eql (overlay-get ov 'count) count))
         (let* ((task-msg (overlay-get ov 'msg))
                (new-info-msg
                 (concat task-msg
                         (concat
                          (propertize "Waiting... " 'face 'warning) "\n"
                          (propertize "\n" 'face
                                      '(:inherit shadow :underline t
                                                 :extend t))))))
           (overlay-put ov 'after-string new-info-msg))))
     info-ov count)))

(defun mevedel-agent-exec--indicate-tool-call (fsm)
  "Display the tool-call indicator for sub-agent FSM."
  (when-let* ((info (gptel-fsm-info fsm))
              (tool-use (plist-get info :tool-use))
              (ov (plist-get info :context)))
    (when (overlay-buffer ov)
      (let* ((task-msg (overlay-get ov 'msg))
             (info-count (overlay-get ov 'count))
             (new-info-msg
              (concat task-msg
                      (concat
                       (propertize "Calling Tools... "
                                   'face 'mode-line-emphasis)
                       (if (= info-count 0) "\n"
                         (format "(+%d)\n" info-count))
                       (mapconcat (lambda (call)
                                    (gptel--format-tool-call
                                     (plist-get call :name)
                                     (map-values (plist-get call :args))))
                                  tool-use)
                       "\n" mevedel-agent-exec--hrule))))
        (overlay-put ov 'count (+ info-count (length tool-use)))
        (overlay-put ov 'after-string new-info-msg)))))

(defun mevedel-agent-exec--task-preview-setup (arg-values _info)
  "Tool-preview renderer for the Agent tool.

Called by gptel during tool preview for each Agent call to format
the call's (TYPE DESCRIPTION PROMPT) argument list inline.  ARG-VALUES
is the positional argument list; the second plist is the tool call
info, unused here.

Delegates the confirmation-overlay wrapping to
`gptel-agent--confirm-overlay' for now -- that helper is outside the
sub-agent-runtime extraction scope and replacing it earns little."
  (pcase-let ((from (point))
              (`(,type ,desc ,prompt) arg-values))
    (insert "("
            (propertize "Agent " 'font-lock-face 'font-lock-keyword-face)
            (propertize (prin1-to-string type)
                        'font-lock-face 'font-lock-escape-face)
            " " (propertize (prin1-to-string desc)
                            'font-lock-face
                            '(:inherit font-lock-constant-face :inherit bold))
            "\n" (propertize (prin1-to-string prompt)
                             'line-prefix "  "
                             'wrap-prefix "  "
                             'font-lock-face 'font-lock-constant-face)
            ")\n\n")
    (gptel-agent--confirm-overlay from (point) t)))


;;
;;; FSM handler table

(defvar mevedel-agent-exec--handlers
  `((WAIT ,#'mevedel-agent-exec--indicate-wait
          ,#'gptel--handle-wait)
    (TPRE ,#'gptel--handle-pre-tool ,#'gptel--fsm-transition)
    (TOOL ,#'mevedel-agent-exec--indicate-tool-call
          ,#'gptel--handle-tool-use)
    (TRET ,#'gptel--handle-post-tool
          ,#'gptel--handle-tool-result))
  "Handler table for the mevedel sub-agent FSM.

Same shape as `gptel-send--transitions': each entry is `(STATE
FN ...)' where FN is called when the FSM transitions into (or out
of) STATE.  Modelled after the upstream `gptel-agent-request--handlers'
table but dispatches to the mevedel-owned status indicators in WAIT
and TOOL.  Passed to `gptel-make-fsm' by
`mevedel-agent-exec--run' (spec 18, step 6).")


;;
;;; Task runner

(defun mevedel-agent-exec--run (main-cb agent-type description prompt
                                         &optional invocation)
  "Dispatch a sub-agent task and route its final response to MAIN-CB.

AGENT-TYPE is the registry key (e.g. `\"explore\"', `\"planner\"').
DESCRIPTION is a short human-facing label shown in the task overlay.
PROMPT is the full instruction handed to the sub-agent.

Optional INVOCATION is the `mevedel-agent-invocation' associated with
this task.  When present it is stashed on the task overlay under the
`mevedel-agent-invocation' property so reminder/message handlers can
reach it, and the FSM's WAIT state is augmented with the mevedel
message-inject and reminder-inject handlers.  The BWAIT parking state
is also installed so background children keep the FSM alive.

Callback contract.  Unlike the upstream `gptel-agent--task', which
fires MAIN-CB on every streamed chunk and drops gptel's end-of-stream
`t' signal, this runner:

  - accumulates streamed string chunks into `partial';
  - fires MAIN-CB exactly once on the `t' branch, after the sub-agent
    turn has completed and no further tool-use is pending.

Returns the spawned FSM.  See spec 18 for the extraction rationale."
  (gptel-with-preset
      (nconc (list :include-reasoning nil
                   :use-tools t
                   :context nil)
             (and gptel-agent-preset
                  (copy-sequence
                   (cond
                    ((symbolp gptel-agent-preset)
                     (gptel-get-preset gptel-agent-preset))
                    ((listp gptel-agent-preset)
                     gptel-agent-preset)
                    (t (error "Invalid `gptel-agent-preset': %S"
                              gptel-agent-preset)))))
             (cdr (assoc agent-type mevedel-agent-exec--agents)))
    (let* ((info (gptel-fsm-info gptel--fsm-last))
           (where (or (plist-get info :tracking-marker)
                      (plist-get info :position)))
           (partial (format "%s result for task: %s\n\n"
                            (capitalize agent-type) description))
           (overlay (mevedel-agent-exec--task-overlay
                     where agent-type description))
           (fsm (gptel-make-fsm :table gptel-send--transitions
                                :handlers mevedel-agent-exec--handlers)))
      (when invocation
        (overlay-put overlay 'mevedel-agent-invocation invocation)
        (setf (gptel-fsm-handlers fsm)
              (mevedel-tools--augment-agent-handlers
               (gptel-fsm-handlers fsm)
               :prepend
               `((WAIT . (,#'mevedel-tools--handle-message-inject
                          ,#'mevedel-tools--handle-wait-inject)))))
        (mevedel-tools--inject-bwait-transition fsm))
      (gptel--update-status " Calling Agent..." 'font-lock-escape-face)
      (gptel-request prompt
        :context overlay
        :fsm fsm
        :stream gptel-stream
        :transforms (list #'gptel--transform-add-context)
        :callback
        (mevedel-agent-exec--make-callback
         main-cb agent-type description where (list partial))))))

(defun mevedel-agent-exec--make-callback (main-cb agent-type description
                                                    where partial-cell)
  "Return the callback used by `mevedel-agent-exec--run'.

MAIN-CB receives the final accumulated partial string exactly once.

AGENT-TYPE and DESCRIPTION decorate the error / abort messages.  WHERE
is the tracking-marker fallback for the initial `tool-call' dispatch.
PARTIAL-CELL is a one-element list holding the running accumulated
text; string chunks are concatenated into its car, and the `t'
completion branch reads it back out.

The dispatch table is:

- `nil': transport error; MAIN-CB receives a formatted error string.
- `(tool-call . CALLS)': update tracking marker and hand off to
  `gptel--display-tool-calls'.
- `(pred stringp)': accumulate into PARTIAL-CELL.  Do NOT fire
  MAIN-CB here -- this is exactly where the upstream bug lived.
- `'t': stream complete; if no tool-use is pending, run the optional
  transformer over the partial and fire MAIN-CB once.
- `'abort': aborted; MAIN-CB receives a formatted abort string."
  (lambda (resp info)
    (let ((ov (plist-get info :context)))
      (pcase resp
        ('nil
         (when ov (delete-overlay ov))
         (funcall main-cb
                  (format "Error: Task %s could not finish task \"%s\".

Error details: %S"
                          agent-type description
                          (plist-get info :error))))
        (`(tool-call . ,calls)
         (unless (plist-get info :tracking-marker)
           (plist-put info :tracking-marker where))
         (gptel--display-tool-calls calls info))
        ((pred stringp)
         ;; Accumulate only -- firing per chunk was the upstream bug
         ;; that drove the extraction.
         (setcar partial-cell (concat (car partial-cell) resp)))
        ('t
         (unless (plist-get info :tool-use)
           (when ov (delete-overlay ov))
           (when-let* ((transformer (plist-get info :transformer)))
             (setcar partial-cell (funcall transformer (car partial-cell))))
           (funcall main-cb (car partial-cell))))
        ('abort
         (when ov (delete-overlay ov))
         (funcall main-cb
                  (format "Error: Task \"%s\" was aborted by the user. \
%s could not finish."
                          description agent-type)))))))


;;
;;; Preview registration

(with-eval-after-load 'gptel-agent-tools
  ;; Override the upstream entry so the Agent tool's tool-call preview
  ;; uses mevedel's renderer.  Upstream registers its version when
  ;; gptel-agent-tools loads; we stamp on top afterwards.
  (setf (alist-get "Agent" gptel--tool-preview-alist nil nil #'equal)
        #'mevedel-agent-exec--task-preview-setup))


(provide 'mevedel-agent-exec)
;;; mevedel-agent-exec.el ends here
