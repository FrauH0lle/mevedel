;;; mevedel-debug.el -- Temporary debug instrumentation -*- lexical-binding: t -*-

;;; Commentary:

;; TEMPORARY development scaffolding for the spec 10 follow-up work on
;; WAIT-handler reminder injection in agent FSMs.  This file is NOT
;; loaded by default and is not required by any other mevedel module.
;; Load it manually (`M-x load-file') when you need to verify that
;; WAIT handlers are firing on every cycle of an agent run.
;;
;; Remove this file before merging once the WAIT-handler mechanism is
;; validated end-to-end.

;;; Code:

(require 'gptel-request)

;; `mevedel-tool-ui'
(declare-function mevedel-tools--agent-invocation-at "mevedel-tool-ui" (fsm))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-turn-count "mevedel-agents" (inv) t)

(defvar mevedel--fsm-trace-enabled nil
  "When non-nil, log FSM transitions to `mevedel--fsm-trace-buffer-name'.")

(defvar mevedel--fsm-trace-buffer-name "*mevedel-fsm-trace*"
  "Name of the buffer used by `mevedel--fsm-trace' to log transitions.")

(defun mevedel--fsm-trace-buffer ()
  "Return the FSM trace buffer, creating it if necessary."
  (get-buffer-create mevedel--fsm-trace-buffer-name))

(defun mevedel--fsm-trace (machine &optional _new-state)
  "Log MACHINE's current FSM state to the trace buffer.

Installed as :after advice on `gptel--fsm-transition' by
`mevedel-debug-fsm-trace-enable'.  Writes one line per transition
containing: timestamp, the new state, the attached agent
invocation's turn count (or `-' for the main chat), the length of
the outgoing message vector, and the list of handler symbols
registered for the new state.  Only writes when
`mevedel--fsm-trace-enabled' is non-nil so the advice can be left
installed but silenced."
  (when mevedel--fsm-trace-enabled
    (let* ((state (gptel-fsm-state machine))
           (info (gptel-fsm-info machine))
           (data (plist-get info :data))
           (msgs (plist-get data :messages))
           (msg-count (cond ((vectorp msgs) (length msgs))
                            ((listp msgs) (length msgs))
                            (t 0)))
           (inv (and (fboundp 'mevedel-tools--agent-invocation-at)
                     (mevedel-tools--agent-invocation-at machine)))
           (turn (if inv (mevedel-agent-invocation-turn-count inv) '-))
           (handlers (mapcar (lambda (h) (if (symbolp h) h '<lambda>))
                             (alist-get state (gptel-fsm-handlers machine)))))
      (with-current-buffer (mevedel--fsm-trace-buffer)
        (goto-char (point-max))
        (insert (format "[%s] state=%s turn=%s msgs=%d handlers=%S\n"
                        (format-time-string "%H:%M:%S.%3N")
                        state turn msg-count handlers))))))

(defun mevedel-debug-fsm-trace-enable ()
  "Enable FSM transition tracing.

Creates the trace buffer, installs `mevedel--fsm-trace' as :after
advice on `gptel--fsm-transition', and flips
`mevedel--fsm-trace-enabled' on.  Safe to call repeatedly."
  (interactive)
  (mevedel--fsm-trace-buffer)
  (advice-add 'gptel--fsm-transition :after #'mevedel--fsm-trace)
  (setq mevedel--fsm-trace-enabled t)
  (message "mevedel FSM trace enabled (buffer: %s)"
           mevedel--fsm-trace-buffer-name))

(defun mevedel-debug-fsm-trace-disable ()
  "Disable FSM transition tracing and remove the advice."
  (interactive)
  (setq mevedel--fsm-trace-enabled nil)
  (advice-remove 'gptel--fsm-transition #'mevedel--fsm-trace)
  (message "mevedel FSM trace disabled"))

(defun mevedel-debug-fsm-trace-clear ()
  "Erase the FSM trace buffer."
  (interactive)
  (with-current-buffer (mevedel--fsm-trace-buffer)
    (erase-buffer)))

(provide 'mevedel-debug)

;;; mevedel-debug.el ends here
