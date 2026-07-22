;;; mevedel-turn.el -- Canonical turn settlement -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the terminal transaction shared by normal gptel turns and direct skill
;; forks.  Every settlement step is isolated so one failure cannot skip later
;; cleanup, persistence, hook delivery, or queued-message drainage.

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-chat'
(declare-function mevedel--implementation-permission-mode-restore
                  "mevedel-chat" ())

;; `mevedel-compact'
(declare-function mevedel--compact-record-token-baseline
                  "mevedel-compact" (fsm))

;; `mevedel-goal'
(declare-function mevedel-goal-dispatch-after-turn "mevedel-goal" (fsm))
(declare-function mevedel-goal-persist-failure "mevedel-goal" (fsm))
(declare-function mevedel-goal-settle-failure
                  "mevedel-goal" (fsm &optional status))
(declare-function mevedel-goal-settle-turn "mevedel-goal" (fsm))

;; `mevedel-hooks'
(declare-function mevedel-hooks-event-plist
                  "mevedel-hooks"
                  (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-run-event
                  "mevedel-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence" (session buffer))
(defvar mevedel-session--read-only-mode)
(defvar mevedel-session--save-failed)

;; `mevedel-structs'
(declare-function mevedel-request-end "mevedel-structs" ())
(declare-function mevedel-request-id "mevedel-structs" (cl-x))
(declare-function mevedel-request-origin "mevedel-structs" (cl-x))
(declare-function mevedel-request-started-at "mevedel-structs" (cl-x))
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-view-composer'
(declare-function mevedel-view--schedule-queued-user-message-drain
                  "mevedel-view-composer" (fsm))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))


;;
;;; Terminal transaction

(defun mevedel--turn-record-settlement (fsm outcome)
  "Record terminal OUTCOME for FSM's active request."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (and (bound-and-true-p mevedel--session)
                 (bound-and-true-p mevedel--current-request)
                 (fboundp 'mevedel-telemetry-record))
        (let* ((request mevedel--current-request)
               (tokens (or (plist-get info :tokens-full)
                           (plist-get info :tokens)))
               (started-at (mevedel-request-started-at request)))
          (mevedel-telemetry-record
           mevedel--session 'request-settled
           :request-id (mevedel-request-id request)
           :origin (mevedel-request-origin request)
           :outcome outcome
           :duration-ms (and started-at
                             (round
                              (* 1000.0
                                 (float-time
                                  (time-subtract (current-time)
                                                 started-at)))))
           :provider-status (plist-get info :status)
           :token-source (if (plist-get info :tokens-full)
                             'tokens-full
                           'tokens)
           :input-tokens (and (listp tokens) (plist-get tokens :input))
           :output-tokens (and (listp tokens) (plist-get tokens :output))
           :cached-tokens (and (listp tokens)
                               (or (plist-get tokens :cached)
                                   (plist-get tokens :cache-read)
                                   (plist-get tokens :cache_read)))))))))

(defun mevedel--fsm-error-message (fsm)
  "Return a compact error message for FSM, or nil."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (error (plist-get info :error))
         (status (plist-get info :status))
         (error-type (and (listp error) (plist-get error :type)))
         (error-message (and (listp error) (plist-get error :message))))
    (or error-message
        (and error-type status (format "%s: %s" error-type status))
        (and error-type (format "%s" error-type))
        (and status (format "%s" status)))))

(defun mevedel--run-turn-terminal-hook (fsm event status)
  "Run top-level turn terminal hook EVENT for FSM with STATUS."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (bound-and-true-p mevedel--session)
        (require 'mevedel-hooks)
        (let* ((workspace (mevedel-workspace))
               (request (and (boundp 'mevedel--current-request)
                             mevedel--current-request))
               (reason (and (eq event 'StopFailure)
                            (or (mevedel--fsm-error-message fsm)
                                (symbol-name status)))))
          (mevedel-hooks-run-event
           event
           (mevedel-hooks-event-plist
            event mevedel--session workspace
            :status (symbol-name status)
            :terminal-reason reason)
           #'ignore
           mevedel--session workspace request nil))))))


(defun mevedel--turn-increment (fsm)
  "Increment the session turn count for FSM's live request buffer."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when mevedel--session
        (cl-incf (mevedel-session-turn-count mevedel--session))))))

(defun mevedel--turn-autosave (fsm)
  "Persist the completed turn represented by FSM."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (and mevedel--session
                 (not (bound-and-true-p mevedel-session--read-only-mode)))
        (condition-case err
            (progn
              (mevedel-session-persistence-save mevedel--session chat-buffer)
              (when (bound-and-true-p mevedel-session--save-failed)
                (setq mevedel-session--save-failed nil)
                (force-mode-line-update)))
          (error
           (display-warning 'mevedel
                            (format "Session auto-save failed: %s" err)
                            :warning)
           (setq-local mevedel-session--save-failed t)
           (force-mode-line-update)))))))

(defun mevedel--turn-restore-permission-mode (fsm)
  "Restore any temporary permission mode for FSM's request buffer."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (mevedel--implementation-permission-mode-restore))))

(defun mevedel--turn-end-request (fsm)
  "End the active mevedel request for FSM's request buffer."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (mevedel-request-end))))

(defun mevedel--run-turn-steps (fsm steps)
  "Run FSM through STEPS without allowing one failure to skip the rest."
  (dolist (step steps)
    (funcall (mevedel--safe-fsm-handler step) fsm)))

(defun mevedel--complete-turn (fsm)
  "Run the canonical successful top-level turn transaction for FSM."
  (mevedel--run-turn-steps
   fsm
   (list #'mevedel--turn-increment
         (lambda (machine)
           (mevedel--turn-record-settlement machine 'success))
         #'mevedel-goal-settle-turn
         #'mevedel--compact-record-token-baseline
         #'mevedel--turn-autosave
         (lambda (machine)
           (mevedel--run-turn-terminal-hook machine 'Stop 'completed))
         #'mevedel--turn-restore-permission-mode
         #'mevedel--turn-end-request
         #'mevedel-goal-dispatch-after-turn
         #'mevedel-view--schedule-queued-user-message-drain)))

(defun mevedel--fail-turn (fsm status)
  "Run failure cleanup for FSM with terminal STATUS."
  (mevedel--run-turn-steps
   fsm
   (list #'mevedel--turn-increment
         (lambda (machine)
           (mevedel--turn-record-settlement machine status))
         #'mevedel--compact-record-token-baseline
         (lambda (machine)
           (mevedel-goal-settle-failure machine status))
         (lambda (machine)
           (mevedel--run-turn-terminal-hook
            machine 'StopFailure status))
         #'mevedel--turn-restore-permission-mode
         #'mevedel--turn-end-request
         #'mevedel-goal-persist-failure
         #'mevedel-goal-dispatch-after-turn)))


(defun mevedel--handler-name (handler)
  "Return a compact display name for FSM HANDLER."
  (cond
   ((symbolp handler) (symbol-name handler))
   ((byte-code-function-p handler) "#<byte-code>")
   ((functionp handler) "#<function>")
   (t (format "%S" handler))))

(defun mevedel--safe-fsm-handler (handler)
  "Return a wrapper to run FSM HANDLER without aborting sibling handlers."
  (lambda (fsm)
    (condition-case err
        (funcall handler fsm)
      (error
       (display-warning
        'mevedel
        (format "FSM handler %s failed: %s"
                (mevedel--handler-name handler)
                (error-message-string err))
        :warning)
       nil))))


(provide 'mevedel-turn)
;;; mevedel-turn.el ends here
