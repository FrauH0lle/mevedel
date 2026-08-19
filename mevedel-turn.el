;;; mevedel-turn.el -- Canonical turn settlement -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the terminal transaction shared by normal gptel turns and direct skill
;; forks.  Every settlement step is isolated so one failure cannot skip later
;; cleanup, persistence, hook delivery, or queued-message drainage.

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)

;; `gptel'
(declare-function gptel-backend-name "ext:gptel" (cl-x) t)
(defvar gptel-backend)

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

;; `mevedel-pending-inputs'
(declare-function mevedel-view--schedule-follow-up-drain
                  "mevedel-pending-inputs" (fsm))

;; `mevedel-plan-handoff'
(declare-function mevedel-plan-handoff-settle-request
                  "mevedel-plan-handoff"
                  (fsm status &optional reason))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence"
                  (session buffer &optional settled))
(defvar mevedel-session--read-only-mode)
(defvar mevedel-session--save-failed)

;; `mevedel-structs'
(declare-function mevedel-request-end "mevedel-structs" ())
(declare-function mevedel-request-id "mevedel-structs" (cl-x))
(declare-function mevedel-request-origin "mevedel-structs" (cl-x))
(declare-function mevedel-request-started-at "mevedel-structs" (cl-x))
(declare-function mevedel-session-pending-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-steering
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-set-pending-input-failure-paused
                  "mevedel-structs" (session paused))
(declare-function mevedel-session-set-pending-inputs
                  "mevedel-structs" (session category entries))
(defvar mevedel--current-request)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-transport'
(declare-function mevedel-transport-run-when-idle
                  "mevedel-transport" (key path thunk))

;; `mevedel-view'
(declare-function mevedel-view-rerender "mevedel-view" (&optional buffer))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" ())

;; `mevedel-view-render'
(declare-function mevedel-view--append-request-summary
                  "mevedel-view-render"
                  (data-buf search-start &optional extra))

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

(defun mevedel--turn-settle-plan-handoff (fsm status)
  "Settle any Direct Plan handoff attached to FSM with STATUS."
  (when (fboundp 'mevedel-plan-handoff-settle-request)
    (mevedel-plan-handoff-settle-request
     fsm status
     (and (not (eq status 'success))
          (or (mevedel--fsm-error-message fsm)
              (if (eq status 'aborted)
                  "Request aborted"
                "Provider request failed"))))))

(defun mevedel--fsm-error-message (fsm)
  "Return a compact error message for FSM, or nil."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (error (plist-get info :error))
         (status (plist-get info :status))
         (error-type (and (listp error) (plist-get error :type)))
         (error-message (and (listp error) (plist-get error :message))))
    (or (and (stringp error) error)
        error-message
        (and error-type status (format "%s: %s" error-type status))
        (and error-type (format "%s" error-type))
        (and status (format "%s" status)))))

(defun mevedel--turn-record-request-failure (fsm)
  "Add FSM's provider failure to its ignored request summary."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (let* ((error-data (plist-get info :error))
             (backend (or (plist-get info :backend) gptel-backend))
             (backend-name
              (and backend
                   (condition-case nil
                       (gptel-backend-name backend)
                     (error nil)))))
        (require 'mevedel-view-render)
        (mevedel-view--append-request-summary
         chat-buffer
         (plist-get info :position)
         (list :outcome 'error
               :backend (or backend-name "Provider")
               :status (plist-get info :status)
               :error-type (and (listp error-data)
                                (plist-get error-data :type))
               :error-code (and (listp error-data)
                                (plist-get error-data :code))
               :error-data error-data
               :message (mevedel--fsm-error-message fsm)
               :retry 'manual))))))

(defun mevedel--run-turn-terminal-hook (fsm event status)
  "Run top-level turn terminal hook EVENT for FSM with STATUS."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (bound-and-true-p mevedel--session)
        (require 'mevedel-hooks)
        (let* ((workspace (mevedel-workspace))
               (reason (and (eq event 'StopFailure)
                            (or (mevedel--fsm-error-message fsm)
                                (symbol-name status)))))
          ;; Deliberately no request: the turn's own teardown drains the
          ;; request's cancellers two settlement steps later, which would
          ;; kill this hook's process before it can settle.  A terminal
          ;; hook outlives its request and is bounded by its own timeout.
          (mevedel-hooks-run-event
           event
           (mevedel-hooks-event-plist
            event mevedel--session workspace
            :status (symbol-name status)
            :terminal-reason reason)
           #'ignore
           mevedel--session workspace nil nil))))))


(defun mevedel--turn-commit (fsm)
  "Commit FSM's request-reserved turn to its live session.
Signal when the request is missing or its reservation is not the next turn."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (unless (and mevedel--session mevedel--current-request)
        (error "Cannot commit turn without an active request"))
      (let ((reserved (mevedel-request-turn mevedel--current-request))
            (expected (1+ (or (mevedel-session-turn-count mevedel--session)
                              0))))
        (unless (equal reserved expected)
          (error "Reserved turn %S does not follow committed turn %S"
                 reserved (mevedel-session-turn-count mevedel--session)))
        (setf (mevedel-session-turn-count mevedel--session) reserved)))))

(defun mevedel--turn-autosave (fsm)
  "Persist the completed turn represented by FSM and refresh its view."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (and mevedel--session
                 (not (bound-and-true-p mevedel-session--read-only-mode)))
        (let (saved)
          (condition-case err
              (progn
                (mevedel-session-persistence-save
                 mevedel--session chat-buffer t)
                (when (bound-and-true-p mevedel-session--save-failed)
                  (setq mevedel-session--save-failed nil)
                  (force-mode-line-update))
                (setq saved t))
            (error
             (display-warning 'mevedel
                              (format "Session auto-save failed: %s" err)
                              :warning)
             (setq-local mevedel-session--save-failed t)
             (force-mode-line-update)))
          (when (and saved (buffer-live-p mevedel--view-buffer))
            (require 'mevedel-view)
            (mevedel-view-rerender mevedel--view-buffer)))))))

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

(defun mevedel--turn-fail-pending-input (fsm)
  "Mark undelivered steering for FSM's dead turn as requiring review."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (and mevedel--session mevedel--current-request)
        (let* ((request-id (mevedel-request-id mevedel--current-request))
               (entries (mevedel-session-pending-steering mevedel--session))
               (failed nil)
               (updated
                (mapcar
                 (lambda (entry)
                   (if (equal request-id (plist-get entry :request-id))
                       (progn
                         (setq failed t)
                         (plist-put (copy-sequence entry)
                                    :state 'failed-turn))
                     entry))
                 entries)))
          (when failed
            (mevedel-session-set-pending-inputs
             mevedel--session 'steering updated)
            (mevedel-session-set-pending-input-failure-paused
             mevedel--session t)
            (when (buffer-live-p mevedel--view-buffer)
              (with-current-buffer mevedel--view-buffer
                (mevedel-view--interaction-rebuild)))))))))

(defun mevedel--run-turn-steps (fsm steps)
  "Run FSM through STEPS without allowing one failure to skip the rest."
  (dolist (step steps)
    (funcall (mevedel--safe-fsm-handler step) fsm)))

(defun mevedel--turn-buffer (fsm)
  "Return FSM's live chat buffer, or nil."
  (when-let* ((info (condition-case nil (gptel-fsm-info fsm) (error nil)))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    buffer))

(defun mevedel--defer-turn-steps (fsm steps)
  "Run FSM through STEPS once no remote operation is in flight.

Each step re-derives its own buffer from FSM, so waiting costs the chain no
context.  Settlement keeps the request open until the chain finishes, so a
Goal continuation or a user send still observes the workflow as busy while
this waits."
  (require 'mevedel-transport)
  (let ((buffer (mevedel--turn-buffer fsm)))
    (mevedel-transport-run-when-idle
     (list 'turn-settlement (or buffer fsm))
     (and buffer (buffer-local-value 'default-directory buffer))
     (lambda () (mevedel--run-turn-steps fsm steps)))))

(defun mevedel--turn-publication-pending-p (fsm)
  "Return non-nil when FSM's session has failed critical publication."
  (when-let* ((info (condition-case nil
                        (gptel-fsm-info fsm)
                      (error nil)))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (and mevedel--session
           (mevedel-session-pending-publication mevedel--session)))))

(defun mevedel--turn-after-publication (function fsm)
  "Call FUNCTION with FSM unless critical publication is pending."
  (unless (mevedel--turn-publication-pending-p fsm)
    (funcall function fsm)))

(defun mevedel--complete-turn (fsm)
  "Run the canonical successful top-level turn transaction for FSM.

The commit is the single-use reservation fence and runs here, synchronously:
it touches no target and nothing may settle if the reservation is wrong.  The
remaining steps publish, so they run once the transport is idle.  gptel drives
this from its process sentinel, which Emacs may dispatch from inside an
unrelated remote operation; publishing from there desynchronizes the
connection and the transaction commits against answers belonging to another
command.

The whole chain defers as one unit rather than step by step, because its order
is load-bearing: `mevedel--turn-end-request\=' follows the autosave, and
inverting them drops the turn's file-history checkpoints."
  (mevedel--turn-commit fsm)
  (mevedel--defer-turn-steps
   fsm
   (list (lambda (machine)
           (mevedel--turn-record-settlement machine 'success))
         (lambda (machine)
           (mevedel--turn-settle-plan-handoff machine 'success))
         #'mevedel-goal-settle-turn
         #'mevedel--compact-record-token-baseline
         #'mevedel--turn-autosave
         (lambda (machine)
           (mevedel--run-turn-terminal-hook machine 'Stop 'completed))
         #'mevedel--turn-restore-permission-mode
         #'mevedel--turn-end-request
         (lambda (machine)
           (mevedel--turn-after-publication
            #'mevedel-goal-dispatch-after-turn machine))
         (lambda (machine)
           (require 'mevedel-pending-inputs)
           (mevedel--turn-after-publication
            #'mevedel-view--schedule-follow-up-drain machine)))))

(defun mevedel--fail-turn (fsm status)
  "Run failure cleanup for FSM with terminal STATUS.

Deferred for the same reason as `mevedel--complete-turn\=': the failure chain
also autosaves, and it reaches here from the same process sentinel."
  (mevedel--turn-commit fsm)
  (mevedel--defer-turn-steps
   fsm
   (append
    (list (lambda (machine)
            (mevedel--turn-record-settlement machine status))
          (lambda (machine)
            (mevedel--turn-settle-plan-handoff machine status))
          #'mevedel--compact-record-token-baseline
          (lambda (machine)
            (mevedel-goal-settle-failure machine status)))
    (and (eq status 'error)
         (list #'mevedel--turn-record-request-failure
               #'mevedel--turn-autosave))
    (list (lambda (machine)
            (mevedel--run-turn-terminal-hook
             machine 'StopFailure status))
          #'mevedel--turn-restore-permission-mode
          #'mevedel--turn-fail-pending-input
          #'mevedel--turn-end-request
          (lambda (machine)
            (mevedel--turn-after-publication
             #'mevedel-goal-persist-failure machine))
          (lambda (machine)
            (mevedel--turn-after-publication
             #'mevedel-goal-dispatch-after-turn machine))))))


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
