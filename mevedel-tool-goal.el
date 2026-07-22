;;; mevedel-tool-goal.el -- Goal control tool -*- lexical-binding: t -*-

;;; Commentary:

;; Defines the permission-free UpdateGoal terminal-state tool.  The handler
;; accepts only the Goal identity captured by the current root request.

;;; Code:

(eval-when-compile
  (require 'mevedel-tool-registry)
  (require 'mevedel-structs))

(require 'subr-x)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-tools'
(defvar mevedel-tools--current-fsm)

;; `mevedel-structs'
(defvar mevedel--session)

(defun mevedel-tool-goal-update
    (status summary &optional session captured-id)
  "Set SESSION's active Goal to terminal STATUS.
SUMMARY is required for `blocked'.  CAPTURED-ID must match its Goal identity."
  (setq session (or session mevedel--session)
        status (if (stringp status) (intern (downcase status)) status))
  (let ((goal (and session (mevedel-session-goal session))))
    (unless goal (error "No goal to update"))
    (unless (eq (mevedel-goal-status goal) 'active)
      (error "Goal is %s, not active" (mevedel-goal-status goal)))
    (unless (memq status '(complete blocked))
      (error "Invalid Goal status: %s" status))
    (unless (equal captured-id (mevedel-goal-id goal))
      (error "Goal identity is stale"))
    (when (and (eq status 'blocked)
               (or (not (stringp summary)) (string-blank-p summary)))
      (error "Blocked Goal requires a nonblank summary"))
    (setf (mevedel-goal-status goal) status
          (mevedel-goal-reason goal)
          (and (eq status 'blocked) (string-trim summary))
          (mevedel-goal-updated-at goal) (format-time-string "%FT%T%z"))
    (format "Goal status changed to %s" status)))

(defun mevedel-tool-goal--handle-update (args)
  "Handle UpdateGoal ARGS from the current root request."
  (let* ((fsm (and (boundp 'mevedel-tools--current-fsm)
                   mevedel-tools--current-fsm))
         (info (and fsm (gptel-fsm-info fsm))))
    (mevedel-tool-goal-update
     (plist-get args :status)
     (plist-get args :summary)
     mevedel--session
     (plist-get info :mevedel-goal-id))))

(defun mevedel-tool-goal--register ()
  "Register the UpdateGoal control tool."
  (mevedel-define-tool
    :name "UpdateGoal"
    :description "Update the active goal. Use only to mark it achieved or genuinely blocked. Do not use blocked merely because work is hard, slow, uncertain, or incomplete. Do not mark complete because the budget is nearly exhausted or you are stopping. You cannot pause, resume, or re-budget a goal; the user controls those."
    :handler #'mevedel-tool-goal--handle-update
    :args ((status string :required
                   "Required. Set to complete only when the objective is achieved and no required work remains. Set to blocked only after the same blocking condition has recurred for at least three consecutive goal turns and user input or an external change is required."
                   :enum ["complete" "blocked"])
           (summary string :optional
                    "Required for blocked: name the recurring condition and exact input or external change needed."))
    :read-only-p t
    :groups (util)))

(provide 'mevedel-tool-goal)
;;; mevedel-tool-goal.el ends here
