;;; mevedel-collaboration-goal.el --- Read-only browser Goal facts -*- lexical-binding: t; -*-

;;; Commentary:
;; Publish the durable Goal without changing execution or authority.  Accounting
;; is already-accounted usage, not an estimate of an in-flight provider call.

;;; Code:
(require 'mevedel-structs)
(require 'mevedel-collaboration-projection)
(require 'mevedel-recovery)

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--broadcast "mevedel-collaboration" (room frame))

(defun mevedel-collaboration--goal-frame (room)
  "Return ROOM's read-only durable Goal frame, including an absent Goal."
  (let* ((goal (when-let* ((session (plist-get room :session)))
                 (mevedel-session-goal session)))
         ;; Two 4 KiB strings remain below 64 KiB even with JSON escaping.
         (objective (and goal (mevedel-goal-objective goal)))
         ;; Reasons can contain raw provider/runtime diagnostics.  The shared
         ;; frame never sends them, even to owners; native inspection keeps them.
         (reason (and goal (mevedel-goal-reason goal)))
         (public-reason (when reason
                          (if (mevedel-goal-reason-diagnostic goal)
                              (mevedel-recovery-message (mevedel-recovery-category reason))
                            reason))))
    (list :t "goal" :goal
          (if goal
              (list :id (mevedel-goal-id goal)
                    :objective (mevedel-collaboration--truncate-bytes objective 4096)
                    :status (symbol-name (mevedel-goal-status goal))
                    :reason (and public-reason
                                 (mevedel-collaboration--truncate-bytes public-reason 4096))
                    :truncated (if (or (> (string-bytes objective) 4096)
                                       (and public-reason (> (string-bytes public-reason) 4096)))
                                   t :json-false)
                    :turns (or (mevedel-goal-turns-run goal) 0)
                    :tokenBudget (mevedel-goal-token-budget goal)
                    :tokensUsed (or (mevedel-goal-tokens-used goal) 0)
                    :incomplete (if (mevedel-goal-tokens-incomplete-p goal) t :json-false)
                    :seconds (or (mevedel-goal-time-used-seconds goal) 0))
            nil))))

(defun mevedel-collaboration--publish-goal (room)
  "Broadcast ROOM's Goal only when its facts change, including clearing it."
  (let ((frame (mevedel-collaboration--goal-frame room)))
    (unless (equal frame (plist-get room :goal))
      (plist-put room :goal frame)
      (mevedel-collaboration--broadcast room frame))))

(provide 'mevedel-collaboration-goal)
;;; mevedel-collaboration-goal.el ends here
