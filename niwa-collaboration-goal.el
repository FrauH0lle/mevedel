;;; niwa-collaboration-goal.el --- Read-only browser Goal facts -*- lexical-binding: t; -*-

;;; Commentary:
;; Publish the durable Goal without changing execution or authority.  Accounting
;; is already-accounted usage, not an estimate of an in-flight provider call.

;;; Code:
(require 'niwa-structs)
(require 'niwa-collaboration-projection)
(require 'niwa-recovery)

;; `niwa-collaboration'
(declare-function niwa-collaboration--broadcast "niwa-collaboration" (room frame))

(defun niwa-collaboration--goal-frame (room)
  "Return ROOM's read-only durable Goal frame, including an absent Goal."
  (let* ((goal (when-let* ((session (plist-get room :session)))
                 (niwa-session-goal session)))
         ;; Two 4 KiB strings remain below 64 KiB even with JSON escaping.
         (objective (and goal (niwa-goal-objective goal)))
         ;; Reasons can contain raw provider/runtime diagnostics.  The shared
         ;; frame never sends them, even to owners; native inspection keeps them.
         (reason (and goal (niwa-goal-reason goal)))
         (public-reason (when reason
                          (if (niwa-goal-reason-diagnostic goal)
                              (niwa-recovery-message (niwa-recovery-category reason))
                            reason))))
    (list :t "goal" :goal
          (if goal
              (list :id (niwa-goal-id goal)
                    :objective (niwa-collaboration--truncate-bytes objective 4096)
                    :status (symbol-name (niwa-goal-status goal))
                    :reason (and public-reason
                                 (niwa-collaboration--truncate-bytes public-reason 4096))
                    :truncated (if (or (> (string-bytes objective) 4096)
                                       (and public-reason (> (string-bytes public-reason) 4096)))
                                   t :json-false)
                    :turns (or (niwa-goal-turns-run goal) 0)
                    :tokenBudget (niwa-goal-token-budget goal)
                    :tokensUsed (or (niwa-goal-tokens-used goal) 0)
                    :incomplete (if (niwa-goal-tokens-incomplete-p goal) t :json-false)
                    :seconds (or (niwa-goal-time-used-seconds goal) 0))
            nil))))

(defun niwa-collaboration--publish-goal (room)
  "Broadcast ROOM's Goal only when its facts change, including clearing it."
  (let ((frame (niwa-collaboration--goal-frame room)))
    (unless (equal frame (plist-get room :goal))
      (plist-put room :goal frame)
      (niwa-collaboration--broadcast room frame))))

(provide 'niwa-collaboration-goal)
;;; niwa-collaboration-goal.el ends here
