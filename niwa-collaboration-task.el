;;; niwa-collaboration-task.el --- browser task sharing -*- lexical-binding: t; -*-

;;; Commentary:

;; Projects the session task list into guest-visible rows and publishes the
;; change-latched tasks frame to collaboration viewers.

;;; Code:

(require 'json)
(require 'niwa-tool-task)

;; `niwa-collaboration'
(declare-function niwa-collaboration--broadcast
                  "niwa-collaboration" (room frame))

;; `niwa-structs'
(declare-function niwa-session-tasks "niwa-structs" (session))
(declare-function niwa-task-blocked-by "niwa-structs" (task))
(declare-function niwa-task-completed-turn "niwa-structs" (task))
(declare-function niwa-task-id "niwa-structs" (task))
(declare-function niwa-task-owner "niwa-structs" (task))
(declare-function niwa-task-status "niwa-structs" (task))
(declare-function niwa-task-subject "niwa-structs" (task))


;;
;;; Projection and publication

(defconst niwa-collaboration--task-frame-max-bytes (* 64 1024)
  "Maximum encoded JSON bytes in one browser task frame.")

(defun niwa-collaboration--task-row (task)
  "Return TASK's guest-visible row."
  (append
   (list (cons "id" (niwa-task-id task))
         (cons "subject" (niwa-task-subject task))
         (cons "status" (symbol-name (niwa-task-status task))))
   (when-let* ((owner (niwa-task-owner task)))
     (list (cons "owner" owner)))
   (when-let* ((activity (niwa-tool-task--activity-summary task)))
     (list (cons "activeForm" activity)))
   (when-let* ((blocked (niwa-task-blocked-by task)))
     (list (cons "blockedBy" (vconcat blocked))))))

(defun niwa-collaboration--tasks-frame (room)
  "Return ROOM's byte-bounded session task-list frame.
Prefer in-progress tasks, then pending tasks, then recent completions."
  (let* ((session (plist-get room :session))
         (tasks (and session (niwa-session-tasks session)))
         in-progress pending completed)
    (dolist (task tasks)
      (pcase (niwa-task-status task)
        ('in-progress (push task in-progress))
        ('completed (push task completed))
        (_ (push task pending))))
    (setq in-progress (niwa-tool-task--sort-active-tasks in-progress)
          pending (niwa-tool-task--sort-active-tasks pending)
          completed
          (sort completed
                (lambda (a b)
                  (let ((a-turn (or (niwa-task-completed-turn a) -1))
                        (b-turn (or (niwa-task-completed-turn b) -1)))
                    (if (= a-turn b-turn)
                        (> (niwa-task-id a) (niwa-task-id b))
                      (> a-turn b-turn))))))
    (let* ((total (length tasks))
           (completed-count (length completed))
           (omitted total)
           (omitted-active (- total completed-count))
           rows noted-owners)
      (catch 'full
        (dolist (task (append in-progress pending completed))
          (let* ((active (not (eq (niwa-task-status task) 'completed)))
                 (next-omitted (1- omitted))
                 (next-omitted-active (- omitted-active (if active 1 0)))
                 (owner (niwa-task-owner task))
                 (row (niwa-collaboration--task-row task))
                 (note (and active (not (member owner noted-owners))
                            (niwa-tool-task--status-note session owner)))
                 (next-rows
                  (append rows (list (if note (append row (list (cons "note" note))) row))))
                 (frame (list :t "tasks" :tasks (vconcat next-rows)
                              :total total :completed completed-count
                              :omitted next-omitted
                              :omittedActive next-omitted-active)))
            ;; ponytail: task writes are sparse; optimize prefix encoding only
            ;; if publication profiling makes this bounded O(n^2) loop visible.
            (if (<= (string-bytes (json-encode frame))
                    niwa-collaboration--task-frame-max-bytes)
                (setq noted-owners (if note (cons owner noted-owners) noted-owners)
                      rows next-rows
                      omitted next-omitted
                      omitted-active next-omitted-active)
              (throw 'full nil)))))
      (list :t "tasks" :tasks (vconcat rows)
            :total total :completed completed-count
            :omitted omitted :omittedActive omitted-active))))

(defun niwa-collaboration--publish-tasks (room)
  "Broadcast ROOM's task list when it has changed, an emptied one included."
  (let ((frame (niwa-collaboration--tasks-frame room)))
    (unless (equal frame (plist-get room :tasks))
      (plist-put room :tasks frame)
      (niwa-collaboration--broadcast room frame))))

(provide 'niwa-collaboration-task)

;;; niwa-collaboration-task.el ends here
