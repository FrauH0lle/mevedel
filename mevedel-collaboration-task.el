;;; mevedel-collaboration-task.el --- browser task sharing -*- lexical-binding: t; -*-

;;; Commentary:

;; Projects the session task list into guest-visible rows and publishes the
;; change-latched tasks frame to collaboration viewers.

;;; Code:

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--broadcast
                  "mevedel-collaboration" (room frame))

;; `mevedel-collaboration-projection'
(declare-function mevedel-collaboration--json-string
                  "mevedel-collaboration-projection" (object))

;; `mevedel-structs'
(declare-function mevedel-session-tasks "mevedel-structs" (session))
(declare-function mevedel-task-blocked-by "mevedel-structs" (task))
(declare-function mevedel-task-completed-turn "mevedel-structs" (task))
(declare-function mevedel-task-id "mevedel-structs" (task))
(declare-function mevedel-task-owner "mevedel-structs" (task))
(declare-function mevedel-task-status "mevedel-structs" (task))
(declare-function mevedel-task-subject "mevedel-structs" (task))


;;
;;; Projection and publication

(defconst mevedel-collaboration--task-frame-max-bytes (* 64 1024)
  "Maximum encoded JSON bytes in one browser task frame.")

(defun mevedel-collaboration--task-row (task)
  "Return TASK's guest-visible row."
  (append
   (list (cons "id" (mevedel-task-id task))
         (cons "subject" (mevedel-task-subject task))
         (cons "status" (symbol-name (mevedel-task-status task))))
   (when-let* ((owner (mevedel-task-owner task)))
     (list (cons "owner" owner)))
   (when-let* ((blocked (mevedel-task-blocked-by task)))
     (list (cons "blockedBy" (vconcat blocked))))))

(defun mevedel-collaboration--tasks-frame (room)
  "Return ROOM's byte-bounded session task-list frame.
Prefer in-progress tasks, then pending tasks, then recent completions."
  (let* ((session (plist-get room :session))
         (tasks (and session (mevedel-session-tasks session)))
         in-progress pending completed)
    (dolist (task tasks)
      (pcase (mevedel-task-status task)
        ('in-progress (push task in-progress))
        ('completed (push task completed))
        (_ (push task pending))))
    (setq in-progress (nreverse in-progress)
          pending (nreverse pending)
          completed
          (sort completed
                (lambda (a b)
                  (let ((a-turn (or (mevedel-task-completed-turn a) -1))
                        (b-turn (or (mevedel-task-completed-turn b) -1)))
                    (if (= a-turn b-turn)
                        (> (mevedel-task-id a) (mevedel-task-id b))
                      (> a-turn b-turn))))))
    (let* ((total (length tasks))
           (completed-count (length completed))
           (omitted total)
           (omitted-active (- total completed-count))
           rows)
      (catch 'full
        (dolist (task (append in-progress pending completed))
          (let* ((active (not (eq (mevedel-task-status task) 'completed)))
                 (next-omitted (1- omitted))
                 (next-omitted-active (- omitted-active (if active 1 0)))
                 (next-rows
                  (append rows (list (mevedel-collaboration--task-row task))))
                 (frame (list :t "tasks" :tasks (vconcat next-rows)
                              :total total :completed completed-count
                              :omitted next-omitted
                              :omittedActive next-omitted-active)))
            ;; ponytail: task writes are sparse; optimize prefix encoding only
            ;; if publication profiling makes this bounded O(n^2) loop visible.
            (if (<= (string-bytes (mevedel-collaboration--json-string frame))
                    mevedel-collaboration--task-frame-max-bytes)
                (setq rows next-rows
                      omitted next-omitted
                      omitted-active next-omitted-active)
              (throw 'full nil)))))
      (list :t "tasks" :tasks (vconcat rows)
            :total total :completed completed-count
            :omitted omitted :omittedActive omitted-active))))

(defun mevedel-collaboration--publish-tasks (room)
  "Broadcast ROOM's task list when it has changed, an emptied one included."
  (let ((frame (mevedel-collaboration--tasks-frame room)))
    (unless (equal frame (plist-get room :tasks))
      (plist-put room :tasks frame)
      (mevedel-collaboration--broadcast room frame))))

(provide 'mevedel-collaboration-task)

;;; mevedel-collaboration-task.el ends here
