;;; test-niwa-collaboration-task.el --- Browser task sharing tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests session-task projection and change-latched browser publication.

;;; Code:

(require 'json)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'niwa-collaboration)
(require 'niwa-collaboration-projection)
(require 'niwa-collaboration-task)
(require 'niwa-collaboration-transport)
(require 'niwa-structs)


(niwa-deftest niwa-collaboration--task-row
  (:doc "projects one task with optional owner and dependency fields")
  (let ((row (niwa-collaboration--task-row
              (niwa-task--create
               :id 2 :subject "Fix propagation" :status 'in-progress
               :owner "/root/worker-1" :blocked-by '(1)
               :metadata '(:activeForm "Fixing propagation"))))
        (plain (niwa-collaboration--task-row
                (niwa-task--create
                 :id 1 :subject "Trace the grant" :status 'pending))))
    (should (equal 2 (cdr (assoc "id" row))))
    (should (equal "in-progress" (cdr (assoc "status" row))))
    (should (equal "Fix propagation" (cdr (assoc "subject" row))))
    (should (equal "/root/worker-1" (cdr (assoc "owner" row))))
    (should (equal [1] (cdr (assoc "blockedBy" row))))
    (should (equal "Fixing propagation" (cdr (assoc "activeForm" row))))
    (should-not (assoc "owner" plain))
    (should-not (assoc "blockedBy" plain))))

(niwa-deftest niwa-collaboration--tasks-frame
  ()
  ,test
  (test)
  :doc "prioritizes active and recent completed tasks within the byte budget"
  (let* ((old (niwa-task--create
               :id 1 :subject "Old completion" :status 'completed
               :completed-turn 2))
         (pending (niwa-task--create
                   :id 2 :subject "Pending" :status 'pending))
         (recent (niwa-task--create
                  :id 3 :subject "Recent completion" :status 'completed
                  :completed-turn 8))
         (running (niwa-task--create
                   :id 4 :subject "Running" :status 'in-progress))
         (session (niwa-session--create
                   :name "tasks" :tasks (list old pending recent running)))
         (room (list :session session))
         (expected
          (list :t "tasks"
                :tasks (vconcat (mapcar #'niwa-collaboration--task-row
                                        (list running pending recent)))
                :total 4 :completed 2 :omitted 1 :omittedActive 0))
         (niwa-collaboration--task-frame-max-bytes
          (string-bytes (json-encode expected)))
         (frame (niwa-collaboration--tasks-frame room)))
    (should (equal expected frame))
    (should (< niwa-collaboration--task-frame-max-bytes
               niwa-collaboration--max-frame-json-bytes))
    (should (<= (string-bytes (json-encode frame))
                niwa-collaboration--task-frame-max-bytes)))

  :doc "reports an active task that cannot fit"
  (let* ((session (niwa-session--create
                   :name "tasks"
                   :tasks (list (niwa-task--create
                                 :id 1 :subject (make-string 200 ?x)
                                 :status 'in-progress)
                                (niwa-task--create
                                 :id 2 :subject "Lower priority"
                                 :status 'pending))))
         (room (list :session session))
         (expected (list :t "tasks" :tasks [] :total 2 :completed 0
                         :omitted 2 :omittedActive 2))
         (niwa-collaboration--task-frame-max-bytes
          (string-bytes (json-encode expected))))
    (should (equal expected (niwa-collaboration--tasks-frame room))))

  :doc "uses native active priority and attaches each active owner's note once"
  (let* ((session
          (niwa-session--create
           :tasks (list (niwa-task--create :id 5 :subject "Later running" :status 'in-progress)
                        (niwa-task--create :id 1 :subject "Blocked"
                                             :status 'pending :blocked-by '(3))
                        (niwa-task--create :id 2 :subject "Ready" :status 'pending)
                        (niwa-task--create :id 3 :subject "Running" :status 'in-progress)
                        (niwa-task--create :id 4 :subject "Done" :status 'completed :owner "past"))
           :task-status-notes '((nil :note "Checking evidence")
                                ("past" :note "Hidden old note"))))
         (rows (append (plist-get (niwa-collaboration--tasks-frame
                                  (list :session session)) :tasks) nil)))
    (should (equal '(3 5 2 1 4) (mapcar (lambda (row) (cdr (assoc "id" row))) rows)))
    (should (equal "Checking evidence" (cdr (assoc "note" (car rows)))))
    (should-not (cl-some (lambda (row) (assoc "note" row)) (cdr rows)))))

(niwa-deftest niwa-collaboration--publish-tasks
  (:doc "broadcasts the task list once per change, an emptied list included")
  (let* ((guests (make-hash-table :test #'eql))
         (session (niwa-session--create
                   :name "tasks"
                   :tasks (list (niwa-task--create
                                 :id 1 :subject "Trace the grant"
                                 :status 'pending))))
         (room (list :session session :guests guests :transport 'transport))
         sent)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
               (lambda (_transport peer frame)
                 (push (cons peer frame) sent)
                 t)))
      (puthash 1 (list :name "g" :writable nil :ready t) guests)
      (niwa-collaboration--publish-tasks room)
      (let ((frame (cdr (car sent))))
        (should (equal "tasks" (plist-get frame :t)))
        (should (= 1 (length (plist-get frame :tasks))))
        (should (= 1 (plist-get frame :total)))
        (should (= 0 (plist-get frame :omitted)))
        (should (equal "Trace the grant"
                       (cdr (assoc "subject"
                                   (aref (plist-get frame :tasks) 0))))))
      ;; An unchanged list is not repeated.
      (setq sent nil)
      (niwa-collaboration--publish-tasks room)
      (should-not sent)
      ;; Clearing the last task broadcasts the empty list, so the
      ;; guest's block is cleared rather than frozen on stale rows.
      (setf (niwa-session-tasks session) nil)
      (niwa-collaboration--publish-tasks room)
      (let ((frame (cdr (car sent))))
        (should (equal [] (plist-get frame :tasks)))
        (should (= 0 (plist-get frame :total)))
        (should (= 0 (plist-get frame :omitted)))))))

(provide 'test-niwa-collaboration-task)

;;; test-niwa-collaboration-task.el ends here
