;;; test-mevedel-collaboration-task.el --- Browser task sharing tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests session-task projection and change-latched browser publication.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-task)
(require 'mevedel-collaboration-transport)
(require 'mevedel-structs)


(mevedel-deftest mevedel-collaboration--task-row
  (:doc "projects one task with optional owner and dependency fields")
  (let ((row (mevedel-collaboration--task-row
              (mevedel-task--create
               :id 2 :subject "Fix propagation" :status 'in-progress
               :owner "/root/worker-1" :blocked-by '(1))))
        (plain (mevedel-collaboration--task-row
                (mevedel-task--create
                 :id 1 :subject "Trace the grant" :status 'pending))))
    (should (equal 2 (cdr (assoc "id" row))))
    (should (equal "in-progress" (cdr (assoc "status" row))))
    (should (equal "Fix propagation" (cdr (assoc "subject" row))))
    (should (equal "/root/worker-1" (cdr (assoc "owner" row))))
    (should (equal [1] (cdr (assoc "blockedBy" row))))
    (should-not (assoc "owner" plain))
    (should-not (assoc "blockedBy" plain))))

(mevedel-deftest mevedel-collaboration--tasks-frame
  ()
  ,test
  (test)
  :doc "prioritizes active and recent completed tasks within the byte budget"
  (let* ((old (mevedel-task--create
               :id 1 :subject "Old completion" :status 'completed
               :completed-turn 2))
         (pending (mevedel-task--create
                   :id 2 :subject "Pending" :status 'pending))
         (recent (mevedel-task--create
                  :id 3 :subject "Recent completion" :status 'completed
                  :completed-turn 8))
         (running (mevedel-task--create
                   :id 4 :subject "Running" :status 'in-progress))
         (session (mevedel-session--create
                   :name "tasks" :tasks (list old pending recent running)))
         (room (list :session session))
         (expected
          (list :t "tasks"
                :tasks (vconcat (mapcar #'mevedel-collaboration--task-row
                                        (list running pending recent)))
                :total 4 :completed 2 :omitted 1 :omittedActive 0))
         (mevedel-collaboration--task-frame-max-bytes
          (string-bytes (mevedel-collaboration--json-string expected)))
         (frame (mevedel-collaboration--tasks-frame room)))
    (should (equal expected frame))
    (should (< mevedel-collaboration--task-frame-max-bytes
               mevedel-collaboration--max-frame-json-bytes))
    (should (<= (string-bytes (mevedel-collaboration--json-string frame))
                mevedel-collaboration--task-frame-max-bytes)))

  :doc "reports an active task that cannot fit"
  (let* ((session (mevedel-session--create
                   :name "tasks"
                   :tasks (list (mevedel-task--create
                                 :id 1 :subject (make-string 200 ?x)
                                 :status 'in-progress)
                                (mevedel-task--create
                                 :id 2 :subject "Lower priority"
                                 :status 'pending))))
         (room (list :session session))
         (expected (list :t "tasks" :tasks [] :total 2 :completed 0
                         :omitted 2 :omittedActive 2))
         (mevedel-collaboration--task-frame-max-bytes
          (string-bytes (mevedel-collaboration--json-string expected))))
    (should (equal expected (mevedel-collaboration--tasks-frame room)))))

(mevedel-deftest mevedel-collaboration--publish-tasks
  (:doc "broadcasts the task list once per change, an emptied list included")
  (let* ((guests (make-hash-table :test #'eql))
         (session (mevedel-session--create
                   :name "tasks"
                   :tasks (list (mevedel-task--create
                                 :id 1 :subject "Trace the grant"
                                 :status 'pending))))
         (room (list :session session :guests guests :transport 'transport))
         sent)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport peer frame)
                 (push (cons peer frame) sent)
                 t)))
      (puthash 1 (list :name "g" :writable nil :ready t) guests)
      (mevedel-collaboration--publish-tasks room)
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
      (mevedel-collaboration--publish-tasks room)
      (should-not sent)
      ;; Clearing the last task broadcasts the empty list, so the
      ;; guest's block is cleared rather than frozen on stale rows.
      (setf (mevedel-session-tasks session) nil)
      (mevedel-collaboration--publish-tasks room)
      (let ((frame (cdr (car sent))))
        (should (equal [] (plist-get frame :tasks)))
        (should (= 0 (plist-get frame :total)))
        (should (= 0 (plist-get frame :omitted)))))))

(provide 'test-mevedel-collaboration-task)

;;; test-mevedel-collaboration-task.el ends here
