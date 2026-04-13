;;; test-mevedel-tool-task.el --- Tests for mevedel-tool-task.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-tool-task)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defun test-mevedel-tool-task--make-session ()
  "Return a fresh session struct for task tests."
  (let ((ws (mevedel-workspace--create
             :type 'project
             :id "/tmp/tasktest/"
             :root "/tmp/tasktest/"
             :name "tasktest")))
    (mevedel-session-create "main" ws)))

(defmacro test-mevedel-tool-task--with-session (session-var &rest body)
  "Bind SESSION-VAR to a fresh session, install it buffer-locally, run BODY."
  (declare (indent 1))
  `(let ((,session-var (test-mevedel-tool-task--make-session))
         (buf (generate-new-buffer " *task-test*")))
     (unwind-protect
         (with-current-buffer buf
           (setq-local mevedel--session ,session-var)
           ,@body)
       (kill-buffer buf))))


;;
;;; Status parsing

(mevedel-deftest mevedel-tool-task--parse-status
  (:doc "`mevedel-tool-task--parse-status' maps strings and symbols to status symbols")
  ,test
  (test)
  :doc "nil and empty string default to pending"
  (progn
    (should (eq 'pending (mevedel-tool-task--parse-status nil)))
    (should (eq 'pending (mevedel-tool-task--parse-status "")))
    (should (eq 'pending (mevedel-tool-task--parse-status :json-false))))

  :doc "string forms parse to symbols"
  (progn
    (should (eq 'pending (mevedel-tool-task--parse-status "pending")))
    (should (eq 'in-progress (mevedel-tool-task--parse-status "in_progress")))
    (should (eq 'in-progress (mevedel-tool-task--parse-status "in-progress")))
    (should (eq 'completed (mevedel-tool-task--parse-status "completed"))))

  :doc "symbols pass through when valid"
  (progn
    (should (eq 'pending (mevedel-tool-task--parse-status 'pending)))
    (should (eq 'in-progress (mevedel-tool-task--parse-status 'in-progress)))
    (should (eq 'completed (mevedel-tool-task--parse-status 'completed))))

  :doc "unknown strings and symbols signal an error"
  (progn
    (should-error (mevedel-tool-task--parse-status "done"))
    (should-error (mevedel-tool-task--parse-status 'blocked))
    (should-error (mevedel-tool-task--parse-status 42))))


;;
;;; Task creation

(mevedel-deftest mevedel-tool-task--handle-create
  (:doc "`mevedel-tool-task--handle-create' creates and stores tasks")
  ,test
  (test)
  :doc "creates a batch of tasks with incrementing IDs"
  (test-mevedel-tool-task--with-session session
    (let ((result (mevedel-tool-task--handle-create
                   (list :tasks
                         (vector
                          (list :subject "first" :status "completed")
                          (list :subject "second" :status "in_progress")
                          (list :subject "third"))))))
      (should (stringp result))
      (let ((tasks (mevedel-session-tasks session)))
        (should (= 3 (length tasks)))
        (should (equal '(1 2 3) (mapcar #'mevedel-task-id tasks)))
        (should (equal '("first" "second" "third")
                       (mapcar #'mevedel-task-subject tasks)))
        (should (equal '(completed in-progress pending)
                       (mapcar #'mevedel-task-status tasks))))))

  :doc "creates a single task from a one-element array"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "only"))))
    (let ((tasks (mevedel-session-tasks session)))
      (should (= 1 (length tasks)))
      (should (equal "only" (mevedel-task-subject (car tasks))))
      (should (eq 'pending (mevedel-task-status (car tasks))))))

  :doc "preserves blockedBy and owner fields"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks
           (vector
            (list :subject "A" :owner "worker-1")
            (list :subject "B" :owner "worker-2" :blockedBy (vector 1)))))
    (let* ((tasks (mevedel-session-tasks session))
           (a (car tasks))
           (b (cadr tasks)))
      (should (equal "worker-1" (mevedel-task-owner a)))
      (should (equal "worker-2" (mevedel-task-owner b)))
      (should (equal '(1) (mevedel-task-blocked-by b)))))

  :doc "rejects tasks with a missing subject"
  (test-mevedel-tool-task--with-session session
    (should-error
     (mevedel-tool-task--handle-create
      (list :tasks (vector (list :status "pending")))))))


;;
;;; Task update

(mevedel-deftest mevedel-tool-task--handle-update
  (:doc "`mevedel-tool-task--handle-update' modifies stored tasks")
  ,test
  (test)
  :doc "updates status and propagates completion to unblock dependents"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks
           (vector (list :subject "A")
                   (list :subject "B" :blockedBy (vector 1)))))
    (let ((tasks (mevedel-session-tasks session)))
      (should (equal '(1) (mevedel-task-blocked-by (cadr tasks)))))
    (mevedel-tool-task--handle-update
     (list :id 1 :status "completed"))
    (let ((tasks (mevedel-session-tasks session)))
      (should (eq 'completed (mevedel-task-status (car tasks))))
      (should (null (mevedel-task-blocked-by (cadr tasks))))))

  :doc "updates owner and subject without touching other fields"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "Original"
                                :description "desc"
                                :owner "old"))))
    (mevedel-tool-task--handle-update
     (list :id 1 :subject "Renamed" :owner "new"))
    (let ((task (car (mevedel-session-tasks session))))
      (should (equal "Renamed" (mevedel-task-subject task)))
      (should (equal "new" (mevedel-task-owner task)))
      (should (equal "desc" (mevedel-task-description task)))))

  :doc "errors on unknown task id"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "A"))))
    (should-error
     (mevedel-tool-task--handle-update
      (list :id 99 :status "completed"))))

  :doc "requires an integer id"
  (test-mevedel-tool-task--with-session session
    (should-error
     (mevedel-tool-task--handle-update (list :status "completed")))))


;;
;;; Task list and get

(mevedel-deftest mevedel-tool-task--handle-list
  (:doc "`mevedel-tool-task--handle-list' lists all tasks and filters by status")
  ,test
  (test)
  :doc "returns a no-task message on empty session"
  (test-mevedel-tool-task--with-session session
    (let ((result (mevedel-tool-task--handle-list nil)))
      (should (stringp result))
      (should (string-match-p "No tasks" result))))

  :doc "returns all tasks when no filter is given"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "one" :status "completed")
                          (list :subject "two" :status "pending"))))
    (let ((result (mevedel-tool-task--handle-list nil)))
      (should (string-match-p "one" result))
      (should (string-match-p "two" result))))

  :doc "filters by status"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "done1" :status "completed")
                          (list :subject "pending1" :status "pending")
                          (list :subject "done2" :status "completed"))))
    (let ((result (mevedel-tool-task--handle-list (list :status "completed"))))
      (should (string-match-p "done1" result))
      (should (string-match-p "done2" result))
      (should-not (string-match-p "pending1" result)))))

(mevedel-deftest mevedel-tool-task--handle-get
  (:doc "`mevedel-tool-task--handle-get' returns task details")
  ,test
  (test)
  :doc "returns subject, owner and description for a known id"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "sample"
                                :description "details here"
                                :owner "agent-x"))))
    (let ((result (mevedel-tool-task--handle-get (list :id 1))))
      (should (string-match-p "sample" result))
      (should (string-match-p "details here" result))
      (should (string-match-p "agent-x" result))))

  :doc "errors on unknown id"
  (test-mevedel-tool-task--with-session session
    (should-error (mevedel-tool-task--handle-get (list :id 1))))

  :doc "requires an integer id"
  (test-mevedel-tool-task--with-session session
    (should-error (mevedel-tool-task--handle-get nil))))


(provide 'test-mevedel-tool-task)
;;; test-mevedel-tool-task.el ends here
