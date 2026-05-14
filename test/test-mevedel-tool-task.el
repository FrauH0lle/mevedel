;;; test-mevedel-tool-task.el --- Tests for mevedel-tool-task.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
(require 'gptel-request)
(require 'mevedel-tool-task)
(require 'mevedel-view)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar gptel--fsm-last)


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

(defmacro test-mevedel-tool-task--with-view (session-var data-var view-var
                                                         &rest body)
  "Bind SESSION-VAR, DATA-VAR, and VIEW-VAR for task view tests."
  (declare (indent 3))
  `(let* ((,session-var (test-mevedel-tool-task--make-session))
          (,data-var (generate-new-buffer " *task-data-test*"))
          (,view-var (generate-new-buffer " *task-view-test*")))
     (unwind-protect
         (with-current-buffer ,data-var
           (setq-local mevedel--session ,session-var)
           (setq-local mevedel--view-buffer ,view-var)
           (insert "assistant text")
           (let ((tracking (copy-marker (point) t)))
             (with-current-buffer ,view-var
               (setq-local mevedel--session ,session-var)
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert "header\n")
                 (setq-local mevedel-view--status-marker
                             (copy-marker (point) t))
                 (setq-local mevedel-view--interaction-marker
                             (copy-marker (point) t))
                 (setq-local mevedel-view--input-marker
                             (copy-marker (point) nil))
                 (insert "> ")))
             (let ((gptel--fsm-last
                    (gptel-make-fsm
                     :info (list :tracking-marker tracking))))
               ,@body)))
       (when (buffer-live-p ,data-var)
         (kill-buffer ,data-var))
       (when (buffer-live-p ,view-var)
         (kill-buffer ,view-var)))))


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
    (setf (mevedel-session-turn-count session) 4)
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
                       (mapcar #'mevedel-task-status tasks)))
        (should (= 5 (mevedel-session-last-task-write-turn session))))))

  :doc "creates a single task from a one-element array"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "only"))))
    (let ((tasks (mevedel-session-tasks session)))
      (should (= 1 (length tasks)))
      (should (equal "only" (mevedel-task-subject (car tasks))))
      (should (eq 'pending (mevedel-task-status (car tasks))))))

  :doc "completed task creation records completed-turn and shows it as recent"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-turn-count session) 4)
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "already done"
                                :status "completed"))))
    (let ((task (car (mevedel-session-tasks session))))
      (should (= 5 (mevedel-task-completed-turn task)))
      (should (string-match-p
               "already done"
               (substring-no-properties
                (mevedel-tool-task--format-groups session))))))

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
      (list :tasks (vector (list :status "pending"))))))

  :doc "rejects a bad batch without partially creating tasks"
  (test-mevedel-tool-task--with-session session
    (should-error
     (mevedel-tool-task--handle-create
      (list :tasks (vector (list :subject "valid")
                           (list :subject "bad" :status "bogus")))))
    (should (null (mevedel-session-tasks session)))
    (should (null (mevedel-session-last-task-write-turn session)))))


;;
;;; Task update

(mevedel-deftest mevedel-tool-task--handle-update
  (:doc "`mevedel-tool-task--handle-update' modifies stored tasks")
  ,test
  (test)
  :doc "updates status and propagates completion to unblock dependents"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-turn-count session) 6)
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
      (should (= 7 (mevedel-task-completed-turn (car tasks))))
      (should (= 7 (mevedel-session-last-task-write-turn session)))
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

  :doc "empty owner means Main/unassigned"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "owned"
                                :owner "worker"))))
    (mevedel-tool-task--handle-update
     (list :id 1 :owner ""))
    (should (null (mevedel-task-owner
                   (car (mevedel-session-tasks session))))))

  :doc "errors on unknown task id"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-turn-count session) 2)
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "A"))))
    (let ((last-write (mevedel-session-last-task-write-turn session)))
      (should (= 3 last-write))
      (should-error
       (mevedel-tool-task--handle-update
        (list :id 99 :status "completed")))
      (should (= last-write
                 (mevedel-session-last-task-write-turn session)))))

  :doc "rejects malformed updates without partial mutation"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "Original"
                                :status "pending"))))
    (let* ((task (car (mevedel-session-tasks session)))
           (last-write (mevedel-session-last-task-write-turn session)))
      (should-error
       (mevedel-tool-task--handle-update
        (list :id 1 :subject "Mutated" :status "bogus")))
      (should (equal "Original" (mevedel-task-subject task)))
      (should (eq 'pending (mevedel-task-status task)))
      (should (= last-write
                 (mevedel-session-last-task-write-turn session)))))

  :doc "preserves completion turn when completed status is unchanged"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "A" :status "completed"))))
    (should-error
     (mevedel-tool-task--handle-update
      (list :id 99 :status "completed")))
    (let ((task (car (mevedel-session-tasks session))))
      (should (= 1 (mevedel-task-completed-turn task)))
      (mevedel-tool-task--handle-update
       (list :id 1 :status "completed"))
      (should (= 1 (mevedel-task-completed-turn task)))))

  :doc "requires an integer id"
  (test-mevedel-tool-task--with-session session
    (should-error
     (mevedel-tool-task--handle-update (list :status "completed")))))


;;
;;; Grouped display

(mevedel-deftest mevedel-tool-task--format-groups
  (:doc "`mevedel-tool-task--format-groups' groups and shortens visible tasks")
  ,test
  (test)
  :doc "groups Main before owner groups sorted by label"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-turn-count session) 10)
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main active" :status 'pending)
                (mevedel-task--create
                 :id 2 :subject "z owned" :status 'in-progress
                 :owner "zeta")
                (mevedel-task--create
                 :id 3 :subject "a owned" :status 'pending
                 :owner "alpha")))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      (let ((main-pos (string-match "Main · 1 active · 0 done" text))
            (alpha-pos (string-match "alpha · 1 active · 0 done" text))
            (zeta-pos (string-match "zeta · 1 active · 0 done" text)))
        (should main-pos)
        (should alpha-pos)
        (should zeta-pos)
        (should (< main-pos alpha-pos))
        (should (< alpha-pos zeta-pos)))))

  :doc "shows recent completed tasks and summarizes older completed tasks"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-turn-count session) 10)
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "active" :status 'pending)
                (mevedel-task--create
                 :id 2 :subject "recent done" :status 'completed
                 :completed-turn 11)
                (mevedel-task--create
                 :id 3 :subject "old done 1" :status 'completed
                 :completed-turn 5)
                (mevedel-task--create
                 :id 4 :subject "old done 2" :status 'completed)))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      (should (string-match-p "active" text))
      (should (string-match-p "recent done" text))
      (should-not (string-match-p "old done 1" text))
      (should-not (string-match-p "old done 2" text))
      (should (string-match-p "2 completed hidden" text))))

  :doc "collapsed display keeps headers and hides task body"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "hidden body" :status 'pending)))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session t))))
      (should (string-match-p "Main · 1 active · 0 done" text))
      (should-not (string-match-p "hidden body" text)))))


;;
;;; View rendering

(mevedel-deftest mevedel-tool-task--display-overlay
  (:doc "`mevedel-tool-task--display-overlay' materializes status-zone text")
  ,test
  (test)
  :doc "inserts read-only task text in the live view buffer"
  (test-mevedel-tool-task--with-view session data view
    (with-current-buffer data
      (mevedel-tool-task--handle-create
       (list :tasks (vector (list :subject "rendered task")))))
    (with-current-buffer view
      (should (string-match-p "rendered task" (buffer-string)))
      (goto-char (point-min))
      (search-forward "rendered task")
      (should (get-text-property (1- (point)) 'read-only))
      (should (overlayp (mevedel-session-task-overlay session)))
      (should (eq view (overlay-buffer
                        (mevedel-session-task-overlay session))))))

  :doc "re-rendering replaces the previous materialized region"
  (test-mevedel-tool-task--with-view session data view
    (with-current-buffer data
      (mevedel-tool-task--handle-create
       (list :tasks (vector (list :subject "one"))))
      (mevedel-tool-task--handle-update
       (list :id 1 :subject "two")))
    (with-current-buffer view
      (should-not (string-match-p "#1 one" (buffer-string)))
      (should (string-match-p "two" (buffer-string)))
      (should (= 1 (how-many "tasks" (point-min) (point-max))))))

  :doc "TAB toggle collapses and expands the materialized task body"
  (test-mevedel-tool-task--with-view session data view
    (with-current-buffer data
      (mevedel-tool-task--handle-create
       (list :tasks (vector (list :subject "toggle body")))))
    (with-current-buffer view
      (goto-char (point-min))
      (search-forward "toggle body")
      (mevedel-toggle-tasks)
      (should (string-match-p "Main · 1 active · 0 done" (buffer-string)))
      (should-not (string-match-p "toggle body" (buffer-string)))
      (goto-char (point-min))
      (search-forward "Main")
      (mevedel-toggle-tasks)
      (should (string-match-p "toggle body" (buffer-string)))))

  :doc "`mevedel-toggle-todos' remains a compatibility alias"
  (progn
    (should (fboundp 'mevedel-toggle-todos))
    (should (eq (symbol-function 'mevedel-toggle-todos)
                'mevedel-toggle-tasks))))


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
