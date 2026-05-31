;;; test-mevedel-tool-task.el --- Tests for mevedel-tool-task.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-agents)
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
                 (let ((prompt-start (point)))
                   (insert (mevedel-view--input-prompt-string))
                   (add-text-properties
                    prompt-start (point)
                    '(read-only t
                      mevedel-view-prompt t
                      front-sticky (read-only mevedel-view-prompt)
                      rear-nonsticky
                      (read-only mevedel-view-prompt font-lock-face)))
                   (setq-local mevedel-view--status-marker
                               (copy-marker prompt-start t))
                   (setq-local mevedel-view--interaction-marker
                               (copy-marker prompt-start t))
                   (setq-local mevedel-view--input-marker
                               (copy-marker prompt-start nil)))))
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

  :doc "accepts a Lisp list of task plists as a batch"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (list (list :subject "A")
                        (list :subject "B"))))
    (let ((tasks (mevedel-session-tasks session)))
      (should (= 2 (length tasks)))
      (should (equal '("A" "B")
                     (mapcar #'mevedel-task-subject tasks)))))

  :doc "accepts a top-level status note for the current owner"
  (test-mevedel-tool-task--with-session session
    (let ((result
           (mevedel-tool-task--handle-create
            (list :tasks (vector (list :subject "active"))
                  :note "Implementing task notes"))))
      (should (string-match-p "Status note for Main"
                              result))
      (should (equal "Implementing task notes"
                     (mevedel-tool-task--status-note session nil)))
      (should (string-match-p
               "└ Implementing task notes"
               (substring-no-properties
                (mevedel-tool-task--format-groups session))))))

  :doc "ignores nil and json-false optional note placeholders"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "active"))
           :note "Keep this note"))
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "another"))
           :note nil :noteOwner nil))
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "third"))
           :note :json-false :noteOwner :json-false))
    (should (equal "Keep this note"
                   (mevedel-tool-task--status-note session nil))))

  :doc "completed task creation records completed-turn and expands on request"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-turn-count session) 4)
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "already done"
                                :status "completed"))))
    (let ((task (car (mevedel-session-tasks session))))
      (should (= 5 (mevedel-task-completed-turn task)))
      (should-not (string-match-p
                   "already done"
                   (substring-no-properties
                    (mevedel-tool-task--format-groups session))))
      (should (string-match-p
               "already done"
               (substring-no-properties
                (mevedel-tool-task--format-groups session t))))))

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

  :doc "uses the current agent id as owner when owner is omitted"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "main task"))))
    (let ((inv (mevedel-agent-invocation--create
                :agent-id "explorer--abc123")))
      (let ((mevedel--agent-invocation inv))
        (mevedel-tool-task--handle-create
         (list :tasks (vector (list :subject "agent task"))))))
    (let* ((tasks (mevedel-session-tasks session))
           (agent-task (cadr tasks))
           (display (substring-no-properties
                     (mevedel-tool-task--format-groups session))))
	      (should (equal "explorer--abc123"
	                     (mevedel-task-owner agent-task)))
	      (should (string-match-p "Main · 1 open · 0 done" display))
	      (should (string-match-p
	               "explorer--abc123 · 1 open · 0 done"
	               display))))

  :doc "abbreviates long agent owner IDs in the display"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "agent task" :status 'pending
                 :owner "explorer--583db40e450f742e3bda29e88efbac03")))
    (let ((display (substring-no-properties
                    (mevedel-tool-task--format-groups session))))
      (should (string-match-p
               "explorer--583db40e · 1 open · 0 done"
               display))
      (should-not (string-match-p
                   "explorer--583db40e450f742e3bda29e88efbac03"
                   display))))

  :doc "explicit empty owner still creates a Main task in an agent"
  (test-mevedel-tool-task--with-session session
    (let ((inv (mevedel-agent-invocation--create
                :agent-id "explorer--abc123")))
      (let ((mevedel--agent-invocation inv))
        (mevedel-tool-task--handle-create
         (list :tasks (vector (list :subject "main task"
                                    :owner ""))))))
    (let ((task (car (mevedel-session-tasks session))))
      (should (null (mevedel-task-owner task)))))

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

  :doc "keeps status notes until an owner has no open tasks"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "A")
                          (list :subject "B"))))
    (mevedel-tool-task--handle-update
     (list :id 1 :status "in_progress" :note "Working on A"))
    (should (equal "Working on A"
                   (mevedel-tool-task--status-note session nil)))
    (mevedel-tool-task--handle-update
     (list :id 1 :status "completed"))
    (should (equal "Working on A"
                   (mevedel-tool-task--status-note session nil)))
    (mevedel-tool-task--handle-update
     (list :id 2 :note nil))
    (should (equal "Working on A"
                   (mevedel-tool-task--status-note session nil)))
    (mevedel-tool-task--handle-update
     (list :id 2 :note :json-false))
    (should (equal "Working on A"
                   (mevedel-tool-task--status-note session nil)))
    (mevedel-tool-task--handle-update
     (list :id 2 :status "completed"))
    (should (null (mevedel-tool-task--status-note session nil))))

  :doc "clears status notes on an explicit empty update note"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "A")
                          (list :subject "B"))))
    (mevedel-tool-task--handle-update
     (list :id 1 :note "Working through the task list"))
    (mevedel-tool-task--handle-update
     (list :id 2 :note ""))
    (should (null (mevedel-tool-task--status-note session nil))))

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
  (:doc "`mevedel-tool-task--format-groups' groups and compacts completed tasks")
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
      (let ((main-pos (string-match "Main · 1 open · 0 done" text))
            (alpha-pos (string-match "alpha · 1 open · 0 done" text))
            (zeta-pos (string-match "zeta · 1 open · 0 done" text)))
        (should main-pos)
        (should alpha-pos)
        (should zeta-pos)
        (should (< main-pos alpha-pos))
        (should (< alpha-pos zeta-pos)))))

  :doc "hides completed tasks by default and expands all completed tasks"
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
      (should-not (string-match-p "recent done" text))
      (should-not (string-match-p "old done 1" text))
      (should-not (string-match-p "old done 2" text))
      (should-not (string-match-p "completed hidden" text)))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session t))))
      (should (string-match-p "active" text))
      (should (string-match-p "recent done" text))
      (should (string-match-p "old done 1" text))
      (should (string-match-p "old done 2" text))
      (should-not (string-match-p "completed hidden" text))))

  :doc "keeps active ordering stable when completed rows are expanded"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 6 :subject "done C" :status 'completed)
                (mevedel-task--create
                 :id 5 :subject "done B" :status 'completed)
                (mevedel-task--create
                 :id 4 :subject "done A" :status 'completed)
                (mevedel-task--create
                 :id 3 :subject "open C" :status 'pending)
                (mevedel-task--create
                 :id 2 :subject "open B" :status 'pending)
                (mevedel-task--create
                 :id 1 :subject "open A" :status 'pending)))
    (dolist (show-completed '(nil t))
      (let ((text (substring-no-properties
                   (mevedel-tool-task--format-groups
                    session show-completed nil 12))))
        (should (< (string-match "#1 open A" text)
                   (string-match "#2 open B" text)))
        (should (< (string-match "#2 open B" text)
                   (string-match "#3 open C" text))))))

  :doc "default display keeps open tasks visible"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "hidden body" :status 'pending)))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      (should (string-match-p "Main · 1 open · 0 done" text))
      (should (string-match-p "hidden body" text))))

  :doc "default display keeps completed-only groups as summaries"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "done only" :status 'completed)))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      (should (string-match-p "Main · 0 open · 1 done" text))
      (should-not (string-match-p "done only" text))))

  :doc "formats blocked tasks and agent activity compactly"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "inspect ui" :status 'in-progress
                 :owner "explorer"
                 :metadata '(:activity "reading TaskListV2"))
                (mevedel-task--create
                 :id 2 :subject "verify overlay" :status 'pending
                 :owner "worker"
                 :blocked-by '(1 7))))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      (should (string-match-p
               "→ #1 inspect ui · reading TaskListV2" text))
      (should (string-match-p
               "○ #2 verify overlay · blocked by #1, #7" text))
      (should-not (string-match-p "@explorer" text))))

  :doc "uses requested status glyphs and task faces"
  (let* ((done (mevedel-task--create
                :id 1 :subject "done" :status 'completed))
         (running (mevedel-task--create
                   :id 2 :subject "running" :status 'in-progress))
         (pending (mevedel-task--create
                   :id 3 :subject "pending" :status 'pending))
         (done-line (mevedel-tool-task--format-one done))
         (running-line (mevedel-tool-task--format-one running))
         (pending-line (mevedel-tool-task--format-one pending)))
    (should (string-prefix-p "✔ #1 done"
                             (substring-no-properties done-line)))
    (should (string-prefix-p "→ #2 running"
                             (substring-no-properties running-line)))
    (should (string-prefix-p "○ #3 pending"
                             (substring-no-properties pending-line)))
    (should (eq 'mevedel-tool-task-completed
                (get-text-property
                 (string-match "done"
                               (substring-no-properties done-line))
                 'face done-line)))
    (should (eq 'mevedel-tool-task-completed
                (get-text-property
                 (string-match "done"
                               (substring-no-properties done-line))
                 'font-lock-face done-line)))
    (should (eq 'mevedel-tool-task-in-progress
                (get-text-property
                 (string-match "running"
                               (substring-no-properties running-line))
                 'face running-line)))
    (should (eq 'mevedel-tool-task-in-progress
                (get-text-property
                 (string-match "running"
                               (substring-no-properties running-line))
                 'font-lock-face running-line)))
    (should (eq 'default
                (get-text-property
                 (string-match "pending"
                               (substring-no-properties pending-line))
                 'face pending-line))))

  :doc "renders owner status notes under matching open groups only"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main active" :status 'pending)
                (mevedel-task--create
                 :id 2 :subject "agent active" :status 'pending
                 :owner "worker")
                (mevedel-task--create
                 :id 3 :subject "done only" :status 'completed
                 :owner "done-owner")))
    (mevedel-tool-task--set-status-note session nil "Main note")
    (mevedel-tool-task--set-status-note session "worker" "Worker note")
    (mevedel-tool-task--set-status-note session "done-owner" "Hidden note")
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      (should (string-match-p "Main · 1 open · 0 done\n  └ Main note" text))
      (should (string-match-p "worker · 1 open · 0 done\n  └ Worker note"
                              text))
      (should-not (string-match-p "Hidden note" text))))

  :doc "line cap counts rendered note rows"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main active" :status 'pending)
                (mevedel-task--create
                 :id 2 :subject "agent active" :status 'pending
                 :owner "worker")))
    (mevedel-tool-task--set-status-note session nil "Main note")
    (mevedel-tool-task--set-status-note session "worker" "Worker note")
    (let* ((text (substring-no-properties
                  (mevedel-tool-task--format-groups session nil nil 5)))
           (lines (split-string text "\n" t)))
      (should (= 5 (length lines)))
      (should (string-match-p "Main note" text))
      (should-not (string-match-p "Worker note" text))))

  :doc "line cap prioritizes open rows and summarizes completed rows"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "blocked active" :status 'pending
                 :blocked-by '(9))
                (mevedel-task--create
                 :id 2 :subject "done one" :status 'completed)
                (mevedel-task--create
                 :id 3 :subject "running active" :status 'in-progress)
                (mevedel-task--create
                 :id 4 :subject "plain active" :status 'pending)
                (mevedel-task--create
                 :id 5 :subject "done two" :status 'completed)))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session t nil 5))))
      (should (string-match-p "Main · 3 open · 2 done" text))
      (should (< (string-match "running active" text)
                 (string-match "plain active" text)))
      (should (< (string-match "plain active" text)
                 (string-match "blocked active" text)))
      (should (string-match-p "… 2 completed" text))
      (should-not (string-match-p "done one" text))
      (should-not (string-match-p "done two" text))))

  :doc "line cap summarizes omitted open rows"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "plain zero" :status 'pending)
                (mevedel-task--create
                 :id 2 :subject "plain one" :status 'pending)
                (mevedel-task--create
                 :id 3 :subject "plain two" :status 'pending)
                (mevedel-task--create
                 :id 4 :subject "plain three" :status 'pending)))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session nil nil 4))))
      (should (string-match-p "plain zero" text))
      (should (string-match-p "plain one" text))
      (should (string-match-p "… 2 more open" text))
      (should-not (string-match-p "plain two" text))
      (should-not (string-match-p "plain three" text))))

  :doc "line cap does not let completed-only summaries displace active groups"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main done" :status 'completed)
                (mevedel-task--create
                 :id 2 :subject "agent active" :status 'pending
                 :owner "worker")))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session nil nil 2))))
      (should (string-match-p "worker · 1 open · 0 done" text))
      (should (string-match-p "agent active" text))
      (should-not (string-match-p "Main · 0 open · 1 done" text))
      (should-not (string-match-p "main done" text))))

  :doc "expanded line cap does not let completed rows displace later active groups"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "blocked active" :status 'pending
                 :blocked-by '(9))
                (mevedel-task--create
                 :id 2 :subject "done one" :status 'completed)
                (mevedel-task--create
                 :id 3 :subject "running active" :status 'in-progress
                 :owner "explorer"
                 :metadata '(:activity "reading TaskListV2"))
                (mevedel-task--create
                 :id 4 :subject "plain active" :status 'pending)
                (mevedel-task--create
                 :id 5 :subject "done two" :status 'completed)))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session t nil 5))))
      (should (string-match-p "running active" text))
      (should (string-match-p "plain active" text))
      (should (string-match-p "blocked active" text))
      (should-not (string-match-p "done one" text))
      (should-not (string-match-p "done two" text))))

  :doc "line cap prefers agent in-progress groups over earlier pending groups"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main pending one" :status 'pending)
                (mevedel-task--create
                 :id 2 :subject "main pending two" :status 'pending)
                (mevedel-task--create
                 :id 3 :subject "main pending three" :status 'pending)
                (mevedel-task--create
                 :id 4 :subject "worker running" :status 'in-progress
                 :owner "worker")))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session nil nil 4))))
      (should (string-match-p "worker running" text))
      (should (string-match-p "main pending one" text))
      (should (string-match-p "… 2 more open" text))))

  :doc "line cap prefers unblocked pending over later blocked rows"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main running" :status 'in-progress)
                (mevedel-task--create
                 :id 2 :subject "main blocked one" :status 'pending
                 :blocked-by '(1))
                (mevedel-task--create
                 :id 3 :subject "main blocked two" :status 'pending
                 :blocked-by '(1))
                (mevedel-task--create
                 :id 4 :subject "worker unblocked" :status 'pending
                 :owner "worker")))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session nil nil 4))))
      (should (string-match-p "main running" text))
      (should (string-match-p "worker unblocked" text))
      (should (string-match-p "… 2 more open" text))
      (should-not (string-match-p "main blocked one" text))
      (should-not (string-match-p "main blocked two" text))))

  :doc "line cap keeps later owner rows visible before same-owner extras"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main running one" :status 'in-progress)
                (mevedel-task--create
                 :id 2 :subject "main running two" :status 'in-progress)
                (mevedel-task--create
                 :id 3 :subject "main running three" :status 'in-progress)
                (mevedel-task--create
                 :id 4 :subject "worker unblocked" :status 'pending
                 :owner "worker")))
    (let* ((text (substring-no-properties
                  (mevedel-tool-task--format-groups session nil nil 4)))
           (lines (split-string text "\n" t)))
      (should (= 4 (length lines)))
      (should (string-match-p "main running one" text))
      (should (string-match-p "worker unblocked" text))
      (should (string-match-p "… 2 more open" text))
      (should-not (string-match-p "main running two" text))
      (should-not (string-match-p "main running three" text))))

  :doc "expanded line cap reserves space before showing completed summary"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "done one" :status 'completed
                 :owner "worker")
                (mevedel-task--create
                 :id 2 :subject "done two" :status 'completed
                 :owner "worker")))
    (let* ((text (substring-no-properties
                  (mevedel-tool-task--format-groups session t nil 2)))
           (lines (split-string text "\n" t)))
      (should (= 2 (length lines)))
      (should (string-match-p "worker · 0 open · 2 done" text))
      (should (string-match-p "done one" text))
      (should (string-match-p "… 1 completed" text))))

  :doc "expanded line cap summarizes completed-only owners omitted by cap"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main done" :status 'completed)
                (mevedel-task--create
                 :id 2 :subject "worker done" :status 'completed
                 :owner "worker")))
    (let* ((text (substring-no-properties
                  (mevedel-tool-task--format-groups session t nil 2)))
           (lines (split-string text "\n" t)))
      (should (= 2 (length lines)))
      (should (string-match-p "Main · 0 open · 1 done" text))
      (should (string-match-p "main done" text))
      (should (string-match-p "… 1 completed" text))
      (should-not (string-match-p "worker done" text))))

  :doc "default line cap summarizes completed-only owners omitted by cap"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main done" :status 'completed)
                (mevedel-task--create
                 :id 2 :subject "worker done" :status 'completed
                 :owner "worker")))
    (let* ((text (substring-no-properties
                  (mevedel-tool-task--format-groups session nil nil 1)))
           (lines (split-string text "\n" t)))
      (should (= 1 (length lines)))
      (should (string-match-p "Main · 0 open · 1 done" text))
      (should (string-match-p "… 1 completed" text))
      (should-not (string-match-p "worker · 0 open · 1 done" text))
      (should-not (string-match-p "main done" text))
      (should-not (string-match-p "worker done" text))))

  :doc "line cap summarizes open rows hidden in wholly omitted groups"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main running" :status 'in-progress)
                (mevedel-task--create
                 :id 2 :subject "worker unblocked" :status 'pending
                 :owner "worker")
                (mevedel-task--create
                 :id 3 :subject "zeta blocked" :status 'pending
                 :owner "zeta"
                 :blocked-by '(1))))
    (let* ((text (substring-no-properties
                  (mevedel-tool-task--format-groups session nil nil 4)))
           (lines (split-string text "\n" t)))
      (should (= 4 (length lines)))
      (should (string-match-p "main running" text))
      (should (string-match-p "worker unblocked" text))
      (should (string-match-p "… 1 more open" text))
      (should-not (string-match-p "zeta blocked" text))))

  :doc "expanded line cap uses a spare line for omitted completed summary"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main running" :status 'in-progress)
                (mevedel-task--create
                 :id 2 :subject "worker unblocked" :status 'pending
                 :owner "worker")
                (mevedel-task--create
                 :id 3 :subject "zeta done" :status 'completed
                 :owner "zeta")))
    (let* ((text (substring-no-properties
                  (mevedel-tool-task--format-groups session t nil 5)))
           (lines (split-string text "\n" t)))
      (should (= 5 (length lines)))
      (should (string-match-p "main running" text))
      (should (string-match-p "worker unblocked" text))
      (should (string-match-p "… 1 completed" text))
      (should-not (string-match-p "zeta done" text))))

  :doc "expanded line cap summarizes completed-only owners when active uses cap"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main running" :status 'in-progress)
                (mevedel-task--create
                 :id 2 :subject "main pending extra" :status 'pending)
                (mevedel-task--create
                 :id 3 :subject "worker pending" :status 'pending
                 :owner "worker")
                (mevedel-task--create
                 :id 4 :subject "zeta done" :status 'completed
                 :owner "zeta")
                (mevedel-task--create
                 :id 5 :subject "omega done" :status 'completed
                 :owner "omega")))
    (let* ((text (substring-no-properties
                  (mevedel-tool-task--format-groups session t nil 5)))
           (lines (split-string text "\n" t)))
      (should (= 5 (length lines)))
      (should (string-match-p "main running" text))
      (should (string-match-p "main pending extra" text))
      (should (string-match-p "worker pending" text))
      (should (string-match-p "… 2 completed" text))
      (should-not (string-match-p "zeta done" text))
      (should-not (string-match-p "omega done" text)))))


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

  :doc "re-rendering preserves multiline composer text starting with >"
  (test-mevedel-tool-task--with-view session data view
    (with-current-buffer view
      (let ((draft "> quoted\nsecond line"))
        (goto-char (mevedel-view--input-start))
        (let ((draft-start (point))
              (inhibit-read-only t))
          (insert draft)
          (remove-text-properties
           draft-start (point)
           '(read-only nil
             mevedel-view-prompt nil
             font-lock-face nil
             face nil
             front-sticky nil
             rear-nonsticky nil)))
        (goto-char (+ (mevedel-view--input-start) 4))
        (with-current-buffer data
          (mevedel-tool-task--handle-create
           (list :tasks (vector (list :subject "one"))))
          (mevedel-tool-task--handle-update
           (list :id 1 :subject "two")))
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (should-not (get-text-property (mevedel-view--input-start)
                                       'read-only))
        (save-excursion
          (let ((display (buffer-substring-no-properties
                          (point-min) mevedel-view--input-marker)))
            (should-not (string-match-p "#1 one" display))
            (should (string-match-p "two" display))
            (goto-char (point-min))
            (search-forward "two" mevedel-view--input-marker)
            (should (get-text-property (match-beginning 0) 'read-only)))))))

  :doc "drifted status marker does not insert task text into composer"
  (test-mevedel-tool-task--with-view session data view
    (with-current-buffer view
      (let ((draft "> quoted\nsecond line"))
        (goto-char (mevedel-view--input-start))
        (let ((draft-start (point))
              (inhibit-read-only t))
          (insert draft)
          (remove-text-properties
           draft-start (point)
           '(read-only nil
             mevedel-view-prompt nil
             font-lock-face nil
             face nil
             front-sticky nil
             rear-nonsticky nil)))
        (set-marker mevedel-view--status-marker (point-max))
        (set-marker mevedel-view--interaction-marker (point-max))
        (set-marker mevedel-view--input-marker (point-max))
        (with-current-buffer data
          (mevedel-tool-task--handle-create
           (list :tasks (vector (list :subject "drift task")))))
        (mevedel-view-refresh-input-prompt)
        (should (string= draft (mevedel-view--input-text)))
        (save-excursion
          (let ((display (buffer-substring-no-properties
                          (point-min) mevedel-view--input-marker))
                (input (buffer-substring-no-properties
                        (mevedel-view--input-start) (point-max))))
            (should (string-match-p "drift task" display))
            (should-not (string-match-p "drift task" input)))))))

  :doc "completed-only tasks do not materialize the overlay"
  (test-mevedel-tool-task--with-view session data view
    (with-current-buffer data
      (mevedel-tool-task--handle-create
       (list :tasks (vector (list :subject "done only"
                                  :status "completed")))))
    (with-current-buffer view
      (should-not (string-match-p "tasks" (buffer-string)))
      (should-not (string-match-p "done only" (buffer-string)))
      (should-not (mevedel-session-task-overlay session))))

  :doc "stale status notes alone do not materialize the overlay"
  (test-mevedel-tool-task--with-view session data view
    (setf (mevedel-session-task-status-notes session)
          '((nil :note "stale note" :updated-turn 1)))
    (with-current-buffer data
      (mevedel-tool-task--display-overlay))
    (with-current-buffer view
      (should-not (string-match-p "tasks" (buffer-string)))
      (should-not (string-match-p "stale note" (buffer-string)))
      (should-not (mevedel-session-task-overlay session))))

  :doc "completing the last open task removes the overlay"
  (test-mevedel-tool-task--with-view session data view
    (with-current-buffer data
      (mevedel-tool-task--handle-create
       (list :tasks (vector (list :subject "last active")))))
    (with-current-buffer view
      (should (string-match-p "last active" (buffer-string)))
      (should (overlayp (mevedel-session-task-overlay session))))
    (with-current-buffer data
      (mevedel-tool-task--handle-update
       (list :id 1 :status "completed")))
    (with-current-buffer view
      (should-not (string-match-p "tasks" (buffer-string)))
      (should-not (string-match-p "last active" (buffer-string)))
      (should-not (mevedel-session-task-overlay session))))

  :doc "TAB toggle hides and shows completed task detail"
  (test-mevedel-tool-task--with-view session data view
    (with-current-buffer data
      (mevedel-tool-task--handle-create
       (list :tasks (vector (list :subject "active body"
                                  :status "in_progress")
                            (list :subject "done body"
                                  :status "completed")))))
    (with-current-buffer view
      (goto-char (point-min))
      (search-forward "active body")
      (should (eq 'mevedel-tool-task-in-progress
                  (get-text-property (1- (point)) 'font-lock-face)))
      (should-not (string-match-p "done body" (buffer-string)))
      (mevedel-toggle-tasks)
      (should (string-match-p "Main · 1 open · 1 done" (buffer-string)))
      (should (string-match-p "active body" (buffer-string)))
      (should (string-match-p "done body" (buffer-string)))
      (goto-char (point-min))
      (search-forward "done body")
      (should (eq 'mevedel-tool-task-completed
                  (get-text-property (1- (point)) 'font-lock-face)))
      (goto-char (point-min))
      (search-forward "Main")
      (mevedel-toggle-tasks)
      (should (string-match-p "active body" (buffer-string)))
      (should-not (string-match-p "done body" (buffer-string)))))

  :doc "`mevedel-toggle-todos' remains a compatibility alias"
  (progn
    (should (fboundp 'mevedel-toggle-todos))
    (should (eq (symbol-function 'mevedel-toggle-todos)
                'mevedel-toggle-tasks))))


;;
;;; Task notes

(mevedel-deftest mevedel-tool-task--handle-note
  (:doc "`mevedel-tool-task--handle-note' updates owner-scoped status notes")
  ,test
  (test)
  :doc "sets and clears the main status note"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "main active"))))
    (let ((result (mevedel-tool-task--handle-note
                   (list :note "Finishing task overlay polish"))))
      (should (string-match-p "Status note for Main" result))
      (should (equal "Finishing task overlay polish"
                     (mevedel-tool-task--status-note session nil))))
    (mevedel-tool-task--handle-note (list :note ""))
    (should (null (mevedel-tool-task--status-note session nil))))

  :doc "defaults agent notes to the current agent owner"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "main active"))))
    (let ((inv (mevedel-agent-invocation--create
                :agent-id "explorer--abc123")))
      (let ((mevedel--agent-invocation inv))
        (mevedel-tool-task--handle-create
         (list :tasks (vector (list :subject "agent active"))))
        (mevedel-tool-task--handle-note
         (list :note "Checking the agent-owned path" :owner nil))))
    (mevedel-tool-task--handle-note
     (list :note "Checking the main path"))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      (should (string-match-p
               "Main · 1 open · 0 done\n  └ Checking the main path"
               text))
      (should (string-match-p
               "explorer--abc123 · 1 open · 0 done\n  └ Checking the agent-owned path"
               text))))

  :doc "can target another owner explicitly"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "worker active"
                                :owner "worker-2"))))
    (mevedel-tool-task--handle-note
     (list :owner "worker-2"
           :note "Waiting on the dependency result"))
    (should (equal "Waiting on the dependency result"
                   (mevedel-tool-task--status-note session "worker-2"))))

  :doc "does not store notes for owners without open tasks"
  (test-mevedel-tool-task--with-session session
    (let ((result (mevedel-tool-task--handle-note
                   (list :note "No open task"))))
      (should (string-match-p "not shown" result))
      (should (null (mevedel-session-task-status-notes session)))))

  :doc "requires a note parameter"
  (test-mevedel-tool-task--with-session session
    (should-error
     (mevedel-tool-task--handle-note nil))))


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
