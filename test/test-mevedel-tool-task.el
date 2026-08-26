;;; test-mevedel-tool-task.el --- Tests for mevedel-tool-task.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-agents)
(require 'mevedel-agent-control)
(require 'gptel-request)
(require 'mevedel-tool-task)
(require 'mevedel-view)
(require 'mevedel-view-agent)
(require 'mevedel-view-composer)
(require 'mevedel-view-render)
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
  "Return a fresh session struct for task-tool cases."
  (let ((ws (mevedel-workspace--create
             :type 'project
             :id "/tmp/tasktest/"
             :root "/tmp/tasktest/"
             :name "tasktest")))
    (mevedel-session-create "main" ws)))

(defun test-mevedel-tool-task--register-agent (session path)
  "Register a retained agent at PATH in SESSION."
  (let ((record (mevedel-agent-record--create :path path)))
    (setf (mevedel-session-agent-registry session)
          (cons (cons path record)
                (mevedel-session-agent-registry session)))
    record))

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
  "Bind SESSION-VAR, DATA-VAR, and VIEW-VAR, then run BODY."
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
               (setq-local mevedel-view--agent-transcript-p nil)
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
    (let ((result
           (plist-get
            (mevedel-tool-task--handle-create
             (list :tasks
                   (vector
                    (list :subject "first" :status "completed")
                    (list :subject "second" :status "in_progress")
                    (list :subject "third"))))
            :result)))
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
           (plist-get
            (mevedel-tool-task--handle-create
             (list :tasks (vector (list :subject "active"))
                   :note "Implementing task notes"))
            :result)))
      (should (string-match-p "Status note for Main"
                              result))
      (should (equal "Implementing task notes"
                     (mevedel-tool-task--status-note session nil)))
      (should (string-prefix-p
               "Implementing task notes\n"
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

  :doc "uses the current canonical agent path when owner is omitted"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "main task"))))
    (let ((inv (mevedel-agent-invocation--create
                :path "/root/explorer"
                :agent-id "explorer--0123456789abcdef0123456789abcdef")))
      (test-mevedel-tool-task--register-agent session "/root/explorer")
      (let ((mevedel--agent-invocation inv))
        (mevedel-tool-task--handle-create
         (list :tasks (vector (list :subject "agent task"))))))
    (let* ((tasks (mevedel-session-tasks session))
           (agent-task (cadr tasks))
           (display (substring-no-properties
                     (mevedel-tool-task--format-groups session))))
	      (should (equal "/root/explorer"
	                     (mevedel-task-owner agent-task)))
	      (should (string-match-p "#1 main task" display))
	      (should (string-match-p "#2 explorer · agent task" display))))

  :doc "shows agent paths without opaque IDs, shortened for display"
  (test-mevedel-tool-task--with-session session
    (test-mevedel-tool-task--register-agent
     session "/root/worker/explorer")
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "agent task" :status 'pending
                 :owner "/root/worker/explorer")))
    (let ((display (substring-no-properties
                    (mevedel-tool-task--format-groups session))))
      ;; The `/root/' every agent shares is dropped; the rest of the
      ;; canonical path stays, and no opaque ID reaches the panel.
      (should (string-match-p "#1 worker/explorer · agent task" display))
      (should-not (string-match-p "/root/" display))))

  :doc "explicit empty owner still creates a Main task in an agent"
  (test-mevedel-tool-task--with-session session
    (let ((inv (mevedel-agent-invocation--create
                :path "/root/explorer"
                :agent-id "explorer--0123456789abcdef0123456789abcdef")))
      (test-mevedel-tool-task--register-agent session "/root/explorer")
      (let ((mevedel--agent-invocation inv))
        (mevedel-tool-task--handle-create
         (list :tasks (vector (list :subject "main task"
                                    :owner ""))))))
    (let ((task (car (mevedel-session-tasks session))))
      (should (null (mevedel-task-owner task)))))

  :doc "rejects unknown canonical owners at the TaskCreate boundary"
  (test-mevedel-tool-task--with-session session
    (should-error
     (mevedel-tool-task--handle-create
      (list :tasks
            (vector (list :subject "orphan" :owner "/root/ghost")))))
    (should-not (mevedel-session-tasks session)))

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
    (should (null (mevedel-session-last-task-write-turn session))))

  :doc "rejects a subject that is blank once its whitespace is collapsed"
  (test-mevedel-tool-task--with-session session
    (should-error
     (mevedel-tool-task--handle-create
      (list :tasks (vector (list :subject "\n\n")))))
    (should (null (mevedel-session-tasks session)))
    (should (null (mevedel-session-last-task-write-turn session))))

  :doc "stores a subject as the one display line the status fragment budgets"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "one\ntwo\tthree"))))
    (should
     (equal "one two three"
            (mevedel-task-subject (car (mevedel-session-tasks session))))))

  :doc "an unknown noteOwner creates no task and no note"
  (test-mevedel-tool-task--with-session session
    (should-error
     (mevedel-tool-task--handle-create
      (list :tasks (vector (list :subject "first"))
            :note "progress" :noteOwner "/root/ghost")))
    (should (null (mevedel-session-tasks session)))
    (should (null (mevedel-session-task-status-notes session)))
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
     (mevedel-tool-task--handle-update (list :status "completed"))))

  :doc "rejects a blank or multiline subject and keeps the stored one"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "Original"))))
    (let ((task (car (mevedel-session-tasks session))))
      (should-error
       (mevedel-tool-task--handle-update (list :id 1 :subject "")))
      (should (equal "Original" (mevedel-task-subject task)))
      (mevedel-tool-task--handle-update (list :id 1 :subject "one\ntwo"))
      (should (equal "one two" (mevedel-task-subject task)))))

  :doc "an unknown noteOwner leaves the task and its dependents untouched"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "A")
                          (list :subject "B" :blockedBy (vector 1)))))
    (let* ((tasks (mevedel-session-tasks session))
           (first (car tasks))
           (second (cadr tasks))
           (last-write (mevedel-session-last-task-write-turn session)))
      (should-error
       (mevedel-tool-task--handle-update
        (list :id 1 :status "completed"
              :note "progress" :noteOwner "/root/ghost")))
      (should (eq 'pending (mevedel-task-status first)))
      (should (null (mevedel-task-completed-turn first)))
      (should (equal '(1) (mevedel-task-blocked-by second)))
      (should (null (mevedel-session-task-status-notes session)))
      (should (= last-write
                 (mevedel-session-last-task-write-turn session))))))


;;
;;; Agent owner lifecycle

(mevedel-deftest mevedel-tool-task-finalize-owner
  (:doc "`mevedel-tool-task-finalize-owner' reconciles completed sub-agent tasks")
  ,test
  (test)
  :doc "completed agents complete only tasks owned by their canonical path"
  (test-mevedel-tool-task--with-session session
    (test-mevedel-tool-task--register-agent
     session "/root/worker/explorer")
    (setf (mevedel-session-turn-count session) 8)
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main open" :status 'pending)
                (mevedel-task--create
                 :id 2 :subject "agent open" :status 'in-progress
                 :owner "/root/worker/explorer")
                (mevedel-task--create
                 :id 3 :subject "agent pending" :status 'pending
                 :owner "/root/worker/explorer")
                (mevedel-task--create
                 :id 4 :subject "proxy owner" :status 'pending
                 :owner "explorer-mevedel")
                (mevedel-task--create
                 :id 5 :subject "blocked" :status 'pending
                 :blocked-by '(2))))
    (mevedel-tool-task--set-status-note
     session "/root/worker/explorer" "Inspecting")
    (should (mevedel-tool-task-finalize-owner
             session "/root/worker/explorer" 'completed))
    (let ((tasks (mevedel-session-tasks session)))
      (should (eq 'pending (mevedel-task-status (nth 0 tasks))))
      (should (eq 'completed (mevedel-task-status (nth 1 tasks))))
      (should (= 9 (mevedel-task-completed-turn (nth 1 tasks))))
      (should (eq 'completed (mevedel-task-status (nth 2 tasks))))
      (should (= 9 (mevedel-task-completed-turn (nth 2 tasks))))
      (should (eq 'pending (mevedel-task-status (nth 3 tasks))))
      (should (null (mevedel-task-blocked-by (nth 4 tasks)))))
    (should (= 9 (mevedel-session-last-task-write-turn session)))
    (should-not (mevedel-tool-task--status-note
                 session "/root/worker/explorer")))

  :doc "non-completed terminal statuses leave owned tasks open"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "agent open" :status 'pending
                 :owner "/root/worker/explorer")))
    (should-not (mevedel-tool-task-finalize-owner
                 session "/root/worker/explorer" 'error))
    (should (eq 'pending (mevedel-task-status
                          (car (mevedel-session-tasks session))))))

  :doc "non-canonical owner labels are not auto-completed"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "proxy owner" :status 'pending
                 :owner "explorer-mevedel")
                (mevedel-task--create
                 :id 2 :subject "double dash owner" :status 'pending
                 :owner "explorer--workstream")))
    (should-not (mevedel-tool-task-finalize-owner
                 session "explorer-mevedel" 'completed))
    (should-not (mevedel-tool-task-finalize-owner
                 session "explorer--workstream" 'completed))
    (dolist (task (mevedel-session-tasks session))
      (should (eq 'pending (mevedel-task-status task))))))


;;
;;; Grouped display

(mevedel-deftest mevedel-tool-task--format-groups
  (:doc "`mevedel-tool-task--format-groups' orders, groups, and caps rows")
  ,test
  (test)
  :doc "orders open tasks globally and renders lone owners inline"
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
    (let* ((text (substring-no-properties
                  (mevedel-tool-task--format-groups session)))
           (zeta-pos (string-match "#2 zeta · z owned" text))
           (main-pos (string-match "#1 main active" text))
           (alpha-pos (string-match "#3 alpha · a owned" text)))
      ;; In progress first, then pending by id: the owner label no
      ;; longer decides where a task appears.
      (should zeta-pos)
      (should main-pos)
      (should alpha-pos)
      (should (< zeta-pos main-pos))
      (should (< main-pos alpha-pos))
      ;; One line per task; a header would only repeat the row.
      (should (= 3 (length (split-string text "\n" t))))
      (should-not (string-match-p "open · " text))
      ;; A subject may well contain the word; only a label matters.
      (let ((case-fold-search nil))
        (should-not (string-match-p "Main" text)))))

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
                    session show-completed 12))))
        (should (< (string-match "#1 open A" text)
                   (string-match "#2 open B" text)))
        (should (< (string-match "#2 open B" text)
                   (string-match "#3 open C" text))))))

  :doc "an owner with several open tasks keeps its rows under one header"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "worker first" :status 'in-progress
                 :owner "worker")
                (mevedel-task--create
                 :id 2 :subject "main between" :status 'in-progress)
                (mevedel-task--create
                 :id 3 :subject "worker second" :status 'pending
                 :owner "worker")))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      ;; The group sits at its best-ordered task and pulls the rest of
      ;; its rows up, so the main row no longer splits them.
      (should (string-match-p
               "^worker\n  → #1 worker first\n  ○ #3 worker second"
               text))
      (should (string-match-p "^→ #2 main between" text))
      (should (< (string-match "worker second" text)
                 (string-match "main between" text)))))

  :doc "completed rows share one done section keeping owner attribution"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "main open" :status 'pending)
                (mevedel-task--create
                 :id 2 :subject "main done" :status 'completed)
                (mevedel-task--create
                 :id 3 :subject "worker open" :status 'pending
                 :owner "worker")
                (mevedel-task--create
                 :id 4 :subject "worker done" :status 'completed
                 :owner "worker")))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session t))))
      (should (string-match-p "^done$" text))
      ;; No completed row hangs under an open owner's header.
      (should (< (string-match "worker open" text)
                 (string-match "^done$" text)))
      (should (string-match-p "  ✔ #2 main done" text))
      (should-not (string-match-p "Main · ✔" text))
      (should (string-match-p "  ✔ #4 worker · worker done" text))))

  :doc "default display keeps open tasks visible without a header"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "hidden body" :status 'pending)))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      (should (equal "○ #1 hidden body" text))))

  :doc "a completed-only task list renders no open rows"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "done only" :status 'completed)))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      (should (equal "No open tasks." text))
      (should-not (string-match-p "done only" text))))

  :doc "shows agent activity in place of the subject it restates"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-agent-registry session)
          '(("/root/explorer" . retained)))
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "inspect ui" :status 'in-progress
                 :owner "/root/explorer"
                 :metadata '(:activity "reading TaskListV2"))
                (mevedel-task--create
                 :id 2 :subject "verify overlay" :status 'pending
                 :owner "worker"
                 :blocked-by '(1 7))))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      (should (string-match-p
               "→ #1 explorer · reading TaskListV2" text))
      (should-not (string-match-p "inspect ui" text))
      (should (string-match-p
               "○ #2 worker · verify overlay · blocked by #1, #7" text))
      (should-not (string-match-p "@explorer" text))))

  :doc "keeps bucket subjects even when their metadata names an activity"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "bucket subject" :status 'in-progress
                 :owner "workstream"
                 :metadata '(:activeForm "bucket activity"))))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      (should (string-match-p "workstream · bucket subject" text))
      (should-not (string-match-p "bucket activity" text))))

  :doc "collapses blockers only when several of them are all running"
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "runner one" :status 'in-progress)
                (mevedel-task--create
                 :id 2 :subject "runner two" :status 'in-progress)
                (mevedel-task--create
                 :id 3 :subject "waiter" :status 'pending)
                (mevedel-task--create
                 :id 4 :subject "all running" :status 'pending
                 :blocked-by '(1 2))
                (mevedel-task--create
                 :id 5 :subject "just one" :status 'pending
                 :blocked-by '(1))
                (mevedel-task--create
                 :id 6 :subject "mixed" :status 'pending
                 :blocked-by '(1 3))))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      (should (string-match-p "all running · blocked by 2 running" text))
      ;; One blocker reads worse as a count than as the id it replaces.
      (should (string-match-p "just one · blocked by #1" text))
      (should (string-match-p "mixed · blocked by #1, #3" text))))

  :doc "bounds a very long single-line subject like the activity summary"
  (let* ((subject (make-string 300 ?x))
         (line (substring-no-properties
                (mevedel-tool-task--format-one
                 (mevedel-task--create
                  :id 9 :subject subject :status 'pending)))))
    (should (< (length line) 90))
    (should (string-match-p "…" line)))

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

  :doc "renders the main note standalone and owner notes anchored"
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
      ;; The main note opens the panel with nothing above to hang from.
      (should (string-prefix-p "Main note\n" text))
      (should (string-match-p "#2 worker · agent active\n  └ Worker note"
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
                  (mevedel-tool-task--format-groups session nil 3)))
           (lines (split-string text "\n" t)))
      (should (= 3 (length lines)))
      (should (string-match-p "Main note" text))
      (should (string-match-p "… 1 more open" text))
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
                 (mevedel-tool-task--format-groups session t 5))))
      (should (< (string-match "running active" text)
                 (string-match "plain active" text)))
      (should (< (string-match "plain active" text)
                 (string-match "blocked active" text)))
      (should (string-match-p "… 2 completed" text))
      ;; The cut landed on the done header; it carries no rows now.
      (should-not (string-match-p "^done$" text))
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
                 (mevedel-tool-task--format-groups session nil 3))))
      (should (string-match-p "plain zero" text))
      (should (string-match-p "plain one" text))
      (should (string-match-p "… 2 more open" text))
      (should-not (string-match-p "plain two" text))
      (should-not (string-match-p "plain three" text))))

  :doc "line cap takes a stable prefix and combines omitted counts"
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
                 :owner "worker")
                (mevedel-task--create
                 :id 5 :subject "main done" :status 'completed)))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session t 4))))
      ;; The running row now leads regardless of its owner.
      (should (string-match-p "worker running" text))
      (should (string-match-p "main pending one" text))
      (should (string-match-p "… 1 more open · 1 completed" text))
      (should-not (string-match-p "main pending three" text))
      (should-not (string-match-p "main done" text)))))

(mevedel-deftest mevedel-tool-task--status-line-budget
  (:doc "`mevedel-tool-task--status-line-budget' always widens when expanded")
  ,test
  (test)
  :doc "expanding never budgets fewer lines than collapsing"
  (let ((buffer (generate-new-buffer " *task-budget-test*")))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (let ((collapsed (mevedel-tool-task--status-line-budget))
                  (expanded (mevedel-tool-task--status-line-budget t)))
              (should (integerp collapsed))
              (should (integerp expanded))
              ;; A truncation reserves one line for its summary, so an
              ;; equal budget would drop an open row on expand.
              (should (> expanded collapsed)))))
      (kill-buffer buffer)))

  :doc "a window too short to widen still gains a line when expanded"
  (let ((buffer (generate-new-buffer " *task-budget-test*")))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'window-body-height)
                       (lambda (&rest _) 8)))
              (should (= (1+ (mevedel-tool-task--status-line-budget))
                         (mevedel-tool-task--status-line-budget t))))))
      (kill-buffer buffer)))

  :doc "returns nil when no window shows the buffer"
  (with-temp-buffer
    (should-not (mevedel-tool-task--status-line-budget))
    (should-not (mevedel-tool-task--status-line-budget t))))

(mevedel-deftest mevedel-tool-task--render-rows
  (:doc "`mevedel-tool-task--render-rows' caps rows without dangling chrome")
  ,test
  (test)
  :doc "reports an empty body as having no open tasks"
  (should (equal "No open tasks."
                 (substring-no-properties
                  (mevedel-tool-task--render-rows nil nil))))

  :doc "joins every row when it fits the cap"
  (should (equal "a\nb"
                 (substring-no-properties
                  (mevedel-tool-task--render-rows
                   '(("a" 1 0) ("b" 0 1)) 2))))

  :doc "drops a trailing header left with nothing under it"
  (let ((text (substring-no-properties
               (mevedel-tool-task--render-rows
                '(("row" 1 0) ("header" 0 0) ("under" 1 0)) 2))))
    (should (equal "row\n  … 1 more open" text)))

  :doc "omits the summary when the dropped rows carry no tasks"
  (let ((text (substring-no-properties
               (mevedel-tool-task--render-rows
                '(("row" 1 0) ("header" 0 0) ("note" 0 0)) 2))))
    (should (equal "row" text))))

(mevedel-deftest mevedel-tool-task-format-active-for-llm
  (:doc "`mevedel-tool-task-format-active-for-llm' uses the TaskList shape")
  ,test
  (test)
  (test-mevedel-tool-task--with-session session
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "active" :status 'pending
                 :owner "/root/worker")
                (mevedel-task--create
                 :id 2 :subject "finished" :status 'completed)))
    (should (equal "#1 [pending] active owner=/root/worker"
                   (mevedel-tool-task-format-active-for-llm session)))))


;;
;;; Tool result renderers

(mevedel-deftest mevedel-tool-task--render-mutation
  (:doc "`mevedel-tool-task--render-mutation' renders task writes as collapsed cards")
  ,test
  (test)
  :doc "renders multi-task creates with a count and id range"
  (let* ((result "Created 3 tasks:\n#1 [pending] first\n#2 [in_progress] second\n#3 [pending] third")
         (rendering
          (mevedel-tool-task--render-mutation "TaskCreate" nil result nil)))
    (should (equal "TaskCreate: 3 created · #1–#3"
                   (plist-get rendering :header)))
    (should (equal result (plist-get rendering :body)))
    (should-not (plist-get rendering :body-mode))
    (should-not (plist-get rendering :status))
    (should (plist-get rendering :initially-collapsed-p))
    (should-not (plist-member rendering :expandable-p)))

  :doc "renders single-task creates with the created task id"
  (let* ((result "Created 1 task:\n#7 [pending] only")
         (rendering
          (mevedel-tool-task--render-mutation "TaskCreate" nil result nil)))
    (should (equal "TaskCreate: 1 created · #7"
                   (plist-get rendering :header)))
    (should (equal result (plist-get rendering :body)))
    (should (plist-get rendering :initially-collapsed-p)))

  :doc "renders updates with the updated id and status"
  (let* ((result "Updated task:\n#2 [completed] Validate parsed values")
         (rendering
          (mevedel-tool-task--render-mutation "TaskUpdate" nil result nil)))
    (should (equal "TaskUpdate: #2 updated · completed"
                   (plist-get rendering :header)))
    (should (equal result (plist-get rendering :body)))
    (should (plist-get rendering :initially-collapsed-p)))

  :doc "uses the first result line and error status for errors"
  (let* ((result "Error: No task with id 99")
         (rendering
          (mevedel-tool-task--render-mutation "TaskUpdate" nil result nil)))
    (should (equal "TaskUpdate: Error: No task with id 99"
                   (plist-get rendering :header)))
    (should (eq 'error (plist-get rendering :status)))
    (should (equal result (plist-get rendering :body)))
    (should (plist-get rendering :initially-collapsed-p))))


;;
;;; View rendering

(mevedel-deftest mevedel-tool-task-refresh-display
  (:doc "`mevedel-tool-task-refresh-display' renders status-zone fragments")
  ,test
  (test)
  :doc "inserts read-only task fragment text in the live view buffer"
  (test-mevedel-tool-task--with-view session data view
    (with-current-buffer data
      (mevedel-tool-task--handle-create
       (list :tasks (vector (list :subject "rendered task")))))
    (with-current-buffer view
      (should (string-match-p "rendered task" (buffer-string)))
      (goto-char (point-min))
      (search-forward "rendered task")
      (should (get-text-property (1- (point)) 'read-only))
      (should (eq 'status (get-text-property
                           (1- (point))
                           'mevedel-view-zone-namespace)))
      (should (eq 'tasks (get-text-property
                          (1- (point)) 'mevedel-view-zone-id)))))

  :doc "re-rendering replaces the previous task fragment region"
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

  :doc "compact task fragment hides completed-only groups until expanded"
  (test-mevedel-tool-task--with-view session data view
    (with-current-buffer data
      (mevedel-tool-task--handle-create
       (list :tasks
             (vector (list :subject "main done" :status "completed")
                     (list :subject "worker active" :owner "worker")))))
    (with-current-buffer view
      (should (string-match-p "worker active" (buffer-string)))
      (should-not (string-match-p "main done" (buffer-string)))
      (goto-char (point-min))
      (search-forward "worker active")
      (mevedel-toggle-tasks)
      (should (string-match-p "^done$" (buffer-string)))
      (should (string-match-p "main done" (buffer-string)))))

  :doc "fragment-backed task render suppresses modification hooks"
  (test-mevedel-tool-task--with-view session data view
    (with-current-buffer view
      (let ((changes 0))
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (with-current-buffer data
          (mevedel-tool-task--handle-create
           (list :tasks (vector (list :subject "quiet task"))))
          (mevedel-tool-task--handle-update
           (list :id 1 :subject "still quiet")))
        (should (= 0 changes))
        (should (string-match-p "still quiet" (buffer-string))))))

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

  :doc "completed-only tasks do not render a status fragment"
  (test-mevedel-tool-task--with-view session data view
    (with-current-buffer data
      (mevedel-tool-task--handle-create
       (list :tasks (vector (list :subject "done only"
                                  :status "completed")))))
    (with-current-buffer view
      (should-not (string-match-p "tasks" (buffer-string)))
      (should-not (string-match-p "done only" (buffer-string)))))

  :doc "stale status notes alone do not render a status fragment"
  (test-mevedel-tool-task--with-view session data view
    (setf (mevedel-session-task-status-notes session)
          '((nil :note "stale note" :updated-turn 1)))
    (with-current-buffer data
      (mevedel-tool-task-refresh-display))
    (with-current-buffer view
      (should-not (string-match-p "tasks" (buffer-string)))
      (should-not (string-match-p "stale note" (buffer-string)))))

  :doc "completing the last open task removes the fragment"
  (test-mevedel-tool-task--with-view session data view
    (with-current-buffer data
      (mevedel-tool-task--handle-create
       (list :tasks (vector (list :subject "last active")))))
    (with-current-buffer view
      (should (string-match-p "last active" (buffer-string)))
      (goto-char (point-min))
      (search-forward "last active")
      (should (eq 'tasks (get-text-property
                          (1- (point)) 'mevedel-view-zone-id))))
    (with-current-buffer data
      (mevedel-tool-task--handle-update
       (list :id 1 :status "completed")))
    (with-current-buffer view
      (should-not (string-match-p "tasks" (buffer-string)))
      (should-not (string-match-p "last active" (buffer-string)))))

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
      (should (string-match-p "1 running · 1 done" (buffer-string)))
      (should (string-match-p "active body" (buffer-string)))
      (should (string-match-p "done body" (buffer-string)))
      (goto-char (point-min))
      (search-forward "done body")
      (should (eq 'mevedel-tool-task-completed
                  (get-text-property (1- (point)) 'font-lock-face)))
      (goto-char (point-min))
      (search-forward "active body")
      (mevedel-toggle-tasks)
      (should (string-match-p "active body" (buffer-string)))
      (should-not (string-match-p "done body" (buffer-string)))))

  :doc "shared fragment activation toggles completed task detail"
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
      (goto-char (match-beginning 0))
      (should (eq 'tasks (get-text-property
                          (point) 'mevedel-view-zone-id)))
      (mevedel-view-activate-at-point)
      (should (string-match-p "done body" (buffer-string)))
      (goto-char (point-min))
      (search-forward "active body")
      (goto-char (match-beginning 0))
      (mevedel-view-activate-at-point)
      (should-not (string-match-p "done body" (buffer-string))))))


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
    (let ((result (plist-get
                   (mevedel-tool-task--handle-note
                    (list :note "Finishing task status polish"))
                   :result)))
      (should (string-match-p "Status note for Main" result))
      (should (equal "Finishing task status polish"
                     (mevedel-tool-task--status-note session nil))))
    (mevedel-tool-task--handle-note (list :note ""))
    (should (null (mevedel-tool-task--status-note session nil))))

  :doc "defaults agent notes to the current agent owner"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "main active"))))
    (test-mevedel-tool-task--register-agent session "/root/explorer")
    (let ((inv (mevedel-agent-invocation--create
                :path "/root/explorer"
                :agent-id "explorer--0123456789abcdef0123456789abcdef")))
      (let ((mevedel--agent-invocation inv))
        (mevedel-tool-task--handle-create
         (list :tasks (vector (list :subject "agent active"))))
        (mevedel-tool-task--handle-note
         (list :note "Checking the agent-owned path" :owner nil))))
    (mevedel-tool-task--handle-note
     (list :note "Checking the main path"))
    (let ((text (substring-no-properties
                 (mevedel-tool-task--format-groups session))))
      (should (string-prefix-p "Checking the main path\n" text))
      (should (string-match-p
               "explorer · agent active\n  └ Checking the agent-owned path"
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
    (let ((result (plist-get
                   (mevedel-tool-task--handle-note
                    (list :note "No open task"))
                   :result)))
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
    (let ((result (plist-get (mevedel-tool-task--handle-list nil) :result)))
      (should (stringp result))
      (should (string-match-p "No tasks" result))))

  :doc "returns all tasks when no filter is given"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "one" :status "completed")
                          (list :subject "two" :status "pending"))))
    (let ((result (plist-get (mevedel-tool-task--handle-list nil) :result)))
      (should (string-match-p "one" result))
      (should (string-match-p "two" result))))

  :doc "filters by status"
  (test-mevedel-tool-task--with-session session
    (mevedel-tool-task--handle-create
     (list :tasks (vector (list :subject "done1" :status "completed")
                          (list :subject "pending1" :status "pending")
                          (list :subject "done2" :status "completed"))))
    (let ((result (plist-get
                   (mevedel-tool-task--handle-list
                    (list :status "completed"))
                   :result)))
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
    (let ((result (plist-get
                   (mevedel-tool-task--handle-get (list :id 1))
                   :result)))
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
