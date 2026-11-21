;;; test-niwa-background-list.el --- Background-work cockpit tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the agent and all-owner execution projection, kind-aware controls,
;; refresh, and slash commands of the background-work cockpit.

;;; Code:

(require 'niwa-report-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "niwa-report-test-support"))

(require 'niwa-background-list)
(require 'niwa-cockpit)
(require 'niwa-execution)
(require 'niwa-schedule)
(require 'niwa-structs)
(require 'niwa-view-agent)
(require 'tabulated-list)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun niwa-background-list-test--context (session view data)
  "Return a cockpit context for SESSION, VIEW, and DATA."
  (list :session session
        :view-buffer view
        :data-buffer data
        :origin-buffer view))

(defun niwa-background-list-test--item (artifact)
  "Return one live execution snapshot backed by ARTIFACT."
  (list :execution-id "exec-000007"
        :owner "/root/explorer"
        :command "python -i"
        :description "Python REPL"
        :state 'running
        :tty t
        :wall-time-seconds 65.2
        :output-bytes 42
        :output-lines 3
        :output-tail ">>> ready"
        :artifact-path artifact
        :sandbox-state 'bubblewrap
        :sandbox-facts
        '(:sandbox bubblewrap :filesystem workspace-write :network isolated)))

(defun niwa-background-list-test--agent ()
  "Return one active agent row as `niwa-view-agent-active-rows' builds it."
  (list :path "/root/counter" :status 'running :activity 'running
        :parent-path "/root" :depth 0
        :started-at (time-subtract (current-time) 30)
        :model-metadata '(:backend "Claude Code" :model "opus" :effort "medium")))

(defmacro niwa-background-list-test--with-cockpit (agents items &rest body)
  "Open the cockpit over AGENTS and execution ITEMS, run BODY in it."
  (declare (indent 2) (debug t))
  `(let* ((session (niwa-session--create :name "main"))
          (view (generate-new-buffer " *background-list-view*"))
          (data (generate-new-buffer " *background-list-data*")))
     (unwind-protect
         (cl-letf (((symbol-function 'niwa-execution-list-user)
                    (lambda (seen) (should (eq session seen)) ,items))
                   ((symbol-function 'niwa-view-agent-active-rows)
                    (lambda (seen) (should (eq session seen)) ,agents)))
           (with-current-buffer
               (niwa-background-list-open
                (niwa-background-list-test--context session view data))
             ,@body))
       (when-let* ((buffer (get-buffer niwa-background-list-buffer-name)))
         (kill-buffer buffer))
       (kill-buffer view)
       (kill-buffer data))))

(niwa-deftest niwa-background-list-open ()
  ,test
  (test)
  :doc "lists agents first, then all-owner executions, with live facts and details"
  (let* ((artifact (make-temp-file "niwa-background-list-artifact-"))
         (item (niwa-background-list-test--item artifact)))
    (unwind-protect
        (niwa-background-list-test--with-cockpit
            (list (niwa-background-list-test--agent)) (list item)
          (should (eq major-mode 'niwa-background-list-mode))
          (should (equal '("/root/counter" "exec-000007")
                         (mapcar #'car tabulated-list-entries)))
          (should (equal "/root/counter" (tabulated-list-get-id)))
          (should (string-match-p
                   "niwa: background work.*main.*1 agent · 1 execution.*? keys"
                   (substring-no-properties
                    (niwa-cockpit-surface-header-line))))
          (should (string-match-p
                   ">>> ready"
                   (niwa-report-test-text
                    (niwa-background-list--details
                     (cadr (niwa-background-list--collect
                            (niwa-cockpit-surface-context)))
                     nil)))))
      (delete-file artifact))))

(niwa-deftest niwa-background-list--entry ()
  ,test
  (test)
  :doc "agent rows show kind, state, path, parent, elapsed and model"
  (let ((row (cadr (niwa-background-list--entry
                    (append '(:kind agent :id "/root/counter")
                            (niwa-background-list-test--agent))
                    nil))))
    (should (equal "agent" (aref row 0)))
    (should (equal "Running" (aref row 1)))
    (should (equal "/root/counter" (aref row 2)))
    (should (equal "/root" (aref row 3)))
    (should (string-match-p "\\`[0-9]+s\\'" (substring-no-properties (aref row 4))))
    (should (equal "opus:medium" (aref row 5))))
  :doc "blocked agents say what they wait for"
  (let ((row (cadr (niwa-background-list--entry
                    '(:kind agent :id "/root/b" :path "/root/b" :status blocked
                      :activity permission-blocked)
                    nil))))
    (should (equal "Blocked" (aref row 1)))
    (should (equal "—" (substring-no-properties (aref row 4))))
    (should (equal "waiting for permission" (aref row 5))))
  :doc "execution rows prefer the description and keep facts in Details"
  (let ((row (cadr (niwa-background-list--entry
                    (append '(:id "exec-000007")
                            (niwa-background-list-test--item "/tmp/a"))
                    nil))))
    (should (equal "bash" (aref row 0)))
    (should (equal "Python REPL" (aref row 2)))
    (should (equal "/root/explorer" (aref row 3)))
    (should (equal "exec-000007 · PTY · 42 bytes · sandbox bubblewrap" (aref row 5))))
  :doc "monitor rows use events and expiry, not elapsed time"
  (let* ((item '(:id "watch-1" :execution-id "watch-1" :kind monitor :owner "/root"
                 :description "Build failures" :command "tail -F build.log"
                 :expires-at 1700000000.0 :event-count 7 :wall-time-seconds 12))
         (row (cadr (niwa-background-list--entry item nil))))
    (should (equal "monitor" (aref row 0)))
    (should (equal "Watching" (aref row 1)))
    (should (equal "—" (substring-no-properties (aref row 4))))
    (should (string-search "7 events" (aref row 5)))
    (should (string-search (format-time-string "until %H:%M" 1700000000.0) (aref row 5)))
    (should (equal row (cadr (niwa-background-list--entry
                             (plist-put (copy-sequence item) :wall-time-seconds 900)
                             nil)))))
  :doc "queued monitors say so and have no fabricated expiry"
  (let ((row (cadr (niwa-background-list--entry
                    '(:id "q" :execution-id "q" :kind monitor :state queued
                      :description "Build failures" :expires-at nil)
                    nil))))
    (should (equal "Queued" (aref row 1)))
    (should-not (string-search "until" (aref row 5))))
  :doc "Elapsed sorts by number across agents and executions"
  (niwa-background-list-test--with-cockpit
      (list (niwa-background-list-test--agent))
      (list (list :execution-id "long" :wall-time-seconds 65.2)
            (list :execution-id "short" :wall-time-seconds 9))
    (setq tabulated-list-sort-key (cons "Elapsed" nil))
    (tabulated-list-print)
    (goto-char (point-min))
    (should (equal "short" (tabulated-list-get-id)))))

(niwa-deftest niwa-background-list--details ()
  ,test
  (test)
  :doc "monitor details retain command and show state, expiry and events"
  (let ((text (niwa-report-test-text
               (niwa-background-list--details
                '(:execution-id "watch-1" :kind monitor :owner "/root"
                  :description "Build failures" :command "tail -F build.log"
                  :expires-at 1700000000.0 :event-count 7)
                nil))))
    (should (string-match-p "Status +Watching" text))
    (should (string-match-p "Description +Build failures" text))
    (should (string-search "tail -F build.log" text))
    (should (string-match-p "Events.*7" text))
    (should-not (string-search "Elapsed" text))
    (should-not (string-search "nil" text)))
  :doc "agent details show path, parent, state and model"
  (let ((text (niwa-report-test-text
               (niwa-background-list--details
                (append '(:kind agent :id "/root/counter")
                        (niwa-background-list-test--agent))
                nil))))
    (should (string-match-p "Agent +/root/counter" text))
    (should (string-match-p "Parent +/root" text))
    (should (string-match-p "State +Running" text))
    (should (string-search "Model     opus" text))))

(niwa-deftest niwa-background-list-send-input ()
  ,test
  (test)
  :doc "sends one newline-terminated line only to a selected PTY execution"
  (let ((item '(:tty t)) call)
    (cl-letf (((symbol-function 'niwa-cockpit-surface-selected)
               (lambda (&optional _) item))
              ((symbol-function 'niwa-background-list--call-control)
               (lambda (&rest args) (setq call args)))
              ((symbol-function 'read-string) (lambda (&rest _) "hello")))
      (niwa-background-list-send-input)
      (should (equal (list #'niwa-execution-write-user "hello\n") call))
      (setq item '(:tty nil) call nil)
      (should-error (niwa-background-list-send-input) :type 'user-error)
      (setq item '(:kind agent :path "/root/a"))
      (should-error (niwa-background-list-send-input) :type 'user-error)
      (should-not call))))

(niwa-deftest niwa-background-list-interrupt (:quiet t)
  ,test
  (test)
  :doc "routes executions through session-wide user authority"
  (let (call)
    (cl-letf (((symbol-function 'niwa-cockpit-surface-selected)
               (lambda (&optional _) '(:execution-id "e")))
              ((symbol-function 'niwa-background-list--call-control)
               (lambda (&rest args) (setq call args))))
      (niwa-background-list-interrupt))
    (should (equal (list #'niwa-execution-interrupt-user) call)))
  :doc "interrupts a selected agent's turn"
  (let (interrupted)
    (cl-letf (((symbol-function 'niwa-cockpit-surface-selected)
               (lambda (&optional _) '(:kind agent :path "/root/a")))
              ((symbol-function 'niwa-background-list--session)
               (lambda (&optional _) 'session))
              ((symbol-function 'niwa-cockpit-surface-refresh) #'ignore)
              ((symbol-function 'niwa-agent-control-interrupt-user)
               (lambda (session path) (setq interrupted (list session path)) t)))
      (niwa-background-list-interrupt))
    (should (equal '(session "/root/a") interrupted))))

(niwa-deftest niwa-background-list-stop (:quiet t)
  ,test
  (test)
  :doc "stops executions and interrupts agents"
  (let (call interrupted item)
    (cl-letf (((symbol-function 'niwa-cockpit-surface-selected)
               (lambda (&optional _) item))
              ((symbol-function 'niwa-background-list--call-control)
               (lambda (&rest args) (setq call args)))
              ((symbol-function 'niwa-background-list--session)
               (lambda (&optional _) 'session))
              ((symbol-function 'niwa-cockpit-surface-refresh) #'ignore)
              ((symbol-function 'niwa-agent-control-interrupt-user)
               (lambda (_session path) (setq interrupted path) nil)))
      (setq item '(:execution-id "e"))
      (niwa-background-list-stop)
      (should (equal (list #'niwa-execution-stop-user) call))
      (setq item '(:kind agent :path "/root/idle"))
      (niwa-background-list-stop)
      (should (equal "/root/idle" interrupted)))))

(niwa-deftest niwa-background-list-visit ()
  ,test
  (test)
  :doc "opens a readable artifact or agent transcript; rejects a stale path"
  (let ((artifact (make-temp-file "niwa-background-visit-"))
        opened transcript item)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-cockpit-surface-selected)
                   (lambda (&optional _) item))
                  ((symbol-function 'find-file)
                   (lambda (path) (setq opened path)))
                  ((symbol-function 'niwa-view-open-agent-transcript)
                   (lambda (path) (setq transcript path))))
          (setq item (list :artifact-path artifact))
          (niwa-background-list-visit)
          (should (equal artifact opened))
          (setq item '(:artifact-path "/missing/niwa-artifact"))
          (should-error (niwa-background-list-visit) :type 'user-error)
          (setq item '(:kind agent :path "/root/counter"))
          (niwa-background-list-visit)
          (should (equal "/root/counter" transcript)))
      (delete-file artifact))))

(niwa-deftest niwa-background-list-quit ()
  ,test
  (test)
  :doc "returns to the owning session cockpit"
  (let (label)
    (cl-letf (((symbol-function 'niwa-cockpit-quit)
               (lambda (&optional seen) (setq label seen))))
      (niwa-background-list-quit))
    (should (equal "background-work cockpit" label))))

(niwa-deftest niwa-background-list-refresh-session ()
  ,test
  (test)
  :doc "refreshes live output and removes finished rows"
  (let* ((artifact (make-temp-file "niwa-background-stale-"))
         (item (niwa-background-list-test--item artifact))
         (agents (list (niwa-background-list-test--agent)))
         (live-p t))
    (unwind-protect
        (niwa-background-list-test--with-cockpit
            (and live-p agents) (and live-p (list item))
          (setq item (plist-put item :output-tail ">>> updated"))
          (niwa-background-list-refresh-session session)
          (forward-line 1)
          (should (equal ">>> updated"
                         (plist-get (niwa-cockpit-surface-selected) :output-tail)))
          (setq live-p nil)
          (niwa-background-list-refresh-session session)
          (should-not tabulated-list-entries)
          (should-error (niwa-background-list-stop) :type 'user-error))
      (delete-file artifact))))

(niwa-deftest niwa-background-list-handle-event ()
  ,test
  (test)
  :doc "refreshes progress and yield without duplicating terminal settlement"
  (let (types)
    (cl-letf (((symbol-function 'niwa-background-list-refresh-session)
               (lambda (session) (push session types))))
      (dolist (type '(progress yield terminal))
        (niwa-background-list-handle-event
         (list :type type :session type))))
    (should (equal '(yield progress) types))))

(niwa-deftest niwa-background-list--call-control ()
  ,test
  (test)
  :doc "refreshes and reports a selected-row race rejected by the backend"
  (let* ((artifact (make-temp-file "niwa-background-race-"))
         (item (niwa-background-list-test--item artifact))
         (live-p t))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-execution-stop-user)
                   (lambda (&rest _)
                     (setq live-p nil)
                     (signal 'niwa-execution-not-found '("gone")))))
          (niwa-background-list-test--with-cockpit nil (and live-p (list item))
            (should-error (niwa-background-list-stop) :type 'user-error)
            (should-not tabulated-list-entries)))
      (delete-file artifact))))

(niwa-deftest niwa-background-list/wake-ups (:quiet t)
  ,test
  (test)
  :doc "lists pending and expired wake-ups after agents and executions; k cancels one"
  (let* ((now 1795176000)
         (niwa-schedule-clock (lambda () now))
         (wake-up (lambda (id note due status)
                    (append (list :id id :note note
                                  :timing (list :kind 'once :at due)
                                  :created-at now :due due :expires-at nil
                                  :status status)
                            (and (eq status 'expired) (list :expired-at now))))))
    (niwa-background-list-test--with-cockpit nil nil
      (setf (niwa-session-schedules session)
            (list (funcall wake-up "w-000001" "Check CI; tell the user only if it failed"
                           (+ now 1200) 'pending)
                  (funcall wake-up "w-000002" "Old check" (- now 60) 'expired)))
      (niwa-cockpit-surface-refresh)
      (should (equal '("w-000001" "w-000002") (mapcar #'car tabulated-list-entries)))
      (let ((row (cadr (car tabulated-list-entries))))
        (should (equal "wake-up" (aref row 0)))
        (should (equal "Pending" (aref row 1)))
        (should (equal "Check CI; tell the user only if it fail…" (aref row 2)))
        (should (equal "in 20m 00s" (substring-no-properties (aref row 4)))))
      (should (equal "Expired" (aref (cadr (cadr tabulated-list-entries)) 1)))
      (should (string-match-p "0 agents · 0 executions · 2 wake-ups"
                              (substring-no-properties
                               (niwa-cockpit-surface-header-line))))
      (goto-char (point-min))
      (niwa-background-list-stop)
      (should (equal '("w-000002")
                     (mapcar (lambda (entry) (plist-get entry :id))
                             (niwa-session-schedules session))))
      (should (equal '("w-000002") (mapcar #'car tabulated-list-entries))))))

(provide 'test-niwa-background-list)
;;; test-niwa-background-list.el ends here
