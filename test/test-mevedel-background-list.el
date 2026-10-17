;;; test-mevedel-background-list.el --- Background-work cockpit tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the agent and all-owner execution projection, kind-aware controls,
;; refresh, and slash commands of the background-work cockpit.

;;; Code:

(require 'mevedel-report-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "mevedel-report-test-support"))

(require 'mevedel-background-list)
(require 'mevedel-cockpit)
(require 'mevedel-execution)
(require 'mevedel-structs)
(require 'tabulated-list)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-background-list-test--context (session view data)
  "Return a cockpit context for SESSION, VIEW, and DATA."
  (list :session session
        :view-buffer view
        :data-buffer data
        :origin-buffer view))

(defun mevedel-background-list-test--item (artifact)
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

(defun mevedel-background-list-test--agent ()
  "Return one active agent row as `mevedel-view-agent-active-rows' builds it."
  (list :path "/root/counter" :status 'running :activity 'running
        :parent-path "/root" :depth 0
        :started-at (time-subtract (current-time) 30)
        :model-metadata '(:backend "Claude Code" :model "opus" :effort "medium")))

(defmacro mevedel-background-list-test--with-cockpit (agents items &rest body)
  "Open the cockpit over AGENTS and execution ITEMS, run BODY in it."
  (declare (indent 2) (debug t))
  `(let* ((session (mevedel-session--create :name "main"))
          (view (generate-new-buffer " *background-list-view*"))
          (data (generate-new-buffer " *background-list-data*")))
     (unwind-protect
         (cl-letf (((symbol-function 'mevedel-execution-list-user)
                    (lambda (seen) (should (eq session seen)) ,items))
                   ((symbol-function 'mevedel-view-agent-active-rows)
                    (lambda (seen) (should (eq session seen)) ,agents)))
           (with-current-buffer
               (mevedel-background-list-open
                (mevedel-background-list-test--context session view data))
             ,@body))
       (when-let* ((buffer (get-buffer mevedel-background-list-buffer-name)))
         (kill-buffer buffer))
       (kill-buffer view)
       (kill-buffer data))))

(mevedel-deftest mevedel-background-list-open ()
  ,test
  (test)
  :doc "lists agents first, then all-owner executions, with live facts and details"
  (let* ((artifact (make-temp-file "mevedel-background-list-artifact-"))
         (item (mevedel-background-list-test--item artifact)))
    (unwind-protect
        (mevedel-background-list-test--with-cockpit
            (list (mevedel-background-list-test--agent)) (list item)
          (should (eq major-mode 'mevedel-background-list-mode))
          (should (equal '("/root/counter" "exec-000007")
                         (mapcar #'car tabulated-list-entries)))
          (should (equal "/root/counter" (tabulated-list-get-id)))
          (should (string-match-p
                   "mevedel: background work.*main.*1 agent · 1 execution.*? keys"
                   (substring-no-properties
                    (mevedel-cockpit-surface-header-line))))
          (should (string-match-p
                   ">>> ready"
                   (mevedel-report-test-text
                    (mevedel-background-list--details
                     (cadr (mevedel-background-list--collect
                            (mevedel-cockpit-surface-context)))
                     nil)))))
      (delete-file artifact))))

(mevedel-deftest mevedel-background-list--entry ()
  ,test
  (test)
  :doc "agent rows show kind, state, path, parent, elapsed and model"
  (let ((row (cadr (mevedel-background-list--entry
                    (append '(:kind agent :id "/root/counter")
                            (mevedel-background-list-test--agent))
                    nil))))
    (should (equal "agent" (aref row 0)))
    (should (equal "Running" (aref row 1)))
    (should (equal "/root/counter" (aref row 2)))
    (should (equal "/root" (aref row 3)))
    (should (string-match-p "\\`[0-9]+s\\'" (substring-no-properties (aref row 4))))
    (should (equal "opus:medium" (aref row 5))))
  :doc "blocked agents say what they wait for"
  (let ((row (cadr (mevedel-background-list--entry
                    '(:kind agent :id "/root/b" :path "/root/b" :status blocked
                      :activity permission-blocked)
                    nil))))
    (should (equal "Blocked" (aref row 1)))
    (should (equal "—" (substring-no-properties (aref row 4))))
    (should (equal "waiting for permission" (aref row 5))))
  :doc "execution rows prefer the description and keep facts in Details"
  (let ((row (cadr (mevedel-background-list--entry
                    (append '(:id "exec-000007")
                            (mevedel-background-list-test--item "/tmp/a"))
                    nil))))
    (should (equal "bash" (aref row 0)))
    (should (equal "Python REPL" (aref row 2)))
    (should (equal "/root/explorer" (aref row 3)))
    (should (equal "exec-000007 · PTY · 42 bytes · sandbox bubblewrap" (aref row 5))))
  :doc "monitor rows use events and expiry, not elapsed time"
  (let* ((item '(:id "watch-1" :execution-id "watch-1" :kind monitor :owner "/root"
                 :description "Build failures" :command "tail -F build.log"
                 :expires-at 1700000000.0 :event-count 7 :wall-time-seconds 12))
         (row (cadr (mevedel-background-list--entry item nil))))
    (should (equal "monitor" (aref row 0)))
    (should (equal "Watching" (aref row 1)))
    (should (equal "—" (substring-no-properties (aref row 4))))
    (should (string-search "7 events" (aref row 5)))
    (should (string-search (format-time-string "until %H:%M" 1700000000.0) (aref row 5)))
    (should (equal row (cadr (mevedel-background-list--entry
                             (plist-put (copy-sequence item) :wall-time-seconds 900)
                             nil)))))
  :doc "queued monitors say so and have no fabricated expiry"
  (let ((row (cadr (mevedel-background-list--entry
                    '(:id "q" :execution-id "q" :kind monitor :state queued
                      :description "Build failures" :expires-at nil)
                    nil))))
    (should (equal "Queued" (aref row 1)))
    (should-not (string-search "until" (aref row 5))))
  :doc "Elapsed sorts by number across agents and executions"
  (mevedel-background-list-test--with-cockpit
      (list (mevedel-background-list-test--agent))
      (list (list :execution-id "long" :wall-time-seconds 65.2)
            (list :execution-id "short" :wall-time-seconds 9))
    (setq tabulated-list-sort-key (cons "Elapsed" nil))
    (tabulated-list-print)
    (goto-char (point-min))
    (should (equal "short" (tabulated-list-get-id)))))

(mevedel-deftest mevedel-background-list--details ()
  ,test
  (test)
  :doc "monitor details retain command and show state, expiry and events"
  (let ((text (mevedel-report-test-text
               (mevedel-background-list--details
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
  (let ((text (mevedel-report-test-text
               (mevedel-background-list--details
                (append '(:kind agent :id "/root/counter")
                        (mevedel-background-list-test--agent))
                nil))))
    (should (string-match-p "Agent +/root/counter" text))
    (should (string-match-p "Parent +/root" text))
    (should (string-match-p "State +Running" text))
    (should (string-search "Model     opus" text))))

(mevedel-deftest mevedel-background-list-send-input ()
  ,test
  (test)
  :doc "sends one newline-terminated line only to a selected PTY execution"
  (let ((item '(:tty t)) call)
    (cl-letf (((symbol-function 'mevedel-cockpit-surface-selected)
               (lambda (&optional _) item))
              ((symbol-function 'mevedel-background-list--call-control)
               (lambda (&rest args) (setq call args)))
              ((symbol-function 'read-string) (lambda (&rest _) "hello")))
      (mevedel-background-list-send-input)
      (should (equal (list #'mevedel-execution-write-user "hello\n") call))
      (setq item '(:tty nil) call nil)
      (should-error (mevedel-background-list-send-input) :type 'user-error)
      (setq item '(:kind agent :path "/root/a"))
      (should-error (mevedel-background-list-send-input) :type 'user-error)
      (should-not call))))

(mevedel-deftest mevedel-background-list-interrupt (:quiet t)
  ,test
  (test)
  :doc "routes executions through session-wide user authority"
  (let (call)
    (cl-letf (((symbol-function 'mevedel-cockpit-surface-selected)
               (lambda (&optional _) '(:execution-id "e")))
              ((symbol-function 'mevedel-background-list--call-control)
               (lambda (&rest args) (setq call args))))
      (mevedel-background-list-interrupt))
    (should (equal (list #'mevedel-execution-interrupt-user) call)))
  :doc "interrupts a selected agent's turn"
  (let (interrupted)
    (cl-letf (((symbol-function 'mevedel-cockpit-surface-selected)
               (lambda (&optional _) '(:kind agent :path "/root/a")))
              ((symbol-function 'mevedel-background-list--session)
               (lambda (&optional _) 'session))
              ((symbol-function 'mevedel-cockpit-surface-refresh) #'ignore)
              ((symbol-function 'mevedel-agent-control-interrupt-user)
               (lambda (session path) (setq interrupted (list session path)) t)))
      (mevedel-background-list-interrupt))
    (should (equal '(session "/root/a") interrupted))))

(mevedel-deftest mevedel-background-list-stop (:quiet t)
  ,test
  (test)
  :doc "stops executions and interrupts agents"
  (let (call interrupted item)
    (cl-letf (((symbol-function 'mevedel-cockpit-surface-selected)
               (lambda (&optional _) item))
              ((symbol-function 'mevedel-background-list--call-control)
               (lambda (&rest args) (setq call args)))
              ((symbol-function 'mevedel-background-list--session)
               (lambda (&optional _) 'session))
              ((symbol-function 'mevedel-cockpit-surface-refresh) #'ignore)
              ((symbol-function 'mevedel-agent-control-interrupt-user)
               (lambda (_session path) (setq interrupted path) nil)))
      (setq item '(:execution-id "e"))
      (mevedel-background-list-stop)
      (should (equal (list #'mevedel-execution-stop-user) call))
      (setq item '(:kind agent :path "/root/idle"))
      (mevedel-background-list-stop)
      (should (equal "/root/idle" interrupted)))))

(mevedel-deftest mevedel-background-list-visit ()
  ,test
  (test)
  :doc "opens a readable artifact or agent transcript; rejects a stale path"
  (let ((artifact (make-temp-file "mevedel-background-visit-"))
        opened transcript item)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-cockpit-surface-selected)
                   (lambda (&optional _) item))
                  ((symbol-function 'find-file)
                   (lambda (path) (setq opened path)))
                  ((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (path) (setq transcript path))))
          (setq item (list :artifact-path artifact))
          (mevedel-background-list-visit)
          (should (equal artifact opened))
          (setq item '(:artifact-path "/missing/mevedel-artifact"))
          (should-error (mevedel-background-list-visit) :type 'user-error)
          (setq item '(:kind agent :path "/root/counter"))
          (mevedel-background-list-visit)
          (should (equal "/root/counter" transcript)))
      (delete-file artifact))))

(mevedel-deftest mevedel-background-list-quit ()
  ,test
  (test)
  :doc "returns to the owning session cockpit"
  (let (label)
    (cl-letf (((symbol-function 'mevedel-cockpit-quit)
               (lambda (&optional seen) (setq label seen))))
      (mevedel-background-list-quit))
    (should (equal "background-work cockpit" label))))

(mevedel-deftest mevedel-background-list-refresh-session ()
  ,test
  (test)
  :doc "refreshes live output and removes finished rows"
  (let* ((artifact (make-temp-file "mevedel-background-stale-"))
         (item (mevedel-background-list-test--item artifact))
         (agents (list (mevedel-background-list-test--agent)))
         (live-p t))
    (unwind-protect
        (mevedel-background-list-test--with-cockpit
            (and live-p agents) (and live-p (list item))
          (setq item (plist-put item :output-tail ">>> updated"))
          (mevedel-background-list-refresh-session session)
          (forward-line 1)
          (should (equal ">>> updated"
                         (plist-get (mevedel-cockpit-surface-selected) :output-tail)))
          (setq live-p nil)
          (mevedel-background-list-refresh-session session)
          (should-not tabulated-list-entries)
          (should-error (mevedel-background-list-stop) :type 'user-error))
      (delete-file artifact))))

(mevedel-deftest mevedel-background-list-handle-event ()
  ,test
  (test)
  :doc "refreshes progress and yield without duplicating terminal settlement"
  (let (types)
    (cl-letf (((symbol-function 'mevedel-background-list-refresh-session)
               (lambda (session) (push session types))))
      (dolist (type '(progress yield terminal))
        (mevedel-background-list-handle-event
         (list :type type :session type))))
    (should (equal '(yield progress) types))))

(mevedel-deftest mevedel-background-list--call-control ()
  ,test
  (test)
  :doc "refreshes and reports a selected-row race rejected by the backend"
  (let* ((artifact (make-temp-file "mevedel-background-race-"))
         (item (mevedel-background-list-test--item artifact))
         (live-p t))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-execution-stop-user)
                   (lambda (&rest _)
                     (setq live-p nil)
                     (signal 'mevedel-execution-not-found '("gone")))))
          (mevedel-background-list-test--with-cockpit nil (and live-p (list item))
            (should-error (mevedel-background-list-stop) :type 'user-error)
            (should-not tabulated-list-entries)))
      (delete-file artifact))))

(provide 'test-mevedel-background-list)
;;; test-mevedel-background-list.el ends here
