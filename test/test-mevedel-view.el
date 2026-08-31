;;; test-mevedel-view.el -- Tests for mevedel-view -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for chat-view coordination and shared surface behavior.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-agent-control)
(require 'mevedel-agents)
(require 'mevedel-execution)
(require 'mevedel-execution-target)
(require 'mevedel-executions-list)
(require 'mevedel-view)
(require 'mevedel-view-stream)
(require 'mevedel-menu)
(require 'mevedel-transport)
(require 'mevedel-structs)
(require 'mevedel-skills-core)
(require 'mevedel-workspace)
(require 'mevedel-plan-mode)
(require 'mevedel-session-persistence)
(require 'mevedel-session-publication)
(require 'mevedel-session-recovery)
(require 'mevedel-permission-queue)

(defvar org-mode-hook)
(declare-function gptel-menu "ext:gptel-transient" ())
(declare-function org-entry-put "org" (pom property value))


;;
;;; Activation

(mevedel-deftest mevedel-view-activate-at-point
  (:doc "mouse activation reads properties from the clicked window")
  (let ((target (generate-new-buffer " *test-click-target*"))
        opened)
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "xagent")
            (put-text-property
             2 7 'mevedel-view-agent-path "/root/test"))
          (cl-letf
              (((symbol-function 'mevedel-view--event-position)
                (lambda (&optional _event) 2))
               ((symbol-function 'mouse-set-point)
                (lambda (_event)
                  (set-buffer target)
                  (goto-char 2)))
               ((symbol-function 'mevedel-view--position-in-input-region-p)
                (lambda (_position) nil))
               ((symbol-function 'mevedel-view-open-agent-transcript-at-point)
                (lambda (&optional _event)
                  (setq opened
                        (get-text-property
                         (point) 'mevedel-view-agent-path)))))
            (with-temp-buffer
              (mevedel-view-activate-at-point 'mouse)))
          (should (equal "/root/test" opened)))
      (when (buffer-live-p target)
        (kill-buffer target)))))


;;
;;; Rendering

(mevedel-deftest mevedel-view--schedule-render
  ()
  ,test
  (test)
  :doc "coalesces stream, tool-boundary, and full requests into one refresh"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-stream-render-delay 1)
          (mevedel-view-tool-boundary-render-delay 1)
          (mevedel-view-rerender-debounce 1)
          (scheduled 0) callback args fake-timers
          (incremental-count 0)
          (full-count 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (unwind-protect
          ;; The mock must arm a real far-future timer: scheduling now
          ;; tests presence on `timer-list', so a bare placeholder value
          ;; would read as a dropped timer and defeat the coalescing.
          (cl-letf* ((real-run-at-time (symbol-function 'run-at-time))
                     ((symbol-function 'run-at-time)
                      (lambda (_delay _repeat function &rest function-args)
                        (cl-incf scheduled)
                        (setq callback function
                              args function-args)
                        (car (push (apply real-run-at-time 3600 nil
                                          function function-args)
                                   fake-timers))))
                     ((symbol-function 'mevedel-view--render-stream-update)
                      (lambda (_data-buffer) (cl-incf incremental-count)))
                     ((symbol-function 'mevedel-view--full-rerender)
                      (lambda () (cl-incf full-count))))
            (with-current-buffer data-buf
              (mevedel-view-stream-schedule))
            (with-current-buffer view-buf
              (mevedel-view--schedule-tool-boundary-render data-buf))
            (mevedel-view-rerender view-buf)
            (should (= 1 scheduled))
            (with-current-buffer view-buf
              (should (eq 'full mevedel-view--pending-render-kind)))
            (apply callback args)
            (should (= 1 full-count))
            (should (= 0 incremental-count))
            (with-current-buffer view-buf
              (should-not mevedel-view--render-timer)
              (should-not mevedel-view--pending-render-kind))
            (setq callback nil args nil)
            (with-current-buffer data-buf
              (mevedel-view-stream-schedule))
            (with-current-buffer view-buf
              (mevedel-view--schedule-tool-boundary-render data-buf))
            (should (= 2 scheduled))
            (apply callback args)
            (should (= 1 full-count))
            (should (= 1 incremental-count)))
        (mapc #'cancel-timer fake-timers))))

  :doc "defers timer flushes while a remote operation is already in flight"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-rerender-debounce 1)
          callback args fake-timers
          (scheduled 0)
          (full-count 0))
      (unwind-protect
          (cl-letf* ((real-run-at-time (symbol-function 'run-at-time))
                     ((symbol-function 'run-at-time)
                      (lambda (_delay _repeat function &rest function-args)
                        (cl-incf scheduled)
                        (setq callback function
                              args function-args)
                        (car (push (apply real-run-at-time 3600 nil
                                          function function-args)
                                   fake-timers))))
                     ((symbol-function 'mevedel-view--full-rerender)
                      (lambda () (cl-incf full-count))))
            (mevedel-view-rerender view-buf)
            ;; A real handler frame, because that is what a render timer lands
            ;; inside.  It must not test `tramp-current-connection': that stays
            ;; set for the life of the process once any remote file is touched,
            ;; which postponed every remote render forever.
            (mevedel-transport--handler-advice
             (lambda (&rest _) (apply callback args))
             'file-exists-p "/ssh:user@host:/srv/x")
            (should (= 2 scheduled))
            (should (= 0 full-count))
            (with-current-buffer view-buf
              (should (eq 'full mevedel-view--pending-render-kind)))
            (apply callback args)
            (should (= 1 full-count)))
        (mapc #'cancel-timer fake-timers))))

  :doc "re-arms when the recorded timer is no longer on `timer-list'"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-rerender-debounce 1)
          (scheduled 0)
          lost)
      ;; Arm a timer on a discarded `timer-list' binding: the shape TRAMP's
      ;; suspended-timers window leaves behind when a stream or tool hook
      ;; schedules a render inside it.
      (let (timer-list)
        (setq lost (run-at-time 3600 nil #'ignore)))
      (should (timerp lost))
      (with-current-buffer view-buf
        (setq mevedel-view--render-timer lost))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (cl-incf scheduled) 'scheduled)))
        (mevedel-view-rerender view-buf))
      (should (= 1 scheduled)))))

(mevedel-deftest mevedel-view--flush-scheduled-render
  (:doc "keeps historical projection fixed while refreshing live chrome")
  (mevedel-view-test--with-buffers
    (let ((full-count 0)
          (incremental-count 0)
          (chrome-count 0))
      (with-current-buffer view-buf
        (setq-local mevedel-view--pending-render-kind 'full
                    mevedel-view--pending-render-data-buffer data-buf))
      (cl-letf
          (((symbol-function 'mevedel-view-historical-segment-p)
            (lambda () t))
           ((symbol-function 'mevedel-view--full-rerender)
            (lambda (&rest _) (cl-incf full-count)))
           ((symbol-function 'mevedel-view--render-stream-update)
            (lambda (&rest _) (cl-incf incremental-count)))
           ((symbol-function 'mevedel-view--render-status)
            (lambda (&rest _) (cl-incf chrome-count)))
           ((symbol-function 'mevedel-view--interaction-rebuild)
            (lambda () (cl-incf chrome-count)))
           ((symbol-function 'mevedel-view--ensure-request-progress)
            (lambda (&rest _) (cl-incf chrome-count))))
        (mevedel-view--flush-scheduled-render view-buf))
      (with-current-buffer view-buf
        (should-not mevedel-view--pending-render-kind))
      (should (= 0 full-count))
      (should (= 0 incremental-count))
      (should (= 3 chrome-count)))))

(mevedel-deftest mevedel-view--status-strip-button ()
  ,test
  (test)
  :doc "status strip button routes clicks to the requested cockpit area"
  (let ((button (mevedel-view--status-strip-button
                 "Mode" 'mode "Open mode cockpit"))
        called)
    (cl-letf (((symbol-function 'mevedel-menu-open)
               (lambda (area) (setq called area))))
      (let* ((map (get-text-property 0 'local-map button))
             (command (lookup-key map [header-line mouse-1])))
        (should (eq (get-text-property 0 'mevedel-view-cockpit-area button)
                    'mode))
        (should (string= button "Mode"))
        (should command)
        (funcall command nil)
        (should (eq called 'mode))))))

(mevedel-deftest mevedel-view--status-strip ()
  ,test
  (test)
  :doc "status strip root label truncates to the workspace tail, then disappears"
  (let ((root "~/Projekte/mevedel/"))
    (should (equal root
                   (mevedel-view--status-strip-root-label root 24)))
    (should (equal "…/mevedel/"
                   (mevedel-view--status-strip-root-label root 10)))
    (should (equal ""
                   (mevedel-view--status-strip-root-label root 9))))

  :doc "status strip shows mevedel-owned session orientation instead of the data header"
  (let* ((root (make-temp-file "mevedel-status-root-" t))
         (workspace (mevedel-workspace-get-or-create
                     'project (format "status-%s" root) root "mevedel"))
         (session (mevedel-session-create "main" workspace)))
    (setf (mevedel-session-permission-mode session) 'ask)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local default-directory (file-name-as-directory root))
            (setq-local header-line-format "GPTEL HEADER")
            (setq-local mevedel--session session)
            (setq-local gptel-model 'gpt-5.5)
            (setq-local gptel-tools '(read edit)))
          (with-current-buffer view-buf
            (let ((line (mevedel-view--status-strip)))
              (should (string-prefix-p "main  " line))
              (should (string-match-p
                       (regexp-quote
                        (file-name-nondirectory
                         (directory-file-name root)))
                       line))
              (should (string-match-p
                        (regexp-quote "ask · idle · gpt-5.5 · 2 tools")
                       line))
              (should-not (string-match-p "mevedel:" line))
              (should-not (string-match-p "\\[gpt-5\\.5\\]" line))
              (should-not (string-match-p "\\[2 tools\\]" line))
              (should-not (string-match-p "GPTEL HEADER" line)))))
	  (when (file-directory-p root)
	    (delete-directory root t))))

  :doc "status strip preserves the model none label"
  (mevedel-view-test--with-buffers
    ;; The label describes an unconfigured gptel, so the data buffer must
    ;; not inherit the harness default model.
    (with-current-buffer data-buf
      (setq-local gptel-model nil))
    (with-current-buffer view-buf
      (let ((line (mevedel-view--status-strip)))
        (should (string-match-p
                 (regexp-quote "ask · idle · model none · 0 tools")
                 line)))))

  :doc "status strip identifies the session execution target"
  (mevedel-view-test--with-buffers
    (let* ((target (mevedel-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (remote-workspace
            (mevedel-workspace--create
             :type 'project :id "remote" :root "/ssh:user@host:/srv/project/"
             :name "remote"))
           (remote-session
            (mevedel-session--create
             :name "remote" :workspace remote-workspace
             :working-directory "/ssh:user@host:/srv/project/"
             :execution-target target :permission-mode 'ask)))
      (with-current-buffer data-buf
        (setq-local mevedel--session remote-session))
      (with-current-buffer view-buf
        (let ((line (mevedel-view--status-strip)))
          (should (string-match-p "ssh:user@host" line))
          (should (text-property-any
                   0 (length line) 'mevedel-view-cockpit-area 'top line))))
      (with-current-buffer data-buf
        (setq-local mevedel--session nil))))

  :doc "target, readiness, lease, and publication refreshes preserve a multiline leading-> composer draft"
  (mevedel-view-test--with-buffers
    (let* ((target (mevedel-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (workspace (mevedel-workspace--create
                       :type 'project :id "remote-status"
                       :root "/ssh:user@host:/srv/project/"
                       :name "remote-status"))
           (session (mevedel-session--create
                     :name "remote-status" :workspace workspace
                     :working-directory "/ssh:user@host:/srv/project/"
                     :execution-target target
                     :permission-mode 'ask))
           (draft "> quoted\nsecond line")
           (point-offset 4))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (mevedel-view-interaction-initialize))
            (with-current-buffer view-buf
              (mevedel-view-test--insert-composer-draft draft point-offset)
              (should (string-match-p "ssh:user@host"
                                      (mevedel-view--status-strip))))
            (setf (mevedel-session-lease session)
                  '(:state lost :unsettled-mutation nil)
                  (mevedel-session-pending-publication session)
                  '(:reason "remote write failed")
                  (mevedel-execution-target-readiness target)
                  '(:status ready
                    :sandbox-mode best-effort
                    :sandbox-status bubblewrap))
            (mevedel-session-recovery-refresh-session-buffers session)
            (with-current-buffer view-buf
              (force-mode-line-update t)
              (let ((line (mevedel-view--status-strip))
                    (header (mevedel-menu--header)))
                (should (string-match-p "ssh:user@host" line))
                (should (string-match-p "lease lost" line))
                (should (string-match-p "publication pending" line))
                ;; Nominal readiness stays out of the cockpit header; the
                ;; lost lease and pending publication earn its alert line.
                (should (string-match-p "lease lost" header))
                (should (string-match-p "publication pending" header))
                (should-not (string-match-p "sandbox bubblewrap" header))
                (should (string= draft (mevedel-view--input-text)))
                (should (= (point)
                           (+ (mevedel-view--input-start) point-offset))))))
        (setf (mevedel-session-pending-publication session) nil)
        (when (buffer-live-p data-buf)
          (with-current-buffer data-buf
            (setq-local mevedel--session nil))))))

  :doc "status strip reuses unchanged output and rebuilds after state changes"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((first (mevedel-view--status-strip))
            second changed)
        (setq second (mevedel-view--status-strip))
        (should (eq first second))
        (with-current-buffer data-buf
          (setq-local gptel-tools '(read)))
        (setq changed (mevedel-view--status-strip))
        (should-not (eq second changed))
        (should (string-match-p "1 tool" changed)))))

  :doc "status strip shows Plan together with its permission policy"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create
                    :name "main" :permission-mode 'full-auto :plan-mode t)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (should (string-match-p
                 (regexp-quote "Plan/full-auto · idle")
                 (mevedel-view--status-strip))))))

  :doc "status strip shows phase-free Goal status and turn accounting"
  (mevedel-view-test--with-buffers
    (let* ((goal (mevedel-goal--create
                  :status 'active :turns-run 5))
           (session (mevedel-session--create
                     :name "main" :goal goal :preset-name 'team)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((line (mevedel-view--status-strip)))
          (should (string-match-p "active · 5 turns" line))
          (should (string-match-p "preset team" line))
          (dolist (area '(goal preset))
            (should (text-property-any
                     0 (length line) 'mevedel-view-cockpit-area area line)))))))

  :doc "status strip shows completion and the restored session model"
  (mevedel-view-test--with-buffers
    (let* ((goal (mevedel-goal--create
                  :status 'complete :turns-run 3))
           (session (mevedel-session--create :name "main" :goal goal)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    gptel-model 'gpt-5.6-sol))
      (with-current-buffer view-buf
        (let ((line (mevedel-view--status-strip)))
          (should (string-match-p "complete · 3 turns · gpt-5.6-sol" line))))))

  :doc "status strip routes click targets to cockpit surfaces"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((line (mevedel-view--status-strip))
            called)
        (cl-letf (((symbol-function 'mevedel-menu-open)
                   (lambda (area) (setq called area))))
          (dolist (area '(top mode model tools))
            (let* ((pos (text-property-any
                         0 (length line)
                         'mevedel-view-cockpit-area area line))
                   (map (and pos (get-text-property pos 'local-map line)))
                   (command (and map
                                 (lookup-key map [header-line mouse-1]))))
              (should pos)
              (should command)
              (setq called nil)
              (funcall command nil)
              (should (eq called area))))))))

  :doc "status strip clicks do not call gptel transients directly"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((line (mevedel-view--status-strip))
             (pos (text-property-any
                   0 (length line)
                   'mevedel-view-cockpit-area 'tools line))
             (map (get-text-property pos 'local-map line))
             (command (lookup-key map [header-line mouse-1]))
             (gptel-called nil))
        (cl-letf (((symbol-function 'gptel-menu)
                   (lambda ()
                     (interactive)
                     (setq gptel-called t)))
                  ((symbol-function 'mevedel-menu-open) #'ignore))
          (funcall command nil)
          (should-not gptel-called)))))

  :doc "status strip keeps the raw data buffer header line"
  (let ((data-buf (generate-new-buffer " *status-data*"))
        (view-buf (generate-new-buffer " *status-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local header-line-format "GPTEL HEADER"))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer data-buf
            (should (equal header-line-format "GPTEL HEADER")))
          (with-current-buffer view-buf
            (should (equal header-line-format
                           '(:eval (mevedel-view--status-strip))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view--status-fragments ()
  ,test
  (test)
  :doc "status zone contains tasks, executions, and agents but no sandbox row"
  (let ((session (mevedel-session--create :name "status")))
    (cl-letf (((symbol-function 'mevedel-execution-count-user)
             (lambda (seen-session)
               (should (eq session seen-session))
               2))
            ((symbol-function 'mevedel-view-agent-status-fragment)
             (lambda ()
               '(:namespace status :id agents :priority 0
                 :body "agents\n"))))
    (let* ((fragments
            (mevedel-view--status-fragments
             (list :session session :task-body "tasks\n")))
           (sandbox (seq-find
                     (lambda (fragment)
                       (eq (plist-get fragment :id) 'sandbox))
                     fragments))
           (tasks (seq-find
                   (lambda (fragment)
                     (eq (plist-get fragment :id) 'tasks))
                     fragments))
           (executions (seq-find
                        (lambda (fragment)
                          (eq (plist-get fragment :id) 'executions))
                        fragments))
           (agents (seq-find
                    (lambda (fragment)
                     (eq (plist-get fragment :id) 'agents))
                     fragments)))
      (should-not sandbox)
      (should (> (plist-get tasks :priority)
                 (plist-get executions :priority)))
      (should (> (plist-get executions :priority)
                 (plist-get agents :priority)))
      (should (string-match-p "Executions: 2 live"
                              (plist-get executions :body)))
      ;; The row needs a blank line under it or it reads as a caption
      ;; for the agents separator that follows.  A zone trims `:body'
      ;; to one newline, so the blank has to travel as a suffix.
      (should (equal "\n" (plist-get executions :body-suffix)))))))

(mevedel-deftest mevedel-view--execution-state-changed ()
  ,test
  (test)
  :doc "live-count redraw preserves a multiline leading-> composer draft"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create :name "execution-status"))
          (draft "> quoted\nsecond line"))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4)))
      (cl-letf (((symbol-function 'mevedel-execution-count-user)
                 (lambda (seen-session)
                   (should (eq session seen-session))
                   1))
                ((symbol-function 'mevedel-view-agent-status-fragment)
                 #'ignore))
        (mevedel-view--execution-state-changed session data-buf))
      (with-current-buffer view-buf
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (should (string-match-p
                 "Executions: 1 live"
                 (buffer-substring-no-properties
                  (point-min) (mevedel-view--input-start)))))))
  :doc "routes an agent execution update to its parent session view"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create :name "agent-execution-status"))
          (agent-data (generate-new-buffer " *mevedel-agent-execution*")))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer agent-data
              (setq-local
               mevedel--agent-invocation
               (mevedel-agent-invocation--create
                :parent-data-buffer data-buf)))
            (cl-letf (((symbol-function 'mevedel-execution-count-user)
                       (lambda (seen-session)
                         (should (eq session seen-session))
                         1))
                      ((symbol-function 'mevedel-view-agent-status-fragment)
                       #'ignore))
              (mevedel-view--execution-state-changed session agent-data))
            (with-current-buffer view-buf
              (should (string-match-p
                       "Executions: 1 live"
                       (buffer-substring-no-properties
                        (point-min) (mevedel-view--input-start))))))
        (kill-buffer agent-data)))))

(mevedel-deftest mevedel-view-open-executions ()
  ,test
  (test)
  :doc "opens the live execution cockpit"
  (let (opened)
    (cl-letf (((symbol-function 'mevedel-executions-list-open)
               (lambda (&optional _context) (setq opened t))))
      (mevedel-view-open-executions))
    (should opened)))

;;
;;; View lifecycle

(mevedel-deftest mevedel-view--on-view-killed
  (:doc "view kill hook cleans up queued interactions")
  ,test
  (test)

  :doc "killing the view aborts both queues and kills the data buffer"
  (let ((data-buf (generate-new-buffer " *test-data-kill-view*"))
        (view-buf (generate-new-buffer " *test-view-kill-view*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/kill-view/"
                   :root "/tmp/kill-view/" :name "kill-view")))
        (outcomes nil))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-permission-queue session)
                (list (list :kind 'generic
                            :tool-name "Read"
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'permission outcome) outcomes)))))
          (setf (mevedel-session-pending-plan-approval session)
                (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'plan outcome) outcomes))))
          (kill-buffer view-buf)
          (should-not (buffer-live-p view-buf))
          (should-not (buffer-live-p data-buf))
          (should (null (mevedel-session-permission-queue session)))
          (should (null (mevedel-session-pending-plan-approval session)))
          (should (equal '((plan . aborted) (permission . aborted))
                         outcomes)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "killing an agent view detaches its observer from retained data"
  (let ((data-buf (generate-new-buffer " *test-agent-data-kill-view*"))
        (view-buf (generate-new-buffer " *test-agent-view-kill-view*"))
        (parent-view (generate-new-buffer " *test-parent-kill-view*"))
        (change-hook #'ignore))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--view-buffer parent-view))
          (mevedel-view--setup
           view-buf data-buf
           (list :agent-transcript-p t
                 :preserve-data-view-buffer t
                 :parent-view parent-view
                 :transcript-info '(:live-buffer t)))
          (with-current-buffer view-buf
            (setq-local mevedel-view--live-source-change-hook change-hook)
            (setq-local mevedel-view--live-data-tail-start
                        (with-current-buffer data-buf
                          (copy-marker (point-min))))
            (setq-local mevedel-view--live-view-tail-start
                        (copy-marker (point-min))))
          (with-current-buffer data-buf
            (add-hook 'before-change-functions change-hook nil t))
          (kill-buffer view-buf)
          (should (buffer-live-p data-buf))
          (with-current-buffer data-buf
            (should-not (memq change-hook before-change-functions))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (when (buffer-live-p parent-view) (kill-buffer parent-view)))))

(mevedel-deftest mevedel-view--on-data-killed
  (:doc "data kill hook cleans up queued interactions")
  ,test
  (test)

  :doc "data teardown stops all executions in the session"
  (let ((session (mevedel-session--create :name "kill"))
        (agent-buffer (generate-new-buffer " *test-data-kill-agent*"))
        stopped)
    (setf (mevedel-session-agent-registry session)
          (list
           (cons "/root/worker"
                 (mevedel-agent-record--create
                  :path "/root/worker"
                  :conversation-buffer agent-buffer))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq-local mevedel--session session)
            (cl-letf
                (((symbol-function 'mevedel-view--abort-data-buffer) #'ignore)
                 ((symbol-function 'mevedel-execution-teardown-session)
                  (lambda (owner-session)
                    (setq stopped owner-session))))
              (mevedel-view--on-data-killed)))
          (should (eq session stopped))
          (should-not (buffer-live-p agent-buffer)))
      (when (buffer-live-p agent-buffer)
        (kill-buffer agent-buffer))))

  :doc "killing the data buffer aborts queued and direct interactions"
  (let ((data-buf (generate-new-buffer " *test-data-kill-data*"))
        (view-buf (generate-new-buffer " *test-view-kill-data*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/kill-data/"
                   :root "/tmp/kill-data/" :name "kill-data")))
        (outcomes nil))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (require 'mevedel-interaction-prompt)
          (with-current-buffer data-buf
            (mevedel--prompt-user-with-overlay
             "Confirm" "Direct request" "Proceed?" nil
             (lambda (outcome)
               (push (cons 'direct outcome) outcomes))))
          (setf (mevedel-session-permission-queue session)
                (list (list :kind 'generic
                            :tool-name "Read"
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'permission outcome) outcomes)))))
          (setf (mevedel-session-pending-plan-approval session)
                (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'plan outcome) outcomes))))
          (kill-buffer data-buf)
          (should-not (buffer-live-p data-buf))
          (should-not (buffer-live-p view-buf))
          (should (null (mevedel-session-permission-queue session)))
          (should (null (mevedel-session-pending-plan-approval session)))
          (should (equal '((direct . aborted)
                           (plan . aborted)
                           (permission . aborted))
                         outcomes)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view--allow-session-close-p (:quiet t)
  ,test
  (test)
  :doc "pending publication blocks closing the data buffer"
  (let ((data-buf (generate-new-buffer " *test-pending-data*"))
        (view-buf (generate-new-buffer " *test-pending-data-view*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/pending-data/"
                   :root "/tmp/pending-data/" :name "pending-data"))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-pending-publication session)
                '(:reason "target unavailable"))
          (should-not (kill-buffer data-buf))
          (should (buffer-live-p data-buf))
          (should (buffer-live-p view-buf))
          (setf (mevedel-session-pending-publication session) nil)
          (should (kill-buffer data-buf))
          (should-not (buffer-live-p view-buf)))
      (setf (mevedel-session-pending-publication session) nil)
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "pending publication blocks closing the paired view"
  (let ((data-buf (generate-new-buffer " *test-pending-view-data*"))
        (view-buf (generate-new-buffer " *test-pending-view*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/pending-view/"
                   :root "/tmp/pending-view/" :name "pending-view"))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-pending-publication session)
                '(:reason "target unavailable"))
          (should-not (kill-buffer view-buf))
          (should (buffer-live-p view-buf))
          (should (buffer-live-p data-buf))
          (setf (mevedel-session-pending-publication session) nil)
          (should (kill-buffer view-buf))
          (should-not (buffer-live-p data-buf)))
      (setf (mevedel-session-pending-publication session) nil)
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "an unsettled remote mutation blocks close until acknowledgement"
  (let ((data-buf (generate-new-buffer " *test-unsettled-data*"))
        (view-buf (generate-new-buffer " *test-unsettled-view*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/unsettled/"
                   :root "/tmp/unsettled/"
                   :name "unsettled"))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-lease session)
                '(:state owned :unsettled-mutation t))
          (cl-letf (((symbol-function
                      'mevedel-execution-unsettled-mutation-p)
                     (lambda (_session) t)))
            (should-not (kill-buffer data-buf)))
          (should (buffer-live-p data-buf))
          (should (buffer-live-p view-buf))
          (setf (mevedel-session-lease session)
                '(:state owned :unsettled-mutation nil))
          (should (kill-buffer data-buf))
          (should-not (buffer-live-p view-buf)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "a foreign read-only inspector can close with a durable mutation latch"
  (let* ((root "/tmp/foreign-close/")
         (data-buf (generate-new-buffer " *test-foreign-close-data*"))
         (view-buf (generate-new-buffer " *test-foreign-close-view*"))
         (session
          (mevedel-session-create
           "main"
           (mevedel-workspace--create
            :type 'project :id root :root root :name "foreign-close"))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-lease session)
                '(:state foreign :unsettled-mutation t))
          (with-current-buffer data-buf
            (setq-local mevedel-session--read-only-mode t)
            (setq buffer-read-only t))
          (cl-letf (((symbol-function
                      'mevedel-execution-unsettled-mutation-p)
                     (lambda (_session) t)))
            (should (kill-buffer data-buf)))
          (should-not (buffer-live-p view-buf))
          (should
           (plist-get (mevedel-session-lease session)
                      :unsettled-mutation)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf)
        (with-current-buffer data-buf
          (setq buffer-read-only nil))
        (kill-buffer data-buf)))))

;;
;;; View command wiring

(mevedel-deftest mevedel-view-mode-map ()
  ,test
  (test)
  :doc "view mode binds the cockpit command"
  (should (eq (lookup-key mevedel-view-mode-map (kbd "C-c C-o"))
              #'mevedel-menu))
  (should-not (eq (lookup-key mevedel-view-mode-map (kbd "C-c C-m"))
                  #'mevedel-menu)))



(provide 'test-mevedel-view)

;;; test-mevedel-view.el ends here
