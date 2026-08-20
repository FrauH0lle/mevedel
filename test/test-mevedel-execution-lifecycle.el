;;; test-mevedel-execution-lifecycle.el --- Execution ownership boundaries -*- lexical-binding: t -*-

;;; Commentary:

;; Tests process lifetime at owner, session, history, and package boundaries.

;;; Code:

(require 'cl-lib)
(require 'mevedel)
(require 'mevedel-agents)
(require 'mevedel-execution)
(require 'mevedel-execution-target)
(require 'mevedel-pipeline)
(require 'mevedel-session-durability)
(require 'mevedel-session-publication)
(require 'mevedel-session-persistence)
(require 'mevedel-transcript-audit)
(require 'mevedel-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-execution-test-helpers"))

;; `tramp'
(declare-function tramp-cleanup-all-connections "tramp-cmds" ())


;;
;;; Ownership and teardown

(mevedel-deftest mevedel-execution--discard-record ()
  ,test
  (test)
  :doc "releases runtime state before invoking teardown callbacks"
  (let* ((session (mevedel-session--create :name "discard-order"))
         teardown-after-cleanup-p
         (record
          (mevedel-execution--record-create
           :finished-p t
           :origin (mevedel-execution--origin-create :session session)
           :teardown-function
           (lambda ()
             (setq teardown-after-cleanup-p
                   (not
                    (gethash
                     'record
                     (mevedel-execution--state-records
                      (mevedel-session-execution-state session))))))
           :token 'record)))
    (puthash
     'record record
     (mevedel-execution--state-records
      (mevedel-execution--state-for-session session)))
    (mevedel-execution--discard-record record 'session-ended)
    (should teardown-after-cleanup-p)))

(mevedel-deftest mevedel-execution-stop-owner ()
  ,test
  (test)
  :doc "stops only the selected owner within platform process limits"
  (let* ((root (make-temp-file "mevedel-owner-lifetime-" t))
         (pid-file (file-name-concat root "child.pid"))
         (scratch-file (file-name-concat root "scratch.path"))
         (tombstone-file (file-name-concat root "tombstone.log"))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         owned sibling pid owned-id owned-process
         helper-result scratch tombstone)
    (unwind-protect
        (progn
          (setq owned
                (test-mevedel-execution--start-managed
                 session root
                 (if (eq system-type 'windows-nt)
                     '("sleep" "30")
                   (list
                    "sh" "-c"
                    "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; wait"
                    "owner" pid-file))
                 :owner "agent-a"))
          (setq sibling
                (test-mevedel-execution--start-managed
                 session root '("sleep" "30")
                 :owner "agent-b"))
          (mevedel-execution-start-helper
           (lambda (result) (setq helper-result result))
           "mevedel-owner-helper"
           (list "sh" "-c"
                 "printf '%s' \"$PWD\" > \"$1\"; exec sleep 30"
                 "helper" scratch-file)
           nil (list root) :session session :owner "agent-a")
          (test-mevedel-execution--wait
           (lambda () (file-readable-p scratch-file)))
          (setq scratch
                (string-trim
                 (with-temp-buffer
                   (insert-file-contents scratch-file)
                   (buffer-string))))
          (write-region "terminal" nil tombstone-file nil 'silent)
          (setq tombstone
                (mevedel-execution--record-create
                 :finished-p t
                 :origin (mevedel-execution--origin-create
                          :owner "agent-a" :session session)
                 :spool-path tombstone-file
                 :token 'owner-tombstone))
          (puthash 'owner-tombstone tombstone
                   (mevedel-execution--state-records
                    (mevedel-session-execution-state session)))
          (setq owned-id (plist-get (plist-get owned :facts) :execution-id))
          (when (eq system-type 'windows-nt)
            (setq owned-process
                  (mevedel-execution--record-process
                   (gethash
                    owned-id
                    (mevedel-execution--state-records
                     (mevedel-session-execution-state session)))))
            (should (process-live-p owned-process)))
          (should (= 3 (mevedel-execution-stop-owner session "agent-a")))
          (if (eq system-type 'windows-nt)
            (test-mevedel-execution--wait
             (lambda () (not (process-live-p owned-process))))
            (setq pid (test-mevedel-execution--read-pid pid-file))
            (test-mevedel-execution--wait
             (lambda () (test-mevedel-execution--process-gone-p pid))))
          (test-mevedel-execution--wait
           (lambda ()
             (not
              (gethash
               owned-id
               (mevedel-execution--state-records
                (mevedel-session-execution-state session))))))
          (should-not (mevedel-execution-list session "agent-a"))
          (should-not (mevedel-execution-owner-live-p session "agent-a"))
          (should (mevedel-execution-owner-live-p session "agent-b"))
          (should-not helper-result)
          (should-not (file-exists-p scratch))
          (should-not (file-exists-p tombstone-file))
          (should (plist-get (plist-get sibling :facts) :execution-id))
          (should owned-id))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))
  :doc "proves delayed remote KILL settlement before clearing mutation state"
  (let* ((root (make-temp-file "mevedel-remote-owner-lifetime-" t))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (original-signal-process (symbol-function 'signal-process))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         session execution-id kill-delivered-p kill-timer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (setq session (test-mevedel-execution--session remote-root))
          (cl-letf
              (((symbol-function 'signal-process)
                (lambda (&rest args)
                  (if (and (integerp (car args))
                           (< (car args) 0)
                           (eq (cadr args) 'KILL)
                           (equal remote-root (nth 2 args)))
                      (progn
                        (setq kill-timer
                              (run-at-time
                               0.02 nil
                               (lambda ()
                                 (unwind-protect
                                     (apply original-signal-process args)
                                   (setq kill-delivered-p t)))))
                        0)
                    (apply original-signal-process args)))))
            (setq execution-id
                  (plist-get
                   (plist-get
                    (test-mevedel-execution--start-managed
                     session remote-root '("sh" "-c" "sleep 30")
                     :owner "agent-a")
                    :facts)
                   :execution-id))
            (should
             (mevedel-session-durability-unsettled-mutation-p session))
            (should (= 1 (mevedel-execution-stop-owner session "agent-a")))
            (should-not
             (mevedel-execution-owner-live-p session "agent-a"))
            (test-mevedel-execution--wait
             (lambda ()
               (and kill-delivered-p
                    (not
                     (gethash
                      execution-id
                      (mevedel-execution--state-records
                       (mevedel-session-execution-state session)))))))
            (should-not
             (mevedel-session-durability-unsettled-mutation-p session))
            (should-not (mevedel-execution-unknown-outcome session))))
      (when (timerp kill-timer)
        (cancel-timer kill-timer))
      (when session
        (mevedel-execution-teardown-session session)
        (when (mevedel-session-save-path session)
          (mevedel-session-durability-lease-release
           (mevedel-session-save-path session) session)))
      ;; This teardown performs target I/O, so it reopens the connection that
      ;; `mevedel-test--with-local-shell-tramp' already closed.  Close it after
      ;; the last target work instead of leaving it for later tests.
      (tramp-cleanup-all-connections)
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-teardown-session ()
  ,test
  (test)
  :doc "immediate teardown obeys platform limits and empties queued work"
  (let* ((root (make-temp-file "mevedel-session-lifetime-" t))
         (pid-file (file-name-concat root "child.pid"))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         first-result second-result pid first-process)
    (unwind-protect
        (progn
          (mevedel-execution-start-bash
           (lambda (value) (setq first-result value))
           :session session :owner "main" :owner-context session
           :command
           (if (eq system-type 'windows-nt)
               '("sleep" "30")
             (list
              "sh" "-c"
              "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; wait"
              "first" pid-file))
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (unless (eq system-type 'windows-nt)
            (test-mevedel-execution--wait
             (lambda () (file-readable-p pid-file))))
          (when (eq system-type 'windows-nt)
            (let* ((execution
                    (car (mevedel-execution-list-user session)))
                   (record
                    (gethash
                     (plist-get execution :execution-id)
                     (mevedel-execution--state-records
                      (mevedel-session-execution-state session)))))
              (setq first-process
                    (mevedel-execution--record-process record))
              (should (process-live-p first-process))))
          (mevedel-execution-start-bash
           (lambda (value) (setq second-result value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "printf queued")
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (should (= 2 (mevedel-execution-teardown-session session)))
          (should-not first-result)
          (should-not second-result)
          (should-not (mevedel-execution-session-live-p session))
          (should (= 0 (hash-table-count
                        (mevedel-execution--state-records
                         (mevedel-session-execution-state session)))))
          (if (eq system-type 'windows-nt)
            (test-mevedel-execution--wait
             (lambda () (not (process-live-p first-process))))
            (setq pid (test-mevedel-execution--read-pid pid-file))
            (test-mevedel-execution--wait
             (lambda () (test-mevedel-execution--process-gone-p pid)))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-session-live-p ()
  ,test
  (test)
  :doc "distinguishes active processes from terminal delivery state"
  (let* ((session (mevedel-session--create :name "liveness"))
         (state (mevedel-execution--state-for-session session))
         (record
          (mevedel-execution--record-create
           :execution-id "exec-live"
           :origin (mevedel-execution--origin-create :session session))))
    (puthash "exec-live" record (mevedel-execution--state-records state))
    (should (mevedel-execution-session-live-p session))
    (setf (mevedel-execution--record-finished-p record) t)
    (should-not (mevedel-execution-session-live-p session))
    (remhash "exec-live" (mevedel-execution--state-records state))))

(mevedel-deftest mevedel-execution-teardown-all ()
  ,test
  (test)
  :doc "drains session and orphan records, including finished tombstones"
  (let* ((mevedel-execution--sessions
          (make-hash-table :test #'eq :weakness 'key))
         (mevedel-execution--orphan-state nil)
         (session (mevedel-session--create :name "global-teardown"))
         (session-state (mevedel-execution--state-for-session session))
         (orphan-state (mevedel-execution--state-for-session nil))
         (session-spool (make-temp-file "mevedel-session-tombstone-"))
         (orphan-spool (make-temp-file "mevedel-orphan-tombstone-"))
         (cleaned 0)
         (session-record
          (mevedel-execution--record-create
           :finished-p t
           :origin (mevedel-execution--origin-create :session session)
           :spool-path session-spool
           :teardown-function (lambda () (cl-incf cleaned))
           :token 'session-record))
         (orphan-record
          (mevedel-execution--record-create
           :origin (mevedel-execution--origin-create :session nil)
           :spool-path orphan-spool
           :teardown-function (lambda () (cl-incf cleaned))
           :token 'orphan-record)))
    (puthash 'session-record session-record
             (mevedel-execution--state-records session-state))
    (puthash 'orphan-record orphan-record
             (mevedel-execution--state-records orphan-state))
    (should (= 2 (mevedel-execution-teardown-all)))
    (should (= 2 cleaned))
    (should (= 0 (hash-table-count
                  (mevedel-execution--state-records session-state))))
    (should (= 0 (hash-table-count
                  (mevedel-execution--state-records orphan-state))))
    (should-not (file-exists-p session-spool))
    (should-not (file-exists-p orphan-spool))))

(mevedel-deftest mevedel-execution-relocate-artifacts ()
  ,test
  (test)
  :doc "retargets retained artifacts without changing execution ownership"
  (let* ((old-root (file-name-as-directory
                    (make-temp-file "mevedel-artifact-old-" t)))
         (new-root (file-name-as-directory
                    (make-temp-file "mevedel-artifact-new-" t)))
         (session (test-mevedel-execution--session old-root))
         (state (mevedel-execution--state-for-session session))
         (old-path (file-name-concat old-root "tool-results/execution.log"))
         (record
          (mevedel-execution--record-create
           :execution-id "exec-000001"
           :origin (mevedel-execution--origin-create
                    :owner "agent-a" :session session)
           :spool-path old-path)))
    (unwind-protect
        (progn
          (puthash "exec-000001" record
                   (mevedel-execution--state-records state))
          (should (= 1 (mevedel-execution-relocate-artifacts
                        session old-root new-root)))
          (should (equal "agent-a"
                         (mevedel-execution--origin-owner
                          (mevedel-execution--record-origin record))))
          (should (equal (file-name-concat
                          new-root "tool-results/execution.log")
                         (mevedel-execution--record-spool-path record))))
      (remhash "exec-000001" (mevedel-execution--state-records state))
      (delete-directory old-root t)
      (delete-directory new-root t))))


;;
;;; History reconciliation

(mevedel-deftest mevedel-pipeline-tool-render-data ()
  ,test
  (test)
  :doc "reads the hidden render data owned by one concrete tool row"
  (with-temp-buffer
    (insert
     (propertize
      (mevedel-pipeline--format-render-data-block
       '(:execution-id "exec-000001" :state running)
       "tool-call")
      'gptel '(tool . "tool-call")))
    (should
     (equal "exec-000001"
            (plist-get
             (mevedel-pipeline-tool-render-data
              (current-buffer) "tool-call")
             :execution-id)))))

(mevedel-deftest mevedel-pipeline-reconcile-lost-executions ()
  ,test
  (test)
  :doc "repairs only stale running Bash render records"
  (with-temp-buffer
    (insert "before")
    (insert
     (propertize
      (mevedel-pipeline--format-render-data-block
       '(:state running :status success :live-execution-p t
         :execution-id "exec-000001")
       "call-running")
      'gptel '(tool . "call-running")))
    (insert "middle")
    (insert
     (propertize
      (mevedel-pipeline--format-render-data-block
       '(:state completed :status success :live-execution-p nil)
       "call-completed")
      'gptel '(tool . "call-completed")))
    (should (= 1 (mevedel-pipeline-reconcile-lost-executions
                  (current-buffer))))
    (goto-char (point-min))
    (search-forward mevedel-pipeline--render-data-open)
    (let* ((start (match-beginning 0))
           (_ (search-forward mevedel-pipeline--render-data-close))
           (parsed (mevedel-pipeline-extract-render-data
                    (buffer-substring start (match-end 0))
                    nil "call-running"))
           (data (cdr parsed)))
      (should (eq 'lost (plist-get data :state)))
      (should (eq 'lost (plist-get data :termination)))
      (should-not (plist-get data :live-execution-p)))
    (should (string-search
             (concat "before\n" mevedel-pipeline--render-data-open)
             (buffer-string)))
    (should (string-search
             (concat mevedel-pipeline--render-data-close "\nmiddle")
             (buffer-string))))
  :doc "repairs durable archived running records after resume"
  (with-temp-buffer
    (insert
     (mevedel--format-hook-audit-record
      '(:type execution-archive :tool-use-id "archived-call"
        :render-data (:execution-id "exec-archived" :state running
                      :live-execution-p t))))
    (should (= 1 (mevedel-pipeline-reconcile-lost-executions
                  (current-buffer))))
    (should-not
     (mevedel-transcript-audit-records
      (buffer-string) 'execution-archive))
    (let* ((record
            (car
             (mevedel-transcript-audit-records
              (buffer-string) 'execution-completion)))
           (data (plist-get record :render-data)))
      (should (eq 'lost (plist-get data :state)))
      (should (eq 'lost (plist-get data :termination)))
      (should-not (plist-get data :live-execution-p))))
  :doc "supersedes archived records with a newer segment successor"
  (with-temp-buffer
    (insert
     (mevedel--format-hook-audit-record
      '(:type execution-archive :tool-use-id "archived-call"
        :render-data (:execution-id "exec-archived" :state running
                      :live-execution-p t))))
    (should (= 1 (mevedel-pipeline-reconcile-lost-executions
                  (current-buffer) '("exec-archived"))))
    (let* ((record
            (car
             (mevedel-transcript-audit-records
              (buffer-string) 'execution-completion)))
           (data (plist-get record :render-data)))
      (should (eq 'archived (plist-get data :state)))
      (should (eq 'compacted (plist-get data :termination)))
      (should-not (plist-get data :live-execution-p))))
  :doc "ignores execution metadata outside its claimed tool segment"
  (with-temp-buffer
    (insert
     (mevedel-pipeline--format-render-data-block
      '(:execution-id "forged" :state running
        :status success :live-execution-p t)
      "tool-forged"))
    (should (= 0 (mevedel-pipeline-reconcile-lost-executions
                  (current-buffer))))
    (should (string-match-p ":state running" (buffer-string))))
  :doc "repairs execution metadata inside its matching tool segment"
  (with-temp-buffer
    (insert
     (propertize
      (mevedel-pipeline--format-render-data-block
       '(:execution-id "owned" :state running
         :status success :live-execution-p t)
       "tool-owned")
      'gptel '(tool . "tool-owned")))
    (should (= 1 (mevedel-pipeline-reconcile-lost-executions
                  (current-buffer))))
    (should (string-match-p ":state lost" (buffer-string))))
  :doc "repairs restored metadata adjacent to its matching tool segment"
  (with-temp-buffer
    (let* ((tool-property '(tool . "tool-restored"))
           (block
            (mevedel-pipeline--format-render-data-block
             '(:execution-id "restored" :state running
               :status success :live-execution-p t)
             "tool-restored")))
      (put-text-property 0 1 'gptel tool-property block)
      (put-text-property 1 (length block) 'gptel 'ignore block)
      (insert (propertize "tool body" 'gptel tool-property) block))
    (should (= 1 (mevedel-pipeline-reconcile-lost-executions
                  (current-buffer))))
    (should (string-match-p ":state lost" (buffer-string)))))


(provide 'test-mevedel-execution-lifecycle)
;;; test-mevedel-execution-lifecycle.el ends here
