;;; test-niwa-execution-lifecycle.el --- Execution ownership boundaries -*- lexical-binding: t -*-

;;; Commentary:

;; Tests process lifetime at owner, session, history, and package boundaries.

;;; Code:

(require 'cl-lib)
(require 'niwa)
(require 'niwa-agents)
(require 'niwa-execution)
(require 'niwa-execution-process)
(require 'niwa-execution-target)
(require 'niwa-pipeline)
(require 'niwa-sandbox)
(require 'niwa-session-durability)
(require 'niwa-session-publication)
(require 'niwa-session-persistence)
(require 'niwa-transcript-audit)
(require 'niwa-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-execution-test-helpers"))

;; `tramp'
(declare-function tramp-cleanup-all-connections "tramp-cmds" ())


;;
;;; Ownership and teardown

(niwa-deftest niwa-execution--discard-record ()
  ,test
  (test)
  :doc "releases runtime state before invoking teardown callbacks"
  (let* ((session (niwa-session--create :name "discard-order"))
         teardown-after-cleanup-p
         (record
          (niwa-execution--record-create
           :finished-p t
           :origin (niwa-execution--origin-create :session session)
           :teardown-function
           (lambda ()
             (setq teardown-after-cleanup-p
                   (not
                    (gethash
                     'record
                     (niwa-execution--state-records
                      (niwa-session-execution-state session))))))
           :token 'record)))
    (puthash
     'record record
     (niwa-execution--state-records
      (niwa-execution--state-for-session session)))
    (niwa-execution--discard-record record 'session-ended)
    (should teardown-after-cleanup-p)))

(niwa-deftest niwa-execution-stop-owner ()
  ,test
  (test)
  :doc "stops only the selected owner within platform process limits"
  (let* ((root (make-temp-file "niwa-owner-lifetime-" t))
         (pid-file (file-name-concat root "child.pid"))
         (scratch-file (file-name-concat root "scratch.path"))
         (tombstone-file (file-name-concat root "tombstone.log"))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         (niwa-execution-process--child-kill-delay 0.05)
         owned sibling pid owned-id
         helper-result scratch tombstone)
    (unwind-protect
        (progn
          (setq owned
                (test-niwa-execution--start-managed
                 session root
                 (if (eq system-type 'windows-nt)
                     '("sleep" "30")
                   (list
                    "sh" "-c"
                    "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; wait"
                    "owner" pid-file))
                 :owner "agent-a"))
          (setq sibling
                (test-niwa-execution--start-managed
                 session root '("sleep" "30")
                 :owner "agent-b"))
          (niwa-execution-start-helper
           (lambda (result) (setq helper-result result))
           "niwa-owner-helper"
           (list "sh" "-c"
                 "printf '%s\n' \"$PWD\" > \"$1\"; exec sleep 30"
                 "helper" scratch-file)
           nil (list root) :session session :owner "agent-a")
          (test-niwa-execution--wait
           (lambda ()
             (when (file-readable-p scratch-file)
               (setq scratch
                     (with-temp-buffer
                       (insert-file-contents scratch-file)
                       (buffer-string)))
               (string-suffix-p "\n" scratch))))
          (setq scratch (string-trim scratch))
          (write-region "terminal" nil tombstone-file nil 'silent)
          (setq tombstone
                (test-niwa-execution--attach-child
                 (niwa-execution--record-create
                  :finished-p t
                  :origin (niwa-execution--origin-create
                           :owner "agent-a" :session session)
                  :token 'owner-tombstone)
                 tombstone-file))
          (puthash 'owner-tombstone tombstone
                   (niwa-execution--state-records
                    (niwa-session-execution-state session)))
          (setq owned-id (plist-get (plist-get owned :facts) :execution-id))
          (should (= 3 (niwa-execution-stop-owner session "agent-a")))
          (unless (eq system-type 'windows-nt)
            (setq pid (test-niwa-execution--read-pid pid-file))
            (test-niwa-execution--wait
             (lambda () (test-niwa-execution--process-gone-p pid))))
          (test-niwa-execution--wait
           (lambda ()
             (not
              (gethash
               owned-id
               (niwa-execution--state-records
                (niwa-session-execution-state session))))))
          (should-not (niwa-execution-list session "agent-a"))
          (should-not (niwa-execution-owner-live-p session "agent-a"))
          (should (niwa-execution-owner-live-p session "agent-b"))
          (should-not helper-result)
          (should-not (file-exists-p scratch))
          (should-not (file-exists-p tombstone-file))
          (should (plist-get (plist-get sibling :facts) :execution-id))
          (should owned-id))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))
  :doc "proves delayed remote KILL settlement before clearing mutation state"
  (let* ((root (make-temp-file "niwa-remote-owner-lifetime-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (original-signal-process (symbol-function 'signal-process))
         (niwa-sandbox-mode 'off)
         (niwa-execution-process--child-kill-delay 0.05)
         session execution-id kill-delivered-p kill-timer)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
          (setq session (test-niwa-execution--session remote-root))
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
                    (test-niwa-execution--start-managed
                     session remote-root
                     '("sh" "-c" "trap '' TERM; sleep 30")
                     :owner "agent-a")
                    :facts)
                   :execution-id))
            (should
             (niwa-session-durability-unsettled-mutation-p session))
            (should (= 1 (niwa-execution-stop-owner session "agent-a")))
            (should-not
             (niwa-execution-owner-live-p session "agent-a"))
            (test-niwa-execution--wait
             (lambda ()
               (and kill-delivered-p
                    (not
                     (gethash
                      execution-id
                      (niwa-execution--state-records
                       (niwa-session-execution-state session)))))))
            (should-not
             (niwa-session-durability-unsettled-mutation-p session))
            (should-not (niwa-execution-unknown-outcome session))))
      (when (timerp kill-timer)
        (cancel-timer kill-timer))
      (when session
        (niwa-execution-teardown-session session)
        (when (niwa-session-save-path session)
          (niwa-session-durability-lease-release
           (niwa-session-save-path session) session)))
      ;; This teardown performs target I/O, so it reopens the connection that
      ;; `niwa-test--with-local-shell-tramp' already closed.  Close it after
      ;; the last target work instead of leaving it for later tests.
      (tramp-cleanup-all-connections)
      (delete-directory root t))))

(niwa-deftest niwa-execution-teardown-session ()
  ,test
  (test)
  :doc "immediate teardown obeys platform limits and empties queued work"
  (let* ((root (make-temp-file "niwa-session-lifetime-" t))
         (pid-file (file-name-concat root "child.pid"))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         first-result second-result pid)
    (unwind-protect
        (progn
          (niwa-execution-start-bash
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
            (test-niwa-execution--wait
             (lambda () (file-readable-p pid-file))))
          (niwa-execution-start-bash
           (lambda (value) (setq second-result value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "printf queued")
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (should (= 2 (niwa-execution-teardown-session session)))
          (should-not first-result)
          (should-not second-result)
          (should-not (niwa-execution-session-live-p session))
          (should (= 0 (hash-table-count
                        (niwa-execution--state-records
                         (niwa-session-execution-state session)))))
          (if (eq system-type 'windows-nt)
            (test-niwa-execution--wait
             (lambda () (not (process-live-p first-process))))
            (setq pid (test-niwa-execution--read-pid pid-file))
            (test-niwa-execution--wait
             (lambda () (test-niwa-execution--process-gone-p pid)))))
      (niwa-execution-teardown-session session)
      (delete-directory root t))))

(niwa-deftest niwa-execution-session-live-p ()
  ,test
  (test)
  :doc "distinguishes active processes from terminal delivery state"
  (let* ((session (niwa-session--create :name "liveness"))
         (state (niwa-execution--state-for-session session))
         (record
          (niwa-execution--record-create
           :execution-id "exec-live"
           :origin (niwa-execution--origin-create :session session))))
    (puthash "exec-live" record (niwa-execution--state-records state))
    (should (niwa-execution-session-live-p session))
    (setf (niwa-execution--record-finished-p record) t)
    (should-not (niwa-execution-session-live-p session))
    (remhash "exec-live" (niwa-execution--state-records state))))

(niwa-deftest niwa-execution-working-p ()
  ,test
  (test)
  :doc "answers for the managed executions of every session, except monitors"
  (let* ((niwa-execution--sessions
          (make-hash-table :test #'eq :weakness 'key))
         (session (niwa-session--create :name "any-live"))
         (state (niwa-execution--state-for-session session))
         (record
          (niwa-execution--record-create
           :execution-id "exec-live"
           :origin (niwa-execution--origin-create :session session)))
         (monitor
          (niwa-execution--record-create
           :execution-id "exec-monitor" :kind 'monitor
           :origin (niwa-execution--origin-create :session session)))
         (helper
          (niwa-execution--record-create
           :origin (niwa-execution--origin-create :session session))))
    (should-not (niwa-execution-working-p))
    (puthash "helper" helper (niwa-execution--state-records state))
    (should-not (niwa-execution-working-p))
    (puthash "exec-monitor" monitor (niwa-execution--state-records state))
    (should-not (niwa-execution-working-p))
    (puthash "exec-live" record (niwa-execution--state-records state))
    (should (niwa-execution-working-p))
    (setf (niwa-execution--record-finished-p record) t)
    (should-not (niwa-execution-working-p))))

(niwa-deftest niwa-execution-teardown-all ()
  ,test
  (test)
  :doc "drains session and orphan records, including finished tombstones"
  (let* ((niwa-execution--sessions
          (make-hash-table :test #'eq :weakness 'key))
         (niwa-execution--orphan-state nil)
         (session (niwa-session--create :name "global-teardown"))
         (session-state (niwa-execution--state-for-session session))
         (orphan-state (niwa-execution--state-for-session nil))
         (session-spool (make-temp-file "niwa-session-tombstone-"))
         (orphan-spool (make-temp-file "niwa-orphan-tombstone-"))
         (cleaned 0)
         (session-record
          (test-niwa-execution--attach-child
           (niwa-execution--record-create
            :finished-p t
            :origin (niwa-execution--origin-create :session session)
            :teardown-function (lambda () (cl-incf cleaned))
            :token 'session-record)
           session-spool))
         (orphan-record
          (test-niwa-execution--attach-child
           (niwa-execution--record-create
            :origin (niwa-execution--origin-create :session nil)
            :teardown-function (lambda () (cl-incf cleaned))
            :token 'orphan-record)
           orphan-spool)))
    (puthash 'session-record session-record
             (niwa-execution--state-records session-state))
    (puthash 'orphan-record orphan-record
             (niwa-execution--state-records orphan-state))
    (should (= 2 (niwa-execution-teardown-all)))
    (should (= 2 cleaned))
    (should (= 0 (hash-table-count
                  (niwa-execution--state-records session-state))))
    (should (= 0 (hash-table-count
                  (niwa-execution--state-records orphan-state))))
    (should-not (file-exists-p session-spool))
    (should-not (file-exists-p orphan-spool))))

(niwa-deftest niwa-execution-relocate-artifacts ()
  ,test
  (test)
  :doc "retargets retained artifacts without changing execution ownership"
  (let* ((old-root (file-name-as-directory
                    (make-temp-file "niwa-artifact-old-" t)))
         (new-root (file-name-as-directory
                    (make-temp-file "niwa-artifact-new-" t)))
         (session (test-niwa-execution--session old-root))
         (state (niwa-execution--state-for-session session))
         (old-path (file-name-concat old-root "tool-results/execution.log"))
         (record
          (test-niwa-execution--attach-child
           (niwa-execution--record-create
            :execution-id "exec-000001"
            :origin (niwa-execution--origin-create
                     :owner "agent-a" :session session))
           old-path)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory old-path) t)
          (write-region "retained" nil old-path nil 'silent)
          (puthash "exec-000001" record
                   (niwa-execution--state-records state))
          (should (= 1 (niwa-execution-relocate-artifacts
                        session old-root new-root)))
          (should (equal "agent-a"
                         (niwa-execution--origin-owner
                          (niwa-execution--record-origin record))))
          (should (equal (file-name-concat
                          new-root "tool-results/execution.log")
                         (niwa-execution--spool-path record))))
      (remhash "exec-000001" (niwa-execution--state-records state))
      (delete-directory old-root t)
      (delete-directory new-root t)))
  :doc "retargets target-native remote output without another publication"
  (let* ((host "execution-relocate")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-remote-artifact-" t)))
         (remote-root (format "/niwamock:%s:%s" host local-root))
         (client-spool (make-temp-file "niwa-client-spool-"))
         session state record)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session (test-niwa-execution--session remote-root)
                state (niwa-execution--state-for-session session))
          (let* ((target (niwa-session-execution-target session))
                 (old-root (niwa-session-save-path session))
                 (new-root
                  (file-name-as-directory
                   (file-name-concat
                    (file-name-directory (directory-file-name old-root))
                    "renamed")))
                 (relative "tool-results/executions/exec-000001.log")
                 (old-native
                  (file-name-concat
                   (niwa-execution-target-native-path target old-root)
                   relative))
                 (new-native
                  (file-name-concat
                   (niwa-execution-target-native-path target new-root)
                   relative)))
            (setq record
                  (test-niwa-execution--attach-child
                   (niwa-execution--record-create
                    :execution-id "exec-000001"
                    :origin (niwa-execution--origin-create
                             :owner "agent-a" :session session)
                    :recoverable-output-path old-native)
                   client-spool))
            (puthash "exec-000001" record
                     (niwa-execution--state-records state))
            (setf (niwa-session-save-path session) new-root)
            (should (= 1 (niwa-execution-relocate-artifacts
                          session old-root new-root)))
            (should (equal client-spool
                           (niwa-execution--spool-path record)))
            (should (equal new-native
                           (niwa-execution--record-recoverable-output-path
                            record)))))
      (when (and state record)
        (remhash "exec-000001" (niwa-execution--state-records state)))
      (when (file-exists-p client-spool)
        (delete-file client-spool))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-execution-lifecycle)
;;; test-niwa-execution-lifecycle.el ends here
