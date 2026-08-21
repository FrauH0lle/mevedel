;;; test-mevedel-execution-lifecycle.el --- Execution ownership boundaries -*- lexical-binding: t -*-

;;; Commentary:

;; Tests process lifetime at owner, session, history, and package boundaries.

;;; Code:

(require 'cl-lib)
(require 'mevedel)
(require 'mevedel-agents)
(require 'mevedel-execution)
(require 'mevedel-execution-process)
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
         (mevedel-execution-process--child-kill-delay 0.05)
         owned sibling pid owned-id
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
                (test-mevedel-execution--attach-child
                 (mevedel-execution--record-create
                  :finished-p t
                  :origin (mevedel-execution--origin-create
                           :owner "agent-a" :session session)
                  :token 'owner-tombstone)
                 tombstone-file))
          (puthash 'owner-tombstone tombstone
                   (mevedel-execution--state-records
                    (mevedel-session-execution-state session)))
          (setq owned-id (plist-get (plist-get owned :facts) :execution-id))
          (should (= 3 (mevedel-execution-stop-owner session "agent-a")))
          (unless (eq system-type 'windows-nt)
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
         (mevedel-execution-process--child-kill-delay 0.05)
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
                     session remote-root
                     '("sh" "-c" "trap '' TERM; sleep 30")
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
         first-result second-result pid)
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
          (test-mevedel-execution--attach-child
           (mevedel-execution--record-create
            :finished-p t
            :origin (mevedel-execution--origin-create :session session)
            :teardown-function (lambda () (cl-incf cleaned))
            :token 'session-record)
           session-spool))
         (orphan-record
          (test-mevedel-execution--attach-child
           (mevedel-execution--record-create
            :origin (mevedel-execution--origin-create :session nil)
            :teardown-function (lambda () (cl-incf cleaned))
            :token 'orphan-record)
           orphan-spool)))
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
          (test-mevedel-execution--attach-child
           (mevedel-execution--record-create
            :execution-id "exec-000001"
            :origin (mevedel-execution--origin-create
                     :owner "agent-a" :session session))
           old-path)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory old-path) t)
          (write-region "retained" nil old-path nil 'silent)
          (puthash "exec-000001" record
                   (mevedel-execution--state-records state))
          (should (= 1 (mevedel-execution-relocate-artifacts
                        session old-root new-root)))
          (should (equal "agent-a"
                         (mevedel-execution--origin-owner
                          (mevedel-execution--record-origin record))))
          (should (equal (file-name-concat
                          new-root "tool-results/execution.log")
                         (mevedel-execution--spool-path record))))
      (remhash "exec-000001" (mevedel-execution--state-records state))
      (delete-directory old-root t)
      (delete-directory new-root t)))
  :doc "retargets target-native remote output without another publication"
  (let* ((host "execution-relocate")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-artifact-" t)))
         (remote-root (format "/mevedelmock:%s:%s" host local-root))
         (client-spool (make-temp-file "mevedel-client-spool-"))
         session state record)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (setq session (test-mevedel-execution--session remote-root)
                state (mevedel-execution--state-for-session session))
          (let* ((target (mevedel-session-execution-target session))
                 (old-root (mevedel-session-save-path session))
                 (new-root
                  (file-name-as-directory
                   (file-name-concat
                    (file-name-directory (directory-file-name old-root))
                    "renamed")))
                 (relative "tool-results/executions/exec-000001.log")
                 (old-native
                  (file-name-concat
                   (mevedel-execution-target-native-path target old-root)
                   relative))
                 (new-native
                  (file-name-concat
                   (mevedel-execution-target-native-path target new-root)
                   relative)))
            (setq record
                  (test-mevedel-execution--attach-child
                   (mevedel-execution--record-create
                    :execution-id "exec-000001"
                    :origin (mevedel-execution--origin-create
                             :owner "agent-a" :session session)
                    :recoverable-output-path old-native)
                   client-spool))
            (puthash "exec-000001" record
                     (mevedel-execution--state-records state))
            (setf (mevedel-session-save-path session) new-root)
            (should (= 1 (mevedel-execution-relocate-artifacts
                          session old-root new-root)))
            (should (equal client-spool
                           (mevedel-execution--spool-path record)))
            (should (equal new-native
                           (mevedel-execution--record-recoverable-output-path
                            record)))))
      (when (and state record)
        (remhash "exec-000001" (mevedel-execution--state-records state)))
      (when (file-exists-p client-spool)
        (delete-file client-spool))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-execution-lifecycle)
;;; test-mevedel-execution-lifecycle.el ends here
