;;; test-mevedel-execution-lifecycle.el --- Execution ownership boundaries -*- lexical-binding: t -*-

;;; Commentary:

;; Tests process lifetime at owner, session, history, and package boundaries.

;;; Code:

(require 'cl-lib)
(require 'mevedel)
(require 'mevedel-execution)
(require 'mevedel-pipeline)
(require 'mevedel-session-persistence)
(require 'mevedel-transcript-audit)
(require 'mevedel-view-stream)
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


;;
;;; Ownership and teardown

(mevedel-deftest mevedel-execution-stop-owner ()
  ,test
  (test)
  :doc "stops only the selected owner and cleans its descendant process"
  (let* ((root (make-temp-file "mevedel-owner-lifetime-" t))
         (pid-file (file-name-concat root "child.pid"))
         (scratch-file (file-name-concat root "scratch.path"))
         (tombstone-file (file-name-concat root "tombstone.log"))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         owned sibling pid owned-id helper-result scratch tombstone)
    (unwind-protect
        (progn
          (setq owned
                (test-mevedel-execution--start-managed
                 session root
                 (list "sh" "-c"
                       "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; wait"
                       "owner" pid-file)
                 :owner "agent-a"))
          (setq sibling
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "while :; do sleep 1; done")
                 :owner "agent-b"))
          (mevedel-execution-start-helper
           (lambda (result) (setq helper-result result))
           "mevedel-owner-helper"
           (list "sh" "-c"
                 "printf '%s' \"$PWD\" > \"$1\"; sleep 30"
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
          (should (= 3 (mevedel-execution-stop-owner session "agent-a")))
          (setq pid (test-mevedel-execution--read-pid pid-file))
          (test-mevedel-execution--wait
           (lambda () (test-mevedel-execution--process-gone-p pid)))
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
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-teardown-session ()
  ,test
  (test)
  :doc "immediate teardown kills descendants and empties queued scheduler work"
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
           (list "sh" "-c"
                 "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; wait"
                 "first" pid-file)
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (test-mevedel-execution--wait (lambda () (file-readable-p pid-file)))
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
          (setq pid (test-mevedel-execution--read-pid pid-file))
          (test-mevedel-execution--wait
           (lambda () (test-mevedel-execution--process-gone-p pid))))
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
       '(:execution-id "exec-000001" :state running))
      'gptel '(tool . "tool-call")))
    (should
     (equal "exec-000001"
            (plist-get
             (mevedel-pipeline-tool-render-data
              (current-buffer) "tool-call")
             :execution-id)))))

(mevedel-deftest mevedel-view-stream-prepare-execution-row-archive ()
  ,test
  (test)
  :doc "separates live and already-completed execution rows"
  (with-temp-buffer
    (insert
     (propertize
      (mevedel-pipeline--format-render-data-block
       '(:execution-id "exec-live" :state running :live-execution-p t))
      'gptel '(tool . "live-call")))
    (insert
     (propertize
      (mevedel-pipeline--format-render-data-block
       '(:execution-id "exec-done" :state completed
         :live-execution-p nil))
      'gptel '(tool . "done-call")))
    (let ((plan
           (mevedel-view-stream-prepare-execution-row-archive
            (current-buffer) '("live-call" "done-call" "missing-call"))))
      (should (equal "live-call" (caar (plist-get plan :live))))
      (should (eq 'running
                  (plist-get (cdar (plist-get plan :live)) :state)))
      (should (equal "done-call"
                     (caar (plist-get plan :completed)))))))

(mevedel-deftest mevedel-view-stream-commit-execution-row-archive ()
  ,test
  (test)
  :doc "marks live rows and persists already-completed rows after compaction"
  (with-temp-buffer
    (let ((plan
           '(:live (("live-call" :execution-id "exec-live"
                     :state running :live-execution-p t))
             :completed (("done-call" :execution-id "exec-done"
                          :state completed)))))
      (insert (mevedel-view-stream-execution-row-archive-text plan))
      (mevedel-view-stream-commit-execution-row-archive
       (current-buffer) plan))
    (should (gethash "live-call"
                     mevedel-view-stream--archived-execution-rows))
    (should (= 1
               (length
                (mevedel-transcript-audit-records
                 (buffer-string) 'execution-completion))))
    (should (= 1
               (length
                (mevedel-transcript-audit-records
                 (buffer-string) 'execution-archive))))))

(mevedel-deftest mevedel-pipeline-reconcile-lost-executions ()
  ,test
  (test)
  :doc "repairs only stale running Bash render records"
  (with-temp-buffer
    (insert "before")
    (insert (mevedel-pipeline--format-render-data-block
             '(:state running :status success :live-execution-p t
               :execution-id "exec-000001")))
    (insert "middle")
    (insert (mevedel-pipeline--format-render-data-block
             '(:state completed :status success :live-execution-p nil)))
    (should (= 1 (mevedel-pipeline-reconcile-lost-executions
                  (current-buffer))))
    (goto-char (point-min))
    (search-forward mevedel-pipeline--render-data-open)
    (let* ((start (match-beginning 0))
           (_ (search-forward mevedel-pipeline--render-data-close))
           (parsed (mevedel-pipeline-extract-render-data
                    (buffer-substring-no-properties start (match-end 0))))
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
      (should-not (plist-get data :live-execution-p)))))

(mevedel-deftest mevedel-view-stream--record-archived-execution-terminal ()
  ,test
  (test)
  :doc "publishes completion transactionally and permits a later save"
  (let* ((path (make-temp-file "mevedel-execution-archive-"))
         (buffer (find-file-noselect path))
         (session (mevedel-session--create :name "archive")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session)
            (let ((archive
                   (mevedel-view-stream-execution-row-archive-text
                    '(:live (("old-call" :execution-id "exec-old"
                              :state running :live-execution-p t))))))
              (insert archive))
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime)
            (insert "pending prompt\n")
            (set-buffer-modified-p nil))
          (mevedel-view-stream-commit-execution-row-archive
           buffer '(:live (("old-call" :execution-id "exec-old"
                            :state running :live-execution-p t))))
          (with-temp-buffer
            (insert-file-contents path)
            (should (= 1
                       (length
                        (mevedel-transcript-audit-records
                         (buffer-string) 'execution-archive)))))
          (require 'mevedel-session-persistence)
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--write-current-buffer-atomically)
                     (lambda (&rest _) (error "Publication failed")))
                    ((symbol-function 'display-warning) #'ignore))
            (mevedel-view-stream-handle-execution-event
             (list :type 'terminal :session session :data-buffer buffer
                   :owner "main" :tool-use-id "old-call"
                   :facts '(:state completed :outcome success :exit-code 0)
                   :whole-output "done")))
          (with-current-buffer buffer
            (should (gethash "old-call"
                             mevedel-view-stream--archived-execution-rows))
            (should (gethash "old-call"
                             mevedel-view-stream--pending-execution-terminals)))
          (with-temp-buffer
            (insert-file-contents path)
            (should (= 1
                       (length
                        (mevedel-transcript-audit-records
                         (buffer-string) 'execution-archive)))))
          (with-current-buffer buffer
            (should-not (buffer-modified-p)))
          (mevedel-view-stream--retry-pending-execution-terminals buffer)
          (with-current-buffer buffer
            (should-not (buffer-modified-p))
            (should (verify-visited-file-modtime buffer))
            (let ((records
                   (mevedel-transcript-audit-records
                    (buffer-string) 'execution-completion)))
              (should (= 1 (length records)))
              (should (equal "old-call"
                             (plist-get (car records) :tool-use-id))))
            (goto-char (point-max))
            (insert "assistant done\n")
            (save-buffer))
          (with-temp-buffer
            (insert-file-contents path)
            (should (string-search "pending prompt" (buffer-string)))
            (should (string-search "assistant done" (buffer-string)))
            (should (= 1
                       (length
                        (mevedel-transcript-audit-records
                         (buffer-string) 'execution-completion))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p path) (delete-file path))))
  :doc "retries a disk-first partial commit from a narrowed live buffer"
  (let* ((path (make-temp-file "mevedel-execution-partial-"))
         (buffer (find-file-noselect path))
         (session (mevedel-session--create :name "partial"))
         (event
          (list :type 'terminal :session session :data-buffer buffer
                :owner "main" :tool-use-id "partial-call"
                :facts '(:state completed :outcome success :exit-code 0)
                :whole-output "done")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert
             (mevedel-view-stream-execution-row-archive-text
              '(:live (("partial-call" :execution-id "exec-partial"
                        :state running :live-execution-p t)))))
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime))
          (mevedel-view-stream-commit-execution-row-archive
           buffer '(:live (("partial-call" :execution-id "exec-partial"
                            :state running :live-execution-p t))))
          (let ((replace
                 (symbol-function
                  'mevedel-view-stream--replace-archived-execution-record)))
            (cl-letf
                (((symbol-function
                   'mevedel-view-stream--replace-archived-execution-record)
                  (lambda (&rest args)
                    (if (eq (current-buffer) buffer)
                        (error "Live publication failed")
                      (apply replace args))))
                 ((symbol-function 'display-warning) #'ignore))
              (mevedel-view-stream-handle-execution-event event)))
          (with-temp-buffer
            (insert-file-contents path)
            (should (= 1
                       (length
                        (mevedel-transcript-audit-records
                         (buffer-string) 'execution-completion)))))
          (with-current-buffer buffer
            (should (= 1
                       (length
                        (mevedel-transcript-audit-records
                         (buffer-string) 'execution-archive))))
            (narrow-to-region (point-max) (point-max)))
          (mevedel-view-stream--retry-pending-execution-terminals buffer)
          (with-current-buffer buffer
            (widen)
            (should-not
             (gethash "partial-call"
                      mevedel-view-stream--pending-execution-terminals))
            (should (= 1
                       (length
                        (mevedel-transcript-audit-records
                         (buffer-string) 'execution-completion))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (widen)
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p path) (delete-file path))))
  :doc "reroutes a terminal queued before its archive marker commits"
  (let* ((path (make-temp-file "mevedel-execution-reroute-"))
         (buffer (find-file-noselect path))
         (session (mevedel-session--create :name "reroute"))
         (event
          (list :type 'terminal :session session :data-buffer buffer
                :owner "main" :tool-use-id "reroute-call"
                :facts '(:state completed :outcome success :exit-code 0)
                :whole-output "done")))
    (unwind-protect
        (progn
          (mevedel-view-stream-handle-execution-event event)
          (with-current-buffer buffer
            (should
             (gethash "reroute-call"
                      mevedel-view-stream--pending-execution-terminals))
            (insert
             (mevedel-view-stream-execution-row-archive-text
              '(:live (("reroute-call" :execution-id "exec-reroute"
                        :state running :live-execution-p t)))))
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime))
          (mevedel-view-stream-commit-execution-row-archive
           buffer '(:live (("reroute-call" :execution-id "exec-reroute"
                            :state running :live-execution-p t))))
          (mevedel-view-stream--retry-pending-execution-terminals buffer)
          (with-current-buffer buffer
            (should-not
             (gethash "reroute-call"
                      mevedel-view-stream--pending-execution-terminals))
            (should (= 1
                       (length
                        (mevedel-transcript-audit-records
                         (buffer-string) 'execution-completion))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p path) (delete-file path)))))


(provide 'test-mevedel-execution-lifecycle)
;;; test-mevedel-execution-lifecycle.el ends here
