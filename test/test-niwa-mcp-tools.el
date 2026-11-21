;;; test-niwa-mcp-tools.el --- MCP pipeline ownership -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the external provider boundary with real sessions and tools.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-engine-test-support"))
(require 'niwa-mcp-tools)

(niwa-deftest niwa-mcp-tools-schemas
  (:doc "uses the registered Read schema including required path")
  (let* ((tool (niwa-tool-ensure "Read"))
         (schemas (json-parse-string
                   (json-serialize
                    (niwa-mcp-tools-schemas
                     (list (niwa-tool-gptel-tool tool)))
                    :false-object :json-false)
                   :object-type 'plist))
         (schema (aref schemas 0))
         (input (plist-get schema :inputSchema)))
    (should (= 1 (length schemas)))
    (should (equal "Read" (plist-get schema :name)))
    (should (equal "object" (plist-get input :type)))
    (should (equal "string" (plist-get
                             (plist-get (plist-get input :properties) :file_path)
                             :type)))
    (should (member "file_path" (append (plist-get input :required) nil)))))

(niwa-deftest niwa-mcp-tools-call (:quiet t)
  ,test
  (test)

  :doc "Read returns real file content and preserves native tool identity"
  (niwa-engine-test--with-session
    (let ((path (file-name-concat root "sample.txt")) delivered)
      (write-region "mcp fixture line\n" nil path nil 'silent)
      (niwa-mcp-tools-call
       request buffer (niwa-tool-ensure "Read")
       (list :file_path path) "toolu_read"
       (lambda (result outcome) (setq delivered (list result outcome))))
      (niwa-test--await 3 "Read did not finish" delivered)
      (should (eq :json-false (plist-get (car delivered) :isError)))
      (should (string-match-p "mcp fixture line"
                              (plist-get (aref (plist-get (car delivered)
                                                         :content) 0) :text)))
      (should (equal "toolu_read" (plist-get (cadr delivered) :tool-use-id)))
      (should (eq 'success (plist-get (cadr delivered) :status)))))

  :doc "a replaced request cannot execute with its successor's authority"
  (niwa-engine-test--with-session
    (let ((calls 0) delivered
          (old request))
      (niwa-request-end)
      (setq request (niwa-request-begin session))
      (niwa-mcp-tools-call
       old buffer
       (niwa-tool--create
        :name "Stale" :read-only-p t
        :handler (lambda (_args) (cl-incf calls) '(:result "bad")))
       nil "toolu_stale"
       (lambda (result outcome) (setq delivered (list result outcome))))
      (should (= calls 0))
      (should (eq t (plist-get (car delivered) :isError)))
      (should (eq 'obsolete-request (plist-get (cadr delivered) :reason)))))

  :doc "cancelling an MCP WaitAgent releases its registration, not the turn"
  (niwa-engine-test--with-session
    (let* (outcomes
           (cancel
            (niwa-mcp-tools-call
             request buffer (niwa-tool-ensure "WaitAgent")
             '(:timeout_ms 3600000) "wait-cancelled"
             (lambda (_result outcome) (push outcome outcomes)))))
      (should (niwa-agent-control-root-waiting-p session))
      (let* ((waiter (niwa-session-agent-root-waiter session))
             (timer (niwa-agent-waiter-timer waiter)))
        ;; This is the canceller the MCP transport calls on notification or
        ;; disconnect.  The surrounding provider turn remains admitted.
        (funcall cancel)
        (should-not (niwa-agent-control-root-waiting-p session))
        (should-not (memq timer timer-list))
        (should-not (niwa-request-cancelled-p request))
        (should (= 1 (length outcomes)))
        (should (eq 'cancelled (plist-get (car outcomes) :reason))))
      (let ((successor
             (niwa-mcp-tools-call
              request buffer (niwa-tool-ensure "WaitAgent")
              '(:timeout_ms 3600000) "wait-successor"
              (lambda (_result outcome) (push outcome outcomes)))))
        (should (niwa-agent-control-root-waiting-p session))
        (funcall cancel)
        (should (niwa-agent-control-root-waiting-p session))
        (niwa-agent-control-wake-root-user session)
        (should-not (niwa-agent-control-root-waiting-p session))
        (should (= 2 (length outcomes)))
        (should (eq 'success (plist-get (car outcomes) :status)))
        (funcall successor)
        (should (= 2 (length outcomes))))))

  :doc "permission denial is an MCP error and never reaches the handler"
  (niwa-engine-test--with-session
    (let ((calls 0) delivered)
      (niwa-mcp-tools-call
       request buffer
       (niwa-tool--create
        :name "Denied" :read-only-p t
        :check-permission-async
        (lambda (_tool _args callback) (funcall callback 'deny))
        :handler (lambda (_args) (cl-incf calls) '(:result "bad")))
       nil "toolu_denied"
       (lambda (result outcome) (setq delivered (list result outcome))))
      (niwa-test--await 3 "Refusal did not finish" delivered)
      (should (= calls 0))
      (should (eq t (plist-get (car delivered) :isError)))
      (should (eq 'permission-denied (plist-get (cadr delivered) :reason)))))

  :doc "child wait cancellation clears waiting activity without cancelling siblings"
  (niwa-engine-test--with-session
    (let* ((child (generate-new-buffer " *waiting-child*"))
           (invocation (niwa-agent-invocation--create
                        :path "/root/worker" :buffer child
                        :parent-session session :transcript-status 'running))
           (record (niwa-agent-record--create
                    :path "/root/worker" :parent-path "/root"
                    :activity 'running :invocation invocation))
           (root-cancel
            (niwa-mcp-tools-call
             request buffer (niwa-tool-ensure "WaitAgent")
             nil "wait-root" #'ignore)))
      (unwind-protect
          (progn
            ;; This transient identity needs no persisted configuration.
            (setf (niwa-session-save-path session) nil
                  (niwa-session-agent-registry session)
                  (list (cons "/root/worker" record)))
            (with-current-buffer child
              (setq-local niwa--session session niwa--workspace workspace
                          niwa--agent-invocation invocation
                          default-directory (file-name-as-directory root)))
            (let ((cancel
                   (niwa-mcp-tools-call
                    invocation child (niwa-tool-ensure "WaitAgent")
                    nil "wait-child" #'ignore)))
              (should (niwa-agent-record-waiter record))
              (should (eq 'waiting (niwa-agent-record-activity record)))
              (funcall cancel)
              (should-not (niwa-agent-record-waiter record))
              (should (eq 'running (niwa-agent-record-activity record)))
              (should (niwa-agent-control-root-waiting-p session))))
        (funcall root-cancel)
        (setf (niwa-session-agent-registry session) nil)
        (kill-buffer child))))

  :doc "call cleanup handles cancellation during startup and synchronous completion"
  (niwa-engine-test--with-session
    (dolist (complete-first '(nil t))
      (let ((cleanups 0) outcomes)
        (let ((cancel
               (niwa-mcp-tools-call
                request buffer
                (niwa-tool--create
                 :name "CleanupRace" :read-only-p t :async-p t
                 :handler
                 (lambda (callback _args)
                   (if complete-first
                       (funcall callback '(:result "completed"))
                     (niwa-request-cancel request))
                   (list :cancel (lambda () (cl-incf cleanups)))))
                nil "cleanup-race"
                (lambda (_result outcome) (push outcome outcomes)))))
          (should (= cleanups (if complete-first 0 1)))
          (funcall cancel)
          (should (= cleanups (if complete-first 0 1)))
          (should (= 1 (length outcomes))))
        (niwa-request-end)
        (setq request (niwa-request-begin session)))))

  :doc "a retained child owns MCP calls independently of the root request"
  (niwa-engine-test--with-session
    (let* ((child (generate-new-buffer " *mcp-child*"))
           (invocation (niwa-agent-invocation--create
                        :path "/root/reader" :buffer child
                        :parent-session session :transcript-status 'running))
           (path (file-name-concat root "child.txt"))
           delivered)
      (unwind-protect
          (progn
            (write-region "child-owned evidence\n" nil path nil 'silent)
            (with-current-buffer child
              (setq-local niwa--session session niwa--workspace workspace
                          niwa--agent-invocation invocation
                          default-directory (file-name-as-directory root)))
            (niwa-mcp-tools-call
             invocation child (niwa-tool-ensure "Read")
             (list :file_path path) "toolu_child"
             (lambda (result outcome) (setq delivered (list result outcome))))
            (niwa-test--await 3 "Child Read did not finish" delivered)
            (should (eq 'success (plist-get (cadr delivered) :status)))
            (should (string-match-p "child-owned evidence"
                                    (plist-get (aref (plist-get (car delivered)
                                                               :content) 0) :text)))
            (should (eq request niwa--current-request))
            (should-not (buffer-local-value 'niwa--current-request child))
            ;; Shared Full Access never overrides this child's Plan ceiling.
            (setf (niwa-agent-invocation-plan-read-only invocation) t)
            (setq delivered nil)
            (niwa-mcp-tools-call
             invocation child (niwa-tool-ensure "ApplyPatch")
             (list :patch
                   (format "*** Begin Patch\n*** Update File: %s\n@@\n-child-owned evidence\n+unauthorized\n*** End Patch"
                           path))
             "toolu_child_patch"
             (lambda (result outcome) (setq delivered (list result outcome))))
            (niwa-test--await 3 "Child Plan refusal did not finish" delivered)
            (should (eq 'permission-denied (plist-get (cadr delivered) :reason)))
            (should (equal "child-owned evidence\n"
                           (with-temp-buffer
                             (insert-file-contents path) (buffer-string)))))
        (kill-buffer child))))

  :doc "retired, replaced and stopped child turns cannot borrow live authority"
  (niwa-engine-test--with-session
    (let ((child (generate-new-buffer " *mcp-child-fences*"))
          (calls 0))
      (unwind-protect
          (dolist (state '(settled replaced wrong-session root-request boundary))
            (let* ((invocation (niwa-agent-invocation--create
                                :path "/root/reader" :buffer child
                                :parent-session session :transcript-status 'running))
                   (tool (niwa-tool--create
                          :name "Probe" :read-only-p t
                          :handler (lambda (_args)
                                     (cl-incf calls) '(:result "bad"))))
                   delivered)
              (with-current-buffer child
                (setq-local niwa--session session niwa--workspace workspace
                            niwa--agent-invocation invocation
                            niwa--current-request nil
                            default-directory (file-name-as-directory root)))
              (pcase state
                ('settled
                 (setf (niwa-agent-invocation-runtime-settled-p invocation) t))
                ('replaced
                 (with-current-buffer child
                   (setq niwa--agent-invocation
                         (niwa-agent-invocation--create :path "/root/reader"))))
                ('wrong-session
                 (with-current-buffer child (setq niwa--session nil)))
                ('root-request
                 (with-current-buffer child (setq niwa--current-request request)))
                ('boundary
                 (setf (niwa-engine-info invocation) (list :buffer child))
                 (should (niwa-turn-end-at-boundary invocation 'hook-stop))
                 (should-not (niwa-agent-invocation-runtime-fsm invocation))))
              (niwa-mcp-tools-call
               invocation child tool nil "toolu_obsolete_child"
               (lambda (result outcome) (setq delivered (list result outcome))))
              (niwa-test--await 3 "Ownership refusal did not finish" delivered)
              (should (= 0 calls))
              (should (eq t (plist-get (car delivered) :isError)))
              (should (eq 'obsolete-request (plist-get (cadr delivered) :reason)))
              (should (eq request niwa--current-request))))
        (kill-buffer child))))

  :doc "captured images are native MCP blocks and display metadata stays local"
  (niwa-engine-test--with-session
    (let (delivered)
      (niwa-mcp-tools-call
       request buffer
       (niwa-tool--create
        :name "Image" :read-only-p t
        :handler (lambda (_args)
                   '(:result "Image result" :status success
                     :render-data (:kind probe)
                     :media ((:kind image :mime "image/png" :data "QUJD")))))
       nil "toolu_image"
       (lambda (result outcome) (setq delivered (list result outcome))))
      (niwa-test--await 3 "Image did not finish" delivered)
      (let ((content (plist-get (car delivered) :content)))
        (should (= 2 (length content)))
        (should (equal '(:type "text" :text "Image result") (aref content 0)))
        (should (equal '(:type "image" :data "QUJD" :mimeType "image/png")
                       (aref content 1))))
      (should (string-match-p "niwa-render-data"
                              (plist-get (cadr delivered) :result)))
      (should (equal "QUJD" (plist-get
                             (car (plist-get (cadr delivered) :media)) :data)))))

  :doc "Goal tools own the external request and settle its usage exactly once"
  (niwa-engine-test--with-session
    (let (delivered)
      (setf (niwa-engine-info request)
            (plist-put (niwa-engine-info request)
                       :tokens-full '(:input 20 :output 5)))
      (niwa-mcp-tools-call
       request buffer (niwa-tool-ensure "CreateGoal")
       '(:objective "Fix the fixture" :token_budget 100) "toolu_goal"
       (lambda (result outcome) (setq delivered (list result outcome))))
      (niwa-test--await 3 "Goal creation did not finish" delivered)
      (should (eq 'success (plist-get (cadr delivered) :status)))
      (let ((goal (niwa-session-goal session)))
        (should (equal "Fix the fixture" (niwa-goal-objective goal)))
        (should (equal (niwa-goal-id goal)
                       (plist-get (niwa-engine-info request) :niwa-goal-id)))
        (should-not (niwa-request-fsm request))
        (niwa-goal-pause)
        (should (plist-get (niwa-engine-info request) :niwa-end-turn))
        (niwa-goal-settle-turn request)
        (niwa-goal-settle-turn request)
        (should (eq 'paused (niwa-goal-status goal)))
        (should (= 25 (niwa-goal-tokens-used goal)))
        (should (= 1 (niwa-goal-turns-run goal)))))))

(provide 'test-niwa-mcp-tools)
;;; test-niwa-mcp-tools.el ends here
