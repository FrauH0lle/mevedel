;;; test-mevedel-mcp-tools.el --- MCP pipeline ownership -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the external provider boundary with real sessions and tools.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-engine-test-support"))
(require 'mevedel-mcp-tools)

(mevedel-deftest mevedel-mcp-tools-schemas
  (:doc "uses the registered Read schema including required path")
  (let* ((tool (mevedel-tool-ensure "Read"))
         (schemas (json-parse-string
                   (json-serialize
                    (mevedel-mcp-tools-schemas
                     (list (mevedel-tool-gptel-tool tool)))
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

(mevedel-deftest mevedel-mcp-tools-call (:quiet t)
  ,test
  (test)

  :doc "Read returns real file content and preserves native tool identity"
  (mevedel-engine-test--with-session
    (let ((path (file-name-concat root "sample.txt")) delivered)
      (write-region "mcp fixture line\n" nil path nil 'silent)
      (mevedel-mcp-tools-call
       request buffer (mevedel-tool-ensure "Read")
       (list :file_path path) "toolu_read"
       (lambda (result outcome) (setq delivered (list result outcome))))
      (mevedel-test--await 3 "Read did not finish" delivered)
      (should (eq :json-false (plist-get (car delivered) :isError)))
      (should (string-match-p "mcp fixture line"
                              (plist-get (aref (plist-get (car delivered)
                                                         :content) 0) :text)))
      (should (equal "toolu_read" (plist-get (cadr delivered) :tool-use-id)))
      (should (eq 'success (plist-get (cadr delivered) :status)))))

  :doc "a replaced request cannot execute with its successor's authority"
  (mevedel-engine-test--with-session
    (let ((calls 0) delivered
          (old request))
      (mevedel-request-end)
      (setq request (mevedel-request-begin session))
      (mevedel-mcp-tools-call
       old buffer
       (mevedel-tool--create
        :name "Stale" :read-only-p t
        :handler (lambda (_args) (cl-incf calls) '(:result "bad")))
       nil "toolu_stale"
       (lambda (result outcome) (setq delivered (list result outcome))))
      (should (= calls 0))
      (should (eq t (plist-get (car delivered) :isError)))
      (should (eq 'obsolete-request (plist-get (cadr delivered) :reason)))))

  :doc "permission denial is an MCP error and never reaches the handler"
  (mevedel-engine-test--with-session
    (let ((calls 0) delivered)
      (mevedel-mcp-tools-call
       request buffer
       (mevedel-tool--create
        :name "Denied" :read-only-p t
        :check-permission-async
        (lambda (_tool _args callback) (funcall callback 'deny))
        :handler (lambda (_args) (cl-incf calls) '(:result "bad")))
       nil "toolu_denied"
       (lambda (result outcome) (setq delivered (list result outcome))))
      (mevedel-test--await 3 "Refusal did not finish" delivered)
      (should (= calls 0))
      (should (eq t (plist-get (car delivered) :isError)))
      (should (eq 'permission-denied (plist-get (cadr delivered) :reason)))))

  :doc "a retained child owns MCP calls independently of the root request"
  (mevedel-engine-test--with-session
    (let* ((child (generate-new-buffer " *mcp-child*"))
           (invocation (mevedel-agent-invocation--create
                        :path "/root/reader" :buffer child
                        :parent-session session :transcript-status 'running))
           (path (file-name-concat root "child.txt"))
           delivered)
      (unwind-protect
          (progn
            (write-region "child-owned evidence\n" nil path nil 'silent)
            (with-current-buffer child
              (setq-local mevedel--session session mevedel--workspace workspace
                          mevedel--agent-invocation invocation
                          default-directory (file-name-as-directory root)))
            (mevedel-mcp-tools-call
             invocation child (mevedel-tool-ensure "Read")
             (list :file_path path) "toolu_child"
             (lambda (result outcome) (setq delivered (list result outcome))))
            (mevedel-test--await 3 "Child Read did not finish" delivered)
            (should (eq 'success (plist-get (cadr delivered) :status)))
            (should (string-match-p "child-owned evidence"
                                    (plist-get (aref (plist-get (car delivered)
                                                               :content) 0) :text)))
            (should (eq request mevedel--current-request))
            (should-not (buffer-local-value 'mevedel--current-request child))
            ;; Shared Full Access never overrides this child's Plan ceiling.
            (setf (mevedel-agent-invocation-plan-read-only invocation) t)
            (setq delivered nil)
            (mevedel-mcp-tools-call
             invocation child (mevedel-tool-ensure "ApplyPatch")
             (list :patch
                   (format "*** Begin Patch\n*** Update File: %s\n@@\n-child-owned evidence\n+unauthorized\n*** End Patch"
                           path))
             "toolu_child_patch"
             (lambda (result outcome) (setq delivered (list result outcome))))
            (mevedel-test--await 3 "Child Plan refusal did not finish" delivered)
            (should (eq 'permission-denied (plist-get (cadr delivered) :reason)))
            (should (equal "child-owned evidence\n"
                           (with-temp-buffer
                             (insert-file-contents path) (buffer-string)))))
        (kill-buffer child))))

  :doc "retired, replaced and stopped child turns cannot borrow live authority"
  (mevedel-engine-test--with-session
    (let ((child (generate-new-buffer " *mcp-child-fences*"))
          (calls 0))
      (unwind-protect
          (dolist (state '(settled replaced wrong-session root-request boundary))
            (let* ((invocation (mevedel-agent-invocation--create
                                :path "/root/reader" :buffer child
                                :parent-session session :transcript-status 'running))
                   (tool (mevedel-tool--create
                          :name "Probe" :read-only-p t
                          :handler (lambda (_args)
                                     (cl-incf calls) '(:result "bad"))))
                   delivered)
              (with-current-buffer child
                (setq-local mevedel--session session mevedel--workspace workspace
                            mevedel--agent-invocation invocation
                            mevedel--current-request nil
                            default-directory (file-name-as-directory root)))
              (pcase state
                ('settled
                 (setf (mevedel-agent-invocation-runtime-settled-p invocation) t))
                ('replaced
                 (with-current-buffer child
                   (setq mevedel--agent-invocation
                         (mevedel-agent-invocation--create :path "/root/reader"))))
                ('wrong-session
                 (with-current-buffer child (setq mevedel--session nil)))
                ('root-request
                 (with-current-buffer child (setq mevedel--current-request request)))
                ('boundary
                 (setf (mevedel-engine-info invocation) (list :buffer child))
                 (should (mevedel-turn-end-at-boundary invocation 'hook-stop))
                 (should-not (mevedel-agent-invocation-runtime-fsm invocation))))
              (mevedel-mcp-tools-call
               invocation child tool nil "toolu_obsolete_child"
               (lambda (result outcome) (setq delivered (list result outcome))))
              (mevedel-test--await 3 "Ownership refusal did not finish" delivered)
              (should (= 0 calls))
              (should (eq t (plist-get (car delivered) :isError)))
              (should (eq 'obsolete-request (plist-get (cadr delivered) :reason)))
              (should (eq request mevedel--current-request))))
        (kill-buffer child))))

  :doc "captured images are native MCP blocks and display metadata stays local"
  (mevedel-engine-test--with-session
    (let (delivered)
      (mevedel-mcp-tools-call
       request buffer
       (mevedel-tool--create
        :name "Image" :read-only-p t
        :handler (lambda (_args)
                   '(:result "Image result" :status success
                     :render-data (:kind probe)
                     :media ((:kind image :mime "image/png" :data "QUJD")))))
       nil "toolu_image"
       (lambda (result outcome) (setq delivered (list result outcome))))
      (mevedel-test--await 3 "Image did not finish" delivered)
      (let ((content (plist-get (car delivered) :content)))
        (should (= 2 (length content)))
        (should (equal '(:type "text" :text "Image result") (aref content 0)))
        (should (equal '(:type "image" :data "QUJD" :mimeType "image/png")
                       (aref content 1))))
      (should (string-match-p "mevedel-render-data"
                              (plist-get (cadr delivered) :result)))
      (should (equal "QUJD" (plist-get
                             (car (plist-get (cadr delivered) :media)) :data)))))

  :doc "Goal tools own the external request and settle its usage exactly once"
  (mevedel-engine-test--with-session
    (let (delivered)
      (setf (mevedel-engine-info request)
            (plist-put (mevedel-engine-info request)
                       :tokens-full '(:input 20 :output 5)))
      (mevedel-mcp-tools-call
       request buffer (mevedel-tool-ensure "CreateGoal")
       '(:objective "Fix the fixture" :token_budget 100) "toolu_goal"
       (lambda (result outcome) (setq delivered (list result outcome))))
      (mevedel-test--await 3 "Goal creation did not finish" delivered)
      (should (eq 'success (plist-get (cadr delivered) :status)))
      (let ((goal (mevedel-session-goal session)))
        (should (equal "Fix the fixture" (mevedel-goal-objective goal)))
        (should (equal (mevedel-goal-id goal)
                       (plist-get (mevedel-engine-info request) :mevedel-goal-id)))
        (should-not (mevedel-request-fsm request))
        (mevedel-goal-pause)
        (should (plist-get (mevedel-engine-info request) :mevedel-end-turn))
        (mevedel-goal-settle-turn request)
        (mevedel-goal-settle-turn request)
        (should (eq 'paused (mevedel-goal-status goal)))
        (should (= 25 (mevedel-goal-tokens-used goal)))
        (should (= 1 (mevedel-goal-turns-run goal)))))))

(provide 'test-mevedel-mcp-tools)
;;; test-mevedel-mcp-tools.el ends here
