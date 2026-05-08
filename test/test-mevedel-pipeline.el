;;; test-mevedel-pipeline.el --- Tests for pipeline engine -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-pipeline)
(require 'mevedel-permissions)
(require 'mevedel-tool-registry)
(require 'mevedel-tools)
(require 'mevedel-session-persistence)
;; gptel-request needed for mevedel-define-tool tests
(require 'gptel-request nil t)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Pipeline runner

(mevedel-deftest mevedel-pipeline--run ()
		 ,test
		 (test)
		 :doc "empty step list calls callback with result"
		 (let (called-with)
		   (mevedel-pipeline--run nil
					  (lambda (r) (setq called-with r))
					  '(:result "done"))
		   (should (equal called-with "done")))
		 :doc "steps execute in order"
		 (let ((order nil))
		   (mevedel-pipeline--run
		    (list (lambda (ctx next _fail)
			    (push 1 order)
			    (funcall next ctx))
			  (lambda (ctx next _fail)
			    (push 2 order)
			    (funcall next ctx))
			  (lambda (ctx next _fail)
			    (push 3 order)
			    (funcall next (plist-put ctx :result "final"))))
		    (lambda (_r) (push 'done order))
		    nil)
		   (should (equal (nreverse order) '(1 2 3 done))))
		 :doc "context threads through steps"
		 (let (result)
		   (mevedel-pipeline--run
		    (list (lambda (ctx next _fail)
			    (funcall next (plist-put ctx :count 1)))
			  (lambda (ctx next _fail)
			    (funcall next (plist-put ctx :count
						     (1+ (plist-get ctx :count)))))
			  (lambda (ctx next _fail)
			    (funcall next (plist-put ctx :result
						     (plist-get ctx :count)))))
		    (lambda (r) (setq result r))
		    nil)
		   (should (equal result 2)))
		 :doc "steps run under the context default-directory"
		 (let* ((root (make-temp-file "mevedel-pipeline-root-" t))
			(other (make-temp-file "mevedel-pipeline-other-" t))
			(context (list :default-directory root))
			seen)
		   (unwind-protect
		       (let ((default-directory other))
			 (mevedel-pipeline--run
			  (list (lambda (ctx next _fail)
				  (setq seen default-directory)
				  (funcall next (plist-put ctx :result "ok"))))
			  #'ignore
			  context)
			 (should (equal seen (file-name-as-directory root))))
		     (delete-directory root t)
		     (delete-directory other t)))
		 :doc "error in step calls callback with error string"
		 (let (result)
		   (mevedel-pipeline--run
		    (list (lambda (_ctx _next _fail)
			    (error "Something broke")))
		    (lambda (r) (setq result r))
		    nil)
		   (should (string-prefix-p "Error:" result)))
		 :doc "validation error produces error callback"
		 (let (result)
		   (mevedel-pipeline--run
		    (list (lambda (_ctx _next _fail)
			    (signal 'mevedel-validation-error '("Bad input"))))
		    (lambda (r) (setq result r))
		    nil)
		   (should (string-match-p "Bad input" result)))
		 :doc "permission denied produces error callback"
		 (let (result)
		   (mevedel-pipeline--run
		    (list (lambda (_ctx _next _fail)
			    (signal 'mevedel-permission-denied '("Not allowed"))))
		    (lambda (r) (setq result r))
		    nil)
		   (should (string-match-p "Permission denied" result))
		   (should (string-match-p "Not allowed" result)))
		 :doc "async step suspends and resumes"
		 (let (result saved-next saved-ctx)
		   (mevedel-pipeline--run
		    (list (lambda (ctx next _fail)
			    ;; Save continuation for later
			    (setq saved-next next saved-ctx ctx))
			  (lambda (ctx next _fail)
			    (funcall next (plist-put ctx :result "after-async"))))
		    (lambda (r) (setq result r))
		    nil)
		   ;; Not yet called
		   (should-not result)
		   ;; Resume
		   (funcall saved-next saved-ctx)
		   (should (equal result "after-async")))
		 :doc "async continuation resumes under the context default-directory"
		 (let* ((root (make-temp-file "mevedel-pipeline-root-" t))
			(other (make-temp-file "mevedel-pipeline-other-" t))
			(context (list :default-directory root))
			saved-next saved-ctx seen)
		   (unwind-protect
		       (progn
			 (mevedel-pipeline--run
			  (list (lambda (ctx next _fail)
				  (setq saved-next next saved-ctx ctx))
				(lambda (ctx next _fail)
				  (setq seen default-directory)
				  (funcall next (plist-put ctx :result "resumed"))))
			  #'ignore
			  context)
			 (let ((default-directory other))
			   (funcall saved-next saved-ctx))
			 (should (equal seen (file-name-as-directory root))))
		     (delete-directory root t)
		     (delete-directory other t)))
		 :doc "error after async step still calls callback"
		 (let (result saved-next saved-ctx)
		   (mevedel-pipeline--run
		    (list (lambda (ctx next _fail)
			    (setq saved-next next saved-ctx ctx))
			  (lambda (_ctx _next _fail)
			    (error "Async failure")))
		    (lambda (r) (setq result r))
		    nil)
		   (funcall saved-next saved-ctx)
		   (should (string-prefix-p "Error:" result)))
		 :doc "fail continuation produces Error: callback"
		 (let (result)
		   (mevedel-pipeline--run
		    (list (lambda (_ctx _next fail)
			    (funcall fail "Something went wrong")))
		    (lambda (r) (setq result r))
		    nil)
		   (should (equal result "Error: Something went wrong")))
		 :doc "second next on the same step is dropped"
		 (let (results saved-next saved-ctx)
		   (mevedel-pipeline--run
		    (list (lambda (ctx next _fail)
			    (setq saved-next next saved-ctx ctx))
			  (lambda (ctx next _fail)
			    (funcall next (plist-put ctx :result "first"))))
		    (lambda (r) (push r results))
		    nil)
		   (funcall saved-next saved-ctx)
		   ;; Latch: a second invocation must not re-enter the chain.
		   (let ((display-warning-minimum-level :emergency))
		     (funcall saved-next saved-ctx))
		   (should (equal results '("first"))))
		 :doc "calling fail after next is a no-op (latch)"
		 (let (results saved-fail saved-next saved-ctx)
		   (mevedel-pipeline--run
		    (list (lambda (ctx next fail)
			    (setq saved-next next saved-fail fail saved-ctx ctx))
			  (lambda (ctx next _fail)
			    (funcall next (plist-put ctx :result "ok"))))
		    (lambda (r) (push r results))
		    nil)
		   (funcall saved-next saved-ctx)
		   (let ((display-warning-minimum-level :emergency))
		     (funcall saved-fail "ignored"))
		   (should (equal results '("ok")))))


;;
;;; Validate step

(mevedel-deftest mevedel-pipeline--step-validate ()
		 ,test
		 (test)
		 :doc "passes with valid args"
		 (let* ((tool (mevedel-tool--create
			       :name "TestTool"
			       :args '((name string :required "Name"))))
			(ctx (list :tool tool :args '(:name "hello")))
			called)
		   (mevedel-pipeline--step-validate
		    ctx (lambda (_c) (setq called t)) #'ignore)
		   (should called))
		 :doc "signals validation error for missing required arg"
		 (let* ((tool (mevedel-tool--create
			       :name "TestTool"
			       :args '((name string :required "Name"))))
			(ctx (list :tool tool :args nil)))
		   (should-error
		    (mevedel-pipeline--step-validate ctx #'ignore #'ignore)
		    :type 'mevedel-validation-error))
		 :doc "signals validation error for wrong type"
		 (let* ((tool (mevedel-tool--create
			       :name "TestTool"
			       :args '((count integer :required "Count"))))
			(ctx (list :tool tool :args '(:count "not-a-number"))))
		   (should-error
		    (mevedel-pipeline--step-validate ctx #'ignore #'ignore)
		    :type 'mevedel-validation-error))
		 :doc "passes with nil args spec"
		 (let* ((tool (mevedel-tool--create :name "NoArgs" :args nil))
			(ctx (list :tool tool :args nil))
			called)
		   (mevedel-pipeline--step-validate
		    ctx (lambda (_c) (setq called t)) #'ignore)
		   (should called)))


;;
;;; Handler step

(mevedel-deftest mevedel-pipeline--step-handler ()
		 ,test
		 (test)
		 :doc "sync handler sets result in context"
		 (let* ((tool (mevedel-tool--create
			       :name "SyncTool"
			       :handler (lambda (args)
					  (format "got %s" (plist-get args :name)))
			       :async-p nil))
			(ctx (list :tool tool :args '(:name "test")))
			result-ctx)
		   (mevedel-pipeline--step-handler
		    ctx (lambda (c) (setq result-ctx c)) #'ignore)
		   (should (equal (plist-get result-ctx :result) "got test")))
		 :doc "async handler calls continuation with result"
		 (let* ((tool (mevedel-tool--create
			       :name "AsyncTool"
			       :handler (lambda (callback args)
					  (funcall callback
						   (format "async %s"
							   (plist-get args :val))))
			       :async-p t))
			(ctx (list :tool tool :args '(:val "data")))
			result-ctx)
		   (mevedel-pipeline--step-handler
		    ctx (lambda (c) (setq result-ctx c)) #'ignore)
		   (should (equal (plist-get result-ctx :result) "async data")))
		 :doc "async handler can defer continuation"
		 (let* (saved-cb
			(tool (mevedel-tool--create
			       :name "DeferTool"
			       :handler (lambda (callback _args)
					  (setq saved-cb callback))
			       :async-p t))
			(ctx (list :tool tool :args nil))
			result-ctx)
		   (mevedel-pipeline--step-handler
		    ctx (lambda (c) (setq result-ctx c)) #'ignore)
		   (should-not result-ctx)
		   (funcall saved-cb "deferred-result")
		   (should (equal (plist-get result-ctx :result) "deferred-result"))))


;;
;;; Step list builder

(mevedel-deftest mevedel-pipeline--build-steps ()
		 ,test
		 (test)
		 :doc "read-only tool includes pre/post hook steps"
		 (let* ((tool (mevedel-tool--create
			       :name "ReadTool"
			       :read-only-p t))
			(steps (mevedel-pipeline--build-steps tool)))
		   (should (= (length steps) 6))
		   (should (eq (nth 0 steps) #'mevedel-pipeline--step-validate))
		   (should (eq (nth 1 steps) #'mevedel-pipeline--step-pre-tool-hooks))
		   (should (eq (nth 2 steps) #'mevedel-pipeline--step-permission))
		   (should (eq (nth 3 steps) #'mevedel-pipeline--step-handler))
		   (should (eq (nth 4 steps) #'mevedel-pipeline--step-attach-render-data))
		   (should (eq (nth 5 steps) #'mevedel-pipeline--step-post-tool-hooks)))
		 :doc "write tool includes snapshot step"
		 (let* ((tool (mevedel-tool--create
			       :name "WriteTool"
			       :read-only-p nil))
			(steps (mevedel-pipeline--build-steps tool)))
		   (should (= (length steps) 7))
		   (should (eq (nth 0 steps) #'mevedel-pipeline--step-validate))
		   (should (eq (nth 1 steps) #'mevedel-pipeline--step-pre-tool-hooks))
		   (should (eq (nth 2 steps) #'mevedel-pipeline--step-permission))
		   (should (eq (nth 3 steps) #'mevedel-pipeline--step-snapshot))
		   (should (eq (nth 4 steps) #'mevedel-pipeline--step-handler))
		   (should (eq (nth 5 steps) #'mevedel-pipeline--step-attach-render-data))
		   (should (eq (nth 6 steps) #'mevedel-pipeline--step-post-tool-hooks)))
		 :doc "includes persist step when max-result-size is set"
		 (let* ((tool (mevedel-tool--create
			       :name "WithPersist"
			       :read-only-p t
			       :max-result-size 1000))
			(steps (mevedel-pipeline--build-steps tool)))
		   (should (= 7 (length steps)))
		   (should (eq (nth 4 steps) #'mevedel-pipeline--step-persist))
		   (should (eq (nth 5 steps)
			       #'mevedel-pipeline--step-attach-render-data))
		   (should (eq (car (last steps))
			       #'mevedel-pipeline--step-post-tool-hooks)))
		 :doc "omits persist step when max-result-size is nil"
		 (let* ((tool (mevedel-tool--create
			       :name "NoPersist"
			       :read-only-p t
			       :max-result-size nil))
			(steps (mevedel-pipeline--build-steps tool)))
		   (should (= 6 (length steps)))
		   (should-not (memq #'mevedel-pipeline--step-persist steps))
		   (should (memq #'mevedel-pipeline--step-attach-render-data steps))
		   (should (memq #'mevedel-pipeline--step-post-tool-hooks steps))))


;;
;;; Pre-tool hook step

(mevedel-deftest mevedel-pipeline--step-pre-tool-hooks
		 (:doc "routes PreToolUse denials through PermissionDenied hooks")
		 ,test
		 (test)
		 (let* ((tool (mevedel-tool--create
			       :name "Write"
			       :args '((file_path string :required "Path"))))
			(context (list :tool tool
				       :args '(:file_path "/tmp/out")
				       :default-directory default-directory))
			permission-denied-p
			result)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (event _payload callback &rest _)
				(pcase event
				  ('PreToolUse
				   (funcall callback
					    '(:permission-decision deny
								   :permission-reason "hook denied")))
				  ('PermissionDenied
				   (setq permission-denied-p t)
				   (funcall callback
					    '(:permission-reason "rewritten denial")))))))
		     (mevedel-pipeline--step-pre-tool-hooks
		      context
		      (lambda (_ctx) (setq result "next"))
		      (lambda (reason) (setq result reason))))
		   (should permission-denied-p)
		   (should (equal result "rewritten denial")))
		 :doc "fails without continuing when updated input is invalid"
		 (let* ((tool (mevedel-tool--create
			       :name "Write"
			       :args '((file_path string :required "Path"))))
			(context (list :tool tool
				       :args '(:file_path "/tmp/out")
				       :default-directory default-directory))
			next-called
			failure)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (_event _payload callback &rest _)
				(funcall callback '(:updated-input (:nope t))))))
		     (mevedel-pipeline--step-pre-tool-hooks
		      context
		      (lambda (_ctx) (setq next-called t))
		      (lambda (reason) (setq failure reason))))
		   (should-not next-called)
		   (should failure)))


;;
;;; Post-tool hook step

(mevedel-deftest mevedel-pipeline--step-post-tool-hooks
		 (:doc "passes captured request, invocation, and response payload to post hooks")
		 ,test
		 (test)
		 (let* ((tool (mevedel-tool--create :name "Read"))
			(request (list :request t))
			(invocation (list :invocation t))
			(context (list :tool tool
				       :args nil
				       :result "ok"
				       :request request
				       :invocation invocation
				       :default-directory default-directory))
			seen-request
			seen-invocation
			seen-payload
			result)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (_event payload callback
					      &optional _session _workspace req inv)
				(setq seen-request req
				      seen-invocation inv
				      seen-payload payload)
				(funcall callback nil))))
		     (mevedel-pipeline--step-post-tool-hooks
		      context
		      (lambda (ctx) (setq result (plist-get ctx :result)))
		      #'ignore))
		   (should (eq seen-request request))
		   (should (eq seen-invocation invocation))
		   (should (equal (plist-get seen-payload :tool-response) "ok"))
		   (should (equal result "ok")))
		 :doc "strips render-data from post-tool hook payload"
		 (let* ((tool (mevedel-tool--create :name "Read"))
			(result (concat "visible"
					(mevedel-pipeline--format-render-data-block
					 '(:kind diff))))
			(context (list :tool tool
				       :args nil
				       :result result
				       :raw-result "visible"
				       :default-directory default-directory))
			seen-payload)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (_event payload callback &rest _)
				(setq seen-payload payload)
				(funcall callback nil))))
		     (mevedel-pipeline--step-post-tool-hooks context #'ignore #'ignore))
		   (should (equal (plist-get seen-payload :tool-response) "visible"))
		   (should-not
		    (string-search mevedel-pipeline--render-data-open
				   (plist-get seen-payload :result)))))

(mevedel-deftest mevedel-pipeline--step-post-tool-hooks/no-block
		 (:doc "does not fail the pipeline for post-tool blocking decisions")
		 ,test
		 (test)
		 (let* ((tool (mevedel-tool--create :name "Read"))
			(context (list :tool tool
				       :args nil
				       :result "ok"
				       :default-directory default-directory))
			result
			failed)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (_event _payload callback &rest _)
				(funcall callback
					 '(:continue nil :stop-reason "too late")))))
		     (mevedel-pipeline--step-post-tool-hooks
		      context
		      (lambda (ctx) (setq result (plist-get ctx :result)))
		      (lambda (reason) (setq failed reason))))
		   (should-not failed)
		   (should (equal result "ok"))))

(mevedel-deftest mevedel-pipeline--step-post-tool-hooks/buffer-local
		 (:doc "runs native post hooks in the captured dispatch buffer")
		 ,test
		 (test)
		 (let* ((tool (mevedel-tool--create :name "Read"))
			(buffer (generate-new-buffer " *mevedel-post-hook-test*"))
			(context (list :tool tool
				       :args nil
				       :result "ok"
				       :buffer buffer
				       :default-directory default-directory))
			seen
			result)
		   (unwind-protect
		       (progn
			 (with-current-buffer buffer
			   (setq-local mevedel-post-tool-use-functions
				       (list (lambda (event)
					       (setq seen (plist-get event :result))
					       nil))))
			 (with-temp-buffer
			   (mevedel-pipeline--step-post-tool-hooks
			    context
			    (lambda (ctx) (setq result (plist-get ctx :result)))
			    #'ignore))
			 (should (equal seen "ok"))
			 (should (equal result "ok")))
		     (when (buffer-live-p buffer)
		       (kill-buffer buffer)))))


;;
;;; Permission step

(mevedel-deftest mevedel-pipeline--step-permission ()
		 ,test
		 (test)
		 :doc "allows read-only tool in default mode"
		 (let* ((tool (mevedel-tool--create
			       :name "Read"
			       :read-only-p t))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			called)
		   (mevedel-pipeline--step-permission
		    ctx (lambda (_c) (setq called t)) #'ignore)
		   (should called))
		 :doc "fails with Permission denied when rules deny"
		 (let* ((tool (mevedel-tool--create
			       :name "Edit"
			       :read-only-p nil))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules '(("Edit" :action deny)))
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			fail-reason)
		   (mevedel-pipeline--step-permission
		    ctx #'ignore (lambda (r) (setq fail-reason r)))
		   (should (equal fail-reason "Permission denied")))
		 :doc "allows when explicit allow rule matches"
		 (let* ((tool (mevedel-tool--create
			       :name "Edit"
			       :read-only-p nil))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules '(("Edit" :action allow)))
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			called)
		   (mevedel-pipeline--step-permission
		    ctx (lambda (_c) (setq called t)) #'ignore)
		   (should called))
		 :doc "allows in trust-all mode"
		 (let* ((tool (mevedel-tool--create
			       :name "Edit"
			       :read-only-p nil))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'trust-all)
			called)
		   (mevedel-pipeline--step-permission
		    ctx (lambda (_c) (setq called t)) #'ignore)
		   (should called))
		 :doc "fails with Permission denied for non-read-only tool in plan mode"
		 (let* ((tool (mevedel-tool--create
			       :name "Edit"
			       :read-only-p nil))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'plan)
			fail-reason)
		   (mevedel-pipeline--step-permission
		    ctx #'ignore (lambda (r) (setq fail-reason r)))
		   (should (equal fail-reason "Permission denied")))
		 :doc "tool check-permission returning allow is respected"
		 (let* ((tool (mevedel-tool--create
			       :name "CustomTool"
			       :check-permission (lambda (_ts _input) 'allow)
			       :read-only-p nil))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			called)
		   (mevedel-pipeline--step-permission
		    ctx (lambda (_c) (setq called t)) #'ignore)
		   (should called))
		 :doc "tool check-permission allow bypasses outside-workspace generic prompt"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-pipeline-root-" t)))
			(outside (file-name-as-directory
				  (make-temp-file "mevedel-pipeline-outside-" t)))
			(ws (mevedel-workspace--create
			     :type 'project :id "root" :root root
			     :name "root" :file-cache nil))
			(session (mevedel-session--create
				  :name "test" :workspace ws))
			(tool (mevedel-tool--create
			       :name "RequestAccess"
			       :check-permission (lambda (_ts _input) 'allow)
			       :get-path (lambda (args) (plist-get args :directory))
			       :read-only-p t))
			(ctx (list :tool tool
				   :args (list :directory outside)
				   :session session
				   :workspace ws))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			called
			enqueued)
		   (unwind-protect
		       (cl-letf (((symbol-function 'mevedel-permission--enqueue)
				  (lambda (&rest _) (setq enqueued t))))
			 (mevedel-pipeline--step-permission
			  ctx (lambda (_c) (setq called t)) #'ignore)
			 (should called)
			 (should-not enqueued))
		     (delete-directory root t)
		     (delete-directory outside t)))
		 :doc "reads session rules from context, not buffer-local"
		 (let* ((tool (mevedel-tool--create
			       :name "Edit"
			       :read-only-p nil))
			(session (mevedel-session--create
				  :name "test"
				  :permission-rules '(("Edit" :action allow))))
			(ctx (list :tool tool :args nil :session session))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			called)
		   (mevedel-pipeline--step-permission
		    ctx (lambda (_c) (setq called t)) #'ignore)
		   (should called))
		 :doc "ignores buffer-local session — only context :session counts"
		 (let* ((tool (mevedel-tool--create
			       :name "Edit"
			       :read-only-p nil))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'plan)
			(mevedel--session (mevedel-session--create
					   :name "phantom"
					   :permission-rules '(("Edit" :action allow))))
			fail-reason)
		   ;; The dynamic mevedel--session has an allow rule but the step must
		   ;; not look at it; only the missing :session in `ctx' applies.
		   (mevedel-pipeline--step-permission
		    ctx #'ignore (lambda (r) (setq fail-reason r)))
		   (should (equal fail-reason "Permission denied")))
		 :doc "sync slot signaling permission-denied surfaces REASON via fail"
		 (let* ((tool (mevedel-tool--create
			       :name "Custom"
			       :check-permission
			       (lambda (_ts _input)
				 (signal 'mevedel-permission-denied '("user feedback X")))
			       :read-only-p nil))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			fail-reason)
		   (mevedel-pipeline--step-permission
		    ctx #'ignore (lambda (r) (setq fail-reason r)))
		   (should (equal fail-reason "Permission denied: user feedback X")))
		 :doc "async slot returning 'allow advances to next"
		 (let* ((tool (mevedel-tool--create
			       :name "AsyncSlot"
			       :check-permission-async
			       (lambda (_ts _input cont) (funcall cont 'allow))
			       :read-only-p nil))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			called)
		   (mevedel-pipeline--step-permission
		    ctx (lambda (_c) (setq called t)) #'ignore)
		   (should called))
		 :doc "async slot returning (deny . REASON) surfaces via fail"
		 (let* ((tool (mevedel-tool--create
			       :name "AsyncSlot"
			       :check-permission-async
			       (lambda (_ts _input cont)
				 (funcall cont '(deny . "Custom slot reason")))
			       :read-only-p nil))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			fail-reason)
		   (mevedel-pipeline--step-permission
		    ctx #'ignore (lambda (r) (setq fail-reason r)))
		   (should (equal fail-reason "Permission denied: Custom slot reason")))
		 :doc "async slot returning (feedback . TEXT) maps to scoped denial with text"
		 (let* ((tool (mevedel-tool--create
			       :name "AsyncSlot"
			       :check-permission-async
			       (lambda (_ts _input cont)
				 (funcall cont '(feedback . "user typed this")))
			       :read-only-p nil))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			fail-reason)
		   (mevedel-pipeline--step-permission
		    ctx #'ignore (lambda (r) (setq fail-reason r)))
		   (should (equal fail-reason "Permission denied: user typed this")))
		 :doc "async slot returning 'aborted surfaces as fail aborted"
		 (let* ((tool (mevedel-tool--create
			       :name "AsyncSlot"
			       :check-permission-async
			       (lambda (_ts _input cont) (funcall cont 'aborted))
			       :read-only-p nil))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			fail-reason)
		   (mevedel-pipeline--step-permission
		    ctx #'ignore (lambda (r) (setq fail-reason r)))
		   (should (equal fail-reason "aborted")))
		 :doc "async slot returning nil falls through to chain"
		 (let* ((tool (mevedel-tool--create
			       :name "AsyncSlot"
			       :check-permission-async
			       (lambda (_ts _input cont) (funcall cont nil))
			       :read-only-p t))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			called)
		   ;; Read-only + default mode → step 8 returns 'allow.
		   (mevedel-pipeline--step-permission
		    ctx (lambda (_c) (setq called t)) #'ignore)
		   (should called))
		 :doc "workspace-root path is not broadened to parent directory when prompted"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-pipeline-root-" t)))
			(root-without-slash (directory-file-name root))
			(ws (mevedel-workspace--create
			     :type 'project :id "root" :root root
			     :name "root" :file-cache nil))
			(session (mevedel-session--create
				  :name "test" :workspace ws))
			(tool (mevedel-tool--create
			       :name "Grep"
			       :read-only-p t
			       :get-path (lambda (args) (plist-get args :path))))
			(ctx (list :tool tool
				   :args (list :path root-without-slash)
				   :session session
				   :workspace ws))
			(mevedel-permission-rules
			 `(("Grep" :path ,root-without-slash :action ask)))
			(mevedel-protected-paths nil)
			captured-entry)
		   (unwind-protect
		       (cl-letf (((symbol-function 'mevedel-permission--enqueue)
				  (lambda (entry &optional _session)
				    (setq captured-entry entry))))
			 (mevedel-pipeline--step-permission ctx #'ignore #'ignore)
			 (should (equal "Grep" (plist-get captured-entry :tool-name)))
			 (should (eq :path (plist-get captured-entry :specifier-key)))
			 (should (equal root-without-slash
					(plist-get captured-entry :specifier-value))))
		     (delete-directory root t)))
		 :doc "error from async prompt callback surfaces through fail, not a strand"
		 ;; When apply-prompt-result throws (e.g. a persistent-rule write
		 ;; failing), the error fires after the runner's outer `condition-case'
		 ;; has already unwound — the dispatcher must catch and route to
		 ;; `fail' rather than letting the error escape and strand the FSM.
		 (let* ((tool (mevedel-tool--create
			       :name "AsyncSlot"
			       :check-permission-async
			       (lambda (_ts _input cont) (funcall cont 'ask))
			       :read-only-p nil))
			(session (mevedel-session--create :name "test"))
			(ctx (list :tool tool :args nil :session session))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			next-called fail-reason)
		   (cl-letf (((symbol-function 'mevedel-permission--prompt-async)
			      (lambda (_t _p _a cont &optional _count _entry)
				(funcall cont 'always-allow)))
			     ((symbol-function 'mevedel-permission--apply-prompt-result)
			      (lambda (&rest _) (error "disk write failed"))))
		     (let ((mevedel--session session))
		       (mevedel-pipeline--step-permission
			ctx
			(lambda (_c) (setq next-called t))
			(lambda (r) (setq fail-reason r)))))
		   (should-not next-called)
		   (should (stringp fail-reason))
		   (should (string-match-p "disk write failed" fail-reason)))
		 :doc "fail-closed PermissionRequest stop beats allow"
		 (let* ((tool (mevedel-tool--create
			       :name "Edit"
			       :read-only-p nil))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules '(("Edit" :action ask)))
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default)
			fail-reason
			next-called)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (_event _payload callback &rest _)
				(funcall callback
					 '(:continue nil
						     :stop-reason "hook failed"
						     :permission-decision allow)))))
		     (mevedel-pipeline--step-permission
		      ctx
		      (lambda (_c) (setq next-called t))
		      (lambda (reason) (setq fail-reason reason))))
		   (should-not next-called)
		   (should (equal fail-reason
				  "Permission denied: blocked by PermissionRequest: hook failed"))))


;;
;;; Permission propagation across parent / sub-agent

(mevedel-deftest mevedel-pipeline--permission-propagation ()
		 ,test
		 (test)

		 :doc "sub-agent sees parent's session rules through context :session"
		 ;; The agent buffer carries `mevedel--session' set buffer-locally
		 ;; to the parent session struct (by reference).  When the pipeline
		 ;; captures `mevedel--session' at tool entry and threads it
		 ;; through `:session' on the context, the permission step honors
		 ;; the parent's session rules.
		 (let* ((tool (mevedel-tool--create :name "Edit" :read-only-p nil))
			(parent-session
			 (mevedel-session--create
			  :name "parent"
			  :permission-rules '(("Edit" :action allow))))
			;; Agent buffer would carry the same struct by reference.
			(sub-agent-session-alias parent-session)
			(ctx (list :tool tool :args nil
				   :session sub-agent-session-alias))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'plan)
			called)
		   (mevedel-pipeline--step-permission
		    ctx (lambda (_c) (setq called t)) #'ignore)
		   (should called))

		 :doc "rule added through sub-agent alias is visible on parent's struct"
		 (let* ((parent-session (mevedel-session--create :name "parent"))
			(sub-agent-alias parent-session))
		   (mevedel-permission--add-session-rule
		    sub-agent-alias "Edit" 'allow "/foo/*")
		   (should (equal (mevedel-session-permission-rules parent-session)
				  '(("Edit" :path "/foo/*" :action allow)))))

		 :doc "permission-mode toggle on parent observed by sub-agent next call"
		 (let* ((tool (mevedel-tool--create :name "Edit" :read-only-p nil))
			(parent-session
			 (mevedel-session--create
			  :name "parent" :permission-mode 'default))
			(sub-agent-alias parent-session)
			(ctx (list :tool tool :args nil :session sub-agent-alias))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'default))
		   ;; Parent flips to plan-mode mid-conversation; sub-agent's next
		   ;; pipeline entry must observe the change.
		   (setf (mevedel-session-permission-mode parent-session) 'plan)
		   (let (fail-reason)
		     (mevedel-pipeline--step-permission
		      ctx #'ignore (lambda (r) (setq fail-reason r)))
		     (should (equal fail-reason "Permission denied"))))

		 :doc "tripwire warning fires when non-read-only tool runs without session"
		 (let* ((tool (mevedel-tool--create :name "Edit" :read-only-p nil))
			(ctx (list :tool tool :args nil))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'trust-all)
			(warned nil))
		   (cl-letf (((symbol-function 'display-warning)
			      (lambda (kind msg &rest _)
				(when (and (eq kind 'mevedel)
					   (string-match-p "no session in context" msg))
				  (setq warned t)))))
		     (mevedel-pipeline--step-permission
		      ctx (lambda (_c) nil) #'ignore))
		   (should warned))

		 :doc "no tripwire when session is present"
		 (let* ((tool (mevedel-tool--create :name "Edit" :read-only-p nil))
			(session (mevedel-session--create :name "p"))
			(ctx (list :tool tool :args nil :session session))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			(mevedel-permission-mode 'trust-all)
			(warned nil))
		   (cl-letf (((symbol-function 'display-warning)
			      (lambda (kind msg &rest _)
				(when (and (eq kind 'mevedel)
					   (string-match-p "no session in context" msg))
				  (setq warned t)))))
		     (mevedel-pipeline--step-permission
		      ctx (lambda (_c) nil) #'ignore))
		   (should-not warned)))


;;
;;; Full pipeline

(mevedel-deftest mevedel-pipeline-run-tool ()
		 ,test
		 (test)
		 :doc "sync tool runs through pipeline"
		 (let* ((tool (mevedel-tool--create
			       :name "Echo"
			       :handler (lambda (args) (plist-get args :msg))
			       :args '((msg string :required "Message"))
			       :read-only-p t
			       :async-p nil))
			result)
		   (mevedel-pipeline-run-tool
		    tool (lambda (r) (setq result r)) '(:msg "hello"))
		   (should (equal result "hello")))
		 :doc "async tool runs through pipeline"
		 (let* ((tool (mevedel-tool--create
			       :name "AsyncEcho"
			       :handler (lambda (cb args) (funcall cb (plist-get args :msg)))
			       :args '((msg string :required "Message"))
			       :read-only-p t
			       :async-p t))
			result)
		   (mevedel-pipeline-run-tool
		    tool (lambda (r) (setq result r)) '(:msg "async hello"))
		   (should (equal result "async hello")))
		 :doc "tool handlers default to the workspace root"
		 (let* ((root (make-temp-file "mevedel-tool-root-" t))
			(other (make-temp-file "mevedel-tool-other-" t))
			(ws (mevedel-workspace--create :root root))
			(mevedel--session (mevedel-session--create
					   :name "main"
					   :workspace ws))
			(tool (mevedel-tool--create
			       :name "PwdTool"
			       :handler (lambda (_args) default-directory)
			       :args nil
			       :read-only-p t
			       :async-p nil))
			result)
		   (unwind-protect
		       (let ((default-directory other))
			 (mevedel-pipeline-run-tool tool (lambda (r) (setq result r)) nil)
			 (should (equal result (file-name-as-directory root))))
		     (delete-directory root t)
		     (delete-directory other t)))
		 :doc "tool handlers default to the session working directory"
		 (let* ((root (make-temp-file "mevedel-tool-root-" t))
			(module-dir (file-name-concat root "packages" "api"))
			(other (make-temp-file "mevedel-tool-other-" t))
			(ws (mevedel-workspace--create :root root))
			(mevedel--session (mevedel-session--create
					   :name "main"
					   :workspace ws
					   :working-directory module-dir))
			(tool (mevedel-tool--create
			       :name "PwdTool"
			       :handler (lambda (_args) default-directory)
			       :args nil
			       :read-only-p t
			       :async-p nil))
			result)
		   (make-directory module-dir t)
		   (unwind-protect
		       (let ((default-directory other))
			 (mevedel-pipeline-run-tool tool (lambda (r) (setq result r)) nil)
			 (should (equal result (file-name-as-directory module-dir))))
		     (delete-directory root t)
		     (delete-directory other t)))
		 :doc "async tool continuations default to the workspace root"
		 (let* ((root (make-temp-file "mevedel-tool-root-" t))
			(other (make-temp-file "mevedel-tool-other-" t))
			(ws (mevedel-workspace--create :root root))
			(mevedel--session (mevedel-session--create
					   :name "main"
					   :workspace ws))
			saved-cb
			(tool (mevedel-tool--create
			       :name "AsyncPwdTool"
			       :handler (lambda (cb _args) (setq saved-cb cb))
			       :args nil
			       :read-only-p t
			       :async-p t))
			result)
		   (unwind-protect
		       (progn
			 (let ((default-directory other))
			   (mevedel-pipeline-run-tool
			    tool (lambda (_r) (setq result default-directory)) nil))
			 (let ((default-directory other))
			   (funcall saved-cb "done"))
			 (should (equal result (file-name-as-directory root))))
		     (delete-directory root t)
		     (delete-directory other t)))
		 :doc "async tool continuations default to the session working directory"
		 (let* ((root (make-temp-file "mevedel-tool-root-" t))
			(module-dir (file-name-concat root "packages" "api"))
			(other (make-temp-file "mevedel-tool-other-" t))
			(ws (mevedel-workspace--create :root root))
			(mevedel--session (mevedel-session--create
					   :name "main"
					   :workspace ws
					   :working-directory module-dir))
			saved-cb
			(tool (mevedel-tool--create
			       :name "AsyncPwdTool"
			       :handler (lambda (cb _args) (setq saved-cb cb))
			       :args nil
			       :read-only-p t
			       :async-p t))
			result)
		   (make-directory module-dir t)
		   (unwind-protect
		       (progn
			 (let ((default-directory other))
			   (mevedel-pipeline-run-tool
			    tool (lambda (_r) (setq result default-directory)) nil))
			 (let ((default-directory other))
			   (funcall saved-cb "done"))
			 (should (equal result (file-name-as-directory module-dir))))
		     (delete-directory root t)
		     (delete-directory other t)))
		 :doc "validation failure returns error"
		 (let* ((tool (mevedel-tool--create
			       :name "Strict"
			       :handler (lambda (_args) "should not run")
			       :args '((name string :required "Name"))
			       :read-only-p t
			       :async-p nil))
			result)
		   (mevedel-pipeline-run-tool
		    tool (lambda (r) (setq result r)) nil)
		   (should (string-prefix-p "Error:" result)))
		 :doc "handler error returns error string"
		 (let* ((tool (mevedel-tool--create
			       :name "Broken"
			       :handler (lambda (_args) (error "Handler exploded"))
			       :args nil
			       :read-only-p t
			       :async-p nil))
			result)
		   (mevedel-pipeline-run-tool
		    tool (lambda (r) (setq result r)) nil)
		   (should (string-match-p "Handler exploded" result)))
		 :doc "persist still fires when handler wraps callback in with-temp-buffer"
		 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel" "sessions" "main")))
			(mevedel--session (mevedel-session--create
					   :name "main"
					   :workspace ws
					   :save-path save-path))
			;; Handler mimics Grep/Glob: all work, including the callback,
			;; runs inside `with-temp-buffer'.  If step-persist were
			;; reading `current-buffer' it would see the temp buffer and
			;; fail to find the session, truncating instead of persisting.
			(tool (mevedel-tool--create
			       :name "BigFromTemp"
			       :handler (lambda (cb _args)
					  (with-temp-buffer
					    (funcall cb (make-string 500 ?y))))
			       :args nil
			       :read-only-p t
			       :async-p t
			       :max-result-size 100))
			result)
		   (unwind-protect
		       (progn
			 (mevedel-pipeline-run-tool
			  tool (lambda (r) (setq result r)) nil)
			 (should (string-prefix-p "<persisted-output>" result))
			 (should (directory-files
				  (file-name-concat save-path "tool-results")
				  nil "\\.txt$")))
		     (delete-directory tmpdir t)))
		 :doc "once-fire guard: signal escaping after next-cont recursion delivered success drops the late error"
		 ;; Regression for the foreground-stash hang's secondary cause.  An
		 ;; inner step (e.g. attach-render-data) fires its NEXT, the recursion
		 ;; runs the empty-steps branch which fires the success delivery, then
		 ;; the deeper step's body signals.  The signal escapes the recursion
		 ;; and is caught by the outer step's condition-case, which now fires
		 ;; the gptel callback directly with an `Error: ...' string -- and the
		 ;; once-fire wrapper drops that late error so the consumer is not
		 ;; double-fired with both success and failure for the same call.
		 (let* ((deliveries nil)
			;; The consumer captures every fire.  Without the once-fire
			;; wrapper the consumer would see two fires: the success and
			;; the late error.
			(callback (lambda (r) (push r deliveries)))
			;; Step that fires NEXT successfully (which recurses through
			;; the empty-steps branch and delivers the success), then
			;; signals.  The runner's condition-case catches the signal
			;; and -- per the fix -- fires `callback' directly with an
			;; `Error: ...' string.  The once-fire wrapper at the entry
			;; must drop that second attempt.
			(post-next-signal-step
			 (lambda (ctx next _fail)
			   (funcall next (plist-put ctx :result "ok-from-step"))
			   (error "step body signaled after next-cont fired")))
			;; Bypass `mevedel-pipeline-run-tool' to install our custom
			;; once-fire wrapper around the bare runner so the test
			;; directly exercises the runner's escape path.
			(called nil)
			(once-callback
			 (lambda (result)
			   (unless called
			     (setq called t)
			     (condition-case err
				 (funcall callback result)
			       (error
				(display-warning
				 'mevedel
				 (format "Pipeline final callback signaled: %S" err)
				 :warning)))))))
		   (let ((display-warning-minimum-level :emergency))
		     (mevedel-pipeline--run
		      (list post-next-signal-step) once-callback nil))
		   (should (= 1 (length deliveries)))
		   (should (equal "ok-from-step" (car deliveries))))

		 :doc "once-fire guard: signaling consumer is caught and does not strand the pipeline"
		 ;; The defensive `condition-case' inside the once-fire wrapper
		 ;; demotes a signaling consumer to a `display-warning' so a
		 ;; misbehaving gptel callback cannot escape and unwind the
		 ;; runner's caller.  Verifies that path -- the wrapper still marks
		 ;; itself fired and refuses the next attempt.
		 (let* ((tool (mevedel-tool--create
			       :name "Signaling"
			       :handler (lambda (_args) "payload")
			       :args nil
			       :read-only-p t
			       :async-p nil))
			(count 0)
			(signaling-cb (lambda (_r)
					(cl-incf count)
					(error "consumer signaled"))))
		   (let ((display-warning-minimum-level :emergency))
		     (mevedel-pipeline-run-tool tool signaling-cb nil))
		   ;; The consumer was called once; its signal was caught inside the
		   ;; once-fire wrapper, so the runner's caller did not unwind.
		   (should (= 1 count))))


;;
;;; Args conversion

(mevedel-deftest mevedel-pipeline--positional-to-plist ()
		 ,test
		 (test)
		 :doc "converts positional args to plist"
		 (let ((specs '((name string :required "Name")
				(count integer :optional "Count")))
		       (values '("hello" 42)))
		   (should (equal (mevedel-pipeline--positional-to-plist values specs)
				  '(:name "hello" :count 42))))
		 :doc "handles empty args"
		 (should (null (mevedel-pipeline--positional-to-plist nil nil)))
		 :doc "handles fewer values than specs"
		 (let ((specs '((a string :required "A")
				(b string :required "B")))
		       (values '("only-one")))
		   (should (equal (mevedel-pipeline--positional-to-plist values specs)
				  '(:a "only-one")))))


;;
;;; Pipeline wrapper via mevedel-define-tool

(mevedel-deftest mevedel-pipeline--define-tool-wrapper ()
		 ,test
		 (test)
		 :doc "generated wrapper runs pipeline"
		 (progn
		   (let ((mevedel-permission-rules '(("TestEcho" :action allow)))
			 (mevedel-protected-paths nil)
			 (mevedel-permission-mode 'default)
			 result)
		     (mevedel-define-tool
		      :name "TestEcho"
		      :description "Echo test"
		      :handler (lambda (args) (plist-get args :msg))
		      :args ((msg string :required "Message"))
		      :read-only-p t
		      :async-p nil)
		     ;; Call the gptel-tool wrapper directly
		     (let* ((mtool (mevedel-tool-get "TestEcho"))
			    (gt (mevedel-tool-gptel-tool mtool))
			    (fn (gptel-tool-function gt)))
		       (funcall fn (lambda (r) (setq result r)) "hello"))
		     (should (equal result "hello"))
		     ;; Clean up
		     (remhash '("mevedel" "TestEcho") mevedel-tool--registry)))
		 :doc "wrapper validates args before calling handler"
		 (progn
		   (let ((mevedel-permission-rules '(("TestStrict" :action allow)))
			 (mevedel-protected-paths nil)
			 (mevedel-permission-mode 'default)
			 result)
		     (mevedel-define-tool
		      :name "TestStrict"
		      :description "Strict test"
		      :handler (lambda (args) (plist-get args :name))
		      :args ((name string :required "Name"))
		      :read-only-p t
		      :async-p nil)
		     (let* ((mtool (mevedel-tool-get "TestStrict"))
			    (gt (mevedel-tool-gptel-tool mtool))
			    (fn (gptel-tool-function gt)))
		       ;; Call with nil (missing required arg)
		       (funcall fn (lambda (r) (setq result r))))
		     (should (string-prefix-p "Error:" result))
		     (remhash '("mevedel" "TestStrict") mevedel-tool--registry))))

;;
;;; Result persistence

(mevedel-deftest mevedel-pipeline--persist-result ()
		 ,test
		 (test)
		 :doc "writes full result to file and returns preview"
		 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel" "sessions" "main")))
			(session (mevedel-session--create
				  :name "main" :workspace ws :save-path save-path))
			(tool (mevedel-tool--create :name "TestTool" :max-result-size 100))
			(result (make-string 500 ?x))
			(persisted (mevedel-pipeline--persist-result result tool session)))
		   (unwind-protect
		       (progn
			 ;; Preview should contain the XML wrapper
			 (should (string-prefix-p "<persisted-output>" persisted))
			 (should (string-suffix-p "</persisted-output>" persisted))
			 ;; Preview should mention the size
			 (should (string-match-p "500 chars" persisted))
			 ;; The persisted file should exist and contain the full result
			 (let ((files (directory-files
				       (file-name-concat save-path "tool-results")
				       t "\\.txt$")))
			   (should (= 1 (length files)))
			   (should (equal result
					  (with-temp-buffer
					    (insert-file-contents (car files))
					    (buffer-string))))))
		     (delete-directory tmpdir t)))
		 :doc "preview truncates to preview-size chars"
		 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel" "sessions" "main")))
			(session (mevedel-session--create
				  :name "main" :workspace ws :save-path save-path))
			(tool (mevedel-tool--create :name "TestTool" :max-result-size 100))
			;; Result with clear line breaks for newline-boundary cutting
			(result (mapconcat (lambda (_) (make-string 79 ?a))
					   (number-sequence 1 100) "\n"))
			(persisted (mevedel-pipeline--persist-result result tool session)))
		   (unwind-protect
		       (progn
			 ;; Preview should be much smaller than the full result
			 (should (< (length persisted) (length result)))
			 ;; Should contain the "..." truncation marker
			 (should (string-match-p "\\.\\.\\." persisted)))
		     (delete-directory tmpdir t)))
		 :doc "materializes session directory when save path is absent"
		 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create :root tmpdir))
			(session (mevedel-session--create :name "main" :workspace ws))
			(tool (mevedel-tool--create :name "TestTool" :max-result-size 100))
			(result (make-string 500 ?x))
			persisted)
		   (unwind-protect
		       (with-temp-buffer
			 (let ((mevedel-session-persistence t))
			   (setq persisted
				 (mevedel-pipeline--persist-result
				  result tool session (current-buffer))))
			 (should (string-prefix-p "<persisted-output>" persisted))
			 (should (mevedel-session-save-path session))
			 (should (directory-files
				  (file-name-concat
				   (mevedel-session-save-path session) "tool-results")
				  nil "\\.txt$")))
		     (delete-directory tmpdir t))))

(mevedel-deftest mevedel-pipeline--truncate-result ()
		 ,test
		 (test)
		 :doc "truncates large result and mentions no session persistence directory"
		 (let* ((tool (mevedel-tool--create :name "BigTool" :max-result-size 100))
			(result (make-string 5000 ?x))
			(truncated (mevedel-pipeline--truncate-result result tool)))
		   (should (< (length truncated) (length result)))
		   (should (string-match-p "no session persistence directory available"
					   truncated))
		   (should (string-match-p "5000 chars" truncated))
		   (should (string-match-p "BigTool" truncated))))

(mevedel-deftest mevedel-pipeline--step-persist ()
		 ,test
		 (test)
		 :doc "passes through when max-result-size is nil"
		 (let* ((tool (mevedel-tool--create :name "NoLimit" :max-result-size nil))
			(ctx (list :tool tool :result (make-string 100000 ?x)))
			next-ctx)
		   (mevedel-pipeline--step-persist
		    ctx (lambda (c) (setq next-ctx c)) #'ignore)
		   (should (equal (plist-get next-ctx :result)
				  (plist-get ctx :result))))
		 :doc "passes through when result is within limit"
		 (let* ((tool (mevedel-tool--create :name "SmallResult" :max-result-size 1000))
			(ctx (list :tool tool :result "short"))
			next-ctx)
		   (mevedel-pipeline--step-persist
		    ctx (lambda (c) (setq next-ctx c)) #'ignore)
		   (should (equal "short" (plist-get next-ctx :result))))
		 :doc "passes through when result is an error"
		 (let* ((tool (mevedel-tool--create :name "ErrTool" :max-result-size 10))
			(ctx (list :tool tool :result "Error: something broke with a lot of text"))
			next-ctx)
		   (mevedel-pipeline--step-persist
		    ctx (lambda (c) (setq next-ctx c)) #'ignore)
		   (should (string-prefix-p "Error:" (plist-get next-ctx :result))))
		 :doc "persists result when over limit"
		 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel" "sessions" "main")))
			(session (mevedel-session--create
				  :name "main" :workspace ws :save-path save-path))
			(tool (mevedel-tool--create :name "BigResult" :max-result-size 100))
			(big-result (make-string 500 ?y))
			(ctx (list :tool tool :result big-result :session session))
			next-ctx)
		   (unwind-protect
		       (progn
			 (mevedel-pipeline--step-persist
			  ctx (lambda (c) (setq next-ctx c)) #'ignore)
			 (should (string-prefix-p "<persisted-output>"
						  (plist-get next-ctx :result)))
			 ;; File should exist on disk
			 (should (directory-files
				  (file-name-concat save-path "tool-results")
				  nil "\\.txt$")))
		     (delete-directory tmpdir t)))
		 :doc "uses global cap when tool limit exceeds it"
		 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel" "sessions" "main")))
			(session (mevedel-session--create
				  :name "main" :workspace ws :save-path save-path))
			;; Tool declares 100000 but global cap is 50000
			(tool (mevedel-tool--create :name "HighLimit" :max-result-size 100000))
			;; Result is 60000 chars: above the 50K global cap but below the
			;; tool's declared 100K
			(big-result (make-string 60000 ?z))
			(ctx (list :tool tool :result big-result :session session))
			next-ctx)
		   (unwind-protect
		       (progn
			 (mevedel-pipeline--step-persist
			  ctx (lambda (c) (setq next-ctx c)) #'ignore)
			 ;; Should be persisted because 60K > 50K global cap
			 (should (string-prefix-p "<persisted-output>"
						  (plist-get next-ctx :result))))
		     (delete-directory tmpdir t)))
		 :doc "truncates when no session is in context"
		 (let* ((tool (mevedel-tool--create :name "NoWS" :max-result-size 10))
			(ctx (list :tool tool :result (make-string 5000 ?w)))
			next-ctx)
		   (mevedel-pipeline--step-persist
		    ctx (lambda (c) (setq next-ctx c)) #'ignore)
		   ;; Should truncate to preview size (no session directory to persist to)
		   (should (< (length (plist-get next-ctx :result)) 5000))
		   (should (string-match-p "no session persistence directory available"
					   (plist-get next-ctx :result))))
		 :doc "ignores buffer-local session — only reads session from context"
		 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel" "sessions" "main")))
			(mevedel--session (mevedel-session--create
					   :name "main"
					   :workspace ws
					   :save-path save-path))
			(tool (mevedel-tool--create :name "Orphan" :max-result-size 10))
			(ctx (list :tool tool :result (make-string 5000 ?q)))
			next-ctx)
		   (unwind-protect
		       (progn
			 ;; Even though mevedel--session is dynamically bound here with
			 ;; a valid save path, step-persist must NOT fall back to
			 ;; reading it — context is the sole source of truth.
			 (mevedel-pipeline--step-persist
			  ctx (lambda (c) (setq next-ctx c)) #'ignore)
			 (should (string-match-p
				  "no session persistence directory available"
				  (plist-get next-ctx :result))))
		     (delete-directory tmpdir t))))


;;
;;; Render-data handling

(mevedel-deftest mevedel-pipeline--render-plist-p ()
		 ,test
		 (test)
		 :doc "accepts plist with :result keyword"
		 (should (mevedel-pipeline--render-plist-p '(:result "ok")))
		 :doc "accepts plist with :result and :render-data"
		 (should (mevedel-pipeline--render-plist-p
			  '(:result "ok" :render-data (:kind diff))))
		 :doc "rejects bare string"
		 (should-not (mevedel-pipeline--render-plist-p "just a string"))
		 :doc "rejects nil"
		 (should-not (mevedel-pipeline--render-plist-p nil))
		 :doc "rejects list with non-keyword head"
		 (should-not (mevedel-pipeline--render-plist-p '("x" "y")))
		 :doc "rejects plist without :result"
		 (should-not (mevedel-pipeline--render-plist-p '(:other 1))))

(mevedel-deftest mevedel-pipeline--split-handler-return ()
		 ,test
		 (test)
		 :doc "splits a full (:result :render-data) plist"
		 (let ((split (mevedel-pipeline--split-handler-return
			       '(:result "done" :render-data (:kind diff :patch "p")))))
		   (should (equal "done" (car split)))
		   (should (equal '(:kind diff :patch "p") (cdr split))))
		 :doc "plist without :render-data yields nil render-data"
		 (let ((split (mevedel-pipeline--split-handler-return '(:result "done"))))
		   (should (equal "done" (car split)))
		   (should (null (cdr split))))
		 :doc "plain string becomes (RESULT . nil)"
		 (let ((split (mevedel-pipeline--split-handler-return "legacy")))
		   (should (equal "legacy" (car split)))
		   (should (null (cdr split)))))

(mevedel-deftest mevedel-pipeline--step-attach-render-data ()
		 ,test
		 (test)
		 :doc "no render-data: result passes through unchanged"
		 (let ((ctx (list :result "hello" :render-data nil))
		       out)
		   (mevedel-pipeline--step-attach-render-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (should (equal "hello" (plist-get out :result))))
		 :doc "with render-data: result gets a delimited hidden block appended"
		 (let ((ctx (list :result "hello" :render-data '(:kind diff :patch "p")))
		       out)
		   (mevedel-pipeline--step-attach-render-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (let ((r (plist-get out :result)))
		     (should (string-prefix-p "hello" r))
		     (should (string-search mevedel-pipeline--render-data-open r))
		     (should (string-search mevedel-pipeline--render-data-close r))))
		 :doc "embedded block carries invisible text property for data-buffer display"
		 (let ((ctx (list :result "x" :render-data '(:kind diff)))
		       out)
		   (mevedel-pipeline--step-attach-render-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (let* ((r (plist-get out :result))
			  (marker (string-search mevedel-pipeline--render-data-open r)))
		     (should marker)
		     ;; gptel 'ignore is not set: gptel's buffer parser extracts tool
		     ;; results via buffer-substring-no-properties, which drops text
		     ;; properties -- the block reaches the LLM via two strip hooks
		     ;; instead (see `mevedel-pipeline--format-render-data-block').
		     (should (eq t (get-text-property marker 'invisible r)))))
		 :doc "non-string result with render-data is passed through unchanged"
		 (let ((ctx (list :result nil :render-data '(:kind diff)))
		       out)
		   (mevedel-pipeline--step-attach-render-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (should (null (plist-get out :result)))))

(mevedel-deftest mevedel-pipeline-extract-render-data ()
		 ,test
		 (test)
		 :doc "round-trip: format then extract yields original payload"
		 (let* ((data '(:kind diff :patch "some patch" :path "/tmp/f"))
			(result (concat "visible body"
					(mevedel-pipeline--format-render-data-block data)))
			(extract (mevedel-pipeline-extract-render-data result)))
		   (should (equal "visible body" (car extract)))
		   (should (equal data (cdr extract))))
		 :doc "string with no delimiter returns (STRING . nil)"
		 (let ((extract (mevedel-pipeline-extract-render-data "just text")))
		   (should (equal "just text" (car extract)))
		   (should (null (cdr extract))))
		 :doc "open delimiter without close yields (ORIGINAL . nil)"
		 (let* ((s (concat "foo\n" mevedel-pipeline--render-data-open "\nunclosed"))
			(extract (mevedel-pipeline-extract-render-data s)))
		   (should (equal s (car extract)))
		   (should (null (cdr extract))))
		 :doc "unreadable payload treated as absent, visible part is original string"
		 (let* ((s (concat "foo"
				   "\n" mevedel-pipeline--render-data-open
				   "\n(:kind diff"
				   "\n" mevedel-pipeline--render-data-close "\n"))
			(extract (mevedel-pipeline-extract-render-data s)))
		   (should (equal s (car extract)))
		   (should (null (cdr extract))))
		 :doc "non-string input returns (INPUT . nil)"
		 (let ((extract (mevedel-pipeline-extract-render-data nil)))
		   (should (null (car extract)))
		   (should (null (cdr extract)))))

(mevedel-deftest mevedel-pipeline--step-handler/render-data ()
		 ,test
		 (test)
		 :doc "handler returning a plist stores :result and :render-data on context"
		 (let* ((tool (mevedel-tool--create
			       :name "PlistReturn"
			       :handler (lambda (_args)
					  (list :result "ok"
						:render-data '(:kind diff :patch "p")))))
			(ctx (list :tool tool :args nil))
			out)
		   (mevedel-pipeline--step-handler ctx (lambda (c) (setq out c)) #'ignore)
		   (should (equal "ok" (plist-get out :result)))
		   (should (equal '(:kind diff :patch "p") (plist-get out :render-data))))
		 :doc "handler returning a bare string leaves render-data nil"
		 (let* ((tool (mevedel-tool--create
			       :name "StringReturn"
			       :handler (lambda (_args) "legacy")))
			(ctx (list :tool tool :args nil))
			out)
		   (mevedel-pipeline--step-handler ctx (lambda (c) (setq out c)) #'ignore)
		   (should (equal "legacy" (plist-get out :result)))
		   (should (null (plist-get out :render-data)))))


;;
;;; Render-data strip hooks

(mevedel-deftest mevedel-pipeline--strip-render-data-blocks ()
		 ,test
		 (test)
		 :doc "strips a single embedded block, leaving the prefix intact"
		 (let* ((block (mevedel-pipeline--format-render-data-block
				'(:kind diff :patch "p")))
			(raw (concat "Changes applied to foo" block))
			(cleaned (mevedel-pipeline--strip-render-data-blocks raw)))
		   (should (string-match-p "Changes applied to foo" cleaned))
		   (should-not (string-match-p (regexp-quote mevedel-pipeline--render-data-open)
					       cleaned))
		   (should-not (string-match-p (regexp-quote mevedel-pipeline--render-data-close)
					       cleaned)))

		 :doc "strips multiple blocks in one pass"
		 (let* ((b1 (mevedel-pipeline--format-render-data-block '(:kind diff :patch "a")))
			(b2 (mevedel-pipeline--format-render-data-block '(:kind diff :patch "b")))
			(raw (concat "A" b1 "middle" b2 "Z"))
			(cleaned (mevedel-pipeline--strip-render-data-blocks raw)))
		   (should (string-match-p "A" cleaned))
		   (should (string-match-p "middle" cleaned))
		   (should (string-match-p "Z" cleaned))
		   (should-not (string-match-p (regexp-quote mevedel-pipeline--render-data-open)
					       cleaned)))

		 :doc "pass-through when no block is present"
		 (should (equal "Changes applied to bar"
				(mevedel-pipeline--strip-render-data-blocks
				 "Changes applied to bar"))))

(mevedel-deftest mevedel--parse-tool-results-scrub-advice ()
		 ,test
		 (test)
		 :doc "strips render-data from :result before ORIG-FUN, restores after"
		 (let* ((block (mevedel-pipeline--format-render-data-block
				'(:kind diff :patch "p")))
			(raw (concat "Changes applied to foo" block))
			(tc (list :name "Edit" :args nil :result raw))
			(seen-by-orig nil)
			(orig-fun (lambda (_backend tool-use)
				    (setq seen-by-orig (plist-get (car tool-use) :result))
				    'dummy))
			(ret (mevedel--parse-tool-results-scrub-advice
			      orig-fun 'dummy-backend (list tc))))
		   ;; ORIG-FUN saw a stripped :result
		   (should (stringp seen-by-orig))
		   (should-not (string-match-p (regexp-quote mevedel-pipeline--render-data-open)
					       seen-by-orig))
		   (should (string-match-p "Changes applied to foo" seen-by-orig))
		   ;; Return value of ORIG-FUN is passed through
		   (should (eq ret 'dummy))
		   ;; The tool-call plist's :result is restored to its original value so
		   ;; downstream consumers (callback, view parser, persistence) keep the
		   ;; block.
		   (should (equal raw (plist-get tc :result))))

		 :doc "pass-through when no tool-call carries a block"
		 (let* ((tc1 (list :name "Read" :args nil :result "clean 1"))
			(tc2 (list :name "Read" :args nil :result "clean 2"))
			(seen nil)
			(orig-fun (lambda (_b tool-use)
				    (setq seen (mapcar (lambda (x) (plist-get x :result))
						       tool-use))
				    'ok)))
		   (mevedel--parse-tool-results-scrub-advice
		    orig-fun 'dummy-backend (list tc1 tc2))
		   (should (equal seen '("clean 1" "clean 2")))
		   (should (equal (plist-get tc1 :result) "clean 1"))
		   (should (equal (plist-get tc2 :result) "clean 2")))

		 :doc "non-string :result is left untouched and handed to ORIG-FUN verbatim"
		 (let* ((tc (list :name "Edit" :args nil :result nil))
			(seen 'uninitialized)
			(orig-fun (lambda (_b tool-use)
				    (setq seen (plist-get (car tool-use) :result))
				    nil)))
		   (mevedel--parse-tool-results-scrub-advice
		    orig-fun 'dummy-backend (list tc))
		   (should (null seen))
		   (should (null (plist-get tc :result))))

		 :doc "restores :result even if ORIG-FUN errors"
		 (let* ((block (mevedel-pipeline--format-render-data-block
				'(:kind diff :patch "p")))
			(raw (concat "foo" block))
			(tc (list :name "Edit" :args nil :result raw)))
		   (should-error
		    (mevedel--parse-tool-results-scrub-advice
		     (lambda (&rest _) (error "boom"))
		     'dummy-backend (list tc)))
		   (should (equal raw (plist-get tc :result)))))

(mevedel-deftest mevedel-pipeline--patch-render-data-block ()
		 ,test
		 (test)
		 :doc "patch updates the block in place and round-trips through extract"
		 (let ((b1 (mevedel-pipeline--format-render-data-block
			    '(:kind agent-transcript :agent-id "a--1" :status running))))
		   (with-temp-buffer
		     (insert "leading text\n")
		     (insert b1)
		     (insert "trailing text\n")
		     (let ((bounds (mevedel-pipeline--find-render-data-block-by-agent-id
				    "a--1")))
		       (should bounds)
		       (mevedel-pipeline--patch-render-data-block
			(car bounds) (cdr bounds)
			'(:kind agent-transcript :agent-id "a--1" :status completed
				:elapsed 1.5)))
		     (let* ((bounds (mevedel-pipeline--find-render-data-block-by-agent-id
				     "a--1"))
			    (raw (buffer-substring-no-properties (car bounds) (cdr bounds)))
			    (parsed (mevedel-pipeline-extract-render-data raw))
			    (plist (cdr parsed)))
		       (should (equal (plist-get plist :status) 'completed))
		       (should (equal (plist-get plist :elapsed) 1.5)))))

		 :doc "patch propertizes the new block with the surrounding gptel property"
		 ;; Without this, the inserted block becomes a hole in the gptel
		 ;; property run that delimits the tool segment; the view buffer's
		 ;; `extract-segments' would then split the single tool segment in
		 ;; two and the LLM-invisible render-data block would render visibly
		 ;; in the user-facing tool body.
		 (let* ((b1 (mevedel-pipeline--format-render-data-block
			     '(:kind agent-transcript :agent-id "a--1" :status running))))
		   (with-temp-buffer
		     (let ((tool-prop '(tool . "tool-id-42")))
		       (insert (propertize "(:name \"Agent\" :args nil)\nlaunch text\n"
					   'gptel tool-prop))
		       (insert (propertize b1 'gptel tool-prop)))
		     (let ((bounds (mevedel-pipeline--find-render-data-block-by-agent-id
				    "a--1")))
		       (mevedel-pipeline--patch-render-data-block
			(car bounds) (cdr bounds)
			'(:kind agent-transcript :agent-id "a--1" :status completed)))
		     (let ((seen (cl-remove-duplicates
				  (let ((acc nil)
					(pos (point-min)))
				    (while (< pos (point-max))
				      (push (get-text-property pos 'gptel) acc)
				      (setq pos (or (next-single-property-change
						     pos 'gptel nil (point-max))
						    (point-max))))
				    acc)
				  :test #'equal)))
		       (should (equal seen '((tool . "tool-id-42")))))))

		 :doc "patch is a no-op on the surrounding text"
		 (let ((b1 (mevedel-pipeline--format-render-data-block
			    '(:kind agent-transcript :agent-id "a--1" :status running))))
		   (with-temp-buffer
		     (insert "before\n")
		     (insert b1)
		     (insert "after\n")
		     (let ((bounds (mevedel-pipeline--find-render-data-block-by-agent-id
				    "a--1")))
		       (mevedel-pipeline--patch-render-data-block
			(car bounds) (cdr bounds)
			'(:kind agent-transcript :agent-id "a--1" :status completed)))
		     (should (string-match-p "\\`before\n" (buffer-string)))
		     (should (string-match-p "after\n\\'" (buffer-string))))))

(provide 'test-mevedel-pipeline)
;;; test-mevedel-pipeline.el ends here
