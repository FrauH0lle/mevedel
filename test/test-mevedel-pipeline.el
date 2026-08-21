;;; test-mevedel-pipeline.el --- Tests for pipeline engine -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-agents)
(require 'mevedel-pipeline)
(require 'mevedel-sandbox)
(require 'mevedel-tool-media)
(require 'mevedel-tool-render-data)
(require 'mevedel-permissions)
(require 'mevedel-execution)
(require 'mevedel-execution-target)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-fs)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-patch)
(require 'mevedel-plan-mode)
(require 'mevedel-patch-review)
(require 'mevedel-tools)
(require 'mevedel-view)
(require 'mevedel-session-durability)
(require 'mevedel-session-persistence)
(require 'mevedel-permission-log)
(require 'mevedel-permission-rules)
(require 'mevedel-permission-prompt)
(require 'mevedel-permission-queue)
(require 'mevedel-telemetry)
;; gptel-request needed for mevedel-define-tool tests
(require 'gptel-request nil t)
(require 'gptel-anthropic nil t)
(require 'gptel-bedrock nil t)
(require 'gptel-openai nil t)
(require 'gptel-openai-responses nil t)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar gptel--known-tools)
(defvar mevedel-bash-dangerous-commands)
(defvar mevedel-tool--registry)
(defvar warning-minimum-level)

(defun test-mevedel-pipeline--format-media-data-block
    (media &optional session buffer tool-use-id)
  "Format MEDIA for tests using SESSION, BUFFER, and TOOL-USE-ID."
  (mevedel-tool-media--format-media-data-block
   media
   (and session
        (mevedel-pipeline-tool-results-dir session buffer))
   tool-use-id))

(defun test-mevedel-pipeline--extract-media-data
    (string &optional session buffer expected-tool-use-id)
  "Extract media from STRING for SESSION, BUFFER, and EXPECTED-TOOL-USE-ID."
  (mevedel-tool-media-extract
   string
   (and session
        (mevedel-pipeline-tool-results-dir session buffer))
   expected-tool-use-id))

(defun test-mevedel-pipeline--raw-bytes (&rest bytes)
  "Return BYTES as an Emacs string of raw byte characters."
  (apply #'string (mapcar #'unibyte-char-to-multibyte bytes)))

(defun test-mevedel-pipeline--run-tool-and-wait (tool args)
  "Run TOOL with ARGS and return its eventual pipeline result.
Running without a session in context is exactly what these cases mean to
cover, so the permission step's warning about it is captured here."
  (let ((deadline (+ (float-time) 5.0))
        done result)
    (mevedel-test--with-captured-diagnostics nil
      (mevedel-pipeline-run-tool
       tool
       (lambda (value)
         (setq result value
               done t))
       args)
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should done)
    result))


;;
;;; Pipeline runner

(mevedel-deftest mevedel-pipeline--format-context-failure
  ()
  ,test
  (test)

  :doc "retains accumulated audit records on failure results"
  (let* ((audit
          (mevedel-tool-repair-audit-record
           'committed
           '((:rule wrap-array-singleton :source generic
                   :paths ((names)) :before string :after array))))
         (result
          (mevedel-pipeline--format-context-failure
           (list :hook-audit-records (list audit)) "handler failed")))
    (should (string-prefix-p "Error: handler failed" result))
    (should (eq 'tool-input-repair
                (plist-get
                 (car (mevedel-test--hook-audit-records result))
                 :type))))

  :doc "audit formatting errors return the bare failure with a safe warning"
  (let (warning)
    (cl-letf (((symbol-function 'mevedel-pipeline--append-hook-audit-records)
               (lambda (&rest _) (error "private audit sentinel")))
              ((symbol-function 'display-warning)
               (lambda (_type message &rest _) (setq warning message))))
      (should
       (equal "Error: handler failed"
              (mevedel-pipeline--format-context-failure
               '(:hook-audit-records ((:bad t))) "handler failed"))))
    (should warning)
    (should-not (string-match-p "private\|sentinel" warning)))

  :doc "does not duplicate audit records already embedded in the reason"
  (let* ((audit
          (mevedel-tool-repair-audit-record
           'committed
           '((:rule wrap-array-singleton :source generic
                   :paths ((names)) :before string :after array))))
         (reason
          (mevedel-pipeline--append-hook-audit-records
           "Permission denied" (list audit)))
         (result
          (mevedel-pipeline--format-context-failure
           (list :hook-audit-records (list audit)) reason)))
    (should (= 1 (length
                  (mevedel-test--hook-audit-records result)))))

  :doc "appends context audit records missing from an audited reason"
  (let* ((embedded
          '(:type tool-permission :event "PreToolUse" :outcome "deny"))
         (context-only
          (mevedel-tool-repair-audit-record
           'committed
           '((:rule wrap-array-singleton :source generic
                   :paths ((names)) :before string :after array))))
         (reason
          (mevedel-pipeline--append-hook-audit-records
           "Permission denied" (list embedded)))
         (result
          (mevedel-pipeline--format-context-failure
           (list :hook-audit-records (list embedded context-only)) reason)))
    (should (equal (list embedded context-only)
                   (mevedel-test--hook-audit-records result)))))

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
		   (mevedel-test--with-captured-diagnostics nil
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
		   (mevedel-test--with-captured-diagnostics nil
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
;;; Path normalization step

(mevedel-deftest mevedel-pipeline--normalize-path-value ()
		 ,test
		 (test)
		 :doc "substitutes environment references before expanding"
		 (let ((process-environment
			(cons "MEVEDEL_TEST_ROOT=/tmp/probe" process-environment)))
		   (should (equal "/tmp/probe/x"
				  (mevedel-pipeline--normalize-path-value
				   "$MEVEDEL_TEST_ROOT/x"))))
		 :doc "expands a relative path against `default-directory'"
		 (let ((default-directory "/tmp/root/"))
		   (should (equal "/tmp/root/a/b"
				  (mevedel-pipeline--normalize-path-value "a/b"))))
		 :doc "keeps an unresolvable reference literal on both sides"
		 (let ((default-directory "/tmp/root/"))
		   (should (equal "/tmp/root/$UNSET_MEVEDEL_VAR/x"
				  (mevedel-pipeline--normalize-path-value
				   "$UNSET_MEVEDEL_VAR/x"))))
		 :doc "leaves non-string and empty values untouched"
		 (progn
		   (should (equal "" (mevedel-pipeline--normalize-path-value "")))
		   (should (null (mevedel-pipeline--normalize-path-value nil)))
		   (should (equal 7 (mevedel-pipeline--normalize-path-value 7))))
		 :doc "expands against the execution target environment"
		 (let ((target (mevedel-execution-target-create
			       "/ssh:user@host:/srv/project/")))
		   (setf (mevedel-execution-target-environment target)
			 '(("HOME" . "/home/user")
			   ("PROJECT_ROOT" . "/srv/project")))
		   (should
		    (equal "/ssh:user@host:/srv/project/lib/a.el"
			   (mevedel-pipeline--normalize-path-value
			    "$PROJECT_ROOT/lib/a.el" target "/srv/project/"))))
		 :doc "rejects unknown target environment variables"
		 (let ((target (mevedel-execution-target-create
			       "/docker:dev:/workspace/")))
		   (setf (mevedel-execution-target-environment target)
			 '(("HOME" . "/root")))
		   (should-error
		    (mevedel-pipeline--normalize-path-value
		     "$UNKNOWN_MEVEDEL_ROOT/a.el" target "/workspace/")
		    :type 'mevedel-execution-target-error)))

(mevedel-deftest mevedel-pipeline--canonicalize-path-value ()
		 ,test
		 (test)
		 :doc "resolves a symlink ancestor while preserving a nonexistent tail"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-canonical-root-" t)))
			(outside (file-name-as-directory
				  (make-temp-file "mevedel-canonical-outside-" t)))
			(link (file-name-concat root "linked"))
			(path (file-name-concat link "missing" "file.txt"))
			(expected
			 (file-name-concat outside "missing" "file.txt")))
		   (unwind-protect
		       (progn
			 (make-symbolic-link outside link)
			 (should
			  (equal expected
				 (mevedel-pipeline--canonicalize-path-value path))))
		     (delete-directory root t)
		     (delete-directory outside t))))

(mevedel-deftest mevedel-pipeline-canonical-path ()
		 ,test
		 (test)
		 :doc "returns only paths precomputed for the active handler"
		 (let ((mevedel-pipeline--canonical-path-map
			'(("/workspace/link" . "/outside/target"))))
		   (should
		    (equal "/outside/target"
			   (mevedel-pipeline-canonical-path "/workspace/link")))
		   (should
		    (equal "/workspace/other"
			   (mevedel-pipeline-canonical-path "/workspace/other")))))

(mevedel-deftest mevedel-pipeline--step-normalize-paths ()
		 ,test
		 (test)
			 :doc "canonicalizes `path' and ordinary `path-or-resource' args"
		 (let* ((default-directory "/tmp/root/")
			(tool (mevedel-tool--create
			       :name "Mixed"
			       :args '((file_path path :required "Path")
				       (label string :required "Label"))))
			(ctx (list :tool tool
				   :args '(:file_path "a/b" :label "c/d")))
			updated)
		   (mevedel-pipeline--step-normalize-paths
		    ctx (lambda (c) (setq updated c)) #'ignore)
		   (should (equal "/tmp/root/a/b"
				  (plist-get (plist-get updated :args) :file_path)))
		   (should (equal "c/d"
				  (plist-get (plist-get updated :args) :label))))
		 :doc "leaves authored resource addresses unchanged"
		 (let* ((default-directory "/tmp/root/")
			(tool (mevedel-tool--create
			       :name "Resource"
			       :args '((file_path path-or-resource :required "Path"))))
			(args '(:file_path "memory://root/topic.md"))
			(ctx (list :tool tool :args args))
			updated)
		   (mevedel-pipeline--step-normalize-paths
		    ctx (lambda (c) (setq updated c)) #'ignore)
		   (should (eq args (plist-get updated :args)))
		   (should (equal "memory://root/topic.md"
				  (plist-get (plist-get updated :args) :file_path))))
		 :doc "normalizes ordinary `path-or-resource' values before authorization"
		 (let* ((default-directory "/tmp/root/")
			(process-environment
			 (cons "MEVEDEL_TEST_ROOT=/tmp/probe" process-environment))
			(tool (mevedel-tool--create
			       :name "Resource"
			       :args '((file_path path-or-resource :required "Path"))))
			(ctx (list :tool tool :args '(:file_path "$MEVEDEL_TEST_ROOT/x")))
			updated)
		   (mevedel-pipeline--step-normalize-paths
		    ctx (lambda (c) (setq updated c)) #'ignore)
		   (should (equal "/tmp/probe/x"
				  (plist-get (plist-get updated :args) :file_path))))
			 :doc "leaves args untouched when nothing needs canonicalizing"
		 (let* ((tool (mevedel-tool--create
			       :name "Absolute"
			       :args '((file_path path :required "Path"))))
			(args '(:file_path "/tmp/already"))
			(ctx (list :tool tool :args args))
			updated)
		   (mevedel-pipeline--step-normalize-paths
		    ctx (lambda (c) (setq updated c)) #'ignore)
		   (should (eq args (plist-get updated :args))))
		 :doc "uses the session target before authorization"
		 (let ((root (file-name-as-directory
			     (make-temp-file "mevedel-normalize-remote-" t))))
		   (unwind-protect
		       (mevedel-test--with-local-shell-tramp '("normalize")
			 (let* ((remote-root
				 (format "/mevedelmock:normalize:%s" root))
				(target (mevedel-execution-target-create remote-root))
				(session (mevedel-session--create
					  :name "remote-path-normalization"
					  :execution-target target
					  :working-directory remote-root))
				(tool (mevedel-tool--create
				       :name "RemotePath"
				       :args '((file_path path :required "Path"))))
				(context (list :tool tool
					       :session session
					       :default-directory remote-root
					       :args '(:file_path "lib/a.el")))
				updated)
			   (mevedel-pipeline--step-normalize-paths
			    context (lambda (value) (setq updated value)) #'ignore)
			   (should
			    (equal (file-name-concat remote-root "lib/a.el")
				   (plist-get
				    (plist-get updated :args) :file_path)))))
		     (delete-directory root t)))
		 :doc "rejects explicit client paths before remote authorization"
		 (let* ((target (mevedel-execution-target-create
			 "/ssh:user@host:/srv/project/"))
		(session (mevedel-session--create
			  :name "remote-client-path"
			  :execution-target target
			  :working-directory "/ssh:user@host:/srv/project/"))
		(tool (mevedel-tool--create
		       :name "RemotePath"
		       :args '((file_path path :required "Path"))))
		(context (list :tool tool
			       :session session
			       :default-directory "/ssh:user@host:/srv/project/"
			       :args '(:file_path "/::/etc/passwd"))))
		   (should-error
		    (mevedel-pipeline--step-normalize-paths
		     context #'ignore #'ignore)
		    :type 'mevedel-execution-target-error))
		 :doc "rejects quoted local paths before authorization"
		 (let* ((target (mevedel-execution-target-create "/srv/project/"))
			(session (mevedel-session--create
			          :name "local-quoted-path"
			          :execution-target target
			          :working-directory "/srv/project/"))
			(tool (mevedel-tool--create
			       :name "LocalPath"
			       :args '((file_path path :required "Path"))))
			(context (list :tool tool
			               :session session
			               :default-directory "/srv/project/"
			               :args '(:file_path "/:/home/user/.ssh/id_rsa"))))
		   (should-error
		    (mevedel-pipeline--step-normalize-paths
		     context #'ignore #'ignore)
		    :type 'mevedel-execution-target-error))
		 :doc "authorization and handler observe one identical path"
		 (let* ((root (file-name-as-directory
				 (make-temp-file "mevedel-normalize-" t)))
			(workspace (mevedel-workspace--create
			            :type 'project :id root :root root))
			(session (mevedel-session--create
				  :name "path-normalization"
				  :workspace workspace
				  :working-directory root
				  :permission-mode 'full-auto))
			(mevedel--session session)
			(process-environment
			 (cons (concat "MEVEDEL_TEST_ROOT=" (directory-file-name root))
			       process-environment))
			(expected (file-name-concat root "secret"))
			(get-path-values nil)
			handler-value
			(tool (mevedel-tool--create
			       :name "PathEcho"
			       :category "mevedel"
			       :handler (lambda (args)
					  (setq handler-value
						(plist-get args :file_path))
					  '(:result "ok"))
			       :args '((file_path path :required "Path"))
			       :get-path (lambda (args)
					   (let ((path (plist-get args :file_path)))
					     (push path get-path-values)
					     path))
			       :read-only-p t
			       :async-p nil))
			result)
		   (mevedel-tool-register tool)
		   (unwind-protect
		       (with-temp-buffer
			 (mevedel-pipeline-run-tool
			  tool (lambda (value) (setq result value))
			  '(:file_path "$MEVEDEL_TEST_ROOT/secret"))
			 (should (equal "ok" result))
			 (should get-path-values)
			 ;; The decision chain must see the substituted target, not the
			 ;; literal "$MEVEDEL_TEST_ROOT/secret" the handler would resolve.
			 (should (seq-every-p (lambda (value) (equal expected value))
					      get-path-values))
			 (should (equal expected handler-value)))
		     (mevedel-tool-clear-registry)
		     (delete-directory root t))))

;;
;;; Handler step

(mevedel-deftest mevedel-pipeline-active-tool-use-id ()
		 ,test
		 (test)
		 :doc "returns the dynamically scoped handler identity"
		 (let ((mevedel-pipeline--active-tool-use-id "call-17"))
		   (should (equal "call-17"
				  (mevedel-pipeline-active-tool-use-id)))))

(mevedel-deftest mevedel-pipeline--step-handler ()
		 ,test
		 (test)
		 :doc "sync handler sets result in context"
		 (let* ((tool (mevedel-tool--create
			       :name "SyncTool"
			       :handler (lambda (args)
					  (list :result
						(format "got %s" (plist-get args :name))))
			       :async-p nil))
			(ctx (list :tool tool :args '(:name "test")))
			result-ctx)
		   (mevedel-pipeline--step-handler
		    ctx (lambda (c) (setq result-ctx c)) #'ignore)
		   (should (equal (plist-get result-ctx :result) "got test"))
		   (should (eq 'success
			       (plist-get result-ctx :handler-status))))
		 :doc "async handler calls continuation with result"
		 (let* ((tool (mevedel-tool--create
			       :name "AsyncTool"
			       :handler (lambda (callback args)
					  (funcall callback
						   (list :result
							 (format "async %s"
								 (plist-get args :val)))))
			       :async-p t))
			(ctx (list :tool tool :args '(:val "data")))
			result-ctx)
		   (mevedel-pipeline--step-handler
		    ctx (lambda (c) (setq result-ctx c)) #'ignore)
		   (should (equal (plist-get result-ctx :result) "async data")))
		 :doc "binds the durable tool-use id while an async handler starts"
		 (let* (seen
			(tool (mevedel-tool--create
			       :name "IdentifiedTool"
			       :handler
			       (lambda (callback _args)
				 (setq seen (mevedel-pipeline-active-tool-use-id))
				 (funcall callback '(:result "ok")))
			       :async-p t))
			(ctx (list :tool tool :args nil :tool-use-id "call-42")))
		   (mevedel-pipeline--step-handler ctx #'ignore #'ignore)
		   (should (equal "call-42" seen)))
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
		   (funcall saved-cb '(:result "deferred-result"))
		   (should (equal (plist-get result-ctx :result) "deferred-result")))
		 :doc "handler runs in the captured dispatch buffer"
		 (let* ((dispatch-buffer (generate-new-buffer " *handler-dispatch*"))
			(tool (mevedel-tool--create
			       :name "ContextTool"
			       :handler (lambda (_args)
					  (list :result default-directory))
			       :async-p nil))
			(ctx (list :tool tool :args nil :buffer dispatch-buffer))
			result-ctx)
		   (unwind-protect
		       (progn
			 (with-current-buffer dispatch-buffer
			   (setq-local default-directory "/captured-dispatch/"))
			 (cl-letf (((symbol-function 'mevedel-pipeline--record-use)
				    #'ignore))
			   (with-temp-buffer
			     (mevedel-pipeline--step-handler
			      ctx (lambda (value) (setq result-ctx value)) #'ignore)))
			 (should (equal (plist-get result-ctx :result)
					"/captured-dispatch/")))
		     (kill-buffer dispatch-buffer)))
		 :doc "handler plist stores render-data and leaves status absent"
		 (let* ((tool (mevedel-tool--create
			       :name "PlistReturn"
			       :handler (lambda (_args)
					  (list :result "ok"
						:render-data '(:kind diff :patch "p")))))
			(ctx (list :tool tool :args nil))
			out)
		   (mevedel-pipeline--step-handler ctx (lambda (c) (setq out c)) #'ignore)
		   (should (equal "ok" (plist-get out :result)))
		   (should (equal '(:kind diff :patch "p") (plist-get out :render-data)))
		   (should-not (plist-member out :status))
		   (should (eq 'success (plist-get out :handler-status))))
		 :doc "handler plist stores explicit status"
		 (let* ((tool (mevedel-tool--create
			       :name "StatusReturn"
			       :handler (lambda (_args)
					  (list :result "plain failure" :status 'error))))
			(ctx (list :tool tool :args nil))
			out)
		   (mevedel-pipeline--step-handler ctx (lambda (c) (setq out c)) #'ignore)
		   (should (eq 'error (plist-get out :status)))
		   (should (eq 'error (plist-get out :handler-status))))
		 :doc "legacy error text is normalized at the handler boundary"
		 (let* ((tool (mevedel-tool--create
			       :name "LegacyFailure"
			       :handler (lambda (_args)
					  (list :result "Error: failed"))))
			(ctx (list :tool tool :args nil))
			out)
		   (mevedel-pipeline--step-handler
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (should-not (plist-member out :status))
		   (should (eq 'error (plist-get out :handler-status))))
		 :doc "handler plist stores media"
		 (let* ((tool (mevedel-tool--create
			       :name "MediaReturn"
			       :handler (lambda (_args)
					  (list :result "ok"
						:media '((:path "/tmp/a.png"
							 :mime "image/png"))))))
			(ctx (list :tool tool :args nil))
			out)
		   (mevedel-pipeline--step-handler ctx (lambda (c) (setq out c)) #'ignore)
		   (should (equal "ok" (plist-get out :result)))
		   (should (equal '((:path "/tmp/a.png" :mime "image/png"))
				  (plist-get out :media))))
		 :doc "handler returning a bare string becomes a canonical handler failure"
		 (let* ((tool (mevedel-tool--create
			       :name "StringReturn"
			       :handler (lambda (_args) "invalid")))
			(ctx (list :tool tool :args nil))
			out
			failure)
		   (mevedel-pipeline--step-handler
		    ctx (lambda (next-context) (setq out next-context))
		    (lambda (reason) (setq failure reason)))
		   (should-not failure)
		   (should (eq 'error (plist-get out :status)))
		   (should (eq 'error (plist-get out :handler-status)))
		   (should (string-match-p "expected a plist containing :result"
					   (plist-get out :result))))
		 :doc "a signaling handler becomes a canonical handler failure"
		 (let* ((tool (mevedel-tool--create
			       :name "SignalingTool"
			       :handler (lambda (_args) (error "Handler exploded"))))
			(ctx (list :tool tool :args nil))
			out
			failure)
		   (mevedel-pipeline--step-handler
		    ctx (lambda (next-context) (setq out next-context))
		    (lambda (reason) (setq failure reason)))
		   (should-not failure)
		   (should (eq 'error (plist-get out :status)))
		   (should (eq 'error (plist-get out :handler-status)))
		   (should (equal "Error: Handler exploded" (plist-get out :result))))
		 :doc "normalizes raw byte result strings before callback"
		 (let* ((raw (string (unibyte-char-to-multibyte #x80)))
			(tool (mevedel-tool--create
			       :name "RawByteReturn"
			       :handler (lambda (_args) (list :result raw))))
			(ctx (list :tool tool :args nil))
			out)
		   (mevedel-pipeline--step-handler ctx (lambda (c) (setq out c)) #'ignore)
		   (should (equal "\\x80" (plist-get out :result)))
		   (should (equal "\\x80" (plist-get out :raw-result)))
		   (should (json-serialize (list :result (plist-get out :result))))))


;;
;;; Step list builder

(mevedel-deftest mevedel-pipeline--untracked-filesystem-effects-p
  ()
  ,test
  (test)
  :doc "classifies shell/eval and delegated mutation surfaces"
  (should
   (mevedel-pipeline--untracked-filesystem-effects-p
    (mevedel-tool--create :name "Bash" :groups '(eval))))
  (should
   (mevedel-pipeline--untracked-filesystem-effects-p
    (mevedel-tool--create :name "Agent" :groups '(util) :read-only-p t)))
  (should
   (mevedel-pipeline--untracked-filesystem-effects-p
    (mevedel-tool--create :name "MkDir" :groups '(edit))))
  (should-not
   (mevedel-pipeline--untracked-filesystem-effects-p
    (mevedel-tool--create :name "Edit" :groups '(edit) :snapshot-p t)))
  (should-not
   (mevedel-pipeline--untracked-filesystem-effects-p
    (mevedel-tool--create :name "Read" :groups '(read) :read-only-p t))))

(mevedel-deftest mevedel-pipeline--step-capture-coverage
  ()
  ,test
  (test)
  :doc "marks accepted directive tools whose filesystem effects are untracked"
  (let* ((request (mevedel-request--create :directive-uuid "directive"))
         (tool (mevedel-tool--create :name "Eval" :groups '(eval)))
         next-context)
    (mevedel-pipeline--step-capture-coverage
     (list :tool tool :request request)
     (lambda (context) (setq next-context context))
     #'ignore)
    (should next-context)
    (should (equal '("Eval")
                   (mapcar #'car
                           (mevedel-request-untracked-effects request))))))

(mevedel-deftest mevedel-pipeline--step-snapshot ()
  ,test
  (test)
  :doc "snapshots every path declared by one aggregate edit"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-multi-snapshot-" t)))
         (path-a (file-name-concat root "a.txt"))
         (path-b (file-name-concat root "b.txt"))
         (missing (file-name-concat root "new.txt"))
         (request (mevedel-request--create
                   :file-snapshots (make-hash-table :test #'equal)))
         (mevedel--current-request request)
         (tool (mevedel-tool--create
                :name "ApplyPatch" :snapshot-p t
                :get-paths (lambda (_args)
                             (list path-a path-b missing))))
         next-called)
    (unwind-protect
        (progn
          (with-temp-file path-a (insert "a"))
          (with-temp-file path-b (insert "b"))
          (mevedel-pipeline--step-snapshot
           (list :tool tool :args nil :request request)
           (lambda (_context) (setq next-called t)) #'ignore)
          (should next-called)
          (let ((snapshots (mevedel-request-file-snapshots request)))
            (should (= 3 (hash-table-count snapshots)))
            (should (equal "a" (gethash path-a snapshots)))
            (should (equal "b" (gethash path-b snapshots)))
            (let ((absent (make-symbol "absent")))
              (should-not (eq absent (gethash missing snapshots absent))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-pipeline--build-steps ()
		 ,test
		 (test)
		 :doc "read-only tool includes pre/post hook steps"
		 (let* ((tool (mevedel-tool--create
			       :name "ReadTool"
			       :read-only-p t))
			(steps (mevedel-pipeline--build-steps tool)))
		   (should (= (length steps) 14))
		   (should (eq (nth 0 steps) #'mevedel-pipeline--step-validate))
		   (should (eq (nth 1 steps) #'mevedel-pipeline--step-pre-tool-hooks))
		   (should (eq (nth 2 steps) #'mevedel-pipeline--step-normalize-paths))
		   (should (eq (nth 3 steps) #'mevedel-pipeline--step-prepare-resources))
                   (should (eq (nth 4 steps) #'mevedel-tool-permission-step))
		   (should (eq (nth 5 steps) #'mevedel-pipeline--step-capture-coverage))
		   (should (eq (nth 6 steps) #'mevedel-pipeline--step-handler))
		   (should (eq (nth 7 steps) #'mevedel-pipeline--step-repair-reminder))
		   (should (eq (nth 8 steps) #'mevedel-pipeline--step-render-transform))
		   (should (eq (nth 9 steps) #'mevedel-pipeline--step-specialist-nudges))
		   (should (eq (nth 10 steps) #'mevedel-pipeline--step-post-tool-hooks))
		   (should (eq (nth 11 steps) #'mevedel-pipeline--step-goal-budget-warning))
		   (should (eq (nth 12 steps) #'mevedel-pipeline--step-attach-render-data))
		   (should (eq (nth 13 steps) #'mevedel-pipeline--step-attach-media-data)))
		 :doc "write tool includes snapshot step"
		 (let* ((tool (mevedel-tool--create
			       :name "WriteTool"
			       :read-only-p nil
			       :snapshot-p t))
			(steps (mevedel-pipeline--build-steps tool)))
		   (should (= (length steps) 15))
		   (should (eq (nth 0 steps) #'mevedel-pipeline--step-validate))
		   (should (eq (nth 1 steps) #'mevedel-pipeline--step-pre-tool-hooks))
		   (should (eq (nth 2 steps) #'mevedel-pipeline--step-normalize-paths))
		   (should (eq (nth 3 steps) #'mevedel-pipeline--step-prepare-resources))
                   (should (eq (nth 4 steps) #'mevedel-tool-permission-step))
		   (should (eq (nth 5 steps) #'mevedel-pipeline--step-capture-coverage))
		   (should (eq (nth 6 steps) #'mevedel-pipeline--step-snapshot))
		   (should (eq (nth 7 steps) #'mevedel-pipeline--step-handler))
		   (should (eq (nth 8 steps) #'mevedel-pipeline--step-repair-reminder))
		   (should (eq (nth 9 steps) #'mevedel-pipeline--step-render-transform))
		   (should (eq (nth 10 steps) #'mevedel-pipeline--step-specialist-nudges))
		   (should (eq (nth 11 steps) #'mevedel-pipeline--step-post-tool-hooks))
		   (should (eq (nth 12 steps) #'mevedel-pipeline--step-goal-budget-warning))
		   (should (eq (nth 13 steps) #'mevedel-pipeline--step-attach-render-data))
		   (should (eq (nth 14 steps) #'mevedel-pipeline--step-attach-media-data)))
		 :doc "mutating tool without snapshot declaration skips snapshot step"
		 (let* ((tool (mevedel-tool--create
			       :name "MkDir"
			       :read-only-p nil))
			(steps (mevedel-pipeline--build-steps tool)))
		   (should (= (length steps) 14))
		   (should-not
		    (memq #'mevedel-pipeline--step-snapshot steps)))
		 :doc "includes persist step when max-result-size is set"
		 (let* ((tool (mevedel-tool--create
			       :name "WithPersist"
			       :read-only-p t
			       :max-result-size 1000))
			(steps (mevedel-pipeline--build-steps tool)))
		   (should (= 16 (length steps)))
		   (should (eq (nth 7 steps)
			       #'mevedel-pipeline--step-repair-reminder))
		   (should (eq (nth 8 steps)
			       #'mevedel-pipeline--step-render-transform))
		   (should (eq (nth 9 steps) #'mevedel-pipeline--step-persist))
		   (should (eq (nth 10 steps)
			       #'mevedel-pipeline--step-specialist-nudges))
		   (should (eq (nth 11 steps)
			       #'mevedel-pipeline--step-post-tool-hooks))
		   (should (eq (nth 12 steps) #'mevedel-pipeline--step-persist))
		   (should (eq (nth 13 steps)
			       #'mevedel-pipeline--step-goal-budget-warning))
		   (should (eq (nth 14 steps)
			       #'mevedel-pipeline--step-attach-render-data))
			   (should (eq (car (last steps))
			       #'mevedel-pipeline--step-attach-media-data)))
		 :doc "omits persist step when max-result-size is nil"
		 (let* ((tool (mevedel-tool--create
			       :name "NoPersist"
			       :read-only-p t
			       :max-result-size nil))
			(steps (mevedel-pipeline--build-steps tool)))
		   (should (= 14 (length steps)))
		   (should-not (memq #'mevedel-pipeline--step-persist steps))
		   (should (memq #'mevedel-pipeline--step-specialist-nudges steps))
		   (should (memq #'mevedel-pipeline--step-attach-render-data steps))
		   (should (memq #'mevedel-pipeline--step-attach-media-data steps))
		   (should (memq #'mevedel-pipeline--step-post-tool-hooks steps))))

(mevedel-deftest mevedel-pipeline--step-goal-budget-warning ()
  ,test (test)
  :doc "appends a model-visible budget warning after the final persisted result"
  (let (out)
    (cl-letf (((symbol-function 'mevedel-goal-tool-result-budget-warning)
               (lambda (_session _fsm) "Wrap up now.")))
      (mevedel-pipeline--step-goal-budget-warning
       '(:result "tool output" :session session :fsm fsm)
       (lambda (context) (setq out context)) #'ignore))
    (should (string-match-p
             (regexp-quote
              "tool output\n\n<system-reminder>\nWrap up now.")
             (plist-get out :result)))))


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
			provenance
			result)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (event payload callback &rest _)
				(pcase event
				  ('PreToolUse
				   (funcall callback
					    '(:permission-decision deny
								   :permission-reason "hook denied")))
				  ('PermissionDenied
				   (setq permission-denied-p t)
				   (setq provenance
					 (plist-get payload :permission-provenance))
				   (funcall callback
					    '(:permission-reason "rewritten denial")))))))
		     (mevedel-pipeline--step-pre-tool-hooks
		      context
		      (lambda (_ctx) (setq result "next"))
		      (lambda (reason) (setq result reason))))
		   (should permission-denied-p)
		   (should (eq 'PreToolUse provenance))
		   (should (equal (mevedel--strip-hook-audit-blocks
                                   result)
				  "rewritten denial"))
                   (let ((record (car (mevedel-test--hook-audit-records
                                       result))))
                     (should (eq (plist-get record :type) 'tool-permission))
                     (should (equal (plist-get record :event) "PreToolUse"))
                     (should (equal (plist-get record :outcome) "deny"))))
		 :doc "PreToolUse-denied Bash diagnostics use sanitized command summary"
			 (let* ((dir (file-name-as-directory
				      (make-temp-file "mevedel-permission-log-" t)))
				(session (mevedel-session--create
					  :name "test" :save-path dir))
				(tool (mevedel-tool--create
				       :name "Bash"
				       :get-pattern (lambda (args)
						      (plist-get args :command))))
				(context (list :tool tool
					       :args '(:command "printf SECRET_TOKEN")
					       :session session))
				(mevedel-permission-log-enabled t)
				failure)
			   (unwind-protect
			       (progn
				 (cl-letf (((symbol-function 'mevedel-hooks-run-event)
					    (lambda (_event _payload callback &rest _)
					      (funcall callback
						       '(:continue nil
							 :stop-reason "blocked")))))
				   (mevedel-pipeline--step-pre-tool-hooks
				    context #'ignore (lambda (reason) (setq failure reason))))
				 (should (equal
                                          (mevedel--strip-hook-audit-blocks
                                           failure)
                                          "blocked by PreToolUse: blocked"))
                                 (let ((record (car (mevedel-test--hook-audit-records
                                                     failure))))
                                   (should (eq (plist-get record :type)
                                               'tool-permission))
                                   (should (equal (plist-get record :event)
                                                  "PreToolUse"))
                                   (should (equal (plist-get record :outcome)
                                                  "deny")))
                                 (let ((entry (car (mevedel-test--permission-log-entries
						    session))))
				   (should (eq 'permission-decision
					       (plist-get entry :event)))
				   (should (eq 'deny (plist-get entry :outcome)))
				   (should (eq 'pre-tool-hook (plist-get entry :via)))
				   (should (equal "printf"
						  (plist-get entry :specifier-value)))
				   (should-not (equal "printf SECRET_TOKEN"
						      (plist-get entry :specifier-value)))))
			     (delete-directory dir t)))
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
		   (should failure))
		 :doc "audits valid updated input before continuing"
		 (let* ((tool (mevedel-tool--create
			       :name "Write"
			       :args '((file_path string :required "Path"))))
			(context (list :tool tool
				       :args '(:file_path "/tmp/old")
				       :default-directory default-directory))
			next-context
			failure)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (_event _payload callback &rest _)
				(funcall callback
					 '(:updated-input
					   (:file_path "/tmp/new")
					   :system-message "normalized")))))
		     (mevedel-pipeline--step-pre-tool-hooks
		      context
		      (lambda (ctx) (setq next-context ctx))
		      (lambda (reason) (setq failure reason))))
		   (should-not failure)
		   (should (equal '(:file_path "/tmp/new")
				  (plist-get next-context :args)))
		   (let ((record (car (plist-get next-context
						  :hook-audit-records))))
		     (should (eq (plist-get record :type)
				 'tool-input-rewrite))
		     (should (equal (plist-get record :event) "PreToolUse"))
		     (should (equal (plist-get record :original-input)
				    '(:file_path "/tmp/old")))
		     (should (equal (plist-get record :updated-input)
				    '(:file_path "/tmp/new")))
		     (should (equal (plist-get record :reason)
				    "normalized"))))
		 :doc "routes PreToolUse stop decisions through PermissionDenied once"
		 (let* ((tool (mevedel-tool--create :name "Write"))
			(context (list :tool tool :args nil))
			events
			provenance
			failure)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (event payload callback &rest _)
				(push event events)
				(when (eq event 'PermissionDenied)
				  (setq provenance
					(plist-get payload :permission-provenance)))
				(funcall callback
					 (and (eq event 'PreToolUse)
					      '(:continue nil :stop-reason "stopped"))))))
		     (mevedel-pipeline--step-pre-tool-hooks
		      context #'ignore (lambda (reason) (setq failure reason))))
		   (should (equal '(PreToolUse PermissionDenied) (nreverse events)))
		   (should (eq 'PreToolUse provenance))
		   (should (string-match-p "stopped" failure))))


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
		 :doc "uses explicit error status for the failure hook and payload"
		 (let* ((tool (mevedel-tool--create :name "Bash"))
			(context (list :tool tool
				       :args nil
				       :tool-use-id "toolu_1"
				       :result "plain failure"
				       :raw-result "plain failure"
				       :status 'error
				       :default-directory default-directory))
			seen-event
			seen-payload)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (event payload callback &rest _)
				(setq seen-event event seen-payload payload)
				(funcall callback nil))))
		     (mevedel-pipeline--step-post-tool-hooks context #'ignore #'ignore))
		   (should (eq 'PostToolUseFailure seen-event))
		   (should (equal "plain failure" (plist-get seen-payload :error))))
		 :doc "canonical handler status, not result text, selects the event"
		 (let* ((tool (mevedel-tool--create :name "Read"))
			(context (list :tool tool
				       :args nil
				       :result "Error: visible success"
				       :raw-result "Error: visible success"
				       :handler-status 'success
				       :default-directory default-directory))
			seen-event)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (event _payload callback &rest _)
				(setq seen-event event)
				(funcall callback nil))))
		     (mevedel-pipeline--step-post-tool-hooks context #'ignore #'ignore))
		   (should (eq 'PostToolUse seen-event)))
		 :doc "updated hook results retain explicit status for rendering"
		 (let* ((tool (mevedel-tool--create :name "Bash"))
			(context (list :tool tool
				       :args nil
				       :tool-use-id "toolu_1"
				       :result "plain failure"
				       :raw-result "plain failure"
				       :status 'error
				       :default-directory default-directory))
			final-result)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (_event _payload callback &rest _)
				(funcall callback '(:updated-result "redacted")))))
		     (mevedel-pipeline--step-post-tool-hooks
		      context
		      (lambda (updated)
			(mevedel-pipeline--step-attach-render-data
			 updated
			 (lambda (final)
			   (setq final-result (plist-get final :result)))
			 #'ignore))
		      #'ignore))
		   (let ((extracted
                          (mevedel-tool-render-data-extract
			   final-result nil "toolu_1")))
		     (should (string-prefix-p "redacted" (car extracted)))
		     (should (eq 'error (plist-get (cdr extracted) :status)))))
		 :doc "strips render-data from post-tool hook payload"
		 (let* ((tool (mevedel-tool--create :name "Read"))
			(result (concat "visible"
                                        (mevedel-tool-render-data-format
					 '(:kind diff) "toolu_1")))
			(context (list :tool tool
				       :args nil
				       :tool-use-id "toolu_1"
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
                      (string-search mevedel-tool-render-data-open
				     (plist-get seen-payload :result))))
		 :doc "summarizes media payloads before post-tool hooks"
		 (let* ((tool (mevedel-tool--create :name "Read"))
			(media '((:path "/tmp/a.png"
				  :mime "image/png"
				  :kind image
				  :data "QUJD")))
			(result (concat "<media-file>\n"
					"path: /tmp/a.png\n"
					"mime_type: image/png\n"
					"encoding: base64\n"
					"data:\n"
					"QUJD\n"
					"</media-file>"
					(test-mevedel-pipeline--format-media-data-block
					 media)))
			(context (list :tool tool
				       :args nil
				       :result result
				       :raw-result result
				       :media media
				       :default-directory default-directory))
			seen-payload)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (_event payload callback &rest _)
				(setq seen-payload payload)
				(funcall callback nil))))
		     (mevedel-pipeline--step-post-tool-hooks context #'ignore #'ignore))
		   (should (string-match-p "media omitted"
					   (plist-get seen-payload :tool-response)))
		   (should-not (string-search "QUJD"
					      (plist-get seen-payload
							 :tool-response)))
		   (should (string-match-p "media omitted"
					   (plist-get seen-payload :raw-result)))
		   (should-not (string-search "QUJD"
					      (plist-get seen-payload
							 :raw-result))))
		 :doc "preserves forged media envelope tails before post-tool hooks"
		 (let* ((tool (mevedel-tool--create :name "Read"))
			(result (concat "<media-file>\n"
					"path: /tmp/a.png\n"
					"mime_type: image/png\n"
					"encoding: base64\n"
					"data:\n"
					"QUJD\n"
					"</media-file>"))
			(context (list :tool tool
				       :args nil
				       :result result
				       :raw-result result
				       :default-directory default-directory))
			seen-payload)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (_event payload callback &rest _)
				(setq seen-payload payload)
				(funcall callback nil))))
		     (mevedel-pipeline--step-post-tool-hooks context #'ignore #'ignore))
		   (should (equal result (plist-get seen-payload :tool-response)))
		   (should (equal result (plist-get seen-payload :raw-result))))
		 :doc "preserves forged media tails when handler media is invalid"
		 (let* ((tool (mevedel-tool--create :name "Read"))
			(result "prefix data:\nSECRET\n</media-file> suffix")
			(context (list :tool tool
				       :args nil
				       :result result
				       :raw-result result
				       :media '((:kind image))
				       :default-directory default-directory))
			seen-payload)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (_event payload callback &rest _)
				(setq seen-payload payload)
				(funcall callback nil))))
		     (mevedel-pipeline--step-post-tool-hooks context #'ignore #'ignore))
		   (should (equal result (plist-get seen-payload :tool-response)))
		   (should (equal result (plist-get seen-payload :raw-result))))
                 :doc "post-tool additional context is event-tagged and audited on the result"
                 (let* ((tool (mevedel-tool--create :name "Read"))
                        (context (list :tool tool
                                       :args nil
                                       :result "visible"
                                       :default-directory default-directory))
                        result
                        (mevedel-post-tool-use-functions
                         (list (lambda (_event)
                                 '(:additional-context ("hook note")
                                   :system-message "because")))))
                   ;; The hook's system message is reported to the user;
                   ;; the audit assertions below own it.
                   (mevedel-test--with-captured-messages nil
                     (mevedel-pipeline--step-post-tool-hooks
                      context
                      (lambda (ctx) (setq result (plist-get ctx :result)))
                      #'ignore))
                   (should (string-match-p
                            "<hook-event name=\"PostToolUse\">"
                            result))
                   (let ((record (car (mevedel-test--hook-audit-records
                                       result)))
                         handler)
                     (should (eq (plist-get record :type) 'tool-context))
                     (should (equal (plist-get record :event) "PostToolUse"))
                     (setq handler (car (plist-get record :handlers)))
                     (should (equal (plist-get handler :contexts)
                                    '("hook note")))
                     (should (equal (plist-get handler :reason) "because"))))
			 :doc "updated hook result clears stale media before later attachment"
		 (let* ((tool (mevedel-tool--create :name "Read"))
			(media '((:path "/tmp/a.png"
				  :mime "image/png"
				  :kind image
				  :data "QUJD")))
			(context (list :tool tool
				       :args nil
				       :result "<media-file>\ndata:\nQUJD\n</media-file>"
				       :media media
				       :default-directory default-directory))
			after-hooks
			after-media)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (_event _payload callback &rest _)
				(funcall callback
					 '(:updated-result
					   "<media-file>\ndata:\nHOOK\n</media-file>")))))
		     (mevedel-pipeline--step-post-tool-hooks
		      context (lambda (ctx) (setq after-hooks ctx)) #'ignore))
		   (should (equal "<media-file>\ndata:\nHOOK\n</media-file>"
				  (mevedel--strip-hook-audit-blocks
                                   (plist-get after-hooks :result))))
                   (let ((record (car (mevedel-test--hook-audit-records
                                       (plist-get after-hooks :result)))))
                     (should (eq (plist-get record :type)
                                 'tool-result-rewrite))
                     (should (equal (plist-get record :event) "PostToolUse"))
                     (should (equal (plist-get record :updated-result)
                                    "<media-file>\ndata:\nHOOK\n</media-file>")))
		   (should-not (plist-get after-hooks :media))
		   (mevedel-pipeline--step-attach-media-data
		    after-hooks (lambda (ctx) (setq after-media ctx)) #'ignore)
			   (should-not (string-search mevedel-tool-media--data-open
					      (plist-get after-media :result))))
			 :doc "updated hook results retain committed repair audit metadata"
			 (let* ((tool (mevedel-tool--create :name "Collect"))
				(repair-audit
				 (mevedel-tool-repair-audit-record
				  'committed
				  '((:rule wrap-array-singleton :source generic
				          :paths ((names)) :before string :after array))))
				(context (list :tool tool :args nil :result "original"
					       :hook-audit-records (list repair-audit)
					       :default-directory default-directory))
				result)
			   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
				      (lambda (_event _payload callback &rest _)
					(funcall callback '(:updated-result "updated")))))
			     (mevedel-pipeline--step-post-tool-hooks
			      context
			      (lambda (ctx) (setq result (plist-get ctx :result)))
			      #'ignore))
			   (should (equal "updated"
					  (mevedel--strip-hook-audit-blocks result)))
			   (should
			    (cl-some
			     (lambda (record)
			       (eq 'tool-input-repair (plist-get record :type)))
                             (mevedel-test--hook-audit-records result)))))

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
;;; Specialist nudges

(mevedel-deftest mevedel-pipeline--step-specialist-nudges ()
  ,test
  (test)
  :doc "delegates generic context fields and forwards the policy result"
  (require 'mevedel-specialist-nudges)
  (let* ((tool (mevedel-tool--create :name "AnyTool"))
         (session (mevedel-session--create :name "main"))
         (invocation 'invocation)
         (context (list :tool tool :args '(:value 1) :result "original"
                        :session session :invocation invocation))
         seen
         out)
    (cl-letf (((symbol-function 'mevedel-specialist-nudges-apply)
               (lambda (actual-context)
                 (setq seen actual-context)
                 (plist-put actual-context :result "guided"))))
      (mevedel-pipeline--step-specialist-nudges
       context (lambda (next-context) (setq out next-context)) #'ignore))
    (should (eq context seen))
    (should (equal "guided" (plist-get out :result)))))


;;
;;; Full pipeline

(mevedel-deftest mevedel-pipeline-apply-patch-resources
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "mevedel-pipeline-patch-resource-" t)))
           (save-path (make-temp-file "mevedel-pipeline-patch-save-" t))
           (ordinary-path (file-name-concat root "ordinary.txt"))
           (local-path (file-name-concat save-path "local" "notes" "new.txt"))
           (workspace (mevedel-workspace--create
                       :type 'test :id root :root root :name "patch-resource"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session--create
                     :name "patch-resource" :workspace workspace
                     :save-path save-path :working-directory root
                     :permission-mode 'full-auto
                     :touched-files (make-hash-table :test #'equal)))
           (buffer (generate-new-buffer " *mevedel-pipeline-patch-resource*")))
   :before-each
   (with-temp-file ordinary-path (insert "old\n"))
   :after-each
   (progn
     (when (buffer-live-p buffer) (kill-buffer buffer))
     (when (file-directory-p root) (delete-directory root t))
     (when (file-directory-p save-path) (delete-directory save-path t))))
  ,test
  (test)
  :doc "Rejects malformed ApplyPatch resources before permission, handler, or post-use"
  (let ((permission-called nil)
        (handler-called nil)
        (post-called nil)
        result
        (tool (mevedel-tool--create
               :name "ApplyPatch"
               :handler (lambda (_args)
                          (setq handler-called t)
                          '(:result "unexpected"))
               :args '((patch string :required "Patch"))
               :read-only-p nil
               :async-p nil)))
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (event _payload callback &rest _)
                 (when (memq event '(PostToolUse PostToolUseFailure))
                   (setq post-called t))
                 (funcall callback nil)))
              ((symbol-function
                'mevedel-check-permission-async-with-metadata)
               (lambda (_name callback &rest _)
                 (setq permission-called t)
                 (funcall callback '(:outcome allow :raw-outcome allow)))))
      (with-current-buffer buffer
        (setq-local default-directory root
                    mevedel--workspace workspace
                    mevedel--session session)
        (mevedel-pipeline-run-tool
         tool (lambda (value) (setq result value))
         (list :patch (string-join
                       '("*** Begin Patch"
                         "*** Add File: local://notes/../bad.txt"
                         "+bad"
                         "*** End Patch")
                       "\n")))))
    (should (string-match-p "Error:" result))
    (should (string-match-p "local://notes/../bad.txt" result))
    (should-not permission-called)
    (should-not handler-called)
    (should-not post-called))

  :doc "Prepares one mixed proposal once and keeps ordinary paths in bookkeeping"
  (let* ((parsed-proposal nil)
         (prepared-context nil)
         (parse-count 0)
         (request (mevedel-request--create
                   :file-snapshots (make-hash-table :test #'equal)))
         (patch (string-join
                 '("*** Begin Patch"
                   "*** Update File: ordinary.txt"
                   "@@"
                   "-old"
                   "+new"
                   "*** Add File: local://notes/new.txt"
                   "+created"
                   "*** End Patch")
                 "\n"))
         (original-parse (symbol-function 'mevedel-tool-patch--parse))
         (tool (mevedel-tool--create
                :name "ApplyPatch"
                :handler (lambda (_args) '(:result "prepared"))
                :args '((patch string :required "Patch"))
                :read-only-p nil
                :async-p t
                :groups '(edit)
                :snapshot-p t
                :get-paths #'mevedel-tool-patch--get-paths)))
    (with-current-buffer buffer
      (setq-local default-directory root
                  mevedel--workspace workspace
                  mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-tool-patch--parse)
                 (lambda (&rest args)
                   (setq parse-count (1+ parse-count))
                   (setq parsed-proposal (apply original-parse args)))))
        (mevedel-pipeline--step-prepare-resources
         (list :tool tool :args (list :patch patch)
               :session session :workspace workspace :request request
               :buffer buffer :default-directory root)
         (lambda (context) (setq prepared-context context)) #'ert-fail)
        (should (eq 'apply-patch
                    (mevedel-pipeline--resource-operation tool)))
        (should (= 1 parse-count))
        (should (eq parsed-proposal
                    (plist-get prepared-context :patch-proposal)))
        (should (equal (list ordinary-path)
                       (mevedel-tool-permission-paths
                        tool (list :patch patch) prepared-context)))
        (should-not (plist-get (plist-get prepared-context :patch-proposal)
                               :local-only-p))
        (should (eq parsed-proposal
                    (plist-get prepared-context :patch-proposal)))))

  :doc "The actual ApplyPatch handler consumes the pipeline proposal once"
  (let* ((parse-count 0)
         (prepare-count 0)
         (prepared nil)
         (materialized nil)
         (result nil)
         (patch (string-join
                 '("*** Begin Patch"
                   "*** Update File: ordinary.txt"
                   "@@"
                   "-old"
                   "+new"
                   "*** Add File: local://notes/new.txt"
                   "+created"
                   "*** End Patch")
                 "\n"))
         (tool (mevedel-tool-ensure "ApplyPatch"))
         (original-parse (symbol-function 'mevedel-tool-patch--parse))
         (original-prepare
          (symbol-function 'mevedel-tool-patch--prepare-resources))
         (original-materialize
          (symbol-function 'mevedel-tool-patch--materialize-resources)))
    (cl-letf (((symbol-function 'mevedel-tool-patch--parse)
               (lambda (&rest args)
                 (setq parse-count (1+ parse-count))
                 (apply original-parse args)))
              ((symbol-function 'mevedel-tool-patch--prepare-resources)
               (lambda (proposal &optional materialize)
                 (setq prepare-count (1+ prepare-count)
                       prepared proposal)
                 (funcall original-prepare proposal materialize)))
              ((symbol-function 'mevedel-tool-patch--materialize-resources)
               (lambda (proposal)
                 (setq materialized proposal)
                 (funcall original-materialize proposal))))
      (should (eq 'apply-patch (mevedel-pipeline--resource-operation tool)))
      (with-current-buffer buffer
        (setq-local default-directory root
                    mevedel--workspace workspace
                    mevedel--session session)
        (mevedel-pipeline-run-tool
         tool (lambda (value) (setq result value)) (list :patch patch))))
    (should (= 1 parse-count))
    (should (= 1 prepare-count))
    (should (eq prepared materialized))
    (should (string-match-p "Applied patch" result))
    (should (equal "new\n"
                   (with-temp-buffer
                     (insert-file-contents ordinary-path)
                     (buffer-string))))
    (should (equal "created\n"
                   (with-temp-buffer
                     (insert-file-contents local-path)
                     (buffer-string))))))

  :doc "A malformed mixed proposal leaves both ordinary and local targets unchanged"
  (let ((permission-called nil)
        (handler-called nil)
        result
        (tool (mevedel-tool--create
               :name "ApplyPatch"
               :handler (lambda (callback args)
                          (setq handler-called t)
                          (mevedel-tool-patch-handler callback args))
               :args '((patch string :required "Patch"))
               :read-only-p nil
               :async-p t
               :groups '(edit)
               :snapshot-p t
               :get-paths #'mevedel-tool-patch--get-paths)))
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (_event _payload callback &rest _)
                 (funcall callback nil)))
              ((symbol-function
                'mevedel-check-permission-async-with-metadata)
               (lambda (_name callback &rest _)
                 (setq permission-called t)
                 (funcall callback '(:outcome allow :raw-outcome allow)))))
      (with-current-buffer buffer
        (setq-local default-directory root
                    mevedel--workspace workspace
                    mevedel--session session)
        (mevedel-pipeline-run-tool
         tool (lambda (value) (setq result value))
         (list :patch (string-join
                       '("*** Begin Patch"
                         "*** Update File: ordinary.txt"
                         "@@"
                         "-does-not-match"
                         "+should-not-apply"
                         "*** Add File: local://notes/new.txt"
                         "+bad"
                         "*** End Patch")
                       "\n")))))
    (should (string-match-p "Error:" result))
    (should permission-called)
    (should-not (file-exists-p local-path))
    (should (equal "old\n"
                   (with-temp-buffer
                     (insert-file-contents ordinary-path)
                   (buffer-string)))))

  :doc "Denied local proposals do not materialize session state"
  (let ((permission-called nil)
        result
        (tool (mevedel-tool--create
               :name "ApplyPatch"
               :handler (lambda (_callback _args)
                          '(:result "unexpected"))
               :args '((patch string :required "Patch"))
               :read-only-p nil
               :async-p t
               :groups '(edit)
               :snapshot-p t
               :get-paths #'mevedel-tool-patch--get-paths)))
    (setf (mevedel-session-save-path session) nil)
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (_event _payload callback &rest _)
                 (funcall callback nil)))
              ((symbol-function
                'mevedel-check-permission-async-with-metadata)
               (lambda (_name callback &rest _)
                 (setq permission-called t)
                 (funcall callback '(:outcome deny :raw-outcome deny)))))
      (with-current-buffer buffer
        (setq-local default-directory root
                    mevedel--workspace workspace
                    mevedel--session session)
        (mevedel-pipeline-run-tool
         tool (lambda (value) (setq result value))
         (list :patch (string-join
                       '("*** Begin Patch"
                         "*** Add File: local://notes/denied.txt"
                         "+denied"
                         "*** End Patch")
                       "\n")))))
    (should permission-called)
    (should (string-match-p "Error:" result))
    (should-not (mevedel-session-save-path session))
    (should-not (file-exists-p (file-name-concat root "local" "notes"
                                                 "denied.txt"))))

  :doc "Plan permits and applies an all-local add update delete and move"
  (let* ((tool (mevedel-tool-ensure "ApplyPatch"))
         (local-root (file-name-concat save-path "local"))
         (update-path (file-name-concat local-root "notes" "update.txt"))
         (delete-path (file-name-concat local-root "notes" "delete.txt"))
         (move-path (file-name-concat local-root "notes" "move.txt"))
         (moved-path (file-name-concat local-root "notes" "moved.txt"))
         (add-path (file-name-concat local-root "notes" "add.txt"))
         result)
    (make-directory (file-name-directory update-path) t)
    (with-temp-file update-path (insert "old\n"))
    (with-temp-file delete-path (insert "delete\n"))
    (with-temp-file move-path (insert "move\n"))
    (setf (mevedel-session-plan-mode session) t)
    (with-current-buffer buffer
      (setq-local default-directory root
                  mevedel--workspace workspace
                  mevedel--session session)
      (mevedel-pipeline-run-tool
       tool (lambda (value) (setq result value))
       (list :patch
             (string-join
              (list "*** Begin Patch"
                    "*** Update File: local://notes/update.txt"
                    "@@"
                    "-old"
                    "+new"
                    "*** Add File: local://notes/add.txt"
                    "+added"
                    "*** Delete File: local://notes/delete.txt"
                    "*** Update File: local://notes/move.txt"
                    "*** Move to: local://notes/moved.txt"
                    "*** End Patch")
              "\n"))))
    (should (string-match-p "Applied patch" result))
    (should (equal "new\n"
                   (with-temp-buffer
                     (insert-file-contents update-path)
                     (buffer-string))))
    (should (equal "added\n"
                   (with-temp-buffer
                     (insert-file-contents add-path)
                     (buffer-string))))
    (should-not (file-exists-p delete-path))
    (should-not (file-exists-p move-path))
    (should (equal "move\n"
                   (with-temp-buffer
                     (insert-file-contents moved-path)
                     (buffer-string))))
    (should (= 0 (hash-table-count (mevedel-session-touched-files session)))))

  :doc "Plan denies ordinary mixed malformed and non-local proposals before materialization"
  (let* ((tool (mevedel-tool-ensure "ApplyPatch"))
         (patches
          (list
           (string-join
            '("*** Begin Patch" "*** Update File: ordinary.txt" "@@"
              "-old" "+ordinary" "*** End Patch") "\n")
           (string-join
            '("*** Begin Patch" "*** Update File: ordinary.txt" "@@"
              "-old" "+mixed" "*** Add File: local://notes/mixed.txt"
              "+mixed" "*** End Patch") "\n")
           (string-join
            '("*** Begin Patch" "*** Add File: local://notes/../bad.txt"
              "+bad" "*** End Patch") "\n")
           (string-join
            '("*** Begin Patch"
              "*** Update File: local://notes/bare-source.txt"
              "*** Move to: local://" "*** End Patch") "\n")
           (string-join
            '("*** Begin Patch" "*** Add File: artifact://notes/bad.txt"
              "+bad" "*** End Patch") "\n")))
         result)
    (setf (mevedel-session-plan-mode session) t
          (mevedel-session-save-path session) nil)
    (dolist (patch patches)
      (setq result nil)
      (with-current-buffer buffer
        (setq-local default-directory root
                    mevedel--workspace workspace
                    mevedel--session session)
        (mevedel-pipeline-run-tool
         tool (lambda (value) (setq result value)) (list :patch patch)))
      (should (string-match-p "Error:" result))
      (should (equal "old\n"
                     (with-temp-buffer
                       (insert-file-contents ordinary-path)
                       (buffer-string))))
      (should-not (file-exists-p (file-name-concat save-path "local")))
      (should-not (file-exists-p (file-name-concat save-path "tool-results"))))
    (should-not (mevedel-session-save-path session))
    (should (= 0 (hash-table-count (mevedel-session-touched-files session)))))

  )

(mevedel-deftest mevedel-pipeline-run-tool
  (:before-each
   (advice-add 'display-warning :around
               #'mevedel-test--drop-sessionless-permission-warning)
   :after-each
   (advice-remove 'display-warning
                  #'mevedel-test--drop-sessionless-permission-warning))
  ,test
  (test)

  :doc "sync tool runs through pipeline"
  (let* ((tool (mevedel-tool--create
                :name "Echo"
                :handler (lambda (args)
                           (list :result (plist-get args :msg)))
                :args '((msg string :required "Message"))
                :read-only-p t
                :async-p nil))
         result)
    (mevedel-pipeline-run-tool
     tool (lambda (value) (setq result value)) '(:msg "hello"))
    (should (equal result "hello")))

  :doc "unknown remote outcome blocks mutation but permits read-only tools"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-unknown-" t)))
         (remote-root (format "/mevedelmock:unknown:%s" root))
         (workspace
          (mevedel-workspace--create
           :type 'project :id remote-root :root remote-root
           :name "unknown-remote"
           :file-cache (mevedel-test-file-cache-create)))
         session
         mutation-permission-called
         mutation-handler-called
         read-permission-called
         read-handler-called
         (mutating-tool
          (mevedel-tool--create
           :name "WriteThing" :read-only-p nil
           :check-permission-async
           (lambda (_tool _input cont)
             (setq mutation-permission-called t)
             (funcall cont 'allow))
           :handler
           (lambda (_args)
             (setq mutation-handler-called t)
             (list :result "mutated"))))
         (read-tool
          (mevedel-tool--create
           :name "Read" :read-only-p t
           :check-permission-async
           (lambda (_tool _input cont)
             (setq read-permission-called t)
             (funcall cont 'allow))
           :handler
           (lambda (_args)
             (setq read-handler-called t)
             (list :result "read"))))
         mutation-result read-result)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("unknown")
          (setq session
                (mevedel-session-create
                 "unknown-remote" workspace remote-root))
          (puthash
           (mevedel-execution-target-identity
            (mevedel-session-execution-target session))
           t mevedel-session-durability--disclosed-targets)
          (with-temp-buffer
            (setq-local default-directory remote-root)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel--session session)
            (mevedel-session-artifacts-assert-mutation-authority
             session (current-buffer))
            (should
             (mevedel-session-durability-set-unsettled-mutation
              session t))
            (setq mutation-result
                  (test-mevedel-pipeline--run-tool-and-wait
                   mutating-tool nil))
            (setq read-result
                  (test-mevedel-pipeline--run-tool-and-wait
                   read-tool nil)))
          (should (string-match-p
                   "unknown remote outcome" mutation-result))
          (should-not mutation-permission-called)
          (should-not mutation-handler-called)
          (should read-permission-called)
          (should read-handler-called)
          (should (equal read-result "read")))
      (when session
        (mevedel-execution-teardown-session session))
      (delete-directory root t)))
  :doc "remote project hook rewrites cannot cross execution targets"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-hook-root-" t)))
         (foreign-root (file-name-as-directory
                        (make-temp-file "mevedel-pipeline-hook-foreign-" t)))
         (remote-root (format "/mevedelmock:%s:%s"
                              (system-name) root))
         (foreign-path (format "/mevedelmock:foreign:%sfile.txt"
                               foreign-root))
         (workspace
          (mevedel-workspace--create
           :type 'project :id remote-root :root remote-root
           :name "remote-hook-boundary"
           :file-cache (mevedel-test-file-cache-create)))
         session
         (tool
          (mevedel-tool--create
           :name "Read" :args '((file_path path :required "Path"))
           :groups '(read) :read-only-p t
           :get-path (lambda (args) (plist-get args :file_path))
           :check-permission-async
           (lambda (&rest _)
             (ert-fail "Cross-target hook rewrite reached permission"))
           :handler (lambda (_args)
                      (ert-fail "Cross-target hook rewrite reached handler"))))
         (mevedel-hooks-persist-log nil)
         (mevedel-permission-mode 'ask)
         (mevedel-protected-paths nil)
         json result)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("foreign")
          (setq session
                (mevedel-session-create
                 "remote-hook-boundary" workspace remote-root))
          (setq json
                (format "{\"updatedInput\":{\"file_path\":%s}}"
                        (json-encode-string foreign-path)))
          (setf
           (mevedel-session-hook-rules session)
           `((PreToolUse
              ((:matcher "Read"
                :hooks ((:type command
                         :source project-file
                         :source-root ,remote-root
                         :command
                         ,(format "printf '%%s' %s"
                                  (shell-quote-argument json)))))))))
          (with-temp-buffer
            (setq-local default-directory remote-root)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel--session session)
            (setq result
                  (test-mevedel-pipeline--run-tool-and-wait
                   tool '(:file_path "inside.txt"))))
          (should (string-prefix-p "Error:" result))
          (should (string-match-p "another execution target" result)))
      (delete-directory root t)
      (delete-directory foreign-root t)))
  :doc "remote same-target additional roots authorize the normal pipeline"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-root-" t)))
         (extra (file-name-as-directory
                 (make-temp-file "mevedel-pipeline-extra-" t)))
         (path (file-name-concat extra "file.txt"))
         (remote-root (format "/mevedelmock:%s:%s"
                              (system-name) root))
         (remote-extra (format "/mevedelmock:%s:%s"
                               (system-name) extra))
         (native-path (file-name-concat extra "file.txt"))
         (remote-path (file-name-concat remote-extra "file.txt"))
         (workspace
          (mevedel-workspace--create
           :type 'project :id remote-root :root remote-root
           :name "remote-additional-root"
           :file-cache (mevedel-test-file-cache-create)))
         session
         seen-path
         (tool
          (mevedel-tool--create
           :name "Read" :args '((file_path path :required "Path"))
           :groups '(read) :read-only-p t
           :get-path (lambda (args) (plist-get args :file_path))
           :handler
           (lambda (args)
             (setq seen-path (plist-get args :file_path))
             (list :result
                   (with-temp-buffer
                     (insert-file-contents seen-path)
                     (buffer-string))))))
         (mevedel-workspace-additional-roots
          (list (cons remote-root (list remote-extra))))
         (mevedel-memory-dirs nil)
         (mevedel-permission-mode 'ask)
         (mevedel-protected-paths nil)
         result)
    (unwind-protect
        (progn
          (write-region "same-target" nil path nil 'silent)
          (mevedel-test--with-local-shell-tramp nil
            (setq session
                  (mevedel-session-create
                   "remote-additional-root" workspace remote-root))
            (with-temp-buffer
              (setq-local default-directory remote-root)
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (cl-letf
                  (((symbol-function 'mevedel-permission--enqueue)
                    (lambda (&rest _)
                      (ert-fail "Same-target additional root prompted"))))
                (setq result
                      (test-mevedel-pipeline--run-tool-and-wait
                       tool (list :file_path native-path))))))
          (should (equal "same-target" result))
          (should (equal remote-path seen-path)))
      (delete-directory root t)
      (delete-directory extra t)))
  :doc "rejects malformed resource addresses before permission or post-use hooks"
  (let* ((permission-called nil)
         (post-called nil)
         (handler-called nil)
         result
         (tool (mevedel-tool--create
                :name "Read"
                :handler (lambda (_args)
                           (setq handler-called t)
                           '(:result "unexpected"))
                :args '((file_path path-or-resource :required "Path"))
                :read-only-p t
                :async-p nil)))
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (event _payload callback &rest _)
                 (when (memq event '(PostToolUse PostToolUseFailure))
                   (setq post-called t))
                 (funcall callback nil)))
              ((symbol-function
                'mevedel-check-permission-async-with-metadata)
               (lambda (&rest _)
                 (setq permission-called t))))
      (with-temp-buffer
        (mevedel-pipeline-run-tool
         tool (lambda (value) (setq result value))
         '(:file_path "local://notes/../secret"))))
    (should (string-match-p "Error:" result))
    (should-not permission-called)
    (should-not post-called)
    (should-not handler-called))

  :doc "pre-tool rewrites are prepared after rewrite validation"
  (let* ((save-path (make-temp-file "mevedel-pipeline-resource-" t))
         (session (mevedel-session--create
                   :name "resource"
                   :save-path save-path))
         (attempt-seen nil)
         result
         (tool (mevedel-tool--create
                :name "Read"
                :handler
                (lambda (args)
                  (setq attempt-seen
                        (mevedel-resource-current-attempt
                         (plist-get args :file_path)))
                  '(:result "prepared"))
                :args '((file_path path-or-resource :required "Path"))
                :read-only-p t
                :async-p nil)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat save-path "local") t)
          (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                   (lambda (event _payload callback &rest _)
                     (if (eq event 'PreToolUse)
                         (funcall callback
                                  '(:updated-input
                                    (:file_path "local://rewritten.txt")))
                       (funcall callback nil))))
                  ((symbol-function
                    'mevedel-check-permission-async-with-metadata)
                   (lambda (_name callback &rest _)
                     (funcall callback
                              '(:outcome allow :raw-outcome allow)))))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (mevedel-pipeline-run-tool
             tool (lambda (value) (setq result value))
             '(:file_path "local://original.txt")))
          (should (string-prefix-p "prepared" result))
          (should attempt-seen)))
      (delete-directory save-path t)))

  :doc "Read authorizes a workspace symlink as its outside target"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-symlink-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "mevedel-pipeline-symlink-outside-" t)))
         (secret (file-name-concat outside "secret.txt"))
         (link (file-name-concat root "innocent.txt"))
         (workspace
          (mevedel-workspace--create
           :type 'project :id root :root root :name "symlink-root"))
         (tool
          (mevedel-tool--create
           :name "Read"
           :handler #'mevedel-tool-fs-read
           :args '((file_path path :required "Path"))
           :groups '(read)
           :read-only-p t
           :get-path (lambda (args) (plist-get args :file_path))))
         (mevedel-permission-rules
          (list (list "Read" :path secret :action 'deny)))
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'full-auto)
         results)
    (unwind-protect
        (progn
          (write-region "TOP-SECRET\n" nil secret nil 'silent)
          (make-symbolic-link secret link)
          (with-temp-buffer
            (setq-local default-directory root)
            (setq-local mevedel--workspace workspace)
            (dolist (path (list secret link))
              (let (result)
                (mevedel-pipeline-run-tool
                 tool (lambda (value) (setq result value))
                 (list :file_path path))
                (push result results))))
          (should
           (cl-every
            (lambda (result)
              (string-prefix-p "Error: Permission denied" result))
            results)))
      (delete-directory root t)
      (delete-directory outside t)))
  :doc "remote Read authorizes a symlink as its target-native destination"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-remote-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "mevedel-pipeline-remote-outside-" t)))
         (secret (file-name-concat outside "secret.txt"))
         (link (file-name-concat root "innocent.txt"))
         (tool
          (mevedel-tool--create
           :name "Read"
           :handler #'mevedel-tool-fs-read
           :args '((file_path path :required "Path"))
           :groups '(read)
           :read-only-p t
           :get-path (lambda (args) (plist-get args :file_path))))
         (mevedel-protected-paths nil)
         results)
    (unwind-protect
        (progn
          (write-region "REMOTE-SECRET\n" nil secret nil 'silent)
          (make-symbolic-link secret link)
          (mevedel-test--with-local-shell-tramp '("read-link")
            (let* ((remote-root
                    (format "/mevedelmock:read-link:%s" root))
                   (remote-secret
                    (format "/mevedelmock:read-link:%s" secret))
                   (remote-link
                    (format "/mevedelmock:read-link:%s" link))
                   (target
                    (mevedel-execution-target-create remote-root))
                   (workspace
                    (mevedel-workspace--create
                     :type 'project :id remote-root :root remote-root
                     :name "remote-symlink-root"
                     :file-cache (mevedel-test-file-cache-create)))
                   (session
                    (mevedel-session--create
                     :name "remote-symlink" :workspace workspace
                     :execution-target target
                     :working-directory remote-root
                     :permission-mode 'full-auto
                     :permission-rules
                     (list (list "Read" :path remote-secret
                                 :action 'deny)))))
              (with-temp-buffer
                (setq-local default-directory remote-root)
                (setq-local mevedel--workspace workspace)
                (setq-local mevedel--session session)
                (dolist (path (list remote-secret remote-link))
                  (let (result)
                    (mevedel-pipeline-run-tool
                     tool (lambda (value) (setq result value))
                     (list :file_path path))
                    (push result results))))))
          (should
           (cl-every
            (lambda (result)
              (string-prefix-p "Error: Permission denied" result))
            results)))
      (delete-directory root t)
      (delete-directory outside t)))
  :doc "Glob and Grep authorize a symlink root as its outside directory"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-search-root-" t)))
         (outside-directory (file-name-as-directory
                         (make-temp-file
                          "mevedel-pipeline-search-outside-" t)))
         (outside (directory-file-name outside-directory))
         (link (file-name-concat root "search-root"))
         (workspace
          (mevedel-workspace--create
           :type 'project :id root :root root :name "search-symlink-root"))
         (session
          (mevedel-session--create
           :name "search-symlink" :workspace workspace
           :working-directory root :permission-mode 'full-auto
           :sandbox-mode 'off
           :permission-rules
           (list (list "Glob" :path outside :action 'deny)
                 (list "Grep" :path outside :action 'deny))))
         (glob
          (mevedel-tool--create
           :name "Glob" :handler #'mevedel-tool-fs-search-glob
           :args '((pattern string :required "Pattern")
                   (path path :optional "Root"))
           :async-p t :read-only-p t :groups '(read)
           :get-path (lambda (args) (plist-get args :path))))
         (grep
          (mevedel-tool--create
           :name "Grep" :handler #'mevedel-tool-fs-search-grep
           :args '((pattern string :required "Pattern")
                   (path path :optional "Root"))
           :async-p t :read-only-p t :groups '(read)
           :get-path (lambda (args) (plist-get args :path))))
         (mevedel-protected-paths nil)
         results)
    (unless (executable-find "rg")
      (ert-skip "rg is required"))
    (unwind-protect
        (progn
          (write-region "needle\n" nil
                        (file-name-concat outside "found.el")
                        nil 'silent)
          (make-symbolic-link outside link)
          (with-temp-buffer
            (setq-local default-directory root)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel--session session)
            (dolist (entry (list (cons glob '(:pattern "*.el"))
                          (cons grep '(:pattern "needle"))))
              (dolist (path (list outside link))
                (push
                 (test-mevedel-pipeline--run-tool-and-wait
                  (car entry)
                  (append (copy-sequence (cdr entry))
                          (list :path path)))
                 results))))
          (should
           (cl-every
            (lambda (result)
              (string-prefix-p "Error: Permission denied" result))
            results)))
      (delete-directory root t)
      (delete-directory outside t)))
  :doc "ApplyPatch authorizes a nonexistent symlink tail as its destination"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-patch-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "mevedel-pipeline-patch-outside-" t)))
         (link (file-name-concat root "linked"))
         (destination
          (file-name-concat outside "new" "sub" "created.txt"))
         (workspace
          (mevedel-workspace--create
           :type 'project :id root :root root :name "patch-symlink-root"
           :file-cache (mevedel-test-file-cache-create)))
         (session
          (mevedel-session--create
           :name "patch-symlink" :workspace workspace
           :working-directory root :permission-mode 'full-auto
           :permission-rules
           (list (list "ApplyPatch" :path destination
                       :action 'deny))))
         (tool
          (mevedel-tool--create
           :name "ApplyPatch"
           :handler #'mevedel-tool-patch-handler
           :args '((patch string :required "Patch"))
           :async-p t :groups '(edit reviewed-edit) :snapshot-p t
           :get-paths #'mevedel-tool-patch--get-paths))
         (patch
          (string-join
           '("*** Begin Patch"
             "*** Add File: linked/new/sub/created.txt"
             "+created"
             "*** End Patch")
           "\n"))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         result)
    (unwind-protect
        (progn
          (make-symbolic-link outside link)
          (with-temp-buffer
            (setq-local default-directory root)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel--session session)
            (setq result
                  (test-mevedel-pipeline--run-tool-and-wait
                   tool (list :patch patch))))
          (should
           (string-prefix-p "Error: Permission denied" result))
          (should-not (file-exists-p destination)))
      (delete-directory root t)
      (delete-directory outside t)))
  :doc "ApplyPatch keeps its authorized destination across review delay"
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-pipeline-review-root-" t)))
           (outside-a (file-name-as-directory
                       (make-temp-file
                        "mevedel-pipeline-review-a-" t)))
           (outside-b (file-name-as-directory
                       (make-temp-file
                        "mevedel-pipeline-review-b-" t)))
           (link (file-name-concat root "linked"))
           (destination-a
            (file-name-concat outside-a "new" "created.txt"))
           (destination-b
            (file-name-concat outside-b "new" "created.txt"))
           (workspace
            (mevedel-workspace--create
             :type 'project :id root :root root
             :name "review-symlink-root"
             :file-cache (mevedel-test-file-cache-create)))
           (session
            (mevedel-session--create
             :name "review-symlink" :workspace workspace
             :working-directory root :permission-mode 'ask
             :resource-grants
             (list (list :path destination-a :access 'write))))
           (tool
            (mevedel-tool--create
             :name "ApplyPatch"
             :handler #'mevedel-tool-patch-handler
             :args '((patch string :required "Patch"))
             :async-p t :groups '(edit reviewed-edit) :snapshot-p t
             :get-paths #'mevedel-tool-patch--get-paths))
           (patch
            (string-join
             '("*** Begin Patch"
               "*** Add File: linked/new/created.txt"
               "+authorized"
               "*** End Patch")
             "\n"))
           (mevedel-permission-rules nil)
           (mevedel-protected-paths nil)
           result)
      (unwind-protect
          (progn
            (make-symbolic-link outside-a link)
            (with-current-buffer data-buf
              (setq-local default-directory root)
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (mevedel-pipeline-run-tool
               tool (lambda (value) (setq result value))
               (list :patch patch)))
            (should-not result)
            (delete-file link)
            (make-symbolic-link outside-b link)
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "[ Apply 1 change in 1 file ]")
              (backward-char 2)
              (let ((command (key-binding (kbd "RET"))))
                (should (eq command #'mevedel-patch-review-submit))
                (call-interactively command)))
            (should (string-search "Applied patch" result))
            (should
             (equal "authorized\n"
                    (with-temp-buffer
                      (insert-file-contents destination-a)
                      (buffer-string))))
            (should-not (file-exists-p destination-b)))
        (delete-directory root t)
        (delete-directory outside-a t)
        (delete-directory outside-b t))))
  :doc "side tool lifecycle audit uses the durable target without content"
		 (let* ((parent (mevedel-session--create :name "parent"))
			(side (mevedel-session--create
			       :name "side" :audit-session parent
			       :permission-mode 'full-auto))
			(tool (mevedel-tool--create
			       :name "PrivateEcho"
			       :handler (lambda (_args) '(:result "private-result"))
			       :args '((file_path string :required "Path"))
			       :read-only-p t))
			calls result)
		   (with-temp-buffer
		     (setq-local mevedel--session side)
		     (cl-letf (((symbol-function 'mevedel-telemetry-record)
				(lambda (session event &rest props)
				  (push (list session event props) calls))))
		       (mevedel-pipeline-run-tool
			tool (lambda (value) (setq result value))
			'(:file_path "/private/path"))))
		   (should (equal "private-result" result))
		   (dolist (event '(tool-received tool-finished))
		     (let ((call (cl-find event calls :key #'cadr)))
		       (should (eq parent (car call)))
		       (should (eq 'btw
				   (plist-get (nth 2 call) :conversation-scope)))
		       (should-not
			(string-match-p
			 (regexp-opt '("/private/path" "private-result"))
			 (prin1-to-string (nth 2 call)))))))
		 :doc "side permission decisions forward only whitelisted audit fields"
		 (let* ((root (make-temp-file "mevedel-side-audit-" t))
			(workspace (mevedel-workspace--create
				    :type 'project :id "audit" :root root
				    :name "audit"))
			(path (file-name-concat root "private"))
			(parent (mevedel-session--create
				 :name "parent" :workspace workspace))
			(side (mevedel-session--create
			       :name "side" :audit-session parent
			       :workspace workspace :working-directory root
			       :permission-mode 'full-auto))
			(tool (mevedel-tool--create
			       :name "PrivateEdit"
			       :handler (lambda (_args) '(:result "changed"))
			       :args '((file_path string :required "Path"))
			       :get-path (lambda (args) (plist-get args :file_path))))
			(mevedel-protected-paths nil)
			calls result)
		   (unwind-protect
		       (progn
			 (with-temp-buffer
			   (setq-local mevedel--session side)
			   (cl-letf (((symbol-function 'mevedel-telemetry-record)
				      (lambda (session event &rest props)
					(push (list session event props) calls))))
			     (mevedel-pipeline-run-tool
			      tool (lambda (value) (setq result value))
			      (list :file_path path))))
			 (should (equal "changed" result))
			 (let ((call
				(cl-find-if
				 (lambda (entry)
				   (and (eq parent (car entry))
					(eq 'permission-decision (cadr entry))))
				 calls)))
			   (should call)
			   (should (eq 'btw
				       (plist-get (nth 2 call) :conversation-scope)))
			   (should (eq :path
				       (plist-get (nth 2 call) :specifier-key)))
			   (should-not (plist-member (nth 2 call) :specifier-value))
			   (should-not
			    (string-match-p (regexp-quote path)
					    (prin1-to-string (nth 2 call))))))
		     (delete-directory root t)))
		 :doc "one-shot request prompts for mutations without storing authority"
		 (let* ((workspace (mevedel-workspace--create
				   :type 'project :id temporary-file-directory
				   :root temporary-file-directory))
				(session (mevedel-session--create
					  :name "one-shot" :workspace workspace
					  :permission-mode 'full-auto))
			(request (mevedel-request--create
				  :session session
				  :one-shot-mutations-p t
				  :skill-permission-rules
				  '(("Edit" :action allow))))
			(tool (mevedel-tool--create
			       :name "Edit"
			       :handler (lambda (_args) '(:result "changed"))))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			entry result)
		   (with-temp-buffer
		     (setq-local mevedel--session session)
		     (setq-local mevedel--current-request request)
		     (cl-letf (((symbol-function 'mevedel-permission--enqueue)
				(lambda (queued &optional _session)
				  (setq entry queued)
				  ;; Even a non-UI reusable outcome must collapse to once.
				  (funcall (plist-get queued :callback)
					   'allow-session))))
		       (mevedel-pipeline-run-tool
			tool (lambda (value) (setq result value)) nil)))
		   (should (plist-get entry :once-only))
		   (should (equal "changed" result))
		   (should-not (mevedel-session-permission-rules session)))
		 :doc "normal sessions omit per-step telemetry spans"
		 (let* ((session (mevedel-session--create
				  :name "normal-telemetry"
				  :permission-mode 'full-auto))
			(tool (mevedel-tool--create
			       :name "QuietEcho"
			       :handler (lambda (_args) '(:result "done"))
			       :read-only-p t
			       :async-p nil))
			started result)
		   (with-temp-buffer
		     (setq-local mevedel--session session)
		     (cl-letf (((symbol-function 'mevedel-telemetry-start)
				(lambda (&rest _) (setq started t)))
			       ((symbol-function 'mevedel-telemetry-record)
				#'ignore))
		       (mevedel-pipeline-run-tool
			tool (lambda (value) (setq result value)) nil)))
		   (should (equal "done" result))
		   (should-not started))
		 :doc "direct native-edit allow reaches the handler as auto-apply authority"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-direct-edit-" t)))
			(path (file-name-concat root "file.txt"))
			(workspace
			 (mevedel-workspace--create
			  :type 'project :id "direct-edit" :root root
			  :name "direct-edit" :file-cache nil))
			(session
			 (mevedel-session--create
			  :name "direct-edit" :workspace workspace
			  :permission-mode 'ask
			  :permission-rules '(("Edit" :action allow))))
			(auto-apply-p nil)
			(tool
			 (mevedel-tool--create
			  :name "Edit" :groups '(edit)
			  :get-path (lambda (args) (plist-get args :file_path))
			  :handler
			  (lambda (callback _args)
			    (setq auto-apply-p
				  (and
				   (boundp 'mevedel-pipeline--auto-apply-edit-p)
				   (symbol-value
				    'mevedel-pipeline--auto-apply-edit-p)))
			    (funcall callback '(:result "done")))
			  :async-p t))
			(mevedel-permission-rules nil)
			(mevedel-protected-paths nil)
			permission-context
			handler-context)
		   (unwind-protect
		       (with-temp-buffer
			 (setq-local mevedel--workspace workspace)
			 (setq-local mevedel--session session)
			 (setq-local default-directory root)
			 (cl-letf
			     (((symbol-function 'mevedel--all-allowed-roots)
			       (lambda (&optional _buffer) (list root))))
                           (mevedel-tool-permission-step
			    (list :tool tool
				  :args (list :file_path path)
				  :session session
				  :workspace workspace
				  :buffer (current-buffer))
			    (lambda (context)
			      (setq permission-context context))
			    #'ert-fail)
			   (mevedel-pipeline--step-handler
			    permission-context
			    (lambda (context) (setq handler-context context))
			    #'ert-fail))
			 (should (equal "done"
					(plist-get handler-context :result)))
			 (should auto-apply-p))
		     (delete-directory root t)))
		 :doc "allowed-root ApplyPatch goes directly to its mandatory review"
		 (mevedel-view-test--with-buffers
		   (let* ((root (file-name-as-directory
				 (make-temp-file "mevedel-pipeline-patch-ret-" t)))
			  (path (file-name-concat root "test.el"))
			  (workspace
			   (mevedel-workspace--create
			    :type 'project :id root :root root :name "patch-ret"
			    :file-cache (mevedel-test-file-cache-create)))
			  (session
			   (mevedel-session--create
			    :name "patch-ret" :workspace workspace
			    :working-directory root :permission-mode 'ask))
			  (patch
			   (string-join
			    '("*** Begin Patch"
			      "*** Update File: test.el"
			      "@@"
			      "-(defun hello-world ())"
			      "+(defun hello-world ()"
			      "+  (interactive)"
			      "+  (message \"Hello, world!\"))"
			      "*** End Patch")
			    "\n"))
			  (tool (or (mevedel-tool-get "ApplyPatch" "mevedel")
				    (progn
				      (mevedel-tool-patch-register)
				      (mevedel-tool-get "ApplyPatch" "mevedel"))))
			  (mevedel-permission-rules nil)
			  (mevedel-protected-paths nil)
			  file-buffer
			  result)
		     (unwind-protect
			 (progn
			   (with-temp-file path
			     (insert "(defun hello-world ())\n"))
			   (setq file-buffer (find-file-noselect path))
			   (with-current-buffer data-buf
			     (setq-local default-directory root
					 mevedel--workspace workspace
					 mevedel--session session)
			     (mevedel-pipeline-run-tool
			      tool (lambda (value) (setq result value))
			      (list :patch patch)))
			   (with-current-buffer view-buf
			     (goto-char (point-min))
			     (should-not (search-forward "Permission Request" nil t))
			     (goto-char (point-min))
			     (search-forward "M test.el")
			     (mevedel-patch-review-toggle-fold)
			     (should (string-search "- │ (defun hello-world ())"
						    (buffer-string)))
			     (should (string-search "Keys:" (buffer-string)))
			     (goto-char (point-min))
			     (search-forward "[ Apply 1 change in 1 file ]")
			     (backward-char 2)
			     (let ((command (key-binding (kbd "RET"))))
			       (should (eq command #'mevedel-patch-review-submit))
			       (call-interactively command)))
			   (should (string-search "Applied patch" result))
			   (should
			    (equal
			     "(defun hello-world ()\n  (interactive)\n  (message \"Hello, world!\"))\n"
			     (with-temp-buffer
			       (insert-file-contents path)
			       (buffer-string))))
			   (should
			    (equal
			     "(defun hello-world ()\n  (interactive)\n  (message \"Hello, world!\"))\n"
			     (with-current-buffer file-buffer (buffer-string))))
			   (should (verify-visited-file-modtime file-buffer)))
		       (when (buffer-live-p file-buffer)
			 (with-current-buffer file-buffer (set-buffer-modified-p nil))
			 (kill-buffer file-buffer))
		       (when (file-directory-p root) (delete-directory root t)))))
		 :doc "request cancellation settles an active pipeline exactly once"
		 (let* ((session (mevedel-session--create
				  :name "cancel-pipeline"
				  :permission-mode 'full-auto))
			(tool (mevedel-tool--create
			       :name "NeverReturns"
			       :handler (lambda (_callback _args) nil)
			       :read-only-p t
			       :async-p t))
			(callback-count 0)
			events
			finished
			request
			result)
		   (with-temp-buffer
		     (setq-local mevedel--session session)
		     (setq request (mevedel-request-begin session))
		     (cl-letf (((symbol-function 'mevedel-telemetry-detailed-p)
				(lambda (_session) t))
			       ((symbol-function 'mevedel-telemetry-start)
				(lambda (_session event &rest props)
				  (list :event event :props props)))
			       ((symbol-function 'mevedel-telemetry-finish)
				(lambda (span &rest props)
				  (push (append span props) finished)))
			       ((symbol-function 'mevedel-telemetry-record)
				(lambda (_session event &rest props)
				  (push (cons event props) events))))
		       (mevedel-pipeline-run-tool
			tool
			(lambda (value)
			  (cl-incf callback-count)
			  (setq result value))
			nil)
		       (should-not result)
		       (mevedel-request-end)
		       (should (= 1 callback-count))
		       (should (string-prefix-p "Error: Request cancelled" result))
		       (should (= 1 (cl-count 'tool-received events :key #'car)))
		       (should (= 1 (cl-count 'tool-finished events :key #'car)))
		       (should
			(eq 'error
			    (plist-get (cdr (cl-find 'tool-finished events :key #'car))
				       :outcome)))
		       (should
			(= 1
			   (cl-count 'cancelled finished
				     :key (lambda (span)
					    (plist-get span :outcome)))))
		       (mevedel-request-drain-cancellers request)
		       (should (= 1 callback-count)))))
		 :doc "each retry gets one pre-hook and one handler-specific post-hook"
		 (let* ((attempt 0)
			(tool (mevedel-tool--create
			       :name "Retry"
			       :handler (lambda (_args)
					  (if (= 1 (cl-incf attempt))
					      (error "First attempt failed")
					    '(:result "ok")))
			       :read-only-p t))
			events
			results)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (event _payload callback &rest _)
				(push event events)
				(funcall callback nil))))
		     (mevedel-pipeline-run-tool
		      tool (lambda (result) (push result results)) nil)
		     (mevedel-pipeline-run-tool
		      tool (lambda (result) (push result results)) nil))
		   (should (equal '(PreToolUse PostToolUseFailure
				    PreToolUse PostToolUse)
				  (nreverse events)))
		   (should (string-prefix-p "Error: First attempt failed"
					    (cadr results)))
		   (should (equal "ok" (car results))))
		 :doc "validation failures run no lifecycle hooks or handler"
		 (let* ((handler-called nil)
			(tool (mevedel-tool--create
			       :name "Invalid"
			       :args '((value string :required "Value"))
			       :handler (lambda (_args)
					  (setq handler-called t)
					  '(:result "unexpected"))
			       :read-only-p t))
			events
			result)
		   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
			      (lambda (event _payload callback &rest _)
				(push event events)
				(funcall callback nil))))
		     (mevedel-pipeline-run-tool
		      tool (lambda (value) (setq result value)) nil))
		   (should (string-prefix-p "Error:" result))
		   (should-not handler-called)
		   (should-not events))
		 :doc "repaired calls reach the handler and append one corrective note"
		 (let* ((tool (mevedel-tool--create
			       :name "Collect"
			       :category "mevedel"
			       :handler (lambda (args)
					  (list :result
						(format "received %d name"
							(length (plist-get args :names)))))
			       :args '((names array :required "Names"
					     :items (:type string)))
			       :read-only-p t
			       :async-p nil))
			result)
		   (mevedel-tool-register tool)
		   (unwind-protect
		       (with-temp-buffer
			 (let* ((adapted
				 (mevedel-tool-repair-pre-tool-call
				  '(:name "Collect" :args (:names "alice"))))
				(args (plist-get adapted :args)))
			   (mevedel-pipeline-run-tool
			    tool (lambda (value) (setq result value)) args)
			   (should (string-match-p "received 1 name" result))
			   (let ((audit
                                  (car (mevedel-test--hook-audit-records
					result))))
			     (should (eq 'tool-input-repair
					 (plist-get audit :type)))
			     (should (eq 'committed (plist-get audit :state)))
			     (should (equal 'wrap-array-singleton
					    (plist-get
					     (car (plist-get audit :repairs)) :rule))))
			   (let ((first (string-match
					 "Note: Repaired tool input" result)))
			     (should first)
			     (should-not
			      (string-match "Note: Repaired tool input"
					    result (1+ first))))))
		     (mevedel-tool-clear-registry)))
		 :doc "records valid main-session execution with actual backend and model"
		 (let* ((session (mevedel-session--create :name "main"))
			(tool (mevedel-tool--create
			       :name "TelemetryEcho"
			       :category "mevedel"
			       :handler (lambda (_args)
					  '(:result "sentinel tool result"))
			       :args '((message string :required "Message"))
			       :read-only-p t
			       :async-p nil))
			result)
		   (mevedel-tool-register tool)
		   (unwind-protect
		       (with-temp-buffer
			 (setq-local mevedel--session session)
			 (should-not
			  (mevedel-tool-repair-pre-tool-call
			   '(:name "TelemetryEcho" :args (:message "sentinel input")
			     :backend main-backend :model main-model)))
			 (mevedel-pipeline-run-tool
			  tool (lambda (value) (setq result value))
			  '(:message "sentinel input"))
			 (should (equal "sentinel tool result" result)))
		     (mevedel-tool-clear-registry))
		   (let ((event (car (mevedel-session-repair-log session))))
		     (should (eq 'valid (plist-get event :outcome)))
		     (should (equal "/root" (plist-get event :origin)))
		     (should (eq 'main-backend (plist-get event :backend)))
		     (should (eq 'main-model (plist-get event :model)))
		     (should (eq 'executed (plist-get event :execution)))
		     (should (eq 'success (plist-get event :result)))
			     (should-not
			      (string-match-p "sentinel" (prin1-to-string event)))))

  :doc "structured errors remain errors in lifecycle and repair telemetry"
  (let* ((session (mevedel-session--create :name "structured-error"))
         (mevedel-tool--registry (make-hash-table :test #'equal))
         (tool
          (mevedel-tool--create
           :name "StructuredFailure"
           :category "mevedel"
           :handler (lambda (_args)
                      '(:result "plain failure" :status error))
           :args '((message string :required "Message"))
           :read-only-p t))
         result)
    (mevedel-tool-register tool)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (should-not
       (mevedel-tool-repair-pre-tool-call
        '(:name "StructuredFailure" :args (:message "hello"))))
      (mevedel-pipeline-run-tool
       tool (lambda (value) (setq result value)) '(:message "hello")))
    (should-not (string-prefix-p "Error:" result))
    (let ((finished
           (cl-find 'tool-finished
                    (mevedel-session-telemetry-pending session)
                    :key (lambda (event) (plist-get event :event))))
          (repair (car (mevedel-session-repair-log session))))
      (should (eq 'error (plist-get finished :outcome)))
      (should (eq 'error (plist-get repair :result)))))

  :doc "rejected duplicate callbacks cannot replace settled handler status"
  (let* ((session (mevedel-session--create :name "settled-status"))
         (mevedel-tool--registry (make-hash-table :test #'equal))
         handler-callback
         post-callback
         result
         (tool
          (mevedel-tool--create
           :name "SettledStatus"
           :category "mevedel"
           :handler (lambda (callback _args)
                      (setq handler-callback callback))
           :args '((message string :required "Message"))
           :read-only-p t
           :async-p t)))
    (mevedel-tool-register tool)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (should-not
       (mevedel-tool-repair-pre-tool-call
        '(:name "SettledStatus" :args (:message "hello"))))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (event _payload callback &rest _)
                   (if (memq event '(PostToolUse PostToolUseFailure))
                       (setq post-callback callback)
                     (funcall callback nil)))))
        (mevedel-pipeline-run-tool
         tool (lambda (value) (setq result value)) '(:message "hello"))
        (funcall handler-callback '(:result "accepted" :status success))
        (should post-callback)
        (mevedel-test--with-captured-diagnostics nil
          (funcall handler-callback '(:result "late" :status error)))
        (funcall post-callback nil)))
    (should (string-prefix-p "accepted" result))
    (should
     (eq 'success
         (plist-get
          (cdr (mevedel-tool-render-data-extract result)) :status)))
    (let ((finished
           (cl-find 'tool-finished
                    (mevedel-session-telemetry-pending session)
                    :key (lambda (event) (plist-get event :event))))
          (repair (car (mevedel-session-repair-log session))))
      (should (eq 'success (plist-get finished :outcome)))
      (should (eq 'success (plist-get repair :result)))))

			 :doc "telemetry construction failures do not block result delivery"
			 (let* ((session (mevedel-session--create :name "main"))
				(tool (mevedel-tool--create
				       :name "TelemetryFailure"
				       :category "mevedel"
				       :handler (lambda (_args) '(:result "delivered"))
				       :args '((message string :required "Message"))
				       :read-only-p t
				       :async-p nil))
				result
				warning)
			   (mevedel-tool-register tool)
			   (unwind-protect
			       (with-temp-buffer
				 (setq-local mevedel--session session)
				 (mevedel-tool-repair-pre-tool-call
				  '(:name "TelemetryFailure" :args (:message "hello")))
				 (cl-letf (((symbol-function 'mevedel-tool-repair--event)
					    (lambda (&rest _)
					      (error "private telemetry sentinel")))
					   ((symbol-function 'display-warning)
					    (lambda (_type message &rest _)
					      (setq warning message))))
				   (mevedel-pipeline-run-tool
				    tool (lambda (value) (setq result value))
				    '(:message "hello")))
				 (should (equal "delivered" result))
				 (should warning)
				 (should-not (string-match-p "private\|sentinel" warning))
				 (should-not mevedel-tool-repair--in-flight))
			     (mevedel-tool-clear-registry)))
			 :doc "repair audit construction failures do not block result delivery"
			 (let* ((tool (mevedel-tool--create
				       :name "AuditConstructionFailure"
				       :category "mevedel"
				       :handler (lambda (_args) '(:result "delivered"))
				       :args '((names array :required "Names"
						     :items (:type string)))
				       :read-only-p t
				       :async-p nil))
				result
				warning)
			   (mevedel-tool-register tool)
			   (unwind-protect
			       (with-temp-buffer
				 (let* ((adapted
					 (mevedel-tool-repair-pre-tool-call
					  '(:name "AuditConstructionFailure"
						  :args (:names "alice"))))
					(args (plist-get adapted :args)))
				   (cl-letf
				       (((symbol-function 'mevedel-tool-repair-audit-record)
					 (lambda (&rest _)
					   (error "private audit sentinel")))
					((symbol-function 'display-warning)
					 (lambda (_type message &rest _)
					   (setq warning message))))
				     (mevedel-pipeline-run-tool
				      tool (lambda (value) (setq result value)) args)))
				 (should (equal "delivered\n\nNote: Repaired tool input: Wrapped the singleton at `names` as an array. Please use the corrected argument shape in later calls."
						result))
				 (should warning)
				 (should-not (string-match-p "private\|sentinel" warning)))
			     (mevedel-tool-clear-registry)))
			 :doc "repaired handler signals retain committed audit metadata"
			 (let* ((tool (mevedel-tool--create
				       :name "RepairedSignal"
				       :category "mevedel"
				       :handler (lambda (_args)
					  (error "handler exploded"))
				       :args '((names array :required "Names"
						     :items (:type string)))
				       :read-only-p t
				       :async-p nil))
				result)
			   (mevedel-tool-register tool)
			   (unwind-protect
			       (with-temp-buffer
				 (let* ((adapted
					 (mevedel-tool-repair-pre-tool-call
					  '(:name "RepairedSignal"
						  :args (:names "alice"))))
					(args (plist-get adapted :args)))
				   (mevedel-pipeline-run-tool
				    tool (lambda (value) (setq result value)) args))
				 (should (string-prefix-p "Error:" result))
				 (let ((first
					(string-match "Note: Repaired tool input" result)))
				   (should first)
				   (should-not
				    (string-match "Note: Repaired tool input"
						  result (1+ first))))
				 (let ((audit
                                        (car (mevedel-test--hook-audit-records
					      result))))
				   (should (eq 'tool-input-repair
					       (plist-get audit :type)))
				   (should (eq 'committed (plist-get audit :state)))))
			     (mevedel-tool-clear-registry)))
		 :doc "repaired PreToolUse denials include one repair audit"
		 (let* ((tool (mevedel-tool--create
			       :name "RepairedDenied"
			       :category "mevedel"
			       :handler (lambda (_args) "must not run")
			       :args '((names array :required "Names"
					     :items (:type string)))
			       :read-only-p t
			       :async-p nil))
			result)
		   (mevedel-tool-register tool)
		   (unwind-protect
		       (with-temp-buffer
			 (let* ((adapted
				 (mevedel-tool-repair-pre-tool-call
				  '(:name "RepairedDenied"
					  :args (:names "alice"))))
				(args (plist-get adapted :args)))
			   (cl-letf (((symbol-function 'mevedel-hooks-run-event)
				      (lambda (event _payload callback &rest _)
					(should (eq event 'PreToolUse))
					(funcall callback
						 '(:continue nil
						   :stop-reason "test denial")))))
			     (mevedel-pipeline-run-tool
			      tool (lambda (value) (setq result value)) args)))
			 (should (string-prefix-p "Error:" result))
			 (let ((repair-audits
				(cl-remove-if-not
				 (lambda (record)
				   (eq 'tool-input-repair
				       (plist-get record :type)))
                                 (mevedel-test--hook-audit-records result))))
			   (should (= 1 (length repair-audits)))))
		     (mevedel-tool-clear-registry)))
                 :doc "path repair is shared by permission, snapshot, handler, and rendering"
                 (let* ((expected "/tmp/notes.md")
                        (workspace (mevedel-workspace--create
                                    :type 'project :id temporary-file-directory
                                    :root temporary-file-directory))
                        (session
                         (mevedel-session--create
                          :name "path-repair"
                          :workspace workspace
                          :permission-mode 'ask
                          :resource-grants
                          (list (list :path expected :access 'write))))
                        (request (mevedel-request--create
                                  :file-snapshots (make-hash-table :test #'equal)))
                        (mevedel--session session)
                        (mevedel--current-request request)
                        (get-path-values nil)
                        snapshot-value
                        handler-value
                        render-value
                        (tool (mevedel-tool--create
                               :name "PathMutation"
                               :category "mevedel"
                               :handler (lambda (args)
                                          (setq handler-value
                                                (plist-get args :file_path))
                                          '(:result "updated"))
                               :args '((file_path path :required "File path"))
                               :check-permission (lambda (_tool _args) 'allow)
                               :get-path (lambda (args)
                                           (let ((path (plist-get args :file_path)))
                                             (push path get-path-values)
                                             path))
                               :render-transform
                               (lambda (_name args _result)
                                 (setq render-value (plist-get args :file_path))
                                 '(:status updated))
                               :read-only-p nil
                               :snapshot-p t
                               :async-p nil))
                        result)
                   (mevedel-tool-register tool)
                   (unwind-protect
                       (with-temp-buffer
                         (cl-letf (((symbol-function
                                     'mevedel-pipeline--snapshot-file-if-needed)
                                    (lambda (_request path)
                                      (setq snapshot-value path))))
                           (let* ((adapted
                                   (mevedel-tool-repair-pre-tool-call
                                    '(:name "PathMutation"
                                            :args
                                            (:file_path
                                             "/tmp/[notes.md](https://notes.md)"))))
                                  (args (plist-get adapted :args)))
                             (mevedel-pipeline-run-tool
                              tool (lambda (value) (setq result value)) args)
                             (should (seq-every-p
                                      (lambda (value) (equal expected value))
                                      get-path-values))
                             (should (equal expected snapshot-value))
                             (should (equal expected handler-value))
                             (should (equal expected render-value))
                             (should (string-match-p
                                      "Note: Repaired tool input" result)))))
                     (mevedel-tool-clear-registry)))
		 :doc "async tool runs through pipeline"
		 (let* ((tool (mevedel-tool--create
			       :name "AsyncEcho"
			       :handler (lambda (cb args)
					  (funcall cb
						   (list :result (plist-get args :msg))))
			       :args '((msg string :required "Message"))
			       :read-only-p t
			       :async-p t))
			result)
		   (mevedel-pipeline-run-tool
		    tool (lambda (r) (setq result r)) '(:msg "async hello"))
		   (should (equal result "async hello")))
		 :doc "network escalation is a separate audited retry without hidden replay"
		 (let* ((root (make-temp-file "mevedel-network-retry-" t))
			(workspace (mevedel-workspace--create
			            :type 'project :id root :root root))
			(session (mevedel-session--create
				  :name "network-retry"
				  :workspace workspace
				  :execution-target
				  (mevedel-execution-target-create root)
				  :save-path root
				  :permission-mode 'full-auto))
			(mevedel--session session)
			(mevedel-permission-log-enabled t)
			(mevedel-permission-rules nil)
			launches first-result second-result)
		   (mevedel-tool-exec--register)
		   (unwind-protect
		       (let ((tool (mevedel-tool-get "Bash")))
			 (cl-letf
			     (((symbol-function
				'mevedel-execution-start-bash)
			       (lambda (callback &rest keys)
				 (let ((additional-permissions
					(plist-get keys :additional-permissions)))
				   (push additional-permissions launches)
				 (funcall
				  callback
				  (if additional-permissions
				      '(:output "downloaded"
					:facts
					(:state completed :termination exited
					 :exit-code 0 :outcome success
					 :wall-time-seconds 0.1 :output-bytes 10
					 :output-lines 1 :omitted-output-bytes 0
					 :tty nil)
					:sandbox-facts
					(:sandbox bubblewrap
					 :filesystem workspace-write
					 :network unrestricted))
				    '(:output "network denied"
				      :facts
				      (:state completed :termination exited
				       :exit-code 7 :outcome failure
				       :wall-time-seconds 0.1 :output-bytes 14
				       :output-lines 1 :omitted-output-bytes 0
				       :tty nil)
				      :sandbox-facts
				      (:sandbox bubblewrap
				       :filesystem workspace-write
				       :network isolated))))))))
			   (mevedel-pipeline-run-tool
			    tool (lambda (result) (setq first-result result))
			    '(:command "pwd"))
			   (should (= 1 (length launches)))
			   (should (string-match-p "network denied" first-result))
			   (should (string-match-p "network: isolated" first-result))
			   (mevedel-pipeline-run-tool
			    tool (lambda (result) (setq second-result result))
			    '(:command "pwd"
			      :sandbox_permissions "with_additional_permissions"
			      :additional_permissions (:network t)
			      :justification "Reach the requested service?"))
			   (should (= 2 (length launches)))
			   (should (equal '((:network t) nil) launches))
			   (should (string-match-p "downloaded" second-result))
			   (should (string-match-p "network: unrestricted"
					   second-result)))
			 (let* ((entries
                                 (mevedel-test--permission-log-entries session))
				(classifier-entries
				 (seq-filter
				  (lambda (entry)
				    (eq 'bash-classifier (plist-get entry :via)))
				  entries))
				(network-entry
				 (seq-find
				  (lambda (entry)
				    (eq 'sandbox-network (plist-get entry :via)))
				  entries)))
			   (should (= 2 (length classifier-entries)))
			   (should (eq 'allow (plist-get network-entry :outcome)))
			   (should
			    (equal '(:network t)
				   (plist-get network-entry
					      :additional-permissions)))))
		     (mevedel-tool-clear-registry)
		     (delete-directory root t)))
		 :doc "filesystem authority does not authorize its Bash consumer"
		 (let* ((root (make-temp-file "mevedel-resource-authority-" t))
			(secret (file-name-concat root "secret"))
			(workspace (mevedel-workspace--create
			            :type 'project :id root :root root))
			(session (mevedel-session--create
				  :name "resource-authority"
				  :workspace workspace
				  :execution-target
				  (mevedel-execution-target-create root)
				  :save-path root
				  :permission-mode 'ask
				  :resource-grants
				  (list (list :path secret :access 'read))))
			(mevedel--session session)
			(mevedel-permission-rules nil)
			enqueued-kinds launched result)
		   (mevedel-tool-exec--register)
		   (unwind-protect
		       (let ((tool (mevedel-tool-get "Bash")))
			 (cl-letf
			     (((symbol-function 'mevedel-permission--enqueue)
			       (lambda (entry &optional _session)
				 (push (plist-get entry :kind) enqueued-kinds)
				 (funcall (plist-get entry :callback) 'deny-once)))
			      ((symbol-function
				'mevedel-execution-start-one-shot)
			       (lambda (&rest _args) (setq launched t))))
			   (mevedel-pipeline-run-tool
			    tool (lambda (value) (setq result value))
			    (list :command (format "custom-reader %s" secret)
				  :sandbox_permissions
				  "with_additional_permissions"
				  :additional_permissions
				  (list :file_system
					(list :read (vector secret)))
				  :justification "Read the requested secret?")))
			 (should (equal '(bash) enqueued-kinds))
			 (should-not launched)
			 (should (string-match-p "Permission denied" result)))
		     (mevedel-tool-clear-registry)
		     (delete-directory root t)))
		 :doc "tool handlers default to the workspace root"
		 (let* ((root (make-temp-file "mevedel-tool-root-" t))
			(other (make-temp-file "mevedel-tool-other-" t))
			(ws (mevedel-workspace--create :root root))
			(mevedel--session (mevedel-session--create
					   :name "main"
					   :workspace ws))
			(tool (mevedel-tool--create
			       :name "PwdTool"
			       :handler (lambda (_args)
					  (list :result default-directory))
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
			       :handler (lambda (_args)
					  (list :result default-directory))
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
			   (funcall saved-cb '(:result "done")))
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
			   (funcall saved-cb '(:result "done")))
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
			(ws (mevedel-workspace--create
			     :type 'project :id tmpdir :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel" "sessions" "main")))
			(mevedel--session (mevedel-session--create
					   :name "main"
					   :workspace ws
					   :execution-target
					   (mevedel-execution-target-create tmpdir)
					   :save-path save-path))
			;; Handler mimics Grep/Glob: all work, including the callback,
			;; runs inside `with-temp-buffer'.  If step-persist were
			;; reading `current-buffer' it would see the temp buffer and
			;; fail to find the session, truncating instead of persisting.
			(tool (mevedel-tool--create
			       :name "BigFromTemp"
			       :handler (lambda (cb _args)
					  (with-temp-buffer
					    (funcall cb
						     (list :result (make-string 500 ?y)))))
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
				   (error "Step body signaled after next-cont fired")))
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
		   (mevedel-test--with-captured-diagnostics nil
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
			       :handler (lambda (_args) '(:result "payload"))
			       :args nil
			       :read-only-p t
			       :async-p nil))
			(count 0)
			(signaling-cb (lambda (_r)
					(cl-incf count)
						(error "Consumer signaled"))))
		   (mevedel-test--with-captured-diagnostics nil
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

(mevedel-deftest mevedel-pipeline--define-tool-wrapper
		 (:before-each
		  (advice-add 'display-warning :around
                              #'mevedel-test--drop-sessionless-permission-warning)
		  :after-each
		  (advice-remove 'display-warning
                                 #'mevedel-test--drop-sessionless-permission-warning))
		 ,test
		 (test)
		 :doc "generated wrapper runs pipeline"
		 (progn
		   (let ((mevedel-permission-rules '(("TestEcho" :action allow)))
			 (mevedel-protected-paths nil)
			 (mevedel-permission-mode 'ask)
			 result)
		     (mevedel-define-tool
		      :name "TestEcho"
		      :description "Echo test"
		      :handler (lambda (args)
				 (list :result (plist-get args :msg)))
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
			 (mevedel-permission-mode 'ask)
			 result)
		     (mevedel-define-tool
		      :name "TestStrict"
		      :description "Strict test"
		      :handler (lambda (args)
				 (list :result (plist-get args :name)))
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

(mevedel-deftest mevedel-pipeline--head-tail-preview ()
		 ,test
		 (test)
		 :doc "returns short output unchanged"
		 (let ((mevedel-pipeline--preview-size 20))
		   (should (equal "short" (mevedel-pipeline--head-tail-preview "short"))))
		 :doc "returns output exactly at the preview limit unchanged"
		 (let ((mevedel-pipeline--preview-size 20)
		       (result "01234567890123456789"))
		   (should (equal result (mevedel-pipeline--head-tail-preview result))))
		 :doc "truncates output immediately above the preview limit"
		 (let ((mevedel-pipeline--preview-size 20))
		   (should
		    (equal
		     "0123456789\n[mevedel: tool output truncated; omitted 1 chars]\n1234567890"
		     (mevedel-pipeline--head-tail-preview "012345678901234567890"))))
		 :doc "keeps equal ends of a long single line and reports the exact omission"
		 (let* ((mevedel-pipeline--preview-size 20)
			(result "hhhhhhhhhhmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmtttttttttt")
			(preview (mevedel-pipeline--head-tail-preview result)))
		   (should
		    (equal
		     "hhhhhhhhhh\n[mevedel: tool output truncated; omitted 30 chars]\ntttttttttt"
		     preview)))
		 :doc "cuts both retained ends at nearby newline boundaries"
		 (let* ((mevedel-pipeline--preview-size 20)
			(result (concat "HEAD-A\n" (make-string 30 ?m) "\nTAIL-B"))
			(preview (mevedel-pipeline--head-tail-preview result)))
		   (should
		    (equal
		     "HEAD-A\n[mevedel: tool output truncated; omitted 31 chars]\nTAIL-B"
		     preview))))

(mevedel-deftest mevedel-pipeline--persist-result ()
		 ,test
		 (test)
		 :doc "writes full result to file and returns preview"
		 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create
			     :type 'project :id tmpdir :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel" "sessions" "main")))
			(_ (make-directory save-path t))
			(session (mevedel-session--create
				  :name "main" :workspace ws :save-path save-path
				  :execution-target
				  (mevedel-execution-target-create tmpdir)))
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
				 (should (string-match-p "Use Read(file_path=" persisted))
				 (should (string-match-p "or Grep(pattern=" persisted))
				 (should (string-match-p
					      "Full output saved to: artifact://TestTool-[^[:space:]]+\\.txt"
					      persisted))
				 (should-not (string-search save-path persisted))
				 (should (string-match-p "Read(file_path=\"artifact://" persisted))
				 (should (string-match-p "Grep(pattern=PATTERN, path=\"artifact://"
							      persisted))
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
		 :doc "publishes remote artifacts and reports a target-native path"
		 (let ((host "artifact")
		       (tmpdir (file-name-as-directory
				(make-temp-file "mevedel-result-remote-" t)))
			published)
		   (unwind-protect
		       (mevedel-test--with-local-shell-tramp (list host)
			 (let* ((root (format "/mevedelmock:%s:%s/" host tmpdir))
				(ws (mevedel-workspace--create :root root))
				(target (mevedel-execution-target-create root))
				(save-path (file-name-as-directory
					    (file-name-concat
					     root ".mevedel" "sessions" "main")))
				(session (mevedel-session--create
					  :name "main" :workspace ws
					  :save-path save-path
					  :execution-target target))
				(tool (mevedel-tool--create :name "TestTool"))
				(result (make-string 500 ?x)))
			   (cl-letf (((symbol-function
                                        'mevedel-session-artifacts-publish-text)
				       (lambda (seen-session path content
						&optional coding)
					 (should-not
					  (file-directory-p
					   (file-name-directory path)))
					 (setq published
					       (list seen-session path content coding)))))
			     (let ((preview
				    (mevedel-pipeline--persist-result
				     result tool session)))
			       (pcase-let ((`(,seen-session ,path ,content ,coding)
					    published))
				 (should (eq session seen-session))
				 (should (equal result content))
				 (should (eq 'utf-8-unix coding))
				 (should (file-remote-p path))
				 ;; The model sees the logical artifact address, never a
				 ;; target path and never a client TRAMP prefix.
				 (should (string-search
					  (concat "artifact://"
						  (file-name-nondirectory path))
					  preview))
				 (should-not
				  (string-search
				   (file-name-directory
				    (file-remote-p path 'localname 'never))
				   preview))
				 (should-not
				  (string-search "/mevedelmock:" preview)))))))
		     (delete-directory tmpdir t)))
		 :doc "normalizes raw UTF-8 bytes before writing persisted results"
		 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create
			     :type 'project :id tmpdir :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel" "sessions" "main")))
			(_ (make-directory save-path t))
			(session (mevedel-session--create
				  :name "main" :workspace ws :save-path save-path
				  :execution-target
				  (mevedel-execution-target-create tmpdir)))
			(tool (mevedel-tool--create :name "TestTool" :max-result-size 100))
			(result (concat "quote "
					(test-mevedel-pipeline--raw-bytes
					 #xe2 #x80 #x9c ?x #xe2 #x80 #x9d)
					(make-string 200 ?x)))
			(persisted (mevedel-pipeline--persist-result result tool session)))
		   (unwind-protect
		       (let ((files (directory-files
				     (file-name-concat save-path "tool-results")
				     t "\\.txt$")))
			 (should (= 1 (length files)))
			 (should (string-match-p "quote “x”" persisted))
			 (with-temp-buffer
			   (insert-file-contents (car files))
			   (should (string-prefix-p "quote “x”" (buffer-string)))))
		     (delete-directory tmpdir t)))
		 :doc "preview preserves the head and tail while the artifact stays complete"
		 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create
			     :type 'project :id tmpdir :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel" "sessions" "main")))
			(_ (make-directory save-path t))
			(session (mevedel-session--create
				  :name "main" :workspace ws :save-path save-path
				  :execution-target
				  (mevedel-execution-target-create tmpdir)))
			(tool (mevedel-tool--create :name "TestTool" :max-result-size 100))
			(result (concat (make-string 1000 ?h)
					(make-string 3000 ?m)
					(make-string 1000 ?t)))
			(persisted (mevedel-pipeline--persist-result result tool session)))
		   (unwind-protect
		       (let ((file (car (directory-files
					(file-name-concat save-path "tool-results")
					t "\\.txt$"))))
			 (should (string-search (make-string 100 ?h) persisted))
			 (should (string-search (make-string 100 ?t) persisted))
			 (should-not (string-search (make-string 100 ?m) persisted))
			 (should (string-search "omitted 3000 chars" persisted))
			 (should (equal result
					(with-temp-buffer
					  (insert-file-contents file)
					  (buffer-string)))))
		     (delete-directory tmpdir t)))
		 :doc "materializes session directory when save path is absent"
		 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create
			     :type 'project :id tmpdir :root tmpdir))
			(session (mevedel-session--create
				  :name "main" :workspace ws
				  :execution-target
				  (mevedel-execution-target-create tmpdir)))
			(tool (mevedel-tool--create :name "TestTool" :max-result-size 100))
			(result (make-string 500 ?x))
			persisted)
		   (unwind-protect
		       (with-temp-buffer
			 (setq persisted
			       (mevedel-pipeline--persist-result
				result tool session (current-buffer)))
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
		   (should (string-match-p "BigTool" truncated))
		   (should (string-match-p "unavailable" truncated))
		   (should (string-match-p "narrower" truncated)))
		 :doc "keeps both ends when persistence is unavailable"
		 (let* ((tool (mevedel-tool--create :name "BigTool" :max-result-size 100))
			(result (concat (make-string 1000 ?h)
					(make-string 3000 ?m)
					(make-string 1000 ?t)))
			(truncated (mevedel-pipeline--truncate-result result tool)))
		   (should (string-search (make-string 100 ?h) truncated))
		   (should (string-search (make-string 100 ?t) truncated))
		   (should-not (string-search (make-string 100 ?m) truncated))
		   (should (string-search "omitted 3000 chars" truncated))))

(mevedel-deftest mevedel-pipeline--truncate-error-result ()
			 ,test
			 (test)
			 :doc "truncates large errors while preserving error status"
			 (let* ((tool (mevedel-tool--create :name "ErrTool" :max-result-size 100))
				(result (concat "Error: " (make-string 5000 ?x)))
				(truncated (mevedel-pipeline--truncate-error-result
					    result tool t)))
			   (should (< (length truncated) (length result)))
			   (should (string-prefix-p "Error:" truncated))
			   (should (string-match-p "output too large" truncated))
			   (should (string-match-p "ErrTool" truncated))
			   (should (string-match-p "narrower scope" truncated)))
			 :doc "uses neutral prose for structured error status"
			 (let* ((tool (mevedel-tool--create :name "ErrTool" :max-result-size 100))
				(result (concat "plain failure " (make-string 5000 ?x)))
				(truncated (mevedel-pipeline--truncate-error-result result tool)))
			   (should-not (string-prefix-p "Error:" truncated))
			   (should (string-prefix-p "Output too large" truncated)))
			 :doc "keeps the failure tail and reports the exact omission"
			 (let* ((tool (mevedel-tool--create :name "ErrTool" :max-result-size 100))
				(result (concat (make-string 1000 ?h)
						(make-string 3000 ?m)
						(make-string 1000 ?t)))
				(truncated (mevedel-pipeline--truncate-error-result result tool)))
			   (should (string-search (make-string 100 ?h) truncated))
			   (should (string-search (make-string 100 ?t) truncated))
			   (should-not (string-search (make-string 100 ?m) truncated))
			   (should (string-search "omitted 3000 chars" truncated))))

(mevedel-deftest mevedel-pipeline--context-status
		 (:before-each
		  (advice-add 'display-warning :around
                              #'mevedel-test--drop-sessionless-permission-warning)
		  :after-each
		  (advice-remove 'display-warning
                                 #'mevedel-test--drop-sessionless-permission-warning))
		 ,test
		 (test)
		 :doc "canonical handler status takes precedence over render status"
		 (should (eq 'success
			     (mevedel-pipeline--context-status
			      '(:handler-status success :status error))))
		 :doc "explicit render status remains valid for direct contexts"
		 (should (eq 'error
			     (mevedel-pipeline--context-status
			      '(:status error :result "visible"))))
		 :doc "result text never determines lifecycle status"
		 (should (eq 'success
			     (mevedel-pipeline--context-status
			      '(:result "Error: visible text"))))
		 :doc "missing status defaults to success"
		 (should (eq 'success
			     (mevedel-pipeline--context-status '(:result "ok")))))

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
		 :doc "passes through when error result is within limit"
		 (let* ((tool (mevedel-tool--create :name "ErrTool" :max-result-size 100))
			(ctx (list :tool tool :result "Error: small"))
			next-ctx)
		   (mevedel-pipeline--step-persist
		    ctx (lambda (c) (setq next-ctx c)) #'ignore)
		   (should (equal "Error: small" (plist-get next-ctx :result))))
		 :doc "truncates error result when over limit"
			 (let* ((tool (mevedel-tool--create :name "ErrTool" :max-result-size 100))
				(result (concat "Error: " (make-string 5000 ?x)))
				(original-length (length result))
				(ctx (list :tool tool :result result
					   :handler-status 'error))
				next-ctx)
			   (mevedel-pipeline--step-persist
			    ctx (lambda (c) (setq next-ctx c)) #'ignore)
			   (should (string-prefix-p "Error:" (plist-get next-ctx :result)))
			   (should (string-match-p "output too large"
						   (plist-get next-ctx :result)))
			   (should (< (length (plist-get next-ctx :result))
				      original-length)))
			 :doc "structured errors keep neutral preview prose and explicit status"
			 (let* ((tool (mevedel-tool--create :name "ErrTool" :max-result-size 100))
				(result (concat (make-string 1000 ?h)
						(make-string 3000 ?m)
						(make-string 1000 ?t)))
				(ctx (list :tool tool :result result :status 'error))
				next-ctx)
			   (mevedel-pipeline--step-persist
			    ctx (lambda (c) (setq next-ctx c)) #'ignore)
			   (let ((preview (plist-get next-ctx :result)))
			     (should (eq 'error (plist-get next-ctx :status)))
			     (should-not (string-prefix-p "Error:" preview))
			     (should (string-search (make-string 100 ?h) preview))
			     (should (string-search (make-string 100 ?t) preview))))
				 :doc "persists result when over limit"
			 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create
			     :type 'project :id tmpdir :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel" "sessions" "main")))
			(_ (make-directory save-path t))
			(session (mevedel-session--create
				  :name "main" :workspace ws :save-path save-path
				  :execution-target
				  (mevedel-execution-target-create tmpdir)))
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
		 :doc "ephemeral requests bound oversized results without materializing"
		 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create :root tmpdir))
			(session (mevedel-session--create
				  :name "ephemeral" :workspace ws))
			(request (mevedel-request--create
				  :session session :ephemeral-p t))
			(tool (mevedel-tool--create
			       :name "BigResult" :max-result-size 10))
			(big-result (concat (make-string 1000 ?h)
					    (make-string 3000 ?m)
					    (make-string 1000 ?t)))
			(mevedel-pipeline--preview-size 20)
			next-ctx)
		   (unwind-protect
		       (with-temp-buffer
			 (mevedel-pipeline--step-persist
			  (list :tool tool :result big-result :session session
				:request request :buffer (current-buffer))
			  (lambda (context) (setq next-ctx context)) #'ignore)
			 (let ((preview (plist-get next-ctx :result)))
			   (should (< (length preview) (length big-result)))
			   (should (string-match-p "ephemeral request" preview))
			   (should-not (string-prefix-p "<persisted-output>" preview)))
			 (should-not (mevedel-session-save-path session))
			 (should-not (file-exists-p
				      (file-name-concat tmpdir ".mevedel"))))
		     (delete-directory tmpdir t)))
		 :doc "uses global cap when tool limit exceeds it"
		 (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
			(ws (mevedel-workspace--create
			     :type 'project :id tmpdir :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel" "sessions" "main")))
			(_ (make-directory save-path t))
			(session (mevedel-session--create
				  :name "main" :workspace ws :save-path save-path
				  :execution-target
				  (mevedel-execution-target-create tmpdir)))
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
;;; Render-data attachment

(mevedel-deftest mevedel-pipeline--handler-return-p ()
		 ,test
		 (test)
		 :doc "accepts plist with :result keyword"
		 (should (mevedel-pipeline--handler-return-p '(:result "ok")))
		 :doc "accepts plist with :result and :render-data"
		 (should (mevedel-pipeline--handler-return-p
			  '(:result "ok" :render-data (:kind diff))))
		 :doc "rejects non-plist render-data before attachment"
		 (should-not
		  (mevedel-pipeline--handler-return-p
		   '(:result "ok" :render-data ((:kind first) (:kind second)))))
		 :doc "accepts explicit success and error status"
		 (should (mevedel-pipeline--handler-return-p
			  '(:result "ok" :status success)))
		 (should (mevedel-pipeline--handler-return-p
			  '(:result "failed" :status error)))
		 :doc "rejects an unknown explicit status"
		 (should-not (mevedel-pipeline--handler-return-p
			      '(:result "maybe" :status unknown)))
		 :doc "rejects bare string"
		 (should-not (mevedel-pipeline--handler-return-p "just a string"))
		 :doc "rejects nil"
		 (should-not (mevedel-pipeline--handler-return-p nil))
		 :doc "rejects list with non-keyword head"
		 (should-not (mevedel-pipeline--handler-return-p '("x" "y")))
		 :doc "rejects plist without :result"
		 (should-not (mevedel-pipeline--handler-return-p '(:other 1)))
		 :doc "rejects odd and dotted lists"
		 (should-not (mevedel-pipeline--handler-return-p '(:result)))
		 (should-not (mevedel-pipeline--handler-return-p '(:result . "ok")))
		 :doc "rejects non-keyword keys"
		 (should-not
		  (mevedel-pipeline--handler-return-p '(:result "ok" extra t))))

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
		 (let ((ctx (list :result "hello" :tool-use-id "toolu_1"
				  :render-data '(:kind diff :patch "p")))
		       out)
		   (mevedel-pipeline--step-attach-render-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (let ((r (plist-get out :result)))
		     (should (string-prefix-p "hello" r))
                     (should (string-search mevedel-tool-render-data-open r))
                     (should (string-search mevedel-tool-render-data-close r))))
		 :doc "embedded block carries invisible text property for data-buffer display"
		 (let ((ctx (list :result "x" :tool-use-id "toolu_1"
				  :render-data '(:kind diff)))
		       out)
		   (mevedel-pipeline--step-attach-render-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (let* ((r (plist-get out :result))
                          (marker (string-search mevedel-tool-render-data-open r)))
		     (should marker)
		     ;; gptel 'ignore is not set: gptel's buffer parser extracts tool
		     ;; results via buffer-substring-no-properties, which drops text
		     ;; properties -- the block reaches the LLM via two strip hooks
                     ;; instead (see `mevedel-tool-render-data-format').
		     (should (eq t (get-text-property marker 'invisible r)))))
		 :doc "gptel tool-result stamping preserves hidden render-data"
		 (let ((ctx (list :result "x" :tool-use-id "toolu_1"
				  :render-data '(:kind diff)))
		       out)
		   (mevedel-pipeline--step-attach-render-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (let* ((r (copy-sequence (plist-get out :result)))
			  (tool-prop '(tool . "call_render"))
                          (marker (string-search mevedel-tool-render-data-open r)))
		     (add-text-properties 0 (length r) `(gptel ,tool-prop) r)
		     (should marker)
		     (should (equal tool-prop (get-text-property marker 'gptel r)))
		     (should (eq t (get-text-property marker 'invisible r)))))
		 :doc "non-string result with render-data is passed through unchanged"
		 (let ((ctx (list :result nil :render-data '(:kind diff)))
		       out)
		   (mevedel-pipeline--step-attach-render-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (should (null (plist-get out :result))))
		 :doc "serializes explicit handler status with render-data"
		 (let ((ctx (list :result "failed"
				  :tool-use-id "toolu_1"
				  :status 'error
				  :render-data '(:kind bash)))
		       out)
		   (mevedel-pipeline--step-attach-render-data
		    ctx (lambda (c) (setq out c)) #'ignore)
                   (let ((data (cdr (mevedel-tool-render-data-extract
				  (plist-get out :result) nil "toolu_1"))))
		     (should (eq 'bash (plist-get data :kind)))
		     (should (eq 'error (plist-get data :status)))))
		 :doc "serializes explicit status even without other render-data"
		 (let ((ctx (list :result "failed" :status 'error
				  :tool-use-id "toolu_1"))
		       out)
		   (mevedel-pipeline--step-attach-render-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (should
		    (eq 'error
			(plist-get
                         (cdr (mevedel-tool-render-data-extract
			       (plist-get out :result) nil "toolu_1"))
			 :status))))
		 :doc "omits read-only sandbox summaries beside existing data"
		 (let* ((summary
			 '(:attempt-count 1 :started-count 1 :refused-count 0
			   :sandbox bubblewrap :filesystem workspace-write
			   :network isolated :proc fresh
			   :additional-read-count 3 :additional-write-count 0))
			(ctx (list :result "ok"
				   :tool-use-id "toolu_1"
				   :render-data '(:kind helper)
				   :sandbox-summary-cell (list summary)))
			out)
		   (mevedel-pipeline--step-attach-render-data
		    ctx (lambda (c) (setq out c)) #'ignore)
                   (let ((data (cdr (mevedel-tool-render-data-extract
				  (plist-get out :result) nil "toolu_1"))))
		     (should (eq 'helper (plist-get data :kind)))
		     (should-not (plist-member data :sandbox-summary))))
		 :doc "serializes warning sandbox summaries beside existing data"
		 (let* ((summary
			 '(:attempt-count 1 :started-count 1 :refused-count 0
			   :sandbox bubblewrap :filesystem workspace-write
			   :network isolated :proc fresh
			   :additional-read-count 0 :additional-write-count 1))
			(ctx (list :result "ok"
				   :tool-use-id "toolu_1"
				   :render-data '(:kind helper)
				   :sandbox-summary-cell (list summary)))
			out)
		   (mevedel-pipeline--step-attach-render-data
		    ctx (lambda (c) (setq out c)) #'ignore)
                   (let ((data (cdr (mevedel-tool-render-data-extract
				  (plist-get out :result) nil "toolu_1"))))
		     (should (eq 'helper (plist-get data :kind)))
		     (should (equal summary
				    (plist-get data :sandbox-summary)))))
		 :doc "omits default sandbox summaries"
		 (let ((ctx
			(list
			 :result "ok"
			 :sandbox-summary-cell
			 (list
			  '(:attempt-count 1 :started-count 1 :refused-count 0
			    :sandbox bubblewrap :filesystem workspace-write
			    :network isolated :proc fresh
			    :additional-read-count 0 :additional-write-count 0))))
		       out)
		   (mevedel-pipeline--step-attach-render-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (should (equal "ok" (plist-get out :result)))))

(mevedel-deftest mevedel-pipeline--step-attach-media-data ()
		 ,test
		 (test)
		 :doc "no media: result passes through unchanged"
		 (let ((ctx (list :result "hello" :media nil))
		       out)
		   (mevedel-pipeline--step-attach-media-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (should (equal "hello" (plist-get out :result))))
		 :doc "with media: result gets a delimited hidden block appended"
		 (let ((ctx (list :result "hello"
				  :media '((:path "/tmp/a.png"
					    :mime "image/png"
					    :kind image
					    :data "captured"))))
		       out)
		   (mevedel-pipeline--step-attach-media-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (let ((r (plist-get out :result)))
		     (should (string-prefix-p "hello" r))
		     (should (string-search mevedel-tool-media--data-open r))
		     (should (string-search mevedel-tool-media--data-close r))
		     (should (eq t (get-text-property
				    (string-search
				     mevedel-tool-media--data-open r)
				    'mevedel-media-data r)))
		     (should (equal "hello"
                                    (car (mevedel-tool-render-data-extract r))))
		     (should-not
		      (string-search mevedel-tool-media--data-open
                                     (mevedel-tool-media-strip-blocks r)))))
		 :doc "ephemeral media remains in memory without materializing the session"
		 (let* ((root (make-temp-file "mevedel-ephemeral-media-" t))
			(workspace (mevedel-workspace--create
				    :type 'project :id "media" :root root
				    :name "media"))
			(session (mevedel-session--create
				  :name "side" :workspace workspace))
			(request (mevedel-request--create
				  :session session :ephemeral-p t))
			out)
		   (unwind-protect
		       (with-temp-buffer
			 (mevedel-pipeline--step-attach-media-data
			  (list :result "hello"
				:media '((:path "/private/image.png"
					  :mime "image/png"
					  :kind image
					  :data "captured"))
				:session session :request request
				:buffer (current-buffer))
			  (lambda (context) (setq out context)) #'ignore)
			 (should (string-search
				  mevedel-tool-media--data-open
				  (plist-get out :result)))
			 (should-not (mevedel-session-save-path session))
			 (should-not (file-exists-p
				      (file-name-concat root ".mevedel"))))
		     (delete-directory root t)))
		 :doc "with media envelope: inline base64 is summarized before append"
		 (let* ((result (concat "<media-file>\n"
					"path: /tmp/a.png\n"
					"mime_type: image/png\n"
					"encoding: base64\n"
					"data:\n"
					"QUJD\n"
					"</media-file>"))
			(ctx (list :result result
				   :media '((:path "/tmp/a.png"
					     :mime "image/png"
					     :kind image
					     :data "QUJD"))))
			out)
		   (mevedel-pipeline--step-attach-media-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (let* ((r (plist-get out :result))
			  (visible (car (test-mevedel-pipeline--extract-media-data
					 r))))
		     (should (string-search "<native media block attached>"
					    visible))
		     (should-not (string-search "QUJD" visible))
		     (should (string-search mevedel-tool-media--data-open
					    r))))
		 :doc "with large media envelope: summarization avoids regexp overflow"
		 (let* ((payload (make-string 500000 ?A))
			(result (concat "<media-file>\n"
					"path: /tmp/a.jpg\n"
					"mime_type: image/jpeg\n"
					"encoding: base64\n"
					"data:\n"
					payload
					"\n</media-file>"))
			(ctx (list :result result
				   :media `((:path "/tmp/a.jpg"
					     :mime "image/jpeg"
					     :kind image
					     :data ,payload))))
			out)
		   (mevedel-pipeline--step-attach-media-data
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (let ((visible (car (test-mevedel-pipeline--extract-media-data
					(plist-get out :result)))))
		     (should (string-search "<native media block attached>"
					    visible))
		     (should-not (string-search payload visible))))
		 :doc "literal media delimiters without text properties are normal text"
		 (let* ((literal (concat "text\n"
					 mevedel-tool-media--data-open "\n"
					 "(:items ((:path \"/tmp/a.png\" :mime \"image/png\" :kind image :data \"QUJD\")))\n"
					 mevedel-tool-media--data-close "\n"
					 "tail"))
			(extract (test-mevedel-pipeline--extract-media-data literal)))
		   (should (equal literal (car extract)))
		 (should-not (cdr extract))
		   (should (equal literal
				  (mevedel-tool-media-strip-blocks
				   literal))))
		 :doc "malformed hidden media block without close is ignored"
		 (let* ((literal (concat
				  "text"
				  (propertize
				   (concat "\n"
					   mevedel-tool-media--data-open
					   "\n(:id \"missing-close\")")
				   'mevedel-media-data t)))
			(extract (test-mevedel-pipeline--extract-media-data literal)))
		   (should (equal literal (car extract)))
		   (should-not (cdr extract)))
		 :doc "persisted media references survive text property loss"
		 (let* ((tmpdir (make-temp-file "mevedel-test-media-store-" t))
			(ws (mevedel-workspace--create :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel" "sessions" "main")))
			(session (mevedel-session--create
				  :name "main" :workspace ws :save-path save-path))
			(media '((:path "/tmp/a.png"
				  :mime "image/png"
				  :kind image
				  :data "captured")))
			(result (concat "hello"
					(test-mevedel-pipeline--format-media-data-block
					 media session nil "toolu_1")))
			(plain (substring-no-properties result))
			extract)
		   (unwind-protect
		       (progn
			 (setq extract
			       (test-mevedel-pipeline--extract-media-data
				plain session nil "toolu_1"))
			 (should (equal "hello" (car extract)))
			 (let ((item (car (cdr extract))))
			   (should (equal "/tmp/a.png" (plist-get item :path)))
			   (should (equal "image/png" (plist-get item :mime)))
			   (should (eq 'image (plist-get item :kind)))
			   (should (equal "captured" (plist-get item :data))))
			 (should (equal
				  "hello"
                                  (car (mevedel-tool-render-data-extract
					plain session "toolu_1")))))
		     (delete-directory tmpdir t)))
		 :doc "view extraction can strip duplicate tool block with wrong gptel id"
		 (let* ((tmpdir (make-temp-file
				 "mevedel-test-media-store-view-" t))
			(ws (mevedel-workspace--create :root tmpdir))
			(save-path (file-name-as-directory
				    (file-name-concat tmpdir ".mevedel"
						      "sessions" "main")))
			(session (mevedel-session--create
				  :name "main" :workspace ws :save-path save-path))
			(media '((:path "/tmp/b.png"
				  :mime "image/png"
				  :kind image
				  :data "captured")))
			(result (concat "hello"
					(test-mevedel-pipeline--format-media-data-block
					 media session nil "toolu_2")))
			(plain (substring-no-properties result)))
		   (unwind-protect
		       (progn
			 (should
			  (string-search
			   mevedel-tool-media--data-open
                           (car (mevedel-tool-render-data-extract
				 plain session "toolu_1"))))
			 (should (equal
				  "hello"
                                  (car (mevedel-tool-render-data-extract
					plain session "toolu_1" t)))))
		     (delete-directory tmpdir t))))

(mevedel-deftest mevedel-pipeline--current-tool-use-id
		 (:before-each
		  (advice-add 'display-warning :around
                              #'mevedel-test--drop-sessionless-permission-warning)
		  :after-each
		  (advice-remove 'display-warning
                                 #'mevedel-test--drop-sessionless-permission-warning))
		 ,test
		 (test)
		 :doc "claims matching duplicate tool calls in dispatch order"
		 (let* ((tool (mevedel-tool--create :name "Read"))
			(tool-call-args '(:file_path "a.png"))
			(pipeline-args '(:file_path "a.png"
					 :offset nil
					 :limit nil
					 :pages nil
					 :max_width nil
					 :max_height nil
					 :max_tokens nil))
			(call-1 (list :id "toolu_1" :name "Read"
				      :args tool-call-args))
			(call-2 (list :id "toolu_2" :name "Read"
				      :args tool-call-args))
			(info (list :tool-use (list call-1 call-2)))
			(fsm (gptel-make-fsm :info info)))
		   (let ((mevedel-tools--current-fsm fsm))
		     (should (equal "toolu_1"
				    (mevedel-pipeline--current-tool-use-id
				     tool pipeline-args)))
		     (should (equal "toolu_2"
				    (mevedel-pipeline--current-tool-use-id
				     tool pipeline-args)))
		     (should-not
		      (mevedel-pipeline--current-tool-use-id
		       tool pipeline-args)))))

(mevedel-deftest mevedel-pipeline--step-render-transform ()
		 ,test
		 (test)
		 :doc "stores transform render-data without changing result or raw-result"
		 (let* ((tool (mevedel-tool--create
			       :name "Transform"
			       :render-transform
			       (lambda (name args result)
				 (list :tool name
				       :arg (plist-get args :x)
				       :chars (length result)))))
			(ctx (list :tool tool :args '(:x "a")
				   :result "abcdef" :raw-result "abcdef"))
			out)
		   (mevedel-pipeline--step-render-transform
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (should (equal "abcdef" (plist-get out :result)))
		   (should (equal "abcdef" (plist-get out :raw-result)))
		   (should (equal '(:tool "Transform" :arg "a" :chars 6)
				  (plist-get out :render-data))))
		 :doc "does not run when handler render-data already exists"
		 (let* ((called nil)
			(tool (mevedel-tool--create
			       :name "HasData"
			       :render-transform
			       (lambda (_name _args _result)
				 (setq called t)
				 '(:new t))))
			(ctx (list :tool tool :args nil :result "ok"
				   :render-data '(:old t)))
			out)
		   (mevedel-pipeline--step-render-transform
		    ctx (lambda (c) (setq out c)) #'ignore)
		   (should-not called)
		   (should (equal '(:old t) (plist-get out :render-data))))
		 :doc "skips non-string and Error results"
		 (let* ((calls 0)
			(tool (mevedel-tool--create
			       :name "Skip"
			       :render-transform
			       (lambda (_name _args _result)
				 (cl-incf calls)
				 '(:data t)))))
		   (dolist (case '((42 success) ("Error: nope" error)))
		     (pcase-let ((`(,result ,status) case))
		       (let (out)
		       (mevedel-pipeline--step-render-transform
			(list :tool tool :args nil :result result
			      :handler-status status)
			(lambda (c) (setq out c)) #'ignore)
			 (should (null (plist-get out :render-data))))))
		   (should (= 0 calls)))
		 :doc "oversized transform metadata is rejected"
		 (let* ((tool (mevedel-tool--create
			       :name "Big"
			       :render-transform
			       (lambda (_name _args _result)
				 (list :body
				       (make-string
					(1+ mevedel-pipeline--render-transform-max-data-size)
					?x)))))
			(warnings nil)
			out)
		   (cl-letf (((symbol-function 'display-warning)
			      (lambda (&rest args) (push args warnings))))
		     (mevedel-pipeline--step-render-transform
		      (list :tool tool :args nil :result "ok")
		      (lambda (c) (setq out c)) #'ignore))
		   (should warnings)
		   (should (null (plist-get out :render-data))))
		 :doc "transform errors warn and preserve context"
		 (let* ((tool (mevedel-tool--create
			       :name "Boom"
			       :render-transform
			       (lambda (_name _args _result)
					 (error "Bad transform"))))
			(warnings nil)
			out)
		   (cl-letf (((symbol-function 'display-warning)
			      (lambda (&rest args) (push args warnings))))
		     (mevedel-pipeline--step-render-transform
		      (list :tool tool :args nil :result "ok")
		      (lambda (c) (setq out c)) #'ignore))
		   (should warnings)
		   (should (null (plist-get out :render-data)))))

(mevedel-deftest mevedel-pipeline--build-steps/render-transform-order ()
		 ,test
		 (test)
		 :doc "render transform runs after handler and before persistence"
		 (let* ((tool (mevedel-tool--create
			       :name "Ordered"
			       :max-result-size 10))
			(steps (mevedel-pipeline--build-steps tool)))
		   (should (< (cl-position #'mevedel-pipeline--step-handler steps)
			      (cl-position #'mevedel-pipeline--step-repair-reminder steps)))
		   (should (< (cl-position #'mevedel-pipeline--step-repair-reminder steps)
			      (cl-position #'mevedel-pipeline--step-render-transform steps)))
		   (should (< (cl-position #'mevedel-pipeline--step-render-transform steps)
			      (cl-position #'mevedel-pipeline--step-persist steps)))))

;;
;;; File checkpoint capture

(mevedel-deftest mevedel-pipeline--snapshot-file-if-needed ()
  ,test
  (test)
  :doc "captures a regular file once at its pre-turn contents"
  (let* ((directory (make-temp-file "mevedel-snapshot-" t))
         (path (file-name-concat directory "tracked.el"))
         (snapshots (make-hash-table :test #'equal))
         (mevedel--current-request
          (mevedel-request--create :file-snapshots snapshots)))
    (unwind-protect
        (progn
          (write-region "before" nil path nil 'silent)
          (mevedel-pipeline--snapshot-file-if-needed
           mevedel--current-request path)
          (write-region "after" nil path nil 'silent)
          (mevedel-pipeline--snapshot-file-if-needed
           mevedel--current-request path)
          (should (equal "before" (gethash path snapshots))))
      (delete-directory directory t)))
  :doc "records a known gap when the pre-turn path cannot be read"
  (let* ((directory (make-temp-file "mevedel-snapshot-gap-" t))
         (snapshots (make-hash-table :test #'equal))
         (mevedel--current-request
          (mevedel-request--create :file-snapshots snapshots)))
    (unwind-protect
        (progn
          (mevedel-pipeline--snapshot-file-if-needed
           mevedel--current-request directory)
          (should (stringp
                   (plist-get (gethash directory snapshots) :gap))))
      (delete-directory directory t))))

(provide 'test-mevedel-pipeline)
;;; test-mevedel-pipeline.el ends here
