;;; test-mevedel-pipeline.el --- Tests for pipeline engine -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-pipeline)
(require 'mevedel-permissions)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)
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
     (list (lambda (ctx next)
             (push 1 order)
             (funcall next ctx))
           (lambda (ctx next)
             (push 2 order)
             (funcall next ctx))
           (lambda (ctx next)
             (push 3 order)
             (funcall next (plist-put ctx :result "final"))))
     (lambda (_r) (push 'done order))
     nil)
    (should (equal (nreverse order) '(1 2 3 done))))
  :doc "context threads through steps"
  (let (result)
    (mevedel-pipeline--run
     (list (lambda (ctx next)
             (funcall next (plist-put ctx :count 1)))
           (lambda (ctx next)
             (funcall next (plist-put ctx :count
                                     (1+ (plist-get ctx :count)))))
           (lambda (ctx next)
             (funcall next (plist-put ctx :result
                                     (plist-get ctx :count)))))
     (lambda (r) (setq result r))
     nil)
    (should (equal result 2)))
  :doc "error in step calls callback with error string"
  (let (result)
    (mevedel-pipeline--run
     (list (lambda (_ctx _next)
             (error "Something broke")))
     (lambda (r) (setq result r))
     nil)
    (should (string-prefix-p "Error:" result)))
  :doc "validation error produces error callback"
  (let (result)
    (mevedel-pipeline--run
     (list (lambda (_ctx _next)
             (signal 'mevedel-validation-error '("Bad input"))))
     (lambda (r) (setq result r))
     nil)
    (should (string-match-p "Bad input" result)))
  :doc "permission denied produces error callback"
  (let (result)
    (mevedel-pipeline--run
     (list (lambda (_ctx _next)
             (signal 'mevedel-permission-denied '("Not allowed"))))
     (lambda (r) (setq result r))
     nil)
    (should (string-match-p "Permission denied" result))
    (should (string-match-p "Not allowed" result)))
  :doc "async step suspends and resumes"
  (let (result saved-next saved-ctx)
    (mevedel-pipeline--run
     (list (lambda (ctx next)
             ;; Save continuation for later
             (setq saved-next next saved-ctx ctx))
           (lambda (ctx next)
             (funcall next (plist-put ctx :result "after-async"))))
     (lambda (r) (setq result r))
     nil)
    ;; Not yet called
    (should-not result)
    ;; Resume
    (funcall saved-next saved-ctx)
    (should (equal result "after-async")))
  :doc "error after async step still calls callback"
  (let (result saved-next saved-ctx)
    (mevedel-pipeline--run
     (list (lambda (ctx next)
             (setq saved-next next saved-ctx ctx))
           (lambda (_ctx _next)
             (error "Async failure")))
     (lambda (r) (setq result r))
     nil)
    (funcall saved-next saved-ctx)
    (should (string-prefix-p "Error:" result))))


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
     ctx (lambda (_c) (setq called t)))
    (should called))
  :doc "signals validation error for missing required arg"
  (let* ((tool (mevedel-tool--create
                :name "TestTool"
                :args '((name string :required "Name"))))
         (ctx (list :tool tool :args nil)))
    (should-error
     (mevedel-pipeline--step-validate ctx #'ignore)
     :type 'mevedel-validation-error))
  :doc "signals validation error for wrong type"
  (let* ((tool (mevedel-tool--create
                :name "TestTool"
                :args '((count integer :required "Count"))))
         (ctx (list :tool tool :args '(:count "not-a-number"))))
    (should-error
     (mevedel-pipeline--step-validate ctx #'ignore)
     :type 'mevedel-validation-error))
  :doc "passes with nil args spec"
  (let* ((tool (mevedel-tool--create :name "NoArgs" :args nil))
         (ctx (list :tool tool :args nil))
         called)
    (mevedel-pipeline--step-validate
     ctx (lambda (_c) (setq called t)))
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
     ctx (lambda (c) (setq result-ctx c)))
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
     ctx (lambda (c) (setq result-ctx c)))
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
     ctx (lambda (c) (setq result-ctx c)))
    (should-not result-ctx)
    (funcall saved-cb "deferred-result")
    (should (equal (plist-get result-ctx :result) "deferred-result"))))


;;
;;; Step list builder

(mevedel-deftest mevedel-pipeline--build-steps ()
  ,test
  (test)
  :doc "read-only tool has validate, permission, and handler"
  (let* ((tool (mevedel-tool--create
                :name "ReadTool"
                :read-only-p t))
         (steps (mevedel-pipeline--build-steps tool)))
    (should (= (length steps) 3))
    (should (eq (nth 0 steps) #'mevedel-pipeline--step-validate))
    (should (eq (nth 1 steps) #'mevedel-pipeline--step-permission))
    (should (eq (nth 2 steps) #'mevedel-pipeline--step-handler)))
  :doc "write tool includes snapshot step"
  (let* ((tool (mevedel-tool--create
                :name "WriteTool"
                :read-only-p nil))
         (steps (mevedel-pipeline--build-steps tool)))
    (should (= (length steps) 4))
    (should (eq (nth 0 steps) #'mevedel-pipeline--step-validate))
    (should (eq (nth 1 steps) #'mevedel-pipeline--step-permission))
    (should (eq (nth 2 steps) #'mevedel-pipeline--step-snapshot))
    (should (eq (nth 3 steps) #'mevedel-pipeline--step-handler)))
  :doc "includes persist step when max-result-size is set"
  (let* ((tool (mevedel-tool--create
                :name "WithPersist"
                :read-only-p t
                :max-result-size 1000))
         (steps (mevedel-pipeline--build-steps tool)))
    (should (= 4 (length steps)))
    (should (eq (car (last steps)) #'mevedel-pipeline--step-persist)))
  :doc "omits persist step when max-result-size is nil"
  (let* ((tool (mevedel-tool--create
                :name "NoPersist"
                :read-only-p t
                :max-result-size nil))
         (steps (mevedel-pipeline--build-steps tool)))
    (should (= 3 (length steps)))))


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
     ctx (lambda (_c) (setq called t)))
    (should called))
  :doc "signals permission-denied when rules deny"
  (let* ((tool (mevedel-tool--create
                :name "Edit"
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules '(("Edit" :action deny)))
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'default))
    (should-error
     (mevedel-pipeline--step-permission ctx #'ignore)
     :type 'mevedel-permission-denied))
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
     ctx (lambda (_c) (setq called t)))
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
     ctx (lambda (_c) (setq called t)))
    (should called))
  :doc "denies non-read-only tool in plan mode"
  (let* ((tool (mevedel-tool--create
                :name "Edit"
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'plan))
    (should-error
     (mevedel-pipeline--step-permission ctx #'ignore)
     :type 'mevedel-permission-denied))
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
     ctx (lambda (_c) (setq called t)))
    (should called))
  :doc "reads session rules from buffer-local"
  (let* ((tool (mevedel-tool--create
                :name "Edit"
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'default)
         (mevedel--session (mevedel-session--create
                            :name "test"
                            :permission-rules '(("Edit" :action allow))))
         called)
    (mevedel-pipeline--step-permission
     ctx (lambda (_c) (setq called t)))
    (should called)))


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
    (should (string-match-p "Handler exploded" result))))


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
         (tool (mevedel-tool--create :name "TestTool" :max-result-size 100))
         (result (make-string 500 ?x))
         (persisted (mevedel-pipeline--persist-result result tool ws)))
    (unwind-protect
        (progn
          ;; Preview should contain the XML wrapper
          (should (string-prefix-p "<persisted-output>" persisted))
          (should (string-suffix-p "</persisted-output>" persisted))
          ;; Preview should mention the size
          (should (string-match-p "500 chars" persisted))
          ;; The persisted file should exist and contain the full result
          (let ((files (directory-files
                        (file-name-concat tmpdir ".mevedel" "tool-results")
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
         (tool (mevedel-tool--create :name "TestTool" :max-result-size 100))
         ;; Result with clear line breaks for newline-boundary cutting
         (result (mapconcat (lambda (_) (make-string 79 ?a))
                            (number-sequence 1 100) "\n"))
         (persisted (mevedel-pipeline--persist-result result tool ws)))
    (unwind-protect
        (progn
          ;; Preview should be much smaller than the full result
          (should (< (length persisted) (length result)))
          ;; Should contain the "..." truncation marker
          (should (string-match-p "\\.\\.\\." persisted)))
      (delete-directory tmpdir t))))

(mevedel-deftest mevedel-pipeline--truncate-result ()
  ,test
  (test)
  :doc "truncates large result and mentions no workspace"
  (let* ((tool (mevedel-tool--create :name "BigTool" :max-result-size 100))
         (result (make-string 5000 ?x))
         (truncated (mevedel-pipeline--truncate-result result tool)))
    (should (< (length truncated) (length result)))
    (should (string-match-p "no workspace available" truncated))
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
     ctx (lambda (c) (setq next-ctx c)))
    (should (equal (plist-get next-ctx :result)
                   (plist-get ctx :result))))
  :doc "passes through when result is within limit"
  (let* ((tool (mevedel-tool--create :name "SmallResult" :max-result-size 1000))
         (ctx (list :tool tool :result "short"))
         next-ctx)
    (mevedel-pipeline--step-persist
     ctx (lambda (c) (setq next-ctx c)))
    (should (equal "short" (plist-get next-ctx :result))))
  :doc "passes through when result is an error"
  (let* ((tool (mevedel-tool--create :name "ErrTool" :max-result-size 10))
         (ctx (list :tool tool :result "Error: something broke with a lot of text"))
         next-ctx)
    (mevedel-pipeline--step-persist
     ctx (lambda (c) (setq next-ctx c)))
    (should (string-prefix-p "Error:" (plist-get next-ctx :result))))
  :doc "persists result when over limit"
  (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
         (ws (mevedel-workspace--create :root tmpdir))
         (mevedel--session (mevedel-session--create :workspace ws))
         (tool (mevedel-tool--create :name "BigResult" :max-result-size 100))
         (big-result (make-string 500 ?y))
         (ctx (list :tool tool :result big-result))
         next-ctx)
    (unwind-protect
        (progn
          (mevedel-pipeline--step-persist
           ctx (lambda (c) (setq next-ctx c)))
          (should (string-prefix-p "<persisted-output>"
                                   (plist-get next-ctx :result)))
          ;; File should exist on disk
          (should (directory-files
                   (file-name-concat tmpdir ".mevedel" "tool-results")
                   nil "\\.txt$")))
      (delete-directory tmpdir t)))
  :doc "uses global cap when tool limit exceeds it"
  (let* ((tmpdir (make-temp-file "mevedel-test-ws-" t))
         (ws (mevedel-workspace--create :root tmpdir))
         (mevedel--session (mevedel-session--create :workspace ws))
         ;; Tool declares 100000 but global cap is 50000
         (tool (mevedel-tool--create :name "HighLimit" :max-result-size 100000))
         ;; Result is 60000 chars: above the 50K global cap but below the
         ;; tool's declared 100K
         (big-result (make-string 60000 ?z))
         (ctx (list :tool tool :result big-result))
         next-ctx)
    (unwind-protect
        (progn
          (mevedel-pipeline--step-persist
           ctx (lambda (c) (setq next-ctx c)))
          ;; Should be persisted because 60K > 50K global cap
          (should (string-prefix-p "<persisted-output>"
                                   (plist-get next-ctx :result))))
      (delete-directory tmpdir t)))
  :doc "truncates when no workspace is available"
  (let* ((mevedel--session nil)
         (tool (mevedel-tool--create :name "NoWS" :max-result-size 10))
         (ctx (list :tool tool :result (make-string 5000 ?w)))
         next-ctx)
    (mevedel-pipeline--step-persist
     ctx (lambda (c) (setq next-ctx c)))
    ;; Should truncate to preview size (no workspace to persist to)
    (should (< (length (plist-get next-ctx :result)) 5000))
    (should (string-match-p "no workspace available" (plist-get next-ctx :result)))))

(provide 'test-mevedel-pipeline)
;;; test-mevedel-pipeline.el ends here
