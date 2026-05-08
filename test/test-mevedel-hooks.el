;;; test-mevedel-hooks.el -- Tests for hook execution -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(require 'mevedel-hooks)


;;
;;; Helpers

(defun mevedel-hooks-test--await (starter)
  "Call STARTER with a callback and wait for its async result."
  (let ((done nil)
        result
        (deadline (+ (float-time) 5)))
    (funcall starter
             (lambda (value)
               (setq result value
                     done t)))
    (while (and (not done)
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (unless done
      (ert-fail "Timed out waiting for hook callback"))
    result))

(defun mevedel-hooks-test--workspace (root)
  "Return a fresh workspace for ROOT."
  (mevedel-workspace-get-or-create
   'project (format "hooks-test:%s" root) root "hooks-test"))

(defun mevedel-hooks-test--session (root)
  "Return a session rooted at ROOT."
  (mevedel-session-create
   "hooks-test" (mevedel-hooks-test--workspace root) root))

(defun mevedel-hooks-test--deny-fn (_event)
  "Test hook returning a deny decision."
  '(:permission-decision deny :permission-reason "blocked"))

(defun mevedel-hooks-test--rewrite-fn (_event)
  "Test hook returning updated input."
  '(:updated-input (:command "echo rewritten")))

(defvar mevedel-hooks-test--read-eval-ran nil)
(defvar mevedel-hooks-test--seen-event nil)
(defvar mevedel-hooks-test--seen-buffer nil)

(defun mevedel-hooks-test--capture-fn (event)
  "Capture EVENT for serial hook tests."
  (setq mevedel-hooks-test--seen-event event)
  nil)

(defun mevedel-hooks-test--suppress-output-fn (_event)
  "Return a reserved decision field for native hook tests."
  '(:suppress-output t))

(defun mevedel-hooks-test--stop-fn (_event)
  "Return a stop decision for terminal-behavior tests."
  '(:continue nil :stop-reason "stop"))

(defun mevedel-hooks-test--context-fn (_event)
  "Return additional context for terminal-behavior tests."
  '(:additional-context ("later")))

(defun mevedel-hooks-test--allow-fn (_event)
  "Return an allow decision for permission hook tests."
  '(:permission-decision allow))

(defun mevedel-hooks-test--buffer-fn (_event)
  "Capture the current buffer for async continuation tests."
  (setq mevedel-hooks-test--seen-buffer (current-buffer))
  nil)


;;
;;; Config and matching

(mevedel-deftest mevedel-hooks-normalize-rules
		 (:doc "normalizes Lisp hook rules and drops invalid handlers")
		 (should
		  (equal
		   (mevedel-hooks-normalize-rules
		    '((PreToolUse
		       ((:matcher "Bash"
				  :hooks ((:type command :command "echo ok")
					  (:type nope :command "ignored")
					  (:type elisp :function mevedel-hooks-test--deny-fn)))))
		      (NoSuchEvent
		       ((:matcher "*" :hooks ((:type command :command "ignored")))))))
		   '((PreToolUse
		      (:matcher "Bash"
				:hooks ((:type command :command "echo ok")
					(:type elisp :function mevedel-hooks-test--deny-fn))))))))

(mevedel-deftest mevedel-hooks--read-json-file
		 (:doc "reads Claude/Codex-style JSON hook config")
		 (let ((file (make-temp-file "mevedel-hooks" nil ".json")))
		   (unwind-protect
		       (progn
			 (with-temp-file file
			   (insert "{\n"
				   "  \"hooks\": {\n"
				   "    \"PreToolUse\": [{\n"
				   "      \"matcher\": \"Bash\",\n"
				   "      \"hooks\": [{\n"
				   "        \"type\": \"command\",\n"
				   "        \"command\": \"echo ok\",\n"
				   "        \"failClosed\": true\n"
				   "      }]\n"
				   "    }]\n"
				   "  }\n"
				   "}\n"))
			 (should
			  (equal
			   (mevedel-hooks--read-config-file file)
			   '((PreToolUse
			      (:matcher "Bash"
					:hooks ((:type command
						       :command "echo ok"
						       :fail-closed t))))))))
		     (delete-file file))))

(mevedel-deftest mevedel-hooks--read-lisp-file
		 (:doc "reads Lisp hook files with read evaluation disabled")
		 (let ((file (make-temp-file "mevedel-hooks" nil ".el"))
		       (mevedel-hooks-test--read-eval-ran nil))
		   (unwind-protect
		       (progn
			 (with-temp-file file
			   (insert "#.(progn "
				   "(setq mevedel-hooks-test--read-eval-ran t) "
				   "nil)"))
			 (should-not (mevedel-hooks--read-config-file file))
			 (should-not mevedel-hooks-test--read-eval-ran))
		     (delete-file file))))

(mevedel-deftest mevedel-hooks-effective-rules
		 (:doc "merges defcustom, user el/json, and trusted project el/json layers")
		 (let* ((root (make-temp-file "mevedel-hooks-ws" t))
			(user-dir (make-temp-file "mevedel-hooks-user" t))
			(workspace (mevedel-hooks-test--workspace root))
			(session (mevedel-session-create "main" workspace root))
			(mevedel-user-dir (file-name-as-directory user-dir))
			(mevedel-hooks-require-project-trust t)
			(mevedel-hook-rules
			 '((PreToolUse
			    ((:matcher "Read"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--rewrite-fn))))))))
		   (unwind-protect
		       (progn
			 (make-directory (file-name-concat root ".mevedel") t)
			 (make-directory user-dir t)
			 (with-temp-file (file-name-concat user-dir "hooks.el")
			   (prin1
			    '((PreToolUse
			       ((:matcher "Bash"
					  :hooks ((:type elisp
							 :function mevedel-hooks-test--deny-fn))))))
			    (current-buffer)))
			 (with-temp-file (file-name-concat user-dir "hooks.json")
			   (insert "{\"hooks\":{\"PostToolUse\":[{\"matcher\":\"Bash\","
				   "\"hooks\":[{\"type\":\"command\",\"command\":\"echo ok\"}]}]}}"))
			 (with-temp-file (file-name-concat root ".mevedel" "hooks.el")
			   (prin1
			    '((PermissionRequest
			       ((:matcher "*"
					  :hooks ((:type elisp
							 :function mevedel-hooks-test--deny-fn))))))
			    (current-buffer)))
			 (with-temp-file (file-name-concat root ".mevedel" "hooks.json")
			   (insert "{\"hooks\":{\"PermissionDenied\":[{\"matcher\":\"*\","
				   "\"hooks\":[{\"type\":\"command\",\"command\":\"echo deny\"}]}]}}"))
			 (should-not
			  (assq 'PermissionRequest
				(mevedel-hooks-effective-rules session workspace)))
			 (mevedel-hooks-trust-project workspace)
			 (let ((rules (mevedel-hooks-effective-rules session workspace)))
			   (should (assq 'PreToolUse rules))
			   (should (assq 'PostToolUse rules))
			   (should (assq 'PermissionRequest rules))
			   (should (assq 'PermissionDenied rules)))
			 (with-temp-file (file-name-concat root ".mevedel" "hooks.el")
			   (prin1
			    '((SessionStart
			       ((:matcher "startup"
					  :hooks ((:type elisp
							 :function mevedel-hooks-test--deny-fn))))))
			    (current-buffer)))
			 (should-not
			  (assq 'SessionStart
				(mevedel-hooks-effective-rules session workspace))))
		     (delete-directory root t)
		     (delete-directory user-dir t))))

(ert-deftest mevedel-hooks-matcher-matches-p ()
  "Match wildcard, exact alternatives, regex, and symbols."
  (dolist (case '((nil "Bash" t)
                  ("*" "Bash" t)
                  ("Bash|Read" "Read" t)
                  ("Bash|Read" "Edit" nil)
                  ("B.*" "Bash" t)
                  (Bash "Bash" t)))
    (pcase-let ((`(,matcher ,target ,expected) case))
      (should (eq (not (null (mevedel-hooks-matcher-matches-p
                              matcher target)))
	                  expected)))))

(mevedel-deftest mevedel-hooks--event-json
		 (:doc "serializes Lisp booleans and nil optional fields as JSON values")
		 (let* ((payload (json-parse-string
				  (mevedel-hooks--event-json
				   '(:hook-event-name SubagentStart
		     :background nil
		     :aggressive t
		     :agent-id nil
		     :tool-input (:command "true"
				   :dry-run :json-false)))
		  :object-type 'alist
		  :array-type 'list
		  :null-object :null
		  :false-object :false)))
		   (should (eq (alist-get 'background payload) :false))
		   (should (eq (alist-get 'aggressive payload) t))
		   (should (eq (alist-get 'agent_id payload) :null))
		   (should (eq (alist-get 'dry_run
					  (alist-get 'tool_input payload))
			       :false)))
		 (let ((table (make-hash-table :test #'equal)))
		   (puthash "nested_false" :json-false table)
		   (let* ((payload (json-parse-string
				    (mevedel-hooks--event-json
				     (list :tool-name "Read" :tool-input table))
				    :object-type 'alist
				    :array-type 'list
				    :false-object :false))
			  (tool-input (alist-get 'tool_input payload)))
		     (should (eq (alist-get 'nested_false tool-input nil nil
					    #'equal)
				 :false)))))


;;
;;; Decisions

(mevedel-deftest mevedel-hooks-merge-decisions
		 (:doc "merges contexts and keeps restrictive permission precedence")
		 (let* ((first '(:permission-decision allow
						      :additional-context ("a")
						      :updated-result "one"))
			(second '(:permission-decision ask
						       :additional-context "b"))
			(third '(:permission-decision deny
						      :permission-reason "no")))
		   (let ((decision
			  (mevedel-hooks-merge-decisions
			   (mevedel-hooks-merge-decisions
			    (mevedel-hooks-merge-decisions nil first)
			    second)
			   third)))
		     (should (eq (plist-get decision :permission-decision) 'deny))
		     (should (equal (plist-get decision :additional-context)
				    '("a" "b")))
		     (should (equal (plist-get decision :updated-result) "one"))
		     (should (equal (plist-get decision :permission-reason) "no")))))

(mevedel-deftest mevedel-hooks--parse-command-decision
		 (:doc "parses root and hookSpecificOutput JSON decisions")
		 ,test
		 (test)
		 (should
		  (equal
		   (mevedel-hooks--parse-command-decision
		    "{\"continue\":false,\"stopReason\":\"halt\"}")
		   '(:continue nil :stop-reason "halt")))
		 (should
		  (equal
		   (mevedel-hooks--parse-command-decision
		    "{\"hookSpecificOutput\":{\"permissionDecision\":\"deny\",\
\"permissionDecisionReason\":\"no\"}}")
		   '(:permission-reason "no" :permission-decision deny)))
		 (should
		  (equal
		   (mevedel-hooks--parse-command-decision
		    "{\"permissionDecision\":\"deny\",\"permissionReason\":\"documented\"}")
		   '(:permission-reason "documented" :permission-decision deny)))
		 (should
		  (equal
		   (mevedel-hooks--parse-command-decision
		    "{\"updatedInput\":{\"file_path\":\"x\",\"old_string\":\"a\"}}")
		   '(:updated-input (:file_path "x" :old_string "a"))))
			 (should
			  (equal
			   (mevedel-hooks--parse-command-decision
			    "{\"suppressOutput\":true}")
			   '(:hook-error "Unsupported hook decision field: suppressOutput")))
			 (should-not
			  (mevedel-hooks--parse-command-decision
			   "{\"suppressOutput\":false}")))

(mevedel-deftest mevedel-hooks--command-timeout
		 (:doc "defaults malformed timeouts and clamps numeric timeouts")
		 (let ((mevedel-hooks-command-timeout 30)
		       (mevedel-hooks-command-timeout-max 120))
		   (should (= (mevedel-hooks--command-timeout
			       '(:type command :command "x" :timeout "30"))
			      30))
		   (should (= (mevedel-hooks--command-timeout
			       '(:type command :command "x" :timeout 500))
			      120))))


;;
;;; Execution

(mevedel-deftest mevedel-hooks-run-event
		 (:doc "runs matching Elisp hooks and ignores nonmatching groups")
		 (let* ((root (make-temp-file "mevedel-hooks-run" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 '((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--rewrite-fn)))
			     (:matcher "Read"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--deny-fn))))))))
		   (unwind-protect
		       (let ((decision
			      (mevedel-hooks-test--await
			       (lambda (cb)
				 (mevedel-hooks-run-event
				  'PreToolUse
				  '(:tool-name "Bash" :tool-input (:command "echo old"))
				  cb session)))))
			 (should (equal decision
					'(:updated-input (:command "echo rewritten"))))
			 (should (= (length (mevedel-session-hook-log session)) 1)))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/serial-mutation
		 (:doc "passes updated input from one hook to later hooks")
		 (let* ((root (make-temp-file "mevedel-hooks-serial" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hooks-test--seen-event nil)
			(mevedel-hook-rules
			 '((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--rewrite-fn)
					       (:type elisp
						      :function mevedel-hooks-test--capture-fn))))))))
		   (unwind-protect
		       (progn
			 (mevedel-hooks-test--await
			  (lambda (cb)
			    (mevedel-hooks-run-event
			     'PreToolUse
			     '(:tool-name "Bash" :tool-input (:command "echo old"))
			     cb session)))
			 (should (equal (plist-get mevedel-hooks-test--seen-event
						   :tool-input)
					'(:command "echo rewritten"))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/command
		 (:doc "runs command hooks, parses JSON stdout, and logs stderr privately")
		 (let* ((root (make-temp-file "mevedel-hooks-command" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 `((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type command
						      :command ,(concat
								 "printf '%s' "
								 "\"{\\\"permissionDecision\\\":"
								 "\\\"ask\\\","
								 "\\\"permissionDecisionReason\\\":"
								 "\\\"review\\\"}\"")
						      :timeout 5))))))))
		   (unwind-protect
		       (let ((decision
			      (mevedel-hooks-test--await
			       (lambda (cb)
				 (mevedel-hooks-run-event
				  'PreToolUse
				  '(:tool-name "Bash" :tool-input (:command "echo hi"))
				  cb session)))))
			 (should (equal decision
					'(:permission-reason "review"
							     :permission-decision ask)))
			 (should (= (length (mevedel-session-hook-log session)) 1))
			 (should (equal (plist-get (car (mevedel-session-hook-log session))
						   :stdout-preview)
					"{\"permissionDecision\":\"ask\",\
\"permissionDecisionReason\":\"review\"}")))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/native-reserved-field
		 (:doc "logs reserved fields returned by native hook functions")
		 (let* ((root (make-temp-file "mevedel-hooks-native" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-pre-tool-use-functions
			 '(mevedel-hooks-test--suppress-output-fn)))
		   (unwind-protect
		       (let ((decision
			      (mevedel-hooks-test--await
			       (lambda (cb)
				 (mevedel-hooks-run-event
				  'PreToolUse
				  '(:tool-name "Bash" :tool-input (:command "echo hi"))
				  cb session)))))
			 (should-not decision)
			 (should (= (length (mevedel-session-hook-log session)) 1))
			 (should (eq (plist-get (car (mevedel-session-hook-log session))
						:status)
				     'error)))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/nonblocking-events-continue
		 (:doc "does not let unsupported stop decisions skip observer hooks")
		 (let* ((root (make-temp-file "mevedel-hooks-observer" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 '((PostToolUse
			    ((:matcher "Read"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--stop-fn)
					       (:type elisp
						      :function mevedel-hooks-test--context-fn))))))))
		   (unwind-protect
		       (let ((decision
			      (mevedel-hooks-test--await
			       (lambda (cb)
				 (mevedel-hooks-run-event
				  'PostToolUse
				  '(:tool-name "Read" :result "ok")
				  cb session)))))
			 (should (equal (plist-get decision :additional-context)
					'("later"))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/permission-request-terminal
		 (:doc "stops later PermissionRequest hooks after fail-closed stop decisions")
		 (let* ((root (make-temp-file "mevedel-hooks-pr-terminal" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 '((PermissionRequest
			    ((:matcher "Edit"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--stop-fn)
					       (:type elisp
						      :function mevedel-hooks-test--allow-fn))))))))
		   (unwind-protect
		       (let ((decision
			      (mevedel-hooks-test--await
			       (lambda (cb)
				 (mevedel-hooks-run-event
				  'PermissionRequest
				  '(:tool-name "Edit")
				  cb session)))))
			 (should (equal decision
					'(:continue nil :stop-reason "stop"))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/command-block
		 (:doc "maps exit-code 2 to event-specific blocking decisions")
		 (let* ((root (make-temp-file "mevedel-hooks-block" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 '((UserPromptSubmit
			    ((:matcher "*"
				       :hooks ((:type command
						      :command "printf blocked >&2; exit 2"
						      :timeout 5))))))))
		   (unwind-protect
		       (let ((decision
			      (mevedel-hooks-test--await
			       (lambda (cb)
				 (mevedel-hooks-run-event
				  'UserPromptSubmit
				  '(:prompt "hi")
				  cb session)))))
			 (should (equal decision
					'(:continue nil :stop-reason "blocked"))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/project-command-cwd
		 (:doc "runs project-sourced command hooks from the workspace root")
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-hooks-project-cwd" t)))
			(subdir (file-name-as-directory
				 (file-name-concat root "subdir")))
			session
			(mevedel-hook-rules
			 '((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type command
						      :command "pwd >&2"
						      :source project-file
						      :timeout 5))))))))
		   (unwind-protect
		       (progn
			 (make-directory subdir t)
			 (setq session
			       (mevedel-session-create
				"hooks-test"
				(mevedel-hooks-test--workspace root)
				subdir))
			 (mevedel-hooks-test--await
			  (lambda (cb)
			    (mevedel-hooks-run-event
			     'PreToolUse
			     (mevedel-hooks-event-plist
			      'PreToolUse session (mevedel-session-workspace session)
			      :cwd subdir
			      :tool-name "Bash"
			      :tool-input '(:command "echo hi"))
			     cb session)))
			 (should
			  (equal
			   (string-trim
			    (plist-get (car (mevedel-session-hook-log session))
				       :stderr-preview))
			   (directory-file-name root))))
			     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/command-continuation-buffer
		 (:doc "resumes later Elisp handlers in the original dispatch buffer")
		 (let* ((root (make-temp-file "mevedel-hooks-continuation" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hooks-test--seen-buffer nil)
			(mevedel-hook-rules
			 '((PostToolUse
			    ((:matcher "Read"
				       :hooks ((:type command
						      :command "printf '{\"systemMessage\":\"ok\"}'"
						      :timeout 5)
					       (:type elisp
						      :function
						      mevedel-hooks-test--buffer-fn))))))))
		   (unwind-protect
		       (with-temp-buffer
			 (let ((dispatch-buffer (current-buffer)))
			   (mevedel-hooks-test--await
			    (lambda (cb)
			      (mevedel-hooks-run-event
			       'PostToolUse
			       '(:tool-name "Read" :result "ok")
			       cb session)))
			   (should (eq mevedel-hooks-test--seen-buffer
				       dispatch-buffer))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/fail-closed
		 (:doc "command failures fail open by default and fail closed when requested")
		 (let* ((root (make-temp-file "mevedel-hooks-fail" t))
			(session (mevedel-hooks-test--session root)))
		   (unwind-protect
		       (let* ((open-decision
			       (let ((mevedel-hook-rules
				      '((PreToolUse
					 ((:matcher "Bash"
						    :hooks ((:type command
								   :command "exit 1"))))))))
				 (mevedel-hooks-test--await
				  (lambda (cb)
				    (mevedel-hooks-run-event
				     'PreToolUse '(:tool-name "Bash") cb session)))))
			      (closed-decision
			       (let ((mevedel-hook-rules
				      '((PreToolUse
					 ((:matcher "Bash"
						    :hooks ((:type command
								   :command "exit 1"
								   :fail-closed t))))))))
				 (mevedel-hooks-test--await
				  (lambda (cb)
				    (mevedel-hooks-run-event
				     'PreToolUse '(:tool-name "Bash") cb session))))))
			 (should-not open-decision)
			 (should (equal closed-decision
					'(:continue nil
						    :stop-reason "Hook exited with status 1"))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-dry
		 (:doc "reports matching hooks without executing them")
		 (let* ((root (make-temp-file "mevedel-hooks-dry" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 '((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type command :command "exit 99")
					       (:type elisp
						      :function mevedel-hooks-test--deny-fn)))
			     (:matcher "Read"
				       :hooks ((:type command :command "exit 99"))))))))
		   (unwind-protect
		       (let ((result
			      (mevedel-hooks-run-dry
			       'PreToolUse
			       '(:tool-name "Bash")
			       session)))
			 (should (eq (plist-get result :event) 'PreToolUse))
			 (should (equal (plist-get result :matcher-target) "Bash"))
			 (should (= (plist-get result :handler-count) 2))
			 (should-not (mevedel-session-hook-log session)))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-log-persistence
		 (:doc "persists sanitized hook log entries under the session directory")
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-hooks-log" t)))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 '((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--rewrite-fn))))))))
		   (unwind-protect
		       (progn
			 (setf (mevedel-session-save-path session) root)
			 (mevedel-hooks-test--await
			  (lambda (cb)
			    (mevedel-hooks-run-event
			     'PreToolUse '(:tool-name "Bash") cb session)))
			 (let ((file (mevedel-hooks-log-path session)))
			   (should (file-readable-p file))
			   (with-temp-buffer
			     (insert-file-contents file)
			     (goto-char (point-min))
			     (let ((entry (read (current-buffer))))
			       (should (eq (plist-get entry :event) 'PreToolUse))
			       (should (eq (plist-get entry :status) 'ok))))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-surface-blocking-decision
		 (:doc "surfaces blocking hook decisions and system messages to the user")
		 (let* ((root (make-temp-file "mevedel-hooks-surface" t))
			(session (mevedel-hooks-test--session root))
			(messages nil)
			(mevedel-hooks-slow-threshold nil)
			(mevedel-hook-rules
			 '((PreToolUse
			    ((:matcher "Read"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--deny-fn))))))))
		   (unwind-protect
		       (cl-letf (((symbol-function 'message)
				  (lambda (fmt &rest args)
				    (push (apply #'format fmt args) messages))))
			 (mevedel-hooks-test--await
			  (lambda (cb)
			    (mevedel-hooks-run-event
			     'PreToolUse '(:tool-name "Read") cb session)))
			 (should (member "mevedel: PreToolUse hook blocked: blocked"
					 messages)))
		     (delete-directory root t)))
		 (let* ((root (make-temp-file "mevedel-hooks-surface" t))
			(session (mevedel-hooks-test--session root))
			(messages nil)
			(mevedel-hooks-slow-threshold nil)
			(mevedel-hook-rules
			 '((PostToolUse
			    ((:matcher "Read"
				       :hooks ((:type elisp
						      :function
						      (lambda (_event)
							'(:system-message "formatted result"))))))))))
		   (unwind-protect
		       (cl-letf (((symbol-function 'message)
				  (lambda (fmt &rest args)
				    (push (apply #'format fmt args) messages))))
			 (mevedel-hooks-test--await
			  (lambda (cb)
			    (mevedel-hooks-run-event
			     'PostToolUse '(:tool-name "Read") cb session)))
			 (should (member "mevedel: PostToolUse hook: formatted result"
					 messages)))
		     (delete-directory root t))))

(provide 'test-mevedel-hooks)
;;; test-mevedel-hooks.el ends here
