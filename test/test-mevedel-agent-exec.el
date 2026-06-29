;;; test-mevedel-agent-exec.el --- Tests for sub-agent runtime -*- lexical-binding: t -*-

;;; Commentary:

;; Regression coverage for the extracted sub-agent runtime.
;; The callback contract is the high-value surface: upstream
;; `gptel-agent--task' fired its main callback on every streamed chunk
;; and dropped gptel's `t' completion signal, so the parent's
;; tool_result was frozen at the first chunk.  These tests pin the
;; corrected contract: chunks accumulate, MAIN-CB fires exactly once on
;; `t', and tool-use guards hold.

;;; Code:

(require 'gptel)
(require 'mevedel-structs)
(require 'mevedel-agents)
(require 'mevedel-agent-exec)
(require 'mevedel-session-persistence)
(require 'mevedel-hooks)
(require 'mevedel-tools)
(require 'mevedel-tool-task)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar gptel-org-branching-context)


;;
;;; Test helpers

(defmacro mevedel-agent-exec-test--with-callback (callback-sym &rest body)
  "Bind CALLBACK-SYM to a freshly-constructed runner callback and run BODY.
The bound callback writes into a fresh partial-cell (seeded with an
empty prefix for readability), and a locally-bound `fired' accumulates
the arguments of every `main-cb' invocation so the test can assert
fire-count and payload."
  (declare (indent 1) (debug t))
  `(let* ((fired nil)
          (main-cb (lambda (&rest args) (push args fired)))
          (partial-cell (list ""))
          (,callback-sym (mevedel-agent-exec--make-callback
                          main-cb "explorer" "Test task"
                          (point-min-marker) partial-cell)))
     (ignore main-cb partial-cell)
     ,@body))


;;
;;; Transcript prompt injections

(mevedel-deftest mevedel-agent-exec--handlers
  (:doc "TOOL state keeps gptel's status and pending-tool cleanup handlers")
  (let ((tool-entry (assq 'TOOL mevedel-agent-exec--handlers)))
    (should (member #'gptel--update-tool-call (cdr tool-entry)))
    (should (member #'gptel--handle-tool-use (cdr tool-entry)))
    (should (member #'gptel--update-tool-ask (cdr tool-entry)))))

(mevedel-deftest mevedel-agent-exec--insert-injected-prompt ()
  ,test
  (test)

  :doc "appended user-role injection clears accidental gptel properties"
  (let* ((buf (generate-new-buffer " *mev-agent-inject-append*"))
         (agent (mevedel-agent--create :name "explorer"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "explorer--inject"
               :buffer buf))
         (block (propertize "<agent-message from=\"main\">\nhello\n</agent-message>"
                            'gptel 'response
                            'invisible t)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert (propertize "Assistant text\n" 'gptel 'response)))
          (cl-letf (((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t)))
            (mevedel-agent-exec--insert-injected-prompt inv block))
          (with-current-buffer buf
            (goto-char (point-min))
            (should (search-forward "<agent-message" nil t))
            (should-not (get-text-property (match-beginning 0) 'gptel))
            (should-not (get-text-property (match-beginning 0) 'invisible))))
      (when (buffer-live-p buf) (kill-buffer buf))))

  :doc "prepended user-role injection clears accidental gptel properties"
  (let* ((buf (generate-new-buffer " *mev-agent-inject-prepend*"))
         (agent (mevedel-agent--create :name "explorer"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "explorer--inject"
               :buffer buf))
         (block (propertize "Reminder text" 'gptel '(tool . "call_1"))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "* Agent Task: inspect\nbody\n"))
          (cl-letf (((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t)))
            (mevedel-agent-exec--insert-injected-prompt inv block 'prepend))
          (with-current-buffer buf
            (goto-char (point-min))
            (should (search-forward "Reminder text" nil t))
            (should-not (get-text-property (match-beginning 0) 'gptel))
            (should (< (match-beginning 0)
                       (progn
                         (search-forward "* Agent Task:")
                         (match-beginning 0))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))


;;
;;; Callback contract

(mevedel-deftest mevedel-agent-exec--make-callback ()
		 ,test
		 (test)

		 :doc "streaming: chunks accumulate; MAIN-CB fires exactly once on `t'"
		 ;; `:stream t' in info suppresses the non-streaming terminal path so
		 ;; chunks accumulate without firing until gptel sends the terminal
		 ;; `t' event.
		 (mevedel-agent-exec-test--with-callback cb
							 (let ((info '(:stream t)))
							   (funcall cb "Found" info)
							   (funcall cb " 2 defcustom" info)
							   (funcall cb "s with :set" info)
							   ;; Before the `t' signal, MAIN-CB must not have fired.
							   (should (null fired))
							   (funcall cb t info)
							   (should (= 1 (length fired)))
							   (should (equal "Found 2 defcustoms with :set"
									  (car (car fired))))))

		 :doc "streaming: chunks do not rebuild partial text before terminal"
		 (let* ((fired nil)
			(main-cb (lambda (&rest args) (push args fired)))
			(partial-cell (list "prefix: "))
			(cb (mevedel-agent-exec--make-callback
			     main-cb "explorer" "Test task"
			     (point-min-marker) partial-cell))
			(info '(:stream t)))
		   (funcall cb "first" info)
		   (funcall cb " second" info)
		   (should (equal "prefix: " (car partial-cell)))
		   (funcall cb t info)
		   (should (= 1 (length fired)))
		   (should (equal "prefix: first second" (car (car fired))))
		   (should (equal "prefix: first second" (car partial-cell))))

		 :doc "streaming: transcript final response overrides noisy accumulator"
		 (let ((buf (generate-new-buffer " *mev-agent-exec-final-response*")))
		   (unwind-protect
		       (let* ((agent (mevedel-agent--create :name "explorer"))
			      (inv (mevedel-agent-invocation--create
				    :agent agent
				    :buffer buf)))
			 (with-current-buffer buf
			   (insert "tool call\n")
			   (put-text-property (point-min) (point) 'gptel '(tool . "read"))
			   (let ((start (point)))
			     (insert "first answer\n")
			     (put-text-property start (point) 'gptel 'response))
			   (let ((start (point)))
			     (insert "tool result\n")
			     (put-text-property start (point) 'gptel '(tool . "read")))
			   (let ((start (point)))
			     (insert "*Summary:* final answer only\n")
			     (put-text-property start (point) 'gptel '(response))))
			 (mevedel-agent-exec-test--with-callback cb
								 (let ((info (list :stream t
										   :mevedel-agent-invocation inv)))
								   (funcall cb "Explore result for task\n\n" info)
								   (funcall cb "(:name \"Read\" ...)\nlarge tool output\n" info)
								   (funcall cb "*Summary:* final answer only\n" info)
								   (funcall cb t info)
								   (should (= 1 (length fired)))
								   (should (equal "*Summary:* final answer only"
										  (car (car fired)))))))
		     (when (buffer-live-p buf) (kill-buffer buf))))

		 :doc "non-streaming (single chunk + `t'): single delivery unchanged"
		 (mevedel-agent-exec-test--with-callback cb
							 (funcall cb "complete response" nil)
							 (funcall cb t nil)
							 (should (= 1 (length fired)))
							 (should (equal "complete response" (car (car fired)))))

		 :doc "non-streaming (:stream absent, no `t'): string branch fires MAIN-CB"
		 ;; gptel removes `:stream' from info when the request is non-streaming
		 ;; and never sends a terminal `t' event -- see gptel-request.el 2864.
		 ;; The string must be treated as terminal here.
		 (mevedel-agent-exec-test--with-callback cb
							 (funcall cb "complete non-streaming response" nil)
							 (should (= 1 (length fired)))
							 (should (equal "complete non-streaming response"
									(car (car fired)))))

		 :doc "non-streaming tool-use turn: string does not fire MAIN-CB"
		 ;; In non-streaming, intermediate tool-use turns also arrive as
		 ;; strings; we must only finalize on the final text-only turn.
		 (mevedel-agent-exec-test--with-callback cb
							 (funcall cb "reasoning about tool call" (list :tool-use '((:name "Read"))))
							 (should (null fired))
							 (funcall cb " continuation after tools" nil)
							 (should (= 1 (length fired)))
							 (should (equal "reasoning about tool call continuation after tools"
									(car (car fired)))))

		 :doc "streaming: single chunk on non-streaming path does not double-fire on later `t'"
		 ;; Defensive: if gptel ever delivers both a non-streaming string and
		 ;; a `t' (shouldn't happen in current gptel), the fired latch keeps
		 ;; MAIN-CB at one invocation.
		 (mevedel-agent-exec-test--with-callback cb
							 (funcall cb "one-shot" nil)
							 (funcall cb t nil)
							 (should (= 1 (length fired)))
							 (should (equal "one-shot" (car (car fired)))))

		 :doc "streaming chunk with `:stream' t: string branch defers to `t'"
		 ;; With `:stream' set, per-chunk firing is disabled; the 't signal
		 ;; remains the terminal.  Mirrors real streaming backends.
		 (mevedel-agent-exec-test--with-callback cb
							 (funcall cb "partial " (list :stream t))
							 (funcall cb "text" (list :stream t))
							 (should (null fired))
							 (funcall cb t (list :stream t))
							 (should (= 1 (length fired)))
							 (should (equal "partial text" (car (car fired)))))

		 :doc "empty response (`t' with no prior `stringp'): single empty delivery"
		 (mevedel-agent-exec-test--with-callback cb
							 (funcall cb t nil)
							 (should (= 1 (length fired)))
							 (should (equal "" (car (car fired)))))

		 :doc "tool-use guard: `t' while :tool-use is non-nil does not fire MAIN-CB"
		 ;; Streaming scenario: `:stream t' keeps string chunks off the
		 ;; non-streaming terminal path; the `t' signal with :tool-use set
		 ;; must not fire either.
		 (mevedel-agent-exec-test--with-callback cb
							 (funcall cb "intermediate chunk" (list :stream t))
							 (funcall cb t (list :stream t :tool-use '((:name "Read"))))
							 (should (null fired))
							 ;; Completion after tool-use clears fires exactly once.
							 (funcall cb " continuation" (list :stream t))
							 (funcall cb t (list :stream t))
							 (should (= 1 (length fired)))
							 (should (equal "intermediate chunk continuation"
									(car (car fired)))))

		 :doc "error (`nil'): MAIN-CB receives formatted error string once"
		 (mevedel-agent-exec-test--with-callback cb
							 (funcall cb nil (list :error "boom"))
							 (should (= 1 (length fired)))
							 (should (string-match-p "could not finish" (car (car fired)))))

		 :doc "error (`nil'): foreground error includes safe transcript path"
			 (let* ((session (mevedel-session--create :name "main"))
				(tempdir (file-name-as-directory
					  (make-temp-file "mevedel-agent-error" t)))
				(rel-path "agents/explorer--error.chat.org")
				(abs-path (expand-file-name rel-path tempdir))
				(buf (generate-new-buffer " *mev-agent-error-transcript*"))
				(agent (mevedel-agent--create :name "explorer"))
				(inv (mevedel-agent-invocation--create
				      :agent agent
				      :agent-id "explorer--error1234567890abcdef"
				      :description "Test task"
				      :parent-session session
				      :buffer buf
				      :transcript-relative-path rel-path
				      :transcript-status 'running))
				(fired nil)
				(main-cb (lambda (&rest args) (push args fired)))
				(cb (mevedel-agent-exec--make-callback
				     main-cb "explorer" "Test task"
				     (point-min-marker)
				     (list "Explorer result for task: Test task\n\n"))))
			   (unwind-protect
			       (progn
				 (setf (mevedel-session-save-path session) tempdir)
				 (make-directory (file-name-directory abs-path) t)
				 (with-temp-file abs-path
				   (insert "saved transcript"))
				 (cl-letf (((symbol-function
					      'mevedel-agent-exec--save-transcript-buffer)
					     (lambda (_invocation) t))
					   ((symbol-function
					      'mevedel-agent-exec--handle-update)
					     (lambda (_invocation) nil))
					   ((symbol-function
					      'mevedel-agent-exec--run-stop-hook)
					     (lambda (_invocation _status) nil))
					   ((symbol-function
					      'mevedel-session-persistence--update-transcript-entry)
					     (lambda (_session _agent-id _updates) nil)))
				   (funcall cb nil (list :error "Malformed JSON in response."
							 :mevedel-agent-invocation inv)))
				 (should (= 1 (length fired)))
				 (let ((body (car (car fired))))
				   (should (string-match-p "Malformed JSON in response" body))
				   (should (string-match-p
					    (regexp-quote (format "Transcript: %s" abs-path))
					    body))
					   (should (string-match-p
					    (regexp-quote (format "Read(file_path=%S)" abs-path))
					    body))))
				     (when (file-directory-p tempdir) (delete-directory tempdir t))
				     (when (buffer-live-p buf) (kill-buffer buf))))

			 :doc "error (`nil'): fallback partial is inlined without scaffold"
			 (let* ((buf (generate-new-buffer " *mev-agent-error-partial*"))
				(agent (mevedel-agent--create :name "explorer"))
				(inv (mevedel-agent-invocation--create
				      :agent agent
				      :agent-id "explorer--partial-error"
				      :description "Test task"
				      :buffer buf
				      :transcript-status 'running))
				(fired nil)
				(main-cb (lambda (&rest args) (push args fired)))
				(cb (mevedel-agent-exec--make-callback
				     main-cb "explorer" "Test task"
				     (point-min-marker)
				     (list "Explorer result for task: Test task\n\n"))))
			   (unwind-protect
			       (cl-letf (((symbol-function
					  'mevedel-agent-exec--save-transcript-buffer)
					 (lambda (_invocation) t))
					((symbol-function
					  'mevedel-agent-exec--handle-update)
					 (lambda (_invocation) nil))
					((symbol-function
					  'mevedel-agent-exec--run-stop-hook)
					 (lambda (_invocation _status) nil)))
				 (funcall cb "partial analysis before parser failure"
					  (list :stream t :mevedel-agent-invocation inv))
				 (funcall cb nil (list :error "boom"
						       :mevedel-agent-invocation inv))
				 (should (= 1 (length fired)))
				 (let ((body (car (car fired))))
				   (should (string-match-p "Partial response recovered" body))
				   (should (string-match-p
					    "partial analysis before parser failure" body))
				   (should-not (string-match-p
						"Explorer result for task" body))))
			     (when (buffer-live-p buf) (kill-buffer buf))))

			 :doc "abort (`'abort'): MAIN-CB receives formatted abort string once"
		 (mevedel-agent-exec-test--with-callback cb
							 (funcall cb 'abort nil)
							 (should (= 1 (length fired)))
							 (should (string-match-p "aborted by the user" (car (car fired)))))

		 :doc "terminal-ready-p: text-only `t' with pending bg-agents does not fire MAIN-CB"
		 ;; Regression for the foreground-stash hang.  When the sub-agent
		 ;; produces an intermediate text turn ("Waiting for the third
		 ;; explorer...") while background children are still running, the
		 ;; FSM parks in BWAIT and resumes later for the real synthesis turn.
		 ;; Without `terminal-ready-p' the `fired' latch committed on the
		 ;; intermediate turn and prevented finalize on the synthesis turn.
		 (let ((buf (generate-new-buffer " *mev-agent-exec-tready*")))
		   (unwind-protect
		       (with-current-buffer buf
			 (let* ((agent (mevedel-agent--create :name "explorer"))
				(inv (mevedel-agent-invocation--create
				      :agent agent
				      :background-agents '("explorer--child-1"))))
			   (mevedel-agent-exec-test--with-callback cb
								   (let ((info (list :stream t
										     :mevedel-agent-invocation inv)))
								     ;; Intermediate text turn: bg-agents non-empty,
								     ;; finalize must NOT fire.
								     (funcall cb "Waiting for child... " info)
								     (funcall cb t info)
								     (should (null fired))
								     ;; Drain bg-agents, simulate BWAIT-RESUME final turn.
								     (setf (mevedel-agent-invocation-background-agents inv)
									   nil)
								     (funcall cb "final synthesis text" info)
								     (funcall cb t info)
								     ;; Now finalize should fire exactly once with the
								     ;; full accumulated partial.
								     (should (= 1 (length fired)))
								     (should (equal "Waiting for child... final synthesis text"
										    (car (car fired))))))))
		     (when (buffer-live-p buf) (kill-buffer buf))))

		 :doc "terminal-ready-p: pending mailbox messages also defer finalize"
		 ;; Same gate covers the case where a bg child has finished and pushed
		 ;; its result into the mailbox but the WAIT message-inject handler
		 ;; has not yet drained it.  An intermediate `t' during that window
		 ;; would have fired the latch under the old implementation.
		 (let ((buf (generate-new-buffer " *mev-agent-exec-tready-msg*")))
		   (unwind-protect
		       (with-current-buffer buf
			 (let* ((agent (mevedel-agent--create :name "explorer"))
				(inv (mevedel-agent-invocation--create
				      :agent agent
				      :messages (list (list :from "child"
							    :body "result")))))
			   (mevedel-agent-exec-test--with-callback cb
								   (let ((info (list :stream t
										     :mevedel-agent-invocation inv)))
								     (funcall cb "intermediate" info)
								     (funcall cb t info)
								     (should (null fired))
								     (setf (mevedel-agent-invocation-messages inv) nil)
								     (funcall cb t info)
								     (should (= 1 (length fired)))))))
		     (when (buffer-live-p buf) (kill-buffer buf))))

		 :doc "terminal-ready-p: legacy callers without an invocation are ready"
		 ;; The fallback at the predicate's tail keeps missing-invocation info
		 ;; firing on the first text-only `t' as before.
		 (mevedel-agent-exec-test--with-callback cb
							 (let ((info (list :stream t)))           ; no :context at all
							   (funcall cb "result" info)
							   (funcall cb t info)
							   (should (= 1 (length fired))))))


;;
;;; Request buffer configuration

(mevedel-deftest mevedel-agent-exec--allocate-agent-buffer ()
			 ,test
			 (test)

			 :doc "forces agent data buffers to use linear gptel Org context"
			 (let* ((root (file-name-as-directory
				       (make-temp-file "mevedel-agent-parent-" t)))
				(workspace (mevedel-workspace--create
					    :type 'project
					    :id root
					    :root root
					    :name "agent"))
				(session (mevedel-session-create "main" workspace root))
				(parent-buf (generate-new-buffer " *mev-agent-parent*"))
				(agent (mevedel-agent--create :name "explorer"))
				(inv (mevedel-agent-invocation--create
				      :agent agent
				      :agent-id "explorer--linear"))
				agent-buf)
			   (unwind-protect
			       (progn
				 (with-current-buffer parent-buf
				   (setq-local mevedel--session session)
				   (setq-local mevedel--workspace workspace))
				 (cl-letf (((symbol-function 'gptel-mode) #'ignore))
				   (let ((gptel-org-branching-context t))
				     (setq agent-buf
					   (mevedel-agent-exec--allocate-agent-buffer
					    inv parent-buf))))
				 (with-current-buffer agent-buf
				   (should-not gptel-org-branching-context)))
			     (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
			     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
			     (delete-directory root t))))

(mevedel-deftest mevedel-agent-exec--apply-request-locals ()
		 ,test
		 (test)

		 :doc "copies active preset values onto the per-agent request buffer"
		 ;; `gptel-request' copies `gptel-tools' and related request state
		 ;; from the request buffer into its prompt buffer.  Dynamic
		 ;; `gptel-with-preset' bindings alone are not enough for the
		 ;; per-agent buffer path.
		 (let ((buf (generate-new-buffer " *mev-agent-exec-locals*"))
		       (tools '(new-tools)))
		   (unwind-protect
		       (progn
			 (with-current-buffer buf
			   (setq-local gptel-tools '(old-tools))
			   (setq-local gptel-use-tools nil)
			   (setq-local gptel-system-prompt "old"))
			 (mevedel-agent-exec--apply-request-locals
			  buf
			  `((gptel-tools . ,tools)
			    (gptel-use-tools . t)
			    (gptel-system-prompt . "agent system")))
			 (with-current-buffer buf
			   (should (eq gptel-tools tools))
			   (should (eq gptel-use-tools t))
			   (should (equal gptel-system-prompt "agent system"))))
			     (when (buffer-live-p buf) (kill-buffer buf)))))


(mevedel-deftest mevedel-agent-exec--refresh-initial-transcript-state ()
  ,test
  (test)

  :doc "marks persisted transcript buffers dirty and saves them"
  (let* ((buf (generate-new-buffer " *mev-agent-refresh-transcript*"))
         (inv (mevedel-agent-invocation--create
               :buffer buf
               :transcript-relative-path "agents/verifier.chat.org"))
         saved)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "prompt")
            (set-buffer-modified-p nil))
          (cl-letf (((symbol-function
                     'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (arg)
                       (setq saved
                             (list arg
                                   (with-current-buffer buf
                                     (buffer-modified-p))))
                       t)))
            (mevedel-agent-exec--refresh-initial-transcript-state inv))
          (should (equal (car saved) inv))
          (should (cadr saved)))
      (when (buffer-live-p buf) (kill-buffer buf))))

  :doc "skips ephemeral agent buffers without a transcript path"
  (let* ((buf (generate-new-buffer " *mev-agent-refresh-ephemeral*"))
         (inv (mevedel-agent-invocation--create :buffer buf))
         saved)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "prompt")
            (set-buffer-modified-p nil))
          (cl-letf (((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_arg) (setq saved t))))
            (mevedel-agent-exec--refresh-initial-transcript-state inv))
          (should-not saved)
          (with-current-buffer buf
            (should-not (buffer-modified-p))))
      (when (buffer-live-p buf) (kill-buffer buf)))))


(mevedel-deftest mevedel-agent-exec--run-lifecycle-hooks ()
			 ,test
			 (test)

		 :doc "fires SubagentStart and SubagentStop with invocation metadata"
		 (let* ((root (make-temp-file "mevedel-agent-hooks" t))
			(workspace (mevedel-workspace-get-or-create
				    'project "agent-hooks" root "agent-hooks"))
			(session (mevedel-session-create "main" workspace root))
			(agent (mevedel-agent--create :name "explorer"))
			(parent-buffer (generate-new-buffer " *mev-agent-parent-hooks*"))
			(inv (mevedel-agent-invocation--create
			      :agent agent
			      :agent-id "explorer-1"
			      :description "inspect hooks"
			      :parent-session session
			      :parent-data-buffer parent-buffer))
			start-event
			stop-event)
		   (unwind-protect
		       (progn
			 (with-current-buffer parent-buffer
			   (setq-local mevedel-subagent-stop-functions
				       (list (lambda (event)
					       (setq stop-event event)
					       nil))))
			 (let ((mevedel-subagent-start-functions
				(list (lambda (event)
					(setq start-event event)
					'(:additional-context ("extra start context"))))))
			   (let ((decision
				  (mevedel-agent-exec--run-start-hook-sync
				   "explorer" "inspect hooks" "prompt body" inv)))
			     (should (equal (plist-get start-event :agent-type) "explorer"))
			     (should (equal (plist-get start-event :agent-id) "explorer-1"))
			     (should (equal (plist-get start-event :prompt) "prompt body"))
			     (should (equal (plist-get decision :additional-context)
					    '("extra start context")))))
			 (setf (mevedel-agent-invocation-terminal-reason inv) "done")
			 (with-temp-buffer
			   (mevedel-agent-exec--run-stop-hook inv 'completed))
			 (should (equal (plist-get stop-event :agent-type) "explorer"))
			 (should (equal (plist-get stop-event :agent-id) "explorer-1"))
			 (should (eq (plist-get stop-event :status) 'completed))
			 (should (equal (plist-get stop-event :terminal-reason) "done")))
			     (delete-directory root t)
			     (when (buffer-live-p parent-buffer)
			       (kill-buffer parent-buffer))
			     (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-agent-exec--run ()
		 ,test
		 (test)

		 :doc "inherits parent include-reasoning into per-agent request buffers"
		 (let ((parent-buf (generate-new-buffer " *mev-agent-parent*"))
		       (agent-buf (generate-new-buffer " *mev-agent-child*"))
		       captured-buffer
		       captured-include-reasoning)
		   (unwind-protect
		       (progn
			 (with-current-buffer parent-buf
			   (let ((gptel-agent-preset '(:include-reasoning nil))
				 (mevedel-agent-exec--agents
				  '(("explorer" :include-reasoning nil)))
				 (gptel-include-reasoning t)
				 (gptel-stream nil)
				 (gptel-backend nil)
				 (gptel-model 'test-model)
				 (gptel-system-prompt "parent system")
				 (gptel-use-tools nil)
				 (gptel-tools nil)
				 (gptel-use-context nil)
				 (gptel-context nil)
				 (gptel-use-curl nil)
				 (gptel-temperature nil)
				 (gptel-max-tokens nil)
				 (gptel-cache nil)
				 (gptel--request-params nil)
				 (gptel--fsm-last nil))
			     (cl-letf (((symbol-function 'gptel-request)
					(lambda (&optional _prompt &rest _args)
					  (setq captured-buffer (current-buffer))
					  (setq captured-include-reasoning
						gptel-include-reasoning)))
				       ((symbol-function 'gptel--update-status)
					#'ignore))
			       (mevedel-agent-exec--run
				#'ignore "explorer" "count defcustoms" "prompt"
				nil agent-buf))))
			 (should (eq captured-buffer agent-buf))
			 (should (eq captured-include-reasoning t))
			 (with-current-buffer agent-buf
			   (should (eq gptel-include-reasoning t))))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
		     (when (buffer-live-p agent-buf) (kill-buffer agent-buf)))))

		 :doc "falls back to the invocation agent spec when buffer registry is missing"
		 (let* ((parent-buf (generate-new-buffer " *mev-agent-parent*"))
			(agent-buf (generate-new-buffer " *mev-agent-child*"))
			(agent (mevedel-agent--create
				:name "reviewer"
				:system-prompt "agent system"
				:tools '((:tool "Bash"))))
			(inv (mevedel-agent-invocation--create :agent agent))
			captured-system
			captured-use-tools
			captured-tools)
		   (unwind-protect
		       (progn
			 (require 'mevedel-tool-exec)
			 (mevedel-tool-exec--register)
			 (with-current-buffer parent-buf
			   (let ((gptel-agent-preset nil)
				 (mevedel-agent-exec--agents nil)
				 (gptel-include-reasoning nil)
				 (gptel-stream nil)
				 (gptel-backend nil)
				 (gptel-model 'test-model)
				 (gptel-system-prompt "parent system")
				 (gptel-use-tools nil)
				 (gptel-tools nil)
				 (gptel-use-context nil)
				 (gptel-context nil)
				 (gptel-use-curl nil)
				 (gptel-temperature nil)
				 (gptel-max-tokens nil)
				 (gptel-cache nil)
				 (gptel--request-params nil)
				 (gptel--fsm-last nil))
			     (cl-letf (((symbol-function 'gptel-request)
					(lambda (&optional _prompt &rest _args)
					  (setq captured-system
						gptel-system-prompt)
					  (setq captured-use-tools gptel-use-tools)
					  (setq captured-tools gptel-tools)))
				       ((symbol-function 'gptel--update-status)
					#'ignore))
			       (mevedel-agent-exec--run
				#'ignore "reviewer" "review changes" "prompt"
				inv agent-buf))))
			 (should (equal captured-system "agent system"))
			 (should captured-use-tools)
			 (should (member "Bash" (mapcar #'gptel-tool-name
							 captured-tools))))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
		     (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))


(mevedel-deftest mevedel-agent-exec--force-initial-tool-use-p ()
		 ,test
		 (test)
		 :doc "only first-turn coordinator invocations force initial tool use"
		 (let* ((agent (mevedel-agent--create :name "coordinator"))
			(inv (mevedel-agent-invocation--create
			      :agent agent
			      :turn-count 0)))
		   (should (mevedel-agent-exec--force-initial-tool-use-p
			    "coordinator" inv))
		   (setf (mevedel-agent-invocation-turn-count inv) 1)
		   (should-not (mevedel-agent-exec--force-initial-tool-use-p
				"coordinator" inv))
		   (setf (mevedel-agent-invocation-turn-count inv) 0)
		   (should-not (mevedel-agent-exec--force-initial-tool-use-p
				"explorer" inv))
		   (should-not (mevedel-agent-exec--force-initial-tool-use-p
				"coordinator" nil))))


(mevedel-deftest mevedel-agent-exec--clear-forced-tool-choice ()
		 ,test
		 (test)
		 :doc "clears provider-specific forced-tool-choice fields"
		 (let* ((data (list :input "x"
				    :tool_choice "required"
				    :tools [openai-tools]
				    :toolConfig (list :toolChoice '(:any ())
						      :tools [bedrock-tools])))
			(fsm (gptel-make-fsm :info (list :data data))))
		   (mevedel-agent-exec--clear-forced-tool-choice fsm)
		   (let* ((updated (plist-get (gptel-fsm-info fsm) :data))
			  (tool-config (plist-get updated :toolConfig)))
		     (should-not (plist-member updated :tool_choice))
		     (should (equal (plist-get updated :tools) [openai-tools]))
		     (should-not (plist-member tool-config :toolChoice))
		     (should (equal (plist-get tool-config :tools) [bedrock-tools]))))

		 :doc "removes empty Gemini toolConfig after dropping force config"
		 (let* ((data (list :contents []
				    :toolConfig
				    (list :functionCallingConfig '(:mode "ANY"))))
			(fsm (gptel-make-fsm :info (list :data data))))
		   (mevedel-agent-exec--clear-forced-tool-choice fsm)
		   (should-not (plist-member (plist-get (gptel-fsm-info fsm) :data)
					     :toolConfig))))


(mevedel-deftest mevedel-agent-exec--invocation-from-info ()
		 ,test
		 (test)

		 :doc "prefers the invocation stored directly on the FSM info plist"
		 (let* ((agent (mevedel-agent--create :name "explorer"))
			(inv (mevedel-agent-invocation--create :agent agent)))
		   (should (eq inv (mevedel-agent-exec--invocation-from-info
				    (list :mevedel-agent-invocation inv)))))

		 :doc "keeps overlay lookup as a compatibility fallback"
		 (let ((buf (generate-new-buffer " *mev-agent-exec-ov*")))
		   (unwind-protect
		       (with-current-buffer buf
			 (let* ((agent (mevedel-agent--create :name "explorer"))
				(inv (mevedel-agent-invocation--create :agent agent))
				(ov (make-overlay (point-min) (point-min))))
			   (overlay-put ov 'mevedel-agent-invocation inv)
			   (should (eq inv (mevedel-agent-exec--invocation-from-info
					    (list :context ov))))))
		     (when (buffer-live-p buf) (kill-buffer buf)))))


(mevedel-deftest mevedel-agent-exec--render-data-bounds ()
		 ,test
		 (test)

		 :doc "uses cached render-data markers without rescanning parent buffer"
		 (let ((buf (generate-new-buffer " *mev-agent-render-data-cache*")))
		   (unwind-protect
		       (with-current-buffer buf
			 (let* ((agent (mevedel-agent--create :name "explorer"))
				(inv (mevedel-agent-invocation--create :agent agent))
				(scans 0))
			   (insert (mevedel-pipeline--format-render-data-block
				    '(:agent-id "explorer--1" :status running)))
			   (mevedel-agent-exec--cache-render-data-bounds
			    inv (point-min) (point-max))
			   (cl-letf (((symbol-function
				       'mevedel-pipeline--find-render-data-block-by-agent-id)
				      (lambda (_agent-id)
					(cl-incf scans)
					nil)))
			     (let ((bounds
				    (mevedel-agent-exec--render-data-bounds
				     inv "explorer--1")))
			       (should (consp bounds))
			       (should (markerp (car bounds)))
			       (should (markerp (cdr bounds)))
			       (should (= 0 scans))))))
		     (when (buffer-live-p buf) (kill-buffer buf))))

		 :doc "rejects stale cached markers before falling back to scan"
		 (let ((buf (generate-new-buffer " *mev-agent-render-data-stale*")))
		   (unwind-protect
		       (with-current-buffer buf
			 (let* ((agent (mevedel-agent--create :name "explorer"))
				(inv (mevedel-agent-invocation--create :agent agent))
				(scans 0)
				(fallback-bounds nil)
				(block-end nil))
			   (insert (mevedel-pipeline--format-render-data-block
				    '(:agent-id "other--1" :status running)))
			   (setq block-end (point))
			   (insert "tail\n")
			   (mevedel-agent-exec--cache-render-data-bounds
			    inv (point-min) block-end)
			   (goto-char (point-max))
			   (setq fallback-bounds (cons (point) nil))
			   (insert (mevedel-pipeline--format-render-data-block
				    '(:agent-id "explorer--1" :status running)))
			   (setcdr fallback-bounds (point))
			   (cl-letf (((symbol-function
			       'mevedel-pipeline--find-render-data-block-by-agent-id)
			      (lambda (agent-id)
				(cl-incf scans)
				(and (equal agent-id "explorer--1")
				     fallback-bounds))))
			     (let ((bounds
				    (mevedel-agent-exec--render-data-bounds
				     inv "explorer--1")))
			       (should (markerp (car bounds)))
			       (should (markerp (cdr bounds)))
			       (should (= (car fallback-bounds)
					  (marker-position (car bounds))))
			       (should (= (cdr fallback-bounds)
					  (marker-position (cdr bounds))))
			       (should (= 1 scans))))))
		     (when (buffer-live-p buf) (kill-buffer buf)))))


(mevedel-deftest mevedel-agent-exec--finalize ()
		 ,test
		 (test)

		 :doc "writes sidecar after terminal activity is promoted to full history"
		 (let* ((parent-buf (generate-new-buffer " *mev-agent-finalize-parent*"))
			(agent (mevedel-agent--create :name "explorer"))
			(agent-id "explorer--finalize")
			(activity (cl-loop for i below 6
					   collect (list :type 'tool-finish
							 :tool-name "Read"
							 :summary (format "tool-%d" i))))
			(session (mevedel-session--create
				  :name "test"
				  :agent-transcripts
				  (list (cons agent-id
					      (list :status 'running
						    :activity (last activity 5))))))
			(inv (mevedel-agent-invocation--create
			      :agent agent
			      :agent-id agent-id
			      :parent-session session
			      :parent-data-buffer parent-buf
			      :background-p t
			      :activity activity))
			(written-entry nil))
		   (unwind-protect
		       (cl-letf (((symbol-function
				   'mevedel-agent-exec--save-transcript-buffer)
				  (lambda (_invocation) t))
				 ((symbol-function 'mevedel-agent-exec--handle-update)
				  (lambda (_invocation) t))
				 ((symbol-function
				   'mevedel-agent-exec--run-stop-hook)
				  (lambda (_invocation _status) t))
				 ((symbol-function
				   'mevedel-view-agent-live-transcript-finalize)
				  (lambda (_invocation) nil))
				 ((symbol-function
				   'mevedel-session-persistence--write-sidecar-now)
				  (lambda (write-session _buffer)
				    (setq written-entry
					  (copy-tree
					   (alist-get agent-id
						      (mevedel-session-agent-transcripts
						       write-session)
						      nil nil #'equal))))))
			 (mevedel-agent-exec--finalize inv 'completed)
			 (should written-entry)
			 (should (eq 'completed (plist-get written-entry :status)))
			 (let ((written-activity (plist-get written-entry :activity)))
			   (should (= 6 (length written-activity)))
			   (should (equal "tool-0"
					  (plist-get (car written-activity) :summary)))
			   (should-not
			    (seq-some (lambda (item)
					(eq (plist-get item :type) 'status))
				      written-activity))))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))


(mevedel-deftest mevedel-agent-exec--finalize-agent-tasks ()
  ,test
  (test)
  :doc "completed finalization persists cleanup of agent-owned tasks"
  (let* ((parent-buf (generate-new-buffer " *mev-agent-task-finalize-parent*"))
         (agent (mevedel-agent--create :name "explorer"))
         (agent-id "explorer--0123456789abcdef0123456789abcdef")
         (session (mevedel-session--create
                   :name "test"
                   :tasks (list
                           (mevedel-task--create
                            :id 1 :subject "agent open"
                            :status 'pending :owner agent-id)
                           (mevedel-task--create
                            :id 2 :subject "main open"
                            :status 'pending))
                   :task-status-notes
                   (list (cons agent-id
                               '(:note "Inspecting"
                                 :updated-turn 1
                                 :updated-at "now")))
                   :agent-transcripts
                   (list (cons agent-id
                               (list :status 'running)))))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id agent-id
               :parent-session session
               :parent-data-buffer parent-buf
               :background-p t))
         (written-tasks nil)
         (written-notes :unset)
         (written-turn :unset))
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-agent-exec--save-transcript-buffer)
                   (lambda (_invocation) t))
                  ((symbol-function 'mevedel-agent-exec--handle-update)
                   (lambda (_invocation) t))
                  ((symbol-function
                    'mevedel-agent-exec--run-stop-hook)
                   (lambda (_invocation _status) t))
                  ((symbol-function
                    'mevedel-view-agent-live-transcript-finalize)
                   (lambda (_invocation) nil))
                  ((symbol-function 'mevedel-tool-task--display-overlay)
                   (lambda () t))
                  ((symbol-function
                    'mevedel-session-persistence--write-sidecar-now)
                   (lambda (write-session _buffer)
                     (setq written-tasks
                           (mapcar #'copy-mevedel-task
                                   (mevedel-session-tasks
                                    write-session)))
                     (setq written-notes
                           (copy-tree
                            (mevedel-session-task-status-notes
                             write-session)))
                     (setq written-turn
                           (mevedel-session-last-task-write-turn
                            write-session)))))
          (mevedel-agent-exec--finalize inv 'completed)
          (should written-tasks)
          (should (eq 'completed
                      (mevedel-task-status (car written-tasks))))
          (should (eq 'pending
                      (mevedel-task-status (cadr written-tasks))))
          (should (= 1 written-turn))
          (should (null written-notes)))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))


(mevedel-deftest mevedel-agent-exec--handle-tret-save ()
		 ,test
		 (test)

		 :doc "debounces transcript saves between tool result cycles"
		 (let* ((agent (mevedel-agent--create :name "explorer"))
			(inv (mevedel-agent-invocation--create :agent agent))
			(fsm (gptel-make-fsm
			      :info (list :mevedel-agent-invocation inv)))
			(save-count 0)
			(mevedel-agent-transcript-save-debounce 10))
		   (unwind-protect
		       (cl-letf (((symbol-function
				   'mevedel-agent-exec--save-transcript-buffer)
				  (lambda (_invocation)
				    (cl-incf save-count))))
			 (mevedel-agent-exec--handle-tret-save fsm)
			 (should (= 0 save-count))
			 (should
			  (timerp
			   (mevedel-agent-invocation-transcript-save-timer inv)))
			 (mevedel-agent-exec--flush-transcript-save inv)
			 (should (= 1 save-count))
			 (should-not
			  (mevedel-agent-invocation-transcript-save-timer inv)))
		     (mevedel-agent-exec--cancel-transcript-save inv)))

		 :doc "zero debounce saves immediately"
		 (let* ((agent (mevedel-agent--create :name "explorer"))
			(inv (mevedel-agent-invocation--create :agent agent))
			(fsm (gptel-make-fsm
			      :info (list :mevedel-agent-invocation inv)))
			(save-count 0)
			(mevedel-agent-transcript-save-debounce 0))
		   (cl-letf (((symbol-function
			       'mevedel-agent-exec--save-transcript-buffer)
			      (lambda (_invocation)
				(cl-incf save-count))))
		     (mevedel-agent-exec--handle-tret-save fsm)
		     (should (= 1 save-count))
		     (should-not
		      (mevedel-agent-invocation-transcript-save-timer inv)))))


(mevedel-deftest mevedel-agent-exec--error-reason-from-info ()
		 ,test
		 (test)
		 :doc "joins HTTP status with plist :type and :message"
		 (let ((reason (mevedel-agent-exec--error-reason-from-info
				'(:status "429"
					  :error (:type "rate_limit_error"
							:message "Too many tokens")))))
		   (should (stringp reason))
		   (should (string-match-p "429" reason))
		   (should (string-match-p "rate_limit_error" reason))
		   (should (string-match-p "Too many tokens" reason)))

		 :doc "treats string :error as a message"
		 (let ((reason (mevedel-agent-exec--error-reason-from-info
				'(:status "500" :error "Internal server error"))))
		   (should (string-match-p "500" reason))
		   (should (string-match-p "Internal server error" reason)))

		 :doc "returns nil when :error is absent and :status is empty"
		 (should-not (mevedel-agent-exec--error-reason-from-info '(:status "")))
		 (should-not (mevedel-agent-exec--error-reason-from-info nil))

		 :doc "truncates long reasons at 200 characters"
		 (let* ((long (make-string 500 ?x))
			(reason (mevedel-agent-exec--error-reason-from-info
				 (list :status "429" :error long))))
		   (should (<= (length reason) 203))
		   (should (string-suffix-p "..." reason))))


(mevedel-deftest mevedel-agent-exec--record-activity ()
		 ,test
		 (test)

		 :doc "keeps all items and calls targeted agent refresh"
		 (let* ((agent (mevedel-agent--create :name "explorer"
						      :description "Explore"))
			(inv (mevedel-agent-invocation--create
			      :agent agent
			      :agent-id "explorer--activity"))
			(parent-buf (generate-new-buffer " *mev-agent-activity-parent*"))
			(view-buf (generate-new-buffer " *mev-agent-activity-view*"))
			(renders 0))
		   (unwind-protect
		       (progn
			 (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
			 (with-current-buffer parent-buf
			   (setq-local mevedel--view-buffer view-buf))
			 (cl-letf (((symbol-function 'mevedel-view-refresh-agent-rendering)
				    (lambda (_buffer _agent-id)
				      (cl-incf renders))))
			   (mevedel-agent-exec--record-activity
			    inv '(:type tool-start :summary "one"))
			   (mevedel-agent-exec--record-activity
			    inv '(:type tool-finish :summary "two"))
			   (mevedel-agent-exec--record-activity
			    inv '(:type waiting :summary "three")))
			 (let ((items (mevedel-agent-invocation-activity inv)))
			   (should (= 3 (length items)))
			   (should (equal "one" (plist-get (car items) :summary)))
			   (should (equal "two" (plist-get (cadr items) :summary)))
			   (should (equal "three" (plist-get (caddr items) :summary)))
			   (should (numberp (plist-get (car items) :time))))
			 (should (= 3 renders)))
		     (when (buffer-live-p view-buf) (kill-buffer view-buf))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

		 :doc "records allowed activity item types without rewriting their type"
		 (let* ((agent (mevedel-agent--create :name "explorer"
						      :description "Explore"))
			(inv (mevedel-agent-invocation--create
			      :agent agent
			      :agent-id "explorer--activity-types"))
			(parent-buf (generate-new-buffer " *mev-agent-activity-parent*"))
			(view-buf (generate-new-buffer " *mev-agent-activity-view*"))
			(types '(tool-start tool-finish tool-error waiting message status)))
		   (unwind-protect
		       (progn
			 (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
			 (with-current-buffer parent-buf
			   (setq-local mevedel--view-buffer view-buf))
			 (cl-letf (((symbol-function 'mevedel-view-refresh-agent-rendering)
				    (lambda (_buffer _agent-id) nil)))
			   (dolist (type types)
			     (mevedel-agent-exec--record-activity
			      inv (list :type type :summary (symbol-name type)))))
			 (should (equal types
					(mapcar (lambda (item) (plist-get item :type))
						(mevedel-agent-invocation-activity inv))))
			 (should (cl-every
				  (lambda (item) (numberp (plist-get item :time)))
				  (mevedel-agent-invocation-activity inv))))
		     (when (buffer-live-p view-buf) (kill-buffer view-buf))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))


(mevedel-deftest mevedel-agent-exec--handle-wait-activity ()
		 ,test
		 (test)

		 :doc "records waiting once for consecutive WAIT cycles"
		 (let* ((agent (mevedel-agent--create :name "explorer"
						      :description "Explore"))
			(inv (mevedel-agent-invocation--create
			      :agent agent
			      :agent-id "explorer--wait"))
			(parent-buf (generate-new-buffer " *mev-agent-wait-parent*"))
			(view-buf (generate-new-buffer " *mev-agent-wait-view*")))
		   (unwind-protect
		       (progn
			 (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
			 (with-current-buffer parent-buf
			   (setq-local mevedel--view-buffer view-buf))
			 (cl-letf (((symbol-function
				     'mevedel-agent-exec--invocation-from-fsm)
				    (lambda (_fsm) inv))
				   ((symbol-function 'mevedel-view-refresh-agent-rendering)
				    (lambda (_buffer _agent-id) nil)))
			   (mevedel-agent-exec--handle-wait-activity 'fsm)
			   (mevedel-agent-exec--handle-wait-activity 'fsm))
			 (should (= 1 (length (mevedel-agent-invocation-activity inv))))
			 (should (eq 'waiting
				     (plist-get
				      (car (mevedel-agent-invocation-activity inv))
				      :type))))
		     (when (buffer-live-p view-buf) (kill-buffer view-buf))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))


(mevedel-deftest mevedel-agent-exec--handle-errs-save
		 (:before-each (mevedel-workspace-clear-registry)
			       :after-each (mevedel-workspace-clear-registry))
		 ,test
		 (test)

		 :doc "background ERRS without callback reports to parent and resumes BWAIT"
		 (let* ((ws (mevedel-workspace-get-or-create
			     'project "/tmp/mae/" "/tmp/mae/" "mae"))
			(session (mevedel-session-create "main" ws))
			(parent-buf (generate-new-buffer " *mev-agent-errs-parent*"))
			(agent-buf (generate-new-buffer " *mev-agent-errs-child*"))
			(agent (mevedel-agent--create :name "explorer"
						      :description "Explore"))
			(inv (mevedel-agent-invocation--create
			      :agent agent
			      :agent-id "explorer--ERRS"
			      :description "survey"
			      :parent-context session
			      :parent-data-buffer parent-buf
			      :buffer agent-buf
			      :background-p t))
			(parent-fsm (gptel-make-fsm
				     :info (list :buffer parent-buf)
				     :handlers nil
				     :state 'BWAIT))
			(child-fsm (gptel-make-fsm
				    :info (list :buffer agent-buf
						:mevedel-agent-invocation inv
						:status "429"
						:error '(:type rate_limit_error
							       :message "too many requests"))
				    :handlers nil
				    :state 'ERRS)))
		   (unwind-protect
		       (progn
			 (setf (mevedel-agent-invocation-parent-fsm inv) parent-fsm)
			 (setf (mevedel-session-background-agents session)
			       '("explorer--ERRS"))
			 (with-current-buffer agent-buf
			   (let ((start (point)))
			     (insert "partial ERRS analysis")
			     (put-text-property start (point) 'gptel 'response)))
			 (with-current-buffer parent-buf
			   (setq-local mevedel-tools--agents-fsm
				       `(("explorer--ERRS" . ,child-fsm))))
			 (cl-letf (((symbol-function 'gptel--handle-error)
				    (lambda (_fsm) nil))
				   ((symbol-function
				     'mevedel-agent-exec--save-transcript-buffer)
				    (lambda (_invocation) t))
				   ((symbol-function 'mevedel-agent-exec--handle-update)
				    (lambda (_invocation) nil)))
			   (mevedel-agent-exec--handle-errs-save child-fsm))
			 (should (eq 'error
				     (mevedel-agent-invocation-transcript-status inv)))
			 (should (mevedel-agent-invocation-background-result-reported-p inv))
			 (should (null (mevedel-session-background-agents session)))
			 (should (= 1 (length (mevedel-session-messages session))))
			 (let ((body (plist-get (car (mevedel-session-messages session))
						:body)))
			   (should (string-match-p
				    "<agent-result agent-id=\"explorer--ERRS\"" body))
			   (should (string-match-p "could not finish" body))
			   (should (string-match-p "rate_limit_error" body))
			   (should (string-match-p "Partial response recovered" body))
			   (should (string-match-p "partial ERRS analysis" body)))
			 (with-current-buffer parent-buf
			   (should-not (assoc "explorer--ERRS"
					      mevedel-tools--agents-fsm)))
			 (should (eq 'WAIT (gptel-fsm-state parent-fsm))))
		     (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))


(provide 'test-mevedel-agent-exec)
;;; test-mevedel-agent-exec.el ends here
