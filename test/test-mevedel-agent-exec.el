;;; test-mevedel-agent-exec.el --- Tests for sub-agent runtime -*- lexical-binding: t -*-

;;; Commentary:

;; Regression coverage for the extracted sub-agent runtime (spec 18).
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
(require 'mevedel-hooks)
(require 'mevedel-tools)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


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
			   (setq-local gptel--system-message "old"))
			 (mevedel-agent-exec--apply-request-locals
			  buf
			  `((gptel-tools . ,tools)
			    (gptel-use-tools . t)
			    (gptel--system-message . "agent system")))
			 (with-current-buffer buf
			   (should (eq gptel-tools tools))
			   (should (eq gptel-use-tools t))
			   (should (equal gptel--system-message "agent system"))))
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
				 (gptel--system-message "parent system")
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

		 :doc "keeps all items and calls mevedel-view-rerender"
		 (let* ((agent (mevedel-agent--create :name "explorer"
						      :description "Explore"))
			(inv (mevedel-agent-invocation--create :agent agent))
			(parent-buf (generate-new-buffer " *mev-agent-activity-parent*"))
			(view-buf (generate-new-buffer " *mev-agent-activity-view*"))
			(renders 0)
			(old-cap (and (boundp 'mevedel-view-agent-activity-max)
				      mevedel-view-agent-activity-max)))
		   (unwind-protect
		       (progn
			 (setq mevedel-view-agent-activity-max 2)
			 (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
			 (with-current-buffer parent-buf
			   (setq-local mevedel--view-buffer view-buf))
			 (cl-letf (((symbol-function 'mevedel-view-rerender)
				    (lambda (&optional _buffer)
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
		     (when old-cap
		       (setq mevedel-view-agent-activity-max old-cap))
		     (when (buffer-live-p view-buf) (kill-buffer view-buf))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

		 :doc "records allowed activity item types without rewriting their type"
		 (let* ((agent (mevedel-agent--create :name "explorer"
						      :description "Explore"))
			(inv (mevedel-agent-invocation--create :agent agent))
			(parent-buf (generate-new-buffer " *mev-agent-activity-parent*"))
			(view-buf (generate-new-buffer " *mev-agent-activity-view*"))
			(old-cap (and (boundp 'mevedel-view-agent-activity-max)
				      mevedel-view-agent-activity-max))
			(types '(tool-start tool-finish tool-error waiting message status)))
		   (unwind-protect
		       (progn
			 (setq mevedel-view-agent-activity-max 10)
			 (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
			 (with-current-buffer parent-buf
			   (setq-local mevedel--view-buffer view-buf))
			 (cl-letf (((symbol-function 'mevedel-view-rerender)
				    (lambda (&optional _buffer) nil)))
			   (dolist (type types)
			     (mevedel-agent-exec--record-activity
			      inv (list :type type :summary (symbol-name type)))))
			 (should (equal types
					(mapcar (lambda (item) (plist-get item :type))
						(mevedel-agent-invocation-activity inv))))
			 (should (cl-every
				  (lambda (item) (numberp (plist-get item :time)))
				  (mevedel-agent-invocation-activity inv))))
		     (when old-cap
		       (setq mevedel-view-agent-activity-max old-cap))
		     (when (buffer-live-p view-buf) (kill-buffer view-buf))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))


(mevedel-deftest mevedel-agent-exec--handle-wait-activity ()
		 ,test
		 (test)

		 :doc "records waiting once for consecutive WAIT cycles"
		 (let* ((agent (mevedel-agent--create :name "explorer"
						      :description "Explore"))
			(inv (mevedel-agent-invocation--create :agent agent))
			(parent-buf (generate-new-buffer " *mev-agent-wait-parent*"))
			(view-buf (generate-new-buffer " *mev-agent-wait-view*"))
			(old-cap (and (boundp 'mevedel-view-agent-activity-max)
				      mevedel-view-agent-activity-max)))
		   (unwind-protect
		       (progn
			 (setq mevedel-view-agent-activity-max 10)
			 (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
			 (with-current-buffer parent-buf
			   (setq-local mevedel--view-buffer view-buf))
			 (cl-letf (((symbol-function
				     'mevedel-agent-exec--invocation-from-fsm)
				    (lambda (_fsm) inv))
				   ((symbol-function 'mevedel-view-rerender)
				    (lambda (&optional _buffer) nil)))
			   (mevedel-agent-exec--handle-wait-activity 'fsm)
			   (mevedel-agent-exec--handle-wait-activity 'fsm))
			 (should (= 1 (length (mevedel-agent-invocation-activity inv))))
			 (should (eq 'waiting
				     (plist-get
				      (car (mevedel-agent-invocation-activity inv))
				      :type))))
		     (when old-cap
		       (setq mevedel-view-agent-activity-max old-cap))
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
			   (should (string-match-p "rate_limit_error" body)))
			 (with-current-buffer parent-buf
			   (should-not (assoc "explorer--ERRS"
					      mevedel-tools--agents-fsm)))
			 (should (eq 'WAIT (gptel-fsm-state parent-fsm))))
		     (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))


(provide 'test-mevedel-agent-exec)
;;; test-mevedel-agent-exec.el ends here
