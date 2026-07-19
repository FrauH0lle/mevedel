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
(require 'mevedel-agent-conversation)
(require 'mevedel-agent-exec)
(require 'mevedel-permissions)
(require 'mevedel-sandbox)
(require 'mevedel-session-persistence)
(require 'mevedel-hooks)
(require 'mevedel-skills-prompt)
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
          (default-invocation
           (mevedel-agent-invocation--create
            :path "/root/test_agent"
            :agent (mevedel-agent--create :name "explorer")
            :agent-id "explorer--callback-test"))
          (raw-callback (mevedel-agent-exec--make-callback
                         main-cb "explorer" "Test task"
                         (point-min-marker) partial-cell))
          (,callback-sym
           (lambda (response info &rest rest)
             (unless (plist-get info :mevedel-agent-invocation)
               (setq info (plist-put info :mevedel-agent-invocation
                                     default-invocation)))
             (apply raw-callback response info rest))))
     (ignore main-cb partial-cell)
     ,@body))


;;
;;; Transcript prompt injections

(mevedel-deftest mevedel-agent-exec--handlers ()
  ,test
  (test)
  :doc "TOOL state keeps gptel's status and pending-tool cleanup handlers"
  (let ((tool-entry (assq 'TOOL mevedel-agent-exec--handlers)))
    (should (member #'gptel--update-tool-call (cdr tool-entry)))
    (should (member #'gptel--handle-tool-use (cdr tool-entry)))
    (should (member #'gptel--update-tool-ask (cdr tool-entry))))

  :doc "WAIT routes continuations through the shared compaction gate"
  (let ((wait-entry (assq 'WAIT mevedel-agent-exec--handlers)))
    (should (member #'mevedel--compact-handle-agent-wait
                    (cdr wait-entry)))
    (should-not (member #'gptel--handle-wait (cdr wait-entry)))))


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
			(inv (mevedel-agent-invocation--create
			      :path "/root/test_agent"
			      :agent (mevedel-agent--create :name "explorer")))
			(cb (mevedel-agent-exec--make-callback
			     main-cb "explorer" "Test task"
			     (point-min-marker) partial-cell))
			(info (list :stream t :mevedel-agent-invocation inv)))
		   (progn
		     (funcall cb "first" info)
		     (funcall cb " second" info)
		     (should (equal "prefix: " (car partial-cell)))
		     (funcall cb t info)
		     (should (= 1 (length fired)))
		     (should (equal "prefix: first second" (car (car fired))))
		     (should (equal "prefix: first second" (car partial-cell)))))

		 :doc "streaming: transcript final response overrides noisy accumulator"
		 (let ((buf (generate-new-buffer " *mev-agent-exec-final-response*")))
		   (unwind-protect
		       (let* ((agent (mevedel-agent--create :name "explorer"))
			      (inv (mevedel-agent-invocation--create
				    :path "/root/test_agent"
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

		 :doc "error (`nil') emits one structured terminal event"
		 (mevedel-agent-exec-test--with-callback cb
							 (funcall cb nil (list :error "boom"))
							 (should (= 1 (length fired)))
							 (let ((event (car (car fired))))
							   (should (eq 'error
							               (plist-get event
							                          :mevedel-agent-terminal-status)))
							   (should (equal "boom"
							                  (plist-get event :error-details)))))

		 :doc "abort (`'abort') emits one structured terminal event"
		 (mevedel-agent-exec-test--with-callback cb
							 (funcall cb 'abort nil)
							 (should (= 1 (length fired)))
							 (let ((event (car (car fired))))
							   (should (eq 'aborted
							               (plist-get event
							                          :mevedel-agent-terminal-status)))
							   (should (string-match-p
							            "aborted by the user"
							            (plist-get event :response)))))


		 )


;;
;;; Request buffer configuration


(mevedel-deftest mevedel-agent-exec--policy-for-invocation
  ()
  ,test
  (test)
  :doc "passes direct skill model and effort over workload policy"
  (let* ((inv (mevedel-agent-invocation--create
               :path "/root/test_agent"
               :skill-model-override '(:tier fast)
               :skill-effort-override 'high))
         captured)
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (&rest args)
                 (setq captured args)
                 '(:model chosen))))
      (should (equal '(:model chosen)
                     (mevedel-agent-exec--policy-for-invocation
                      "reviewer" inv))))
    (should (equal '("reviewer" (:tier fast) high) captured))))

(mevedel-deftest mevedel-agent-exec--request-preset
  ()
  ,test
  (test)
  :doc "resolves a named gptel-agent preset"
  (let ((gptel-agent-preset 'named-preset)
        (gptel-include-reasoning 'ignore)
        captured)
    (cl-letf (((symbol-function 'gptel-get-preset)
               (lambda (name)
                 (setq captured name)
                 '(:temperature 0.4))))
      (let ((preset
             (mevedel-agent-exec--request-preset "default" nil)))
        (should (eq 'named-preset captured))
        (should (eq t (plist-get preset :use-tools)))
        (should (equal 0.4 (plist-get preset :temperature)))
        (should (eq 'ignore (plist-get preset :include-reasoning))))))

  :doc "copies an inline preset before adding request fields"
  (let* ((inline '(:temperature 0.2))
         (gptel-agent-preset inline)
         (preset (mevedel-agent-exec--request-preset "default" nil)))
    (should (equal '(:temperature 0.2) inline))
    (should (equal 0.2 (plist-get preset :temperature))))

  :doc "rejects an invalid preset value"
  (let ((gptel-agent-preset 42))
    (should-error
     (mevedel-agent-exec--request-preset "default" nil)
     :type 'error)))

(mevedel-deftest mevedel-agent-exec--request-snapshot
  ()
  ,test
  (test)
  :doc "captures every inherited request local through one schema"
  (let* ((gptel--num-messages-to-send 7)
         (gptel--request-params '(:custom "parent"))
         (gptel--schema '(:type object))
         (gptel-backend 'parent-backend)
         (gptel-cache t)
         (gptel-context '(("context.txt" . "parent context")))
         (gptel-include-reasoning 'ignore)
         (gptel-max-tokens 321)
         (gptel-mode t)
         (gptel-model 'parent-model)
         (gptel-reasoning-effort 'low)
         (gptel-stream nil)
         (gptel-system-prompt "Parent system.")
         (gptel-temperature 0.25)
         (gptel-tools '(parent-tool))
         (gptel-track-media t)
         (gptel-track-response nil)
         (gptel-use-context 'system)
         (gptel-use-curl nil)
         (gptel-use-tools 'force)
         (context gptel-context)
         (mevedel-model-tiers '((custom :provider "Backend:model")))
         (mevedel-model-workloads '((explorer :tier custom)))
         (tiers mevedel-model-tiers)
         (workloads mevedel-model-workloads)
         (snapshot
          (mevedel-agent-exec--request-snapshot
           '(:backend frozen-backend :model frozen-model :effort high))))
    (should (= (length mevedel-agent-exec--request-local-symbols)
               (length snapshot)))
    (should (eq 'frozen-backend (alist-get 'gptel-backend snapshot)))
    (should (eq 'frozen-model (alist-get 'gptel-model snapshot)))
    (should (eq 'high (alist-get 'gptel-reasoning-effort snapshot)))
    (should (= 7 (alist-get 'gptel--num-messages-to-send snapshot)))
    (should (equal '(:type object) (alist-get 'gptel--schema snapshot)))
    (should (eq t (alist-get 'gptel-track-media snapshot)))
    (should-not (alist-get 'gptel-track-response snapshot))
    (should (equal '(:custom "parent")
                   (alist-get 'gptel--request-params snapshot)))
    (should-not (eq context (alist-get 'gptel-context snapshot)))
    (should (equal tiers (alist-get 'mevedel-model-tiers snapshot)))
    (should-not (eq tiers (alist-get 'mevedel-model-tiers snapshot)))
    (should (equal workloads (alist-get 'mevedel-model-workloads snapshot)))
    (should-not
     (eq workloads (alist-get 'mevedel-model-workloads snapshot)))))

(mevedel-deftest mevedel-agent-exec-freeze-configuration
  (:before-each (mevedel-tools-register))
  ,test
  (test)
  :doc "captures exact request policy, instructions, tools, and inherited config"
  (let* ((agent
          (mevedel-agent--create
           :name "freeze_test"
           :description "Freeze request config"
           :tools '((:tool "Read"))
           :system-prompt "Frozen system."))
         (invocation (mevedel-agent-invocation-create agent))
         (gptel-agent-preset nil)
         (gptel--num-messages-to-send 7)
         (gptel--schema '(:type object))
         (gptel-context '(("context.txt" . "parent context")))
         (gptel-use-context 'system)
         (gptel-track-media t)
         (gptel-track-response nil)
         (gptel-include-reasoning 'ignore)
         (gptel-temperature 0.25)
         (gptel-max-tokens 321)
         (gptel-cache t)
         (gptel--request-params '(:custom "parent"))
         (policy '(:backend frozen-backend :model frozen-model :effort high)))
    (let* ((configuration
            (mevedel-agent-exec-freeze-configuration
             "freeze_test" invocation policy))
           (frozen-agent
            (mevedel-agent-configuration-agent configuration))
           (snapshot
            (mevedel-agent-configuration-request-locals configuration)))
      (should (mevedel-agent-configuration-p configuration))
      (should (mevedel-agent-frozen-p frozen-agent))
      (should (equal "freeze_test" (mevedel-agent-name frozen-agent)))
      (should (eq 'frozen-backend (alist-get 'gptel-backend snapshot)))
      (should (eq 'frozen-model (alist-get 'gptel-model snapshot)))
      (should (eq 'high (alist-get 'gptel-reasoning-effort snapshot)))
      (should (equal "Frozen system."
                     (alist-get 'gptel-system-prompt snapshot)))
      (should (equal 'system (alist-get 'gptel-use-context snapshot)))
      (should (equal 'ignore (alist-get 'gptel-include-reasoning snapshot)))
      (should (eq t (alist-get 'gptel-track-media snapshot)))
      (should-not (alist-get 'gptel-track-response snapshot))
      (should (= 7 (alist-get 'gptel--num-messages-to-send snapshot)))
      (should (equal '(:type object) (alist-get 'gptel--schema snapshot)))
      (should (equal 0.25 (alist-get 'gptel-temperature snapshot)))
      (should (equal 321 (alist-get 'gptel-max-tokens snapshot)))
      (should (equal '(:custom "parent")
                     (alist-get 'gptel--request-params snapshot)))
      (should (cl-some
               (lambda (tool) (equal "Read" (gptel-tool-name tool)))
               (alist-get 'gptel-tools snapshot))))))


(mevedel-deftest mevedel-agent-exec--refresh-initial-transcript-state ()
  ,test
  (test)

  :doc "marks persisted transcript buffers dirty and saves them"
  (let* ((buf (generate-new-buffer " *mev-agent-refresh-transcript*"))
         (inv (mevedel-agent-invocation--create
               :path "/root/test_agent"
               :buffer buf
               :transcript-relative-path "agents/verifier.chat.org"))
         saved)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "prompt")
            (set-buffer-modified-p nil))
          (cl-letf (((symbol-function
                     'mevedel-agent-conversation-save)
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
                      'mevedel-agent-conversation-save)
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
			      :path "/root/test_agent"
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
			     (should (equal (plist-get start-event :role) "explorer"))
			     (should (equal (plist-get start-event :agent-path)
			                    "/root/test_agent"))
			     (should (equal (plist-get start-event :prompt) "prompt body"))
			     (should (equal (plist-get decision :additional-context)
					    '("extra start context")))
                             (let ((audit (car (mevedel-agent-exec--start-hook-audit-records
                                                decision))))
                               (should (eq (plist-get audit :type)
                                           'subagent-context))
                               (should (equal (plist-get audit :event)
                                              "SubagentStart"))
                               (should-not (plist-member audit :context)))))
			 (setf (mevedel-agent-invocation-terminal-reason inv) "done")
			 (with-temp-buffer
			   (mevedel-agent-exec-run-stop-hook inv 'completed))
			 (should (equal (plist-get stop-event :role) "explorer"))
			 (should (equal (plist-get stop-event :agent-path)
			                "/root/test_agent"))
			 (should (eq (plist-get stop-event :status) 'completed))
			 (should (equal (plist-get stop-event :terminal-reason) "done")))
			     (delete-directory root t)
			     (when (buffer-live-p parent-buffer)
			       (kill-buffer parent-buffer))
			     (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-agent-exec-run ()
		 ,test
		 (test)

		 :doc "inherits parent include-reasoning into per-agent request buffers"
		 (let ((parent-buf (generate-new-buffer " *mev-agent-parent*"))
		       (agent-buf (generate-new-buffer " *mev-agent-child*"))
		       (inv (mevedel-agent-invocation--create
		             :path "/root/test_agent"
		             :agent (mevedel-agent-default)))
		       (parent-tiers '((custom)))
		       (parent-workloads '((explorer :tier custom)))
		       captured-buffer
		       captured-include-reasoning
		       captured-system
		       captured-tiers
		       captured-tools
		       captured-workloads
		       captured-fsm)
		   (unwind-protect
		       (progn
			 (with-current-buffer parent-buf
			   (let ((gptel-agent-preset '(:include-reasoning nil))
				 (mevedel-agents--specs
				  '(("explorer" :include-reasoning nil)))
				 (gptel-include-reasoning t)
				 (gptel-stream nil)
				 (gptel-backend nil)
				 (gptel-model 'test-model)
				 (gptel-reasoning-effort nil)
				 (mevedel-model-tiers parent-tiers)
				 (mevedel-model-workloads parent-workloads)
				 (gptel-system-prompt "parent system")
				 (gptel-use-tools nil)
				 (gptel-tools '(parent-tool))
				 (gptel-use-context nil)
				 (gptel-context nil)
				 (gptel-use-curl nil)
				 (gptel-temperature nil)
				 (gptel-max-tokens nil)
				 (gptel-cache nil)
				 (gptel--request-params nil)
				 (gptel--fsm-last nil))
			     (setf
			      (mevedel-agent-invocation-frozen-configuration inv)
			      (mevedel-agent-exec-freeze-configuration
			       "default" inv
			       (list :backend gptel-backend
			             :model gptel-model
			             :effort gptel-reasoning-effort)))
			     (cl-letf (((symbol-function 'gptel-request)
					(lambda (&optional _prompt &rest _args)
					  (setq captured-buffer (current-buffer))
					  (setq captured-include-reasoning
						gptel-include-reasoning
						captured-system gptel-system-prompt
						captured-tools gptel-tools)
					  (setq captured-tiers mevedel-model-tiers
						captured-workloads
						mevedel-model-workloads)))
				       ((symbol-function 'gptel--update-status)
					#'ignore))
		       (setq captured-fsm
			     (mevedel-agent-exec-run
			      #'ignore "default" "count defcustoms" "prompt"
			      inv agent-buf)))))
			 (should (eq captured-buffer agent-buf))
			 (should (eq captured-include-reasoning t))
			 (should (equal captured-system "parent system"))
			 (should (equal captured-tools '(parent-tool)))
			 (should (equal '((custom)) captured-tiers))
			 (should (equal '((explorer :tier custom))
					captured-workloads))
			 (should-not (eq parent-tiers captured-tiers))
		 (should-not (eq parent-workloads captured-workloads))
		 (should
		  (equal
		   '(:backend nil :model test-model :max-tokens nil
		     :request-params nil)
		   (plist-get (gptel-fsm-info captured-fsm)
			      :mevedel-compaction-target-policy)))
		 (with-current-buffer agent-buf
			   (should (eq gptel-include-reasoning t))))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
		     (when (buffer-live-p agent-buf) (kill-buffer agent-buf)))))

		 :doc "freezes the invocation agent spec when buffer registry is missing"
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
			 (require 'mevedel-tool-ui)
			 (mevedel-tool-ui--register)
			 (with-current-buffer parent-buf
			   (let ((gptel-agent-preset nil)
				 (mevedel-agents--specs nil)
				 (gptel-include-reasoning nil)
				 (gptel-stream nil)
				 (gptel-backend nil)
				 (gptel-model 'test-model)
				 (gptel-reasoning-effort nil)
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
			     (setf
			      (mevedel-agent-invocation-frozen-configuration inv)
			      (mevedel-agent-exec-freeze-configuration
			       "reviewer" inv
			       (list :backend gptel-backend
			             :model gptel-model
			             :effort gptel-reasoning-effort)))
			     (cl-letf (((symbol-function 'gptel-request)
					(lambda (&optional _prompt &rest _args)
					  (setq captured-system
						gptel-system-prompt)
					  (setq captured-use-tools gptel-use-tools)
					  (setq captured-tools gptel-tools)))
				       ((symbol-function 'gptel--update-status)
					#'ignore))
			       (mevedel-agent-exec-run
				#'ignore "reviewer" "review changes" "prompt"
				inv agent-buf))))
			 (should (equal captured-system "agent system"))
			 (should captured-use-tools)
			 (should (member "Bash" (mapcar #'gptel-tool-name
							 captured-tools))))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
		     (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))


(mevedel-deftest mevedel-agent-exec--invocation-from-info ()
		 ,test
		 (test)

		 :doc "prefers the invocation stored directly on the FSM info plist"
		 (let* ((agent (mevedel-agent--create :name "explorer"))
			(inv (mevedel-agent-invocation--create :agent agent)))
		   (should (eq inv (mevedel-agent-exec--invocation-from-info
				    (list :mevedel-agent-invocation inv)))))

		 )



(mevedel-deftest mevedel-agent-exec--handle-tret-save ()
		 ,test
		 (test)

		 :doc "requests a deferred conversation save after tool results"
		 (let* ((agent (mevedel-agent--create :name "explorer"))
			(inv (mevedel-agent-invocation--create :agent agent))
			(fsm (gptel-make-fsm
			      :info (list :mevedel-agent-invocation inv)))
			called)
		   (cl-letf (((symbol-function 'mevedel-agent-conversation-save)
			      (lambda (&rest args) (setq called args))))
		     (mevedel-agent-exec--handle-tret-save fsm))
		   (should (equal (list inv t) called))))


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



(mevedel-deftest mevedel-agent-exec--handle-wait-activity ()
		 ,test
		 (test)

		 :doc "records waiting once for consecutive WAIT cycles"
		 (let* ((agent (mevedel-agent--create :name "explorer"
						      :description "Explore"))
			(inv (mevedel-agent-invocation--create
			      :path "/root/test_agent"
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


(provide 'test-mevedel-agent-exec)
;;; test-mevedel-agent-exec.el ends here
