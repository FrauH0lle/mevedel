;;; test-niwa-agent-exec.el --- Tests for sub-agent runtime -*- lexical-binding: t -*-

;;; Commentary:

;; Regression coverage for the extracted sub-agent runtime. The callback
;; contract is the high-value surface. These tests pin the corrected contract:
;; chunks accumulate, MAIN-CB fires exactly once on `t', and tool-use guards
;; hold.

;;; Code:

(require 'gptel)
(require 'niwa-structs)
(require 'niwa-agents)
(require 'niwa-agent-conversation)
(require 'niwa-agent-exec)
(require 'niwa-permissions)
(require 'niwa-sandbox)
(require 'niwa-session-persistence)
(require 'niwa-skills-prompt)
(require 'niwa-tools)
(require 'niwa-tool-task)
(require 'niwa-view)
(require 'niwa-view-agent)
(require 'niwa-view-zone)
(require 'niwa-workspace)
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

(defmacro niwa-agent-exec-test--with-callback (callback-sym &rest body)
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
           (niwa-agent-invocation--create
            :path "/root/test_agent"
            :agent (niwa-agent--create :name "explorer")
            :agent-id "explorer--callback-test"))
          (raw-callback (niwa-agent-exec--make-callback
                         main-cb "explorer" "Test task"
                         (point-min-marker) partial-cell))
          (,callback-sym
           (lambda (response info &rest rest)
             (unless (plist-get info :niwa-agent-invocation)
               (setq info (plist-put info :niwa-agent-invocation
                                     default-invocation)))
             (apply raw-callback response info rest))))
     (ignore main-cb partial-cell)
     ,@body))


;;
;;; Transcript prompt injections

(niwa-deftest niwa-agent-exec--handlers ()
  ,test
  (test)
  :doc "TOOL state keeps gptel's status and pending-tool cleanup handlers"
  (let ((tool-entry (assq 'TOOL niwa-agent-exec--handlers)))
    (should (member #'gptel--update-tool-call (cdr tool-entry)))
    (should (member #'gptel--handle-tool-use (cdr tool-entry)))
    (should (member #'gptel--update-tool-ask (cdr tool-entry))))

  :doc "WAIT routes continuations through the shared compaction gate"
  (let ((wait-entry (assq 'WAIT niwa-agent-exec--handlers)))
    (should-not (member #'niwa-reminders--handle-inject
                        (cdr wait-entry)))
    (should (member #'niwa--compact-handle-agent-wait
                    (cdr wait-entry)))
    (should-not (member #'gptel--handle-wait (cdr wait-entry))))

  :doc "Agent-tool cold path binds every handler before compaction loads"
  (let ((root (file-name-directory (locate-library "niwa-agent-exec")))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (let ((status
             (call-process
              emacs nil t nil "--batch" "-Q" "-L" root
              "--eval"
              (prin1-to-string
               '(progn
                  ;; The Agent tool lives in `niwa-tools'; agent runtime
                  ;; then loads execution lazily.
                  (require 'niwa)
                  (require 'niwa-tools)
                  (require 'niwa-agent-exec)
                  (when (featurep 'niwa-compact)
                    (error "Agent execution must not load compaction eagerly"))
                  (dolist (entry niwa-agent-exec--handlers)
                    (dolist (handler (cdr entry))
                      (unless (fboundp handler)
                        (error "Unbound %s handler: %S"
                               (car entry) handler)))))))))
        (ert-info ((buffer-string))
          (should (equal 0 status))
          (should (string-empty-p (string-trim (buffer-string)))))))))


(niwa-deftest niwa-agent-exec--make-callback ()
		 ,test
		 (test)

		 :doc "streaming: chunks accumulate; MAIN-CB fires exactly once on `t'"
		 ;; `:stream t' in info suppresses the non-streaming terminal path so
		 ;; chunks accumulate without firing until gptel sends the terminal
		 ;; `t' event.
		 (niwa-agent-exec-test--with-callback cb
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

		 :doc "appends the stop note of a turn ended before a final answer"
		 (niwa-agent-exec-test--with-callback cb
							 (let ((info (list :stream t
									   :niwa-agent-stop-note "[Stopped.]")))
							   (funcall cb "Checking one more file" info)
							   (funcall cb t info)
							   (should (string-suffix-p
								    "Checking one more file\n\n[Stopped.]"
								    (car (car fired))))))

		 :doc "streaming: reasoning stream events pass through without firing"
		 ;; gptel delivers reasoning as (reasoning . TEXT) and closes the
		 ;; block with the improper list (reasoning . t).  Neither is a
		 ;; terminal plist and neither may signal.
		 (niwa-agent-exec-test--with-callback cb
							 (let ((info '(:stream t)))
							   (funcall cb '(reasoning . "thinking...") info)
							   (funcall cb '(reasoning . t) info)
							   (funcall cb "answer" info)
							   (should (null fired))
							   (funcall cb t info)
							   (should (= 1 (length fired)))
							   (should (equal "answer" (car (car fired))))))

		 :doc "streaming: chunks do not rebuild partial text before terminal"
		 (let* ((fired nil)
			(main-cb (lambda (&rest args) (push args fired)))
			(partial-cell (list "prefix: "))
			(inv (niwa-agent-invocation--create
			      :path "/root/test_agent"
			      :agent (niwa-agent--create :name "explorer")))
			(cb (niwa-agent-exec--make-callback
			     main-cb "explorer" "Test task"
			     (point-min-marker) partial-cell))
			(info (list :stream t :niwa-agent-invocation inv)))
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
		       (let* ((agent (niwa-agent--create :name "explorer"))
			      (inv (niwa-agent-invocation--create
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
			 (niwa-agent-exec-test--with-callback cb
								 (let ((info (list :stream t
										   :niwa-agent-invocation inv)))
								   (funcall cb "Explore result for task\n\n" info)
								   (funcall cb "(:name \"Read\" ...)\nlarge tool output\n" info)
								   (funcall cb "*Summary:* final answer only\n" info)
								   (funcall cb t info)
								   (should (= 1 (length fired)))
								   (should (equal "*Summary:* final answer only"
										  (car (car fired)))))))
		     (when (buffer-live-p buf) (kill-buffer buf))))

		 :doc "non-streaming (single chunk + `t'): single delivery unchanged"
		 (niwa-agent-exec-test--with-callback cb
							 (funcall cb "complete response" nil)
							 (funcall cb t nil)
							 (should (= 1 (length fired)))
							 (should (equal "complete response" (car (car fired)))))

		 :doc "non-streaming (:stream absent, no `t'): string branch fires MAIN-CB"
		 ;; gptel removes `:stream' from info when the request is non-streaming,
		 ;; and `gptel-curl--stream-cleanup' only runs for streaming requests.
		 ;; The string must be treated as terminal here.
		 (niwa-agent-exec-test--with-callback cb
							 (funcall cb "complete non-streaming response" nil)
							 (should (= 1 (length fired)))
							 (should (equal "complete non-streaming response"
									(car (car fired)))))

		 :doc "non-streaming tool-use turn: string does not fire MAIN-CB"
		 ;; In non-streaming, intermediate tool-use turns also arrive as
		 ;; strings; we must only finalize on the final text-only turn.
		 (niwa-agent-exec-test--with-callback cb
							 (funcall cb "reasoning about tool call" (list :tool-use '((:name "Read"))))
							 (should (null fired))
							 (funcall cb " continuation after tools" nil)
							 (should (= 1 (length fired)))
							 (should (equal "reasoning about tool call continuation after tools"
									(car (car fired)))))

		 :doc "streaming chunk with `:stream' t: string branch defers to `t'"
		 ;; With `:stream' set, per-chunk firing is disabled; the 't signal
		 ;; remains the terminal.  Mirrors real streaming backends.
		 (niwa-agent-exec-test--with-callback cb
							 (funcall cb "partial " (list :stream t))
							 (funcall cb "text" (list :stream t))
							 (should (null fired))
							 (funcall cb t (list :stream t))
							 (should (= 1 (length fired)))
							 (should (equal "partial text" (car (car fired)))))

		 :doc "empty response (`t' with no prior `stringp'): single empty delivery"
		 (niwa-agent-exec-test--with-callback cb
							 (funcall cb t nil)
							 (should (= 1 (length fired)))
							 (should (equal "" (car (car fired)))))

		 :doc "tool-use guard: `t' while :tool-use is non-nil does not fire MAIN-CB"
		 ;; Streaming scenario: `:stream t' keeps string chunks off the
		 ;; non-streaming terminal path; the `t' signal with :tool-use set
		 ;; must not fire either.
		 (niwa-agent-exec-test--with-callback cb
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
		 (niwa-agent-exec-test--with-callback cb
							 (funcall cb nil (list :error "boom"))
							 (should (= 1 (length fired)))
							 (let ((event (car (car fired))))
							   (should (eq 'error
							               (plist-get event
							                          :niwa-agent-terminal-status)))
							   (should (equal "boom"
							                  (plist-get event :error-details)))))

                 :doc "in-band error on `t' terminal settles as error, not empty completion"
                 ;; A provider can fail in-band on an HTTP 200 stream: the
                 ;; parser stashes the error on INFO and
                 ;; `gptel-curl--stream-cleanup' still fires `t'.
                 (niwa-agent-exec-test--with-callback cb
                                                         (let ((info (list :stream t
                                                                           :status "HTTP/2 200"
                                                                           :error '(:type "service_unavailable_error"))))
                                                           (funcall cb "partial text" info)
                                                           (funcall cb t info)
                                                           (should (= 1 (length fired)))
                                                           (let ((event (car (car fired))))
                                                             (should (eq 'error
                                                                         (plist-get
                                                                          event :niwa-agent-terminal-status)))
                                                             (should (equal '(:type "service_unavailable_error")
                                                                            (plist-get event :error-details)))
                                                             (should (string-match-p
                                                                      "partial text"
                                                                      (plist-get event :fallback-partial))))
                                                           (should (equal "HTTP/2 200: service_unavailable_error"
                                                                          (niwa-agent-invocation-terminal-reason
                                                                           default-invocation)))))

                 :doc "in-band error on the non-streaming string terminal settles as error"
                 (niwa-agent-exec-test--with-callback cb
                                                         (funcall cb "response text" (list :error "boom"))
                                                         (should (= 1 (length fired)))
                                                         (let ((event (car (car fired))))
                                                           (should (eq 'error
                                                                       (plist-get
                                                                        event :niwa-agent-terminal-status)))
                                                           (should (equal "boom"
                                                                          (plist-get event :error-details)))))

		 :doc "abort (`'abort') emits one structured terminal event"
		 (niwa-agent-exec-test--with-callback cb
							 (funcall cb 'abort nil)
							 (should (= 1 (length fired)))
							 (let ((event (car (car fired))))
							   (should (eq 'aborted
							               (plist-get event
							                          :niwa-agent-terminal-status)))
							   (should (string-match-p
							            "aborted by the user"
							            (plist-get event :response)))))

                 :doc "terminal bookkeeping errors become one structured error event"
                 (niwa-agent-exec-test--with-callback cb
                                                         (funcall cb "answer"
                                                                  (list :transformer
                                                                        (lambda (_text)
                                                                          (error "Broken transformer"))))
                                                         (should (= 1 (length fired)))
                                                         (let ((event (car (car fired))))
                                                           (should (eq 'error
                                                                       (plist-get
                                                                        event :niwa-agent-terminal-status)))
                                                           (should (string-match-p
                                                                    "Broken transformer"
                                                                    (plist-get event :error-details)))))

                 :doc "a rejected terminal handoff stays pending for one later retry"
                 (let* ((attempts 0)
                        (delivered nil)
                        (event '(:niwa-agent-terminal-status error
                                 :fallback-partial "partial response"))
                        scheduled
                        (main-cb
                         (lambda (value)
                           (cl-incf attempts)
                           (if (= attempts 1)
                               (error "Commit unavailable")
                             (setq delivered value))))
                        (inv (niwa-agent-invocation--create
                              :path "/root/test_agent"
                              :agent (niwa-agent--create :name "explorer")))
                        (cb (niwa-agent-exec--make-callback
                             main-cb "explorer" "Test task"
                             (point-min-marker) (list "")))
                        (info (list :stream t :niwa-agent-invocation inv)))
                   (cl-letf (((symbol-function 'run-at-time)
                              (lambda (_delay _repeat function &rest args)
                                (setq scheduled (cons function args))
                                'retry-timer)))
                     (funcall cb event info)
                     (should (= 1 attempts))
                     (should-not delivered)
                     (apply (car scheduled) (cdr scheduled)))
                   (should (= 2 attempts))
                   (should (equal event delivered)))


		 )


;;
;;; Request buffer configuration


(niwa-deftest niwa-agent-exec--policy-for-invocation
  ()
  ,test
  (test)
  :doc "passes direct skill model and effort over workload policy"
  (let* ((inv (niwa-agent-invocation--create
               :path "/root/test_agent"
               :skill-model-override '(:tier fast)
               :skill-effort-override 'high))
         captured)
    (cl-letf (((symbol-function 'niwa-model-resolve-workload)
               (lambda (&rest args)
                 (setq captured args)
                 '(:model chosen))))
      (should (equal '(:model chosen)
                     (niwa-agent-exec--policy-for-invocation
                      "reviewer" inv))))
    (should (equal '("reviewer" (:tier fast) high) captured))))

(niwa-deftest niwa-agent-exec--request-preset
  ()
  ,test
  (test)
  :doc "default role keeps request defaults, inherited reasoning and tools"
  (let* ((gptel-include-reasoning 'ignore)
         (preset (niwa-agent-exec--request-preset "default" nil)))
    (should (equal '(:use-tools t :context nil) (seq-take preset 4)))
    (should (eq 'ignore (plist-get preset :include-reasoning)))
    (should (equal '(inherited)
                   (funcall (plist-get (plist-get preset :tools) :function)
                            '(inherited)))))

  :doc "named role contributes its own prompt and tools"
  (let* ((agent (niwa-agent--create :name "role" :system-prompt "Role prompt."))
         (invocation (niwa-agent-invocation-create agent))
         (preset (niwa-agent-exec--request-preset "role" invocation)))
    (should (equal "Role prompt." (plist-get preset :system)))))

(niwa-deftest niwa-agent-exec-request-snapshot
  ()
  ,test
  (test)

  :doc "materializes a dynamic system prompt once into frozen plain data"
  (let* ((calls 0)
         (gptel-system-prompt
          (lambda ()
            (cl-incf calls)
            '("Dynamic system." "Second part."))))
    (let* ((snapshot
            (niwa-agent-exec-request-snapshot
             '(:backend frozen-backend :model frozen-model :effort high)))
           (system-prompt (alist-get 'gptel-system-prompt snapshot)))
      (should (= 1 calls))
      (should (equal '("Dynamic system." "Second part.") system-prompt))
      (should-not (functionp system-prompt))))

  :doc "materializes the prompt under the model the request will use"
  ;; Parts of the prompt are budgeted against the model's context
  ;; window, so an agent on a small local model must not be handed a
  ;; prompt sized for the spawning buffer's model.
  (let* ((seen nil)
         (gptel-backend 'parent-backend)
         (gptel-model 'parent-model)
         (gptel-system-prompt
          (lambda ()
            (setq seen (cons gptel-backend gptel-model))
            "System.")))
    (niwa-agent-exec-request-snapshot
     '(:backend frozen-backend :model frozen-model :effort high))
    (should (equal '(frozen-backend . frozen-model) seen)))

  :doc "keeps ToolCall in the retained-agent request snapshot"
  (let* ((ptc (gptel-make-tool :name "ToolCall" :function #'ignore
                                :description "ToolCall" :args nil))
         (read (gptel-make-tool :name "Read" :function #'ignore
                                 :description "Read" :args nil))
         (gptel-tools (list ptc read))
         (snapshot (niwa-agent-exec-request-snapshot nil)))
    (should (equal (list ptc read) (alist-get 'gptel-tools snapshot))))

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
         (niwa-model-tiers '((custom :provider "Backend:model")))
         (niwa-model-workloads '((explorer :tier custom)))
         (tiers niwa-model-tiers)
         (workloads niwa-model-workloads)
         (snapshot
          (niwa-agent-exec-request-snapshot
           '(:backend frozen-backend :model frozen-model :effort high))))
    (should (= (length niwa-agent-request-local-symbols)
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
    (should (equal tiers (alist-get 'niwa-model-tiers snapshot)))
    (should-not (eq tiers (alist-get 'niwa-model-tiers snapshot)))
    (should (equal workloads (alist-get 'niwa-model-workloads snapshot)))
    (should-not
     (eq workloads (alist-get 'niwa-model-workloads snapshot)))))

(niwa-deftest niwa-agent-exec-freeze-configuration
  (:before-each (niwa-tools-register))
  ,test
  (test)
  :doc "captures exact request policy, instructions, tools, and inherited config"
  (let* ((agent
          (niwa-agent--create
           :name "freeze_test"
           :description "Freeze request config"
           :tools '((:tool "Read"))
           :system-prompt "Frozen system."))
         (invocation (niwa-agent-invocation-create agent))
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
            (niwa-agent-exec-freeze-configuration
             "freeze_test" invocation policy))
           (frozen-agent
            (niwa-agent-configuration-agent configuration))
           (snapshot
            (niwa-agent-configuration-request-locals configuration)))
      (should (niwa-agent-configuration-p configuration))
      (should (niwa-agent-frozen-p frozen-agent))
      (should (equal "freeze_test" (niwa-agent-name frozen-agent)))
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
               (alist-get 'gptel-tools snapshot)))))

  :doc "materializes dynamic instructions under the agent request context"
  (let* ((prompt-calls 0)
         (agent
          (niwa-agent--create
           :name "freeze_dynamic"
           :description "Freeze dynamic request config"
           :tools '((:tool "ToolCall"))
           :system-prompt
           (lambda ()
             (format "%d/%s/%s/%s"
                     (cl-incf prompt-calls)
                     gptel-backend gptel-model
                     (mapconcat #'gptel-tool-name gptel-tools ",")))))
         (invocation (niwa-agent-invocation-create agent))
         (gptel-tools
          (list (gptel-make-tool :name "Read" :function #'ignore
                                 :description "parent")))
         (configuration
          (niwa-agent-exec-freeze-configuration
           "freeze_dynamic" invocation
           '(:backend frozen-backend :model frozen-model :effort high))))
    (let* ((frozen-agent
            (niwa-agent-configuration-agent configuration))
           (prompt
           (alist-get
            'gptel-system-prompt
            (niwa-agent-configuration-request-locals configuration))))
      (should (= 1 prompt-calls))
      (should (string-prefix-p "1/frozen-backend/frozen-model/" prompt))
      (should (string-match-p "ToolCall" prompt))
      (should-not (string-match-p "Read" prompt))
      (should (equal prompt (niwa-agent-system-prompt frozen-agent)))))

  :doc "materializes the default role's inherited dynamic prompt once"
  (let* ((prompt-calls 0)
         (agent
          (niwa-agent--create
           :name "default"
           :description "Default request config"
           :tools nil
           :system-prompt "Unused role prompt."))
         (invocation (niwa-agent-invocation-create agent))
         (gptel-system-prompt
          (lambda ()
            (format "%d/%s" (cl-incf prompt-calls) gptel-model)))
         (configuration
          (niwa-agent-exec-freeze-configuration
           "default" invocation
           '(:backend frozen-backend :model frozen-model :effort high)))
         (frozen-agent
          (niwa-agent-configuration-agent configuration))
         (prompt
          (alist-get
           'gptel-system-prompt
           (niwa-agent-configuration-request-locals configuration))))
    (should (= 1 prompt-calls))
    (should (equal "1/frozen-model" prompt))
    (should (equal prompt (niwa-agent-system-prompt frozen-agent))))

  :doc "the default role inherits native tools and adds native SendMessage"
  (let* ((read (niwa-tool-gptel-tool (niwa-tool-get "Read" "niwa")))
         (gptel-tools (list read))
         (invocation (niwa-agent-invocation-create (niwa-agent-default)))
         (configuration
          (progn
            (setf (niwa-agent-invocation-tool-catalog invocation)
                  '((("niwa" "SendMessage") . "Message")))
            (niwa-agent-exec-freeze-configuration
             "default" invocation
             '(:backend frozen-backend :model frozen-model :effort high))))
         (tools (alist-get 'gptel-tools
                           (niwa-agent-configuration-request-locals
                            configuration))))
    (should (equal '("Read" "SendMessage") (mapcar #'gptel-tool-name tools)))))


(niwa-deftest niwa-agent-exec-acknowledge-provider ()
  ,test
  (test)
  :doc "replaces only the invocation's copy with the acknowledged provider"
  (let* ((backend (gptel--make-backend :name "Acknowledging Provider"))
         (locals `((gptel-backend . ,backend) (gptel-model . requested-model)
                   (gptel-reasoning-effort . high)))
         (frozen (niwa-agent-configuration--create
                  :agent (niwa-agent-default) :request-locals locals))
         (invocation (niwa-agent-invocation--create
                      :frozen-configuration frozen)))
    (niwa-agent-exec-acknowledge-provider invocation 'served-model nil)
    (should (equal '(:backend "Acknowledging Provider" :model "served-model"
                     :effort nil)
                   (niwa-agent-configuration-model-metadata
                    (niwa-agent-invocation-frozen-configuration invocation))))
    ;; The record's frozen identity keeps the request.
    (should (eq 'requested-model (alist-get 'gptel-model locals)))
    (should (eq 'high (alist-get 'gptel-reasoning-effort locals))))
  :doc "keeps the same configuration object when nothing changed"
  (let* ((frozen (niwa-agent-configuration--create
                  :agent (niwa-agent-default)
                  :request-locals '((gptel-model . same-model)
                                    (gptel-reasoning-effort . low))))
         (invocation (niwa-agent-invocation--create
                      :frozen-configuration frozen)))
    (niwa-agent-exec-acknowledge-provider invocation "same-model" 'low)
    (should (eq frozen (niwa-agent-invocation-frozen-configuration
                        invocation))))
  :doc "a changed acknowledgement refreshes the parent roster once, keeping the draft"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session-create
                     "main" (niwa-workspace--create
                             :type 'project :id "ack-view" :name "ack-view"
                             :root temporary-file-directory)))
           (invocation
            (niwa-agent-invocation--create
             :path "/root/partner" :agent-id "storage-partner"
             :parent-session session :parent-data-buffer data-buf
             :frozen-configuration
             (niwa-agent-configuration--create
              :agent (niwa-agent-default)
              :request-locals
              `((gptel-backend . ,(gptel--make-backend :name "Claude Code"))
                (gptel-model . opus) (gptel-reasoning-effort . high)))))
           (record (niwa-agent-record--create
                    :id "storage-partner" :path "/root/partner"
                    :parent-path "/root" :role "default" :activity 'running
                    :invocation invocation))
           (niwa-view-agent-refresh-delay 0)
           (refresh (symbol-function 'niwa-agent-conversation-refresh))
           (refreshes 0))
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/partner" record)))
      (with-current-buffer data-buf (setq-local niwa--session session))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> keep\nsecond line" 3)
        (niwa-view--render-agent-status)
        (should (string-search "Running   agent    /root/partner · opus:high"
                               (buffer-string))))
      (cl-letf (((symbol-function 'niwa-agent-conversation-refresh)
                 (lambda (value)
                   (cl-incf refreshes)
                   (funcall refresh value))))
        ;; Claude confirming the requested selection changes nothing.
        (niwa-agent-exec-acknowledge-provider invocation 'opus 'high)
        (should (= 0 refreshes))
        ;; Its default effort replaces the launch selection in the roster.
        (niwa-agent-exec-acknowledge-provider invocation "opus" nil)
        (should (= 1 refreshes)))
      (with-current-buffer view-buf
        (should (string-search "Running   agent    /root/partner · opus" (buffer-string)))
        (should-not (string-search "opus:high" (buffer-string)))
        (should (equal "> keep\nsecond line" (niwa-view-composer-draft-text)))
        (should (= 3 (niwa-view-test--composer-point-offset)))))))

(niwa-deftest niwa-agent-exec-run ()
		 ,test
		 (test)

		 :doc "inherits parent include-reasoning into per-agent request buffers"
		 (let ((parent-buf (generate-new-buffer " *mev-agent-parent*"))
		       (agent-buf (generate-new-buffer " *mev-agent-child*"))
		       (inv (niwa-agent-invocation--create
		             :path "/root/test_agent"
		             :agent (niwa-agent-default)))
		       (parent-tiers '((custom)))
		       (parent-workloads '((explorer :tier custom)))
		       (parent-tool (gptel--make-tool-internal :name "parent-tool"))
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
			   (let ((niwa-agents--specs
				  '(("explorer" :include-reasoning nil)))
				 (gptel-include-reasoning t)
				 (gptel-stream nil)
				 (gptel-backend nil)
				 (gptel-model 'test-model)
				 (gptel-reasoning-effort nil)
				 (niwa-model-tiers parent-tiers)
				 (niwa-model-workloads parent-workloads)
				 (gptel-system-prompt "parent system")
				 (gptel-use-tools nil)
				 (gptel-tools (list parent-tool))
				 (gptel-use-context nil)
				 (gptel-context nil)
				 (gptel-use-curl nil)
				 (gptel-temperature nil)
				 (gptel-max-tokens nil)
				 (gptel-cache nil)
				 (gptel--request-params nil)
				 (gptel--fsm-last nil))
			     (setf
			      (niwa-agent-invocation-frozen-configuration inv)
			      (niwa-agent-exec-freeze-configuration
			       "default" inv
			       (list :backend gptel-backend
			             :model gptel-model
			             :effort gptel-reasoning-effort)))
			     (cl-letf (((symbol-function 'gptel-request)
					(lambda (&optional _prompt &rest args)
					  (setq captured-buffer (current-buffer))
					  (setq captured-include-reasoning
						gptel-include-reasoning
						captured-system gptel-system-prompt
						captured-tools gptel-tools)
					  (setq captured-tiers niwa-model-tiers
						captured-workloads
						niwa-model-workloads)
					  ;; The real `gptel-request' replaces
					  ;; the FSM info plist wholesale, so
					  ;; keys set before the call are lost.
					  (when-let* ((fsm (plist-get args :fsm)))
					    (setf (gptel-fsm-info fsm)
						  (list :buffer (current-buffer))))))
				       ((symbol-function 'gptel--update-status)
					#'ignore))
		       (setq captured-fsm
			     (niwa-agent-exec-run
			      #'ignore "default" "count defcustoms" inv agent-buf)))))
			 (should (eq captured-buffer agent-buf))
			 (should (eq captured-include-reasoning t))
			 (should (equal captured-system "parent system"))
			 (should (equal captured-tools (list parent-tool)))
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
			      :niwa-compaction-target-policy)))
		 (should (eq inv (plist-get (gptel-fsm-info captured-fsm)
					    :niwa-agent-invocation)))
		 (should (functionp
			  (plist-get (gptel-fsm-info captured-fsm)
				     :niwa-agent-terminal-callback)))
		 (should-not (eq #'ignore
				 (plist-get (gptel-fsm-info captured-fsm)
					    :niwa-agent-terminal-callback)))
		 (with-current-buffer agent-buf
			   (should (eq gptel-include-reasoning t))))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
		     (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))

		 :doc "freezes the invocation agent spec when buffer registry is missing"
		 (let* ((parent-buf (generate-new-buffer " *mev-agent-parent*"))
			(agent-buf (generate-new-buffer " *mev-agent-child*"))
			(agent (niwa-agent--create
				:name "reviewer"
				:system-prompt "agent system"
				:tools '((:tool "Bash"))))
			(inv (niwa-agent-invocation--create :agent agent))
			captured-system
			captured-use-tools
			captured-tools)
		   (unwind-protect
		       (progn
			 (niwa-tools-register)
			 (with-current-buffer parent-buf
			   (let ((niwa-agents--specs nil)
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
			      (niwa-agent-invocation-frozen-configuration inv)
			      (niwa-agent-exec-freeze-configuration
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
			       (niwa-agent-exec-run
				#'ignore "reviewer" "review changes" inv agent-buf))))
			 (should (equal captured-system "agent system"))
			 (should captured-use-tools)
			 (should (member "Bash" (mapcar #'gptel-tool-name
							 captured-tools))))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
		     (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))

		 :doc "keeps a frozen model the registered backend does not list"
		 ;; gptel sends any `gptel-model' symbol; an unlisted model on the
		 ;; still-registered backend must not fall back to the default.
		 (let* ((gptel--known-backends nil)
			(backend (gptel-make-openai "Unlisted" :key "k"
					     :models '(listed-model)))
			(agent-buf (generate-new-buffer " *mev-agent-child*"))
			(inv (niwa-agent-invocation--create
			      :path "/root/test_agent"
			      :agent (niwa-agent-default)))
			captured)
		   (unwind-protect
		       (with-temp-buffer
			 (let ((niwa-agents--specs nil)
			       (gptel-backend backend)
			       (gptel-model 'unlisted-model)
			       (gptel-stream nil)
			       (gptel--fsm-last nil))
			   (setf (niwa-agent-invocation-frozen-configuration inv)
				 (niwa-agent-exec-freeze-configuration
				  "default" inv
				  (list :backend backend :model 'unlisted-model
				        :effort nil)))
			   (cl-letf (((symbol-function 'gptel-request)
				      (lambda (&rest _)
					(setq captured (cons gptel-backend gptel-model))))
				     ((symbol-function 'gptel--update-status) #'ignore))
			     (niwa-agent-exec-run
			      #'ignore "default" "task" inv agent-buf))
			   (should (eq backend (car captured)))
			   (should (eq 'unlisted-model (cdr captured)))))
		     (kill-buffer agent-buf))))


(niwa-deftest niwa-agent-exec--invocation-from-fsm ()
  ,test
  (test)
  :doc "reads the invocation from FSM info and tolerates absent state"
  (let* ((inv (niwa-agent-invocation--create))
         (fsm (gptel-make-fsm :info (list :niwa-agent-invocation inv))))
    (should (eq inv (niwa-agent-exec--invocation-from-fsm fsm)))
    (should-not (niwa-agent-exec--invocation-from-fsm nil))
    (should-not (niwa-agent-exec--invocation-from-fsm (gptel-make-fsm)))))


(niwa-deftest niwa-agent-exec--wrap-callback ()
  ,test
  (test)
  :doc "forwards inserts to gptel's stock callback chosen from final :stream"
  ;; gptel installs its default `:callback' only when the provider request
  ;; is sent, possibly after `gptel-request' returned, so the wrapper must
  ;; not depend on a callback captured at dispatch.
  (let (inserted bookkept)
    (cl-letf (((symbol-function 'gptel-curl--stream-insert-response)
               (lambda (resp _info &optional _raw)
                 (push (cons 'stream resp) inserted)))
              ((symbol-function 'gptel--insert-response)
               (lambda (resp _info &optional _raw)
                 (push (cons 'plain resp) inserted))))
      (let ((wrapped (niwa-agent-exec--wrap-callback
                      (lambda (resp _info &rest _)
                        (push resp bookkept)) 'unavailable)))
        (funcall wrapped "chunk" (list :stream t))
        (funcall wrapped "whole" (list :buffer nil))
        (funcall wrapped t (list :stream t))))
    (should (equal '((plain . "whole") (stream . "chunk")) inserted))
    (should (equal '(t "whole" "chunk") bookkept))))


(niwa-deftest niwa-agent-exec--handle-wait-turn ()
  ,test
  (test)
  :doc "counts each sample, warns near the cap, and ends the capped turn"
  (let* ((inv (niwa-agent-invocation--create
               :agent (niwa-agent--create :name "explorer" :max-turns 5)
               :reminders (list (niwa-reminders-make-max-turns-warning))
               :turn-count 0))
         (fsm (gptel-make-fsm :info (list :niwa-agent-invocation inv)))
         (types (lambda ()
                  (mapcar (lambda (entry) (plist-get entry :type))
                          (plist-get (gptel-fsm-info fsm)
                                     :niwa-reminder-entries)))))
    (dotimes (_ 3) (niwa-agent-exec--handle-wait-turn fsm))
    (should (= 3 (niwa-agent-invocation-turn-count inv)))
    (should-not (funcall types))
    (niwa-agent-exec--handle-wait-turn fsm)
    (should (equal '(max-turns-warning) (funcall types)))
    ;; Delivery commits the one-shot warning.
    (mapc #'funcall (plist-get (gptel-fsm-info fsm) :niwa-reminder-commits))
    (should-not (niwa-turn-end-requested-p (gptel-fsm-info fsm)))
    (niwa-agent-exec--handle-wait-turn fsm)
    (should (equal '(max-turns-warning max-turns-limit) (funcall types)))
    (should (string-search "final turn (5 of 5)"
                           (plist-get (cadr (plist-get (gptel-fsm-info fsm)
                                                       :niwa-reminder-entries))
                                      :body)))
    (should (eq 'agent-turn-limit
                (niwa-turn-end-requested-p (gptel-fsm-info fsm))))
    ;; A later sample of the same ended turn neither re-warns nor repeats.
    (niwa-agent-exec--handle-wait-turn fsm)
    (should (= 2 (length (funcall types)))))

  :doc "counts agents without a cap without staging anything"
  (let* ((inv (niwa-agent-invocation--create
               :agent (niwa-agent--create :name "explorer") :turn-count 0))
         (fsm (gptel-make-fsm :info (list :niwa-agent-invocation inv))))
    (niwa-agent-exec--handle-wait-turn fsm)
    (should (= 1 (niwa-agent-invocation-turn-count inv)))
    (should-not (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))
    (should-not (niwa-turn-end-requested-p (gptel-fsm-info fsm)))))


(niwa-deftest niwa-agent-exec--handle-wait-goal-budget ()
  ,test
  (test)
  :doc "stages a due Goal budget notice with its delivery commit"
  (let ((fsm (gptel-make-fsm :info (list :buffer nil))))
    (cl-letf (((symbol-function 'niwa-goal-agent-budget-notice)
               (lambda (_fsm) (list :body "Budget at 50%." :commit #'ignore))))
      (niwa-agent-exec--handle-wait-goal-budget fsm))
    (should (equal '((:type goal-budget :body "Budget at 50%."))
                   (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)))
    (should (memq #'ignore (plist-get (gptel-fsm-info fsm)
                                      :niwa-reminder-commits)))))


(niwa-deftest niwa-agent-exec--handle-done-ended ()
  ,test
  (test)
  :doc "delivers the terminal event only for a turn ended at its tool results"
  (let* (delivered
         (info (list :tool-use '((:name "Read"))
                     :niwa-agent-terminal-callback
                     (lambda (event callback-info)
                       (push (cons event (plist-get callback-info :tool-use))
                             delivered))))
         (fsm (gptel-make-fsm :info info)))
    (niwa-agent-exec--handle-done-ended fsm)
    (should-not delivered)
    (plist-put info :niwa-end-turn 'hook-stop)
    (niwa-agent-exec--handle-done-ended fsm)
    (should (equal '((t)) delivered))
    (should (equal "[Stopped before a final answer: a hook stopped the turn.]"
                   (plist-get info :niwa-agent-stop-note))))

  :doc "names the agent's cap when the turn limit stopped it"
  (let* ((info (list :niwa-end-turn 'agent-turn-limit
                     :niwa-agent-invocation
                     (niwa-agent-invocation--create
                      :agent (niwa-agent--create :name "explorer"
                                                    :max-turns 150))
                     :niwa-agent-terminal-callback #'ignore)))
    (niwa-agent-exec--handle-done-ended (gptel-make-fsm :info info))
    (should (equal "[Stopped before a final answer: the 150-turn limit was reached.]"
                   (plist-get info :niwa-agent-stop-note)))))


(niwa-deftest niwa-agent-exec--handle-tret-save ()
		 ,test
		 (test)

		 :doc "requests a deferred conversation save after tool results"
		 (let* ((agent (niwa-agent--create :name "explorer"))
			(inv (niwa-agent-invocation--create :agent agent))
			(fsm (gptel-make-fsm
			      :info (list :niwa-agent-invocation inv)))
			called)
		   (cl-letf (((symbol-function 'niwa-agent-conversation-save)
			      (lambda (&rest args) (setq called args))))
		     (niwa-agent-exec--handle-tret-save fsm))
		   (should (equal (list inv t) called))))


(niwa-deftest niwa-agent-exec--handle-errs-save ()
			 ,test
			 (test)

			 :doc "routes FSM-side errors through guarded terminal delivery"
			 (let* ((inv (niwa-agent-invocation--create
				      :path "/root/test_agent"
				      :agent (niwa-agent--create :name "explorer")))
				(info (list :error "compaction failed"
					    :niwa-agent-invocation inv))
				(fsm (gptel-make-fsm :info info))
				delivered)
			   (plist-put
			    info :niwa-agent-terminal-callback
			    (lambda (event callback-info)
			      (setq delivered (cons event callback-info))))
			   (cl-letf (((symbol-function 'gptel--handle-error) #'ignore)
				     ((symbol-function 'niwa-agent-conversation-final-response)
				      (lambda (_inv) "partial response")))
			     (niwa-agent-exec--handle-errs-save fsm))
			   (should (eq info (cdr delivered)))
			   (should (equal "compaction failed"
					  (plist-get (car delivered) :error-details)))
			   (should (equal "partial response"
					  (plist-get (car delivered) :fallback-partial))))
			 )


(niwa-deftest niwa-agent-exec--error-reason-from-info ()
		 ,test
		 (test)
		 :doc "joins HTTP status with plist :type and :message"
		 (let ((reason (niwa-agent-exec--error-reason-from-info
				'(:status "429"
					  :error (:type "rate_limit_error"
							:message "Too many tokens")))))
		   (should (stringp reason))
		   (should (string-match-p "429" reason))
		   (should (string-match-p "rate_limit_error" reason))
		   (should (string-match-p "Too many tokens" reason)))

		 :doc "treats string :error as a message"
		 (let ((reason (niwa-agent-exec--error-reason-from-info
				'(:status "500" :error "Internal server error"))))
		   (should (string-match-p "500" reason))
		   (should (string-match-p "Internal server error" reason)))

		 :doc "returns nil when :error is absent and :status is empty"
		 (should-not (niwa-agent-exec--error-reason-from-info '(:status "")))
		 (should-not (niwa-agent-exec--error-reason-from-info nil))

		 :doc "truncates long reasons at 200 characters"
		 (let* ((long (make-string 500 ?x))
			(reason (niwa-agent-exec--error-reason-from-info
				 (list :status "429" :error long))))
		   (should (<= (length reason) 203))
		   (should (string-suffix-p "..." reason))))



(niwa-deftest niwa-agent-exec--handle-wait-activity ()
		 ,test
		 (test)

		 :doc "records waiting once for consecutive WAIT cycles"
		 (let* ((agent (niwa-agent--create :name "explorer"
						      :description "Explore"))
			(inv (niwa-agent-invocation--create
			      :path "/root/test_agent"
			      :agent agent
			      :agent-id "explorer--wait"))
			(parent-buf (generate-new-buffer " *mev-agent-wait-parent*"))
			(view-buf (generate-new-buffer " *mev-agent-wait-view*")))
		   (unwind-protect
		       (progn
			 (setf (niwa-agent-invocation-parent-data-buffer inv) parent-buf)
			 (with-current-buffer parent-buf
			   (setq-local niwa--view-buffer view-buf))
			 (cl-letf (((symbol-function
				     'niwa-agent-exec--invocation-from-fsm)
				    (lambda (_fsm) inv))
				   ((symbol-function 'niwa-view-refresh-agent-rendering)
				    (lambda (_buffer _agent-id) nil)))
			   (niwa-agent-exec--handle-wait-activity 'fsm)
			   (niwa-agent-exec--handle-wait-activity 'fsm))
			 (should (= 1 (length (niwa-agent-invocation-activity inv))))
			 (should (eq 'waiting
				     (plist-get
				      (car (niwa-agent-invocation-activity inv))
				      :type))))
		     (when (buffer-live-p view-buf) (kill-buffer view-buf))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))


(provide 'test-niwa-agent-exec)

(ert-deftest niwa-agent-exec-run/codex-cache-key-in-payload ()
  "The realized Codex agent payload carries `<session-id>:<agent-id>'."
  (let* ((gptel--known-backends nil)
         (backend (gptel-make-openai-oauth "Agent Codex"))
         (agent-buf (generate-new-buffer " *mev-agent-cache*"))
         (inv (niwa-agent-invocation--create
               :path "/root/test_agent" :agent-id "a--11111111"
               :agent (niwa-agent-default)))
         (orig (symbol-function 'gptel-request))
         (gptel--openai-oauth-token-file (make-temp-file "mev-token-"))
         key)
    ;; A current token, so the real `niwa-auth-assert-ready' passes.
    (with-temp-file gptel--openai-oauth-token-file
      (prin1 (list :access_token "current" :expires_at (+ (float-time) 3600))
             (current-buffer)))
    (unwind-protect
        (with-temp-buffer
          ;; As `niwa-agent-conversation-open' leaves the agent buffer.
          (with-current-buffer agent-buf
            (setq-local niwa--session
                        (niwa-session--create :session-id "root-id")
                        niwa--agent-invocation inv))
          (let ((niwa-agents--specs nil)
                (gptel-backend backend) (gptel-model 'gpt-5.5)
                (gptel-stream nil) (gptel-use-tools nil) (gptel--fsm-last nil))
            (setf (niwa-agent-invocation-frozen-configuration inv)
                  (niwa-agent-exec-freeze-configuration
                   "default" inv (list :backend backend :model 'gpt-5.5 :effort nil)))
            ;; Run the real agent send; only gptel's dispatch is cut short
            ;; by :dry-run, after its prompt transforms realize the payload.
            (cl-letf (((symbol-function 'gptel-request)
                       (lambda (_prompt &rest args)
                         (let ((fsm (apply orig "hi" :dry-run t args)))
                           (setq key (plist-get (plist-get (gptel-fsm-info fsm) :data)
                                                :prompt_cache_key))
                           fsm)))
                      ((symbol-function 'gptel--update-status) #'ignore))
              (niwa-agent-exec-run #'ignore "default" "task" inv agent-buf))
            (should (equal "root-id:a--11111111" key))))
      (delete-file gptel--openai-oauth-token-file)
      (kill-buffer agent-buf))))

;;; test-niwa-agent-exec.el ends here
