;;; test-niwa-compact-run.el -- Tests for compaction run -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-request)
(require 'niwa)
(require 'niwa-agent-control)
(require 'niwa-agent-exec)
(require 'niwa-agent-runtime)
(require 'niwa-compact)
(require 'niwa-compact-estimation)
(require 'niwa-compact-evidence)
(require 'niwa-compact-run)
(require 'niwa-compact-target)
(require 'niwa-execution-transcript)
(require 'niwa-models)
(require 'niwa-hooks)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-system)
(require 'niwa-utilities)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-compact-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-compact-test-support"))

(defvar gptel--request-params)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-model)
(defvar gptel-reasoning-effort)

(niwa-deftest niwa-compact-run--label ()
  (with-temp-buffer
    (let* ((session (niwa-session--create :name "UI work"))
           (target-backend (gptel--make-backend :name "Codex"))
           (summary-backend (gptel--make-backend :name "Claude Code"))
           (state (niwa-compact-run--state-create
                   :session session :chat-buffer (current-buffer)
                   :target '(:origin "/root/partner")
                   :target-policy (list :backend target-backend :model 'target-model)
                   :policy (list :backend summary-backend :model (make-symbol "sonnet")))))
      (should (equal "[UI work /root/partner; target Codex:target-model; summary Claude Code:sonnet]"
                     (niwa-compact-run--label state)))
      (setf (niwa-compact-run--state-target state) nil)
      (should (string-prefix-p "[UI work /root;" (niwa-compact-run--label state)))
      (setf (niwa-compact-run--state-session state) nil)
      (should (string-prefix-p (format "[%s /root;" (buffer-name))
                               (niwa-compact-run--label state))))))

(niwa-deftest niwa-compact-run/model-messages ()
  (with-temp-buffer
    (let* ((state (niwa-compact-run--state-create
                   :chat-buffer (current-buffer) :aggressive t
                   :target '(:origin "/root/partner" :start ignore :complete ignore)
                   :target-policy (list :backend (gptel--make-backend :name "Codex")
                                        :model 'target-model)
                   :policy (list :backend (gptel--make-backend :name "Claude Code")
                                 :model (make-symbol "sonnet"))
                   :tokens-before 80000))
           (niwa-compact-run-warn-on-completion nil)
           messages)
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'niwa-compact-run--send-request) #'ignore)
                ((symbol-function 'niwa-compact-estimation-estimate-tokens)
                 (lambda () 16000))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (niwa-compact-run--begin-attempt state)
        (niwa-compact-run--finish-success state "summary"))
      (should (= 2 (length messages)))
      (dolist (text messages)
        (should (string-match-p
                 (regexp-quote "/root/partner; target Codex:target-model; summary Claude Code:sonnet]")
                 text)))
      (should (string-prefix-p "niwa: compacting [" (cadr messages)))
      (should (string-suffix-p "(buffer 80k -> ...)" (cadr messages)))
      (should (string-prefix-p "niwa: compaction complete [" (car messages)))
      (should (string-suffix-p "(buffer 80k -> 16k tokens, 0 turns preserved)"
                               (car messages))))))

(niwa-deftest niwa-compact-run--finish ()
  ,test
  (test)
  :doc "settles once and clears the in-flight marker"
  (with-temp-buffer
    (setq-local niwa-compact-run-in-flight t)
    (let* (results
           (state
            (niwa-compact-run--state-create
             :callback (lambda (err) (push err results))
             :chat-buffer (current-buffer))))
      (niwa-compact-run--finish state nil)
      (niwa-compact-run--finish state "late")
      (should (equal results '(nil)))
      (should-not niwa-compact-run-in-flight)))
  :doc "successful manual compaction releases held background input once"
  (dolist (case '((nil nil t) (t nil nil) (nil "failed" nil)))
    (with-temp-buffer
      (setq-local niwa-compact-run-in-flight t)
      (let* ((state (niwa-compact-run--state-create
                     :chat-buffer (current-buffer) :auto (car case)))
             offered)
        (cl-letf (((symbol-function 'niwa-pending-inputs-schedule-held-input-delivery)
                   (lambda (buffer)
                     (should-not niwa-compact-run-in-flight)
                     (push buffer offered))))
          (niwa-compact-run--finish state (cadr case))
          (niwa-compact-run--finish state (cadr case)))
        (should (equal (and (nth 2 case) (list (current-buffer))) offered))))))

(niwa-deftest niwa-compact-run--fail ()
  ,test
  (test)
  :doc "records one terminal failure and settles the run"
  (with-temp-buffer
    (setq-local niwa-compact-run-in-flight t)
    (let* ((niwa-compact-run-failure-count 0)
           result
           (state
            (niwa-compact-run--state-create
             :attempt 3
             :auto t
             :callback (lambda (err) (setq result err))
             :chat-buffer (current-buffer))))
      (cl-letf (((symbol-function 'display-warning) #'ignore))
        (niwa-compact-run--fail state "failed" t))
      (should (equal result "failed"))
      (should (= niwa-compact-run-failure-count 1))
      (should-not niwa-compact-run-in-flight))))

(niwa-deftest niwa-compact-run--finish-success ()
  ,test
  (test)
  :doc "runs PostCompact and target completion before settling"
  (let (completed result)
    (let ((state
           (niwa-compact-run--state-create
            :aggressive t
            :callback (lambda (err) (setq result err))
            :chat-buffer (current-buffer)
            :target
            (list :complete (lambda (&rest _) (setq completed t)))
            :tokens-before 200
            :trigger "manual")))
      (cl-letf (((symbol-function 'niwa-compact-estimation-estimate-tokens)
                 (lambda () 100))
                ((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'message) #'ignore))
        (let ((niwa-compact-run-warn-on-completion nil))
          (niwa-compact-run--finish-success state "summary")))
      (should completed)
      (should-not result))))

(niwa-deftest niwa-compact-run--apply-summary ()
  ,test
  (test)
  :doc "normalizes and applies the summary before successful settlement"
  (let (applied result)
    (let ((state
           (niwa-compact-run--state-create
            :aggressive t
            :callback (lambda (err) (setq result err))
            :chat-buffer (current-buffer)
            :summary-ready (lambda (summary) (concat summary " ready"))
            :target
            (list :apply
                  (lambda (_target summary &rest _)
                    (setq applied summary))
                  :complete #'ignore)
            :tokens-before 200
            :trigger "manual")))
      (cl-letf (((symbol-function 'niwa-compact-estimation-estimate-tokens)
                 (lambda () 100))
                ((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'message) #'ignore))
        (let ((niwa-compact-run-warn-on-completion nil))
          (niwa-compact-run--apply-summary state "summary" nil)))
      (should (equal applied "summary ready"))
      (should-not result))))

(niwa-deftest niwa-compact-run--send-request ()
  ,test
  (test)
  :doc "applies a prepared summary without sending a model request"
  (let (applied)
    (let ((state
           (niwa-compact-run--state-create
            :chat-buffer (current-buffer)
            :policy nil
            :prepared-summary "prepared")))
      (cl-letf (((symbol-function 'gptel-get-preset)
                 (lambda (&rest _) '(:description "test")))
                ((symbol-function 'gptel-request)
                 (lambda (&rest _)
                   (ert-fail "Prepared summary sent a request")))
                ((symbol-function 'niwa-compact-run--apply-summary)
                 (lambda (_state summary _audits)
                   (setq applied summary))))
        (niwa-compact-run--send-request state "system" nil))
      (should (equal applied "prepared"))))

  :doc "passes handoff focus and transformed evidence without previous authority"
  (let* ((target '(:previous-summary "retained"))
         (state
          (niwa-compact-run--state-create
           :chat-buffer (current-buffer)
           :focus "exact task"
           :old-content "evidence"
           :policy 'policy
           :purpose 'handoff
           :source-transform (lambda (source) (concat "filtered " source))
           :target target))
         captured)
    (cl-letf (((symbol-function 'niwa-context-summary-generate)
               (lambda (source purpose _callback &rest args)
                 (setq captured (list source purpose args))
                 #'ignore)))
      (niwa-compact-run--send-request state nil nil))
    (should (equal "filtered evidence" (car captured)))
    (should (eq 'handoff (cadr captured)))
    (should (equal "exact task" (plist-get (caddr captured) :focus)))
    (should-not (plist-get (caddr captured) :previous-summary))))

(niwa-deftest niwa-compact-run--begin-attempt ()
  ,test
  (test)
  :doc "admits the first attempt and starts the target once"
  (let (started sent-context)
    (let ((state
           (niwa-compact-run--state-create
            :old-content "body"
            :policy nil
            :target
            (list :origin "/root"
                  :start (lambda (_target) (setq started t)))
            :tokens-before 100
            :trigger "manual")))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'niwa-compact-run--send-request)
                 (lambda (_state context _audits)
                   (setq sent-context context)))
                ((symbol-function 'message) #'ignore))
        (niwa-compact-run--begin-attempt state))
      (should started)
      (should-not sent-context)
      (should (= (niwa-compact-run--state-attempt state) 1)))))

(niwa-deftest niwa-compact-run--prepare ()
  ,test
  (test)
  :doc "populates the run state from compactable and pending text"
  (with-temp-buffer
    (insert "history\n")
    (let* ((pending-start (point))
           (_ (insert "pending"))
           (ready #'identity)
           (state
            (niwa-compact-run--state-create
             :aggressive t
             :target '(:body-start 1 :invocation root))))
      (should
       (eq state
           (niwa-compact-run--prepare
            state pending-start '(:summary-policy policy)
            "instructions" pending-start "prepared" ready)))
      (should (string-match-p
               "history"
               (niwa-compact-run--state-old-content state)))
      (should-not (string-match-p
                   "pending"
                   (niwa-compact-run--state-old-content state)))
      (should (equal "pending"
                     (niwa-compact-run--state-pending-text state)))
      (should (eq 'policy (niwa-compact-run--state-policy state)))
      (should (equal "prepared"
                     (niwa-compact-run--state-prepared-summary state)))
      (should (eq ready
                  (niwa-compact-run--state-summary-ready state)))))

  :doc "keeps old directive turns in summarizer history as ordinary turns"
  (with-temp-buffer
    (insert "ordinary before\n")
    (insert (niwa--format-hook-audit-record
             '(:type directive-turn-boundary :edge start
               :directive-id "d1" :action discuss :turn 2)))
    (insert "directive prompt\ndirective answer\n")
    (insert (niwa--format-hook-audit-record
             '(:type directive-turn-boundary :edge end
               :directive-id "d1" :action discuss :turn 2
               :outcome success :sequence 1)))
    (insert "ordinary after\n")
    (let ((state
           (niwa-compact-run--state-create
            :aggressive t
            :target '(:body-start 1 :invocation root))))
      (niwa-compact-run--prepare
       state (point-max) '(:summary-policy policy)
       nil nil "prepared" #'identity)
      (let ((history (niwa-compact-run--state-old-content state)))
        (should (string-match-p "ordinary before" history))
        (should (string-match-p "ordinary after" history))
        (should (string-match-p "directive prompt" history))
        (should (string-match-p "directive answer" history))))))

(niwa-deftest niwa-compact-run-start (:quiet t)
  ,test
  (test)
  :doc "a rejected attempt leaves no unmatched telemetry span"
  ;; A start with no finish is an interval with no end, and the Goal
  ;; reproduction procedure reads those pairs to attribute time to
  ;; compaction that never ran.
  (test-niwa-compact--with-persisted-buffer (buffer session)
    (insert "Prompt\n")
    (insert (propertize "Response\n" 'gptel 'response))
    ;; No boundary yet: the attempt is measured, rejected, and its
    ;; span closed before the signal reaches the caller.
    (should-error (niwa-compact-run-start) :type 'user-error)
    (let ((niwa-compact-run-in-flight t))
      (should-error (niwa-compact-run-start) :type 'user-error))
    (let* ((stages
            (mapcar (lambda (event) (plist-get event :stage))
                    (seq-filter
                     (lambda (event)
                       (eq (plist-get event :event) 'compaction))
                     (test-niwa-compact--read-telemetry session)))))
      (should (= (seq-count (lambda (stage) (eq stage 'start)) stages)
                 (seq-count (lambda (stage) (eq stage 'finish)) stages)))))

  :doc "a settlement before the signal is not finished twice"
  ;; `--begin-attempt' settles through the owner and re-signals; the
  ;; outer handler must find the span already consumed.
  (test-niwa-compact--with-persisted-buffer (buffer session)
    (insert "Prompt\n")
    (insert (propertize "Response\n" 'gptel 'response)
            "Prompt two\n"
            (propertize "Response two\n" 'gptel 'response))
    (cl-letf (((symbol-function 'niwa-compact-run--begin-attempt)
               (lambda (state)
                 (niwa-compact-run--finish state "boom")
                 (error "Hook failed"))))
      ;; Aggressive drops the preserved tail, so the admission gates pass
      ;; and the signal really comes from the settled attempt.
      (let ((err (should-error (niwa-compact-run-start :aggressive t))))
        (should (equal "Hook failed" (cadr err)))))
    (let ((stages
           (mapcar (lambda (event) (plist-get event :stage))
                   (seq-filter
                    (lambda (event)
                      (eq (plist-get event :event) 'compaction))
                    (test-niwa-compact--read-telemetry session)))))
      (should (equal '(start finish) stages))))

  :doc "rejects an unpersisted buffer before hooks or model requests"
  (with-temp-buffer
    (org-mode)
    (insert "Prompt\n")
    (let (hook-called request-called)
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (&rest _)
                   (setq hook-called t)))
                ((symbol-function 'gptel-request)
                 (lambda (&rest _)
                   (setq request-called t))))
        (should-error
         (niwa-compact-run-start :aggressive t :pending-start (point-max))
         :type 'user-error))
      (should-not hook-called)
      (should-not request-called)))

  :doc "summarizer-only pressure skips when no compactable prefix remains"
  (test-niwa-compact--with-persisted-buffer (buffer session)
    (insert "Prompt\n")
    (insert (propertize "Response\n" 'gptel 'response))
    (let ((pending-start (point)) result)
      (insert "Pending\n")
      (niwa-compact-run-start
       :pending-start pending-start
       :auto t
       :admission
       '(:summary-policy (:backend nil :model nil :max-tokens 0)
         :target-pressure nil)
       :callback (lambda (err) (setq result err)))
      (should (eq result :skip))))

  :doc "target pressure fails when no compactable prefix remains"
  (test-niwa-compact--with-persisted-buffer (buffer session)
    (insert "Prompt\n")
    (insert (propertize "Response\n" 'gptel 'response))
    (let ((pending-start (point)) result request-called)
      (insert "Pending\n")
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (setq request-called t))))
        (niwa-compact-run-start
         :pending-start pending-start
         :auto t
         :admission
         '(:summary-policy (:backend nil :model nil :max-tokens 0)
           :target-pressure t)
         :callback (lambda (err) (setq result err))))
      (should (string-match-p "No compactable history" result))
      (should-not request-called)))

  :doc "preflight includes the capped body, base prompt, and PreCompact context"
  (test-niwa-compact--with-persisted-buffer (chat-buf session)
    (let (result request-called)
      (insert (make-string 120 ?b) "\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (put 'niwa-small-summary-model :context-window 0.08)
      (cl-letf (((symbol-function 'niwa-system-render-prompt-file)
                     (lambda (&rest _) (make-string 120 ?s)))
                    ((symbol-function 'niwa-hooks-run-event)
                     (lambda (_event _plist callback &rest _)
                       (funcall callback
                                (list :additional-context
                                      (list (make-string 120 ?h))))))
                    ((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'gptel-request)
                     (lambda (&rest _) (setq request-called t))))
            (let ((niwa-model-reserve-tokens 0))
              (niwa-compact-run-start
               :aggressive t
               :pending-start (point-max)
               :auto t
               :admission
               '(:summary-policy
                 (:backend nil :model niwa-small-summary-model
                  :max-tokens 0 :request-params nil)
               :target-pressure t)
               :callback (lambda (err) (setq result err)))))
      (should (string-match-p "exceeds usable context" result))
      (should-not request-called)
      (should (= niwa-compact-run-failure-count 0))
      (should-not niwa-compact-run-in-flight)))

  :doc "summarizes a forked history prefix while excluding the stable task anchor"
  (test-niwa-compact--with-persisted-buffer (chat-buf session)
    (let (captured-prompt result)
      (insert "Inherited live context.\n")
      (let ((history-prefix-end (point)))
        (insert "Stable child task anchor.\n")
        (let ((body-start (point)))
          (insert "Child conversation body.\n")
          (let ((pending-start (point))
                (target (niwa-compact-target-main-target)))
            (insert "Pending continuation.\n")
            (setq target
                  (plist-put target :history-prefix-regions
                             (list (cons (point-min) history-prefix-end))))
            (setq target (plist-put target :body-start body-start))
            (cl-letf (((symbol-function 'niwa-system-render-prompt-file)
                       (lambda (&rest _) "system prompt"))
                      ((symbol-function 'niwa-hooks-run-event)
                       (lambda (_event _plist callback &rest _)
                         (funcall callback nil)))
                      ((symbol-function 'display-warning) #'ignore)
                      ((symbol-function 'gptel-request)
                       (lambda (prompt &rest args)
                         (setq captured-prompt prompt)
                         (funcall (plist-get args :callback) 'abort nil))))
              (niwa-compact-run-start
               :target target
               :aggressive t
               :pending-start pending-start
               :callback (lambda (err) (setq result err)))))))
      (should (string-match-p "Inherited live context" captured-prompt))
      (should (string-match-p "Child conversation body" captured-prompt))
      (should-not (string-match-p "Stable child task anchor" captured-prompt))
      (should (equal result "Compaction aborted"))))

  :doc "successful root compaction starts a compact epoch after PostCompact"
  (test-niwa-compact--with-persisted-buffer (chat-buf session)
    (let (events result)
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (let* ((pending-start (point))
             (target (niwa-compact-target-main-target)))
        (insert "Pending prompt\n")
        (setq target
              (plist-put target :apply
                         (lambda (&rest _)
                           (push 'apply events))))
        (setq target
              (plist-put target :complete
                         (lambda (&rest _)
                           (push 'complete events))))
        (cl-letf (((symbol-function 'niwa-system-render-prompt-file)
                   (lambda (&rest _) "system prompt"))
                  ((symbol-function 'niwa-hooks-run-event)
                   (lambda (event _payload callback &rest _)
                     (push event events)
                     (funcall callback nil)))
                  ((symbol-function 'niwa--run-session-start-hooks)
                   (lambda (source)
                     (push (list 'SessionStart source) events)
                     (niwa-hooks-record-session-context
                      session
                      '(:additional-context ("compact context"))
                      'SessionStart)))
                  ((symbol-function 'gptel-get-preset)
                   (lambda (&rest _) '(:description "test")))
                  ((symbol-function 'gptel-request)
                   (lambda (_prompt &rest args)
                     (funcall (plist-get args :callback)
                              test-niwa-compact--valid-summary nil)))
                  ((symbol-function 'message) #'ignore))
          (niwa-compact-run-start
           :target target
           :aggressive t
           :pending-start pending-start
           :auto t
           :admission
           '(:summary-policy (:backend nil :model nil :max-tokens 0)
             :target-pressure t)
           :callback (lambda (err) (setq result err)))))
      (should-not result)
      (should (equal (nreverse events)
                     '(PreCompact apply PostCompact
                       (SessionStart "compact") complete)))
      (should (string-match-p "compact context" (buffer-string)))
      (should-not (niwa-session-hook-context-pending session))
      (should (string-match-p
               "compact context"
               niwa-compact-target-current-request-hook-context))))

  :doc "successful retained-agent compaction does not start a root epoch"
  (test-niwa-compact--with-persisted-buffer (chat-buf session)
    (let (events result)
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (let* ((pending-start (point))
             (target (niwa-compact-target-main-target)))
        (insert "Pending prompt\n")
        (setq target (plist-put target :invocation 'retained-agent))
        (setq target (plist-put target :origin "/root/agent"))
        (setq target (plist-put target :apply (lambda (&rest _))))
        (setq target (plist-put target :complete (lambda (&rest _))))
        (cl-letf (((symbol-function 'niwa-system-render-prompt-file)
                   (lambda (&rest _) "system prompt"))
                  ((symbol-function 'niwa-hooks-run-event)
                   (lambda (event _payload callback &rest _)
                     (push event events)
                     (funcall callback nil)))
                  ((symbol-function 'niwa--run-session-start-hooks)
                   (lambda (_source)
                     (ert-fail "Agent compaction started a root epoch")))
                  ((symbol-function 'gptel-get-preset)
                   (lambda (&rest _) '(:description "test")))
                  ((symbol-function 'gptel-request)
                   (lambda (_prompt &rest args)
                     (funcall (plist-get args :callback)
                              test-niwa-compact--valid-summary nil)))
                  ((symbol-function 'message) #'ignore))
          (niwa-compact-run-start
           :target target
           :aggressive t
           :pending-start pending-start
           :auto t
           :admission
           '(:summary-policy (:backend nil :model nil :max-tokens 0)
             :target-pressure t)
           :callback (lambda (err) (setq result err)))))
      (should-not result)
      (should (equal (nreverse events) '(PreCompact PostCompact)))))

  :doc "applies a prepared summary without another model request"
  (test-niwa-compact--with-persisted-buffer (chat-buf session)
    (let (applied result)
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (let ((target (niwa-compact-target-main-target)))
        (setq target
              (plist-put target :apply
                         (lambda (_target summary &rest _)
                           (setq applied summary))))
        (setq target (plist-put target :complete (lambda (&rest _))))
        (cl-letf (((symbol-function 'niwa-system-render-prompt-file)
                   (lambda (&rest _) "system prompt"))
                  ((symbol-function 'niwa-hooks-run-event)
                   (lambda (_event _payload callback &rest _)
                     (funcall callback nil)))
                  ((symbol-function 'niwa--run-session-start-hooks)
                   #'ignore)
                  ((symbol-function 'gptel-request)
                   (lambda (&rest _)
                     (ert-fail "Prepared summary sent another request")))
                  ((symbol-function 'message) #'ignore))
          (niwa-compact-run-start
           :target target
           :aggressive t
           :prepared-summary "cached"
           :summary-ready (lambda (summary) (concat summary " ready"))
           :callback (lambda (err) (setq result err)))))
      (should-not result)
      (should (equal applied "cached ready"))))

  :doc "blocked compaction emits neither PostCompact nor SessionStart"
  (test-niwa-compact--with-persisted-buffer (chat-buf session)
    (let (events result)
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (event _payload callback &rest _)
                   (push event events)
                   (funcall callback
                            '(:continue nil :stop-reason "blocked"))))
                ((symbol-function 'niwa--run-session-start-hooks)
                 (lambda (_source)
                   (ert-fail "Blocked compaction started a context epoch")))
                ((symbol-function 'gptel-request)
                 (lambda (&rest _)
                   (ert-fail "Blocked compaction sent a request"))))
        (niwa-compact-run-start
         :aggressive t
         :pending-start (point-max)
         :callback (lambda (err) (setq result err))))
      (should (equal "blocked" result))
      (should (equal (nreverse events) '(PreCompact)))))

  :doc "cancels the isolated summary request and settles compaction"
  (test-niwa-compact--with-persisted-buffer (chat-buf session)
    (let (provider-callback provider-cancelled result)
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'niwa-context-summary-generate)
                 (lambda (_source _purpose callback &rest _)
                   (setq provider-callback callback)
                   (lambda ()
                     (setq provider-cancelled t)
                     (funcall provider-callback '(:outcome aborted)))))
                ((symbol-function 'display-warning) #'ignore)
                ((symbol-function 'message) #'ignore))
        (niwa-compact-run-start
         :aggressive t
         :pending-start (point-max)
         :callback (lambda (err) (setq result err)))
        (should niwa-compact-run-in-flight)
        (funcall niwa-compact-run-cancel))
      (should provider-cancelled)
      (should (equal result "Compaction aborted"))
      (should-not niwa-compact-run-in-flight)
      (should-not niwa-compact-run-cancel)
      (should (= niwa-compact-run-failure-count 0))))

  :doc "cancellation rejects a late PreCompact callback"
  (test-niwa-compact--with-persisted-buffer (chat-buf session)
    (let (hook-callback result
          (requests 0)
          (starts 0))
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (let ((target (niwa-compact-target-main-target)))
        (setq target
              (plist-put target :start (lambda (&rest _) (cl-incf starts))))
        (cl-letf (((symbol-function 'niwa-hooks-run-event)
                   (lambda (_event _payload callback &rest _)
                     (setq hook-callback callback)))
                  ((symbol-function 'niwa-context-summary-generate)
                   (lambda (&rest _)
                     (cl-incf requests))))
          (niwa-compact-run-start
           :target target
           :aggressive t
           :pending-start (point-max)
           :callback (lambda (err) (setq result err)))
          (should hook-callback)
          (funcall niwa-compact-run-cancel)
          (funcall hook-callback nil)))
      (should (equal result "Compaction aborted"))
      (should (= starts 0))
      (should (= requests 0))))

  :doc "applied compaction wins cancellation while PostCompact is held"
  (test-niwa-compact--with-persisted-buffer (chat-buf session)
    (let (hook-callback results
          (completes 0)
          (epochs 0))
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (let ((target (niwa-compact-target-main-target)))
        (setq target (plist-put target :apply #'ignore))
        (setq target
              (plist-put target :complete
                         (lambda (&rest _) (cl-incf completes))))
        (cl-letf (((symbol-function 'niwa-hooks-run-event)
                   (lambda (event _payload callback &rest _)
                     (if (eq event 'PostCompact)
                         (setq hook-callback callback)
                       (funcall callback nil))))
                  ((symbol-function 'niwa-compact-target-begin-root-context-epoch)
                   (lambda (&rest _) (cl-incf epochs)))
                  ((symbol-function 'message) #'ignore))
          (niwa-compact-run-start
           :target target
           :aggressive t
           :prepared-summary "cached"
           :callback (lambda (err) (push err results)))
          (should hook-callback)
          (funcall niwa-compact-run-cancel)
          (should-not results)
          (should niwa-compact-run-in-flight)
          (funcall hook-callback nil)))
      (should (equal results '(nil)))
      (should (= epochs 1))
      (should (= completes 1))
      (should-not niwa-compact-run-in-flight)
      (should-not niwa-compact-run-cancel)))

  :doc "cancellation owns retry backoff and rejects stale summary callbacks"
  (test-niwa-compact--with-persisted-buffer (chat-buf session)
    (let ((requests 0)
          (hooks 0)
          (retry-token (list 'retry))
          applied
          cancelled-timer
          provider-callback
          result
          retry-callback)
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (let ((target (niwa-compact-target-main-target)))
        (setq target
              (plist-put
               target :apply
               (lambda (_target summary &rest _)
                 (setq applied summary))))
        (setq target (plist-put target :complete #'ignore))
        (cl-letf (((symbol-function 'niwa-hooks-run-event)
                   (lambda (_event _payload callback &rest _)
                     (cl-incf hooks)
                     (funcall callback nil)))
                  ((symbol-function 'niwa-context-summary-generate)
                   (lambda (_source _purpose callback &rest _)
                     (cl-incf requests)
                     (setq provider-callback callback)
                     (when (= requests 1)
                       (funcall callback
                                '(:outcome error :error "temporary")))
                     #'ignore))
                  ((symbol-function 'run-at-time)
                   (lambda (_delay _repeat callback &rest args)
                     (setq retry-callback
                           (lambda () (apply callback args)))
                     retry-token))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer)
                     (setq cancelled-timer timer)))
                  ((symbol-function 'display-warning) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (niwa-compact-run-start
           :target target
           :aggressive t
           :pending-start (point-max)
           :callback (lambda (err) (setq result err)))
          (should retry-callback)
          (funcall niwa-compact-run-cancel)
          (funcall provider-callback
                   '(:outcome success :summary "late summary"))
          (funcall retry-callback)))
      (should (equal result "Compaction aborted"))
      (should (eq cancelled-timer retry-token))
      (should (= hooks 1))
      (should (= requests 1))
      (should-not applied)
      (should-not niwa-compact-run-in-flight)
      (should-not niwa-compact-run-cancel)))

  :doc "settles a synchronous generator failure as a retryable attempt"
  (test-niwa-compact--with-persisted-buffer (chat-buf session)
    (let ((attempts 0) result)
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'niwa-context-summary-generate)
                 (lambda (&rest _)
                   (cl-incf attempts)
                   (error "Summarization provider is unavailable")))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest args)
                   (apply function args)))
                ((symbol-function 'display-warning) #'ignore)
                ((symbol-function 'message) #'ignore))
        (niwa-compact-run-start
         :aggressive t
         :pending-start (point-max)
         :callback (lambda (err) (setq result err))))
      (should (= 3 attempts))
      (should (string-match-p "Summarization provider is unavailable" result))
      (should-not niwa-compact-run-in-flight)))

  :doc "request failures retain three identical attempts"
  (test-niwa-compact--with-persisted-buffer (chat-buf session)
    (let ((attempts 0) events prompts result)
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (cl-letf (((symbol-function 'niwa-system-render-prompt-file)
                     (lambda (&rest _) "system prompt"))
                    ((symbol-function 'niwa-hooks-run-event)
                     (lambda (event _plist callback &rest _)
                       (push event events)
                       (funcall callback nil)))
                    ((symbol-function 'niwa--run-session-start-hooks)
                     (lambda (_source)
                       (ert-fail "Failed compaction started a context epoch")))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _) '(:description "test")))
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest args)
                       (apply function args)))
                    ((symbol-function 'message) #'ignore)
                    ((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'gptel-request)
                     (lambda (prompt &rest args)
                       (cl-incf attempts)
                       (push prompt prompts)
                       (funcall (plist-get args :callback)
                                nil '(:error "temporary")))))
            (niwa-compact-run-start
             :aggressive t
             :pending-start (point-max)
             :auto t
             :admission
             '(:summary-policy (:backend nil :model nil :max-tokens 0)
               :target-pressure t)
             :callback (lambda (err) (setq result err))))
      (should (= attempts 3))
      (should (equal (nreverse events)
                     '(PreCompact PreCompact PreCompact)))
      (should (= 1 (length (delete-dups prompts))))
      (should (string-match-p "temporary" result))))

  :doc "manual compaction failure stops the view spinner"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-chat*"))
        (view-buf (generate-new-buffer " *niwa-compact-view*"))
        (updated-status nil)
        (stopped nil))
    (unwind-protect
        (with-current-buffer chat-buf
          (org-mode)
          (setq-local niwa-compact-run-in-flight nil)
          (setq-local niwa--view-buffer view-buf)
          (setq-local niwa--session nil)
          (insert "Prompt\n")
          (insert (propertize "Response\n" 'gptel 'response))
          (setq-local gptel--request-alist nil)
          (setq-local gptel-use-tools nil)
          (setq-local gptel-tools nil)
          (cl-letf (((symbol-function 'niwa-compact-target-current-persisted-p)
                     (lambda () t))
                    ((symbol-function 'niwa-system-render-prompt-file)
                     (lambda (&rest _)
                       "system prompt"))
                    ((symbol-function 'niwa-view-stream-show-progress)
                     (lambda (_view status &optional _owner)
                       (setq updated-status status)))
                    ((symbol-function 'niwa-view--stop-spinner)
                     (lambda ()
                       (setq stopped t)))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _)
                       '(:description "test")))
                    ((symbol-function 'run-at-time)
                     (lambda (_time _repeat function &rest args)
                       (apply function args)
                       nil))
                    ((symbol-function 'message)
                     #'ignore)
                    ((symbol-function 'display-warning)
                     #'ignore)
                    ((symbol-function 'gptel-request)
                     (lambda (_prompt &rest args)
                       (funcall (plist-get args :callback) 'abort nil))))
            (niwa-compact-run-start :aggressive t :pending-start (point-max)))
          (should (equal updated-status "Compacting..."))
          (should stopped)
          (should-not niwa-compact-run-in-flight))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))
      (when (buffer-live-p view-buf)
        (kill-buffer view-buf))))

  :doc "applies a generated continuation summary with PreCompact evidence"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-summary-callback*"))
        applied-summary
        applied-hook-audits
        captured-source
        captured-purpose
        failure)
    (unwind-protect
        (with-current-buffer chat-buf
          (org-mode)
          (setq-local niwa-compact-run-in-flight nil)
          (setq-local niwa--session nil)
          (insert "Prompt\n")
          (insert (propertize "Response\n" 'gptel 'response))
          (setq-local gptel--request-alist nil)
          (setq-local gptel-use-tools nil)
          (setq-local gptel-tools nil)
          (setq-local gptel-stream t)
          (let ((niwa-compact-run-warn-on-completion nil)
                (niwa-pre-compact-functions
                 (list (lambda (_event)
                         '(:additional-context ("compact note")
                           :system-message "because")))))
            (cl-letf (((symbol-function 'niwa-compact-target-current-persisted-p)
                       (lambda () t))
                      ((symbol-function 'niwa-compact-target--apply)
                       (lambda (summary _tail _pending hook-audits &rest _)
                         (setq applied-summary summary
                               applied-hook-audits hook-audits)))
                      ((symbol-function 'niwa-reminders-rearm-plan-reference)
                       #'ignore)
                      ((symbol-function 'message)
                       #'ignore)
                      ((symbol-function 'display-warning)
                       (lambda (_type message &rest _)
                         (setq failure message)))
                      ((symbol-function 'niwa-context-summary-generate)
                       (lambda (source purpose callback &rest _)
                         (setq captured-source source
                               captured-purpose purpose)
                         (funcall callback
                                  (list :outcome 'success
                                        :summary
                                        test-niwa-compact--valid-summary))
                         #'ignore)))
              (niwa-compact-run-start :aggressive t :pending-start (point-max))))
          (should (eq captured-purpose 'continuation))
          (should (string-match-p "compact note" captured-source))
          (should-not failure)
          (should (equal test-niwa-compact--valid-summary applied-summary))
          (should-not (string-match-p "<!-- niwa-hook-audit -->"
                                      applied-summary))
          (should (= 1 (length applied-hook-audits)))
          (let ((audit (car applied-hook-audits)))
            (should (eq (plist-get audit :type) 'compact-context))
            (should (equal (plist-get (car (plist-get audit :handlers))
                                     :contexts)
                           '("compact note"))))
          (should-not niwa-compact-run-in-flight))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))))

  :doc "uses the summarization workload tier for the request"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-workload*"))
        (workload-backend (gptel--make-backend :name "workload-backend"))
        (captured-workload nil)
        captured-policy)
    (unwind-protect
        (with-current-buffer chat-buf
          (org-mode)
          (setq-local niwa-compact-run-in-flight nil)
          (setq-local niwa--session nil)
          (insert "Prompt\n")
          (insert (propertize "Response\n" 'gptel 'response))
          (cl-letf (((symbol-function 'niwa-compact-target-current-persisted-p)
                     (lambda () t))
                    ((symbol-function 'niwa-model-resolve-workload)
                     (lambda (workload &rest _)
                       (setq captured-workload workload)
                       (list :backend workload-backend :model 'workload-model
                             :effort 'high)))
                    ((symbol-function 'message)
                     #'ignore)
                    ((symbol-function 'display-warning)
                     #'ignore)
                    ((symbol-function 'niwa-context-summary-generate)
                     (lambda (_source _purpose callback &rest args)
                       (setq captured-policy (plist-get args :policy))
                       (funcall callback '(:outcome aborted))
                       #'ignore)))
            (niwa-compact-run-start :aggressive t :pending-start (point-max)))
          (should (eq captured-workload 'summarization))
          (should (eq (plist-get captured-policy :backend)
                      workload-backend))
          (should (eq (plist-get captured-policy :model) 'workload-model))
          (should (eq (plist-get captured-policy :effort) 'high))
          (should-not niwa-compact-run-in-flight))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))))

  :doc "async PreCompact hook marks compaction in flight before request"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-prehook*"))
        hook-callback
        request-called)
    (unwind-protect
        (with-current-buffer chat-buf
          (org-mode)
          (setq-local niwa-compact-run-in-flight nil)
          (setq-local niwa--session nil)
          (insert "Prompt\n")
          (insert (propertize "Response\n" 'gptel 'response))
          (setq-local gptel--request-alist nil)
          (setq-local gptel-use-tools nil)
          (setq-local gptel-tools nil)
          (cl-letf (((symbol-function 'niwa-compact-target-current-persisted-p)
                     (lambda () t))
                    ((symbol-function 'niwa-system-render-prompt-file)
                     (lambda (&rest _)
                       "system prompt"))
                    ((symbol-function 'gptel-request)
                     (lambda (&rest _)
                       (setq request-called t)))
                    ((symbol-function 'niwa-hooks-run-event)
                     (lambda (_event _plist callback &rest _)
                       (setq hook-callback callback)))
                    ((symbol-function 'message)
                     #'ignore)
                    ((symbol-function 'display-warning)
                     #'ignore))
            (niwa-compact-run-start :aggressive t :pending-start (point-max))
            (should niwa-compact-run-in-flight)
            (should hook-callback)
            (should-not request-called)
            (should-error
             (niwa-compact-run-start :aggressive t :pending-start (point-max))
             :type 'user-error)
            (funcall hook-callback
                     '(:continue nil :stop-reason "blocked"))
            (should-not niwa-compact-run-in-flight)))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))))

  :doc "auto compaction keeps file-reference reminder out of pending FIFO"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-auto-reminder*"))
        queued)
    (unwind-protect
        (with-current-buffer chat-buf
          (org-mode)
          (setq-local niwa-compact-run-in-flight nil)
          (setq-local niwa--session nil)
          (insert "Prompt\n")
          (insert (propertize "Response\n" 'gptel 'response))
          (setq-local gptel--request-alist nil)
          (setq-local gptel-use-tools nil)
          (setq-local gptel-tools nil)
          (let ((niwa-compact-run-warn-on-completion nil))
            (cl-letf (((symbol-function 'niwa-compact-target-current-persisted-p)
                       (lambda () t))
                      ((symbol-function 'niwa-system-render-prompt-file)
                       (lambda (&rest _)
                         "system prompt"))
                      ((symbol-function 'gptel-get-preset)
                       (lambda (&rest _)
                         '(:description "test")))
                      ((symbol-function 'niwa-compact-target--apply)
                       #'ignore)
                      ((symbol-function 'niwa-reminders-rearm-plan-reference)
                       #'ignore)
                      ((symbol-function 'niwa-compact-target-file-reference-reminder-body)
                       (lambda (&rest _)
                         "Re-read /tmp/old.el"))
                      ((symbol-function 'niwa-session-enqueue-pending-reminder)
                       (lambda (_session body)
                         (setq queued body)))
                      ((symbol-function 'niwa-hooks-run-event)
                       (lambda (_event _plist callback &rest _)
                         (funcall callback nil)))
                      ((symbol-function 'message)
                       #'ignore)
                      ((symbol-function 'display-warning)
                       #'ignore)
                      ((symbol-function 'gptel-request)
                       (lambda (_prompt &rest args)
                         (funcall (plist-get args :callback)
                                  test-niwa-compact--valid-summary nil))))
              (niwa-compact-run-start
               :aggressive t
               :pending-start (point-max)
               :auto t)))
          (should (equal niwa-compact-target-current-request-reminder
                         "Re-read /tmp/old.el"))
          (should-not queued))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))


(provide 'test-niwa-compact-run)

;;; test-niwa-compact-run.el ends here
