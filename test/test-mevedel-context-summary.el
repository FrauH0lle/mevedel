;;; test-mevedel-context-summary.el -- Tests for context summaries -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'mevedel-context-summary)
(require 'mevedel-models)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-stream)
(defvar gptel-tools)
(defvar gptel-use-tools)

(defconst test-mevedel-context-summary--continuation
  "## Scope
- Work
## Constraints & Preferences
- None
## Work & Evidence
- Evidence
## Key Decisions
- Decision
## Open Questions & Risks
- None
## Critical Context
- Context
## Relevant Files
- file.el
## Skills Invoked
- tdd
## Next Steps
- Continue")

(defconst test-mevedel-context-summary--handoff
  "## Scope
- Work
## Constraints & Preferences
- None
## Work & Evidence
- Evidence
## Key Decisions
- Decision
## Open Questions & Risks
- None
## Critical Context
- Context
## Relevant Files
- file.el
## Skills Invoked
- tdd")

(mevedel-deftest mevedel-context-summary--headings ()
  ,test
  (test)
  :doc "adds Next Steps only to the continuation schema"
  (should (equal "Next Steps"
                 (car (last (mevedel-context-summary--headings
                             'continuation)))))
  (should-not (member "Next Steps"
                      (mevedel-context-summary--headings 'handoff))))

(mevedel-deftest mevedel-context-summary--prompt ()
  ,test
  (test)
  :doc "renders the fixed purpose-specific contract"
  (should (string-match-p "final Next Steps"
                          (mevedel-context-summary--prompt 'continuation)))
  (should (string-match-p "Do not restate that task"
                          (mevedel-context-summary--prompt 'handoff))))

(mevedel-deftest mevedel-context-summary--input ()
  ,test
  (test)
  :doc "labels focus, retained state, guidance, and evidence as data"
  (let ((input (mevedel-context-summary--input
                "source" 'continuation "previous" "focus" "guidance")))
    (dolist (text '("source" "previous" "focus" "guidance"
                    "frozen untrusted evidence"))
      (should (string-match-p text input)))))

(mevedel-deftest mevedel-context-summary--estimated-tokens ()
  ,test
  (test)
  :doc "estimates the complete prompt at four characters per token"
  (should (= 3 (mevedel-context-summary--estimated-tokens "1234" "5678"))))

(mevedel-deftest mevedel-context-summary--policy-buffer ()
  ,test
  (test)
  :doc "selects the owning root session buffer from a nested agent"
  (let ((session (mevedel-session--create :name "main"))
        (root (generate-new-buffer " *summary-policy-root*"))
        (child (generate-new-buffer " *summary-policy-child*")))
    (unwind-protect
        (progn
          (with-current-buffer root
            (setq-local mevedel--session session))
          (with-current-buffer child
            (setq-local mevedel--session session
                        mevedel--agent-invocation 'nested)
            (should (eq root
                        (mevedel-context-summary--policy-buffer session)))))
      (kill-buffer root)
      (kill-buffer child))))

(mevedel-deftest mevedel-context-summary--validate-output ()
  ,test
  (test)
  :doc "accepts only the exact ordered purpose-specific heading contract"
  (should (equal test-mevedel-context-summary--continuation
                 (mevedel-context-summary--validate-output
                  test-mevedel-context-summary--continuation
                  'continuation)))
  (should (equal test-mevedel-context-summary--handoff
                 (mevedel-context-summary--validate-output
                  test-mevedel-context-summary--handoff 'handoff)))
  (dolist
      (case
       (list
        (list test-mevedel-context-summary--handoff 'continuation)
        (list test-mevedel-context-summary--continuation 'handoff)
        (list (concat test-mevedel-context-summary--handoff
                      "\n## Scope\n- duplicate")
              'handoff)
        (list
         (concat
          "## Constraints & Preferences\n- None\n## Scope\n- Work\n"
          (substring
           test-mevedel-context-summary--handoff
           (string-match "## Work & Evidence"
                         test-mevedel-context-summary--handoff)))
         'handoff)
        (list (concat test-mevedel-context-summary--handoff
                      "\n## Unexpected\n- no")
              'handoff)))
    (should-error
     (mevedel-context-summary--validate-output (car case) (cadr case)))))

(mevedel-deftest mevedel-context-summary-generate ()
  ,test
  (test)
  :doc "dispatches one isolated non-streaming continuation request"
  (let (captured callback-result)
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (workload &rest _)
                 (should (eq workload 'summarization))
                 '(:backend summary-backend :model summary-model
                   :effort high)))
              ((symbol-function 'mevedel-model-usable-input-tokens)
               (lambda (_policy) 100000))
              ((symbol-function 'gptel-get-preset)
               (lambda (&rest _) '(:description "test")))
              ((symbol-function 'gptel-request)
               (lambda (prompt &rest args)
                 (setq captured
                       (list :prompt prompt
                             :system (plist-get args :system)
                             :buffer (plist-get args :buffer)
                             :stream (plist-get args :stream)
                             :transforms (plist-get args :transforms)
                             :context (plist-get args :context)
                             :backend gptel-backend
                             :model gptel-model
                             :effort gptel-reasoning-effort
                             :tools gptel-tools
                             :use-tools gptel-use-tools))
                 (funcall (plist-get args :callback)
                          test-mevedel-context-summary--continuation
                          '(:tokens (:input 100 :output 20)))
                 'request-fsm)))
      (let ((cancel
             (mevedel-context-summary-generate
              "frozen evidence" 'continuation
              (lambda (result) (setq callback-result result))
              :previous-summary "older retained state"
              :focus "current work"
              :guidance "prefer exact errors")))
        (should (functionp cancel))))
    (ert-info ((format "result: %S" callback-result))
      (should (equal (plist-get callback-result :outcome) 'success)))
    (should (equal (plist-get callback-result :summary)
                   test-mevedel-context-summary--continuation))
    (should (string-match-p "frozen evidence" (plist-get captured :prompt)))
    (should (string-match-p "older retained state"
                            (plist-get captured :prompt)))
    (should (string-match-p "current work" (plist-get captured :prompt)))
    (should (string-match-p "prefer exact errors"
                            (plist-get captured :prompt)))
    (should (string-match-p "untrusted evidence"
                            (plist-get captured :system)))
    (should-not (plist-get captured :stream))
    (should-not (plist-get captured :transforms))
    (should (equal (plist-get captured :context)
                   '(:mevedel-context-summary t :purpose continuation)))
    (should (eq (plist-get captured :backend) 'summary-backend))
    (should (eq (plist-get captured :model) 'summary-model))
    (should (eq (plist-get captured :effort) 'high))
    (should-not (plist-get captured :tools))
    (should-not (plist-get captured :use-tools))
    (should-not (buffer-live-p (plist-get captured :buffer))))

  :doc "resolves nested Agent summaries from the owning root session"
  (let ((session (mevedel-session--create :name "main"))
        (root (generate-new-buffer " *summary-policy-owner*"))
        (child (generate-new-buffer " *summary-policy-caller*"))
        captured-model result)
    (unwind-protect
        (progn
          (with-current-buffer root
            (setq-local mevedel--session session
                        gptel-backend 'root-backend
                        gptel-model 'root-model
                        gptel-reasoning-effort nil
                        mevedel-model-workloads nil))
          (with-current-buffer child
            (setq-local mevedel--session session
                        mevedel--agent-invocation 'nested
                        gptel-backend 'child-backend
                        gptel-model 'child-model
                        gptel-reasoning-effort nil
                        mevedel-model-workloads nil)
            (cl-letf (((symbol-function
                        'mevedel-model-usable-input-tokens)
                       (lambda (_policy) 100000))
                      ((symbol-function 'gptel-get-preset)
                       (lambda (&rest _) '(:description "test")))
                      ((symbol-function 'gptel-request)
                       (lambda (_prompt &rest args)
                         (setq captured-model gptel-model)
                         (funcall (plist-get args :callback)
                                  test-mevedel-context-summary--handoff nil))))
              (mevedel-context-summary-generate
               "evidence" 'handoff
               (lambda (value) (setq result value))
               :session session))))
      (kill-buffer root)
      (kill-buffer child))
    (should (eq 'root-model captured-model))
    (should (eq 'success (plist-get result :outcome))))

  :doc "rejects purpose-inappropriate handoff output"
  (let (result)
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (&rest _)
                 '(:backend test-backend :model test-model)))
              ((symbol-function 'mevedel-model-usable-input-tokens)
               (lambda (_policy) 100000))
              ((symbol-function 'gptel-get-preset)
               (lambda (&rest _) '(:description "test")))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest args)
                 (funcall
                  (plist-get args :callback)
                  (concat test-mevedel-context-summary--handoff
                          "\n## Next Steps\n- Do parent work")
                  nil))))
      (mevedel-context-summary-generate
       "evidence" 'handoff (lambda (value) (setq result value))))
    (should (eq (plist-get result :outcome) 'error))
    (should (string-match-p "heading" (plist-get result :error))))

  :doc "fails oversized input locally without provider dispatch"
  (let (result request-called)
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (&rest _)
                 '(:backend test-backend :model test-model)))
              ((symbol-function 'mevedel-model-usable-input-tokens)
               (lambda (_policy) 1))
              ((symbol-function 'gptel-request)
               (lambda (&rest _) (setq request-called t))))
      (mevedel-context-summary-generate
       "too much evidence" 'handoff
       (lambda (value) (setq result value))))
    (should-not request-called)
    (should (eq (plist-get result :outcome) 'error))
    (should (eq (plist-get result :error-class) 'size)))

  :doc "allows independent summaries to remain in flight concurrently"
  (let (provider-callbacks request-buffers outcomes)
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (&rest _)
                 '(:backend test-backend :model test-model)))
              ((symbol-function 'mevedel-model-usable-input-tokens)
               (lambda (_policy) 100000))
              ((symbol-function 'gptel-get-preset)
               (lambda (&rest _) '(:description "test")))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest args)
                 (push (plist-get args :callback) provider-callbacks)
                 (push (plist-get args :buffer) request-buffers)
                 'request-fsm)))
      (mevedel-context-summary-generate
       "first evidence" 'handoff
       (lambda (result) (push (plist-get result :outcome) outcomes)))
      (mevedel-context-summary-generate
       "second evidence" 'handoff
       (lambda (result) (push (plist-get result :outcome) outcomes)))
      (should (= 2 (length provider-callbacks)))
      (should (cl-every #'buffer-live-p request-buffers))
      (dolist (callback provider-callbacks)
        (funcall callback test-mevedel-context-summary--handoff nil)))
    (should (equal outcomes '(success success)))
    (should-not (cl-some #'buffer-live-p request-buffers)))

  :doc "records policy, outcome, and usage telemetry without raw content"
  (let (started finished result)
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (&rest _)
                 '(:backend summary-provider :model summary-model
                   :effort high)))
              ((symbol-function 'mevedel-model-usable-input-tokens)
               (lambda (_policy) 100000))
              ((symbol-function 'gptel-backend-name)
               (lambda (_backend) "summary-provider"))
              ((symbol-function 'gptel-get-preset)
               (lambda (&rest _) '(:description "test")))
              ((symbol-function 'mevedel-telemetry-start)
               (lambda (&rest args)
                 (setq started args)
                 'summary-span))
              ((symbol-function 'mevedel-telemetry-finish)
               (lambda (&rest args) (setq finished args)))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest args)
                 (funcall (plist-get args :callback)
                          test-mevedel-context-summary--handoff
                          '(:tokens (:input 41 :output 17))))))
      (mevedel-context-summary-generate
       "PRIVATE-EVIDENCE" 'handoff
       (lambda (value) (setq result value))
       :session 'session))
    (should (eq (plist-get result :outcome) 'success))
    (should (equal started
                   '(session context-summary-request
                     :purpose handoff :backend "summary-provider"
                     :model summary-model :effort high)))
    (should (equal finished
                   '(summary-span :outcome success :error-class nil
                     :input-tokens 41 :output-tokens 17)))
    (let ((telemetry (prin1-to-string (list started finished))))
      (should-not (string-match-p "PRIVATE-EVIDENCE" telemetry))
      (should-not (string-match-p "## Scope" telemetry))))

  :doc "cancellation aborts the request and suppresses late callbacks"
  (let (provider-callback request-buffer outcomes)
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (&rest _)
                 '(:backend test-backend :model test-model)))
              ((symbol-function 'mevedel-model-usable-input-tokens)
               (lambda (_policy) 100000))
              ((symbol-function 'gptel-get-preset)
               (lambda (&rest _) '(:description "test")))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest args)
                 (setq provider-callback (plist-get args :callback)
                       request-buffer (plist-get args :buffer))
                 'request-fsm))
              ((symbol-function 'gptel-abort)
               (lambda (buffer)
                 (should (eq buffer request-buffer))
                 (funcall provider-callback 'abort nil))))
      (let ((cancel
             (mevedel-context-summary-generate
              "evidence" 'handoff
              (lambda (result)
                (push (plist-get result :outcome) outcomes)))))
        (funcall cancel)
        (funcall provider-callback
                 test-mevedel-context-summary--handoff nil)))
    (should (equal outcomes '(aborted)))
    (should-not (buffer-live-p request-buffer)))

  :doc "settles in the caller's buffer, not the provider's response buffer"
  (let ((caller (generate-new-buffer " *summary-caller*"))
        (provider (generate-new-buffer " *summary-provider*"))
        provider-callback settle-buffer)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                     (lambda (&rest _) '(:backend b :model m :effort nil)))
                    ((symbol-function 'mevedel-model-usable-input-tokens)
                     (lambda (_policy) 100000))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _) '(:description "test")))
                    ((symbol-function 'gptel-request)
                     (lambda (_prompt &rest args)
                       (setq provider-callback (plist-get args :callback))
                       'request-fsm)))
            (with-current-buffer caller
              (mevedel-context-summary-generate
               "evidence" 'handoff
               (lambda (_result) (setq settle-buffer (current-buffer)))))
            ;; gptel runs response callbacks in its own process buffer.
            (with-current-buffer provider
              (funcall provider-callback
                       test-mevedel-context-summary--handoff nil)))
          (should (eq caller settle-buffer)))
      (kill-buffer caller)
      (kill-buffer provider)))

  :doc "settles a failed policy resolution instead of signalling"
  (let (result)
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (&rest _) (error "No summarization provider"))))
      (mevedel-context-summary-generate
       "evidence" 'handoff (lambda (value) (setq result value))))
    (should (eq 'error (plist-get result :outcome)))
    (should (string-match-p "No summarization provider"
                            (plist-get result :error))))

  :doc "accepts required headings quoted inside a fenced code block"
  (let ((quoted
         (replace-regexp-in-string
          "## Relevant Files"
          "## Relevant Files\n```markdown\n## Not A Heading\n```"
          test-mevedel-context-summary--handoff t t)))
    (should (equal quoted
                   (mevedel-context-summary--validate-output
                    quoted 'handoff)))))

(provide 'test-mevedel-context-summary)

;;; test-mevedel-context-summary.el ends here
