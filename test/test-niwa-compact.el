;;; test-niwa-compact.el -- Tests for niwa-compact.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
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
(require 'niwa-tools)
(require 'niwa-utilities)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
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


(niwa-deftest niwa-compact ()
  ,test
  (test)
  :doc "refuses direct invocation from historical segment inspection"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (cl-letf (((symbol-function 'niwa-view-historical-segment-p)
                 (lambda () t)))
        (should-error (niwa-compact) :type 'user-error))))

  :doc "loads facade-owned compaction gates without the umbrella"
  (let ((root (file-name-directory (locate-library "niwa-compact")))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (should
       (= 0
          (call-process
           emacs nil t nil "--batch" "-Q" "-L" root
           "--eval"
           (prin1-to-string
            '(progn
               (require 'niwa-compact)
               (let ((niwa-compact-auto nil))
                 (niwa--compact-auto-eligible-p))
               (unless (and (featurep 'niwa-compact-run)
                            (featurep 'niwa-compact-target))
                 (error "Facade did not load compaction owners")))))))
      (should (string-empty-p (string-trim (buffer-string)))))))


(niwa-deftest niwa--compact-should-compact-p ()
  ,test
  (test)
  :doc "persists both budgets and fallback provenance even below the trigger"
  (let* ((root (make-temp-file "niwa-context-diagnostics-" t))
         (workspace (niwa-workspace--create
                     :type 'project :id "context-diagnostics" :root root
                     :name "context-diagnostics"))
         (session (niwa-session-create "main" workspace root))
         (gptel-model (make-symbol "unknown-target"))
         (gptel-backend nil)
         (gptel-max-tokens nil)
         (gptel--request-params nil)
         (summary-model (make-symbol "known-summary"))
         (niwa-model-context-limit nil)
         (niwa-model-reserve-tokens 20000)
         (niwa-compact-estimation-token-threshold 0.8))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (setf (niwa-session-save-path session) root)
          (put summary-model :context-window 1000)
          (cl-letf (((symbol-function 'niwa-compact-estimation-workload-policy)
                     (lambda () (list :model summary-model))))
            (should-not (niwa--compact-should-compact-p 10)))
          (let ((entry (with-temp-buffer
                         (insert-file-contents
                          (file-name-concat root "telemetry-log.el"))
                         (read (current-buffer)))))
            (should (eq 'compaction-threshold-evaluated (plist-get entry :event)))
            (should (= 128000 (plist-get entry :model-context-window)))
            (should (eq 'built-in-fallback (plist-get entry :model-context-source)))
            (should (= 20000 (plist-get entry :target-reserve-tokens)))
            (should (= 108000 (plist-get entry :target-usable-tokens)))
            (should (= 86400 (plist-get entry :target-threshold)))
            (should (equal (symbol-name summary-model)
                           (symbol-name (plist-get entry :summary-model))))
            (should (= 1000000 (plist-get entry :summary-context-window)))
            (should (eq 'model-metadata (plist-get entry :summary-context-source)))
            (should (= 20000 (plist-get entry :summary-reserve-tokens)))
            (should (= 980000 (plist-get entry :summary-usable-tokens)))
            (should (= 784000 (plist-get entry :summary-threshold)))
            (should (= 0.8 (plist-get entry :threshold-ratio)))
            (should (= 86400 (plist-get entry :threshold)))))
      (delete-directory root t))))

(niwa-deftest niwa--compact-provider-wait
  (:doc "injects reminders and records dispatch before forwarding the FSM wait")
  (let* ((session (niwa-session--create :name "provider-dispatch"))
         (buffer (generate-new-buffer " *compact-provider-dispatch*"))
         (fsm (gptel-make-fsm
               :info (list :buffer buffer :niwa-request-id "request-1"
                           :model 'model :reasoning-effort 'high)))
         captured forwarded events)
    (unwind-protect
        (progn
          (with-current-buffer buffer (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-telemetry-record)
                     (lambda (_session event &rest props)
                       (setq captured (cons event props))))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (value)
                       (push 'provider events)
                       (setq forwarded value)))
                    ((symbol-function 'niwa-context-delivery-stage)
                     (lambda (_fsm) (push 'context events)))
                    ((symbol-function 'niwa-reminders--handle-inject)
                     (lambda (_fsm) (push 'reminders events))))
            (niwa--compact-provider-wait fsm))
          (should (eq fsm forwarded))
          (should (equal '(context reminders provider) (nreverse events)))
          (should (eq 'provider-dispatch (car captured)))
          (should (equal "request-1"
                         (plist-get (cdr captured) :request-id))))
      (kill-buffer buffer))))


(niwa-deftest niwa--compact-provider-wait/prepared-diffs
  (:doc "a cancelled request ends a dispatch still waiting on its diffs")
  ;; Nothing else moves a machine that has not sent its request out of WAIT:
  ;; gptel's abort only finds requests with a live process.
  (let* ((session (niwa-session--create :name "provider-prepare"))
         (buffer (generate-new-buffer " *compact-provider-prepare*"))
         (request (niwa-request--create :id "request-2"))
         (fsm (gptel-make-fsm
               :info (list :buffer buffer :niwa-request request)))
         stopped transitions forwarded)
    (unwind-protect
        (progn
          (with-current-buffer buffer (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-telemetry-record) #'ignore)
                    ((symbol-function 'niwa-reminders-prepare-edited-files)
                     (lambda (_fsm _continuation)
                       (lambda () (setq stopped t))))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (_) (setq forwarded t)))
                    ((symbol-function 'gptel--fsm-transition)
                     (lambda (_fsm state) (push state transitions))))
            (niwa--compact-provider-wait fsm)
            (should-not forwarded)
            (should-not transitions)
            (niwa-request-cancel request)
            (should stopped)
            (should (equal '(ABRT) transitions))
            (should-not forwarded)))
      (kill-buffer buffer))))

(niwa-deftest niwa--compact-transform-auto ()
  ,test
  (test)
  :doc "successful auto-compaction preserves later prompt-buffer transforms"
  (let ((source-buf (generate-new-buffer " *niwa-compact-source*"))
        (prompt-buf (generate-new-buffer " *niwa-compact-prompt*"))
        (continued nil)
        fsm)
    (unwind-protect
        (progn
          (with-current-buffer source-buf
            (org-mode)
            (setq-local niwa-compact-run-in-flight nil)
            (setq-local niwa--view-buffer nil)
            (insert "Old prompt\n")
            (insert (propertize "Old response\n" 'gptel 'response))
            (insert "Pending prompt\n"))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert-buffer-substring source-buf))
          (setq fsm (gptel-make-fsm
                     :info (list :buffer source-buf)))
          (cl-letf (((symbol-function 'niwa--compact-should-compact-p)
                       (lambda (&optional _token-estimate)
                         '(:summary-policy nil :target-pressure nil)))
                      ((symbol-function 'niwa-compact-run-start)
                       (lambda (&rest args)
                         (with-current-buffer prompt-buf
                           (goto-char (point-min))
                           (insert "Late prefix\n")
                           (when-let* ((start (niwa-compact-evidence-find-boundary)))
                             (goto-char start)
                             (insert "Late context\n\n"))
                           (goto-char (point-max))
                           (insert "Concurrent gptel context\n")
                           (setq gptel-system-prompt
                                 (concat "Concurrent gptel context\n\n"
                                         gptel-system-prompt)))
                         (with-current-buffer source-buf
                           (setq-local niwa-compact-target-current-request-reminder
                                       "Re-read /tmp/old.el")
                           (setq-local
                            niwa-compact-target-current-request-hook-context
                            "<hook-context>\n<hook-event name=\"SessionStart\">\ncompact context\n</hook-event>\n</hook-context>")
                           (let ((inhibit-read-only t))
                             (erase-buffer)
                             (insert "#+begin_summary\nSummary\n#+end_summary\n")
                             (insert "Tail prompt\n")
                             (insert (propertize "Tail response\n"
                                                 'gptel 'response))
                             (insert "Pending prompt\n")))
                         (funcall (plist-get args :callback) nil))))
              (with-current-buffer prompt-buf
                (let ((gptel-backend nil)
                      (gptel-model nil)
                      (gptel-reasoning-effort 'high)
                      (gptel-system-prompt "Effective system")
                      (gptel-max-tokens nil)
                      (gptel--request-params nil))
                  (niwa--compact-transform-auto
                   (lambda () (setq continued t))
                   fsm))))
          (with-current-buffer prompt-buf
            (let* ((text (buffer-string))
                   (snapshot
                    (plist-get (gptel-fsm-info fsm)
                               :niwa-model-context))
                   (context-start
                    (string-match "Concurrent gptel context\n" text)))
              (should continued)
              (should (string-match-p "Summary" text))
              (should (string-match-p "Late prefix" text))
              (should (string-match-p "Late context" text))
              (let ((first (string-match "compact context" text)))
                (should first)
                (should-not (string-match "compact context" text (1+ first))))
              ;; The file-reference reminder is staged for the WAIT
              ;; injector rather than written into the prompt text.
              (should (equal '((:type compact-file-references
                                :body "Re-read /tmp/old.el"))
                             (plist-get (gptel-fsm-info fsm)
                                        :niwa-reminder-entries)))
              (should-not (string-match-p "<system-reminder>" text))
              (should-not (string-match-p "Old prompt" text))
              (should (string-match-p "Pending prompt" text))
              (should context-start)
              (should-not (string-match-p "Concurrent gptel context"
                                          snapshot))
              (should (string-match-p "Late context" snapshot))
              (should (string-match-p "compact context" snapshot))
              (let ((locals
                     (plist-get (gptel-fsm-info fsm)
                                :niwa-request-locals)))
                (should (equal "Effective system"
                               (alist-get 'gptel-system-prompt locals)))
                (should (eq 'high
                            (alist-get 'gptel-reasoning-effort locals))))))
          (with-current-buffer source-buf
            (should-not niwa-compact-target-current-request-reminder)
            (should-not niwa-compact-target-current-request-hook-context)))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf))))

  :doc "blocks target-pressure prompt without a transcript boundary"
  (let ((source-buf (generate-new-buffer " *niwa-compact-source*"))
        (prompt-buf (generate-new-buffer " *niwa-compact-prompt*"))
        (continued 0) failure)
    (unwind-protect
        (progn
          (with-current-buffer source-buf
            (org-mode)
            (insert "No response boundary\n"))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert-buffer-substring source-buf))
          (let ((fsm (gptel-make-fsm :info (list :buffer source-buf))))
            (cl-letf (((symbol-function 'niwa--compact-should-compact-p)
                       (lambda (&optional _tokens)
                         '(:summary-policy nil :target-pressure t)))
                      ((symbol-function 'niwa--compact-auto-failure)
                       (lambda (_fsm _buffer err) (setq failure err))))
              (with-current-buffer prompt-buf
                (niwa--compact-transform-auto
                 (lambda () (cl-incf continued)) fsm))))
          (should (= continued 0))
          (should (string-match-p "No compactable history" failure)))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf))))

  :doc "auto threshold uses source baselines and transformed prompt growth"
  (dolist (baseline '(t nil))
    (let ((source-buf (generate-new-buffer " *niwa-compact-source*"))
          (prompt-buf (generate-new-buffer " *niwa-compact-prompt*"))
          (ran nil)
          (continued nil))
      (unwind-protect
          (progn
            (with-current-buffer source-buf
              (org-mode)
              (setq-local niwa-compact-run-in-flight nil)
              (insert "Old prompt\n")
              (insert (propertize "Old response\n" 'gptel 'response))
              (insert "Small pending\n")
              (when baseline
                (setq-local niwa-compact-estimation--known-token-baseline
                            (list :tokens 100
                                  :position (copy-marker (point-max))))))
            (with-current-buffer prompt-buf
              (org-mode)
              (if baseline
                  (insert "tiny\n")
                (insert-buffer-substring source-buf)
                (insert (make-string 400 ?x))))
            (let ((fsm (gptel-make-fsm
                        :info (list :buffer source-buf))))
              (cl-letf (((symbol-function 'niwa--compact-auto-eligible-p)
                         (lambda () t))
                        ((symbol-function 'niwa-compact-run-start)
                         (lambda (&rest args)
                           (setq ran t)
                           (funcall (plist-get args :callback) :skip))))
                (with-current-buffer prompt-buf
                  (let ((niwa-compact-estimation-token-threshold 0.5)
                        (niwa-model-context-limit 200)
                        (gptel-backend nil)
                        (gptel-model nil)
                        (gptel-max-tokens nil)
                        (gptel--request-params nil))
                    (niwa--compact-transform-auto
                     (lambda () (setq continued t))
                     fsm)))))
            (should ran)
            (should continued))
        (when (buffer-live-p source-buf)
          (kill-buffer source-buf))
        (when (buffer-live-p prompt-buf)
          (kill-buffer prompt-buf)))))

  :doc "summarizer pressure triggers before target-model pressure"
  (let ((source-buf (generate-new-buffer " *niwa-compact-source*"))
        (prompt-buf (generate-new-buffer " *niwa-compact-prompt*"))
        captured-args captured-policy)
    (unwind-protect
        (progn
          (put 'niwa-target-model :context-window 2)
          (put 'niwa-summary-model :context-window 0.2)
          (with-current-buffer source-buf
            (org-mode)
            (setq-local niwa-compact-run-in-flight nil)
            (insert "Old prompt\n")
            (insert (propertize "Old response\n" 'gptel 'response))
            (insert "Pending\n")
            (setq-local niwa-compact-estimation--known-token-baseline
                        (list :tokens 200
                              :position (copy-marker (point-max)))))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert-buffer-substring source-buf))
          (let ((fsm (gptel-make-fsm :info (list :buffer source-buf))))
            (cl-letf (((symbol-function 'niwa--compact-auto-eligible-p)
                       (lambda () t))
                      ((symbol-function 'niwa-model-resolve-workload)
                       (lambda (&rest _)
                         '(:backend nil :model niwa-summary-model)))
                      ((symbol-function 'niwa-compact-run-start)
                       (lambda (&rest args)
                         (setq captured-args args
                               captured-policy
                               (plist-get
                                (gptel-fsm-info fsm)
                                :niwa-compaction-target-policy)))))
              (with-current-buffer prompt-buf
                (let ((niwa-compact-estimation-token-threshold 0.8)
                      (niwa-model-reserve-tokens 0)
                      (gptel-backend nil)
                      (gptel-model 'niwa-target-model)
                      (gptel-max-tokens 150)
                      (gptel--request-params '(:temperature 0.5)))
                  (niwa--compact-transform-auto #'ignore fsm)))))
          (should captured-args)
          (should
           (equal captured-policy
                  '(:backend nil :model niwa-target-model
                    :effort nil
                    :max-tokens 150
                    :request-params (:temperature 0.5))))
          (should-not (plist-get (plist-get captured-args :admission)
                                 :target-pressure))
          (should (eq (plist-get
                       (plist-get (plist-get captured-args :admission)
                                  :summary-policy)
                       :model)
                      'niwa-summary-model)))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf)))))

(niwa-deftest niwa--compact-rebuild-info-data-from-buffer ()
  ,test
  (test)
  :doc "stages the current-request reminder before realizing continuation data"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-rebuild*"))
        captured)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (org-mode)
            (setq-local niwa--session
                        (niwa-session--create
                         :name "plan" :plan-mode t))
            (setq-local niwa-compact-target-current-request-reminder
                        "Re-read /tmp/old.el")
            (insert "Prompt\n")
            (let ((response-start (point)))
              (insert "Response\n")
              (put-text-property response-start (point) 'gptel 'response))
            (insert "Tool result\n"))
          (let ((fsm (gptel-make-fsm
                      :info (list :buffer chat-buf
                                  :data 'old-data))))
            (cl-letf (((symbol-function 'gptel--create-prompt-buffer)
                       (lambda (&optional _prompt-end)
                         (let ((buf (generate-new-buffer
                                     " *niwa-compact-realize*")))
                           (with-current-buffer buf
                             (org-mode)
                             (insert-buffer-substring chat-buf))
                           buf)))
                      ((symbol-function 'gptel--realize-query)
                       (lambda (realize-fsm)
                         (let ((prompt-buffer
                                (plist-get (gptel-fsm-info realize-fsm)
                                           :data)))
                           (with-current-buffer prompt-buffer
                             (setq captured (buffer-string)))
                           (plist-put (gptel-fsm-info realize-fsm)
                                      :data 'realized)))))
              (niwa--compact-rebuild-info-data-from-buffer fsm chat-buf)
              (should (eq (plist-get (gptel-fsm-info fsm) :data) 'realized))
              ;; Staged for the WAIT injector, never inlined into the
              ;; rebuilt prompt text.
              (let ((entries (plist-get (gptel-fsm-info fsm)
                                        :niwa-reminder-entries)))
                (should (equal '(:type compact-file-references
                                 :body "Re-read /tmp/old.el")
                               (car entries)))
                (should (eq 'plan-mode (plist-get (cadr entries) :type)))
                (should (string-match-p
                         "<proposed_plan>\n"
                         (plist-get (cadr entries) :body))))
              (should-not (string-match-p "<system-reminder>" captured))
              (with-current-buffer chat-buf
                (should-not niwa-compact-target-current-request-reminder)))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(niwa-deftest niwa--compact-main-wait-decision ()
  ,test
  (test)
  :doc "evaluates continuation admission with the realized request policy"
  (let* ((chat-buf (generate-new-buffer " *niwa-compact-decision*"))
         (policy '(:backend chosen-backend :model chosen-model
                   :effort high :max-tokens 321
                   :request-params (:temperature 0)))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :history '(TRET)
                           :data 'realized
                           :niwa-compaction-target-policy policy)))
         captured)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-compact-estimation-estimate-data-tokens)
                   (lambda (data)
                     (should (eq data 'realized))
                     42))
                  ((symbol-function 'niwa--compact-should-compact-p)
                   (lambda (estimate)
                     (setq captured
                           (list estimate gptel-backend gptel-model
                                 gptel-reasoning-effort gptel-max-tokens
                                 gptel--request-params))
                     'admitted)))
          (should
           (equal (list :admission 'admitted :target-policy policy)
                  (niwa--compact-main-wait-decision fsm)))
          (should
           (equal '(42 chosen-backend chosen-model high 321
                       (:temperature 0))
                  captured)))
      (kill-buffer chat-buf))))

(niwa-deftest niwa--compact-defer-steering-p ()
  ,test
  (test)
  :doc "caches one admission decision for the final WAIT gate"
  (let* ((decision '(:admission (:target-pressure nil)))
         (fsm (gptel-make-fsm :info nil)))
    (cl-letf (((symbol-function 'niwa--compact-main-wait-decision)
               (lambda (_fsm) decision)))
      (should (equal '(:target-pressure nil)
                     (niwa--compact-defer-steering-p fsm)))
      (should (eq decision
                  (plist-get (gptel-fsm-info fsm)
                             :niwa-compaction-wait-decision))))))

(niwa-deftest niwa--compact-target-provider-wait ()
  ,test
  (test)
  :doc "root compaction reinjects steering before provider dispatch"
  (let ((fsm (gptel-make-fsm :info nil))
        events)
    (cl-letf (((symbol-function 'niwa-tools--handle-steering-inject)
               (lambda (_fsm skip-compaction-gate)
                 (push (list 'steering skip-compaction-gate) events)))
              ((symbol-function 'niwa--compact-provider-wait)
               (lambda (_fsm) (push 'provider events))))
      (niwa--compact-target-provider-wait '(:invocation nil) fsm))
    (should (equal '((steering t) provider) (nreverse events))))
  :doc "a new interaction hold prevents post-compact provider dispatch"
  (let ((fsm (gptel-make-fsm :info nil))
        sent)
    (cl-letf (((symbol-function 'niwa-tools--handle-steering-inject)
              (lambda (actual-fsm _skip-compaction-gate)
                 (setf (gptel-fsm-info actual-fsm)
                       (plist-put (gptel-fsm-info actual-fsm)
                                  :niwa-pending-input-hold t))))
              ((symbol-function 'niwa--compact-provider-wait)
               (lambda (_fsm) (setq sent t))))
      (niwa--compact-target-provider-wait '(:invocation nil) fsm))
    (should-not sent))
  :doc "agent compaction dispatches without root steering"
  (let ((fsm (gptel-make-fsm :info nil))
        events)
    (cl-letf (((symbol-function 'niwa-tools--handle-steering-inject)
               (lambda (&rest _) (error "Root steering called")))
              ((symbol-function 'niwa--compact-provider-wait)
               (lambda (_fsm) (push 'provider events))))
      (niwa--compact-target-provider-wait '(:invocation agent) fsm))
    (should (equal '(provider) (nreverse events)))))

(niwa-deftest niwa--compact-handle-wait ()
  ,test
  (test)
  :doc "does not run continuation gate on initial INIT to WAIT"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-wait*"))
        (sent 0)
        (checked nil))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(INIT)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content
                                                   (make-string 400 ?x))))))))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'niwa--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       (setq checked t)
                       t)))
            (niwa--compact-handle-wait fsm))
          (should (= sent 1))
          (should-not checked))
      (kill-buffer chat-buf)))

  :doc "held pending input keeps WAIT open without provider dispatch"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-held-wait*"))
        (sent 0))
    (unwind-protect
        (let ((fsm
               (gptel-make-fsm
                :info (list :buffer chat-buf
                            :history '(TRET)
                            :niwa-pending-input-hold t))))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent))))
            (niwa--compact-handle-wait fsm))
          (should (= sent 0)))
      (kill-buffer chat-buf)))

  :doc "consumes the admission decision cached before steering injection"
  (with-temp-buffer
    (let* ((decision '(:admission admitted :target-policy nil))
           (fsm
            (gptel-make-fsm
             :info (list :buffer (current-buffer)
                         :niwa-compaction-wait-decision decision)))
           captured)
      (cl-letf (((symbol-function 'niwa--compact-main-wait-decision)
                 (lambda (_fsm) (error "Recomputed admission")))
                ((symbol-function 'niwa-compact-target-main-target)
                 (lambda () 'target))
                ((symbol-function 'niwa--compact-handle-target-wait)
                 (lambda (actual-fsm target admission
                          &optional _pending-start)
                   (setq captured (list actual-fsm target admission)))))
        (niwa--compact-handle-wait fsm))
      (should (equal (list fsm 'target 'admitted) captured))
      (should-not (plist-member (gptel-fsm-info fsm)
                                :niwa-compaction-wait-decision))))

  :doc "sends continuation directly when realized data is below threshold"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-wait*"))
        (sent 0)
        (ran nil))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content "small")))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'niwa--compact-auto-eligible-p)
                     (lambda () t))
                    ((symbol-function 'niwa-compact-run-start)
                     (lambda (&rest _args) (setq ran t))))
            (let ((niwa-compact-estimation-token-threshold 0.5)
                  (gptel-model nil)
                  (niwa-model-context-limit 200))
              (niwa--compact-handle-wait fsm)))
          (should (= sent 1))
          (should-not ran))
      (kill-buffer chat-buf)))

  :doc "preserves the realized reasoning effort during continuation admission"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-wait*"))
        captured-effort
        sent-effort)
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list
                           :buffer chat-buf
                           :history '(TRET)
                           :niwa-compaction-target-policy
                           '(:backend target-backend :model target-model
                             :effort max :max-tokens nil
                             :request-params nil)
                           :data '(:messages [])))))
          (with-current-buffer chat-buf
            (setq-local gptel-reasoning-effort 'medium))
          (cl-letf (((symbol-function 'niwa--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       (setq captured-effort gptel-reasoning-effort)
                       nil))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm)
                       (setq sent-effort gptel-reasoning-effort))))
            (niwa--compact-handle-wait fsm))
          (should (eq captured-effort 'max))
          (should (eq sent-effort 'max)))
      (kill-buffer chat-buf)))

  :doc "does not compact continuation when image payload is below media-aware threshold"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-wait*"))
        (sent 0)
        (ran nil))
    (unwind-protect
        (let* ((prefix "data:image/png;base64,")
               (url (concat prefix (make-string 400 ?A)))
               (fsm (gptel-make-fsm
                     :info (list :buffer chat-buf
                                 :history '(TRET)
                                 :data (list :messages
                                             (vector
                                              (list :role "user"
                                                    :content
                                                    (vector
                                                     (list :type "input_image"
                                                           :image_url
                                                           url)))))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'niwa-compact-run-start)
                     (lambda (&rest _args) (setq ran t))))
            (let ((niwa-compact-estimation-token-threshold 0.5)
                  (niwa-model-context-limit 200)
                  (niwa-compact-estimation-image-token-estimate 10))
              (niwa--compact-handle-wait fsm)))
          (should (= sent 1))
          (should-not ran))
      (kill-buffer chat-buf)))

  :doc "uses the realized request reserve instead of chat-buffer settings"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-wait*"))
        (sent 0)
        (ran nil))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info
                    (list
                     :buffer chat-buf
                     :history '(TRET)
                     :niwa-compaction-target-policy
                     '(:backend nil :model nil :max-tokens 100
                       :request-params nil)
                     :data
                     (list :messages
                           (vector
                            (list :role "user"
                                  :content (make-string 240 ?x))))))))
          (with-current-buffer chat-buf
            (setq-local gptel-max-tokens nil)
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'niwa--compact-auto-eligible-p)
                     (lambda () t))
                    ((symbol-function 'niwa-compact-run-start)
                     (lambda (&rest args)
                       (setq ran t)
                       (funcall (plist-get args :callback) :skip))))
            (let ((niwa-compact-estimation-token-threshold 0.5)
                  (niwa-model-context-limit 200)
                  (niwa-model-reserve-tokens 0))
              (niwa--compact-handle-wait fsm)))
          (should ran)
          (should (= sent 1)))
      (kill-buffer chat-buf)))

  :doc "runs continuation gate on TRET to WAIT over threshold"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-wait*"))
        (sent 0)
        (ran nil))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content
                                                   (make-string 400 ?x))))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'niwa--compact-auto-eligible-p)
                     (lambda () t))
                    ((symbol-function 'niwa-compact-run-start)
                     (lambda (&rest args)
                       (setq ran t)
                       (funcall (plist-get args :callback) :skip))))
            (let ((niwa-compact-estimation-token-threshold 0.5)
                  (gptel-model nil)
                  (niwa-model-context-limit 200))
              (niwa--compact-handle-wait fsm)))
          (should ran)
          (should (= sent 1)))
      (kill-buffer chat-buf)))

  :doc "suppresses original wait handler until compaction succeeds"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-wait*"))
        (sent 0)
        (rebuilt nil)
        callback)
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content
                                                   (make-string 400 ?x))))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'niwa--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       '(:summary-policy nil :target-pressure nil)))
                    ((symbol-function 'niwa-compact-run-start)
                     (lambda (&rest args)
                       (setq callback (plist-get args :callback))))
                    ((symbol-function
                      'niwa--compact-rebuild-info-data-from-buffer)
                     (lambda (_fsm _chat-buffer)
                       (setq rebuilt t))))
            (niwa--compact-handle-wait fsm)
            (should (= sent 0))
            (should callback)
            (funcall callback nil))
          (should rebuilt)
          (should (= sent 1)))
      (kill-buffer chat-buf)))

  :doc "does not send continuation when compaction fails"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-wait*"))
        (sent 0)
        warning)
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content
                                                   (make-string 400 ?x))))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'niwa--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       '(:summary-policy nil :target-pressure nil)))
                    ((symbol-function 'niwa-compact-run-start)
                     (lambda (&rest args)
                       (funcall (plist-get args :callback) "boom")))
                    ((symbol-function 'display-warning)
                     (lambda (_type message &optional _level _buffer)
                       (setq warning message)))
                    ((symbol-function 'gptel--update-status)
                     #'ignore))
            (niwa--compact-handle-wait fsm))
          (should (= sent 0))
          (should (string-match-p
                   "boom (auto-compaction failed; request not sent)"
                   warning)))
      (kill-buffer chat-buf)))

  :doc "blocks target-pressure continuation without a transcript boundary"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-wait*"))
        (sent 0) failure)
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data '(:messages [])))))
          (with-current-buffer chat-buf
            (insert "No response boundary\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'niwa--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       '(:summary-policy nil :target-pressure t)))
                    ((symbol-function 'niwa--compact-auto-failure)
                     (lambda (_fsm _buffer err) (setq failure err))))
            (niwa--compact-handle-wait fsm))
          (should (= sent 0))
          (should (string-match-p "No compactable history" failure)))
      (kill-buffer chat-buf)))

  :doc "allows summarizer-only continuation without a transcript boundary"
  (let ((chat-buf (generate-new-buffer " *niwa-compact-wait*"))
        (sent 0))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data '(:messages [])))))
          (with-current-buffer chat-buf
            (insert "No response boundary\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'niwa--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       '(:summary-policy nil :target-pressure nil))))
            (niwa--compact-handle-wait fsm))
          (should (= sent 1)))
      (kill-buffer chat-buf))))


(niwa-deftest niwa--compact-main-resume-status ()
  ,test
  (test)
  :doc "restores main request progress after continuation compaction"
  (let ((chat-buffer (generate-new-buffer " *niwa-compact-chat*"))
        (view-buffer (generate-new-buffer " *niwa-compact-view*"))
        spinner)
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local niwa--view-buffer view-buffer))
          (cl-letf (((symbol-function 'niwa-view-stream-show-progress)
                     (lambda (view text &optional owner)
                       (setq spinner (list view text owner)))))
            (niwa--compact-main-resume-status
             (list :buffer chat-buffer)))
          (should (equal (list view-buffer "Thinking..." nil) spinner)))
      (kill-buffer chat-buffer)
      (kill-buffer view-buffer))))

(niwa-deftest niwa--compact-target-resume ()
  ,test
  (test)
  :doc "rebuilds a compacted target and resumes its FSM once"
  (let* ((buffer (generate-new-buffer " *niwa-compact-resume*"))
         (marker (with-current-buffer buffer (copy-marker (point-min))))
         (fsm (gptel-make-fsm :info (list :position marker)))
         rebuilt status (waits 0))
    (unwind-protect
        (progn
          (with-current-buffer buffer (insert "compacted"))
          (cl-letf (((symbol-function
                      'niwa--compact-rebuild-info-data-from-buffer)
                     (lambda (actual-fsm actual-buffer)
                       (setq rebuilt (list actual-fsm actual-buffer))))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf waits))))
            (niwa--compact-target-resume
             (list :buffer buffer
                   :resume-status
                   (lambda (_target) (setq status t)))
             fsm))
          (should (equal (list fsm buffer) rebuilt))
          (should status)
          (should (= 1 waits))
          (should (= (with-current-buffer buffer (point-max))
                     (marker-position marker))))
      (set-marker marker nil)
      (kill-buffer buffer)))
  :doc "continued output stays after the summary when rotation collapses stream markers"
  (dolist (callback '(gptel-curl--stream-insert-response gptel--insert-response))
    (niwa-view-test--with-buffers
      (let ((draft "> quoted\nsecond line")
            (niwa-gptel-stream-bridge-insert-batch-delay nil))
        (with-current-buffer view-buf
          (niwa-view-test--insert-composer-draft draft 4))
        (with-current-buffer data-buf
          (insert "Old conversation\n")
          (let* ((info (list :buffer data-buf
                             :position (copy-marker (point-max))
                             :tracking-marker (copy-marker (point-max))
                             :reasoning-marker (copy-marker (point-max))
                             :include-reasoning 'ignore))
                 (fsm (gptel-make-fsm :info info)))
            ;; Segment replacement collapses all the old insertion markers.
            (erase-buffer)
            (insert (niwa-session-artifacts-summary-block "Private handoff"))
            (cl-letf (((symbol-function 'niwa--compact-rebuild-info-data-from-buffer)
                       #'ignore)
                      ((symbol-function 'gptel--handle-wait)
                       (lambda (_fsm)
                         (funcall callback '(reasoning . "New reasoning") info)
                         (when (eq callback 'gptel-curl--stream-insert-response)
                           (funcall callback '(reasoning . t) info))
                         (funcall callback "Continued response" info))))
              (niwa--compact-target-resume (list :buffer data-buf) fsm))
            (should (niwa-session-artifacts-segment-summary-bounds))
            (should (< (string-search "Private handoff" (buffer-string))
                       (string-search "Continued response" (buffer-string))))))
        (with-current-buffer view-buf
          (niwa-view--full-rerender)
          (should (string-match-p "conversation compacted" (buffer-string)))
          (should-not (string-match-p "Private handoff" (buffer-string)))
          (should (string-match-p "Continued response" (buffer-string)))
          (should (equal draft (niwa-view-composer-draft-text)))
          (should (= 4 (niwa-view-test--composer-point-offset)))
          ;; Expanding shows what the model received: preface and summary.
          (save-excursion
            (goto-char (point-min))
            (search-forward "conversation compacted")
            (niwa-view-toggle-section))
          (should (string-match-p "Private handoff" (buffer-string)))
          (should (string-match-p "Another language model started"
                                  (buffer-string)))
          (should-not (string-match-p "begin_summary\\|end_summary"
                                      (buffer-string)))
          (should (equal draft (niwa-view-composer-draft-text))))))))

(niwa-deftest niwa--compact-agent-terminal-failure ()
  ,test
  (test)
  :doc "clears activity, records a structured error, and enters ERRS"
  (let* ((invocation
          (niwa-agent-invocation-create
           (niwa-agent--create :name "explorer")))
         (fsm
          (gptel-make-fsm
           :info (list :buffer nil
                       :niwa-agent-invocation invocation)))
         activity status transition)
    (cl-letf (((symbol-function 'gptel--fsm-transition)
               (lambda (_fsm state) (setq transition state)))
              ((symbol-function 'niwa-agent-conversation-record-activity)
               (lambda (_invocation item) (setq activity item)))
              ((symbol-function 'gptel--update-status)
               (lambda (text &optional _face) (setq status text))))
      (niwa--compact-agent-terminal-failure nil fsm "boom"))
    (should (equal '(:type status :summary "error") activity))
    (should (equal " Agent failed" status))
    (should (eq 'ERRS transition))
    (should (equal "Compaction failed: boom"
                   (plist-get (gptel-fsm-info fsm) :status)))
    (should (equal '(:type "compaction_error" :message "boom")
                   (plist-get (gptel-fsm-info fsm) :error)))))

(niwa-deftest niwa--compact-terminate-request ()
  ,test
  (test)
  :doc "records structured failure and enters gptel's terminal error state"
  (let ((fsm (gptel-make-fsm :info (list :buffer nil)))
        status transition)
    (cl-letf (((symbol-function 'gptel--fsm-transition)
               (lambda (_fsm state) (setq transition state)))
              ((symbol-function 'gptel--update-status)
               (lambda (text &optional _face) (setq status text))))
      (niwa--compact-terminate-request fsm " Failed" 'boom))
    (should (equal " Failed" status))
    (should (eq 'ERRS transition))
    (should (equal "Compaction failed: boom"
                   (plist-get (gptel-fsm-info fsm) :status)))
    (should (equal '(:type "compaction_error" :message "boom")
                   (plist-get (gptel-fsm-info fsm) :error))))

  :doc "settles a pre-send refusal through gptel and releases its prompt buffer"
  (with-temp-buffer
    (let* ((gptel-log-level nil)
           (prompt (generate-new-buffer " *compact-refused-prompt*"))
           (fsm (gptel-make-fsm
                 :state 'INIT
                 :handlers (list (list 'ERRS #'gptel--fsm-last))
                 :info (list :buffer (current-buffer) :data prompt))))
      (unwind-protect
          (progn
            (niwa--compact-terminate-request fsm " Failed" "No history")
            (should (eq (gptel-fsm-state fsm) 'ERRS))
            (should (eq gptel--fsm-last fsm))
            (should (plist-get (gptel-fsm-info fsm) :end-time))
            (should-not (buffer-live-p prompt))
            (should-not (plist-get (gptel-fsm-info fsm) :data)))
        (when (buffer-live-p prompt)
          (kill-buffer prompt))))))

(niwa-deftest niwa--compact-main-failure ()
  ,test
  (test)
  :doc "routes target failure through main automatic failure reporting"
  (let ((buffer (generate-new-buffer " *niwa-compact-failure*"))
        (fsm (gptel-make-fsm :info (list :buffer nil)))
        captured paused)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa--compact-auto-failure)
                   (lambda (actual-fsm actual-buffer err)
                     (setq captured (list actual-fsm actual-buffer err))))
                  ((symbol-function 'niwa-goal-pause-runtime-failure)
                   (lambda (actual-buffer reason)
                     (setq paused (list actual-buffer reason)))))
          (niwa--compact-main-failure
           (list :buffer buffer) fsm "boom")
          (should (equal (list fsm buffer "boom") captured))
          (should (equal (list buffer "Compaction failed: boom") paused)))
      (kill-buffer buffer))))

(niwa-deftest niwa--compact-auto-failure ()
  ,test
  (test)

  :doc "warns and moves the refused request to its terminal state"
  ;; A compaction that refuses to send owns the machine it refused for.
  ;; Left in WAIT the turn never settles, so the view keeps reporting a
  ;; running request and queued pending inputs wait on a request that will
  ;; never reach them.
  (let ((buffer (generate-new-buffer " *niwa-compact-auto-failure*"))
        warning status transition)
    (unwind-protect
        (let ((fsm (gptel-make-fsm :info (list :buffer buffer))))
          (cl-letf (((symbol-function 'gptel--fsm-transition)
                     (lambda (_fsm state) (setq transition state)))
                    ((symbol-function 'gptel--update-status)
                     (lambda (text &optional _face) (setq status text)))
                    ((symbol-function 'display-warning)
                     (lambda (_type text &rest _) (setq warning text))))
            (niwa--compact-auto-failure fsm buffer "boom"))
          (should (string-prefix-p "niwa [workspace unavailable]: " warning))
          (should (string-search "boom (auto-compaction failed; request not sent)" warning))
          (should (equal " Compaction failed" status))
          (should (eq 'ERRS transition))
          (should (equal "Compaction failed: boom"
                         (plist-get (gptel-fsm-info fsm) :status)))
          (should (equal '(:type "compaction_error" :message "boom")
                         (plist-get (gptel-fsm-info fsm) :error))))
      (kill-buffer buffer)))

  :doc "reports and settles nothing for a dead chat buffer"
  (let ((buffer (generate-new-buffer " *niwa-compact-auto-failure*"))
        (transition 'none))
    (kill-buffer buffer)
    (cl-letf (((symbol-function 'gptel--fsm-transition)
               (lambda (_fsm state) (setq transition state))))
      (niwa--compact-auto-failure
       (gptel-make-fsm :info (list :buffer nil)) buffer "boom"))
    (should (eq 'none transition))))

(niwa-deftest niwa--compact-handle-target-wait ()
  ,test
  (test)
  :doc "passes through when admission does not request compaction"
  (let ((fsm (gptel-make-fsm :info nil)) (waits 0))
    (cl-letf (((symbol-function 'gptel--handle-wait)
               (lambda (_fsm) (cl-incf waits))))
      (niwa--compact-handle-target-wait fsm nil nil))
    (should (= 1 waits)))

  :doc "settles no-boundary continuations according to target pressure"
  (dolist (pressure '(nil t))
    (let ((fsm (gptel-make-fsm :info nil)) (waits 0) failure)
      (cl-letf (((symbol-function 'niwa-compact-evidence-find-boundary)
                 (lambda () nil))
                ((symbol-function 'gptel--handle-wait)
                 (lambda (_fsm) (cl-incf waits))))
        (niwa--compact-handle-target-wait
         fsm
         (list :fail (lambda (_target _fsm err) (setq failure err)))
         (list :target-pressure pressure)))
      (if pressure
          (progn
            (should (= 0 waits))
            (should (string-match-p "No compactable history" failure)))
        (should (= 1 waits)))))

  :doc "resumes success and routes run or rebuild failures once"
  (dolist (outcome '(success run-error rebuild-error))
    (let ((fsm (gptel-make-fsm :info nil)) resumed failure)
      (cl-letf (((symbol-function 'niwa-compact-evidence-find-boundary)
                 (lambda () 10))
                ((symbol-function 'niwa-compact-run-start)
                 (lambda (&rest args)
                   (funcall (plist-get args :callback)
                            (and (eq outcome 'run-error) "run failed")))))
        (niwa--compact-handle-target-wait
         fsm
         (list :resume
               (lambda (_target _fsm)
                 (if (eq outcome 'rebuild-error)
                     (error "rebuild failed")
                   (setq resumed t)))
               :fail
               (lambda (_target _fsm err) (setq failure err)))
         '(:target-pressure t)))
      (pcase outcome
        ('success (should resumed) (should-not failure))
        ('run-error (should (equal "run failed" failure)))
        ('rebuild-error (should (equal "rebuild failed" failure)))))))

(niwa-deftest niwa--compact-handle-agent-wait ()
  ,test
  (test)
  :doc "archives, compacts, rebuilds, and resumes one persisted continuation"
  (test-niwa-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (let ((task (test-niwa-compact--insert-agent-task
                 invocation "inspect" "Keep this exact task."))
          pre-event post-event captured-system captured-body
          resumed-data statuses messages)
      (setf (niwa-session-goal session) "PARENT-GOAL-ONLY")
      (setf (niwa-session-invoked-skills session)
            (list
             (niwa-skill-invocation-record--create
              :name "parent-only-skill"
              :role 'command
              :origin 'user
              :turn 1)))
      (setf (niwa-session-touched-files session)
            '(("/tmp/PARENT-FILE-ONLY" . 1)))
      (dolist (turn '(("Old response one.\n" . "Old user two.\n")
                      ("Old response two.\n" . "Old user three.\n")
                      ("Old response three.\n" . "Recent user four.\n")))
        (let ((start (point)))
          (insert (car turn))
          (put-text-property start (point) 'gptel 'response))
        (insert (cdr turn)))
      (let ((start (point)))
        (insert "Recent response four.\n")
        (put-text-property start (point) 'gptel 'response))
      (insert "Pending tool result.\n")
      (should (niwa-agent-conversation-save invocation))
      (let* ((original (buffer-string))
             (data (list :messages
                         (vector
                          (list :role "user"
                                :content (make-string 1000 ?x)))))
             (fsm
              (gptel-make-fsm
               :info
               (list :buffer agent-buffer
                     :history '(TRET)
                     :data data
                     :backend nil
                     :model 'niwa-agent-target-model
                     :niwa-agent-invocation invocation
                     :niwa-compaction-target-policy
                     '(:backend nil :model niwa-agent-target-model
                       :max-tokens 0 :request-params nil)))))
        (put 'niwa-agent-target-model :context-window 0.4)
        (put 'niwa-agent-summary-model :context-window 2)
        (let ((niwa-compact-estimation-token-threshold 0.5)
              (niwa-model-reserve-tokens 0)
              (niwa-compact-evidence-tail-turns 1)
              (niwa-compact-evidence-tail-budget 1.0)
              (niwa-compact-run-warn-on-completion t)
              (gptel-backend nil)
              (gptel-model 'niwa-agent-target-model)
              (gptel-max-tokens 0)
              (gptel--request-params nil)
              (gptel-stream nil)
              (niwa-pre-compact-functions
               (list
                (lambda (event)
                  (setq pre-event event)
                  '(:additional-context ("agent hook context")))))
              (niwa-post-compact-functions
               (list (lambda (event) (setq post-event event)))))
          (cl-letf (((symbol-function 'niwa-model-resolve-workload)
                     (lambda (&rest _)
                       '(:backend nil :model niwa-agent-summary-model)))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _)
                       '(:description "test" :max-tokens nil
                         :request-params nil)))
                    ((symbol-function 'gptel-request)
                     (lambda (body &rest args)
                       (setq captured-body body
                             captured-system (plist-get args :system))
                       (funcall (plist-get args :callback)
                                (replace-regexp-in-string
                                 "- test"
                                 "- Continue the agent task."
                                 test-niwa-compact--valid-summary
                                 nil t)
                                nil)))
                    ((symbol-function
                      'niwa--compact-rebuild-info-data-from-buffer)
                     (lambda (rebuild-fsm buffer)
                       (plist-put
                        (gptel-fsm-info rebuild-fsm) :data
                        (with-current-buffer buffer (buffer-string)))))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (wait-fsm)
                       (push (plist-get (gptel-fsm-info wait-fsm) :data)
                             resumed-data)))
                    ((symbol-function 'gptel--update-status)
                     (lambda (status &optional _face) (push status statuses)))
                    ((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (when format-string
                         (let ((rendered
                                (apply #'format format-string args)))
                           (push rendered messages)
                           rendered)))))
            (niwa--compact-handle-agent-wait fsm)))
        (let* ((canonical (buffer-string))
               (archives
                (directory-files
                 (file-name-directory canonical-path) t
                 "\\.compact-[0-9]+\\.chat\\.org\\'")))
          (should (= 1 (length archives)))
          (should (equal original
                         (with-temp-buffer
                           (insert-file-contents (car archives))
                           (buffer-string))))
          (should (niwa--same-file-p canonical-path buffer-file-name))
          (should (string-prefix-p task canonical))
          (should (string-match-p "#\\+begin_summary" canonical))
          (should (string-match-p "Continue the agent task" canonical))
          (should (string-match-p "Recent user four" canonical))
          (should (string-match-p "Pending tool result" canonical))
          (should-not (string-match-p "Old user two" canonical))
          (should (string-match-p "niwa-hook-audit" canonical))
          (should-not (plist-get (gptel-fsm-info fsm) :error))
          (should (= 1 (length resumed-data)))
          (should (string-match-p "Continue the agent task"
                                  (car resumed-data)))
          (should (equal (plist-get pre-event :origin) "/root/explorer"))
          (should (equal (plist-get pre-event :transcript-path)
                         canonical-path))
          (should (equal (plist-get post-event :origin) "/root/explorer"))
          (should (string-match-p "agent hook context" captured-body))
          (should (string-match-p "Old user two" captured-body))
          (should-not (string-match-p "Recent user four" captured-body))
          (dolist (parent-only
                   '("PARENT-GOAL-ONLY" "parent-only-skill"
                     "PARENT-FILE-ONLY"))
            (should-not (string-match-p parent-only captured-system))
            (should-not (string-match-p parent-only captured-body))
            (should-not (string-match-p parent-only canonical)))
          (should (member " Compacting..." statuses))
          (should-not
           (cl-some (lambda (entry) (string-match-p "long threads" entry))
                    messages))
          (should-not niwa-compact-target-current-request-reminder)))))

  :doc "preserves reasoning and prose before the current persisted tool batch"
  (test-niwa-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-niwa-compact--insert-agent-task
     invocation "inspect" "Keep this exact task.")
    (let ((old-start (point)))
      (insert "old tool result\n")
      (put-text-property old-start (point) 'gptel '(tool . "call-old")))
    (let ((batch-start (point)))
      (insert "#+begin_reasoning\nthinking\n#+end_reasoning\n")
      (insert (propertize "assistant preface\n" 'gptel 'response))
      (let ((current-start (point)))
        (insert "current tool result\n")
        (put-text-property current-start (point)
                           'gptel '(tool . "call-current")))
      (should (niwa-agent-conversation-save invocation))
      (let* ((fsm
              (gptel-make-fsm
               :info
               (list :buffer agent-buffer
                     :history '(TRET)
                     :data '(:messages [])
                     :tool-use '((:id "call-current"))
                     :niwa-agent-invocation invocation
                     :niwa-compaction-target-policy '(:model test))))
             pending-start waits)
        (let ((niwa-compact-auto t)
              (niwa--compact-auto-disabled nil)
              (niwa-compact-run-in-flight nil))
          (cl-letf (((symbol-function 'niwa-compact-estimation-admission)
                     (lambda (&rest _) '(:target-pressure t)))
                    ((symbol-function 'niwa-compact-run-start)
                     (lambda (&rest args)
                       (setq pending-start
                             (plist-get args :pending-start))))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf waits))))
            (niwa--compact-handle-agent-wait fsm)))
        (should (= batch-start pending-start))
        (should-not waits))))

  :doc "updates the anchored summary and archives each persisted continuation"
  (test-niwa-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (let ((task (test-niwa-compact--insert-agent-task
                 invocation "inspect" "Keep this exact task."))
          (request-data
           (list :messages
                 (vector
                  (list :role "user" :content (make-string 1000 ?x)))))
          systems bodies (resumed 0)
          first-canonical before-second)
      (dolist (turn '(("Old response one.\n" . "Old user two.\n")
                      ("Old response two.\n" . "Old user three.\n")
                      ("Old response three.\n" . "Recent user four.\n")))
        (let ((start (point)))
          (insert (car turn))
          (put-text-property start (point) 'gptel 'response))
        (insert (cdr turn)))
      (let ((start (point)))
        (insert "Recent response four.\n")
        (put-text-property start (point) 'gptel 'response))
      (insert "Pending result four.\n")
      (should (niwa-agent-conversation-save invocation))
      (let ((original (buffer-string))
            (fsm
             (gptel-make-fsm
              :info
              (list :buffer agent-buffer
                    :history '(TRET)
                    :data request-data
                    :backend nil
                    :model 'niwa-agent-target-model
                    :niwa-agent-invocation invocation
                    :niwa-compaction-target-policy
                    '(:backend nil :model niwa-agent-target-model
                      :max-tokens 0 :request-params nil)))))
        (put 'niwa-agent-target-model :context-window 0.4)
        (put 'niwa-agent-summary-model :context-window 2)
        (let ((niwa-compact-estimation-token-threshold 0.5)
              (niwa-model-reserve-tokens 0)
              (niwa-compact-evidence-tail-turns 1)
              (niwa-compact-evidence-tail-budget 1.0)
              (gptel-backend nil)
              (gptel-model 'niwa-agent-target-model)
              (gptel-max-tokens 0)
              (gptel--request-params nil)
              (gptel-stream nil))
          (cl-letf (((symbol-function 'niwa-model-resolve-workload)
                     (lambda (&rest _)
                       '(:backend nil :model niwa-agent-summary-model)))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _)
                       '(:description "test" :max-tokens nil
                         :request-params nil)))
                    ((symbol-function 'gptel-request)
                     (lambda (body &rest args)
                       (push body bodies)
                       (push (plist-get args :system) systems)
                       (funcall
                        (plist-get args :callback)
                        (replace-regexp-in-string
                         "- test"
                         (if (= (length bodies) 1)
                             "- First retained fact."
                           "- Updated retained fact.")
                         test-niwa-compact--valid-summary nil t)
                        nil)))
                    ((symbol-function
                      'niwa--compact-rebuild-info-data-from-buffer)
                     (lambda (rebuild-fsm _buffer)
                       (plist-put (gptel-fsm-info rebuild-fsm)
                                  :data request-data)))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf resumed)))
                    ((symbol-function 'gptel--update-status) #'ignore)
                    ((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (niwa--compact-handle-agent-wait fsm)
            (setq first-canonical (buffer-string))
            (goto-char (point-max))
            (insert "New user five.\n")
            (let ((start (point)))
              (insert "New response five.\n")
              (put-text-property start (point) 'gptel 'response))
            (insert "New user six.\n")
            (let ((start (point)))
              (insert "New response six.\n")
              (put-text-property start (point) 'gptel 'response))
            (insert "Latest user seven.\n")
            (let ((start (point)))
              (insert "Latest response seven.\n")
              (put-text-property start (point) 'gptel 'response))
            (insert "Pending result seven.\n")
            (should (niwa-agent-conversation-save invocation))
            (setq before-second (buffer-string))
            (plist-put (gptel-fsm-info fsm) :data request-data)
            (niwa--compact-handle-agent-wait fsm)))
        (let* ((canonical (buffer-string))
               (archive-directory (file-name-directory canonical-path))
               (first-archive
                (expand-file-name
                 "explorer-test.compact-0001.chat.org"
                 archive-directory))
               (second-archive
                (expand-file-name
                 "explorer-test.compact-0002.chat.org"
                 archive-directory)))
          (should (= 2 resumed))
          (should (= 2 (length bodies)))
          (should (file-exists-p first-archive))
          (should (file-exists-p second-archive))
          (should
           (equal original
                  (with-temp-buffer
                    (insert-file-contents first-archive)
                    (buffer-string))))
          (should
           (equal before-second
                  (with-temp-buffer
                    (insert-file-contents second-archive)
                    (buffer-string))))
          (should (string-prefix-p task first-canonical))
          (should (string-prefix-p task canonical))
          (should (= 1 (how-many "^#\\+begin_summary" (point-min)
                                 (point-max))))
          (should (string-match-p "Updated retained fact" canonical))
          (should (string-match-p "Latest user seven" canonical))
          (should (string-match-p "Pending result seven" canonical))
          (should-not (string-match-p "New user five" canonical))
          (should (string-match-p "previous continuation summary"
                                  (car bodies)))
          (should (string-match-p "First retained fact" (car bodies)))
          (should (string-match-p "New user five" (car bodies)))
          (should-not (string-match-p "Latest user seven" (car bodies)))
          (should (niwa--same-file-p canonical-path buffer-file-name))))))

  :doc "does not inspect or compact an agent's initial WAIT"
  (let* ((agent-buffer (generate-new-buffer " *niwa-agent-initial-wait*"))
         (invocation (niwa-agent-invocation-create
                      (niwa-agent--create :name "explorer")))
         (fsm (gptel-make-fsm
               :info (list :buffer agent-buffer
                           :history '(INIT)
                           :niwa-agent-invocation invocation)))
         (waits 0)
         compacted)
    (unwind-protect
        (cl-letf (((symbol-function 'gptel--handle-wait)
                   (lambda (_fsm) (cl-incf waits)))
                  ((symbol-function 'niwa-compact-run-start)
                   (lambda (&rest _) (setq compacted t))))
          (niwa--compact-handle-agent-wait fsm)
          (should (= 1 waits))
          (should-not compacted))
      (kill-buffer agent-buffer)))

  :doc "lets ephemeral agents ignore summarizer-only pressure but not target pressure"
  (let* ((agent-buffer (generate-new-buffer " *niwa-ephemeral-agent*"))
         (invocation
          (niwa-agent-invocation-create
           (niwa-agent--create :name "explorer")))
         (fsm
          (gptel-make-fsm
           :table gptel-send--transitions
           :handlers (copy-tree niwa-agent-exec--handlers)
           :state 'WAIT
           :info
           (list :buffer agent-buffer
                 :history '(TRET)
                 :data
                 (list :messages
                       (vector
                        (list :role "user" :content (make-string 1000 ?x))))
                 :niwa-agent-invocation invocation
                 :niwa-compaction-target-policy
                 '(:backend nil :model niwa-large-agent-model
                   :max-tokens 0 :request-params nil))))
         (waits 0) (terminal-events 0))
    (unwind-protect
        (progn
          (setf (niwa-agent-invocation-buffer invocation) agent-buffer)
          (setf (niwa-agent-invocation-agent-id invocation)
                "explorer--ephemeral")
          (setf (niwa-agent-invocation-transcript-status invocation)
                'running)
          (plist-put
           (gptel-fsm-info fsm) :niwa-agent-terminal-callback
           (lambda (_response _info)
             (cl-incf terminal-events)
             (setf (niwa-agent-invocation-transcript-status invocation)
                   'error)))
          (put 'niwa-large-agent-model :context-window 2)
          (put 'niwa-small-agent-model :context-window 0.4)
          (put 'niwa-agent-summary-model :context-window 0.4)
          (let ((niwa-compact-estimation-token-threshold 0.5)
                (niwa-model-reserve-tokens 0))
            (cl-letf (((symbol-function 'niwa-model-resolve-workload)
                       (lambda (&rest _)
                         (error "Summarizer policy must not be resolved")))
                      ((symbol-function 'gptel-get-preset)
                       (lambda (&rest _)
                         '(:description "test" :max-tokens nil
                           :request-params nil)))
                      ((symbol-function 'gptel--handle-wait)
                       (lambda (_fsm) (cl-incf waits)))
                      ((symbol-function 'gptel--handle-error) #'ignore)
                      ((symbol-function 'gptel--update-status) #'ignore))
              (with-current-buffer agent-buffer
                (insert "Ephemeral transcript stays intact.\n"))
              (niwa--compact-handle-agent-wait fsm)
              (should (= 1 waits))
              (should (= 0 terminal-events))
              (plist-put
               (gptel-fsm-info fsm) :niwa-compaction-target-policy
               '(:backend nil :model niwa-small-agent-model
                 :max-tokens 0 :request-params nil))
              (niwa--compact-handle-agent-wait fsm)))
          (should (= 1 waits))
          (should (= 1 terminal-events))
          (should (eq 'ERRS (gptel-fsm-state fsm)))
          (should (eq 'error
                      (niwa-agent-invocation-transcript-status invocation)))
          (with-current-buffer agent-buffer
            (should-not buffer-file-name)
            (should (equal "Ephemeral transcript stays intact.\n"
                           (buffer-string)))))
      (when (buffer-live-p agent-buffer)
        (kill-buffer agent-buffer))))

  :doc "lets a prefixless persisted agent proceed only below target pressure"
  (test-niwa-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-niwa-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((start (point)))
      (insert "First recent response.\n")
      (put-text-property start (point) 'gptel 'response))
    (insert "Only recent user turn.\n")
    (let ((start (point)))
      (insert "Second recent response.\n")
      (put-text-property start (point) 'gptel 'response))
    (insert "Pending tool result.\n")
    (should (niwa-agent-conversation-save invocation))
    (let* ((original (buffer-string))
           (fsm
            (gptel-make-fsm
             :table gptel-send--transitions
             :handlers (copy-tree niwa-agent-exec--handlers)
             :state 'WAIT
             :info
             (list :buffer agent-buffer
                   :history '(TRET)
                   :data
                   (list :messages
                         (vector
                          (list :role "user"
                                :content (make-string 1000 ?x))))
                   :niwa-agent-invocation invocation
                   :niwa-compaction-target-policy
                   '(:backend nil :model niwa-large-agent-model
                     :max-tokens 0 :request-params nil))))
           (waits 0) (terminal-events 0) (requests 0))
      (plist-put
       (gptel-fsm-info fsm) :niwa-agent-terminal-callback
       (lambda (_response _info)
         (cl-incf terminal-events)
         (setf (niwa-agent-invocation-transcript-status invocation)
               'error)))
      (put 'niwa-large-agent-model :context-window 20)
      (put 'niwa-small-agent-model :context-window 0.4)
      (put 'niwa-agent-summary-model :context-window 0.4)
      (let ((niwa-compact-estimation-token-threshold 0.5)
            (niwa-model-reserve-tokens 0)
            (niwa-compact-evidence-tail-turns 10)
            (niwa-compact-evidence-tail-budget 1.0))
        (cl-letf (((symbol-function 'niwa-model-resolve-workload)
                   (lambda (&rest _)
                     '(:backend nil :model niwa-agent-summary-model)))
                  ((symbol-function 'gptel-get-preset)
                   (lambda (&rest _)
                     '(:description "test" :max-tokens nil
                       :request-params nil)))
                  ((symbol-function 'gptel-request)
                   (lambda (&rest _) (cl-incf requests)))
                  ((symbol-function 'gptel--handle-wait)
                   (lambda (_fsm) (cl-incf waits)))
                  ((symbol-function 'gptel--handle-error) #'ignore)
                  ((symbol-function 'gptel--update-status) #'ignore)
                  ((symbol-function 'display-warning) #'ignore))
          (niwa--compact-handle-agent-wait fsm)
          (should (= 1 waits))
          (should (= 0 terminal-events))
          (plist-put
           (gptel-fsm-info fsm) :niwa-compaction-target-policy
           '(:backend nil :model niwa-small-agent-model
             :max-tokens 0 :request-params nil))
          (niwa--compact-handle-agent-wait fsm)))
      (should (= 0 requests))
      (should (= 1 waits))
      (should (= 1 terminal-events))
      (should (eq 'ERRS (gptel-fsm-state fsm)))
      (should (equal original (buffer-string)))
      (should
       (equal original
              (with-temp-buffer
                (insert (niwa-session-artifacts-read-artifact
                        session (file-relative-name canonical-path (niwa-session-save-path session))))
                (buffer-string)))))))


(provide 'test-niwa-compact)

(ert-deftest niwa--compact-rebuild-info-data-from-buffer/codex-cache-key ()
  "The real post-compaction rebuild keeps the root and agent cache keys."
  (let* ((gptel--known-backends nil)
         (backend (gptel-make-openai-oauth "Rebuild Codex"))
         (session (niwa-session--create :session-id "root-id"))
         (bufs nil))
    (cl-flet ((rebuilt-key (agent-id)
                (let ((chat (generate-new-buffer " *mev-rebuild-key*")))
                  (push chat bufs)
                  (with-current-buffer chat
                    (insert "Prompt\n")
                    (setq-local gptel-backend backend gptel-model 'gpt-5.5
                                gptel-use-tools nil
                                niwa--session session)
                    (when agent-id
                      (setq-local niwa--agent-invocation
                                  (niwa-agent-invocation--create :agent-id agent-id))))
                  ;; Real prompt buffer and realization; nothing is sent.
                  (let ((fsm (gptel-make-fsm
                              :info (list :buffer chat :backend backend
                                          :model 'gpt-5.5 :data 'old))))
                    (niwa--compact-rebuild-info-data-from-buffer fsm chat)
                    (plist-get (plist-get (gptel-fsm-info fsm) :data)
                               :prompt_cache_key)))))
      (unwind-protect
          (progn
            (should (equal "root-id" (rebuilt-key nil)))
            (should (equal "root-id:a--11111111" (rebuilt-key "a--11111111"))))
        (mapc #'kill-buffer bufs)))))

;;; test-niwa-compact.el ends here
