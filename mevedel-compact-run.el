;;; mevedel-compact-run.el -- Asynchronous compaction runs -*- lexical-binding: t -*-

;;; Commentary:

;; Owns one compaction attempt from evidence preparation through request,
;; retry, cancellation, application, and exact settlement.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-compact-estimation'
(declare-function mevedel-compact-estimation-clear-baseline
                  "mevedel-compact-estimation" ())
(declare-function mevedel-compact-estimation-estimate-tokens
                  "mevedel-compact-estimation" ())
(declare-function mevedel-compact-estimation-target-policy
                  "mevedel-compact-estimation" ())
(declare-function mevedel-compact-estimation-telemetry-inputs
                  "mevedel-compact-estimation" (estimate target-policy))
(declare-function mevedel-compact-estimation-workload-policy
                  "mevedel-compact-estimation" ())

;; `mevedel-compact-evidence'
(declare-function mevedel-compact-evidence-archived-tool-use-ids
                  "mevedel-compact-evidence" (begin end))
(declare-function mevedel-compact-evidence-buffer-active-p
                  "mevedel-compact-evidence" (buf))
(declare-function mevedel-compact-evidence-find-boundary
                  "mevedel-compact-evidence" ())
(declare-function mevedel-compact-evidence-region-with-tool-output-cap
                  "mevedel-compact-evidence"
                  (beg end cap &optional no-properties))
(declare-function mevedel-compact-evidence-select
                  "mevedel-compact-evidence" (target limit aggressive))
(defvar mevedel-compact-evidence-tail-tool-output-max)
(defvar mevedel-compact-evidence-tail-turns)

;; `mevedel-compact-target'
(declare-function mevedel-compact-target-begin-root-context-epoch
                  "mevedel-compact-target" (target auto))
(declare-function mevedel-compact-target-call
                  "mevedel-compact-target" (target operation &rest args))
(declare-function mevedel-compact-target-hook-audit-records
                  "mevedel-compact-target" (decision))
(declare-function mevedel-compact-target-main-target
                  "mevedel-compact-target" ())
(defvar mevedel-compact-target-current-request-hook-context)
(defvar mevedel-compact-target-current-request-reminder)

;; `mevedel-context-summary'
(declare-function mevedel-context-summary-generate
                  "mevedel-context-summary"
                  (source purpose callback &rest keys))

;; `mevedel-execution-transcript'
(declare-function mevedel-execution-transcript-prepare-archive
                  "mevedel-execution-transcript" (data-buffer tool-use-ids))

;; `mevedel-hooks'
(declare-function mevedel-hooks-additional-context-string
                  "mevedel-hooks" (decision &optional event))
(declare-function mevedel-hooks-event-plist
                  "mevedel-hooks" (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-run-event
                  "mevedel-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))

;; `mevedel-session-persistence'
(defvar mevedel-session--read-only-mode)

;; `mevedel-structs'
(defvar mevedel--view-buffer)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-finish
                  "mevedel-telemetry" (span &rest props))
(declare-function mevedel-telemetry-start
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-view-stream'
(declare-function mevedel-view--stop-request-progress
                  "mevedel-view-stream" ())
(declare-function mevedel-view--stop-spinner "mevedel-view-stream" ())

(defcustom mevedel-compact-run-warn-on-completion t
  "Whether to show a one-shot accuracy message after compaction."
  :type 'boolean
  :group 'mevedel)

(defvar-local mevedel-compact-run-in-flight nil
  "Non-nil while a compaction request is active in this buffer.")

(defvar-local mevedel-compact-run-cancel nil
  "Cancellation thunk for the active compaction run, or nil.")

(defvar-local mevedel-compact-run-failure-count 0
  "Consecutive compaction failures in this session.")

(defvar-local mevedel-compact-run-warning-shown nil
  "Non-nil after showing the post-compaction accuracy warning.")

(cl-defstruct (mevedel-compact-run--state
               (:constructor mevedel-compact-run--state-create))
  "Mutable state for one asynchronous compaction run."
  aggressive
  applied
  archived-tool-use-ids
  auto
  callback
  chat-buffer
  focus
  instructions
  invocation
  old-content
  pending-text
  policy
  prepared-summary
  preserved-tail-turns
  purpose
  request-cancel
  retry-timer
  session
  settled
  source-transform
  summary-ready
  tail-text
  target
  telemetry-span
  tokens-before
  trigger
  workspace
  (attempt 0))

(defun mevedel-compact-run--finish (state err)
  "Settle compaction STATE once with ERR."
  (unless (mevedel-compact-run--state-settled state)
    (setf (mevedel-compact-run--state-settled state) t)
    (let ((chat-buffer (mevedel-compact-run--state-chat-buffer state))
          (span (mevedel-compact-run--state-telemetry-span state))
          (retry-timer (mevedel-compact-run--state-retry-timer state)))
      (when retry-timer
        (cancel-timer retry-timer))
      (when (buffer-live-p chat-buffer)
        (with-current-buffer chat-buffer
          (setq-local mevedel-compact-run-in-flight nil)
          (setq-local mevedel-compact-run-cancel nil)))
      (setf (mevedel-compact-run--state-request-cancel state) nil
            (mevedel-compact-run--state-retry-timer state) nil)
      (when span
        (mevedel-telemetry-finish
         span
         :outcome (cond
                   ((null err) 'success)
                   ((eq err :skip) 'skipped)
                   (t 'error))
         :tokens-after
         (and (buffer-live-p chat-buffer)
              (with-current-buffer chat-buffer
                (ignore-errors (mevedel-compact-estimation-estimate-tokens))))
         :error-class
         (and err (if (symbolp err) err 'compaction-error))))
      (when-let* ((callback (mevedel-compact-run--state-callback state)))
        (funcall callback err)))))

(defun mevedel-compact-run--fail
    (state err retryable &optional ignore-failure-count)
  "Fail compaction STATE with ERR.
Retry when RETRYABLE and attempts remain.  IGNORE-FAILURE-COUNT leaves
the persistent failure counter unchanged."
  (unless (mevedel-compact-run--state-settled state)
    (if (and retryable
             (< (mevedel-compact-run--state-attempt state) 3))
        (let* ((attempt (mevedel-compact-run--state-attempt state))
               (delay (expt 2 (1- attempt)))
               (chat-buffer (mevedel-compact-run--state-chat-buffer state)))
          (message "mevedel: compaction failed, retrying in %ss (%s)"
                   delay err)
          (setf
           (mevedel-compact-run--state-retry-timer state)
           (run-at-time
            delay nil
            (lambda ()
              (unless (mevedel-compact-run--state-settled state)
                (setf (mevedel-compact-run--state-retry-timer state) nil
                      (mevedel-compact-run--state-request-cancel state) nil)
                (when (buffer-live-p chat-buffer)
                  (with-current-buffer chat-buffer
                    (mevedel-compact-run--begin-attempt state))))))))
      (unless ignore-failure-count
        (cl-incf mevedel-compact-run-failure-count))
      (display-warning 'mevedel err :warning)
      (unless (mevedel-compact-run--state-auto state)
        (when-let* ((vb mevedel--view-buffer)
                    (_ (buffer-live-p vb)))
          (with-current-buffer vb
            (if (fboundp 'mevedel-view--stop-request-progress)
                (mevedel-view--stop-request-progress)
              (mevedel-view--stop-spinner)))))
      (mevedel-compact-run--finish state err))))

(defun mevedel-compact-run--finish-success (state summary)
  "Complete compaction STATE after applying SUMMARY."
  (let* ((tokens-before (mevedel-compact-run--state-tokens-before state))
         (tokens-after (mevedel-compact-estimation-estimate-tokens))
         (aggressive (mevedel-compact-run--state-aggressive state))
         (session (mevedel-compact-run--state-session state))
         (workspace (mevedel-compact-run--state-workspace state))
         (target (mevedel-compact-run--state-target state)))
    (message
     "mevedel: compaction complete (%dk -> %dk tokens, %d turns preserved)"
     (/ tokens-before 1000)
     (/ tokens-after 1000)
     (if aggressive 0 mevedel-compact-evidence-tail-turns))
    (mevedel-hooks-run-event
     'PostCompact
     (mevedel-hooks-event-plist
      'PostCompact session workspace
      :trigger (mevedel-compact-run--state-trigger state)
      :summary summary
      :tokens-before tokens-before
      :tokens-after tokens-after
      :aggressive aggressive
      :origin (plist-get target :origin)
      :transcript-path (plist-get target :transcript-path))
     (lambda (_decision)
       (unless (mevedel-compact-run--state-settled state)
         (unwind-protect
             (progn
               (condition-case err
                   (mevedel-compact-target-begin-root-context-epoch
                    target (mevedel-compact-run--state-auto state))
                 (error
                  (display-warning
                   'mevedel
                   (format "SessionStart after compaction failed: %s"
                           (error-message-string err))
                   :warning)))
               (condition-case err
                   (mevedel-compact-target-call
                    target :complete
                    (mevedel-compact-run--state-auto state))
                 (error
                  (display-warning
                   'mevedel
                   (format "Compaction view completion failed: %s"
                           (error-message-string err))
                   :warning)))
               (when (and (plist-get target :warn-on-completion)
                          mevedel-compact-run-warn-on-completion
                          (not mevedel-compact-run-warning-shown))
                 (setq mevedel-compact-run-warning-shown t)
                 (message
                  "mevedel: long threads with multiple compactions can reduce model accuracy; consider starting a new session for unrelated work")))
           (mevedel-compact-run--finish state nil))))
     session workspace nil
     (mevedel-compact-run--state-invocation state))))

(defun mevedel-compact-run--apply-summary (state summary hook-audits)
  "Apply SUMMARY and HOOK-AUDITS for compaction STATE once."
  (unless (mevedel-compact-run--state-settled state)
    (condition-case err
        (progn
          (when-let* ((summary-ready
                       (mevedel-compact-run--state-summary-ready state)))
            (setq summary (funcall summary-ready summary)))
          (when (mevedel-compact-run--state-archived-tool-use-ids state)
            (require 'mevedel-execution-transcript)
            (setf
             (mevedel-compact-run--state-target state)
             (plist-put
              (mevedel-compact-run--state-target state)
              :execution-archive-plan
              (mevedel-execution-transcript-prepare-archive
               (mevedel-compact-run--state-chat-buffer state)
               (mevedel-compact-run--state-archived-tool-use-ids state)))))
          (mevedel-compact-target-call
           (mevedel-compact-run--state-target state)
           :apply summary
           (mevedel-compact-run--state-tail-text state)
           (mevedel-compact-run--state-pending-text state)
           hook-audits
           (mevedel-compact-run--state-auto state)
           (mevedel-compact-run--state-preserved-tail-turns state))
          (setf (mevedel-compact-run--state-applied state) t)
          (mevedel-compact-estimation-clear-baseline)
          (setq mevedel-compact-run-failure-count 0)
          (mevedel-compact-run--finish-success state summary))
      (error
       (mevedel-compact-run--fail state (format "%s" err) nil)))))

(defun mevedel-compact-run--handle-summary
    (state hook-audits result)
  "Handle one context-summary RESULT for STATE and HOOK-AUDITS."
  (when (and (not (mevedel-compact-run--state-settled state))
             (buffer-live-p (mevedel-compact-run--state-chat-buffer state)))
    (with-current-buffer (mevedel-compact-run--state-chat-buffer state)
      (pcase (plist-get result :outcome)
        ('success
         (mevedel-compact-run--apply-summary
          state (plist-get result :summary) hook-audits))
        ('aborted
         (mevedel-compact-run--fail state "Compaction aborted" nil t))
        ('error
         (let ((sizep (eq (plist-get result :error-class) 'size)))
           (mevedel-compact-run--fail
            state
            (format "Compaction failed: %s" (plist-get result :error))
            (not sizep)
            sizep)))))))

(defun mevedel-compact-run--send-request (state hook-context hook-audits)
  "Generate STATE's summary with HOOK-CONTEXT and HOOK-AUDITS.
A synchronous generator failure settles STATE as a retryable attempt
rather than escaping, because the caller may be an asynchronous hook."
  (unless (mevedel-compact-run--state-settled state)
    (condition-case err
        (if-let* ((prepared (mevedel-compact-run--state-prepared-summary state)))
            (mevedel-compact-run--apply-summary state prepared hook-audits)
          (require 'mevedel-context-summary)
          (let* ((source
                  (if hook-context
                      (concat
                       (mevedel-compact-run--state-old-content state)
                       "\n\n--- evidence item; provenance: hook-context ---\n"
                       hook-context
                       "\n--- end evidence item ---")
                    (mevedel-compact-run--state-old-content state)))
                 (source-transform
                  (mevedel-compact-run--state-source-transform state))
                 (source (if source-transform
                             (funcall source-transform source)
                           source))
                 (purpose (mevedel-compact-run--state-purpose state))
                 (cancel
                  (mevedel-context-summary-generate
                   source purpose
                   (apply-partially
                    #'mevedel-compact-run--handle-summary state hook-audits)
                   :session (mevedel-compact-run--state-session state)
                   :previous-summary
                   (and (eq purpose 'continuation)
                        (plist-get (mevedel-compact-run--state-target state)
                                   :previous-summary))
                   :focus (mevedel-compact-run--state-focus state)
                   :guidance (mevedel-compact-run--state-instructions state)
                   :policy (mevedel-compact-run--state-policy state))))
            (unless (mevedel-compact-run--state-settled state)
              (setf (mevedel-compact-run--state-request-cancel state) cancel))))
      (error
       (mevedel-compact-run--fail state (format "%s" err) t)))))

(defun mevedel-compact-run--begin-attempt (state)
  "Run STATE's PreCompact hook and start one summary attempt."
  (unless (mevedel-compact-run--state-settled state)
    (cl-incf (mevedel-compact-run--state-attempt state))
    (let ((session (mevedel-compact-run--state-session state))
          (workspace (mevedel-compact-run--state-workspace state))
          (target (mevedel-compact-run--state-target state)))
      (condition-case err
          (mevedel-hooks-run-event
           'PreCompact
           (mevedel-hooks-event-plist
            'PreCompact session workspace
            :trigger (mevedel-compact-run--state-trigger state)
            :tokens-before (mevedel-compact-run--state-tokens-before state)
            :aggressive (mevedel-compact-run--state-aggressive state)
            :instructions (mevedel-compact-run--state-instructions state)
            :origin (plist-get target :origin)
            :transcript-path (plist-get target :transcript-path))
           (lambda (decision)
             (unless (mevedel-compact-run--state-settled state)
               (if (and (plist-member decision :continue)
                        (not (plist-get decision :continue)))
                   (mevedel-compact-run--finish
                    state
                    (or (plist-get decision :stop-reason)
                        "PreCompact hook stopped compaction"))
                 (let* ((context
                         (mevedel-hooks-additional-context-string
                          decision 'PreCompact))
                        (hook-audits
                         (and context
                              (mevedel-compact-target-hook-audit-records decision))))
                   (message "mevedel: compacting (%dk -> ...)"
                            (/ (mevedel-compact-run--state-tokens-before state)
                               1000))
                   (when (= (mevedel-compact-run--state-attempt state) 1)
                     (mevedel-compact-target-call target :start))
                   (mevedel-compact-run--send-request
                    state context hook-audits)))))
           session workspace nil
           (mevedel-compact-run--state-invocation state))
        (error
         (mevedel-compact-run--finish state (format "%s" err))
         (signal (car err) (cdr err)))))))


(defun mevedel-compact-run--prepare
    (state limit admission instructions pending-start
           prepared-summary summary-ready)
  "Populate compaction STATE from history ending at LIMIT."
  (let* ((target (mevedel-compact-run--state-target state))
         (aggressive (mevedel-compact-run--state-aggressive state))
         (selection
          (mevedel-compact-evidence-select target limit aggressive))
         (history-regions (plist-get selection :history-regions))
         (preserved-tail-turns
          (plist-get selection :preserved-tail-turns))
         (tail-start (plist-get selection :tail-start))
         (archived-tool-use-ids
          (delete-dups
           (mapcan
            (lambda (range)
              (mevedel-compact-evidence-archived-tool-use-ids
               (car range) (cdr range)))
            history-regions))))
    (setf
     (mevedel-compact-run--state-archived-tool-use-ids state)
     archived-tool-use-ids
     (mevedel-compact-run--state-instructions state) instructions
     (mevedel-compact-run--state-invocation state)
     (plist-get target :invocation)
     (mevedel-compact-run--state-old-content state)
     (plist-get selection :content)
     (mevedel-compact-run--state-pending-text state)
     (and pending-start
          (buffer-substring pending-start (point-max)))
     (mevedel-compact-run--state-policy state)
     (or (plist-get admission :summary-policy)
         (mevedel-compact-estimation-workload-policy))
     (mevedel-compact-run--state-prepared-summary state)
     prepared-summary
     (mevedel-compact-run--state-preserved-tail-turns state)
     preserved-tail-turns
     (mevedel-compact-run--state-summary-ready state) summary-ready
     (mevedel-compact-run--state-tail-text state)
     (and (not aggressive)
          (mevedel-compact-evidence-region-with-tool-output-cap
           tail-start limit mevedel-compact-evidence-tail-tool-output-max)))
    state))

(cl-defun mevedel-compact-run-start
    (&key aggressive instructions pending-start callback auto
          admission target prepared-summary summary-ready
          purpose focus source-transform)
  "Run compaction in the current chat buffer.
AGGRESSIVE drops the preserved tail.  INSTRUCTIONS are manual summary
instructions.  PENDING-START marks an inserted-but-unsent prompt region.
CALLBACK receives (ERR) when compaction settles.  AUTO marks an
auto-compaction call.  ADMISSION carries the pre-resolved summarizer
policy and whether the active model crossed its own threshold.  TARGET
is the private adapter for a persisted agent transcript; nil selects
the active main-session segment.  PREPARED-SUMMARY skips the model
request.  SUMMARY-READY may persist or normalize a completed summary
before it is applied.  PURPOSE defaults to `continuation'.  FOCUS is
task-relevance data for a handoff summary.  SOURCE-TRANSFORM filters the
  complete projected evidence immediately before generation."
  (require 'mevedel-compact-estimation)
  (require 'mevedel-compact-evidence)
  (require 'mevedel-compact-target)
  (let* ((chat-buffer (current-buffer))
         (target (or target (mevedel-compact-target-main-target)))
         (session (plist-get target :session))
         (tokens-before (mevedel-compact-estimation-estimate-tokens))
         (target-policy (mevedel-compact-estimation-target-policy))
         (telemetry-span
          (and session
               (fboundp 'mevedel-telemetry-start)
               (apply #'mevedel-telemetry-start
                      session 'compaction
                      :trigger (if auto 'auto 'manual)
                      :aggressive (and aggressive t)
                      :tokens-before tokens-before
                      :target-origin (plist-get target :origin)
                      (mevedel-compact-estimation-telemetry-inputs
                       tokens-before target-policy))))
         (state
          (mevedel-compact-run--state-create
           :aggressive aggressive
           :auto auto
           :callback callback
           :chat-buffer chat-buffer
           :focus focus
           :purpose (or purpose 'continuation)
           :session session
           :source-transform source-transform
           :target target
           :telemetry-span telemetry-span
           :tokens-before tokens-before
           :trigger (if auto "auto" "manual")
           :workspace (plist-get target :workspace))))
    (setq mevedel-compact-target-current-request-reminder nil
          mevedel-compact-target-current-request-hook-context nil)
    (when mevedel-compact-run-in-flight
      (user-error "Compaction already in progress"))
    (when (bound-and-true-p mevedel-session--read-only-mode)
      (user-error "Session is read-only"))
    (when (and (not pending-start)
               (mevedel-compact-evidence-buffer-active-p chat-buffer))
      (user-error "Cannot compact while a request is active"))
    (unless (plist-get target :eligible-p)
      (user-error "Current buffer is not the active persisted segment"))
    (let ((limit (or pending-start (mevedel-compact-evidence-find-boundary))))
      (unless limit
        (if auto
            (cl-return-from mevedel-compact-run-start
              (mevedel-compact-run--finish state :skip))
          (user-error "Not enough conversation content to compact")))
      (mevedel-compact-run--prepare
       state limit admission instructions pending-start
       prepared-summary summary-ready)
      (when (string-blank-p
             (mevedel-compact-run--state-old-content state))
        (if auto
            (cl-return-from mevedel-compact-run-start
              (mevedel-compact-run--finish
               state
               (if (plist-get admission :target-pressure)
                   "No compactable history remains at target pressure"
                 :skip)))
          (user-error "Not enough conversation content to compact")))
      (require 'mevedel-hooks)
      (setq mevedel-compact-run-in-flight t)
      (setq mevedel-compact-run-cancel
            (lambda ()
              (unless (or (mevedel-compact-run--state-settled state)
                          (mevedel-compact-run--state-applied state))
                (when-let* ((cancel
                             (mevedel-compact-run--state-request-cancel
                              state)))
                  (funcall cancel))
                (unless (or (mevedel-compact-run--state-settled state)
                            (mevedel-compact-run--state-applied state))
                  (mevedel-compact-run--fail
                   state "Compaction aborted" nil t)))))
      (mevedel-compact-run--begin-attempt state))))

(provide 'mevedel-compact-run)

;;; mevedel-compact-run.el ends here
