;;; niwa-compact-run.el -- Asynchronous compaction runs -*- lexical-binding: t -*-

;;; Commentary:

;; Owns one compaction attempt from evidence preparation through request,
;; retry, cancellation, application, and exact settlement.

;;; Code:

(require 'niwa-telemetry)
(require 'niwa-utilities)

(eval-when-compile (require 'cl-lib))

;; `gptel'
(declare-function gptel-backend-name "ext:gptel" (backend))

;; `niwa-compact-estimation'
(declare-function niwa-compact-estimation-clear-baseline
                  "niwa-compact-estimation" ())
(declare-function niwa-compact-estimation-estimate-tokens
                  "niwa-compact-estimation" ())
(declare-function niwa-compact-estimation-target-policy
                  "niwa-compact-estimation" ())
(declare-function niwa-compact-estimation-telemetry-inputs
                  "niwa-compact-estimation" (estimate target-policy))
(declare-function niwa-compact-estimation-workload-policy
                  "niwa-compact-estimation" ())

;; `niwa-compact-evidence'
(declare-function niwa-compact-evidence-archived-tool-use-ids
                  "niwa-compact-evidence" (begin end))
(declare-function niwa-compact-evidence-buffer-active-p
                  "niwa-compact-evidence" (buf))
(declare-function niwa-compact-evidence-find-boundary
                  "niwa-compact-evidence" ())
(declare-function niwa-compact-evidence-region-with-tool-output-cap
                  "niwa-compact-evidence"
                  (beg end cap &optional no-properties))
(declare-function niwa-compact-evidence-select
                  "niwa-compact-evidence" (target limit aggressive))
(defvar niwa-compact-evidence-tail-tool-output-max)
(defvar niwa-compact-evidence-tail-turns)

;; `niwa-compact-target'
(declare-function niwa-compact-target-begin-root-context-epoch
                  "niwa-compact-target" (target auto))
(declare-function niwa-compact-target-call
                  "niwa-compact-target" (target operation &rest args))
(declare-function niwa-compact-target-hook-audit-records
                  "niwa-compact-target" (decision))
(declare-function niwa-compact-target-main-target
                  "niwa-compact-target" ())
(defvar niwa-compact-target-current-request-hook-context)
(defvar niwa-compact-target-current-request-reminder)

;; `niwa-context-summary'
(declare-function niwa-context-summary-generate
                  "niwa-context-summary"
                  (source purpose callback &rest keys))
(autoload 'niwa-context-summary-generate "niwa-context-summary")

;; `niwa-execution-transcript'
(declare-function niwa-execution-transcript-prepare-archive
                  "niwa-execution-transcript" (data-buffer tool-use-ids))
(autoload 'niwa-execution-transcript-prepare-archive
  "niwa-execution-transcript")

;; `niwa-hooks'
(declare-function niwa-hooks-additional-context-string
                  "niwa-hooks" (decision &optional event))
(declare-function niwa-hooks-event-plist
                  "niwa-hooks" (event &optional session workspace &rest extra))
(declare-function niwa-hooks-run-event
                  "niwa-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))

;; `niwa-journal-capture'
(declare-function niwa-journal-capture-checkpoint "niwa-journal-capture" (session buffer))
(declare-function niwa-journal-capture-seal-and-schedule "niwa-journal-capture"
                  (session buffer trigger &optional captures))
(autoload 'niwa-journal-capture-checkpoint "niwa-journal-capture")
(autoload 'niwa-journal-capture-seal-and-schedule "niwa-journal-capture")

;; `niwa-pending-inputs'
(declare-function niwa-pending-inputs-schedule-held-input-delivery
                  "niwa-pending-inputs" (&optional data-buffer))
(autoload 'niwa-pending-inputs-schedule-held-input-delivery
  "niwa-pending-inputs")

;; `niwa-session-persistence'
(defvar niwa-session--read-only-mode)

;; `niwa-structs'
(declare-function niwa-session-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-p "niwa-structs" (object))
(defvar niwa--view-buffer)

;; `niwa-view-stream'
(declare-function niwa-view-stream-end-progress
                  "niwa-view-stream" (view))
(autoload 'niwa-view-stream-end-progress "niwa-view-stream")

(defcustom niwa-compact-run-warn-on-completion t
  "Whether to show a one-shot accuracy message after compaction."
  :type 'boolean
  :group 'niwa)

(defvar-local niwa-compact-run-in-flight nil
  "Non-nil while a compaction request is active in this buffer.")

(defvar-local niwa-compact-run-cancel nil
  "Cancellation thunk for the active compaction run, or nil.")

(defvar niwa-compact-run-callback-aborted-p nil
  "Non-nil while delivering a cancelled compaction's callback.")

(defvar-local niwa-compact-run-failure-count 0
  "Consecutive compaction failures in this session.")

(defvar-local niwa-compact-run-warning-shown nil
  "Non-nil after showing the post-compaction accuracy warning.")

(cl-defstruct (niwa-compact-run--state
               (:constructor niwa-compact-run--state-create))
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
  journal-captures
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
  target-policy
  telemetry-span
  tokens-before
  trigger
  workspace
  (attempt 0))

(defun niwa-compact-run--label (state)
  "Return conversation and frozen model identities for compaction STATE."
  (let* ((session (niwa-compact-run--state-session state))
         (buffer (niwa-compact-run--state-chat-buffer state))
         (models
          (mapcar
           (lambda (policy)
             (if (not (listp policy)) "unknown"
               (format "%s:%s"
                       (or (and (plist-get policy :backend)
                                (gptel-backend-name (plist-get policy :backend)))
                           "unknown")
                       (or (plist-get policy :model) "unknown"))))
           (list (niwa-compact-run--state-target-policy state)
                 (niwa-compact-run--state-policy state)))))
    (format
     "[%s %s; target %s; summary %s]"
     (or (and (niwa-session-p session) (niwa-session-name session))
         (and (buffer-live-p buffer) (buffer-name buffer))
         "conversation")
     (or (plist-get (niwa-compact-run--state-target state) :origin) "/root")
     (car models) (cadr models))))

(defun niwa-compact-run--finish (state err &optional aborted)
  "Settle compaction STATE once with ERR, identifying an ABORTED run."
  (unless (niwa-compact-run--state-settled state)
    (setf (niwa-compact-run--state-settled state) t)
    (let ((chat-buffer (niwa-compact-run--state-chat-buffer state))
          (span (niwa-compact-run--state-telemetry-span state))
          (retry-timer (niwa-compact-run--state-retry-timer state)))
      (when retry-timer
        (cancel-timer retry-timer))
      (when (buffer-live-p chat-buffer)
        (with-current-buffer chat-buffer
          (setq-local niwa-compact-run-in-flight nil)
          (setq-local niwa-compact-run-cancel nil)))
      (setf (niwa-compact-run--state-request-cancel state) nil
            (niwa-compact-run--state-retry-timer state) nil)
      (when span
        ;; Consume the span: an error handler further out closes an
        ;; unsettled span itself, and must find nothing once the
        ;; settlement owner has reported.
        (setf (niwa-compact-run--state-telemetry-span state) nil)
        (niwa-telemetry-finish
         span
         :outcome (cond
                   ((null err) 'success)
                   ((eq err :skip) 'skipped)
                   (t 'error))
         :tokens-after
         (and (buffer-live-p chat-buffer)
              (with-current-buffer chat-buffer
                (ignore-errors (niwa-compact-estimation-estimate-tokens))))
         :error-class
         (and err (if (symbolp err) err 'compaction-error))))
      (when-let* ((callback (niwa-compact-run--state-callback state)))
        (let ((niwa-compact-run-callback-aborted-p aborted))
          (funcall callback err)))
      ;; Automatic compaction continues its existing turn.  Manual
      ;; compaction has no turn-end edge to release inputs held by it.
      (when (and (null err) (not (niwa-compact-run--state-auto state))
                 (buffer-live-p chat-buffer))
        (niwa-pending-inputs-schedule-held-input-delivery chat-buffer)))))

(defun niwa-compact-run--fail
    (state err retryable &optional aborted)
  "Fail compaction STATE with ERR.
Retry when RETRYABLE and attempts remain.  An ABORTED run leaves the
persistent failure counter unchanged."
  (unless (niwa-compact-run--state-settled state)
    (if (and retryable
             (< (niwa-compact-run--state-attempt state) 3))
        (let* ((attempt (niwa-compact-run--state-attempt state))
               (delay (expt 2 (1- attempt)))
               (chat-buffer (niwa-compact-run--state-chat-buffer state)))
          (niwa-diagnostic-message
           (niwa-diagnostic-scope (niwa-compact-run--state-session state))
           "%s (compaction failed, retrying in %ss)" err delay)
          (setf
           (niwa-compact-run--state-retry-timer state)
           (run-at-time
            delay nil
            (lambda ()
              (unless (niwa-compact-run--state-settled state)
                (setf (niwa-compact-run--state-retry-timer state) nil
                      (niwa-compact-run--state-request-cancel state) nil)
                (when (buffer-live-p chat-buffer)
                  (with-current-buffer chat-buffer
                    (niwa-compact-run--begin-attempt state))))))))
      (unless aborted
        (cl-incf niwa-compact-run-failure-count))
      (niwa-diagnostic-warning
       (niwa-diagnostic-scope (niwa-compact-run--state-session state))
       'niwa err :warning)
      (unless (niwa-compact-run--state-auto state)
        (niwa-view-stream-end-progress niwa--view-buffer))
      (niwa-compact-run--finish state err aborted))))

(defun niwa-compact-run--finish-success (state summary)
  "Complete compaction STATE after applying SUMMARY."
  (let* ((tokens-before (niwa-compact-run--state-tokens-before state))
         (tokens-after (niwa-compact-estimation-estimate-tokens))
         (aggressive (niwa-compact-run--state-aggressive state))
         (session (niwa-compact-run--state-session state))
         (workspace (niwa-compact-run--state-workspace state))
         (target (niwa-compact-run--state-target state)))
    (when (and (not (niwa-compact-run--state-invocation state))
               (niwa-compact-run--state-journal-captures state))
      (niwa-journal-capture-seal-and-schedule
       session (niwa-compact-run--state-chat-buffer state) 'compaction
       (niwa-compact-run--state-journal-captures state)))
    (message
     "niwa: compaction complete %s (buffer %dk -> %dk tokens, %d turns preserved)"
     (niwa-compact-run--label state)
     (/ tokens-before 1000)
     (/ tokens-after 1000)
     (if aggressive 0 niwa-compact-evidence-tail-turns))
    (niwa-hooks-run-event
     'PostCompact
     (niwa-hooks-event-plist
      'PostCompact session workspace
      :trigger (niwa-compact-run--state-trigger state)
      :summary summary
      :tokens-before tokens-before
      :tokens-after tokens-after
      :aggressive aggressive
      :origin (plist-get target :origin)
      :transcript-path (plist-get target :transcript-path))
     (lambda (_decision)
       (unless (niwa-compact-run--state-settled state)
         (unwind-protect
             (progn
               (condition-case err
                   (niwa-compact-target-begin-root-context-epoch
                    target (niwa-compact-run--state-auto state))
                 (error
                  (niwa-diagnostic-warning (niwa-diagnostic-scope session)
                   'niwa
                   (format "%s (SessionStart after compaction failed)"
                           (error-message-string err))
                   :warning)))
               (condition-case err
                   (niwa-compact-target-call
                    target :complete
                    (niwa-compact-run--state-auto state))
                 (error
                  (niwa-diagnostic-warning (niwa-diagnostic-scope session)
                   'niwa
                   (format "%s (compaction view completion failed)"
                           (error-message-string err))
                   :warning)))
               (when (and (plist-get target :warn-on-completion)
                          niwa-compact-run-warn-on-completion
                          (not niwa-compact-run-warning-shown))
                 (setq niwa-compact-run-warning-shown t)
                 (niwa-diagnostic-message (niwa-diagnostic-scope session)
                  "Long threads with multiple compactions can reduce model accuracy; consider starting a new session for unrelated work")))
           (niwa-compact-run--finish state nil))))
     session workspace nil
     (niwa-compact-run--state-invocation state))))

(defun niwa-compact-run--apply-summary (state summary hook-audits)
  "Apply SUMMARY and HOOK-AUDITS for compaction STATE once."
  (unless (niwa-compact-run--state-settled state)
    (condition-case err
        (progn
          (when-let* ((summary-ready
                       (niwa-compact-run--state-summary-ready state)))
            (setq summary (funcall summary-ready summary)))
          (when (niwa-compact-run--state-archived-tool-use-ids state)
            (setf
             (niwa-compact-run--state-target state)
             (plist-put
              (niwa-compact-run--state-target state)
              :execution-archive-plan
              (niwa-execution-transcript-prepare-archive
               (niwa-compact-run--state-chat-buffer state)
               (niwa-compact-run--state-archived-tool-use-ids state)))))
          (niwa-compact-target-call
           (niwa-compact-run--state-target state)
           :apply summary
           (niwa-compact-run--state-tail-text state)
           (niwa-compact-run--state-pending-text state)
           hook-audits
           (niwa-compact-run--state-auto state)
           (niwa-compact-run--state-preserved-tail-turns state))
          (setf (niwa-compact-run--state-applied state) t)
          (niwa-compact-estimation-clear-baseline)
          (setq niwa-compact-run-failure-count 0)
          (niwa-compact-run--finish-success state summary))
      (error
       (niwa-compact-run--fail state (format "%s" err) nil)))))

(defun niwa-compact-run--handle-summary
    (state hook-audits result)
  "Handle one context-summary RESULT for STATE and HOOK-AUDITS."
  (when (and (not (niwa-compact-run--state-settled state))
             (buffer-live-p (niwa-compact-run--state-chat-buffer state)))
    (with-current-buffer (niwa-compact-run--state-chat-buffer state)
      (pcase (plist-get result :outcome)
        ('success
         (niwa-compact-run--apply-summary
          state (plist-get result :summary) hook-audits))
        ('aborted
         (niwa-compact-run--fail state "Compaction aborted" nil t))
        ('error
         (let ((sizep (eq (plist-get result :error-class) 'size)))
           (niwa-compact-run--fail
            state
            (format "Compaction failed: %s" (plist-get result :error))
            (not sizep)
            sizep)))))))

(defun niwa-compact-run--send-request (state hook-context hook-audits)
  "Generate STATE's summary with HOOK-CONTEXT and HOOK-AUDITS.
A synchronous generator failure settles STATE as a retryable attempt
rather than escaping, because the caller may be an asynchronous hook."
  (unless (niwa-compact-run--state-settled state)
    (condition-case err
        (if-let* ((prepared (niwa-compact-run--state-prepared-summary state)))
            (niwa-compact-run--apply-summary state prepared hook-audits)
          (let* ((source
                  (if hook-context
                      (concat
                       (niwa-compact-run--state-old-content state)
                       "\n\n--- evidence item; provenance: hook-context ---\n"
                       hook-context
                       "\n--- end evidence item ---")
                    (niwa-compact-run--state-old-content state)))
                 (source-transform
                  (niwa-compact-run--state-source-transform state))
                 (source (if source-transform
                             (funcall source-transform source)
                           source))
                 (purpose (niwa-compact-run--state-purpose state))
                 (cancel
                  (niwa-context-summary-generate
                   source purpose
                   (apply-partially
                    #'niwa-compact-run--handle-summary state hook-audits)
                   :session (niwa-compact-run--state-session state)
                   :previous-summary
                   (and (eq purpose 'continuation)
                        (plist-get (niwa-compact-run--state-target state)
                                   :previous-summary))
                   :focus (niwa-compact-run--state-focus state)
                   :guidance (niwa-compact-run--state-instructions state)
                   :policy (niwa-compact-run--state-policy state))))
            (unless (niwa-compact-run--state-settled state)
              (setf (niwa-compact-run--state-request-cancel state) cancel))))
      (error
       (niwa-compact-run--fail state (format "%s" err) t)))))

(defun niwa-compact-run--begin-attempt (state)
  "Run STATE's PreCompact hook and start one summary attempt."
  (unless (niwa-compact-run--state-settled state)
    (cl-incf (niwa-compact-run--state-attempt state))
    (let ((session (niwa-compact-run--state-session state))
          (workspace (niwa-compact-run--state-workspace state))
          (target (niwa-compact-run--state-target state)))
      (condition-case err
          (niwa-hooks-run-event
           'PreCompact
           (niwa-hooks-event-plist
            'PreCompact session workspace
            :trigger (niwa-compact-run--state-trigger state)
            :tokens-before (niwa-compact-run--state-tokens-before state)
            :aggressive (niwa-compact-run--state-aggressive state)
            :instructions (niwa-compact-run--state-instructions state)
            :origin (plist-get target :origin)
            :transcript-path (plist-get target :transcript-path))
           (lambda (decision)
             (unless (niwa-compact-run--state-settled state)
               (if (and (plist-member decision :continue)
                        (not (plist-get decision :continue)))
                   (niwa-compact-run--finish
                    state
                    (or (plist-get decision :stop-reason)
                        "PreCompact hook stopped compaction"))
                 (let* ((context
                         (niwa-hooks-additional-context-string
                          decision 'PreCompact))
                        (hook-audits
                         (and context
                              (niwa-compact-target-hook-audit-records decision))))
                   (message "niwa: compacting %s (buffer %dk -> ...)"
                            (niwa-compact-run--label state)
                            (/ (niwa-compact-run--state-tokens-before state)
                               1000))
                   (when (= (niwa-compact-run--state-attempt state) 1)
                     (niwa-compact-target-call target :start))
                   (niwa-compact-run--send-request
                    state context hook-audits)))))
           session workspace nil
           (niwa-compact-run--state-invocation state))
        (error
         (niwa-compact-run--finish state (format "%s" err))
         (signal (car err) (cdr err)))))))


(defun niwa-compact-run--prepare
    (state limit admission instructions pending-start
           prepared-summary summary-ready)
  "Populate compaction STATE from history ending at LIMIT."
  (let* ((target (niwa-compact-run--state-target state))
         (aggressive (niwa-compact-run--state-aggressive state))
         (selection
          (niwa-compact-evidence-select target limit aggressive))
         (history-regions (plist-get selection :history-regions))
         (preserved-tail-turns
          (plist-get selection :preserved-tail-turns))
         (tail-start (plist-get selection :tail-start))
         (archived-tool-use-ids
          (delete-dups
           (mapcan
            (lambda (range)
              (niwa-compact-evidence-archived-tool-use-ids
               (car range) (cdr range)))
            history-regions))))
    (setf
     (niwa-compact-run--state-archived-tool-use-ids state)
     archived-tool-use-ids
     (niwa-compact-run--state-instructions state) instructions
     (niwa-compact-run--state-invocation state)
     (plist-get target :invocation)
     (niwa-compact-run--state-old-content state)
     (plist-get selection :content)
     (niwa-compact-run--state-pending-text state)
     (and pending-start
          (buffer-substring pending-start (point-max)))
     (niwa-compact-run--state-policy state)
     (or (plist-get admission :summary-policy)
         (niwa-compact-estimation-workload-policy))
     (niwa-compact-run--state-prepared-summary state)
     prepared-summary
     (niwa-compact-run--state-preserved-tail-turns state)
     preserved-tail-turns
     (niwa-compact-run--state-summary-ready state) summary-ready
     (niwa-compact-run--state-tail-text state)
     (and (not aggressive)
          (niwa-compact-evidence-region-with-tool-output-cap
           tail-start limit niwa-compact-evidence-tail-tool-output-max)))
    ;; Freeze completed evidence before the attempt replaces the transcript.
    ;; A failed attempt leaves only an unsealed checkpoint for later recovery.
    (when (and (niwa-compact-run--state-session state)
               (not (plist-get target :invocation)))
      (condition-case err
          (when-let* ((capture (niwa-journal-capture-checkpoint
                                (niwa-compact-run--state-session state)
                                (niwa-compact-run--state-chat-buffer state))))
            (setf (niwa-compact-run--state-journal-captures state) (list capture)))
        (error
         (niwa-diagnostic-warning
          (niwa-diagnostic-scope (niwa-compact-run--state-session state)) 'niwa
                          (format "%s (journal capture checkpoint failed)" (error-message-string err))
                          :warning))))
    state))

(cl-defun niwa-compact-run-start
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
  (require 'niwa-compact-estimation)
  (require 'niwa-compact-evidence)
  (require 'niwa-compact-target)
  (let* ((chat-buffer (current-buffer))
         (target (or target (niwa-compact-target-main-target))))
    ;; Refuse before anything is measured: an attempt that is never
    ;; admitted has no interval, and a telemetry start with no finish
    ;; makes the log claim a compaction that never ran.
    (when niwa-compact-run-in-flight
      (user-error "Compaction already in progress"))
    (when (bound-and-true-p niwa-session--read-only-mode)
      (user-error "Session is read-only"))
    (when (and (not pending-start)
               (niwa-compact-evidence-buffer-active-p chat-buffer))
      (user-error "Cannot compact while a request is active"))
    (unless (plist-get target :eligible-p)
      (user-error "Current buffer is not the active persisted segment"))
    (let* ((session (plist-get target :session))
           (tokens-before (niwa-compact-estimation-estimate-tokens))
           (target-policy (niwa-compact-estimation-target-policy))
           (telemetry-span
            (and session
                 (apply #'niwa-telemetry-start
                        session 'compaction
                        :trigger (if auto 'auto 'manual)
                        :aggressive (and aggressive t)
                        :tokens-before tokens-before
                        :target-origin (plist-get target :origin)
                        (niwa-compact-estimation-telemetry-inputs
                         tokens-before target-policy))))
           (state
            (niwa-compact-run--state-create
             :target-policy target-policy
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
      (setq niwa-compact-target-current-request-reminder nil
            niwa-compact-target-current-request-hook-context nil)
      (condition-case err
          (let ((limit (or pending-start
                           (niwa-compact-evidence-find-boundary))))
            (unless limit
              (if auto
                  (cl-return-from niwa-compact-run-start
                    (niwa-compact-run--finish state :skip))
                (user-error "Not enough conversation content to compact")))
            (niwa-compact-run--prepare
             state limit admission instructions pending-start
             prepared-summary summary-ready)
            (when (string-blank-p
                   (niwa-compact-run--state-old-content state))
              (if auto
                  (cl-return-from niwa-compact-run-start
                    (niwa-compact-run--finish
                     state
                     (if (plist-get admission :target-pressure)
                         "No compactable history remains at target pressure"
                       :skip)))
                (user-error "Not enough conversation content to compact")))
            (require 'niwa-hooks)
            (setq niwa-compact-run-in-flight t)
            (setq niwa-compact-run-cancel
                  (lambda ()
                    (unless (or (niwa-compact-run--state-settled state)
                                (niwa-compact-run--state-applied state))
                      (when-let* ((cancel
                                   (niwa-compact-run--state-request-cancel
                                    state)))
                        (funcall cancel))
                      (unless (or (niwa-compact-run--state-settled state)
                                  (niwa-compact-run--state-applied state))
                        (niwa-compact-run--fail
                         state "Compaction aborted" nil t)))))
            (niwa-compact-run--begin-attempt state))
        ;; The attempt never started, so the span is closed here rather
        ;; than through the settlement owner: settling would report an
        ;; outcome to a caller that is about to receive this signal.
        (error
         (niwa-compact-run--abandon-telemetry state)
         (signal (car err) (cdr err)))))))

(defun niwa-compact-run--abandon-telemetry (state)
  "Close STATE's telemetry span for an attempt that never began.
A span the settlement owner already consumed is gone by the time this
runs, so an attempt that settled before signalling is not re-finished."
  (when-let* ((span (niwa-compact-run--state-telemetry-span state)))
    (setf (niwa-compact-run--state-telemetry-span state) nil)
    (niwa-telemetry-finish span :outcome 'rejected)))

(provide 'niwa-compact-run)

;;; niwa-compact-run.el ends here
