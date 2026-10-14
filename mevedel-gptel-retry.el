;;; mevedel-gptel-retry.el --- Attempt-local gptel recovery -*- lexical-binding: t -*-

;;; Commentary:

;; Recover a failed native gptel sample without repeating turn preparation or
;; tools.  gptel still owns transport, parsing and successful transitions.
;; Request-local retry states park cleanup until an owned timer resends the
;; prepared payload.  Only admitted mevedel requests participate; ACP and
;; background requests retain their own recovery policies.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'gptel)
(require 'mevedel-agents)
(require 'mevedel-engine)
(require 'mevedel-structs)
(require 'mevedel-turn)
(require 'mevedel-transport)

;; `gptel'
(declare-function gptel--insert-response "ext:gptel" (response info &optional raw))
(declare-function gptel--openai-responses-update-tokens "ext:gptel-openai-responses" (usage info))
(declare-function gptel--stream-convert-markdown->org "ext:gptel-org" (start-marker))
(declare-function gptel-curl--stream-insert-response "ext:gptel" (response info &optional raw))
(defvar gptel-post-response-functions)
(defvar gptel-post-stream-hook)

;; `mevedel-agent-conversation'
(declare-function mevedel-agent-conversation-record-activity
                  "mevedel-agent-conversation" (invocation item &optional suppress-rerender))

;; `mevedel-gptel-stream-bridge'
(declare-function mevedel-gptel-stream-bridge-discard-batch
                  "mevedel-gptel-stream-bridge" (info))
(autoload 'mevedel-gptel-stream-bridge-discard-batch "mevedel-gptel-stream-bridge")

;; `mevedel-view'
(declare-function mevedel-view-rerender "mevedel-view" (&optional buffer))

;; `mevedel-view-render'
(declare-function mevedel-view-render-invalidate-live-tail "mevedel-view-render" ())

;; `mevedel-view-stream'
(declare-function mevedel-view--restore-spinner-status "mevedel-view-stream" (owner snapshot))
(declare-function mevedel-view--spinner-status-snapshot "mevedel-view-stream" ())
(declare-function mevedel-view--stop-spinner "mevedel-view-stream" ())
(declare-function mevedel-view--update-spinner "mevedel-view-stream" (status &optional owner))
(defvar mevedel-view--spinner-owner)

(defcustom mevedel-gptel-retry-max-retries 10
  "Maximum retries after the first attempt of an admitted native gptel sample.
Zero disables automatic sample recovery.  Successful tool-loop samples start
a fresh allowance.  Claude Code and background requests are unaffected."
  :type 'natnum :group 'mevedel)

(defvar mevedel-gptel-retry--states nil
  "Live sample owners, including requests parked without a transport.")
(defvar mevedel-gptel-retry--installed nil
  "Non-nil when the owning gptel adapters are installed.")
(defvar-local mevedel-gptel-retry--fsm nil
  "Native gptel machine whose cancellation belongs to this buffer.")
(defvar mevedel-gptel-retry--dispatch-state nil
  "Sample whose native dispatch is creating an Org stream converter.")
(defvar mevedel-gptel-retry--cleanup-header-fsm nil
  "Machine whose stream headers already performed cleanup's TYPE transition.")

(defun mevedel-gptel-retry--retryable-p (error-data status)
  "Classify ERROR-DATA and HTTP STATUS conservatively.
Permanent structured errors take precedence over transport status and text."
  (let* ((data (and (proper-list-p error-data) error-data))
         (code (mevedel-gptel-retry--json-error (plist-get data :code)))
         (type (mevedel-gptel-retry--json-error (plist-get data :type)))
         (message (format "%s" (if (stringp error-data) error-data
                                 (or (plist-get data :message) ""))))
         (codes (delete "error" (mapcar (lambda (value) (downcase (format "%s" value)))
                                       (delq nil (list code type))))))
    (cond
     ((or (member status '("400" "401" "402" "403" "404" "413"))
          (cl-intersection codes '("400" "401" "402" "403" "404" "413") :test #'equal)
          (string-match-p "\\bquota exhausted\\b\\|\\busage limit reached\\b\\|\\binsufficient quota\\b"
                          (downcase message))
          (cl-intersection
           codes '("invalid_request_error" "invalid_request" "invalid_prompt"
                   "authentication_error" "permission_error" "model_not_found"
                   "context_length_exceeded" "context_window_exceeded"
                   "insufficient_quota" "usage_limit_reached" "quota_exceeded"
                   "credit_balance_exhausted" "organization_spend_limit_exceeded"
                   "project_spend_limit_exceeded" "usage_not_included"
                   "content_filter" "max_output_tokens" "cancelled") :test #'equal)) nil)
     ((cl-intersection
       codes '("server_error" "internal_server_error" "overloaded_error"
               "server_is_overloaded" "model_at_capacity" "model_capacity_exceeded"
               "service_unavailable_error" "rate_limit_exceeded" "rate_limit_error" "slow_down"
               "stream_interrupted" "timeout_error") :test #'equal) t)
     ((or (cl-some (lambda (code)
                    (or (member code '("408" "429"))
                        (string-match-p "\\`5[0-9][0-9]\\'" code))) codes)
          (equal status "408") (equal status "429")
          (and (stringp status) (string-match-p "\\`5[0-9][0-9]\\'" status))) t)
     ((and (null codes)
           (string-match-p
            "\\bmodel\\(?: is\\)? at capacity\\b\\|\\btemporarily overloaded\\b\\|\\bserver is overloaded\\b"
            (downcase message))) t)
     ((string-match-p
       "\\`Curl failed with exit code \\(6\\|7\\|18\\|28\\|35\\|52\\|55\\|56\\|92\\)\\."
       message) t))))

(defun mevedel-gptel-retry--delay (attempt advice)
  "Return backoff for retry ATTEMPT, honoring numeric or date ADVICE."
  (let ((server
         (cond ((and (numberp advice) (>= advice 0)) advice)
               ((and (stringp advice)
                     (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" advice))
                (string-to-number advice))
               ((stringp advice)
                (condition-case nil
                    (max 0 (- (float-time (date-to-time advice)) (float-time)))
                  (error nil))))))
    (or server (* (min 30 (expt 2 (1- attempt)))
                  (+ 0.9 (/ (random 201) 1000.0))))))

(defun mevedel-gptel-retry--json-error (value)
  "Return VALUE unless it is JSON null or false.
Responses events carry an explicit `\"error\": null' on success."
  (unless (memq value '(nil :null :json-false)) value))

(defun mevedel-gptel-retry--live-p (state &optional info)
  "Return non-nil while STATE and optional attempt INFO own their request."
  (let* ((fsm (plist-get state :fsm))
         (current (gptel-fsm-info fsm))
         (buffer (plist-get current :buffer))
         (request (plist-get state :request))
         (invocation (plist-get state :invocation)))
    (and (not (plist-get state :cancelled))
         (not (plist-get state :closed))
         (eq state (plist-get current :mevedel-retry-state))
         (or (null info)
             (eq (plist-get info :mevedel-retry-attempt)
                 (plist-get current :mevedel-retry-attempt)))
         (buffer-live-p buffer)
         (eq request (buffer-local-value 'mevedel--current-request buffer))
         (not (mevedel-request-cancelled-p request))
         (or (null invocation)
             (and (not (mevedel-agent-invocation-runtime-settled-p invocation))
                  (eq invocation (buffer-local-value 'mevedel--agent-invocation buffer)))))))

(defun mevedel-gptel-retry--status (state text)
  "Display transient TEXT only while STATE owns its root or agent."
  (when (mevedel-gptel-retry--live-p state)
    (let* ((info (gptel-fsm-info (plist-get state :fsm)))
           (buffer (plist-get info :buffer))
           (invocation (plist-get state :invocation))
           (view (buffer-local-value 'mevedel--view-buffer buffer)))
      (if invocation
          (mevedel-agent-conversation-record-activity invocation (list :type 'status :summary text))
        (when (buffer-live-p view)
          (with-current-buffer view
            ;; Remember what the wait replaced, even no status, so recovery
            ;; can hand it back.
            (unless (plist-get state :progress)
              (plist-put state :progress (list (mevedel-view--spinner-status-snapshot))))
            (mevedel-view--update-spinner text 'provider-retry)))))))

(defun mevedel-gptel-retry--restore-status (state)
  "Return root progress claimed by STATE's retry wait to its prior status.
A status claimed since by another owner is left alone."
  (when-let* ((captured (plist-get state :progress)))
    (plist-put state :progress nil)
    (let* ((buffer (plist-get (gptel-fsm-info (plist-get state :fsm)) :buffer))
           (view (and (buffer-live-p buffer)
                      (buffer-local-value 'mevedel--view-buffer buffer))))
      (when (buffer-live-p view)
        (with-current-buffer view
          (if (car captured)
              (mevedel-view--restore-spinner-status 'provider-retry (car captured))
            (when (eq mevedel-view--spinner-owner 'provider-retry)
              (mevedel-view--stop-spinner))))))))

(defun mevedel-gptel-retry--cancel (state)
  "Cancel STATE's timer and request once, including a transport-free wait.
Once a response has handed the machine to tools, STATE has no provider work
left; release it so their aborted results still advance the machine, and mark
the machine so its next dispatch aborts instead of sampling again."
  (cond
   ((or (plist-get state :cancelled) (plist-get state :closed)))
   ((plist-get state :tools)
    (let ((info (gptel-fsm-info (plist-get state :fsm))))
      (when (eq state (plist-get info :mevedel-retry-state))
        (plist-put info :mevedel-retry-cancelled t)))
    (mevedel-gptel-retry--close state))
   (t
    (let* ((fsm (plist-get state :fsm))
           (current (eq state (plist-get (gptel-fsm-info fsm) :mevedel-retry-state))))
      (plist-put state :cancelled t)
      (plist-put state :data nil)
      (when-let* ((timer (plist-get state :timer))) (cancel-timer timer))
      (plist-put state :timer nil)
      (setq mevedel-gptel-retry--states (delq state mevedel-gptel-retry--states))
      (when (and current (memq (gptel-fsm-state fsm) '(WAIT TYPE MRETRY MRESEND)))
        (mevedel-gptel-stream-bridge-discard-batch (gptel-fsm-info fsm))
        ;; Buffer-scoped gptel-abort could kill a replacement request.
        (mevedel-gptel-retry--abort-transports fsm))))))

(defun mevedel-gptel-retry--abort-transports (fsm)
  "Retire exactly FSM's native transports, including late registrations."
  (unwind-protect
      (dolist (entry (copy-sequence gptel--request-alist))
        (when (eq fsm (cadr entry))
          ;; Native abort observers finish this physical call even though the
          ;; cancelled retry gate suppresses its underlying terminal callback.
          (when-let* ((callback (plist-get (gptel-fsm-info fsm) :callback)))
            (condition-case err
                (funcall callback 'abort (gptel-fsm-info fsm))
              (error (message "mevedel: cancellation callback failed: %s" (error-message-string err)))))
          (unwind-protect (funcall (cddr entry))
            (when (processp (car entry))
              (mevedel-transport-resume-process (car entry)))
            (setq gptel--request-alist (delq entry gptel--request-alist)))))
    (unless (eq (gptel-fsm-state fsm) 'ABRT)
      (gptel--fsm-transition fsm 'ABRT))))

(defun mevedel-gptel-retry--close (state)
  "Release STATE's sample snapshot without cancelling accepted completion."
  (plist-put state :closed t)
  (plist-put state :data nil)
  (setq mevedel-gptel-retry--states (delq state mevedel-gptel-retry--states)))

(defun mevedel-gptel-retry--sum-tokens (previous current)
  "Sum reported numeric fields of PREVIOUS and CURRENT without inventing usage."
  (let ((total (copy-sequence previous)))
    (cl-loop for (key value) on current by #'cddr
             when (numberp value)
             do (setq total (plist-put total key (+ (or (plist-get total key) 0) value))))
    total))

(defun mevedel-gptel-retry--totalize-usage (state info)
  "Add STATE's failed-attempt usage to terminal INFO exactly once."
  (unless (plist-get info :mevedel-retry-usage-totalized)
    (plist-put info :mevedel-retry-usage-totalized t)
    (plist-put info :mevedel-retry-usage-incomplete
               (or (plist-get state :usage-incomplete) (null (plist-get info :tokens))))
    (plist-put info :tokens
               (mevedel-gptel-retry--sum-tokens
                (plist-get state :reported-tokens) (plist-get info :tokens)))))

(defun mevedel-gptel-retry--buffer-killed ()
  "Cancel the retry owned by the dying data buffer."
  (when mevedel-gptel-retry--fsm
    (when-let* ((state (plist-get (gptel-fsm-info mevedel-gptel-retry--fsm)
                                :mevedel-retry-state)))
      (mevedel-gptel-retry--cancel state))))

(defun mevedel-gptel-retry--pending-p (info)
  "Accept INFO's transient sample failure before a terminal transition."
  (when-let* ((state (plist-get info :mevedel-retry-state))
              ((mevedel-gptel-retry--live-p state info))
              (error-data (or (mevedel-gptel-retry--json-error
                               (plist-get info :mevedel-retry-provider-error))
                              (mevedel-gptel-retry--json-error (plist-get info :error)))))
    ;; A later curl exit must not hide an earlier structured provider refusal.
    (plist-put info :error error-data)
    (if (not (mevedel-gptel-retry--retryable-p error-data (plist-get info :http-status)))
        (progn
          (plist-put state :pending nil)
          (plist-put info :mevedel-retry-exhausted nil)
          (plist-put info :mevedel-retry-permanent t)
          nil)
      (plist-put info :mevedel-retry-permanent nil)
      (or (plist-get state :pending)
          (progn
          (unless (plist-get info :mevedel-retry-recorded)
            (plist-put info :mevedel-retry-recorded t)
            (let ((diagnostic
                   (list :attempt (1+ (plist-get state :retries))
                         :http-status (plist-get info :http-status)
                         :type (when-let* ((type (and (listp error-data) (mevedel-gptel-retry--json-error (plist-get error-data :type)))))
                                 (truncate-string-to-width (format "%s" type) 128))
                         :code (when-let* ((code (and (listp error-data) (mevedel-gptel-retry--json-error (plist-get error-data :code)))))
                                 (truncate-string-to-width (format "%s" code) 128))
                         :message (truncate-string-to-width
                                   (format "%s" (if (stringp error-data) error-data
                                                  (or (plist-get error-data :message) "Provider request failed")))
                                   1000))))
              (plist-put state :errors (append (plist-get state :errors) (list diagnostic)))
              (plist-put info :mevedel-retry-errors (plist-get state :errors))))
          (if (< (plist-get state :retries) (plist-get state :limit))
              (plist-put state :pending t)
            (plist-put info :mevedel-retry-exhausted t)
            nil))))))

(defun mevedel-gptel-retry--callback (state info callback response callback-info &rest args)
  "Gate attempt INFO's RESPONSE before CALLBACK can settle or insert it.
STATE owns the logical sample; CALLBACK-INFO is gptel's callback context."
  (when (mevedel-gptel-retry--live-p state info)
    (let ((terminal (or (memq response '(t nil))
                       (and (stringp response)
                            (not (plist-get callback-info :stream))))))
      (plist-put callback-info :error
                 (mevedel-gptel-retry--json-error (plist-get callback-info :error)))
      (when-let* ((provider-error (plist-get callback-info :mevedel-retry-provider-error)))
        (plist-put callback-info :error provider-error))
      (unless (and (not (eq response 'abort))
                   (if (plist-get callback-info :error)
                       (mevedel-gptel-retry--pending-p callback-info)
                     (plist-get state :pending)))
        (when (and terminal (plist-get callback-info :error))
          (mevedel-gptel-retry--rollback state t))
        (when terminal (mevedel-gptel-retry--totalize-usage state callback-info))
        (when (mevedel-gptel-retry--live-p state info)
          (unwind-protect
              (progn
                (unless (plist-get callback-info :error)
                  (mevedel-gptel-retry--restore-status state))
                (when (and terminal
                           (or (plist-get callback-info :error)
                               (not (plist-get callback-info :tool-use))))
                  (plist-put state :terminal-callback t))
                (apply callback response callback-info args))
            (plist-put state :terminal-callback nil)))))))

(defun mevedel-gptel-retry--insert (original response info &optional raw)
  "Tag ORIGINAL's inserted RESPONSE as provisional output of attempt INFO."
  (let ((state (plist-get info :mevedel-retry-state)))
    (if (not state)
        (funcall original response info raw)
      (when (mevedel-gptel-retry--live-p state info)
       (if (not (stringp response))
           (funcall original response info raw)
        (let* ((buffer (plist-get info :buffer))
               (start (copy-marker (or (plist-get info :tracking-marker)
                                       (plist-get info :position)) nil))
               (placeholder (and (not (plist-get info :tracking-marker))
                                 (= start (with-current-buffer buffer (point-max)))))
               captured)
          (unwind-protect
              (with-current-buffer buffer
                (let ((gptel-post-stream-hook
                       (cons (lambda ()
                               ;; Capture before user hooks can append notices
                               ;; or move the native tracking marker.
                               (setq captured t)
                               (mevedel-gptel-retry--tag-output
                                info start (point)
                                (and placeholder (eq (char-after) ?\n))))
                             gptel-post-stream-hook)))
                  (prog1 (funcall original response info raw)
                    (unless captured
                      ;; Nonstream insertions record their end without a hook.
                      (mevedel-gptel-retry--tag-output
                       info start (marker-position (plist-get info :tracking-marker)))))))
            (set-marker start nil))))))))

(defun mevedel-gptel-retry--tag-output (info start end &optional placeholder)
  "Tag INFO's native insertion from START to END, including owned PLACEHOLDER."
  (let ((limit (+ end (if placeholder 1 0))))
    (when (< start limit)
      (with-silent-modifications
        (put-text-property start limit 'mevedel-retry-output (plist-get info :mevedel-retry-attempt))
        (dolist (boundary (delete-dups (list end limit)))
          (when (> boundary start)
            (let ((rear (get-text-property (1- boundary) 'rear-nonsticky)))
              (unless (eq rear t)
                (put-text-property (1- boundary) boundary 'rear-nonsticky
                                   (cons 'mevedel-retry-output rear))))))))))

(defun mevedel-gptel-retry--rollback (state &optional terminal)
  "Discard failed output and restore STATE's sample, retaining TERMINAL error."
  (let* ((fsm (plist-get state :fsm))
         (info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer)))
    (unless terminal
      (plist-put state :reported-tokens
                 (mevedel-gptel-retry--sum-tokens (plist-get state :reported-tokens)
                                                 (plist-get info :tokens)))
      (unless (plist-get info :tokens) (plist-put state :usage-incomplete t)))
    (mevedel-gptel-stream-bridge-discard-batch info)
    (with-current-buffer buffer
      (let ((inhibit-read-only t) (inhibit-modification-hooks t)
            (position (point-max)))
        (while (> position (point-min))
          (let ((start (previous-single-property-change
                        position 'mevedel-retry-output nil (point-min))))
            (when (eq (plist-get info :mevedel-retry-attempt)
                      (get-text-property (1- position) 'mevedel-retry-output))
              (delete-region start position))
            (setq position start))))
      (when-let* ((view mevedel--view-buffer) ((buffer-live-p view)))
        (with-current-buffer view
          (mevedel-view-render-invalidate-live-tail))
        (mevedel-view-rerender view)))
    (when-let* ((reset (plist-get info :mevedel-retry-reset-output)))
      (funcall reset))
    (when-let* ((dispose (plist-get state :converter-dispose)))
      (funcall dispose)
      (plist-put state :converter-dispose nil))
    (dolist (key '(:position :tracking-marker :reasoning-marker))
      (let ((old (plist-get state key)))
        (plist-put info key (and old (copy-marker old (marker-insertion-type old))))))
    (plist-put info :transformer nil)
    ;; gptel initializes its stream transformer together with the first UUID.
    (plist-put info :uuid nil)
    (plist-put info :data (copy-tree (plist-get state :data) t))
    (dolist (key (append '(:tool-use :tool-result :partial_json :response-items
                          :partial_text :partial_reasoning :reasoning-chunks :signature)
                        (unless terminal
                          '(:error :http-status :status :reasoning :reasoning-block :stop-reason :tokens
                            :mevedel-retry-terminal :mevedel-retry-recorded
                            :mevedel-retry-check-completion
                            :mevedel-retry-advice :mevedel-retry-provider-error
                            :mevedel-retry-usage-totalized :mevedel-retry-usage-incomplete))))
      (plist-put info key nil))))

(defun mevedel-gptel-retry--resend (state)
  "Resend STATE's prepared sample after the previous transport has unwound."
  (plist-put state :timer nil)
  (when (mevedel-gptel-retry--live-p state)
    (if (plist-get state :completing)
        (plist-put state :timer
                   (mevedel-transport-run-at-time 0.05 #'mevedel-gptel-retry--resend state))
      (condition-case err
          (progn
            (mevedel-gptel-retry--rollback state)
            ;; Rendering can yield to interruption or a replacement turn.
            (when (mevedel-gptel-retry--live-p state)
              (plist-put state :pending nil)
              (cl-incf (plist-get state :retries))
              (mevedel-gptel-retry--status state "Retrying provider request…")
              (when (mevedel-gptel-retry--live-p state)
                (gptel--fsm-transition (plist-get state :fsm) 'MRESEND))))
        (error
         (let* ((fsm (plist-get state :fsm)) (info (gptel-fsm-info fsm)))
           (plist-put state :pending nil)
           (plist-put info :mevedel-retry-permanent t)
           (plist-put info :error (list :type "retry_recovery_failed"
                                       :message (error-message-string err)))
           (gptel--fsm-transition fsm 'ERRS)))))))

(defun mevedel-gptel-retry--park (fsm)
  "Schedule FSM's accepted retry once; repeated cleanup transitions are inert."
  (let* ((info (gptel-fsm-info fsm))
         (state (plist-get info :mevedel-retry-state)))
    (when (and (plist-get state :pending) (not (plist-get state :timer))
               (mevedel-gptel-retry--live-p state info))
      (let* ((attempt (1+ (plist-get state :retries)))
             (delay (mevedel-gptel-retry--delay attempt (plist-get info :mevedel-retry-advice))))
        (mevedel-gptel-retry--status
         state (format "Provider temporarily unavailable · retry %d/%d in %.0fs"
                       attempt (plist-get state :limit) delay))
        (when (mevedel-gptel-retry--live-p state info)
          (plist-put state :timer
                     (mevedel-transport-run-at-time delay #'mevedel-gptel-retry--resend state)))))))

(defun mevedel-gptel-retry--send (fsm)
  "Dispatch one physical attempt of FSM through gptel's native helper."
  (let* ((old (gptel-fsm-info fsm))
         (state (plist-get old :mevedel-retry-state))
         (info (copy-sequence old))
         (callback (plist-get state :callback))
         (wrapper (apply-partially #'mevedel-gptel-retry--callback state info callback))
         (mevedel-gptel-retry--dispatch-state state))
    (when (mevedel-gptel-retry--live-p state old)
     (plist-put info :mevedel-retry-attempt (make-symbol "gptel-attempt"))
    (plist-put info :callback wrapper)
    (setf (gptel-fsm-info fsm) info)
    (gptel--handle-wait fsm)
    (dolist (entry gptel--request-alist)
      (when (and (eq fsm (cadr entry)) (processp (car entry)))
        (unless (process-get (car entry) 'mevedel-retry-info)
          (process-put (car entry) 'mevedel-retry-info (gptel-fsm-info fsm)))))
    (when (plist-get state :cancelled) (mevedel-gptel-retry--abort-transports fsm)))))

(defun mevedel-gptel-retry-dispatch (fsm)
  "Send FSM's logical sample, enabling bounded recovery for admitted requests.
A machine whose sample was cancelled in its tool phase, or whose agent
invocation has settled, aborts instead: its owner no longer accepts output."
  (unless mevedel-gptel-retry--installed (mevedel-gptel-retry-install))
  (let* ((info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer))
         (request (and (buffer-live-p buffer)
                       (buffer-local-value 'mevedel--current-request buffer))))
    (if (or (plist-get info :mevedel-retry-cancelled)
            ;; Tools can finish after interruption settled the invocation.
            (let ((invocation (plist-get info :mevedel-agent-invocation)))
              (and (mevedel-agent-invocation-p invocation)
                   (mevedel-agent-invocation-runtime-settled-p invocation))))
        (gptel--fsm-transition fsm 'ABRT)
      (if (not (and request (> mevedel-gptel-retry-max-retries 0)
                    (not (mevedel-engine-external-p (plist-get info :backend)))))
          (progn
            (when-let* ((previous (plist-get info :mevedel-retry-state)))
              (setq info (copy-sequence info))
              (setf (gptel-fsm-info fsm) info)
              (mevedel-gptel-retry--close previous)
              (plist-put info :callback (plist-get previous :callback))
              (plist-put info :mevedel-retry-state nil))
            (gptel--handle-wait fsm))
        (let* ((callback (if (eq (plist-get info :callback) (plist-get info :mevedel-usage-wrapper))
                             (plist-get info :mevedel-usage-original)
                           (plist-get info :callback)))
               (previous (plist-get info :mevedel-retry-state))
               (state (list :fsm fsm :request request :retries 0
                            :limit mevedel-gptel-retry-max-retries
                            :data (copy-tree (plist-get info :data) t)
                            :callback (if previous (plist-get previous :callback)
                                        (or callback (if (and gptel-use-curl (plist-get info :stream))
                                                         #'gptel-curl--stream-insert-response
                                                       #'gptel--insert-response)))
                            :invocation (buffer-local-value 'mevedel--agent-invocation buffer))))
          (when previous
            (plist-put previous :closed t)
            (plist-put previous :data nil)
            (setq mevedel-gptel-retry--states (delq previous mevedel-gptel-retry--states)))
          (dolist (key '(:position :tracking-marker :reasoning-marker))
            (when-let* ((marker (plist-get info key)))
              (plist-put state key (copy-marker marker (marker-insertion-type marker)))))
          (plist-put info :mevedel-retry-state state)
          (plist-put info :mevedel-retry-exhausted nil)
          (plist-put info :mevedel-retry-errors nil)
          (plist-put info :mevedel-retry-terminal nil)
          (plist-put info :mevedel-retry-check-completion nil)
          (plist-put info :mevedel-retry-advice nil)
          (plist-put info :mevedel-retry-provider-error nil)
          (plist-put info :mevedel-retry-permanent nil)
          (plist-put info :mevedel-retry-usage-totalized nil)
          (plist-put info :mevedel-retry-usage-incomplete nil)
          (plist-put info :mevedel-retry-recorded nil)
          (plist-put info :mevedel-retry-reset-output nil)
          (dolist (key '(:stop-reason :reasoning-block :partial_text :partial_reasoning :reasoning-chunks :signature))
            (plist-put info key nil))
          (let ((table (copy-tree (gptel-fsm-table fsm)))
                (handlers (copy-tree (gptel-fsm-handlers fsm))))
            (unless (assq 'MRETRY table)
              (push (cons #'mevedel-gptel-retry--pending-p 'MRETRY) (cdr (assq 'TYPE table)))
              (push '(MRETRY . ((t . MRETRY))) table)
              (push '(MRESEND . ((t . TYPE))) table)
              (push '(MRETRY mevedel-gptel-retry--park) handlers)
              (push '(MRESEND mevedel-gptel-retry--send) handlers))
            (setf (gptel-fsm-table fsm) table (gptel-fsm-handlers fsm) handlers))
          (push state mevedel-gptel-retry--states)
          (with-current-buffer buffer
            (setq mevedel-gptel-retry--fsm fsm)
            (add-hook 'kill-buffer-hook #'mevedel-gptel-retry--buffer-killed nil t))
          (when-let* ((invocation (plist-get state :invocation)))
            (setf (mevedel-agent-invocation-runtime-cancel invocation)
                  (lambda ()
                    (when-let* ((current (plist-get (gptel-fsm-info fsm) :mevedel-retry-state)))
                      (mevedel-gptel-retry--cancel current))
                    (unless (plist-get (gptel-fsm-info fsm) :mevedel-retry-state)
                      (mevedel-gptel-retry--abort-transports fsm))
                    nil)))
          (unless (plist-get info :mevedel-retry-canceller)
            (plist-put info :mevedel-retry-canceller t)
            (mevedel-request-push-canceller
             request (lambda ()
                       (when-let* ((current (plist-get (gptel-fsm-info fsm) :mevedel-retry-state)))
                         ;; Result publication ends its request inside an
                         ;; accepted callback, ahead of native DONE/ERRS.
                         ;; Explicit runtime interruption calls --cancel directly.
                         (if (and (plist-get current :terminal-callback)
                                  (not (plist-get current :cancelled)))
                             (mevedel-gptel-retry--close current)
                           (mevedel-gptel-retry--cancel current))))))
          (mevedel-gptel-retry--send fsm))))))

(defun mevedel-gptel-retry--parse-stream (original backend info)
  "Use ORIGINAL's native parsing, retaining stream errors and completion.
Inspect complete SSE or JSON-line envelopes, not provider text or tool data."
  (let ((start (point)) (state (plist-get info :mevedel-retry-state)))
    (if (not state)
        (funcall original backend info)
      (when (mevedel-gptel-retry--live-p state info)
        (let ((text (funcall original backend info)))
          ;; Some native parsers leave in-band errors unread.  Scan complete
          ;; envelopes independently, without advancing the native parse cursor.
          (save-excursion
            (goto-char start)
            (while (re-search-forward "^\\(?:event: *\\([^\r\n]+\\)\r?$\\|data: *\\|\\({\\)\\)" nil t)
              (let ((event (match-string 1))
                    (json-line (match-string 2)))
                (when event
                  (forward-line 1)
                  (when (looking-at "data: *") (goto-char (match-end 0))))
                (when json-line (backward-char))
                (if (looking-at "\\[DONE\\]")
                    (progn
                      (plist-put info :mevedel-retry-terminal t)
                      (forward-line 1))
                  (let* ((complete nil)
                         (data (condition-case nil
                                   (let ((parsed (gptel--json-read)))
                                     (setq complete t)
                                     (and (listp parsed) parsed))
                                 (error nil)))
                         (response (plist-get data :response))
                         (choices (plist-get data :choices))
                         (error-data (or (mevedel-gptel-retry--json-error (plist-get data :error))
                                         (mevedel-gptel-retry--json-error (plist-get response :error))
                                         (and (equal event "error")
                                              (not (plist-member data :error)) data))))
                    ;; Unknown native stream formats retain gptel's EOF policy.
                    ;; Only recognizable envelopes promise a completion marker.
                    (when (and complete
                               (or (plist-member data :choices)
                                   (plist-member data :done)
                                   (and event (string-match-p
                                               "\\`\\(?:response\\.\\|content_block_\\|message_\\)" event))))
                      (plist-put info :mevedel-retry-check-completion t))
                    (when (and complete
                               (or error-data
                                   (eq (plist-get data :done) t)
                                   ;; Compatible servers may close after this
                                   ;; chunk without the [DONE] sentinel.
                                   (and (vectorp choices)
                                        (seq-some (lambda (choice)
                                                    (and (listp choice)
                                                         (mevedel-gptel-retry--json-error
                                                          (plist-get choice :finish_reason))))
                                                  choices))
                                   (member event '("message_stop" "response.failed" "response.incomplete" "response.completed"))))
                      (plist-put info :mevedel-retry-terminal t)
                      ;; Native Responses parsing omits usage on these failures.
                      (when (member event '("response.failed" "response.incomplete"))
                        (gptel--openai-responses-update-tokens (plist-get response :usage) info))
                      (cond
                       (error-data (plist-put info :error error-data))
                       ((equal event "response.failed")
                        (plist-put info :error '(:code "server_error" :message "Response failed")))
                       ((equal event "response.incomplete")
                        (let ((reason (or (plist-get (plist-get response :incomplete_details) :reason) "unknown")))
                          (plist-put info :error
                                     (list :code (if (equal reason "interrupted") "stream_interrupted" reason)
                                           :message (format "Response incomplete: %s" reason))))))))))))
          (when (mevedel-gptel-retry--json-error (plist-get info :stop-reason))
            (plist-put info :mevedel-retry-terminal t))
          (plist-put info :error (mevedel-gptel-retry--json-error (plist-get info :error)))
          (when-let* ((error-data (plist-get info :error)))
            (plist-put info :mevedel-retry-provider-error error-data)
            (let ((headers (and (listp error-data) (plist-get error-data :headers))))
              (plist-put info :mevedel-retry-advice
                         (or (plist-get headers :retry-after)
                             (plist-get headers :Retry-After)
                             (and (listp error-data) (plist-get error-data :retry_after))))))
          text)))))

(defun mevedel-gptel-retry--converter (original marker)
  "Retain ORIGINAL's converter cleanup for MARKER on a failed attempt."
  (let ((before gptel-post-response-functions)
        (converter (funcall original marker)))
    (when mevedel-gptel-retry--dispatch-state
      (let ((hooks (cl-set-difference gptel-post-response-functions before)))
        (plist-put
         mevedel-gptel-retry--dispatch-state :converter-dispose
         (lambda ()
           (when (marker-buffer marker)
             (with-current-buffer (marker-buffer marker)
               (dolist (hook hooks)
                 (when (memq hook gptel-post-response-functions)
                   (funcall hook (marker-position marker) nil)))))))))
    converter))

(defun mevedel-gptel-retry--parse-response (original backend response info &rest args)
  "Normalize in-band errors from non-streaming RESPONSE before ORIGINAL."
  (when (plist-get info :mevedel-retry-state)
    ;; Gemini streams also enter this native generic with individual candidates.
    (when (and (listp response) (plist-member response :candidates))
      (plist-put info :mevedel-retry-check-completion t))
    (when-let* ((error-data (and (listp response)
                               (mevedel-gptel-retry--json-error (plist-get response :error)))))
      (plist-put info :error error-data)))
  (apply original backend response info args))

(defun mevedel-gptel-retry--parse-http (original &rest args)
  "Retain retry advice and normalized errors around ORIGINAL's HTTP parsing.
Both native curl and URL parsers receive request INFO as their last argument."
  (let ((info (car (last args))))
    (when (plist-get info :mevedel-retry-state)
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t))
          (when-let* ((end (re-search-forward "\r?\n\r?\n" nil t)))
            (goto-char (point-min))
            (when (re-search-forward "^retry-after: *\\([^\r\n]+\\)" end t)
              (plist-put info :mevedel-retry-advice (string-trim (match-string 1)))))))))
  (let* ((result (apply original args))
         (info (car (last args)))
         (error-data (and (plist-get info :mevedel-retry-state)
                          (mevedel-gptel-retry--json-error (plist-get info :error)))))
    ;; Native parsers return (TEXT HTTP-STATUS STATUS ERROR).  In-band errors
    ;; must reach that same boundary, including physical-call usage observers.
    (if error-data (append (seq-take result 3) (list error-data)) result)))

(defun mevedel-gptel-retry--filter (original process output)
  "Ignore OUTPUT from a retired attempt before ORIGINAL parses or inserts it."
  (let* ((info (process-get process 'mevedel-retry-info))
         (state (plist-get info :mevedel-retry-state)))
    (when (or (null state) (mevedel-gptel-retry--live-p state info))
      (funcall original process output))))

(defun mevedel-gptel-retry--transition (original fsm &optional new-state)
  "Gate cancelled transitions and release FSM's owner at terminal NEW-STATE.
ORIGINAL still owns native transitions and all their handlers."
  (if (and (null new-state) (eq fsm mevedel-gptel-retry--cleanup-header-fsm))
      ;; Nonzero stream cleanup unconditionally attempts WAIT -> TYPE before
      ;; its callback.  Headers already did that: keep TYPE until the callback
      ;; runs, then let cleanup's final transition settle or park the sample.
      (setq mevedel-gptel-retry--cleanup-header-fsm nil)
    (let ((state (plist-get (gptel-fsm-info fsm) :mevedel-retry-state)))
      (if (null state)
          (funcall original fsm new-state)
        (when (or (not (plist-get state :cancelled)) (eq new-state 'ABRT))
          (let* ((next (or new-state (gptel--fsm-next fsm)))
                 (terminal (memq next '(DONE ERRS ABRT))))
            (when (eq next 'TOOL)
              ;; Tools now own the machine; a recovered tool-only response may
              ;; deliver no callback to end the retry status.
              (plist-put state :tools t)
              (mevedel-gptel-retry--restore-status state))
            ;; Empty successful responses need no text callback.  Native terminal
            ;; handlers can still end the request without turning DONE into ABRT.
            (when terminal
              (mevedel-gptel-retry--totalize-usage state (gptel-fsm-info fsm))
              (plist-put state :terminal-callback t))
            (unwind-protect (funcall original fsm next)
              (when terminal
                (plist-put state :terminal-callback nil)
                (mevedel-gptel-retry--close state)))))))))

(defun mevedel-gptel-retry--cleanup (original process status)
  "Complete PROCESS via ORIGINAL, detecting truncated HTTP200 streams first."
  (let* ((fsm (car (alist-get process gptel--request-alist)))
         (info (or (process-get process 'mevedel-retry-info)
                   (and fsm (gptel-fsm-info fsm))))
         (state (plist-get info :mevedel-retry-state))
         (mevedel-gptel-retry--cleanup-header-fsm
          (and fsm (plist-get info :stream)
               (not (zerop (process-exit-status process)))
               (eq (gptel-fsm-state fsm) 'TYPE)
               ;; Callback ordering also matters when retries are disabled.
               ;; Leave unrelated native gptel requests unchanged.
               (or state
                   (when-let* ((buffer (plist-get info :buffer))
                               ((buffer-live-p buffer))
                               (request (buffer-local-value
                                         'mevedel--current-request buffer)))
                     (eq fsm (mevedel-request-fsm request))))
               fsm)))
    (if (not state)
        (funcall original process status)
      (when (and (not (memq (process-status process) '(stop run)))
                 (mevedel-gptel-retry--live-p state info))
        (plist-put state :completing t)
        (unwind-protect
            (progn
              (when (and (plist-get info :stream)
                         (plist-get info :mevedel-retry-check-completion)
                         (zerop (process-exit-status process))
                         (member (plist-get info :http-status) '("200" "100"))
                         (not (plist-get info :error))
                         (not (plist-get info :mevedel-retry-terminal)))
                (plist-put info :error '(:code "stream_interrupted" :message "Stream ended before response completion"))
                (when-let* ((call (plist-get info :mevedel-usage-call)))
                  (plist-put call :provider-error t)))
              (when (buffer-live-p (process-buffer process))
                (with-current-buffer (process-buffer process)
                  (save-excursion
                    (goto-char (point-min))
                    (let ((case-fold-search t))
                      (when-let* ((end (re-search-forward "\r?\n\r?\n" nil t)))
                        (goto-char (point-min))
                        (when (re-search-forward "^retry-after: *\\([^\r\n]+\\)" end t)
                          (plist-put info :mevedel-retry-advice (string-trim (match-string 1)))))))))
              (funcall original process status))
          ;; Ownership ends at a native terminal transition or the next WAIT,
          ;; not HTTP completion: asynchronous tool callbacks still need it.
          (plist-put state :completing nil))))))

(defun mevedel-gptel-retry-install ()
  "Install sparse gptel adapters; requests opt in at their dispatch boundary."
  (setq mevedel-gptel-retry--installed t)
  (dolist (pair '((gptel-curl--parse-stream . mevedel-gptel-retry--parse-stream)
                  (gptel-curl--parse-response . mevedel-gptel-retry--parse-http)
                  (gptel--url-parse-response . mevedel-gptel-retry--parse-http)
                  (gptel--fsm-transition . mevedel-gptel-retry--transition)
                  (gptel--stream-convert-markdown->org . mevedel-gptel-retry--converter)
                  (gptel--parse-response . mevedel-gptel-retry--parse-response)
                  (gptel-curl--stream-filter . mevedel-gptel-retry--filter)
                  (gptel-curl--stream-cleanup . mevedel-gptel-retry--cleanup)
                  (gptel-curl--sentinel . mevedel-gptel-retry--cleanup)
                  (gptel-curl--stream-insert-response . mevedel-gptel-retry--insert)
                  (gptel--insert-response . mevedel-gptel-retry--insert)))
    (unless (advice-member-p (cdr pair) (car pair))
      (advice-add (car pair) :around (cdr pair)
                  (list (cons 'depth (if (memq (car pair)
                                               '(gptel-curl--stream-cleanup gptel-curl--sentinel
                                                 gptel-curl--stream-filter))
                                          -90 100)))))))

(defun mevedel-gptel-retry-uninstall ()
  "Cancel owned recovery and remove gptel adapters."
  (setq mevedel-gptel-retry--installed nil)
  (dolist (state (copy-sequence mevedel-gptel-retry--states))
    (mevedel-gptel-retry--cancel state))
  (dolist (pair '((gptel-curl--parse-stream . mevedel-gptel-retry--parse-stream)
                  (gptel-curl--parse-response . mevedel-gptel-retry--parse-http)
                  (gptel--url-parse-response . mevedel-gptel-retry--parse-http)
                  (gptel--fsm-transition . mevedel-gptel-retry--transition)
                  (gptel--stream-convert-markdown->org . mevedel-gptel-retry--converter)
                  (gptel--parse-response . mevedel-gptel-retry--parse-response)
                  (gptel-curl--stream-filter . mevedel-gptel-retry--filter)
                  (gptel-curl--stream-cleanup . mevedel-gptel-retry--cleanup)
                  (gptel-curl--sentinel . mevedel-gptel-retry--cleanup)
                  (gptel-curl--stream-insert-response . mevedel-gptel-retry--insert)
                  (gptel--insert-response . mevedel-gptel-retry--insert)))
    (advice-remove (car pair) (cdr pair))))

(provide 'mevedel-gptel-retry)
;;; mevedel-gptel-retry.el ends here
