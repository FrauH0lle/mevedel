;;; niwa-gptel-retry.el --- Attempt-local gptel recovery -*- lexical-binding: t -*-

;;; Commentary:

;; Recover a failed native gptel sample without repeating turn preparation or
;; tools.  gptel still owns transport, parsing and successful transitions.
;; Request-local retry states park cleanup until an owned timer resends the
;; prepared payload.  Only admitted niwa requests and retained agent turns,
;; which their runtime owns without a root request, participate; ACP and
;; background requests retain their own recovery policies.
;; Explicit root retry uses this replay, or forks ACP history at the answer
;; before the failed prompt and replays that input without undoing tool effects.

;;; Code:

(require 'niwa-utilities)

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'gptel)
(require 'niwa-agents)
(require 'niwa-engine)
(require 'niwa-reminders)
(require 'niwa-structs)
(require 'niwa-turn)
(require 'niwa-transport)

;; `gptel'
(declare-function gptel--insert-response "ext:gptel" (response info &optional raw))
(declare-function gptel--openai-responses-update-tokens "ext:gptel-openai-responses" (usage info))
(declare-function gptel--stream-convert-markdown->org "ext:gptel-org" (start-marker))
(declare-function gptel-curl--stream-insert-response "ext:gptel" (response info &optional raw))
(declare-function gptel-gemini-p "ext:gptel-gemini" (object))
(defvar gptel-post-response-functions)
(defvar gptel-post-stream-hook)

;; `niwa-agent-conversation'
(declare-function niwa-agent-conversation-record-activity
                  "niwa-agent-conversation" (invocation item &optional suppress-rerender))

;; `niwa-claude-code-history'
(declare-function niwa-claude-code-history-unresumable-p "niwa-claude-code-history" (record))
(autoload 'niwa-claude-code-history-unresumable-p "niwa-claude-code-history")

;; `niwa-claude-code-fork'
(declare-function niwa-claude-code-fork-histories
                  "niwa-claude-code-fork" (histories complete &optional session))
(autoload 'niwa-claude-code-fork-histories "niwa-claude-code-fork")

;; `niwa-claude-code-session'
(declare-function niwa-claude-code-send
                  "niwa-claude-code-session" (&optional model-input model-policy prepared-input))
(autoload 'niwa-claude-code-send "niwa-claude-code-session")
(declare-function niwa-claude-code-reset-root-delivery "niwa-claude-code-session" (session))
(autoload 'niwa-claude-code-reset-root-delivery "niwa-claude-code-session")

;; `niwa-goal'
(declare-function niwa-goal-capture-request "niwa-goal" (fsm))

;; `niwa-gptel-stream-bridge'
(declare-function niwa-gptel-stream-bridge-discard-batch
                  "niwa-gptel-stream-bridge" (info))
(autoload 'niwa-gptel-stream-bridge-discard-batch "niwa-gptel-stream-bridge")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-content-start "niwa-session-artifacts" (buffer))
(autoload 'niwa-session-artifacts-content-start "niwa-session-artifacts")
(declare-function niwa-session-artifacts-save "niwa-session-artifacts" (session &optional buffer require-commit))
(autoload 'niwa-session-artifacts-save "niwa-session-artifacts")

;; `niwa-view'
(declare-function niwa-view-rerender "niwa-view" (&optional buffer))

;; `niwa-view-render'
(declare-function niwa-view-render-invalidate-live-tail "niwa-view-render" ())
(declare-function niwa-view-render-refresh-latest-turn "niwa-view-render" (data-buf))
(autoload 'niwa-view-render-refresh-latest-turn "niwa-view-render")

;; `niwa-view-stream'
(declare-function niwa-view-stream-claim-progress "niwa-view-stream" (view status owner))
(declare-function niwa-view-stream-ensure-progress-for-fsm "niwa-view-stream" (fsm))
(declare-function niwa-view-stream-release-progress "niwa-view-stream" (view token))
(declare-function niwa-view-stream-show-progress "niwa-view-stream" (view status &optional owner))
(autoload 'niwa-view-stream-claim-progress "niwa-view-stream")
(autoload 'niwa-view-stream-ensure-progress-for-fsm "niwa-view-stream")
(autoload 'niwa-view-stream-release-progress "niwa-view-stream")
(autoload 'niwa-view-stream-show-progress "niwa-view-stream")

(defcustom niwa-gptel-retry-max-retries 10
  "Maximum retries after the first attempt of an admitted native gptel sample.
Zero disables automatic sample recovery.  Successful tool-loop samples start
a fresh allowance.  Claude Code and background requests are unaffected."
  :type 'natnum :group 'niwa)

(defvar niwa-gptel-retry--states nil
  "Live sample owners, including requests parked without a transport.")
(defvar niwa-gptel-retry--installed nil
  "Non-nil when the owning gptel adapters are installed.")
(defvar-local niwa-gptel-retry--fsm nil
  "Native gptel machine whose cancellation belongs to this buffer.")
(defvar niwa-gptel-retry--dispatch-state nil
  "Sample whose native dispatch is creating an Org stream converter.")
(defvar niwa-gptel-retry--cleanup-header-fsm nil
  "Machine whose stream headers already performed cleanup's TYPE transition.")
(defvar-local niwa-gptel-retry--failure nil
  "Prepared sample or external input retained for an explicit failed-turn retry.")

(defun niwa-gptel-retry--source-hash ()
  "Hash canonical transcript content, excluding the mutable metadata drawer."
  (secure-hash 'sha256 (current-buffer)
               (niwa-session-artifacts-content-start (current-buffer)) (point-max)))

(defun niwa-gptel-retry--same-source-p (failure)
  "Return whether FAILURE still names this buffer's unchanged transcript."
  (or (eql (plist-get failure :tick) (buffer-chars-modified-tick))
      (when (equal (plist-get failure :source-hash) (niwa-gptel-retry--source-hash))
        (plist-put failure :tick (buffer-chars-modified-tick)))))

(defun niwa-gptel-retry-record-failure (fsm)
  "Finalize FSM's replay identity after its historical disclosure is inserted."
  (when-let* ((info (niwa-engine-info fsm))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (and (niwa-request-p fsm)
                 (not niwa--agent-invocation)
                 (plist-get info :niwa-retry-input)
                 (plist-get (plist-get info :niwa-claude-history) :id))
        (setq niwa-gptel-retry--failure
              (list :original fsm :request fsm :external-history t
                    :history-id (plist-get (plist-get info :niwa-claude-history) :id)
                    :before-history (copy-tree (plist-get info :niwa-native-before-history))
                    :output-start (marker-position (plist-get info :position))
                    :prompt (plist-get info :niwa-retry-prompt)
                    :transcript-input (plist-get info :niwa-retry-transcript-input)
                    :input (plist-get info :niwa-retry-input)
                    :model-policy (plist-get info :niwa-retry-model-policy))))
      (when (eq fsm (plist-get niwa-gptel-retry--failure :original))
        (plist-put niwa-gptel-retry--failure :id
                   (plist-get info :niwa-request-id))
        (plist-put niwa-gptel-retry--failure :turn
                   (niwa-session-turn-count niwa--session))
        (plist-put niwa-gptel-retry--failure :source-hash (niwa-gptel-retry--source-hash))
        (plist-put niwa-gptel-retry--failure :tick (buffer-chars-modified-tick))))))

(defun niwa-gptel-retry--retain-failure (state)
  "Retain STATE's prepared sample before terminal handlers release its owner."
  (when-let* ((request (plist-get state :request))
              ((not (or (plist-get state :invocation) (plist-get state :tools))))
              (fsm (plist-get state :fsm))
              (buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq niwa-gptel-retry--failure
            (list :original fsm :request request :fsm (gptel-copy-fsm fsm)
                  :state (copy-sequence state))))))

(defun niwa-gptel-retry-available-p (buffer &optional failure-id)
  "Return BUFFER's retryable failure ID, or nil.
When FAILURE-ID is supplied, require that exact historical request identity.
Replay is live-process only, and expires on transcript edits or another turn."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((id (plist-get niwa-gptel-retry--failure :id))
            (request (plist-get niwa-gptel-retry--failure :request)))
        (and id request niwa--session
             (or (null failure-id) (equal failure-id id))
             (null niwa--current-request)
             (null niwa--turn-settlements-pending)
             (eq niwa--session (niwa-request-session request))
             (or (not (plist-get niwa-gptel-retry--failure :external-history))
                 (let ((history (alist-get "root" (niwa-session-external-conversations niwa--session)
                                           nil nil #'equal)))
                   (and (equal (plist-get niwa-gptel-retry--failure :history-id) (plist-get history :id))
                        (not (niwa-claude-code-history-unresumable-p history)))))
             (equal (plist-get niwa-gptel-retry--failure :turn)
                    (niwa-session-turn-count niwa--session))
             (not (niwa-session-pending-input-p niwa--session))
             (niwa-gptel-retry--same-source-p niwa-gptel-retry--failure)
             id)))))

(defun niwa-gptel-retry-refresh-availability (buffer)
  "Refresh BUFFER's latest failure header when replay availability changes."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and niwa-gptel-retry--failure niwa--session
                 (null niwa--current-request) (null niwa--turn-settlements-pending)
                 (equal (plist-get niwa-gptel-retry--failure :turn)
                        (niwa-session-turn-count niwa--session)))
        (let ((available (and (niwa-gptel-retry-available-p buffer) t)))
          (unless (eq available (plist-get niwa-gptel-retry--failure :rendered-available))
            (plist-put niwa-gptel-retry--failure :rendered-available available)
            (when (buffer-live-p niwa--view-buffer)
              (with-current-buffer niwa--view-buffer
                (niwa-view-render-refresh-latest-turn buffer)))))))))

(defun niwa-gptel-retry--discard-external-output (start)
  "Remove failed prose and reasoning after START, retaining tools and errors."
  (let ((position (point-max)) (inhibit-read-only t) (inhibit-modification-hooks t))
    (while (> position start)
      (let* ((begin (previous-single-property-change position 'gptel nil start))
             (type (get-text-property (1- position) 'gptel)))
        (when (memq (if (consp type) (car type) type) '(response reasoning))
          (delete-region begin position))
        (setq position begin)))))

(defun niwa-gptel-retry--external (buffer failure request failure-id)
  "Prepare FAILURE's native boundary, then replay it under admitted REQUEST.
BUFFER owns the callback; FAILURE-ID preserves the same logical turn."
  (let* ((session niwa--session)
         (previous (copy-tree (niwa-session-external-conversations session)))
         (before (copy-tree (plist-get failure :before-history)))
         (legacy (and (plist-get before :id)
                      (not (plist-get (car (last (plist-get before :boundaries))) :message-id))))
         (hashes (niwa-session-workspace-instruction-hashes session))
         (mentions (niwa-session-mentions-shown session))
         (reminders (niwa-session-reminders session))
         (pending (niwa-session-pending-reminders session))
         (plan (cl-find 'plan-reference reminders :key #'niwa-reminder-type))
         (fired (and plan (niwa-reminder-last-fired plan)))
         installed committed)
    (let ((complete
     (lambda (histories diagnosis cleanup)
       (condition-case err
           (progn
             (when diagnosis (error "%s" diagnosis))
             (unless (buffer-live-p buffer) (error "Retry buffer closed during native preparation"))
             (with-current-buffer buffer
               (unless (and (eq request niwa--current-request)
                            (not (niwa-request-cancelled-p request))
                            (equal previous (niwa-session-external-conversations session))
                            (equal (plist-get failure :turn) (niwa-session-turn-count session))
                            (not (niwa-session-pending-input-p session))
                            (niwa-gptel-retry--same-source-p failure))
                 (user-error "The failed turn changed during native Retry preparation"))
               (let ((history (or (cdar histories) (list :engine 'claude-code :state 'unstarted))))
                 (when (plist-get history :id) (setq history (plist-put history :state 'uncertain)))
                 (setf (alist-get "root" (niwa-session-external-conversations session) nil nil #'equal)
                       history)
                 (setq installed history))
               ;; The replacement identity must never be published with receipts
               ;; from the failed branch, including across an editor crash.
               (niwa-claude-code-reset-root-delivery session)
               (niwa-session-enqueue-pending-reminder
                session (concat niwa-reminders-reconciliation-body
                                (if legacy
                                    " This Retry reuses the same native session because its older history has no recorded answer boundary; partial native output may remain."
                                  " This is a Retry from before a failed prompt.")
                                " Its tool effects remain; never replay a prior tool call merely because its result is missing."))
               (niwa-session-ensure-reminder session (niwa-reminders-make-pending-events))
               (let ((head (plist-get (niwa-session-publication session) :head)))
                 (condition-case publish-error
                     (progn
                       (unless (niwa-session-artifacts-save session buffer t)
                         (error "Native Retry identity did not publish"))
                       (setq committed t))
                   (error
                    (setq committed (not (equal head (plist-get (niwa-session-publication session) :head))))
                    (signal (car publish-error) (cdr publish-error)))))
               (niwa-gptel-retry--discard-external-output (plist-get failure :output-start))
               (setf (niwa-engine-info request)
                     (plist-put (niwa-engine-info request) :niwa-retry-of failure-id))
               (goto-char (point-max))
               (niwa-claude-code-send (plist-get failure :prompt)
                                        (plist-get failure :model-policy)
                                        (copy-tree (plist-get failure :input) t))))
         ((error quit)
          (unless committed (funcall cleanup))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (and (not committed) installed
                         (eq installed (alist-get "root" (niwa-session-external-conversations session) nil nil #'equal)))
                (setf (alist-get "root" (niwa-session-external-conversations session) nil nil #'equal)
                      (copy-tree (cdr (assoc "root" previous)))
                      (niwa-session-workspace-instruction-hashes session) hashes
                      (niwa-session-mentions-shown session) mentions
                      (niwa-session-reminders session) reminders
                      (niwa-session-pending-reminders session) pending)
                (when plan (setf (niwa-reminder-last-fired plan) fired)))
              (when (and (not committed) (eq request niwa--current-request))
                (setq niwa-gptel-retry--failure failure))
              (when (eq request niwa--current-request) (niwa-request-end))
              (when (buffer-live-p niwa--view-buffer) (niwa-view-rerender niwa--view-buffer))))
          (niwa-diagnostic-message (niwa-diagnostic-scope session)
                                      "Retry failed: %s" (error-message-string err)))))))
      (if legacy
          (progn
            (message "Retry reuses the retained native session; no recorded answer boundary is available")
            (funcall complete (list (cons "root" before)) nil #'ignore))
        (niwa-claude-code-fork-histories
         (when before (list (cons "root" before))) complete session)))))

(defun niwa-gptel-retry-failed-turn (buffer failure-id)
  "Retry BUFFER's failed request identified by FAILURE-ID.
Native requests replay their prepared sample without completed tools or failed
output.  External requests fork at the answer before the failed prompt and
replay that prompt.  File effects, tool records and error disclosures remain.
Admission rechecks ownership and recovery before consuming the retry."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and niwa--session (niwa-session-pending-input-p niwa--session))
        (user-error "Resolve queued input first before retrying the failed turn"))))
  (unless (and failure-id (niwa-gptel-retry-available-p buffer failure-id))
    (user-error "Retry unavailable: the retained request, transcript or turn changed"))
  (with-current-buffer buffer
    (let* ((failure niwa-gptel-retry--failure)
           (old-request (plist-get failure :request))
           (fsm (when (plist-get failure :fsm) (gptel-copy-fsm (plist-get failure :fsm))))
           (state (copy-sequence (plist-get failure :state)))
           ;; Terminal flags, observers and callback closures belong to the old
           ;; admission, not the new turn.  Keep only native request context.
           (info (cl-loop for (key value) on (and fsm (gptel-fsm-info fsm)) by #'cddr
                          unless (string-prefix-p ":niwa-" (symbol-name key))
                          append (list key value)))
           (niwa--pending-implementation-policy
            (niwa-request-implementation-policy old-request))
           request)
      (condition-case err
          (progn
            (niwa-request-begin
             niwa--session
             (list :permission-mode (niwa-request-permission-mode-override old-request)
                   :plan-read-only (niwa-request-plan-read-only old-request)
                   :admitted
                   (lambda (admitted)
                     (setq request admitted)
                     (when (niwa-session-pending-input-p niwa--session)
                       (user-error "Resolve queued input first before retrying the failed turn"))
                     (setf (niwa-request-turn request) (niwa-request-turn old-request)
                             (niwa-request-file-snapshots request)
                             (copy-hash-table (niwa-request-file-snapshots old-request))
                             (niwa-request-untracked-effects request)
                             (copy-tree (niwa-request-untracked-effects old-request))))))
            (unless (and (eq request niwa--current-request)
                         (eq failure niwa-gptel-retry--failure)
                         (equal (plist-get failure :turn) (niwa-session-turn-count niwa--session))
                         (not (niwa-session-pending-input-p niwa--session))
                         (niwa-gptel-retry--same-source-p failure))
              (user-error "Retained request, transcript, queued input or turn changed during retry admission"))
            (setq niwa-gptel-retry--failure nil)
            (setf (niwa-request-fsm request) fsm
                  (niwa-request-model-policy request) (niwa-request-model-policy old-request)
                  (niwa-request-skill-permission-rules request) (niwa-request-skill-permission-rules old-request)
                  (niwa-request-ptc-primitives request) (niwa-request-ptc-primitives old-request)
                  (niwa-request-hook-rules request) (niwa-request-hook-rules old-request)
                  (niwa-request-one-shot-mutations-p request) (niwa-request-one-shot-mutations-p old-request))
            (if (plist-get failure :external-history)
                (niwa-gptel-retry--external buffer failure request failure-id)
            (setf (gptel-fsm-info fsm) info
                  (gptel-fsm-state fsm) 'MRESEND)
            (plist-put state :fsm fsm)
            (plist-put state :request request)
            (plist-put state :closed nil)
            (plist-put state :position (copy-marker (point-max)))
            (plist-put state :tracking-marker nil)
            (plist-put state :reasoning-marker nil)
            (plist-put info :niwa-retry-state state)
            (plist-put info :niwa-retry-attempt
                       (plist-get (gptel-fsm-info (plist-get failure :fsm)) :niwa-retry-attempt))
            (plist-put info :callback (plist-get state :callback))
            (plist-put info :niwa-request request)
            (plist-put info :niwa-request-id (niwa-request-id request))
            (plist-put info :niwa-request-begun t)
            (plist-put info :niwa-retry-of failure-id)
            (plist-put info :tokens-full nil)
            (niwa-gptel-retry--rollback state)
            (niwa-goal-capture-request fsm)
            (niwa-view-stream-ensure-progress-for-fsm fsm)
            ;; This is the same replay boundary as automatic recovery: no WAIT
            ;; preparation, no new prompt, and no previously completed tools.
            (niwa-gptel-retry-dispatch fsm))
            (when (buffer-live-p niwa--view-buffer)
              (niwa-view-rerender niwa--view-buffer))
            (niwa-request-id request))
        ((error quit)
         (when (eq request niwa--current-request) (niwa-request-end))
         (signal (car err) (cdr err)))))))

(defun niwa-gptel-retry--retryable-p (error-data status)
  "Classify ERROR-DATA and HTTP STATUS conservatively.
Permanent structured errors take precedence over transport status and text."
  (let* ((data (and (proper-list-p error-data) error-data))
         (code (niwa-gptel-retry--json-error (plist-get data :code)))
         (type (niwa-gptel-retry--json-error (plist-get data :type)))
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

(defun niwa-gptel-retry--delay (attempt advice)
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

(defun niwa-gptel-retry--json-error (value)
  "Return VALUE unless it is JSON null or false.
Responses events carry an explicit `\"error\": null' on success."
  (unless (memq value '(nil :null :json-false)) value))

(defun niwa-gptel-retry--owner-p (fsm request invocation)
  "Return non-nil when root REQUEST or retained agent INVOCATION owns FSM.
Retained agent turns have no root request: their runtime owns the machine."
  (or request
      (and (niwa-agent-invocation-p invocation)
           (eq fsm (niwa-agent-invocation-runtime-fsm invocation)))))

(defun niwa-gptel-retry--live-p (state &optional info)
  "Return non-nil while STATE and optional attempt INFO own their request."
  (let* ((fsm (plist-get state :fsm))
         (current (gptel-fsm-info fsm))
         (buffer (plist-get current :buffer))
         (request (plist-get state :request))
         (invocation (plist-get state :invocation)))
    (and (not (plist-get state :cancelled))
         (not (plist-get state :closed))
         (eq state (plist-get current :niwa-retry-state))
         (or (null info)
             (eq (plist-get info :niwa-retry-attempt)
                 (plist-get current :niwa-retry-attempt)))
         (buffer-live-p buffer)
         (eq request (buffer-local-value 'niwa--current-request buffer))
         (niwa-gptel-retry--owner-p fsm request invocation)
         (not (and request (niwa-request-cancelled-p request)))
         (or (null invocation)
             (and (not (niwa-agent-invocation-runtime-settled-p invocation))
                  (eq invocation (buffer-local-value 'niwa--agent-invocation buffer)))))))

(defun niwa-gptel-retry--status (state text)
  "Display transient TEXT only while STATE owns its root or agent."
  (when (niwa-gptel-retry--live-p state)
    (let* ((info (gptel-fsm-info (plist-get state :fsm)))
           (buffer (plist-get info :buffer))
           (invocation (plist-get state :invocation))
           (view (buffer-local-value 'niwa--view-buffer buffer)))
      (if invocation
          (niwa-agent-conversation-record-activity invocation (list :type 'status :summary text))
        (when (buffer-live-p view)
          ;; Remember what the wait replaced, even no status, so recovery
          ;; can hand it back.
          (if (plist-get state :progress)
              (niwa-view-stream-show-progress view text 'provider-retry)
            (plist-put state :progress
                       (list (niwa-view-stream-claim-progress
                              view text 'provider-retry)))))))))

(defun niwa-gptel-retry--restore-status (state)
  "Return root progress claimed by STATE's retry wait to its prior status.
A status claimed since by another owner is left alone."
  (when-let* ((captured (plist-get state :progress)))
    (plist-put state :progress nil)
    (let* ((buffer (plist-get (gptel-fsm-info (plist-get state :fsm)) :buffer))
           (view (and (buffer-live-p buffer)
                      (buffer-local-value 'niwa--view-buffer buffer))))
      (niwa-view-stream-release-progress view (car captured)))))

(defun niwa-gptel-retry--cancel (state)
  "Cancel STATE's timer and request once, including a transport-free wait.
Once a response has handed the machine to tools, STATE has no provider work
left; release it so their aborted results still advance the machine, and mark
the machine so its next dispatch aborts instead of sampling again."
  (cond
   ((or (plist-get state :cancelled) (plist-get state :closed)))
   ((plist-get state :tools)
    (let ((info (gptel-fsm-info (plist-get state :fsm))))
      (when (eq state (plist-get info :niwa-retry-state))
        (plist-put info :niwa-retry-cancelled t)))
    (niwa-gptel-retry--close state))
   (t
    (let* ((fsm (plist-get state :fsm))
           (current (eq state (plist-get (gptel-fsm-info fsm) :niwa-retry-state))))
      (plist-put state :cancelled t)
      (plist-put state :data nil)
      (when-let* ((timer (plist-get state :timer))) (cancel-timer timer))
      (plist-put state :timer nil)
      (setq niwa-gptel-retry--states (delq state niwa-gptel-retry--states))
      (when (and current (memq (gptel-fsm-state fsm) '(WAIT TYPE MRETRY MRESEND)))
        (niwa-gptel-stream-bridge-discard-batch (gptel-fsm-info fsm))
        ;; Buffer-scoped gptel-abort could kill a replacement request.
        (niwa-gptel-retry--abort-transports fsm))))))

(defun niwa-gptel-retry--abort-transports (fsm)
  "Retire exactly FSM's native transports, including late registrations."
  (unwind-protect
      (dolist (entry (copy-sequence gptel--request-alist))
        (when (eq fsm (cadr entry))
          ;; Native abort observers finish this physical call even though the
          ;; cancelled retry gate suppresses its underlying terminal callback.
          (when-let* ((callback (plist-get (gptel-fsm-info fsm) :callback)))
            (condition-case err
                (funcall callback 'abort (gptel-fsm-info fsm))
              (error
               (niwa-diagnostic-message
                (or (plist-get (plist-get (gptel-fsm-info fsm) :niwa-retry-state) :scope)
                    (niwa-diagnostic-scope (plist-get (gptel-fsm-info fsm) :buffer)))
                "Cancellation callback failed: %s" (error-message-string err)))))
          (unwind-protect (funcall (cddr entry))
            (when (processp (car entry))
              (niwa-transport-resume-process (car entry)))
            (setq gptel--request-alist (delq entry gptel--request-alist)))))
    (unless (eq (gptel-fsm-state fsm) 'ABRT)
      (gptel--fsm-transition fsm 'ABRT))))

(defun niwa-gptel-retry--close (state)
  "Release STATE's sample snapshot without cancelling accepted completion."
  (plist-put state :closed t)
  (plist-put state :data nil)
  (setq niwa-gptel-retry--states (delq state niwa-gptel-retry--states)))

(defun niwa-gptel-retry--sum-tokens (previous current)
  "Sum reported numeric fields of PREVIOUS and CURRENT without inventing usage."
  (let ((total (copy-sequence previous)))
    (cl-loop for (key value) on current by #'cddr
             when (numberp value)
             do (setq total (plist-put total key (+ (or (plist-get total key) 0) value))))
    total))

(defun niwa-gptel-retry--totalize-usage (state info)
  "Add STATE's failed-attempt usage to terminal INFO exactly once."
  (unless (plist-get info :niwa-retry-usage-totalized)
    (plist-put info :niwa-retry-usage-totalized t)
    (plist-put info :niwa-retry-usage-incomplete
               (or (plist-get state :usage-incomplete) (null (plist-get info :tokens))))
    (plist-put info :tokens
               (niwa-gptel-retry--sum-tokens
                (plist-get state :reported-tokens) (plist-get info :tokens)))))

(defun niwa-gptel-retry--buffer-killed ()
  "Cancel the retry owned by the dying data buffer."
  (when niwa-gptel-retry--fsm
    (when-let* ((state (plist-get (gptel-fsm-info niwa-gptel-retry--fsm)
                                :niwa-retry-state)))
      (niwa-gptel-retry--cancel state))))

(defun niwa-gptel-retry--pending-p (info)
  "Accept INFO's transient sample failure before a terminal transition."
  (when-let* ((state (plist-get info :niwa-retry-state))
              ((niwa-gptel-retry--live-p state info))
              (error-data (or (niwa-gptel-retry--json-error
                               (plist-get info :niwa-retry-provider-error))
                              (niwa-gptel-retry--json-error (plist-get info :error)))))
    ;; A later curl exit must not hide an earlier structured provider refusal.
    (plist-put info :error error-data)
    (if (not (niwa-gptel-retry--retryable-p error-data (plist-get info :http-status)))
        (progn
          (plist-put state :pending nil)
          (plist-put info :niwa-retry-exhausted nil)
          (plist-put info :niwa-retry-permanent t)
          nil)
      (plist-put info :niwa-retry-permanent nil)
      (or (plist-get state :pending)
          (progn
          (unless (plist-get info :niwa-retry-recorded)
            (plist-put info :niwa-retry-recorded t)
            (let ((diagnostic
                   (list :attempt (1+ (plist-get state :retries))
                         :http-status (plist-get info :http-status)
                         :type (when-let* ((type (and (listp error-data) (niwa-gptel-retry--json-error (plist-get error-data :type)))))
                                 (truncate-string-to-width (format "%s" type) 128))
                         :code (when-let* ((code (and (listp error-data) (niwa-gptel-retry--json-error (plist-get error-data :code)))))
                                 (truncate-string-to-width (format "%s" code) 128))
                         :message (truncate-string-to-width
                                   (format "%s" (if (stringp error-data) error-data
                                                  (or (plist-get error-data :message) "Provider request failed")))
                                   1000))))
              (plist-put state :errors (append (plist-get state :errors) (list diagnostic)))
              (plist-put info :niwa-retry-errors (plist-get state :errors))))
          (if (< (plist-get state :retries) (plist-get state :limit))
              (plist-put state :pending t)
            (plist-put info :niwa-retry-exhausted (> (plist-get state :limit) 0))
            nil))))))

(defun niwa-gptel-retry--callback (state info callback response callback-info &rest args)
  "Gate attempt INFO's RESPONSE before CALLBACK can settle or insert it.
STATE owns the logical sample; CALLBACK-INFO is gptel's callback context."
  (when (niwa-gptel-retry--live-p state info)
    (let ((terminal (or (memq response '(t nil))
                       (and (stringp response)
                            (not (plist-get callback-info :stream))))))
      (plist-put callback-info :error
                 (niwa-gptel-retry--json-error (plist-get callback-info :error)))
      (when-let* ((provider-error (plist-get callback-info :niwa-retry-provider-error)))
        (plist-put callback-info :error provider-error))
      (unless (and (not (eq response 'abort))
                   (if (plist-get callback-info :error)
                       (niwa-gptel-retry--pending-p callback-info)
                     (plist-get state :pending)))
        (when (and terminal (plist-get callback-info :error))
          (niwa-gptel-retry--rollback state t))
        (when terminal (niwa-gptel-retry--totalize-usage state callback-info))
        (when (niwa-gptel-retry--live-p state info)
          (unwind-protect
              (progn
                (unless (plist-get callback-info :error)
                  (niwa-gptel-retry--restore-status state))
                (when (and terminal
                           (or (plist-get callback-info :error)
                               (not (plist-get callback-info :tool-use))))
                  (plist-put state :terminal-callback t))
                (apply callback response callback-info args))
            (plist-put state :terminal-callback nil)))))))

(defun niwa-gptel-retry--insert (original response info &optional raw)
  "Tag ORIGINAL's inserted RESPONSE as provisional output of attempt INFO."
  (let ((state (plist-get info :niwa-retry-state)))
    (if (not state)
        (funcall original response info raw)
      (when (niwa-gptel-retry--live-p state info)
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
                               (niwa-gptel-retry--tag-output
                                info start (point)
                                (and placeholder (eq (char-after) ?\n))))
                             gptel-post-stream-hook)))
                  (prog1 (funcall original response info raw)
                    (unless captured
                      ;; Nonstream insertions record their end without a hook.
                      (niwa-gptel-retry--tag-output
                       info start (marker-position (plist-get info :tracking-marker)))))))
            (set-marker start nil))))))))

(defun niwa-gptel-retry--tag-output (info start end &optional placeholder)
  "Tag INFO's native insertion from START to END, including owned PLACEHOLDER."
  (let ((limit (+ end (if placeholder 1 0))))
    (when (< start limit)
      (with-silent-modifications
        (put-text-property start limit 'niwa-retry-output (plist-get info :niwa-retry-attempt))
        (dolist (boundary (delete-dups (list end limit)))
          (when (> boundary start)
            (let ((rear (get-text-property (1- boundary) 'rear-nonsticky)))
              (unless (eq rear t)
                (put-text-property (1- boundary) boundary 'rear-nonsticky
                                   (cons 'niwa-retry-output rear))))))))))

(defun niwa-gptel-retry--rollback (state &optional terminal)
  "Discard failed output and restore STATE's sample, retaining TERMINAL error."
  (let* ((fsm (plist-get state :fsm))
         (info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer)))
    (unless terminal
      (plist-put state :reported-tokens
                 (niwa-gptel-retry--sum-tokens (plist-get state :reported-tokens)
                                                 (plist-get info :tokens)))
      (unless (plist-get info :tokens) (plist-put state :usage-incomplete t)))
    (niwa-gptel-stream-bridge-discard-batch info)
    (with-current-buffer buffer
      (let ((inhibit-read-only t) (inhibit-modification-hooks t)
            (position (point-max)))
        (while (> position (point-min))
          (let ((start (previous-single-property-change
                        position 'niwa-retry-output nil (point-min))))
            (when (and (plist-get info :niwa-retry-attempt)
                       (eq (plist-get info :niwa-retry-attempt)
                           (get-text-property (1- position) 'niwa-retry-output)))
              (delete-region start position))
            (setq position start))))
      (when-let* ((view niwa--view-buffer) ((buffer-live-p view)))
        (with-current-buffer view
          (niwa-view-render-invalidate-live-tail))
        (niwa-view-rerender view)))
    (when-let* ((reset (plist-get info :niwa-retry-reset-output)))
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
                            :niwa-retry-terminal :niwa-retry-recorded
                            :niwa-retry-check-completion
                            :niwa-retry-advice :niwa-retry-provider-error
                            :niwa-retry-usage-totalized :niwa-retry-usage-incomplete))))
      (plist-put info key nil))))

(defun niwa-gptel-retry--resend (state)
  "Resend STATE's prepared sample after the previous transport has unwound."
  (plist-put state :timer nil)
  (when (niwa-gptel-retry--live-p state)
    (if (plist-get state :completing)
        (plist-put state :timer
                   (niwa-transport-run-at-time 0.05 #'niwa-gptel-retry--resend state))
      (condition-case err
          (progn
            (niwa-gptel-retry--rollback state)
            ;; Rendering can yield to interruption or a replacement turn.
            (when (niwa-gptel-retry--live-p state)
              (plist-put state :pending nil)
              (cl-incf (plist-get state :retries))
              (niwa-gptel-retry--status state "Retrying provider request…")
              (when (niwa-gptel-retry--live-p state)
                (gptel--fsm-transition (plist-get state :fsm) 'MRESEND))))
        (error
         (let* ((fsm (plist-get state :fsm)) (info (gptel-fsm-info fsm)))
           (plist-put state :pending nil)
           (plist-put info :niwa-retry-permanent t)
           (plist-put info :error (list :type "retry_recovery_failed"
                                       :message (error-message-string err)))
           (gptel--fsm-transition fsm 'ERRS)))))))

(defun niwa-gptel-retry--park (fsm)
  "Schedule FSM's accepted retry once; repeated cleanup transitions are inert."
  (let* ((info (gptel-fsm-info fsm))
         (state (plist-get info :niwa-retry-state)))
    (when (and (plist-get state :pending) (not (plist-get state :timer))
               (niwa-gptel-retry--live-p state info))
      (let* ((attempt (1+ (plist-get state :retries)))
             (delay (niwa-gptel-retry--delay attempt (plist-get info :niwa-retry-advice))))
        (niwa-gptel-retry--status
         state (format "Provider temporarily unavailable · retry %d/%d in %.0fs"
                       attempt (plist-get state :limit) delay))
        (when (niwa-gptel-retry--live-p state info)
          (plist-put state :timer
                     (niwa-transport-run-at-time delay #'niwa-gptel-retry--resend state)))))))

(defun niwa-gptel-retry--send (fsm)
  "Dispatch one physical attempt of FSM through gptel's native helper."
  (let* ((old (gptel-fsm-info fsm))
         (state (plist-get old :niwa-retry-state))
         (info (copy-sequence old))
         (callback (plist-get state :callback))
         (wrapper (apply-partially #'niwa-gptel-retry--callback state info callback))
         (niwa-gptel-retry--dispatch-state state))
    (when (niwa-gptel-retry--live-p state old)
     (plist-put info :niwa-retry-attempt (make-symbol "gptel-attempt"))
    (plist-put info :callback wrapper)
    (setf (gptel-fsm-info fsm) info)
    (gptel--handle-wait fsm)
    (dolist (entry gptel--request-alist)
      (when (and (eq fsm (cadr entry)) (processp (car entry)))
        (unless (process-get (car entry) 'niwa-retry-info)
          (process-put (car entry) 'niwa-retry-info (gptel-fsm-info fsm)))))
    (when (plist-get state :cancelled) (niwa-gptel-retry--abort-transports fsm)))))

(defun niwa-gptel-retry-dispatch (fsm)
  "Send FSM's logical sample, enabling bounded recovery for admitted requests.
A machine whose sample was cancelled in its tool phase, or whose agent
invocation has settled, aborts instead: its owner no longer accepts output."
  (unless niwa-gptel-retry--installed (niwa-gptel-retry-install))
  (let* ((info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer))
         (request (and (buffer-live-p buffer)
                       (buffer-local-value 'niwa--current-request buffer)))
         (invocation (or (plist-get info :niwa-agent-invocation)
                         (and (buffer-live-p buffer)
                              (buffer-local-value 'niwa--agent-invocation buffer)))))
    (if (or (plist-get info :niwa-retry-cancelled)
            ;; Tools can finish after interruption settled the invocation.
            (and (niwa-agent-invocation-p invocation)
                 (niwa-agent-invocation-runtime-settled-p invocation)))
        (gptel--fsm-transition fsm 'ABRT)
      (if (not (and (niwa-gptel-retry--owner-p fsm request invocation)
                    (or (not invocation) (> niwa-gptel-retry-max-retries 0))
                    (not (niwa-engine-external-p (plist-get info :backend)))))
          (progn
            (when-let* ((previous (plist-get info :niwa-retry-state)))
              (setq info (copy-sequence info))
              (setf (gptel-fsm-info fsm) info)
              (niwa-gptel-retry--close previous)
              (plist-put info :callback (plist-get previous :callback))
              (plist-put info :niwa-retry-state nil))
            (gptel--handle-wait fsm))
        (let* ((callback (if (eq (plist-get info :callback) (plist-get info :niwa-usage-wrapper))
                             (plist-get info :niwa-usage-original)
                           (plist-get info :callback)))
               (previous (plist-get info :niwa-retry-state))
               (state (list :fsm fsm :request request :retries 0
                            :scope (niwa-diagnostic-scope (plist-get info :buffer))
                            :limit niwa-gptel-retry-max-retries
                            :data (copy-tree (plist-get info :data) t)
                            :callback (if previous (plist-get previous :callback)
                                        (or callback (if (and gptel-use-curl (plist-get info :stream))
                                                         #'gptel-curl--stream-insert-response
                                                       #'gptel--insert-response)))
                            :invocation invocation)))
          (when previous
            (plist-put previous :closed t)
            (plist-put previous :data nil)
            (setq niwa-gptel-retry--states (delq previous niwa-gptel-retry--states)))
          (dolist (key '(:position :tracking-marker :reasoning-marker))
            (when-let* ((marker (plist-get info key)))
              (plist-put state key (copy-marker marker (marker-insertion-type marker)))))
          (plist-put info :niwa-retry-state state)
          (plist-put info :niwa-retry-exhausted nil)
          (plist-put info :niwa-retry-errors nil)
          (plist-put info :niwa-retry-terminal nil)
          (plist-put info :niwa-retry-check-completion nil)
          (plist-put info :niwa-retry-advice nil)
          (plist-put info :niwa-retry-provider-error nil)
          (plist-put info :niwa-retry-permanent nil)
          (plist-put info :niwa-retry-usage-totalized nil)
          (plist-put info :niwa-retry-usage-incomplete nil)
          (plist-put info :niwa-retry-recorded nil)
          (plist-put info :niwa-retry-reset-output nil)
          (dolist (key '(:stop-reason :reasoning-block :partial_text :partial_reasoning :reasoning-chunks :signature))
            (plist-put info key nil))
          (let ((table (copy-tree (gptel-fsm-table fsm)))
                (handlers (copy-tree (gptel-fsm-handlers fsm))))
            (unless (assq 'MRETRY table)
              (push (cons #'niwa-gptel-retry--pending-p 'MRETRY) (cdr (assq 'TYPE table)))
              (push '(MRETRY . ((t . MRETRY))) table)
              (push '(MRESEND . ((t . TYPE))) table)
              (push '(MRETRY niwa-gptel-retry--park) handlers)
              (push '(MRESEND niwa-gptel-retry--send) handlers))
            (setf (gptel-fsm-table fsm) table (gptel-fsm-handlers fsm) handlers))
          (push state niwa-gptel-retry--states)
          (with-current-buffer buffer
            (setq niwa-gptel-retry--fsm fsm)
            (add-hook 'kill-buffer-hook #'niwa-gptel-retry--buffer-killed nil t))
          (when-let* ((invocation (plist-get state :invocation)))
            (setf (niwa-agent-invocation-runtime-cancel invocation)
                  (lambda ()
                    (when-let* ((current (plist-get (gptel-fsm-info fsm) :niwa-retry-state)))
                      (niwa-gptel-retry--cancel current))
                    (unless (plist-get (gptel-fsm-info fsm) :niwa-retry-state)
                      (niwa-gptel-retry--abort-transports fsm))
                    nil)))
          (unless (or (null request) (plist-get info :niwa-retry-canceller))
            (plist-put info :niwa-retry-canceller t)
            (niwa-request-push-canceller
             request (lambda ()
                       (when-let* ((current (plist-get (gptel-fsm-info fsm) :niwa-retry-state)))
                         ;; Result publication ends its request inside an
                         ;; accepted callback, ahead of native DONE/ERRS.
                         ;; Explicit runtime interruption calls --cancel directly.
                         (if (and (plist-get current :terminal-callback)
                                  (not (plist-get current :cancelled)))
                             (niwa-gptel-retry--close current)
                           (niwa-gptel-retry--cancel current))))))
          (niwa-gptel-retry--send fsm))))))

(defun niwa-gptel-retry--parse-stream (original backend info)
  "Use ORIGINAL's native parsing, retaining stream errors and completion.
Inspect complete SSE or JSON-line envelopes, not provider text or tool data."
  (let ((start (point)) (state (plist-get info :niwa-retry-state)))
    (cond
     ;; Gemini rewinds using match data even when no object was found.  A
     ;; header-only first chunk has neither an object nor prior match data.
     ((and (fboundp 'gptel-gemini-p) (gptel-gemini-p backend)
           (or state
               (when-let* ((buffer (plist-get info :buffer))
                           ((buffer-live-p buffer)))
                 (buffer-local-value 'niwa--current-request buffer)))
           (not (save-excursion
                  (save-match-data (search-forward "{" nil t)))))
      "")
     ((not state) (funcall original backend info))
     (t
      (when (niwa-gptel-retry--live-p state info)
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
                      (plist-put info :niwa-retry-terminal t)
                      (forward-line 1))
                  (let* ((complete nil)
                         (data (condition-case nil
                                   (let ((parsed (gptel--json-read)))
                                     (setq complete t)
                                     (and (listp parsed) parsed))
                                 (error nil)))
                         (response (plist-get data :response))
                         (choices (plist-get data :choices))
                         (error-data (or (niwa-gptel-retry--json-error (plist-get data :error))
                                         (niwa-gptel-retry--json-error (plist-get response :error))
                                         (and (equal event "error")
                                              (not (plist-member data :error)) data))))
                    ;; Unknown native stream formats retain gptel's EOF policy.
                    ;; Only recognizable envelopes promise a completion marker.
                    (when (and complete
                               (or (plist-member data :choices)
                                   (plist-member data :done)
                                   (and event (string-match-p
                                               "\\`\\(?:response\\.\\|content_block_\\|message_\\)" event))))
                      (plist-put info :niwa-retry-check-completion t))
                    (when (and complete
                               (or error-data
                                   (eq (plist-get data :done) t)
                                   ;; Compatible servers may close after this
                                   ;; chunk without the [DONE] sentinel.
                                   (and (vectorp choices)
                                        (seq-some (lambda (choice)
                                                    (and (listp choice)
                                                         (niwa-gptel-retry--json-error
                                                          (plist-get choice :finish_reason))))
                                                  choices))
                                   (member event '("message_stop" "response.failed" "response.incomplete" "response.completed"))))
                      (plist-put info :niwa-retry-terminal t)
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
          (when (niwa-gptel-retry--json-error (plist-get info :stop-reason))
            (plist-put info :niwa-retry-terminal t))
          (plist-put info :error (niwa-gptel-retry--json-error (plist-get info :error)))
          (when-let* ((error-data (plist-get info :error)))
            (plist-put info :niwa-retry-provider-error error-data)
            (let ((headers (and (listp error-data) (plist-get error-data :headers))))
              (plist-put info :niwa-retry-advice
                         (or (plist-get headers :retry-after)
                             (plist-get headers :Retry-After)
                             (and (listp error-data) (plist-get error-data :retry_after))))))
          text))))))

(defun niwa-gptel-retry--converter (original marker)
  "Retain ORIGINAL's converter cleanup for MARKER on a failed attempt."
  (let ((before gptel-post-response-functions)
        (converter (funcall original marker)))
    (when niwa-gptel-retry--dispatch-state
      (let ((hooks (cl-set-difference gptel-post-response-functions before)))
        (plist-put
         niwa-gptel-retry--dispatch-state :converter-dispose
         (lambda ()
           (when (marker-buffer marker)
             (with-current-buffer (marker-buffer marker)
               (dolist (hook hooks)
                 (when (memq hook gptel-post-response-functions)
                   (funcall hook (marker-position marker) nil)))))))))
    converter))

(defun niwa-gptel-retry--parse-response (original backend response info &rest args)
  "Normalize in-band errors from non-streaming RESPONSE before ORIGINAL."
  (when (plist-get info :niwa-retry-state)
    ;; Gemini streams also enter this native generic with individual candidates.
    (when (and (listp response) (plist-member response :candidates))
      (plist-put info :niwa-retry-check-completion t))
    (when-let* ((error-data (and (listp response)
                               (niwa-gptel-retry--json-error (plist-get response :error)))))
      (plist-put info :error error-data)))
  (apply original backend response info args))

(defun niwa-gptel-retry--parse-http (original &rest args)
  "Retain retry advice and normalized errors around ORIGINAL's HTTP parsing.
Both native curl and URL parsers receive request INFO as their last argument."
  (let ((info (car (last args))))
    (when (plist-get info :niwa-retry-state)
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t))
          (when-let* ((end (re-search-forward "\r?\n\r?\n" nil t)))
            (goto-char (point-min))
            (when (re-search-forward "^retry-after: *\\([^\r\n]+\\)" end t)
              (plist-put info :niwa-retry-advice (string-trim (match-string 1)))))))))
  (let* ((result (apply original args))
         (info (car (last args)))
         (error-data (and (plist-get info :niwa-retry-state)
                          (niwa-gptel-retry--json-error (plist-get info :error)))))
    ;; Native parsers return (TEXT HTTP-STATUS STATUS ERROR).  In-band errors
    ;; must reach that same boundary, including physical-call usage observers.
    (if error-data (append (seq-take result 3) (list error-data)) result)))

(defun niwa-gptel-retry--filter (original process output)
  "Ignore OUTPUT from a retired attempt before ORIGINAL parses or inserts it."
  (let* ((info (process-get process 'niwa-retry-info))
         (state (plist-get info :niwa-retry-state)))
    (when (or (null state) (niwa-gptel-retry--live-p state info))
      (funcall original process output))))

(defun niwa-gptel-retry--transition (original fsm &optional new-state)
  "Gate cancelled transitions and release FSM's owner at terminal NEW-STATE.
ORIGINAL still owns native transitions and all their handlers."
  (if (and (null new-state) (eq fsm niwa-gptel-retry--cleanup-header-fsm))
      ;; Nonzero stream cleanup unconditionally attempts WAIT -> TYPE before
      ;; its callback.  Headers already did that: keep TYPE until the callback
      ;; runs, then let cleanup's final transition settle or park the sample.
      (setq niwa-gptel-retry--cleanup-header-fsm nil)
    (let ((state (plist-get (gptel-fsm-info fsm) :niwa-retry-state)))
      (if (null state)
          (funcall original fsm new-state)
        (when (or (not (plist-get state :cancelled)) (eq new-state 'ABRT))
          (let* ((next (or new-state (gptel--fsm-next fsm)))
                 (terminal (memq next '(DONE ERRS ABRT))))
            (when (eq next 'TOOL)
              ;; Tools now own the machine; a recovered tool-only response may
              ;; deliver no callback to end the retry status.
              (plist-put state :tools t)
              (niwa-gptel-retry--restore-status state))
            ;; Empty successful responses need no text callback.  Native terminal
            ;; handlers can still end the request without turning DONE into ABRT.
            (when terminal
              (niwa-gptel-retry--totalize-usage state (gptel-fsm-info fsm))
              (plist-put state :terminal-callback t))
            (when (eq next 'ERRS) (niwa-gptel-retry--retain-failure state))
            (unwind-protect (funcall original fsm next)
              (when terminal
                (plist-put state :terminal-callback nil)
                (niwa-gptel-retry--close state)))))))))

(defun niwa-gptel-retry--cleanup (original process status)
  "Complete PROCESS via ORIGINAL, detecting truncated HTTP200 streams first."
  (let* ((fsm (car (alist-get process gptel--request-alist)))
         (info (or (process-get process 'niwa-retry-info)
                   (and fsm (gptel-fsm-info fsm))))
         (state (plist-get info :niwa-retry-state))
         (niwa-gptel-retry--cleanup-header-fsm
          (and fsm (plist-get info :stream)
               (not (zerop (process-exit-status process)))
               (eq (gptel-fsm-state fsm) 'TYPE)
               ;; Callback ordering also matters when retries are disabled.
               ;; Leave unrelated native gptel requests unchanged.
               (or state
                   (when-let* ((buffer (plist-get info :buffer))
                               ((buffer-live-p buffer))
                               (request (buffer-local-value
                                         'niwa--current-request buffer)))
                     (eq fsm (niwa-request-fsm request))))
               fsm)))
    (if (not state)
        (funcall original process status)
      (when (and (not (memq (process-status process) '(stop run)))
                 (niwa-gptel-retry--live-p state info))
        (plist-put state :completing t)
        (unwind-protect
            (progn
              (when (and (plist-get info :stream)
                         (plist-get info :niwa-retry-check-completion)
                         (zerop (process-exit-status process))
                         (member (plist-get info :http-status) '("200" "100"))
                         (not (plist-get info :error))
                         (not (plist-get info :niwa-retry-terminal)))
                (plist-put info :error '(:code "stream_interrupted" :message "Stream ended before response completion"))
                (when-let* ((call (plist-get info :niwa-usage-call)))
                  (plist-put call :provider-error t)))
              (when (buffer-live-p (process-buffer process))
                (with-current-buffer (process-buffer process)
                  (save-excursion
                    (goto-char (point-min))
                    (let ((case-fold-search t))
                      (when-let* ((end (re-search-forward "\r?\n\r?\n" nil t)))
                        (goto-char (point-min))
                        (when (re-search-forward "^retry-after: *\\([^\r\n]+\\)" end t)
                          (plist-put info :niwa-retry-advice (string-trim (match-string 1)))))))))
              (funcall original process status))
          ;; Ownership ends at a native terminal transition or the next WAIT,
          ;; not HTTP completion: asynchronous tool callbacks still need it.
          (plist-put state :completing nil))))))

(defun niwa-gptel-retry-install ()
  "Install sparse gptel adapters; requests opt in at their dispatch boundary."
  (setq niwa-gptel-retry--installed t)
  (dolist (pair '((gptel-curl--parse-stream . niwa-gptel-retry--parse-stream)
                  (gptel-curl--parse-response . niwa-gptel-retry--parse-http)
                  (gptel--url-parse-response . niwa-gptel-retry--parse-http)
                  (gptel--fsm-transition . niwa-gptel-retry--transition)
                  (gptel--stream-convert-markdown->org . niwa-gptel-retry--converter)
                  (gptel--parse-response . niwa-gptel-retry--parse-response)
                  (gptel-curl--stream-filter . niwa-gptel-retry--filter)
                  (gptel-curl--stream-cleanup . niwa-gptel-retry--cleanup)
                  (gptel-curl--sentinel . niwa-gptel-retry--cleanup)
                  (gptel-curl--stream-insert-response . niwa-gptel-retry--insert)
                  (gptel--insert-response . niwa-gptel-retry--insert)))
    (unless (advice-member-p (cdr pair) (car pair))
      (advice-add (car pair) :around (cdr pair)
                  (list (cons 'depth (if (memq (car pair)
                                               '(gptel-curl--stream-cleanup gptel-curl--sentinel
                                                 gptel-curl--stream-filter))
                                          -90 100)))))))

(defun niwa-gptel-retry-uninstall ()
  "Cancel owned recovery and remove gptel adapters."
  (setq niwa-gptel-retry--installed nil)
  (dolist (state (copy-sequence niwa-gptel-retry--states))
    (niwa-gptel-retry--cancel state))
  (dolist (pair '((gptel-curl--parse-stream . niwa-gptel-retry--parse-stream)
                  (gptel-curl--parse-response . niwa-gptel-retry--parse-http)
                  (gptel--url-parse-response . niwa-gptel-retry--parse-http)
                  (gptel--fsm-transition . niwa-gptel-retry--transition)
                  (gptel--stream-convert-markdown->org . niwa-gptel-retry--converter)
                  (gptel--parse-response . niwa-gptel-retry--parse-response)
                  (gptel-curl--stream-filter . niwa-gptel-retry--filter)
                  (gptel-curl--stream-cleanup . niwa-gptel-retry--cleanup)
                  (gptel-curl--sentinel . niwa-gptel-retry--cleanup)
                  (gptel-curl--stream-insert-response . niwa-gptel-retry--insert)
                  (gptel--insert-response . niwa-gptel-retry--insert)))
    (advice-remove (car pair) (cdr pair))))

(provide 'niwa-gptel-retry)
;;; niwa-gptel-retry.el ends here
