;;; mevedel-agent-runtime.el --- Retained agent request runner -*- lexical-binding: t -*-

;;; Commentary:

;; Provider boundary for retained Agent V2 conversations.  The durable tree,
;; admission, addressing, mailboxes, and waits live in `mevedel-agent-control'.
;; This module starts one asynchronous provider turn, persists its transcript,
;; settles it exactly once, and interrupts it by canonical invocation.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  ;; Required for the cl-defstruct `setf' expanders of invocation slots.
  (require 'mevedel-agents)
  (require 'subr-x))

;; `gptel-request'
(declare-function gptel-abort "ext:gptel-request" (buf))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--request-alist)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control--commit-session
                  "mevedel-agent-control" (session))
(defvar mevedel-agent-control-suppress-persistence)

;; `mevedel-agent-conversation'
(declare-function mevedel-agent-conversation-configure
                  "mevedel-agent-conversation" (invocation &optional buffer))
(declare-function mevedel-agent-conversation-final-activity
                  "mevedel-agent-conversation" (invocation))
(declare-function mevedel-agent-conversation-final-response
                  "mevedel-agent-conversation" (invocation))
(declare-function mevedel-agent-conversation-open
                  "mevedel-agent-conversation"
                  (invocation parent-data-buffer &optional existing-buffer))
(declare-function mevedel-agent-conversation-record-activity
                  "mevedel-agent-conversation"
                  (invocation item &optional suppress-rerender))
(declare-function mevedel-agent-conversation-refresh
                  "mevedel-agent-conversation" (invocation))
(declare-function mevedel-agent-conversation-save
                  "mevedel-agent-conversation" (invocation &optional deferred))
(defvar mevedel--agent-invocation)

;; `mevedel-agent-exec'
(declare-function mevedel-agent-exec-freeze-configuration
                  "mevedel-agent-exec"
                  (agent-type invocation &optional model-policy))
(declare-function mevedel-agent-exec-run
                  "mevedel-agent-exec"
                  (main-cb agent-type description invocation agent-buffer))

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-transcript-path-p
                  "mevedel-agent-persistence" (path save-path))

;; `mevedel-agents'
(declare-function mevedel-agent-configuration-agent
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-configuration-p
                  "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-activity
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-create "mevedel-agents" (agent))
(declare-function mevedel-agent-invocation-description
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-frozen-configuration
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-hook-audits
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-turn
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-plan-read-only
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-require-path
                  "mevedel-agents" (invocation))
(declare-function mevedel-agent-invocation-runtime-execution-results
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-runtime-budget-timer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-runtime-fsm
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-runtime-pending-response
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-runtime-settle-callback
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-runtime-settled-p
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-sidecar-dirty
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-permission-rules
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-started-at
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(declare-function mevedel-plan-read-only-request-p "mevedel-agents" ())
(defvar mevedel-agent-task-path-property)

;; `mevedel-execution'
(declare-function mevedel-execution-owner-live-p
                  "mevedel-execution" (session owner))
(declare-function mevedel-execution-stop-owner
                  "mevedel-execution" (session owner))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

;; `mevedel-hooks'
(declare-function mevedel-hooks-context-audit-records
                  "mevedel-hooks" (decision event type &optional omit-context))
(declare-function mevedel-hooks-context-entries
                  "mevedel-hooks" (decision event))
(declare-function mevedel-hooks-decision-reason
                  "mevedel-hooks" (decision))
(declare-function mevedel-hooks-event-plist
                  "mevedel-hooks"
                  (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-format-context
                  "mevedel-hooks" (entries))
(declare-function mevedel-hooks-run-event
                  "mevedel-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--record-running-transcript
                  "mevedel-session-persistence" (session entry))
(declare-function mevedel-session-persistence--shallow-ensure-files
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence--update-transcript-entry
                  "mevedel-session-persistence" (session agent-id updates))
(declare-function mevedel-session-persistence--write-sidecar-now
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-artifact-present-p
                  "mevedel-session-persistence" (session logical))
(declare-function mevedel-session-persistence-publish-agent-terminal-state
                  "mevedel-session-persistence" (invocation))
(defvar mevedel-session--read-only-mode)

;; `mevedel-structs'
(declare-function mevedel-request-end
                  "mevedel-structs" (&optional abort-plan-approval))
(declare-function mevedel-session-agent-transcripts
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-p "mevedel-structs" (cl-x))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-tool-task'
(declare-function mevedel-tool-task--refresh-display "mevedel-tool-task" ())
(declare-function mevedel-tool-task-finalize-owner
                  "mevedel-tool-task" (session owner status))

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-transcript-audit" (record))
(declare-function mevedel--hook-prompt-rewrite-audit-record
                  "mevedel-transcript-audit"
                  (event original submitted &optional reason))

;; `mevedel-view-agent'
(declare-function mevedel-view-agent-live-transcript-finalize
                  "mevedel-view-agent" (invocation))

(defconst mevedel-agent-runtime--partial-max-chars (* 32 1024)
  "Maximum inline partial response size for an interrupted turn.")

(defvar mevedel-agent-runtime--defer-terminal-publication-p nil
  "Non-nil while remote terminal agent state is being assembled in memory.")


;;
;;; Transcript persistence

(defun mevedel-agent-runtime-dispatch--abandon-persistence (invocation)
  "Drop incomplete persistence state for INVOCATION."
  (let ((session (mevedel-agent-invocation-parent-session invocation))
        (agent-id (mevedel-agent-invocation-agent-id invocation))
        (buffer (mevedel-agent-invocation-buffer invocation))
        transcript)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq transcript buffer-file-name)
        (set-buffer-modified-p nil)
        (setq buffer-file-name nil)))
    (when (and transcript
               session
               (mevedel-session-save-path session)
               (file-in-directory-p transcript
                                    (mevedel-session-save-path session))
               (file-exists-p transcript))
      (delete-file transcript))
    (setf (mevedel-agent-invocation-transcript-relative-path invocation) nil
          (mevedel-agent-invocation-sidecar-dirty invocation) nil)
    (when (and session agent-id)
      (setf (mevedel-session-agent-transcripts session)
            (assoc-delete-all
             agent-id (mevedel-session-agent-transcripts session))))))

(defun mevedel-agent-runtime--setup-transcript (invocation agent-buffer)
  "Create and register INVOCATION's transcript for AGENT-BUFFER."
  (let ((session (mevedel-agent-invocation-parent-session invocation))
        (parent-buffer
         (mevedel-agent-invocation-parent-data-buffer invocation))
        (agent-id (mevedel-agent-invocation-agent-id invocation)))
    (when (and session
               (buffer-live-p agent-buffer)
               (buffer-live-p parent-buffer)
               (not (buffer-local-value
                     'mevedel-session--read-only-mode parent-buffer)))
      (condition-case err
          (when-let* ((save-path
                       (progn
                         (require 'mevedel-session-persistence)
                         (mevedel-session-persistence--shallow-ensure-files
                          session parent-buffer))))
            (let* ((agent
                    (mevedel-agent-invocation-agent invocation))
                   (agent-type (or (and agent (mevedel-agent-name agent))
                                   "agent"))
                   (bits (split-string agent-id "--" t))
                   (suffix (if-let* ((id-suffix (cadr bits)))
                               (substring id-suffix 0 (min 8 (length id-suffix)))
                             "anon"))
                   (timestamp (format-time-string "%FT%H-%M-%S"))
                   relative absolute)
              (cl-loop
               for n from 1
               for candidate =
               (format "agents/%s--%s--%s%s.chat.org"
                       agent-type timestamp suffix
                       (if (= n 1) "" (format "-%d" n)))
               for path = (expand-file-name candidate save-path)
               while (mevedel-session-persistence-artifact-present-p
                      session candidate)
               finally (setq relative candidate absolute path))
              (with-current-buffer agent-buffer
                (set-visited-file-name absolute t t))
              (setf
               (mevedel-agent-invocation-transcript-relative-path invocation)
               relative
               (mevedel-agent-invocation-sidecar-dirty invocation) t)
              (let ((now (format-time-string "%FT%H-%M-%S")))
                (mevedel-session-persistence--record-running-transcript
                 session
                 (cons agent-id
                       (list :agent-type agent-type
                             :agent-path
                             (mevedel-agent-invocation-path invocation)
                             :description
                             (mevedel-agent-invocation-description invocation)
                             :path relative
                             :status 'running
                             :created-at now
                             :updated-at now
                             :parent-turn
                             (mevedel-agent-invocation-parent-turn
                              invocation)))))))
        (error
         (message "mevedel: transcript persistence setup failed: %S" err))))))


;;
;;; Terminal settlement

(defun mevedel-agent-runtime--execution-live-p (invocation)
  "Return non-nil when INVOCATION still owns an execution."
  (when-let* ((session
               (mevedel-agent-invocation-parent-session invocation)))
    (require 'mevedel-execution)
    (mevedel-execution-owner-live-p
     session (mevedel-agent-invocation-require-path invocation))))

(defun mevedel-agent-runtime--with-execution-results (invocation response)
  "Append INVOCATION's yielded execution results to RESPONSE."
  (if-let* ((results
             (nreverse
              (copy-sequence
               (mevedel-agent-invocation-runtime-execution-results
                invocation)))))
      (concat (or response "Agent turn finished without a text response.")
              "\n\nBash completion after the agent's final response:\n\n"
              (string-join results "\n\n"))
    response))

(defun mevedel-agent-runtime--transcript-path (invocation)
  "Return INVOCATION's qualified logical transcript path when published."
  (when-let* ((relative
               (mevedel-agent-invocation-transcript-relative-path invocation))
              (session (mevedel-agent-invocation-parent-session invocation))
              (save-path (mevedel-session-save-path session)))
    (condition-case err
        (progn
          (require 'mevedel-agent-persistence)
          (when (and
                 (mevedel-agent-persistence-transcript-path-p
                  relative save-path)
                 (mevedel-session-persistence-artifact-present-p
                  session relative))
            (expand-file-name relative save-path)))
      (error
       (message "mevedel: transcript path validation failed: %S" err)
       nil))))

(defun mevedel-agent-runtime--partial-text (invocation &optional fallback)
  "Return bounded partial output for INVOCATION or FALLBACK."
  (when-let* ((raw (or (ignore-errors
                         (mevedel-agent-conversation-final-response invocation))
                       fallback))
              ((stringp raw))
              (text
               (string-trim
                (replace-regexp-in-string
                 "\\`[[:alpha:]]+ result for task: [^\n]*\n\n" "" raw)))
              ((not (string-empty-p text))))
    (if (> (length text) mevedel-agent-runtime--partial-max-chars)
        (concat (substring text 0 mevedel-agent-runtime--partial-max-chars)
                "\n\n[Partial response truncated.]")
      text)))

(defun mevedel-agent-runtime--recovery-text (invocation &optional fallback)
  "Return useful recovery text for INVOCATION or FALLBACK."
  (let ((partial (mevedel-agent-runtime--partial-text invocation fallback))
        (transcript (mevedel-agent-runtime--transcript-path invocation)))
    (concat
     (if partial
         (format "\n\nPartial response:\n\n%s" partial)
       "\n\nNo partial response was available.")
     (if transcript
         (format "\n\nTranscript: %s\nRead it with: Read(file_path=%S)"
                 transcript transcript)
       ""))))

(defun mevedel-agent-runtime--interrupted-response (invocation reason)
  "Return INVOCATION's interrupted result for REASON."
  (concat
   (format "Agent turn interrupted before finishing task \"%s\".\n\nReason: %s\nAgent path: %s"
           (or (mevedel-agent-invocation-description invocation) "")
           reason
           (or (mevedel-agent-invocation-path invocation) "unknown"))
   "\n\nTools or commands may have partially changed state. Reconcile current state and verify effects before retrying or claiming success."
   (mevedel-agent-runtime--recovery-text invocation)))

(defun mevedel-agent-runtime--error-response (invocation event)
  "Return INVOCATION's parent-visible error response for EVENT."
  (let* ((agent (mevedel-agent-invocation-agent invocation))
         (agent-type (or (and agent (mevedel-agent-name agent)) "agent"))
         (details (or (plist-get event :error-details)
                      (mevedel-agent-invocation-terminal-reason invocation)
                      "Agent turn failed")))
    (concat
     (format "Error: Task %s could not finish task \"%s\".\n\nError details: %S\nAgent path: %s"
             agent-type
             (or (mevedel-agent-invocation-description invocation) "")
             details
             (or (mevedel-agent-invocation-path invocation) "unknown"))
     (mevedel-agent-runtime--recovery-text
      invocation (plist-get event :fallback-partial)))))

(defun mevedel-agent-runtime--finalize-step (invocation label function)
  "Run terminal FUNCTION for INVOCATION, warning under LABEL on failure."
  (condition-case err
      (funcall function)
    (error
     (display-warning
      'mevedel
      (format "Agent finalization step %s failed for %s: %s"
              label
              (or (mevedel-agent-invocation-path invocation) "unknown")
              (error-message-string err))
      :warning))))

(defun mevedel-agent-runtime--finalize (invocation status)
  "Persist terminal STATUS and lifecycle effects for INVOCATION."
  (unless (memq (mevedel-agent-invocation-transcript-status invocation)
                '(completed error aborted))
    (let ((buffer (mevedel-agent-invocation-buffer invocation))
          (session (mevedel-agent-invocation-parent-session invocation))
          (parent-buffer
           (mevedel-agent-invocation-parent-data-buffer invocation)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (mevedel-request-end)))
      (when session
        (require 'mevedel-execution)
        (mevedel-execution-stop-owner
         session (mevedel-agent-invocation-require-path invocation)))
      (setf (mevedel-agent-invocation-transcript-status invocation) status)
      (mevedel-agent-runtime--finalize-step
       invocation 'transcript-status
       (lambda ()
         (when session
           (require 'mevedel-session-persistence)
           (mevedel-session-persistence--update-transcript-entry
            session (mevedel-agent-invocation-agent-id invocation)
            (list :status status
                  :updated-at (format-time-string "%FT%H-%M-%S"))))))
      (unless mevedel-agent-runtime--defer-terminal-publication-p
        (mevedel-agent-runtime--finalize-step
         invocation 'transcript-save
         (lambda () (mevedel-agent-conversation-save invocation))))
      (mevedel-agent-runtime--finalize-step
       invocation 'activity
       (lambda ()
         (mevedel-agent-conversation-record-activity
          invocation (list :type 'status :status status
                           :summary (symbol-name status))
          t)))
      (mevedel-agent-runtime--finalize-step
       invocation 'transcript-activity
       (lambda ()
         (when session
           (mevedel-session-persistence--update-transcript-entry
            session (mevedel-agent-invocation-agent-id invocation)
            (list :activity
                  (mevedel-agent-conversation-final-activity invocation))))))
      (mevedel-agent-runtime--finalize-step
       invocation 'tasks
       (lambda ()
         (when (and session (eq status 'completed))
           (require 'mevedel-tool-task)
           (when (mevedel-tool-task-finalize-owner
                  session (mevedel-agent-invocation-require-path invocation)
                  status)
             (when (buffer-live-p parent-buffer)
               (with-current-buffer parent-buffer
                 (mevedel-tool-task--refresh-display)))))))
      (mevedel-agent-runtime--finalize-step
       invocation 'handle
       (lambda () (mevedel-agent-conversation-refresh invocation)))
      (unless mevedel-agent-runtime--defer-terminal-publication-p
        (mevedel-agent-runtime--finalize-step
         invocation 'sidecar
         (lambda ()
           (when (and session (buffer-live-p parent-buffer))
             (mevedel-session-persistence--write-sidecar-now
              session parent-buffer)))))
      (setf (mevedel-agent-invocation-activity invocation) nil)
      (mevedel-agent-runtime--finalize-step
       invocation 'hook
       (lambda () (mevedel-agent-runtime--run-stop-hook invocation status)))
      (mevedel-agent-runtime--finalize-step
       invocation 'view
       (lambda ()
         (when (fboundp 'mevedel-view-agent-live-transcript-finalize)
           (mevedel-view-agent-live-transcript-finalize invocation)))))))

(defun mevedel-agent-runtime--settle (invocation response &optional event)
  "Finalize INVOCATION and deliver RESPONSE and EVENT exactly once."
  (unless (mevedel-agent-invocation-runtime-settled-p invocation)
    (let* ((pending
            (mevedel-agent-invocation-runtime-pending-response invocation))
           (committed-p
            (and (listp pending)
                 (eq 'committed (plist-get pending :phase))))
           status visible transaction remote-terminal-publication-p)
      (if committed-p
          (setq response (plist-get pending :response)
                event (plist-get pending :event)
                status (plist-get pending :status)
                visible (plist-get pending :visible)
                transaction (plist-get pending :transaction)
                remote-terminal-publication-p (plist-get pending :remote))
        (when (and (listp pending) (plist-member pending :response))
          (setq response (plist-get pending :response)
                event (plist-get pending :event)))
        (setq pending (list :response response :event event))
        (setf (mevedel-agent-invocation-runtime-pending-response invocation)
              pending)
        (let* ((event-status
                (and (listp event)
                     (plist-get event :mevedel-agent-terminal-status)))
               (callback
                (mevedel-agent-invocation-runtime-settle-callback invocation))
               (session
                (mevedel-agent-invocation-parent-session invocation))
               (save-path (and session
                               (mevedel-session-save-path session))))
          (setq status (pcase event-status
                         ('error 'error)
                         ('aborted 'aborted)
                         (_ 'completed))
                visible
                (pcase status
                  ('error
                   (mevedel-agent-runtime--error-response invocation event))
                  ('aborted
                   (mevedel-agent-runtime--interrupted-response
                    invocation
                    (or (mevedel-agent-invocation-terminal-reason invocation)
                        "interrupted")))
                  (_ (mevedel-agent-runtime--with-execution-results
                      invocation response)))
                remote-terminal-publication-p
                (and session
                     save-path
                     (progn
                       (require 'mevedel-session-persistence)
                       (mevedel-session-persistence-artifact-present-p
                        session "session.meta.el"))
                     (mevedel-agent-invocation-transcript-relative-path
                      invocation)
                     (buffer-live-p
                      (mevedel-agent-invocation-buffer invocation))
                     (buffer-live-p
                      (mevedel-agent-invocation-parent-data-buffer invocation))
                     (when-let* ((target
                                  (mevedel-session-execution-target session)))
                       (require 'mevedel-execution-target)
                       (mevedel-execution-target-remote-p target))))
          (let ((mevedel-agent-runtime--defer-terminal-publication-p
                 remote-terminal-publication-p))
            (mevedel-agent-runtime--finalize invocation status))
          (when callback
            (let ((mevedel-agent-control-suppress-persistence t))
              (setq transaction
                    (funcall callback invocation visible event))))
          (condition-case err
              (if remote-terminal-publication-p
                  (progn
                    (require 'mevedel-session-persistence)
                    (mevedel-session-persistence-publish-agent-terminal-state
                     invocation))
                (when (consp transaction)
                  (mevedel-agent-control--commit-session session)))
            (error
             (when (functionp (car-safe transaction))
               (funcall (car transaction)))
             (signal (car err) (cdr err))))
          (setq pending
                (list :response response :event event :phase 'committed
                      :status status :visible visible
                      :transaction transaction
                      :remote remote-terminal-publication-p))
          (setf (mevedel-agent-invocation-runtime-pending-response invocation)
                pending)))
      (when (functionp (cdr-safe transaction))
        (funcall (cdr transaction)))
      (setf (mevedel-agent-invocation-runtime-settled-p invocation) t
            (mevedel-agent-invocation-runtime-fsm invocation) nil
            (mevedel-agent-invocation-runtime-pending-response invocation) nil)
      (when-let* ((timer
                   (mevedel-agent-invocation-runtime-budget-timer invocation)))
        (cancel-timer timer)
        (setf (mevedel-agent-invocation-runtime-budget-timer invocation) nil))
      (when-let* ((session
                   (mevedel-agent-invocation-parent-session invocation))
                  ((fboundp 'mevedel-telemetry-record)))
        (mevedel-telemetry-record
         session 'agent-settled
         :agent-id (mevedel-agent-invocation-agent-id invocation)
         :agent-path (mevedel-agent-invocation-path invocation)
         :agent-type
         (and (mevedel-agent-invocation-agent invocation)
              (mevedel-agent-name
               (mevedel-agent-invocation-agent invocation)))
         :outcome status
         :duration-ms
         (when-let* ((started-at
                      (mevedel-agent-invocation-started-at invocation)))
           (round
            (* 1000.0
               (float-time (time-subtract (current-time) started-at)))))))
      visible)))

(defun mevedel-agent-runtime--budget-expired (invocation seconds)
  "Interrupt INVOCATION after its completion budget of SECONDS expires."
  (setf (mevedel-agent-invocation-runtime-budget-timer invocation) nil)
  (unless (mevedel-agent-invocation-runtime-settled-p invocation)
    (mevedel-agent-runtime-interrupt
     invocation (format "Goal investigation budget expired after %d seconds"
                        seconds))))

(defun mevedel-agent-runtime-bound-turn (invocation seconds)
  "Bound INVOCATION to SECONDS and return it."
  (unless (and (integerp seconds) (> seconds 0))
    (error "Agent completion budget must be positive"))
  (setf (mevedel-agent-invocation-runtime-budget-timer invocation)
        (run-at-time seconds nil
                     #'mevedel-agent-runtime--budget-expired
                     invocation seconds))
  invocation)

(defun mevedel-agent-runtime--handle-provider-result (invocation response)
  "Settle INVOCATION from provider RESPONSE, or hold for yielded Bash."
  (if (and (stringp response)
           (mevedel-agent-runtime--execution-live-p invocation))
      (setf (mevedel-agent-invocation-runtime-pending-response invocation)
            response)
    (mevedel-agent-runtime--settle invocation response
                                   (and (listp response) response))))

(defun mevedel-agent-runtime-queue-execution-completion
    (context owner body)
  "Secure yielded Bash BODY for invocation CONTEXT owned by OWNER.
Settle a held provider response once its last owned execution has finished."
  (when (and (mevedel-agent-invocation-p context)
             (equal owner (mevedel-agent-invocation-require-path context))
             (stringp body)
             (not (mevedel-agent-invocation-runtime-settled-p context)))
    (push body
          (mevedel-agent-invocation-runtime-execution-results context))
    (when-let* ((response
                 (mevedel-agent-invocation-runtime-pending-response context))
                ((not (mevedel-agent-runtime--execution-live-p context))))
      (mevedel-agent-runtime--settle context response))
    t))


;;
;;; Interruption

(defun mevedel-agent-runtime--request-live-p (invocation)
  "Return non-nil when INVOCATION has an active provider request."
  (let ((buffer (mevedel-agent-invocation-buffer invocation)))
    (and (buffer-live-p buffer)
         (boundp 'gptel--request-alist)
         (cl-some
          (lambda (entry)
            (let* ((fsm (cadr entry))
                   (info (and fsm (gptel-fsm-info fsm))))
              (eq (and info (plist-get info :buffer)) buffer)))
          gptel--request-alist))))

(defun mevedel-agent-runtime-interrupt (invocation reason)
  "Interrupt INVOCATION for REASON and settle its turn once."
  (unless (mevedel-agent-invocation-p invocation)
    (error "Interrupt target has no live invocation"))
  (if (mevedel-agent-invocation-runtime-settled-p invocation)
      (mevedel-agent-runtime--interrupted-response invocation reason)
    (let* ((buffer (mevedel-agent-invocation-buffer invocation))
           (fsm (mevedel-agent-invocation-runtime-fsm invocation))
           (response (mevedel-agent-runtime--interrupted-response
                      invocation reason))
           (previous-reason
            (mevedel-agent-invocation-terminal-reason invocation)))
      (setf (mevedel-agent-invocation-terminal-reason invocation) reason)
      (condition-case err
          (when (mevedel-agent-runtime--request-live-p invocation)
            (let* ((info (gptel-fsm-info fsm))
                   (provider-callback (plist-get info :callback)))
              (unwind-protect
                  (progn
                    (setf (gptel-fsm-info fsm)
                          (plist-put info :callback #'ignore))
                    (gptel-abort buffer))
                (setf (gptel-fsm-info fsm)
                      (plist-put (gptel-fsm-info fsm)
                                 :callback provider-callback)))))
        (error
         (setf (mevedel-agent-invocation-terminal-reason invocation)
               previous-reason)
         (signal (car err) (cdr err))))
      (mevedel-agent-runtime--settle
       invocation response
       (list :mevedel-agent-terminal-status 'aborted :response response)))))


;;
;;; Turn hooks

(defun mevedel-agent-runtime--run-hook-sync (event payload invocation)
  "Run EVENT with PAYLOAD for INVOCATION and return its decision."
  (require 'mevedel-hooks)
  (let* ((session (mevedel-agent-invocation-parent-session invocation))
         (workspace (and session (mevedel-session-workspace session)))
         done
         decision)
    (mevedel-hooks-run-event
     event payload
     (lambda (result)
       (setq decision result
             done t))
     session workspace nil invocation)
    (while (not done)
      (accept-process-output nil 0.05))
    decision))

(defun mevedel-agent-runtime--run-prompt-hook (prompt invocation)
  "Run `UserPromptSubmit' for agent PROMPT and return its decision."
  (require 'mevedel-hooks)
  (let* ((session (mevedel-agent-invocation-parent-session invocation))
         (workspace (and session (mevedel-session-workspace session))))
    (mevedel-agent-runtime--run-hook-sync
     'UserPromptSubmit
     (mevedel-hooks-event-plist
      'UserPromptSubmit session workspace
      :agent-path (mevedel-agent-invocation-path invocation)
      :prompt prompt
      :display-text prompt)
     invocation)))

(defun mevedel-agent-runtime--run-stop-hook (invocation status)
  "Fire `SubagentStop' hooks for INVOCATION terminal STATUS."
  (when (mevedel-agent-invocation-p invocation)
    (require 'mevedel-hooks)
    (let* ((session (mevedel-agent-invocation-parent-session invocation))
           (workspace (and session (mevedel-session-workspace session)))
           (agent (mevedel-agent-invocation-agent invocation))
           (agent-type (and agent (mevedel-agent-name agent)))
           (parent-buffer
            (mevedel-agent-invocation-parent-data-buffer invocation))
           (runner
            (lambda ()
              (mevedel-hooks-run-event
               'SubagentStop
               (mevedel-hooks-event-plist
                'SubagentStop session workspace
                :agent-path (mevedel-agent-invocation-path invocation)
                :role agent-type
                :description
                (mevedel-agent-invocation-description invocation)
                :status status
                :terminal-reason
                (mevedel-agent-invocation-terminal-reason invocation)
                :transcript-relative-path
                (mevedel-agent-invocation-transcript-relative-path
                 invocation))
               #'ignore
               session workspace nil invocation))))
      (if (and parent-buffer (buffer-live-p parent-buffer))
          (with-current-buffer parent-buffer
            (funcall runner))
        (funcall runner)))))

(defun mevedel-agent-runtime--prepare-followup
    (prompt invocation pending-hook-context on-hook-context)
  "Prepare one retained follow-up PROMPT for INVOCATION.
PENDING-HOOK-CONTEXT belongs to the conversation; ON-HOOK-CONTEXT records a
blocked transition."
  (let* ((prompt-decision
          (mevedel-agent-runtime--run-prompt-hook prompt invocation))
         (prompt-context
          (mevedel-hooks-context-entries
           prompt-decision 'UserPromptSubmit)))
    (when (and (plist-member prompt-decision :continue)
               (not (plist-get prompt-decision :continue))
               on-hook-context)
      (funcall on-hook-context
               (append pending-hook-context prompt-context)))
    (mevedel-agent-runtime--prepared-turn
     prompt nil prompt-decision pending-hook-context)))

(defun mevedel-agent-runtime--prepared-turn
    (prompt start-decision prompt-decision pending-hook-context)
  "Build one final agent task from PROMPT and its hook decisions."
  (when (and (plist-member start-decision :continue)
             (not (plist-get start-decision :continue)))
    (error "%s" (or (plist-get start-decision :stop-reason)
                    "SubagentStart hook stopped sub-agent")))
  (when (and (plist-member prompt-decision :continue)
             (not (plist-get prompt-decision :continue)))
    (error "%s" (or (plist-get prompt-decision :stop-reason)
                    "UserPromptSubmit hook stopped agent task")))
  (when-let* ((msg (plist-get prompt-decision :system-message)))
    (message "mevedel: %s" msg))
  (let* ((submitted
          (if (stringp (plist-get prompt-decision :updated-input))
              (plist-get prompt-decision :updated-input)
            prompt))
         (context
          (mevedel-hooks-format-context
           (append
            (mevedel-hooks-context-entries
             start-decision 'SubagentStart)
            pending-hook-context
            (mevedel-hooks-context-entries
             prompt-decision 'UserPromptSubmit))))
         (effective-prompt
          (if context (concat submitted "\n\n" context) submitted))
         (rewrite-audit
          (progn
            (require 'mevedel-transcript-audit)
            (mevedel--hook-prompt-rewrite-audit-record
             'UserPromptSubmit prompt submitted
             (mevedel-hooks-decision-reason prompt-decision)))))
    (list :prompt effective-prompt
          :audits (and rewrite-audit (list rewrite-audit)))))

(cl-defun mevedel-agent-runtime-prepare-task
    (agent description prompt path callback
           &key skill-permission-rules cancelled-p)
  "Prepare one new AGENT task asynchronously and call CALLBACK.
PATH is the reserved canonical child path.  CALLBACK receives an outcome
plist carrying either `:turn' and `:start-hook-audits' or `:error'."
  (require 'mevedel-hooks)
  (let* ((session (and (boundp 'mevedel--session) mevedel--session))
         (workspace (and session (mevedel-session-workspace session)))
         (invocation (mevedel-agent-invocation-create agent))
         settled)
    (setf (mevedel-agent-invocation-path invocation) path
          (mevedel-agent-invocation-description invocation) description
          (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-agent-invocation-parent-data-buffer invocation)
          (current-buffer)
          (mevedel-agent-invocation-parent-turn invocation)
          (and session (mevedel-current-turn session))
          (mevedel-agent-invocation-plan-read-only invocation)
          (mevedel-plan-read-only-request-p))
    (when skill-permission-rules
      (setf (mevedel-agent-invocation-skill-permission-rules invocation)
            skill-permission-rules))
    (cl-labels
        ((cancelled () (and cancelled-p (funcall cancelled-p)))
         (finish (outcome)
           (unless (or settled (cancelled))
             (setq settled t)
             (funcall callback outcome)))
         (fail (err)
           (finish (list :outcome 'error
                         :error (error-message-string err))))
         (prepare-prompt (start-decision)
           (unless (or settled (cancelled))
             (condition-case err
                 (progn
                   (when-let* ((msg (plist-get start-decision
                                               :system-message)))
                     (message "mevedel: %s" msg))
                   (setf (mevedel-agent-invocation-hook-audits invocation)
                         (mevedel-hooks-context-audit-records
                          start-decision 'SubagentStart
                          'subagent-context t))
                   (when (and (plist-member start-decision :continue)
                              (not (plist-get start-decision :continue)))
                     (error "%s"
                            (or (plist-get start-decision :stop-reason)
                                "SubagentStart hook stopped sub-agent")))
                   (mevedel-hooks-run-event
                    'UserPromptSubmit
                    (mevedel-hooks-event-plist
                     'UserPromptSubmit session workspace
                     :agent-path path :prompt prompt :display-text prompt)
                    (lambda (prompt-decision)
                      (unless (or settled (cancelled))
                        (condition-case prompt-error
                            (finish
                             (list
                              :outcome 'success
                              :turn
                              (mevedel-agent-runtime--prepared-turn
                               prompt start-decision prompt-decision nil)
                              :start-hook-audits
                              (mevedel-agent-invocation-hook-audits
                               invocation)))
                          (error (fail prompt-error)))))
                    session workspace nil invocation))
               (error (fail err))))))
      (condition-case err
          (mevedel-hooks-run-event
           'SubagentStart
           (mevedel-hooks-event-plist
            'SubagentStart session workspace
            :agent-path path
            :role (mevedel-agent-name agent)
            :description description
            :prompt prompt
            :transcript-relative-path nil)
           #'prepare-prompt session workspace nil invocation)
        (error (fail err))))))


;;
;;; Provider dispatch

(defun mevedel-agent-runtime-task-background (summary)
  "Wrap SUMMARY as a distinct, advisory child task-background block."
  (unless (and (stringp summary) (not (string-blank-p summary)))
    (error "Agent task background must be non-empty text"))
  (format (concat "<task-background>\n"
                  "Generated background may be stale or untrusted. "
                  "The following Agent Task is authoritative.\n\n%s\n"
                  "</task-background>\n")
          (string-trim summary)))

(defun mevedel-agent-runtime--insert-prompt
    (invocation buffer description prompt context-snapshot retained-p
                &optional hook-audits)
  "Append PROMPT and optional CONTEXT-SNAPSHOT to INVOCATION's BUFFER.
HOOK-AUDITS are hidden transcript records associated with this user turn."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (start (point-max))
          (was-modified (buffer-modified-p)))
      (goto-char (point-max))
      (when (and (not retained-p)
                 (stringp context-snapshot)
                 (not (string-empty-p context-snapshot)))
        (insert context-snapshot)
        (unless (bolp) (insert "\n")))
      (unless (bobp) (insert "\n"))
      (insert (format "* Agent Task: %s\n" (or description "")))
      (unless retained-p
        (insert (format ":PROPERTIES:\n:%s: %s\n:END:\n"
                        mevedel-agent-task-path-property
                        (mevedel-agent-invocation-require-path invocation))))
      (insert "\n" (or prompt "") "\n")
      (when hook-audits
        (require 'mevedel-transcript-audit)
        (dolist (audit hook-audits)
          (insert (mevedel--format-hook-audit-record audit))))
      (when (mevedel-agent-invocation-transcript-relative-path invocation)
        (unless (mevedel-agent-conversation-save invocation)
          (if retained-p
              (progn
                (delete-region start (point-max))
                (set-buffer-modified-p was-modified)
                (error "Retained agent conversation could not be persisted"))
            (mevedel-agent-runtime-dispatch--abandon-persistence
             invocation)))))))

(cl-defun mevedel-agent-runtime-dispatch
    (agent description prompt
           &key context-snapshot model-policy skill-permission-rules
           prepared-turn start-hook-audits
           on-invocation on-settle path frozen-configuration
           retained-id retained-buffer retained-transcript
           pending-hook-context on-hook-context parent-tool-use-id)
  "Start one asynchronous retained agent turn and return its invocation.
AGENT starts a new conversation; FROZEN-CONFIGURATION and the three retained
identity values continue one.  PATH is the conversation's canonical address.
ON-SETTLE receives (INVOCATION RESPONSE EVENT) exactly once."
  (require 'mevedel-agent-conversation)
  (require 'mevedel-agent-exec)
  (unless (and (stringp path) (string-match-p "\\`/root/" path))
    (error "Agent requires a canonical path below /root"))
  (when (and frozen-configuration
             (not (mevedel-agent-configuration-p frozen-configuration)))
    (error "Invalid frozen agent configuration"))
  (let ((retained-values (delq nil (list retained-id retained-buffer
                                         retained-transcript))))
    (when (and retained-values (/= (length retained-values) 3))
      (error "Retained agent identity requires id, buffer, and transcript")))
  (let* ((retained-p retained-id)
         (agent (if frozen-configuration
                    (mevedel-agent-configuration-agent frozen-configuration)
                  agent))
         (_ (unless agent (error "Agent configuration is required")))
         (agent-type (mevedel-agent-name agent))
         (agent-id
          (or retained-id
              (format "%s--%s" agent-type
                      (md5 (format "%s%s%s%s" (system-name) (emacs-pid)
                                   (current-time) (random))))))
         (invocation (mevedel-agent-invocation-create agent))
         (parent-buffer (current-buffer))
         (session (and (boundp 'mevedel--session) mevedel--session))
         publication-ready)
    (unless (mevedel-session-p session)
      (error "Agent requires an active mevedel session"))
    (unless (or retained-p prepared-turn)
      (error "Initial agent turn requires prepared task"))
    (setf (mevedel-agent-invocation-agent-id invocation) agent-id
          (mevedel-agent-invocation-path invocation) path
          (mevedel-agent-invocation-description invocation) description
          (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-agent-invocation-parent-data-buffer invocation) parent-buffer
          (mevedel-agent-invocation-parent-turn invocation)
          (mevedel-current-turn session)
          (mevedel-agent-invocation-plan-read-only invocation)
          (mevedel-plan-read-only-request-p)
          (mevedel-agent-invocation-parent-tool-use-id invocation)
          parent-tool-use-id
          (mevedel-agent-invocation-sandbox-summary-cell invocation)
          (list nil)
          (mevedel-agent-invocation-transcript-status invocation) 'running
          (mevedel-agent-invocation-runtime-settle-callback invocation)
          (lambda (settled-invocation response event)
            (if publication-ready
                (when on-settle
                  (funcall on-settle settled-invocation response event))
              (error "Agent invocation publication is pending"))))
    (when (fboundp 'mevedel-telemetry-record)
      (mevedel-telemetry-record
       session 'agent-dispatch
       :agent-id agent-id
       :agent-path path
       :agent-type agent-type
       :retained (and retained-p t)
       :parent-turn (mevedel-agent-invocation-parent-turn invocation)))
    (when skill-permission-rules
      (setf (mevedel-agent-invocation-skill-permission-rules invocation)
            skill-permission-rules))
    (let ((configuration
           (or frozen-configuration
               (mevedel-agent-exec-freeze-configuration
                agent-type invocation model-policy))))
      (setf (mevedel-agent-invocation-frozen-configuration invocation)
            configuration
            (mevedel-agent-invocation-agent invocation)
            (mevedel-agent-configuration-agent configuration)))
    (let ((buffer
           (if retained-p
               (if (buffer-live-p retained-buffer)
                   retained-buffer
                 (error "Retained agent buffer is not live"))
             (mevedel-agent-conversation-open
              invocation parent-buffer))))
      (setf (mevedel-agent-invocation-buffer invocation) buffer)
      (if retained-p
          (progn
            (setf (mevedel-agent-invocation-transcript-relative-path invocation)
                  retained-transcript)
            (with-current-buffer buffer
              (setq-local mevedel--agent-invocation invocation)))
        (mevedel-agent-runtime--setup-transcript invocation buffer))
      (mevedel-agent-conversation-configure invocation buffer)
      (let (published-p)
        (condition-case err
            (let ((turn
                   (if retained-p
                       (mevedel-agent-runtime--prepare-followup
                        prompt invocation pending-hook-context
                        on-hook-context)
                     prepared-turn)))
              (when start-hook-audits
                (setf (mevedel-agent-invocation-hook-audits invocation)
                      start-hook-audits))
              (mevedel-agent-runtime--insert-prompt
               invocation buffer description (plist-get turn :prompt)
               context-snapshot retained-p (plist-get turn :audits))
              (when (and pending-hook-context on-hook-context)
                (funcall on-hook-context nil))
              (when (and on-settle
                         (not
                          (mevedel-agent-invocation-transcript-relative-path
                           invocation)))
                (error "Agent conversation could not be persisted"))
              (let ((fsm
                     (mevedel-agent-exec-run
                      (apply-partially
                       #'mevedel-agent-runtime--handle-provider-result
                       invocation)
                      agent-type description invocation buffer)))
                (unless fsm
                  (error "Agent provider request did not start"))
                (when on-invocation
                  (funcall on-invocation invocation))
                (setq published-p t
                      publication-ready t)
                (when (fboundp 'mevedel-telemetry-record)
                  (mevedel-telemetry-record
                   session 'agent-request-sent
                   :agent-id agent-id
                   :agent-path path
                   :agent-type agent-type
                   :retained (and retained-p t)))
                invocation))
          (error
           (if published-p
               (progn
                 (setf (mevedel-agent-invocation-terminal-reason invocation)
                       (error-message-string err))
                 (mevedel-agent-runtime--settle
                  invocation nil
                  (list :mevedel-agent-terminal-status 'error
                        :error-details (error-message-string err))))
             (when (mevedel-agent-invocation-runtime-fsm invocation)
               (mevedel-agent-runtime--finalize invocation 'error)
               (setf (mevedel-agent-invocation-runtime-settled-p invocation) t
                     (mevedel-agent-invocation-runtime-fsm invocation) nil))
             (unless retained-p
               (mevedel-agent-runtime-dispatch--abandon-persistence invocation)
               (when (buffer-live-p buffer)
                 (kill-buffer buffer))))
           (signal (car err) (cdr err))))))))

(provide 'mevedel-agent-runtime)

;;; mevedel-agent-runtime.el ends here
