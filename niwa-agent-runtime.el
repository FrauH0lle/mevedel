;;; niwa-agent-runtime.el --- Retained agent request runner -*- lexical-binding: t -*-

;;; Commentary:

;; Provider boundary for retained Agent V2 conversations.  The durable tree,
;; admission, addressing, mailboxes, and waits live in `niwa-agent-control'.
;; This module starts one asynchronous provider turn, persists its transcript,
;; settles it exactly once, and interrupts it by canonical invocation.

;;; Code:

(require 'niwa-telemetry)
(require 'niwa-utilities)

(require 'niwa-agent-directory)

(require 'niwa-recovery)

;; `setf' on a slot of a struct defined elsewhere needs that
;; `cl-defstruct' at compile time: `declare-function' supplies the
;; accessor but not the setter, and without the expander the form
;; compiles to a call to a function that does not exist.
(eval-when-compile
  (require 'gptel-request)
  (require 'niwa-structs))

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; Required for invocation accessors and `setf' expanders throughout.
(require 'niwa-agents)

;; `cl-seq'
(declare-function cl-some "cl-seq" (cl-pred cl-seq &rest cl-rest))

;; `gptel-request'
(declare-function gptel-abort "ext:gptel-request" (buf))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--fsm-last)
(defvar gptel--request-alist)

;; `niwa-agent-control'
(declare-function niwa-agent-control-clear-agent-wakes
                  "niwa-agent-control" (session path))
(declare-function niwa-agent-control-commit-session
                  "niwa-agent-control" (session &optional token))
(declare-function niwa-agent-control-enqueue-execution-result
                  "niwa-agent-control"
                  (session owner body &optional execution-id wake meta
                           acknowledgement))
(declare-function niwa-agent-control-settlement-finished
                  "niwa-agent-control" (session))
(autoload 'niwa-agent-control-clear-agent-wakes "niwa-agent-control")
(autoload 'niwa-agent-control-commit-session "niwa-agent-control")
(autoload 'niwa-agent-control-enqueue-execution-result "niwa-agent-control")
(autoload 'niwa-agent-control-settlement-finished "niwa-agent-control")
(defvar niwa-agent-control-settling)
(defvar niwa-agent-control-suppress-persistence)

;; `niwa-agent-conversation'
(declare-function niwa-agent-conversation--cancel-save
                  "niwa-agent-conversation" (invocation))
(declare-function niwa-agent-conversation-configure
                  "niwa-agent-conversation" (invocation &optional buffer))
(declare-function niwa-agent-conversation-fenced-p
                  "niwa-agent-conversation" (invocation))
(declare-function niwa-agent-conversation-final-response
                  "niwa-agent-conversation" (invocation))
(declare-function niwa-agent-conversation-open
                  "niwa-agent-conversation"
                  (invocation parent-data-buffer &optional existing-buffer))
(declare-function niwa-agent-conversation-record-activity
                  "niwa-agent-conversation"
                  (invocation item &optional suppress-rerender))
(declare-function niwa-agent-conversation-refresh
                  "niwa-agent-conversation" (invocation))
(declare-function niwa-agent-conversation-save
                  "niwa-agent-conversation" (invocation &optional deferred token))
(autoload 'niwa-agent-conversation--cancel-save "niwa-agent-conversation")
(autoload 'niwa-agent-conversation-configure "niwa-agent-conversation")
(autoload 'niwa-agent-conversation-fenced-p "niwa-agent-conversation")
(autoload 'niwa-agent-conversation-final-response
  "niwa-agent-conversation")
(autoload 'niwa-agent-conversation-open "niwa-agent-conversation")
(autoload 'niwa-agent-conversation-record-activity
  "niwa-agent-conversation")
(autoload 'niwa-agent-conversation-refresh "niwa-agent-conversation")
(autoload 'niwa-agent-conversation-save "niwa-agent-conversation")
(defvar niwa--agent-invocation)

;; `niwa-agent-exec'
(declare-function niwa-agent-exec-freeze-configuration
                  "niwa-agent-exec"
                  (agent-type invocation &optional model-policy))
(declare-function niwa-agent-exec-refresh-provider
                  "niwa-agent-exec" (invocation))
(declare-function niwa-agent-exec-run
                  "niwa-agent-exec"
                  (main-cb agent-type description invocation agent-buffer))
(autoload 'niwa-agent-exec-freeze-configuration "niwa-agent-exec")
(autoload 'niwa-agent-exec-refresh-provider "niwa-agent-exec")
(autoload 'niwa-agent-exec-run "niwa-agent-exec")

;; `niwa-agent-persistence'
(declare-function niwa-agent-persistence-transcript-path-p
                  "niwa-agent-persistence" (path save-path))
(autoload 'niwa-agent-persistence-transcript-path-p
  "niwa-agent-persistence")

;; `niwa-execution'
(declare-function niwa-execution-owner-live-p
                  "niwa-execution" (session owner))
(declare-function niwa-execution-stop-owner
                  "niwa-execution" (session owner))
(autoload 'niwa-execution-owner-live-p "niwa-execution")
(autoload 'niwa-execution-stop-owner "niwa-execution")

;; `niwa-goal'
(declare-function niwa-goal-accounting-owner "niwa-goal" (&optional buffer))
(autoload 'niwa-goal-accounting-owner "niwa-goal")

;; `niwa-hooks'
(declare-function niwa-hooks-context-audit-records
                  "niwa-hooks" (decision event type &optional omit-context))
(declare-function niwa-hooks-context-entries
                  "niwa-hooks" (decision event))
(declare-function niwa-hooks-decision-reason
                  "niwa-hooks" (decision))
(declare-function niwa-hooks-event-plist
                  "niwa-hooks"
                  (event &optional session workspace &rest extra))
(declare-function niwa-hooks-format-context
                  "niwa-hooks" (entries))
(declare-function niwa-hooks-run-event
                  "niwa-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))
(autoload 'niwa-hooks-context-audit-records "niwa-hooks")
(autoload 'niwa-hooks-context-entries "niwa-hooks")
(autoload 'niwa-hooks-decision-reason "niwa-hooks")
(autoload 'niwa-hooks-event-plist "niwa-hooks")
(autoload 'niwa-hooks-format-context "niwa-hooks")
(autoload 'niwa-hooks-run-event "niwa-hooks")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-artifact-present-p
                  "niwa-session-artifacts"
                  (session logical &optional committed-only))
(declare-function niwa-session-artifacts-publish-agent-terminal-state
                  "niwa-session-artifacts" (invocation &optional token))
(autoload 'niwa-session-artifacts-artifact-present-p
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-publish-agent-terminal-state
  "niwa-session-artifacts")

;; `niwa-session-codec'
(declare-function niwa-session-codec-portable-authority-p
                  "niwa-session-codec" (session))
(autoload 'niwa-session-codec-portable-authority-p
  "niwa-session-codec")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-record-running-transcript
                  "niwa-session-persistence" (session entry))
(declare-function niwa-session-persistence-shallow-ensure-files
                  "niwa-session-persistence" (session buffer))
(declare-function niwa-session-persistence-update-transcript-entry
                  "niwa-session-persistence" (session agent-id updates))
(declare-function niwa-session-persistence-write-sidecar-now
                  "niwa-session-persistence"
                  (session buffer &optional prepared checkpoint token))
(autoload 'niwa-session-persistence-record-running-transcript
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-shallow-ensure-files
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-update-transcript-entry
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-write-sidecar-now
  "niwa-session-persistence")
(defvar niwa-session--read-only-mode)

;; `niwa-session-publication'
(declare-function niwa-session-publication-defer
                  "niwa-session-publication"
                  (session preparation &optional failure token))
(autoload 'niwa-session-publication-defer "niwa-session-publication")

;; `niwa-structs'
(declare-function niwa-session-agent-transcripts
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-p "niwa-structs" (cl-x))
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(declare-function niwa-session-turn-count "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-telemetry'
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))

;; `niwa-tool-task'
(declare-function niwa-tool-task-refresh-display "niwa-tool-task" ())
(declare-function niwa-tool-task-finalize-owner
                  "niwa-tool-task" (session owner status))
(autoload 'niwa-tool-task-finalize-owner "niwa-tool-task")
(autoload 'niwa-tool-task-refresh-display "niwa-tool-task")

;; `niwa-transcript-audit'
(declare-function niwa--format-hook-audit-record
                  "niwa-transcript-audit" (record))
(declare-function niwa--hook-prompt-rewrite-audit-record
                  "niwa-transcript-audit"
                  (event original submitted &optional reason))
(autoload 'niwa--format-hook-audit-record "niwa-transcript-audit")
(autoload 'niwa--hook-prompt-rewrite-audit-record
  "niwa-transcript-audit")
(defvar niwa-transcript-audit--buffer-record-bytes)
(defvar niwa-transcript-audit--buffer-records)

;; `niwa-turn'
(declare-function niwa-current-turn "niwa-turn" (session))
(declare-function niwa-request-end
                  "niwa-turn" (&optional abort-plan-approval))
(autoload 'niwa-current-turn "niwa-turn")
(autoload 'niwa-request-end "niwa-turn")

;; `niwa-view-agent'
(declare-function niwa-view-agent-live-transcript-finalize
                  "niwa-view-agent" (invocation))
(declare-function niwa-view-agent-live-transcript-start
                  "niwa-view-agent" (invocation))
(autoload 'niwa-view-agent-live-transcript-finalize
  "niwa-view-agent")
(autoload 'niwa-view-agent-live-transcript-start "niwa-view-agent")

;; `niwa-worktree'
(declare-function niwa-worktree-repository-root
                  "niwa-worktree" (directory))
(autoload 'niwa-worktree-repository-root "niwa-worktree")

(defconst niwa-agent-runtime--partial-max-chars (* 32 1024)
  "Maximum inline partial response size for an interrupted turn.")

(defvar niwa-agent-runtime--defer-terminal-publication-p nil
  "Non-nil while published terminal agent state is assembled in memory.")


;;
;;; Transcript persistence

(defun niwa-agent-runtime-abandon-persistence (invocation)
  "Drop incomplete persistence state for INVOCATION."
  (let ((session (niwa-agent-invocation-parent-session invocation))
        (agent-id (niwa-agent-invocation-agent-id invocation))
        (buffer (niwa-agent-invocation-buffer invocation))
        transcript)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq transcript buffer-file-name)
        (set-buffer-modified-p nil)
        (setq buffer-file-name nil)))
    (when (and transcript
               session
               (niwa-session-save-path session)
               (file-in-directory-p transcript
                                    (niwa-session-save-path session))
               (file-exists-p transcript))
      (delete-file transcript))
    (setf (niwa-agent-invocation-transcript-relative-path invocation) nil
          (niwa-agent-invocation-sidecar-dirty invocation) nil)
    (when (and session agent-id)
      (setf (niwa-session-agent-transcripts session)
            (assoc-delete-all
             agent-id (niwa-session-agent-transcripts session))))))

(defun niwa-agent-runtime--create-transcript (invocation agent-buffer session save-path)
  "Allocate, visit and register INVOCATION's transcript under SAVE-PATH.
AGENT-BUFFER visits the transcript; SESSION records it as running."
  (let* ((agent-id (niwa-agent-invocation-agent-id invocation))
         (agent
          (niwa-agent-invocation-agent invocation))
         (agent-type (or (and agent (niwa-agent-name agent))
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
     while (niwa-session-artifacts-artifact-present-p
            session candidate)
     finally (setq relative candidate absolute path))
    (with-current-buffer agent-buffer
      ;; Visiting renames the buffer after the file; keep its
      ;; hidden conversation name out of buffer lists.
      (let ((name (buffer-name)))
        (set-visited-file-name absolute t t)
        (rename-buffer name t)))
    (setf
     (niwa-agent-invocation-transcript-relative-path invocation)
     relative
     (niwa-agent-invocation-sidecar-dirty invocation) t)
    (let ((now (format-time-string "%FT%H-%M-%S")))
      (niwa-session-persistence-record-running-transcript
       session
       (cons agent-id
             (list :agent-type agent-type
                   :agent-path
                   (niwa-agent-invocation-path invocation)
                   :description
                   (niwa-agent-invocation-description invocation)
                   :path relative
                   :status 'running
                   :created-at now
                   :updated-at now
                   :parent-turn
                   (niwa-agent-invocation-parent-turn
                    invocation)))))))

(defun niwa-agent-runtime--setup-transcript (invocation agent-buffer)
  "Create and register INVOCATION's transcript for AGENT-BUFFER.
When none can be created, record why as INVOCATION's persistence failure."
  (let ((session (niwa-agent-invocation-parent-session invocation))
        (parent-buffer
         (niwa-agent-invocation-parent-data-buffer invocation)))
    (when-let* ((reason
                 (cond
                  ((not session) "no parent session")
                  ((not (buffer-live-p agent-buffer)) "agent buffer is not live")
                  ((not (buffer-live-p parent-buffer)) "parent buffer is not live")
                  ((buffer-local-value 'niwa-session--read-only-mode parent-buffer)
                   "parent session is read-only")
                  (t
                   (condition-case err
                       (if-let* ((save-path
                                  (niwa-session-persistence-shallow-ensure-files
                                   session parent-buffer)))
                           (progn
                             (niwa-agent-runtime--create-transcript
                              invocation agent-buffer session save-path)
                             nil)
                         "session files could not be created")
                     (error
                      (niwa-diagnostic-message
                       (niwa-diagnostic-scope session)
                       "Transcript persistence setup failed: %S" err)
                      (error-message-string err)))))))
      (setf (niwa-agent-invocation-persistence-failure invocation)
            (concat "transcript setup: " reason)))))


;;
;;; Terminal settlement

(defun niwa-agent-runtime--execution-live-p (invocation)
  "Return non-nil when INVOCATION still owns an execution."
  (when-let* ((session
               (niwa-agent-invocation-parent-session invocation)))
    (niwa-execution-owner-live-p
     session (niwa-agent-invocation-require-path invocation))))

(defun niwa-agent-runtime--transcript-path (invocation)
  "Return INVOCATION's qualified logical transcript path when published."
  (when-let* ((relative
               (niwa-agent-invocation-transcript-relative-path invocation))
              (session (niwa-agent-invocation-parent-session invocation))
              (save-path (niwa-session-save-path session)))
    (condition-case err
        (when (and
               (niwa-agent-persistence-transcript-path-p relative save-path)
               (niwa-session-artifacts-artifact-present-p session relative))
          (expand-file-name relative save-path))
      (error
       (niwa-diagnostic-message (niwa-diagnostic-scope session)
                                   "Transcript path validation failed: %S" err)
       nil))))

(defun niwa-agent-runtime--partial-text (invocation &optional fallback)
  "Return bounded partial output for INVOCATION or FALLBACK."
  (when-let* ((raw (or (ignore-errors
                         (niwa-agent-conversation-final-response invocation))
                       fallback))
              ((stringp raw))
              (text
               (string-trim
                (replace-regexp-in-string
                 "\\`[[:alpha:]]+ result for task: [^\n]*\n\n" "" raw)))
              ((not (string-empty-p text))))
    (if (> (length text) niwa-agent-runtime--partial-max-chars)
        (concat (substring text 0 niwa-agent-runtime--partial-max-chars)
                "\n\n[Partial response truncated.]")
      text)))

(defun niwa-agent-runtime--recovery-text (invocation &optional fallback)
  "Return useful recovery text for INVOCATION or FALLBACK."
  (let ((partial (niwa-agent-runtime--partial-text invocation fallback))
        (transcript (niwa-agent-runtime--transcript-path invocation)))
    (concat
     (if partial
         (format "\n\nPartial response:\n\n%s" partial)
       "\n\nNo partial response was available.")
     (if transcript
         (format "\n\nTranscript: %s\nRead it with: Read(file_path=%S)"
                 transcript transcript)
       ""))))

(defun niwa-agent-runtime--interrupted-response (invocation reason)
  "Return INVOCATION's interrupted result for REASON."
  (concat
   (format "Agent turn interrupted before finishing task \"%s\".\n\nReason: %s\nAgent path: %s"
           (or (niwa-agent-invocation-description invocation) "")
           reason
           (or (niwa-agent-invocation-path invocation) "unknown"))
   "\n\nTools or commands may have partially changed state. Reconcile current state and verify effects before retrying or claiming success."
   (niwa-agent-runtime--recovery-text invocation)))

(defun niwa-agent-runtime--error-response (invocation event)
  "Return INVOCATION's parent-visible error response for EVENT."
  (let* ((agent (niwa-agent-invocation-agent invocation))
         (agent-type (or (and agent (niwa-agent-name agent)) "agent"))
         (details (or (plist-get event :error-details)
                      (niwa-agent-invocation-terminal-reason invocation)
                      "Agent turn failed")))
    (concat
     (format "Error: Task %s could not finish task \"%s\".\n\nError details: %S\nAgent path: %s"
             agent-type
             (or (niwa-agent-invocation-description invocation) "")
             details
             (or (niwa-agent-invocation-path invocation) "unknown"))
     (niwa-agent-runtime--recovery-text
      invocation (plist-get event :fallback-partial)))))

(defun niwa-agent-runtime--finalize-step (invocation label function)
  "Run terminal FUNCTION for INVOCATION, warning under LABEL on failure."
  (condition-case err
      (funcall function)
    (error
     (niwa-diagnostic-warning
      (niwa-diagnostic-scope
       (niwa-agent-invocation-parent-session invocation)) 'niwa
      (format "Agent finalization step %s failed for %s: %s"
              label
              (or (niwa-agent-invocation-path invocation) "unknown")
              (error-message-string err))
      :warning))))

(defun niwa-agent-runtime--finalize (invocation status &optional token)
  "Persist terminal STATUS and lifecycle effects for INVOCATION.
TOKEN is the caller's publication gate ownership, handed to gated writers."
  ;; `incomplete' is terminal like the other three: finalizing over it
  ;; would run terminal lifecycle effects twice for a hydrated agent.
  (unless (or (niwa-agent-conversation-fenced-p invocation)
              (memq (niwa-agent-invocation-transcript-status invocation)
                    '(completed error aborted incomplete)))
    (let ((buffer (niwa-agent-invocation-buffer invocation))
          (session (niwa-agent-invocation-parent-session invocation))
          (parent-buffer
           (niwa-agent-invocation-parent-data-buffer invocation)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (niwa-request-end)))
      (when session
        (niwa-execution-stop-owner
         session (niwa-agent-invocation-require-path invocation)))
      (setf (niwa-agent-invocation-transcript-status invocation) status)
      (niwa-agent-runtime--finalize-step
       invocation 'transcript-status
       (lambda ()
         (when session
           (niwa-session-persistence-update-transcript-entry
            session (niwa-agent-invocation-agent-id invocation)
            (list :status status
                  :updated-at (format-time-string "%FT%H-%M-%S"))))))
      (unless niwa-agent-runtime--defer-terminal-publication-p
        (niwa-agent-runtime--finalize-step
         invocation 'transcript-save
         (lambda () (niwa-agent-conversation-save invocation nil token))))
      (niwa-agent-runtime--finalize-step
       invocation 'activity
       (lambda ()
         (niwa-agent-conversation-record-activity
          invocation (list :type 'status :status status
                           :summary (symbol-name status))
          t)))
      (niwa-agent-runtime--finalize-step
       invocation 'tasks
       (lambda ()
         (when (and session (eq status 'completed))
           (when (niwa-tool-task-finalize-owner
                  session (niwa-agent-invocation-require-path invocation)
                  status)
             (when (buffer-live-p parent-buffer)
               (with-current-buffer parent-buffer
                 (niwa-tool-task-refresh-display)))))))
      (niwa-agent-runtime--finalize-step
       invocation 'handle
       (lambda () (niwa-agent-conversation-refresh invocation)))
      (unless niwa-agent-runtime--defer-terminal-publication-p
        (niwa-agent-runtime--finalize-step
         invocation 'sidecar
         (lambda ()
           (when (and session (buffer-live-p parent-buffer))
             (niwa-session-persistence-write-sidecar-now
              session parent-buffer nil nil token)))))
      (setf (niwa-agent-invocation-activity invocation) nil)
      (niwa-agent-runtime--finalize-step
       invocation 'hook
       (lambda () (niwa-agent-runtime--run-stop-hook invocation status)))
      (niwa-agent-runtime--finalize-step
       invocation 'view
       (lambda ()
         (when (fboundp 'niwa-view-agent-live-transcript-finalize)
           (niwa-view-agent-live-transcript-finalize invocation)))))))

(defun niwa-agent-runtime--settle
    (invocation response &optional event token acknowledgement)
  "Finalize INVOCATION and deliver RESPONSE and EVENT exactly once.
TOKEN proves the caller already owns the session's publication gate, as a
queued preparation does.  Without it, a held gate queues settlement as a
preparation that runs before any registry mutation or sidecar capture, in the
buffer that requested it, and a failure is reported without losing the turn.
ACKNOWLEDGEMENT receives (SUCCESS ERROR) exactly once after durable settlement
or definitive failure, including when the preparation runs later."
  (let* (acknowledged
         (acknowledge
          (lambda (success error)
            (unless acknowledged
              (setq acknowledged t)
              (when acknowledgement
                (funcall acknowledgement success error))))))
    (condition-case err
        (if (niwa-agent-invocation-runtime-settled-p invocation)
            (funcall acknowledge t nil)
          (cond
           ((memq invocation niwa-agent-control-settling)
            ;; A nested terminal callback for the same turn; the outer call
            ;; reports when settlement ends.
            (prog1 (niwa-agent-runtime--settle-1 invocation response event token)
              (funcall acknowledge t nil)))
           ((niwa-agent-runtime--defer-settlement
             invocation response event token acknowledge)
            ;; Queued: the turn is not settled yet, so report no response.
            nil)
           (t
            (prog1
                (unwind-protect
                    (let ((niwa-agent-control-settling
                           (cons invocation niwa-agent-control-settling)))
                      (niwa-agent-runtime--settle-1 invocation response event token))
                  (when-let* ((session (niwa-agent-invocation-parent-session invocation)))
                    (niwa-agent-control-settlement-finished session)))
              (funcall acknowledge t nil)))))
      ((error quit)
       ;; Post-commit notification/cleanup failure cannot undo authoritative
       ;; adoption.  Before that point, the producer must retain its output.
       (if (or (niwa-agent-invocation-runtime-settled-p invocation)
               (let ((pending (niwa-agent-invocation-runtime-pending-response
                               invocation)))
                 (and (listp pending) (eq 'committed (plist-get pending :phase)))))
           (funcall acknowledge t nil)
         (funcall acknowledge nil (error-message-string err)))
       (signal (car err) (cdr err))))))

(defun niwa-agent-runtime--defer-until-gate
    (invocation what function &optional token failure)
  "Queue FUNCTION for INVOCATION behind its session's publication gate.
Return non-nil when queued, nil when the gate is open and the caller proceeds.
The publication owner decides whether TOKEN proves the caller already owns
the gate; such a caller is not queued.  FUNCTION receives the fresh token of
its queued run, before any registry mutation or sidecar capture, in the
buffer that requested it.  A failure is passed to FAILURE, if supplied, and
warned as WHAT, never dropped silently."
  (when-let* ((session (niwa-agent-invocation-parent-session invocation))
              ((not (niwa-agent-conversation-fenced-p invocation))))
    (let ((buffer (current-buffer))
          (scope (niwa-diagnostic-scope session)))
      (niwa-session-publication-defer
       session
       (lambda (queued-token)
         (with-current-buffer (if (buffer-live-p buffer) buffer
                                (current-buffer))
           (funcall function queued-token)))
       (lambda (err)
         (when failure (funcall failure err))
         (niwa-diagnostic-warning
          scope 'niwa
          (format "Deferred %s of %s failed: %s" what
                  (or (niwa-agent-invocation-path invocation) "unknown")
                  (error-message-string err))
          :warning))
       token))))

(defun niwa-agent-runtime--defer-settlement
    (invocation response event &optional token acknowledgement)
  "Queue INVOCATION's settlement with RESPONSE and EVENT behind the gate.
Return non-nil when queued; the failure leaves the invocation unsettled.
A caller holding TOKEN is never queued.  ACKNOWLEDGEMENT follows settlement
or a failed preparation exactly once.  Teardown drains accepted preparations."
  (let* (acknowledged
         (acknowledge
          (lambda (success error)
            (unless acknowledged
              (setq acknowledged t)
              (when acknowledgement
                (funcall acknowledgement success error))))))
    (niwa-agent-runtime--defer-until-gate
     invocation "settlement"
     (lambda (queued-token)
       (niwa-agent-runtime--settle
        invocation response event queued-token acknowledge))
     token
     (lambda (err)
       (funcall acknowledge nil (error-message-string err))))))

(cl-defun niwa-agent-runtime--settle-1 (invocation response event token)
  "Finalize INVOCATION and deliver RESPONSE and EVENT; see the caller.
TOKEN, possibly nil, is the gate ownership its mutations hand on explicitly."
  (let* ((fenced-p (niwa-agent-conversation-fenced-p invocation))
         (pending
          (niwa-agent-invocation-runtime-pending-response invocation))
         (committed-p
          (and (not fenced-p) (listp pending)
               (eq 'committed (plist-get pending :phase))))
         status visible transaction terminal-publication-p)
    (if committed-p
        (setq status (plist-get pending :status)
              visible (plist-get pending :visible)
              transaction (plist-get pending :transaction))
      (when (and (listp pending) (plist-member pending :response))
        (setq response (plist-get pending :response)
              event (plist-get pending :event)))
      (setq pending (list :response response :event event))
      (setf (niwa-agent-invocation-runtime-pending-response invocation)
            pending)
      (let* ((event-status
              (and (listp event)
                   (plist-get event :niwa-agent-terminal-status)))
             (callback
              (niwa-agent-invocation-runtime-settle-callback invocation))
             (session
              (niwa-agent-invocation-parent-session invocation))
             (save-path (and session
                             (niwa-session-save-path session))))
        (setq status (pcase (if fenced-p 'aborted event-status)
                       ('error 'error)
                       ('aborted 'aborted)
                       (_ 'completed))
              visible
              (pcase status
                ('error
                 (niwa-agent-runtime--error-response invocation event))
                ('aborted
                 (niwa-agent-runtime--interrupted-response
                  invocation
                  (or (niwa-agent-invocation-terminal-reason invocation)
                      "interrupted")))
                (_ response))
              terminal-publication-p
              (and (not fenced-p) session
                   save-path
                   (niwa-session-artifacts-artifact-present-p
                    session "session.meta.el")
                   (niwa-agent-invocation-transcript-relative-path
                    invocation)
                   (buffer-live-p
                    (niwa-agent-invocation-buffer invocation))
                   (buffer-live-p
                    (niwa-agent-invocation-parent-data-buffer invocation))
                   (niwa-session-execution-target session)
                   ;; Portable authority resolves the transcript through
                   ;; the publication, local target included: a direct
                   ;; write would leave resume unable to see it.
                   (niwa-session-codec-portable-authority-p session)))
        (when (eq status 'error)
          (let ((detail (plist-get event :error-details)))
            (niwa-recovery-report
             session (concat "agent:" (niwa-agent-invocation-agent-id invocation))
             (niwa-recovery-category (and (stringp detail) detail))
             (format "Agent %s failed; inspect its transcript before continuing"
                     (niwa-agent-invocation-require-path invocation)) nil)))
        (if fenced-p
            (progn
              (niwa-agent-conversation--cancel-save invocation)
              (setf (niwa-agent-invocation-transcript-status invocation) 'aborted)
              (when-let* ((buffer (niwa-agent-invocation-buffer invocation))
                          ((buffer-live-p buffer))
                          ((eq invocation (buffer-local-value 'niwa--agent-invocation buffer))))
                (let ((niwa-agent-control-suppress-persistence t))
                  (with-current-buffer buffer (niwa-request-end)))))
          (let ((niwa-agent-runtime--defer-terminal-publication-p
                 terminal-publication-p))
            (niwa-agent-runtime--finalize invocation status token)))
        (when callback
          (let ((niwa-agent-control-suppress-persistence t))
            (setq transaction
                  (funcall callback invocation visible event token))))
        (condition-case err
            (progn
              (unless (niwa-agent-conversation-fenced-p invocation)
                (if terminal-publication-p
                    (niwa-session-artifacts-publish-agent-terminal-state
                     invocation token)
                  (when (consp transaction)
                    (niwa-agent-control-commit-session session token))))
              (when (and (not fenced-p) (niwa-agent-conversation-fenced-p invocation))
                (when (functionp (car-safe transaction)) (funcall (car transaction)))
                (setq transaction nil)
                (cl-return-from niwa-agent-runtime--settle-1
                  (niwa-agent-runtime--settle-1 invocation response event token))))
          ;; A quit must not leave the staged idle record and its
          ;; uncommitted RESULT behind either.
          ((error quit)
           (when (functionp (car-safe transaction))
             (funcall (car transaction)))
           (signal (car err) (cdr err))))
        (setq pending
              (list :phase 'committed :status status :visible visible
                    :transaction transaction))
        (setf (niwa-agent-invocation-runtime-pending-response invocation)
              pending)))
    (when (functionp (cdr-safe transaction))
      (funcall (cdr transaction)))
    (setf (niwa-agent-invocation-runtime-settled-p invocation) t
          (niwa-agent-invocation-runtime-fsm invocation) nil
          (niwa-agent-invocation-runtime-cancel invocation) nil
          (niwa-agent-invocation-runtime-pending-response invocation) nil)
    (niwa-agent-runtime--release-settled-state invocation)
    (when-let* ((timer
                 (niwa-agent-invocation-runtime-budget-timer invocation)))
      (cancel-timer timer)
      (setf (niwa-agent-invocation-runtime-budget-timer invocation) nil))
    (when-let* ((session
                 (niwa-agent-invocation-parent-session invocation)))
      (niwa-telemetry-record
       session 'agent-settled
       :agent-id (niwa-agent-invocation-agent-id invocation)
       :agent-path (niwa-agent-invocation-path invocation)
       :agent-type
       (and (niwa-agent-invocation-agent invocation)
            (niwa-agent-name
             (niwa-agent-invocation-agent invocation)))
       :outcome status
       :duration-ms
       (when-let* ((started-at
                    (niwa-agent-invocation-started-at invocation)))
         (round
          (* 1000.0
             (float-time (time-subtract (current-time) started-at)))))))
    visible))

(defun niwa-agent-runtime--release-settled-state (invocation)
  "Drop memory a settled INVOCATION's buffer no longer needs.
A retained agent can sit idle for hours.  Its last request's serialized
payload serves only request inspection and a decoding memo is rebuilt on
demand, yet together they kept as much Lisp data alive as the transcript
itself, making every later garbage collection slower.  A continuation
builds a fresh request.  A retried settlement can run after the agent's
next turn started in the same buffer; that turn's state is left alone."
  (when-let* ((context (niwa-agent-invocation-runtime-context invocation)))
    (when-let* ((start (plist-get context :niwa-agent-prompt-start)))
      (set-marker start nil))
    (dolist (key '(:niwa-agent-prompt :niwa-agent-prompt-start))
      (when (plist-get context key) (plist-put context key nil))))
  (when-let* ((buffer (niwa-agent-invocation-buffer invocation))
              ((buffer-live-p buffer))
              ((eq invocation (buffer-local-value 'niwa--agent-invocation
                                                  buffer))))
    (with-current-buffer buffer
      (when-let* ((fsm (bound-and-true-p gptel--fsm-last)))
        (setf (gptel-fsm-info fsm) (plist-put (gptel-fsm-info fsm) :data nil)))
      (setq niwa-transcript-audit--buffer-records nil
            niwa-transcript-audit--buffer-record-bytes 0))))

(defun niwa-agent-runtime--budget-expired (invocation seconds)
  "Interrupt INVOCATION after its completion budget of SECONDS expires."
  (setf (niwa-agent-invocation-runtime-budget-timer invocation) nil)
  (unless (niwa-agent-invocation-runtime-settled-p invocation)
    (niwa-agent-runtime-interrupt
     invocation (format "Goal investigation budget expired after %d seconds"
                        seconds))))

(defun niwa-agent-runtime-bound-turn (invocation seconds)
  "Bound INVOCATION to SECONDS and return it."
  (unless (and (integerp seconds) (> seconds 0))
    (error "Agent completion budget must be positive"))
  (setf (niwa-agent-invocation-runtime-budget-timer invocation)
        (run-at-time seconds nil
                     #'niwa-agent-runtime--budget-expired
                     invocation seconds))
  invocation)

(defun niwa-agent-runtime--handle-provider-result (invocation response)
  "Settle INVOCATION from provider RESPONSE, or hold for yielded Bash."
  (if (and (stringp response)
           (niwa-agent-runtime--execution-live-p invocation))
      (setf (niwa-agent-invocation-runtime-pending-response invocation)
            response)
    (niwa-agent-runtime--settle invocation response
                                   (and (listp response) response))))

(defun niwa-agent-runtime-queue-execution-completion
    (context owner body &optional meta acknowledgement)
  "Secure yielded Bash BODY for invocation CONTEXT owned by OWNER.
META carries structural facts for the delivered record's opening tag.
While the agent still works, BODY goes to its own mailbox and reaches it at
its next provider request.  Once it has answered, BODY goes to its caller with
the result, and a held response settles after its last owned execution.
ACKNOWLEDGEMENT, called with SUCCESS and ERROR exactly once, runs when the
mailbox secures BODY or definitively fails; a held response with other live
executions acknowledges after BODY joins its results, while the last execution
waits for durable terminal settlement.  Return `pending' when acknowledgement
runs later, t when it already ran, and nil, after acknowledging failure,
when CONTEXT no longer accepts BODY."
  (if (not (and (niwa-agent-invocation-p context)
                (equal owner (niwa-agent-invocation-require-path context))
                (stringp body)
                (not (niwa-agent-invocation-runtime-settled-p context))))
      (progn
        (when acknowledgement
          (funcall acknowledgement nil "Execution owner no longer accepts results"))
        nil)
    (if-let* ((response
               (niwa-agent-invocation-runtime-pending-response context)))
        (let ((entry (cons body meta)) acknowledged)
          (push entry
                (niwa-agent-invocation-runtime-execution-results context))
          (if (niwa-agent-runtime--execution-live-p context)
              (progn
                (when acknowledgement (funcall acknowledgement t nil))
                t)
            (niwa-agent-runtime--settle
             context response nil nil
             (lambda (success error)
               (unless acknowledged
                 (setq acknowledged t)
                 (unless success
                   ;; Retry must stage this output only once, without removing
                   ;; a sibling completion retained by the held response.
                   (setf (niwa-agent-invocation-runtime-execution-results context)
                         (delq entry (niwa-agent-invocation-runtime-execution-results
                                      context))))
                 (when acknowledgement
                   (funcall acknowledgement success error)))))
            (if acknowledged t 'pending)))
      (if (eq (niwa-agent-control-enqueue-execution-result
               (niwa-agent-invocation-parent-session context) owner body
               nil nil meta acknowledgement)
              'pending)
          'pending
        t))))


;;
;;; Interruption

(defun niwa-agent-runtime--request-live-p (invocation)
  "Return non-nil when INVOCATION has an active provider request."
  (let ((buffer (niwa-agent-invocation-buffer invocation)))
    (and (buffer-live-p buffer)
         (boundp 'gptel--request-alist)
         (cl-some
          (lambda (entry)
            (let* ((fsm (cadr entry))
                   (info (and fsm (gptel-fsm-info fsm))))
              (eq (and info (plist-get info :buffer)) buffer)))
          gptel--request-alist))))

(defun niwa-agent-runtime-interrupt (invocation reason &optional token)
  "Interrupt INVOCATION for REASON and settle its turn once.
TOKEN is the caller's publication gate ownership; without it a held gate
queues the whole interrupt.
A native runtime canceller may return `deferred' when it owns asynchronous
terminal acknowledgement and will settle the invocation itself."
  (unless (niwa-agent-invocation-p invocation)
    (error "Interrupt target has no live invocation"))
  (if (niwa-agent-invocation-runtime-settled-p invocation)
      (niwa-agent-runtime--interrupted-response invocation reason)
    (if (niwa-agent-runtime--defer-until-gate
         invocation "interrupt"
         (lambda (queued-token)
           (niwa-agent-runtime-interrupt invocation reason queued-token))
         token)
        ;; The abort and the settlement must stay together: aborting the
        ;; provider request now would leave a dead window in which the turn
        ;; still reads as live.  Both happen once the gate opens.
        (niwa-agent-runtime--interrupted-response invocation reason)
    ;; Mail queued before the interrupt must not restart the stopped agent;
    ;; mail sent afterwards still does.
    (when-let* ((session (niwa-agent-invocation-parent-session invocation))
                (path (niwa-agent-invocation-path invocation)))
      (niwa-agent-control-clear-agent-wakes session path))
    (let* ((buffer (niwa-agent-invocation-buffer invocation))
           (fsm (niwa-agent-invocation-runtime-fsm invocation))
           (response (niwa-agent-runtime--interrupted-response
                      invocation reason))
           (previous-reason
            (niwa-agent-invocation-terminal-reason invocation))
           deferred)
      (setf (niwa-agent-invocation-terminal-reason invocation) reason)
      (condition-case err
          (if-let* ((cancel (niwa-agent-invocation-runtime-cancel invocation)))
              (setq deferred (eq 'deferred (funcall cancel)))
            (when (niwa-agent-runtime--request-live-p invocation)
              (let* ((info (gptel-fsm-info fsm))
                     (provider-callback (plist-get info :callback)))
                (unwind-protect
                    (progn
                      (setf (gptel-fsm-info fsm)
                            (plist-put info :callback #'ignore))
                      (gptel-abort buffer))
                  (setf (gptel-fsm-info fsm)
                        (plist-put (gptel-fsm-info fsm)
                                   :callback provider-callback))))))
        (error
         (setf (niwa-agent-invocation-terminal-reason invocation)
               previous-reason)
         (signal (car err) (cdr err))))
      (or (unless deferred
            (niwa-agent-runtime--settle
             invocation response
             (list :niwa-agent-terminal-status 'aborted :response response)
             token))
          response)))))


;;
;;; Turn hooks

(defun niwa-agent-runtime--run-prompt-hook (prompt invocation)
  "Run `UserPromptSubmit' for agent PROMPT and wait for its decision."
  (let* ((session (niwa-agent-invocation-parent-session invocation))
         (workspace (and session (niwa-session-workspace session)))
         done decision)
    (niwa-hooks-run-event
     'UserPromptSubmit
     (niwa-hooks-event-plist
      'UserPromptSubmit session workspace
      :agent-path (niwa-agent-invocation-path invocation)
      :prompt prompt
      :display-text prompt)
     (lambda (result)
       (setq decision result
             done t))
     session workspace nil invocation)
    (while (not done)
      (accept-process-output nil 0.05))
    decision))

(defun niwa-agent-runtime--run-stop-hook (invocation status)
  "Fire `SubagentStop' hooks for INVOCATION terminal STATUS."
  (when (niwa-agent-invocation-p invocation)
    (let* ((session (niwa-agent-invocation-parent-session invocation))
           (workspace (and session (niwa-session-workspace session)))
           (agent (niwa-agent-invocation-agent invocation))
           (agent-type (and agent (niwa-agent-name agent)))
           (parent-buffer
            (niwa-agent-invocation-parent-data-buffer invocation))
           (runner
            (lambda ()
              (niwa-hooks-run-event
               'SubagentStop
               (niwa-hooks-event-plist
                'SubagentStop session workspace
                :agent-path (niwa-agent-invocation-path invocation)
                :role agent-type
                :description
                (niwa-agent-invocation-description invocation)
                :status status
                :terminal-reason
                (niwa-agent-invocation-terminal-reason invocation)
                :transcript-relative-path
                (niwa-agent-invocation-transcript-relative-path
                 invocation))
               #'ignore
               session workspace nil invocation))))
      (if (and parent-buffer (buffer-live-p parent-buffer))
          (with-current-buffer parent-buffer
            (funcall runner))
        (funcall runner)))))

(defun niwa-agent-runtime--prepare-followup
    (prompt invocation pending-hook-context on-hook-context)
  "Prepare one retained follow-up PROMPT for INVOCATION.
PENDING-HOOK-CONTEXT belongs to the conversation; ON-HOOK-CONTEXT records a
blocked transition."
  (let* ((prompt-decision
          (niwa-agent-runtime--run-prompt-hook prompt invocation))
         (prompt-context
          (niwa-hooks-context-entries
           prompt-decision 'UserPromptSubmit)))
    (when (and (plist-member prompt-decision :continue)
               (not (plist-get prompt-decision :continue))
               on-hook-context)
      (funcall on-hook-context
               (append pending-hook-context prompt-context)))
    (niwa-agent-runtime--prepared-turn
     prompt nil prompt-decision pending-hook-context)))

(defun niwa-agent-runtime--prepared-turn
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
    (message "niwa: %s" msg))
  (let* ((submitted
          (if (stringp (plist-get prompt-decision :updated-input))
              (plist-get prompt-decision :updated-input)
            prompt))
         (context
          (niwa-hooks-format-context
           (append
            (niwa-hooks-context-entries
             start-decision 'SubagentStart)
            pending-hook-context
            (niwa-hooks-context-entries
             prompt-decision 'UserPromptSubmit))))
         (effective-prompt
          (if context (concat submitted "\n\n" context) submitted))
         (rewrite-audit
          (niwa--hook-prompt-rewrite-audit-record
           'UserPromptSubmit prompt submitted
           (niwa-hooks-decision-reason prompt-decision))))
    (list :prompt effective-prompt
          :audits (and rewrite-audit (list rewrite-audit)))))

(cl-defun niwa-agent-runtime-prepare-task
    (agent description prompt path callback
           &key skill-permission-rules cancelled-p)
  "Prepare one new AGENT task asynchronously and call CALLBACK.
PATH is the reserved canonical child path.  CALLBACK receives an outcome
plist carrying either `:turn' and `:start-hook-audits' or `:error'."
  (let* ((session (and (boundp 'niwa--session) niwa--session))
         (workspace (and session (niwa-session-workspace session)))
         (invocation (niwa-agent-invocation-create agent))
         settled)
    (setf (niwa-agent-invocation-path invocation) path
          (niwa-agent-invocation-description invocation) description
          (niwa-agent-invocation-parent-session invocation) session
          (niwa-agent-invocation-parent-data-buffer invocation)
          (current-buffer)
          (niwa-agent-invocation-parent-turn invocation)
          (and session (niwa-current-turn session))
          (niwa-agent-invocation-plan-read-only invocation)
          (niwa-plan-read-only-p session))
    (when skill-permission-rules
      (setf (niwa-agent-invocation-skill-permission-rules invocation)
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
                     (message "niwa: %s" msg))
                   (setf (niwa-agent-invocation-hook-audits invocation)
                         (niwa-hooks-context-audit-records
                          start-decision 'SubagentStart
                          'subagent-context t))
                   (when (and (plist-member start-decision :continue)
                              (not (plist-get start-decision :continue)))
                     (error "%s"
                            (or (plist-get start-decision :stop-reason)
                                "SubagentStart hook stopped sub-agent")))
                   (niwa-hooks-run-event
                    'UserPromptSubmit
                    (niwa-hooks-event-plist
                     'UserPromptSubmit session workspace
                     :agent-path path :prompt prompt :display-text prompt)
                    (lambda (prompt-decision)
                      (unless (or settled (cancelled))
                        (condition-case prompt-error
                            (finish
                             (list
                              :outcome 'success
                              :turn
                              (niwa-agent-runtime--prepared-turn
                               prompt start-decision prompt-decision nil)
                              :start-hook-audits
                              (niwa-agent-invocation-hook-audits
                               invocation)))
                          (error (fail prompt-error)))))
                    session workspace nil invocation))
               (error (fail err))))))
      (condition-case err
          (niwa-hooks-run-event
           'SubagentStart
           (niwa-hooks-event-plist
            'SubagentStart session workspace
            :agent-path path
            :role (niwa-agent-name agent)
            :description description
            :prompt prompt
            :transcript-relative-path nil)
           #'prepare-prompt session workspace nil invocation)
        (error (fail err))))))


;;
;;; Provider dispatch

(defun niwa-agent-runtime-task-background (summary)
  "Wrap SUMMARY as a distinct, advisory child task-background block."
  (unless (and (stringp summary) (not (string-blank-p summary)))
    (error "Agent task background must be non-empty text"))
  (format (concat "<task-background>\n"
                  "Generated background may be stale or untrusted. "
                  "The following Agent Task is authoritative.\n\n%s\n"
                  "</task-background>\n")
          (string-trim summary)))

(defun niwa-agent-runtime--insert-prompt
    (invocation buffer description prompt context-snapshot retained-p
                &optional hook-audits token)
  "Append PROMPT and optional CONTEXT-SNAPSHOT to INVOCATION's BUFFER.
TOKEN is the caller's publication gate ownership for the persisting save.
HOOK-AUDITS are hidden transcript records associated with this user turn.
Return a marker at the start of the turn's Agent Task heading."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (start (point-max))
          (was-modified (buffer-modified-p))
          heading)
      (goto-char (point-max))
      (when (and (not retained-p)
                 (stringp context-snapshot)
                 (not (string-empty-p context-snapshot)))
        (insert context-snapshot)
        (unless (bolp) (insert "\n")))
      (unless (bobp) (insert "\n"))
      (setq heading (point-marker))
      (insert (format "* Agent Task: %s\n" (or description "")))
      (unless retained-p
        (insert (format ":PROPERTIES:\n:%s: %s\n:END:\n"
                        niwa-agent-task-path-property
                        (niwa-agent-invocation-require-path invocation))))
      (insert "\n" (or prompt "") "\n")
      (when hook-audits
        (dolist (audit hook-audits)
          (insert (niwa--format-hook-audit-record audit))))
      (when (niwa-agent-invocation-transcript-relative-path invocation)
        (unless (niwa-agent-conversation-save invocation nil token)
          (if retained-p
              (progn
                (delete-region start (point-max))
                (set-buffer-modified-p was-modified)
                (error "Retained agent conversation could not be persisted: %s"
                       (or (niwa-agent-invocation-persistence-failure invocation)
                           "save did not complete")))
            (niwa-agent-runtime-abandon-persistence
             invocation))))
      heading)))

(cl-defun niwa-agent-runtime-dispatch
    (agent description prompt
           &key context-snapshot model-policy skill-permission-rules
           prepared-turn start-hook-audits
           on-invocation on-settle path frozen-configuration
           retained-id retained-buffer retained-transcript
           pending-hook-context on-hook-context parent-tool-use-id
           plan-read-only token working-directory)
  "Start one asynchronous retained agent turn and return its invocation.
AGENT starts a new conversation; FROZEN-CONFIGURATION and the three retained
identity values continue one.  PATH is the conversation's canonical address.
ON-SETTLE receives (INVOCATION RESPONSE EVENT TOKEN) exactly once.
TOKEN, when the caller runs under a queued publication preparation, is its
gate ownership: every persisting step of the turn setup and of its error
settlement carries it, so the error cleanup cannot wait on its own gate.
The turn is Plan read-only when PLAN-READ-ONLY is non-nil or its caller
works under Plan read-only authority.  WORKING-DIRECTORY is the retained
conversation's optional execution directory, never a resource grant."
  (unless (and (stringp path) (string-match-p "\\`/root/" path))
    (error "Agent requires a canonical path below /root"))
  (when (and frozen-configuration
             (not (niwa-agent-configuration-p frozen-configuration)))
    (error "Invalid frozen agent configuration"))
  (let ((retained-values (delq nil (list retained-id retained-buffer
                                         retained-transcript))))
    (when (and retained-values (/= (length retained-values) 3))
      (error "Retained agent identity requires id, buffer, and transcript")))
  (let* ((retained-p retained-id)
         (agent (if frozen-configuration
                    (niwa-agent-configuration-agent frozen-configuration)
                  agent))
         (_ (unless agent (error "Agent configuration is required")))
         (agent-type (niwa-agent-name agent))
         (agent-id
          (or retained-id
              (format "%s--%s" agent-type
                      (md5 (format "%s%s%s%s" (system-name) (emacs-pid)
                                   (current-time) (random))))))
         (invocation (niwa-agent-invocation-create agent))
         (parent-buffer (current-buffer))
         (session (and (boundp 'niwa--session) niwa--session))
         (previous-issue
          (and session retained-p
               (cl-find (concat "agent:" agent-id)
                        (niwa-session-recovery-issues session)
                        :key (lambda (issue) (plist-get issue :id))
                        :test #'equal)))
         publication-ready)
    (unless (niwa-session-p session)
      (error "Agent requires an active niwa session"))
    (when (memq session (bound-and-true-p niwa-claude-code-fork--sessions))
      (niwa-diagnostic-user-error
       (niwa-diagnostic-scope session)
       "Wait for native history preparation before starting a turn"))
    (unless (or retained-p prepared-turn)
      (error "Initial agent turn requires prepared task"))
    (when working-directory
      (setq working-directory
            (niwa-agent-directory-resolve session working-directory)))
    (setf (niwa-agent-invocation-agent-id invocation) agent-id
          (niwa-agent-invocation-working-directory invocation) working-directory
          (niwa-agent-invocation-path invocation) path
          (niwa-agent-invocation-description invocation) description
          (niwa-agent-invocation-parent-session invocation) session
          (niwa-agent-invocation-parent-data-buffer invocation) parent-buffer
          (niwa-agent-invocation-parent-turn invocation)
          (niwa-current-turn session)
          (niwa-agent-invocation-goal-owner invocation)
          (niwa-goal-accounting-owner parent-buffer)
          (niwa-agent-invocation-plan-read-only invocation)
          (or plan-read-only (niwa-plan-read-only-p session))
          (niwa-agent-invocation-parent-tool-use-id invocation)
          parent-tool-use-id
          (niwa-agent-invocation-sandbox-summary-cell invocation)
          (list nil)
          (niwa-agent-invocation-transcript-status invocation) 'running
          (niwa-agent-invocation-runtime-settle-callback invocation)
          (lambda (settled-invocation response event token)
            (if publication-ready
                (when on-settle
                  (funcall on-settle settled-invocation response event token))
              (error "Agent invocation publication is pending"))))
    (niwa-telemetry-record
     session 'agent-dispatch
     :agent-id agent-id
     :agent-path path
     :agent-type agent-type
     :retained (and retained-p t)
     :parent-turn (niwa-agent-invocation-parent-turn invocation))
    (when skill-permission-rules
      (setf (niwa-agent-invocation-skill-permission-rules invocation)
            skill-permission-rules))
    (let* ((niwa--conversation-working-directory working-directory)
           (configuration
            (or frozen-configuration
                (niwa-agent-exec-freeze-configuration
                 agent-type invocation model-policy))))
      (setf (niwa-agent-invocation-frozen-configuration invocation)
            configuration
            (niwa-agent-invocation-agent invocation)
            (niwa-agent-configuration-agent configuration)))
    (let ((buffer
           (if retained-p
               (if (buffer-live-p retained-buffer)
                   retained-buffer
                 (error "Retained agent buffer is not live"))
             (niwa-agent-conversation-open
              invocation parent-buffer))))
      (setf (niwa-agent-invocation-buffer invocation) buffer)
      (if retained-p
          (progn
            (setf (niwa-agent-invocation-transcript-relative-path invocation)
                  retained-transcript)
            (with-current-buffer buffer
              (setq-local niwa--agent-invocation invocation)))
        (niwa-agent-runtime--setup-transcript invocation buffer))
      (setf (niwa-agent-invocation-ownership-epoch invocation)
            (niwa-session-ownership-epoch session))
      (niwa-agent-conversation-configure invocation buffer)
      ;; Hydration itself performs no directory probes.  Discover a linked
      ;; checkout's instructions only after this turn validated the target and
      ;; current repository membership above.
      (when working-directory
        (with-current-buffer buffer
          (unless (file-in-directory-p
                   working-directory
                   (niwa-workspace-root (niwa-session-workspace session)))
            (setq-local niwa--conversation-instruction-root
                        (niwa-worktree-repository-root working-directory)))))
      (let (published-p)
        (condition-case err
            (let ((turn
                   (if retained-p
                       (niwa-agent-runtime--prepare-followup
                        prompt invocation pending-hook-context
                        on-hook-context)
                     prepared-turn)))
              (when start-hook-audits
                (setf (niwa-agent-invocation-hook-audits invocation)
                      start-hook-audits))
              (setf (niwa-engine-info invocation)
                    (plist-put (niwa-engine-info invocation)
                               :niwa-agent-prompt-start
                               (niwa-agent-runtime--insert-prompt
                                invocation buffer description (plist-get turn :prompt)
                                context-snapshot retained-p (plist-get turn :audits) token)))
              (when (and pending-hook-context on-hook-context)
                (funcall on-hook-context nil))
              ;; Only an external engine sends the turn as one text; gptel
              ;; reads it from the buffer, so skip the transcript-sized copy.
              (when (niwa-engine-external-p
                     (alist-get 'gptel-backend
                                (niwa-agent-configuration-request-locals
                                 (niwa-agent-exec-refresh-provider invocation))))
                (setf (niwa-engine-info invocation)
                      (plist-put (niwa-engine-info invocation) :niwa-agent-prompt
                                 (if (and context-snapshot
                                          (not (string-empty-p context-snapshot)))
                                     (concat context-snapshot "\n\n"
                                             (plist-get turn :prompt))
                                   (plist-get turn :prompt)))))
              (when (and on-settle
                         (not
                          (niwa-agent-invocation-transcript-relative-path
                           invocation)))
                (error "Agent conversation could not be persisted: %s"
                       (or (niwa-agent-invocation-persistence-failure
                            invocation)
                           "first transcript save did not complete")))
              (let ((fsm
                     (niwa-agent-exec-run
                      (apply-partially
                       #'niwa-agent-runtime--handle-provider-result
                       invocation)
                      agent-type description invocation buffer)))
                (unless fsm
                  (error "Agent provider request did not start"))
                (when on-invocation
                  (funcall on-invocation invocation))
                (when (fboundp 'niwa-view-agent-live-transcript-start)
                  (niwa-view-agent-live-transcript-start invocation))
                (setq published-p t
                      publication-ready t)
                ;; Equal failure notices are deduplicated.  A synchronous
                ;; terminal error can therefore retain the old issue object
                ;; while its callback waits for publication; do not clear it.
                (let ((pending (niwa-agent-invocation-runtime-pending-response
                                invocation)))
                  (when (and previous-issue
                             (not (plist-get previous-issue :blocking))
                             (memq previous-issue
                                   (niwa-session-recovery-issues session))
                             (not (memq
                                   (niwa-agent-invocation-transcript-status invocation)
                                   '(error aborted)))
                             (not (and (listp pending)
                                       (memq (plist-get (plist-get pending :event)
                                                        :niwa-agent-terminal-status)
                                             '(error aborted)))))
                    (niwa-recovery-clear session (plist-get previous-issue :id))
                    (niwa-recovery-save session)))
                (niwa-telemetry-record
                 session 'agent-request-sent
                 :agent-id agent-id
                 :agent-path path
                 :agent-type agent-type
                 :retained (and retained-p t))
                invocation))
          ((error quit)
           (if published-p
               (progn
                 (setf (niwa-agent-invocation-terminal-reason invocation)
                       (error-message-string err))
                 (niwa-agent-runtime--settle
                  invocation nil
                  (list :niwa-agent-terminal-status 'error
                        :error-details (error-message-string err))
                  token))
             (when (or (niwa-agent-invocation-runtime-fsm invocation)
                       (niwa-agent-invocation-runtime-cancel invocation))
               ;; This handler owns settlement.  Mark it first so the abort a
               ;; cancel delivers is accepted as a no-op, instead of being
               ;; rejected as unpublished and left on a provider retry timer.
               (setf (niwa-agent-invocation-runtime-settled-p invocation) t)
               (when-let* ((cancel (niwa-agent-invocation-runtime-cancel invocation)))
                 (funcall cancel))
               (niwa-agent-runtime--finalize invocation 'error token)
               (setf (niwa-agent-invocation-runtime-fsm invocation) nil
                     (niwa-agent-invocation-runtime-cancel invocation) nil))
             (unless retained-p
               (niwa-agent-runtime-abandon-persistence invocation)
               (when (buffer-live-p buffer)
                 (kill-buffer buffer))))
           (signal (car err) (cdr err))))))))

(provide 'niwa-agent-runtime)

;;; niwa-agent-runtime.el ends here
