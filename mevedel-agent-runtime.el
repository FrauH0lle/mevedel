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

;; `mevedel-agent-exec'
(declare-function mevedel-agent-exec--allocate-agent-buffer
                  "mevedel-agent-exec" (invocation parent-data-buffer))
(declare-function mevedel-agent-exec--final-activity-snapshot
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec--final-response-text
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec--flush-transcript-save
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec--handle-update
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec--record-activity
                  "mevedel-agent-exec" (invocation item &optional reserved))
(declare-function mevedel-agent-exec--run
                  "mevedel-agent-exec"
                  (main-cb agent-type description prompt invocation
                           agent-buffer))
(declare-function mevedel-agent-exec--run-stop-hook
                  "mevedel-agent-exec" (invocation status))
(declare-function mevedel-agent-exec--save-transcript-buffer
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec-freeze-configuration
                  "mevedel-agent-exec"
                  (agent-type invocation &optional model-policy))
(defvar mevedel-agent-exec--suppress-activity-rerender)

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
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-turn
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-require-path
                  "mevedel-agents" (invocation))
(declare-function mevedel-agent-invocation-runtime-execution-results
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
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(defvar mevedel--agent-invocation)
(defvar mevedel-agent-task-path-property)

;; `mevedel-execution'
(declare-function mevedel-execution-owner-live-p
                  "mevedel-execution" (session owner))
(declare-function mevedel-execution-stop-owner
                  "mevedel-execution" (session owner))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--record-running-transcript
                  "mevedel-session-persistence" (session entry))
(declare-function mevedel-session-persistence--shallow-ensure-files
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence--update-transcript-entry
                  "mevedel-session-persistence" (session agent-id updates))
(declare-function mevedel-session-persistence--write-sidecar-now
                  "mevedel-session-persistence" (session buffer))
(defvar mevedel-session--read-only-mode)

;; `mevedel-structs'
(declare-function mevedel-request-end
                  "mevedel-structs" (&optional abort-plan-queue))
(declare-function mevedel-session-agent-transcripts
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-p "mevedel-structs" (cl-x))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-tool-task'
(declare-function mevedel-tool-task--refresh-display "mevedel-tool-task" ())
(declare-function mevedel-tool-task-finalize-owner
                  "mevedel-tool-task" (session owner status))

;; `mevedel-view-agent'
(declare-function mevedel-view-agent-live-transcript-finalize
                  "mevedel-view-agent" (invocation))

(defconst mevedel-agent-runtime--partial-max-chars (* 32 1024)
  "Maximum inline partial response size for an interrupted turn.")


;;
;;; Transcript persistence

(defun mevedel-agent-runtime-dispatch--abandon-persistence (invocation)
  "Drop incomplete persistence state for INVOCATION."
  (let ((session (mevedel-agent-invocation-parent-session invocation))
        (agent-id (mevedel-agent-invocation-agent-id invocation))
        (buffer (mevedel-agent-invocation-buffer invocation)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (setq buffer-file-name nil)))
    (setf (mevedel-agent-invocation-transcript-relative-path invocation) nil
          (mevedel-agent-invocation-sidecar-dirty invocation) nil)
    (when (and session agent-id)
      (setf (mevedel-session-agent-transcripts session)
            (assoc-delete-all
             agent-id (mevedel-session-agent-transcripts session))))))

(defun mevedel-agent-runtime--mark-start-failed (invocation reason)
  "Mark INVOCATION as failed to start for REASON."
  (setf (mevedel-agent-invocation-transcript-status invocation) 'error
        (mevedel-agent-invocation-terminal-reason invocation) reason)
  (when-let* ((session (mevedel-agent-invocation-parent-session invocation))
              (agent-id (mevedel-agent-invocation-agent-id invocation)))
    (require 'mevedel-session-persistence)
    (mevedel-session-persistence--update-transcript-entry
     session agent-id (list :status 'error :reason reason))))

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
               while (file-exists-p path)
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
  "Return INVOCATION's safe, readable absolute transcript path."
  (when-let* ((relative
               (mevedel-agent-invocation-transcript-relative-path invocation))
              (session (mevedel-agent-invocation-parent-session invocation))
              (save-path (mevedel-session-save-path session)))
    (condition-case err
        (progn
          (require 'mevedel-agent-persistence)
          (let ((path (expand-file-name relative save-path)))
            (when (and
                   (mevedel-agent-persistence-transcript-path-p
                    relative save-path)
                   (not (file-symlink-p path))
                   (file-regular-p path)
                   (file-readable-p path))
              path)))
      (error
       (message "mevedel: transcript path validation failed: %S" err)
       nil))))

(defun mevedel-agent-runtime--partial-text (invocation &optional fallback)
  "Return bounded partial output for INVOCATION or FALLBACK."
  (when-let* ((raw (or (ignore-errors
                         (mevedel-agent-exec--final-response-text invocation))
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
    (setf (mevedel-agent-invocation-transcript-status invocation) status)
    (let ((buffer (mevedel-agent-invocation-buffer invocation))
          (session (mevedel-agent-invocation-parent-session invocation))
          (parent-buffer
           (mevedel-agent-invocation-parent-data-buffer invocation)))
      (mevedel-agent-runtime--finalize-step
       invocation 'request
       (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (mevedel-request-end)))))
      (mevedel-agent-runtime--finalize-step
       invocation 'executions
       (lambda ()
         (when session
           (require 'mevedel-execution)
           (mevedel-execution-stop-owner
            session (mevedel-agent-invocation-require-path invocation)))))
      (mevedel-agent-runtime--finalize-step
       invocation 'transcript-status
       (lambda ()
         (when session
           (require 'mevedel-session-persistence)
           (mevedel-session-persistence--update-transcript-entry
            session (mevedel-agent-invocation-agent-id invocation)
            (list :status status
                  :updated-at (format-time-string "%FT%H-%M-%S"))))))
      (mevedel-agent-runtime--finalize-step
       invocation 'transcript-save
       (lambda () (mevedel-agent-exec--flush-transcript-save invocation)))
      (mevedel-agent-runtime--finalize-step
       invocation 'activity
       (lambda ()
         (let ((mevedel-agent-exec--suppress-activity-rerender t))
           (mevedel-agent-exec--record-activity
            invocation (list :type 'status :status status
                             :summary (symbol-name status))))))
      (mevedel-agent-runtime--finalize-step
       invocation 'transcript-activity
       (lambda ()
         (when session
           (mevedel-session-persistence--update-transcript-entry
            session (mevedel-agent-invocation-agent-id invocation)
            (list :activity
                  (mevedel-agent-exec--final-activity-snapshot invocation))))))
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
       (lambda () (mevedel-agent-exec--handle-update invocation)))
      (mevedel-agent-runtime--finalize-step
       invocation 'sidecar
       (lambda ()
         (when (and session (buffer-live-p parent-buffer))
           (mevedel-session-persistence--write-sidecar-now
            session parent-buffer))))
      (setf (mevedel-agent-invocation-activity invocation) nil)
      (mevedel-agent-runtime--finalize-step
       invocation 'hook
       (lambda () (mevedel-agent-exec--run-stop-hook invocation status)))
      (mevedel-agent-runtime--finalize-step
       invocation 'view
       (lambda ()
         (when (fboundp 'mevedel-view-agent-live-transcript-finalize)
           (mevedel-view-agent-live-transcript-finalize invocation)))))))

(defun mevedel-agent-runtime--settle (invocation response &optional event)
  "Finalize INVOCATION and deliver RESPONSE and EVENT exactly once."
  (unless (mevedel-agent-invocation-runtime-settled-p invocation)
    (let* ((event-status (and (listp event)
                              (plist-get event
                                         :mevedel-agent-terminal-status)))
           (status (pcase event-status
                     ('error 'error)
                     ('aborted 'aborted)
                     (_ 'completed)))
           (visible
            (pcase status
              ('error (mevedel-agent-runtime--error-response invocation event))
              ('aborted
               (mevedel-agent-runtime--interrupted-response
                invocation
                (or (mevedel-agent-invocation-terminal-reason invocation)
                    "interrupted")))
              (_ (mevedel-agent-runtime--with-execution-results
                  invocation response))))
           (callback
            (mevedel-agent-invocation-runtime-settle-callback invocation)))
      (setf (mevedel-agent-invocation-runtime-settled-p invocation) t
            (mevedel-agent-invocation-runtime-fsm invocation) nil
            (mevedel-agent-invocation-runtime-pending-response invocation) nil)
      (condition-case err
          (mevedel-agent-runtime--finalize invocation status)
        (error
         (display-warning
          'mevedel
          (format "Agent lifecycle finalization failed: %s"
                  (error-message-string err))
          :warning)))
      (when callback
        (funcall callback invocation visible event))
      visible)))

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
;;; Provider dispatch

(defun mevedel-agent-runtime--insert-prompt
    (invocation buffer description prompt context-snapshot retained-p)
  "Append PROMPT and optional CONTEXT-SNAPSHOT to INVOCATION's BUFFER."
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
      (when (mevedel-agent-invocation-transcript-relative-path invocation)
        (unless (mevedel-agent-exec--save-transcript-buffer invocation)
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
           on-invocation on-settle path frozen-configuration
           retained-id retained-buffer retained-transcript)
  "Start one asynchronous retained agent turn and return its invocation.
AGENT starts a new conversation; FROZEN-CONFIGURATION and the three retained
identity values continue one.  PATH is the conversation's canonical address.
ON-SETTLE receives (INVOCATION RESPONSE EVENT) exactly once."
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
         (session (and (boundp 'mevedel--session) mevedel--session)))
    (unless (mevedel-session-p session)
      (error "Agent requires an active mevedel session"))
    (setf (mevedel-agent-invocation-agent-id invocation) agent-id
          (mevedel-agent-invocation-path invocation) path
          (mevedel-agent-invocation-description invocation) description
          (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-agent-invocation-parent-data-buffer invocation) parent-buffer
          (mevedel-agent-invocation-parent-turn invocation)
          (1+ (or (and session (mevedel-session-turn-count session)) 0))
          (mevedel-agent-invocation-transcript-status invocation) 'running
          (mevedel-agent-invocation-runtime-settle-callback invocation)
          on-settle)
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
             (mevedel-agent-exec--allocate-agent-buffer
              invocation parent-buffer))))
      (setf (mevedel-agent-invocation-buffer invocation) buffer)
      (if retained-p
          (progn
            (setf (mevedel-agent-invocation-transcript-relative-path invocation)
                  retained-transcript)
            (with-current-buffer buffer
              (setq-local mevedel--agent-invocation invocation)))
        (mevedel-agent-runtime--setup-transcript invocation buffer))
      (mevedel-agent-runtime--insert-prompt
       invocation buffer description prompt context-snapshot retained-p)
      (when (and on-settle
                 (not (mevedel-agent-invocation-transcript-relative-path
                       invocation)))
        (error "Agent conversation could not be persisted"))
      (when on-invocation
        (funcall on-invocation invocation))
      (condition-case err
          (let ((fsm
                 (mevedel-agent-exec--run
                  (apply-partially
                   #'mevedel-agent-runtime--handle-provider-result invocation)
                  agent-type description prompt invocation buffer)))
            (unless fsm
              (error "Agent provider request did not start"))
            invocation)
        (error
         (mevedel-agent-runtime--mark-start-failed
          invocation (error-message-string err))
         (signal (car err) (cdr err)))))))

(provide 'mevedel-agent-runtime)

;;; mevedel-agent-runtime.el ends here
