;;; mevedel-agent-control.el -- Durable agent-tree control -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the path-addressed, session-scoped agent registry and the admission and
;; settlement transaction for asynchronous agent turns.  Provider execution
;; remains in `mevedel-agent-runtime'; this module gives it durable identities,
;; tree-wide capacity, canonical RESULT/MAIL/USER records, retained mailboxes,
;; turn-local interruption, and the WaitAgent suspension and wake lifecycle.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `mevedel-agent-conversation'
(declare-function mevedel-agent-conversation-final-response
                  "mevedel-agent-conversation" (invocation))
(declare-function mevedel-agent-conversation-save
                  "mevedel-agent-conversation" (invocation &optional deferred))
(defvar mevedel--agent-invocation)

;; `mevedel-agent-runtime'
(declare-function mevedel-agent-runtime-dispatch
                  "mevedel-agent-runtime" t t)
(declare-function mevedel-agent-runtime-dispatch--abandon-persistence
                  "mevedel-agent-runtime" (invocation))
(declare-function mevedel-agent-runtime-interrupt
                  "mevedel-agent-runtime" (invocation reason))

;; `mevedel-agents'
(declare-function copy-mevedel-agent "mevedel-agents" (cl-x))
(declare-function mevedel-agent-configuration-p
                  "mevedel-agents" (cl-x))
(declare-function mevedel-agent-freeze "mevedel-agents" (agent))
(declare-function mevedel-agent-hook-rules "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-frozen-configuration
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-resolve-role "mevedel-agents" (role))
(declare-function mevedel-agents-specs "mevedel-agents" (&optional buffer))

;; `mevedel-compact'
(declare-function mevedel-compact-context-snapshot
                  "mevedel-compact" (fork-turns))

;; `mevedel-models'
(declare-function mevedel-model-parse-effort "mevedel-models" (value))
(declare-function mevedel-model-parse-selector "mevedel-models" (value))
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-save-agent-state
                  "mevedel-session-persistence" (session))

;; `mevedel-structs'
(declare-function mevedel-agent-path-p "mevedel-structs" (path))
(declare-function mevedel-session--set-agent-registry
                  "mevedel-structs" (session registry))
(declare-function mevedel-session--set-agent-reservations
                  "mevedel-structs" (session reservations))
(declare-function mevedel-session--set-agent-root-waiter
                  "mevedel-structs" (session waiter))
(declare-function mevedel-session--set-messages
                  "mevedel-structs" (session messages))
(declare-function mevedel-session-agent-registry
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-reservations
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-root-activity
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-root-waiter
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-turn-capacity
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-messages
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-p "mevedel-structs" (object))
(declare-function mevedel-session-save-path
                  "mevedel-structs" (cl-x) t)

;; `mevedel-utilities'
(declare-function mevedel--head-tail-preview-parts
                  "mevedel-utilities"
                  (head tail total-length &optional preview-size))

(defconst mevedel-agent-control--result-limit (* 32 1024)
  "Maximum number of characters in an inline RESULT payload.")

(defconst mevedel-agent-control--wait-default-ms 30000
  "Default WaitAgent timeout in milliseconds.")

(defconst mevedel-agent-control--wait-min-ms 10000
  "Minimum WaitAgent timeout in milliseconds.")

(defconst mevedel-agent-control--wait-max-ms 3600000
  "Maximum WaitAgent timeout in milliseconds.")

(defconst mevedel-agent-control--active-activities
  '(starting running waiting permission-blocked interaction-blocked)
  "Persisted agent activities that own one active-turn slot.")

(defun mevedel-agent-control-active-activity-p (activity)
  "Return non-nil when ACTIVITY owns one active-turn slot."
  (memq activity mevedel-agent-control--active-activities))

(defvar mevedel-agent-control--suppress-persistence nil
  "Non-nil while a multi-step registry repair must be persisted atomically.")

(cl-defstruct (mevedel-agent-waiter
               (:constructor mevedel-agent-waiter--create))
  "One suspended ordinary asynchronous WaitAgent tool call."
  callback
  release
  timer)

(cl-defstruct (mevedel-agent-record
               (:constructor mevedel-agent-record--create))
  "One retained non-root agent identity in a root session."
  id
  path
  parent-path
  role
  configuration
  activity
  conversation-location
  conversation-buffer
  invocation
  mailbox
  hook-context-pending
  blockers
  waiter
  result-handler)

(defun mevedel-agent-control--active-p (record)
  "Return non-nil when RECORD owns one active-turn slot."
  (mevedel-agent-control-active-activity-p
   (mevedel-agent-record-activity record)))

(defun mevedel-agent-control--persist-session (session)
  "Best-effort persist SESSION's changed agent state."
  (unless mevedel-agent-control--suppress-persistence
    (when (mevedel-session-save-path session)
      (require 'mevedel-session-persistence)
      (mevedel-session-persistence-save-agent-state session))))

(defun mevedel-agent-control-block-turn (session path activity)
  "Block PATH's current turn in SESSION with durable ACTIVITY.

ACTIVITY must be `waiting', `permission-blocked', or
`interaction-blocked'.  Return an idempotent release closure bound to the
current invocation, or nil when PATH has no active retained turn.  Overlapping
blockers compose and stale releases cannot alter a later follow-up."
  (unless (memq activity '(waiting permission-blocked interaction-blocked))
    (error "Invalid blocked agent activity: %s" activity))
  (when-let* (((not (equal path "/root")))
              (entry (assoc path (mevedel-session-agent-registry session)))
              (record (cdr entry))
              ((mevedel-agent-control--active-p record))
              (invocation (mevedel-agent-record-invocation record)))
    (let ((token (list activity)))
      (push token (mevedel-agent-record-blockers record))
      (unless (eq activity (mevedel-agent-record-activity record))
        (setf (mevedel-agent-record-activity record) activity)
        (mevedel-agent-control--persist-session session))
      (lambda ()
        (when (and (eq record
                       (cdr (assoc path
                                   (mevedel-session-agent-registry session))))
                   (eq invocation (mevedel-agent-record-invocation record))
                   (memq token (mevedel-agent-record-blockers record)))
          (setf (mevedel-agent-record-blockers record)
                (delq token (mevedel-agent-record-blockers record)))
          (let ((next (or (caar (mevedel-agent-record-blockers record))
                          'running)))
            (unless (eq next (mevedel-agent-record-activity record))
              (setf (mevedel-agent-record-activity record) next)
              (mevedel-agent-control--persist-session session))))))))

(defun mevedel-agent-control--active-count (session)
  "Return the number of active non-root turns in SESSION."
  (+ (length (mevedel-session-agent-reservations session))
     (cl-count-if
      (lambda (entry)
        (mevedel-agent-control--active-p (cdr entry)))
      (mevedel-session-agent-registry session))))

(defun mevedel-agent-control-active-turn-p (session)
  "Return non-nil when SESSION has an active non-root agent turn."
  (> (mevedel-agent-control--active-count session) 0))

(defun mevedel-agent-control-retained-buffer-p (session buffer)
  "Return non-nil when SESSION retains BUFFER as an agent conversation."
  (and (mevedel-session-p session)
       (buffer-live-p buffer)
       (cl-some
        (lambda (entry)
          (eq buffer
              (mevedel-agent-record-conversation-buffer (cdr entry))))
        (mevedel-session-agent-registry session))))

(defun mevedel-agent-control-teardown-session (session)
  "Kill every retained conversation buffer owned by SESSION."
  (when (mevedel-session-p session)
    (mevedel-session--set-agent-reservations session nil)
    (mevedel-agent-control-cancel-wait session "/root")
    (dolist (entry (mevedel-session-agent-registry session))
      (mevedel-agent-control-cancel-wait session (car entry))
      (when-let* ((buffer
                   (mevedel-agent-record-conversation-buffer (cdr entry)))
                  ((buffer-live-p buffer)))
        (let ((saved-p
               (with-current-buffer buffer
                 (or (not (buffer-modified-p))
                     (when (and (boundp 'mevedel--agent-invocation)
                                mevedel--agent-invocation)
                       (require 'mevedel-agent-conversation)
                       (mevedel-agent-conversation-save
                        mevedel--agent-invocation))))))
          (if saved-p
              (kill-buffer buffer)
            (display-warning
             'mevedel
             (format
              "Retained agent transcript could not be saved during session teardown: %s"
              (car entry))
             :warning)))))))

(defun mevedel-agent-control--path-for-invocation (session invocation)
  "Return INVOCATION's retained canonical path in SESSION."
  (let ((path (mevedel-agent-invocation-path invocation)))
    (and (mevedel-agent-path-p path)
         (assoc path (mevedel-session-agent-registry session))
         path)))

(defun mevedel-agent-control-current-path (session)
  "Return the current caller's canonical path in SESSION."
  (let ((invocation (and (boundp 'mevedel--agent-invocation)
                         mevedel--agent-invocation)))
    (if (not invocation)
        "/root"
      (or (mevedel-agent-control--path-for-invocation session invocation)
          (error "Current agent is not registered: %s"
                 (mevedel-agent-invocation-path invocation))))))

(defun mevedel-agent-control-resolve-path (session caller-path target)
  "Resolve TARGET from CALLER-PATH to an addressable path in SESSION."
  (unless (mevedel-agent-path-p caller-path)
    (error "Invalid caller agent path: %s" caller-path))
  (unless (and (stringp target) (not (string-empty-p target)))
    (user-error "Agent target must be a non-empty path"))
  (let ((path
         (if (string-prefix-p "/" target)
             target
           (concat caller-path "/" target))))
    (unless (mevedel-agent-path-p path)
      (user-error "Invalid agent target: %s" target))
    (unless (or (equal path "/root")
                (assoc path (mevedel-session-agent-registry session)))
      (user-error "Unknown agent target: %s" target))
    path))

(defun mevedel-agent-control--record-at-path (session path)
  "Return SESSION's retained record at PATH, or signal an error."
  (or (cdr (assoc path (mevedel-session-agent-registry session)))
      (error "Unknown agent path: %s" path)))

(defun mevedel-agent-control--mailbox-queue (session path)
  "Return PATH's reverse-order unread queue in SESSION."
  (if (equal path "/root")
      (mevedel-session-messages session)
    (mevedel-agent-record-mailbox
     (mevedel-agent-control--record-at-path session path))))

(defun mevedel-agent-control--set-mailbox-queue (session path queue)
  "Set PATH's reverse-order unread QUEUE in SESSION."
  (if (equal path "/root")
      (mevedel-session--set-messages session queue)
    (setf (mevedel-agent-record-mailbox
           (mevedel-agent-control--record-at-path session path))
          queue)))

(defun mevedel-agent-control--mailbox (session path)
  "Return PATH's unread records in SESSION in FIFO order."
  (reverse (mevedel-agent-control--mailbox-queue session path)))

(defun mevedel-agent-control-context-mailbox (context)
  "Return CONTEXT's retained unread records in FIFO order."
  (if (mevedel-session-p context)
      (mevedel-agent-control--mailbox context "/root")
    (let* ((session (mevedel-agent-invocation-parent-session context))
           (path (and session
                      (mevedel-agent-control--path-for-invocation
                       session context))))
      (and path (mevedel-agent-control--mailbox session path)))))

(defun mevedel-agent-control-clear-context-mailbox (context)
  "Remove all retained unread records for CONTEXT."
  (if (mevedel-session-p context)
      (progn
        (mevedel-agent-control--set-mailbox-queue context "/root" nil)
        (mevedel-agent-control--persist-session context))
    (let* ((session (mevedel-agent-invocation-parent-session context))
           (path (and session
                      (mevedel-agent-control--path-for-invocation
                       session context))))
      (when path
        (mevedel-agent-control--set-mailbox-queue session path nil)
        (mevedel-agent-control--persist-session session)))))

(defun mevedel-agent-control--waiter (session path)
  "Return PATH's active waiter in SESSION, or nil."
  (if (equal path "/root")
      (mevedel-session-agent-root-waiter session)
    (when-let* ((record
                 (cdr (assoc path
                             (mevedel-session-agent-registry session)))))
      (mevedel-agent-record-waiter record))))

(defun mevedel-agent-control--set-waiter (session path waiter)
  "Set PATH's active WAITER in SESSION."
  (if (equal path "/root")
      (mevedel-session--set-agent-root-waiter session waiter)
    (setf (mevedel-agent-record-waiter
           (mevedel-agent-control--record-at-path session path))
          waiter)))

(defun mevedel-agent-control-cancel-wait (session path)
  "Cancel PATH's active wait in SESSION without invoking its callback."
  (when-let* ((waiter (mevedel-agent-control--waiter session path)))
    (mevedel-agent-control--set-waiter session path nil)
    (when-let* ((timer (mevedel-agent-waiter-timer waiter))
                ((timerp timer)))
      (cancel-timer timer))
    (when-let* ((release (mevedel-agent-waiter-release waiter)))
      (funcall release))
    t))

(defun mevedel-agent-control--wake (session path reason)
  "Wake PATH's waiter in SESSION with REASON exactly once."
  (when-let* ((waiter (mevedel-agent-control--waiter session path)))
    (let ((callback (mevedel-agent-waiter-callback waiter)))
      (mevedel-agent-control-cancel-wait session path)
      (funcall callback reason)
      t)))

(defun mevedel-agent-control--wait-timeout (session path waiter)
  "Wake PATH for timeout if WAITER is still current in SESSION."
  (when (eq waiter (mevedel-agent-control--waiter session path))
    (mevedel-agent-control--wake session path 'timeout)))

(defun mevedel-agent-control-wait (session callback &optional timeout-ms)
  "Suspend SESSION's current caller until mail, steering, or TIMEOUT-MS.
CALLBACK receives one of `mailbox', `steering', `user', or `timeout'.
Return the caller path when suspended, and nil after an immediate release."
  (let ((timeout (or timeout-ms mevedel-agent-control--wait-default-ms)))
    (unless (and (integerp timeout)
                 (<= mevedel-agent-control--wait-min-ms timeout)
                 (<= timeout mevedel-agent-control--wait-max-ms))
      (user-error "WaitAgent timeout_ms must be an integer from %d through %d"
                  mevedel-agent-control--wait-min-ms
                  mevedel-agent-control--wait-max-ms))
    (let ((path (mevedel-agent-control-current-path session)))
      (cond
       ((mevedel-agent-control--mailbox-queue session path)
        (funcall callback 'mailbox)
        nil)
       ((mevedel-agent-control--waiter session path)
        (error "Agent is already waiting: %s" path))
       (t
        (let ((waiter (mevedel-agent-waiter--create :callback callback)))
          (mevedel-agent-control--set-waiter session path waiter)
          (setf (mevedel-agent-waiter-release waiter)
                (mevedel-agent-control-block-turn session path 'waiting))
          (condition-case err
              (setf (mevedel-agent-waiter-timer waiter)
                    (run-at-time
                     (/ timeout 1000.0) nil
                     #'mevedel-agent-control--wait-timeout
                     session path waiter))
            (error
             (mevedel-agent-control-cancel-wait session path)
             (signal (car err) (cdr err))))
          path))))))

(defun mevedel-agent-control-root-waiting-p (session)
  "Return non-nil when SESSION's root has an active WaitAgent callback."
  (and (mevedel-agent-control--waiter session "/root") t))

(defun mevedel-agent-control--enqueue
    (session recipient record &optional wake-reason)
  "Queue RECORD for RECIPIENT in SESSION and wake a matching wait."
  (mevedel-agent-control--set-mailbox-queue
   session recipient
   (cons record (mevedel-agent-control--mailbox-queue session recipient)))
  (mevedel-agent-control--wake
   session recipient (or wake-reason 'mailbox))
  (mevedel-agent-control--persist-session session))

(defun mevedel-agent-control-steer-user
    (session message &optional before-wake metadata)
  "Deliver MESSAGE when SESSION's root is explicitly waiting.
Call BEFORE-WAKE before releasing the wait.  Return non-nil only when the
input became steering for the current turn.  METADATA may carry the separate
transcript payload and hook audits."
  (when (mevedel-agent-control--waiter session "/root")
    (when before-wake
      (funcall before-wake))
    (mevedel-agent-control--enqueue
     session "/root"
     (list :type 'USER
           :sender "user"
           :recipient "/root"
           :payload message
           :transcript-payload (plist-get metadata :transcript-payload)
           :hook-audits (plist-get metadata :hook-audits)
           :timestamp (current-time))
     'user)
    t))

(defun mevedel-agent-control-send-message (session target message)
  "Queue one canonical MAIL with MESSAGE for TARGET in SESSION.
Return the resolved recipient path.  Sending never activates a turn."
  (unless (and (stringp message) (not (string-empty-p message)))
    (user-error "SendMessage message must be non-empty plain text"))
  (let* ((sender (mevedel-agent-control-current-path session))
         (recipient
          (mevedel-agent-control-resolve-path session sender target)))
    (mevedel-agent-control--enqueue
     session recipient
     (list :type 'MAIL
           :sender sender
           :recipient recipient
           :payload message
           :timestamp (current-time)))
    recipient))

(defun mevedel-agent-control-enqueue-execution-result (session owner body)
  "Queue yielded execution BODY for root OWNER in SESSION."
  (unless (and (equal owner "/root") (stringp body))
    (error "Invalid root execution result"))
  (mevedel-agent-control--enqueue
   session "/root"
   (list :type 'MAIL
         :sender "/root"
         :recipient "/root"
         :payload body
         :timestamp (current-time))))

(defun mevedel-agent-control-list-agents (session &optional path-prefix)
  "Return SESSION's minimal roster, optionally below PATH-PREFIX."
  (when (and path-prefix
             (not (mevedel-agent-path-p path-prefix)))
    (user-error "Invalid agent path prefix: %s" path-prefix))
  (let* ((records
          (mapcar
           (lambda (entry)
             (let ((record (cdr entry)))
               (list :path (mevedel-agent-record-path record)
                     :role (mevedel-agent-record-role record)
                     :activity
                     (symbol-name
                      (pcase (mevedel-agent-record-activity record)
                        ('starting 'starting)
                        ('idle 'idle)
                        (_ 'running))))))
           (mevedel-session-agent-registry session)))
         (roster
          (cons (list :path "/root" :role "default"
                      :activity
                      (symbol-name
                       (pcase (mevedel-session-agent-root-activity session)
                         ('starting 'starting)
                         ('idle 'idle)
                         (_ 'running))))
                records))
         (filtered
          (if path-prefix
              (cl-remove-if-not
               (lambda (entry)
                 (let ((path (plist-get entry :path)))
                   (or (equal path path-prefix)
                       (string-prefix-p (concat path-prefix "/") path))))
               roster)
            roster)))
    (sort filtered
          (lambda (a b)
            (string-lessp (plist-get a :path) (plist-get b :path))))))

(defun mevedel-agent-control-context-path (context)
  "Return CONTEXT's canonical path in its root session."
  (if (mevedel-session-p context)
      "/root"
    (let ((session (mevedel-agent-invocation-parent-session context)))
      (or (and session
               (mevedel-agent-control--path-for-invocation session context))
          (error "Agent invocation is not registered: %s"
                 (mevedel-agent-invocation-agent-id context))))))

(defun mevedel-agent-control-direct-children (session parent-path)
  "Return sorted path and role references below PARENT-PATH in SESSION."
  (sort
   (cl-loop for (_path . record) in (mevedel-session-agent-registry session)
            when (equal parent-path
                        (mevedel-agent-record-parent-path record))
            collect (list :path (mevedel-agent-record-path record)
                          :role (mevedel-agent-record-role record)))
   (lambda (a b)
     (string-lessp (plist-get a :path) (plist-get b :path)))))

(defun mevedel-agent-control--recovery-result (record)
  "Return RECORD's interrupted recovery payload with useful partial text."
  (let* ((buffer (mevedel-agent-record-conversation-buffer record))
         (identity
          (and (buffer-live-p buffer)
               (buffer-local-value 'mevedel--agent-invocation buffer)))
         (partial
          (and identity
               (progn
                 (require 'mevedel-agent-conversation)
                 (ignore-errors
                   (mevedel-agent-conversation-final-response identity)))))
         (location (mevedel-agent-record-conversation-location record)))
    (mevedel-agent-control--bounded-result
     record
     (concat
     "Agent turn was interrupted by session recovery."
      "\n\nReason: the persisted turn had no live provider "
      "request after resume."
      (when partial
        (format "\n\nPartial response:\n\n%s" partial))
      (when location
        (format "\n\nTranscript: %s" location))))))

(defun mevedel-agent-control-recover-interrupted (session)
  "Recover every persisted active turn in SESSION as an interrupted RESULT.

Return the number of repaired records.  The caller owns the atomic sidecar
rewrite after the complete registry has been repaired."
  (let ((mevedel-agent-control--suppress-persistence t)
        (count 0))
    (dolist (entry (mevedel-session-agent-registry session))
      (let ((record (cdr entry)))
        (when (mevedel-agent-control--active-p record)
          (setf (mevedel-agent-record-activity record) 'idle)
          (setf (mevedel-agent-record-invocation record) nil)
          (setf (mevedel-agent-record-blockers record) nil)
          (mevedel-agent-control--enqueue
           session (mevedel-agent-record-parent-path record)
           (list :type 'RESULT
                 :sender (mevedel-agent-record-path record)
                 :recipient (mevedel-agent-record-parent-path record)
                 :outcome 'interrupted
                 :payload (mevedel-agent-control--recovery-result record)
                 :timestamp (current-time)))
          (cl-incf count))))
    count))

(defun mevedel-agent-control--validate-spawn (session task-name message)
  "Validate SESSION, TASK-NAME, and MESSAGE for a new child."
  (unless (mevedel-session-p session)
    (error "Agent requires an active mevedel session"))
  (unless (and (stringp task-name)
               (let ((case-fold-search nil))
                 (string-match-p "\\`[a-z0-9_]+\\'" task-name))
               (not (equal task-name "root")))
    (user-error
     "Agent task_name must be one lowercase ASCII path segment"))
  (unless (and (stringp message) (not (string-empty-p message)))
    (user-error "Agent message must be non-empty plain text")))

(defun mevedel-agent-control--normalize-fork-turns (value)
  "Normalize model-facing context fork VALUE.

Nil defaults to `all'.  The only explicit forms are \"all\", \"none\", and
positive decimal strings."
  (cond
   ((or (null value) (equal value "all")) 'all)
   ((equal value "none") 'none)
   ((and (stringp value)
         (string-match-p "\\`[1-9][0-9]*\\'" value))
    (string-to-number value))
   (t (user-error
       "Agent fork_turns must be all, none, or a positive integer string"))))

(defun mevedel-agent-control--reserve
    (session parent-path task-name role)
  "Atomically reserve TASK-NAME below PARENT-PATH in SESSION for ROLE."
  (let* ((path (concat parent-path "/" task-name))
         (registry (mevedel-session-agent-registry session))
         (reservations (mevedel-session-agent-reservations session)))
    (when (or (assoc path registry) (assoc path reservations))
      (user-error "Agent path is already reserved: %s" path))
    (when (>= (mevedel-agent-control--active-count session)
              (mevedel-session-agent-turn-capacity session))
      (user-error "Agent tree is at its active-turn capacity"))
    (let ((record
           (mevedel-agent-record--create
            :path path
            :parent-path parent-path
            :role role
            :configuration (list :role role)
            :activity 'starting)))
      (mevedel-session--set-agent-reservations
       session (cons (cons path record) reservations))
      record)))

(defun mevedel-agent-control--rollback (session record)
  "Remove unpublished RECORD from SESSION and release its slot."
  (mevedel-session--set-agent-reservations
   session
   (assoc-delete-all (mevedel-agent-record-path record)
                     (mevedel-session-agent-reservations session)))
  (when-let* ((invocation (mevedel-agent-record-invocation record)))
    (mevedel-agent-runtime-dispatch--abandon-persistence invocation)
    (when-let* ((buffer (mevedel-agent-invocation-buffer invocation))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer)))
  (mevedel-agent-control--persist-session session))

(defun mevedel-agent-control--bounded-result (record response)
  "Return RECORD's bounded terminal payload for RESPONSE."
  (let ((text (if (stringp response) response (format "%S" response))))
    (if (<= (length text) mevedel-agent-control--result-limit)
        text
      (let* ((location (mevedel-agent-record-conversation-location record))
             (suffix (format "\n\nFull transcript: %s"
                             (or location "unavailable")))
             (preview-size
              (max 2 (- mevedel-agent-control--result-limit
                        (length suffix) 128)))
             (preview
              (plist-get
               (mevedel--head-tail-preview-parts
                text text (length text) preview-size)
               :text)))
        (concat preview suffix)))))

(defun mevedel-agent-control--publish-result (session record result)
  "Publish RECORD's canonical RESULT in SESSION.

Every turn first enqueues RESULT for the spawn parent.  A workflow may install
a one-shot result handler when it owns the parent interaction.  Successful
workflow delivery consumes that exact queued record; a failed handler leaves
it queued for ordinary parent delivery."
  (let* ((recipient (mevedel-agent-record-parent-path record))
         (handler (mevedel-agent-record-result-handler record)))
    (mevedel-agent-control--enqueue session recipient result)
    (when handler
      (setf (mevedel-agent-record-result-handler record) nil)
      (condition-case err
          (progn
            (funcall handler result)
            (mevedel-agent-control--set-mailbox-queue
             session recipient
             (delq result
                   (mevedel-agent-control--mailbox-queue session recipient)))
            (mevedel-agent-control--persist-session session))
        (error
         (display-warning
          'mevedel
          (format "Agent result handler failed for %s: %s"
                  (mevedel-agent-record-path record)
                  (error-message-string err))
          :warning))))))

(defun mevedel-agent-control--settle
    (session record invocation response &optional event)
  "Settle RECORD's INVOCATION in SESSION from RESPONSE and terminal EVENT."
  (when (and (eq invocation (mevedel-agent-record-invocation record))
             (mevedel-agent-control--active-p record))
    (let* ((outcome
            (pcase (mevedel-agent-invocation-transcript-status invocation)
              ('error 'errored)
              ('aborted 'interrupted)
              (_ (if (and (stringp response)
                          (string-prefix-p "Error:" response))
                     'errored
                   'completed))))
           (payload
            (if (eq outcome 'errored)
                (or (and (listp event) (plist-get event :error-details))
                    (mevedel-agent-invocation-terminal-reason invocation)
                    "Agent turn failed")
              response)))
      (setf (mevedel-agent-record-activity record) 'idle)
      (setf (mevedel-agent-record-invocation record) nil)
      (setf (mevedel-agent-record-blockers record) nil)
      (mevedel-agent-control-cancel-wait
       session (mevedel-agent-record-path record))
      (mevedel-agent-control--publish-result
       session record
       (list :type 'RESULT
             :sender (mevedel-agent-record-path record)
             :recipient (mevedel-agent-record-parent-path record)
             :outcome outcome
             :payload (mevedel-agent-control--bounded-result record payload)
             :timestamp (current-time))))))

(defun mevedel-agent-control--record-invocation
    (session record invocation)
  "Publish RECORD with INVOCATION in SESSION's retained registry."
  (unless (equal (mevedel-agent-invocation-path invocation)
                 (mevedel-agent-record-path record))
    (error "Agent invocation path mismatch: %s"
           (mevedel-agent-invocation-path invocation)))
  (setf (mevedel-agent-record-id record)
        (mevedel-agent-invocation-agent-id invocation))
  (setf (mevedel-agent-record-invocation record) invocation)
  (setf (mevedel-agent-record-blockers record) nil)
  (setf (mevedel-agent-record-conversation-buffer record)
        (mevedel-agent-invocation-buffer invocation))
  (setf (mevedel-agent-record-conversation-location record)
        (mevedel-agent-invocation-transcript-relative-path invocation))
  (setf (mevedel-agent-record-configuration record)
        (mevedel-agent-invocation-frozen-configuration invocation))
  (let ((path (mevedel-agent-record-path record)))
    (mevedel-session--set-agent-reservations
     session
     (assoc-delete-all path (mevedel-session-agent-reservations session)))
    (mevedel-session--set-agent-registry
     session
     (cons (cons path record) (mevedel-session-agent-registry session)))))

(defun mevedel-agent-control--set-hook-context (record entries)
  "Replace RECORD's pending hook context with a copy of ENTRIES."
  (setf (mevedel-agent-record-hook-context-pending record)
        (copy-tree entries)))

(defun mevedel-agent-control--dispatch-followup (session record message)
  "Dispatch MESSAGE as RECORD's next provider turn in SESSION."
  (let ((buffer (mevedel-agent-record-conversation-buffer record)))
    (unless (buffer-live-p buffer)
      (error "Agent conversation is not live: %s"
             (mevedel-agent-record-path record)))
    (let ((configuration (mevedel-agent-record-configuration record)))
      (unless (mevedel-agent-configuration-p configuration)
        (error "Agent has no frozen configuration: %s"
               (mevedel-agent-record-path record)))
      (require 'mevedel-agent-runtime)
      (unless
          (mevedel-agent-runtime-dispatch
           nil (file-name-nondirectory (mevedel-agent-record-path record))
           message
           :path (mevedel-agent-record-path record)
           :frozen-configuration configuration
           :retained-id (mevedel-agent-record-id record)
           :retained-buffer buffer
           :retained-transcript
           (mevedel-agent-record-conversation-location record)
           :pending-hook-context
           (mevedel-agent-record-hook-context-pending record)
           :on-hook-context
           (apply-partially
            #'mevedel-agent-control--set-hook-context record)
           :on-invocation
           (apply-partially
            #'mevedel-agent-control--record-invocation session record)
           :on-settle
           (apply-partially
            #'mevedel-agent-control--settle session record))
        (error "Agent provider request did not start")))
    (when (eq (mevedel-agent-record-activity record) 'starting)
      (setf (mevedel-agent-record-activity record) 'running))
    (mevedel-agent-control--persist-session session)
    record))

(defun mevedel-agent-control-followup (session target message)
  "Start or steer TARGET with MESSAGE in SESSION and return its record."
  (unless (and (stringp message) (not (string-empty-p message)))
    (user-error "FollowupAgent message must be non-empty plain text"))
  (let* ((caller-path (mevedel-agent-control-current-path session))
         (path (mevedel-agent-control-resolve-path
                session caller-path target)))
    (when (equal path "/root")
      (user-error "FollowupAgent cannot activate /root"))
    (let ((record (cdr (assoc path
                              (mevedel-session-agent-registry session)))))
      (if (mevedel-agent-control--active-p record)
          (mevedel-agent-control--enqueue
           session path
           (list :type 'MAIL
                 :sender caller-path
                 :recipient path
                 :payload message
                 :timestamp (current-time))
           'steering)
        (when (>= (mevedel-agent-control--active-count session)
                  (mevedel-session-agent-turn-capacity session))
          (user-error "Agent tree is at its active-turn capacity"))
        (setf (mevedel-agent-record-activity record) 'starting)
        (condition-case err
            (mevedel-agent-control--dispatch-followup session record message)
          (error
           (setf (mevedel-agent-record-activity record) 'idle)
           (setf (mevedel-agent-record-invocation record) nil)
           (mevedel-agent-control--persist-session session)
           (signal (car err) (cdr err)))))
      record)))

(defun mevedel-agent-control-interrupt (session target)
  "Interrupt TARGET's current turn in SESSION and return its prior activity."
  (let* ((caller-path (mevedel-agent-control-current-path session))
         (path (mevedel-agent-control-resolve-path
                session caller-path target)))
    (when (equal path "/root")
      (user-error "InterruptAgent cannot target /root"))
    (when (equal path caller-path)
      (user-error "InterruptAgent cannot target the caller"))
    (let* ((record (mevedel-agent-control--record-at-path session path))
           (activity (mevedel-agent-record-activity record)))
      (when (mevedel-agent-control--active-p record)
        (require 'mevedel-agent-runtime)
        (mevedel-agent-runtime-interrupt
         (mevedel-agent-record-invocation record)
         (format "interrupted by %s" caller-path)))
      (list :path path :previous-activity activity))))

(cl-defun mevedel-agent-control-spawn
    (session task-name message
             &key role agent fork-turns model effort model-policy
             description on-invocation result-handler
             skill-permission-rules skill-hook-rules)
  "Spawn TASK-NAME with MESSAGE and optional controls in SESSION.
Return the committed retained record."
  (mevedel-agent-control--validate-spawn session task-name message)
  (require 'mevedel-agents)
  (when (and role (null agent)
             (mevedel-agents-specs)
             (not (assoc-string role (mevedel-agents-specs))))
    (user-error "Agent role is not available to this session: %s" role))
  (let* ((resolved-agent
          (or agent (mevedel-agent-resolve-role role)))
         (resolved-agent
          (if skill-hook-rules
              (let ((copy (copy-mevedel-agent resolved-agent)))
                (setf (mevedel-agent-hook-rules copy)
                      (append (mevedel-agent-hook-rules copy)
                              skill-hook-rules))
                copy)
            resolved-agent))
         (resolved-agent (mevedel-agent-freeze resolved-agent))
         (role-name (mevedel-agent-name resolved-agent))
         (fork-turns (mevedel-agent-control--normalize-fork-turns fork-turns))
         (model-selector
          (progn
            (require 'mevedel-models)
            (mevedel-model-parse-selector model)))
         (effort (mevedel-model-parse-effort effort))
         (model-policy
          (or model-policy
              (mevedel-model-resolve-workload
               (intern role-name) model-selector effort)))
         (context-snapshot
          (progn
            (require 'mevedel-compact)
            (mevedel-compact-context-snapshot fork-turns)))
         (caller-path (mevedel-agent-control-current-path session))
         (record (mevedel-agent-control--reserve
                  session caller-path task-name role-name))
         committed)
    (setf (mevedel-agent-record-result-handler record) result-handler)
    (unwind-protect
        (progn
          (require 'mevedel-agent-runtime)
          (unless
              (mevedel-agent-runtime-dispatch
               resolved-agent (or description task-name) message
               :context-snapshot context-snapshot
               :model-policy model-policy
               :skill-permission-rules skill-permission-rules
               :path (mevedel-agent-record-path record)
               :on-invocation
               (lambda (invocation)
                 (mevedel-agent-control--record-invocation
                  session record invocation)
                 (setq committed t)
                 (when on-invocation
                   (funcall on-invocation invocation)))
               :on-settle
               (apply-partially
                #'mevedel-agent-control--settle session record))
            (error "Agent provider request did not start"))
          (unless (mevedel-agent-record-conversation-location record)
            (error "Agent conversation could not be persisted"))
          (when (eq (mevedel-agent-record-activity record) 'starting)
            (setf (mevedel-agent-record-activity record) 'running))
          (mevedel-agent-control--persist-session session)
          record)
      (unless committed
        (mevedel-agent-control--rollback session record)))))

(provide 'mevedel-agent-control)

;;; mevedel-agent-control.el ends here
