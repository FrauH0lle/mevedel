;;; mevedel-agent-control.el -- Durable agent-tree control -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the path-addressed, session-scoped agent registry and the admission and
;; settlement transaction for asynchronous agent turns.  Provider execution
;; remains in `mevedel-agent-runtime'; this module gives it durable identities,
;; tree-wide capacity, and canonical RESULT records.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `mevedel-agent-exec'
(declare-function mevedel-agent-exec--flush-transcript-save
                  "mevedel-agent-exec" (invocation))
(defvar mevedel--agent-invocation)

;; `mevedel-agent-runtime'
(declare-function mevedel-agent-runtime--ctx-push-message
                  "mevedel-agent-runtime" (ctx message))
(declare-function mevedel-agent-runtime-dispatch
                  "mevedel-agent-runtime" t t)
(declare-function mevedel-agent-runtime-dispatch--abandon-persistence
                  "mevedel-agent-runtime" (invocation))
(declare-function mevedel-agent-runtime-steer
                  "mevedel-agent-runtime" (invocation sender message))

;; `mevedel-agents'
(declare-function mevedel-agent-default "mevedel-agents" ())
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)

;; `mevedel-structs'
(declare-function mevedel-session--set-agent-registry
                  "mevedel-structs" (session registry))
(declare-function mevedel-session-agent-registry
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-root-activity
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-turn-capacity
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-p "mevedel-structs" (object))

;; `mevedel-utilities'
(declare-function mevedel--head-tail-preview-parts
                  "mevedel-utilities"
                  (head tail total-length &optional preview-size))

(defconst mevedel-agent-control--result-limit (* 32 1024)
  "Maximum number of characters in an inline RESULT payload.")

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
  invocation)

(defun mevedel-agent-control--active-p (record)
  "Return non-nil when RECORD owns one active-turn slot."
  (memq (mevedel-agent-record-activity record) '(starting running)))

(defun mevedel-agent-control--active-count (session)
  "Return the number of active non-root turns in SESSION."
  (cl-count-if
   (lambda (entry)
     (mevedel-agent-control--active-p (cdr entry)))
   (mevedel-session-agent-registry session)))

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
    (dolist (entry (mevedel-session-agent-registry session))
      (when-let* ((buffer
                   (mevedel-agent-record-conversation-buffer (cdr entry)))
                  ((buffer-live-p buffer)))
        (let ((saved-p
               (with-current-buffer buffer
                 (or (not (buffer-modified-p))
                     (when (and (boundp 'mevedel--agent-invocation)
                                mevedel--agent-invocation)
                       (require 'mevedel-agent-exec)
                       (mevedel-agent-exec--flush-transcript-save
                        mevedel--agent-invocation))))))
          (if saved-p
              (kill-buffer buffer)
            (display-warning
             'mevedel
             (format
              "Retained agent transcript could not be saved during session teardown: %s"
              (car entry))
             :warning)))))))

(defun mevedel-agent-control--canonical-path-p (path)
  "Return non-nil when PATH is a canonical agent path."
  (and (stringp path)
       (let ((case-fold-search nil))
         (string-match-p
          "\\`/root\\(?:/[a-z0-9_]+\\)*\\'" path))))

(defun mevedel-agent-control-current-path (session)
  "Return the current caller's canonical path in SESSION."
  (let ((invocation (and (boundp 'mevedel--agent-invocation)
                         mevedel--agent-invocation)))
    (if (not invocation)
        "/root"
      (let* ((agent-id (mevedel-agent-invocation-agent-id invocation))
             (entry
              (cl-find agent-id
                       (mevedel-session-agent-registry session)
                       :key (lambda (item)
                              (mevedel-agent-record-id (cdr item)))
                       :test #'equal)))
        (unless entry
          (error "Current agent is not registered: %s" agent-id))
        (car entry)))))

(defun mevedel-agent-control-resolve-path (session caller-path target)
  "Resolve TARGET from CALLER-PATH to an addressable path in SESSION."
  (unless (mevedel-agent-control--canonical-path-p caller-path)
    (error "Invalid caller agent path: %s" caller-path))
  (unless (and (stringp target) (not (string-empty-p target)))
    (user-error "Agent target must be a non-empty path"))
  (let ((path
         (if (string-prefix-p "/" target)
             target
           (concat caller-path "/" target))))
    (unless (mevedel-agent-control--canonical-path-p path)
      (user-error "Invalid agent target: %s" target))
    (unless (or (equal path "/root")
                (assoc path (mevedel-session-agent-registry session)))
      (user-error "Unknown agent target: %s" target))
    path))

(defun mevedel-agent-control-list-agents (session &optional path-prefix)
  "Return SESSION's minimal roster, optionally below PATH-PREFIX."
  (when (and path-prefix
             (not (mevedel-agent-control--canonical-path-p path-prefix)))
    (user-error "Invalid agent path prefix: %s" path-prefix))
  (let* ((records
          (mapcar
           (lambda (entry)
             (let ((record (cdr entry)))
               (list :path (mevedel-agent-record-path record)
                     :role (mevedel-agent-record-role record)
                     :activity
                     (symbol-name
                      (mevedel-agent-record-activity record)))))
           (mevedel-session-agent-registry session)))
         (roster
          (cons (list :path "/root" :role "default"
                      :activity
                      (symbol-name
                       (mevedel-session-agent-root-activity session)))
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

(defun mevedel-agent-control--reserve (session task-name)
  "Atomically reserve TASK-NAME and one active slot in SESSION."
  (let* ((path (concat "/root/" task-name))
         (registry (mevedel-session-agent-registry session)))
    (when (assoc path registry)
      (user-error "Agent path is already reserved: %s" path))
    (when (>= (mevedel-agent-control--active-count session)
              (mevedel-session-agent-turn-capacity session))
      (user-error "Agent tree is at its active-turn capacity"))
    (let ((record
           (mevedel-agent-record--create
            :path path
            :parent-path "/root"
            :role "default"
            :configuration '(:role "default")
            :activity 'starting)))
      (mevedel-session--set-agent-registry
       session (cons (cons path record) registry))
      record)))

(defun mevedel-agent-control--rollback (session record)
  "Remove unpublished RECORD from SESSION and release its slot."
  (mevedel-session--set-agent-registry
   session
   (assoc-delete-all (mevedel-agent-record-path record)
                     (mevedel-session-agent-registry session)))
  (when-let* ((invocation (mevedel-agent-record-invocation record)))
    (mevedel-agent-runtime-dispatch--abandon-persistence invocation)
    (when-let* ((buffer (mevedel-agent-invocation-buffer invocation))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer))))

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
      (mevedel-agent-runtime--ctx-push-message
       session
       (list :type 'RESULT
             :sender (mevedel-agent-record-path record)
             :recipient (mevedel-agent-record-parent-path record)
             :outcome outcome
             :payload (mevedel-agent-control--bounded-result record payload)
             :timestamp (current-time))))))

(defun mevedel-agent-control--record-invocation (record invocation)
  "Attach INVOCATION and its conversation location to RECORD."
  (setf (mevedel-agent-record-id record)
        (mevedel-agent-invocation-agent-id invocation))
  (setf (mevedel-agent-record-invocation record) invocation)
  (setf (mevedel-agent-record-conversation-buffer record)
        (mevedel-agent-invocation-buffer invocation))
  (setf (mevedel-agent-record-conversation-location record)
        (mevedel-agent-invocation-transcript-relative-path invocation)))

(cl-defun mevedel-agent-control--dispatch-followup
    (session record message
             &key parent-fsm message-handler terminal-handler)
  "Dispatch MESSAGE as RECORD's next provider turn in SESSION."
  (let ((buffer (mevedel-agent-record-conversation-buffer record)))
    (unless (buffer-live-p buffer)
      (error "Agent conversation is not live: %s"
             (mevedel-agent-record-path record)))
    (unless
        (mevedel-agent-runtime-dispatch
         #'ignore (mevedel-agent-default)
         (file-name-nondirectory (mevedel-agent-record-path record)) message
         :background t
         :parent-context session
         :parent-fsm parent-fsm
         :message-handler message-handler
         :terminal-handler terminal-handler
         :retained-id (mevedel-agent-record-id record)
         :retained-buffer buffer
         :retained-transcript
         (mevedel-agent-record-conversation-location record)
         :on-invocation
         (apply-partially
          #'mevedel-agent-control--record-invocation record)
         :on-settle
         (apply-partially
          #'mevedel-agent-control--settle session record))
      (error "Agent provider request did not start"))
    (when (eq (mevedel-agent-record-activity record) 'starting)
      (setf (mevedel-agent-record-activity record) 'running))
    record))

(cl-defun mevedel-agent-control-followup
    (session target message
             &key parent-fsm message-handler terminal-handler)
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
          (mevedel-agent-runtime-steer
           (mevedel-agent-record-invocation record)
           caller-path message)
        (when (>= (mevedel-agent-control--active-count session)
                  (mevedel-session-agent-turn-capacity session))
          (user-error "Agent tree is at its active-turn capacity"))
        (setf (mevedel-agent-record-activity record) 'starting)
        (condition-case err
            (mevedel-agent-control--dispatch-followup
             session record message
             :parent-fsm parent-fsm
             :message-handler message-handler
             :terminal-handler terminal-handler)
          (error
           (setf (mevedel-agent-record-activity record) 'idle)
           (setf (mevedel-agent-record-invocation record) nil)
           (signal (car err) (cdr err)))))
      record)))

(cl-defun mevedel-agent-control-spawn
    (session task-name message
             &key parent-fsm message-handler terminal-handler)
  "Spawn TASK-NAME with MESSAGE in SESSION and return its retained record."
  (mevedel-agent-control--validate-spawn session task-name message)
  (let ((record (mevedel-agent-control--reserve session task-name))
        committed)
    (unwind-protect
        (progn
          (require 'mevedel-agents)
          (require 'mevedel-agent-runtime)
          (unless
              (mevedel-agent-runtime-dispatch
               #'ignore (mevedel-agent-default) task-name message
               :background t
               :parent-context session
               :parent-fsm parent-fsm
               :message-handler message-handler
               :terminal-handler terminal-handler
               :on-invocation
               (apply-partially
                #'mevedel-agent-control--record-invocation record)
               :on-settle
               (apply-partially
                #'mevedel-agent-control--settle session record))
            (error "Agent provider request did not start"))
          (unless (mevedel-agent-record-conversation-location record)
            (error "Agent conversation could not be persisted"))
          (when (eq (mevedel-agent-record-activity record) 'starting)
            (setf (mevedel-agent-record-activity record) 'running))
          (setq committed t)
          record)
      (unless committed
        (mevedel-agent-control--rollback session record)))))

(provide 'mevedel-agent-control)

;;; mevedel-agent-control.el ends here
