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

;; `mevedel-agent-runtime'
(declare-function mevedel-agent-runtime--ctx-push-message
                  "mevedel-agent-runtime" (ctx message))
(declare-function mevedel-agent-runtime-dispatch
                  "mevedel-agent-runtime" t t)
(declare-function mevedel-agent-runtime-dispatch--abandon-persistence
                  "mevedel-agent-runtime" (invocation))

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
  (setf (mevedel-agent-record-conversation-location record)
        (mevedel-agent-invocation-transcript-relative-path invocation)))

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
