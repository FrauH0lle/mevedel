;;; mevedel-agent-persistence.el --- Durable agent-tree storage -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the explicit retained-agent registry's sidecar codec and cold-resume
;; hydration.  Live admission, messaging, waiting, and settlement remain in
;; `mevedel-agent-control'; this module is the persistence boundary for the
;; tree's identities, frozen configurations, mailboxes, and conversations.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-agent-control)
  (require 'mevedel-agents)
  (require 'mevedel-structs)
  (require 'subr-x))

;; `gptel-request'
(declare-function gptel-backend-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-backend-p "ext:gptel-request" (cl-x))
(declare-function gptel-get-backend "ext:gptel-request" (name))
(declare-function gptel-get-tool "ext:gptel-request" (path))
(declare-function gptel-tool-category "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-p "ext:gptel-request" (cl-x))

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-active-activity-p
                  "mevedel-agent-control" (activity))
(declare-function mevedel-agent-control-recover-interrupted
                  "mevedel-agent-control" (session))

;; `mevedel-agent-exec'
(declare-function mevedel-agent-exec--allocate-agent-buffer
                  "mevedel-agent-exec" (invocation parent-data-buffer))
(declare-function mevedel-agent-exec--apply-request-locals
                  "mevedel-agent-exec" (buffer values))
(defvar mevedel--agent-invocation)

;; `mevedel-reminders'
(declare-function mevedel-reminders--recipe-p
                  "mevedel-reminders" (recipe))
(declare-function mevedel-reminders-restore-agent-templates
                  "mevedel-reminders" (recipes))
(declare-function mevedel-reminders-serialize-agent-templates
                  "mevedel-reminders" (reminders))

;; `mevedel-transcript'
(declare-function mevedel-transcript-normalize-properties
                  "mevedel-transcript" ())

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-gptel-state
                  "mevedel-transcript-restore" ())

;; `mevedel-utilities'
(declare-function mevedel--plain-data-p "mevedel-utilities" (value))

(define-error 'mevedel-agent-persistence-invalid-data
  "Invalid persisted agent data")


;;
;;; Sidecar codec

(defun mevedel-agent-persistence-sanitize-mailbox (raw recipient)
  "Return canonical unread records from reverse-order queue RAW for RECIPIENT.

Malformed records are dropped.  The returned queue keeps RAW's stored order;
callers reverse it only when delivering the mailbox as FIFO."
  (require 'mevedel-utilities)
  (cl-labels
      ((timestamp-p (value)
         (or (integerp value)
             (floatp value)
             (and (proper-list-p value)
                  value
                  (cl-every #'integerp value)))))
    (cl-loop
     for entry in (and (proper-list-p raw) raw)
     for type = (and (proper-list-p entry) (plist-get entry :type))
     for sender = (and type (plist-get entry :sender))
     for target = (and type (plist-get entry :recipient))
     for payload = (and type (plist-get entry :payload))
     for timestamp = (and type (plist-get entry :timestamp))
     when
     (and (memq type '(MAIL RESULT USER))
          (equal target recipient)
          (stringp payload)
          (timestamp-p timestamp)
          (pcase type
            ('USER (and (equal sender "user") (equal recipient "/root")))
            ('MAIL (mevedel-agent-path-p sender))
            ('RESULT
             (and (mevedel-agent-path-p sender)
                  (memq (plist-get entry :outcome)
                        '(completed errored interrupted))))))
     collect
     (let ((clean
            (list :type type
                  :sender sender
                  :recipient target
                  :payload payload
                  :timestamp (copy-tree timestamp))))
       (when (eq type 'RESULT)
         (setq clean
               (plist-put clean :outcome (plist-get entry :outcome))))
       (when (and (eq type 'USER)
                  (mevedel--plain-data-p
                   (plist-get entry :transcript-payload)))
         (setq clean
               (plist-put clean :transcript-payload
                          (copy-tree
                           (plist-get entry :transcript-payload)))))
       (when (and (eq type 'USER)
                  (mevedel--plain-data-p
                   (plist-get entry :hook-audits)))
         (setq clean
               (plist-put clean :hook-audits
                          (copy-tree (plist-get entry :hook-audits)))))
       clean))))

(defun mevedel-agent-persistence--invalid (format-string &rest args)
  "Signal invalid persisted agent data described by FORMAT-STRING and ARGS."
  (signal 'mevedel-agent-persistence-invalid-data
          (list (apply #'format format-string args))))

(defun mevedel-agent-persistence--encode-local (symbol value)
  "Encode frozen request-local SYMBOL's live VALUE as read-safe data."
  (cond
   ((eq symbol 'gptel-backend)
    (unless (and (fboundp 'gptel-backend-p) (gptel-backend-p value))
      (error "Invalid live agent backend"))
    (gptel-backend-name value))
   ((eq symbol 'gptel-tools)
    (unless (and (proper-list-p value) (cl-every #'gptel-tool-p value))
      (error "Invalid live agent tools"))
    (mapcar
     (lambda (tool)
       (list (or (gptel-tool-category tool) "misc")
             (gptel-tool-name tool)))
     value))
   ((eq symbol 'gptel-context)
    (unless (proper-list-p value)
      (error "Invalid live agent context"))
    (cl-loop
     for item in value
     when (or (stringp item)
              (and (proper-list-p item)
                   (stringp (car item))
                   (mevedel--plain-data-p item)))
     collect (copy-tree item)))
   ((mevedel--plain-data-p value)
    (copy-tree value))
   (t (error "Invalid live agent request local: %s" symbol))))

(defun mevedel-agent-persistence--encode-configuration (configuration)
  "Encode frozen agent CONFIGURATION as read-safe sidecar data."
  (unless (mevedel-agent-configuration-p configuration)
    (error "Invalid live agent configuration"))
  (let* ((agent (mevedel-agent-configuration-agent configuration))
         (name (mevedel-agent-name agent))
         (description (mevedel-agent-description agent))
         (tools (mevedel-agent-tools agent))
         (system-prompt (mevedel-agent-system-prompt agent))
         (max-turns (mevedel-agent-max-turns agent))
         (hook-rules (mevedel-agent-hook-rules agent))
         (request-locals
          (mevedel-agent-configuration-request-locals configuration)))
    (unless
        (and (stringp name) (not (string-empty-p name))
             (stringp description)
             (proper-list-p tools) (mevedel--plain-data-p tools)
             (or (null system-prompt) (stringp system-prompt))
             (or (null max-turns)
                 (and (integerp max-turns) (> max-turns 0)))
             (proper-list-p hook-rules) (mevedel--plain-data-p hook-rules)
             (proper-list-p request-locals))
      (error "Invalid live frozen agent configuration"))
    (list
     :agent
     (list :name name
           :description description
           :tools (copy-tree tools)
           :system-prompt system-prompt
           :max-turns max-turns
           :reminders
           (mevedel-reminders-serialize-agent-templates
            (mevedel-agent-reminders agent))
           :hook-rules (copy-tree hook-rules))
     :request-locals
     (mapcar
      (lambda (entry)
        (unless (and (consp entry) (symbolp (car entry)))
          (error "Invalid live agent request local name"))
        (cons (car entry)
              (mevedel-agent-persistence--encode-local
               (car entry) (cdr entry))))
      request-locals))))

(defun mevedel-agent-persistence--identity-p
    (id path parent role activity location seen-paths seen-ids)
  "Return non-nil when retained identity fields form one new valid record."
  (let ((expected-parent
         (and (mevedel-agent-path-p path)
              (directory-file-name (file-name-directory path)))))
    (and (stringp id) (not (string-empty-p id))
         (mevedel-agent-path-p path) (not (equal path "/root"))
         (mevedel-agent-path-p parent) (equal parent expected-parent)
         (stringp role) (not (string-empty-p role))
         (or (eq activity 'idle)
             (mevedel-agent-control-active-activity-p activity))
         (stringp location) (not (string-empty-p location))
         (not (file-name-absolute-p location))
         (string-suffix-p ".chat.org" location)
         (not (string-match-p
               "\\(?:^\\|/\\)\\.\\.\\(?:/\\|$\\)" location))
         (not (gethash path seen-paths))
         (not (gethash id seen-ids)))))

(defun mevedel-agent-persistence--serialize-record
    (entry seen-paths seen-ids)
  "Encode registry ENTRY, rejecting duplicates in SEEN-PATHS and SEEN-IDS."
  (let* ((record (cdr entry))
         (id (and (mevedel-agent-record-p record)
                  (mevedel-agent-record-id record)))
         (path (and id (mevedel-agent-record-path record)))
         (parent (and id (mevedel-agent-record-parent-path record)))
         (role (and id (mevedel-agent-record-role record)))
         (activity (and id (mevedel-agent-record-activity record)))
         (location
          (and id (mevedel-agent-record-conversation-location record))))
    (unless
        (and (equal (car entry) path)
             (mevedel-agent-persistence--identity-p
              id path parent role activity location seen-paths seen-ids))
      (error "Invalid live agent registry entry"))
    (let ((encoded
           (list :id id
                 :path path
                 :parent-path parent
                 :role role
                 :configuration
                 (mevedel-agent-persistence--encode-configuration
                  (mevedel-agent-record-configuration record))
                 :activity activity
                 :conversation-location location
                 :mailbox
                 (mevedel-agent-persistence-sanitize-mailbox
                  (mevedel-agent-record-mailbox record) path))))
      (puthash path t seen-paths)
      (puthash id t seen-ids)
      encoded)))

(defun mevedel-agent-persistence-serialize-registry (session)
  "Return SESSION's retained registry as read-safe sidecar data.

Invalid live registry state signals an error so a save cannot appear to
succeed after silently losing an addressable agent."
  (require 'mevedel-agent-control)
  (require 'mevedel-reminders)
  (require 'mevedel-utilities)
  (let ((seen-paths (make-hash-table :test #'equal))
        (seen-ids (make-hash-table :test #'equal)))
    (mapcar
     (lambda (entry)
       (mevedel-agent-persistence--serialize-record
        entry seen-paths seen-ids))
     (mevedel-session-agent-registry session))))

(defun mevedel-agent-persistence--decode-local (symbol encoded)
  "Decode frozen request-local SYMBOL from read-safe ENCODED data."
  (cond
   ((eq symbol 'gptel-backend)
    (unless (stringp encoded)
      (mevedel-agent-persistence--invalid
       "Invalid persisted agent backend"))
    (condition-case nil
        (gptel-get-backend encoded)
      (user-error
       (mevedel-agent-persistence--invalid
        "Unknown persisted agent backend: %s" encoded))))
   ((eq symbol 'gptel-tools)
    (unless (proper-list-p encoded)
      (mevedel-agent-persistence--invalid "Invalid persisted agent tools"))
    (mapcar
     (lambda (path)
       (unless (and (proper-list-p path)
                    (= (length path) 2)
                    (cl-every #'stringp path))
         (mevedel-agent-persistence--invalid
          "Invalid persisted agent tool path"))
       (let ((tool
              (condition-case nil
                  (gptel-get-tool path)
                (error
                 (mevedel-agent-persistence--invalid
                  "Unknown persisted agent tool: %S" path)))))
         (unless (gptel-tool-p tool)
           (mevedel-agent-persistence--invalid
            "Unknown persisted agent tool: %S" path))
         tool))
     encoded))
   ((eq symbol 'gptel-context)
    (unless
        (and (proper-list-p encoded)
             (cl-every
              (lambda (item)
                (or (stringp item)
                    (and (proper-list-p item)
                         (stringp (car item))
                         (mevedel--plain-data-p item))))
              encoded))
      (mevedel-agent-persistence--invalid
       "Invalid persisted agent context"))
    (copy-tree encoded))
   ((mevedel--plain-data-p encoded)
    (copy-tree encoded))
   (t
    (mevedel-agent-persistence--invalid
     "Invalid persisted agent request local"))))

(defun mevedel-agent-persistence--decode-configuration (persisted role)
  "Decode PERSISTED frozen configuration for ROLE."
  (let* ((agent-data (and (proper-list-p persisted)
                          (plist-get persisted :agent)))
         (name (and (proper-list-p agent-data)
                    (plist-get agent-data :name)))
         (description (and name (plist-get agent-data :description)))
         (tools (and name (plist-get agent-data :tools)))
         (system-prompt (and name (plist-get agent-data :system-prompt)))
         (max-turns (and name (plist-get agent-data :max-turns)))
         (reminders-data (and name (plist-get agent-data :reminders)))
         (hook-rules (and name (plist-get agent-data :hook-rules)))
         (locals-data (and (proper-list-p persisted)
                           (plist-get persisted :request-locals))))
    (unless
        (and (stringp name) (equal name role)
             (stringp description)
             (proper-list-p tools) (mevedel--plain-data-p tools)
             (or (null system-prompt) (stringp system-prompt))
             (or (null max-turns)
                 (and (integerp max-turns) (> max-turns 0)))
             (proper-list-p reminders-data)
             (cl-every #'mevedel-reminders--recipe-p reminders-data)
             (proper-list-p hook-rules) (mevedel--plain-data-p hook-rules)
             (proper-list-p locals-data))
      (mevedel-agent-persistence--invalid
       "Invalid persisted agent configuration"))
    (let* ((agent
            (mevedel-agent--create
             :name name
             :description description
             :tools (copy-tree tools)
             :system-prompt system-prompt
             :max-turns max-turns
             :reminders
             (mevedel-reminders-restore-agent-templates reminders-data)
             :hook-rules (copy-tree hook-rules)
             :frozen-p t))
           (locals
            (mapcar
             (lambda (entry)
               (unless (and (consp entry) (symbolp (car entry)))
                 (mevedel-agent-persistence--invalid
                  "Invalid persisted agent request local name"))
               (cons (car entry)
                     (mevedel-agent-persistence--decode-local
                      (car entry) (cdr entry))))
             locals-data)))
      (mevedel-agent-configuration--create
       :agent agent :request-locals locals))))

(defun mevedel-agent-persistence--deserialize-record
    (entry seen-paths seen-ids)
  "Decode registry ENTRY, rejecting duplicates in SEEN-PATHS and SEEN-IDS."
  (let* ((id (and (proper-list-p entry) (plist-get entry :id)))
         (path (and id (plist-get entry :path)))
         (parent (and id (plist-get entry :parent-path)))
         (role (and id (plist-get entry :role)))
         (activity (and id (plist-get entry :activity)))
         (location (and id (plist-get entry :conversation-location))))
    (unless (mevedel-agent-persistence--identity-p
             id path parent role activity location seen-paths seen-ids)
      (mevedel-agent-persistence--invalid
       "Invalid persisted agent identity"))
    (let ((record
           (mevedel-agent-record--create
            :id id
            :path path
            :parent-path parent
            :role role
            :configuration
            (mevedel-agent-persistence--decode-configuration
             (plist-get entry :configuration) role)
            :activity activity
            :conversation-location location
            :mailbox
            (mevedel-agent-persistence-sanitize-mailbox
             (plist-get entry :mailbox) path))))
      (puthash path t seen-paths)
      (puthash id t seen-ids)
      (cons path record))))

(defun mevedel-agent-persistence--drop-orphans (candidates)
  "Return CANDIDATES without records whose parent is absent."
  (let ((changed t))
    (while changed
      (setq changed nil
            candidates
            (cl-remove-if
             (lambda (entry)
               (let ((parent (mevedel-agent-record-parent-path (cdr entry))))
                 (when (and (not (equal parent "/root"))
                            (not (assoc parent candidates)))
                   (setq changed t)
                   t)))
             candidates)))
    candidates))

(defun mevedel-agent-persistence-deserialize-registry (raw)
  "Return a validated retained registry decoded from sidecar data RAW.

Malformed identities are diagnosed and dropped independently.  Unexpected
programming errors propagate instead of masquerading as corrupt user data."
  (require 'gptel)
  (require 'mevedel-agent-control)
  (require 'mevedel-agents)
  (require 'mevedel-reminders)
  (require 'mevedel-utilities)
  (let ((seen-paths (make-hash-table :test #'equal))
        (seen-ids (make-hash-table :test #'equal))
        candidates)
    (dolist (entry (and (proper-list-p raw) raw))
      (condition-case err
          (push (mevedel-agent-persistence--deserialize-record
                 entry seen-paths seen-ids)
                candidates)
        (mevedel-agent-persistence-invalid-data
         (display-warning
          'mevedel
          (format "Persisted agent identity was dropped: %s"
                  (error-message-string err))
          :warning))))
    (mevedel-agent-persistence--drop-orphans (nreverse candidates))))


;;
;;; Cold resume

(defun mevedel-agent-persistence-transcript-path-p (path save-path)
  "Return non-nil when PATH resolves safely below SAVE-PATH's agents dir."
  (and (stringp path)
       (not (string-empty-p path))
       (not (file-name-absolute-p path))
       (not (string-match-p "\\(?:^\\|/\\)\\.\\.\\(?:/\\|$\\)" path))
       (string-suffix-p ".chat.org" path)
       (let* ((agents-dir (file-name-as-directory
                           (expand-file-name "agents" save-path)))
              (resolved (expand-file-name path save-path)))
         (and (string-prefix-p agents-dir
                               (file-name-as-directory
                                (file-name-directory resolved)))
              (or (not (file-exists-p resolved))
                  (file-in-directory-p
                   (file-truename resolved)
                  (file-truename agents-dir)))))))

(defun mevedel-agent-persistence-restore-tree
    (session root-buffer readonly-p)
  "Hydrate SESSION's retained conversations below ROOT-BUFFER.

Invalid or missing conversation files reject only their own identity and its
descendants.  When READONLY-P is nil, active persisted turns recover as
interrupted without dispatching a provider request.  Return the number of
dropped or recovered records."
  (require 'mevedel-agent-control)
  (require 'mevedel-agent-exec)
  (require 'mevedel-agents)
  (let* ((save-path (mevedel-session-save-path session))
         (records
          (sort (copy-sequence (mevedel-session-agent-registry session))
                (lambda (left right)
                  (< (length (split-string (car left) "/" t))
                     (length (split-string (car right) "/" t))))))
         accepted
         (dropped 0))
    (dolist (entry records)
      (let* ((record (cdr entry))
             (path (mevedel-agent-record-path record))
             (parent (mevedel-agent-record-parent-path record))
             (relative
              (mevedel-agent-record-conversation-location record))
             (absolute (and relative (expand-file-name relative save-path)))
             buffer)
        (condition-case err
            (progn
              (unless (and (or (equal parent "/root")
                               (assoc parent accepted))
                           (mevedel-agent-persistence-transcript-path-p
                            relative save-path)
                           (file-regular-p absolute))
                (signal 'mevedel-agent-persistence-invalid-data
                        (list (format "Invalid retained agent conversation: %s"
                                      path))))
              (let* ((configuration
                      (mevedel-agent-record-configuration record))
                     (agent
                      (mevedel-agent-configuration-agent configuration))
                     (invocation (mevedel-agent-invocation-create agent)))
                (setf (mevedel-agent-invocation-agent-id invocation)
                      (mevedel-agent-record-id record)
                      (mevedel-agent-invocation-path invocation) path
                      (mevedel-agent-invocation-description invocation)
                      (mevedel-agent-record-role record)
                      (mevedel-agent-invocation-parent-session invocation)
                      session
                      (mevedel-agent-invocation-parent-data-buffer invocation)
                      root-buffer
                      (mevedel-agent-invocation-transcript-relative-path
                       invocation)
                      relative
                      (mevedel-agent-invocation-transcript-status invocation)
                      (cond
                       ((eq (mevedel-agent-record-activity record) 'idle)
                        'completed)
                       (readonly-p 'running)
                       (t 'aborted))
                      (mevedel-agent-invocation-frozen-configuration invocation)
                      configuration)
                (setq buffer
                      (mevedel-agent-exec--allocate-agent-buffer
                       invocation root-buffer))
                (setf (mevedel-agent-invocation-buffer invocation) buffer)
                (with-current-buffer buffer
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert-file-contents absolute))
                  (set-visited-file-name absolute t)
                  (setq-local mevedel--agent-invocation invocation)
                  (require 'mevedel-transcript-restore)
                  (mevedel-transcript-restore-gptel-state)
                  (mevedel-transcript-normalize-properties)
                  (mevedel-agent-exec--apply-request-locals
                   buffer
                   (mevedel-agent-configuration-request-locals
                    configuration))
                  (set-buffer-modified-p nil)
                  (set-visited-file-modtime))
                (when (and (not readonly-p)
                           (not (eq (mevedel-agent-record-activity record)
                                    'idle)))
                  (when-let* ((entry
                               (assoc
                                (mevedel-agent-record-id record)
                                (mevedel-session-agent-transcripts session))))
                    (let ((metadata (copy-sequence (cdr entry))))
                      (setf (plist-get metadata :status) 'aborted
                            (plist-get metadata :reason)
                            "Interrupted during session recovery")
                      (setcdr entry metadata))))
                (setf (mevedel-agent-record-conversation-buffer record) buffer
                      (mevedel-agent-record-invocation record) nil)
                (when readonly-p
                  (setf (mevedel-agent-record-activity record) 'idle
                        (mevedel-agent-record-blockers record) nil))
                (push entry accepted)))
          ((mevedel-agent-persistence-invalid-data file-error)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (kill-buffer buffer))
           (cl-incf dropped)
           (display-warning
            'mevedel
            (format "Retained agent %s was not restored: %s"
                    path (error-message-string err))
            :warning)))))
    (setq accepted (nreverse accepted))
    (setf (mevedel-session-agent-registry session) accepted)
    (+ dropped
       (if readonly-p
           0
         (mevedel-agent-control-recover-interrupted session)))))

(provide 'mevedel-agent-persistence)

;;; mevedel-agent-persistence.el ends here
