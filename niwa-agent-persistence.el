;;; niwa-agent-persistence.el --- Durable agent-tree storage -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the explicit retained-agent registry's sidecar codec and cold-resume
;; hydration.  Live admission, messaging, waiting, and settlement remain in
;; `niwa-agent-control'; this module is the persistence boundary for the
;; tree's identities, frozen configurations, mailboxes, and conversations.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'niwa-agent-control)
(require 'niwa-agents)
(require 'niwa-execution-target)
(require 'niwa-reminders)
(require 'niwa-structs)
(require 'niwa-tool-registry)
(require 'niwa-utilities)

;; `gptel-request'
(declare-function gptel-backend-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-backend-p "ext:gptel-request" (cl-x))
(declare-function gptel-get-backend "ext:gptel-request" (name))
(declare-function gptel-get-tool "ext:gptel-request" (path))
(declare-function gptel-tool-category "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-p "ext:gptel-request" (cl-x))

;; `niwa-agent-conversation'
(declare-function niwa-agent-conversation-hydrate
                  "niwa-agent-conversation"
                  (invocation parent-data-buffer logical-path
                              &optional inspection))
(autoload 'niwa-agent-conversation-hydrate
  "niwa-agent-conversation")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-artifact-present-p
                  "niwa-session-artifacts"
                  (session logical &optional committed-only))
(autoload 'niwa-session-artifacts-artifact-present-p
  "niwa-session-artifacts")

(define-error 'niwa-agent-persistence-invalid-data
  "Invalid persisted agent data")


;;
;;; Sidecar codec

(defun niwa-agent-persistence-sanitize-mailbox (raw recipient)
  "Return canonical unread records from reverse-order queue RAW for RECIPIENT.

Malformed records are dropped.  The returned queue keeps RAW's stored order;
callers reverse it only when delivering the mailbox as FIFO.  Runtime-only
fields such as a record's wake mark are not part of the canonical form."
  (cl-labels
      ((timestamp-p (value)
         (or (integerp value)
             (floatp value)
             (and (proper-list-p value)
                  value
                  (cl-every #'integerp value)))))
    (cl-loop
     for entry in (and (proper-list-p raw) raw)
     for id = (and (proper-list-p entry) (plist-get entry :id))
     for type = (and (proper-list-p entry) (plist-get entry :type))
     for sender = (and type (plist-get entry :sender))
     for target = (and type (plist-get entry :recipient))
     for payload = (and type (plist-get entry :payload))
     for timestamp = (and type (plist-get entry :timestamp))
     when
     (and (stringp id) (not (string-empty-p id))
          (memq type '(EXECUTION MAIL MONITOR MONITOR_TERMINAL RESULT USER WAKE_UP))
          (equal target recipient)
          (stringp payload)
          (timestamp-p timestamp)
          (pcase type
            ('USER (and (equal sender "user") (equal recipient "/root")))
            ('WAKE_UP (and (equal sender "/root") (equal recipient "/root")))
            ((or 'MONITOR 'MONITOR_TERMINAL)
             (and (equal sender "/root") (equal recipient "/root")
                  (stringp (plist-get entry :execution-id))
                  (stringp (plist-get entry :description))))
            ('EXECUTION
             (and (niwa-agent-path-p sender)
                  (niwa-agent-path-p recipient)
                  (or (and (equal sender "/root") (equal recipient "/root"))
                      (string-match-p
                       (concat "\\`" (regexp-quote recipient) "/[^/]+\\'")
                       sender))))
            ('MAIL (niwa-agent-path-p sender))
            ('RESULT
             (and (niwa-agent-path-p sender)
                  (memq (plist-get entry :outcome)
                        '(completed errored interrupted))))))
     collect
     (let ((clean
            (list :id (copy-sequence id)
                  :type type
                  :sender sender
                  :recipient target
                  :payload payload
                  :timestamp (copy-tree timestamp))))
       (when (eq type 'RESULT)
         (setq clean
               (plist-put clean :outcome (plist-get entry :outcome))))
       (when (memq type '(MONITOR MONITOR_TERMINAL))
         (setq clean
               (append clean
                       (list :execution-id
                             (copy-sequence (plist-get entry :execution-id))
                             :description
                             (copy-sequence (plist-get entry :description))))))
       (when (and (plist-get entry :meta)
                  (niwa--plain-data-p (plist-get entry :meta)))
         (setq clean
               (plist-put clean :meta (copy-tree (plist-get entry :meta)))))
       (when (and (eq type 'USER)
                  (niwa--plain-data-p
                   (plist-get entry :transcript-payload)))
         (setq clean
               (plist-put clean :transcript-payload
                          (copy-tree
                           (plist-get entry :transcript-payload)))))
       (when (and (eq type 'USER)
                  (niwa--plain-data-p
                   (plist-get entry :hook-audits)))
         (setq clean
               (plist-put clean :hook-audits
                          (copy-tree (plist-get entry :hook-audits)))))
       clean))))

(defun niwa-agent-persistence--invalid (format-string &rest args)
  "Signal invalid persisted agent data described by FORMAT-STRING and ARGS."
  (signal 'niwa-agent-persistence-invalid-data
          (list (apply #'format format-string args))))

(defun niwa-agent-persistence--encode-local (symbol value)
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
                   (niwa--plain-data-p item)))
     collect (copy-tree item)))
   ((niwa--plain-data-p value)
    (copy-tree value))
   (t (error "Invalid live agent request local: %s" symbol))))

(defun niwa-agent-persistence--encode-configuration (configuration)
  "Encode frozen agent CONFIGURATION as read-safe sidecar data."
  (unless (niwa-agent-configuration-p configuration)
    (error "Invalid live agent configuration"))
  (let* ((agent (niwa-agent-configuration-agent configuration))
         (name (niwa-agent-name agent))
         (description (niwa-agent-description agent))
         (tools (niwa-agent-tools agent))
         (system-prompt (niwa-agent-system-prompt agent))
         (max-turns (niwa-agent-max-turns agent))
         (hook-rules (niwa-agent-hook-rules agent))
         (request-locals
          (niwa-agent-configuration-request-locals configuration)))
    (unless
        (and (stringp name) (not (string-empty-p name))
             (stringp description)
             (proper-list-p tools) (niwa--plain-data-p tools)
             (or (null system-prompt) (stringp system-prompt))
             (or (null max-turns)
                 (and (integerp max-turns) (> max-turns 0)))
             (proper-list-p hook-rules) (niwa--plain-data-p hook-rules)
             (niwa-agent-request-locals-p request-locals t))
      (error "Invalid live frozen agent configuration"))
    (list
     :agent
     (list :name name
           :description description
           :tools (copy-tree tools)
           :system-prompt system-prompt
           :context-components (copy-sequence (niwa-agent-context-components agent))
           :max-turns max-turns
           :reminders
           (niwa-reminders-serialize-agent-templates
            (niwa-agent-reminders agent))
           :hook-rules (copy-tree hook-rules))
     :request-locals
     (mapcar
      (lambda (entry)
        (cons (car entry)
              (niwa-agent-persistence--encode-local
               (car entry) (cdr entry))))
      request-locals))))

(defun niwa-agent-persistence--identity-p
    (id path parent role activity location seen-paths seen-ids)
  "Return non-nil when retained identity fields form one new valid record."
  (let ((expected-parent
         (and (niwa-agent-path-p path)
              (directory-file-name (file-name-directory path)))))
    (and (stringp id) (not (string-empty-p id))
         (niwa-agent-path-p path) (not (equal path "/root"))
         (niwa-agent-path-p parent) (equal parent expected-parent)
         (stringp role) (not (string-empty-p role))
         (or (eq activity 'idle)
             (niwa-agent-control-active-activity-p activity))
         (stringp location) (not (string-empty-p location))
         (not (file-name-absolute-p location))
         (string-suffix-p ".chat.org" location)
         (not (string-match-p
               "\\(?:^\\|/\\)\\.\\.\\(?:/\\|$\\)" location))
         (not (gethash path seen-paths))
         (not (gethash id seen-ids)))))

(defun niwa-agent-persistence--settled-result-p
    (payload outcome activity)
  "Return non-nil when settled PAYLOAD and OUTCOME fit ACTIVITY.

Active records cannot retain a previous settled result.  Idle records may
have no result yet, but a stored result always includes both fields."
  (or (and (null payload) (null outcome))
      (and (eq activity 'idle)
           (stringp payload)
           (memq outcome '(completed errored interrupted)))))

(defun niwa-agent-persistence--serialize-record
    (entry seen-paths seen-ids target)
  "Encode registry ENTRY for TARGET's native filesystem domain.
Reject duplicates in SEEN-PATHS and SEEN-IDS."
  (let* ((record (cdr entry))
         (id (and (niwa-agent-record-p record)
                  (niwa-agent-record-id record)))
         (path (and id (niwa-agent-record-path record)))
         (parent (and id (niwa-agent-record-parent-path record)))
         (role (and id (niwa-agent-record-role record)))
         (working-directory
          (and id (niwa-agent-record-working-directory record)))
         (activity (and id (niwa-agent-record-activity record)))
         (location
          (and id (niwa-agent-record-conversation-location record)))
         (hook-context
          (and id (niwa-agent-record-hook-context-pending record)))
         (settled-result
          (and id (niwa-agent-record-settled-result record)))
         (settled-outcome
          (and id (niwa-agent-record-settled-outcome record)))
         (model-metadata
          (and id (niwa-agent-control-record-model-metadata record))))
    (unless
        (and (equal (car entry) path)
             (niwa-agent-persistence--identity-p
              id path parent role activity location seen-paths seen-ids)
             (or (null working-directory)
                 (and (stringp working-directory)
                      (file-name-absolute-p working-directory)))
             (proper-list-p hook-context)
             (niwa--plain-data-p hook-context)
             (niwa-agent-persistence--settled-result-p
              settled-result settled-outcome activity)
             (or (null model-metadata)
                 (niwa-agent-model-metadata-p model-metadata)))
      (error "Invalid live agent registry entry"))
    (let ((encoded
           (list :id id
                 :path path
                 :parent-path parent
                 :role role
                 :working-directory (and working-directory
                                         (niwa-execution-target-native-path
                                          target working-directory))
                 :configuration
                 (niwa-agent-persistence--encode-configuration
                  (niwa-agent-record-configuration record))
                 :activity activity
                 :settled-result (and settled-result
                                      (copy-sequence settled-result))
                 :settled-outcome settled-outcome
                 :model-metadata (copy-sequence model-metadata)
                 :conversation-location location
                 :hook-context-pending (copy-tree hook-context)
                 :mailbox
                 (niwa-agent-persistence-sanitize-mailbox
                  (niwa-agent-record-mailbox record) path))))
      (puthash path t seen-paths)
      (puthash id t seen-ids)
      encoded)))

(defun niwa-agent-persistence-serialize-registry (session)
  "Return SESSION's retained registry as read-safe sidecar data.

Invalid live registry state signals an error so a save cannot appear to
succeed after silently losing an addressable agent."
  (let ((seen-paths (make-hash-table :test #'equal))
        (seen-ids (make-hash-table :test #'equal))
        (configurations (make-hash-table :test #'equal)))
    (mapcar
     (lambda (entry)
       (let ((encoded (niwa-agent-persistence--serialize-record
                       entry seen-paths seen-ids
                       (niwa-session-execution-target session))))
         ;; Agents of one type share a frozen configuration of many
         ;; kilobytes.  The sidecar prints with `print-circle', so one
         ;; shared object is written once and referenced by every other
         ;; record; decoding copies what it keeps, so sharing is safe.
         (let ((configuration (plist-get encoded :configuration)))
           (plist-put encoded :configuration
                      (or (gethash configuration configurations)
                          (puthash configuration configuration
                                   configurations))))))
     (niwa-session-agent-registry session))))

(defun niwa-agent-persistence--decode-local (symbol encoded)
  "Decode frozen request-local SYMBOL from read-safe ENCODED data."
  (cond
   ((eq symbol 'gptel-backend)
    (unless (stringp encoded)
      (niwa-agent-persistence--invalid
       "Invalid persisted agent backend"))
    (condition-case nil
        (gptel-get-backend encoded)
      (user-error
       (gptel--make-backend :name encoded))))
   ((eq symbol 'gptel-tools)
    (unless (proper-list-p encoded)
      (niwa-agent-persistence--invalid "Invalid persisted agent tools"))
    (mapcar
     (lambda (path)
       (unless (and (proper-list-p path)
                    (= (length path) 2)
                    (cl-every #'stringp path))
         (niwa-agent-persistence--invalid
          "Invalid persisted agent tool path"))
       (let ((tool (or (ignore-errors (gptel-get-tool path))
                       (when (equal (car path) "niwa")
                         ;; A cold editor has not necessarily used this built-in
                         ;; yet.  Its owning registrar supplies the same schema.
                         (niwa-tool-ensure (cadr path))
                         (ignore-errors (gptel-get-tool path))))))
         (unless (gptel-tool-p tool)
           (niwa-agent-persistence--invalid
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
                         (niwa--plain-data-p item))))
              encoded))
      (niwa-agent-persistence--invalid
       "Invalid persisted agent context"))
    (copy-tree encoded))
   ((niwa--plain-data-p encoded)
    (copy-tree encoded))
   (t
    (niwa-agent-persistence--invalid
     "Invalid persisted agent request local"))))

(defun niwa-agent-persistence--decode-configuration (persisted role)
  "Decode PERSISTED frozen configuration for ROLE."
  (let* ((agent-data (and (proper-list-p persisted)
                          (plist-get persisted :agent)))
         (name (and (proper-list-p agent-data)
                    (plist-get agent-data :name)))
         (description (and name (plist-get agent-data :description)))
         (tools (and name (plist-get agent-data :tools)))
         (system-prompt (and name (plist-get agent-data :system-prompt)))
         (context-components (plist-get agent-data :context-components))
         (max-turns (and name (plist-get agent-data :max-turns)))
         (reminders-data (and name (plist-get agent-data :reminders)))
         (hook-rules (and name (plist-get agent-data :hook-rules)))
         (locals-data (and (proper-list-p persisted)
                           (plist-get persisted :request-locals))))
    (unless
        (and (proper-list-p context-components)
             (cl-every (lambda (entry)
                         (memq entry niwa-system-retained-components))
                       context-components)
             (stringp name) (equal name role)
             (stringp description)
             (proper-list-p tools) (niwa--plain-data-p tools)
             (or (null system-prompt) (stringp system-prompt))
             (or (null max-turns)
                 (and (integerp max-turns) (> max-turns 0)))
             (proper-list-p reminders-data)
             (cl-every #'niwa-reminders--recipe-p reminders-data)
             (proper-list-p hook-rules) (niwa--plain-data-p hook-rules)
             (proper-list-p locals-data))
      (niwa-agent-persistence--invalid
       "Invalid persisted agent configuration"))
    (unless (niwa-agent-request-locals-p locals-data t)
      (niwa-agent-persistence--invalid
       "Invalid persisted agent request-local schema"))
    (let* ((agent
            (niwa-agent--create
             :name name
             :description description
             :tools (copy-tree tools)
             :system-prompt system-prompt
             :context-components context-components
             :max-turns max-turns
             :reminders
             (niwa-reminders-restore-agent-templates reminders-data)
             :hook-rules (copy-tree hook-rules)
             :frozen-p t))
           (locals
            (mapcar
             (lambda (entry)
               (cons (car entry)
                     (niwa-agent-persistence--decode-local
                      (car entry) (cdr entry))))
             locals-data)))
      (niwa-agent-configuration--create
       :agent agent :request-locals locals))))

(defun niwa-agent-persistence--deserialize-record
    (entry seen-paths seen-ids target)
  "Decode registry ENTRY into TARGET's current client filesystem domain.
Reject duplicates in SEEN-PATHS and SEEN-IDS."
  (let* ((id (and (proper-list-p entry) (plist-get entry :id)))
         (path (and id (plist-get entry :path)))
         (parent (and id (plist-get entry :parent-path)))
         (role (and id (plist-get entry :role)))
         (working-directory (and id (plist-get entry :working-directory)))
         (activity (and id (plist-get entry :activity)))
         (location (and id (plist-get entry :conversation-location)))
         (hook-context (and id (plist-get entry :hook-context-pending)))
         (settled-result (and id (plist-get entry :settled-result)))
         (settled-outcome (and id (plist-get entry :settled-outcome)))
         (model-metadata (and id (plist-get entry :model-metadata))))
    (unless (and (niwa-agent-persistence--identity-p
                  id path parent role activity location seen-paths seen-ids)
                 (plist-member entry :working-directory)
                 (or (null working-directory)
                     (and (stringp working-directory)
                          (not (file-remote-p working-directory))
                          (file-name-absolute-p working-directory)))
                 (proper-list-p hook-context)
                 (niwa--plain-data-p hook-context)
                 (niwa-agent-persistence--settled-result-p
                  settled-result settled-outcome activity)
                 (or (null model-metadata)
                     (niwa-agent-model-metadata-p model-metadata)))
      (niwa-agent-persistence--invalid
       "Invalid persisted agent identity"))
    (let ((record
           (niwa-agent-record--create
            :id id
            :path path
            :parent-path parent
            :role role
            :working-directory (and working-directory
                                    (niwa-execution-target-expand-path
                                     target working-directory))
            :configuration
            (niwa-agent-persistence--decode-configuration
             (plist-get entry :configuration) role)
            :activity activity
            :settled-result (and settled-result
                                 (copy-sequence settled-result))
            :settled-outcome settled-outcome
            :model-metadata (copy-sequence model-metadata)
            :conversation-location location
            :hook-context-pending (copy-tree hook-context)
            :mailbox
            (niwa-agent-persistence-sanitize-mailbox
             (plist-get entry :mailbox) path))))
      (puthash path t seen-paths)
      (puthash id t seen-ids)
      (cons path record))))

(defun niwa-agent-persistence--drop-orphans (candidates)
  "Return CANDIDATES without records whose parent is absent."
  (let ((changed t))
    (while changed
      (setq changed nil
            candidates
            (cl-remove-if
             (lambda (entry)
               (let ((parent (niwa-agent-record-parent-path (cdr entry))))
                 (when (and (not (equal parent "/root"))
                            (not (assoc parent candidates)))
                   (setq changed t)
                   t)))
             candidates)))
    candidates))

(defun niwa-agent-persistence-deserialize-registry (raw target)
  "Return a validated retained registry decoded from sidecar data RAW.
Qualify target-native working directories through the current TARGET.

Malformed identities are diagnosed and dropped independently.  Unexpected
  programming errors propagate instead of masquerading as corrupt user data."
  (require 'gptel)
  (let ((seen-paths (make-hash-table :test #'equal))
        (seen-ids (make-hash-table :test #'equal))
        candidates)
    (dolist (entry (and (proper-list-p raw) raw))
      (condition-case err
          (push (niwa-agent-persistence--deserialize-record
                 entry seen-paths seen-ids target)
                candidates)
        (niwa-agent-persistence-invalid-data
         (niwa-diagnostic-warning
          (niwa-diagnostic-scope
           (and target (niwa-execution-target-workspace-root target))) 'niwa
          (format "Persisted agent identity was dropped: %s"
                  (error-message-string err))
          :warning))))
    (niwa-agent-persistence--drop-orphans (nreverse candidates))))


;;
;;; Cold resume

(defun niwa-agent-persistence-transcript-path-p (path save-path)
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
              (or (file-remote-p save-path)
                  (not (file-exists-p resolved))
                  (file-in-directory-p
                   (file-truename resolved)
                  (file-truename agents-dir)))))))

(defun niwa-agent-persistence-ensure-conversation
    (session record root-buffer &optional readonly-p)
  "Return RECORD's conversation in SESSION, loading it on first access.
ROOT-BUFFER supplies the live request context.  READONLY-P opens an inspection
without save authority.  Publish the resident buffer only after successful
hydration."
  (let ((resident (niwa-agent-record-conversation-buffer record)))
    (if (and (buffer-live-p resident)
             (or readonly-p
                 (not (with-current-buffer resident
                        (bound-and-true-p niwa-session--inspection-buffer-p)))))
        resident
      (unless (and (buffer-live-p root-buffer)
                   (eq session (buffer-local-value 'niwa--session root-buffer)))
        (error "Agent conversation has no root session context"))
      (let* ((path (niwa-agent-record-path record))
             (relative (niwa-agent-record-conversation-location record))
             (configuration (niwa-agent-record-configuration record)))
        (unless (and (niwa-agent-configuration-p configuration)
                     (niwa-agent-request-locals-p
                      (niwa-agent-configuration-request-locals configuration))
                     (niwa-agent-persistence-transcript-path-p
                      relative (niwa-session-save-path session)))
          (signal 'niwa-agent-persistence-invalid-data
                  (list (format "Invalid retained agent conversation: %s" path))))
        (let* ((agent (niwa-agent-configuration-agent configuration))
               (invocation (niwa-agent-invocation-create agent)))
          (setf (niwa-agent-invocation-agent-id invocation)
                (niwa-agent-record-id record)
                (niwa-agent-invocation-path invocation) path
                (niwa-agent-invocation-description invocation)
                (niwa-agent-record-role record)
                (niwa-agent-invocation-working-directory invocation)
                (niwa-agent-record-working-directory record)
                (niwa-agent-invocation-parent-session invocation)
                session
                (niwa-agent-invocation-parent-data-buffer invocation)
                root-buffer
                (niwa-agent-invocation-transcript-relative-path
                 invocation)
                relative
                (niwa-agent-invocation-transcript-status invocation)
                (cond
                 ((eq (niwa-agent-record-activity record) 'idle)
                  ;; The durable outcome is what this turn reported;
                  ;; deriving the label from activity alone tells the
                  ;; user a failed or interrupted agent finished.
                  (pcase (niwa-agent-record-settled-outcome record)
                    ('errored 'error)
                    ('interrupted 'aborted)
                    ('completed 'completed)
                    (_ 'incomplete)))
                 (readonly-p 'running)
                 (t 'aborted))
                (niwa-agent-invocation-frozen-configuration invocation)
                configuration)
          (let ((buffer (niwa-agent-conversation-hydrate
                         invocation root-buffer relative readonly-p)))
            (setf (niwa-agent-record-conversation-buffer record) buffer)
            buffer))))))

(defun niwa-agent-persistence-restore-tree
    (session root-buffer readonly-p)
  "Restore SESSION's retained identities below ROOT-BUFFER.
Idle conversations remain on disk until their first access.

Invalid or missing conversation files reject only their own identity and its
descendants.  When READONLY-P is nil, active persisted turns recover as
interrupted without dispatching a provider request.  Return the number of
dropped or recovered records."
  (let* ((save-path (niwa-session-save-path session))
         (records
          (sort (copy-sequence (niwa-session-agent-registry session))
                (lambda (left right)
                  (< (length (split-string (car left) "/" t))
                     (length (split-string (car right) "/" t))))))
         accepted
         (dropped 0))
    (dolist (entry records)
      (let* ((record (cdr entry))
             (path (niwa-agent-record-path record))
             (parent (niwa-agent-record-parent-path record))
             (relative
              (niwa-agent-record-conversation-location record))
             buffer)
        (condition-case err
            (progn
              (unless (and (or (equal parent "/root")
                               (assoc parent accepted))
                           (niwa-agent-persistence-transcript-path-p
                            relative save-path)
                           (niwa-session-artifacts-artifact-present-p
                            session relative))
                (signal 'niwa-agent-persistence-invalid-data
                        (list (format "Invalid retained agent conversation: %s"
                                      path))))
              (let ((configuration (niwa-agent-record-configuration record)))
                (unless (and (niwa-agent-configuration-p configuration)
                             (niwa-agent-request-locals-p
                              (niwa-agent-configuration-request-locals configuration)))
                  (signal 'niwa-agent-persistence-invalid-data
                          (list (format "Invalid retained agent configuration: %s" path))))
                ;; Recovery needs partial responses from abandoned active turns.
                ;; Settled identities need only their durable registry until access.
                (unless (eq (niwa-agent-record-activity record) 'idle)
                  (setq buffer
                        (niwa-agent-persistence-ensure-conversation
                         session record root-buffer readonly-p)))
                (when (and (not readonly-p)
                           (not (eq (niwa-agent-record-activity record)
                                    'idle)))
                  (when-let* ((entry
                               (assoc
                                (niwa-agent-record-id record)
                                (niwa-session-agent-transcripts session))))
                    (let ((metadata (copy-sequence (cdr entry))))
                      (setf (plist-get metadata :status) 'aborted
                            (plist-get metadata :reason)
                            "Interrupted during session recovery")
                      (setcdr entry metadata))))
                (setf (niwa-agent-record-conversation-buffer record) buffer
                      (niwa-agent-record-invocation record) nil)
                (when readonly-p
                  (setf (niwa-agent-record-activity record) 'idle
                        (niwa-agent-record-blockers record) nil))
                (push entry accepted)))
          ((niwa-agent-persistence-invalid-data file-error)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (kill-buffer buffer))
           (cl-incf dropped)
           (niwa-diagnostic-warning
            (niwa-diagnostic-scope session) 'niwa
            (format "Retained agent %s was not restored: %s"
                    path (error-message-string err))
            :warning)))))
    (setq accepted (nreverse accepted))
    (setf (niwa-session-agent-registry session) accepted)
    (+ dropped
       (if readonly-p
           0
         (niwa-agent-control-recover-interrupted session)))))

(provide 'niwa-agent-persistence)

;;; niwa-agent-persistence.el ends here
