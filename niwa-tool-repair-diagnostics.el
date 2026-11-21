;;; niwa-tool-repair-diagnostics.el --- Repair audit and telemetry -*- lexical-binding: t -*-

;;; Commentary:

;; Owns value-free repair audit records, redacted telemetry, persistence,
;; and dispatch-result lifecycle tracking.

;;; Code:

(require 'niwa-structs)
(require 'niwa-telemetry)
(require 'niwa-utilities)

;; `gptel'
(declare-function gptel-backend-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-backend-p "ext:gptel-request" (cl-x))

;; `niwa-agents'
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-parent-session
                  "niwa-agents" (cl-x) t)
(defvar niwa--agent-invocation)

;; `niwa-chat'
(defvar niwa--session)

;; `niwa-execution-target'
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))

;; `niwa-session-publication'
(declare-function niwa-session-publication-append-diagnostic
                  "niwa-session-publication" (session path content))

;; `niwa-tool-repair'
(defvar niwa-tool-repair--in-flight)
(defvar niwa-tool-repair--max-audit-records)
(defvar niwa-tool-repair--max-telemetry-items)
(defvar niwa-tool-repair--max-telemetry-path-depth)
(defvar niwa-tool-repair--shape-identifiers)
(defvar niwa-tool-repair-log-limit)
(defvar niwa-tool-repair-persist-log)

;; `niwa-telemetry'
(declare-function niwa-telemetry-record-audit
                  "niwa-telemetry" (session event &rest props))

;; `niwa-transcript-audit'
(declare-function niwa--format-hook-audit-record
                  "niwa-transcript-audit" (record))

;; `niwa-utilities'
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))

(defun niwa-tool-repair--audit-identifier-p (value)
  "Return non-nil when VALUE is a bounded value-free identifier."
  (and (symbolp value)
       (<= (length (symbol-name value)) 48)
       (string-match-p "\\`[-[:alnum:]_]+\\'" (symbol-name value))))

(defun niwa-tool-repair--normalize-audit-path (path)
  "Return a safe copy of schema PATH, or nil when malformed."
  (when (and (consp path)
             (when-let* ((length (proper-list-p path)))
               (<= length niwa-tool-repair--max-telemetry-path-depth))
             (seq-every-p
              (lambda (part)
                (or (and (integerp part) (>= part 0))
                    (niwa-tool-repair--audit-identifier-p part)))
              path))
    (copy-sequence path)))

(defun niwa-tool-repair--normalize-audit-repair (record)
  "Return value-free repair RECORD fields, or nil when malformed."
  (when (and (proper-list-p record) (keywordp (car-safe record)))
    (let ((rule (plist-get record :rule))
          (source (plist-get record :source))
          (paths (plist-get record :paths))
          (before (plist-get record :before))
          (after (plist-get record :after)))
      (when (and (niwa-tool-repair--audit-identifier-p rule)
                 (memq source '(generic tool))
                 (consp paths)
                 (when-let* ((length (proper-list-p paths)))
                   (<= length niwa-tool-repair--max-telemetry-items))
                 (seq-every-p #'niwa-tool-repair--normalize-audit-path paths)
                 (memq before niwa-tool-repair--shape-identifiers)
                 (memq after niwa-tool-repair--shape-identifiers))
        (list :rule rule :source source
              :paths (mapcar #'copy-sequence paths)
              :before before :after after)))))

(defun niwa-tool-repair-normalize-audit-record (record)
  "Return a bounded value-free tool repair audit RECORD, or nil."
  (when (proper-list-p record)
    (let ((state (plist-get record :state))
          (repairs (plist-get record :repairs)))
      (when (and (eq (plist-get record :type) 'tool-input-repair)
                 (memq state '(committed abandoned))
                 (consp repairs)
                 (when-let* ((length (proper-list-p repairs)))
                   (<= length niwa-tool-repair--max-audit-records)))
        (when-let* ((normalized
                     (mapcar #'niwa-tool-repair--normalize-audit-repair
                             repairs))
                    ((not (memq nil normalized))))
          (list :type 'tool-input-repair :state state
                :repairs normalized))))))

(defun niwa-tool-repair-audit-record (state repairs)
  "Return a normalized audit record for STATE and REPAIRS, or nil."
  (niwa-tool-repair-normalize-audit-record
   (list :type 'tool-input-repair :state state :repairs repairs)))

(defun niwa-tool-repair-format-audit-block (state repairs &optional scope)
  "Return a hidden value-free audit block for STATE and REPAIRS.
Report formatting failures in the originating SCOPE."
  (condition-case nil
      (if-let* ((record (niwa-tool-repair-audit-record state repairs)))
          (progn
            (unless (fboundp 'niwa--format-hook-audit-record)
              (require 'niwa-transcript-audit))
            (niwa--format-hook-audit-record record))
        "")
    (error
     (ignore-errors
       (niwa--warn-once (niwa-diagnostic-scope scope) 'repair-audit-formatting
                           "Tool input repair audit formatting failed"))
     "")))

(defun niwa-tool-repair--current-session ()
  "Return the top-level session associated with the current buffer."
  (or (and (boundp 'niwa--session) niwa--session)
      (and (fboundp 'niwa-agent-invocation-p)
           (niwa-agent-invocation-p niwa--agent-invocation)
           (niwa-agent-invocation-parent-session
            niwa--agent-invocation))))

(defun niwa-tool-repair--safe-dimension (value)
  "Return a bounded telemetry dimension for VALUE."
  (cond
   ((null value) 'unknown)
   ((symbolp value)
    (let ((name (symbol-name value)))
      (if (and (<= (length name) 128)
               (not (string-match-p "[\n\r]" name)))
          value
        'redacted)))
   ((stringp value)
    (if (and (<= (length value) 128)
             (not (string-match-p "[\n\r]" value)))
        value
      "<redacted>"))
   (t (type-of value))))

(defun niwa-tool-repair--safe-backend (backend)
  "Return the stable provider name represented by BACKEND."
  (niwa-tool-repair--safe-dimension
   (if (and (fboundp 'gptel-backend-p) (gptel-backend-p backend))
       (gptel-backend-name backend)
     backend)))

(defun niwa-tool-repair--safe-category (value allowed)
  "Return VALUE when it is in ALLOWED, otherwise `unknown'."
  (if (memq value allowed) value 'unknown))

(defun niwa-tool-repair--safe-rule (rule)
  "Return a bounded telemetry representation of RULE."
  (and (niwa-tool-repair--audit-identifier-p rule) rule))

(defun niwa-tool-repair--bounded-values (values sanitizer)
  "Return sanitized VALUES and their omitted count using SANITIZER."
  (let ((tail values) kept (count 0))
    (while (and tail (< count niwa-tool-repair--max-telemetry-items))
      (when-let* ((value (funcall sanitizer (pop tail))))
        (push value kept))
      (setq count (1+ count)))
    (cons (nreverse kept) (length tail))))

(defun niwa-tool-repair--safe-telemetry-path (path)
  "Return bounded schema PATH plus its omitted component count."
  (let ((tail path) kept (count 0))
    (while (and tail (< count niwa-tool-repair--max-telemetry-path-depth))
      (let ((part (pop tail)))
        (push
         (cond
          ((and (integerp part) (>= part 0)) part)
          ((niwa-tool-repair--audit-identifier-p part) part)
          (t 'redacted))
         kept))
      (setq count (1+ count)))
    (cons (nreverse kept) (length tail))))

(defun niwa-tool-repair--safe-event (event)
  "Return the whitelisted, bounded representation of telemetry EVENT."
  (let* ((rules (niwa-tool-repair--bounded-values
                 (plist-get event :rules) #'niwa-tool-repair--safe-rule))
         (raw-paths (niwa-tool-repair--bounded-values
                     (plist-get event :paths) #'identity))
         (safe-paths (mapcar #'niwa-tool-repair--safe-telemetry-path
                             (car raw-paths)))
         (issues (niwa-tool-repair--bounded-values
                  (plist-get event :issue-kinds)
                  #'niwa-tool-repair--safe-rule)))
    (list
     :time (niwa-tool-repair--safe-dimension (plist-get event :time))
     :origin (niwa-tool-repair--safe-dimension (plist-get event :origin))
     :backend (niwa-tool-repair--safe-backend (plist-get event :backend))
     :model (niwa-tool-repair--safe-dimension (plist-get event :model))
     :tool (niwa-tool-repair--safe-dimension (plist-get event :tool))
     :outcome (niwa-tool-repair--safe-category
               (plist-get event :outcome)
               '(valid repaired invalid abandoned))
     :repair-enabled (and (plist-get event :repair-enabled) t)
     :rules (car rules) :rules-omitted (cdr rules)
     :paths (mapcar #'car safe-paths) :paths-omitted (cdr raw-paths)
     :path-components-omitted (apply #'+ (mapcar #'cdr safe-paths))
     :issue-kinds (car issues) :issue-kinds-omitted (cdr issues)
     :execution (niwa-tool-repair--safe-category
                 (plist-get event :execution) '(executed not-executed))
     :result (niwa-tool-repair--safe-category
              (plist-get event :result) '(success error none))
     :failure-class
     (and (eq (plist-get event :failure-class) 'internal-repair-error)
          'internal-repair-error))))

(defun niwa-tool-repair-log-path (session)
  "Return SESSION's persistent repair telemetry path, or nil."
  (when-let* ((save-path (and session (niwa-session-save-path session))))
    (file-name-concat save-path "repair-log.el")))

(defun niwa-tool-repair--event-text (event)
  "Return redacted telemetry EVENT in its durable line format."
  (let ((print-length nil)
        (print-level nil)
        (print-quoted t))
    (concat (prin1-to-string event) "\n")))

(defun niwa-tool-repair--persist-content (session content)
  "Append serialized repair telemetry CONTENT for SESSION."
  (when-let* ((file (and niwa-tool-repair-persist-log
                         (niwa-tool-repair-log-path session))))
    (condition-case nil
        (let ((target (niwa-session-execution-target session)))
          (if (and target
                   (niwa-execution-target-remote-p target))
              (progn
                (unless (fboundp
                         'niwa-session-publication-append-diagnostic)
                  (require 'niwa-session-publication))
                (niwa-session-publication-append-diagnostic
                 session file content))
            (let ((coding-system-for-write 'utf-8-unix))
              (condition-case nil
                  (write-region content nil file t 'silent)
                (file-missing
                 (make-directory (file-name-directory file) t)
                 (write-region content nil file t 'silent))))
            t))
      (error
       (niwa--warn-once (niwa-diagnostic-scope session) 'repair-telemetry-persistence
                           "Repair telemetry persistence failed")
       nil))))

(defun niwa-tool-repair--persist-event (session event)
  "Append redacted telemetry EVENT to SESSION's persistent log."
  (niwa-tool-repair--persist-content
   session (niwa-tool-repair--event-text event)))

(defun niwa-tool-repair-flush-log (session)
  "Persist SESSION's queued repair diagnostics, retaining failures."
  (when session
    (let* ((pending (niwa-session-repair-log-pending session))
           (target (niwa-session-execution-target session))
           (remote-p
            (and target
                 (niwa-execution-target-remote-p target))))
      (if (and pending remote-p)
          (when (niwa-tool-repair--persist-content
                 session (mapconcat #'niwa-tool-repair--event-text
                                    pending ""))
            (setf (niwa-session-repair-log-pending session) nil))
        (while (and pending
                    (niwa-tool-repair--persist-event session (car pending)))
          (setq pending (cdr pending)))
        (setf (niwa-session-repair-log-pending session) pending)))))

(defun niwa-tool-repair-log-event (session event)
  "Record nontrivial redacted EVENT for SESSION without blocking execution.
Unchanged valid inputs do not enter memory, persistence, or telemetry."
  (condition-case nil
      (when (and session
                 (or (not (eq (plist-get event :outcome) 'valid))
                     (plist-get event :rules)
                     (plist-get event :issue-kinds)))
        (let* ((event (niwa-tool-repair--safe-event event))
               (log (append (niwa-session-repair-log session)
                            (list event))))
          (when (> (length log) niwa-tool-repair-log-limit)
            (setq log (last log niwa-tool-repair-log-limit)))
          (setf (niwa-session-repair-log session) log)
          (when niwa-tool-repair-persist-log
            (let* ((pending
                    (niwa-session-repair-log-pending session))
                   (target (niwa-session-execution-target session))
                   (remote-p
                    (and target
                         (niwa-execution-target-remote-p target))))
              (when (or pending remote-p
                        (not
                         (niwa-tool-repair--persist-event session event)))
                (setf (niwa-session-repair-log-pending session)
                      (append pending (list event))))))
          (apply #'niwa-telemetry-record-audit
                 session 'tool-input-repair
                 (append
                  (list :stage 'settled
                        :repair-count (length (plist-get event :rules))
                        :issue-count
                        (length (plist-get event :issue-kinds)))
                  event)))
        t)
    (error
     (ignore-errors
       (niwa--warn-once (niwa-diagnostic-scope session) 'repair-telemetry-logging
                           "Repair telemetry logging failed"))
     t)))

(defun niwa-tool-repair--entry-paths (entry)
  "Return schema paths represented by telemetry ENTRY."
  (delete-dups
   (append
    (mapcan (lambda (record) (copy-tree (plist-get record :paths) t))
            (plist-get entry :repairs))
    (mapcar (lambda (issue) (copy-tree (plist-get issue :path) t))
            (plist-get entry :issues)))))

(defun niwa-tool-repair--event (entry outcome result)
  "Build a value-free telemetry event from ENTRY, OUTCOME, and RESULT."
  (list :time (format-time-string "%FT%T%z")
        :origin (plist-get entry :origin)
        :backend (plist-get entry :backend)
        :model (plist-get entry :model)
        :tool (plist-get entry :tool)
        :outcome outcome
        :repair-enabled (plist-get entry :repair-enabled)
        :rules (mapcar (lambda (record) (plist-get record :rule))
                       (plist-get entry :repairs))
        :paths (niwa-tool-repair--entry-paths entry)
        :issue-kinds (mapcar (lambda (issue) (plist-get issue :kind))
                             (plist-get entry :issues))
        :execution (plist-get entry :execution)
        :result result
        :failure-class (plist-get entry :failure-class)))

(defun niwa-tool-repair--release-entry (entry)
  "Stop tracking completed pipeline ENTRY in its dispatch buffer."
  (when-let* ((buffer (plist-get entry :buffer)) ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq niwa-tool-repair--in-flight
            (delq entry niwa-tool-repair--in-flight)))))

(defun niwa-tool-repair-record-result
    (entry result &optional outcome result-classification)
  "Record ENTRY telemetry after RESULT without exposing result content."
  (condition-case nil
      (when (and entry (not (plist-get entry :telemetry-recorded)))
        (plist-put entry :telemetry-recorded t)
        (unwind-protect
            (let ((outcome
                   (or outcome
                       (pcase (plist-get entry :status)
                         ((or 'valid 'repaired) (plist-get entry :status))
                         ('invalid (if (plist-get entry :abandoned-repairs)
                                       'abandoned
                                     'invalid))
                         (_ 'abandoned))))
                  (classification
                   (or result-classification
                       (if (and (stringp result)
                                (or (string-prefix-p "Error:" result)
                                    (string-prefix-p "<tool_call_error>"
                                                     result)))
                           'error
                         'success))))
              (when (or (not (eq outcome 'valid))
                        (plist-get entry :repairs)
                        (plist-get entry :issues))
                (niwa-tool-repair-log-event
                 (plist-get entry :session)
                 (niwa-tool-repair--event entry outcome classification))))
          (niwa-tool-repair--release-entry entry)))
    (error
     (ignore-errors (niwa-tool-repair--release-entry entry))
     (ignore-errors
       (niwa--warn-once (niwa-diagnostic-scope (plist-get entry :session))
                           'repair-telemetry-logging
                           "Repair telemetry logging failed"))
     nil)))

(defun niwa-tool-repair-mark-executed (entry)
  "Mark telemetry ENTRY as having entered its tool handler."
  (when entry (plist-put entry :execution 'executed)))

(provide 'niwa-tool-repair-diagnostics)

;;; niwa-tool-repair-diagnostics.el ends here
