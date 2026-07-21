;;; mevedel-tool-repair-diagnostics.el --- Repair audit and telemetry -*- lexical-binding: t -*-

;;; Commentary:

;; Owns value-free repair audit records, redacted telemetry, persistence,
;; and dispatch-result lifecycle tracking.

;;; Code:

(eval-when-compile (require 'mevedel-structs))

;; `gptel'
(declare-function gptel-backend-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-backend-p "ext:gptel-request" (cl-x))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(defvar mevedel--agent-invocation)

;; `mevedel-chat'
(defvar mevedel--session)

;; `mevedel-tool-repair'
(defvar mevedel-tool-repair--in-flight)
(defvar mevedel-tool-repair--max-audit-records)
(defvar mevedel-tool-repair--max-telemetry-items)
(defvar mevedel-tool-repair--max-telemetry-path-depth)
(defvar mevedel-tool-repair--shape-identifiers)
(defvar mevedel-tool-repair-log-limit)
(defvar mevedel-tool-repair-persist-log)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-transcript-audit" (record))

(defun mevedel-tool-repair--audit-identifier-p (value)
  "Return non-nil when VALUE is a bounded value-free identifier."
  (and (symbolp value)
       (<= (length (symbol-name value)) 48)
       (string-match-p "\\`[-[:alnum:]_]+\\'" (symbol-name value))))

(defun mevedel-tool-repair--normalize-audit-path (path)
  "Return a safe copy of schema PATH, or nil when malformed."
  (when (and (consp path)
             (when-let* ((length (proper-list-p path)))
               (<= length mevedel-tool-repair--max-telemetry-path-depth))
             (seq-every-p
              (lambda (part)
                (or (and (integerp part) (>= part 0))
                    (mevedel-tool-repair--audit-identifier-p part)))
              path))
    (copy-sequence path)))

(defun mevedel-tool-repair--normalize-audit-repair (record)
  "Return value-free repair RECORD fields, or nil when malformed."
  (when (and (proper-list-p record) (keywordp (car-safe record)))
    (let ((rule (plist-get record :rule))
          (source (plist-get record :source))
          (paths (plist-get record :paths))
          (before (plist-get record :before))
          (after (plist-get record :after)))
      (when (and (mevedel-tool-repair--audit-identifier-p rule)
                 (memq source '(generic tool))
                 (consp paths)
                 (when-let* ((length (proper-list-p paths)))
                   (<= length mevedel-tool-repair--max-telemetry-items))
                 (seq-every-p #'mevedel-tool-repair--normalize-audit-path paths)
                 (memq before mevedel-tool-repair--shape-identifiers)
                 (memq after mevedel-tool-repair--shape-identifiers))
        (list :rule rule :source source
              :paths (mapcar #'copy-sequence paths)
              :before before :after after)))))

(defun mevedel-tool-repair-normalize-audit-record (record)
  "Return a bounded value-free tool repair audit RECORD, or nil."
  (when (proper-list-p record)
    (let ((state (plist-get record :state))
          (repairs (plist-get record :repairs)))
      (when (and (eq (plist-get record :type) 'tool-input-repair)
                 (memq state '(committed abandoned))
                 (consp repairs)
                 (when-let* ((length (proper-list-p repairs)))
                   (<= length mevedel-tool-repair--max-audit-records)))
        (when-let* ((normalized
                     (mapcar #'mevedel-tool-repair--normalize-audit-repair
                             repairs))
                    ((not (memq nil normalized))))
          (list :type 'tool-input-repair :state state
                :repairs normalized))))))

(defun mevedel-tool-repair-audit-record (state repairs)
  "Return a normalized audit record for STATE and REPAIRS, or nil."
  (mevedel-tool-repair-normalize-audit-record
   (list :type 'tool-input-repair :state state :repairs repairs)))

(defun mevedel-tool-repair-format-audit-block (state repairs)
  "Return a hidden value-free audit block for STATE and REPAIRS."
  (condition-case nil
      (if-let* ((record (mevedel-tool-repair-audit-record state repairs)))
          (progn
            (require 'mevedel-transcript-audit)
            (mevedel--format-hook-audit-record record))
        "")
    (error
     (ignore-errors
       (display-warning 'mevedel "Tool input repair audit formatting failed"
                        :warning))
     "")))

(defun mevedel-tool-repair--current-session ()
  "Return the top-level session associated with the current buffer."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--agent-invocation)
           (fboundp 'mevedel-agent-invocation-p)
           (mevedel-agent-invocation-p mevedel--agent-invocation)
           (mevedel-agent-invocation-parent-session
            mevedel--agent-invocation))))

(defun mevedel-tool-repair--safe-dimension (value)
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

(defun mevedel-tool-repair--safe-backend (backend)
  "Return the stable provider name represented by BACKEND."
  (mevedel-tool-repair--safe-dimension
   (if (and (fboundp 'gptel-backend-p) (gptel-backend-p backend))
       (gptel-backend-name backend)
     backend)))

(defun mevedel-tool-repair--safe-category (value allowed)
  "Return VALUE when it is in ALLOWED, otherwise `unknown'."
  (if (memq value allowed) value 'unknown))

(defun mevedel-tool-repair--safe-rule (rule)
  "Return a bounded telemetry representation of RULE."
  (and (mevedel-tool-repair--audit-identifier-p rule) rule))

(defun mevedel-tool-repair--bounded-values (values sanitizer)
  "Return sanitized VALUES and their omitted count using SANITIZER."
  (let ((tail values) kept (count 0))
    (while (and tail (< count mevedel-tool-repair--max-telemetry-items))
      (when-let* ((value (funcall sanitizer (pop tail))))
        (push value kept))
      (setq count (1+ count)))
    (cons (nreverse kept) (length tail))))

(defun mevedel-tool-repair--safe-telemetry-path (path)
  "Return bounded schema PATH plus its omitted component count."
  (let ((tail path) kept (count 0))
    (while (and tail (< count mevedel-tool-repair--max-telemetry-path-depth))
      (let ((part (pop tail)))
        (push
         (cond
          ((and (integerp part) (>= part 0)) part)
          ((and (symbolp part)
                (<= (length (symbol-name part)) 48)
                (string-match-p "\\`[-[:alnum:]_]+\\'" (symbol-name part)))
           part)
          (t 'redacted))
         kept))
      (setq count (1+ count)))
    (cons (nreverse kept) (length tail))))

(defun mevedel-tool-repair--safe-event (event)
  "Return the whitelisted, bounded representation of telemetry EVENT."
  (let* ((rules (mevedel-tool-repair--bounded-values
                 (plist-get event :rules) #'mevedel-tool-repair--safe-rule))
         (raw-paths (mevedel-tool-repair--bounded-values
                     (plist-get event :paths) #'identity))
         (safe-paths (mapcar #'mevedel-tool-repair--safe-telemetry-path
                             (car raw-paths)))
         (issues (mevedel-tool-repair--bounded-values
                  (plist-get event :issue-kinds)
                  #'mevedel-tool-repair--safe-rule)))
    (list
     :time (mevedel-tool-repair--safe-dimension (plist-get event :time))
     :origin (mevedel-tool-repair--safe-dimension (plist-get event :origin))
     :backend (mevedel-tool-repair--safe-backend (plist-get event :backend))
     :model (mevedel-tool-repair--safe-dimension (plist-get event :model))
     :tool (mevedel-tool-repair--safe-dimension (plist-get event :tool))
     :outcome (mevedel-tool-repair--safe-category
               (plist-get event :outcome)
               '(valid repaired invalid abandoned))
     :repair-enabled (and (plist-get event :repair-enabled) t)
     :rules (car rules) :rules-omitted (cdr rules)
     :paths (mapcar #'car safe-paths) :paths-omitted (cdr raw-paths)
     :path-components-omitted (apply #'+ (mapcar #'cdr safe-paths))
     :issue-kinds (car issues) :issue-kinds-omitted (cdr issues)
     :execution (mevedel-tool-repair--safe-category
                 (plist-get event :execution) '(executed not-executed))
     :result (mevedel-tool-repair--safe-category
              (plist-get event :result) '(success error none))
     :failure-class
     (and (eq (plist-get event :failure-class) 'internal-repair-error)
          'internal-repair-error))))

(defun mevedel-tool-repair-log-path (session)
  "Return SESSION's persistent repair telemetry path, or nil."
  (when-let* ((save-path (and session (mevedel-session-save-path session))))
    (file-name-concat save-path "repair-log.el")))

(defun mevedel-tool-repair--persist-event (session event)
  "Append redacted telemetry EVENT to SESSION's local log."
  (when-let* ((file (and mevedel-tool-repair-persist-log
                         (mevedel-tool-repair-log-path session))))
    (condition-case nil
        (let ((print-length nil) (print-level nil) (print-quoted t))
          (make-directory (file-name-directory file) t)
          (with-temp-buffer
            (prin1 event (current-buffer))
            (insert "\n")
            (append-to-file (point-min) (point-max) file)))
      (error
       (display-warning 'mevedel "Repair telemetry persistence failed"
                        :warning)))))

(defun mevedel-tool-repair-log-event (session event)
  "Record redacted telemetry EVENT for SESSION without blocking execution."
  (condition-case nil
      (when session
        (let* ((event (mevedel-tool-repair--safe-event event))
               (log (append (mevedel-session-repair-log session) (list event))))
          (when (> (length log) mevedel-tool-repair-log-limit)
            (setq log (last log mevedel-tool-repair-log-limit)))
          (setf (mevedel-session-repair-log session) log)
          (mevedel-tool-repair--persist-event session event)
          (when (fboundp 'mevedel-telemetry-record)
            (apply #'mevedel-telemetry-record
                   session 'tool-input-repair
                   :stage 'settled
                   :repair-count (length (plist-get event :rules))
                   :issue-count (length (plist-get event :issue-kinds))
                   event)))
        t)
    (error
     (ignore-errors
       (display-warning 'mevedel "Repair telemetry logging failed" :warning))
     t)))

(defun mevedel-tool-repair--entry-paths (entry)
  "Return schema paths represented by telemetry ENTRY."
  (delete-dups
   (append
    (mapcan (lambda (record) (copy-tree (plist-get record :paths) t))
            (plist-get entry :repairs))
    (mapcar (lambda (issue) (copy-tree (plist-get issue :path) t))
            (plist-get entry :issues)))))

(defun mevedel-tool-repair--event (entry outcome result)
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
        :paths (mevedel-tool-repair--entry-paths entry)
        :issue-kinds (mapcar (lambda (issue) (plist-get issue :kind))
                             (plist-get entry :issues))
        :execution (plist-get entry :execution)
        :result result
        :failure-class (plist-get entry :failure-class)))

(defun mevedel-tool-repair--release-entry (entry)
  "Stop tracking completed pipeline ENTRY in its dispatch buffer."
  (when-let* ((buffer (plist-get entry :buffer)) ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq mevedel-tool-repair--in-flight
            (delq entry mevedel-tool-repair--in-flight)))))

(defun mevedel-tool-repair-record-result
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
              (mevedel-tool-repair-log-event
               (plist-get entry :session)
               (mevedel-tool-repair--event entry outcome classification)))
          (mevedel-tool-repair--release-entry entry)))
    (error
     (ignore-errors (mevedel-tool-repair--release-entry entry))
     (ignore-errors
       (display-warning 'mevedel "Repair telemetry logging failed" :warning))
     nil)))

(defun mevedel-tool-repair-mark-executed (entry)
  "Mark telemetry ENTRY as having entered its tool handler."
  (when entry (plist-put entry :execution 'executed)))

(provide 'mevedel-tool-repair-diagnostics)

;;; mevedel-tool-repair-diagnostics.el ends here
