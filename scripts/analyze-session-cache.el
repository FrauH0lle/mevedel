;;; analyze-session-cache.el --- Analyze saved provider-call usage -*- lexical-binding: t -*-

;;; Commentary:

;; An offline reader for session telemetry, with no mevedel or provider runtime
;; dependencies.  Only provider-call events supply token evidence: cumulative
;; request settlements are deliberately ignored.  Unknown counts stay unknown.
;;
;; Run with explicit files, session directories, or a sessions directory:
;; emacs -Q --batch -l scripts/analyze-session-cache.el \
;;   -f mevedel-analyze-session-cache-main -- --json .mevedel/sessions

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defconst mevedel-analyze-session-cache--token-fields
  '(:input-tokens :cached-tokens :cache-write-tokens :output-tokens)
  "Per-call token fields, with cache writes a subset of input.")

(defconst mevedel-analyze-session-cache--identity-fields
  '(:session-id :request-id :turn :agent-path :backend :backend-type
    :model :workload :purpose)
  "Ownership and routing fields frozen at dispatch.")

(defconst mevedel-analyze-session-cache--event-fields
  (append '(:call-id :stage :time :outcome :duration-ms :usage-status)
          mevedel-analyze-session-cache--identity-fields
          mevedel-analyze-session-cache--token-fields)
  "Metadata retained for analysis; unknown fields are never rendered.")

(defconst mevedel-analyze-session-cache--max-record-length (* 1024 1024)
  "Maximum characters the Lisp reader may parse in one telemetry record.")

(defun mevedel-analyze-session-cache--plist-p (value)
  "Return non-nil when VALUE is a proper plist with unique keyword keys."
  (and (proper-list-p value)
       (cl-evenp (length value))
       (let ((rest value) seen valid)
         (setq valid t)
         (while (and rest valid)
           (let ((key (pop rest)))
             (pop rest)
             (if (or (not (keywordp key)) (memq key seen))
                 (setq valid nil)
               (push key seen))))
         valid)))

(defun mevedel-analyze-session-cache--count-p (value)
  "Return non-nil when VALUE is a nonnegative integer token count."
  (and (integerp value) (>= value 0)))

(defun mevedel-analyze-session-cache--name (value)
  "Return a bounded scalar identifier for VALUE, or nil."
  (let ((name (cond ((stringp value) value)
                    ((and value (symbolp value)) (symbol-name value)))))
    (and name (substring-no-properties name 0 (min 512 (length name))))))

(defun mevedel-analyze-session-cache--key (field)
  "Return the JSON key for keyword FIELD."
  (intern (replace-regexp-in-string "-" "_" (substring (symbol-name field) 1))))

(defun mevedel-analyze-session-cache--issue (kind file &optional line fields)
  "Return a bounded diagnostic of KIND at FILE and LINE, naming FIELDS only."
  `((kind . ,kind) (file . ,file) (line . ,line)
    (fields . ,(vconcat (mapcar (lambda (field)
                                 (symbol-name (mevedel-analyze-session-cache--key field)))
                               fields)))))

(defun mevedel-analyze-session-cache--files (paths)
  "Discover unique telemetry files in PATHS and return (FILES . ISSUES).
Explicit files need not have the default telemetry filename.  Directories
are searched recursively without following directory symlinks."
  (let (files issues)
    (dolist (path paths)
      (condition-case nil
          (cond
           ((file-directory-p path)
            (setq files
                  (append (directory-files-recursively
                           path "\\(?:\\`\\|/\\)telemetry-log\\.el\\'")
                          files)))
           ((file-regular-p path) (push path files))
           (t (push (mevedel-analyze-session-cache--issue
                     "missing-path" (expand-file-name path)) issues)))
        (file-error
         (push (mevedel-analyze-session-cache--issue
                "unreadable-path" (expand-file-name path)) issues))))
    (cons (sort (delete-dups (mapcar #'file-truename files)) #'string<)
          (nreverse issues))))

(defun mevedel-analyze-session-cache--read-file (file)
  "Read FILE without evaluation, returning metadata records and diagnostics.
Each physical line is one Lisp value.  Circular reader syntax is disabled.
Malformed lines do not discard later complete records, and reader error
messages are never echoed because they may contain log payloads."
  (let (records issues (other-events 0) (line 0))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (not (eobp))
            (cl-incf line)
            (let* ((begin (point)) (end (line-end-position))
                   (text (buffer-substring-no-properties begin end))
                   (last-line (>= (1+ end) (point-max)))
                   (read-circle nil))
              (when (> (length text) mevedel-analyze-session-cache--max-record-length)
                (push (mevedel-analyze-session-cache--issue "oversized-line" file line)
                      issues))
              (unless (or (string-blank-p text)
                          (> (length text) mevedel-analyze-session-cache--max-record-length))
                (condition-case err
                    (let* ((parsed (read-from-string text))
                           (record (car parsed)))
                      (unless (string-blank-p (substring text (cdr parsed)))
                        (error "Extra data"))
                      (if (not (and (mevedel-analyze-session-cache--plist-p record)
                                    (eq (plist-get record :schema-version) 1)
                                    (symbolp (plist-get record :event))
                                    (plist-get record :event)))
                          (push (mevedel-analyze-session-cache--issue
                                 "invalid-record" file line) issues)
                        (if (not (eq (plist-get record :event) 'provider-call))
                            (cl-incf other-events)
                          (if (not (and (memq (plist-get record :stage)
                                              '(start finish))
                                        (stringp (plist-get record :call-id))
                                        (not (string-empty-p
                                              (plist-get record :call-id)))
                                        (<= (length (plist-get record :call-id)) 512)
                                        (stringp (plist-get record :session-id))
                                        (not (string-empty-p
                                              (plist-get record :session-id)))
                                        (<= (length (plist-get record :session-id)) 512)))
                              (push (mevedel-analyze-session-cache--issue
                                     "invalid-call" file line) issues)
                            (let (metadata)
                              (dolist (field mevedel-analyze-session-cache--event-fields)
                                (setq metadata
                                      (plist-put metadata field (plist-get record field))))
                              (push (list :metadata metadata :file file :line line)
                                    records))))))
                  (error
                   (push (mevedel-analyze-session-cache--issue
                          (if (and last-line (eq (car err) 'end-of-file))
                              "interrupted-trailing-record" "malformed-line")
                          file line) issues))))
              (forward-line 1))))
      (file-error
       (push (mevedel-analyze-session-cache--issue "unreadable-file" file) issues)))
    (list :records (nreverse records) :issues (nreverse issues)
          :other-events other-events)))

(defun mevedel-analyze-session-cache--different-fields (first second fields)
  "Return FIELDS with differing values in plists FIRST and SECOND."
  (cl-remove-if (lambda (field)
                  (equal (plist-get first field) (plist-get second field)))
                fields))

(defun mevedel-analyze-session-cache--reference (record)
  "Return the source reference of RECORD."
  (when record
    `((file . ,(plist-get record :file)) (line . ,(plist-get record :line)))))

(defun mevedel-analyze-session-cache--timestamp (value)
  "Return VALUE only when it is an ISO telemetry timestamp."
  (and (stringp value)
       (string-match-p
        "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\(?:\\.[0-9]+\\)?\\(?:Z\\|[+-][0-9]\\{2\\}:?[0-9]\\{2\\}\\)\\'"
        value)
       (ignore-errors (date-to-time value) value)))

(defun mevedel-analyze-session-cache--call (id state)
  "Build the public per-call record for ID from deduplicated STATE."
  (let* ((start (plist-get state :start)) (finish (plist-get state :finish))
         (metadata (plist-get (or finish start) :metadata))
         (conflict (plist-get state :conflict))
         (status (plist-get metadata :usage-status))
         (outcome (plist-get metadata :outcome))
         (duration (plist-get metadata :duration-ms))
         (known 0) invalid tokens identity)
    (dolist (field mevedel-analyze-session-cache--identity-fields)
      (push (cons (mevedel-analyze-session-cache--key field)
                  (if (eq field :turn)
                      (and (mevedel-analyze-session-cache--count-p
                            (plist-get metadata field))
                           (plist-get metadata field))
                    (mevedel-analyze-session-cache--name
                     (plist-get metadata field)))) identity))
    (dolist (field mevedel-analyze-session-cache--token-fields)
      (let ((value (and finish (not conflict) (plist-get metadata field))))
        (when (and value (not (mevedel-analyze-session-cache--count-p value)))
          (push field invalid)
          (setq value nil))
        (when (and (eq field :cache-write-tokens) value
                   (mevedel-analyze-session-cache--count-p
                    (plist-get metadata :input-tokens))
                   (> value (plist-get metadata :input-tokens)))
          (push field invalid)
          (setq value nil))
        (when (and value (not (eq field :cache-write-tokens))) (cl-incf known))
        (push (cons (mevedel-analyze-session-cache--key field) value) tokens)))
    (setq status
          (cond ((not finish) "missing")
                ((or conflict invalid (eq status 'invalid)) "invalid")
                ((eq status 'partial) "partial")
                ((= known 3) "complete")
                ((> known 0) "partial")
                (t "missing")))
    (append
     `((call_id . ,(mevedel-analyze-session-cache--name id)))
     (nreverse identity)
     `((started_at . ,(mevedel-analyze-session-cache--timestamp
                      (plist-get (plist-get start :metadata) :time)))
       (finished_at . ,(mevedel-analyze-session-cache--timestamp
                       (plist-get (plist-get finish :metadata) :time)))
       (start . ,(mevedel-analyze-session-cache--reference start))
       (finish . ,(mevedel-analyze-session-cache--reference finish))
       (incomplete . ,(if finish :false t))
       (conflicting . ,(if conflict t :false))
       (outcome . ,(cond ((not finish) "incomplete")
                        ((memq outcome '(success error aborted)) (symbol-name outcome))
                        (t "unknown")))
       (duration_ms . ,(and finish (numberp duration) (>= duration 0)
                           (< duration 1.0e+INF) duration))
       (usage_status . ,status)
       (invalid_fields . ,(vconcat (mapcar
                                   (lambda (field)
                                     (symbol-name (mevedel-analyze-session-cache--key field)))
                                   (nreverse invalid)))))
     (nreverse tokens))))

(defun mevedel-analyze-session-cache--chronological-p (first second)
  "Return non-nil when call FIRST precedes call SECOND."
  (let* ((a (or (alist-get 'started_at first) (alist-get 'finished_at first)))
         (b (or (alist-get 'started_at second) (alist-get 'finished_at second)))
         (a-time (and a (date-to-time a))) (b-time (and b (date-to-time b))))
    (cond ((and a-time b-time (not (time-equal-p a-time b-time)))
           (time-less-p a-time b-time))
          ((and a-time (not b-time)) t)
          ((and b-time (not a-time)) nil)
          (t (string< (alist-get 'call_id first) (alist-get 'call_id second))))))

(defun mevedel-analyze-session-cache--frequency (calls field)
  "Count CALLS by their scalar FIELD value, returning a JSON object."
  (let (result)
    (dolist (call calls)
      (let ((key (intern (or (alist-get field call) "unknown"))))
        (setf (alist-get key result) (1+ (or (alist-get key result) 0)))))
    result))

(defun mevedel-analyze-session-cache--summary (calls)
  "Summarize CALLS without converting unknown counts into zeros."
  (let ((count (length calls)) (finished 0) (missing-start 0) (conflicting 0)
        (pairs 0) (input 0) (cached 0) (reported-cache 0) (positive-cache 0)
        totals coverage missing)
    (dolist (call calls)
      (when (alist-get 'finish call) (cl-incf finished))
      (unless (alist-get 'start call) (cl-incf missing-start))
      (when (eq (alist-get 'conflicting call) t) (cl-incf conflicting))
      (let ((i (alist-get 'input_tokens call)) (c (alist-get 'cached_tokens call)))
        (when (and i c)
          (cl-incf pairs) (cl-incf input i) (cl-incf cached c))
        (when c
          (cl-incf reported-cache)
          (when (> c 0) (cl-incf positive-cache)))))
    (dolist (field mevedel-analyze-session-cache--token-fields)
      (let ((key (mevedel-analyze-session-cache--key field)) (sum 0) (known 0))
        (dolist (call calls)
          (let ((value (alist-get key call)))
            (when value (cl-incf known) (cl-incf sum value))))
        (push (cons key (and (> known 0) sum)) totals)
        (push (cons key known) coverage)
        (push (cons key (- count known)) missing)))
    `((calls . ,count) (finished_calls . ,finished)
      (incomplete_calls . ,(- count finished)) (missing_start_calls . ,missing-start)
      (conflicting_calls . ,conflicting)
      (totals . ,(nreverse totals)) (field_coverage . ,(nreverse coverage))
      (missing_fields . ,(nreverse missing))
      (usage_status . ,(or (mevedel-analyze-session-cache--frequency calls 'usage_status)
                          (make-hash-table)))
      (outcomes . ,(or (mevedel-analyze-session-cache--frequency calls 'outcome)
                      (make-hash-table)))
      (ratio_calls . ,pairs) (ratio_denominator_tokens . ,(+ input cached))
      (ratio_cached_tokens . ,cached)
      (ratio_coverage . ,(and (> count 0) (/ (float pairs) count)))
      (weighted_cached_input_share . ,(and (> (+ input cached) 0)
                                          (/ (float cached) (+ input cached))))
      (cache_reporting_calls . ,reported-cache) (cache_positive_calls . ,positive-cache)
      (cache_read_coverage . ,(and (> count 0) (/ (float reported-cache) count)))
      (cache_positive_call_fraction . ,(and (> reported-cache 0)
                                           (/ (float positive-cache) reported-cache))))))

(defun mevedel-analyze-session-cache--groups (calls fields)
  "Group CALLS by FIELDS, returning a sorted vector of summaries."
  (let ((table (make-hash-table :test 'equal)) result)
    (dolist (call calls)
      (let ((key (mapcar (lambda (field) (alist-get field call)) fields)))
        (puthash key (cons call (gethash key table)) table)))
    (maphash
     (lambda (key members)
       (push (append (cl-mapcar #'cons fields key)
                     `((summary . ,(mevedel-analyze-session-cache--summary members))))
             result)) table)
    (vconcat (sort result (lambda (a b) (string< (prin1-to-string (mapcar #'cdr
                                                                 (butlast a)))
                                                (prin1-to-string (mapcar #'cdr
                                                                        (butlast b)))))))))

(defun mevedel-analyze-session-cache (paths)
  "Analyze telemetry files and directories in PATHS, returning a JSON-ready alist.
Call identities are deduplicated across all inputs.  Identical stage records
count once; conflicting stages or ownership exclude that call's token counts.
Only normalized per-call events are evidence.  Older logs are not backfilled."
  (let* ((discovery (mevedel-analyze-session-cache--files paths))
         (files (car discovery)) (issues (cdr discovery))
         (table (make-hash-table :test 'equal))
         (duplicates 0) (other-events 0) calls file-reports)
    (dolist (file files)
      (let ((parsed (mevedel-analyze-session-cache--read-file file)))
        (setq issues (append issues (plist-get parsed :issues)))
        (cl-incf other-events (plist-get parsed :other-events))
        (push `((file . ,file)
                (provider_call_events . ,(length (plist-get parsed :records)))
                (other_events . ,(plist-get parsed :other-events))
                (issues . ,(length (plist-get parsed :issues)))) file-reports)
        (dolist (record (plist-get parsed :records))
          (let* ((metadata (plist-get record :metadata))
                 (id (plist-get metadata :call-id))
                 (stage (if (eq (plist-get metadata :stage) 'start) :start :finish))
                 (state (gethash id table)) (existing (plist-get state stage)))
            (if existing
                (let ((different (mevedel-analyze-session-cache--different-fields
                                  (plist-get existing :metadata) metadata
                                  mevedel-analyze-session-cache--event-fields)))
                  (if different
                      (progn
                        (setq state (plist-put state :conflict t))
                        (push (mevedel-analyze-session-cache--issue
                               "conflicting-duplicate" file (plist-get record :line)
                               different) issues))
                    (cl-incf duplicates)))
              (setq state (plist-put state stage record)))
            (let ((start (plist-get (plist-get state :start) :metadata))
                  (finish (plist-get (plist-get state :finish) :metadata)))
              (when (and start finish (not (plist-get state :ownership-checked)))
                (setq state (plist-put state :ownership-checked t))
                (let ((different (mevedel-analyze-session-cache--different-fields
                                  start finish mevedel-analyze-session-cache--identity-fields)))
                  (when different
                    (setq state (plist-put state :conflict t))
                    (push (mevedel-analyze-session-cache--issue
                           "conflicting-ownership" file (plist-get record :line)
                           different) issues)))))
            (puthash id state table)))))
    (maphash (lambda (id state)
               (push (mevedel-analyze-session-cache--call id state) calls)) table)
    (setq calls (sort calls #'mevedel-analyze-session-cache--chronological-p))
    `((schema_version . 1)
      (coverage_note . ,(cond
                        ((null calls) "Insufficient per-call data; cumulative settlements are not analyzed.")
                        ((cl-some (lambda (file) (= (alist-get 'provider_call_events file) 0))
                                  file-reports)
                         "Limited per-call coverage: some input files contain no provider-call events; older settlements are not backfilled.")
                        (t "Coverage is limited to observed provider calls; pre-instrumentation calls cannot be inferred from settlements.")))
      (summary . ,(mevedel-analyze-session-cache--summary calls))
      (duplicate_records . ,duplicates) (other_events_ignored . ,other-events)
      (files . ,(vconcat (nreverse file-reports)))
      (issues . ,(vconcat (sort issues
                               (lambda (a b)
                                 (if (equal (alist-get 'file a) (alist-get 'file b))
                                     (< (or (alist-get 'line a) 0) (or (alist-get 'line b) 0))
                                   (string< (alist-get 'file a) (alist-get 'file b)))))))
      (breakdowns . ((session . ,(mevedel-analyze-session-cache--groups calls '(session_id)))
                     (backend_model . ,(mevedel-analyze-session-cache--groups
                                        calls '(backend backend_type model)))
                     (workload . ,(mevedel-analyze-session-cache--groups calls '(workload)))))
      (calls . ,(vconcat calls)))))

(defun mevedel-analyze-session-cache--percentage (value)
  "Format ratio VALUE as a percentage, or mark it unavailable."
  (if value (format "%.2f%%" (* 100 value)) "unavailable"))

(defun mevedel-analyze-session-cache--totals-text (summary)
  "Format token totals from SUMMARY, keeping unknown totals visible."
  (let ((totals (alist-get 'totals summary)))
    (mapconcat (lambda (field)
                 (format "%s=%s" field (or (alist-get field totals) "unknown")))
               '(input_tokens cached_tokens cache_write_tokens output_tokens) " ")))

(defun mevedel-analyze-session-cache-format-text (report)
  "Return a concise text rendering of cache analysis REPORT."
  (with-temp-buffer
    (let* ((summary (alist-get 'summary report)) (count (alist-get 'calls summary)))
      (insert (format "Provider-call cache analysis: %d calls in %d files\n%s\n"
                      count (length (alist-get 'files report))
                      (alist-get 'coverage_note report)))
      (insert (format "Tokens: %s\n" (mevedel-analyze-session-cache--totals-text summary)))
      (insert (format "Cached-input share: %s (%d/%d paired calls; denominator %d tokens)\n"
                      (mevedel-analyze-session-cache--percentage
                       (alist-get 'weighted_cached_input_share summary))
                      (alist-get 'ratio_calls summary) count
                      (alist-get 'ratio_denominator_tokens summary)))
      (insert (format "Positive cache reads: %s (%d positive / %d reporting; %d/%d calls covered)\n"
                      (mevedel-analyze-session-cache--percentage
                       (alist-get 'cache_positive_call_fraction summary))
                      (alist-get 'cache_positive_calls summary)
                      (alist-get 'cache_reporting_calls summary)
                      (alist-get 'cache_reporting_calls summary) count))
      (insert (format "Incomplete: %d; missing starts: %d; conflicting: %d; duplicate records: %d\n"
                      (alist-get 'incomplete_calls summary)
                      (alist-get 'missing_start_calls summary)
                      (alist-get 'conflicting_calls summary)
                      (alist-get 'duplicate_records report)))
      (dolist (field '(field_coverage missing_fields usage_status outcomes))
        (let ((value (alist-get field summary)))
          (insert (format "%s: %s\n" field
                          (if (hash-table-p value) "none"
                            (mapconcat (lambda (pair) (format "%s=%s" (car pair) (cdr pair)))
                                       value " ")))))))
    (dolist (group '(session backend_model workload))
      (insert (format "\nBy %s:\n" group))
      (seq-doseq (entry (alist-get group (alist-get 'breakdowns report)))
        (let ((summary (alist-get 'summary entry)))
          (insert (format "  %s: %d calls; %s; cached share %s (%d paired calls)\n"
                          (mapconcat (lambda (pair) (format "%s" (or (cdr pair) "unknown")))
                                     (butlast entry) " / ")
                          (alist-get 'calls summary)
                          (mevedel-analyze-session-cache--totals-text summary)
                          (mevedel-analyze-session-cache--percentage
                           (alist-get 'weighted_cached_input_share summary))
                          (alist-get 'ratio_calls summary))))))
    (insert "\nCalls (chronological; unknown times last):\n")
    (seq-doseq (call (alist-get 'calls report))
      (let ((source (or (alist-get 'finish call) (alist-get 'start call))))
        (insert (format "  %s %s session=%s turn=%s request=%s agent=%s %s/%s %s outcome=%s usage=%s duration-ms=%s %s %s:%s%s\n"
                        (or (alist-get 'started_at call) (alist-get 'finished_at call) "unknown-time")
                        (alist-get 'call_id call) (alist-get 'session_id call)
                        (or (alist-get 'turn call) "unknown")
                        (or (alist-get 'request_id call) "unknown")
                        (or (alist-get 'agent_path call) "unknown")
                        (or (alist-get 'backend call) "unknown")
                        (or (alist-get 'model call) "unknown")
                        (or (alist-get 'workload call) "unknown")
                        (alist-get 'outcome call) (alist-get 'usage_status call)
                        (or (alist-get 'duration_ms call) "unknown")
                        (mevedel-analyze-session-cache--totals-text `((totals . ,call)))
                        (alist-get 'file source) (alist-get 'line source)
                        (if (eq (alist-get 'conflicting call) t) " [conflict: counts excluded]" "")))))
    (unless (= (length (alist-get 'issues report)) 0)
      (insert "\nDiagnostics (log contents omitted):\n")
      (seq-doseq (issue (alist-get 'issues report))
        (insert (format "  %s %s%s\n" (alist-get 'kind issue) (alist-get 'file issue)
                        (if (alist-get 'line issue)
                            (format ":%d" (alist-get 'line issue)) "")))))
    (buffer-string)))

(defun mevedel-analyze-session-cache-main ()
  "Batch entry point for offline session analysis.
Consume --json and file or directory arguments from `command-line-args-left'."
  (let ((args command-line-args-left) json help paths)
    (setq command-line-args-left nil)
    (condition-case err
        (progn
          (dolist (arg args)
            (cond ((equal arg "--") nil)
                  ((equal arg "--json") (setq json t))
                  ((equal arg "--help") (setq help t))
                  ((string-prefix-p "--" arg) (error "Unknown option: %s" arg))
                  (t (push arg paths))))
          (if help
              (princ "Usage: emacs -Q --batch -l scripts/analyze-session-cache.el -f mevedel-analyze-session-cache-main -- [--json] PATH...\n")
            (unless paths (error "Provide telemetry files or session directories"))
            (let ((report (mevedel-analyze-session-cache (nreverse paths))))
              (princ (if json
                         (concat (json-serialize report :null-object nil :false-object :false) "\n")
                       (mevedel-analyze-session-cache-format-text report))))))
      (error
       (princ (format "%s\n" (error-message-string err)) #'external-debugging-output)
       (kill-emacs 2)))))

(provide 'mevedel-session-cache-analysis)
;;; analyze-session-cache.el ends here
