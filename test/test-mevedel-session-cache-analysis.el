;;; test-mevedel-session-cache-analysis.el --- Offline cache analysis tests -*- lexical-binding: t -*-

;;; Commentary:

;; Known-count telemetry fixtures exercise the offline reader and public CLI.
;; No provider calls, existing log modifications, or external dependencies.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defconst test-mevedel-session-cache-analysis--script
  (expand-file-name
   (file-name-concat ".." "scripts" "analyze-session-cache.el")
   (file-name-directory
    (or buffer-file-name load-file-name byte-compile-current-file)))
  "Source file for the standalone public batch command.")

(require 'mevedel-session-cache-analysis
         test-mevedel-session-cache-analysis--script)

(defun test-mevedel-session-cache-analysis--event (id stage &rest properties)
  "Build a provider-call event for ID at STAGE, overriding PROPERTIES."
  (let ((event (append
                (list :schema-version 1 :event 'provider-call :call-id id
                      :stage stage :time "2026-10-03T09:30:00.000+0000"
                      :session-id "session-1" :request-id "request-1" :turn 1
                      :agent-path "/root" :backend "provider" :backend-type 'gptel-openai
                      :model 'model-1 :workload 'root)
                (when (eq stage 'finish)
                  (list :outcome 'success :duration-ms 20 :usage-status 'complete
                        :input-tokens 100 :cached-tokens 0 :cache-write-tokens nil
                        :output-tokens 10)))))
    (while properties
      (setq event (plist-put event (pop properties) (pop properties))))
    event))

(defun test-mevedel-session-cache-analysis--write (file records &optional tail)
  "Write RECORDS as telemetry lines to FILE, followed by literal TAIL."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (let ((print-escape-newlines t) (print-level nil) (print-length nil))
      (dolist (record records)
        (prin1 record (current-buffer))
        (insert "\n")))
    (when tail (insert tail))))

(cl-defmacro test-mevedel-session-cache-analysis--with-log ((root file) &rest body)
  "Run BODY with temporary ROOT and default telemetry FILE, then remove ROOT."
  (declare (indent 1))
  `(let* ((,root (make-temp-file "mevedel-cache-analysis-" t))
          (,file (file-name-concat ,root "session-1" "telemetry-log.el")))
     (unwind-protect (progn ,@body)
       (delete-directory ,root t))))

(defun test-mevedel-session-cache-analysis--cli (&rest args)
  "Run the public batch CLI with ARGS, returning (EXIT-CODE . OUTPUT)."
  (with-temp-buffer
    (let ((status (apply #'call-process
                         (file-name-concat invocation-directory invocation-name)
                         nil (list (current-buffer) t) nil
                         "-Q" "--batch" "-l" test-mevedel-session-cache-analysis--script
                         "-f" "mevedel-analyze-session-cache-main" "--" args)))
      (cons status (buffer-string)))))

(mevedel-deftest mevedel-analyze-session-cache
  ()
  ,test
  (test)
  :doc "totals and paired token-weighted ratios exclude cumulative settlements"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (test-mevedel-session-cache-analysis--write
     file
     (list
      (test-mevedel-session-cache-analysis--event "a" 'start)
      (test-mevedel-session-cache-analysis--event
       "a" 'finish :input-tokens 100 :cached-tokens 900 :cache-write-tokens 80)
      (test-mevedel-session-cache-analysis--event "b" 'start)
      (test-mevedel-session-cache-analysis--event
       "b" 'finish :input-tokens 100 :cached-tokens 0 :cache-write-tokens 0 :output-tokens 20)
      '(:schema-version 1 :event request-settled :input-tokens 999999
        :cached-tokens 999999 :output-tokens 999999)))
    (let* ((report (mevedel-analyze-session-cache (list root)))
           (summary (alist-get 'summary report))
           (totals (alist-get 'totals summary)))
      (should (= (alist-get 'calls summary) 2))
      (should (= (alist-get 'input_tokens totals) 200))
      (should (= (alist-get 'cached_tokens totals) 900))
      (should (= (alist-get 'cache_write_tokens totals) 80))
      (should (= (alist-get 'output_tokens totals) 30))
      (should (= (alist-get 'weighted_cached_input_share summary) (/ 900.0 1100)))
      (should (= (alist-get 'cache_positive_call_fraction summary) 0.5))
      (should (= (alist-get 'ratio_denominator_tokens summary) 1100))
      (should (= (alist-get 'ratio_calls summary) 2))
      (should (= (alist-get 'ratio_coverage summary) 1.0))
      (should (= (alist-get 'other_events_ignored report) 1))
      (should (= (alist-get 'line (alist-get 'start (aref (alist-get 'calls report) 0))) 1))
      (should (= (alist-get 'line (alist-get 'finish (aref (alist-get 'calls report) 0))) 2))))

  :doc "partial and missing counts disclose paired and field coverage"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (test-mevedel-session-cache-analysis--write
     file
     (list (test-mevedel-session-cache-analysis--event
            "a" 'finish :input-tokens 20 :cached-tokens 80 :output-tokens 5)
           (test-mevedel-session-cache-analysis--event
            "b" 'finish :cached-tokens nil :output-tokens nil :usage-status 'partial)
           (test-mevedel-session-cache-analysis--event
            "c" 'finish :input-tokens nil :cached-tokens 50 :output-tokens nil)
           (test-mevedel-session-cache-analysis--event
            "d" 'finish :input-tokens nil :cached-tokens nil :output-tokens nil
            :outcome 'error :usage-status 'missing)
           (test-mevedel-session-cache-analysis--event "e" 'start)))
    (let* ((report (mevedel-analyze-session-cache (list file)))
           (summary (alist-get 'summary report)))
      (should (= (alist-get 'calls summary) 5))
      (should (= (alist-get 'finished_calls summary) 4))
      (should (= (alist-get 'incomplete_calls summary) 1))
      (should (= (alist-get 'missing_start_calls summary) 4))
      (should (= (alist-get 'weighted_cached_input_share summary) 0.8))
      (should (= (alist-get 'ratio_coverage summary) 0.2))
      (should (= (alist-get 'cache_positive_call_fraction summary) 1.0))
      (should (= (alist-get 'cache_read_coverage summary) 0.4))
      (should (= (alist-get 'cached_tokens (alist-get 'totals summary)) 130))
      (should (= (alist-get 'input_tokens (alist-get 'field_coverage summary)) 2))
      (should (= (alist-get 'cached_tokens (alist-get 'missing_fields summary)) 3))
      (should (= (alist-get 'complete (alist-get 'usage_status summary)) 1))
      (should (= (alist-get 'partial (alist-get 'usage_status summary)) 2))
      (should (= (alist-get 'missing (alist-get 'usage_status summary)) 2))
      (should (= (alist-get 'error (alist-get 'outcomes summary)) 1))
      (should (= (alist-get 'incomplete (alist-get 'outcomes summary)) 1))))

  :doc "zero counts stay known while a zero ratio denominator is unavailable"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (test-mevedel-session-cache-analysis--write
     file (list (test-mevedel-session-cache-analysis--event
                 "zero" 'finish :input-tokens 0 :cached-tokens 0
                 :cache-write-tokens 0 :output-tokens 0)))
    (let* ((report (mevedel-analyze-session-cache (list root)))
           (summary (alist-get 'summary report)))
      (should (= (alist-get 'input_tokens (alist-get 'totals summary)) 0))
      (should (= (alist-get 'cache_write_tokens (alist-get 'field_coverage summary)) 1))
      (should (= (alist-get 'ratio_calls summary) 1))
      (should-not (alist-get 'weighted_cached_input_share summary))
      (should (= (alist-get 'cache_positive_call_fraction summary) 0.0))
      (should (string-match-p "Cached-input share: unavailable"
                              (mevedel-analyze-session-cache-format-text report)))))

  :doc "invalid count types and impossible cache-write subsets are exposed"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (test-mevedel-session-cache-analysis--write
     file (list
           (test-mevedel-session-cache-analysis--event
            "negative" 'finish :input-tokens -10 :cached-tokens 5
            :output-tokens 2.5 :cache-write-tokens '(payload "SECRET"))
           (test-mevedel-session-cache-analysis--event
            "subset" 'finish :input-tokens 10 :cached-tokens 90 :cache-write-tokens 11)
           (test-mevedel-session-cache-analysis--event
            "upstream-invalid" 'finish :usage-status 'invalid :cached-tokens nil)))
    (let* ((report (mevedel-analyze-session-cache (list file)))
           (summary (alist-get 'summary report))
           (call (aref (alist-get 'calls report) 0)))
      (should (= (alist-get 'invalid (alist-get 'usage_status summary)) 3))
      (should (= (alist-get 'input_tokens (alist-get 'totals summary)) 110))
      (should (= (alist-get 'cached_tokens (alist-get 'totals summary)) 95))
      (should-not (alist-get 'cache_write_tokens (alist-get 'totals summary)))
      (should (= (alist-get 'ratio_calls summary) 1))
      (should (= (alist-get 'weighted_cached_input_share summary) 0.9))
      (should (equal (alist-get 'invalid_fields call)
                     ["input_tokens" "cache_write_tokens" "output_tokens"]))
      (should-not (string-match-p "SECRET" (json-serialize report :null-object nil :false-object :false)))))

  :doc "overlapping paths and identical copied events count each identity once"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (let* ((copy (file-name-concat root "session-copy" "telemetry-log.el"))
           (records (list (test-mevedel-session-cache-analysis--event "same" 'start)
                          (test-mevedel-session-cache-analysis--event "same" 'finish))))
      (test-mevedel-session-cache-analysis--write file records)
      (test-mevedel-session-cache-analysis--write copy records)
      (let* ((report (mevedel-analyze-session-cache
                      (list root file (file-name-directory file) copy)))
             (summary (alist-get 'summary report)))
        (should (= (length (alist-get 'files report)) 2))
        (should (= (alist-get 'calls summary) 1))
        (should (= (alist-get 'input_tokens (alist-get 'totals summary)) 100))
        (should (= (alist-get 'duplicate_records report) 2))
        (should (= (length (alist-get 'issues report)) 0)))))

  :doc "conflicting duplicates and changed ownership exclude ambiguous counts"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (test-mevedel-session-cache-analysis--write
     file (list
           (test-mevedel-session-cache-analysis--event "duplicate" 'finish)
           (test-mevedel-session-cache-analysis--event "duplicate" 'finish :cached-tokens 500)
           (test-mevedel-session-cache-analysis--event "ownership" 'start)
           (test-mevedel-session-cache-analysis--event "ownership" 'finish :model 'other-model)
           (test-mevedel-session-cache-analysis--event "valid" 'finish)))
    (let* ((report (mevedel-analyze-session-cache (list root)))
           (summary (alist-get 'summary report))
           (issues (alist-get 'issues report)))
      (should (= (alist-get 'calls summary) 3))
      (should (= (alist-get 'conflicting_calls summary) 2))
      (should (= (alist-get 'ratio_calls summary) 1))
      (should (= (alist-get 'input_tokens (alist-get 'totals summary)) 100))
      (should (equal (alist-get 'kind (aref issues 0)) "conflicting-duplicate"))
      (should (equal (alist-get 'fields (aref issues 0)) ["cached_tokens"]))
      (should (equal (alist-get 'kind (aref issues 1)) "conflicting-ownership"))
      (should (string-match-p "counts excluded"
                              (mevedel-analyze-session-cache-format-text report)))
      (should (stringp (json-serialize report :null-object nil :false-object :false)))))

  :doc "session routing workload and time-zone-aware chronology remain separate"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (test-mevedel-session-cache-analysis--write
     file (list
           (test-mevedel-session-cache-analysis--event
            "a-late" 'finish :time "2026-10-03T12:00:00.000+0200")
           (test-mevedel-session-cache-analysis--event
            "z-early" 'finish :time "2026-10-03T09:00:00.000+0000"
            :session-id "session-2" :workload 'agent :agent-path "/root/reviewer"
            :backend "other-provider" :model "model-2" :turn 2 :outcome 'aborted)
           (test-mevedel-session-cache-analysis--event
            "helper" 'finish :workload 'naming :purpose 'title)
           (test-mevedel-session-cache-analysis--event
            "summary" 'finish :workload 'context-summary :purpose 'compaction)))
    (let* ((report (mevedel-analyze-session-cache (list root)))
           (calls (alist-get 'calls report))
           (groups (alist-get 'breakdowns report)))
      (should (equal (alist-get 'call_id (aref calls 0)) "z-early"))
      (should (equal (alist-get 'agent_path (aref calls 0)) "/root/reviewer"))
      (should (= (alist-get 'turn (aref calls 0)) 2))
      (should (= (length (alist-get 'session groups)) 2))
      (should (= (length (alist-get 'backend_model groups)) 2))
      (should (= (length (alist-get 'workload groups)) 4))
      (should (equal (alist-get 'outcome (aref calls 0)) "aborted"))
      (should (equal (alist-get 'call_id (aref calls 3)) "a-late"))))

  :doc "empty older and mixed logs never substitute settlement usage"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (let ((older (file-name-concat root "older" "telemetry-log.el")))
      (test-mevedel-session-cache-analysis--write file nil)
      (let ((empty (mevedel-analyze-session-cache (list root))))
        (should (= (alist-get 'calls (alist-get 'summary empty)) 0))
        (should-not (alist-get 'input_tokens (alist-get 'totals (alist-get 'summary empty))))
        (should (string-match-p "Insufficient per-call data" (alist-get 'coverage_note empty))))
      (test-mevedel-session-cache-analysis--write
       older '((:schema-version 1 :event request-settled :input-tokens 3000
                :cached-tokens 9000 :output-tokens 400)))
      (let ((old (mevedel-analyze-session-cache (list older))))
        (should (= (alist-get 'other_events_ignored old) 1))
        (should (= (alist-get 'calls (alist-get 'summary old)) 0)))
      (test-mevedel-session-cache-analysis--write
       file (list (test-mevedel-session-cache-analysis--event "new" 'finish)))
      (let* ((mixed (mevedel-analyze-session-cache (list root)))
             (summary (alist-get 'summary mixed)))
        (should (= (alist-get 'input_tokens (alist-get 'totals summary)) 100))
        (should (string-match-p "Limited per-call coverage" (alist-get 'coverage_note mixed))))))

  :doc "malformed lines and interrupted tails are flagged without evaluating data"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (let ((sentinel (file-name-concat root "MUST-NOT-EXIST")))
      (test-mevedel-session-cache-analysis--write
       file '((:schema-version 1 :event other)
              (:schema-version 1 :event provider-call :stage start)
              (:schema-version 1 :event other :event provider-call)
              (:schema-version 1 :event other . 5)
              (:schema-version 2 :event provider-call)
              (write-region "SECRET" nil "MUST-NOT-EXIST"))
       (concat ") bad SECRET\n#1=(:schema-version 1 :event other :bad #1#)\n"
               (format "#.(write-region \"SECRET\" nil %S)\n" sentinel)
               (prin1-to-string (test-mevedel-session-cache-analysis--event "valid" 'finish))
               "\n(:schema-version 1 :event provider-call\n"))
      (let* ((report (mevedel-analyze-session-cache (list file)))
             (issues (alist-get 'issues report)))
        (should (= (alist-get 'calls (alist-get 'summary report)) 1))
        (should (= (length issues) 9))
        (should (= (alist-get 'line (aref issues 0)) 2))
        (should (equal (alist-get 'kind (aref issues (1- (length issues))))
                       "interrupted-trailing-record"))
        (should-not (file-exists-p sentinel))
        (should-not (string-match-p "SECRET" (mevedel-analyze-session-cache-format-text report)))
        (should-not (string-match-p "SECRET" (json-serialize report :null-object nil :false-object :false))))))

  :doc "unknown payload-like properties are neither retained nor rendered"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (test-mevedel-session-cache-analysis--write
     file (list (test-mevedel-session-cache-analysis--event
                 "safe" 'finish :prompt "SECRET PROMPT" :response "SECRET RESPONSE"
                 :headers '(authorization "SECRET HEADER")
                 :tool-results '(:input "SECRET INPUT" :output "SECRET OUTPUT"))))
    (let* ((report (mevedel-analyze-session-cache (list root)))
           (text (mevedel-analyze-session-cache-format-text report)))
      (should (= (alist-get 'calls (alist-get 'summary report)) 1))
      (should-not (string-match-p "SECRET\\|PROMPT\\|RESPONSE\\|HEADER" text))
      (should-not (string-match-p "SECRET" (json-serialize report :null-object nil :false-object :false)))))

  :doc "reader work and identity size are bounded without discarding later records"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (let ((mevedel-analyze-session-cache--max-record-length 4096))
      (test-mevedel-session-cache-analysis--write
       file (list (test-mevedel-session-cache-analysis--event
                   (make-string 513 ?x) 'finish))
       (concat (make-string 4097 ?x) "\n"
               (prin1-to-string (test-mevedel-session-cache-analysis--event "valid" 'finish))
               "\n"))
      (let* ((report (mevedel-analyze-session-cache (list file)))
             (issues (alist-get 'issues report)))
        (should (= (alist-get 'calls (alist-get 'summary report)) 1))
        (should (= (length issues) 2))
        (should (equal (alist-get 'kind (aref issues 0)) "invalid-call"))
        (should (equal (alist-get 'kind (aref issues 1)) "oversized-line")))))

  :doc "missing paths produce bounded diagnostics rather than fabricated usage"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (let ((report (mevedel-analyze-session-cache (list file))))
      (should (= (alist-get 'calls (alist-get 'summary report)) 0))
      (should (equal (alist-get 'kind (aref (alist-get 'issues report) 0)) "missing-path")))))

(mevedel-deftest mevedel-analyze-session-cache-format-text
  (:doc "text reports counts coverage breakdowns and source lines without JSON")
  (test-mevedel-session-cache-analysis--with-log (root file)
    (test-mevedel-session-cache-analysis--write
     file (list (test-mevedel-session-cache-analysis--event
                 "observed" 'finish :cached-tokens 100 :cache-write-tokens 40)))
    (let ((text (mevedel-analyze-session-cache-format-text
                 (mevedel-analyze-session-cache (list root)))))
      (dolist (pattern '("1 calls in 1 files" "Cached-input share: 50.00%"
                         "Positive cache reads: 100.00%" "1/1 calls covered"
                         "cache_write_tokens=40" "By session:" "By backend_model:"
                         "By workload:" "observed" "telemetry-log.el:1"))
        (should (string-match-p (regexp-quote pattern) text))))))

(mevedel-deftest mevedel-analyze-session-cache-main
  ()
  ,test
  (test)
  :doc "public JSON batch command handles directories explicit files and overlaps"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (test-mevedel-session-cache-analysis--write
     file (list (test-mevedel-session-cache-analysis--event
                 "cli" 'finish :cached-tokens 300 :cache-write-tokens 25)))
    (let* ((result (test-mevedel-session-cache-analysis--cli "--json" root file))
           (report (json-parse-string (cdr result) :object-type 'alist :null-object nil
                                      :false-object :false))
           (summary (alist-get 'summary report)))
      (should (= (car result) 0))
      (should (= (alist-get 'calls summary) 1))
      (should (= (alist-get 'cached_tokens (alist-get 'totals summary)) 300))
      (should (= (alist-get 'weighted_cached_input_share summary) 0.75))
      (should (= (length (alist-get 'files report)) 1))
      (should (= (length (alist-get 'calls report)) 1))
      (should (eq (alist-get 'conflicting (aref (alist-get 'calls report) 0)) :false))))

  :doc "public text batch command consumes options and prints the report only"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (test-mevedel-session-cache-analysis--write
     file (list (test-mevedel-session-cache-analysis--event "text-cli" 'start)))
    (let ((result (test-mevedel-session-cache-analysis--cli file)))
      (should (= (car result) 0))
      (should (string-prefix-p "Provider-call cache analysis:" (cdr result)))
      (should (string-match-p "Incomplete: 1" (cdr result)))
      (should (string-match-p "Cached-input share: unavailable" (cdr result)))
      (should-not (string-match-p "Debugger\\|Warning\\|Error" (cdr result)))))

  :doc "public JSON command renders conflict diagnostics and unknown counts safely"
  (test-mevedel-session-cache-analysis--with-log (root file)
    (test-mevedel-session-cache-analysis--write
     file (list (test-mevedel-session-cache-analysis--event "conflict" 'finish)
                (test-mevedel-session-cache-analysis--event
                 "conflict" 'finish :output-tokens 20)) "(:interrupted")
    (let* ((result (test-mevedel-session-cache-analysis--cli "--json" file))
           (report (json-parse-string (cdr result) :object-type 'alist :null-object nil
                                      :false-object :false)))
      (should (= (car result) 0))
      (should (= (alist-get 'conflicting_calls (alist-get 'summary report)) 1))
      (should-not (alist-get 'input_tokens (alist-get 'totals (alist-get 'summary report))))
      (should (= (length (alist-get 'issues report)) 2))))

  :doc "public command offers help and rejects absent paths and unknown options"
  (let ((help (test-mevedel-session-cache-analysis--cli "--help"))
        (missing (test-mevedel-session-cache-analysis--cli))
        (unknown (test-mevedel-session-cache-analysis--cli "--bogus")))
    (should (= (car help) 0))
    (should (string-prefix-p "Usage:" (cdr help)))
    (should (= (car missing) 2))
    (should (string-match-p "Provide telemetry files" (cdr missing)))
    (should (= (car unknown) 2))
    (should (string-match-p "Unknown option" (cdr unknown)))))

(provide 'test-mevedel-session-cache-analysis)
;;; test-mevedel-session-cache-analysis.el ends here
