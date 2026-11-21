;;; test-niwa-execution-telemetry.el -- Tests for execution telemetry -*- lexical-binding: t -*-

;;; Commentary:

;; Tests safe execution telemetry projection, sandbox summaries, and profiler
;; resource capture.

;;; Code:

(require 'cl-lib)
(require 'niwa-agents)
(require 'niwa-execution-telemetry)
(require 'niwa-structs)
(require 'niwa-telemetry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-execution-test-helpers"))

(niwa-deftest niwa-execution-telemetry-command-properties
  (:doc "recognizes Eask invocations and bounded repository-local targets")
  (progn
    (should (eq 'full
                (plist-get
                 (niwa-execution-telemetry-command-properties
                  "eask test ert")
                 :test-scope)))
    (should
     (equal '("test/test-one.el" "test/nested/test-two.el")
            (plist-get
             (niwa-execution-telemetry-command-properties
              "eask test ert test/test-one.el test/nested/test-two.el")
             :test-targets)))
    (should-not
     (niwa-execution-telemetry-command-properties
      "printf 'eask test ert'"))
    ;; Cleanup and compilation are not suites; the test script is.
    (dolist (case '(("npx @emacs-eask/cli clean elc" . clean)
                    ("npx @emacs-eask/cli compile" . compile)
                    ("npx @emacs-eask/cli run script test -- --jobs 4" . full)
                    ("npx @emacs-eask/cli clean elc && npx @emacs-eask/cli test ert test/test-one.el" . focused)
                    ("eask upgrade gptel" . other)))
      (should (eq (cdr case)
                  (plist-get (niwa-execution-telemetry-command-properties (car case))
                             :test-scope))))))

(niwa-deftest niwa-execution-telemetry-context-create
  (:doc "captures one execution's summary owners behind an opaque context")
  (let* ((pipeline-cell (list nil))
         (agent-cell (list nil))
         (invocation
          (niwa-agent-invocation--create :sandbox-summary-cell agent-cell))
         (context
          (niwa-execution-telemetry-context-create
           :owner "/root" :invocation invocation
           :pipeline-summary-cell pipeline-cell)))
    (should context)
    (niwa-execution-telemetry-record-sandbox-attempt
     context '(:sandbox bubblewrap :filesystem workspace-write
               :network isolated :proc fresh)
     t nil)
    (should (= 1 (plist-get (car pipeline-cell) :attempt-count)))
    (should (= 1 (plist-get (car agent-cell) :attempt-count)))))

(niwa-deftest niwa-execution-telemetry-context-properties
  (:doc "reports a claimed resource path and its eventual byte count")
  (let* ((root (make-temp-file "niwa-context-properties-" t))
         (report (file-name-concat root "diagnostics" "run" "report.txt"))
         (context (niwa-execution-telemetry-context-create)))
    (unwind-protect
        (progn
          (setf
           (niwa-execution-telemetry--context-resource-report-path context)
           report)
          (let ((properties
                 (niwa-execution-telemetry-context-properties context)))
            (should (plist-get properties :native-resource-capture))
            (should-not
             (plist-member properties :native-resource-report-bytes)))
          (make-directory (file-name-directory report) t)
          (write-region "facts" nil report nil 'silent)
          (should (= 5
                     (plist-get
                      (niwa-execution-telemetry-context-properties context)
                      :native-resource-report-bytes))))
      (delete-directory root t))))

(niwa-deftest niwa-execution-telemetry-context-summary
  (:doc "returns an isolated copy of the current sandbox summary")
  (let ((context (niwa-execution-telemetry-context-create)))
    (niwa-execution-telemetry-record-sandbox-attempt
     context '(:sandbox bubblewrap) t nil)
    (let ((summary (niwa-execution-telemetry-context-summary context)))
      (plist-put summary :sandbox 'changed)
      (should
       (eq 'bubblewrap
           (plist-get
            (niwa-execution-telemetry-context-summary context)
            :sandbox))))))

(niwa-deftest niwa-execution-telemetry-finish
  (:doc "forwards terminal properties only to the context-owned span")
  (let* ((context (niwa-execution-telemetry-context-create))
         (span '(:span-id "span"))
         captured)
    (setf (niwa-execution-telemetry--context-span context) span)
    (cl-letf (((symbol-function 'niwa-telemetry-finish)
               (lambda (seen &rest properties)
                 (setq captured (cons seen properties)))))
      (niwa-execution-telemetry-finish context :outcome 'success))
    (should (eq span (car captured)))
    (should (eq 'success (plist-get (cdr captured) :outcome)))))

(niwa-deftest niwa-execution-telemetry--cache-identity
  (:doc "hashes cache roots deterministically without exposing their values")
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "HOME" "/secret/one")
    (let ((first (niwa-execution-telemetry--cache-identity)))
      (should (= 64 (length first)))
      (should-not (string-match-p "secret" first))
      (should (equal first (niwa-execution-telemetry--cache-identity)))
      (setenv "HOME" "/secret/two")
      (should-not
       (equal first (niwa-execution-telemetry--cache-identity))))))

(niwa-deftest niwa-execution-telemetry-prepare-resource-capture
  (:before-each (skip-unless (file-executable-p "/usr/bin/time"))
   :doc "wraps at most one profiled full suite with GNU time")
  (let* ((root (make-temp-file "niwa-resource-capture-" t))
         (session (test-niwa-execution--session root))
         (context
          (niwa-execution-telemetry-context-create :session session))
         (niwa-execution-telemetry--resource-capture-claims
          (make-hash-table :test #'equal))
         (niwa-telemetry--profiler-session session)
         (niwa-telemetry--profiler-run-id "run-test"))
    (unwind-protect
        (let* ((command-text "npx @emacs-eask/cli test ert test/test-*")
               (command (list "bash" "-lc" command-text))
               (wrapped
                (niwa-execution-telemetry-prepare-resource-capture
                 context command-text command))
               (properties
                (niwa-execution-telemetry-context-properties context)))
          (should wrapped)
          (should
           (equal
            (append (list "/usr/bin/time" "-v" "-o"
                          (niwa-execution-telemetry--context-resource-report-path
                           context)
                          "--")
                    command)
            wrapped))
          (should (string-suffix-p
                   "diagnostics/run-test/full-suite-time.txt"
                   (plist-get properties :resource-report-relative-path)))
          (should-not
           (niwa-execution-telemetry-prepare-resource-capture
            context command-text command)))
      (delete-directory root t))))

(niwa-deftest niwa-execution-telemetry-safe-facts
  (:doc "keeps confinement dimensions while excluding private reasons")
  (let ((facts
         (niwa-execution-telemetry-safe-facts
          '(:sandbox bubblewrap :filesystem workspace-write
            :network isolated :proc fresh :protected-paths 4
            :additional-filesystem-read 2
            :additional-filesystem-write 1
            :reason "private launcher output"))))
    (should (eq 'bubblewrap (plist-get facts :sandbox)))
    (should (= 4 (plist-get facts :protected-path-count)))
    (should (= 2 (plist-get facts :additional-read-count)))
    (should-not (plist-member facts :reason))))

(niwa-deftest niwa-execution-telemetry-record
  (:doc "uses the audit target and removes path-bearing properties")
  (let* ((parent (niwa-session--create :name "parent"))
         (side (niwa-session--create :name "side" :audit-session parent))
         (context
          (niwa-execution-telemetry-context-create
           :session side :execution-id "exec-1" :tool-use-id "tool-1"
           :owner "/root"))
         captured)
    (cl-letf (((symbol-function 'niwa-telemetry-record)
               (lambda (session event &rest props)
                 (setq captured (list session event props)))))
      (niwa-execution-telemetry-record
       context 'execution-enqueued
       '(:lane read :command-hash "abc123"
         :test-targets ("/private/test.el")
         :resource-report-relative-path "private/report.txt"
         :justification "private justification")
       t))
    (should (eq parent (car captured)))
    (should (eq 'execution-enqueued (cadr captured)))
    (let ((props (nth 2 captured)))
      (should (eq 'btw (plist-get props :conversation-scope)))
      (should (eq 'read (plist-get props :lane)))
      (should (equal "abc123" (plist-get props :command-hash)))
      (dolist (key '(:test-targets :resource-report-relative-path
                     :justification))
        (should-not (plist-member props key))))))

(niwa-deftest niwa-execution-telemetry-record-sandbox-attempt
  (:doc "aggregates attempts once per distinct summary cell")
  (let* ((cell (list nil))
         (context
          (niwa-execution-telemetry-context-create
           :pipeline-summary-cell cell)))
    (niwa-execution-telemetry-record-sandbox-attempt
     context
     '(:sandbox bubblewrap :filesystem workspace-write
       :network isolated :proc fresh :additional-filesystem-read 2)
     t nil)
    (niwa-execution-telemetry-record-sandbox-attempt
     context
     '(:sandbox escalated :filesystem unrestricted
       :network unrestricted :proc host :additional-filesystem-write 1)
     t nil)
    (let ((summary (car cell)))
      (should (= 2 (plist-get summary :attempt-count)))
      (should (= 2 (plist-get summary :started-count)))
      (should (eq 'escalated (plist-get summary :sandbox)))
      (should (eq 'unrestricted (plist-get summary :filesystem)))
      (should (= 2 (plist-get summary :additional-read-count)))
      (should (= 1 (plist-get summary :additional-write-count))))))

(niwa-deftest niwa-execution-telemetry-mark-direct-fallback
  ()
  ,test
  (test)
  :doc "warns and marks only the first direct fallback in a live session"
  (let ((session (niwa-session--create :name "main"))
        (niwa-execution-telemetry--fallback-sessions
         (make-hash-table :test #'eq :weakness 'key))
        warnings first second)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level _buffer-name)
                 (push message warnings))))
      (setq first
            (niwa-execution-telemetry-mark-direct-fallback
             session '(:sandbox unavailable :filesystem unrestricted
                       :network unrestricted))
            second
            (niwa-execution-telemetry-mark-direct-fallback
             session '(:sandbox unavailable :filesystem unrestricted
                       :network unrestricted))))
    (should (plist-get first :first-direct-fallback))
    (should-not (plist-get second :first-direct-fallback))
    (should (= 1 (length warnings))))
  :doc "a session-less helper neither warns nor claims a bucket of its own"
  (let ((session (niwa-session--create :name "main"))
        (niwa-execution-telemetry--fallback-sessions
         (make-hash-table :test #'eq :weakness 'key))
        warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level _buffer-name)
                 (push message warnings))))
      ;; A local-only helper runs with the session deliberately unbound.
      ;; Its facts still record the unconfined run; only the warning and
      ;; the bucket are session-scoped.
      (should
       (plist-get
        (niwa-execution-telemetry-mark-direct-fallback
         nil '(:sandbox unavailable :filesystem unrestricted
               :network unrestricted))
        :first-direct-fallback))
      (should (zerop (length warnings)))
      (should
       (zerop (hash-table-count
               niwa-execution-telemetry--fallback-sessions)))
      ;; The real session still gets its one warning afterwards.
      (should
       (plist-get
        (niwa-execution-telemetry-mark-direct-fallback
         session '(:sandbox unavailable :filesystem unrestricted
                   :network unrestricted))
        :first-direct-fallback))
      (should (= 1 (length warnings))))))

(niwa-deftest niwa-execution-telemetry-sandbox-summary-class
  ()
  ,test
  (test)
  :doc "leaves the default boundary and read-only mounts unclassified"
  (let ((default
         '(:attempt-count 1 :started-count 1 :refused-count 0
           :sandbox bubblewrap :filesystem workspace-write
           :network isolated :proc fresh
           :additional-read-count 0 :additional-write-count 0)))
    (should-not (niwa-execution-telemetry-sandbox-summary-class default))
    (should-not
     (niwa-execution-telemetry-sandbox-summary-class
      (plist-put (copy-sequence default) :additional-read-count 1))))
  :doc "warns only when a child was refused or never started"
  (let ((default
         '(:attempt-count 2 :started-count 2 :refused-count 0
           :sandbox bubblewrap :filesystem workspace-write
           :network isolated :proc fresh
           :additional-read-count 0 :additional-write-count 0)))
    (should
     (eq 'warning
         (niwa-execution-telemetry-sandbox-summary-class
          (plist-put (copy-sequence default) :started-count 1))))
    (should
     (eq 'warning
         (niwa-execution-telemetry-sandbox-summary-class
          (plist-put (copy-sequence default) :refused-count 1)))))
  :doc "notes a wider boundary the session was configured or granted"
  (let ((granted
         '(:attempt-count 2 :started-count 2 :refused-count 0
           :sandbox bubblewrap :filesystem workspace-write
           :network isolated :proc fresh
           :additional-read-count 0 :additional-write-count 0)))
    (dolist (deviation '((:additional-write-count 1)
                         (:network unrestricted)
                         (:filesystem unrestricted)
                         (:proc host)
                         (:sandbox escalated)
                         (:sandbox unavailable)
                         (:sandbox off)))
      (should
       (eq 'note
           (niwa-execution-telemetry-sandbox-summary-class
            (plist-put (copy-sequence granted)
                       (car deviation) (cadr deviation))))))))

(provide 'test-niwa-execution-telemetry)
;;; test-niwa-execution-telemetry.el ends here
