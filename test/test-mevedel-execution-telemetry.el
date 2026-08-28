;;; test-mevedel-execution-telemetry.el -- Tests for execution telemetry -*- lexical-binding: t -*-

;;; Commentary:

;; Tests safe execution telemetry projection, sandbox summaries, and profiler
;; resource capture.

;;; Code:

(require 'cl-lib)
(require 'mevedel-agents)
(require 'mevedel-execution-telemetry)
(require 'mevedel-structs)
(require 'mevedel-telemetry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-execution-test-helpers"))

(mevedel-deftest mevedel-execution-telemetry-command-properties
  (:doc "recognizes Eask invocations and bounded repository-local targets")
  (progn
    (should (eq 'full
                (plist-get
                 (mevedel-execution-telemetry-command-properties
                  "eask test ert")
                 :test-scope)))
    (should
     (equal '("test/test-one.el" "test/nested/test-two.el")
            (plist-get
             (mevedel-execution-telemetry-command-properties
              "eask test ert test/test-one.el test/nested/test-two.el")
             :test-targets)))
    (should-not
     (mevedel-execution-telemetry-command-properties
      "printf 'eask test ert'"))))

(mevedel-deftest mevedel-execution-telemetry-context-create
  (:doc "captures one execution's summary owners behind an opaque context")
  (let* ((pipeline-cell (list nil))
         (agent-cell (list nil))
         (invocation
          (mevedel-agent-invocation--create :sandbox-summary-cell agent-cell))
         (context
          (mevedel-execution-telemetry-context-create
           :owner "/root" :invocation invocation
           :pipeline-summary-cell pipeline-cell)))
    (should context)
    (mevedel-execution-telemetry-record-sandbox-attempt
     context '(:sandbox bubblewrap :filesystem workspace-write
               :network isolated :proc fresh)
     t nil)
    (should (= 1 (plist-get (car pipeline-cell) :attempt-count)))
    (should (= 1 (plist-get (car agent-cell) :attempt-count)))))

(mevedel-deftest mevedel-execution-telemetry-context-properties
  (:doc "reports a claimed resource path and its eventual byte count")
  (let* ((root (make-temp-file "mevedel-context-properties-" t))
         (report (file-name-concat root "diagnostics" "run" "report.txt"))
         (context (mevedel-execution-telemetry-context-create)))
    (unwind-protect
        (progn
          (setf
           (mevedel-execution-telemetry--context-resource-report-path context)
           report)
          (let ((properties
                 (mevedel-execution-telemetry-context-properties context)))
            (should (plist-get properties :native-resource-capture))
            (should-not
             (plist-member properties :native-resource-report-bytes)))
          (make-directory (file-name-directory report) t)
          (write-region "facts" nil report nil 'silent)
          (should (= 5
                     (plist-get
                      (mevedel-execution-telemetry-context-properties context)
                      :native-resource-report-bytes))))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-telemetry-context-summary
  (:doc "returns an isolated copy of the current sandbox summary")
  (let ((context (mevedel-execution-telemetry-context-create)))
    (mevedel-execution-telemetry-record-sandbox-attempt
     context '(:sandbox bubblewrap) t nil)
    (let ((summary (mevedel-execution-telemetry-context-summary context)))
      (plist-put summary :sandbox 'changed)
      (should
       (eq 'bubblewrap
           (plist-get
            (mevedel-execution-telemetry-context-summary context)
            :sandbox))))))

(mevedel-deftest mevedel-execution-telemetry-finish
  (:doc "forwards terminal properties only to the context-owned span")
  (let* ((context (mevedel-execution-telemetry-context-create))
         (span '(:span-id "span"))
         captured)
    (setf (mevedel-execution-telemetry--context-span context) span)
    (cl-letf (((symbol-function 'mevedel-telemetry-finish)
               (lambda (seen &rest properties)
                 (setq captured (cons seen properties)))))
      (mevedel-execution-telemetry-finish context :outcome 'success))
    (should (eq span (car captured)))
    (should (eq 'success (plist-get (cdr captured) :outcome)))))

(mevedel-deftest mevedel-execution-telemetry--cache-identity
  (:doc "hashes cache roots deterministically without exposing their values")
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "HOME" "/secret/one")
    (let ((first (mevedel-execution-telemetry--cache-identity)))
      (should (= 64 (length first)))
      (should-not (string-match-p "secret" first))
      (should (equal first (mevedel-execution-telemetry--cache-identity)))
      (setenv "HOME" "/secret/two")
      (should-not
       (equal first (mevedel-execution-telemetry--cache-identity))))))

(mevedel-deftest mevedel-execution-telemetry-prepare-resource-capture
  (:doc "wraps at most one profiled full suite with GNU time")
  (skip-unless (file-executable-p "/usr/bin/time"))
  (let* ((root (make-temp-file "mevedel-resource-capture-" t))
         (session (test-mevedel-execution--session root))
         (context
          (mevedel-execution-telemetry-context-create :session session))
         (mevedel-execution-telemetry--resource-capture-claims
          (make-hash-table :test #'equal))
         (mevedel-telemetry--profiler-session session)
         (mevedel-telemetry--profiler-run-id "run-test"))
    (unwind-protect
        (let* ((command-text "npx @emacs-eask/cli test ert test/test-*")
               (command (list "bash" "-lc" command-text))
               (wrapped
                (mevedel-execution-telemetry-prepare-resource-capture
                 context command-text command))
               (properties
                (mevedel-execution-telemetry-context-properties context)))
          (should wrapped)
          (should
           (equal
            (append (list "/usr/bin/time" "-v" "-o"
                          (mevedel-execution-telemetry--context-resource-report-path
                           context)
                          "--")
                    command)
            wrapped))
          (should (string-suffix-p
                   "diagnostics/run-test/full-suite-time.txt"
                   (plist-get properties :resource-report-relative-path)))
          (should-not
           (mevedel-execution-telemetry-prepare-resource-capture
            context command-text command)))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-telemetry-safe-facts
  (:doc "keeps confinement dimensions while excluding private reasons")
  (let ((facts
         (mevedel-execution-telemetry-safe-facts
          '(:sandbox bubblewrap :filesystem workspace-write
            :network isolated :proc fresh :protected-paths 4
            :additional-filesystem-read 2
            :additional-filesystem-write 1
            :reason "private launcher output"))))
    (should (eq 'bubblewrap (plist-get facts :sandbox)))
    (should (= 4 (plist-get facts :protected-path-count)))
    (should (= 2 (plist-get facts :additional-read-count)))
    (should-not (plist-member facts :reason))))

(mevedel-deftest mevedel-execution-telemetry-record
  (:doc "uses the audit target and removes path-bearing properties")
  (let* ((parent (mevedel-session--create :name "parent"))
         (side (mevedel-session--create :name "side" :audit-session parent))
         (context
          (mevedel-execution-telemetry-context-create
           :session side :execution-id "exec-1" :tool-use-id "tool-1"
           :owner "/root"))
         captured)
    (cl-letf (((symbol-function 'mevedel-telemetry-record)
               (lambda (session event &rest props)
                 (setq captured (list session event props)))))
      (mevedel-execution-telemetry-record
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

(mevedel-deftest mevedel-execution-telemetry-record-sandbox-attempt
  (:doc "aggregates attempts once per distinct summary cell")
  (let* ((cell (list nil))
         (context
          (mevedel-execution-telemetry-context-create
           :pipeline-summary-cell cell)))
    (mevedel-execution-telemetry-record-sandbox-attempt
     context
     '(:sandbox bubblewrap :filesystem workspace-write
       :network isolated :proc fresh :additional-filesystem-read 2)
     t nil)
    (mevedel-execution-telemetry-record-sandbox-attempt
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

(mevedel-deftest mevedel-execution-telemetry-mark-direct-fallback
  ()
  ,test
  (test)
  :doc "warns and marks only the first direct fallback in a live session"
  (let ((session (mevedel-session--create :name "main"))
        (mevedel-execution-telemetry--fallback-sessions
         (make-hash-table :test #'eq :weakness 'key))
        warnings first second)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level _buffer-name)
                 (push message warnings))))
      (setq first
            (mevedel-execution-telemetry-mark-direct-fallback
             session '(:sandbox unavailable :filesystem unrestricted
                       :network unrestricted))
            second
            (mevedel-execution-telemetry-mark-direct-fallback
             session '(:sandbox unavailable :filesystem unrestricted
                       :network unrestricted))))
    (should (plist-get first :first-direct-fallback))
    (should-not (plist-get second :first-direct-fallback))
    (should (= 1 (length warnings))))
  :doc "a session-less helper neither warns nor claims a bucket of its own"
  (let ((session (mevedel-session--create :name "main"))
        (mevedel-execution-telemetry--fallback-sessions
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
        (mevedel-execution-telemetry-mark-direct-fallback
         nil '(:sandbox unavailable :filesystem unrestricted
               :network unrestricted))
        :first-direct-fallback))
      (should (zerop (length warnings)))
      (should
       (zerop (hash-table-count
               mevedel-execution-telemetry--fallback-sessions)))
      ;; The real session still gets its one warning afterwards.
      (should
       (plist-get
        (mevedel-execution-telemetry-mark-direct-fallback
         session '(:sandbox unavailable :filesystem unrestricted
                   :network unrestricted))
        :first-direct-fallback))
      (should (= 1 (length warnings))))))

(mevedel-deftest mevedel-execution-telemetry-sandbox-summary-class
  ()
  ,test
  (test)
  :doc "classifies only material sandbox deviations as warnings"
  (let ((default
         '(:attempt-count 1 :started-count 1 :refused-count 0
           :sandbox bubblewrap :filesystem workspace-write
           :network isolated :proc fresh
           :additional-read-count 0 :additional-write-count 0)))
    (should-not (mevedel-execution-telemetry-sandbox-summary-class default))
    (should-not
     (mevedel-execution-telemetry-sandbox-summary-class
      (plist-put (copy-sequence default) :additional-read-count 1)))
    (should
     (eq 'warning
         (mevedel-execution-telemetry-sandbox-summary-class
          (plist-put (copy-sequence default) :started-count 0)))))
  :doc "notes an unconfined run whose only deviation is the sandbox itself"
  (let ((unconfined
         '(:attempt-count 2 :started-count 2 :refused-count 0
           :sandbox unavailable :filesystem unrestricted
           :network unrestricted :proc nil
           :additional-read-count 0 :additional-write-count 0)))
    (should
     (eq 'note
         (mevedel-execution-telemetry-sandbox-summary-class unconfined)))
    (should
     (eq 'note
         (mevedel-execution-telemetry-sandbox-summary-class
          (plist-put (copy-sequence unconfined) :sandbox 'off))))
    (should
     (eq 'warning
         (mevedel-execution-telemetry-sandbox-summary-class
          (plist-put (copy-sequence unconfined) :started-count 1))))
    (should
     (eq 'warning
         (mevedel-execution-telemetry-sandbox-summary-class
          (plist-put (copy-sequence unconfined) :refused-count 1))))
    (should
     (eq 'warning
         (mevedel-execution-telemetry-sandbox-summary-class
          (plist-put (copy-sequence unconfined)
                     :additional-write-count 1))))
    (should
     (eq 'warning
         (mevedel-execution-telemetry-sandbox-summary-class
          (plist-put (copy-sequence unconfined) :sandbox 'escalated))))))

(provide 'test-mevedel-execution-telemetry)
;;; test-mevedel-execution-telemetry.el ends here
