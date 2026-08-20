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
  (let* ((root (make-temp-file "mevedel-resource-capture-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-telemetry--profiler-session session)
         (mevedel-telemetry--profiler-run-id "run-test"))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (program)
                     (and (equal program "time") "/usr/bin/time")))
                  ((symbol-function 'file-truename) #'identity))
          (let* ((command-text "npx @emacs-eask/cli test ert test/test-*")
                 (command (list "bash" "-lc" command-text))
                 (capture
                  (mevedel-execution-telemetry-prepare-resource-capture
                   session command-text command)))
            (should capture)
            (should
             (equal
              (list "/usr/bin/time" "-v" "-o"
                    (plist-get capture :report) "--"
                    "bash" "-lc" command-text)
              (plist-get capture :command)))
            (should (string-suffix-p
                     "diagnostics/run-test/full-suite-time.txt"
                     (plist-get capture :report)))
            (should-not
             (mevedel-execution-telemetry-prepare-resource-capture
              session command-text command))))
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
         captured)
    (cl-letf (((symbol-function 'mevedel-telemetry-record)
               (lambda (session event &rest props)
                 (setq captured (list session event props)))))
      (mevedel-execution-telemetry-record
       side "exec-1" "tool-1" "/root" 'execution-enqueued
       '(:lane read :command-hash "abc123"
         :test-targets ("/private/test.el")
         :resource-report-relative-path "private/report.txt"
         :justification "private justification")))
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
  (let ((cell (list nil)))
    (mevedel-execution-telemetry-record-sandbox-attempt
     '(:sandbox bubblewrap :filesystem workspace-write
       :network isolated :proc fresh :additional-filesystem-read 2)
     t nil cell)
    (mevedel-execution-telemetry-record-sandbox-attempt
     '(:sandbox escalated :filesystem unrestricted
       :network unrestricted :proc host :additional-filesystem-write 1)
     t nil cell cell)
    (let ((summary (car cell)))
      (should (= 2 (plist-get summary :attempt-count)))
      (should (= 2 (plist-get summary :started-count)))
      (should (eq 'escalated (plist-get summary :sandbox)))
      (should (eq 'unrestricted (plist-get summary :filesystem)))
      (should (= 2 (plist-get summary :additional-read-count)))
      (should (= 1 (plist-get summary :additional-write-count))))))

(mevedel-deftest mevedel-execution-telemetry-mark-direct-fallback
  (:doc "warns and marks only the first direct fallback in a live session")
  (let ((session (mevedel-session--create :name "main"))
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
    (should (= 1 (length warnings)))))

(mevedel-deftest mevedel-execution-telemetry-sandbox-summary-class
  (:doc "classifies only material sandbox deviations as warnings")
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
          (plist-put (copy-sequence default) :started-count 0))))))

(mevedel-deftest mevedel-execution-telemetry-agent-summary-cell
  (:doc "returns only an invocation's direct-child aggregation cell")
  (let* ((cell (list nil))
         (invocation
          (mevedel-agent-invocation--create :sandbox-summary-cell cell)))
    (should (eq cell
                (mevedel-execution-telemetry-agent-summary-cell invocation)))
    (should-not (mevedel-execution-telemetry-agent-summary-cell nil))))

(provide 'test-mevedel-execution-telemetry)
;;; test-mevedel-execution-telemetry.el ends here
