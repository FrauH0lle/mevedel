;;; niwa-memory-list-test-support.el --- Async cockpit fixtures -*- lexical-binding: t -*-

;;; Commentary:
;; Wait for native display and checked-recovery owners, not arbitrary sleeps.

;;; Code:
(require 'niwa-memory-list)

(defun niwa-test-memory-list-await (&optional buffer)
  "Wait for BUFFER's native display, topic observation and recovery to settle."
  (with-current-buffer (or buffer (current-buffer))
    (let ((workspace (niwa-cockpit-context-workspace niwa-cockpit--context)))
      (with-timeout (45 (ert-fail "Memory cockpit did not finish its observation"))
        (while (or niwa-memory-list--job
                   niwa-memory-list--memories-job
                   niwa-memory-list--recovery
                   (niwa-memory-decision-recovering-p workspace))
          (accept-process-output nil .01)))
      (should-not niwa-memory-list--error))))

(defun niwa-test-memory-list-await-memories (&optional buffer)
  "Wait only for BUFFER's topic observation, leaving history scans running."
  (with-current-buffer (or buffer (current-buffer))
    (with-timeout (45 (ert-fail "Memory topics were not observed"))
      (while niwa-memory-list--memories-job (accept-process-output nil .01)))
    (should (eq 'current niwa-memory-list--memories-state))))

(provide 'niwa-memory-list-test-support)
;;; niwa-memory-list-test-support.el ends here
