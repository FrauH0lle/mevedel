;;; mevedel-memory-list-test-support.el --- Async cockpit fixtures -*- lexical-binding: t -*-

;;; Commentary:
;; Wait for native display and checked-recovery owners, not arbitrary sleeps.

;;; Code:
(require 'mevedel-memory-list)

(defun mevedel-test-memory-list-await (&optional buffer)
  "Wait for BUFFER's native display and scheduled recovery to settle."
  (with-current-buffer (or buffer (current-buffer))
    (let ((workspace (mevedel-cockpit-context-workspace mevedel-cockpit--context)))
      (with-timeout (45 (ert-fail "Memory cockpit did not finish its observation"))
        (while (or mevedel-memory-list--job
                   mevedel-memory-list--recovery
                   (mevedel-memory-decision-recovering-p workspace))
          (accept-process-output nil .01)))
      (should-not mevedel-memory-list--error))))

(provide 'mevedel-memory-list-test-support)
;;; mevedel-memory-list-test-support.el ends here
