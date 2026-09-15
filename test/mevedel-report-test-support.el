;;; mevedel-report-test-support.el --- Native report test support -*- lexical-binding: t -*-

;;; Commentary:

;; Read report content through the cockpit and its native section navigation.

;;; Code:

(require 'mevedel-cockpit)
(require 'mevedel-report)

(defun mevedel-report-test-text (report)
  "Open REPORT and return the text reachable through its section navigation."
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *report content test*")) text)
      (unwind-protect
          (progn
            (mevedel-cockpit-show-help buffer report)
            (with-current-buffer buffer
              (if (plist-get report :navigator)
                  (dolist (section (plist-get report :sections))
                    (mevedel-report-select-section (plist-get section :id))
                    (push (buffer-substring-no-properties (point-min) (point-max)) text))
                (push (buffer-substring-no-properties (point-min) (point-max)) text)))
            (string-join (nreverse text) "\n"))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'mevedel-report-test-support)
;;; mevedel-report-test-support.el ends here
