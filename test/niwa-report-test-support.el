;;; niwa-report-test-support.el --- Native report test support -*- lexical-binding: t -*-

;;; Commentary:

;; Read report content through the cockpit and its native section navigation.

;;; Code:

(require 'niwa-cockpit)
(require 'niwa-report)

(defun niwa-report-test-text (report)
  "Open REPORT and return the text reachable through its section navigation."
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *report content test*")) text)
      (unwind-protect
          (progn
            (niwa-cockpit-show-help buffer report)
            (with-current-buffer buffer
              (if (plist-get report :navigator)
                  (dolist (section (plist-get report :sections))
                    (niwa-report-select-section (plist-get section :id))
                    (push (buffer-substring-no-properties (point-min) (point-max)) text))
                (push (buffer-substring-no-properties (point-min) (point-max)) text)))
            (string-join (nreverse text) "\n"))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'niwa-report-test-support)
;;; niwa-report-test-support.el ends here
