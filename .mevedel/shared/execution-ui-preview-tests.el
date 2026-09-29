;;; execution-ui-preview-tests.el --- Preview checks -*- lexical-binding: t -*-

;;; Commentary:
;; ERT checks for the standalone study, run via the project's isolated Eask.

;;; Code:
(require 'ert)
(load (expand-file-name "execution-ui-preview.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest execution-preview-layouts-and-disclosures ()
  (dolist (scenario '(running success failure input))
    (dolist (variant '(a b c))
      (with-temp-buffer
        (mevedel-execution-preview-mode)
        (setq mevedel-execution-preview--scenario scenario
              mevedel-execution-preview--variant variant)
        (mevedel-execution-preview--render)
        (should buffer-read-only)
        (should-not (buffer-modified-p))
        (should (string-match-p "SIMULATED" (buffer-string)))
        (should-not (string-match-p "WriteStdin: poll" (buffer-string)))
        (dolist (kind '(output details history))
          (let ((key (cons variant kind)))
            (mevedel-execution-preview--toggle key)
            (should (member key mevedel-execution-preview--open))))
        (should (string-match-p "WriteStdin: poll" (buffer-string)))
        (should (string-match-p "Working directory: /project/mevedel" (buffer-string)))
        (goto-char (point-min))
        (let ((count 0)
              (tail (plist-get (alist-get scenario mevedel-execution-preview--fixtures) :tail)))
          (while (search-forward tail nil t) (cl-incf count))
          (should (= count 1)))
        (mevedel-execution-preview--toggle (cons variant 'output))
        (should-not (member (cons variant 'output) mevedel-execution-preview--open))))))

(ert-deftest execution-preview-buttons-and-entry ()
  (save-window-excursion
    (let ((existing (get-buffer "*Execution UI variants*")))
      (should-not existing)
      (unwind-protect
          (progn
            (mevedel-execution-preview)
            (with-current-buffer "*Execution UI variants*"
              (should (eq major-mode 'mevedel-execution-preview-mode))
              (goto-char (point-min))
              (search-forward "Failure")
              (button-activate (button-at (1- (point))))
              (should (eq mevedel-execution-preview--scenario 'failure))
              (goto-char (point-min))
              (search-forward "Show result")
              (button-activate (button-at (1- (point))))
              (should (member '(c . output) mevedel-execution-preview--open))
              (should (string-match-p "Expected 1 output block" (buffer-string)))
              (goto-char (point-min))
              (search-forward "Compare all")
              (forward-button 2)
              (button-activate (button-at (point)))
              (should (eq mevedel-execution-preview--variant 'b))))
        (when-let* ((buffer (get-buffer "*Execution UI variants*")))
          (kill-buffer buffer))))))

;;; execution-ui-preview-tests.el ends here
