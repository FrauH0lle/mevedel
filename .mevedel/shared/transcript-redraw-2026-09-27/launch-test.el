;;; launch-test.el --- Eask-owned measurement launcher -*- lexical-binding: t -*-
(require 'helpers (expand-file-name "test/helpers.el"))
(mevedel-deftest transcript-redraw-probe ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "PROBE_EMACS" (expand-file-name invocation-name invocation-directory))
    (setenv "PROBE_LOAD_PATH"
            (prin1-to-string (list 'setq 'load-path (list 'quote load-path))))
    (with-temp-buffer
      (let ((status (call-process
                     "python3" nil t nil
                     (expand-file-name ".mevedel/shared/transcript-redraw-2026-09-27/runner.py"))))
        (ert-info ((buffer-string)) (should (= 0 status)))))))
