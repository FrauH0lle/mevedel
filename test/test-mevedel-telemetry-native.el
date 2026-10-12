;;; test-mevedel-telemetry-native.el --- Native heartbeat tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise real native waits through telemetry in a separate Emacs, keeping
;; process-wide native hooks out of the ERT runner.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-telemetry--lag-watch ()
  (let ((root (make-temp-file "mevedel-native-lag-" t))
        (output (generate-new-buffer " *native-lag-test*"))
        (fixture (expand-file-name
                  ,fixture
                  (file-name-concat (file-name-directory (locate-library "mevedel"))
                                    "test" "fixtures")))
        process)
    (unwind-protect
        (progn
          (skip-unless (and (eq system-type 'gnu/linux) (featurep 'pgtk) (fboundp 'module-load)
                            module-file-suffix (executable-find "cc")
                            (executable-find "pkg-config")
                            (zerop (call-process "pkg-config" nil nil nil
                                                 "--exists" "gtk+-3.0" "wayland-client"))))
          (setq process
                (make-process
                 :name "mevedel-native-lag-test" :buffer output :noquery t
                 :connection-type 'pipe :sentinel #'ignore
                 :command
                 (list (expand-file-name invocation-name invocation-directory)
                       "-Q" "--batch" "--eval"
                       (prin1-to-string
                        `(setq load-path ',load-path mevedel-test-native-root ,root))
                       "-l" fixture)))
          (with-timeout (20 (ert-fail "Native heartbeat fixture timed out"))
            (while (process-live-p process)
              (accept-process-output process 0.05)))
          (with-current-buffer output
            (ert-info ((buffer-string))
              (should (= 0 (process-exit-status process)))
              (should (string-empty-p (buffer-string))))))
      (when process (delete-process process))
      (kill-buffer output)
      (delete-directory root t)))
  (fixture)
  :doc "native sampling preserves process ownership, stall sensitivity and teardown"
  "telemetry-native.el"
  :doc "native waits isolate their deadline from collection, advice and other waits"
  "native-wait.el"
  :doc "native waits refuse an already advised condition primitive"
  "native-wait-advised.el")

(provide 'test-mevedel-telemetry-native)
;;; test-mevedel-telemetry-native.el ends here
