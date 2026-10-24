;;; test-mevedel-graphical-check.el --- Graphical runner contracts -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise launch mechanics in batch.  The graphical-check command itself
;; covers its real frame preflight, view capture and graphical ERT execution.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'graphical-check
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "graphical-check"))

(mevedel-deftest mevedel-graphical-check--options ()
  ,test
  (test)
  :doc "accepts explicit output, repeated grammar roots and a positive deadline"
  (let ((directory (make-temp-file "graphical-options-" t)))
    (unwind-protect
        (should (equal (list (expand-file-name "capture")
                            (list directory directory) 3)
                       (mevedel-graphical-check--options
                        (list "--" "--output" "capture" "--grammar-dir" directory
                              "--grammar-dir" directory "--timeout" "3"))))
      (delete-directory directory t)))
  :doc "rejects unknown options, missing arguments and unsafe deadlines"
  (dolist (args '(("--unknown" "x") ("--output") ("--timeout" "0")
                  ("--timeout" "-1") ("--timeout" "5oops")
                  ("--grammar-dir" "/does-not-exist/graphical-grammar")))
    (should-error (mevedel-graphical-check--options args))))

(mevedel-deftest mevedel-graphical-check--environment ()
  ,test
  (test)
  :doc "privatizes HOME/XDG while preserving the absolute compositor socket"
  (let* ((temporary (make-temp-file "graphical-environment-" t))
         (home (file-name-concat temporary "home"))
         (socket (file-name-concat temporary "wayland-test"))
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (make-directory home)
          (with-temp-file socket (insert "socket placeholder"))
          (setenv "WAYLAND_DISPLAY" "wayland-test")
          (setenv "XDG_RUNTIME_DIR" temporary)
          (setenv "DBUS_SESSION_BUS_ADDRESS" "do-not-use-host-bus")
          (let ((environment (mevedel-graphical-check--environment home)))
            (should (equal "wayland-test" (getenv "WAYLAND_DISPLAY")))
            (let ((process-environment environment))
              (should (equal home (getenv "HOME")))
              (should (equal socket (getenv "WAYLAND_DISPLAY")))
              (should-not (getenv "DBUS_SESSION_BUS_ADDRESS"))
              (dolist (name '("XDG_CACHE_HOME" "XDG_CONFIG_HOME" "XDG_DATA_HOME"
                              "XDG_STATE_HOME" "XDG_RUNTIME_DIR"))
                (should (file-in-directory-p (getenv name) home))
                (should (= #o700 (file-modes (getenv name))))))))
      (delete-directory temporary t)))
  :doc "fails clearly without a display or with an unreachable Wayland socket"
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "DISPLAY" nil)
    (setenv "WAYLAND_DISPLAY" nil)
    (should (string-match-p
             "No graphical display"
             (error-message-string
              (should-error (mevedel-graphical-check--environment "/unused")))))
    (setenv "WAYLAND_DISPLAY" "/does-not-exist/wayland-test")
    (should (string-match-p
             "Wayland socket is unavailable"
             (error-message-string
              (should-error (mevedel-graphical-check--environment "/unused")))))))

(mevedel-deftest mevedel-graphical-check--wait
  (:doc "preserves success, failure and a bounded timeout from real subprocesses")
  (dolist (case '(("(kill-emacs 0)" nil)
                  ("(kill-emacs 7)" "Graphical Emacs failed (7)")
                  ("(sleep-for 10)" "timed out")))
    (let ((process (make-process
                    :name "graphical-wait-test" :noquery t
                    :buffer nil :connection-type 'pipe
                    :sentinel #'ignore
                    :command (list (expand-file-name invocation-name invocation-directory)
                                   "-Q" "--batch" "--eval" (car case)))))
      (unwind-protect
          (if (cadr case)
              (should (string-match-p
                       (regexp-quote (cadr case))
                       (error-message-string
                        (should-error (mevedel-graphical-check--wait process 2)))))
            (should-not (mevedel-graphical-check--wait process 2)))
        (when (process-live-p process) (delete-process process)))
      (should-not (process-live-p process)))))

(mevedel-deftest mevedel-graphical-check--preflight
  (:doc "does not let a batch frame masquerade as graphical coverage")
  (when noninteractive
    (should (string-match-p
             "real graphical Emacs"
             (error-message-string
              (should-error (mevedel-graphical-check--preflight)))))))

(provide 'test-mevedel-graphical-check)
;;; test-mevedel-graphical-check.el ends here
