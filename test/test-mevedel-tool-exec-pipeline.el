;;; test-mevedel-tool-exec-pipeline.el --- Pipeline status tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise final pipeline reporting through the registered Bash tool.

;;; Code:

(require 'mevedel-tool-exec)
(require 'mevedel-tools)
(require 'mevedel-pipeline)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-tool-exec--pipeline-facts ()
  ,test
  (test)
  :doc "classifies every stage, retaining expected outcomes and downstream SIGPIPE"
  (dolist (case '(("false | cat" (1 0) (1))
                  ("grep -q needle missing | cat" (2 0) (1))
                  ("grep -q needle /dev/null | cat" (1 0) nil)
                  ("diff a b | head" (1 0) nil)
                  ("test -e missing | cat" (1 0) nil)
                  ("yes | head" (141 0) nil)
                  ("false | yes | head" (7 141 0) (1))
                  ("true | false" (0 141) (2))))
    (should (equal (nth 2 case)
                   (plist-get (mevedel-tool-exec--pipeline-facts
                               (car case) (cadr case))
                              :pipeline-failed-stages)))))

(mevedel-deftest mevedel-tool-exec--pipeline-source
  (:quiet t)
  ,test
  (test)
  :doc "registered Bash preserves raw exit and control flow while exposing pipeline failure"
  (let* ((root (make-temp-file "mevedel-bash-pipeline-" t))
         (session (mevedel-skills-test--make-session "pipelines" root)))
    (setf (mevedel-session-permission-mode session) 'full-auto
          (mevedel-session-sandbox-mode session) 'off)
    (unwind-protect
        (with-temp-buffer
          (setq default-directory (file-name-as-directory root))
          (should (zerop (call-process "git" nil nil nil "init" "-q" root)))
          (setq-local mevedel--session session
                      mevedel--workspace (mevedel-session-workspace session)
                      mevedel--current-request
                      (mevedel-request--create :session session :ephemeral-p t))
          (dolist (case '(("(printf '57 tests failed\\n'; exit 7) 2>&1 | tail -2" 0 failure (7 0))
                          ("false | cat" 0 failure (1 0))
                          ("false | cat" 0 failure (1 0) t)
                          ("printf '%s\\n' \"$x\" | tail -1" 0 success (0 0))
                          ("set -- sample; (printf '%s\\n' \"$@\"; exit 7) | tail -1" 0 failure (7 0))
                          ("for f in *.el; do printf '%s\\n' \"${f}\"; false; done | tail -1" 0 failure (1 0))
                          ("for f in *.el; do printf '%s\\n' \"$f\"; done | tail -1" 0 success (0 0))
                          ("for f in x; do trap 'exit $((40 + PIPESTATUS[0]))' EXIT; done; false | cat" 41 failure nil)
                          ("eval 'trap \"exit 42\" EXIT'; false | cat" 42 failure nil)
                          ("git status --porcelain | head" 0 success (0 0))
                          ("yes | head -1" 0 success (141 0))
                          ("yes | head -1 && printf continued" 0 success nil)
                          ("(exit 7) | yes | head -1" 0 failure (7 141 0))
                          ("trap 'printf user-exit-trap' EXIT; false | cat" 0 success nil)
                          ("trap 'exit 42' EXIT; true | cat" 42 failure nil)
                          ("test -v a[x]; false | cat" 42 failure nil)
                          ("[ -v a[x] ]; false | cat" 42 failure nil)
                          ("trap 'exit $((40 + PIPESTATUS[0]))' EXIT; false | cat" 41 failure nil)
                          ("2>/dev/null trap '(( PIPESTATUS[0] )) && exit 41' EXIT; false | cat"
                           41 failure nil)
                          ("grep -q absent /dev/null | head" 0 success (1 0))
                          ("printf foo | grep absent" 1 no-match (0 1))
                          ("printf a > a; printf b > b; diff a b | head" 0 success (1 0))
                          ("export IFS=,; false | cat" 0 failure (1 0))
                          ("false | cat || grep absent /dev/null | cat" 0 success (1 0))
                          ("grep absent /dev/null | cat || false | cat" 0 success (1 0))
                          ("false | cat; exit 0" 0 success nil)
                          ("false | cat; exec true" 0 success nil)
                          ("exec > redirected; false | cat" 0 success nil)
                          ("printf '\\036'" 0 success nil)))
            ;; Arithmetic subscript evaluation can install a caller trap from
            ;; environment data even when the command has no dollar syntax.
            (let ((process-environment
                   (cons "x=a[${ trap \"exit 42\" EXIT; printf 0; }]"
                         process-environment))
                  result)
              (mevedel-pipeline-run-tool-outcome
               (mevedel-tool-ensure "Bash")
               (lambda (value) (setq result value))
               (append (list :command (car case) :description "Test pipeline status")
                       (when (nth 4 case) '(:tty t))))
              (with-timeout (10 (ert-fail "Bash pipeline did not settle"))
                (while (null result) (accept-process-output nil 0.02)))
              (let ((facts (plist-get result :render-data))
                    (text (plist-get result :result)))
                (ert-info ((format "Command: %s; result: %S" (car case) result))
                  (should (equal (nth 1 case) (plist-get facts :exit-code))))
                (ert-info ((format "Command: %s; facts: %S" (car case) facts))
                  (should (eq (nth 2 case) (plist-get facts :outcome))))
                (should (equal (nth 3 case) (plist-get facts :pipeline-exit-codes)))
                (should-not (string-search "\036pipeline_" text))
                (when (string-search "set -- sample" (car case))
                  (should (string-prefix-p "sample\n" text)))
                (when (string-search "for f in *.el" (car case))
                  (should (string-prefix-p "*.el\n" text)))
                (when (plist-get facts :pipeline-failed-stages)
                  (should (eq 'error (plist-get result :status)))
                  (should (string-search "Pipeline failed at stage(s) 1" text)))
                (when (string-search "continued" (car case))
                  (should (string-prefix-p "y\ncontinued" text)))
                (when (string-search "printf user-exit-trap" (car case))
                  (should (string-prefix-p "user-exit-trap" text)))
                (when (equal "printf '\\036'" (car case))
                  (should (string-prefix-p "\036" text)))
                (when (string-search "exec > redirected" (car case))
                  (should (zerop (file-attribute-size
                                 (file-attributes (file-name-concat root "redirected")))))))))
          (let (started settled)
            (mevedel-pipeline-run-tool-outcome
             (mevedel-tool-ensure "Bash")
             (lambda (value) (setq started value))
             '(:command "bash -c 'until test -e pipeline-ready; do sleep 0.02; done'; false | cat"
                        :yield_time_ms 250
                        :description "Yield pipeline status"))
            (with-timeout (10 (ert-fail "Pipeline did not yield"))
              (while (null started) (accept-process-output nil 0.02)))
            (let ((id (plist-get (plist-get started :render-data) :execution-id)))
              (should (stringp id))
              (write-region "" nil (file-name-concat root "pipeline-ready") nil 'silent)
              (mevedel-pipeline-run-tool-outcome
               (mevedel-tool-ensure "WriteStdin")
               (lambda (value) (setq settled value))
               (list :execution_id id :yield_time_ms 5000))
              (with-timeout (10 (ert-fail "Yielded pipeline did not settle"))
                (while (null settled) (accept-process-output nil 0.02)))
              (should (eq 'failure (plist-get (plist-get settled :render-data) :outcome)))
              (should (equal '(1 0) (plist-get (plist-get settled :render-data)
                                             :pipeline-exit-codes)))))
          ;; Disable only the producer as a discriminating negative control:
          ;; the same public tool path must reproduce the old false success.
          (let (result)
            (cl-letf (((symbol-function 'mevedel-tool-exec--pipeline-source)
                       (lambda (source _marker &optional _original-source) source)))
              (mevedel-pipeline-run-tool-outcome
               (mevedel-tool-ensure "Bash")
               (lambda (value) (setq result value))
               '(:command "for f in *.el; do printf '%s\\n' \"$f\"; false; done | tail -1"
                          :description "Negative pipeline control"))
              (with-timeout (10 (ert-fail "Negative control did not settle"))
                (while (null result) (accept-process-output nil 0.02))))
            (should (eq 'success (plist-get (plist-get result :render-data) :outcome)))
            (should-not (plist-get (plist-get result :render-data) :pipeline-exit-codes))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t))))

(provide 'test-mevedel-tool-exec-pipeline)
;;; test-mevedel-tool-exec-pipeline.el ends here
