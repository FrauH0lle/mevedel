;;; test-mevedel-execution.el --- Tests for managed child execution -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the bounded one-shot process boundary used by Bash, batch Eval, and
;; native external helpers.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution)
(require 'mevedel-sandbox)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-execution--workspace (root)
  "Return a test workspace rooted at ROOT."
  (mevedel-workspace--create
   :type 'test :id root :root root :name "execution"
   :file-cache (mevedel-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil :total-bytes 0)))

(defun test-mevedel-execution--process-gone-p (pid)
  "Return non-nil when PID no longer names a live process."
  (null (process-attributes pid)))


;;
;;; One-shot execution

(mevedel-deftest mevedel-execution-start-one-shot ()
  ,test
  (test)
  :doc "settles an exited child even when Emacs does not deliver its sentinel"
  (let ((original-make-process (symbol-function 'make-process))
        (mevedel-sandbox-mode 'off)
        done result)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (apply original-make-process
                        (plist-put args :sentinel #'ignore)))))
      (should-not
       (mevedel-execution-start-one-shot
        (lambda (child-result)
          (setq result child-result
                done t))
        :name "mevedel-test-missed-sentinel"
        :command '("sh" "-c" "printf recovered; exit 7")
        :workdir temporary-file-directory
        :writable-roots (list temporary-file-directory)))
      (with-timeout (2 (error "Missed sentinel was not recovered"))
        (while (not done)
          (accept-process-output nil 0.05)))
      (should (= 7 (plist-get result :exit-code)))
      (should (equal "recovered" (plist-get result :output))))))

(mevedel-deftest mevedel-execution-run-one-shot ()
  ,test
  (test)
  :doc "returns complete output and structured terminal facts"
  (let ((mevedel-sandbox-mode 'off))
    (let ((result
           (mevedel-execution-run-one-shot
            :name "mevedel-test-success"
            :command '("sh" "-c" "printf 'hello'; printf ' world' >&2")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (= 0 (plist-get result :exit-code)))
      (should (equal "hello world" (plist-get result :output)))
      (should-not (plist-get result :timed-out-p))
      (should-not (plist-get result :output-limit-p))
      (should (numberp (plist-get result :wall-time-seconds)))
      (should-not (plist-member result :process))))
  :doc "counts raw output bytes independently of decoded characters"
  (let ((mevedel-sandbox-mode 'off))
    (let ((result
           (mevedel-execution-run-one-shot
            :name "mevedel-test-byte-count"
            :command '("sh" "-c" "printf '\\303\\244'")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (equal (string #xe4) (plist-get result :output)))
      (should (= 2 (plist-get result :output-bytes)))))
  :doc "reports spawn failure without exposing a process"
  (let ((mevedel-sandbox-mode 'off))
    (let ((result
           (mevedel-execution-run-one-shot
            :name "mevedel-test-failed-spawn"
            :command '("/definitely/missing/mevedel-executable")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (= -1 (plist-get result :exit-code)))
      (should (plist-get result :error))
      (should-not (plist-member result :process))))
  :doc "applies stable child defaults while allowing command overrides"
  (let ((mevedel-sandbox-mode 'off))
    (let ((defaults
           (mevedel-execution-run-one-shot
            :name "mevedel-test-environment"
            :command
            '("sh" "-c"
              "printf '%s' \"$NO_COLOR|$TERM|$LC_ALL|$LANG|$COLORTERM|$PAGER|$GIT_PAGER|$GH_PAGER|$MEVEDEL_EXECUTION\"")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory)))
          (override
           (mevedel-execution-run-one-shot
            :name "mevedel-test-environment-override"
            :command '("sh" "-c" "PAGER=less; printf '%s' \"$PAGER\"")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (equal "1|dumb|C.UTF-8|C.UTF-8||cat|cat|cat|1"
                     (plist-get defaults :output)))
      (should (equal "less" (plist-get override :output)))))
  :doc "terminates a timed-out command and its process group"
  (let* ((root (make-temp-file "mevedel-execution-timeout-" t))
         (pid-file (file-name-concat root "child.pid"))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         result pid)
    (unwind-protect
        (progn
          (setq result
                (mevedel-execution-run-one-shot
                 :name "mevedel-test-timeout"
                 :command
                 (list "sh" "-c"
                       "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; wait"
                       "mevedel-test-timeout" pid-file)
                 :workdir root
                 :writable-roots (list root)
                 :timeout 0.1))
          (should (plist-get result :timed-out-p))
          (should (file-readable-p pid-file))
          (setq pid
                (string-to-number
                 (string-trim
                  (with-temp-buffer
                    (insert-file-contents pid-file)
                    (buffer-string)))))
          (with-timeout (1 (error "Descendant process survived timeout"))
            (while (not (test-mevedel-execution--process-gone-p pid))
              (accept-process-output nil 0.02))))
      (delete-directory root t)))
  :doc "spools large output without retaining a process buffer"
  (let ((mevedel-sandbox-mode 'off)
        (mevedel-execution-output-limit (* 2 1024 1024)))
    (let ((result
           (mevedel-execution-run-one-shot
            :name "mevedel-test-large-output"
            :command
            '("sh" "-c"
              "printf head; head -c 1048576 /dev/zero | tr '\\0' x; printf tail")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (= 0 (plist-get result :exit-code)))
      (should (= (+ 8 1048576) (length (plist-get result :output))))
      (should (string-prefix-p "head" (plist-get result :output)))
      (should (string-suffix-p "tail" (plist-get result :output)))
      (should-not (get-buffer " *mevedel-test-large-output*"))))
  :doc "enforces the output spool cap and reports the limit"
  (let ((mevedel-sandbox-mode 'off)
        (mevedel-execution-output-limit 4096)
        (mevedel-execution--child-kill-delay 0.05))
    (let ((result
           (mevedel-execution-run-one-shot
            :name "mevedel-test-output-cap"
            :command
            '("sh" "-c" "head -c 100000 /dev/zero | tr '\\0' x")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (plist-get result :output-limit-p))
      (should (= 4096 (string-bytes (plist-get result :output))))))
  :doc "allows output exactly equal to the configured spool cap"
  (let ((mevedel-sandbox-mode 'off)
        (mevedel-execution-output-limit 4096))
    (let ((result
           (mevedel-execution-run-one-shot
            :name "mevedel-test-exact-output-cap"
            :command '("sh" "-c" "head -c 4096 /dev/zero | tr '\\0' x")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (= 0 (plist-get result :exit-code)))
      (should-not (plist-get result :output-limit-p))
      (should (= 4096 (plist-get result :output-bytes))))))


;;
;;; External helpers

(mevedel-deftest mevedel-execution-start-helper ()
  ,test
  (test)
  :doc "cleans its private working directory after asynchronous settlement"
  (let ((mevedel-sandbox-mode 'off)
        result done)
    (mevedel-execution-start-helper
     (lambda (child-result)
       (setq result child-result
             done t))
     "mevedel-test-helper-async" '("pwd") nil nil)
    (with-timeout (5 (error "Timed out"))
      (while (not done)
        (accept-process-output nil 0.05)))
    (let ((scratch (string-trim (plist-get result :output))))
      (should (string-match-p "mevedel-helper-" scratch))
      (should-not (file-exists-p scratch))))
  :doc "declares read paths, artifact roots, and a private writable scratch root"
  (let* ((root (make-temp-file "mevedel-helper-profile-" t))
         (input (file-name-concat root "input"))
         captured)
    (unwind-protect
        (progn
          (with-temp-file input (insert "input"))
          (cl-letf (((symbol-function 'mevedel-execution-start-one-shot)
                     (lambda (callback &rest args)
                       (setq captured args)
                       (funcall callback
                                '(:exit-code 0 :output ""
                                  :timed-out-p nil)))))
            (mevedel-execution-start-helper
             #'ignore "mevedel-test-helper-profile" '("true") (list input)
             (list root)))
          (should (member (file-name-as-directory root)
                          (plist-get captured :writable-roots)))
          (should
           (equal (list :file-system
                        (list (list :path input :access 'read)))
                  (plist-get captured :additional-permissions)))
          (should-not (file-exists-p (plist-get captured :workdir))))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-run-helper ()
  ,test
  (test)
  :doc "preserves helper output, artifact writes, and confinement facts"
  (let* ((root (make-temp-file "mevedel-helper-run-" t))
         (input (file-name-concat root "input"))
         (output (file-name-concat root "output"))
         (session (mevedel-session-create
                   "main" (test-mevedel-execution--workspace root)))
         (mevedel-sandbox-mode 'off))
    (unwind-protect
        (progn
          (with-temp-file input (insert "helper-input"))
          (let ((result
                 (mevedel-execution-run-helper
                  "mevedel-test-helper"
                  (list "sh" "-c"
                        "cat \"$1\"; printf helper-output > \"$2\""
                        "mevedel-test-helper" input output)
                  (list input) (list root) nil session)))
            (should (= 0 (plist-get result :exit-code)))
            (should (equal "helper-input" (plist-get result :output)))
            (should (eq 'off
                        (plist-get (plist-get result :sandbox-facts)
                                   :sandbox)))
            (should-not (processp (mevedel-session-execution-state session)))
            (should-not (plist-member result :process))
            (should (equal "helper-output"
                           (with-temp-buffer
                             (insert-file-contents output)
                             (buffer-string))))))
      (delete-directory root t)))
  :doc "required confinement refuses a helper before execution"
  (let ((mevedel-sandbox-mode 'required)
        (mevedel-sandbox--probe-cache
         '(:available nil :reason "test unavailable")))
    (let ((result
           (mevedel-execution-run-helper
            "mevedel-test-helper-required" '("true") nil nil)))
      (should (= -1 (plist-get result :exit-code)))
      (should (string-match-p "test unavailable"
                              (error-message-string
                               (plist-get result :error)))))))

(provide 'test-mevedel-execution)
;;; test-mevedel-execution.el ends here
