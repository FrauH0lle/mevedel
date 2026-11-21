;;; test-niwa-execution-process-stderr.el -- Separate stderr tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises native stderr pipes, startup buffering, the shared spool cap,
;; and cleanup through the same process owner used by managed executions.

;;; Code:

(require 'cl-lib)
(require 'niwa-execution-process)
(require 'niwa-transport)
(require 'tramp-sh)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-execution-test-helpers"))

(defun test-niwa-process-stderr--wait (predicate)
  "Wait for PREDICATE with a bounded test deadline."
  (with-timeout (10 (ert-fail "Child did not settle"))
    (while (not (funcall predicate))
      (accept-process-output nil 0.02))))

(defun test-niwa-process-stderr--run (workdir command &optional confined)
  "Check separated COMMAND output in WORKDIR, optionally CONFINED."
  (let (result stdout stderr pipe buffer
        (terminals 0)
        (filtered 0)
        child)
    (unwind-protect
        (progn
          (setq child
                (niwa-execution-process-create
                 :workdir workdir
                 :filter-function
                 (lambda (_child chunk)
                   (cl-incf filtered)
                   (string-replace "PRIVATE:" "" chunk))
                 :output-function
                 (lambda (_child chunk) (push chunk stdout))
                 :stderr-function
                 (lambda (_child chunk) (push chunk stderr))
                 :terminal-function
                 (lambda (_child value)
                   (cl-incf terminals)
                   (setq result value))))
          (should (niwa-execution-process-start
                   child :name "niwa-separated-output" :command command
                   :coding 'utf-8-unix :separate-stderr t :confined confined))
          (setq pipe (niwa-execution-process--child-stderr-process child)
                buffer (niwa-execution-process--child-stderr-buffer child))
          ;; The stderr pipe decodes like stdout, not by the locale.
          (when (process-live-p pipe)
            (should (eq 'utf-8-unix (car (process-coding-system pipe)))))
          (test-niwa-process-stderr--wait (lambda () result))
          (should (= terminals 1))
          (should (equal 0 (plist-get result :exit-code)))
          (should (> filtered 0))
          (should (equal "out\n" (apply #'concat (nreverse stdout))))
          ;; Neither the caller's stdout filter nor private group-marker
          ;; recognition may consume a stderr line that looks private.
          (should (equal "PRIVATE:NIWA_PROCESS_GROUP_123:456\nerr-tail"
                         (apply #'concat (nreverse stderr))))
          (let ((output (plist-get result :output)))
            (should (string-match-p (regexp-quote "out\n") output))
            (should (string-match-p
                     (regexp-quote "PRIVATE:NIWA_PROCESS_GROUP_123:456\n")
                     output))
            (should (string-match-p "err-tail" output))
            (should (= (string-bytes output) (plist-get result :output-bytes)))
            (should (= (length output)
                       (+ (length "out\n")
                          (length "PRIVATE:NIWA_PROCESS_GROUP_123:456\nerr-tail")))))
          (should-not (buffer-live-p buffer))
          (when pipe
            (should-not (process-live-p pipe))
            (should-not (memq pipe (process-list))))
          (should-not (niwa-execution-process--child-stderr-buffer child))
          (should-not (niwa-execution-process--child-stderr-process child))
          (plist-get (niwa-execution-process-status child) :direct-async-p))
      (when child (niwa-execution-process-release child)))))

(defconst test-niwa-process-stderr--script
  "printf 'PRIVATE:NIWA_PROCESS_GROUP_123:456\\n' >&2; printf 'PRIVATE:out\\n'; printf err-tail >&2"
  "Small command whose stderr resembles private stdout metadata.")

(niwa-deftest niwa-execution-process-separated-stderr ()
  ,test
  (test)
  :doc "local native pipe keeps callbacks separate and leaves no adapter text"
  (test-niwa-process-stderr--run
   temporary-file-directory
   (list "sh" "-c" test-niwa-process-stderr--script))
  :doc "real Bubblewrap keeps stderr separate and cleans its pipe"
  (progn
    (skip-unless (executable-find "bwrap"))
    (test-niwa-process-stderr--run
     temporary-file-directory
     (list "bwrap" "--ro-bind" "/" "/" "--proc" "/proc" "--dev" "/dev"
           "--unshare-pid" "--die-with-parent" "sh" "-c"
           test-niwa-process-stderr--script)
     t))
  :doc "the TRAMP sh adapter keeps stderr in a target file, collected once"
  (niwa-test--with-local-shell-tramp nil
    (let ((niwa-transport-remote-direct-async nil)
          result child stdout stderr remote-path)
      (unwind-protect
          (progn
            (setq child
                  (niwa-execution-process-create
                   :workdir (format "/niwamock:%s:%s" (system-name)
                                    temporary-file-directory)
                   :output-function (lambda (_child chunk) (push chunk stdout))
                   :stderr-function (lambda (_child chunk) (push chunk stderr))
                   :terminal-function (lambda (_child value) (setq result value))))
            (should (niwa-execution-process-start
                     child :name "niwa-separated-output"
                     :command (list "sh" "-c" "printf 'caf\\303\\251' >&2; printf out")
                     :coding 'utf-8-unix :separate-stderr t))
            ;; No buffer-backed stderr, so TRAMP adds no FIFO, second
            ;; connection, or sentinel cleanup.
            (should-not (niwa-execution-process--child-stderr-buffer child))
            (setq remote-path
                  (niwa-execution-process--child-stderr-remote-path child))
            (should (file-remote-p remote-path))
            (test-niwa-process-stderr--wait (lambda () result))
            (should (equal 0 (plist-get result :exit-code)))
            (should-not stderr)
            (should (equal "out" (apply #'concat (nreverse stdout))))
            (should (file-exists-p remote-path))
            (should (equal '("café" . nil) (niwa-execution-process-collect-stderr child)))
            (should (equal "outcafé" (niwa-execution-process-read child)))
            (should-not (file-exists-p remote-path))
            ;; Collection happens once.
            (should-not (niwa-execution-process-collect-stderr child)))
        (when child (niwa-execution-process-release child)))))
  :doc "an uncollected target stderr file is deleted when discarded"
  (niwa-test--with-local-shell-tramp nil
    (let ((niwa-transport-remote-direct-async nil)
          result child remote-path)
      (unwind-protect
          (progn
            (setq child
                  (niwa-execution-process-create
                   :workdir (format "/niwamock:%s:%s" (system-name)
                                    temporary-file-directory)
                   :terminal-function (lambda (_child value) (setq result value))))
            (should (niwa-execution-process-start
                     child :name "niwa-separated-output"
                     :command (list "sh" "-c" "printf err >&2")
                     :coding 'utf-8-unix :separate-stderr t))
            (setq remote-path
                  (niwa-execution-process--child-stderr-remote-path child))
            (test-niwa-process-stderr--wait (lambda () result))
            (niwa-execution-process-discard-stderr child)
            (should-not (file-exists-p remote-path))
            (should-not (niwa-execution-process-collect-stderr child)))
        (when child (niwa-execution-process-release child)))))
  :doc "collection respects the shared output cap"
  (niwa-test--with-local-shell-tramp nil
    (let ((niwa-transport-remote-direct-async nil)
          (niwa-execution-process-output-limit 5)
          result child)
      (unwind-protect
          (progn
            (setq child
                  (niwa-execution-process-create
                   :workdir (format "/niwamock:%s:%s" (system-name)
                                    temporary-file-directory)
                   :terminal-function (lambda (_child value) (setq result value))))
            (should (niwa-execution-process-start
                     child :name "niwa-separated-output"
                     :command (list "sh" "-c" "printf abcdefghijklmnopqrstuvwxyz >&2; printf xy")
                     :coding 'utf-8-unix :separate-stderr t))
            (test-niwa-process-stderr--wait (lambda () result))
            ;; The target file itself was bounded before collection: the
            ;; command wrote 26 bytes, the file holds at most cap + 1.
            (should (<= (file-attribute-size
                         (file-attributes
                          (niwa-execution-process--child-stderr-remote-path child)))
                        6))
            (should (equal '("abc" . t) (niwa-execution-process-collect-stderr child)))
            (should (equal "xyabc" (niwa-execution-process-read child)))
            (should (plist-get (niwa-execution-process-status child)
                               :output-limit-p)))
        (when child (niwa-execution-process-release child)))))
  :doc "a failed read signals instead of silently dropping stderr"
  (niwa-test--with-local-shell-tramp nil
    (let ((niwa-transport-remote-direct-async nil)
          result child remote-path)
      (unwind-protect
          (progn
            (setq child
                  (niwa-execution-process-create
                   :workdir (format "/niwamock:%s:%s" (system-name)
                                    temporary-file-directory)
                   :terminal-function (lambda (_child value) (setq result value))))
            (should (niwa-execution-process-start
                     child :name "niwa-separated-output"
                     :command (list "sh" "-c" "printf err >&2")
                     :coding 'utf-8-unix :separate-stderr t))
            (test-niwa-process-stderr--wait (lambda () result))
            (setq remote-path
                  (niwa-execution-process--child-stderr-remote-path child))
            (cl-letf (((symbol-function 'insert-file-contents-literally)
                       (lambda (&rest _) (signal 'file-error '("Injected read failure")))))
              (should-error (niwa-execution-process-collect-stderr child)
                            :type 'file-error))
            (should-not (file-exists-p remote-path)))
        (when child (niwa-execution-process-release child)))))
  :doc "opt-in real SSH separates stderr on both the sh and direct-async adapters"
  ;; The shared helper skips without a target and applies the runner's SSH
  ;; configuration, as the other real-SSH journeys do.
  (let* ((base (test-niwa-execution-remote--real-root
                "NIWA_TEST_SSH_ROOT" 'ssh))
         (root (test-niwa-execution-remote--real-temp-directory
                base "niwa-stderr-")))
    (unwind-protect
        (test-niwa-execution-remote--with-temporary-directory root
          (let ((niwa-transport-remote-direct-async t))
            (should
             (test-niwa-process-stderr--run
              root (list "sh" "-c" test-niwa-process-stderr--script))))
          ;; The sh adapter keeps stderr in a target file and leaves a
          ;; concurrent command on the same connection undisturbed.
          (let ((niwa-transport-remote-direct-async nil)
                result child stdout remote-path)
            (unwind-protect
                (progn
                  (setq child
                        (niwa-execution-process-create
                         :workdir root
                         :output-function (lambda (_child chunk) (push chunk stdout))
                         :terminal-function (lambda (_child value) (setq result value))))
                  (should (niwa-execution-process-start
                           child :name "niwa-separated-output"
                           :command (list "sh" "-c"
                                          "sleep 0.3; printf err >&2; printf out; exit 4")
                           :coding 'utf-8-unix :separate-stderr t))
                  (setq remote-path
                        (niwa-execution-process--child-stderr-remote-path child))
                  (should (equal 7 (let ((default-directory root))
                                     (process-file "sh" nil nil nil "-c"
                                                   "sleep 1; exit 7"))))
                  (test-niwa-process-stderr--wait (lambda () result))
                  (should (equal 4 (plist-get result :exit-code)))
                  (should (equal "out" (apply #'concat (nreverse stdout))))
                  (should (equal '("err" . nil) (niwa-execution-process-collect-stderr child)))
                  (should-not (file-exists-p remote-path)))
              (when child (niwa-execution-process-release child)))))
      (unwind-protect
          (delete-directory root t)
        (tramp-cleanup-all-connections))))
  :doc "ordinary Bash output remains merged when separation is not requested"
  (let (result stdout stderr child)
    (unwind-protect
        (progn
          (setq child
                (niwa-execution-process-create
                 :workdir temporary-file-directory
                 :output-function (lambda (_child chunk) (push chunk stdout))
                 :stderr-function (lambda (_child chunk) (push chunk stderr))
                 :terminal-function (lambda (_child value) (setq result value))))
          (niwa-execution-process-start
           child :name "niwa-merged-output" :coding 'utf-8-unix
           :command '("sh" "-c" "printf out; printf err >&2"))
          (test-niwa-process-stderr--wait (lambda () result))
          (should (equal "outerr" (apply #'concat (nreverse stdout))))
          (should (equal "outerr" (plist-get result :output)))
          (should-not stderr))
      (when child (niwa-execution-process-release child))))
  :doc "both stream orders share one cap and callbacks receive retained bytes only"
  (dolist (stderr-first '(nil t))
    (let* ((root (make-temp-file "niwa-stderr-cap-" t))
           (gate (file-name-concat root "continue"))
           (niwa-execution-process-output-limit 7)
           (niwa-execution-process--child-kill-delay 0.02)
           result stdout stderr child)
      (unwind-protect
          (progn
            (setq child
                  (niwa-execution-process-create
                   :workdir root
                   :output-function
                   (lambda (_child chunk)
                     (push chunk stdout)
                     (write-region "" nil gate nil 'silent))
                   :stderr-function
                   (lambda (_child chunk)
                     (push chunk stderr)
                     (write-region "" nil gate nil 'silent))
                   :terminal-function (lambda (_child value) (setq result value))))
            (niwa-execution-process-start
             child :name "niwa-shared-cap" :coding 'utf-8-unix
             :separate-stderr t
             :command
             (list "sh" "-c"
                   (format "printf abc%s; while [ ! -f continue ]; do sleep .01; done; printf 123456789%s"
                           (if stderr-first " >&2" "")
                           (if stderr-first "" " >&2"))))
            (test-niwa-process-stderr--wait (lambda () result))
            (should (equal "abc1234" (plist-get result :output)))
            (should (= 7 (plist-get result :output-bytes)))
            (should (plist-get result :output-limit-p))
            (should (eq 'output-limit (plist-get result :termination)))
            (should (equal (if stderr-first "1234" "abc")
                           (apply #'concat (nreverse stdout))))
            (should (equal (if stderr-first "abc" "1234")
                           (apply #'concat (nreverse stderr)))))
        (when child (niwa-execution-process-release child))
        (delete-directory root t)))))

(niwa-deftest niwa-execution-process--attach-stderr ()
  ,test
  (test)
  :doc "retains actual startup output received before installing the filter"
  (let* ((root (make-temp-file "niwa-stderr-startup-" t))
         (gate (file-name-concat root "continue"))
         (make-process-function (symbol-function 'make-process))
         result stderr startup child)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (let* ((process (apply make-process-function args))
                            (buffer (plist-get args :stderr))
                            (pipe (get-buffer-process buffer)))
                       (test-niwa-process-stderr--wait
                        (lambda ()
                          (accept-process-output pipe 0.01 nil 1)
                          (with-current-buffer buffer
                            (setq startup (buffer-string))
                            (> (buffer-size) 0))))
                       process))))
          (setq child
                (niwa-execution-process-create
                 :workdir root
                 :stderr-function (lambda (_child chunk) (push chunk stderr))
                 :terminal-function (lambda (_child value) (setq result value))))
          (niwa-execution-process-start
           child :name "niwa-stderr-startup" :coding 'utf-8-unix
           :separate-stderr t
           :command '("sh" "-c" "printf early >&2; while [ ! -f continue ]; do sleep .01; done; printf late >&2"))
          (should (equal "early" startup))
          (should (equal '("early") stderr))
          (write-region "" nil gate nil 'silent)
          (test-niwa-process-stderr--wait (lambda () result))
          (should (equal "earlylate" (plist-get result :output)))
          (should (equal "earlylate" (apply #'concat (nreverse stderr)))))
      (when child (niwa-execution-process-release child))
      (delete-directory root t)))
  :doc "startup buffer survives an adapter whose stderr process is already gone"
  (let* ((buffer (generate-new-buffer " *niwa-stderr-fallback-test*"))
         result stderr
         (child (niwa-execution-process-create
                 :workdir temporary-file-directory
                 :stderr-function (lambda (_child chunk) (push chunk stderr))
                 :terminal-function (lambda (_child value) (setq result value)))))
    (unwind-protect
        (progn
          (setf (niwa-execution-process--child-stderr-buffer child) buffer
                (niwa-execution-process--child-coding child) 'utf-8-unix)
          (with-current-buffer buffer (insert "buffered-stderr"))
          (niwa-execution-process--attach-stderr child)
          (should (equal '("buffered-stderr") stderr))
          (should (zerop (buffer-size buffer)))
          (niwa-execution-process--finish child 0)
          (should (equal "buffered-stderr" (plist-get result :output)))
          (should-not (buffer-live-p buffer)))
      (niwa-execution-process-release child))))

(niwa-deftest niwa-execution-process--drain-stderr ()
  ,test
  (test)
  :doc "fallback buffer and pipe drain before the immutable terminal snapshot"
  (let* ((root (make-temp-file "niwa-stderr-drain-" t))
         (gate (file-name-concat root "continue"))
         result stderr child pipe buffer)
    (unwind-protect
        (progn
          (setq child
                (niwa-execution-process-create
                 :workdir root
                 :output-function (lambda (&rest _) (ert-fail "Stderr reached stdout"))
                 :stderr-function (lambda (_child chunk) (push chunk stderr))
                 :terminal-function (lambda (_child value) (setq result value))))
          (niwa-execution-process-start
           child :name "niwa-stderr-drain" :coding 'utf-8-unix
           :separate-stderr t
           :command '("sh" "-c" "while [ ! -f continue ]; do sleep .01; done; printf terminal-stderr >&2"))
          (setq pipe (niwa-execution-process--child-stderr-process child)
                buffer (niwa-execution-process--child-stderr-buffer child))
          ;; Emulate a buffer-writing adapter instead of our streaming filter.
          (set-process-filter pipe nil)
          (write-region "" nil gate nil 'silent)
          (test-niwa-process-stderr--wait (lambda () result))
          (should (equal "terminal-stderr" (plist-get result :output)))
          (should (equal "terminal-stderr" (apply #'concat (nreverse stderr))))
          (should-not (buffer-live-p buffer))
          (should-not (memq pipe (process-list))))
      (when child (niwa-execution-process-release child))
      (delete-directory root t)))
  :doc "escaped descendant continuously writing stderr cannot block settlement"
  (progn
    (skip-unless (and (executable-find "setsid") (executable-find "python3")))
    (let* ((root (make-temp-file "niwa-stderr-descendant-" t))
           (niwa-execution-process-output-limit (* 1024 1024))
           (script (concat "import os,time\n"
                           "open('ready','w').close()\n"
                           "end=time.monotonic()+3\n"
                           "try:\n"
                           " while time.monotonic()<end: os.write(2,b'x'*65536)\n"
                           "except BrokenPipeError: pass\n"
                           "open('done','w').close()\n"))
           (started (float-time))
           result child)
      (unwind-protect
          (progn
            (setq child
                  (niwa-execution-process-create
                   :workdir root
                   :terminal-function (lambda (_child value) (setq result value))))
            (niwa-execution-process-start
             child :name "niwa-stderr-descendant" :coding 'utf-8-unix
             :separate-stderr t
             :command
             (list "sh" "-c"
                   (concat "setsid python3 -c " (shell-quote-argument script)
                           " </dev/null >/dev/null & "
                           "while [ ! -f ready ]; do sleep .001; done")))
            (test-niwa-process-stderr--wait (lambda () result))
            (should (< (- (float-time) started) 1.5))
            (should (<= (plist-get result :output-bytes)
                        niwa-execution-process-output-limit)))
        (when child (niwa-execution-process-release child))
        ;; Closing our pipe releases the escaped writer via BrokenPipeError;
        ;; its own deadline also bounds teardown if the assertion fails.
        (when (file-exists-p (file-name-concat root "ready"))
          (test-niwa-process-stderr--wait
           (lambda () (file-exists-p (file-name-concat root "done")))))
        (delete-directory root t))))
  :doc "failed pipe draining restores the original filter after retaining buffered text"
  (dolist (buffer-filter '(nil t))
    (let* ((buffer (generate-new-buffer " *niwa-stderr-drain-error*"))
           (pipe (make-pipe-process :name "niwa-stderr-drain-error"
                                    :buffer buffer :noquery t :sentinel #'ignore))
           stderr filter
           (child (niwa-execution-process-create
                   :workdir temporary-file-directory
                   :stderr-function (lambda (_child chunk) (push chunk stderr)))))
      (unwind-protect
          (progn
            (setf (niwa-execution-process--child-stderr-buffer child) buffer
                  (niwa-execution-process--child-coding child) 'utf-8-unix)
            (niwa-execution-process--attach-stderr child)
            (when buffer-filter (set-process-filter pipe nil))
            (setq filter (process-filter pipe))
            (with-current-buffer buffer (insert "buffered-before-error"))
            (cl-letf (((symbol-function 'accept-process-output)
                       (lambda (process &rest _)
                         (should (eq pipe process))
                         (should-not (eq filter (process-filter pipe)))
                         (error "Injected drain failure"))))
              (should (equal '(error "Injected drain failure")
                             (should-error
                              (niwa-execution-process--drain-stderr child)))))
            (should (process-live-p pipe))
            (should (eq filter (process-filter pipe)))
            (should (equal '("buffered-before-error") stderr))
            (should (equal "buffered-before-error"
                           (niwa-execution-process-read child)))
            (should (zerop (buffer-size buffer))))
        (niwa-execution-process-release child))
      (should-not (buffer-live-p buffer))
      (should-not (memq pipe (process-list))))))

(niwa-deftest niwa-execution-process--time-out ()
  ,test
  (test)
  :doc "synchronous timeout settlement records the reason without leaking a timer"
  (let* (result
         (child (niwa-execution-process-create
                 :workdir temporary-file-directory
                 :terminal-function (lambda (_child value) (setq result value)))))
    (unwind-protect
        (progn
          (niwa-execution-process--time-out child)
          (should (plist-get result :timed-out-p))
          (should (eq 'timed-out (plist-get result :termination)))
          (should-not
           (niwa-execution-process--child-timeout-settle-timer child)))
      (niwa-execution-process-release child))))

(niwa-deftest niwa-execution-process--settle-timed-out ()
  ,test
  (test)
  :doc "timeout fallback settles once and releases its retained timer"
  (let* (result
         (terminals 0)
         (child (niwa-execution-process-create
                 :workdir temporary-file-directory
                 :terminal-function (lambda (_child value)
                                      (cl-incf terminals)
                                      (setq result value))))
         (timer (run-at-time 60 nil #'ignore)))
    (unwind-protect
        (progn
          (setf (niwa-execution-process--child-timed-out-p child) t
                (niwa-execution-process--child-termination child) 'timed-out
                (niwa-execution-process--child-timeout-settle-timer child) timer)
          (niwa-execution-process--settle-timed-out child)
          (niwa-execution-process--settle-timed-out child)
          (should (= terminals 1))
          (should (plist-get result :timed-out-p))
          (should (eq 'timed-out (plist-get result :termination)))
          (should-not (memq timer timer-list)))
      (cancel-timer timer)
      (niwa-execution-process-release child))))

(niwa-deftest niwa-execution-process-stderr-cleanup ()
  ,test
  (test)
  :doc "spawn failure cleans a buffer allocated before make-process"
  (let (child result buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq buffer (plist-get args :stderr))
                     (error "Injected spawn failure"))))
          (setq child
                (niwa-execution-process-create
                 :workdir temporary-file-directory
                 :terminal-function (lambda (_child value) (setq result value))))
          (should-not
           (niwa-execution-process-start
            child :name "niwa-stderr-failure" :command '("sh" "-c" "true")
            :coding 'utf-8-unix :separate-stderr t))
          (should (plist-get result :error))
          (should (bufferp buffer))
          (should-not (buffer-live-p buffer)))
      (when child (niwa-execution-process-release child))))
  :doc "release and timeout settlement cancel the fallback timer and pipe"
  (dolist (release '(nil t))
    (let ((niwa-execution-process--child-kill-delay 0.02)
          result child fallback buffer pipe)
      (unwind-protect
          (progn
            (setq child
                  (niwa-execution-process-create
                   :workdir temporary-file-directory
                   :terminal-function (lambda (_child value) (setq result value))))
            (niwa-execution-process-start
             child :name "niwa-stderr-timeout"
             :command '("sh" "-c" "exec sleep 30")
             :coding 'utf-8-unix :separate-stderr t)
            (setq buffer (niwa-execution-process--child-stderr-buffer child)
                  pipe (niwa-execution-process--child-stderr-process child))
            (niwa-execution-process--time-out child)
            (setq fallback
                  (niwa-execution-process--child-timeout-settle-timer child))
            (should (timerp fallback))
            (if release
                (niwa-execution-process-release child)
              (test-niwa-process-stderr--wait (lambda () result))
              (should (plist-get result :timed-out-p))
              (should (eq 'timed-out (plist-get result :termination))))
            (should-not (memq fallback timer-list))
            (should-not (buffer-live-p buffer))
            (should-not (memq pipe (process-list))))
        (when child (niwa-execution-process-release child))))))

(provide 'test-niwa-execution-process-stderr)
;;; test-niwa-execution-process-stderr.el ends here
