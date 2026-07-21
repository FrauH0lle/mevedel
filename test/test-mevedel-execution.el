;;; test-mevedel-execution.el --- Tests for managed child execution -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the bounded one-shot process boundary used by Bash, batch Eval, and
;; native external helpers.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution)
(require 'mevedel-sandbox)
(require 'mevedel-structs)
(require 'mevedel-telemetry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-execution-test-helpers"))


;;
;;; One-shot execution

(mevedel-deftest mevedel-execution-start-one-shot ()
  ,test
  (test)
  :doc "preserves filter output when the watchdog observes exit before delivery"
  (let ((original-make-process (symbol-function 'make-process))
        (original-run-at-time (symbol-function 'run-at-time))
        (mevedel-sandbox-mode 'off)
        chunks filter watch done result)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq filter (plist-get args :filter))
                 (apply
                  original-make-process
                  (plist-put
                   (plist-put
                    args :filter
                    (lambda (process chunk)
                      (push (cons process chunk) chunks)))
                   :sentinel #'ignore))))
              ((symbol-function 'run-at-time)
               (lambda (time repeat function &rest args)
                 (if (and (equal time 0.1) (equal repeat 0.1))
                     (progn
                       (setq watch (lambda () (apply function args)))
                       (funcall original-run-at-time 3600 nil #'ignore))
                   (apply original-run-at-time
                          time repeat function args)))))
      (mevedel-execution-start-one-shot
       (lambda (child-result)
         (setq result child-result
               done t))
       :name "mevedel-test-watchdog-output"
       :command '("sh" "-c" "printf recovered")
       :workdir temporary-file-directory
       :writable-roots (list temporary-file-directory))
      (with-timeout (2 (error "Process did not exit"))
        (while (not chunks)
          (accept-process-output nil 0.01))
        (while (process-live-p (caar chunks))
          (accept-process-output nil 0.01)))
      (funcall watch)
      (dolist (entry (nreverse chunks))
        (funcall filter (car entry) (cdr entry)))
      (with-timeout (2 (error "One-shot process did not settle"))
        (while (not done)
          (accept-process-output nil 0.01)))
      (should (= 0 (plist-get result :exit-code)))
      (should (equal "recovered" (plist-get result :output)))))
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
  :doc "owner teardown settles a synchronous one-shot caller"
  (let* ((root (make-temp-file "mevedel-one-shot-teardown-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-execution--workspace root) root))
         (mevedel-sandbox-mode 'off)
         result)
    (unwind-protect
        (progn
          (run-at-time
           0.05 nil
           (lambda ()
             (mevedel-execution-stop-owner session "agent-a")))
          (setq result
                (mevedel-execution-run-one-shot
                 :name "mevedel-test-owner-teardown"
                 :command '("sh" "-c" "sleep 30")
                 :workdir root :writable-roots (list root)
                 :session session :owner "agent-a"))
          (should (= -1 (plist-get result :exit-code)))
          (should (string-match-p
                   "owner was torn down"
                   (error-message-string (plist-get result :error)))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))
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
          (setq pid (test-mevedel-execution--read-pid pid-file))
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
;;; Managed Bash execution

(mevedel-deftest mevedel-execution--begin-stop ()
  ,test
  (test)
  :doc "latches the first terminal cause across competing stop events"
  (let* ((mevedel-execution--child-kill-delay 30)
         (record (mevedel-execution--record-create)))
    (unwind-protect
        (progn
          (should (mevedel-execution--begin-stop record 'stopped))
          (should-not (mevedel-execution--begin-stop record 'timed-out))
          (should (eq 'stopped
                      (mevedel-execution--record-termination record))))
      (setf (mevedel-execution--record-finished-p record) t)
      (mevedel-execution--release-runtime record))))

(mevedel-deftest mevedel-execution--group-live-p ()
  ,test
  (test)
  :doc "requires a successful process-group probe"
  (let ((record (mevedel-execution--record-create :group-id 42)))
    (cl-letf (((symbol-function 'signal-process)
               (lambda (_group _signal) -1)))
      (should-not (mevedel-execution--group-live-p record)))
    (cl-letf (((symbol-function 'signal-process)
               (lambda (_group _signal) 0)))
      (should (mevedel-execution--group-live-p record)))))

(mevedel-deftest mevedel-execution--utf8-prefix ()
  ,test
  (test)
  :doc "never splits a multibyte character at the byte budget"
  (let ((euro (string #x20ac)))
    (should (equal (concat "a" euro)
                   (mevedel-execution--utf8-prefix
                    (concat "a" euro "b") 4)))
    (should (equal "a" (mevedel-execution--utf8-prefix
                        (concat "a" euro "b") 3)))))

(mevedel-deftest mevedel-execution--retain-output ()
  ,test
  (test)
  :doc "keeps a bounded whole-artifact head and tail across chunks"
  (let ((mevedel-execution-inline-output-limit 3)
        (record (mevedel-execution--record-create)))
    (mevedel-execution--retain-output record "ab")
    (mevedel-execution--retain-output record "cde")
    (should (= 5 (mevedel-execution--record-output-chars record)))
    (should (equal "abc" (mevedel-execution--record-output-head record)))
    (should (equal "cde" (mevedel-execution--record-output-tail record)))
    (should (= 5 (mevedel-execution--record-unread-chars record)))
    (should (equal "abc" (mevedel-execution--record-unread-head record)))
    (should (equal "cde" (mevedel-execution--record-unread-tail record)))))

(mevedel-deftest mevedel-execution--write-managed-output ()
  ,test
  (test)
  :doc "spools complete UTF-8 characters at the output limit"
  (let* ((spool (make-temp-file "mevedel-managed-write-"))
         (euro (string #x20ac))
         (mevedel-execution-output-limit 4)
         (mevedel-execution--child-kill-delay 30)
         (record
          (mevedel-execution--record-create
           :spool-path spool :newline-count 0)))
    (unwind-protect
        (progn
          (mevedel-execution--write-managed-output
           record (concat "a" euro "b"))
          (should (equal (concat "a" euro)
                         (with-temp-buffer
                           (insert-file-contents spool)
                           (buffer-string))))
          (should (mevedel-execution--record-output-limit-p record)))
      (setf (mevedel-execution--record-finished-p record) t)
      (mevedel-execution--release-runtime record)
      (delete-file spool))))

(mevedel-deftest mevedel-execution--unread-preview ()
  ,test
  (test)
  :doc "uses shared character preview semantics and reports omitted bytes"
  (let ((mevedel-execution-inline-output-limit 10)
        (record
         (mevedel-execution--record-create
          :unread-chars 20
          :unread-head "1234567890"
          :unread-tail "abcdefghij")))
    (let ((preview (mevedel-execution--unread-preview record 20)))
      (should (string-match-p "omitted 10 chars"
                              (plist-get preview :output)))
      (should (= 10 (plist-get preview :omitted))))))

(mevedel-deftest mevedel-execution--managed-append ()
  ,test
  (test)
  :doc "ignores process-filter output delivered after terminal settlement"
  (let ((spool (make-temp-file "mevedel-managed-finished-")))
    (unwind-protect
        (let ((record
               (mevedel-execution--record-create
                :finished-p t :spool-path spool)))
          (mevedel-execution--managed-append record "late output\n")
          (should (= 0 (file-attribute-size (file-attributes spool)))))
      (delete-file spool))))

(mevedel-deftest mevedel-execution--resolve-outcome ()
  ,test
  (test)
  :doc "accepts canonical outcomes and defaults without an adapter resolver"
  (should (eq 'success (mevedel-execution--resolve-outcome nil 0 'exited)))
  (should (eq 'failure (mevedel-execution--resolve-outcome nil 1 'exited)))
  (dolist (outcome '(success failure no-match different false))
    (should
     (eq outcome
         (mevedel-execution--resolve-outcome
          (lambda (_exit-code _termination) outcome)
          1 'exited))))
  :doc "contains throwing and invalid adapter resolvers"
  (dolist (resolver
           (list (lambda (_exit-code _termination) (error "Resolver failed"))
                 (lambda (_exit-code _termination) 'invalid)))
    (should (eq 'success
                (mevedel-execution--resolve-outcome resolver 0 'exited)))
    (should (eq 'failure
                (mevedel-execution--resolve-outcome resolver 1 'exited)))))

(mevedel-deftest mevedel-execution--cancel-observer ()
  ,test
  (test)
  :doc "request abort detaches a waiting poll without consuming its output"
  (let* ((root (make-temp-file "mevedel-managed-poll-abort-" t))
         (session (test-mevedel-execution--session root))
         (request (mevedel-request--create :session session))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         initial abandoned final id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c" "printf one; sleep .1; printf two")))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (mevedel-execution-observe
           session "main" id (lambda (value) (setq abandoned value))
           :wait-ms 5000 :request request)
          (mevedel-request-drain-cancellers request)
          (should-not abandoned)
          (setq final (test-mevedel-execution--observe session id))
          (should (equal "two" (plist-get final :output))))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-list ()
  ,test
  (test)
  :doc "lists yielded executions for the canonical owner only"
  (let* ((root (make-temp-file "mevedel-managed-list-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         one two)
    (unwind-protect
        (progn
          (setq one
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep 30")
                 :owner "agent--one"))
          (setq two
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep 30")
                 :owner "agent--two"))
          (should (= 1 (length (mevedel-execution-list
                                session "agent--one"))))
          (should (= 1 (length (mevedel-execution-list
                                session "agent--two"))))
          (should-not (plist-member
                       (car (mevedel-execution-list session "agent--one"))
                       :process))
          (test-mevedel-execution--stop-all
           session "agent--one"
           (list (plist-get (plist-get one :facts) :execution-id)))
          (test-mevedel-execution--stop-all
           session "agent--two"
           (list (plist-get (plist-get two :facts) :execution-id))))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-owner-live-p ()
  ,test
  (test)
  :doc "tracks unsettled processes only for their captured owner"
  (let* ((root (make-temp-file "mevedel-managed-owner-live-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         initial id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep 30")
                 :owner "agent--owner"))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (should (mevedel-execution-owner-live-p session "agent--owner"))
          (should-not (mevedel-execution-owner-live-p session "main"))
          (test-mevedel-execution--stop-all
           session "agent--owner" (list id))
          (should-not
           (mevedel-execution-owner-live-p session "agent--owner")))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-stop ()
  ,test
  (test)
  :doc "stops the process group and returns unread terminal output"
  (let* ((root (make-temp-file "mevedel-managed-stop-" t))
         (session (test-mevedel-execution--session root))
         (pid-file (file-name-concat root "child.pid"))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         initial polled stopped id pid)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 (list "sh" "-c"
                       "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; printf ready; wait"
                       "managed-stop" pid-file)))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (mevedel-execution-observe
           session "main" id (lambda (value) (setq polled value))
           :wait-ms 300000)
          (mevedel-execution-stop
           session "main" id (lambda (value) (setq stopped value)))
          (should (eq 'running
                      (plist-get (plist-get polled :facts) :state)))
          (test-mevedel-execution--wait (lambda () stopped))
          (should (eq 'stopped
                      (plist-get (plist-get stopped :facts) :termination)))
          (should (plist-get stopped :claimed-final-p))
          (setq pid (test-mevedel-execution--read-pid pid-file))
          (test-mevedel-execution--wait
           (lambda () (test-mevedel-execution--process-gone-p pid))))
      (delete-directory root t)))
  :doc "force-kills a PTY process group that ignores TERM"
  (let* ((root (make-temp-file "mevedel-managed-pty-kill-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         initial stopped id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c"
                   "trap '' TERM; printf ready; while :; do sleep 1; done")
                 :tty t))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (let* ((state (mevedel-session-execution-state session))
                 (record
                  (gethash id (mevedel-execution--state-records state))))
            (should (mevedel-execution--begin-stop record 'stopped)))
          (should-error
           (mevedel-execution-observe
            session "main" id #'ignore :chars "late\n" :wait-ms 250)
           :type 'mevedel-execution-input-error)
          (setq stopped (test-mevedel-execution--observe session id))
          (should (eq 'stopped
                      (plist-get (plist-get stopped :facts) :termination)))
          (should (integerp
                   (plist-get (plist-get stopped :facts) :exit-code)))
          (should-not (zerop
                       (plist-get (plist-get stopped :facts) :exit-code)))
          (should (plist-get (plist-get stopped :facts) :tty)))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution--eask-command-p
  (:doc "recognizes direct and npx Eask commands without broad false positives")
  (progn
    (should (mevedel-execution--eask-command-p "eask test ert"))
    (should (mevedel-execution--eask-command-p
             "npx @emacs-eask/cli test ert test/test-one.el"))
    (should (mevedel-execution--eask-command-p
             "EASK_DEBUG=1 npx @emacs-eask/cli test ert"))
    (should-not
     (mevedel-execution--eask-command-p "printf 'eask test ert'"))))

(mevedel-deftest mevedel-execution--eask-targets
  (:doc "retains only bounded repository-local Emacs Lisp test targets")
  (progn
    (should
     (equal '("test/test-one.el" "test/nested/test-two.el")
            (mevedel-execution--eask-targets
             "eask test ert test/test-one.el test/nested/test-two.el")))
    (should-not (mevedel-execution--eask-targets
                 "eask test ert test/test-* --verbose"))))

(mevedel-deftest mevedel-execution--cache-identity
  (:doc "hashes cache roots deterministically without exposing their values")
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "HOME" "/secret/one")
    (let ((first (mevedel-execution--cache-identity)))
      (should (= 64 (length first)))
      (should-not (string-match-p "secret" first))
      (should (equal first (mevedel-execution--cache-identity)))
      (setenv "HOME" "/secret/two")
      (should-not (equal first (mevedel-execution--cache-identity))))))

(mevedel-deftest mevedel-execution--full-eask-command-p
  (:doc "distinguishes a full ERT suite from focused Eask files")
  (progn
    (should (mevedel-execution--full-eask-command-p
             "npx @emacs-eask/cli test ert test/test-*"))
    (should-not (mevedel-execution--full-eask-command-p
                 "npx @emacs-eask/cli test ert test/test-one.el"))
    (should-not (mevedel-execution--full-eask-command-p "eask lint"))))

(mevedel-deftest mevedel-execution--resource-capture
  (:doc "wraps at most one profiled full suite with GNU time")
  (let* ((root (make-temp-file "mevedel-resource-capture-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-telemetry--profiler-session session)
         (mevedel-telemetry--profiler-run-id "run-test"))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (program)
                     (and (equal program "time") "/usr/bin/time")))
                  ((symbol-function 'file-truename) #'identity))
          (let* ((command "npx @emacs-eask/cli test ert test/test-*")
                 (capture
                  (mevedel-execution--resource-capture session command)))
            (should capture)
            (should
             (string-match-p "/usr/bin/time" (plist-get capture :command)))
            (should (string-suffix-p
                     "diagnostics/run-test/full-suite-time.txt"
                     (plist-get capture :report)))
            (should-not
             (mevedel-execution--resource-capture session command))))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution--telemetry-facts
  (:doc "keeps confinement dimensions while excluding human-readable reasons")
  (let ((facts (mevedel-execution--telemetry-facts
                '(:sandbox bubblewrap :filesystem workspace-write
                  :network isolated :proc fresh :protected-paths 4
                  :additional-filesystem-read 2
                  :additional-filesystem-write 1
                  :reason "private launcher output"))))
    (should (eq 'bubblewrap (plist-get facts :sandbox)))
    (should (= 4 (plist-get facts :protected-path-count)))
    (should (= 2 (plist-get facts :additional-read-count)))
    (should-not (plist-member facts :reason))))

(provide 'test-mevedel-execution)
;;; test-mevedel-execution.el ends here
