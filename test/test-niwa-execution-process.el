;;; test-niwa-execution-process.el -- Process lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the opaque child process, spool, and process-group owner.

;;; Code:

(require 'cl-lib)
(require 'niwa-execution-process)
(require 'niwa-transport)
(require 'niwa-utilities)
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

(defun test-niwa-execution-process--wait-for-exit (process &optional timeout)
  "Wait for PROCESS exit without timers, bounded by TIMEOUT seconds or five."
  (let ((deadline (time-add nil (or timeout 5))))
    (while (and (process-live-p process) (time-less-p nil deadline))
      (accept-process-output process 0.01 nil 1))
    (when (process-live-p process)
      (ert-fail "Child did not exit without timer delivery"))))

(niwa-deftest test-niwa-execution-process--wait-for-exit
  (:doc "a non-exiting child reaches the clock deadline with timers suppressed")
  (let ((process (make-process :name "niwa-test-non-exiting" :noquery t
                               :command '("sh" "-c" "exec sleep 30")
                               :connection-type 'pipe))
        ran timer)
    (unwind-protect
        (progn
          (setq timer (run-at-time 0 nil (lambda () (setq ran t))))
          (let ((failure (should-error
                          (test-niwa-execution-process--wait-for-exit process 0.02)
                          :type 'ert-test-failed)))
            (should (string-search "Child did not exit without timer delivery"
                                   (error-message-string failure))))
          (should-not ran))
      (when timer (cancel-timer timer))
      (delete-process process))))

(defun test-niwa-execution-process--wait (predicate)
  "Wait until PREDICATE returns non-nil."
  (with-timeout (5 (error "Process did not settle"))
    (while (not (funcall predicate))
      (accept-process-output nil 0.02))))

(niwa-deftest niwa-execution-process--filter-pipeline-marker ()
  ,test
  (test)
  :doc "removes a split private frame without changing surrounding bytes"
  (let ((child (niwa-execution-process--child-create :pipeline-marker "token")))
    (should (equal "before"
                   (niwa-execution-process--filter-pipeline-marker
                    child "before\036tok")))
    (should (equal "" (niwa-execution-process--filter-pipeline-marker
                      child "en:7 141")))
    (should (equal "after" (niwa-execution-process--filter-pipeline-marker
                           child " 0\037after")))
    (should (equal '(7 141 0)
                   (plist-get (niwa-execution-process-status child)
                              :pipeline-exit-codes))))
  :doc "preserves unrelated and malformed frames"
  (let* ((child (niwa-execution-process--child-create :pipeline-marker "token"))
         (text "a\036other:7 0\037b\036token:bad\037c"))
    (should (equal text (niwa-execution-process--filter-pipeline-marker child text)))
    (should-not (niwa-execution-process--child-pipeline-exit-codes child))))

(niwa-deftest niwa-execution-process--direct-async-p ()
  ,test
  (test)
  :doc "gates the private channel on flag, tty, capability, and size"
  (let* ((remote "/ssh:user@host:/srv/project/")
         (command '("sh" "-c" "printf ok"))
         (child (niwa-execution-process-create :workdir remote))
         (tty-child
          (niwa-execution-process-create :workdir remote :tty t)))
    (unwind-protect
        (progn
          (should
           (niwa-execution-process--direct-async-p
            child command remote))
          (let ((niwa-transport-remote-direct-async nil))
            (should-not
             (niwa-execution-process--direct-async-p
              child command remote)))
          (should-not
           (niwa-execution-process--direct-async-p
            tty-child command remote))
          (should-not
           (niwa-execution-process--direct-async-p
            child command "/tmp/"))
          (should-not
           (niwa-execution-process--direct-async-p
            child command "/sshx:user@host:/srv/project/"))
          (should-not
           (niwa-execution-process--direct-async-p
            child command "/ssh:jump|ssh:host:/srv/project/"))
          (should-not
           (niwa-execution-process--direct-async-p
            child (list "sh" "-c" (make-string 4096 ?x)) remote)))
      (niwa-execution-process-release child)
      (niwa-execution-process-release tty-child))))

(niwa-deftest niwa-execution-process--remote-command ()
  ,test
  (test)
  :doc "carries the stable environment only on a direct-async wrapper"
  (let ((direct (niwa-execution-process-create))
        (classic (niwa-execution-process-create)))
    (unwind-protect
        (progn
          (setf (niwa-execution-process--child-direct-async-p direct) t)
          (let ((command
                 (niwa-execution-process--remote-command
                  direct '("sh" "-c" "printf ok"))))
            (should (equal "env" (car command)))
            (should (member "NO_COLOR=1" command))
            (should (member "setsid" command)))
          (should
           (equal "setsid"
                  (car
                   (niwa-execution-process--remote-command
                    classic '("sh" "-c" "printf ok"))))))
      (niwa-execution-process-release direct)
      (niwa-execution-process-release classic))))

(niwa-deftest niwa-execution-process-remote-group-status
  (:doc "an identity too incomplete to ask about never reaches the target")
  ;; A recorded outcome may be missing the group number, the start time
  ;; that pins it, or a remote workdir.  Each case must answer without a
  ;; connection attempt, because `ambiguous' is what keeps the caller
  ;; from treating a question it cannot ask as a proof of death.
  (dolist (case '((nil "981" "/ssh:host:/project/")
                 (0 "981" "/ssh:host:/project/")
                 (42 nil "/ssh:host:/project/")
                 (42 "981" nil)
                 (42 "981" "/local/project/")))
    (should
     (eq 'ambiguous
         (apply #'niwa-execution-process-remote-group-status case)))))

(niwa-deftest niwa-execution-process--remote-group-status ()
  ,test
  (test)
  :doc "distinguishes live, settled, and reused process groups"
  (skip-unless (file-directory-p "/proc"))
  (let* ((root (make-temp-file "niwa-process-group-status-" t))
         (remote-root (format "/niwamock:%s:%s/" (system-name) root))
         (pid-file (file-name-concat root "group.pid"))
         (leader
          (make-process
           :name "niwa-test-group-status" :buffer nil
           :command
           (list "sh" "-c"
                 "setsid sh -c 'echo $$ > \"$1\"; sleep 30' sh \"$1\""
                 "sh" pid-file)
           :connection-type 'pipe :noquery t))
         group-id start-time
         (child (niwa-execution-process-create :workdir remote-root)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
          (test-niwa-execution-process--wait
           (lambda () (file-exists-p pid-file)))
          (setq group-id
                (string-to-number
                 (string-trim
                  (with-temp-buffer
                    (insert-file-contents pid-file)
                    (buffer-string))))
                start-time
                (with-temp-buffer
                  (insert-file-contents (format "/proc/%d/stat" group-id))
                  (nth 19
                       (split-string
                        (car (last (split-string (buffer-string) ") ")))))))
          (setf (niwa-execution-process--child-group-id child) group-id
                (niwa-execution-process--child-group-start-time child)
                start-time)
          (should
           (eq 'live
               (niwa-execution-process--remote-group-status child)))
          (setf (niwa-execution-process--child-group-start-time child) "0")
          (should
           (eq 'ambiguous
               (niwa-execution-process--remote-group-status child)))
          (signal-process (- group-id) 'KILL)
          (should
           (eq 'dead
               (niwa-execution-process--remote-group-status child))))
      (when (process-live-p leader) (delete-process leader))
      (when (and group-id
                 (eq 0 (ignore-errors (signal-process (- group-id) 0))))
        (ignore-errors (signal-process (- group-id) 'KILL)))
      (niwa-execution-process-release child)
      (delete-directory root t)))
  :doc "refuses to signal after the captured remote group identity changes"
  (let* ((root (make-temp-file "niwa-process-reused-group-" t))
         (pid-file (file-name-concat root "group.pid"))
         (remote-root (format "/niwamock:%s:%s/" (system-name) root))
         (spool (file-name-concat root "output"))
         (original-signal-process (symbol-function 'signal-process))
         (niwa-execution-process--child-kill-delay 0.02)
         calls child result group-id)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
          (cl-letf
              (((symbol-function
                 'niwa-execution-process--remote-group-status)
                (lambda (_child) 'ambiguous))
               ((symbol-function 'signal-process)
                (lambda (&rest args)
                  (push args calls)
                  (apply original-signal-process args))))
            (write-region "" nil spool nil 'silent)
            (setq child
                  (niwa-execution-process-create
                   :workdir remote-root :spool-path spool
                   :terminal-function
                   (lambda (_child value) (setq result value))))
            (test-niwa-execution--timeout-after-ready
             pid-file
             (lambda ()
               (niwa-execution-process-start
                child :name "niwa-test-process-reused-group"
                :command
                '("sh" "-c"
                  "ps -o pgid= -p $$ | tr -d ' ' > group.pid; trap '' TERM; sleep 30 & wait")
                :coding 'utf-8-unix :timeout 0.05)))
            (test-niwa-execution-process--wait (lambda () result))
            (setq group-id
                  (string-to-number
                   (string-trim
                    (with-temp-buffer
                      (insert-file-contents pid-file)
                      (buffer-string)))))
            (should (eq 'unknown (plist-get result :termination)))
            (should-not
             (cl-some
              (lambda (args)
                (and (integerp (car args))
                     (< (car args) 0)
                     (memq (cadr args) '(TERM KILL))
                     (equal remote-root (nth 2 args))))
              calls))))
      (when child (niwa-execution-process-release child))
      (when (and group-id (> group-id 0))
        (ignore-errors
          (funcall original-signal-process (- group-id) 'KILL)))
      (delete-directory root t)))
  :doc "keeps a proven stop reason when a transient ambiguity resolves"
  ;; A group leader that exits a moment before its last member reads as
  ;; ambiguous for exactly that moment.  The escalation re-probes, so the
  ;; window must not spend the child's already proven stop reason.
  (let* ((root (make-temp-file "niwa-process-transient-ambiguity-" t))
         (pid-file (file-name-concat root "group.pid"))
         (remote-root (format "/niwamock:%s:%s/" (system-name) root))
         (spool (file-name-concat root "output"))
         (original-signal-process (symbol-function 'signal-process))
         (niwa-execution-process--child-kill-delay 0.02)
         (probes 0)
         child result group-id)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
          (cl-letf
              (((symbol-function
                 'niwa-execution-process--remote-group-status)
                (lambda (_child)
                  (setq probes (1+ probes))
                  (if (= probes 1) 'ambiguous 'dead))))
            (write-region "" nil spool nil 'silent)
            (setq child
                  (niwa-execution-process-create
                   :workdir remote-root :spool-path spool
                   :terminal-function
                   (lambda (_child value) (setq result value))))
            (niwa-execution-process-start
             child :name "niwa-test-process-transient-ambiguity"
             :command
             '("sh" "-c"
               "ps -o pgid= -p $$ | tr -d ' ' > group.pid; sleep 30")
             :coding 'utf-8-unix)
            (test-niwa-execution-process--wait
             (lambda () (niwa-execution-process--child-group-id child)))
            (niwa-execution-process-stop child 'output-write-failed)
            (test-niwa-execution-process--wait (lambda () result))
            (should (< 1 probes))
            (should (eq 'output-write-failed
                        (plist-get result :termination)))))
      (when child (niwa-execution-process-release child))
      (when (file-exists-p pid-file)
        (setq group-id
              (string-to-number
               (string-trim
                (with-temp-buffer
                  (insert-file-contents pid-file)
                  (buffer-string))))))
      (when (and group-id (> group-id 0))
        (ignore-errors
          (funcall original-signal-process (- group-id) 'KILL)))
      (delete-directory root t)))
  :doc "bounds a timed-out remote child whose stop escalation wedges"
  (let* ((root (make-temp-file "niwa-process-wedged-" t))
         (pid-file (file-name-concat root "group.pid"))
         (remote-root (format "/niwamock:%s:%s/" (system-name) root))
         (spool (file-name-concat root "output"))
         (original-signal-process (symbol-function 'signal-process))
         (niwa-execution-process--child-kill-delay 0.02)
         (niwa-execution-process--remote-control-timeout 0.02)
         child result group-id settlement-budget)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
          (cl-letf*
              (((symbol-function 'niwa-execution-process--start-stop)
                #'ignore)
               (schedule (symbol-function 'niwa-transport-run-at-time))
               ((symbol-function 'niwa-transport-run-at-time)
                (lambda (seconds function &rest args)
                  (when (eq function #'niwa-execution-process--settle-timed-out)
                    (setq settlement-budget seconds))
                  (apply schedule seconds function args))))
            (write-region "" nil spool nil 'silent)
            (setq child
                  (niwa-execution-process-create
                   :workdir remote-root :spool-path spool
                   :terminal-function
                   (lambda (_child value) (setq result value))))
            (test-niwa-execution--timeout-after-ready
             pid-file
             (lambda ()
               (niwa-execution-process-start
                child :name "niwa-test-process-wedged"
                :command
                '("sh" "-c"
                  "ps -o pgid= -p $$ | tr -d ' ' > group.pid; sleep 30")
                :coding 'utf-8-unix :timeout 0.05)))
            (test-niwa-execution-process--wait (lambda () result))
            (setq group-id
                  (string-to-number
                   (string-trim
                    (with-temp-buffer
                      (insert-file-contents pid-file)
                      (buffer-string)))))
            ;; Check the actual escalation budget, not host scheduling latency.
            (should (< (abs (- settlement-budget 0.12)) 0.000001))
            (should (plist-get result :timed-out-p))
            (should (eq 'unknown (plist-get result :termination)))))
      (when child (niwa-execution-process-release child))
      (when (and group-id (> group-id 0))
        (ignore-errors
          (funcall original-signal-process (- group-id) 'KILL)))
      (delete-directory root t))))

(niwa-deftest niwa-execution-process-interrupt-attribution ()
  ,test
  (test)
  :doc "requested INT and unsolicited signals differ; ignored INT may finish"
  (skip-unless (and (not (eq system-type 'windows-nt))
                    (executable-find "bash")))
  (let* ((root (make-temp-file "niwa-interrupt-attribution-" t))
         (niwa-execution-process--child-kill-delay 0.05)
         requested unsolicited ignored later-signal
         (child (niwa-execution-process-create
                 :workdir root
                 :terminal-function (lambda (_child value)
                                      (setq requested value))))
         (other (niwa-execution-process-create
                 :workdir root
                 :terminal-function (lambda (_child value)
                                      (setq unsolicited value))))
         (survivor (niwa-execution-process-create
                    :workdir root
                    :terminal-function (lambda (_child value)
                                         (setq ignored value))))
         (later (niwa-execution-process-create
                 :workdir root
                 :terminal-function (lambda (_child value)
                                      (setq later-signal value)))))
    (unwind-protect
        (progn
          (niwa-execution-process-start
           child :name "niwa-requested-interrupt"
           :command '("bash" "--noprofile" "--norc" "-c" "exec sleep 30")
           :coding 'utf-8-unix)
          (should (niwa-execution-process-interrupt child))
          (test-niwa-execution-process--wait (lambda () requested))
          (should (eq 'interrupted (plist-get requested :termination)))
          (niwa-execution-process-start
           other :name "niwa-unsolicited-signal"
           :command '("bash" "--noprofile" "--norc" "-c"
                      "sleep .05; kill -PIPE $$")
           :coding 'utf-8-unix)
          (test-niwa-execution-process--wait (lambda () unsolicited))
          (should (eq 'signaled (plist-get unsolicited :termination)))
          (niwa-execution-process-start
           survivor :name "niwa-ignored-interrupt"
           :command '("bash" "--noprofile" "--norc" "-c"
                      "trap '' INT; printf ready; sleep .3; exit 0")
           :coding 'utf-8-unix)
          (test-niwa-execution-process--wait
           (lambda () (string-match-p
                       "ready" (niwa-execution-process-read survivor))))
          (should (niwa-execution-process-interrupt survivor))
          (test-niwa-execution-process--wait (lambda () ignored))
          (should (zerop (plist-get ignored :exit-code)))
          (should-not (plist-get ignored :termination))
          (niwa-execution-process-start
           later :name "niwa-later-signal"
           :command '("bash" "--noprofile" "--norc" "-c"
                      "trap '' INT; printf ready; sleep .3; kill -PIPE $$")
           :coding 'utf-8-unix)
          (test-niwa-execution-process--wait
           (lambda () (string-match-p
                       "ready" (niwa-execution-process-read later))))
          (should (niwa-execution-process-interrupt later))
          (test-niwa-execution-process--wait (lambda () later-signal))
          (should (eq 'signaled (plist-get later-signal :termination))))
      (niwa-execution-process-release child)
      (niwa-execution-process-release other)
      (niwa-execution-process-release survivor)
      (niwa-execution-process-release later)
      (delete-directory root t))))

(niwa-deftest niwa-execution-process-start ()
  ,test
  (test)
  :doc "settles an exit observed inside a remote command after it returns"
  ;; A sentinel can run inside another remote command, where TRAMP has bound
  ;; the timer list away.  The settle timer it arms must neither be lost nor
  ;; fire in that command, where settlement's own target work is refused.
  (let* ((root (make-temp-file "niwa-process-nested-" t))
         (spool (file-name-concat root "output"))
         nested result watch
         (child
          (niwa-execution-process-create
           :workdir root :spool-path spool
           :terminal-function
           (lambda (_child value)
             (setq nested (niwa-transport-nested-p)
                   result value)))))
    (unwind-protect
        (progn
          (write-region "" nil spool nil 'silent)
          (niwa-execution-process-start
           child :name "niwa-test-process-nested"
           :command '("sh" "-c" "exit 0") :coding 'utf-8-unix)
          (setq watch (niwa-execution-process--child-watch-timer child))
          (let ((process (niwa-execution-process--child-process child)))
            ;; Observe the exit only from the simulated remote command: no
            ;; sentinel, and a wait that runs neither timers nor sentinels.
            (set-process-sentinel process #'ignore)
            (test-niwa-execution-process--wait-for-exit process)
            (niwa-transport--handler-advice
             (lambda ()
               (let (timer-list timer-idle-list)
                 (niwa-execution-process--ended child process)
                 ;; TRAMP's own wait runs timers armed on its binding.
                 (accept-process-output nil 0.05)
                 (should-not result)))))
          (test-niwa-execution-process--wait (lambda () result))
          (should-not nested)
          (should (= 0 (plist-get result :exit-code)))
          (should-not (memq watch timer-list)))
      (niwa-execution-process-release child)
      (delete-directory root t)))

  :doc "emits one complete terminal result after a successful child"
  (let* ((root (make-temp-file "niwa-process-start-" t))
         (spool (file-name-concat root "output"))
         (terminals 0)
         result callback-output callback-live callback-terminal timers
         (child
          (niwa-execution-process-create
           :workdir root :spool-path spool
           :terminal-function
           (lambda (child value)
             (cl-incf terminals)
             (setq result value
                   callback-output (niwa-execution-process-read child)
                   callback-live (niwa-execution-process-live-p child)
                   callback-terminal
                   (niwa-execution-process-terminal-p child))))))
    (unwind-protect
        (progn
          (write-region "" nil spool nil 'silent)
          (niwa-execution-process-start
           child :name "niwa-test-process-start"
           :command '("sh" "-c" "printf 'hello world'")
           :coding 'utf-8-unix :timeout 30)
          (setq timers
                (list (niwa-execution-process--child-watch-timer child)
                      (niwa-execution-process--child-timeout-timer child)))
          (test-niwa-execution-process--wait (lambda () result))
          (accept-process-output nil 0.05)
          (should (= 1 terminals))
          (should (= 0 (plist-get result :exit-code)))
          (should (equal "hello world" (plist-get result :output)))
          (should (= 11 (plist-get result :output-bytes)))
          (should (equal "hello world" callback-output))
          (should-not callback-live)
          (should callback-terminal)
          (dolist (timer timers)
            (should (timerp timer))
            (should-not (memq timer timer-list)))
          (should (equal spool (niwa-execution-process-spool-path child)))
          (should (equal "hello world" (niwa-execution-process-read child)))
          (niwa-execution-process-release child)
          (should-not (file-exists-p spool)))
      (niwa-execution-process-release child)
      (delete-directory root t)))
  :doc "preserves the exit code when a child exits before stdin EOF"
  (dolist (status '(0 7))
    (let* ((root (make-temp-file "niwa-process-eof-" t))
           (spool (file-name-concat root "output"))
           (gate (file-name-concat root "exit"))
           result raced watchdog-failure started
           (child (niwa-execution-process-create
                   :workdir root :spool-path spool
                   :terminal-function (lambda (_child value) (setq result value)))))
      (unwind-protect
          (cl-letf* ((send-eof (symbol-function 'process-send-eof))
                     ((symbol-function 'process-send-eof)
                      (lambda (&optional process)
                        (when (eq process (niwa-execution-process--child-process child))
                          (setq raced t)
                          (write-region "" nil gate nil 'silent)
                          (condition-case failure
                              (test-niwa-execution-process--wait-for-exit process)
                            (ert-test-failed (setq watchdog-failure failure))))
                        (funcall send-eof process))))
            (write-region "" nil spool nil 'silent)
            (setq started
                  (niwa-execution-process-start
                   child :name "niwa-test-process-eof"
                   :command (list "sh" "-c"
                                  "while [ ! -f \"$1\" ]; do sleep 0.01; done; printf done; exit \"$2\""
                                  "niwa-test" gate (number-to-string status))
                   :coding 'utf-8-unix))
            ;; Start catches process errors, including ERT watchdog failures.
            ;; Re-signal outside that catch before checking its return value.
            (when watchdog-failure
              (signal (car watchdog-failure) (cdr watchdog-failure)))
            (should (eq child started))
            (test-niwa-execution-process--wait (lambda () result))
            (should raced)
            (should (equal status (plist-get result :exit-code)))
            (should-not (plist-get result :error))
            (should (equal "done" (plist-get result :output))))
        (niwa-execution-process-release child)
        (delete-directory root t))))
  :doc "re-signals the EOF watchdog outside the production start error handler"
  (let* ((test (cl-find-if
                (lambda (test)
                  (equal (ert-test-documentation test)
                         "preserves the exit code when a child exits before stdin EOF"))
                (ert-select-tests "\\`niwa-execution-process-start/test" t)))
         (result
          (cl-letf (((symbol-function 'test-niwa-execution-process--wait-for-exit)
                     (lambda (&rest _)
                       (ert-fail "Child did not exit without timer delivery"))))
            (ert-run-test test))))
    (should (ert-test-failed-p result))
    (should (equal '(ert-test-failed "Child did not exit without timer delivery")
                   (ert-test-failed-condition result))))
  :doc "drains filter output when the watchdog observes exit first"
  (let* ((root (make-temp-file "niwa-process-watchdog-" t))
         (spool (file-name-concat root "output"))
         (original-make-process (symbol-function 'make-process))
         (original-accept-process-output
          (symbol-function 'accept-process-output))
         (original-run-at-time (symbol-function 'run-at-time))
         chunks filter settle watch result drain-just-this-one child)
    (unwind-protect
        (cl-letf
            (((symbol-function 'make-process)
              (lambda (&rest args)
                (setq filter (plist-get args :filter))
                (apply original-make-process
                       (plist-put
                        (plist-put
                         args :filter
                         (lambda (process chunk)
                           (push (cons process chunk) chunks)))
                        :sentinel #'ignore))))
             ((symbol-function 'niwa--run-periodic-timer)
              (lambda (_scope period function &rest args)
                (should (= period niwa-execution-process--watch-interval))
                (setq watch (lambda () (apply function args)))
                (funcall original-run-at-time 3600 nil #'ignore)))
             ((symbol-function 'run-at-time)
              (lambda (time repeat function &rest args)
                (cond
                 ((eq function
                      #'niwa-execution-process--settle-main-exit)
                  (setq settle (lambda () (apply function args)))
                  (funcall original-run-at-time 3600 nil #'ignore))
                 (t
                  (apply original-run-at-time time repeat function args))))))
          (write-region "" nil spool nil 'silent)
          (setq child
                (niwa-execution-process-create
                 :workdir root :spool-path spool
                 :terminal-function
                 (lambda (_child value) (setq result value))))
          (niwa-execution-process-start
           child :name "niwa-test-process-watchdog"
           :command '("sh" "-c" "printf recovered")
           :coding 'utf-8-unix)
          (with-timeout (2 (error "Process did not exit"))
            (while (not chunks)
              (funcall original-accept-process-output nil 0.01))
            (while (process-live-p (caar chunks))
              (funcall original-accept-process-output nil 0.01)))
          (funcall watch)
          (should settle)
          (cl-letf (((symbol-function 'accept-process-output)
                     (lambda (_process _seconds _millisec just-this-one)
                       (setq drain-just-this-one just-this-one)
                       (when chunks
                         (dolist (entry (nreverse chunks))
                           (funcall filter (car entry) (cdr entry)))
                         (setq chunks nil)
                         t))))
            (funcall settle))
          (should (= 1 drain-just-this-one))
          (should (= 0 (plist-get result :exit-code)))
          (should (equal "recovered" (plist-get result :output))))
      (when child (niwa-execution-process-release child))
      (delete-directory root t)))
  :doc "retains complete characters at the byte limit"
  (let* ((root (make-temp-file "niwa-process-limit-" t))
         (spool (file-name-concat root "output"))
         (niwa-execution-process-output-limit 4)
         (niwa-execution-process--child-kill-delay 0.05)
         result
         (child
          (niwa-execution-process-create
           :workdir root :spool-path spool
           :terminal-function (lambda (_child value) (setq result value)))))
    (unwind-protect
        (progn
          (write-region "" nil spool nil 'silent)
          (niwa-execution-process-start
           child :name "niwa-test-process-limit"
           :command '("sh" "-c" "printf 'a\\342\\202\\254b'")
           :coding 'utf-8-unix)
          (test-niwa-execution-process--wait (lambda () result))
          (should (equal (concat "a" (string #x20ac))
                         (plist-get result :output)))
          (should (= 4 (plist-get result :output-bytes)))
          (should (plist-get result :output-limit-p)))
      (niwa-execution-process-release child)
      (delete-directory root t)))
  :doc "settles a spool write failure without retaining output"
  (let* ((root (make-temp-file "niwa-process-write-failure-" t))
         (spool-root (file-name-concat root "spool"))
         (spool (file-name-concat spool-root "output"))
         (niwa-execution-process--child-kill-delay 0.05)
         result
         (child
          (niwa-execution-process-create
           :workdir root :spool-path spool
           :terminal-function (lambda (_child value) (setq result value)))))
    (unwind-protect
        (progn
          (make-directory spool-root)
          (write-region "" nil spool nil 'silent)
          (delete-directory spool-root t)
          (niwa-execution-process-start
           child :name "niwa-test-process-write-failure"
           :command '("sh" "-c" "printf lost; sleep 30")
           :coding 'utf-8-unix)
          (test-niwa-execution-process--wait (lambda () result))
          (should (plist-get result :error))
          (should (eq 'output-write-failed
                      (plist-get result :termination)))
          (should (= 0 (plist-get result :output-bytes))))
      (niwa-execution-process-release child)
      (delete-directory root t)))
  :doc "scopes a remote direct-async cache property to one spawn"
  (let* ((root (make-temp-file "niwa-process-property-" t))
         (spool (file-name-concat root "output"))
         (remote "/ssh:host:/srv/project/")
         (prefix (file-remote-p remote))
         (prior-direct-async '("-t" "-t"))
         (initial-properties
          `((,(regexp-quote prefix) "direct-async" ,prior-direct-async)
            ("existing" "unrelated" keep)))
         (tramp-connection-properties (copy-tree initial-properties))
         (tramp-cache-data (make-hash-table :test #'equal))
         (real-make-process (symbol-function 'make-process))
         child process spawn-direct-async)
    (let ((vec (tramp-dissect-file-name prefix)))
      (should (equal prior-direct-async
                     (tramp-get-method-parameter vec 'tramp-direct-async)))
      (unwind-protect
          (cl-letf
              (((symbol-function
                 'niwa-execution-process--localize-command)
                (lambda (command _workdir _target) command))
               ((symbol-function
                 'niwa-execution-process--direct-async-p)
                (lambda (&rest _args) t))
               ((symbol-function
                 'niwa-execution-process--remote-command)
                (lambda (_child command) command))
               ((symbol-function 'executable-find)
                (lambda (&rest _args) "/bin/sh"))
               ((symbol-function 'make-process)
                (lambda (&rest _args)
                  (setq spawn-direct-async
                        (tramp-get-method-parameter vec 'tramp-direct-async)
                        process
                        (let ((default-directory temporary-file-directory))
                          (funcall real-make-process
                                   :name "niwa-test-scoped-spawn"
                                   :buffer nil
                                   :command '("sh" "-c" "sleep 10")
                                   :noquery t :sentinel #'ignore)))
                  process)))
            (write-region "" nil spool nil 'silent)
            (setq child
                  (niwa-execution-process-create
                   :workdir remote :spool-path spool))
            (niwa-execution-process-start
             child :name "niwa-test-scoped-spawn"
             :command '("sh" "-c" "true") :coding 'utf-8-unix)
            (should (eq t spawn-direct-async))
            (should (equal prior-direct-async
                           (tramp-get-method-parameter
                            vec 'tramp-direct-async)))
            (should (equal initial-properties tramp-connection-properties)))
        (when child (niwa-execution-process-release child))
        (when (process-live-p process) (delete-process process))
        (delete-directory root t)))))

(niwa-deftest niwa-execution-process-stop ()
  ,test
  (test)
  :doc "terminates a live child and preserves the latched reason"
  (let* ((root (make-temp-file "niwa-process-stop-" t))
         (spool (file-name-concat root "output"))
         (niwa-execution-process--child-kill-delay 0.05)
         result
         (child
          (niwa-execution-process-create
           :workdir root :spool-path spool
           :terminal-function (lambda (_child value) (setq result value)))))
    (unwind-protect
        (progn
          (write-region "" nil spool nil 'silent)
          (niwa-execution-process-start
           child :name "niwa-test-process-stop"
           :command '("sh" "-c" "trap '' TERM; while :; do sleep 1; done")
           :coding 'utf-8-unix)
          (should (niwa-execution-process-stop child 'stopped))
          (should-not (niwa-execution-process-stop child 'timed-out))
          (test-niwa-execution-process--wait (lambda () result))
          (should (eq 'stopped (plist-get result :termination)))
          (should-not (zerop (plist-get result :exit-code))))
      (niwa-execution-process-release child)
      (delete-directory root t)))

  :doc "also terminates descendants that moved into their own process group"
  ;; GNU `timeout' calls setpgid (0, 0) unless --foreground, so a signal to
  ;; the shell's group alone never reaches it or the command it supervises.
  (dolist (case '((escaped . "timeout 60 sh -c 'echo $$ > \"$0\"; exec sleep 600' \"$0\"; echo done")
                  (escaped-remote-default . "timeout 60 sh -c 'echo $$ > \"$0\"; exec sleep 600' \"$0\"; echo done")
                  (escaped-ignoring-term
                   . "timeout 60 sh -c 'trap \"\" TERM; echo $$ > \"$0\"; while :; do sleep 1; done' \"$0\"; echo done")
                  (control . "sh -c 'echo $$ > \"$0\"; exec sleep 600' \"$0\"; echo done")))
    (let* ((root (make-temp-file "niwa-process-stop-" t))
           (spool (file-name-concat root "output"))
           (pid-file (file-name-concat root "pid"))
           (niwa-execution-process--child-kill-delay 0.05)
           result pid
           (child
            (niwa-execution-process-create
             :workdir root :spool-path spool
             :terminal-function (lambda (_child value) (setq result value)))))
      (unwind-protect
          (progn
            (write-region "" nil spool nil 'silent)
            (niwa-execution-process-start
             child :name "niwa-test-process-stop-descendant"
             :command (list "sh" "-c" (cdr case) pid-file)
             :coding 'utf-8-unix)
            (test-niwa-execution-process--wait
             (lambda ()
               (and (file-exists-p pid-file)
                    (setq pid (string-to-number
                               (with-temp-buffer
                                 (insert-file-contents pid-file)
                                 (buffer-string))))
                    (> pid 0))))
            (should (process-attributes pid))
            (let* ((remote (eq (car case) 'escaped-remote-default))
                   (default-directory
                    (if remote "/ssh:process-table:/project/" default-directory))
                   (file-name-handler-alist
                    (cons
                     (cons "\\`/ssh:process-table:"
                           (lambda (operation &rest _args)
                             (pcase operation
                               ('file-remote-p "/ssh:process-table:")
                               ((or 'list-system-processes 'process-attributes) nil)
                               (_ (error "Unexpected remote operation: %S" operation)))))
                     file-name-handler-alist)))
              ;; An empty remote table must not hide the real local descendant.
              (when remote
                (should-not (list-system-processes))
                (should-not (process-attributes pid)))
              (should (niwa-execution-process-stop child 'stopped)))
            (test-niwa-execution-process--wait (lambda () result))
            ;; The record settles only once its descendants are gone.
            (should-not (member (alist-get 'state (process-attributes pid))
                                '("S" "R" "D" "T")))
            (should (eq 'stopped (plist-get result :termination))))
        (when (and pid (process-attributes pid))
          (ignore-errors (signal-process pid 'KILL)))
        (niwa-execution-process-release child)
        (delete-directory root t)))))

(niwa-deftest niwa-execution-process--process-table
  (:doc "indexes parent links and ignores vanished processes or missing parents")
  (let ((attrs '((ppid . 1) (pgrp . 2))))
    (cl-letf (((symbol-function 'list-system-processes) (lambda () '(2 3 4)))
              ((symbol-function 'process-attributes)
               (lambda (pid)
                 (pcase pid (2 attrs) (3 '((pgrp . 3)))))))
      (let ((table (niwa-execution-process--process-table)))
        (should (equal attrs (gethash 2 (car table))))
        (should (= 1 (hash-table-count (car table))))
        (should (equal '(2) (gethash 1 (cdr table))))
        (should (= 1 (hash-table-count (cdr table))))))))

(niwa-deftest niwa-execution-process--start-identity ()
  ,test
  (test)
  :doc "stays equal across reads for one process and differs between processes"
  (let* ((pid (emacs-pid))
         (first (niwa-execution-process--start-identity
                 pid (process-attributes pid))))
    (should first)
    (dotimes (_ 200)
      (should (equal first (niwa-execution-process--start-identity
                            pid (process-attributes pid)))))
    (let ((process (make-process :name "niwa-test-start-identity"
                                 :command '("sleep" "30") :noquery t
                                 :connection-type 'pipe)))
      (unwind-protect
          (should-not
           (equal first (niwa-execution-process--start-identity
                         (process-id process)
                         (process-attributes (process-id process)))))
        (delete-process process))))
  :doc "uses the stable host start attribute when /proc evidence is unavailable"
  (cl-letf (((symbol-function 'niwa--process-identity) (lambda (_pid) nil)))
    (should (equal '(123 456)
                   (niwa-execution-process--start-identity
                    42 '((start . (123 456))))))))

(niwa-deftest niwa-execution-process--delete-spool ()
  ,test
  (test)

  :doc "a busy transport defers the deletion, whatever the path looks like"
  ;; Deciding this from the spelling was wrong twice: `file-remote-p' on
  ;; the name as passed, then on its expansion.  Only the transport
  ;; matters, so a plainly local name defers too when the connection is
  ;; mid-command.
  (dolist (path '("/niwamock:host:/srv/project/child.spool"
                  "/tmp/niwa-execution-output-abc"))
    (let (deferred-thunk deleted)
      (cl-letf (((symbol-function 'niwa-transport-busy-p)
                 (lambda (&optional _) t))
                ((symbol-function 'niwa-transport-run-when-idle)
                 (lambda (_key _path thunk &optional _on-cancel)
                   (setq deferred-thunk thunk)
                   t))
                ((symbol-function 'delete-file)
                 (lambda (name &optional _) (setq deleted name))))
        (niwa-execution-process--delete-spool path)
        (should-not deleted)
        (should (functionp deferred-thunk))
        (funcall deferred-thunk)
        (should (equal path deleted)))))

  :doc "an idle transport deletes immediately"
  (let (scheduled deleted)
    (cl-letf (((symbol-function 'niwa-transport-busy-p)
               (lambda (&optional _) nil))
              ((symbol-function 'niwa-transport-run-when-idle)
               (lambda (&rest _) (setq scheduled t) t))
              ((symbol-function 'delete-file)
               (lambda (name &optional _) (setq deleted name))))
      (niwa-execution-process--delete-spool "/tmp/spool")
      (should-not scheduled)
      (should (equal "/tmp/spool" deleted))))

  :doc "a disabled transport deletes rather than leaking the spool"
  ;; `niwa-transport-run-when-idle' returns nil when integration is
  ;; off, and drops the work with it.
  (let (deleted)
    (cl-letf (((symbol-function 'niwa-transport-busy-p)
               (lambda (&optional _) t))
              ((symbol-function 'niwa-transport-run-when-idle)
               (lambda (&rest _) nil))
              ((symbol-function 'delete-file)
               (lambda (name &optional _) (setq deleted name))))
      (niwa-execution-process--delete-spool "/tmp/spool")
      (should (equal "/tmp/spool" deleted)))))

(provide 'test-niwa-execution-process)
;;; test-niwa-execution-process.el ends here
