;;; test-mevedel-execution-process.el -- Process lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the opaque child process, spool, and process-group owner.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution-process)
(require 'tramp-sh)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-execution-process--wait (predicate)
  "Wait until PREDICATE returns non-nil."
  (with-timeout (5 (error "Process did not settle"))
    (while (not (funcall predicate))
      (accept-process-output nil 0.02))))

(mevedel-deftest mevedel-execution-process--direct-async-p ()
  ,test
  (test)
  :doc "gates the private channel on flag, tty, capability, and size"
  (let* ((remote "/ssh:user@host:/srv/project/")
         (command '("sh" "-c" "printf ok"))
         (child (mevedel-execution-process-create :workdir remote))
         (tty-child
          (mevedel-execution-process-create :workdir remote :tty t)))
    (unwind-protect
        (progn
          (should
           (mevedel-execution-process--direct-async-p
            child command remote))
          (let ((mevedel-execution-process-remote-direct-async nil))
            (should-not
             (mevedel-execution-process--direct-async-p
              child command remote)))
          (should-not
           (mevedel-execution-process--direct-async-p
            tty-child command remote))
          (should-not
           (mevedel-execution-process--direct-async-p
            child command "/tmp/"))
          (should-not
           (mevedel-execution-process--direct-async-p
            child command "/sshx:user@host:/srv/project/"))
          (should-not
           (mevedel-execution-process--direct-async-p
            child command "/ssh:jump|ssh:host:/srv/project/"))
          (should-not
           (mevedel-execution-process--direct-async-p
            child (list "sh" "-c" (make-string 4096 ?x)) remote)))
      (mevedel-execution-process-release child)
      (mevedel-execution-process-release tty-child))))

(mevedel-deftest mevedel-execution-process--remote-command ()
  ,test
  (test)
  :doc "carries the stable environment only on a direct-async wrapper"
  (let ((direct (mevedel-execution-process-create))
        (classic (mevedel-execution-process-create)))
    (unwind-protect
        (progn
          (setf (mevedel-execution-process--child-direct-async-p direct) t)
          (let ((command
                 (mevedel-execution-process--remote-command
                  direct '("sh" "-c" "printf ok"))))
            (should (equal "env" (car command)))
            (should (member "NO_COLOR=1" command))
            (should (member "setsid" command)))
          (should
           (equal "setsid"
                  (car
                   (mevedel-execution-process--remote-command
                    classic '("sh" "-c" "printf ok"))))))
      (mevedel-execution-process-release direct)
      (mevedel-execution-process-release classic))))

(mevedel-deftest mevedel-execution-process--remote-group-status ()
  ,test
  (test)
  :doc "distinguishes live, settled, and reused process groups"
  (skip-unless (file-directory-p "/proc"))
  (let* ((root (make-temp-file "mevedel-process-group-status-" t))
         (remote-root (format "/mevedelmock:%s:%s/" (system-name) root))
         (pid-file (file-name-concat root "group.pid"))
         (leader
          (make-process
           :name "mevedel-test-group-status" :buffer nil
           :command
           (list "sh" "-c"
                 "setsid sh -c 'echo $$ > \"$1\"; sleep 30' sh \"$1\""
                 "sh" pid-file)
           :connection-type 'pipe :noquery t))
         group-id start-time
         (child (mevedel-execution-process-create :workdir remote-root)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (test-mevedel-execution-process--wait
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
          (setf (mevedel-execution-process--child-group-id child) group-id
                (mevedel-execution-process--child-group-start-time child)
                start-time)
          (should
           (eq 'live
               (mevedel-execution-process--remote-group-status child)))
          (setf (mevedel-execution-process--child-group-start-time child) "0")
          (should
           (eq 'ambiguous
               (mevedel-execution-process--remote-group-status child)))
          (signal-process (- group-id) 'KILL)
          (should
           (eq 'dead
               (mevedel-execution-process--remote-group-status child))))
      (when (process-live-p leader) (delete-process leader))
      (when (and group-id
                 (eq 0 (ignore-errors (signal-process (- group-id) 0))))
        (ignore-errors (signal-process (- group-id) 'KILL)))
      (mevedel-execution-process-release child)
      (delete-directory root t)))
  :doc "refuses to signal after the captured remote group identity changes"
  (let* ((root (make-temp-file "mevedel-process-reused-group-" t))
         (pid-file (file-name-concat root "group.pid"))
         (remote-root (format "/mevedelmock:%s:%s/" (system-name) root))
         (spool (file-name-concat root "output"))
         (original-signal-process (symbol-function 'signal-process))
         (mevedel-execution-process--child-kill-delay 0.02)
         calls child result group-id)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (cl-letf
              (((symbol-function
                 'mevedel-execution-process--remote-group-status)
                (lambda (_child) 'ambiguous))
               ((symbol-function 'signal-process)
                (lambda (&rest args)
                  (push args calls)
                  (apply original-signal-process args))))
            (write-region "" nil spool nil 'silent)
            (setq child
                  (mevedel-execution-process-create
                   :workdir remote-root :spool-path spool
                   :terminal-function
                   (lambda (_child value) (setq result value))))
            (mevedel-execution-process-start
             child :name "mevedel-test-process-reused-group"
             :command
             '("sh" "-c"
               "ps -o pgid= -p $$ | tr -d ' ' > group.pid; trap '' TERM; sleep 30 & wait")
             :coding 'utf-8-unix :timeout 0.05)
            (test-mevedel-execution-process--wait (lambda () result))
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
      (when child (mevedel-execution-process-release child))
      (when (and group-id (> group-id 0))
        (ignore-errors
          (funcall original-signal-process (- group-id) 'KILL)))
      (delete-directory root t)))
  :doc "bounds a timed-out remote child whose stop escalation wedges"
  (let* ((root (make-temp-file "mevedel-process-wedged-" t))
         (pid-file (file-name-concat root "group.pid"))
         (remote-root (format "/mevedelmock:%s:%s/" (system-name) root))
         (spool (file-name-concat root "output"))
         (original-signal-process (symbol-function 'signal-process))
         (mevedel-execution-process--child-kill-delay 0.02)
         (mevedel-execution-process--remote-control-timeout 0.02)
         child result group-id started-at)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (cl-letf
              (((symbol-function 'mevedel-execution-process--start-stop)
                #'ignore))
            (write-region "" nil spool nil 'silent)
            (setq child
                  (mevedel-execution-process-create
                   :workdir remote-root :spool-path spool
                   :terminal-function
                   (lambda (_child value) (setq result value)))
                  started-at (float-time))
            (mevedel-execution-process-start
             child :name "mevedel-test-process-wedged"
             :command
             '("sh" "-c"
               "ps -o pgid= -p $$ | tr -d ' ' > group.pid; sleep 30")
             :coding 'utf-8-unix :timeout 0.05)
            (test-mevedel-execution-process--wait (lambda () result))
            (setq group-id
                  (string-to-number
                   (string-trim
                    (with-temp-buffer
                      (insert-file-contents pid-file)
                      (buffer-string)))))
            (should (< (- (float-time) started-at) 1.0))
            (should (plist-get result :timed-out-p))
            (should (eq 'unknown (plist-get result :termination)))))
      (when child (mevedel-execution-process-release child))
      (when (and group-id (> group-id 0))
        (ignore-errors
          (funcall original-signal-process (- group-id) 'KILL)))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-process-start ()
  ,test
  (test)
  :doc "emits one complete terminal result after a successful child"
  (let* ((root (make-temp-file "mevedel-process-start-" t))
         (spool (file-name-concat root "output"))
         (terminals 0)
         result
         (child
          (mevedel-execution-process-create
           :workdir root :spool-path spool
           :terminal-function
           (lambda (_child value)
             (cl-incf terminals)
             (setq result value)))))
    (unwind-protect
        (progn
          (write-region "" nil spool nil 'silent)
          (mevedel-execution-process-start
           child :name "mevedel-test-process-start"
           :command '("sh" "-c" "printf 'hello world'")
           :coding 'utf-8-unix)
          (test-mevedel-execution-process--wait (lambda () result))
          (accept-process-output nil 0.05)
          (should (= 1 terminals))
          (should (= 0 (plist-get result :exit-code)))
          (should (equal "hello world" (plist-get result :output)))
          (should (= 11 (plist-get result :output-bytes))))
      (mevedel-execution-process-release child)
      (delete-directory root t)))
  :doc "drains filter output when the watchdog observes exit first"
  (let* ((root (make-temp-file "mevedel-process-watchdog-" t))
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
             ((symbol-function 'run-at-time)
              (lambda (time repeat function &rest args)
                (cond
                 ((and (equal time 0.1) (equal repeat 0.1))
                  (setq watch (lambda () (apply function args)))
                  (funcall original-run-at-time 3600 nil #'ignore))
                 ((eq function
                      #'mevedel-execution-process--settle-main-exit)
                  (setq settle (lambda () (apply function args)))
                  (funcall original-run-at-time 3600 nil #'ignore))
                 (t
                  (apply original-run-at-time time repeat function args))))))
          (write-region "" nil spool nil 'silent)
          (setq child
                (mevedel-execution-process-create
                 :workdir root :spool-path spool
                 :terminal-function
                 (lambda (_child value) (setq result value))))
          (mevedel-execution-process-start
           child :name "mevedel-test-process-watchdog"
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
      (when child (mevedel-execution-process-release child))
      (delete-directory root t)))
  :doc "retains complete characters at the byte limit"
  (let* ((root (make-temp-file "mevedel-process-limit-" t))
         (spool (file-name-concat root "output"))
         (mevedel-execution-process-output-limit 4)
         (mevedel-execution-process--child-kill-delay 0.05)
         result
         (child
          (mevedel-execution-process-create
           :workdir root :spool-path spool
           :terminal-function (lambda (_child value) (setq result value)))))
    (unwind-protect
        (progn
          (write-region "" nil spool nil 'silent)
          (mevedel-execution-process-start
           child :name "mevedel-test-process-limit"
           :command '("sh" "-c" "printf 'a\\342\\202\\254b'")
           :coding 'utf-8-unix)
          (test-mevedel-execution-process--wait (lambda () result))
          (should (equal (concat "a" (string #x20ac))
                         (plist-get result :output)))
          (should (= 4 (plist-get result :output-bytes)))
          (should (plist-get result :output-limit-p)))
      (mevedel-execution-process-release child)
      (delete-directory root t)))
  :doc "settles a spool write failure without retaining output"
  (let* ((root (make-temp-file "mevedel-process-write-failure-" t))
         (spool-root (file-name-concat root "spool"))
         (spool (file-name-concat spool-root "output"))
         (mevedel-execution-process--child-kill-delay 0.05)
         result
         (child
          (mevedel-execution-process-create
           :workdir root :spool-path spool
           :terminal-function (lambda (_child value) (setq result value)))))
    (unwind-protect
        (progn
          (make-directory spool-root)
          (write-region "" nil spool nil 'silent)
          (delete-directory spool-root t)
          (mevedel-execution-process-start
           child :name "mevedel-test-process-write-failure"
           :command '("sh" "-c" "printf lost; sleep 30")
           :coding 'utf-8-unix)
          (test-mevedel-execution-process--wait (lambda () result))
          (should (plist-get result :error))
          (should (eq 'output-write-failed
                      (plist-get result :termination)))
          (should (= 0 (plist-get result :output-bytes))))
      (mevedel-execution-process-release child)
      (delete-directory root t)))
  :doc "scopes a remote direct-async cache property to one spawn"
  (let* ((root (make-temp-file "mevedel-process-property-" t))
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
                 'mevedel-execution-process--localize-command)
                (lambda (command _workdir _target) command))
               ((symbol-function
                 'mevedel-execution-process--direct-async-p)
                (lambda (&rest _args) t))
               ((symbol-function
                 'mevedel-execution-process--remote-command)
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
                                   :name "mevedel-test-scoped-spawn"
                                   :buffer nil
                                   :command '("sh" "-c" "sleep 10")
                                   :noquery t :sentinel #'ignore)))
                  process)))
            (write-region "" nil spool nil 'silent)
            (setq child
                  (mevedel-execution-process-create
                   :workdir remote :spool-path spool))
            (mevedel-execution-process-start
             child :name "mevedel-test-scoped-spawn"
             :command '("sh" "-c" "true") :coding 'utf-8-unix)
            (should (eq t spawn-direct-async))
            (should (equal prior-direct-async
                           (tramp-get-method-parameter
                            vec 'tramp-direct-async)))
            (should (equal initial-properties tramp-connection-properties)))
        (when child (mevedel-execution-process-release child))
        (when (process-live-p process) (delete-process process))
        (delete-directory root t)))))

(mevedel-deftest mevedel-execution-process-stop ()
  ,test
  (test)
  :doc "terminates a live child and preserves the latched reason"
  (let* ((root (make-temp-file "mevedel-process-stop-" t))
         (spool (file-name-concat root "output"))
         (mevedel-execution-process--child-kill-delay 0.05)
         result
         (child
          (mevedel-execution-process-create
           :workdir root :spool-path spool
           :terminal-function (lambda (_child value) (setq result value)))))
    (unwind-protect
        (progn
          (write-region "" nil spool nil 'silent)
          (mevedel-execution-process-start
           child :name "mevedel-test-process-stop"
           :command '("sh" "-c" "trap '' TERM; while :; do sleep 1; done")
           :coding 'utf-8-unix)
          (should (mevedel-execution-process-stop child 'stopped))
          (should-not (mevedel-execution-process-stop child 'timed-out))
          (test-mevedel-execution-process--wait (lambda () result))
          (should (eq 'stopped (plist-get result :termination)))
          (should-not (zerop (plist-get result :exit-code))))
      (mevedel-execution-process-release child)
      (delete-directory root t))))

(provide 'test-mevedel-execution-process)
;;; test-mevedel-execution-process.el ends here
