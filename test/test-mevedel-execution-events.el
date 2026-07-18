;;; test-mevedel-execution-events.el --- Execution event lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests observation races, progress events, final claims, and mailbox delivery.

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
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-execution-test-helpers"))

(mevedel-deftest mevedel-execution-start-bash ()
  ,test
  (test)
  :doc "requires a session owner before spawning managed Bash"
  (let ((mevedel-sandbox-mode 'off))
    (should-error
     (mevedel-execution-start-bash
      #'ignore :session nil :owner "main"
      :command '("sh" "-c" "printf unreachable")
      :workdir temporary-file-directory
      :writable-roots (list temporary-file-directory))
     :type 'mevedel-execution-error))
  :doc "returns terminal output before yielding and removes the temporary spool"
  (let* ((root (make-temp-file "mevedel-managed-terminal-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         observation)
    (unwind-protect
        (progn
          (setq observation
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "printf done")
                 :yield-time-ms nil))
          (should (equal "done" (plist-get observation :output)))
          (should (eq 'completed
                      (plist-get (plist-get observation :facts) :state)))
          (should-not (plist-get (plist-get observation :facts)
                                 :execution-id))
          (should-not (plist-get (plist-get observation :facts)
                                 :output-path))
          (should-not
           (directory-files (file-name-concat root "artifacts")
                            nil "\\`execution-")))
      (delete-directory root t)))
  :doc "retains the adapter outcome resolver across yield and observation"
  (let* ((root (make-temp-file "mevedel-managed-outcome-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         initial final)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep .05; exit 1")
                 :outcome-function
                 (lambda (exit-code termination)
                   (if (and (= exit-code 1) (eq termination 'exited))
                       'false
                     'failure))))
          (should (eq 'running
                      (plist-get (plist-get initial :facts) :state)))
          (setq final
                (test-mevedel-execution--observe
                 session
                 (plist-get (plist-get initial :facts) :execution-id)))
          (should (= 1 (plist-get (plist-get final :facts) :exit-code)))
          (should (eq 'false
                      (plist-get (plist-get final :facts) :outcome))))
      (delete-directory root t)))
  :doc "emits throttled progress with tail, counters, timeout, and yielded id"
  (let* ((root (make-temp-file "mevedel-managed-progress-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (default-progress-delay mevedel-execution-progress-delay)
         (default-progress-interval mevedel-execution-progress-interval)
         (mevedel-execution-progress-delay 0.02)
         (mevedel-execution-progress-interval 0.05)
         (origin-buffer (current-buffer))
         events initial final)
    (unwind-protect
        (let ((mevedel-execution-event-functions
               (list (lambda (event) (push event events) nil))))
          (should (= 2 default-progress-delay))
          (should (= 0.25 default-progress-interval))
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c" "printf start; sleep .65; printf end")
                 :timeout 3 :tool-args '(:command "progress")
                 :tool-use-id "call-progress"
                 :data-buffer origin-buffer))
          (let ((id (plist-get (plist-get initial :facts) :execution-id)))
            (test-mevedel-execution--wait
             (lambda ()
               (>= (length (cl-remove-if-not
                            (lambda (event)
                              (eq (plist-get event :type) 'progress))
                            events))
                   2)))
            (setq final (test-mevedel-execution--observe session id))
            (let ((progress (nreverse
                             (cl-remove-if-not
                              (lambda (event)
                                (eq (plist-get event :type) 'progress))
                              events))))
              (should (cl-every
                       (lambda (pair)
                         (>= (- (plist-get (cadr pair) :emitted-at)
                                (plist-get (car pair) :emitted-at))
                             0.24))
                       (cl-mapcar #'list progress (cdr progress))))
              (let* ((event (car (last progress)))
                     (facts (plist-get event :facts)))
                (should (equal "call-progress"
                               (plist-get event :tool-use-id)))
                (should (eq origin-buffer (plist-get event :data-buffer)))
                (should (equal '(:command "progress")
                               (plist-get event :tool-args)))
                (should (string-match-p "start" (plist-get event :output-tail)))
                (should (equal id (plist-get facts :execution-id)))
                (should (> (plist-get facts :output-bytes) 0))
                (should (> (plist-get facts :output-lines) 0))
                (should (= 3 (plist-get event :timeout-seconds)))))))
      (delete-directory root t)))
  :doc "retains one oversized pre-yield artifact with a head-and-tail preview"
  (let* ((root (make-temp-file "mevedel-managed-oversized-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution-inline-output-limit 10)
         observation)
    (unwind-protect
        (progn
          (setq observation
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c" "printf 1234567890abcdefghij")
                 :yield-time-ms nil))
          (should (string-prefix-p "12345" (plist-get observation :output)))
          (should (string-suffix-p "fghij" (plist-get observation :output)))
          (should (string-match-p "omitted 10 chars"
                                  (plist-get observation :output)))
          (should (= 10 (plist-get (plist-get observation :facts)
                                   :omitted-output-bytes)))
          (let ((path (plist-get (plist-get observation :facts)
                                 :output-path)))
            (should (file-exists-p path))
            (should (equal "1234567890abcdefghij"
                           (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))))))
      (delete-directory root t)))
  :doc "preserves split UTF-8 characters in pipe and PTY filter chunks"
  (let* ((root (make-temp-file "mevedel-managed-utf8-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off))
    (unwind-protect
        (dolist (tty '(nil t))
          (let (initial final id)
            (setq initial
                  (test-mevedel-execution--start-managed
                   session root
                   '("sh" "-c"
                     "printf '\\342'; sleep .05; printf '\\202'; sleep .05; printf '\\254'")
                   :tty tty))
            (setq id (plist-get (plist-get initial :facts) :execution-id))
            (should (equal "" (plist-get initial :output)))
            (setq final (test-mevedel-execution--observe session id))
            (should (equal (string #x20ac) (plist-get final :output)))
            (should (eq (and tty t)
                        (plist-get (plist-get final :facts) :tty)))))
      (delete-directory root t)))
  :doc "line-buffered programs expose progress only through a PTY"
  (skip-unless (executable-find "python3"))
  (let* ((root (make-temp-file "mevedel-managed-buffering-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off))
    (unwind-protect
        (dolist (tty '(nil t))
          (let (initial final id)
            (setq initial
                  (test-mevedel-execution--start-managed
                   session root
                   '("python3" "-c"
                     "import time; print('ready'); time.sleep(1); print('done')")
                   :tty tty :yield-time-ms 100))
            (setq id (plist-get (plist-get initial :facts) :execution-id))
            (if tty
                (should (string-match-p "ready" (plist-get initial :output)))
              (should (equal "" (plist-get initial :output))))
            (setq final (test-mevedel-execution--observe session id))
            (should (string-match-p "done" (plist-get final :output)))))
      (delete-directory root t)))
  :doc "yields a stable owner-scoped id and retained session artifact"
  (let* ((root (make-temp-file "mevedel-managed-yield-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         observation)
    (unwind-protect
        (progn
          (setq observation
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c" "printf first; sleep 2; printf second")
                 :owner "agent--one"))
          (let* ((facts (plist-get observation :facts))
                 (id (plist-get facts :execution-id))
                 (path (plist-get facts :output-path)))
            (should (stringp id))
            (should (eq 'running (plist-get facts :state)))
            (should (file-in-directory-p
                     path (file-name-concat root "artifacts")))
            (should (file-exists-p path))
            (should (equal id
                           (plist-get
                            (car (mevedel-execution-list
                                  session "agent--one"))
                            :execution-id)))
            (test-mevedel-execution--stop-all
             session "agent--one" (list id))))
      (delete-directory root t)))
  :doc "foreground request abort stops work while yielded work survives it"
  (let* ((root (make-temp-file "mevedel-managed-abort-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         (foreground-request (mevedel-request--create :session session))
         (yielded-request (mevedel-request--create :session session))
         foreground yielded foreground-id yielded-id)
    (unwind-protect
        (progn
          (mevedel-execution-start-bash
           (lambda (value) (setq foreground value))
           :session session :owner "main" :request foreground-request
           :command '("sh" "-c" "sleep 30")
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :yield-time-ms 1000)
          (mevedel-request-drain-cancellers foreground-request)
          (test-mevedel-execution--wait (lambda () foreground))
          (setq foreground-id
                (plist-get (plist-get foreground :facts) :execution-id))
          (should-not foreground-id)
          (should (eq 'aborted
                      (plist-get (plist-get foreground :facts)
                                 :termination)))
          (mevedel-execution-start-bash
           (lambda (value) (setq yielded value))
           :session session :owner "main" :request yielded-request
           :command '("sh" "-c" "sleep 30")
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :yield-time-ms 10)
          (test-mevedel-execution--wait (lambda () yielded))
          (setq yielded-id
                (plist-get (plist-get yielded :facts) :execution-id))
          (mevedel-request-drain-cancellers yielded-request)
          (should (equal yielded-id
                         (plist-get
                          (car (mevedel-execution-list session "main"))
                          :execution-id)))
          (test-mevedel-execution--stop-all
           session "main" (list yielded-id)))
      (delete-directory root t)))
  :doc "timeout and output caps remain active after yielding"
  (let* ((root (make-temp-file "mevedel-managed-limits-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution-output-limit 64)
         (mevedel-execution--child-kill-delay 0.05)
         initial final id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c" "printf before; sleep .05; head -c 10000 /dev/zero | tr '\\0' x")
                 :timeout 5))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (setq final (test-mevedel-execution--observe session id))
          (should (eq 'output-limit
                      (plist-get (plist-get final :facts) :termination)))
          (should (= 64 (plist-get (plist-get final :facts)
                                   :output-bytes)))
          (should (file-exists-p
                   (plist-get (plist-get final :facts) :output-path)))
          (dolist (tty '(nil t))
            (let ((mevedel-execution-output-limit 4096)
                  timeout-initial timeout-final timeout-id)
              (setq timeout-initial
                    (test-mevedel-execution--start-managed
                     session root
                     '("sh" "-c" "printf before-timeout; sleep 30")
                     :timeout 0.1 :tty tty))
              (setq timeout-id
                    (plist-get
                     (plist-get timeout-initial :facts) :execution-id))
              (setq timeout-final
                    (test-mevedel-execution--observe session timeout-id))
              (should (eq 'timed-out
                          (plist-get (plist-get timeout-final :facts)
                                     :termination)))
              (should (eq (and tty t)
                          (plist-get (plist-get timeout-final :facts) :tty)))
              (should (string-match-p
                       "before-timeout" (plist-get timeout-initial :output)))
              (should (equal "" (plist-get timeout-final :output))))))
      (delete-directory root t)))
  :doc "cleans descendants when the managed shell exits"
  (let* ((root (make-temp-file "mevedel-managed-descendant-" t))
         (session (test-mevedel-execution--session root))
         (pid-file (file-name-concat root "child.pid"))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         observation pid)
    (unwind-protect
        (progn
          (setq observation
                (test-mevedel-execution--start-managed
                 session root
                 (list "sh" "-c"
                       "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; exit 0"
                       "managed-descendant" pid-file)
                 :yield-time-ms nil))
          (setq pid (test-mevedel-execution--read-pid pid-file))
          (test-mevedel-execution--wait
           (lambda () (test-mevedel-execution--process-gone-p pid)))
          (should (= 0 (plist-get (plist-get observation :facts)
                                  :exit-code))))
      (delete-directory root t)))
  :doc "skips recurring progress when no transcript row can consume it"
  (let* ((root (make-temp-file "mevedel-managed-no-progress-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         (mevedel-execution-event-functions (list #'ignore))
         initial id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep 30")))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (let ((record
                 (gethash
                  id
                  (mevedel-execution--state-records
                   (mevedel-session-execution-state session)))))
            (should record)
            (should-not
             (mevedel-execution--record-progress-timer record)))
          (test-mevedel-execution--stop-all session "main" (list id)))
      (delete-directory root t)))
  :doc "refuses a sixty-fifth live managed process without eviction"
  (let* ((root (make-temp-file "mevedel-managed-limit-" t))
         (session (test-mevedel-execution--session root))
         (artifacts (file-name-concat root "artifacts"))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         ids)
    (unwind-protect
        (progn
          (dotimes (_ 64)
            (let ((observation
                   (test-mevedel-execution--start-managed
                    session root '("sh" "-c" "sleep 30"))))
              (push (plist-get (plist-get observation :facts) :execution-id)
                    ids)))
          (should-error
           (mevedel-execution-start-bash
            #'ignore :session session :owner "main"
            :command '("sh" "-c" "sleep 30")
            :workdir root :writable-roots (list root)
            :artifact-directory artifacts :yield-time-ms 10)
           :type 'mevedel-execution-limit)
          (should (= 64 (length (mevedel-execution-list session "main"))))
          (test-mevedel-execution--stop-all session "main" ids))
      (delete-directory root t))))


(mevedel-deftest mevedel-execution-observe ()
  ,test
  (test)
  :doc "polls only unread output and rejects ordinary pipe input"
  (let* ((root (make-temp-file "mevedel-managed-observe-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         initial first final id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c" "printf one; sleep 1; printf two")))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (should (equal "one" (plist-get initial :output)))
          (should-error
           (mevedel-execution-observe
            session "main" id #'ignore :chars "input" :wait-ms 250)
           :type 'mevedel-execution-input-error)
          (setq first
                (test-mevedel-execution--observe
                 session id :wait-ms 10))
          (should (equal "" (plist-get first :output)))
          (setq final (test-mevedel-execution--observe session id))
          (should (equal "two" (plist-get final :output)))
          (should (plist-get final :claimed-final-p))
          (should (eq 'completed
                      (plist-get (plist-get final :facts) :state)))
          (should-error
           (mevedel-execution-observe session "main" id #'ignore)
           :type 'mevedel-execution-not-found))
      (delete-directory root t)))
  :doc "writes repeated PTY input and returns only newly unread output"
  (let* ((root (make-temp-file "mevedel-managed-pty-input-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         initial first pending second final id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c"
                   "test -t 0 && test -t 1 || exit 9; printf 'start> '; while IFS= read -r line; do printf 'got:%s\\nnext> ' \"$line\"; done; printf eof")
                 :tty t))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (should (string-match-p "start>" (plist-get initial :output)))
          (should (plist-get (plist-get initial :facts) :tty))
          (setq first
                (test-mevedel-execution--observe
                 session id :chars "one\n" :wait-ms 250))
          (should (string-match-p "got:one" (plist-get first :output)))
          (should-not (string-match-p "start>" (plist-get first :output)))
          (mevedel-execution-observe
           session "main" id (lambda (value) (setq pending value))
           :wait-ms 300000)
          (setq second
                (test-mevedel-execution--observe
                 session id :chars "two\n" :wait-ms 250))
          (should (eq 'running
                      (plist-get (plist-get pending :facts) :state)))
          (should (string-match-p "got:two" (plist-get second :output)))
          (should-not (string-match-p "got:one" (plist-get second :output)))
          (setq final
                (test-mevedel-execution--observe
                 session id :chars (string 4)))
          (should (string-match-p "eof" (plist-get final :output)))
          (should-not (string-match-p "got:two" (plist-get final :output)))
          (should (eq 'completed
                      (plist-get (plist-get final :facts) :state))))
      (delete-directory root t)))
  :doc "Ctrl-C interrupts the execution group in pipe and PTY modes"
  (dolist (tty '(nil t))
    (let* ((root (make-temp-file "mevedel-managed-interrupt-" t))
           (session (test-mevedel-execution--session root))
           (mevedel-sandbox-mode 'off)
           (mevedel-execution--child-kill-delay 0.05)
           initial pending final id)
      (unwind-protect
          (progn
            (setq initial
                  (test-mevedel-execution--start-managed
                   session root '("sh" "-c" "printf ready; exec sleep 30")
                   :tty tty))
            (setq id (plist-get (plist-get initial :facts) :execution-id))
            (mevedel-execution-observe
             session "main" id (lambda (value) (setq pending value))
             :wait-ms 300000)
            (setq final
                  (test-mevedel-execution--observe
                   session id :chars (string 3)))
            (should (eq 'running
                        (plist-get (plist-get pending :facts) :state)))
            (should (= 2 (plist-get (plist-get final :facts) :exit-code)))
            (should (eq 'signaled
                        (plist-get (plist-get final :facts) :termination)))
            (should (eq 'completed
                        (plist-get (plist-get final :facts) :state)))
            (should (eq (and tty t)
                        (plist-get (plist-get final :facts) :tty))))
        (delete-directory root t))))
  :doc "Ctrl-C stops a Bash loop through confined pipe and PTY transports"
  (skip-unless (plist-get (mevedel-sandbox-probe) :available))
  (dolist (tty '(nil t))
    (let* ((root (make-temp-file "mevedel-managed-confined-interrupt-" t))
           (session (test-mevedel-execution--session root))
           (mevedel-sandbox-mode 'required)
           (initial
            (test-mevedel-execution--start-managed
             session root
             '("bash" "-lc"
               "trap 'printf \"interrupted\\n\"; exit 130' INT; while :; do printf 'heartbeat\\n'; sleep 1; done")
             :timeout 120 :tty tty))
           (id (plist-get (plist-get initial :facts) :execution-id))
           final)
      (unwind-protect
          (progn
            (test-mevedel-execution--observe session id :wait-ms 1200)
            (setq final
                  (test-mevedel-execution--observe
                   session id :chars (string 3)))
            (should (= 130 (plist-get (plist-get final :facts) :exit-code)))
            (should (eq 'exited
                        (plist-get (plist-get final :facts) :termination)))
            (should (eq 'completed
                        (plist-get (plist-get final :facts) :state))))
        (delete-directory root t))))
  :doc "keeps trapped and late Ctrl-C exit status distinct from signals"
  (dolist (command '(("sh" "-c"
                      "trap 'exit 2' INT; printf ready; while :; do sleep 1; done")
                     ("sh" "-c" "printf ready; sleep .05; exit 2")))
    (let* ((root (make-temp-file "mevedel-managed-exit-two-" t))
           (session (test-mevedel-execution--session root))
           (mevedel-sandbox-mode 'off)
           (initial
            (test-mevedel-execution--start-managed session root command))
           (id (plist-get (plist-get initial :facts) :execution-id))
           final)
      (unwind-protect
          (progn
            (when (string-match-p "sleep .05" (car (last command)))
              (test-mevedel-execution--wait
               (lambda ()
                 (mevedel-execution--record-finished-p
                  (gethash
                   id
                   (mevedel-execution--state-records
                    (mevedel-session-execution-state session)))))))
            (setq final
                  (test-mevedel-execution--observe
                   session id :chars (string 3)))
            (should (= 2 (plist-get (plist-get final :facts) :exit-code)))
            (should (eq 'exited
                        (plist-get (plist-get final :facts) :termination))))
        (delete-directory root t))))
  :doc "independent completion waits for a model observer instead of recalling Bash"
  (let* ((root (make-temp-file "mevedel-managed-independent-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         (calls 0) initial final id)
    (unwind-protect
        (progn
          (mevedel-execution-start-bash
           (lambda (value)
             (setq calls (1+ calls)
                   initial value))
           :session session :owner "main"
           :command '("sh" "-c" "sleep .05; printf finished")
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :yield-time-ms 10)
          (test-mevedel-execution--wait (lambda () initial))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (test-mevedel-execution--wait
           (lambda ()
             (mevedel-execution--record-finished-p
              (gethash
               id
               (mevedel-execution--state-records
                (mevedel-session-execution-state session))))))
          (should-not (mevedel-execution-list session "main"))
          (should (= 1 calls))
          (mevedel-execution-observe
           session "main" id (lambda (value) (setq final value)))
          (should (equal "finished" (plist-get final :output)))
          (should (plist-get final :claimed-final-p)))
      (delete-directory root t)))
  :doc "independent completion secures one mailbox event and retires the handle"
  (let* ((root (make-temp-file "mevedel-managed-mailbox-complete-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         events initial id)
    (unwind-protect
        (let ((mevedel-execution-event-functions
               (list (lambda (event) (push event events))))
              (mevedel-execution-mailbox-delivery-function
               (lambda (_event _context) t)))
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep .05; printf done")
                 :tool-use-id "call-independent"))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (test-mevedel-execution--wait
           (lambda ()
             (cl-find-if
              (lambda (event)
                (and (eq (plist-get event :type) 'terminal)
                     (eq (plist-get event :delivery) 'mailbox)))
              events)))
          (let* ((event (cl-find-if
                         (lambda (candidate)
                           (eq (plist-get candidate :type) 'terminal))
                         events))
                 (observation (plist-get event :observation)))
            (should (equal "done" (plist-get observation :output)))
            (should (equal "done" (plist-get event :whole-output)))
            (should (plist-get observation :claimed-final-p)))
          (should-not (mevedel-execution-list session "main"))
          (should-error
           (mevedel-execution-observe session "main" id #'ignore)
           :type 'mevedel-execution-not-found))
      (delete-directory root t)))
  :doc "mailbox and passive event mutations are isolated from each other"
  (let* ((root (make-temp-file "mevedel-managed-event-isolation-" t))
         (session (test-mevedel-execution--session root))
         (owner-context (list :private-owner t))
         (mevedel-sandbox-mode 'off)
         mailbox-event mailbox-context mailbox-leaf
         passive-before passive-event initial)
    (unwind-protect
        (let ((mevedel-execution-event-functions
               (list
                (lambda (event)
                  (setq passive-event event)
                  (setq passive-before
                        (plist-get (plist-get event :observation) :output))
                  (plist-put (plist-get event :observation)
                             :output "corrupted"))))
              (mevedel-execution-mailbox-delivery-function
               (lambda (event context)
                 (setq mailbox-event event
                       mailbox-context context)
                 (aset (plist-get (plist-get event :observation) :output)
                       0 ?X)
                 (setq mailbox-leaf
                       (plist-get (plist-get event :observation) :output))
                 (plist-put (plist-get event :observation)
                            :output "mailbox-mutated")
                 t)))
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep .05; printf original")
                 :owner-context owner-context))
          (should (plist-get (plist-get initial :facts) :execution-id))
          (test-mevedel-execution--wait (lambda () mailbox-event))
          (should (eq owner-context mailbox-context))
          (should-not (plist-member mailbox-event :owner-context))
          (should-not (plist-member passive-event :owner-context))
          (should (equal "Xriginal" mailbox-leaf))
          (should (equal "mailbox-mutated"
                         (plist-get (plist-get mailbox-event :observation)
                                    :output)))
          (should (equal "original" passive-before))
          (should (equal "corrupted"
                         (plist-get (plist-get passive-event :observation)
                                    :output))))
      (delete-directory root t)))
  :doc "passive listeners cannot acknowledge a rejected mailbox delivery"
  (let* ((root (make-temp-file "mevedel-managed-mailbox-rejected-" t))
         (session (test-mevedel-execution--session root))
         (owner "explorer--rejected")
         (mevedel-sandbox-mode 'off)
         events initial final id)
    (unwind-protect
        (let ((mevedel-execution-event-functions
               (list
                (lambda (event)
                  (push event events)
                  'rerender-timer)))
              (mevedel-execution-mailbox-delivery-function
               (lambda (_event _context) nil)))
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep .05; printf unclaimed")
                 :owner owner))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (test-mevedel-execution--wait
           (lambda ()
             (cl-find-if
              (lambda (event)
                (eq (plist-get event :type) 'terminal))
              events)))
          (let ((record
                 (gethash
                  id
                  (mevedel-execution--state-records
                   (mevedel-session-execution-state session)))))
            (should record)
            (should-not (mevedel-execution--record-delivery-state record))
            (should (mevedel-execution-owner-live-p session owner)))
          (mevedel-execution-observe
           session owner id (lambda (value) (setq final value)))
          (should (equal "unclaimed" (plist-get final :output)))
          (should-not (mevedel-execution-owner-live-p session owner)))
      (delete-directory root t)))
  :doc "a reentrant mailbox observer cannot claim or redeliver the terminal result"
  (let* ((root (make-temp-file "mevedel-managed-reentrant-claim-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         events initial id reentrant-error)
    (unwind-protect
        (let ((mevedel-execution-event-functions
               (list (lambda (event) (push event events))))
              (mevedel-execution-mailbox-delivery-function
               (lambda (_event _context)
                 (condition-case err
                     (mevedel-execution-observe
                      session "main" id #'ignore)
                   (mevedel-execution-not-found
                    (setq reentrant-error err)))
                 (should-error
                  (mevedel-execution-stop-user session id)
                  :type 'mevedel-execution-not-found)
                 t)))
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep .05; printf done")))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (test-mevedel-execution--wait
           (lambda ()
             (cl-find-if
              (lambda (event)
                (eq (plist-get event :type) 'terminal))
              events)))
          (should reentrant-error)
          (should (= 1 (length
                        (cl-remove-if-not
                         (lambda (event)
                           (eq (plist-get event :type) 'terminal))
                         events))))
          (should-error
           (mevedel-execution-observe session "main" id #'ignore)
           :type 'mevedel-execution-not-found))
      (delete-directory root t)))
  :doc "a waiting model observer wins completion without a mailbox duplicate"
  (let* ((root (make-temp-file "mevedel-managed-model-claim-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         events initial final id)
    (unwind-protect
        (let ((mevedel-execution-event-functions
               (list (lambda (event) (push event events))))
              (mevedel-execution-mailbox-delivery-function
               (lambda (_event _context) t)))
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep .05; printf observed")))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (mevedel-execution-observe
           session "main" id (lambda (value) (setq final value))
           :wait-ms 5000)
          (test-mevedel-execution--wait (lambda () final))
          (should (equal "observed" (plist-get final :output)))
          (should (= 1 (length
                        (cl-remove-if-not
                         (lambda (event)
                           (eq (plist-get event :type) 'terminal))
                         events))))
          (should (eq 'model
                      (plist-get
                       (cl-find-if
                        (lambda (event)
                          (eq (plist-get event :type) 'terminal))
                        events)
                       :delivery)))
          (should-not
           (cl-find-if
            (lambda (event) (eq (plist-get event :delivery) 'mailbox))
            events)))
      (delete-directory root t)))
  :doc "bad outcome resolvers cannot block final delivery or handle cleanup"
  (dolist (resolver
           (list (lambda (_exit-code _termination) (error "Resolver failed"))
                 (lambda (_exit-code _termination) 'invalid)))
    (let* ((root (make-temp-file "mevedel-managed-bad-outcome-" t))
           (session (test-mevedel-execution--session root))
           (mevedel-sandbox-mode 'off)
           (initial
            (test-mevedel-execution--start-managed
             session root '("sh" "-c" "sleep .05; exit 1")
             :outcome-function resolver))
           (id (plist-get (plist-get initial :facts) :execution-id))
           final)
      (unwind-protect
          (progn
            (test-mevedel-execution--wait
             (lambda ()
               (mevedel-execution--record-finished-p
                (mevedel-execution--owned-yielded-record
                 session "main" id))))
            (setq final (test-mevedel-execution--observe session id))
            (should (eq 'failure
                        (plist-get (plist-get final :facts) :outcome)))
            (should (plist-get final :claimed-final-p))
            (should-not (mevedel-execution-list session "main")))
        (delete-directory root t))))
  :doc "owner mismatches reveal no execution state"
  (let* ((root (make-temp-file "mevedel-managed-owner-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         initial id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep 30")
                 :owner "agent--one"))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (should-error
           (mevedel-execution-observe session "agent--two" id #'ignore)
           :type 'mevedel-execution-not-found)
          (test-mevedel-execution--stop-all
           session "agent--one" (list id)))
      (delete-directory root t))))

(provide 'test-mevedel-execution-events)
;;; test-mevedel-execution-events.el ends here
