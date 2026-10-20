;;; test-mevedel-monitor.el --- Monitor runtime tests -*- lexical-binding: t -*-

;;; Commentary:
;; Bounded framing, mailbox batching, lifetime, and tool admission.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution)
(require 'mevedel-tool-exec)
(require 'mevedel-view)
(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name
                                   byte-compile-current-file)) "helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name
                                   byte-compile-current-file))
          "mevedel-execution-test-helpers"))

(mevedel-deftest mevedel-monitor-framing ()
  ,test
  (test)
  :doc "frames split CRLF, empty lines and multiple lines with one 200ms timer"
  (let ((record (mevedel-execution--record-create :kind 'monitor)) timers)
    (cl-letf (((symbol-function 'mevedel-transport-run-at-time)
               (lambda (seconds function &rest args)
                 (push (list seconds function args) timers)
                 'pending)))
      (mevedel-execution--monitor-frame record "one\r")
      (should-not timers)
      (mevedel-execution--monitor-frame record "\n\ntwo\npar")
      (mevedel-execution--monitor-frame record "tial")
      (should (equal '("two" "" "one")
                     (mevedel-execution--record-monitor-lines record)))
      (should (equal "partial" (mevedel-execution--record-monitor-partial record)))
      (should (= 3 (mevedel-execution--record-monitor-count record)))
      (should (= 1 (length timers)))
      (should (= 0.2 (caar timers)))))
  :doc "bounds partial lines, counts truncation, and caps UTF-8 batches at 8KiB"
  (let ((record (mevedel-execution--record-create :kind 'monitor)))
    (cl-letf (((symbol-function 'mevedel-transport-run-at-time)
               (lambda (&rest _) 'pending)))
      (mevedel-execution--monitor-frame record (make-string 5000 ?x))
      (should (= 2000 (length (mevedel-execution--record-monitor-partial record))))
      (mevedel-execution--monitor-frame record "\r\n")
      (should (string-suffix-p "[3000 characters truncated]\n"
                               (mevedel-execution--monitor-body record)))
      (dotimes (_ 8)
        (mevedel-execution--monitor-frame record (concat (make-string 2000 ?😀) "\n")))
      (should (<= (string-bytes (mevedel-execution--monitor-body record)) 8192))
      (should (= 8 (mevedel-execution--record-monitor-omitted record)))
      (should (= 9 (mevedel-execution--record-monitor-count record)))))
  :doc "stops at the 101st line in a rolling minute without retaining the rest"
  (let ((record (mevedel-execution--record-create :kind 'monitor))
        (now 100.0) stopped)
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now))
              ((symbol-function 'mevedel-transport-run-at-time)
               (lambda (&rest _) 'pending))
              ((symbol-function 'mevedel-execution--stop-process)
               (lambda (value reason)
                 (setq stopped reason)
                 (setf (mevedel-execution--record-stop-p value) t))))
      (mevedel-execution--monitor-frame record (make-string 100 ?\n))
      (should-not stopped)
      (setq now 161.0)
      (mevedel-execution--monitor-frame record "next\n")
      (should-not stopped)
      (mevedel-execution--monitor-frame record (make-string 1000 ?\n))
      (should (eq 'flood stopped))
      (should (= 201 (mevedel-execution--record-monitor-count record)))
      (should (= 101 (length (mevedel-execution--record-monitor-times record)))))))

(mevedel-deftest mevedel-monitor-batching ()
  ,test
  (test)
  :doc "delivers a single batch, retains failed delivery, and stops on unread flood"
  (let ((record (mevedel-execution--record-create
                 :kind 'monitor :origin (mevedel-execution--origin-create)))
        (unread 0) (accept nil) events stopped timer)
    (cl-letf (((symbol-function 'mevedel-transport-run-at-time)
               (lambda (seconds function &rest args)
                 (should (= seconds 0.2))
                 (setq timer (lambda () (apply function args)))
                 'pending))
              ((symbol-function 'mevedel-execution--event)
               (lambda (_ type &rest properties) (cons :type (cons type properties))))
              ((symbol-function 'mevedel-execution--deliver-mailbox-event)
               (lambda (_ event ack)
                 (if accept
                     (progn (push event events) (funcall ack t nil))
                   (funcall ack nil "Rejected"))
                 nil))
              ((symbol-function 'mevedel-execution--emit-event) #'ignore)
              ((symbol-function 'mevedel-execution--notify-state-change) #'ignore)
              ((symbol-function 'mevedel-agent-control-monitor-unread-bytes)
               (lambda (&rest _) unread))
              ((symbol-function 'mevedel-execution--stop-process)
               (lambda (_ reason) (setq stopped reason))))
      (mevedel-execution--monitor-frame record "a\nb\n")
      (should-not events)
      (funcall timer)
      (should (equal "a\nb\n" (mevedel-execution--monitor-body record)))
      (setq accept t unread 65537)
      (mevedel-execution--monitor-frame record "c\n")
      (funcall timer)
      (should (= 1 (length events)))
      (should (eq 'monitor-batch (plist-get (car events) :type)))
      (should (equal "a\nb\nc\n" (plist-get (car events) :body)))
      (should (equal '("a" "b" "c") (plist-get (car events) :monitor-lines)))
      (should (= 0 (plist-get (car events) :monitor-omitted)))
      (should (string-empty-p (mevedel-execution--monitor-body record)))
      (should (eq 'flood stopped)))))

(mevedel-deftest mevedel-monitor-admission ()
  ,test
  (test)
  :doc "rejects agents and tty before sharing Bash preparation"
  (cl-letf (((symbol-function 'mevedel-current-origin) (lambda () "/root"))
            ((symbol-function 'mevedel-tool-exec--bash)
             (lambda (_ args kind) (list kind args))))
    (should-error
     (mevedel-tool-exec--monitor #'ignore '(:description "ok" :tty :json-false)))
    (should (equal '(monitor (:description "ok"))
                   (mevedel-tool-exec--monitor #'ignore '(:description "ok"))))
    (cl-letf (((symbol-function 'mevedel-current-origin) (lambda () "/root/child")))
      (should-error (mevedel-tool-exec--monitor #'ignore '(:description "ok")))))
  :doc "validates description and lifetime at the execution boundary before launch"
  (let* ((root (make-temp-file "mevedel-monitor-admission-" t))
         (session (test-mevedel-execution--session root)))
    (unwind-protect
        (dolist (case '(((:description nil) "Monitor requires a non-empty description")
                        ((:description "") "Monitor requires a non-empty description")
                        ((:description " \t") "Monitor requires a non-empty description")
                        ((:description "ok" :timeout-ms 999)
                         "Monitor timeout_ms must be between 1000 and 3600000")
                        ((:description "ok" :timeout-ms 3600001)
                         "Monitor timeout_ms must be between 1000 and 3600000")
                        ((:description "ok" :timeout-ms 1000.0)
                         "Monitor timeout_ms must be between 1000 and 3600000")))
          (should
           (equal (list 'error (cadr case))
                  (should-error
                   (apply #'mevedel-execution-start-bash #'ignore
                          :kind 'monitor :session session :owner "/root"
                          :command '("sh" "-c" "exit 0") :workdir root
                          (car case)))))
          (should (zerop (mevedel-execution-count-user session))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t))))

(mevedel-deftest mevedel-monitor-lifetime ()
  ,test
  (test)
  :doc "yields immediately, separates stderr, flushes final partial and notifies once"
  (let* ((root (make-temp-file "mevedel-monitor-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         initial events progress)
    (unwind-protect
        (let ((mevedel-execution-mailbox-delivery-function
               (lambda (event _ ack) (push event events) (funcall ack t nil) t))
              (mevedel-execution-event-functions
               (list (lambda (event)
                       (when (eq (plist-get event :type) 'progress)
                         (push event progress))))))
          (mevedel-execution-start-bash
           (lambda (observation) (setq initial observation))
           :kind 'monitor :description "changes" :session session :owner "/root"
           :owner-context session :command '("sh" "-c" "printf 'warning\\n' >&2; printf 'one\\n'; sleep .4; printf partial")
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts"))
          (should initial)
          (should (eq 'running (plist-get (plist-get initial :facts) :state)))
          (should (plist-get (plist-get initial :facts) :execution-id))
          (should (= 1 (mevedel-execution-count-user session)))
          (should-not (mevedel-execution-working-p))
          (test-mevedel-execution--wait
           (lambda () (cl-find 'terminal events :key (lambda (e) (plist-get e :type)))))
          (should-not progress)
          (let* ((ordered (reverse events))
                 (terminal (car (last ordered)))
                 (facts (plist-get terminal :facts))
                 (body (mevedel-tool-exec--monitor-terminal-body terminal)))
            (should (= 2 (length ordered)))
            (should (equal "one\n" (plist-get (car ordered) :body)))
            (should (equal '("partial") (plist-get terminal :monitor-lines)))
            (should (string-prefix-p "Monitor changes: exited with code 0" body))
            (should (= 2 (plist-get facts :event-count)))
            (should (eq 'monitor (plist-get facts :kind)))
            (should-not (string-match-p "warning" body))
            (should (string-match-p "warning" (plist-get terminal :whole-output)))
            (should (string-prefix-p "<monitor-execution "
                                     (mevedel-tool-exec--execution-facts-xml facts)))
            (accept-process-output nil 0.3)
            (should (= 2 (length events)))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))
  :doc "a process that exits inside startup still yields and sends one terminal notice"
  (let* ((root (make-temp-file "mevedel-monitor-early-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (start (symbol-function 'mevedel-execution-process-start))
         initial events)
    (unwind-protect
        (let ((mevedel-execution-mailbox-delivery-function
               (lambda (event _ ack) (push event events) (funcall ack t nil) t)))
          (cl-letf (((symbol-function 'mevedel-execution-process-start)
                     (lambda (child &rest args)
                       (prog1 (apply start child args)
                         (test-mevedel-execution--wait
                          (lambda () (not (mevedel-execution-process-live-p child))))))))
            (mevedel-execution-start-bash
             (lambda (observation) (push observation initial))
             :kind 'monitor :description "quick" :session session :owner "/root"
             :owner-context session :command '("sh" "-c" "printf done")
             :workdir root :writable-roots (list root)
             :artifact-directory (file-name-concat root "artifacts")))
          (test-mevedel-execution--wait (lambda () events))
          (should (= 1 (length initial)))
          (should (equal "" (plist-get (car initial) :output)))
          ;; The start acknowledgement never claims the started monitor is queued.
          (should (eq 'running (plist-get (plist-get (car initial) :facts) :state)))
          (should (= 1 (length events)))
          (should (eq 'terminal (plist-get (car events) :type)))
          (should (equal '("done") (plist-get (car events) :monitor-lines)))
          (should (string-prefix-p "Monitor quick: exited with code 0"
                                   (mevedel-tool-exec--monitor-terminal-body (car events)))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t))))

(mevedel-deftest mevedel-monitor-quiet-timers ()
  ,test
  (test)
  :doc "a silent monitor with a live tool observer never arms a progress clock"
  (let* ((root (make-temp-file "mevedel-monitor-quiet-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution-progress-delay 0.01)
         (mevedel-execution-progress-interval 0.01)
         initial events)
    (unwind-protect
        (let ((mevedel-execution-event-functions
               (list (lambda (event) (push event events)))))
          (mevedel-execution-start-bash
           (lambda (observation) (setq initial observation))
           :kind 'monitor :description "silent" :tool-use-id "quiet-tool"
           :session session :owner "/root" :owner-context session
           :command '("sh" "-c" "sleep 5") :workdir root
           :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts"))
          (let* ((id (plist-get (plist-get initial :facts) :execution-id))
                 (record (gethash id (mevedel-execution--state-records
                                     (mevedel-session-execution-state session)))))
            (should-not (mevedel-execution--record-progress-timer record))
            (should-not (mevedel-execution--record-yield-timer record))
            (should-not (mevedel-execution--record-monitor-timer record))
            (accept-process-output nil 0.3)
            (should-not (cl-find 'progress events :key (lambda (e) (plist-get e :type))))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t))))

(mevedel-deftest mevedel-monitor-terminal ()
  ,test
  (test)
  :doc "expiry, flood, explicit stop and teardown settle without leaked batch timers"
  (dolist (mode '(expiry flood stop teardown))
    (let* ((root (make-temp-file "mevedel-monitor-stop-" t))
           (session (test-mevedel-execution--session root))
           (mevedel-sandbox-mode 'off)
           initial events stopped)
      (unwind-protect
          (let ((mevedel-execution-mailbox-delivery-function
                 (lambda (event _ ack) (push event events) (funcall ack t nil) t)))
            (mevedel-execution-start-bash
             (lambda (observation) (setq initial observation))
             :kind 'monitor :description "lifetime" :timeout-ms 1000
             :session session :owner "/root" :owner-context session
             :command (list "sh" "-c"
                            (if (eq mode 'flood)
                                "i=0; while test $i -lt 102; do printf 'line\\n'; i=$((i+1)); done; sleep 5"
                              "printf partial; sleep 5"))
             :workdir root :writable-roots (list root)
             :artifact-directory (file-name-concat root "artifacts"))
            (let* ((id (plist-get (plist-get initial :facts) :execution-id))
                   (record (gethash id (mevedel-execution--state-records
                                       (mevedel-session-execution-state session)))))
              (pcase mode
                ('stop (mevedel-execution-stop session "/root" id
                         (lambda (value) (setq stopped value))))
                ('teardown (mevedel-execution-teardown-session session)))
              (unless (eq mode 'teardown)
                (test-mevedel-execution--wait
                 (lambda () (cl-find 'terminal events :key (lambda (e) (plist-get e :type))))))
              (should-not (mevedel-execution--record-monitor-timer record))
              (if (eq mode 'teardown)
                  (should-not events)
                (let* ((terminal (cl-find 'terminal events :key (lambda (e) (plist-get e :type))))
                       (facts (plist-get terminal :facts)))
                  (should (eq (plist-get facts :termination)
                              (pcase mode ('expiry 'timed-out) ('flood 'flood) (_ 'stopped))))
                  (when (eq mode 'stop) (should stopped))
                  (accept-process-output nil 0.25)
                  (should (= 1 (cl-count 'terminal events :key (lambda (e) (plist-get e :type)))))))))
        (mevedel-execution-teardown-session session)
        (delete-directory root t)))))

(mevedel-deftest mevedel-monitor-settlement ()
  ,test
  (test)
  :doc "retains and counts the final line without stopping or arming a batch timer"
  (dolist (count '(0 100))
    (let ((record (mevedel-execution--record-create
                   :kind 'monitor :monitor-count count
                   :monitor-times (make-list count (float-time))
                   :monitor-partial "tail" :monitor-partial-length 4)))
      (cl-letf (((symbol-function 'mevedel-execution--stop-process)
                 (lambda (&rest _) (ert-fail "Settlement tried to stop the child")))
                ((symbol-function 'mevedel-transport-run-at-time)
                 (lambda (&rest _) (ert-fail "Settlement armed a batch timer"))))
        (mevedel-execution--monitor-line record t))
      (should (= (1+ count) (mevedel-execution--record-monitor-count record)))
      (should (equal "tail\n" (mevedel-execution--monitor-body record)))
      (should (equal "" (mevedel-execution--record-monitor-partial record)))
      (should-not (mevedel-execution--record-monitor-timer record))))
  :doc "exit and timeout keep their reason when a final partial becomes event 101"
  (dolist (termination '(exited timed-out))
    (let* ((root (make-temp-file "mevedel-monitor-final-" t))
           (session (test-mevedel-execution--session root))
           (mevedel-sandbox-mode 'off)
           initial events)
      (unwind-protect
          (let ((mevedel-execution-mailbox-delivery-function
                 (lambda (event _ ack) (push event events) (funcall ack t nil) t)))
            (mevedel-execution-start-bash
             (lambda (observation) (setq initial observation))
             :kind 'monitor :description "final" :timeout-ms 1000
             :session session :owner "/root" :owner-context session
             :command (list "sh" "-c"
                            (concat "i=0; while test $i -lt 100; do printf 'line\\n'; i=$((i+1)); done; printf tail"
                                    (when (eq termination 'timed-out) "; sleep 5")))
             :workdir root :writable-roots (list root)
             :artifact-directory (file-name-concat root "artifacts"))
            (test-mevedel-execution--wait
             (lambda () (cl-find 'terminal events :key (lambda (e) (plist-get e :type)))))
            (let* ((terminal (cl-find 'terminal events :key (lambda (e) (plist-get e :type))))
                   (facts (plist-get terminal :facts))
                   (id (plist-get (plist-get initial :facts) :execution-id))
                   (record (gethash id (mevedel-execution--state-records
                                       (mevedel-session-execution-state session)))))
              (should (eq termination (plist-get facts :termination)))
              (should (= 101 (plist-get facts :event-count)))
              (should (equal "tail" (car (last (plist-get terminal :monitor-lines)))))
              (should (integerp (plist-get terminal :monitor-omitted)))
              (should-not (mevedel-execution--record-stop-p record))
              (should-not (mevedel-execution--record-monitor-timer record))
              (accept-process-output nil 0.25)
              (should (= 1 (cl-count 'terminal events :key (lambda (e) (plist-get e :type)))))))
        (mevedel-execution-teardown-session session)
        (delete-directory root t)))))

(mevedel-deftest mevedel-monitor-status ()
  ,test
  (test)
  :doc "Watching rows have a fixed expiry and event count rather than elapsed time"
  (let* ((facts '(:kind monitor :description "builds" :owner "/root"
                  :expires-at 1700000000.0 :event-count 7 :state running
                  :wall-time-seconds 123456))
         (line (mevedel-view-execution--line facts))
         (metadata (mevedel-tool-exec-format-execution-metadata facts)))
    (should (string-match-p "Watching  monitor  builds" line))
    (should (string-match-p "Watching: builds" metadata))
    (should (string-match-p "until " line))
    (should (string-match-p "expires " metadata))
    (dolist (text (list line metadata))
      (should (string-match-p "7 events" text))
      (should-not (string-match-p "123456" text))))
  :doc "the model reads the expiry as a calendar timestamp"
  (let ((xml (mevedel-tool-exec--execution-facts-xml
              '(:kind monitor :execution-id "e" :expires-at 1700000000.0))))
    (should (string-match-p
             (regexp-quote
              (format "expires_at=\"%s\""
                      (format-time-string "%FT%T%z" 1700000000.0)))
             xml))
    (should-not (string-match-p "1700000000" xml))))

(provide 'test-mevedel-monitor)
;;; test-mevedel-monitor.el ends here
