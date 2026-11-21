;;; test-niwa-monitor.el --- Monitor runtime tests -*- lexical-binding: t -*-

;;; Commentary:
;; Bounded framing, mailbox batching, lifetime, and tool admission.

;;; Code:

(require 'cl-lib)
(require 'niwa-execution)
(require 'niwa-tool-exec)
(require 'niwa-view)
(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name
                                   byte-compile-current-file)) "helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name
                                   byte-compile-current-file))
          "niwa-execution-test-helpers"))

(niwa-deftest niwa-monitor-framing ()
  ,test
  (test)
  :doc "frames split CRLF, empty lines and multiple lines with one 200ms timer"
  (let ((record (niwa-execution--record-create :kind 'monitor)) timers)
    (cl-letf (((symbol-function 'niwa-transport-run-at-time)
               (lambda (seconds function &rest args)
                 (push (list seconds function args) timers)
                 'pending)))
      (niwa-execution--monitor-frame record "one\r")
      (should-not timers)
      (niwa-execution--monitor-frame record "\n\ntwo\npar")
      (niwa-execution--monitor-frame record "tial")
      (should (equal '("two" "" "one")
                     (niwa-execution--record-monitor-lines record)))
      (should (equal "partial" (niwa-execution--record-monitor-partial record)))
      (should (= 3 (niwa-execution--record-monitor-count record)))
      (should (= 1 (length timers)))
      (should (= 0.2 (caar timers)))))
  :doc "bounds partial lines, counts truncation, and caps UTF-8 batches at 8KiB"
  (let ((record (niwa-execution--record-create :kind 'monitor)))
    (cl-letf (((symbol-function 'niwa-transport-run-at-time)
               (lambda (&rest _) 'pending)))
      (niwa-execution--monitor-frame record (make-string 5000 ?x))
      (should (= 2000 (length (niwa-execution--record-monitor-partial record))))
      (niwa-execution--monitor-frame record "\r\n")
      (should (string-suffix-p "[3000 characters truncated]\n"
                               (niwa-execution--monitor-body record)))
      (dotimes (_ 8)
        (niwa-execution--monitor-frame record (concat (make-string 2000 ?😀) "\n")))
      (should (<= (string-bytes (niwa-execution--monitor-body record)) 8192))
      (should (= 8 (niwa-execution--record-monitor-omitted record)))
      (should (= 9 (niwa-execution--record-monitor-count record)))))
  :doc "stops at the 101st line in a rolling minute without retaining the rest"
  (let ((record (niwa-execution--record-create :kind 'monitor))
        (now 100.0) stopped)
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now))
              ((symbol-function 'niwa-transport-run-at-time)
               (lambda (&rest _) 'pending))
              ((symbol-function 'niwa-execution--stop-process)
               (lambda (value reason)
                 (setq stopped reason)
                 (setf (niwa-execution--record-stop-p value) t))))
      (niwa-execution--monitor-frame record (make-string 100 ?\n))
      (should-not stopped)
      (setq now 161.0)
      (niwa-execution--monitor-frame record "next\n")
      (should-not stopped)
      (niwa-execution--monitor-frame record (make-string 1000 ?\n))
      (should (eq 'flood stopped))
      (should (= 201 (niwa-execution--record-monitor-count record)))
      (should (= 101 (length (niwa-execution--record-monitor-times record)))))))

(niwa-deftest niwa-monitor-batching ()
  ,test
  (test)
  :doc "delivers a single batch, retains failed delivery, and stops on unread flood"
  (let ((record (niwa-execution--record-create
                 :kind 'monitor :origin (niwa-execution--origin-create)))
        (unread 0) (accept nil) events stopped timer)
    (cl-letf (((symbol-function 'niwa-transport-run-at-time)
               (lambda (seconds function &rest args)
                 (should (= seconds 0.2))
                 (setq timer (lambda () (apply function args)))
                 'pending))
              ((symbol-function 'niwa-execution--event)
               (lambda (_ type &rest properties) (cons :type (cons type properties))))
              ((symbol-function 'niwa-execution--deliver-mailbox-event)
               (lambda (_ event ack)
                 (if accept
                     (progn (push event events) (funcall ack t nil))
                   (funcall ack nil "Rejected"))
                 nil))
              ((symbol-function 'niwa-execution--emit-event) #'ignore)
              ((symbol-function 'niwa-execution--notify-state-change) #'ignore)
              ((symbol-function 'niwa-agent-control-monitor-unread-bytes)
               (lambda (&rest _) unread))
              ((symbol-function 'niwa-execution--stop-process)
               (lambda (_ reason) (setq stopped reason))))
      (niwa-execution--monitor-frame record "a\nb\n")
      (should-not events)
      (funcall timer)
      (should (equal "a\nb\n" (niwa-execution--monitor-body record)))
      (setq accept t unread 65537)
      (niwa-execution--monitor-frame record "c\n")
      (funcall timer)
      (should (= 1 (length events)))
      (should (eq 'monitor-batch (plist-get (car events) :type)))
      (should (equal "a\nb\nc\n" (plist-get (car events) :body)))
      (should (equal '("a" "b" "c") (plist-get (car events) :monitor-lines)))
      (should (= 0 (plist-get (car events) :monitor-omitted)))
      (should (string-empty-p (niwa-execution--monitor-body record)))
      (should (eq 'flood stopped)))))

(niwa-deftest niwa-monitor-admission ()
  ,test
  (test)
  :doc "rejects agents and tty before sharing Bash preparation"
  (cl-letf (((symbol-function 'niwa-current-origin) (lambda () "/root"))
            ((symbol-function 'niwa-tool-exec--bash)
             (lambda (_ args kind) (list kind args))))
    (should-error
     (niwa-tool-exec--monitor #'ignore '(:description "ok" :tty :json-false)))
    (should (equal '(monitor (:description "ok"))
                   (niwa-tool-exec--monitor #'ignore '(:description "ok"))))
    (cl-letf (((symbol-function 'niwa-current-origin) (lambda () "/root/child")))
      (should-error (niwa-tool-exec--monitor #'ignore '(:description "ok")))))
  :doc "validates description and lifetime at the execution boundary before launch"
  (let* ((root (make-temp-file "niwa-monitor-admission-" t))
         (session (test-niwa-execution--session root)))
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
                   (apply #'niwa-execution-start-bash #'ignore
                          :kind 'monitor :session session :owner "/root"
                          :command '("sh" "-c" "exit 0") :workdir root
                          (car case)))))
          (should (zerop (niwa-execution-count-user session))))
      (niwa-execution-teardown-session session)
      (delete-directory root t))))

(niwa-deftest niwa-monitor-lifetime ()
  ,test
  (test)
  :doc "yields immediately, separates stderr, flushes final partial and notifies once"
  (let* ((root (make-temp-file "niwa-monitor-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         initial events progress)
    (unwind-protect
        (let ((niwa-execution-mailbox-delivery-function
               (lambda (event _ ack) (push event events) (funcall ack t nil) t))
              (niwa-execution-event-functions
               (list (lambda (event)
                       (when (eq (plist-get event :type) 'progress)
                         (push event progress))))))
          (niwa-execution-start-bash
           (lambda (observation) (setq initial observation))
           :kind 'monitor :description "changes" :session session :owner "/root"
           :owner-context session :command '("sh" "-c" "printf 'warning\\n' >&2; printf 'one\\n'; sleep .4; printf partial")
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts"))
          (should initial)
          (should (eq 'running (plist-get (plist-get initial :facts) :state)))
          (should (plist-get (plist-get initial :facts) :execution-id))
          (should (= 1 (niwa-execution-count-user session)))
          (should-not (niwa-execution-working-p))
          (test-niwa-execution--wait
           (lambda () (cl-find 'terminal events :key (lambda (e) (plist-get e :type)))))
          (should-not progress)
          (let* ((ordered (reverse events))
                 (terminal (car (last ordered)))
                 (facts (plist-get terminal :facts))
                 (body (niwa-tool-exec--monitor-terminal-body terminal)))
            (should (= 2 (length ordered)))
            (should (equal "one\n" (plist-get (car ordered) :body)))
            (should (equal '("partial") (plist-get terminal :monitor-lines)))
            (should (string-prefix-p "Monitor changes: exited with code 0" body))
            (should (= 2 (plist-get facts :event-count)))
            (should (eq 'monitor (plist-get facts :kind)))
            (should-not (string-match-p "warning" body))
            (should (string-match-p "warning" (plist-get terminal :whole-output)))
            (should (string-prefix-p "<monitor-execution "
                                     (niwa-tool-exec--execution-facts-xml facts)))
            (accept-process-output nil 0.3)
            (should (= 2 (length events)))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))
  :doc "a process that exits inside startup still yields and sends one terminal notice"
  (let* ((root (make-temp-file "niwa-monitor-early-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         (start (symbol-function 'niwa-execution-process-start))
         initial events)
    (unwind-protect
        (let ((niwa-execution-mailbox-delivery-function
               (lambda (event _ ack) (push event events) (funcall ack t nil) t)))
          (cl-letf (((symbol-function 'niwa-execution-process-start)
                     (lambda (child &rest args)
                       (prog1 (apply start child args)
                         (test-niwa-execution--wait
                          (lambda () (not (niwa-execution-process-live-p child))))))))
            (niwa-execution-start-bash
             (lambda (observation) (push observation initial))
             :kind 'monitor :description "quick" :session session :owner "/root"
             :owner-context session :command '("sh" "-c" "printf done")
             :workdir root :writable-roots (list root)
             :artifact-directory (file-name-concat root "artifacts")))
          (test-niwa-execution--wait (lambda () events))
          (should (= 1 (length initial)))
          (should (equal "" (plist-get (car initial) :output)))
          ;; The start acknowledgement never claims the started monitor is queued.
          (should (eq 'running (plist-get (plist-get (car initial) :facts) :state)))
          (should (= 1 (length events)))
          (should (eq 'terminal (plist-get (car events) :type)))
          (should (equal '("done") (plist-get (car events) :monitor-lines)))
          (should (string-prefix-p "Monitor quick: exited with code 0"
                                   (niwa-tool-exec--monitor-terminal-body (car events)))))
      (niwa-execution-teardown-session session)
      (delete-directory root t))))

(niwa-deftest niwa-monitor-quiet-timers ()
  ,test
  (test)
  :doc "a silent monitor with a live tool observer never arms a progress clock"
  (let* ((root (make-temp-file "niwa-monitor-quiet-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         (niwa-execution-progress-delay 0.01)
         (niwa-execution-progress-interval 0.01)
         initial events)
    (unwind-protect
        (let ((niwa-execution-event-functions
               (list (lambda (event) (push event events)))))
          (niwa-execution-start-bash
           (lambda (observation) (setq initial observation))
           :kind 'monitor :description "silent" :tool-use-id "quiet-tool"
           :session session :owner "/root" :owner-context session
           :command '("sh" "-c" "sleep 5") :workdir root
           :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts"))
          (let* ((id (plist-get (plist-get initial :facts) :execution-id))
                 (record (gethash id (niwa-execution--state-records
                                     (niwa-session-execution-state session)))))
            (should-not (niwa-execution--record-progress-timer record))
            (should-not (niwa-execution--record-yield-timer record))
            (should-not (niwa-execution--record-monitor-timer record))
            (accept-process-output nil 0.3)
            (should-not (cl-find 'progress events :key (lambda (e) (plist-get e :type))))))
      (niwa-execution-teardown-session session)
      (delete-directory root t))))

(niwa-deftest niwa-monitor-scheduler-release (:quiet t)
  ,test
  (test)
  :doc "admits an exclusive command after Monitor yields while its watcher stays live"
  (let* ((root (make-temp-file "niwa-monitor-scheduler-" t))
         (session (test-niwa-execution--session root))
         (started (file-name-concat root "monitor-started"))
         (stop (file-name-concat root "monitor-stop"))
         (completed (file-name-concat root "exclusive-completed"))
         (niwa-sandbox-mode 'off)
         monitor-initial exclusive-result events)
    (unwind-protect
        (let ((niwa-execution-mailbox-delivery-function
               (lambda (event _owner-context acknowledge)
                 (push event events)
                 (funcall acknowledge t nil)
                 t)))
          (niwa-execution-start-bash
           (lambda (observation) (setq monitor-initial observation))
           :kind 'monitor :description "scheduler lease"
           :session session :owner "/root" :owner-context session
           :command
           (list "sh" "-c"
                 "touch \"$1\"; while [ ! -e \"$2\" ]; do sleep .02; done"
                 "monitor-scheduler" started stop)
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts"))
          (test-niwa-execution--wait (lambda () monitor-initial))
          (should (eq 'running
                      (plist-get (plist-get monitor-initial :facts) :state)))
          (let* ((id (plist-get (plist-get monitor-initial :facts) :execution-id))
                 (record (gethash id (niwa-execution--state-records
                                      (niwa-session-execution-state session)))))
            (test-niwa-execution--wait (lambda () (file-exists-p started)))
            (should (niwa-execution-process-live-p
                     (niwa-execution--record-child record)))
            (niwa-execution-start-bash
             (lambda (observation) (setq exclusive-result observation))
             :session session :owner "/root" :owner-context session
             :command (list "sh" "-c" "touch \"$1\"" "exclusive" completed)
             :workdir root :writable-roots (list root)
             :artifact-directory (file-name-concat root "artifacts")
             :yield-time-ms nil)
            (test-niwa-execution--wait
             (lambda () (and exclusive-result (file-exists-p completed))))
            (should (eq 'completed
                        (plist-get (plist-get exclusive-result :facts) :state)))
            ;; The dependent command completed before the still-running watcher
            ;; was stopped; releasing only at Monitor termination cannot pass.
            (should (niwa-execution-process-live-p
                     (niwa-execution--record-child record)))
            (with-temp-file stop)
            (test-niwa-execution--wait
             (lambda ()
               (cl-find 'terminal events
                        :key (lambda (event) (plist-get event :type)))))))
      (with-temp-file stop)
      (niwa-execution-teardown-session session)
      (delete-directory root t))))

(niwa-deftest niwa-monitor-terminal ()
  ,test
  (test)
  :doc "expiry, flood, explicit stop and teardown settle without leaked batch timers"
  (dolist (mode '(expiry flood stop teardown))
    (let* ((root (make-temp-file "niwa-monitor-stop-" t))
           (session (test-niwa-execution--session root))
           (niwa-sandbox-mode 'off)
           initial events stopped)
      (unwind-protect
          (let ((niwa-execution-mailbox-delivery-function
                 (lambda (event _ ack) (push event events) (funcall ack t nil) t)))
            (niwa-execution-start-bash
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
                   (record (gethash id (niwa-execution--state-records
                                       (niwa-session-execution-state session)))))
              (pcase mode
                ('stop (niwa-execution-stop session "/root" id
                         (lambda (value) (setq stopped value))))
                ('teardown (niwa-execution-teardown-session session)))
              (unless (eq mode 'teardown)
                (test-niwa-execution--wait
                 (lambda () (cl-find 'terminal events :key (lambda (e) (plist-get e :type))))))
              (should-not (niwa-execution--record-monitor-timer record))
              (if (eq mode 'teardown)
                  (should-not events)
                (let* ((terminal (cl-find 'terminal events :key (lambda (e) (plist-get e :type))))
                       (facts (plist-get terminal :facts)))
                  (should (eq (plist-get facts :termination)
                              (pcase mode ('expiry 'timed-out) ('flood 'flood) (_ 'stopped))))
                  (when (eq mode 'stop) (should stopped))
                  (accept-process-output nil 0.25)
                  (should (= 1 (cl-count 'terminal events :key (lambda (e) (plist-get e :type)))))))))
        (niwa-execution-teardown-session session)
        (delete-directory root t)))))

(niwa-deftest niwa-monitor-settlement ()
  ,test
  (test)
  :doc "retains and counts the final line without stopping or arming a batch timer"
  (dolist (count '(0 100))
    (let ((record (niwa-execution--record-create
                   :kind 'monitor :monitor-count count
                   :monitor-times (make-list count (float-time))
                   :monitor-partial "tail" :monitor-partial-length 4)))
      (cl-letf (((symbol-function 'niwa-execution--stop-process)
                 (lambda (&rest _) (ert-fail "Settlement tried to stop the child")))
                ((symbol-function 'niwa-transport-run-at-time)
                 (lambda (&rest _) (ert-fail "Settlement armed a batch timer"))))
        (niwa-execution--monitor-line record t))
      (should (= (1+ count) (niwa-execution--record-monitor-count record)))
      (should (equal "tail\n" (niwa-execution--monitor-body record)))
      (should (equal "" (niwa-execution--record-monitor-partial record)))
      (should-not (niwa-execution--record-monitor-timer record))))
  :doc "exit and timeout keep their reason when a final partial becomes event 101"
  (dolist (termination '(exited timed-out))
    (let* ((root (make-temp-file "niwa-monitor-final-" t))
           (session (test-niwa-execution--session root))
           (niwa-sandbox-mode 'off)
           initial events)
      (unwind-protect
          (let ((niwa-execution-mailbox-delivery-function
                 (lambda (event _ ack) (push event events) (funcall ack t nil) t)))
            (niwa-execution-start-bash
             (lambda (observation) (setq initial observation))
             :kind 'monitor :description "final" :timeout-ms 1000
             :session session :owner "/root" :owner-context session
             :command (list "sh" "-c"
                            (concat "i=0; while test $i -lt 100; do printf 'line\\n'; i=$((i+1)); done; printf tail"
                                    (when (eq termination 'timed-out) "; sleep 5")))
             :workdir root :writable-roots (list root)
             :artifact-directory (file-name-concat root "artifacts"))
            (test-niwa-execution--wait
             (lambda () (cl-find 'terminal events :key (lambda (e) (plist-get e :type)))))
            (let* ((terminal (cl-find 'terminal events :key (lambda (e) (plist-get e :type))))
                   (facts (plist-get terminal :facts))
                   (id (plist-get (plist-get initial :facts) :execution-id))
                   (record (gethash id (niwa-execution--state-records
                                       (niwa-session-execution-state session)))))
              (should (eq termination (plist-get facts :termination)))
              (should (= 101 (plist-get facts :event-count)))
              (should (equal "tail" (car (last (plist-get terminal :monitor-lines)))))
              (should (integerp (plist-get terminal :monitor-omitted)))
              (should-not (niwa-execution--record-stop-p record))
              (should-not (niwa-execution--record-monitor-timer record))
              (accept-process-output nil 0.25)
              (should (= 1 (cl-count 'terminal events :key (lambda (e) (plist-get e :type)))))))
        (niwa-execution-teardown-session session)
        (delete-directory root t)))))

(niwa-deftest niwa-monitor-status ()
  ,test
  (test)
  :doc "Watching rows have a fixed expiry and event count rather than elapsed time"
  (let* ((facts '(:kind monitor :description "builds" :owner "/root"
                  :expires-at 1700000000.0 :event-count 7 :state running
                  :wall-time-seconds 123456))
         (line (niwa-view-execution--line facts))
         (metadata (niwa-tool-exec-format-execution-metadata facts)))
    (should (string-match-p "Watching  monitor  builds" line))
    (should (string-match-p "Watching: builds" metadata))
    (should (string-match-p "until " line))
    (should (string-match-p "expires " metadata))
    (dolist (text (list line metadata))
      (should (string-match-p "7 events" text))
      (should-not (string-match-p "123456" text))))
  :doc "the model reads the expiry as a calendar timestamp"
  (let ((xml (niwa-tool-exec--execution-facts-xml
              '(:kind monitor :execution-id "e" :expires-at 1700000000.0))))
    (should (string-match-p
             (regexp-quote
              (format "expires_at=\"%s\""
                      (format-time-string "%FT%T%z" 1700000000.0)))
             xml))
    (should-not (string-match-p "1700000000" xml))))

(provide 'test-niwa-monitor)
;;; test-niwa-monitor.el ends here
