;;; test-niwa-execution.el --- Tests for managed child execution -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the bounded one-shot process boundary used by Bash, fresh Eval, and
;; native external helpers.

;;; Code:

(require 'cl-lib)
(require 'niwa-agents)
(require 'niwa-execution)
(require 'niwa-execution-process)
(require 'niwa-permission-rules)
(require 'niwa-resource)
(require 'niwa-sandbox)
(require 'niwa-structs)
(require 'niwa-telemetry)
(require 'niwa-transport)
(require 'niwa-utilities)
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


;;
;;; One-shot execution

(niwa-deftest niwa-execution--diagnostic-scope ()
  ,test
  (test)
  :doc "uses the record session, never the callback's current workspace"
  (let* ((session (niwa-session--create
                   :workspace (niwa-workspace--create :root "/owner/project")))
         (record (niwa-execution--record-create
                  :origin (niwa-execution--origin-create :session session))))
    (with-temp-buffer
      (setq-local niwa--session
                  (niwa-session--create
                   :workspace (niwa-workspace--create :root "/other/project")))
      (should (equal "/owner/project"
                     (niwa-execution--diagnostic-scope record)))))
  :doc "marks a genuinely ownerless record unavailable"
  (should (eq 'unavailable
              (niwa-execution--diagnostic-scope
               (niwa-execution--record-create)))))

(niwa-deftest test-niwa-execution--wait ()
  ,test
  (test)
  :doc "returns when the predicate becomes ready"
  (let ((calls 0))
    (test-niwa-execution--wait (lambda () (= (cl-incf calls) 2)))
    (should (= calls 2)))
  :doc "elapsed deadline survives loss of the with-timeout timer"
  (let ((clock 1000.0) (calls 0) cancelled)
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) clock))
              ((symbol-function 'accept-process-output)
               (lambda (&rest _) (setq clock (+ clock 0.02)))))
      (should
       (equal '(error "Timed out")
              (should-error
               (test-niwa-execution--wait
                (lambda ()
                  (cl-incf calls)
                  (when (> calls 4) (error "Deadline guard did not run"))
                  ;; Model TRAMP losing suspended timeout timers, without
                  ;; disturbing unrelated event-loop timers in this process.
                  (dolist (timer with-timeout-timers)
                    (push timer cancelled)
                    (cancel-timer timer))
                  nil)
                0.05))))
      (should cancelled)
      (should (= calls 4)))))

(niwa-deftest test-niwa-execution--timeout-after-ready
  (:doc "waits for a nonempty marker only for timeout delivery, preserving its budget")
  (let* ((root (make-temp-file "niwa-timeout-ready-" t))
         (path (file-name-concat root "ready"))
         (waits 0) scheduled)
    (unwind-protect
        (cl-letf (((symbol-function 'test-niwa-execution--wait)
                   (lambda (predicate &rest _)
                     (cl-incf waits)
                     (should-not (funcall predicate))
                     (with-temp-file path)
                     (should-not (funcall predicate))
                     (write-region "ready" nil path nil 'silent)
                     (should (funcall predicate))))
                  ((symbol-function 'niwa-transport-run-at-time)
                   (lambda (&rest args) (push args scheduled) 'timer)))
          (should
           (eq 'result
               (test-niwa-execution--timeout-after-ready
                path
                (lambda ()
                  (niwa-transport-run-at-time 7 #'ignore 'ordinary)
                  (should (zerop waits))
                  (niwa-transport-run-at-time
                   0.05 #'niwa-execution-process--time-out 'child)
                  'result))))
          (should (= 1 waits))
          (should (equal scheduled
                         '((0.05 niwa-execution-process--time-out child)
                           (7 ignore ordinary)))))
      (delete-directory root t))))

(niwa-deftest niwa-execution--next-id ()
  ,test
  (test)
  :doc "keeps IDs distinct across fresh session states with restarted counters"
  (let* ((first (niwa-execution--new-state))
         (second (niwa-execution--new-state))
         (first-id (niwa-execution--next-id first))
         (second-id (niwa-execution--next-id second)))
    (should (string-match-p "\\`exec-[[:alnum:]]+-000001\\'" first-id))
    (should (string-suffix-p "-000002" (niwa-execution--next-id first)))
    (should-not (equal first-id second-id))))

(niwa-deftest niwa-execution--terminal-event ()
  ,test
  (test)
  :doc "marks preview truncation independently of observation omissions"
  (let ((niwa-execution-inline-output-limit 6)
        (record (niwa-execution--record-create)))
    (cl-letf (((symbol-function 'niwa-execution--event)
               (lambda (_record _type &rest properties) properties)))
      (niwa-execution--retain-output record "abcdefghi")
      (let ((event (niwa-execution--terminal-event
                    record 'model '(:facts (:omitted-output-bytes 0)))))
        (should (plist-get event :whole-output-truncated-p))
        (should (= 0 (plist-get (plist-get event :facts)
                                :omitted-output-bytes)))))))

(niwa-deftest niwa-execution--emit-progress ()
  ,test
  (test)
  :doc "marks cumulative preview truncation on live events"
  (let ((niwa-execution-inline-output-limit 6)
        (record (niwa-execution--record-create)) event)
    (niwa-execution--retain-output record "abcdefghi")
    (cl-letf (((symbol-function 'niwa-execution--event)
               (lambda (_record _type &rest properties) properties))
              ((symbol-function 'niwa-execution--emit-event)
               (lambda (value) (setq event value)))
              ((symbol-function 'niwa--ui-timer-activate) (lambda (&rest _) nil)))
      (niwa-execution--emit-progress record))
    (should (plist-get event :output-preview-truncated-p)))

  :doc "shares whole-second quiet updates without violating the output rate bound"
  (let ((record (niwa-execution--record-create)) timer)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-execution--event) #'ignore)
                  ((symbol-function 'niwa-execution--emit-event) #'ignore))
          (niwa-execution--emit-progress record)
          (setq timer (niwa-execution--record-progress-timer record))
          (let ((due (float-time (timer--time timer)))
                (emitted (niwa-execution--record-progress-emitted-at record)))
            (should (<= 0.25 (- due emitted) 1.26))
            (should (= due (floor due))))
          (should (niwa--ui-timer-pending-p timer))
          (should-not (memq timer timer-list))
          (timer-set-time timer (time-add nil 1))
          (niwa-execution--hasten-progress record)
          ;; Output moves the same event out of the shared queue, earlier.
          (should (eq timer (niwa-execution--record-progress-timer record)))
          (should-not (memq timer niwa--coalesced-timers))
          (should (memq timer timer-list))
          (should (< (float-time (timer--time timer)) (+ (float-time) 0.3))))
      (niwa--ui-timer-cancel timer)
      (niwa--ui-timer-cancel (niwa-execution--record-progress-timer record))))

  :doc "honours an output interval longer than the quiet interval"
  (let ((record (niwa-execution--record-create))
        (niwa-execution-progress-interval 3)
        timer)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-execution--event) #'ignore)
                  ((symbol-function 'niwa-execution--emit-event) #'ignore))
          (niwa-execution--emit-progress record)
          (setq timer (niwa-execution--record-progress-timer record))
          (let ((due (float-time (timer--time timer)))
                (emitted (niwa-execution--record-progress-emitted-at record)))
            ;; At least the interval apart, still on a shared whole second.
            (should (= due (floor due)))
            (should (<= 3 (- due emitted) 4.01))))
      (niwa--ui-timer-cancel timer)))

  :doc "terminal cleanup cancels shared quiet progress without stopping other jobs"
  (let ((record (niwa-execution--record-create)) timer other)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-execution--event) #'ignore)
                  ((symbol-function 'niwa-execution--emit-event) #'ignore))
          (setq other (niwa--run-periodic-timer 'unavailable 1 #'ignore))
          (niwa-execution--emit-progress record)
          (setq timer (niwa-execution--record-progress-timer record))
          (niwa-execution--release-runtime record)
          (should-not (niwa--ui-timer-pending-p timer))
          (should (niwa--ui-timer-pending-p other)))
      (niwa--ui-timer-cancel timer)
      (niwa--ui-timer-cancel other))))

(niwa-deftest niwa-execution--hasten-progress ()
  ,test
  (test)
  :doc "brings a quiet wait forward to the output interval"
  (let* ((record (niwa-execution--record-create
                  :progress-emitted-at (- (float-time) 0.1)
                  :progress-timer (run-at-time 0.9 nil #'ignore)))
         (old (niwa-execution--record-progress-timer record)))
    (unwind-protect
        (progn
          (niwa-execution--hasten-progress record)
          (let ((timer (niwa-execution--record-progress-timer record)))
            (should (eq old timer))
            (should (= 1 (cl-count old timer-list)))
            (should (< (float-time (timer--time timer)) (+ (float-time) 0.2)))
            ;; Further output before that event leaves it alone.
            (niwa-execution--hasten-progress record)
            (should (eq timer (niwa-execution--record-progress-timer record)))))
      (cancel-timer old)
      (cancel-timer (niwa-execution--record-progress-timer record))))

  :doc "keeps the initial delay before the first event"
  (let ((record (niwa-execution--record-create
                 :progress-timer (run-at-time 2 nil #'ignore))))
    (unwind-protect
        (let ((timer (niwa-execution--record-progress-timer record)))
          (niwa-execution--hasten-progress record)
          (should (eq timer (niwa-execution--record-progress-timer record))))
      (cancel-timer (niwa-execution--record-progress-timer record))))

  :doc "moves a suspended event instead of arming a second one"
  (let* ((record (niwa-execution--record-create
                  :progress-emitted-at (- (float-time) 0.1)
                  :progress-timer (run-at-time 0.9 nil #'ignore)))
         (old (niwa-execution--record-progress-timer record)))
    (unwind-protect
        (progn
          (niwa-transport-with-exclusive-connection
            (niwa-execution--hasten-progress record))
          (should (eq old (niwa-execution--record-progress-timer record)))
          (should (= 1 (cl-count old timer-list)))
          (should (< (float-time (timer--time old)) (+ (float-time) 0.2))))
      (niwa--ui-timer-cancel old))))

(niwa-deftest niwa-execution--user-snapshot ()
  ,test
  (test)
  :doc "Bash descriptions reach facts and consumer snapshots"
  (let* ((spool (make-temp-file "niwa-description-"))
         (record (niwa-execution--record-create
                 :execution-id "exec-description" :kind 'bash
                 :description "Run tests" :started-at (float-time)
                 :child (niwa-execution-process--child-create
                         :started-at (float-time) :spool-path spool)
                 :origin (niwa-execution--origin-create
                          :owner "/root" :tool-use-id "bash-description"
                          :tool-args '(:command "make test")))))
    (unwind-protect
        (progn
          (should (equal "Run tests"
                         (plist-get (niwa-execution--facts record) :description)))
          (let ((snapshot (niwa-execution--user-snapshot record)))
            (should (equal "Run tests" (plist-get snapshot :description)))
            (should (equal "make test" (plist-get snapshot :command)))))
      (delete-file spool)))
  :doc "live guest snapshots retain cumulative tail truncation independent of poll omissions"
  (let ((niwa-execution-inline-output-limit 6)
        (record (niwa-execution--record-create
                 :execution-id "exec-1"
                 :origin (niwa-execution--origin-create
                          :owner "/root" :tool-use-id "bash-1"))))
    (cl-letf (((symbol-function 'niwa-execution--facts)
               (lambda (_) '(:state running :omitted-output-bytes 0))))
      (niwa-execution--retain-output record "abcdef")
      (should-not (plist-get (niwa-execution--user-snapshot record)
                             :output-preview-truncated-p))
      (niwa-execution--retain-output record "ghi")
      (let ((snapshot (niwa-execution--user-snapshot record)))
        (should (equal "bash-1" (plist-get snapshot :tool-use-id)))
        (should (equal "defghi" (plist-get snapshot :output-tail)))
        (should (plist-get snapshot :output-preview-truncated-p))
        (should (= 0 (plist-get snapshot :omitted-output-bytes)))))))

(niwa-deftest niwa-execution-start-one-shot ()
  ,test
  (test)
  :doc "settles a child that exits before launch setup resumes"
  (let ((original-make-process (symbol-function 'make-process))
        (original-accept-process-output
         (symbol-function 'accept-process-output))
        (niwa-sandbox-mode 'off)
        done result)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((process (apply original-make-process args)))
                   (while (process-live-p process)
                     (funcall original-accept-process-output
                              process 0.01 nil t))
                   process))))
      (niwa-execution-start-one-shot
       (lambda (child-result)
         (setq result child-result
               done t))
       :name "niwa-test-immediate-exit"
       :command '("sh" "-c" "printf immediate")
       :workdir temporary-file-directory
       :writable-roots (list temporary-file-directory))
      (with-timeout (2 (error "Immediate child did not settle"))
        (while (not done)
          (funcall original-accept-process-output nil 0.01)))
      (should (= 0 (plist-get result :exit-code)))
      (should-not (plist-get result :error))
      (should (equal "immediate" (plist-get result :output)))))
  :doc "settles an exited child even when Emacs does not deliver its sentinel"
  (let* ((root (make-temp-file "niwa-missed-sentinel-" t))
         (gate (file-name-concat root "finish"))
         (original-make-process (symbol-function 'make-process))
         (schedule (symbol-function 'niwa--run-periodic-timer))
         (niwa-sandbox-mode 'off)
         process watch watch-timer done result)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq process
                           (apply original-make-process
                                  (plist-put args :sentinel #'ignore)))))
                  ((symbol-function 'niwa--run-periodic-timer)
                   (lambda (scope period function &rest args)
                     (should (= 1.0 period))
                     (should-not watch)
                     (setq watch (lambda () (apply function args)))
                     (setq watch-timer (apply schedule scope 3600 function args)))))
          (should
           (functionp
            (niwa-execution-start-one-shot
             (lambda (child-result)
               (setq result child-result
                     done t))
             :name "niwa-test-missed-sentinel"
             :command (list "sh" "-c"
                            "printf recovered; while test ! -e \"$1\"; do sleep .01; done; exit 7"
                            "missed-sentinel" gate)
             :workdir root :writable-roots (list root))))
          (should watch)
          ;; The watch must not settle a live child; suppressing the sentinel
          ;; must leave an exited child unsettled until the captured check runs.
          (should (process-live-p process))
          (funcall watch)
          (should-not done)
          (with-temp-file gate)
          (test-niwa-execution--wait
           (lambda () (memq (process-status process) '(exit signal))))
          (should-not done)
          (funcall watch)
          (test-niwa-execution--wait (lambda () done))
          (should (= 7 (plist-get result :exit-code)))
          (should (equal "recovered" (plist-get result :output))))
      (when watch-timer (niwa--ui-timer-cancel watch-timer))
      (when (and process (process-live-p process)) (delete-process process))
      (delete-directory root t)))
  :doc "passes the session sandbox policy to the child boundary"
  ;; A private directory: protected-path discovery walks the workdir, and
  ;; parallel test workers churn the shared temporary directory under it.
  (let ((session (niwa-session--create :sandbox-mode 'required))
        (niwa-sandbox-mode 'off)
        (workdir (file-name-as-directory
                  (make-temp-file "niwa-one-shot-mode-" t)))
        captured-mode done)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-sandbox-prepare)
                   (lambda (command _workdir _roots
                                    &optional _additional _permissions mode
                                    _temporary-root _candidates _scope)
                     (setq captured-mode mode)
                     (list :state 'unrestricted
                           :command command
                           :facts '(:sandbox off
                                    :filesystem unrestricted
                                    :network unrestricted)))))
          (niwa-execution-start-one-shot
           (lambda (_child-result) (setq done t))
           :name "niwa-test-session-sandbox-mode"
           :command '("true")
           :workdir workdir
           :writable-roots (list workdir)
           :session session)
          (with-timeout (2 (error "Process did not exit"))
            (while (not done)
              (accept-process-output nil 0.01))))
      (delete-directory workdir t))
    (should (eq 'required captured-mode)))
  :doc "shares one terminal contract for direct and confined child attempts"
  (skip-unless (plist-get (niwa-sandbox-probe) :available))
  (dolist (mode '(off required))
    (dolist (cancel-p '(nil t))
      (let* ((root (make-temp-file "niwa-one-shot-contract-" t))
             (ready (file-name-concat root "ready"))
             (protected (file-name-concat root "protected-missing"))
             (niwa-protected-paths
              `((,(concat protected "/**") . inaccessible)))
             (niwa-sandbox-mode mode)
             (niwa-execution-process--child-kill-delay 0.05)
             (niwa-execution-telemetry-summary-cell (list nil))
             (settlements 0)
             result cancel callback-saw-placeholder)
        (unwind-protect
            (progn
              (setq cancel
                    (niwa-execution-start-one-shot
                     (lambda (value)
                       (cl-incf settlements)
                       (setq result value
                             callback-saw-placeholder
                             (file-exists-p protected)))
                     :name "niwa-one-shot-contract"
                     :command
                     (list "sh" "-c"
                           (if cancel-p
                               "printf ready; touch ready; exec sleep 30"
                             "printf ready; touch ready; printf done"))
                     :workdir root :writable-roots (list root)))
              (should (functionp cancel))
              (when cancel-p
                (test-niwa-execution--wait
                 (lambda () (file-exists-p ready)))
                (when (eq mode 'required)
                  (should (file-directory-p protected)))
                (funcall cancel)
                (funcall cancel))
              (test-niwa-execution--wait (lambda () result))
              (ert-info ((format "mode=%S cancel=%S result=%S" mode cancel-p result))
                (should-not (plist-get result :error)))
              (should (= 1 settlements))
              (should-not callback-saw-placeholder)
              (should (equal (if cancel-p "ready" "readydone")
                             (plist-get result :output)))
              (if cancel-p
                  (should (eq 'aborted (plist-get result :termination)))
                (should (= 0 (plist-get result :exit-code))))
              (let ((summary (plist-get result :sandbox-summary)))
                (should (= 1 (plist-get summary :attempt-count)))
                (should (= 1 (plist-get summary :started-count)))
                (should (= 0 (plist-get summary :refused-count)))
                (should (eq (if (eq mode 'off) 'off 'bubblewrap)
                            (plist-get summary :sandbox)))
                (should (equal summary
                               (car niwa-execution-telemetry-summary-cell))))
              (funcall cancel)
              (should (= 1 settlements)))
          (niwa-execution-teardown-all)
          (delete-directory root t))))))

(niwa-deftest niwa-execution-run-one-shot ()
  ,test
  (test)
  :doc "edits refuses an unavailable sandbox while full-auto starts directly"
  (let* ((root (make-temp-file "niwa-mode-launch-" t))
         (session (niwa-session--create
                   :name "mode-launch" :working-directory root
                   :sandbox-mode 'best-effort :permission-mode 'edits))
         (niwa-sandbox--probe-cache
          '((nil . (:available nil :reason "Test backend unavailable")))))
    (unwind-protect
        (progn
          (let ((result
                 (niwa-execution-run-one-shot
                  :name "niwa-mode-refusal" :command '("sh" "-c" "touch marker")
                  :workdir root :writable-roots (list root) :session session)))
            (should (plist-get result :error))
            (should-not (file-exists-p (file-name-concat root "marker")))
            (should (= 0 (plist-get (plist-get result :sandbox-summary)
                                   :started-count))))
          (setf (niwa-session-permission-mode session) 'full-auto)
          (let ((result
                 (niwa-execution-run-one-shot
                  :name "niwa-mode-direct" :command '("sh" "-c" "touch marker")
                  :workdir root :writable-roots nil :session session)))
            (should (= 0 (plist-get result :exit-code)))
            (should (file-exists-p (file-name-concat root "marker")))
            (should (eq 'unrestricted
                        (plist-get (plist-get result :sandbox-summary)
                                   :filesystem)))
            (should (eq 'unrestricted
                        (plist-get (plist-get result :sandbox-summary)
                                   :network)))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))
  :doc "returns complete output and structured terminal facts"
  (let ((niwa-sandbox-mode 'off))
    (let ((result
           (niwa-execution-run-one-shot
            :name "niwa-test-success"
            :command '("sh" "-c" "printf 'hello'; printf ' world' >&2")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (= 0 (plist-get result :exit-code)))
      (should (equal "hello world" (plist-get result :output)))
      (should-not (plist-get result :timed-out-p))
      (should-not (plist-get result :output-limit-p))
      (should (numberp (plist-get result :wall-time-seconds)))
      (should
       (equal '(:attempt-count 1 :started-count 1 :refused-count 0
                :sandbox off :filesystem unrestricted
                :network unrestricted :proc nil
                :additional-read-count 0 :additional-write-count 0)
              (plist-get result :sandbox-summary)))
      (should-not (plist-member result :process))))
  :doc "owner teardown settles a synchronous one-shot caller"
  (let* ((root (make-temp-file "niwa-one-shot-teardown-" t))
         (session (niwa-session-create
                   "main" (test-niwa-execution--workspace root) root))
         (niwa-sandbox-mode 'off)
         result)
    (unwind-protect
        (progn
          (run-at-time
           0.05 nil
           (lambda ()
             (niwa-execution-stop-owner session "agent-a")))
          (setq result
                (niwa-execution-run-one-shot
                 :name "niwa-test-owner-teardown"
                 :command '("sh" "-c" "sleep 30")
                 :workdir root :writable-roots (list root)
                 :session session :owner "agent-a"))
          (should (= -1 (plist-get result :exit-code)))
          (should (string-match-p
                   "owner was torn down"
                   (error-message-string (plist-get result :error)))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))
  :doc "counts raw output bytes independently of decoded characters"
  (let ((niwa-sandbox-mode 'off))
    (let ((result
           (niwa-execution-run-one-shot
            :name "niwa-test-byte-count"
            :command '("sh" "-c" "printf '\\303\\244'")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (equal (string #xe4) (plist-get result :output)))
      (should (= 2 (plist-get result :output-bytes)))))
  :doc "reports spawn failure without exposing a process"
  (let ((niwa-sandbox-mode 'off))
    (let ((result
           (niwa-execution-run-one-shot
            :name "niwa-test-failed-spawn"
            :command '("/definitely/missing/niwa-executable")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (= -1 (plist-get result :exit-code)))
      (should (plist-get result :error))
      (should-not (plist-member result :process))))
  :doc "refuses a failed confined launcher without retrying unrestricted"
  (let* ((root (make-temp-file "niwa-fallback-proof-" t))
         (replayed (file-name-concat root "replayed"))
         (niwa-sandbox--probe-cache nil)
         (niwa-execution--orphan-state nil)
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'display-warning) #'ignore)
                  ((symbol-function 'niwa-sandbox-prepare)
                   (lambda (&rest _)
                     (list
                      :state 'confined :marker "not-emitted"
                      :command '("sh" "-c" "exit 125")
                      :facts
                      '(:sandbox bubblewrap :filesystem workspace-write
                        :network isolated)))))
          (setq result
                (niwa-execution-run-one-shot
                 :name "niwa-test-fallback-proof"
                 :command (list "sh" "-c" "printf replayed > \"$1\"" "fixture" replayed)
                 :workdir root :writable-roots (list root)))
          (should-not (file-exists-p replayed))
          (should (= 125 (plist-get result :exit-code)))
          (should
           (plist-get (plist-get result :sandbox-facts)
                      :refused)))
      (delete-directory root t)))
  :doc "never replays a command after a signal, timeout, or emitted marker"
  (let* ((root (make-temp-file "niwa-fallback-uncertain-" t))
         (niwa-execution-process--child-kill-delay 0.05))
    (unwind-protect
        (dolist
            (case
             `(("signal" ("sh" "-c" "kill -TERM $$") nil)
               ("timeout" ("sh" "-c" "sleep 30") 0.05)
               ("command" ("sh" "-c" "printf '%s\\n' command-started; exit 7")
                nil)))
          (pcase-let* ((`(,name ,launcher ,timeout) case)
                       (replayed (file-name-concat root name))
                       (marker (if (equal name "command")
                                   "command-started"
                                 "not-emitted")))
            (cl-letf (((symbol-function 'niwa-sandbox-prepare)
                       (lambda (&rest _)
                         (list
                          :state 'confined :marker marker
                          :command launcher
                          :facts
                          '(:sandbox bubblewrap :filesystem workspace-write
                            :network isolated)))))
              (niwa-execution-run-one-shot
               :name (format "niwa-test-no-replay-%s" name)
               :command (list "sh" "-c" "printf replayed > \"$1\"" "fixture" replayed)
               :workdir root :writable-roots (list root)
               :timeout timeout))
            (should-not (file-exists-p replayed))))
      (delete-directory root t)))
  :doc "applies stable child defaults while allowing command overrides"
  (let ((niwa-sandbox-mode 'off))
    (let ((defaults
           (niwa-execution-run-one-shot
            :name "niwa-test-environment"
            :command
            '("sh" "-c"
              "printf '%s' \"$NO_COLOR|$TERM|$LC_ALL|$LANG|$COLORTERM|$PAGER|$GIT_PAGER|$GH_PAGER|$NIWA_EXECUTION\"")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory)))
          (override
           (niwa-execution-run-one-shot
            :name "niwa-test-environment-override"
            :command '("sh" "-c" "PAGER=less; printf '%s' \"$PAGER\"")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (equal "1|dumb|C.UTF-8|C.UTF-8||cat|cat|cat|1"
                     (plist-get defaults :output)))
      (should (equal "less" (plist-get override :output)))))
  :doc "terminates a timed-out command and its process group"
  (let* ((root (make-temp-file "niwa-execution-timeout-" t))
         (pid-file (file-name-concat root "child.pid"))
         (niwa-sandbox-mode 'off)
         (niwa-execution-process--child-kill-delay 0.05)
         result pid)
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (setq result
                (test-niwa-execution--timeout-after-ready
                 pid-file
                 (lambda ()
                   (niwa-execution-run-one-shot
                    :name "niwa-test-timeout"
                    :command
                    (list "sh" "-c"
                          "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; wait"
                          "niwa-test-timeout" pid-file)
                    :workdir root
                    :writable-roots (list root)
                    :timeout 0.1))))
          (should (plist-get result :timed-out-p))
          (should (file-readable-p pid-file))
          (setq pid (test-niwa-execution--read-pid pid-file))
          (with-timeout (5 (error "Descendant process survived timeout"))
            (while (not (test-niwa-execution--process-gone-p pid))
              (accept-process-output nil 0.02))))
      (delete-directory root t)))
  :doc "drains local descendants after the main process exits normally"
  (let* ((root (make-temp-file "niwa-execution-normal-exit-" t))
         (pid-file (file-name-concat root "child.pid"))
         (niwa-sandbox-mode 'off)
         (niwa-execution-process--child-kill-delay 0.05)
         result pid)
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (setq result
                (niwa-execution-run-one-shot
                 :name "niwa-test-normal-exit"
                 :command
                 (list "sh" "-c"
                       (concat
                        "trap '' HUP; sleep 30 </dev/null >/dev/null 2>&1 & "
                        "child=$!; printf '%s' \"$child\" > \"$1\"; exit 0")
                       "niwa-test-normal-exit" pid-file)
                 :workdir root
                 :writable-roots (list root)))
          (should (= 0 (plist-get result :exit-code)))
          (should (file-readable-p pid-file))
          (setq pid (test-niwa-execution--read-pid pid-file))
          (should (test-niwa-execution--process-gone-p pid)))
      (when (and pid (not (test-niwa-execution--process-gone-p pid)))
        (ignore-errors (signal-process pid 'KILL)))
      (delete-directory root t)))
  :doc "spools large output without retaining a process buffer"
  (let ((niwa-sandbox-mode 'off)
        (niwa-execution-process-output-limit (* 2 1024 1024)))
    (let ((result
           (niwa-execution-run-one-shot
            :name "niwa-test-large-output"
            :command
            '("sh" "-c"
              "printf head; head -c 1048576 /dev/zero | tr '\\0' x; printf tail")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (= 0 (plist-get result :exit-code)))
      (should (= (+ 8 1048576) (length (plist-get result :output))))
      (should (string-prefix-p "head" (plist-get result :output)))
      (should (string-suffix-p "tail" (plist-get result :output)))
      (should-not (get-buffer " *niwa-test-large-output*"))))
  :doc "enforces the output spool cap and reports the limit"
  (let ((niwa-sandbox-mode 'off)
        (niwa-execution-process-output-limit 4096)
        (niwa-execution-process--child-kill-delay 0.05))
    (let ((result
           (niwa-execution-run-one-shot
            :name "niwa-test-output-cap"
            :command
            '("sh" "-c" "head -c 100000 /dev/zero | tr '\\0' x")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (plist-get result :output-limit-p))
      (should (= 4096 (string-bytes (plist-get result :output))))))
  :doc "allows output exactly equal to the configured spool cap"
  (let ((niwa-sandbox-mode 'off)
        (niwa-execution-process-output-limit 4096))
    (let ((result
           (niwa-execution-run-one-shot
            :name "niwa-test-exact-output-cap"
            :command '("sh" "-c" "head -c 4096 /dev/zero | tr '\\0' x")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (= 0 (plist-get result :exit-code)))
      (should-not (plist-get result :output-limit-p))
      (should (= 4096 (plist-get result :output-bytes)))))
  :doc "does not plant Git or workspace metadata in the system temporary root"
  (progn
    (skip-unless (plist-get (niwa-sandbox-probe) :available))
    (let* ((root (make-temp-file "niwa-one-shot-temporary-root-" t))
           (temporary-file-directory (file-name-as-directory root))
           (niwa-sandbox-mode 'required))
      (unwind-protect
          (let ((result
                 (niwa-execution-run-one-shot
                  :name "niwa-test-temporary-root"
                  :command
                  (list "sh" "-c"
                        (concat "for path in .git .niwa; do "
                                "test ! -e \"$path\" || printf '%s\\n' \"$path\"; done"))
                  :workdir root :writable-roots (list root))))
            ;; Observe the mounted tree while the child is alive, not just
            ;; after synthetic mount-target cleanup has removed an empty .git.
            (should (= 0 (plist-get result :exit-code)))
            (should (equal "" (plist-get result :output)))
            (should (eq 'bubblewrap
                        (plist-get (plist-get result :sandbox-facts) :sandbox)))
            (should-not (file-exists-p (file-name-concat root ".git")))
            (should-not (file-exists-p (file-name-concat root ".niwa"))))
        (delete-directory root t)))))


;;
;;; Managed Bash execution

(niwa-deftest niwa-execution--retain-output ()
  ,test
  (test)
  :doc "keeps a bounded whole-artifact head and tail across chunks"
  (let ((niwa-execution-inline-output-limit 3)
        (record (niwa-execution--record-create)))
    (niwa-execution--retain-output record "ab")
    (niwa-execution--retain-output record "cde")
    (should (= 5 (niwa-execution--record-output-chars record)))
    (should (equal "abc" (niwa-execution--record-output-head record)))
    (should (equal "cde" (niwa-execution--record-output-tail record)))
    (should (= 5 (niwa-execution--record-unread-chars record)))
    (should (equal "abc" (niwa-execution--record-unread-head record)))
    (should (equal "cde" (niwa-execution--record-unread-tail record)))))

(niwa-deftest niwa-execution--unread-preview ()
  ,test
  (test)
  :doc "uses shared character preview semantics and reports omitted bytes"
  (let ((niwa-execution-inline-output-limit 10)
        (record
         (niwa-execution--record-create
          :unread-chars 20
          :unread-head "1234567890"
          :unread-tail "abcdefghij")))
    (let ((preview (niwa-execution--unread-preview record 20)))
      (should (string-match-p "omitted 10 chars"
                              (plist-get preview :output)))
      (should (= 10 (plist-get preview :omitted))))))

(niwa-deftest niwa-execution--unread-preview-artifact ()
  ,test
  (test)
  :doc "unread preview of a yielded execution names its readable artifact"
  (let* ((root (make-temp-file "niwa-execution-marker-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         (niwa-execution-inline-output-limit 20)
         (finish (file-name-concat root "finish"))
         initial final id)
    (niwa-execution-start-bash
     (lambda (value) (setq initial value))
     :session session :owner "main" :owner-context session
     :command (list "sh" "-c"
                    "sleep 0.2; printf '%0300d' 0; while test ! -e \"$1\"; do sleep .01; done"
                    "marker" finish)
     :workdir root :writable-roots (list root)
     :artifact-directory
     (file-name-concat (niwa-session-save-path session)
                       "tool-results" "executions")
     :yield-time-ms 10)
    (test-niwa-execution--wait (lambda () initial))
    (setq id (plist-get (plist-get initial :facts) :execution-id))
    (with-temp-file finish)
    (setq final (test-niwa-execution--observe session id))
    (let ((address (plist-get (plist-get final :facts) :output-path))
          (output (plist-get final :output)))
      (should (string-prefix-p "artifact://executions/" address))
      (should (string-match-p "tool output truncated" output))
      (should (string-match-p
               (regexp-quote (format "Read(file_path=%S, offset=1" address))
               output)))))

(niwa-deftest niwa-execution--collect-stderr ()
  ,test
  (test)
  :doc "counts collected stderr and marks a capped collection in the preview"
  (let ((record (niwa-execution--record-create
                 :child 'child :newline-count 0 :output-chars 0
                 :output-head "" :output-tail "")))
    (cl-letf (((symbol-function 'niwa-execution-process-collect-stderr)
               (lambda (_child) (cons "err\n" t))))
      (niwa-execution--collect-stderr record))
    (should (= 1 (niwa-execution--record-newline-count record)))
    (should (niwa-execution--record-output-limit-p record))
    (should (string-match-p "err\n\n\\[stderr truncated at the output limit\\]"
                            (niwa-execution--record-unread-tail record))))
  :doc "reports a failed collection and still lets delivery proceed"
  (let ((record (niwa-execution--record-create :child 'child))
        warning)
    (cl-letf (((symbol-function 'niwa-execution-process-collect-stderr)
               (lambda (_child) (signal 'file-error '("Injected read failure")))))
      (niwa-test--with-captured-diagnostics warning
        (niwa-execution--collect-stderr record)))
    (should (string-match-p "Could not collect remote execution stderr" warning))
    (should-not (niwa-execution--record-output-limit-p record)))
  :doc "a child without a target file collects nothing"
  (let ((record (niwa-execution--record-create :child 'child)))
    (cl-letf (((symbol-function 'niwa-execution-process-collect-stderr)
               #'ignore))
      (niwa-execution--collect-stderr record))
    (should-not (niwa-execution--record-output-chars record))))

(niwa-deftest niwa-execution--managed-append ()
  ,test
  (test)
  :doc "ignores process-filter output delivered after terminal settlement"
  (let ((record (niwa-execution--record-create :finished-p t)))
    (niwa-execution--managed-append record "late output\n")
    (should-not (niwa-execution--record-output-chars record))))

(niwa-deftest niwa-execution--resolve-outcome ()
  ,test
  (test)
  :doc "accepts canonical outcomes and defaults without an adapter resolver"
  (should (eq 'success (niwa-execution--resolve-outcome nil 0 'exited)))
  (should (eq 'failure (niwa-execution--resolve-outcome nil 1 'exited)))
  (dolist (outcome '(success failure no-match different false))
    (should
     (eq outcome
         (niwa-execution--resolve-outcome
          (lambda (_exit-code _termination) outcome)
          1 'exited))))
  :doc "contains throwing and invalid adapter resolvers"
  (dolist (resolver
           (list (lambda (_exit-code _termination) (error "Resolver failed"))
                 (lambda (_exit-code _termination) 'invalid)))
    ;; Each bad resolver is reported once; the fallback outcome is what
    ;; these cases assert.
    (niwa-test--with-captured-diagnostics nil
      (should (eq 'success
                  (niwa-execution--resolve-outcome resolver 0 'exited)))
      (should (eq 'failure
                  (niwa-execution--resolve-outcome resolver 1 'exited))))))

(niwa-deftest niwa-execution--cancel-observer ()
  ,test
  (test)
  :doc "request abort detaches a waiting poll without consuming its output"
  (let* ((root (make-temp-file "niwa-managed-poll-abort-" t))
         (gate (file-name-concat root "finish"))
         (session (test-niwa-execution--session root))
         (request (niwa-request--create :session session))
         (niwa-sandbox-mode 'off)
         (niwa-execution-process--child-kill-delay 0.05)
         initial abandoned final id)
    (unwind-protect
        (progn
          (setq initial
                (test-niwa-execution--start-managed
                 session root
                 (list "sh" "-c"
                       "printf one; while test ! -e \"$1\"; do sleep .01; done; printf two"
                       "poll-abort" gate)
                 :yield-time-ms 10))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (test-niwa-execution--wait
           (lambda ()
             (unless (equal "one" (plist-get initial :output))
               (setf (plist-get initial :output)
                     (concat (plist-get initial :output)
                             (plist-get (test-niwa-execution--observe
                                         session id :wait-ms 0) :output))))
             (equal "one" (plist-get initial :output))))
          (should (equal "one" (plist-get initial :output)))
          (niwa-execution-observe
           session "main" id (lambda (value) (setq abandoned value))
           :wait-ms 5000 :request request)
          (niwa-request-drain-cancellers request)
          (should-not abandoned)
          (with-temp-file gate)
          (setq final (test-niwa-execution--observe session id))
          (should (equal "two" (plist-get final :output)))
          (should-not abandoned))
      (niwa-execution-teardown-session session)
      (delete-directory root t))))

(niwa-deftest niwa-execution-list ()
  ,test
  (test)
  :doc "lists yielded executions for the canonical owner only"
  (let* ((root (make-temp-file "niwa-managed-list-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         (niwa-execution-process--child-kill-delay 0.05)
         one two)
    (unwind-protect
        (progn
          (setq one
                (test-niwa-execution--start-managed
                 session root '("sh" "-c" "sleep 30")
                 :owner "agent--one"))
          (setq two
                (test-niwa-execution--start-managed
                 session root '("sh" "-c" "sleep 30")
                 :owner "agent--two"))
          (should (= 1 (length (niwa-execution-list
                                session "agent--one"))))
          (should (= 1 (length (niwa-execution-list
                                session "agent--two"))))
          (should-not (plist-member
                       (car (niwa-execution-list session "agent--one"))
                       :process))
          (test-niwa-execution--stop-all
           session "agent--one"
           (list (plist-get (plist-get one :facts) :execution-id)))
          (test-niwa-execution--stop-all
           session "agent--two"
           (list (plist-get (plist-get two :facts) :execution-id))))
      (delete-directory root t))))

(niwa-deftest niwa-execution-owner-live-p ()
  ,test
  (test)
  :doc "tracks unsettled processes only for their captured owner"
  (let* ((root (make-temp-file "niwa-managed-owner-live-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         (niwa-execution-process--child-kill-delay 0.05)
         initial id)
    (unwind-protect
        (progn
          (setq initial
                (test-niwa-execution--start-managed
                 session root '("sh" "-c" "sleep 30")
                 :owner "agent--owner"))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (should (niwa-execution-owner-live-p session "agent--owner"))
          (should-not (niwa-execution-owner-live-p session "main"))
          (test-niwa-execution--stop-all
           session "agent--owner" (list id))
          (should-not
           (niwa-execution-owner-live-p session "agent--owner")))
      (delete-directory root t))))

(niwa-deftest niwa-execution-stop ()
  ,test
  (test)
  :doc "stops the process group and returns unread terminal output"
  (let* ((root (make-temp-file "niwa-managed-stop-" t))
         (session (test-niwa-execution--session root))
         (pid-file (file-name-concat root "child.pid"))
         (niwa-sandbox-mode 'off)
         (niwa-execution-process--child-kill-delay 0.05)
         initial polled stopped id pid)
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (setq initial
                (test-niwa-execution--start-managed
                 session root
                 (list "sh" "-c"
                       "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; printf ready; wait"
                       "managed-stop" pid-file)))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (niwa-execution-observe
           session "main" id (lambda (value) (setq polled value))
           :wait-ms 300000)
          (test-niwa-execution--wait
           (lambda () (file-readable-p pid-file)))
          (niwa-execution-stop
           session "main" id (lambda (value) (setq stopped value)))
          (should (eq 'running
                      (plist-get (plist-get polled :facts) :state)))
          (test-niwa-execution--wait (lambda () stopped))
          (should (eq 'stopped
                      (plist-get (plist-get stopped :facts) :termination)))
          (should (plist-get stopped :claimed-final-p))
          (setq pid (test-niwa-execution--read-pid pid-file))
          (test-niwa-execution--wait
           (lambda () (test-niwa-execution--process-gone-p pid))))
      (delete-directory root t)))
  :doc "force-kills a PTY process group that ignores TERM"
  (let* ((root (make-temp-file "niwa-managed-pty-kill-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         (niwa-execution-process--child-kill-delay 0.05)
         initial stopped id)
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (setq initial
                (test-niwa-execution--start-managed
                 session root
                 '("sh" "-c"
                   "trap '' TERM; printf ready; while :; do sleep 1; done")
                 :tty t))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (should (niwa-execution-stop-user session id))
          (should-error
           (niwa-execution-observe
            session "main" id #'ignore :chars "late\n" :wait-ms 250)
           :type 'niwa-execution-input-error)
          (setq stopped (test-niwa-execution--observe session id))
          (should (eq 'stopped
                      (plist-get (plist-get stopped :facts) :termination)))
          (should (integerp
                   (plist-get (plist-get stopped :facts) :exit-code)))
          (should-not (zerop
                       (plist-get (plist-get stopped :facts) :exit-code)))
          (should (plist-get (plist-get stopped :facts) :tty)))
      (delete-directory root t))))

(niwa-deftest niwa-execution-stop-all-user ()
  ,test
  (test)
  :doc "stops root, sub-agent, and already-stopping executions session-wide"
  (let* ((root (make-temp-file "niwa-managed-stop-all-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         (niwa-execution-process--child-kill-delay 0.2)
         main main-id)
    (unwind-protect
        (progn
          (setq main
                (test-niwa-execution--start-managed
                 session root '("sh" "-c" "sleep 30")
                 :owner "main")
                main-id
                (plist-get (plist-get main :facts) :execution-id))
          (test-niwa-execution--start-managed
           session root '("sh" "-c" "sleep 30")
           :owner "agent--one")
          (niwa-execution-stop-user session main-id)
          (should (= 2 (niwa-execution-stop-all-user session)))
          (test-niwa-execution--wait
           (lambda () (zerop (niwa-execution-count-user session))))
          (should (= 0 (niwa-execution-stop-all-user session))))
      (niwa-execution-teardown-session session)
      (delete-directory root t))))

(niwa-deftest niwa-execution--discard-record/deferred
  (:doc "teardown and transport cancellation own queued natural completion")
  (dolist (yielded '(nil t))
    (dolist (reason '(session-ended owner-stopped transport-cancelled))
      (let* ((root (make-temp-file "niwa-deferred-teardown-" t))
             (session (test-niwa-execution--session root))
             (release (file-name-concat root "release"))
             (niwa-sandbox-mode 'off)
             (finish (symbol-function 'niwa-execution--finish-managed))
             (callbacks 0) observation torn-down key mail pending-after-teardown
             (niwa-execution-mailbox-delivery-function
              (lambda (_event _context ack) (setq mail t) (funcall ack t nil) t)))
        (unwind-protect
            (cl-letf (((symbol-function 'niwa-execution--finish-managed)
                       (lambda (record &optional defer)
                         (funcall finish record defer)
                         (when defer
                           (setq key (list 'execution-delivery
                                           (niwa-execution--record-token record)))
                           ;; No event-loop opportunity between settlement
                           ;; and teardown: delivery is still queued.
                           (pcase reason
                             ('session-ended (niwa-execution-teardown-session session))
                             ('owner-stopped (niwa-execution-stop-owner session "main"))
                             ('transport-cancelled (niwa-transport-cancel-pending key)))
                           (setq torn-down t
                                 pending-after-teardown
                                 (niwa-execution--record-delivery-pending-p record))))))
              (niwa-execution-start-bash
               (lambda (value) (cl-incf callbacks) (setq observation value))
               :session session :owner "main" :owner-context session
               :command (list "sh" "-c"
                              "while [ ! -e \"$1\" ]; do sleep 0.01; done; printf done"
                              "wait-for-release" release)
               :workdir root :writable-roots (list root)
               :artifact-directory root :yield-time-ms (and yielded 1))
              (when yielded
                (test-niwa-execution--wait (lambda () observation))
                (should (eq 'running (plist-get (plist-get observation :facts) :state))))
              (write-region "" nil release nil 'silent)
              (test-niwa-execution--wait (lambda () torn-down))
              (should-not (gethash key niwa-transport--pending))
              (should-not pending-after-teardown)
              (should-not (niwa-execution-session-live-p session))
              (should (= callbacks (if (or yielded (eq reason 'owner-stopped)) 1 0)))
              (when (and (not yielded) (eq reason 'owner-stopped))
                (should (equal "done" (plist-get observation :output)))
                (should (eq 'completed (plist-get (plist-get observation :facts) :state))))
              (accept-process-output nil 0.05)
              (should-not mail)
              (should (= callbacks (if (or yielded (eq reason 'owner-stopped)) 1 0))))
          (when key (niwa-transport-cancel-pending key))
          (niwa-execution-teardown-session session)
          (delete-directory root t))))))

(niwa-deftest niwa-execution--finish-managed/no-mailbox
  (:doc "forced completion without a mailbox clears delivery work but stays pollable")
  (let* ((root (make-temp-file "niwa-stopped-no-mailbox-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         (niwa-execution-mailbox-delivery-function nil)
         initial id record)
    (unwind-protect
        (progn
          (setq initial (test-niwa-execution--start-managed
                         session root '("sh" "-c" "sleep 30"))
                id (plist-get (plist-get initial :facts) :execution-id)
                record (niwa-execution--owned-yielded-record session "main" id))
          (niwa-execution-stop-user session id)
          (test-niwa-execution--wait
           (lambda () (niwa-execution--record-finished-p record)))
          (should-not (niwa-execution--record-delivery-pending-p record))
          (should-not (niwa-execution--record-delivery-state record))
          (should-not (niwa-execution-session-live-p session))
          (let ((final (test-niwa-execution--observe session id)))
            (should (eq 'stopped (plist-get (plist-get final :facts) :termination)))))
      (niwa-execution-teardown-session session)
      (delete-directory root t))))

(niwa-deftest niwa-execution-start-bash
  (:doc "runs managed commands through confinement and resource capture")
  ,test
  (test)
  :doc "a command that ends on its own delivers after its settlement returns"
  (let* ((root (make-temp-file "niwa-managed-deferred-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         (settling nil)
         (finish (symbol-function 'niwa-execution-process--finish))
         observation delivered-while-settling live-before-delivery)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-execution-process--finish)
                   (lambda (&rest args)
                     (setq settling t)
                     (unwind-protect (apply finish args)
                       (setq settling nil
                             live-before-delivery
                             (niwa-execution-session-live-p session))))))
          (niwa-execution-start-bash
           (lambda (value)
             (setq observation value
                   delivered-while-settling settling))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "printf done")
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (test-niwa-execution--wait (lambda () observation))
          (should-not delivered-while-settling)
          (should live-before-delivery)
          (should-not (niwa-execution-session-live-p session))
          (should (eq 'completed (plist-get (plist-get observation :facts) :state))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))
  :doc "retains the original approval across yield and independent completion"
  (let* ((root (make-temp-file "niwa-managed-approval-" t))
         (session (test-niwa-execution--session root))
         (approvals (list (list :source 'reviewer :outcome 'allow-once
                               :reason (copy-sequence "Reviewed command"))))
         observation terminal
         (niwa-execution-mailbox-delivery-function
          (lambda (event _owner ack)
            (setq terminal event)
            (funcall ack t nil)
            t)))
    (unwind-protect
        (progn
          (niwa-execution-start-bash
           (lambda (value) (setq observation value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "while test ! -e release; do sleep 0.02; done; printf done")
           :tool-use-id "reviewed-command"
           :permission-approvals approvals
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms 10)
          ;; Invocation metadata must be snapshotted, including string leaves.
          (aset (plist-get (car approvals) :reason) 0 ?X)
          (test-niwa-execution--wait (lambda () observation))
          (should (equal '((:source reviewer :outcome allow-once
                                   :reason "Reviewed command"))
                         (plist-get observation :permission-approvals)))
          (let ((snapshot (car (niwa-execution-list-user session))))
            (should (equal '((:source reviewer :outcome allow-once
                                     :reason "Reviewed command"))
                           (plist-get snapshot :permission-approvals)))
            (aset (plist-get (car (plist-get snapshot :permission-approvals)) :reason) 0 ?Y)
            (should (equal "Reviewed command"
                           (plist-get
                            (car (plist-get (car (niwa-execution-list-user session))
                                           :permission-approvals)) :reason))))
          (write-region "release" nil (file-name-concat root "release") nil 'silent)
          (test-niwa-execution--wait (lambda () terminal))
          (should (equal "reviewed-command" (plist-get terminal :tool-use-id)))
          (should (equal '((:source reviewer :outcome allow-once
                                   :reason "Reviewed command"))
                         (plist-get (plist-get terminal :observation)
                                    :permission-approvals)))
          (should (equal "done" (plist-get terminal :whole-output))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))
  :doc "queued full-access work cannot launch after a restrictive mode change"
  (let* ((root (make-temp-file "niwa-queued-mode-" t))
         (session (test-niwa-execution--session root))
         first second
         (settlements 0))
    (unwind-protect
        (progn
          (setf (niwa-session-permission-mode session) 'full-auto)
          (niwa-execution-start-bash
           (lambda (value) (setq first value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "touch ready; while test ! -e release; do sleep 0.02; done")
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (test-niwa-execution--wait
           (lambda () (file-exists-p (file-name-concat root "ready"))))
          (niwa-execution-start-bash
           (lambda (value) (setq second value) (cl-incf settlements))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "touch obsolete-authority")
           :sandbox-permissions 'require-escalated
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (setf (niwa-session-permission-mode session) 'edits)
          (write-region "release" nil (file-name-concat root "release") nil 'silent)
          (test-niwa-execution--wait (lambda () (and first second)))
          (should (= 1 settlements))
          (should (= -1 (plist-get (plist-get second :facts) :exit-code)))
          (should-not (file-exists-p (file-name-concat root "obsolete-authority"))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))
  :doc "settles a failed confined launcher once without a replacement process"
  (let* ((root (make-temp-file "niwa-managed-fallback-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox--probe-cache nil)
         (niwa-execution--orphan-state nil)
         (callbacks 0)
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'display-warning) #'ignore)
                  ((symbol-function 'niwa-sandbox-prepare)
                   (lambda (&rest _)
                     (list
                      :state 'confined :marker "not-emitted"
                      :command '("sh" "-c" "exit 125")
                      :facts
                      '(:sandbox bubblewrap :filesystem workspace-write
                        :network isolated)))))
          (niwa-execution-start-bash
           (lambda (value)
             (setq callbacks (1+ callbacks)
                   result value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "printf direct")
           :tool-args '(:command "printf direct")
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (test-niwa-execution--wait (lambda () result))
          (accept-process-output nil 0.1)
          (should (= 1 callbacks))
          (should (= 125 (plist-get (plist-get result :facts) :exit-code)))
          (should-not (string-match-p "direct" (plist-get result :output))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))

  :doc "passes raw command text and argv to native resource capture"
  (let* ((root (make-temp-file "niwa-managed-resource-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         captured result)
    (unwind-protect
        (cl-letf
             (((symbol-function
               'niwa-execution-telemetry-prepare-resource-capture)
              (lambda (context command-text command)
                (setq captured (list context command-text command))
                nil)))
          (niwa-execution-start-bash
           (lambda (value) (setq result value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "printf ok")
           :tool-args '(:command "printf ok")
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (test-niwa-execution--wait (lambda () result))
          (should (car captured))
          (should (equal '("printf ok" ("sh" "-c" "printf ok"))
                         (cdr captured))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))

  :doc "delayed admission uses the data buffer's frozen protected-path policy"
  (let* ((root (make-temp-file "niwa-managed-policy-" t))
         (session (test-niwa-execution--session root))
         (data-buffer (generate-new-buffer " *niwa-policy-source*"))
         (original-policy (default-value 'niwa-protected-paths))
         (frozen-policy '(("/frozen/**" . inaccessible)))
         (ambient-policy '(("/ambient/**" . read-only)))
         admission captured-policy)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local niwa-protected-paths frozen-policy))
          (cl-letf
              (((symbol-function 'niwa-execution-scheduler-submit)
                (lambda (_scheduler _mode start &optional _admit-p _reject _scope)
                  (setq admission start)
                  nil))
               ((symbol-function 'niwa-sandbox-prepare)
                (lambda (&rest _)
                  (setq captured-policy (copy-tree niwa-protected-paths))
                  '(:state refused :error "test refusal"
                    :facts (:sandbox refused :filesystem unavailable
                            :network unavailable :refused t)))))
            (niwa-execution-start-bash
             #'ignore
             :session session :data-buffer data-buffer
             :owner "main" :owner-context session
             :command '("ignored") :tool-args '(:command "ignored")
             :workdir root :writable-roots (list root)
             :artifact-directory root :yield-time-ms nil)
            (should admission)
            (setq-default niwa-protected-paths ambient-policy)
            (with-temp-buffer
              (funcall admission nil)))
          (should (equal frozen-policy captured-policy)))
      (setq-default niwa-protected-paths original-policy)
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))

  :doc "rejects a queued mutation when an earlier remote outcome becomes unknown"
  (let* ((root (make-temp-file "niwa-managed-queued-unknown-" t))
         (session (test-niwa-execution--session root))
         (state (niwa-execution--state-for-session session))
         admissible reject result)
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-execution-scheduler-submit)
              (lambda (_scheduler _mode _start &optional admit-p rejection _scope)
                (setq admissible admit-p
                      reject rejection)
                nil)))
          (niwa-execution-start-bash
           (lambda (value) (setq result value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "printf blocked")
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (should (funcall admissible))
          (setf (niwa-execution--state-unknown-outcome state)
                '(:group-id 42 :workdir "/ssh:host:/project/"))
          (should-not (funcall admissible))
          (funcall reject nil)
          (should (eq 'unknown
                      (plist-get (plist-get result :facts) :termination))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))

  :doc "blocks mutating execution until an unknown remote outcome is acknowledged"
  (let* ((root (make-temp-file "niwa-managed-unknown-" t))
         (session (test-niwa-execution--session root))
         (state (niwa-execution--state-for-session session)))
    (unwind-protect
        (progn
          (setf (niwa-execution--state-unknown-outcome state)
                '(:group-id 42 :workdir "/ssh:host:/project/"))
          (should (niwa-execution-mutation-blocked-p session))
          (should-error
           (niwa-execution-start-bash
            #'ignore :session session :owner "main"
            :command '("sh" "-c" "printf blocked")
            :workdir root :writable-roots (list root)
            :artifact-directory root :yield-time-ms nil)
           :type 'niwa-execution-error)
          (niwa-execution-acknowledge-unknown session)
          (should-not (niwa-execution-mutation-blocked-p session)))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))

  :doc "re-proving a dead process group clears the block without a human"
  (let* ((root (make-temp-file "niwa-managed-reprove-" t))
         (session (test-niwa-execution--session root))
         (state (niwa-execution--state-for-session session))
         asked)
    (unwind-protect
        (cl-letf (((symbol-function
                    'niwa-execution-process-remote-group-status)
                   (lambda (group-id start-time workdir)
                     (setq asked (list group-id start-time workdir))
                     'dead)))
          (setf (niwa-execution--state-unknown-outcome state)
                '(:group-id 42 :group-start-time "981"
                  :workdir "/ssh:host:/project/"))
          (should (niwa-execution-mutation-blocked-p session))
          (should (niwa-execution-reprove-unknown-outcome session))
          (should (equal '(42 "981" "/ssh:host:/project/") asked))
          (should-not (niwa-execution-mutation-blocked-p session))
          (should-not (niwa-execution-mutation-refused-p session)))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))

  :doc "a group that is live, ambiguous, or unreachable stays blocked"
  (dolist (answer '(live ambiguous signal))
    (let* ((root (make-temp-file "niwa-managed-unproved-" t))
           (session (test-niwa-execution--session root))
           (state (niwa-execution--state-for-session session)))
      (unwind-protect
          (cl-letf (((symbol-function
                      'niwa-execution-process-remote-group-status)
                     (lambda (&rest _)
                       (if (eq answer 'signal)
                           (error "Target process status probe timed out")
                         answer))))
            (setf (niwa-execution--state-unknown-outcome state)
                  '(:group-id 42 :group-start-time "981"
                    :workdir "/ssh:host:/project/"))
            (should-not (niwa-execution-reprove-unknown-outcome session))
            (should (niwa-execution-mutation-refused-p session)))
        (niwa-execution-teardown-session session)
        (delete-directory root t))))

  :doc "an outcome without process identity never reaches the target"
  (let* ((root (make-temp-file "niwa-managed-anonymous-" t))
         (session (test-niwa-execution--session root))
         (state (niwa-execution--state-for-session session)))
    (unwind-protect
        (progn
          (setf (niwa-execution--state-unknown-outcome state)
                '(:group-id 42 :workdir "/ssh:host:/project/"))
          (should-not (niwa-execution-reprove-unknown-outcome session))
          (should (niwa-execution-mutation-refused-p session)))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))

  :doc "a busy transport leaves the block in place without probing the target"
  (let* ((root (make-temp-file "niwa-managed-busy-reprove-" t))
         (session (test-niwa-execution--session root))
         (state (niwa-execution--state-for-session session)))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-transport-busy-p)
                   (lambda (&optional _) t))
                  ((symbol-function
                    'niwa-execution-process-remote-group-status)
                   (lambda (&rest _)
                     (error "Busy transport must not reach the target"))))
          (setf (niwa-execution--state-unknown-outcome state)
                '(:group-id 42 :group-start-time "981"
                  :workdir "/ssh:host:/project/"))
          (should-not (niwa-execution-reprove-unknown-outcome session))
          (should (niwa-execution-mutation-refused-p session)))
      (niwa-execution-teardown-session session)
      (delete-directory root t))))

(niwa-deftest niwa-execution--artifact-address
  (:doc "publishes logical addresses and hides non-session artifact paths")
  ,test
  (test)
  (let* ((root (make-temp-file "niwa-execution-address-" t))
         (session (test-niwa-execution--session root))
         (artifact-directory
          (file-name-concat (niwa-session-save-path session)
                            "tool-results" "executions"))
         (niwa-sandbox-mode 'off)
         (temporary-artifact-directory (file-name-concat root "artifacts"))
         (finish (file-name-concat root "finish"))
         (finish-yielded (file-name-concat root "finish-yielded"))
         (finish-pending (file-name-concat root "finish-pending"))
         (resolutions 0)
         (count (lambda (original &rest args)
                  (cl-incf resolutions)
                  (apply original args)))
         initial final id yielded foreground pending-final)
    (advice-add 'niwa-resource-artifact-address :around count)
    (unwind-protect
        (progn
          (let ((arm (symbol-function 'niwa-execution--arm-managed-timers)))
            (cl-letf (((symbol-function 'niwa-execution--arm-managed-timers)
                       (lambda (record)
                         ;; The first observation must follow output, not shell startup.
                         (test-niwa-execution--wait
                          (lambda ()
                            (equal "first"
                                   (niwa-execution--record-output-head record))))
                         (funcall arm record))))
              (niwa-execution-start-bash
               (lambda (value) (setq initial value))
               :session session :owner "main" :owner-context session
               :command (list "sh" "-c"
                              "printf first; while test ! -e \"$1\"; do sleep .01; done; printf second"
                              "artifact-address" finish)
               :workdir root :writable-roots (list root)
               :artifact-directory artifact-directory :yield-time-ms 10)))
          (test-niwa-execution--wait (lambda () initial))
          (should (eq 'running (plist-get (plist-get initial :facts) :state)))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (let ((address (plist-get (plist-get initial :facts) :output-path)))
            (should (string-prefix-p "artifact://executions/" address))
            (should-not (file-name-absolute-p address))
            (should-not (string-match-p (regexp-quote root) address)))
          (with-temp-file finish)
          (setq final (test-niwa-execution--observe session id))
          (should (eq 'completed (plist-get (plist-get final :facts) :state)))
          (should (equal (plist-get (plist-get initial :facts) :output-path)
                         (plist-get (plist-get final :facts) :output-path)))
          ;; Its events share one resolution of the unchanged spool path.
          (should (= 1 resolutions))
          (should (string-match-p "first" (plist-get initial :output)))
          (should (string-match-p "second" (plist-get final :output)))
          (niwa-execution-start-bash
           (lambda (value) (setq yielded value))
           :session session :owner "main" :owner-context session
           :command (list "sh" "-c"
                          "while test ! -e \"$1\"; do sleep .01; done; printf yielded"
                          "artifact-address" finish-yielded)
           :workdir root :writable-roots (list root)
           :artifact-directory temporary-artifact-directory :yield-time-ms 10)
          (test-niwa-execution--wait (lambda () yielded))
          (should (eq 'running (plist-get (plist-get yielded :facts) :state)))
          (should-not (plist-get (plist-get yielded :facts) :output-path))
          (with-temp-file finish-yielded)
          (niwa-execution-start-bash
           (lambda (value) (setq foreground value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "printf foreground")
           :tool-args '(:command "printf foreground")
           :workdir root :writable-roots (list root)
           :artifact-directory temporary-artifact-directory
           :yield-time-ms nil)
          (test-niwa-execution--wait (lambda () foreground))
          (should-not (plist-get (plist-get foreground :facts) :output-path))
          (niwa-execution-start-bash
           (lambda (value) (setq pending-final value))
           :session session :owner "main" :owner-context session
           :command (list "sh" "-c"
                          "while test ! -e \"$1\"; do sleep .01; done; printf pending"
                          "artifact-address" finish-pending)
           :workdir root :writable-roots (list root)
           :artifact-directory temporary-artifact-directory
           :yield-time-ms nil)
          (let* ((state (niwa-session-execution-state session))
                 (pending-id
                  (format "%s-%06d"
                          (niwa-execution--state-id-prefix state)
                          (niwa-execution--state-next-id state)))
                 (record (gethash pending-id
                                  (niwa-execution--state-records state)))
                 (spool (niwa-execution--spool-path record)))
            (should (string-match-p "\.niwa-pending-executions"
                                    spool))
            (should-not (niwa-resource-artifact-address spool session)))
          (with-temp-file finish-pending)
          (test-niwa-execution--wait (lambda () pending-final)))
      (advice-remove 'niwa-resource-artifact-address count)
      (niwa-execution-teardown-session session)
      (delete-directory root t))))

(provide 'test-niwa-execution)
;;; test-niwa-execution.el ends here
