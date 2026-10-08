;;; test-mevedel-execution.el --- Tests for managed child execution -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the bounded one-shot process boundary used by Bash, batch Eval, and
;; native external helpers.

;;; Code:

(require 'cl-lib)
(require 'mevedel-agents)
(require 'mevedel-execution)
(require 'mevedel-execution-process)
(require 'mevedel-resource)
(require 'mevedel-sandbox)
(require 'mevedel-structs)
(require 'mevedel-telemetry)
(require 'mevedel-transport)
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

(mevedel-deftest mevedel-execution--next-id ()
  ,test
  (test)
  :doc "keeps IDs distinct across fresh session states with restarted counters"
  (let* ((first (mevedel-execution--new-state))
         (second (mevedel-execution--new-state))
         (first-id (mevedel-execution--next-id first))
         (second-id (mevedel-execution--next-id second)))
    (should (string-match-p "\\`exec-[[:alnum:]]+-000001\\'" first-id))
    (should (string-suffix-p "-000002" (mevedel-execution--next-id first)))
    (should-not (equal first-id second-id))))

(mevedel-deftest mevedel-execution--terminal-event ()
  ,test
  (test)
  :doc "marks preview truncation independently of observation omissions"
  (let ((mevedel-execution-inline-output-limit 6)
        (record (mevedel-execution--record-create)))
    (cl-letf (((symbol-function 'mevedel-execution--event)
               (lambda (_record _type &rest properties) properties)))
      (mevedel-execution--retain-output record "abcdefghi")
      (let ((event (mevedel-execution--terminal-event
                    record 'model '(:facts (:omitted-output-bytes 0)))))
        (should (plist-get event :whole-output-truncated-p))
        (should (= 0 (plist-get (plist-get event :facts)
                                :omitted-output-bytes)))))))

(mevedel-deftest mevedel-execution--emit-progress ()
  ,test
  (test)
  :doc "marks cumulative preview truncation on live events"
  (let ((mevedel-execution-inline-output-limit 6)
        (record (mevedel-execution--record-create)) event)
    (mevedel-execution--retain-output record "abcdefghi")
    (cl-letf (((symbol-function 'mevedel-execution--event)
               (lambda (_record _type &rest properties) properties))
              ((symbol-function 'mevedel-execution--emit-event)
               (lambda (value) (setq event value)))
              ((symbol-function 'mevedel--ui-timer-activate) (lambda (&rest _) nil)))
      (mevedel-execution--emit-progress record))
    (should (plist-get event :output-preview-truncated-p)))

  :doc "shares whole-second quiet updates without violating the output rate bound"
  (let ((record (mevedel-execution--record-create)) timer)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-execution--event) #'ignore)
                  ((symbol-function 'mevedel-execution--emit-event) #'ignore))
          (mevedel-execution--emit-progress record)
          (setq timer (mevedel-execution--record-progress-timer record))
          (let ((due (float-time (timer--time timer)))
                (emitted (mevedel-execution--record-progress-emitted-at record)))
            (should (<= 0.25 (- due emitted) 1.26))
            (should (= due (floor due))))
          (should (mevedel--ui-timer-pending-p timer))
          (should-not (memq timer timer-list))
          (timer-set-time timer (time-add nil 1))
          (mevedel-execution--hasten-progress record)
          ;; Output moves the same event out of the shared queue, earlier.
          (should (eq timer (mevedel-execution--record-progress-timer record)))
          (should-not (memq timer mevedel--coalesced-timers))
          (should (memq timer timer-list))
          (should (< (float-time (timer--time timer)) (+ (float-time) 0.3))))
      (mevedel--ui-timer-cancel timer)
      (mevedel--ui-timer-cancel (mevedel-execution--record-progress-timer record))))

  :doc "honours an output interval longer than the quiet interval"
  (let ((record (mevedel-execution--record-create))
        (mevedel-execution-progress-interval 3)
        timer)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-execution--event) #'ignore)
                  ((symbol-function 'mevedel-execution--emit-event) #'ignore))
          (mevedel-execution--emit-progress record)
          (setq timer (mevedel-execution--record-progress-timer record))
          (let ((due (float-time (timer--time timer)))
                (emitted (mevedel-execution--record-progress-emitted-at record)))
            ;; At least the interval apart, still on a shared whole second.
            (should (= due (floor due)))
            (should (<= 3 (- due emitted) 4.01))))
      (mevedel--ui-timer-cancel timer)))

  :doc "terminal cleanup cancels shared quiet progress without stopping other jobs"
  (let ((record (mevedel-execution--record-create)) timer other)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-execution--event) #'ignore)
                  ((symbol-function 'mevedel-execution--emit-event) #'ignore))
          (setq other (mevedel--run-periodic-timer 1 #'ignore))
          (mevedel-execution--emit-progress record)
          (setq timer (mevedel-execution--record-progress-timer record))
          (mevedel-execution--release-runtime record)
          (should-not (mevedel--ui-timer-pending-p timer))
          (should (mevedel--ui-timer-pending-p other)))
      (mevedel--ui-timer-cancel timer)
      (mevedel--ui-timer-cancel other))))

(mevedel-deftest mevedel-execution--hasten-progress ()
  ,test
  (test)
  :doc "brings a quiet wait forward to the output interval"
  (let* ((record (mevedel-execution--record-create
                  :progress-emitted-at (- (float-time) 0.1)
                  :progress-timer (run-at-time 0.9 nil #'ignore)))
         (old (mevedel-execution--record-progress-timer record)))
    (unwind-protect
        (progn
          (mevedel-execution--hasten-progress record)
          (let ((timer (mevedel-execution--record-progress-timer record)))
            (should (eq old timer))
            (should (= 1 (cl-count old timer-list)))
            (should (< (float-time (timer--time timer)) (+ (float-time) 0.2)))
            ;; Further output before that event leaves it alone.
            (mevedel-execution--hasten-progress record)
            (should (eq timer (mevedel-execution--record-progress-timer record)))))
      (cancel-timer old)
      (cancel-timer (mevedel-execution--record-progress-timer record))))

  :doc "keeps the initial delay before the first event"
  (let ((record (mevedel-execution--record-create
                 :progress-timer (run-at-time 2 nil #'ignore))))
    (unwind-protect
        (let ((timer (mevedel-execution--record-progress-timer record)))
          (mevedel-execution--hasten-progress record)
          (should (eq timer (mevedel-execution--record-progress-timer record))))
      (cancel-timer (mevedel-execution--record-progress-timer record))))

  :doc "moves a suspended event instead of arming a second one"
  (let* ((record (mevedel-execution--record-create
                  :progress-emitted-at (- (float-time) 0.1)
                  :progress-timer (run-at-time 0.9 nil #'ignore)))
         (old (mevedel-execution--record-progress-timer record)))
    (unwind-protect
        (progn
          (mevedel-transport-with-exclusive-connection
            (mevedel-execution--hasten-progress record))
          (should (eq old (mevedel-execution--record-progress-timer record)))
          (should (= 1 (cl-count old timer-list)))
          (should (< (float-time (timer--time old)) (+ (float-time) 0.2))))
      (mevedel--ui-timer-cancel old))))

(mevedel-deftest mevedel-execution--user-snapshot ()
  ,test
  (test)
  :doc "live guest snapshots retain cumulative tail truncation independent of poll omissions"
  (let ((mevedel-execution-inline-output-limit 6)
        (record (mevedel-execution--record-create
                 :execution-id "exec-1"
                 :origin (mevedel-execution--origin-create :owner "/root"))))
    (cl-letf (((symbol-function 'mevedel-execution--facts)
               (lambda (_) '(:state running :omitted-output-bytes 0))))
      (mevedel-execution--retain-output record "abcdef")
      (should-not (plist-get (mevedel-execution--user-snapshot record)
                             :output-preview-truncated-p))
      (mevedel-execution--retain-output record "ghi")
      (let ((snapshot (mevedel-execution--user-snapshot record)))
        (should (equal "defghi" (plist-get snapshot :output-tail)))
        (should (plist-get snapshot :output-preview-truncated-p))
        (should (= 0 (plist-get snapshot :omitted-output-bytes)))))))

(mevedel-deftest mevedel-execution-start-one-shot ()
  ,test
  (test)
  :doc "settles a child that exits before launch setup resumes"
  (let ((original-make-process (symbol-function 'make-process))
        (original-accept-process-output
         (symbol-function 'accept-process-output))
        (mevedel-sandbox-mode 'off)
        done result)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((process (apply original-make-process args)))
                   (while (process-live-p process)
                     (funcall original-accept-process-output
                              process 0.01 nil t))
                   process))))
      (mevedel-execution-start-one-shot
       (lambda (child-result)
         (setq result child-result
               done t))
       :name "mevedel-test-immediate-exit"
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
  (let ((original-make-process (symbol-function 'make-process))
        (mevedel-sandbox-mode 'off)
        done result)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (apply original-make-process
                        (plist-put args :sentinel #'ignore)))))
      (should
       (functionp
        (mevedel-execution-start-one-shot
         (lambda (child-result)
           (setq result child-result
                 done t))
         :name "mevedel-test-missed-sentinel"
         :command '("sh" "-c" "printf recovered; exit 7")
         :workdir temporary-file-directory
         :writable-roots (list temporary-file-directory))))
      (with-timeout (2 (error "Missed sentinel was not recovered"))
        (while (not done)
          (accept-process-output nil 0.05)))
      (should (= 7 (plist-get result :exit-code)))
      (should (equal "recovered" (plist-get result :output)))))
  :doc "passes the session sandbox policy to the child boundary"
  ;; A private directory: protected-path discovery walks the workdir, and
  ;; parallel test workers churn the shared temporary directory under it.
  (let ((session (mevedel-session--create :sandbox-mode 'required))
        (mevedel-sandbox-mode 'off)
        (workdir (file-name-as-directory
                  (make-temp-file "mevedel-one-shot-mode-" t)))
        captured-mode done)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-sandbox-prepare)
                   (lambda (command _workdir _roots
                                    &optional _additional _permissions mode
                                    _temporary-root _candidates)
                     (setq captured-mode mode)
                     (list :state 'unrestricted
                           :command command
                           :facts '(:sandbox off
                                    :filesystem unrestricted
                                    :network unrestricted)))))
          (mevedel-execution-start-one-shot
           (lambda (_child-result) (setq done t))
           :name "mevedel-test-session-sandbox-mode"
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
  (skip-unless (plist-get (mevedel-sandbox-probe) :available))
  (dolist (mode '(off required))
    (dolist (cancel-p '(nil t))
      (let* ((root (make-temp-file "mevedel-one-shot-contract-" t))
             (ready (file-name-concat root "ready"))
             (protected (file-name-concat root "protected-missing"))
             (mevedel-protected-paths
              `((,(concat protected "/**") . inaccessible)))
             (mevedel-sandbox-mode mode)
             (mevedel-execution-process--child-kill-delay 0.05)
             (mevedel-execution-telemetry-summary-cell (list nil))
             (settlements 0)
             result cancel callback-saw-placeholder)
        (unwind-protect
            (progn
              (setq cancel
                    (mevedel-execution-start-one-shot
                     (lambda (value)
                       (cl-incf settlements)
                       (setq result value
                             callback-saw-placeholder
                             (file-exists-p protected)))
                     :name "mevedel-one-shot-contract"
                     :command
                     (list "sh" "-c"
                           (if cancel-p
                               "printf ready; touch ready; exec sleep 30"
                             "printf ready; touch ready; printf done"))
                     :workdir root :writable-roots (list root)))
              (should (functionp cancel))
              (when cancel-p
                (test-mevedel-execution--wait
                 (lambda () (file-exists-p ready)))
                (when (eq mode 'required)
                  (should (file-directory-p protected)))
                (funcall cancel)
                (funcall cancel))
              (test-mevedel-execution--wait (lambda () result))
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
                               (car mevedel-execution-telemetry-summary-cell))))
              (funcall cancel)
              (should (= 1 settlements)))
          (mevedel-execution-teardown-all)
          (delete-directory root t))))))

(mevedel-deftest mevedel-execution-run-one-shot ()
  ,test
  (test)
  :doc "edits refuses an unavailable sandbox while full-auto starts directly"
  (let* ((root (make-temp-file "mevedel-mode-launch-" t))
         (session (mevedel-session--create
                   :name "mode-launch" :working-directory root
                   :sandbox-mode 'best-effort :permission-mode 'edits))
         (mevedel-sandbox--probe-cache
          '((nil . (:available nil :reason "Test backend unavailable")))))
    (unwind-protect
        (progn
          (let ((result
                 (mevedel-execution-run-one-shot
                  :name "mevedel-mode-refusal" :command '("sh" "-c" "touch marker")
                  :workdir root :writable-roots (list root) :session session)))
            (should (plist-get result :error))
            (should-not (file-exists-p (file-name-concat root "marker")))
            (should (= 0 (plist-get (plist-get result :sandbox-summary)
                                   :started-count))))
          (setf (mevedel-session-permission-mode session) 'full-auto)
          (let ((result
                 (mevedel-execution-run-one-shot
                  :name "mevedel-mode-direct" :command '("sh" "-c" "touch marker")
                  :workdir root :writable-roots nil :session session)))
            (should (= 0 (plist-get result :exit-code)))
            (should (file-exists-p (file-name-concat root "marker")))
            (should (eq 'unrestricted
                        (plist-get (plist-get result :sandbox-summary)
                                   :filesystem)))
            (should (eq 'unrestricted
                        (plist-get (plist-get result :sandbox-summary)
                                   :network)))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))
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
      (should
       (equal '(:attempt-count 1 :started-count 1 :refused-count 0
                :sandbox off :filesystem unrestricted
                :network unrestricted :proc nil
                :additional-read-count 0 :additional-write-count 0)
              (plist-get result :sandbox-summary)))
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
  :doc "refuses a failed confined launcher without retrying unrestricted"
  (let* ((root (make-temp-file "mevedel-fallback-proof-" t))
         (replayed (file-name-concat root "replayed"))
         (mevedel-sandbox--probe-cache nil)
         (mevedel-execution--orphan-state nil)
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'display-warning) #'ignore)
                  ((symbol-function 'mevedel-sandbox-prepare)
                   (lambda (&rest _)
                     (list
                      :state 'confined :marker "not-emitted"
                      :command '("sh" "-c" "exit 125")
                      :facts
                      '(:sandbox bubblewrap :filesystem workspace-write
                        :network isolated)))))
          (setq result
                (mevedel-execution-run-one-shot
                 :name "mevedel-test-fallback-proof"
                 :command (list "sh" "-c" "printf replayed > \"$1\"" "fixture" replayed)
                 :workdir root :writable-roots (list root)))
          (should-not (file-exists-p replayed))
          (should (= 125 (plist-get result :exit-code)))
          (should
           (plist-get (plist-get result :sandbox-facts)
                      :refused)))
      (delete-directory root t)))
  :doc "never replays a command after a signal, timeout, or emitted marker"
  (let* ((root (make-temp-file "mevedel-fallback-uncertain-" t))
         (mevedel-execution-process--child-kill-delay 0.05))
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
            (cl-letf (((symbol-function 'mevedel-sandbox-prepare)
                       (lambda (&rest _)
                         (list
                          :state 'confined :marker marker
                          :command launcher
                          :facts
                          '(:sandbox bubblewrap :filesystem workspace-write
                            :network isolated)))))
              (mevedel-execution-run-one-shot
               :name (format "mevedel-test-no-replay-%s" name)
               :command (list "sh" "-c" "printf replayed > \"$1\"" "fixture" replayed)
               :workdir root :writable-roots (list root)
               :timeout timeout))
            (should-not (file-exists-p replayed))))
      (delete-directory root t)))
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
         (mevedel-execution-process--child-kill-delay 0.05)
         result pid)
    (skip-unless (not (eq system-type 'windows-nt)))
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
  :doc "drains local descendants after the main process exits normally"
  (let* ((root (make-temp-file "mevedel-execution-normal-exit-" t))
         (pid-file (file-name-concat root "child.pid"))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution-process--child-kill-delay 0.05)
         result pid)
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (setq result
                (mevedel-execution-run-one-shot
                 :name "mevedel-test-normal-exit"
                 :command
                 (list "sh" "-c"
                       (concat
                        "trap '' HUP; sleep 30 </dev/null >/dev/null 2>&1 & "
                        "child=$!; printf '%s' \"$child\" > \"$1\"; exit 0")
                       "mevedel-test-normal-exit" pid-file)
                 :workdir root
                 :writable-roots (list root)))
          (should (= 0 (plist-get result :exit-code)))
          (should (file-readable-p pid-file))
          (setq pid (test-mevedel-execution--read-pid pid-file))
          (should (test-mevedel-execution--process-gone-p pid)))
      (when (and pid (not (test-mevedel-execution--process-gone-p pid)))
        (ignore-errors (signal-process pid 'KILL)))
      (delete-directory root t)))
  :doc "spools large output without retaining a process buffer"
  (let ((mevedel-sandbox-mode 'off)
        (mevedel-execution-process-output-limit (* 2 1024 1024)))
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
        (mevedel-execution-process-output-limit 4096)
        (mevedel-execution-process--child-kill-delay 0.05))
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
        (mevedel-execution-process-output-limit 4096))
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
  (let ((record (mevedel-execution--record-create :finished-p t)))
    (mevedel-execution--managed-append record "late output\n")
    (should-not (mevedel-execution--record-output-chars record))))

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
    ;; Each bad resolver is reported once; the fallback outcome is what
    ;; these cases assert.
    (mevedel-test--with-captured-diagnostics nil
      (should (eq 'success
                  (mevedel-execution--resolve-outcome resolver 0 'exited)))
      (should (eq 'failure
                  (mevedel-execution--resolve-outcome resolver 1 'exited))))))

(mevedel-deftest mevedel-execution--cancel-observer ()
  ,test
  (test)
  :doc "request abort detaches a waiting poll without consuming its output"
  (let* ((root (make-temp-file "mevedel-managed-poll-abort-" t))
         (session (test-mevedel-execution--session root))
         (request (mevedel-request--create :session session))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution-process--child-kill-delay 0.05)
         initial abandoned final id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c" "printf one; sleep 1; printf two")
                 :yield-time-ms 250))
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
         (mevedel-execution-process--child-kill-delay 0.05)
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
         (mevedel-execution-process--child-kill-delay 0.05)
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
         (mevedel-execution-process--child-kill-delay 0.05)
         initial polled stopped id pid)
    (skip-unless (not (eq system-type 'windows-nt)))
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
          (test-mevedel-execution--wait
           (lambda () (file-readable-p pid-file)))
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
         (mevedel-execution-process--child-kill-delay 0.05)
         initial stopped id)
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c"
                   "trap '' TERM; printf ready; while :; do sleep 1; done")
                 :tty t))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (should (mevedel-execution-stop-user session id))
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

(mevedel-deftest mevedel-execution-stop-all-user ()
  ,test
  (test)
  :doc "stops root, sub-agent, and already-stopping executions session-wide"
  (let* ((root (make-temp-file "mevedel-managed-stop-all-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution-process--child-kill-delay 0.2)
         main main-id)
    (unwind-protect
        (progn
          (setq main
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep 30")
                 :owner "main")
                main-id
                (plist-get (plist-get main :facts) :execution-id))
          (test-mevedel-execution--start-managed
           session root '("sh" "-c" "sleep 30")
           :owner "agent--one")
          (mevedel-execution-stop-user session main-id)
          (should (= 2 (mevedel-execution-stop-all-user session)))
          (test-mevedel-execution--wait
           (lambda () (zerop (mevedel-execution-count-user session))))
          (should (= 0 (mevedel-execution-stop-all-user session))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-start-bash
  (:doc "runs managed commands through confinement and resource capture")
  ,test
  (test)
  :doc "queued full-access work cannot launch after a restrictive mode change"
  (let* ((root (make-temp-file "mevedel-queued-mode-" t))
         (session (test-mevedel-execution--session root))
         first second
         (settlements 0))
    (unwind-protect
        (progn
          (setf (mevedel-session-permission-mode session) 'full-auto)
          (mevedel-execution-start-bash
           (lambda (value) (setq first value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "touch ready; while test ! -e release; do sleep 0.02; done")
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (test-mevedel-execution--wait
           (lambda () (file-exists-p (file-name-concat root "ready"))))
          (mevedel-execution-start-bash
           (lambda (value) (setq second value) (cl-incf settlements))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "touch obsolete-authority")
           :sandbox-permissions 'require-escalated
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (setf (mevedel-session-permission-mode session) 'edits)
          (write-region "release" nil (file-name-concat root "release") nil 'silent)
          (test-mevedel-execution--wait (lambda () (and first second)))
          (should (= 1 settlements))
          (should (= -1 (plist-get (plist-get second :facts) :exit-code)))
          (should-not (file-exists-p (file-name-concat root "obsolete-authority"))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))
  :doc "settles a failed confined launcher once without a replacement process"
  (let* ((root (make-temp-file "mevedel-managed-fallback-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox--probe-cache nil)
         (mevedel-execution--orphan-state nil)
         (callbacks 0)
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'display-warning) #'ignore)
                  ((symbol-function 'mevedel-sandbox-prepare)
                   (lambda (&rest _)
                     (list
                      :state 'confined :marker "not-emitted"
                      :command '("sh" "-c" "exit 125")
                      :facts
                      '(:sandbox bubblewrap :filesystem workspace-write
                        :network isolated)))))
          (mevedel-execution-start-bash
           (lambda (value)
             (setq callbacks (1+ callbacks)
                   result value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "printf direct")
           :tool-args '(:command "printf direct")
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (test-mevedel-execution--wait (lambda () result))
          (accept-process-output nil 0.1)
          (should (= 1 callbacks))
          (should (= 125 (plist-get (plist-get result :facts) :exit-code)))
          (should-not (string-match-p "direct" (plist-get result :output))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))

  :doc "passes raw command text and argv to native resource capture"
  (let* ((root (make-temp-file "mevedel-managed-resource-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         captured result)
    (unwind-protect
        (cl-letf
             (((symbol-function
               'mevedel-execution-telemetry-prepare-resource-capture)
              (lambda (context command-text command)
                (setq captured (list context command-text command))
                nil)))
          (mevedel-execution-start-bash
           (lambda (value) (setq result value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "printf ok")
           :tool-args '(:command "printf ok")
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (test-mevedel-execution--wait (lambda () result))
          (should (car captured))
          (should (equal '("printf ok" ("sh" "-c" "printf ok"))
                         (cdr captured))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))

  :doc "delayed admission uses the data buffer's frozen protected-path policy"
  (let* ((root (make-temp-file "mevedel-managed-policy-" t))
         (session (test-mevedel-execution--session root))
         (data-buffer (generate-new-buffer " *mevedel-policy-source*"))
         (original-policy (default-value 'mevedel-protected-paths))
         (frozen-policy '(("/frozen/**" . inaccessible)))
         (ambient-policy '(("/ambient/**" . read-only)))
         admission captured-policy)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel-protected-paths frozen-policy))
          (cl-letf
              (((symbol-function 'mevedel-execution-scheduler-submit)
                (lambda (_scheduler _mode start &optional _admit-p _reject)
                  (setq admission start)
                  nil))
               ((symbol-function 'mevedel-sandbox-prepare)
                (lambda (&rest _)
                  (setq captured-policy (copy-tree mevedel-protected-paths))
                  '(:state refused :error "test refusal"
                    :facts (:sandbox refused :filesystem unavailable
                            :network unavailable :refused t)))))
            (mevedel-execution-start-bash
             #'ignore
             :session session :data-buffer data-buffer
             :owner "main" :owner-context session
             :command '("ignored") :tool-args '(:command "ignored")
             :workdir root :writable-roots (list root)
             :artifact-directory root :yield-time-ms nil)
            (should admission)
            (setq-default mevedel-protected-paths ambient-policy)
            (with-temp-buffer
              (funcall admission nil)))
          (should (equal frozen-policy captured-policy)))
      (setq-default mevedel-protected-paths original-policy)
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))

  :doc "rejects a queued mutation when an earlier remote outcome becomes unknown"
  (let* ((root (make-temp-file "mevedel-managed-queued-unknown-" t))
         (session (test-mevedel-execution--session root))
         (state (mevedel-execution--state-for-session session))
         admissible reject result)
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-execution-scheduler-submit)
              (lambda (_scheduler _mode _start &optional admit-p rejection)
                (setq admissible admit-p
                      reject rejection)
                nil)))
          (mevedel-execution-start-bash
           (lambda (value) (setq result value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "printf blocked")
           :workdir root :writable-roots (list root)
           :artifact-directory root :yield-time-ms nil)
          (should (funcall admissible))
          (setf (mevedel-execution--state-unknown-outcome state)
                '(:group-id 42 :workdir "/ssh:host:/project/"))
          (should-not (funcall admissible))
          (funcall reject nil)
          (should (eq 'unknown
                      (plist-get (plist-get result :facts) :termination))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))

  :doc "blocks mutating execution until an unknown remote outcome is acknowledged"
  (let* ((root (make-temp-file "mevedel-managed-unknown-" t))
         (session (test-mevedel-execution--session root))
         (state (mevedel-execution--state-for-session session)))
    (unwind-protect
        (progn
          (setf (mevedel-execution--state-unknown-outcome state)
                '(:group-id 42 :workdir "/ssh:host:/project/"))
          (should (mevedel-execution-mutation-blocked-p session))
          (should-error
           (mevedel-execution-start-bash
            #'ignore :session session :owner "main"
            :command '("sh" "-c" "printf blocked")
            :workdir root :writable-roots (list root)
            :artifact-directory root :yield-time-ms nil)
           :type 'mevedel-execution-error)
          (mevedel-execution-acknowledge-unknown session)
          (should-not (mevedel-execution-mutation-blocked-p session)))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))

  :doc "re-proving a dead process group clears the block without a human"
  (let* ((root (make-temp-file "mevedel-managed-reprove-" t))
         (session (test-mevedel-execution--session root))
         (state (mevedel-execution--state-for-session session))
         asked)
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-execution-process-remote-group-status)
                   (lambda (group-id start-time workdir)
                     (setq asked (list group-id start-time workdir))
                     'dead)))
          (setf (mevedel-execution--state-unknown-outcome state)
                '(:group-id 42 :group-start-time "981"
                  :workdir "/ssh:host:/project/"))
          (should (mevedel-execution-mutation-blocked-p session))
          (should (mevedel-execution-reprove-unknown-outcome session))
          (should (equal '(42 "981" "/ssh:host:/project/") asked))
          (should-not (mevedel-execution-mutation-blocked-p session))
          (should-not (mevedel-execution-mutation-refused-p session)))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))

  :doc "a group that is live, ambiguous, or unreachable stays blocked"
  (dolist (answer '(live ambiguous signal))
    (let* ((root (make-temp-file "mevedel-managed-unproved-" t))
           (session (test-mevedel-execution--session root))
           (state (mevedel-execution--state-for-session session)))
      (unwind-protect
          (cl-letf (((symbol-function
                      'mevedel-execution-process-remote-group-status)
                     (lambda (&rest _)
                       (if (eq answer 'signal)
                           (error "Target process status probe timed out")
                         answer))))
            (setf (mevedel-execution--state-unknown-outcome state)
                  '(:group-id 42 :group-start-time "981"
                    :workdir "/ssh:host:/project/"))
            (should-not (mevedel-execution-reprove-unknown-outcome session))
            (should (mevedel-execution-mutation-refused-p session)))
        (mevedel-execution-teardown-session session)
        (delete-directory root t))))

  :doc "an outcome without process identity never reaches the target"
  (let* ((root (make-temp-file "mevedel-managed-anonymous-" t))
         (session (test-mevedel-execution--session root))
         (state (mevedel-execution--state-for-session session)))
    (unwind-protect
        (progn
          (setf (mevedel-execution--state-unknown-outcome state)
                '(:group-id 42 :workdir "/ssh:host:/project/"))
          (should-not (mevedel-execution-reprove-unknown-outcome session))
          (should (mevedel-execution-mutation-refused-p session)))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))

  :doc "a busy transport leaves the block in place without probing the target"
  (let* ((root (make-temp-file "mevedel-managed-busy-reprove-" t))
         (session (test-mevedel-execution--session root))
         (state (mevedel-execution--state-for-session session)))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-transport-busy-p)
                   (lambda (&optional _) t))
                  ((symbol-function
                    'mevedel-execution-process-remote-group-status)
                   (lambda (&rest _)
                     (error "Busy transport must not reach the target"))))
          (setf (mevedel-execution--state-unknown-outcome state)
                '(:group-id 42 :group-start-time "981"
                  :workdir "/ssh:host:/project/"))
          (should-not (mevedel-execution-reprove-unknown-outcome session))
          (should (mevedel-execution-mutation-refused-p session)))
      (mevedel-execution-teardown-session session)
      (delete-directory root t))))

(mevedel-deftest mevedel-execution--artifact-address
  (:doc "publishes logical addresses and hides non-session artifact paths")
  ,test
  (test)
  (let* ((root (make-temp-file "mevedel-execution-address-" t))
         (session (test-mevedel-execution--session root))
         (artifact-directory
          (file-name-concat (mevedel-session-save-path session)
                            "tool-results" "executions"))
         (mevedel-sandbox-mode 'off)
         (temporary-artifact-directory (file-name-concat root "artifacts"))
         (resolutions 0)
         (count (lambda (original &rest args)
                  (cl-incf resolutions)
                  (apply original args)))
         initial final id yielded foreground pending-final)
    (advice-add 'mevedel-resource-artifact-address :around count)
    (unwind-protect
        (progn
          (mevedel-execution-start-bash
           (lambda (value) (setq initial value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "printf first; sleep .2; printf second")
           :tool-args '(:command "printf first; sleep .2; printf second")
           :workdir root :writable-roots (list root)
           :artifact-directory artifact-directory :yield-time-ms 10)
          (test-mevedel-execution--wait (lambda () initial))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (let ((address (plist-get (plist-get initial :facts) :output-path)))
            (should (string-prefix-p "artifact://executions/" address))
            (should-not (file-name-absolute-p address))
            (should-not (string-match-p (regexp-quote root) address)))
          (setq final (test-mevedel-execution--observe session id))
          (should (equal (plist-get (plist-get initial :facts) :output-path)
                         (plist-get (plist-get final :facts) :output-path)))
          ;; Its events share one resolution of the unchanged spool path.
          (should (= 1 resolutions))
          (should (string-match-p "first" (plist-get initial :output)))
          (should (string-match-p "second" (plist-get final :output)))
          (mevedel-execution-start-bash
           (lambda (value) (setq yielded value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "sleep .2; printf yielded")
           :tool-args '(:command "sleep .2; printf yielded")
           :workdir root :writable-roots (list root)
           :artifact-directory temporary-artifact-directory :yield-time-ms 10)
          (test-mevedel-execution--wait (lambda () yielded))
          (should-not (plist-get (plist-get yielded :facts) :output-path))
          (mevedel-execution-start-bash
           (lambda (value) (setq foreground value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "printf foreground")
           :tool-args '(:command "printf foreground")
           :workdir root :writable-roots (list root)
           :artifact-directory temporary-artifact-directory
           :yield-time-ms nil)
          (test-mevedel-execution--wait (lambda () foreground))
          (should-not (plist-get (plist-get foreground :facts) :output-path))
          (mevedel-execution-start-bash
           (lambda (value) (setq pending-final value))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "sleep .2; printf pending")
           :tool-args '(:command "sleep .2; printf pending")
           :workdir root :writable-roots (list root)
           :artifact-directory temporary-artifact-directory
           :yield-time-ms nil)
          (let* ((state (mevedel-session-execution-state session))
                 (pending-id
                  (format "%s-%06d"
                          (mevedel-execution--state-id-prefix state)
                          (mevedel-execution--state-next-id state)))
                 (record (gethash pending-id
                                  (mevedel-execution--state-records state)))
                 (spool (mevedel-execution--spool-path record)))
            (should (string-match-p "\.mevedel-pending-executions"
                                    spool))
            (should-not (mevedel-resource-artifact-address spool session)))
          (test-mevedel-execution--wait (lambda () pending-final)))
      (advice-remove 'mevedel-resource-artifact-address count)
      (mevedel-execution-teardown-session session)
      (delete-directory root t))))

(provide 'test-mevedel-execution)
;;; test-mevedel-execution.el ends here
