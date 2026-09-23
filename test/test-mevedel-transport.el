;;; test-mevedel-transport.el --- Remote transport reentrancy tests -*- lexical-binding: t -*-

;;; Commentary:

;; Covers nesting detection and deferral of target work to an idle transport.

;;; Code:

(require 'mevedel-transport)
(require 'mevedel-utilities)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-transport--detach ()
  ,test
  (test)
  :doc "cancels deferred work on TRAMP unload and reinstalls on reload"
  (let ((mevedel-transport-retry-seconds 5)
        (methods (copy-tree tramp-methods))
        tramp-features
        timer)
    (unwind-protect
        (progn
          (require 'tramp)
          (setq tramp-features
                (cl-remove-if-not
                 (lambda (feature)
                   (string-prefix-p "tramp" (symbol-name feature)))
                 features))
          (mevedel-transport-install)
          (let ((mevedel-transport--depth 1))
            (mevedel-transport-run-when-idle
             'reload-test "/ssh:user@host:/srv/x" #'ignore))
          (setq timer (car (gethash 'reload-test mevedel-transport--pending)))
          (should (timerp timer))
          (unload-feature 'tramp 'force)
          (should-not (gethash 'reload-test mevedel-transport--pending))
          (should-not (memq timer timer-list))
          (require 'tramp)
          (should (advice-member-p #'mevedel-transport--handler-advice
                                   'tramp-file-name-handler))
          (should (memq #'mevedel-transport--detach tramp-unload-hook)))
      (mevedel-transport-cancel-pending)
      (unless (featurep 'tramp)
        (require 'tramp))
      (dolist (feature (reverse tramp-features))
        (require feature nil t))
      (setq tramp-methods methods)
      (mevedel-transport-install))))

(mevedel-deftest mevedel-transport-run-at-time ()
  ,test
  (test)
  :doc "arms an ordinary timer outside a remote command"
  (let ((timer (mevedel-transport-run-at-time 60 #'ignore)))
    (unwind-protect
        (progn
          (should (memq timer timer-list))
          (should-not mevedel-transport--held-timers))
      (cancel-timer timer)))

  :doc "survives TRAMP's suspended timer list and fires after the handler"
  (let (timer lost fired)
    (mevedel-transport--handler-advice
     (lambda ()
       ;; What `with-tramp-suspended-timers' does around a remote command.
       (let (timer-list timer-idle-list)
         (setq lost (run-at-time 60 nil #'ignore))
         (setq timer (mevedel-transport-run-at-time 0 (lambda () (setq fired t))))
         (should (mevedel--timer-pending-p timer)))))
    (should-not (memq lost timer-list))
    (should (memq timer timer-list))
    (should-not mevedel-transport--held-timers)
    (with-timeout (2 (ert-fail "Held timer never fired"))
      (while (not fired) (accept-process-output nil 0.01))))

  :doc "never runs its function nested inside the remote command"
  (let (fired inside)
    (mevedel-transport--handler-advice
     (lambda ()
       (mevedel-transport-run-at-time 0 (lambda () (setq fired t)))
       (accept-process-output nil 0.03)
       (setq inside fired)))
    (should-not inside)
    (with-timeout (2 (ert-fail "Held timer never fired"))
      (while (not fired) (accept-process-output nil 0.01))))

  :doc "releases held timers only when the outermost frame returns"
  (let (timer released-inside)
    (mevedel-transport--handler-advice
     (lambda ()
       (mevedel-transport--handler-advice
        (lambda () (setq timer (mevedel-transport-run-at-time 60 #'ignore))))
       (setq released-inside (memq timer timer-list))))
    (unwind-protect
        (progn
          (should-not released-inside)
          (should (memq timer timer-list)))
      (cancel-timer timer))))

(mevedel-deftest mevedel-transport-call-as-remote-operation ()
  ,test
  (test)
  :doc "counts a remote operation that bypasses TRAMP's handler"
  (let (inside timer)
    (should (eq 'value
                (mevedel-transport-call-as-remote-operation
                 (lambda ()
                   (setq inside (mevedel-transport-nested-p))
                   (let (timer-list timer-idle-list)
                     (setq timer (mevedel-transport-run-at-time 60 #'ignore)))
                   'value))))
    (unwind-protect
        (progn
          (should inside)
          (should-not (mevedel-transport-nested-p))
          ;; Timers armed inside are released once the operation returns.
          (should (memq timer timer-list))
          (should-not mevedel-transport--held-timers))
      (cancel-timer timer))))

(mevedel-deftest mevedel-transport-call-with-spawn-channel ()
  ,test
  (test)
  :doc "runs a local spawn unchanged"
  (should (eq 'local (mevedel-transport-call-with-spawn-channel
                      nil t (lambda () 'local))))

  :doc "selects the remote channel for the spawn only"
  (require 'tramp)
  (let ((remote "/ssh:mevedel-spawn-channel-test:/tmp/"))
    (should (mevedel-transport-call-with-spawn-channel
             remote t
             (lambda () (tramp-direct-async-process-p :file-handler t))))
    (should-not (mevedel-transport-call-with-spawn-channel
                 remote nil
                 (lambda () (tramp-direct-async-process-p :file-handler t))))
    (should-not (tramp-get-connection-property
                 (tramp-dissect-file-name remote) "direct-async" nil))))

(mevedel-deftest mevedel-transport-nested-p ()
  ,test
  (test)
  :doc "counts a handler frame and uncounts it on a normal return"
  (progn
    (should-not (mevedel-transport-nested-p))
    (should (mevedel-transport--handler-advice
             (lambda (&rest _) (mevedel-transport-nested-p))
             'file-exists-p "/ssh:user@host:/srv/x"))
    (should-not (mevedel-transport-nested-p)))

  :doc "uncounts a handler frame that exits non-locally"
  (progn
    (should-not (mevedel-transport-nested-p))
    (should-error
     (mevedel-transport--handler-advice
      (lambda (&rest _) (error "Remote failure"))
      'file-exists-p "/ssh:user@host:/srv/x"))
    (should-not (mevedel-transport-nested-p))
    (should (eq 'thrown
                (catch 'done
                  (mevedel-transport--handler-advice
                   (lambda (&rest _) (throw 'done 'thrown))
                   'file-exists-p "/ssh:user@host:/srv/x"))))
    (should-not (mevedel-transport-nested-p)))

  :doc "counts nested handler frames"
  (mevedel-transport--handler-advice
   (lambda (&rest _)
     (should (= 1 mevedel-transport--depth))
     (mevedel-transport--handler-advice
      (lambda (&rest _) (should (= 2 mevedel-transport--depth)))
      'file-exists-p "/ssh:user@host:/srv/y"))
   'file-exists-p "/ssh:user@host:/srv/x"))

(mevedel-deftest mevedel-transport-busy-p ()
  ,test
  (test)
  :doc "reports an idle transport for local and unconnected targets"
  (progn
    (should-not (mevedel-transport-busy-p))
    (should-not (mevedel-transport-busy-p "/srv/project/.mevedel"))
    (should-not (mevedel-transport-busy-p nil))
    ;; No live connection process means nothing is in flight.
    (should-not
     (mevedel-transport-busy-p "/ssh:user@host:/srv/project/.mevedel")))

  :doc "reports busy from the handler depth, whoever opened the frame"
  ;; The depth is what covers the window TRAMP's own lock leaves open, and it
  ;; does not depend on the operation being one of ours.
  (mevedel-transport--handler-advice
   (lambda (&rest _)
     (should (mevedel-transport-busy-p))
     (should (mevedel-transport-busy-p "/srv/project"))
     (should (mevedel-transport-busy-p "/ssh:user@host:/srv/project")))
   'file-exists-p "/ssh:user@host:/srv/x")

  :doc "reports busy from a held connection lock"
  (let ((process nil)
        (locked nil))
    (require 'tramp)
    (cl-letf (((symbol-function 'tramp-get-connection-process)
               (lambda (_vec) process))
              ((symbol-function 'tramp-get-connection-property)
               (lambda (_key property &optional _default)
                 (and (equal property "locked") locked))))
      (setq process (start-process "mevedel-transport-test" nil "sleep" "30"))
      (unwind-protect
          (progn
            (should-not
             (mevedel-transport-busy-p "/ssh:user@host:/srv/project"))
            (setq locked t)
            (should
             (mevedel-transport-busy-p "/ssh:user@host:/srv/project"))
            ;; A local path has no connection to consult.
            (should-not (mevedel-transport-busy-p "/srv/project")))
        (delete-process process)))))

(mevedel-deftest mevedel-transport-with-exclusive-connection ()
  ,test
  (test)
  :doc "keeps timers from running while a target operation is in flight"
  ;; The predicate stops this package nesting inside others; it cannot stop
  ;; a syntax checker's idle timer from sending its own command on our
  ;; connection and consuming the reply we were waiting for.
  (let ((fired 0) timer)
    (unwind-protect
        (progn
          (setq timer (run-at-time 0.01 nil (lambda () (cl-incf fired))))
          (mevedel-transport-with-exclusive-connection
            (should (null timer-list))
            (should (null timer-idle-list))
            (sleep-for 0.05)
            (should (= 0 fired)))
          (with-timeout (2 (ert-fail "Suspended timer never resumed"))
            (while (= 0 fired)
              (accept-process-output nil 0.01)))
          (should (= 1 fired)))
      (when (timerp timer) (cancel-timer timer))))

  :doc "re-arms a timer the body schedules instead of dropping it"
  ;; The durable path arms the lease renewal timer from inside a transaction,
  ;; and losing it would let the lease expire.
  (let ((fired 0) inner)
    (unwind-protect
        (progn
          (mevedel-transport-with-exclusive-connection
            (setq inner (run-at-time 0.01 nil (lambda () (cl-incf fired))))
            (should (equal (list inner) timer-list)))
          (should (memq inner timer-list))
          (with-timeout (2 (ert-fail "Re-armed timer never fired"))
            (while (= 0 fired)
              (accept-process-output nil 0.01)))
          (should (= 1 fired))
          (should-not (memq inner timer-list)))
      (when (timerp inner) (cancel-timer inner))))

  :doc "preserves an idle timer's eligibility during the current idle period"
  (let (timer)
    (unwind-protect
        (progn
          (mevedel-transport-with-exclusive-connection
            (mevedel-transport-with-exclusive-connection
              (setq timer (run-with-idle-timer 60 nil #'ignore))
              (should-not (timer--triggered timer))))
          (should (memq timer timer-idle-list))
          (should-not (timer--triggered timer)))
      (when timer (cancel-timer timer))))

  :doc "preserves a timer explicitly waiting for the next idle period"
  (let (timer)
    (unwind-protect
        (progn
          (mevedel-transport-with-exclusive-connection
            (setq timer (timer-create))
            (timer-set-function timer #'ignore)
            (timer-set-idle-time timer 60 nil)
            (timer-activate-when-idle timer))
          (should (memq timer timer-idle-list))
          (should (timer--triggered timer)))
      (when timer (cancel-timer timer))))

  :doc "re-arms a body-scheduled timer even when the body exits non-locally"
  (let ((fired 0) inner)
    (unwind-protect
        (progn
          (should-error
           (mevedel-transport-with-exclusive-connection
             (setq inner (run-at-time 0.01 nil (lambda () (cl-incf fired))))
             (error "Target failure")))
          (should (memq inner timer-list))
          (with-timeout (2 (ert-fail "Re-armed timer never fired"))
            (while (= 0 fired)
              (accept-process-output nil 0.01)))
          (should (= 1 fired)))
      (when (timerp inner) (cancel-timer inner))))

  :doc "leaves a `with-timeout' opened inside the body bounded"
  ;; The mutation-authority probe opens one, and a suspension that swallowed
  ;; it would turn a wedged target into an unbounded block.
  (should (eq 'timed-out
              (mevedel-transport-with-exclusive-connection
                (with-timeout (0.05 'timed-out)
                  (sleep-for 2)
                  'finished))))

  :doc "charges an enclosing `with-timeout' for the time the body took"
  ;; `with-timeout-suspend' stops the clock, which is what a debugger wants
  ;; and the opposite of what a caller wants here: a bound on remote work
  ;; would move by however long each section held the connection, so a
  ;; deadline that passed inside one would never arrive.
  (with-timeout (10 (ert-fail "Enclosing timeout fired during the test"))
    (let ((timer (car with-timeout-timers)))
      (mevedel-transport-with-exclusive-connection
        (sleep-for 0.3))
      (should (< (float-time (time-subtract (timer--time timer) nil))
                 9.8))))

  :doc "restores the timer list when the body exits non-locally"
  (let ((before timer-list))
    (should-error
     (mevedel-transport-with-exclusive-connection
       (error "Target failure")))
    (should (eq before timer-list))
    (should (eq 'thrown
                (catch 'done
                  (mevedel-transport-with-exclusive-connection
                    (throw 'done 'thrown)))))
    (should (eq before timer-list))))

(mevedel-deftest mevedel-transport-run-when-idle ()
  ,test
  (test)
  :doc "runs immediately when nothing is in flight"
  (let ((runs 0))
    (unwind-protect
        (progn
          (should
           (mevedel-transport-run-when-idle
            'test-key "/srv/project" (lambda () (cl-incf runs))))
          (should (= 1 runs)))
      (mevedel-transport-cancel-pending)))

  :doc "rejects work while transport integration is disabled"
  (let ((mevedel-transport--enabled-p nil)
        (runs 0))
    (should-not
     (mevedel-transport-run-when-idle
      'disabled "/srv/project" (lambda () (cl-incf runs))))
    (should (= 0 runs)))

  :doc "defers while busy and runs once the transport frees"
  (let ((runs 0)
        (mevedel-transport-retry-seconds 0.01))
    (unwind-protect
        (progn
          (mevedel-transport--handler-advice
           (lambda (&rest _)
             (mevedel-transport-run-when-idle
              'test-key "/srv/project" (lambda () (cl-incf runs)))
             ;; Still inside the frame: the work must not have run.
             (should (= 0 runs)))
           'file-exists-p "/ssh:user@host:/srv/x")
          (should (= 0 runs))
          (with-timeout (2 (ert-fail "Deferred work never ran"))
            (while (= 0 runs)
              (accept-process-output nil 0.01)))
          (should (= 1 runs)))
      (mevedel-transport-cancel-pending)))

  :doc "re-arms a retry timer dropped by TRAMP's suspended timer binding"
  (let ((runs 0)
        (mevedel-transport-retry-seconds 0.01)
        timer)
    (unwind-protect
        (progn
          (mevedel-transport--handler-advice
           (lambda (&rest _)
             (let (timer-list)
               (mevedel-transport-run-when-idle
                'dropped-timer "/srv/project"
                (lambda () (cl-incf runs)))
               (setq timer
                     (car (gethash 'dropped-timer mevedel-transport--pending)))
               (should (timerp timer))))
           'file-exists-p "/ssh:user@host:/srv/x")
          (should (memq timer timer-list))
          (with-timeout (2 (ert-fail "Re-armed work never ran"))
            (while (= 0 runs)
              (accept-process-output nil 0.01)))
          (should (= 1 runs)))
      (mevedel-transport-cancel-pending)))

  :doc "coalesces repeated scheduling of one key into a single retry"
  (let ((runs 0)
        (mevedel-transport-retry-seconds 0.01))
    (unwind-protect
        (progn
          (mevedel-transport--handler-advice
           (lambda (&rest _)
             (dotimes (_ 5)
               (mevedel-transport-run-when-idle
                'test-key "/srv/project" (lambda () (cl-incf runs))))
             (should (= 1 (hash-table-count
                           mevedel-transport--pending))))
           'file-exists-p "/ssh:user@host:/srv/x")
          (with-timeout (2 (ert-fail "Deferred work never ran"))
            (while (= 0 runs)
              (accept-process-output nil 0.01)))
          ;; One retry timer means the coalesced work runs once, not five
          ;; times.
          (should (= 1 runs))
          (should (= 0 (hash-table-count mevedel-transport--pending))))
      (mevedel-transport-cancel-pending)))

  :doc "cancels pending work by key and in bulk"
  (let ((runs 0)
        (mevedel-transport-retry-seconds 5))
    (unwind-protect
        (progn
          (mevedel-transport--handler-advice
           (lambda (&rest _)
             (mevedel-transport-run-when-idle
              'first "/srv/project" (lambda () (cl-incf runs)))
             (mevedel-transport-run-when-idle
              'second "/srv/project" (lambda () (cl-incf runs))))
           'file-exists-p "/ssh:user@host:/srv/x")
          (should (= 2 (hash-table-count mevedel-transport--pending)))
          (mevedel-transport-cancel-pending 'first)
          (should (= 1 (hash-table-count mevedel-transport--pending)))
          (mevedel-transport-cancel-pending)
          (should (= 0 (hash-table-count mevedel-transport--pending)))
          (should (= 0 runs)))
      (mevedel-transport-cancel-pending)))

  :doc "inline replacement fences the older same-key retry"
  (let ((mevedel-transport-retry-seconds 60)
        old-runs new-runs old-cancels old)
    (unwind-protect
        (progn
          (let ((mevedel-transport--depth 1))
            (mevedel-transport-run-when-idle
             'replace "/srv/project" (lambda () (setq old-runs t))
             (lambda () (setq old-cancels t)))
            (setq old (car (gethash 'replace mevedel-transport--pending))))
          (mevedel-transport-run-when-idle
           'replace "/srv/project" (lambda () (setq new-runs t)))
          (should new-runs)
          (should-not old-cancels)
          (apply (timer--function old) (timer--args old))
          (should-not old-runs)
          (should-not (gethash 'replace mevedel-transport--pending)))
      (when (timerp old) (cancel-timer old))
      (mevedel-transport-cancel-pending)))

  :doc "cancelled retry cannot consume a replacement entry or its canceller"
  (let ((mevedel-transport-retry-seconds 60)
        (old-runs 0) (new-runs 0) (old-cancels 0) (new-cancels 0)
        old replacement)
    (unwind-protect
        (progn
          (let ((mevedel-transport--depth 1))
            (mevedel-transport-run-when-idle
             'replace "/srv/project" (lambda () (cl-incf old-runs))
             (lambda () (cl-incf old-cancels)))
            (setq old (car (gethash 'replace mevedel-transport--pending)))
            (mevedel-transport-cancel-pending 'replace)
            (mevedel-transport-run-when-idle
             'replace "/srv/project" (lambda () (cl-incf new-runs))
             (lambda () (cl-incf new-cancels)))
            (setq replacement (car (gethash 'replace mevedel-transport--pending))))
          (apply (timer--function old) (timer--args old))
          (should (= 0 old-runs))
          (should (eq replacement (car (gethash 'replace mevedel-transport--pending))))
          (mevedel-transport-cancel-pending 'replace)
          (apply (timer--function replacement) (timer--args replacement))
          (should (= 0 new-runs))
          (should (= 1 old-cancels))
          (should (= 1 new-cancels)))
      (dolist (timer (list old replacement))
        (when (timerp timer) (cancel-timer timer)))
      (mevedel-transport-cancel-pending)))

  :doc "cancellation fences a timer restored after exclusive transport use"
  (let ((mevedel-transport-retry-seconds 60)
        (runs 0) (cancels 0) timer)
    (unwind-protect
        (progn
          (let ((mevedel-transport--depth 1))
            (mevedel-transport-run-when-idle
             'suspended "/srv/project" (lambda () (cl-incf runs))
             (lambda () (cl-incf cancels)))
            (setq timer (car (gethash 'suspended mevedel-transport--pending))))
          (mevedel-transport-with-exclusive-connection
            (mevedel-transport-cancel-pending 'suspended))
          (should (memq timer timer-list))
          (apply (timer--function timer) (timer--args timer))
          (should (= 0 runs))
          (should (= 1 cancels))
          (should-not (gethash 'suspended mevedel-transport--pending)))
      (when (timerp timer) (cancel-timer timer))
      (mevedel-transport-cancel-pending)))

  :doc "busy retry transfers ownership without losing its cancellation callback"
  (let ((mevedel-transport-retry-seconds 60)
        (runs 0) (cancels 0) old replacement)
    (unwind-protect
        (progn
          (let ((mevedel-transport--depth 1))
            (mevedel-transport-run-when-idle
             'retry "/srv/project" (lambda () (cl-incf runs))
             (lambda () (cl-incf cancels)))
            (setq old (car (gethash 'retry mevedel-transport--pending)))
            (cancel-timer old)
            (apply (timer--function old) (timer--args old))
            (setq replacement (car (gethash 'retry mevedel-transport--pending)))
            (should (timerp replacement))
            (should-not (eq old replacement))
            (apply (timer--function old) (timer--args old))
            (should (eq replacement (car (gethash 'retry mevedel-transport--pending)))))
          (mevedel-transport-cancel-pending 'retry)
          (apply (timer--function replacement) (timer--args replacement))
          (should (= 0 runs))
          (should (= 1 cancels)))
      (dolist (timer (list old replacement))
        (when (timerp timer) (cancel-timer timer)))
      (mevedel-transport-cancel-pending)))

  :doc "notifies queued work once when cancellation prevents its thunk"
  (let ((runs 0)
        (cancels 0)
        (mevedel-transport-retry-seconds 5))
    (unwind-protect
        (progn
          (mevedel-transport--handler-advice
           (lambda (&rest _)
             (mevedel-transport-run-when-idle
              'cancel-callback "/srv/project"
              (lambda () (cl-incf runs))
              (lambda () (cl-incf cancels))))
           'file-exists-p "/ssh:user@host:/srv/x")
          (mevedel-transport-cancel-pending 'cancel-callback)
          (mevedel-transport-cancel-pending 'cancel-callback)
          (should (= 0 runs))
          (should (= 1 cancels)))
      (mevedel-transport-cancel-pending))))

(mevedel-deftest mevedel-transport-cancel-pending ()
  ,test
  (test)
  :doc "coalescing retains the first work item's cancellation policy"
  (dolist (first-canceller '(nil t))
    (let ((mevedel-transport-retry-seconds 60)
          (first-cancels 0) (later-cancels 0))
      (unwind-protect
          (progn
            (let ((mevedel-transport--depth 1))
              (mevedel-transport-run-when-idle
               'coalesced "/srv/project" #'ignore
               (when first-canceller (lambda () (cl-incf first-cancels))))
              (mevedel-transport-run-when-idle
               'coalesced "/srv/project" #'ignore
               (lambda () (cl-incf later-cancels))))
            (mevedel-transport-cancel-pending 'coalesced)
            (should (= first-cancels (if first-canceller 1 0)))
            (should (= later-cancels 0)))
        (mevedel-transport-cancel-pending))))

  :doc "bulk cancellation retires all work before callbacks and tolerates errors"
  (let ((mevedel-transport-retry-seconds 60)
        (cancels 0))
    (unwind-protect
        (progn
          (let ((mevedel-transport--depth 1))
            (dolist (key '(first second third))
              (mevedel-transport-run-when-idle
               key "/srv/project" #'ignore
               (lambda ()
                 (should (= 0 (hash-table-count mevedel-transport--pending)))
                 (cl-incf cancels)
                 (error "Cancellation failed")))))
          (mevedel-transport-cancel-pending)
          (should (= 3 cancels))
          (should (= 0 (hash-table-count mevedel-transport--pending))))
      (mevedel-transport-cancel-pending)))

  :doc "bulk cancellation does not erase work scheduled by a canceller"
  (let ((mevedel-transport-retry-seconds 60)
        (old-cancels 0) (new-cancels 0) old replacement)
    (unwind-protect
        (let ((mevedel-transport--depth 1))
          (mevedel-transport-run-when-idle
           'replace "/srv/project" #'ignore
           (lambda ()
             (cl-incf old-cancels)
             (mevedel-transport-run-when-idle
              'replace "/srv/project" #'ignore
              (lambda () (cl-incf new-cancels)))
             (setq replacement (car (gethash 'replace mevedel-transport--pending)))))
          (setq old (car (gethash 'replace mevedel-transport--pending)))
          (mevedel-transport-cancel-pending)
          (should (= 1 old-cancels))
          (should (eq replacement (car (gethash 'replace mevedel-transport--pending))))
          (apply (timer--function old) (timer--args old))
          (should (eq replacement (car (gethash 'replace mevedel-transport--pending))))
          (mevedel-transport-cancel-pending 'replace)
          (should (= 1 new-cancels))
          (should-not (gethash 'replace mevedel-transport--pending)))
      (dolist (timer (list old replacement))
        (when (timerp timer) (cancel-timer timer)))
      (mevedel-transport-cancel-pending)))

  :doc "teardown fences an already delivered retry after reinstall"
  (let ((mevedel-transport-retry-seconds 60)
        (runs 0) (cancels 0) timer)
    (unwind-protect
        (progn
          (let ((mevedel-transport--depth 1))
            (mevedel-transport-run-when-idle
             'teardown "/srv/project" (lambda () (cl-incf runs))
             (lambda () (cl-incf cancels)))
            (setq timer (car (gethash 'teardown mevedel-transport--pending))))
          (mevedel-transport-uninstall)
          (mevedel-transport-install)
          (apply (timer--function timer) (timer--args timer))
          (should (= 0 runs))
          (should (= 1 cancels)))
      (when (timerp timer) (cancel-timer timer))
      (mevedel-transport-install)
      (mevedel-transport-cancel-pending))))

(mevedel-deftest mevedel-transport-schedule-idle ()
  ,test
  (test)
  :doc "coalesces repeated scheduling into one deferred run"
  (let ((table (make-hash-table :test #'equal)) (runs 0))
    (unwind-protect
        (progn
          (should (timerp (mevedel-transport-schedule-idle
                           table "k" 'test-idle "/srv/project" (lambda () (cl-incf runs)))))
          (should-not (mevedel-transport-schedule-idle
                       table "k" 'test-idle "/srv/project" (lambda () (cl-incf runs))))
          (should (= 1 (hash-table-count table)))
          (with-timeout (2 (ert-fail "Deferred work never ran"))
            (while (= 0 runs) (accept-process-output nil 0.01)))
          (should (= 1 runs))
          (should (= 0 (hash-table-count table))))
      (mevedel-transport-cancel-idle table 'test-idle)))

  :doc "cancellation fences a queued timer so its thunk never runs"
  (let ((table (make-hash-table :test #'equal)) (runs 0))
    (let* ((timer (mevedel-transport-schedule-idle
                   table "k" 'test-idle "/srv/project" (lambda () (cl-incf runs))))
           (function (timer--function timer)) (args (timer--args timer)))
      (mevedel-transport-cancel-idle table 'test-idle)
      (should (= 0 (hash-table-count table)))
      (apply function args)
      (should (= 0 runs))))

  :doc "a disabled transport drops the entry without running"
  (let ((table (make-hash-table :test #'equal)) (runs 0))
    (unwind-protect
        (let* ((timer (mevedel-transport-schedule-idle
                       table "k" 'test-idle "/srv/project" (lambda () (cl-incf runs))))
               (mevedel-transport--enabled-p nil))
          (apply (timer--function timer) (timer--args timer))
          (should (= 0 runs))
          (should (= 0 (hash-table-count table))))
      (mevedel-transport-cancel-idle table 'test-idle))))

(mevedel-deftest mevedel-transport-schedule-idle/responsiveness ()
  ,test
  (test)
  :doc "separates already-due background jobs with an event-loop opportunity"
  (let ((table (make-hash-table :test #'equal))
        (mevedel-transport--background-resume-at 0)
        input input-timer second saw-input)
    (unwind-protect
        (let ((first (mevedel-transport-schedule-idle
                      table "first" 'test-background "/tmp"
                      (lambda ()
                        (setq input-timer (run-at-time 0 nil (lambda () (setq input t)))))))
              (next (mevedel-transport-schedule-idle
                     table "next" 'test-background "/tmp"
                     (lambda () (setq second t saw-input input)))))
          ;; Both callbacks are already due in the same timer batch.
          (cancel-timer first)
          (cancel-timer next)
          (apply (timer--function first) (timer--args first))
          (apply (timer--function next) (timer--args next))
          (let ((deadline (+ (float-time) 2)))
            (while (and (not second) (< (float-time) deadline)) (sleep-for .002)))
          (should second)
          (should saw-input)
          (should (= 0 (hash-table-count table))))
      (when input-timer (cancel-timer input-timer))
      (mevedel-transport-cancel-idle table 'test-background))))

(mevedel-deftest mevedel-transport-schedule-idle/shared-target ()
  (let ((table (make-hash-table :test #'eq))
        (mevedel-transport--pending (make-hash-table :test #'equal))
        (mevedel-transport--background-resume-at 0)
        (runs nil))
    (unwind-protect
        (progn
          (dolist (key '(first second))
            (let ((timer (mevedel-transport-schedule-idle
                          table key 'test-shared "/tmp"
                          (lambda () (push key runs)))))
              (cancel-timer timer)
              (let ((mevedel-transport--depth 1))
                (apply (timer--function timer) (timer--args timer)))))
          (should (= 2 (hash-table-count mevedel-transport--pending)))
          (with-timeout (2 (ert-fail "Shared-target work was lost"))
            (while (< (length runs) 2) (sleep-for .01)))
          (should (memq 'first runs))
          (should (memq 'second runs))
          (should (zerop (hash-table-count table))))
      (mevedel-transport-cancel-idle table 'test-shared)
      (mevedel-transport-cancel-pending))))

(mevedel-deftest mevedel-transport-schedule-idle/pending-input ()
  (let ((table (make-hash-table :test #'eq))
        (mevedel-transport--background-resume-at 0)
        (unread-command-events (list ?x))
        ran)
    (unwind-protect
        (let ((timer (mevedel-transport-schedule-idle
                      table 'input 'test-input "/tmp" (lambda () (setq ran t)))))
          (cancel-timer timer)
          (should (input-pending-p))
          (apply (timer--function timer) (timer--args timer))
          (should-not ran)
          (should (eq timer (gethash 'input table)))
          ;; Consume an actual Emacs event before background work resumes.
          (should (eq ?x (read-event nil nil .1)))
          (with-timeout (2 (ert-fail "Background work did not resume"))
            (while (not ran) (sleep-for .01)))
          (should (zerop (hash-table-count table))))
      (mevedel-transport-cancel-idle table 'test-input))))

(provide 'test-mevedel-transport)

;;; test-mevedel-transport.el ends here
