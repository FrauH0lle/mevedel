;;; test-mevedel-transport.el --- Remote transport reentrancy tests -*- lexical-binding: t -*-

;;; Commentary:

;; Covers nesting detection and deferral of target work to an idle transport.

;;; Code:

(require 'mevedel-transport)
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
          (setq timer (gethash 'reload-test mevedel-transport--pending))
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
      (mevedel-transport-install))))

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
                     (gethash 'dropped-timer mevedel-transport--pending))
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

(provide 'test-mevedel-transport)

;;; test-mevedel-transport.el ends here
