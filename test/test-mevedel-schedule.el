;;; test-mevedel-schedule.el --- Wake-up firing and the scheduler -*- lexical-binding: t -*-

;;; Commentary:
;; Due wake-ups fire through the injectable clock into the real root mailbox
;; and wake an idle root through the real background-event path and holds.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-agent-control)
(require 'mevedel-chat)
(require 'mevedel-pending-inputs)
(require 'mevedel-recovery)
(require 'mevedel-schedule)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-durability)
(require 'mevedel-structs)
(require 'mevedel-tools)
(require 'mevedel-transport)
(require 'mevedel-view)
(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-session-test-support"))

(defconst mevedel-schedule-test--now 1795176000
  "2026-11-20 13:00:00 in Europe/Zurich (12:00 UTC).")

(defmacro mevedel-schedule-test--with-session (&rest body)
  "Run BODY in an idle root session with a view, a fixed clock and capture.
`now' holds the clock's seconds.  `submitted' collects (PROMPT ATTRIBUTION)
for each started turn; `mevedel-schedule-test--settle' runs offered work."
  (declare (indent 0) (debug t))
  `(mevedel-view-test--with-buffers
     (let* ((workspace (mevedel-workspace--create
                        :type 'file :id "schedules" :root "/tmp/schedules"
                        :name "schedules"))
            (session (mevedel-session-create "main" workspace))
            (now mevedel-schedule-test--now)
            (mevedel-schedule-clock (lambda () now))
            (zone (getenv "TZ"))
            (run-at-time-original (symbol-function 'run-at-time))
            offers submitted)
       (set-time-zone-rule "Europe/Zurich")
       (with-current-buffer data-buf
         (setq-local mevedel--session session
                     mevedel--workspace workspace
                     mevedel--view-buffer view-buf))
       (with-current-buffer view-buf
         (setq-local mevedel--session session))
       (setf (mevedel-session-root-buffer session) data-buf)
       (unwind-protect
           (cl-letf (((symbol-function 'mevedel-session-artifacts-assert-mutation-authority)
                      #'ignore)
                     ((symbol-function 'mevedel-session-durability-lease-owned-p)
                      (lambda (&rest _) t))
                     ((symbol-function 'mevedel-transport-run-when-idle)
                      (lambda (_key _path thunk &rest _) (funcall thunk) t))
                     ((symbol-function 'mevedel--submit-generated-turn)
                      (lambda (prompt &rest _)
                        (push (list prompt mevedel-view--pending-guest-attribution)
                              submitted)))
                     ;; Capture the idle-work offer; the scheduler's own timer
                     ;; stays real so its arming can be observed.
                     ((symbol-function 'run-at-time)
                      (lambda (time repeat function &rest args)
                        (if (eq function #'mevedel-pending-inputs--offer-idle-work-now)
                            (push (cons function args) offers)
                          (apply run-at-time-original time repeat function args)))))
             (cl-flet ((mevedel-schedule-test--settle ()
                         (while offers
                           (let ((offer (pop offers)))
                             (apply (car offer) (cdr offer))))))
               ,@body))
         (kill-buffer data-buf)
         (should-not mevedel-schedule--timer)
         (set-time-zone-rule zone)))))

(defun mevedel-schedule-test--timers ()
  "Return the pending scheduler timers."
  (cl-remove-if-not (lambda (timer) (eq (timer--function timer) #'mevedel-schedule-run-due))
                    timer-list))

(defun mevedel-schedule-test--wake-ups (session)
  "Return SESSION's unread WAKE_UP records."
  (cl-remove-if-not (lambda (record) (eq 'WAKE_UP (plist-get record :type)))
                    (mevedel-agent-control-context-mailbox session)))

(mevedel-deftest mevedel-schedule-run-due ()
  ,test
  (test)
  :doc "a due wake-up delivers one attributed record and starts one background-event turn"
  (mevedel-schedule-test--with-session
    (mevedel-schedule-create session data-buf "20m" "Check CI; tell the user only if it failed")
    (setq now (+ now 1199))
    (mevedel-schedule-run-due)
    (mevedel-schedule-test--settle)
    (should-not (mevedel-agent-control-context-mailbox session))
    (setq now (+ now 1))
    (mevedel-schedule-run-due)
    (mevedel-schedule-test--settle)
    (let* ((records (mevedel-schedule-test--wake-ups session))
           (block (mevedel-tools--message-delivery-block (car records))))
      (should (= 1 (length records)))
      (should (equal "Check CI; tell the user only if it failed"
                     (plist-get (car records) :payload)))
      (should (string-prefix-p "<agent-message type=\"WAKE_UP\" sender=\"/root\" recipient=\"/root\"" block))
      (should (string-search "scheduled_at=\"2026-11-20T13:20:00+01:00\"" block))
      (should (string-search "fired_at=\"2026-11-20T13:20:00+01:00\"" block))
      (should-not (string-search "overdue_by" block)))
    (should-not (mevedel-session-schedules session))
    (should (equal `((,mevedel-pending-inputs--event-trigger (:type background-event)))
                   submitted))
    (should-not (mevedel-schedule-test--timers)))

  :doc "a late firing carries an overdue annotation"
  (mevedel-schedule-test--with-session
    (mevedel-schedule-create session data-buf "20m" "Check CI")
    (setq now (+ now 1200 1500))
    (mevedel-schedule-run-due)
    (let ((block (mevedel-tools--message-delivery-block
                  (car (mevedel-schedule-test--wake-ups session)))))
      (should (string-search "fired_at=\"2026-11-20T13:45:00+01:00\"" block))
      (should (string-search "overdue_by=\"25m 00s\"" block))))

  :doc "a firing past its expiry is recorded as expired without mail or a turn"
  (mevedel-schedule-test--with-session
    (mevedel-schedule-create session data-buf "20m" "Check CI" "30m")
    (setq now (+ now 2400))
    (mevedel-schedule-run-due)
    (mevedel-schedule-test--settle)
    (should-not (mevedel-agent-control-context-mailbox session))
    (should-not submitted)
    (let ((entry (car (mevedel-session-schedules session))))
      (should (eq 'expired (plist-get entry :status)))
      (should (= now (plist-get entry :expired-at)))))

  :doc "a wake-up waits for a running turn and coalesces with other mail"
  (mevedel-schedule-test--with-session
    (mevedel-agent-control-enqueue-execution-result session "/root" "done" "exec-1" t)
    (with-current-buffer data-buf
      (setq-local mevedel--current-request (mevedel-request--create :session session)))
    (mevedel-schedule-create session data-buf "20m" "Check CI")
    (setq now (+ now 1200))
    (mevedel-schedule-run-due)
    (mevedel-schedule-test--settle)
    (should-not submitted)
    (should (= 2 (length (mevedel-agent-control-context-mailbox session))))
    (with-current-buffer data-buf
      (setq-local mevedel--current-request nil))
    (mevedel-pending-inputs-offer-root-event session data-buf)
    (mevedel-schedule-test--settle)
    (should (= 1 (length submitted))))

  :doc "the composer draft, a pending decision and a queued follow-up hold the wake"
  (dolist (hold
           (list (cons "draft"
                       (lambda (_session view)
                         (with-current-buffer view
                           (mevedel-view-test--insert-composer-draft "> unsent\nsecond"))))
                 (cons "permission"
                       (lambda (session _view)
                         (setf (mevedel-session-permission-queue session)
                               (list (list :id "p")))))
                 (cons "follow-up"
                       (lambda (session _view)
                         (mevedel-session-set-pending-inputs
                          session 'follow-up (list (list :input "first")))))
                 (cons "paused input"
                       (lambda (session _view)
                         (mevedel-session-set-pending-input-paused session t)))))
    (ert-info ((car hold))
      (mevedel-schedule-test--with-session
        (cl-letf (((symbol-function 'mevedel-recovery-save) #'ignore)
                  ((symbol-function 'mevedel-view--run-follow-up-drain) #'ignore))
          (mevedel-schedule-create session data-buf "20m" "Check CI")
          (funcall (cdr hold) session view-buf)
          (setq now (+ now 1200))
          (mevedel-schedule-run-due)
          (mevedel-schedule-test--settle))
        (should-not submitted)
        (should (= 1 (length (mevedel-schedule-test--wake-ups session))))
        (should (mevedel-agent-control-wake-pending-p session)))))

  :doc "the woken turn is a background event and a paused or blocked Goal stays as it was"
  (dolist (status '(paused blocked))
    (mevedel-schedule-test--with-session
      (let ((goal (mevedel-goal--create :id "g" :objective "Ship" :status status)))
        (setf (mevedel-session-goal session) goal)
        (mevedel-schedule-create session data-buf "20m" "Check CI")
        (setq now (+ now 1200))
        (mevedel-schedule-run-due)
        (mevedel-schedule-test--settle)
        (should (equal `((,mevedel-pending-inputs--event-trigger (:type background-event)))
                       submitted))
        (should (eq status (mevedel-goal-status goal))))))

  :doc "a failed mailbox commit keeps the wake-up pending, warns and retries after a minute"
  (mevedel-schedule-test--with-session
    (let (warning)
      (mevedel-schedule-create session data-buf "20m" "Check CI")
      (setq now (+ now 1200))
      (mevedel-test--with-captured-diagnostics warning
        (cl-letf (((symbol-function 'mevedel-agent-control-commit-session)
                   (lambda (&rest _) (error "Disk full"))))
          (mevedel-schedule-run-due)))
      (should (string-search "could not be delivered (Disk full)" warning))
      (should-not (mevedel-agent-control-context-mailbox session))
      (should (= 1 (length (mevedel-schedule-pending session))))
      (should (= 1 (length (mevedel-schedule-test--timers))))
      (setq now (+ now 59))
      (mevedel-schedule-run-due)
      (should-not (mevedel-agent-control-context-mailbox session))
      (setq now (+ now 1))
      (mevedel-schedule-run-due)
      (mevedel-schedule-test--settle)
      (should (= 1 (length (mevedel-schedule-test--wake-ups session))))
      (should-not (mevedel-schedule-pending session))
      (should (= 1 (length submitted))))))

(mevedel-deftest mevedel-schedule--rearm ()
  ,test
  (test)
  :doc "one timer is armed for the earliest wake-up and re-armed on every change"
  (mevedel-schedule-test--with-session
    (cl-flet ((armed-in ()
                (let ((timers (mevedel-schedule-test--timers)))
                  (should (= 1 (length timers)))
                  (round (float-time (time-subtract (timer--time (car timers)) nil))))))
      (let ((later (mevedel-schedule-create session data-buf "1h" "later")))
        (should (< (abs (- 3600 (armed-in))) 5))
        (let ((sooner (mevedel-schedule-create session data-buf "20m" "sooner")))
          (should (< (abs (- 1200 (armed-in))) 5))
          (mevedel-schedule-cancel session data-buf (plist-get sooner :id))
          (should (< (abs (- 3600 (armed-in))) 5)))
        (mevedel-schedule-cancel session data-buf (plist-get later :id))
        (should-not (mevedel-schedule-test--timers)))))

  :doc "a buffer that lost the session lease neither arms nor fires its wake-ups"
  (mevedel-schedule-test--with-session
    (mevedel-schedule-create session data-buf "20m" "Check CI")
    (with-current-buffer data-buf
      (setq-local mevedel-session--read-only-mode t))
    (setq now (+ now 1200))
    (mevedel-schedule-run-due)
    (mevedel-schedule-test--settle)
    (should-not (mevedel-agent-control-context-mailbox session))
    (should (= 1 (length (mevedel-schedule-pending session))))
    (should-not (mevedel-schedule-test--timers))))

(mevedel-deftest mevedel-schedule-restore-session ()
  ,test
  (test)
  :doc "a resumed session re-arms its persisted wake-up and fires into its mailbox"
  (let* ((root (make-temp-file "mevedel-schedule-restore-" t))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *schedule-restore*"))
         (now mevedel-schedule-test--now)
         (mevedel-schedule-clock (lambda () now))
         restored submitted)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--submit-generated-turn)
                   ;; A real turn would send a provider request and consume the mail.
                   (lambda (prompt &rest _) (push prompt submitted))))
          (with-current-buffer buffer
            (org-mode)
            (insert "Check CI later\n")
            (mevedel-session-artifacts-save session buffer)
            (setq-local mevedel--session session))
          (setf (mevedel-session-root-buffer session) buffer)
          (mevedel-schedule-create session buffer "20m" "Check CI")
          (should (mevedel-schedule-test--timers))
          (test-mevedel-session-persistence--release-and-kill buffer session)
          (should-not (mevedel-schedule-test--timers))
          (setq restored (mevedel-session-persistence-restore
                          (mevedel-session-save-path session) nil nil workspace))
          (should (= 1 (length (mevedel-schedule-test--timers))))
          (setq now (+ now 1200))
          (mevedel-schedule-run-due)
          (let* ((restored-session (buffer-local-value 'mevedel--session restored))
                 mail)
            ;; A persisted session commits background mail in its next batch.
            (mevedel-test--await 5 "The wake-up was not committed"
              (setq mail (mevedel-agent-control-context-mailbox restored-session)))
            (should (equal '(WAKE_UP) (mapcar (lambda (record) (plist-get record :type)) mail)))
            (should (equal "Check CI" (plist-get (car mail) :payload)))
            (should-not (mevedel-schedule-pending restored-session))
            ;; The firing and its removal from the store committed together.
            (let ((saved (mevedel-session-codec-read
                          (file-name-concat (mevedel-session-save-path restored-session)
                                            "session.meta.el"))))
              (should-not (plist-get saved :schedules))
              (should (equal '(WAKE_UP) (mapcar (lambda (record) (plist-get record :type))
                                                (plist-get saved :messages)))))
            ;; The restored root is idle and has its view, so it is offered a turn.
            (mevedel-test--await 5 "The restored root was not woken" submitted)
            (should (equal (list mevedel-pending-inputs--event-trigger) submitted))))
      (test-mevedel-session-persistence--release-and-kill buffer session)
      (when (buffer-live-p restored)
        (test-mevedel-session-persistence--release-and-kill
         restored (buffer-local-value 'mevedel--session restored)))
      (should-not (mevedel-schedule-test--timers))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-schedule)
;;; test-mevedel-schedule.el ends here
