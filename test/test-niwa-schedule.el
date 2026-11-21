;;; test-niwa-schedule.el --- Wake-up firing and the scheduler -*- lexical-binding: t -*-

;;; Commentary:
;; Due wake-ups fire through the injectable clock into the real root mailbox
;; and wake an idle root through the real background-event path and holds.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-agent-control)
(require 'niwa-background-list)
(require 'niwa-chat)
(require 'niwa-pending-inputs)
(require 'niwa-recovery)
(require 'niwa-schedule)
(require 'niwa-session-artifacts)
(require 'niwa-session-durability)
(require 'niwa-structs)
(require 'niwa-tools)
(require 'niwa-transport)
(require 'niwa-view)
(require 'niwa-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-session-test-support"))

(defconst niwa-schedule-test--now 1795176000
  "2026-11-20 13:00:00 in Europe/Zurich (12:00 UTC).")

(defmacro niwa-schedule-test--with-session (&rest body)
  "Run BODY in an idle root session with a view, a fixed clock and capture.
`now' holds the clock's seconds.  `submitted' collects (PROMPT ATTRIBUTION)
for each started turn; `niwa-schedule-test--settle' runs offered work."
  (declare (indent 0) (debug t))
  `(niwa-view-test--with-buffers
     (let* ((workspace (niwa-workspace--create
                        :type 'file :id "schedules" :root "/tmp/schedules"
                        :name "schedules"))
            (session (niwa-session-create "main" workspace))
            (now niwa-schedule-test--now)
            (niwa-schedule-clock (lambda () now))
            (zone (getenv "TZ"))
            (run-at-time-original (symbol-function 'run-at-time))
            offers submitted)
       (set-time-zone-rule "Europe/Zurich")
       (with-current-buffer data-buf
         (setq-local niwa--session session
                     niwa--workspace workspace
                     niwa--view-buffer view-buf))
       (with-current-buffer view-buf
         (setq-local niwa--session session))
       (setf (niwa-session-root-buffer session) data-buf)
       (unwind-protect
           (cl-letf (((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
                      #'ignore)
                     ((symbol-function 'niwa-session-durability-lease-owned-p)
                      (lambda (&rest _) t))
                     ((symbol-function 'niwa-transport-run-when-idle)
                      (lambda (_key _path thunk &rest _) (funcall thunk) t))
                     ((symbol-function 'niwa--submit-generated-turn)
                      (lambda (prompt &rest _)
                        (push (list prompt niwa-view--pending-guest-attribution)
                              submitted)))
                     ;; Capture the idle-work offer; the scheduler's own timer
                     ;; stays real so its arming can be observed.
                     ((symbol-function 'run-at-time)
                      (lambda (time repeat function &rest args)
                        (if (eq function #'niwa-pending-inputs--offer-idle-work-now)
                            (push (cons function args) offers)
                          (apply run-at-time-original time repeat function args)))))
             (cl-flet ((niwa-schedule-test--settle ()
                         (while offers
                           (let ((offer (pop offers)))
                             (apply (car offer) (cdr offer))))))
               ,@body))
         (kill-buffer data-buf)
         (should-not niwa-schedule--timer)
         (set-time-zone-rule zone)))))

(defun niwa-schedule-test--timers ()
  "Return the pending scheduler timers."
  (cl-remove-if-not (lambda (timer) (eq (timer--function timer) #'niwa-schedule-run-due))
                    timer-list))

(defun niwa-schedule-test--wake-ups (session)
  "Return SESSION's unread WAKE_UP records."
  (cl-remove-if-not (lambda (record) (eq 'WAKE_UP (plist-get record :type)))
                    (niwa-agent-control-context-mailbox session)))

(niwa-deftest niwa-schedule-run-due ()
  ,test
  (test)
  :doc "a due wake-up delivers one attributed record and starts one background-event turn"
  (niwa-schedule-test--with-session
    (niwa-schedule-create session data-buf "20m" "Check CI; tell the user only if it failed")
    (setq now (+ now 1199))
    (niwa-schedule-run-due)
    (niwa-schedule-test--settle)
    (should-not (niwa-agent-control-context-mailbox session))
    (setq now (+ now 1))
    (niwa-schedule-run-due)
    (niwa-schedule-test--settle)
    (let* ((records (niwa-schedule-test--wake-ups session))
           (block (niwa-tools--message-delivery-block (car records))))
      (should (= 1 (length records)))
      (should (equal "Check CI; tell the user only if it failed"
                     (plist-get (car records) :payload)))
      (should (string-prefix-p "<agent-message type=\"WAKE_UP\" sender=\"/root\" recipient=\"/root\"" block))
      (should (string-search "scheduled_at=\"2026-11-20T13:20:00+01:00\"" block))
      (should (string-search "fired_at=\"2026-11-20T13:20:00+01:00\"" block))
      (should-not (string-search "overdue_by" block)))
    (should-not (niwa-session-schedules session))
    (should (equal `((,niwa-pending-inputs--event-trigger (:type background-event)))
                   submitted))
    (should-not (niwa-schedule-test--timers)))

  :doc "a late firing carries an overdue annotation"
  (niwa-schedule-test--with-session
    (niwa-schedule-create session data-buf "20m" "Check CI")
    (setq now (+ now 1200 1500))
    (niwa-schedule-run-due)
    (let ((block (niwa-tools--message-delivery-block
                  (car (niwa-schedule-test--wake-ups session)))))
      (should (string-search "fired_at=\"2026-11-20T13:45:00+01:00\"" block))
      (should (string-search "overdue_by=\"25m 00s\"" block))))

  :doc "a recurring wake-up re-arms from its run; missed runs fire once, annotated"
  (niwa-schedule-test--with-session
    (niwa-schedule-create session data-buf "every 20m" "Babysit PR 412")
    (setq now (+ now 1200))
    (niwa-schedule-run-due)
    (should (= 1 (length (niwa-schedule-test--wake-ups session))))
    (should (= (+ now 1200) (plist-get (car (niwa-schedule-pending session)) :due)))
    (should (= 1 (length (niwa-schedule-test--timers))))
    ;; Asleep for two hours: five runs were missed.
    (setq now (+ now 7200))
    (niwa-schedule-run-due)
    (let ((records (niwa-schedule-test--wake-ups session)))
      (should (= 2 (length records)))
      (should (string-search "overdue_by=\"1h 40m\""
                             (niwa-tools--message-delivery-block (car (last records))))))
    (let ((details (niwa-background-list--details
                    (cons :kind (cons 'wake-up (car (niwa-schedule-pending session)))) nil)))
      (should (string-search "Skipped" (plist-get (car (plist-get details :sections)) :body))))
    (should (= (+ now 1200) (plist-get (car (niwa-schedule-pending session)) :due))))

  :doc "a cron wake-up re-arms for its next calendar time"
  (niwa-schedule-test--with-session
    (niwa-schedule-create session data-buf "0 9 * * 1-5" "Morning CI summary")
    ;; Fri 13:00 -> Mon 2026-11-23 09:00 CET, then Tue 09:00.
    (should (= 1795420800 (plist-get (car (niwa-schedule-pending session)) :due)))
    (setq now 1795420800)
    (niwa-schedule-run-due)
    (should (= 1 (length (niwa-schedule-test--wake-ups session))))
    (should (= 1795507200 (plist-get (car (niwa-schedule-pending session)) :due))))

  :doc "a recurring wake-up whose next run falls after its expiry expires at this firing"
  (niwa-schedule-test--with-session
    (niwa-schedule-create session data-buf "every 1h" "Babysit PR 412" "90m")
    (setq now (+ now 3600))
    (niwa-schedule-run-due)
    (should (= 1 (length (niwa-schedule-test--wake-ups session))))
    (should-not (niwa-schedule-pending session))
    (let ((entry (car (niwa-session-schedules session))))
      (should (eq 'expired (plist-get entry :status)))
      (should (= now (plist-get entry :expired-at))))
    (should-not (niwa-schedule-test--timers)))

  :doc "a firing past its expiry is recorded as expired without mail or a turn"
  (niwa-schedule-test--with-session
    (niwa-schedule-create session data-buf "20m" "Check CI" "30m")
    (setq now (+ now 2400))
    (niwa-schedule-run-due)
    (niwa-schedule-test--settle)
    (should-not (niwa-agent-control-context-mailbox session))
    (should-not submitted)
    (let ((entry (car (niwa-session-schedules session))))
      (should (eq 'expired (plist-get entry :status)))
      (should (= now (plist-get entry :expired-at)))))

  :doc "a wake-up waits for a running turn and coalesces with other mail"
  (niwa-schedule-test--with-session
    (niwa-agent-control-enqueue-execution-result session "/root" "done" "exec-1" t)
    (with-current-buffer data-buf
      (setq-local niwa--current-request (niwa-request--create :session session)))
    (niwa-schedule-create session data-buf "20m" "Check CI")
    (setq now (+ now 1200))
    (niwa-schedule-run-due)
    (niwa-schedule-test--settle)
    (should-not submitted)
    (should (= 2 (length (niwa-agent-control-context-mailbox session))))
    (with-current-buffer data-buf
      (setq-local niwa--current-request nil))
    (niwa-pending-inputs-offer-root-event session data-buf)
    (niwa-schedule-test--settle)
    (should (= 1 (length submitted))))

  :doc "the composer draft, a pending decision and a queued follow-up hold the wake"
  (dolist (hold
           (list (cons "draft"
                       (lambda (_session view)
                         (with-current-buffer view
                           (niwa-view-test--insert-composer-draft "> unsent\nsecond"))))
                 (cons "permission"
                       (lambda (session _view)
                         (setf (niwa-session-permission-queue session)
                               (list (list :id "p")))))
                 (cons "follow-up"
                       (lambda (session _view)
                         (niwa-session-set-pending-inputs
                          session 'follow-up (list (list :input "first")))))
                 (cons "paused input"
                       (lambda (session _view)
                         (niwa-session-set-pending-input-paused session t)))))
    (ert-info ((car hold))
      (niwa-schedule-test--with-session
        (cl-letf (((symbol-function 'niwa-recovery-save) #'ignore)
                  ((symbol-function 'niwa-view--run-follow-up-drain) #'ignore))
          (niwa-schedule-create session data-buf "20m" "Check CI")
          (funcall (cdr hold) session view-buf)
          (setq now (+ now 1200))
          (niwa-schedule-run-due)
          (niwa-schedule-test--settle))
        (should-not submitted)
        (should (= 1 (length (niwa-schedule-test--wake-ups session))))
        (should (niwa-agent-control-wake-pending-p session)))))

  :doc "the woken turn is a background event and a paused or blocked Goal stays as it was"
  (dolist (status '(paused blocked))
    (niwa-schedule-test--with-session
      (let ((goal (niwa-goal--create :id "g" :objective "Ship" :status status)))
        (setf (niwa-session-goal session) goal)
        (niwa-schedule-create session data-buf "20m" "Check CI")
        (setq now (+ now 1200))
        (niwa-schedule-run-due)
        (niwa-schedule-test--settle)
        (should (equal `((,niwa-pending-inputs--event-trigger (:type background-event)))
                       submitted))
        (should (eq status (niwa-goal-status goal))))))

  :doc "a failed mailbox commit keeps the wake-up pending, warns and retries after a minute"
  (niwa-schedule-test--with-session
    (let (warning)
      (niwa-schedule-create session data-buf "20m" "Check CI")
      (setq now (+ now 1200))
      (niwa-test--with-captured-diagnostics warning
        (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
                   (lambda (&rest _) (error "Disk full"))))
          (niwa-schedule-run-due)))
      (should (string-search "could not be delivered (Disk full)" warning))
      (should-not (niwa-agent-control-context-mailbox session))
      (should (= 1 (length (niwa-schedule-pending session))))
      (should (= 1 (length (niwa-schedule-test--timers))))
      (setq now (+ now 59))
      (niwa-schedule-run-due)
      (should-not (niwa-agent-control-context-mailbox session))
      (setq now (+ now 1))
      (niwa-schedule-run-due)
      (niwa-schedule-test--settle)
      (should (= 1 (length (niwa-schedule-test--wake-ups session))))
      (should-not (niwa-schedule-pending session))
      (should (= 1 (length submitted))))))

(defmacro niwa-schedule-test--with-woken-policies (&rest body)
  "Run BODY with models `sch-main' (the session's) and `sch-cheap' registered.
`policies' collects (MODEL-NAME EFFORT) as each woken turn would resolve them,
after which the stub drains the pending request context like admission does."
  (declare (indent 0) (debug t))
  `(let ((gptel--known-backends nil)
         policies)
     (gptel-make-openai "sch" :key "k" :models '(sch-main sch-cheap))
     (put 'sch-cheap :reasoning-effort '(member low high))
     (with-current-buffer data-buf
       (setq-local gptel-backend (gptel-get-backend "sch")
                   gptel-model 'sch-main))
     (cl-letf (((symbol-function 'niwa--submit-generated-turn)
                (lambda (&rest _)
                  (let ((policy (niwa-skills-request-model-policy)))
                    (push (list (gptel--model-name (plist-get policy :model))
                                (plist-get policy :effort))
                          policies)
                    (setq-local niwa-skills--pending-request-context nil)))))
       ,@body)))

(niwa-deftest niwa-schedule-run-due/model-and-effort ()
  ,test
  (test)
  :doc "a wake-up's model and effort apply to its turn only, not to the session's selection"
  (niwa-schedule-test--with-session
    (niwa-schedule-test--with-woken-policies
      (with-current-buffer data-buf
        (niwa-schedule-create session data-buf "5m" "cheap check" nil
                                 "sch:sch-cheap" "low")
        (niwa-schedule-create session data-buf "10m" "ordinary check"))
      (setq now (+ now 300))
      (niwa-schedule-run-due)
      (niwa-schedule-test--settle)
      (should (equal '(("sch-cheap" low)) policies))
      (should (eq 'sch-main (buffer-local-value 'gptel-model data-buf)))
      (should-not (buffer-local-value 'niwa-skills--pending-request-context data-buf))
      ;; The next turn returns to the session's model.
      (niwa-agent-control-clear-context-mailbox session)
      (setq now (+ now 300))
      (niwa-schedule-run-due)
      (niwa-schedule-test--settle)
      (should (equal '(("sch-main" nil) ("sch-cheap" low)) policies))))

  :doc "a turn that also answers a wake-up without an override runs on the session's model"
  (niwa-schedule-test--with-session
    (niwa-schedule-test--with-woken-policies
      (with-current-buffer data-buf
        (niwa-schedule-create session data-buf "5m" "cheap check" nil
                                 "sch:sch-cheap" "low")
        (niwa-schedule-create session data-buf "5m" "ordinary check"))
      (setq now (+ now 300))
      (niwa-schedule-run-due)
      (niwa-schedule-test--settle)
      (should policies)
      (should (cl-every (lambda (policy) (equal '("sch-main" nil) policy)) policies))))

  :doc "an unavailable model or an unsupported effort is an error at creation"
  (niwa-schedule-test--with-session
    (niwa-schedule-test--with-woken-policies
      (with-current-buffer data-buf
        (dolist (case '(("sch:missing" nil "not registered")
                        ("sch:sch-cheap" "extreme" "unsupported")
                        ("sch:sch-main" "low" "does not support")))
          (ert-info ((format "%S" case))
            (should (string-search
                     (nth 2 case)
                     (cadr (should-error
                            (niwa-schedule-create
                             session data-buf "5m" "x" nil (nth 0 case) (nth 1 case))
                            :type 'user-error))))))
        (should-not (niwa-session-schedules session))))))

(niwa-deftest niwa-schedule-wake-request-context ()
  ,test
  (test)
  :doc "unread mail of wake-ups all asking for one model and effort yields that override"
  (niwa-schedule-test--with-session
    (niwa-schedule-test--with-woken-policies
      (should-not (niwa-schedule-wake-request-context session))
      (with-current-buffer data-buf
        (dotimes (_ 2)
          (niwa-schedule-create session data-buf "5m" "cheap" nil "sch:sch-cheap" "low")))
      (setq now (+ now 300))
      (niwa-schedule-run-due)
      (should (= 2 (length (niwa-schedule-test--wake-ups session))))
      (should (equal (list :model (niwa-model-parse-selector "sch:sch-cheap") :effort 'low)
                     (niwa-schedule-wake-request-context session)))))

  :doc "a wake-up without an override or with another one keeps the session's selection"
  (dolist (other '((nil nil) ("sch:sch-cheap" "high")))
    (niwa-schedule-test--with-session
      (niwa-schedule-test--with-woken-policies
        (with-current-buffer data-buf
          (niwa-schedule-create session data-buf "5m" "cheap" nil "sch:sch-cheap" "low")
          (niwa-schedule-create session data-buf "5m" "other" nil (car other) (cadr other)))
        (setq now (+ now 300))
        (niwa-schedule-run-due)
        (should (= 2 (length (niwa-schedule-test--wake-ups session))))
        (ert-info ((format "%S" other))
          (should-not (niwa-schedule-wake-request-context session)))))))

(niwa-deftest niwa-schedule-settle-wake ()
  ,test
  (test)
  :doc "each settled run is recorded and its mail retired; three consecutive failures pause"
  (niwa-schedule-test--with-session
    (let* (notices
           (niwa-notify-function (lambda (kind details)
                                      (push (cons kind (plist-get details :text)) notices)))
           (id (plist-get (niwa-schedule-create session data-buf "every 20m" "poll") :id)))
      (dolist (step '((error 1) (error 2) (success 0) (error 1) (abort 1) (error 2) (error 3)))
        (setq now (plist-get (car (niwa-schedule-pending session)) :due))
        (niwa-schedule-run-due)
        (let ((context (list :wake-runs (niwa-schedule-test--wake-ups session))))
          (niwa-schedule-settle-wake session data-buf context (car step) "Login expired")
          (should-not (plist-get context :wake-runs)))
        (should-not (niwa-schedule-test--wake-ups session))
        (should (= (cadr step) (plist-get (niwa-schedule--find id (niwa-session-schedules session))
                                          :failures))))
      (let ((entry (niwa-schedule--find id (niwa-session-schedules session))))
        (should (eq 'paused (plist-get entry :status)))
        (should (string-search "Login expired" (plist-get entry :pause-reason)))
        (should (equal '(failed failed skipped failed done failed failed)
                       (mapcar (lambda (run) (plist-get run :outcome)) (plist-get entry :runs))))
        (should (equal "Interrupted" (plist-get (nth 2 (plist-get entry :runs)) :reason))))
      (should (equal '(failure failure failure failure failure) (mapcar #'car notices)))
      (should (string-search "paused" (cdar notices)))
      (should-not (niwa-schedule-test--timers))))

  :doc "a failed one-shot is retained as a failed entry; a turn without wake runs changes nothing"
  (niwa-schedule-test--with-session
    (let ((niwa-notify-function #'ignore))
      (niwa-schedule-create session data-buf "20m" "Check CI")
      (setq now (+ now 1200))
      (niwa-schedule-run-due)
      (let ((mail (niwa-schedule-test--wake-ups session)))
        (niwa-schedule-settle-wake session data-buf nil 'error "Provider down")
        (should (equal mail (niwa-schedule-test--wake-ups session)))
        (niwa-schedule-settle-wake session data-buf (list :wake-runs mail) 'error "Provider down"))
      (let ((entry (car (niwa-session-schedules session))))
        (should (eq 'failed (plist-get entry :status)))
        (should (equal "Check CI" (plist-get entry :note)))
        (should (equal "Provider down" (plist-get (car (plist-get entry :runs)) :reason))))
      (should-not (niwa-schedule-test--wake-ups session)))))

(niwa-deftest niwa-schedule--rearm ()
  ,test
  (test)
  :doc "one timer is armed for the earliest wake-up and re-armed on every change"
  (niwa-schedule-test--with-session
    (cl-flet ((armed-in ()
                (let ((timers (niwa-schedule-test--timers)))
                  (should (= 1 (length timers)))
                  (round (float-time (time-subtract (timer--time (car timers)) nil))))))
      (let ((later (niwa-schedule-create session data-buf "1h" "later")))
        (should (< (abs (- 3600 (armed-in))) 5))
        (let ((sooner (niwa-schedule-create session data-buf "20m" "sooner")))
          (should (< (abs (- 1200 (armed-in))) 5))
          (niwa-schedule-cancel session data-buf (plist-get sooner :id))
          (should (< (abs (- 3600 (armed-in))) 5)))
        (niwa-schedule-cancel session data-buf (plist-get later :id))
        (should-not (niwa-schedule-test--timers)))))

  :doc "a buffer that lost the session lease neither arms nor fires its wake-ups"
  (niwa-schedule-test--with-session
    (niwa-schedule-create session data-buf "20m" "Check CI")
    (with-current-buffer data-buf
      (setq-local niwa-session--read-only-mode t))
    (setq now (+ now 1200))
    (niwa-schedule-run-due)
    (niwa-schedule-test--settle)
    (should-not (niwa-agent-control-context-mailbox session))
    (should (= 1 (length (niwa-schedule-pending session))))
    (should-not (niwa-schedule-test--timers))))

(niwa-deftest niwa-schedule-restore-session ()
  ,test
  (test)
  :doc "a resumed session re-arms its persisted wake-up and fires into its mailbox"
  (let* ((root (make-temp-file "niwa-schedule-restore-" t))
         (niwa-user-dir (file-name-as-directory (file-name-concat root "user")))
         (workspace (test-niwa-session-persistence--make-workspace root))
         (session (niwa-session-create "main" workspace))
         (buffer (generate-new-buffer " *schedule-restore*"))
         (now niwa-schedule-test--now)
         (niwa-schedule-clock (lambda () now))
         restored submitted)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa--submit-generated-turn)
                   ;; A real turn would send a provider request and consume the mail.
                   (lambda (prompt &rest _) (push prompt submitted))))
          (with-current-buffer buffer
            (org-mode)
            (insert "Check CI later\n")
            (niwa-session-artifacts-save session buffer)
            (setq-local niwa--session session))
          (setf (niwa-session-root-buffer session) buffer)
          (niwa-schedule-create session buffer "20m" "Check CI")
          (should (niwa-schedule-test--timers))
          (test-niwa-session-persistence--release-and-kill buffer session)
          ;; Closed, the session stays scheduled; resuming hands it to its buffer.
          (should (= 1 (length (niwa-schedule-test--timers))))
          (setq restored (niwa-session-persistence-restore
                          (niwa-session-save-path session) nil nil workspace))
          (should (= 1 (length (niwa-schedule-test--timers))))
          (should-not niwa-schedule--closed)
          (setq now (+ now 1200))
          (niwa-schedule-run-due)
          (let* ((restored-session (buffer-local-value 'niwa--session restored))
                 mail)
            ;; A persisted session commits background mail in its next batch.
            (niwa-test--await 5 "The wake-up was not committed"
              (setq mail (niwa-agent-control-context-mailbox restored-session)))
            (should (equal '(WAKE_UP) (mapcar (lambda (record) (plist-get record :type)) mail)))
            (should (equal "Check CI" (plist-get (car mail) :payload)))
            (should-not (niwa-schedule-pending restored-session))
            ;; The firing and its removal from the store committed together.
            (let ((saved (niwa-session-codec-read
                          (file-name-concat (niwa-session-save-path restored-session)
                                            "session.meta.el"))))
              (should-not (plist-get saved :schedules))
              (should (equal '(WAKE_UP) (mapcar (lambda (record) (plist-get record :type))
                                                (plist-get saved :messages)))))
            ;; The restored root is idle and has its view, so it is offered a turn.
            (niwa-test--await 5 "The restored root was not woken" submitted)
            (should (equal (list niwa-pending-inputs--event-trigger) submitted))))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (when (buffer-live-p restored)
        (test-niwa-session-persistence--release-and-kill
         restored (buffer-local-value 'niwa--session restored)))
      (should-not (niwa-schedule-test--timers))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-schedule)
;;; test-niwa-schedule.el ends here
