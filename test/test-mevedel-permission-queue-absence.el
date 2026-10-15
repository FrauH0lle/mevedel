;;; test-mevedel-permission-queue-absence.el -- Tests for the permission queue absence timeout -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `mevedel-permission-queue-timeout': the shared deadline, expiry
;; of the whole unanswered queue, the waiting-for-user hold that follows, and
;; the actions that end it.  Timers are replaced by recorded closures so a
;; test fires expiry deterministically and leaves no live timer behind.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-structs)
(require 'mevedel-permission-queue)
(require 'mevedel-pending-inputs)
(require 'mevedel-goal)
(require 'mevedel-tool-permission)
(require 'mevedel-tools)

(defvar test-pqa--timers nil
  "Recorded (TIMER FUNCTION ARGS SECONDS) of the stubbed `run-at-time'.")

(defmacro test-pqa--with-timeout (seconds &rest body)
  "Run BODY with the absence timeout at SECONDS and recorded timers."
  (declare (indent 1))
  `(let ((mevedel-permission-queue-timeout ,seconds)
         (mevedel-permission-reviewer 'user)
         (test-pqa--timers nil))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (time _repeat function &rest args)
                  (let ((timer (timer-create)))
                    (push (list timer function args time) test-pqa--timers)
                    timer)))
               ((symbol-function 'mevedel-permission-queue--render-entry)
                #'ignore)
               ((symbol-function 'mevedel-collaboration-retire-permission-requests)
                #'ignore))
       ,@body)))

(defun test-pqa--session ()
  "Return a fresh ask-mode session."
  (mevedel-session--create :name "absence" :permission-mode 'ask))

(defun test-pqa--enqueue (session &optional kind callback)
  "Enqueue a KIND entry on SESSION settling through CALLBACK; return its cell."
  (let ((cell (list '(:source user))))
    (mevedel-permission--enqueue
     (list :kind (or kind 'generic) :tool-name "Read" :command "ls"
           :origin "/root" :approval-decision-cell cell
           :callback (or callback #'ignore))
     session)
    cell))

(defun test-pqa--fire-latest ()
  "Run the most recently scheduled expiry."
  (pcase-let ((`(,_timer ,function ,args ,_time) (car test-pqa--timers)))
    (apply function args)))

(mevedel-deftest mevedel-permission-queue--arm
  (:quiet t :doc "deadline lifecycle")
  ,test
  (test)

  :doc "a disabled timeout creates no state or timer"
  (test-pqa--with-timeout nil
    (let ((session (test-pqa--session)))
      (test-pqa--enqueue session)
      (should (mevedel-session-permission-queue session))
      (should-not (mevedel-session-permission-absence session))
      (should-not (mevedel-permission-absence-deadline session))
      (should-not test-pqa--timers)
      (mevedel-permission-queue-abort-all session)))

  :doc "the first card arms one shared deadline that siblings do not extend"
  (test-pqa--with-timeout 3600
    (let ((session (test-pqa--session)))
      (test-pqa--enqueue session)
      (let ((state (mevedel-session-permission-absence session))
            (deadline (mevedel-permission-absence-deadline session)))
        (should (< (abs (- deadline (+ (float-time) 3600))) 5))
        (test-pqa--enqueue session 'bash)
        (test-pqa--enqueue session 'eval)
        (should (eq state (mevedel-session-permission-absence session)))
        (should (= 1 (length test-pqa--timers))))
      (mevedel-permission-queue-abort-all session)))

  :doc "a genuine answer restarts the period and a superseded timer is a no-op"
  (test-pqa--with-timeout 3600
    (let ((session (test-pqa--session)))
      (test-pqa--enqueue session)
      (test-pqa--enqueue session)
      (let ((old (mevedel-session-permission-absence session))
            (head (car (mevedel-session-permission-queue session))))
        (mevedel-permission-queue--on-head-outcome head 'allow-once)
        (should (= 1 (length (mevedel-session-permission-queue session))))
        (should (= 2 (length test-pqa--timers)))
        (should-not (eq old (mevedel-session-permission-absence session)))
        (mevedel-permission-queue--expire session old)
        (should (mevedel-session-permission-queue session))
        (should-not (mevedel-permission-absence-held-p session)))
      (mevedel-permission-queue-abort-all session)))

  :doc "render failure, abort and coalescing never restart the period"
  (test-pqa--with-timeout 3600
    (let ((session (test-pqa--session)))
      (dotimes (_ 4) (test-pqa--enqueue session))
      (let ((state (mevedel-session-permission-absence session)))
        (mevedel-permission-queue--pop
         (car (mevedel-session-permission-queue session)) 'deny-once
         'render-failed)
        (mevedel-permission-queue--on-head-outcome
         (car (mevedel-session-permission-queue session)) 'aborted)
        (cl-letf (((symbol-function 'mevedel-permission-queue--reevaluate)
                   (lambda (_) 'ask)))
          (mevedel-permission-queue--coalesce 'allow-session session))
        (should (eq state (mevedel-session-permission-absence session)))
        (should (= 1 (length test-pqa--timers))))
      (mevedel-permission-queue-abort-all session)))

  :doc "an empty queue cancels the deadline"
  (test-pqa--with-timeout 3600
    (let ((session (test-pqa--session)))
      (test-pqa--enqueue session)
      (mevedel-permission-queue--on-head-outcome
       (car (mevedel-session-permission-queue session)) 'deny-once)
      (should-not (mevedel-session-permission-absence session))
      (test-pqa--enqueue session)
      (mevedel-permission-queue-abort-all session)
      (should-not (mevedel-session-permission-absence session)))))

(mevedel-deftest mevedel-permission-queue--expire
  (:quiet t :doc "expiry denies the whole queue and starts the hold")
  ,test
  (test)

  :doc "every kind is denied with the absence reason and absence provenance"
  (test-pqa--with-timeout 3600
    (let* ((session (test-pqa--session))
           (outcomes nil)
           (cells (mapcar
                   (lambda (kind)
                     (test-pqa--enqueue
                      session kind
                      (lambda (outcome) (push (cons kind outcome) outcomes))))
                   '(generic bash eval sandbox)))
           (entries (copy-sequence (mevedel-session-permission-queue session)))
           retired)
      (cl-letf (((symbol-function 'mevedel-collaboration-retire-permission-requests)
                 (lambda (_session queue) (setq retired queue))))
        (test-pqa--fire-latest))
      (should (equal entries retired))
      (should-not (mevedel-session-permission-queue session))
      (should (= 4 (length outcomes)))
      (dolist (item outcomes)
        (should (eq 'deny (cadr item)))
        (should (string-match-p "expired without a human response"
                                (cddr item))))
      (dolist (cell cells)
        (should (eq 'absence (plist-get (car cell) :source))))
      (should (mevedel-permission-absence-held-p session))
      (should (= 4 (plist-get (mevedel-permission-absence-hold session) :count)))
      (should-not (mevedel-permission-absence-deadline session))))

  :doc "a callback enqueueing again is denied under the hold, not carded"
  (test-pqa--with-timeout 3600
    (let* ((session (test-pqa--session))
           (rendered nil) (second nil))
      (test-pqa--enqueue
       session 'generic
       (lambda (_)
         (test-pqa--enqueue session 'generic
                            (lambda (outcome) (setq second outcome)))))
      (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
                 (lambda (_) (setq rendered t))))
        (test-pqa--fire-latest))
      (should-not rendered)
      (should (eq 'deny (car second)))
      (should (string-match-p "without prompting" (cdr second)))
      (should-not (mevedel-session-permission-queue session))
      (should (= 1 (length test-pqa--timers)))))

  :doc "a coalesce interrupted by expiry cannot restore the expired entries"
  (test-pqa--with-timeout 3600
    (let* ((session (test-pqa--session))
           (outcomes nil))
      (test-pqa--enqueue session 'generic
                         (lambda (outcome) (push outcome outcomes)))
      (test-pqa--enqueue session 'generic
                         (lambda (outcome) (push outcome outcomes)))
      (cl-letf (((symbol-function 'mevedel-permission-queue--reevaluate)
                 (lambda (_entry)
                   (when (mevedel-session-permission-queue session)
                     (test-pqa--fire-latest))
                   'ask)))
        (mevedel-permission-queue--coalesce 'allow-session session))
      (should-not (mevedel-session-permission-queue session))
      (should (= 2 (length outcomes)))
      (should (mevedel-permission-absence-held-p session))))

  :doc "an erroring callback still settles every entry once"
  (test-pqa--with-timeout 3600
    (let* ((session (test-pqa--session))
           (count 0))
      (test-pqa--enqueue session 'generic (lambda (_) (error "Boom")))
      (test-pqa--enqueue session 'generic (lambda (_) (cl-incf count)))
      (mevedel-test--with-captured-diagnostics nil
        (test-pqa--fire-latest))
      (should (= 1 count))
      (should-not (mevedel-session-permission-queue session))))

  :doc "a late answer to an expired card grants nothing and says so"
  (test-pqa--with-timeout 3600
    (let* ((session (test-pqa--session))
           (outcomes nil) messages)
      (test-pqa--enqueue session 'generic
                         (lambda (outcome) (push outcome outcomes)))
      (let ((entry (car (mevedel-session-permission-queue session))))
        (test-pqa--fire-latest)
        (mevedel-test--with-captured-messages messages
          (mevedel-permission-queue--on-head-outcome entry 'allow-once))
        (should (= 1 (length outcomes)))
        (should (eq 'deny (car (car outcomes))))
        (should (string-match-p "Request expired; nothing was approved"
                                messages)))))

  :doc "reviewer fallback during the hold is denied once without a card"
  (test-pqa--with-timeout 3600
    (let* ((session (test-pqa--session))
           (mevedel-permission-reviewer 'auto)
           (outcomes nil) rendered)
      (setf (mevedel-session-permission-absence session)
            (list :expired-at (float-time) :count 1))
      (cl-letf (((symbol-function 'mevedel-permission-review-start)
                 (lambda (_entry fallback) (funcall fallback)))
                ((symbol-function 'mevedel-permission-queue--render-entry)
                 (lambda (_) (setq rendered t))))
        (test-pqa--enqueue session 'generic
                           (lambda (outcome) (push outcome outcomes))))
      (should-not rendered)
      (should (= 1 (length outcomes)))
      (should (eq 'deny (car (car outcomes))))
      (should-not (mevedel-session-permission-queue session))
      (should-not test-pqa--timers)))

  :doc "disabling the option cancels the deadline and clears the hold"
  (test-pqa--with-timeout 3600
    (let ((held (test-pqa--session))
          (armed (test-pqa--session))
          (first (generate-new-buffer " *absence-held*"))
          (second (generate-new-buffer " *absence-armed*")))
      (unwind-protect
          (progn
            (test-pqa--enqueue held)
            (test-pqa--fire-latest)
            (test-pqa--enqueue armed)
            (should (mevedel-permission-absence-deadline armed))
            (with-current-buffer first (setq-local mevedel--session held))
            (with-current-buffer second (setq-local mevedel--session armed))
            (mevedel-permission-queue--set-timeout
             'mevedel-permission-queue-timeout nil)
            (should-not (mevedel-session-permission-absence held))
            (should-not (mevedel-session-permission-absence armed))
            (should-not (mevedel-permission-absence-held-p held)))
        (kill-buffer first)
        (kill-buffer second)
        (setq mevedel-permission-queue-timeout 3600)
        (mevedel-permission-queue-abort-all armed)))))

(mevedel-deftest mevedel-permission-absence-clear
  (:quiet t :doc "only clearing the hold resumes automatic work")
  ,test
  (test)

  :doc "clearing ends the hold and the next request is carded again"
  (test-pqa--with-timeout 3600
    (let ((session (test-pqa--session)) rendered)
      (test-pqa--enqueue session)
      (test-pqa--fire-latest)
      (should (mevedel-permission-absence-clear session))
      (should-not (mevedel-permission-absence-clear session))
      (should-not (mevedel-permission-absence-held-p session))
      (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
                 (lambda (_) (setq rendered t))))
        (test-pqa--enqueue session))
      (should rendered)
      (should (mevedel-permission-absence-deadline session))
      (mevedel-permission-queue-abort-all session)))

  :doc "the hold blocks follow-up drain and Goal continuation"
  (test-pqa--with-timeout 3600
    (let ((session (test-pqa--session)))
      (test-pqa--enqueue session)
      (test-pqa--fire-latest)
      (should (mevedel-view--follow-up-auto-drain-blocked-p session))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (setf (mevedel-session-goal session)
              (mevedel-goal--create :id "goal" :objective "objective"
                                    :status 'active))
        (should (eq 'absence (mevedel-goal-continue-if-idle
                              session (current-buffer)))))
      (setf (mevedel-session-goal session) nil)
      (mevedel-permission-absence-clear session)
      (cl-letf (((symbol-function 'mevedel-view--reserved-goal-handoff-id)
                 #'ignore))
        (should-not (mevedel-view--follow-up-auto-drain-blocked-p session))))))

(mevedel-deftest mevedel-permission-queue--absence-adapters
  (:quiet t :doc "expiry reaches the model through the real permission step")
  ,test
  (test)

  :doc "a Bash card fails with the absence reason and absence provenance"
  (test-pqa--with-timeout 3600
    (let ((session (mevedel-session--create
                    :permission-mode 'ask :sandbox-mode 'off
                    :authority-mode 'pid-lock))
          (mevedel-permission-rules nil)
          failure)
      (with-temp-buffer
        (setq-local mevedel--session session)
        (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                   (lambda (_event _payload callback &rest _)
                     (funcall callback nil))))
          (mevedel-tool-permission-step
           (list :tool (mevedel-tool-ensure "Bash")
                 :args (list :command "touch example")
                 :session session :buffer (current-buffer))
           (lambda (_) (ert-fail "Expiry must not allow"))
           (lambda (&rest args) (setq failure args)))
          (should (mevedel-session-permission-queue session))
          (test-pqa--fire-latest)))
      (should failure)
      (should (string-match-p "expired without a human response"
                              (car failure)))
      (should (eq 'absence
                  (plist-get (cadr failure) :permission-denial-provenance)))
      (should (eq 'permission-denied (nth 2 failure)))
      (should (mevedel-permission-absence-held-p session)))))

(provide 'test-mevedel-permission-queue-absence)
;;; test-mevedel-permission-queue-absence.el ends here
