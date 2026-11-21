;;; test-niwa-permission-queue-absence.el -- Tests for the permission queue absence timeout -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `niwa-permission-queue-timeout': the shared deadline, expiry
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
(require 'niwa-structs)
(require 'niwa-permission-queue)
(require 'niwa-pending-inputs)
(require 'niwa-goal)
(require 'niwa-tool-permission)
(require 'niwa-tools)

(defvar test-pqa--timers nil
  "Recorded (TIMER FUNCTION ARGS SECONDS) of the stubbed `run-at-time'.")

(defmacro test-pqa--with-timeout (seconds &rest body)
  "Run BODY with the absence timeout at SECONDS and recorded timers."
  (declare (indent 1))
  `(let ((niwa-permission-queue-timeout ,seconds)
         (niwa-permission-reviewer 'user)
         (test-pqa--timers nil))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (time _repeat function &rest args)
                  (let ((timer (timer-create)))
                    (push (list timer function args time) test-pqa--timers)
                    timer)))
               ((symbol-function 'niwa-permission-queue--render-entry)
                #'ignore)
               ((symbol-function 'niwa-collaboration-retire-permission-requests)
                #'ignore))
       ,@body)))

(defun test-pqa--session ()
  "Return a fresh ask-mode session."
  (niwa-session--create :name "absence" :permission-mode 'ask))

(defun test-pqa--enqueue (session &optional kind callback)
  "Enqueue a KIND entry on SESSION settling through CALLBACK; return its cell."
  (let ((cell (list '(:source user))))
    (niwa-permission-request
     (list :kind (or kind 'generic) :tool-name "Read" :command "ls"
           :origin "/root" :approval-decision-cell cell
           :callback (or callback #'ignore))
     session)
    cell))

(defun test-pqa--fire-latest ()
  "Run the most recently scheduled expiry."
  (pcase-let ((`(,_timer ,function ,args ,_time) (car test-pqa--timers)))
    (apply function args)))

(niwa-deftest niwa-permission-queue--arm
  (:quiet t :doc "deadline lifecycle")
  ,test
  (test)

  :doc "a disabled timeout creates no state or timer"
  (test-pqa--with-timeout nil
    (let ((session (test-pqa--session)))
      (test-pqa--enqueue session)
      (should (niwa-session-permission-queue session))
      (should-not (niwa-session-permission-absence session))
      (should-not (niwa-permission-absence-deadline session))
      (should-not test-pqa--timers)
      (niwa-permission-queue-abort-all session)))

  :doc "the first card arms one shared deadline that siblings do not extend"

  :doc "the deadline is armed through the TRAMP-safe timer"
  (test-pqa--with-timeout 3600
    (let ((session (test-pqa--session)) armed)
      (cl-letf (((symbol-function 'niwa-transport-run-at-time)
                 (lambda (seconds function &rest args)
                   (push (list seconds function args) armed)
                   (timer-create))))
        (test-pqa--enqueue session))
      (should (equal (list (list 3600 #'niwa-permission-queue--expire
                                 (list session
                                       (niwa-session-permission-absence session))))
                     armed))
      (niwa-permission-queue-abort-all session)))

  :doc "the first card arms one shared deadline that siblings do not extend"
  (test-pqa--with-timeout 3600
    (let ((session (test-pqa--session)))
      (test-pqa--enqueue session)
      (let ((state (niwa-session-permission-absence session))
            (deadline (niwa-permission-absence-deadline session)))
        (should (< (abs (- deadline (+ (float-time) 3600))) 5))
        (test-pqa--enqueue session 'bash)
        (test-pqa--enqueue session 'eval)
        (should (eq state (niwa-session-permission-absence session)))
        (should (= 1 (length test-pqa--timers))))
      (niwa-permission-queue-abort-all session)))

  :doc "a genuine answer restarts the period and a superseded timer is a no-op"
  (test-pqa--with-timeout 3600
    (let ((session (test-pqa--session)))
      (test-pqa--enqueue session)
      (test-pqa--enqueue session)
      (let ((old (niwa-session-permission-absence session))
            (head (car (niwa-session-permission-queue session))))
        (niwa-permission-queue--on-head-outcome head 'allow-once)
        (should (= 1 (length (niwa-session-permission-queue session))))
        (should (= 2 (length test-pqa--timers)))
        (should-not (eq old (niwa-session-permission-absence session)))
        (niwa-permission-queue--expire session old)
        (should (niwa-session-permission-queue session))
        (should-not (niwa-permission-absence-held-p session)))
      (niwa-permission-queue-abort-all session)))

  :doc "render failure, abort and coalescing never restart the period"
  (test-pqa--with-timeout 3600
    (let ((session (test-pqa--session)))
      (dotimes (_ 4) (test-pqa--enqueue session))
      (let ((state (niwa-session-permission-absence session)))
        (niwa-permission-queue--pop
         (car (niwa-session-permission-queue session)) 'deny-once
         'render-failed)
        (niwa-permission-queue--on-head-outcome
         (car (niwa-session-permission-queue session)) 'aborted)
        (cl-letf (((symbol-function 'niwa-permission-queue--reevaluate)
                   (lambda (_) 'ask)))
          (niwa-permission-queue--coalesce 'allow-session session))
        (should (eq state (niwa-session-permission-absence session)))
        (should (= 1 (length test-pqa--timers))))
      (niwa-permission-queue-abort-all session)))

  :doc "an empty queue cancels the deadline"
  (test-pqa--with-timeout 3600
    (let ((session (test-pqa--session)))
      (test-pqa--enqueue session)
      (niwa-permission-queue--on-head-outcome
       (car (niwa-session-permission-queue session)) 'deny-once)
      (should-not (niwa-session-permission-absence session))
      (test-pqa--enqueue session)
      (niwa-permission-queue-abort-all session)
      (should-not (niwa-session-permission-absence session)))))

(niwa-deftest niwa-permission-queue--expire
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
           (entries (copy-sequence (niwa-session-permission-queue session)))
           retired)
      (cl-letf (((symbol-function 'niwa-collaboration-retire-permission-requests)
                 (lambda (_session queue) (setq retired queue))))
        (test-pqa--fire-latest))
      (should (equal entries retired))
      (should-not (niwa-session-permission-queue session))
      (should (= 4 (length outcomes)))
      (dolist (item outcomes)
        (should (eq 'deny (cadr item)))
        (should (string-match-p "expired without a human response"
                                (cddr item))))
      (dolist (cell cells)
        (should (eq 'absence (plist-get (car cell) :source))))
      (should (niwa-permission-absence-held-p session))
      (should (= 4 (plist-get (niwa-permission-absence-hold session) :count)))
      (should-not (niwa-permission-absence-deadline session))))

  :doc "a callback enqueueing again is denied under the hold, not carded"
  (test-pqa--with-timeout 3600
    (let* ((session (test-pqa--session))
           (rendered nil) (second nil))
      (test-pqa--enqueue
       session 'generic
       (lambda (_)
         (test-pqa--enqueue session 'generic
                            (lambda (outcome) (setq second outcome)))))
      (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                 (lambda (_) (setq rendered t))))
        (test-pqa--fire-latest))
      (should-not rendered)
      (should (eq 'deny (car second)))
      (should (string-match-p "without prompting" (cdr second)))
      (should-not (niwa-session-permission-queue session))
      (should (= 1 (length test-pqa--timers)))))

  :doc "a coalesce interrupted by expiry cannot restore the expired entries"
  (test-pqa--with-timeout 3600
    (let* ((session (test-pqa--session))
           (outcomes nil))
      (test-pqa--enqueue session 'generic
                         (lambda (outcome) (push outcome outcomes)))
      (test-pqa--enqueue session 'generic
                         (lambda (outcome) (push outcome outcomes)))
      (cl-letf (((symbol-function 'niwa-permission-queue--reevaluate)
                 (lambda (_entry)
                   (when (niwa-session-permission-queue session)
                     (test-pqa--fire-latest))
                   'ask)))
        (niwa-permission-queue--coalesce 'allow-session session))
      (should-not (niwa-session-permission-queue session))
      (should (= 2 (length outcomes)))
      (should (niwa-permission-absence-held-p session))))

  :doc "an erroring callback still settles every entry once"
  (test-pqa--with-timeout 3600
    (let* ((session (test-pqa--session))
           (count 0))
      (test-pqa--enqueue session 'generic (lambda (_) (error "Boom")))
      (test-pqa--enqueue session 'generic (lambda (_) (cl-incf count)))
      (niwa-test--with-captured-diagnostics nil
        (test-pqa--fire-latest))
      (should (= 1 count))
      (should-not (niwa-session-permission-queue session))))

  :doc "a late answer to an expired card grants nothing and says so"
  (test-pqa--with-timeout 3600
    (let* ((session (test-pqa--session))
           (outcomes nil) messages)
      (test-pqa--enqueue session 'generic
                         (lambda (outcome) (push outcome outcomes)))
      (let ((entry (car (niwa-session-permission-queue session))))
        (test-pqa--fire-latest)
        (niwa-test--with-captured-messages messages
          (niwa-permission-queue--on-head-outcome entry 'allow-once))
        (should (= 1 (length outcomes)))
        (should (eq 'deny (car (car outcomes))))
        (should (string-match-p "Request expired; nothing was approved"
                                messages)))))

  :doc "reviewer fallback during the hold is denied once without a card"
  (test-pqa--with-timeout 3600
    (let* ((session (test-pqa--session))
           (niwa-permission-reviewer 'auto)
           (outcomes nil) rendered)
      (setf (niwa-session-permission-absence session)
            (list :expired-at (float-time) :count 1))
      (cl-letf (((symbol-function 'niwa-permission-review-start)
                 (lambda (_entry fallback) (funcall fallback)))
                ((symbol-function 'niwa-permission-queue--render-entry)
                 (lambda (_) (setq rendered t))))
        (test-pqa--enqueue session 'generic
                           (lambda (outcome) (push outcome outcomes))))
      (should-not rendered)
      (should (= 1 (length outcomes)))
      (should (eq 'deny (car (car outcomes))))
      (should-not (niwa-session-permission-queue session))
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
            (should (niwa-permission-absence-deadline armed))
            (with-current-buffer first (setq-local niwa--session held))
            (with-current-buffer second (setq-local niwa--session armed))
            (let (rebuilt)
              ;; Both cards redraw: the armed one must drop its stale deadline.
              (cl-letf (((symbol-function 'niwa-permission-queue--rebuild-view)
                         (lambda (session) (push session rebuilt))))
                (niwa-permission-queue--set-timeout
                 'niwa-permission-queue-timeout nil))
              (should (memq held rebuilt))
              (should (memq armed rebuilt))
              (should (= 2 (length rebuilt))))
            ;; Disabling leaves the queue itself untouched.
            (should (= 1 (length (niwa-session-permission-queue armed))))
            (should-not (niwa-session-permission-absence held))
            (should-not (niwa-session-permission-absence armed))
            (should-not (niwa-permission-absence-held-p held)))
        (kill-buffer first)
        (kill-buffer second)
        (setq niwa-permission-queue-timeout 3600)
        (niwa-permission-queue-abort-all armed)))))

(niwa-deftest niwa-permission-absence-clear
  (:quiet t :doc "only clearing the hold resumes automatic work")
  ,test
  (test)

  :doc "clearing ends the hold and the next request is carded again"
  (test-pqa--with-timeout 3600
    (let ((session (test-pqa--session)) rendered)
      (test-pqa--enqueue session)
      (test-pqa--fire-latest)
      (should (niwa-permission-absence-clear session))
      (should-not (niwa-permission-absence-clear session))
      (should-not (niwa-permission-absence-held-p session))
      (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                 (lambda (_) (setq rendered t))))
        (test-pqa--enqueue session))
      (should rendered)
      (should (niwa-permission-absence-deadline session))
      (niwa-permission-queue-abort-all session)))

  :doc "the hold blocks follow-up drain and Goal continuation"
  (test-pqa--with-timeout 3600
    (let ((session (test-pqa--session)))
      (test-pqa--enqueue session)
      (test-pqa--fire-latest)
      (should (niwa-view--follow-up-auto-drain-blocked-p session))
      (with-temp-buffer
        (setq-local niwa--session session)
        (setf (niwa-session-goal session)
              (niwa-goal--create :id "goal" :objective "objective"
                                    :status 'active))
        (should (eq 'absence (niwa-goal-continue-if-idle
                              session (current-buffer)))))
      (setf (niwa-session-goal session) nil)
      (niwa-permission-absence-clear session)
      (cl-letf (((symbol-function 'niwa-view--reserved-goal-handoff-id)
                 #'ignore))
        (should-not (niwa-view--follow-up-auto-drain-blocked-p session))))))

(niwa-deftest niwa-permission-queue--absence-adapters
  (:quiet t :doc "expiry reaches the model through the real permission step")
  ,test
  (test)

  :doc "a Bash card fails with the absence reason and absence provenance"
  (test-pqa--with-timeout 3600
    (let ((session (niwa-session--create
                    :permission-mode 'ask :sandbox-mode 'off
                    ))
          (niwa-permission-rules nil)
          failure)
      (with-temp-buffer
        (setq-local niwa--session session)
        (cl-letf (((symbol-function 'niwa-hooks-run-event)
                   (lambda (_event _payload callback &rest _)
                     (funcall callback nil))))
          (niwa-tool-permission-step
           (list :tool (niwa-tool-ensure "Bash")
                 :args (list :command "touch example")
                 :session session :buffer (current-buffer))
           (lambda (_) (ert-fail "Expiry must not allow"))
           (lambda (&rest args) (setq failure args)))
          (should (niwa-session-permission-queue session))
          (test-pqa--fire-latest)))
      (should failure)
      (should (string-match-p "expired without a human response"
                              (car failure)))
      (should (eq 'absence
                  (plist-get (cadr failure) :permission-denial-provenance)))
      (should (eq 'permission-denied (nth 2 failure)))
      (should (niwa-permission-absence-held-p session)))))

(provide 'test-niwa-permission-queue-absence)
;;; test-niwa-permission-queue-absence.el ends here
