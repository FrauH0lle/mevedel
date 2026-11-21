;;; test-niwa-journal-idle.el -- Idle journal sealing and maintenance tests -*- lexical-binding: t -*-

;;; Commentary:

;; Real sessions, captures and timers; the downstream schedulers are recorded
;; where a case asserts which opportunities were queued.

;;; Code:

(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-test-support"))
(require 'niwa-journal-idle)

(defvar niwa--agent-invocation)

(defun niwa-test-journal-idle--drain ()
  "Wait until due idle seals have left their transport queue."
  (with-timeout (5 (ert-fail "Idle seal did not run"))
    (while (> (hash-table-count niwa-journal-idle--seal-pending) 0)
      (accept-process-output nil 0.01))))

(defmacro niwa-test-journal-idle--recording (calls &rest body)
  "Run BODY recording downstream scheduler calls into CALLS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'niwa-journal-process-schedule)
              (lambda (workspace &optional recover)
                (push (list 'process workspace recover) ,calls)))
             ((symbol-function 'niwa-memory-pass-schedule)
              (lambda (workspace) (push (list 'pass workspace) ,calls)))
             ((symbol-function 'niwa-journal-cleanup-schedule)
              (lambda (workspace &optional _force) (push (list 'cleanup workspace) ,calls))))
     ,@body))

(niwa-deftest niwa-journal-idle-arm-seal
  (:vars* ((niwa-journal-idle--inhibit-scheduling nil)
           (niwa-journal-seal-idle-minutes 20)
           (niwa-journal-idle-maintenance-minutes 10))
   :after-each ((niwa-journal-idle-stop-all)))
  ,test
  (test)
  :doc "each completed root turn restarts one seal period and starts maintenance"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-session-set-root-buffer session buffer)
     (niwa-test-journal-capture--turn session buffer "First" "First result")
     (let ((first (gethash session niwa-journal-idle--seal-timers)))
       (should (timerp first))
       (should (memq first timer-list))
       (should (memq niwa-journal-idle--maintenance-timer timer-list))
       (niwa-test-journal-capture--turn session buffer "Second" "Second result")
       (let ((second (gethash session niwa-journal-idle--seal-timers)))
         (should (timerp second))
         (should-not (eq first second))
         (should-not (memq first timer-list))
         (should (equal (list session buffer 2) (timer--args second)))))))

  :doc "killing the root buffer cancels its armed seal"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-journal-idle-arm-seal session buffer)
     (let ((timer (gethash session niwa-journal-idle--seal-timers)))
       (should (timerp timer))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (let ((niwa-journal-enabled nil)) (kill-buffer buffer))
       (should-not (gethash session niwa-journal-idle--seal-timers))
       (should-not (memq timer timer-list)))))

  :doc "disabled sealing or journaling arms no seal but still maintains the workspace"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (let ((niwa-journal-seal-idle-minutes nil))
       (niwa-journal-idle-arm-seal session buffer))
     (let ((niwa-journal-enabled nil))
       (niwa-journal-idle-arm-seal session buffer))
     (should (= 0 (hash-table-count niwa-journal-idle--seal-timers)))
     (should (timerp niwa-journal-idle--maintenance-timer))))

  :doc "inhibited scheduling arms nothing"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (let ((niwa-journal-idle--inhibit-scheduling t))
       (niwa-journal-idle-arm-seal session buffer))
     (should (= 0 (hash-table-count niwa-journal-idle--seal-timers)))
     (should-not niwa-journal-idle--maintenance-timer))))

(niwa-deftest niwa-journal-idle--seal-due
  (:vars* ((niwa-journal-idle--inhibit-scheduling nil)
           (niwa-journal-seal-idle-minutes 20)
           (niwa-journal-idle-maintenance-minutes nil))
   :after-each ((niwa-journal-idle-stop-all)))
  ,test
  (test)
  :doc "a quiet session seals its ready checkpoint with the idle trigger and queues processing"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-session-set-root-buffer session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            calls)
       (should-not (niwa-journal-capture-trigger workspace capture))
       (niwa-test-journal-idle--recording calls
         (niwa-journal-idle--seal-due session buffer (niwa-session-turn-count session))
         (niwa-test-journal-idle--drain))
       (should (eq 'idle (niwa-journal-capture-trigger workspace capture)))
       (should (equal (list (list 'process workspace nil)) calls))
       (should-not (gethash session niwa-journal-idle--seal-timers))
       ;; Later work forms a new checkpoint outside the sealed one.
       (niwa-test-journal-capture--turn session buffer "Later" "Later result")
       (should (= 2 (length (niwa-journal-capture-list workspace)))))))

  :doc "a newer completed turn leaves the stale period without sealing or re-arming"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-session-set-root-buffer session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let ((workspace (niwa-session-workspace session)))
       (niwa-journal-idle--seal-due session buffer (1- (niwa-session-turn-count session)))
       (should (= 0 (hash-table-count niwa-journal-idle--seal-pending)))
       (should-not (niwa-journal-capture-trigger
                    workspace (car (niwa-journal-capture-list workspace)))))))

  :doc "a running request restarts the period instead of sealing"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-session-set-root-buffer session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let ((workspace (niwa-session-workspace session))
           (turn (niwa-session-turn-count session)))
       (niwa-journal-idle-stop-all)
       (with-current-buffer buffer (setq-local niwa--current-request 'running))
       (unwind-protect
           (progn
             (niwa-journal-idle--seal-due session buffer turn)
             (should (= 0 (hash-table-count niwa-journal-idle--seal-pending)))
             (should (equal (list session buffer turn)
                            (timer--args (gethash session niwa-journal-idle--seal-timers))))
             (should-not (niwa-journal-capture-trigger
                          workspace (car (niwa-journal-capture-list workspace)))))
         (with-current-buffer buffer (setq-local niwa--current-request nil)))))))

(niwa-deftest niwa-journal-idle--quiet-p ()
  ,test
  (test)
  :doc "requires the live root buffer, no running request and an unchanged turn"
  (let* ((root (make-temp-file "niwa-journal-idle-" t))
         (session (niwa-session--create :workspace (niwa-workspace--create :root root)
                                           :turn-count 3))
         (buffer (generate-new-buffer " *journal-idle-quiet*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer (setq-local niwa--session session))
          (should-not (niwa-journal-idle--quiet-p session buffer 3))
          (niwa-session-set-root-buffer session buffer)
          (should (niwa-journal-idle--quiet-p session buffer 3))
          (should-not (niwa-journal-idle--quiet-p session buffer 2))
          (with-current-buffer buffer (setq-local niwa--current-request 'running))
          (should-not (niwa-journal-idle--quiet-p session buffer 3))
          (kill-buffer buffer)
          (should-not (niwa-journal-idle--quiet-p session buffer 3)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(niwa-deftest niwa-journal-idle--live-workspaces ()
  ,test
  (test)
  :doc "returns each workspace of a live root session once, ignoring other session buffers"
  (let* ((first-root (make-temp-file "niwa-journal-idle-a-" t))
         (second-root (make-temp-file "niwa-journal-idle-b-" t))
         (first (niwa-workspace--create :root first-root))
         (second (niwa-workspace--create :root second-root))
         buffers)
    (cl-labels ((session-buffer (workspace &optional root-p agent-p)
                  (let ((buffer (generate-new-buffer " *journal-idle-session*"))
                        (session (niwa-session--create :workspace workspace)))
                    (push buffer buffers)
                    (with-current-buffer buffer
                      (setq-local niwa--session session)
                      (when agent-p (setq-local niwa--agent-invocation t)))
                    (when root-p (niwa-session-set-root-buffer session buffer))
                    buffer))
                (ours ()
                  (seq-filter (lambda (workspace) (memq workspace (list first second)))
                              (niwa-journal-idle--live-workspaces))))
      (unwind-protect
          (progn
            (session-buffer first t)
            (session-buffer first t)
            (session-buffer second)
            (session-buffer second t t)
            (should (equal (list first) (ours)))
            (session-buffer second t)
            (should (= 2 (length (ours))))
            (should (memq second (ours))))
        (mapc #'kill-buffer buffers)
        (delete-directory first-root t)
        (delete-directory second-root t)))))

(niwa-deftest niwa-journal-idle--maintain
  (:vars* ((root (make-temp-file "niwa-journal-idle-" t))
           (local (niwa-workspace--create :root root))
           (remote (niwa-workspace--create :root "/ssh:idle-host:/srv/project/"))
           (niwa-journal-idle--inhibit-scheduling nil)
           calls)
   :after-each ((delete-directory root t)))
  (cl-letf (((symbol-function 'niwa-journal-idle--live-workspaces)
             (lambda () (list local remote))))
    ,test)
  (test)
  :doc "local workspaces get processing with recovery, a review offer and cleanup; remote ones none"
  (progn
    (niwa-test-journal-idle--recording calls
      (niwa-journal-idle--maintain))
    (should (equal (list (list 'process local t) (list 'pass local) (list 'cleanup local))
                   (reverse calls)))
    (should-not niwa-journal-idle--quiet-timer))

  :doc "inhibited maintenance queues nothing"
  (let ((niwa-journal-idle--inhibit-scheduling t))
    (niwa-test-journal-idle--recording calls
      (niwa-journal-idle--maintain))
    (should-not calls))

  :doc "a scheduling failure is reported as a warning"
  (let (warning)
    (cl-letf (((symbol-function 'niwa-journal-process-schedule)
               (lambda (&rest _) (error "Queue unavailable"))))
      (niwa-test--with-captured-diagnostics warning
        (niwa-journal-idle--maintain)))
    (should (string-match-p "Journal maintenance failed: Queue unavailable" warning))))

(niwa-deftest niwa-journal-idle--maintenance-due
  (:vars* ((niwa-journal-idle--inhibit-scheduling nil)
           (niwa-journal-idle-maintenance-minutes 10)
           workspaces)
   :after-each ((niwa-journal-idle-stop-all)))
  (cl-letf (((symbol-function 'niwa-journal-idle--live-workspaces)
             (lambda () workspaces)))
    ,test)
  (test)
  :doc "with a live root session it waits for quiet input and arms the next opportunity"
  (progn
    (setq workspaces (list 'workspace))
    (niwa-journal-idle--maintenance-due)
    (should (memq niwa-journal-idle--quiet-timer timer-idle-list))
    (should (eq #'niwa-journal-idle--maintain (timer--function niwa-journal-idle--quiet-timer)))
    (should (memq niwa-journal-idle--maintenance-timer timer-list))
    (let ((quiet niwa-journal-idle--quiet-timer))
      (niwa-journal-idle--maintenance-due)
      (should (eq quiet niwa-journal-idle--quiet-timer))))

  :doc "without root sessions the timer stops"
  (progn
    (niwa-journal-idle--maintenance-due)
    (should-not niwa-journal-idle--quiet-timer)
    (should-not niwa-journal-idle--maintenance-timer))

  :doc "disabling maintenance stops the timer"
  (let ((niwa-journal-idle-maintenance-minutes nil))
    (setq workspaces (list 'workspace))
    (niwa-journal-idle--maintenance-due)
    (should-not niwa-journal-idle--quiet-timer)
    (should-not niwa-journal-idle--maintenance-timer)))

(niwa-deftest niwa-journal-idle-ensure-maintenance
  (:vars* ((niwa-journal-idle--inhibit-scheduling nil)
           (niwa-journal-idle-maintenance-minutes 10))
   :after-each ((niwa-journal-idle-stop-all)))
  ,test
  (test)
  :doc "arms one maintenance timer at the configured interval"
  (progn
    (niwa-journal-idle-ensure-maintenance)
    (let ((timer niwa-journal-idle--maintenance-timer))
      (should (memq timer timer-list))
      (should (< 590 (float-time (time-subtract (timer--time timer) nil)) 601))
      (niwa-journal-idle-ensure-maintenance)
      (should (eq timer niwa-journal-idle--maintenance-timer))
      (should (memq #'niwa-journal-idle-stop-all kill-emacs-hook))))
  :doc "disabled or inhibited maintenance arms nothing"
  (progn
    (let ((niwa-journal-idle-maintenance-minutes nil))
      (niwa-journal-idle-ensure-maintenance))
    (let ((niwa-journal-idle--inhibit-scheduling t))
      (niwa-journal-idle-ensure-maintenance))
    (should-not niwa-journal-idle--maintenance-timer)))

(niwa-deftest niwa-journal-idle-session-opened
  (:vars* ((root (make-temp-file "niwa-journal-idle-" t))
           (workspace (niwa-workspace--create :root root))
           (niwa-journal-idle--inhibit-scheduling nil)
           (niwa-journal-idle-maintenance-minutes 10)
           calls)
   :after-each ((niwa-journal-idle-stop-all)
                (delete-directory root t)))
  (with-temp-buffer
    (setq-local niwa--session (niwa-session--create :workspace workspace))
    ,test)
  (test)
  :doc "opening a session queues recovery, processing and a review offer without running them"
  (progn
    (should (memq #'niwa-journal-idle-session-opened
                  (default-value 'niwa-session-start-hook)))
    (niwa-test-journal-idle--recording calls
      (niwa-journal-idle-session-opened))
    (should (equal (list (list 'process workspace t) (list 'pass workspace)) (reverse calls)))
    (should (memq niwa-journal-idle--maintenance-timer timer-list)))

  :doc "an agent buffer or inhibited scheduling queues nothing"
  (progn
    (let ((niwa-journal-idle--inhibit-scheduling t))
      (niwa-test-journal-idle--recording calls
        (niwa-journal-idle-session-opened)))
    (setq-local niwa--agent-invocation t)
    (niwa-test-journal-idle--recording calls
      (niwa-journal-idle-session-opened))
    (should-not calls)
    (should-not niwa-journal-idle--maintenance-timer)))

(niwa-deftest niwa-journal-idle-stop-all
  (:vars* ((niwa-journal-idle--inhibit-scheduling nil)
           (niwa-journal-seal-idle-minutes 20)
           (niwa-journal-idle-maintenance-minutes 10)))
  ,test
  (test)
  :doc "cancels armed and due seals and both maintenance timers"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-session-set-root-buffer session buffer)
     (niwa-journal-idle-arm-seal session buffer)
     (let ((seal (gethash session niwa-journal-idle--seal-timers))
           (maintenance niwa-journal-idle--maintenance-timer))
       (cl-letf (((symbol-function 'niwa-journal-idle--live-workspaces)
                  (lambda () (list (niwa-session-workspace session)))))
         (niwa-journal-idle--maintenance-due))
       (let ((quiet niwa-journal-idle--quiet-timer)
             (next niwa-journal-idle--maintenance-timer))
         (niwa-journal-idle--seal-due session buffer (niwa-session-turn-count session))
         (should (= 1 (hash-table-count niwa-journal-idle--seal-pending)))
         (niwa-journal-idle-stop-all)
         (should (= 0 (hash-table-count niwa-journal-idle--seal-timers)))
         (should (= 0 (hash-table-count niwa-journal-idle--seal-pending)))
         (should-not (gethash (list 'journal-idle-seal session) niwa-transport--pending))
         (dolist (timer (list seal maintenance next))
           (should-not (memq timer timer-list)))
         (should-not (memq quiet timer-idle-list))
         (should-not niwa-journal-idle--maintenance-timer)
         (should-not niwa-journal-idle--quiet-timer))))))

(provide 'test-niwa-journal-idle)
;;; test-niwa-journal-idle.el ends here
