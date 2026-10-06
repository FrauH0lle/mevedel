;;; test-mevedel-journal-idle.el -- Idle journal sealing and maintenance tests -*- lexical-binding: t -*-

;;; Commentary:

;; Real sessions, captures and timers; the downstream schedulers are recorded
;; where a case asserts which opportunities were queued.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))
(require 'mevedel-journal-idle)

(defvar mevedel--agent-invocation)

(defun mevedel-test-journal-idle--drain ()
  "Wait until due idle seals have left their transport queue."
  (with-timeout (5 (ert-fail "Idle seal did not run"))
    (while (> (hash-table-count mevedel-journal-idle--seal-pending) 0)
      (accept-process-output nil 0.01))))

(defmacro mevedel-test-journal-idle--recording (calls &rest body)
  "Run BODY recording downstream scheduler calls into CALLS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'mevedel-journal-process-schedule)
              (lambda (workspace &optional recover)
                (push (list 'process workspace recover) ,calls)))
             ((symbol-function 'mevedel-memory-pass-schedule)
              (lambda (workspace) (push (list 'pass workspace) ,calls)))
             ((symbol-function 'mevedel-journal-cleanup-schedule)
              (lambda (workspace &optional _force) (push (list 'cleanup workspace) ,calls))))
     ,@body))

(mevedel-deftest mevedel-journal-idle-arm-seal
  (:vars* ((mevedel-journal-idle--inhibit-scheduling nil)
           (mevedel-journal-seal-idle-minutes 20)
           (mevedel-journal-idle-maintenance-minutes 10))
   :after-each ((mevedel-journal-idle-stop-all)))
  ,test
  (test)
  :doc "each completed root turn restarts one seal period and starts maintenance"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-session-set-root-buffer session buffer)
     (mevedel-test-journal-capture--turn session buffer "First" "First result")
     (let ((first (gethash session mevedel-journal-idle--seal-timers)))
       (should (timerp first))
       (should (memq first timer-list))
       (should (memq mevedel-journal-idle--maintenance-timer timer-list))
       (mevedel-test-journal-capture--turn session buffer "Second" "Second result")
       (let ((second (gethash session mevedel-journal-idle--seal-timers)))
         (should (timerp second))
         (should-not (eq first second))
         (should-not (memq first timer-list))
         (should (equal (list session buffer 2) (timer--args second)))))))

  :doc "killing the root buffer cancels its armed seal"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-journal-idle-arm-seal session buffer)
     (let ((timer (gethash session mevedel-journal-idle--seal-timers)))
       (should (timerp timer))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (let ((mevedel-journal-enabled nil)) (kill-buffer buffer))
       (should-not (gethash session mevedel-journal-idle--seal-timers))
       (should-not (memq timer timer-list)))))

  :doc "disabled sealing or journaling arms no seal but still maintains the workspace"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (let ((mevedel-journal-seal-idle-minutes nil))
       (mevedel-journal-idle-arm-seal session buffer))
     (let ((mevedel-journal-enabled nil))
       (mevedel-journal-idle-arm-seal session buffer))
     (should (= 0 (hash-table-count mevedel-journal-idle--seal-timers)))
     (should (timerp mevedel-journal-idle--maintenance-timer))))

  :doc "inhibited scheduling arms nothing"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (let ((mevedel-journal-idle--inhibit-scheduling t))
       (mevedel-journal-idle-arm-seal session buffer))
     (should (= 0 (hash-table-count mevedel-journal-idle--seal-timers)))
     (should-not mevedel-journal-idle--maintenance-timer))))

(mevedel-deftest mevedel-journal-idle--seal-due
  (:vars* ((mevedel-journal-idle--inhibit-scheduling nil)
           (mevedel-journal-seal-idle-minutes 20)
           (mevedel-journal-idle-maintenance-minutes nil))
   :after-each ((mevedel-journal-idle-stop-all)))
  ,test
  (test)
  :doc "a quiet session seals its ready checkpoint with the idle trigger and queues processing"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-session-set-root-buffer session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-list workspace)))
            calls)
       (should-not (mevedel-journal-capture-trigger workspace capture))
       (mevedel-test-journal-idle--recording calls
         (mevedel-journal-idle--seal-due session buffer (mevedel-session-turn-count session))
         (mevedel-test-journal-idle--drain))
       (should (eq 'idle (mevedel-journal-capture-trigger workspace capture)))
       (should (equal (list (list 'process workspace nil)) calls))
       (should-not (gethash session mevedel-journal-idle--seal-timers))
       ;; Later work forms a new checkpoint outside the sealed one.
       (mevedel-test-journal-capture--turn session buffer "Later" "Later result")
       (should (= 2 (length (mevedel-journal-capture-list workspace)))))))

  :doc "a newer completed turn leaves the stale period without sealing or re-arming"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-session-set-root-buffer session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let ((workspace (mevedel-session-workspace session)))
       (mevedel-journal-idle--seal-due session buffer (1- (mevedel-session-turn-count session)))
       (should (= 0 (hash-table-count mevedel-journal-idle--seal-pending)))
       (should-not (mevedel-journal-capture-trigger
                    workspace (car (mevedel-journal-capture-list workspace)))))))

  :doc "a running request restarts the period instead of sealing"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-session-set-root-buffer session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let ((workspace (mevedel-session-workspace session))
           (turn (mevedel-session-turn-count session)))
       (mevedel-journal-idle-stop-all)
       (with-current-buffer buffer (setq-local mevedel--current-request 'running))
       (unwind-protect
           (progn
             (mevedel-journal-idle--seal-due session buffer turn)
             (should (= 0 (hash-table-count mevedel-journal-idle--seal-pending)))
             (should (equal (list session buffer turn)
                            (timer--args (gethash session mevedel-journal-idle--seal-timers))))
             (should-not (mevedel-journal-capture-trigger
                          workspace (car (mevedel-journal-capture-list workspace)))))
         (with-current-buffer buffer (setq-local mevedel--current-request nil)))))))

(mevedel-deftest mevedel-journal-idle--quiet-p ()
  ,test
  (test)
  :doc "requires the live root buffer, no running request and an unchanged turn"
  (let* ((root (make-temp-file "mevedel-journal-idle-" t))
         (session (mevedel-session--create :workspace (mevedel-workspace--create :root root)
                                           :turn-count 3))
         (buffer (generate-new-buffer " *journal-idle-quiet*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer (setq-local mevedel--session session))
          (should-not (mevedel-journal-idle--quiet-p session buffer 3))
          (mevedel-session-set-root-buffer session buffer)
          (should (mevedel-journal-idle--quiet-p session buffer 3))
          (should-not (mevedel-journal-idle--quiet-p session buffer 2))
          (with-current-buffer buffer (setq-local mevedel--current-request 'running))
          (should-not (mevedel-journal-idle--quiet-p session buffer 3))
          (kill-buffer buffer)
          (should-not (mevedel-journal-idle--quiet-p session buffer 3)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-idle--live-workspaces ()
  ,test
  (test)
  :doc "returns each workspace of a live root session once, ignoring other session buffers"
  (let* ((first-root (make-temp-file "mevedel-journal-idle-a-" t))
         (second-root (make-temp-file "mevedel-journal-idle-b-" t))
         (first (mevedel-workspace--create :root first-root))
         (second (mevedel-workspace--create :root second-root))
         buffers)
    (cl-labels ((session-buffer (workspace &optional root-p agent-p)
                  (let ((buffer (generate-new-buffer " *journal-idle-session*"))
                        (session (mevedel-session--create :workspace workspace)))
                    (push buffer buffers)
                    (with-current-buffer buffer
                      (setq-local mevedel--session session)
                      (when agent-p (setq-local mevedel--agent-invocation t)))
                    (when root-p (mevedel-session-set-root-buffer session buffer))
                    buffer))
                (ours ()
                  (seq-filter (lambda (workspace) (memq workspace (list first second)))
                              (mevedel-journal-idle--live-workspaces))))
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

(mevedel-deftest mevedel-journal-idle--maintain
  (:vars* ((root (make-temp-file "mevedel-journal-idle-" t))
           (local (mevedel-workspace--create :root root))
           (remote (mevedel-workspace--create :root "/ssh:idle-host:/srv/project/"))
           (mevedel-journal-idle--inhibit-scheduling nil)
           calls)
   :after-each ((delete-directory root t)))
  (cl-letf (((symbol-function 'mevedel-journal-idle--live-workspaces)
             (lambda () (list local remote))))
    ,test)
  (test)
  :doc "local workspaces get processing with recovery, a review offer and cleanup; remote ones none"
  (progn
    (mevedel-test-journal-idle--recording calls
      (mevedel-journal-idle--maintain))
    (should (equal (list (list 'process local t) (list 'pass local) (list 'cleanup local))
                   (reverse calls)))
    (should-not mevedel-journal-idle--quiet-timer))

  :doc "inhibited maintenance queues nothing"
  (let ((mevedel-journal-idle--inhibit-scheduling t))
    (mevedel-test-journal-idle--recording calls
      (mevedel-journal-idle--maintain))
    (should-not calls))

  :doc "a scheduling failure is reported as a warning"
  (let (warning)
    (cl-letf (((symbol-function 'mevedel-journal-process-schedule)
               (lambda (&rest _) (error "Queue unavailable"))))
      (mevedel-test--with-captured-diagnostics warning
        (mevedel-journal-idle--maintain)))
    (should (string-match-p "Journal maintenance failed: Queue unavailable" warning))))

(mevedel-deftest mevedel-journal-idle--maintenance-due
  (:vars* ((mevedel-journal-idle--inhibit-scheduling nil)
           (mevedel-journal-idle-maintenance-minutes 10)
           workspaces)
   :after-each ((mevedel-journal-idle-stop-all)))
  (cl-letf (((symbol-function 'mevedel-journal-idle--live-workspaces)
             (lambda () workspaces)))
    ,test)
  (test)
  :doc "with a live root session it waits for quiet input and arms the next opportunity"
  (progn
    (setq workspaces (list 'workspace))
    (mevedel-journal-idle--maintenance-due)
    (should (memq mevedel-journal-idle--quiet-timer timer-idle-list))
    (should (eq #'mevedel-journal-idle--maintain (timer--function mevedel-journal-idle--quiet-timer)))
    (should (memq mevedel-journal-idle--maintenance-timer timer-list))
    (let ((quiet mevedel-journal-idle--quiet-timer))
      (mevedel-journal-idle--maintenance-due)
      (should (eq quiet mevedel-journal-idle--quiet-timer))))

  :doc "without root sessions the timer stops"
  (progn
    (mevedel-journal-idle--maintenance-due)
    (should-not mevedel-journal-idle--quiet-timer)
    (should-not mevedel-journal-idle--maintenance-timer))

  :doc "disabling maintenance stops the timer"
  (let ((mevedel-journal-idle-maintenance-minutes nil))
    (setq workspaces (list 'workspace))
    (mevedel-journal-idle--maintenance-due)
    (should-not mevedel-journal-idle--quiet-timer)
    (should-not mevedel-journal-idle--maintenance-timer)))

(mevedel-deftest mevedel-journal-idle-ensure-maintenance
  (:vars* ((mevedel-journal-idle--inhibit-scheduling nil)
           (mevedel-journal-idle-maintenance-minutes 10))
   :after-each ((mevedel-journal-idle-stop-all)))
  ,test
  (test)
  :doc "arms one maintenance timer at the configured interval"
  (progn
    (mevedel-journal-idle-ensure-maintenance)
    (let ((timer mevedel-journal-idle--maintenance-timer))
      (should (memq timer timer-list))
      (should (< 590 (float-time (time-subtract (timer--time timer) nil)) 601))
      (mevedel-journal-idle-ensure-maintenance)
      (should (eq timer mevedel-journal-idle--maintenance-timer))
      (should (memq #'mevedel-journal-idle-stop-all kill-emacs-hook))))
  :doc "disabled or inhibited maintenance arms nothing"
  (progn
    (let ((mevedel-journal-idle-maintenance-minutes nil))
      (mevedel-journal-idle-ensure-maintenance))
    (let ((mevedel-journal-idle--inhibit-scheduling t))
      (mevedel-journal-idle-ensure-maintenance))
    (should-not mevedel-journal-idle--maintenance-timer)))

(mevedel-deftest mevedel-journal-idle-session-opened
  (:vars* ((root (make-temp-file "mevedel-journal-idle-" t))
           (workspace (mevedel-workspace--create :root root))
           (mevedel-journal-idle--inhibit-scheduling nil)
           (mevedel-journal-idle-maintenance-minutes 10)
           calls)
   :after-each ((mevedel-journal-idle-stop-all)
                (delete-directory root t)))
  (with-temp-buffer
    (setq-local mevedel--session (mevedel-session--create :workspace workspace))
    ,test)
  (test)
  :doc "opening a session queues recovery, processing and a review offer without running them"
  (progn
    (should (memq #'mevedel-journal-idle-session-opened
                  (default-value 'mevedel-session-start-hook)))
    (mevedel-test-journal-idle--recording calls
      (mevedel-journal-idle-session-opened))
    (should (equal (list (list 'process workspace t) (list 'pass workspace)) (reverse calls)))
    (should (memq mevedel-journal-idle--maintenance-timer timer-list)))

  :doc "an agent buffer or inhibited scheduling queues nothing"
  (progn
    (let ((mevedel-journal-idle--inhibit-scheduling t))
      (mevedel-test-journal-idle--recording calls
        (mevedel-journal-idle-session-opened)))
    (setq-local mevedel--agent-invocation t)
    (mevedel-test-journal-idle--recording calls
      (mevedel-journal-idle-session-opened))
    (should-not calls)
    (should-not mevedel-journal-idle--maintenance-timer)))

(mevedel-deftest mevedel-journal-idle-stop-all
  (:vars* ((mevedel-journal-idle--inhibit-scheduling nil)
           (mevedel-journal-seal-idle-minutes 20)
           (mevedel-journal-idle-maintenance-minutes 10)))
  ,test
  (test)
  :doc "cancels armed and due seals and both maintenance timers"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-session-set-root-buffer session buffer)
     (mevedel-journal-idle-arm-seal session buffer)
     (let ((seal (gethash session mevedel-journal-idle--seal-timers))
           (maintenance mevedel-journal-idle--maintenance-timer))
       (cl-letf (((symbol-function 'mevedel-journal-idle--live-workspaces)
                  (lambda () (list (mevedel-session-workspace session)))))
         (mevedel-journal-idle--maintenance-due))
       (let ((quiet mevedel-journal-idle--quiet-timer)
             (next mevedel-journal-idle--maintenance-timer))
         (mevedel-journal-idle--seal-due session buffer (mevedel-session-turn-count session))
         (should (= 1 (hash-table-count mevedel-journal-idle--seal-pending)))
         (mevedel-journal-idle-stop-all)
         (should (= 0 (hash-table-count mevedel-journal-idle--seal-timers)))
         (should (= 0 (hash-table-count mevedel-journal-idle--seal-pending)))
         (should-not (gethash (list 'journal-idle-seal session) mevedel-transport--pending))
         (dolist (timer (list seal maintenance next))
           (should-not (memq timer timer-list)))
         (should-not (memq quiet timer-idle-list))
         (should-not mevedel-journal-idle--maintenance-timer)
         (should-not mevedel-journal-idle--quiet-timer))))))

(provide 'test-mevedel-journal-idle)
;;; test-mevedel-journal-idle.el ends here
