;;; test-mevedel-memory-pass-retry.el --- Deferred automatic decisions -*- lexical-binding: t -*-

;;; Commentary:
;; Real retained decisions and timer callbacks must keep their originating policy.

;;; Code:

(require 'memory-history-fixtures
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "memory-history-fixtures"))
(require 'mevedel-memory-pass)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-write)
(require 'mevedel-journal-claim)
(require 'mevedel-journal-worker)
(require 'mevedel-resource)
(require 'mevedel-session-control-fs)
(require 'mevedel-workspace-identity)

(defvar mevedel-test-retry--reader nil
  "Buffer of an open session that uses the fixture's held note.")

(defun mevedel-test-retry-use (proposal)
  "Make an open session use PROPOSAL's shared note through the resource path."
  (with-current-buffer mevedel-test-retry--reader
    (mevedel-resource-prepare 'read (mevedel-memory-notes-address (plist-get proposal :file))
                              (list :session mevedel--session))))

(defun mevedel-test-retry-release ()
  "Close the session that used the fixture's held note."
  (with-current-buffer mevedel-test-retry--reader
    (kill-local-variable 'mevedel--session)))

(defun mevedel-test-retry-wait (predicate &optional diagnostics)
  "Drive native child callbacks until PREDICATE or a bounded test deadline.
DIAGNOSTICS returns bounded details when the expected callback never arrives."
  (let ((deadline (+ (float-time) 30)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil .01)))
  (unless (funcall predicate)
    (ert-fail (list :missing-callback (and diagnostics (funcall diagnostics))))))

(defmacro mevedel-test-retry-fixture (&rest body)
  "Run BODY with real pending decisions, original controller and unrelated buffer."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-retry-" t))
          (workspace (mevedel-workspace--create :root root))
          (mevedel-memory-dirs nil)
          (mevedel-memory-consolidation-mode 'propose)
          (mevedel-journal-cleanup--inhibit-scheduling t)
          (mevedel-memory-pass--deferred (make-hash-table :test #'eq))
          (origin (generate-new-buffer " *retry-controller*"))
          (unrelated (generate-new-buffer " *retry-unrelated*"))
          (mevedel-test-retry--reader (generate-new-buffer " *retry-reader*"))
          (_identity (mevedel-workspace-identity-ensure root))
          (accepted (with-current-buffer origin
                      (setq-local mevedel-memory-consolidation-mode 'auto)
                      (setq-local mevedel--session (mevedel-session--create :workspace workspace))
                      (with-current-buffer mevedel-test-retry--reader
                        (setq-local mevedel--session (mevedel-session--create :workspace workspace)))
                      (mevedel-test-memory-history-create workspace 1 1 100 t)))
          (accepted (car accepted))
          (proposal (car (plist-get accepted :proposals)))
          (path (plist-get (cdr (car (plist-get proposal :before))) :path))
          (key (mevedel-memory-store--claim-directory workspace))
          (state (list :workspace workspace :origin-buffer origin :settled t
                       :claim (plist-get (plist-get accepted :prepared) :claim)))
          (schedule (symbol-function 'run-with-timer))
          retry-timer result warnings)
     (unwind-protect
         (mevedel-test--with-idle-application
          (cl-letf (((symbol-function 'run-with-timer)
                    (lambda (seconds repeat function &rest args)
                      (let ((timer (apply schedule seconds repeat function args)))
                        (when (eq function #'mevedel-memory-pass--retry-notes)
                          (setq retry-timer timer))
                        timer)))
                   ((symbol-function 'display-warning)
                    (lambda (_type text &rest _) (push text warnings))))
           (puthash key state mevedel-memory-pass--running)
           ,@body))
       (when retry-timer (cancel-timer retry-timer))
       (when-let* ((running (gethash key mevedel-memory-pass--running))
                   (job (plist-get running :application)))
         (plist-put job :done t)
         (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer))))
       (when-let* ((operation (gethash key mevedel-memory-decision--operations)))
         (mevedel-memory-decision--operation-stop operation))
       (remhash key mevedel-memory-pass--running)
       (when (buffer-live-p origin) (kill-buffer origin))
       (kill-buffer mevedel-test-retry--reader)
       (kill-buffer unrelated)
       (delete-directory root t))))

(defun mevedel-test-retry-defer (state accepted)
  "Defer real ACCEPTED proposals in STATE, then end the session's use of the note."
  (let ((origin (plist-get state :origin-buffer)) result)
    (ignore origin)
    (mevedel-test-retry-use (car (plist-get accepted :proposals)))
    (mevedel-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
    (mevedel-test-retry-wait (lambda () result))
    (should (= 1 (plist-get result :held-count)))
    (mevedel-test-retry-release)
    (remhash (mevedel-memory-store--claim-directory (plist-get state :workspace)) mevedel-memory-pass--running)))

(mevedel-deftest mevedel-memory-pass--retry-notes ()
  ,test
  (test)
  :doc "deferred callback uses controller-local auto even with unrelated propose buffer current"
  (mevedel-test-retry-fixture
    (mevedel-test-retry-use proposal)
    (mevedel-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
    (mevedel-test-retry-wait (lambda () result))
    (should (= 1 (plist-get result :held-count)))
    (should retry-timer)
    (should (file-exists-p path))
    (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id)))
    (mevedel-test-retry-release)
    (remhash key mevedel-memory-pass--running)
    (with-current-buffer unrelated
      (should (eq 'propose mevedel-memory-consolidation-mode))
      (apply (timer--function retry-timer) (timer--args retry-timer)))
    (mevedel-test-retry-wait
     (lambda () (not (gethash key mevedel-memory-pass--running)))
     (lambda ()
       (let* ((running (gethash key mevedel-memory-pass--running))
              (job (plist-get running :application))
              (operation (gethash key mevedel-memory-decision--operations)))
         (list :done (plist-get job :done) :error (plist-get job :error)
               :pending-loaded (plist-get job :pending-loaded) :items (length (plist-get job :items))
               :operation (and operation t) :warnings warnings))))
    (should-not (file-exists-p path))
    (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status)))
    (should-not warnings)
    (should-not (gethash workspace mevedel-memory-pass--deferred))
    (should-not (gethash key mevedel-memory-decision--operations)))

  :doc "invalid origin mode lifetime session workspace or fresh identity cannot inherit unrelated auto policy"
  (dolist (change '(mode killed session workspace identity))
    (mevedel-test-retry-fixture
      (mevedel-test-retry-defer state accepted)
      (with-current-buffer unrelated (setq-local mevedel-memory-consolidation-mode 'auto))
      (let ((identity-file (mevedel-workspace-identity--path root))
            (identity (mevedel-workspace-identity-read root)))
        (pcase change
          ('mode (with-current-buffer origin (setq mevedel-memory-consolidation-mode 'propose)))
          ('killed (kill-buffer origin))
          ('session (with-current-buffer origin
                      (setq mevedel--session (mevedel-session--create :workspace workspace))))
          ('workspace (setf (mevedel-session-workspace (buffer-local-value 'mevedel--session origin))
                            (mevedel-workspace--create :root root)))
          ('identity (write-region (concat (make-string 64 (if (eq (aref identity 0) ?a) ?b ?a)) "\n")
                                   nil identity-file nil 'silent)))
        (with-current-buffer unrelated
          (apply (timer--function retry-timer) (timer--args retry-timer)))
        (should (file-exists-p path))
        (should-not warnings)
        (should-not (gethash workspace mevedel-memory-pass--deferred))
        (should-not (gethash key mevedel-memory-pass--running))
        (should-not (gethash key mevedel-memory-decision--operations))
        (when (eq change 'identity) (write-region (concat identity "\n") nil identity-file nil 'silent))
        (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id))))))

  :doc "explicit cancellation retires deferred timer and stale delivery cannot restart pending work"
  (mevedel-test-retry-fixture
    (mevedel-test-retry-defer state accepted)
    (should (mevedel-memory-pass-cancel workspace))
    (should-not (mevedel-memory-pass-cancel workspace))
    (with-current-buffer unrelated
      (setq-local mevedel-memory-consolidation-mode 'auto)
      (apply (timer--function retry-timer) (timer--args retry-timer)))
    (should (file-exists-p path))
    (should-not warnings)
    (should-not (gethash workspace mevedel-memory-pass--deferred))
    (should-not (gethash key mevedel-memory-pass--running))
    (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id))))

  :doc "synchronous recovery fallback still restores controller-local auto without a worker"
  (mevedel-test-retry-fixture
    (mevedel-test-retry-defer state accepted)
    (cl-letf (((symbol-value 'mevedel-journal-worker--child-p) t)
              ((symbol-function 'mevedel-journal-worker-start)
               (lambda (&rest _) (ert-fail "Fallback launched a child"))))
      (with-current-buffer unrelated
        (apply (timer--function retry-timer) (timer--args retry-timer)))
      (mevedel-test-retry-wait (lambda () (not (gethash key mevedel-memory-pass--running)))))
    (should-not (file-exists-p path))
    (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status)))
    (should-not warnings))

  :doc "mode invalidation during pending discovery or item continuation holds quietly and fences real work"
  (dolist (stage '(1 2))
    (mevedel-test-retry-fixture
      (mevedel-test-retry-defer state accepted)
      (let ((start (symbol-function 'mevedel-journal-worker-start))
            (ready (symbol-function 'mevedel-memory-decision--operation-ready))
            (launches 0) (continuations 0) operation)
        (cl-letf (((symbol-function 'mevedel-journal-worker-start)
                   (lambda (&rest args)
                     (let ((child (apply start args)))
                       (cl-incf launches)
                       (if (= launches 1)
                           (should (plist-get (nth 4 args) :pending-selection))
                         (should (plist-get (nth 4 args) :application))
                         (should (equal (plist-get proposal :id) (plist-get (nth 4 args) :proposal-id))))
                       (when (= stage 1)
                         (setq operation (gethash key mevedel-memory-decision--operations))
                         (with-current-buffer origin (setq mevedel-memory-consolidation-mode 'propose)))
                       child)))
                  ((symbol-function 'mevedel-memory-decision--operation-ready)
                   (lambda (job reply)
                     (cl-incf continuations)
                     (when (and (= stage 2) (= continuations 2))
                       ;; The adjacent claim skips redundant recovery, but still
                       ;; needs selected-decision discovery in a second child.
                       ;; Invalidate its actor before editor admission.
                       (should (plist-get job :recovered))
                       (should-not (plist-get job :recovery-started-at))
                       (setq operation job)
                       (with-current-buffer origin (setq mevedel-memory-consolidation-mode 'propose)))
                     (funcall ready job reply))))
          (with-current-buffer unrelated
            (apply (timer--function retry-timer) (timer--args retry-timer)))
          (mevedel-test-retry-wait (lambda () (not (gethash key mevedel-memory-pass--running)))))
        (should operation)
        (should (= stage launches))
        (should (= continuations stage))
        (should (mevedel-journal-claim-outcome (plist-get operation :claim)))
        (should-not (process-live-p (plist-get operation :worker)))
        (should-not warnings)
        (should (file-exists-p path))
        (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id)))
        (should-not (gethash workspace mevedel-memory-pass--deferred)))))

  :doc "fallback mode change after real intent preparation preserves pending decision at final write gate"
  (mevedel-test-retry-fixture
    (mevedel-test-retry-defer state accepted)
    (let ((commit (symbol-function 'mevedel-memory-notes-commit)) injected)
      (cl-letf (((symbol-value 'mevedel-journal-worker--child-p) t)
                ((symbol-function 'mevedel-memory-notes-commit)
                 (lambda (&rest args)
                   (unless injected
                     (setq injected t)
                     (with-current-buffer origin (setq mevedel-memory-consolidation-mode 'propose)))
                   (apply commit args))))
        (with-current-buffer unrelated
          (apply (timer--function retry-timer) (timer--args retry-timer)))
        (mevedel-test-retry-wait (lambda () (not (gethash key mevedel-memory-pass--running)))))
      (should injected)
      (should (file-exists-p path))
      (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id)))
      (should-not warnings)
      (should-not (gethash key mevedel-memory-decision--operations))
      (should-not (gethash workspace mevedel-memory-pass--deferred))))

  :doc "explicit cancellation while native retry child runs is quiet and leaves pending bytes fenced"
  (mevedel-test-retry-fixture
    (mevedel-test-retry-defer state accepted)
    (let ((start (symbol-function 'mevedel-journal-worker-start)) operation)
      (cl-letf (((symbol-function 'mevedel-journal-worker-start)
                 (lambda (&rest args)
                   (let ((child (apply start args)))
                     (setq operation (gethash key mevedel-memory-decision--operations))
                     (signal-process child 'SIGSTOP)
                     (run-at-time .01 nil #'mevedel-memory-pass-cancel workspace)
                     child))))
        (with-current-buffer unrelated
          (apply (timer--function retry-timer) (timer--args retry-timer)))
        (mevedel-test-retry-wait (lambda () (not (gethash key mevedel-memory-pass--running)))))
      (should operation)
      (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get operation :claim)) :status)))
      (should-not (process-live-p (plist-get operation :worker)))
      (should (file-exists-p path))
      (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id)))
      (should-not warnings)
      (should-not (gethash workspace mevedel-memory-pass--deferred))))

  :doc "genuine retry worker IO failure retains an actionable diagnostic and pending decision"
  (mevedel-test-retry-fixture
    (mevedel-test-retry-defer state accepted)
    (let (injected)
      (cl-letf (((symbol-function 'mevedel-journal-worker-start)
                 (lambda (&rest _) (setq injected t) (error "Injected retry worker IO failure"))))
        (with-current-buffer unrelated
          (apply (timer--function retry-timer) (timer--args retry-timer)))
        (mevedel-test-retry-wait (lambda () (not (gethash key mevedel-memory-pass--running)))))
      (should injected)
      (should (seq-some (lambda (text) (string-match-p "Injected retry worker IO failure" text)) warnings))
      (should (file-exists-p path))
      (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id)))
      (should-not (gethash key mevedel-memory-decision--operations))))

  :doc "IO error or C-g while retiring a held intent stays actionable with original marker retained"
  (dolist (failure '(io quit))
    (mevedel-test-retry-fixture
      (mevedel-test-retry-defer state accepted)
      (let ((commit (symbol-function 'mevedel-memory-notes-commit))
            (run (symbol-function 'mevedel-session-control-fs-run-program))
            (apply-async (symbol-function 'mevedel-memory-pass--apply-async))
            (abandon (symbol-function 'mevedel-memory-write--abandon))
            held injected intent owner)
        (cl-letf (((symbol-value 'mevedel-journal-worker--child-p) t)
                  ((symbol-function 'mevedel-memory-notes-commit)
                   (lambda (&rest args)
                     (unless held
                       (setq held t)
                       (with-current-buffer origin (setq mevedel-memory-consolidation-mode 'propose)))
                     (apply commit args)))
                  ((symbol-function 'mevedel-memory-write--abandon)
                   (lambda (workspace claim target value)
                     (setq intent value owner claim)
                     (funcall abandon workspace claim target value)))
                  ((symbol-function 'mevedel-session-control-fs-run-program)
                   (lambda (program &optional directory)
                     (when (and intent (seq-some (lambda (op) (and (eq (plist-get op :op) 'delete-file)
                                                                 (equal (mevedel-memory-write--pin intent) (plist-get op :path)))) program))
                       (setq injected t)
                       (if (eq failure 'quit) (signal 'quit nil) (error "Injected retirement IO failure")))
                     (funcall run program directory)))
                  ((symbol-function 'mevedel-memory-pass--apply-async)
                   (lambda (state accepted callback)
                     (funcall apply-async state accepted (lambda (value) (setq result value) (funcall callback value))))))
          (with-current-buffer unrelated
            (apply (timer--function retry-timer) (timer--args retry-timer)))
          (mevedel-test-retry-wait (lambda () (not (gethash key mevedel-memory-pass--running)))))
        (should held)
        (should injected)
        (should (= 1 (plist-get result :unapplied-count)))
        (should (eq (eq failure 'quit) (plist-get result :cancelled)))
        (should (string-match-p "Held memory write retirement failed" (plist-get result :error)))
        (should (seq-some (lambda (text) (string-match-p "Held memory write retirement failed" text)) warnings))
        (should (file-exists-p (mevedel-memory-write--pin intent)))
        (should (eq 'before (mevedel-memory-write--state intent)))
        (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome owner) :status)))
        (should (file-exists-p path))
        (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id)))
        (should-not (gethash key mevedel-memory-decision--operations)))))

  :doc "origin invalidation during fresh identity IO cannot leave a running retry or warning"
  (dolist (change '(killed mode session))
    (mevedel-test-retry-fixture
      (mevedel-test-retry-defer state accepted)
      (let ((read (symbol-function 'mevedel-workspace-identity-read)) injected)
        (cl-letf (((symbol-function 'mevedel-workspace-identity-read)
                   (lambda (directory)
                     (prog1 (funcall read directory)
                       (unless injected
                         (setq injected t)
                         (pcase change
                           ('killed (kill-buffer origin))
                           ('mode (with-current-buffer origin (setq mevedel-memory-consolidation-mode 'propose)))
                           ('session (with-current-buffer origin
                                       (setq mevedel--session (mevedel-session--create :workspace workspace))))))))))
          (with-current-buffer unrelated
            (apply (timer--function retry-timer) (timer--args retry-timer))))
        (should injected)
        (should-not warnings)
        (should-not (gethash key mevedel-memory-pass--running))
        (should-not (gethash workspace mevedel-memory-pass--deferred))
        (should-not (gethash key mevedel-memory-decision--operations))
        (should (file-exists-p path))
        (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id))))))

  :doc "retry warns on genuine cleanup failure even when its authenticated item was applied"
  (mevedel-test-retry-fixture
    (mevedel-test-retry-defer state accepted)
    (let ((apply-owned (symbol-function 'mevedel-memory-decision--apply-owned))
          (settle (symbol-function 'mevedel-journal-claim-settle)) job injected)
      (cl-letf (((symbol-function 'mevedel-memory-decision--apply-owned)
                 (lambda (&rest args)
                   (setq job (gethash key mevedel-memory-decision--operations))
                   (prog1 (apply apply-owned args)
                     (mevedel-memory-decision--operation-stop job))))
                ((symbol-function 'mevedel-journal-claim-settle)
                 (lambda (token &rest args)
                   (when (and job (not injected) (plist-get job :done) (eq token (plist-get job :claim)))
                     (setq injected t)
                     (error "Injected retry applied cleanup failure"))
                   (apply settle token args))))
        (with-current-buffer unrelated
          (apply (timer--function retry-timer) (timer--args retry-timer)))
        (mevedel-test-retry-wait (lambda () (not (gethash key mevedel-memory-pass--running)))))
      (should injected)
      (should (seq-some (lambda (text) (string-match-p "Injected retry applied cleanup failure" text)) warnings))
      (should-not (seq-some (lambda (text) (string-match-p "were not applied" text)) warnings))
      (should-not (file-exists-p path))
      (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status)))
      (should-not (gethash workspace mevedel-memory-pass--deferred))
      (should-not (gethash key mevedel-memory-decision--operations))))

  :doc "stale old timer cannot consume a newer retry for the same workspace"
  (mevedel-test-retry-fixture
    (mevedel-test-retry-defer state accepted)
    (let ((old retry-timer))
      (should (mevedel-memory-pass-cancel workspace))
      (mevedel-memory-pass--defer-notes state)
      (let ((current (gethash workspace mevedel-memory-pass--deferred)))
        (with-current-buffer unrelated (apply (timer--function old) (timer--args old)))
        (should (eq current (gethash workspace mevedel-memory-pass--deferred)))
        (should-not (gethash key mevedel-memory-pass--running))
        (should (file-exists-p path))
        (should-not warnings)))))

(provide 'test-mevedel-memory-pass-retry)
;;; test-mevedel-memory-pass-retry.el ends here
