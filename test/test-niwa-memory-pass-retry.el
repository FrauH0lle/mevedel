;;; test-niwa-memory-pass-retry.el --- Deferred automatic decisions -*- lexical-binding: t -*-

;;; Commentary:
;; Real retained decisions and timer callbacks must keep their originating policy.

;;; Code:

(require 'memory-history-fixtures
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "memory-history-fixtures"))
(require 'niwa-memory-pass)
(require 'niwa-memory-decision)
(require 'niwa-memory-notes)
(require 'niwa-memory-write)
(require 'niwa-journal-claim)
(require 'niwa-journal-worker)
(require 'niwa-resource)
(require 'niwa-session-control-fs)
(require 'niwa-workspace-identity)

(defvar niwa-test-retry--reader nil
  "Buffer of an open session that uses the fixture's held note.")

(defun niwa-test-retry-use (proposal)
  "Make an open session use PROPOSAL's shared note through the resource path."
  (with-current-buffer niwa-test-retry--reader
    (niwa-resource-prepare 'read (niwa-memory-notes-address (plist-get proposal :file))
                              (list :session niwa--session))))

(defun niwa-test-retry-release ()
  "Close the session that used the fixture's held note."
  (with-current-buffer niwa-test-retry--reader
    (kill-local-variable 'niwa--session)))

(defun niwa-test-retry-wait (predicate &optional diagnostics)
  "Drive native child callbacks until PREDICATE or a bounded test deadline.
DIAGNOSTICS returns bounded details when the expected callback never arrives."
  (let ((deadline (+ (float-time) 30)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (niwa-test--application-step)))
  (unless (funcall predicate)
    (ert-fail (list :missing-callback (and diagnostics (funcall diagnostics))))))

(defmacro niwa-test-retry-fixture (&rest body)
  "Run BODY with real pending decisions, original controller and unrelated buffer."
  (declare (indent 0) (debug t))
  `(let* ((root (niwa-test-memory-history-root 'retry-worker))
          (workspace (niwa-workspace--create :root root))
          (niwa-memory-dirs nil)
          (niwa-memory-consolidation-mode 'propose)
          (niwa-journal-cleanup--inhibit-scheduling t)
          (niwa-memory-pass--deferred (make-hash-table :test #'eq))
          (origin (generate-new-buffer " *retry-controller*"))
          (unrelated (generate-new-buffer " *retry-unrelated*"))
          (niwa-test-retry--reader (generate-new-buffer " *retry-reader*"))
          (_identity (niwa-workspace-identity-ensure root))
          (accepted (with-current-buffer origin
                      (setq-local niwa-memory-consolidation-mode 'auto)
                      (setq-local niwa--session (niwa-session--create :workspace workspace))
                      (with-current-buffer niwa-test-retry--reader
                        (setq-local niwa--session (niwa-session--create :workspace workspace)))
                      (niwa-test-memory-history-build
                       'retry-worker root
                       (lambda () (niwa-test-memory-history-create workspace 1 1 100 t)))))
          (accepted (car accepted))
          (proposal (car (plist-get accepted :proposals)))
          (path (plist-get (cdr (car (plist-get proposal :before))) :path))
          (key (niwa-memory-store--claim-directory workspace))
          (state (list :workspace workspace :origin-buffer origin :settled t
                       :claim (plist-get (plist-get accepted :prepared) :claim)))
          (schedule (symbol-function 'run-with-timer))
          retry-timer result warnings)
     (unwind-protect
         (niwa-test--with-idle-application
          (cl-letf (((symbol-function 'run-with-timer)
                    (lambda (seconds repeat function &rest args)
                      (let ((timer (apply schedule seconds repeat function args)))
                        (when (eq function #'niwa-memory-pass--retry-notes)
                          (setq retry-timer timer))
                        timer)))
                   ((symbol-function 'display-warning)
                    (lambda (_type text &rest _) (push text warnings))))
           (puthash key state niwa-memory-pass--running)
           ,@body))
       (when retry-timer (cancel-timer retry-timer))
       (when-let* ((running (gethash key niwa-memory-pass--running))
                   (job (plist-get running :application)))
         (plist-put job :done t)
         (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer))))
       (when-let* ((operation (gethash key niwa-memory-decision--operations)))
         (niwa-memory-decision--operation-stop operation))
       (remhash key niwa-memory-pass--running)
       (when (buffer-live-p origin) (kill-buffer origin))
       (kill-buffer niwa-test-retry--reader)
       (kill-buffer unrelated)
       (delete-directory root t))))

(defun niwa-test-retry-defer (state accepted)
  "Defer real ACCEPTED proposals in STATE, then end the session's use of the note."
  (let ((origin (plist-get state :origin-buffer)) result)
    (ignore origin)
    (niwa-test-retry-use (car (plist-get accepted :proposals)))
    (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
    (niwa-test-retry-wait (lambda () result))
    (should (= 1 (plist-get result :held-count)))
    (niwa-test-retry-release)
    (remhash (niwa-memory-store--claim-directory (plist-get state :workspace)) niwa-memory-pass--running)))

(niwa-deftest niwa-memory-pass--retry-notes ()
  ,test
  (test)
  :doc "deferred callback uses controller-local auto even with unrelated propose buffer current"
  (niwa-test-retry-fixture
    (niwa-test-retry-use proposal)
    (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
    (niwa-test-retry-wait (lambda () result))
    (should (= 1 (plist-get result :held-count)))
    (should retry-timer)
    (should (file-exists-p path))
    (should-not (niwa-memory-decision-status workspace (plist-get proposal :id)))
    (niwa-test-retry-release)
    (remhash key niwa-memory-pass--running)
    (with-current-buffer unrelated
      (should (eq 'propose niwa-memory-consolidation-mode))
      (apply (timer--function retry-timer) (timer--args retry-timer)))
    (niwa-test-retry-wait
     (lambda () (not (gethash key niwa-memory-pass--running)))
     (lambda ()
       (let* ((running (gethash key niwa-memory-pass--running))
              (job (plist-get running :application))
              (operation (gethash key niwa-memory-decision--operations)))
         (list :done (plist-get job :done) :error (plist-get job :error)
               :pending-loaded (plist-get job :pending-loaded) :items (length (plist-get job :items))
               :operation (and operation t) :warnings warnings))))
    (should-not (file-exists-p path))
    (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status)))
    (should-not warnings)
    (should-not (gethash workspace niwa-memory-pass--deferred))
    (should-not (gethash key niwa-memory-decision--operations)))

  :doc "invalid origin mode lifetime session workspace or fresh identity cannot inherit unrelated auto policy"
  (dolist (change '(mode killed session workspace identity))
    (niwa-test-retry-fixture
      (niwa-test-retry-defer state accepted)
      (with-current-buffer unrelated (setq-local niwa-memory-consolidation-mode 'auto))
      (let ((identity-file (niwa-workspace-identity--path root))
            (identity (niwa-workspace-identity-read root)))
        (pcase change
          ('mode (with-current-buffer origin (setq niwa-memory-consolidation-mode 'propose)))
          ('killed (kill-buffer origin))
          ('session (with-current-buffer origin
                      (setq niwa--session (niwa-session--create :workspace workspace))))
          ('workspace (setf (niwa-session-workspace (buffer-local-value 'niwa--session origin))
                            (niwa-workspace--create :root root)))
          ('identity (write-region (concat (make-string 64 (if (eq (aref identity 0) ?a) ?b ?a)) "\n")
                                   nil identity-file nil 'silent)))
        (with-current-buffer unrelated
          (apply (timer--function retry-timer) (timer--args retry-timer)))
        (should (file-exists-p path))
        (should-not warnings)
        (should-not (gethash workspace niwa-memory-pass--deferred))
        (should-not (gethash key niwa-memory-pass--running))
        (should-not (gethash key niwa-memory-decision--operations))
        (when (eq change 'identity) (write-region (concat identity "\n") nil identity-file nil 'silent))
        (should-not (niwa-memory-decision-status workspace (plist-get proposal :id))))))

  :doc "explicit cancellation retires deferred timer and stale delivery cannot restart pending work"
  (niwa-test-retry-fixture
    (niwa-test-retry-defer state accepted)
    (should (niwa-memory-pass-cancel workspace))
    (should-not (niwa-memory-pass-cancel workspace))
    (with-current-buffer unrelated
      (setq-local niwa-memory-consolidation-mode 'auto)
      (apply (timer--function retry-timer) (timer--args retry-timer)))
    (should (file-exists-p path))
    (should-not warnings)
    (should-not (gethash workspace niwa-memory-pass--deferred))
    (should-not (gethash key niwa-memory-pass--running))
    (should-not (niwa-memory-decision-status workspace (plist-get proposal :id))))

  :doc "synchronous recovery fallback still restores controller-local auto without a worker"
  (niwa-test-retry-fixture
    (niwa-test-retry-defer state accepted)
    (cl-letf (((symbol-value 'niwa-journal-worker--child-p) t)
              ((symbol-function 'niwa-journal-worker-start)
               (lambda (&rest _) (ert-fail "Fallback launched a child"))))
      (with-current-buffer unrelated
        (apply (timer--function retry-timer) (timer--args retry-timer)))
      (niwa-test-retry-wait (lambda () (not (gethash key niwa-memory-pass--running)))))
    (should-not (file-exists-p path))
    (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status)))
    (should-not warnings))

  :doc "mode invalidation during pending discovery or item continuation holds quietly and fences real work"
  (dolist (stage '(1 2))
    (niwa-test-retry-fixture
      (niwa-test-retry-defer state accepted)
      (let ((start (symbol-function 'niwa-journal-worker-start))
            (ready (symbol-function 'niwa-memory-decision--operation-ready))
            (launches 0) (continuations 0) operation)
        (cl-letf (((symbol-function 'niwa-journal-worker-start)
                   (lambda (&rest args)
                     (let ((child (apply start args)))
                       (cl-incf launches)
                       (if (= launches 1)
                           (should (plist-get (nth 2 args) :pending-selection))
                         (should (plist-get (nth 2 args) :application))
                         (should (equal (plist-get proposal :id) (plist-get (nth 2 args) :proposal-id))))
                       (when (= stage 1)
                         (setq operation (gethash key niwa-memory-decision--operations))
                         (with-current-buffer origin (setq niwa-memory-consolidation-mode 'propose)))
                       child)))
                  ((symbol-function 'niwa-memory-decision--operation-ready)
                   (lambda (job reply)
                     (cl-incf continuations)
                     (when (and (= stage 2) (= continuations 2))
                       ;; The adjacent claim skips redundant recovery, but still
                       ;; needs selected-decision discovery in a second child.
                       ;; Invalidate its actor before editor admission.
                       (should (plist-get job :recovered))
                       (should-not (plist-get job :recovery-started-at))
                       (setq operation job)
                       (with-current-buffer origin (setq niwa-memory-consolidation-mode 'propose)))
                     (funcall ready job reply))))
          (with-current-buffer unrelated
            (apply (timer--function retry-timer) (timer--args retry-timer)))
          (niwa-test-retry-wait (lambda () (not (gethash key niwa-memory-pass--running)))))
        (should operation)
        (should (= stage launches))
        (should (= continuations stage))
        (should (niwa-journal-claim-outcome (plist-get operation :claim)))
        (should-not (process-live-p (plist-get operation :worker)))
        (should-not warnings)
        (should (file-exists-p path))
        (should-not (niwa-memory-decision-status workspace (plist-get proposal :id)))
        (should-not (gethash workspace niwa-memory-pass--deferred)))))

  :doc "fallback mode change after real intent preparation preserves pending decision at final write gate"
  (niwa-test-retry-fixture
    (niwa-test-retry-defer state accepted)
    (let ((commit (symbol-function 'niwa-memory-notes-commit)) injected)
      (cl-letf (((symbol-value 'niwa-journal-worker--child-p) t)
                ((symbol-function 'niwa-memory-notes-commit)
                 (lambda (&rest args)
                   (unless injected
                     (setq injected t)
                     (with-current-buffer origin (setq niwa-memory-consolidation-mode 'propose)))
                   (apply commit args))))
        (with-current-buffer unrelated
          (apply (timer--function retry-timer) (timer--args retry-timer)))
        (niwa-test-retry-wait (lambda () (not (gethash key niwa-memory-pass--running)))))
      (should injected)
      (should (file-exists-p path))
      (should-not (niwa-memory-decision-status workspace (plist-get proposal :id)))
      (should-not warnings)
      (should-not (gethash key niwa-memory-decision--operations))
      (should-not (gethash workspace niwa-memory-pass--deferred))))

  :doc "explicit cancellation while native retry child runs is quiet and leaves pending bytes fenced"
  (niwa-test-retry-fixture
    (niwa-test-retry-defer state accepted)
    (let ((start (symbol-function 'niwa-journal-worker-start)) operation)
      (cl-letf (((symbol-function 'niwa-journal-worker-start)
                 (lambda (&rest args)
                   (let ((child (apply start args)))
                     (setq operation (gethash key niwa-memory-decision--operations))
                     (signal-process child 'SIGSTOP)
                     (run-at-time .01 nil #'niwa-memory-pass-cancel workspace)
                     child))))
        (with-current-buffer unrelated
          (apply (timer--function retry-timer) (timer--args retry-timer)))
        (niwa-test-retry-wait (lambda () (not (gethash key niwa-memory-pass--running)))))
      (should operation)
      (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome (plist-get operation :claim)) :status)))
      (should-not (process-live-p (plist-get operation :worker)))
      (should (file-exists-p path))
      (should-not (niwa-memory-decision-status workspace (plist-get proposal :id)))
      (should-not warnings)
      (should-not (gethash workspace niwa-memory-pass--deferred))))

  :doc "genuine retry worker IO failure retains an actionable diagnostic and pending decision"
  (niwa-test-retry-fixture
    (niwa-test-retry-defer state accepted)
    (let (injected)
      (cl-letf (((symbol-function 'niwa-journal-worker-start)
                 (lambda (&rest _) (setq injected t) (error "Injected retry worker IO failure"))))
        (with-current-buffer unrelated
          (apply (timer--function retry-timer) (timer--args retry-timer)))
        (niwa-test-retry-wait (lambda () (not (gethash key niwa-memory-pass--running)))))
      (should injected)
      (should (seq-some (lambda (text) (string-match-p "Injected retry worker IO failure" text)) warnings))
      (should (file-exists-p path))
      (should-not (niwa-memory-decision-status workspace (plist-get proposal :id)))
      (should-not (gethash key niwa-memory-decision--operations))))

  :doc "a late mode change before deferred deletion keeps the file, elects nothing and releases the operation"
  (niwa-test-retry-fixture
    (niwa-test-retry-defer state accepted)
    (let ((commit (symbol-function 'niwa-memory-notes-commit)) held)
      (cl-letf (((symbol-value 'niwa-journal-worker--child-p) t)
                ((symbol-function 'niwa-memory-notes-commit)
                 (lambda (&rest args)
                   (setq held t)
                   (with-current-buffer origin (setq niwa-memory-consolidation-mode 'propose))
                   (apply commit args))))
        (with-current-buffer unrelated (apply (timer--function retry-timer) (timer--args retry-timer)))
        (niwa-test-retry-wait (lambda () (not (gethash key niwa-memory-pass--running)))))
      (should held)
      (should (file-exists-p path))
      (should-not (niwa-memory-decision-status workspace (plist-get proposal :id)))
      (should-not (gethash key niwa-memory-decision--operations))))

  :doc "origin invalidation during fresh identity IO cannot leave a running retry or warning"
  (dolist (change '(killed mode session))
    (niwa-test-retry-fixture
      (niwa-test-retry-defer state accepted)
      (let ((read (symbol-function 'niwa-workspace-identity-read)) injected)
        (cl-letf (((symbol-function 'niwa-workspace-identity-read)
                   (lambda (directory)
                     (prog1 (funcall read directory)
                       (unless injected
                         (setq injected t)
                         (pcase change
                           ('killed (kill-buffer origin))
                           ('mode (with-current-buffer origin (setq niwa-memory-consolidation-mode 'propose)))
                           ('session (with-current-buffer origin
                                       (setq niwa--session (niwa-session--create :workspace workspace))))))))))
          (with-current-buffer unrelated
            (apply (timer--function retry-timer) (timer--args retry-timer))))
        (should injected)
        (should-not warnings)
        (should-not (gethash key niwa-memory-pass--running))
        (should-not (gethash workspace niwa-memory-pass--deferred))
        (should-not (gethash key niwa-memory-decision--operations))
        (should (file-exists-p path))
        (should-not (niwa-memory-decision-status workspace (plist-get proposal :id))))))

  :doc "retry warns on genuine cleanup failure even when its authenticated item was applied"
  (niwa-test-retry-fixture
    (niwa-test-retry-defer state accepted)
    (let ((apply-owned (symbol-function 'niwa-memory-decision--apply-owned))
          (settle (symbol-function 'niwa-journal-claim-settle)) job injected)
      (cl-letf (((symbol-function 'niwa-memory-decision--apply-owned)
                 (lambda (&rest args)
                   (setq job (gethash key niwa-memory-decision--operations))
                   (prog1 (apply apply-owned args)
                     (niwa-memory-decision--operation-stop job))))
                ((symbol-function 'niwa-journal-claim-settle)
                 (lambda (token &rest args)
                   (when (and job (not injected) (plist-get job :done) (eq token (plist-get job :claim)))
                     (setq injected t)
                     (error "Injected retry applied cleanup failure"))
                   (apply settle token args))))
        (with-current-buffer unrelated
          (apply (timer--function retry-timer) (timer--args retry-timer)))
        (niwa-test-retry-wait (lambda () (not (gethash key niwa-memory-pass--running)))))
      (should injected)
      (should (seq-some (lambda (text) (string-match-p "Injected retry applied cleanup failure" text)) warnings))
      (should-not (seq-some (lambda (text) (string-match-p "were not applied" text)) warnings))
      (should-not (file-exists-p path))
      (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status)))
      (should-not (gethash workspace niwa-memory-pass--deferred))
      (should-not (gethash key niwa-memory-decision--operations))))

  :doc "stale old timer cannot consume a newer retry for the same workspace"
  (niwa-test-retry-fixture
    (niwa-test-retry-defer state accepted)
    (let ((old retry-timer))
      (should (niwa-memory-pass-cancel workspace))
      (niwa-memory-pass--defer-notes state)
      (let ((current (gethash workspace niwa-memory-pass--deferred)))
        (with-current-buffer unrelated (apply (timer--function old) (timer--args old)))
        (should (eq current (gethash workspace niwa-memory-pass--deferred)))
        (should-not (gethash key niwa-memory-pass--running))
        (should (file-exists-p path))
        (should-not warnings)))))

(provide 'test-niwa-memory-pass-retry)
;;; test-niwa-memory-pass-retry.el ends here
