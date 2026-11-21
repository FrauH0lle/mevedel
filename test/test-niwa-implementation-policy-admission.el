;;; test-niwa-implementation-policy-admission.el -- Preparation races -*- lexical-binding: t -*-

;;; Commentary:

;; Hold an actual gptel async transform, then exercise the production WAIT
;; admission and terminal handlers.  Only the network transport is replaced.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-engine-test-support"))
(require 'gptel-openai)
(require 'niwa-implementation-policy)
(require 'niwa-goal)
(require 'niwa-presets)

(defmacro niwa-preparation-test--with-goal (&rest body)
  "Run BODY with an accepted-policy Goal and distinct baseline selections.
An initial :file-workspace FORM selects file storage for policy-only cases."
  (declare (indent 0) (debug t))
  (let ((file-workspace (when (eq (car body) :file-workspace)
                         (pop body) (pop body))))
    `(niwa-engine-test--with-session :file-workspace ,file-workspace
       (niwa-request-end)
       (niwa-skills-test--with-model-backends
         (let* ((policy '(:model-provider "Balanced:balanced-model"
                         :reasoning-effort nil :permission-mode full-auto))
                (global-budget (default-value 'niwa-goal-token-budget))
                (gptel-use-curl t)
                (gptel-use-tools nil)
                (gptel-use-context nil)
                (niwa-journal-enabled nil)
                goal sent)
           (niwa-model-set-session-provider
            session (niwa-model-resolve-provider "Fast:fast-model") buffer)
           (niwa-model-set-session-effort session 'high buffer)
           (niwa-permission-mode-set-raw 'ask)
           (setq-local niwa-goal-token-budget 4321 niwa-model-workloads nil)
           (setq goal (niwa-goal-create
                       "Implement accepted outcome" :session session
                       :implementation-policy policy :token-budget 200))
           (cl-letf (((symbol-function 'gptel-curl-get-response)
                      (lambda (fsm) (push fsm sent))))
             (niwa-test--with-captured-diagnostics nil ,@body))
           (should (equal "Fast:fast-model" (niwa-session-model-provider session)))
           (should (eq 'high (niwa-session-reasoning-effort session)))
           (should (eq 'ask (niwa-session-permission-mode session)))
           (should (eq 'fast-model gptel-model))
           (should (eq (gptel-get-backend "Fast") gptel-backend))
           (should (eq 'high gptel-reasoning-effort))
           (should (= 4321 niwa-goal-token-budget))
           (should (equal global-budget (default-value 'niwa-goal-token-budget))))))))

(defun niwa-preparation-test--send (&optional hold)
  "Send a real gptel request; HOLD optionally receives its async continuation."
  (gptel-request
   "Read the implementation evidence" :system nil
   :transforms
   (append '(niwa-skills--transform-apply-request-model-policy)
           (when hold (list (lambda (continue _fsm) (funcall hold continue)))))
   :fsm (gptel-make-fsm
         :table (niwa-preset--build-transitions (copy-tree gptel-send--transitions))
         :handlers (niwa-preset--build-handlers (copy-tree gptel-send--handlers)))))

(defun niwa-preparation-test--abort (fsm)
  "Abort admitted FSM and wait for canonical deferred settlement."
  (gptel--fsm-transition fsm 'ABRT)
  (let ((deadline (+ (float-time) 5)))
    (while (and (niwa-turn-busy-p) (< (float-time) deadline))
      (sleep-for .002)))
  (should-not niwa--current-request)
  (should-not (niwa-turn-busy-p)))

(niwa-deftest niwa-preset--guard-preparation
  (:quiet t)
  ,test
  (test)
  :doc "held transforms reject changed Goal policy or owner and allow a fresh submission"
  (dolist (change '(pause clear resume replace edit policy))
    ;; These permutations invalidate a held request snapshot, not publication.
    ;; Pause remains the project-backed control for this matrix.
    (niwa-preparation-test--with-goal :file-workspace (not (eq change 'pause))
      (when (eq change 'resume) (niwa-goal-pause))
      (let* (resume
             (fsm (niwa-preparation-test--send
                   (lambda (continue) (setq resume continue))))
             (prepared (plist-get (gptel-fsm-info fsm) :niwa-implementation-preparation)))
        (should resume)
        (should (plist-member prepared :policy))
        (should-not sent)
        (should-not niwa--current-request)
        (pcase change
          ('pause (niwa-goal-pause))
          ('clear (niwa-goal-clear))
          ('resume (niwa-goal-resume))
          ('replace
           (niwa-goal-clear)
           (setq goal (niwa-goal-create
                       "Replacement" :session session :implementation-policy policy)))
          ('edit (niwa-goal-edit "Revised objective"))
          ('policy
           (setf (niwa-goal-implementation-policy goal)
                 '(:model-provider "Balanced:balanced-model"
                   :reasoning-effort high :permission-mode edits))))
        (let* ((current-goal (niwa-session-goal session))
               (status (and current-goal (niwa-goal-status current-goal))))
          (funcall resume)
          (should (eq 'ERRS (gptel-fsm-state fsm)))
          (should (plist-get (gptel-fsm-info fsm) :niwa-turn-settled))
          (should (string-search "submit again"
                                 (plist-get (plist-get (gptel-fsm-info fsm) :error) :message)))
          (should-not (plist-get (gptel-fsm-info fsm) :niwa-request-id))
          (should-not sent)
          (should-not niwa--current-request)
          (should-not (niwa-turn-busy-p))
          (should (eq current-goal (niwa-session-goal session)))
          (when current-goal (should (eq status (niwa-goal-status current-goal)))))
        (let* ((expected (niwa-implementation-policy-current))
               (fresh (niwa-preparation-test--send))
               (request niwa--current-request)
               (info (gptel-fsm-info fresh)))
          (should (equal (list fresh) sent))
          (should (equal expected (niwa-request-implementation-policy request)))
          (should (eq (if expected 'balanced-model 'fast-model) (plist-get info :model)))
          (should (eq (if expected (plist-get expected :reasoning-effort) 'high)
                      (plist-get info :reasoning-effort)))
          (should (eq (or (plist-get expected :permission-mode) 'ask)
                      (niwa-permission-mode-effective session nil nil request)))
          (niwa-preparation-test--abort fresh)))))

  :doc "an older continuation cannot adopt, cancel or clear a replacement request"
  (niwa-preparation-test--with-goal
    (let* (resume
           (old (niwa-preparation-test--send
                 (lambda (continue) (setq resume continue)))))
      (niwa-goal-pause)
      (let* ((new (niwa-preparation-test--send))
             (request niwa--current-request)
             cancelled)
        (setq-local gptel--fsm-last new)
        (niwa-request-push-canceller request (lambda () (setq cancelled t)))
        (funcall resume)
        (should (eq 'ERRS (gptel-fsm-state old)))
        (should (plist-get (gptel-fsm-info old) :niwa-turn-settled))
        (should-not (plist-get (gptel-fsm-info old) :niwa-request-id))
        (should (equal (list new) sent))
        (should (eq request niwa--current-request))
        (should (eq new (niwa-request-fsm request)))
        (should (eq new gptel--fsm-last))
        (should-not (niwa-request-cancelled-p request))
        (should-not cancelled)
        (niwa-preparation-test--abort new))))

  :doc "yielding readiness permits baseline inspection then rejects changed preparation"
  (niwa-preparation-test--with-goal
    (let ((check (symbol-function 'niwa-request-assert-target-ready))
          fsm)
      (cl-letf (((symbol-function 'niwa-request-assert-target-ready)
                 (lambda (owner)
                   (prog1 (funcall check owner)
                     (niwa-goal-pause)
                     (should-not (niwa-implementation-policy-current session buffer))))))
        (setq fsm (niwa-preparation-test--send)))
      (should (eq 'ERRS (gptel-fsm-state fsm)))
      (should (plist-get (gptel-fsm-info fsm) :niwa-turn-settled))
      (should-not sent)
      (should-not niwa--current-request)
      (should-not (niwa-turn-busy-p))
      (let ((fresh (niwa-preparation-test--send)))
        (should (equal (list fresh) sent))
        (should (eq 'fast-model (plist-get (gptel-fsm-info fresh) :model)))
        (niwa-preparation-test--abort fresh))))

  :doc "unchanged preparation and already admitted Goal requests retain their snapshot"
  (niwa-preparation-test--with-goal
    (let* (resume
           (fsm (niwa-preparation-test--send
                 (lambda (continue) (setq resume continue)))))
      (funcall resume)
      (let ((request niwa--current-request))
        (should (equal (list fsm) sent))
        (should (equal policy (niwa-request-implementation-policy request)))
        (niwa-goal-pause)
        (let* ((info (gptel-fsm-info fsm))
               (continuation-policy (niwa-skills-request-model-policy)))
          (should (eq 'balanced-model (plist-get continuation-policy :model)))
          (should (eq 'full-auto (niwa-permission-mode-effective session nil nil request)))
          (should (equal policy (niwa-request-implementation-policy request)))
          (should (eq 'balanced-model (plist-get info :model))))
        (niwa-preparation-test--abort fsm))))

  :doc "reentrant admission during readiness leaves the replacement running and old FSM settled"
  (niwa-preparation-test--with-goal
    (let ((check (symbol-function 'niwa-request-assert-target-ready))
          entered replacement request cancelled old)
      (cl-letf (((symbol-function 'niwa-request-assert-target-ready)
                 (lambda (owner)
                   (prog1 (funcall check owner)
                     (unless entered
                       (setq entered t
                             replacement (niwa-preparation-test--send)
                             request niwa--current-request)
                       (setq-local gptel--fsm-last replacement)
                       (niwa-request-push-canceller
                        request (lambda () (setq cancelled t))))))))
        (setq old (niwa-preparation-test--send)))
      (should (eq 'ERRS (gptel-fsm-state old)))
      (should (plist-get (gptel-fsm-info old) :niwa-turn-settled))
      (should-not (plist-get (gptel-fsm-info old) :niwa-request-id))
      (should (equal (list replacement) sent))
      (should (eq request niwa--current-request))
      (should (eq replacement (niwa-request-fsm request)))
      (should (eq replacement gptel--fsm-last))
      (should-not (niwa-request-cancelled-p request))
      (should-not cancelled)
      (should (eq 'active (niwa-goal-status goal)))
      (niwa-preparation-test--abort replacement)))

  :doc "an error after this FSM's own admission surfaces instead of reading as stale"
  (niwa-preparation-test--with-goal
    (let* (resume
           (fsm (niwa-preparation-test--send
                 (lambda (continue) (setq resume continue))))
           (info (gptel-fsm-info fsm)))
      (cl-letf (((symbol-function 'niwa-sleep-update)
                 (lambda (&rest _) (error "Post-admission failure"))))
        ;; An intermittent failure here left no trace of why `resume' did not
        ;; signal.  Report the FSM's settlement state with the failure; the
        ;; assertion itself is unchanged.
        (condition-case failure
            (should (equal "Post-admission failure"
                           (cadr (should-error (funcall resume)))))
          (ert-test-failed
           (let ((current (gptel-fsm-info fsm)))
             (ert-fail
              (list (cadr failure)
                    ;; One string: ERT truncates nested printed lists.
                    (format
                     "%S"
                     (list :fsm-state (gptel-fsm-state fsm)
                           :fsm-history (plist-get current :history)
                           :preparation-rejected
                           (plist-get current :niwa-preparation-rejected)
                           :error (plist-get current :error)
                           :request-id (plist-get current :niwa-request-id)
                           :request-begun
                           (plist-get current :niwa-request-begun)
                           :current-request
                           (and niwa--current-request t)))))))))
      (should-not (plist-get info :niwa-preparation-rejected))
      (should niwa--current-request)
      (should (eq niwa--current-request (plist-get info :niwa-request)))
      (should (eq fsm (niwa-request-fsm niwa--current-request)))
      (niwa-preparation-test--abort fsm)))

  :doc "ordinary Goals without accepted policy remain on their unrelated baseline path"
  (niwa-preparation-test--with-goal
    (setf (niwa-goal-implementation-policy goal) nil)
    (let* (resume
           (fsm (niwa-preparation-test--send
                 (lambda (continue) (setq resume continue)))))
      (niwa-goal-clear)
      (setq goal (niwa-goal-create "Ordinary replacement" :session session))
      (funcall resume)
      (should (equal (list fsm) sent))
      (should (eq 'fast-model (plist-get (gptel-fsm-info fsm) :model)))
      (should-not (niwa-request-implementation-policy niwa--current-request))
      (should (equal (niwa-goal-id goal) (plist-get (gptel-fsm-info fsm) :niwa-goal-id)))
      (niwa-preparation-test--abort fsm)))

  :doc "Direct policy is independent of Goal changes and replacement pending identity survives"
  (niwa-preparation-test--with-goal
    (niwa-goal-clear)
    (setq-local niwa--pending-implementation-policy (copy-tree policy))
    (let* (resume
           (fsm (niwa-preparation-test--send
                 (lambda (continue) (setq resume continue))))
           (new-pending (copy-tree niwa--pending-implementation-policy)))
      (setq-local niwa--pending-implementation-policy new-pending)
      (funcall resume)
      (should (eq 'ERRS (gptel-fsm-state fsm)))
      (should-not sent)
      (should (eq new-pending niwa--pending-implementation-policy))
      (let* (next-resume
             (fresh (niwa-preparation-test--send
                     (lambda (continue) (setq next-resume continue)))))
        (setq goal (niwa-goal-create "Ordinary Goal" :session session))
        (funcall next-resume)
        (should (equal (list fresh) sent))
        (should (equal policy (niwa-request-implementation-policy niwa--current-request)))
        (should-not niwa--pending-implementation-policy)
        (niwa-preparation-test--abort fresh))))

  :doc "pending accepted Goal kickoff rejects a changed owner even with unchanged policy"
  (niwa-preparation-test--with-goal
    (setq-local niwa--pending-implementation-policy (copy-tree policy))
    (let* (resume
           (fsm (niwa-preparation-test--send
                 (lambda (continue) (setq resume continue)))))
      (niwa-goal-clear)
      (setq goal (niwa-goal-create
                  "Replacement accepted Goal" :session session
                  :implementation-policy policy))
      (funcall resume)
      (should (eq 'ERRS (gptel-fsm-state fsm)))
      (should (plist-get (gptel-fsm-info fsm) :niwa-turn-settled))
      (should-not sent)
      (should-not niwa--current-request)
      (should-not niwa--pending-implementation-policy)
      (should (eq 'active (niwa-goal-status goal)))
      (let ((fresh (niwa-preparation-test--send)))
        (should (equal (list fresh) sent))
        (should (eq 'balanced-model (plist-get (gptel-fsm-info fresh) :model)))
        (niwa-preparation-test--abort fresh)))))

(provide 'test-niwa-implementation-policy-admission)
;;; test-niwa-implementation-policy-admission.el ends here
