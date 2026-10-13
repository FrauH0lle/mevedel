;;; test-mevedel-implementation-policy-admission.el -- Preparation races -*- lexical-binding: t -*-

;;; Commentary:

;; Hold an actual gptel async transform, then exercise the production WAIT
;; admission and terminal handlers.  Only the network transport is replaced.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-engine-test-support"))
(require 'gptel-openai)
(require 'mevedel-implementation-policy)
(require 'mevedel-goal)
(require 'mevedel-presets)

(defmacro mevedel-preparation-test--with-goal (&rest body)
  "Run BODY with an accepted-policy Goal and distinct baseline selections."
  (declare (indent 0) (debug t))
  `(mevedel-engine-test--with-session
     (mevedel-request-end)
     (mevedel-skills-test--with-model-backends
       (let* ((policy '(:model-provider "Balanced:balanced-model"
                       :reasoning-effort nil :permission-mode full-auto))
              (global-budget (default-value 'mevedel-goal-token-budget))
              (gptel-use-curl t)
              (gptel-use-tools nil)
              (gptel-use-context nil)
              (mevedel-journal-enabled nil)
              goal sent)
         (mevedel-model-set-session-provider
          session (mevedel-model-resolve-provider "Fast:fast-model") buffer)
         (mevedel-model-set-session-effort session 'high buffer)
         (mevedel-permission-mode-set-raw 'ask)
         (setq-local mevedel-goal-token-budget 4321 mevedel-model-workloads nil)
         (setq goal (mevedel-goal-create
                     "Implement accepted outcome" :session session
                     :implementation-policy policy :token-budget 200))
         (cl-letf (((symbol-function 'gptel-curl-get-response)
                    (lambda (fsm) (push fsm sent))))
           (mevedel-test--with-captured-diagnostics nil ,@body))
         (should (equal "Fast:fast-model" (mevedel-session-model-provider session)))
         (should (eq 'high (mevedel-session-reasoning-effort session)))
         (should (eq 'ask (mevedel-session-permission-mode session)))
         (should (eq 'fast-model gptel-model))
         (should (eq (gptel-get-backend "Fast") gptel-backend))
         (should (eq 'high gptel-reasoning-effort))
         (should (= 4321 mevedel-goal-token-budget))
         (should (equal global-budget (default-value 'mevedel-goal-token-budget)))))))

(defun mevedel-preparation-test--send (&optional hold)
  "Send a real gptel request; HOLD optionally receives its async continuation."
  (gptel-request
   "Read the implementation evidence" :system nil
   :transforms
   (append '(mevedel-skills--transform-apply-request-model-policy)
           (when hold (list (lambda (continue _fsm) (funcall hold continue)))))
   :fsm (gptel-make-fsm
         :table (mevedel-preset--build-transitions (copy-tree gptel-send--transitions))
         :handlers (mevedel-preset--build-handlers (copy-tree gptel-send--handlers)))))

(defun mevedel-preparation-test--abort (fsm)
  "Abort admitted FSM and wait for canonical deferred settlement."
  (gptel--fsm-transition fsm 'ABRT)
  (let ((deadline (+ (float-time) 5)))
    (while (and (mevedel-turn-busy-p) (< (float-time) deadline))
      (sleep-for .002)))
  (should-not mevedel--current-request)
  (should-not (mevedel-turn-busy-p)))

(mevedel-deftest mevedel-preset--guard-preparation
  (:quiet t)
  ,test
  (test)
  :doc "held transforms reject changed Goal policy or owner and allow a fresh submission"
  (dolist (change '(pause clear resume replace edit policy))
    (mevedel-preparation-test--with-goal
      (when (eq change 'resume) (mevedel-goal-pause))
      (let* (resume
             (fsm (mevedel-preparation-test--send
                   (lambda (continue) (setq resume continue))))
             (prepared (plist-get (gptel-fsm-info fsm) :mevedel-implementation-preparation)))
        (should resume)
        (should (plist-member prepared :policy))
        (should-not sent)
        (should-not mevedel--current-request)
        (pcase change
          ('pause (mevedel-goal-pause))
          ('clear (mevedel-goal-clear))
          ('resume (mevedel-goal-resume))
          ('replace
           (mevedel-goal-clear)
           (setq goal (mevedel-goal-create
                       "Replacement" :session session :implementation-policy policy)))
          ('edit (mevedel-goal-edit "Revised objective"))
          ('policy
           (setf (mevedel-goal-implementation-policy goal)
                 '(:model-provider "Balanced:balanced-model"
                   :reasoning-effort high :permission-mode edits))))
        (let* ((current-goal (mevedel-session-goal session))
               (status (and current-goal (mevedel-goal-status current-goal))))
          (funcall resume)
          (should (eq 'ERRS (gptel-fsm-state fsm)))
          (should (plist-get (gptel-fsm-info fsm) :mevedel-turn-settled))
          (should (string-search "submit again"
                                 (plist-get (plist-get (gptel-fsm-info fsm) :error) :message)))
          (should-not (plist-get (gptel-fsm-info fsm) :mevedel-request-id))
          (should-not sent)
          (should-not mevedel--current-request)
          (should-not (mevedel-turn-busy-p))
          (should (eq current-goal (mevedel-session-goal session)))
          (when current-goal (should (eq status (mevedel-goal-status current-goal)))))
        (let* ((expected (mevedel-implementation-policy-current))
               (fresh (mevedel-preparation-test--send))
               (request mevedel--current-request)
               (info (gptel-fsm-info fresh)))
          (should (equal (list fresh) sent))
          (should (equal expected (mevedel-request-implementation-policy request)))
          (should (eq (if expected 'balanced-model 'fast-model) (plist-get info :model)))
          (should (eq (if expected (plist-get expected :reasoning-effort) 'high)
                      (plist-get info :reasoning-effort)))
          (should (eq (or (plist-get expected :permission-mode) 'ask)
                      (mevedel-permission-mode-effective session nil nil request)))
          (mevedel-preparation-test--abort fresh)))))

  :doc "an older continuation cannot adopt, cancel or clear a replacement request"
  (mevedel-preparation-test--with-goal
    (let* (resume
           (old (mevedel-preparation-test--send
                 (lambda (continue) (setq resume continue)))))
      (mevedel-goal-pause)
      (let* ((new (mevedel-preparation-test--send))
             (request mevedel--current-request)
             cancelled)
        (setq-local gptel--fsm-last new)
        (mevedel-request-push-canceller request (lambda () (setq cancelled t)))
        (funcall resume)
        (should (eq 'ERRS (gptel-fsm-state old)))
        (should (plist-get (gptel-fsm-info old) :mevedel-turn-settled))
        (should-not (plist-get (gptel-fsm-info old) :mevedel-request-id))
        (should (equal (list new) sent))
        (should (eq request mevedel--current-request))
        (should (eq new (mevedel-request-fsm request)))
        (should (eq new gptel--fsm-last))
        (should-not (mevedel-request-cancelled-p request))
        (should-not cancelled)
        (mevedel-preparation-test--abort new))))

  :doc "yielding readiness permits baseline inspection then rejects changed preparation"
  (mevedel-preparation-test--with-goal
    (let ((check (symbol-function 'mevedel-request-assert-target-ready))
          fsm)
      (cl-letf (((symbol-function 'mevedel-request-assert-target-ready)
                 (lambda (owner)
                   (prog1 (funcall check owner)
                     (mevedel-goal-pause)
                     (should-not (mevedel-implementation-policy-current session buffer))))))
        (setq fsm (mevedel-preparation-test--send)))
      (should (eq 'ERRS (gptel-fsm-state fsm)))
      (should (plist-get (gptel-fsm-info fsm) :mevedel-turn-settled))
      (should-not sent)
      (should-not mevedel--current-request)
      (should-not (mevedel-turn-busy-p))
      (let ((fresh (mevedel-preparation-test--send)))
        (should (equal (list fresh) sent))
        (should (eq 'fast-model (plist-get (gptel-fsm-info fresh) :model)))
        (mevedel-preparation-test--abort fresh))))

  :doc "unchanged preparation and already admitted Goal requests retain their snapshot"
  (mevedel-preparation-test--with-goal
    (let* (resume
           (fsm (mevedel-preparation-test--send
                 (lambda (continue) (setq resume continue)))))
      (funcall resume)
      (let ((request mevedel--current-request))
        (should (equal (list fsm) sent))
        (should (equal policy (mevedel-request-implementation-policy request)))
        (mevedel-goal-pause)
        (let* ((info (gptel-fsm-info fsm))
               (continuation-policy (mevedel-skills-request-model-policy)))
          (should (eq 'balanced-model (plist-get continuation-policy :model)))
          (should (eq 'full-auto (mevedel-permission-mode-effective session nil nil request)))
          (should (equal policy (mevedel-request-implementation-policy request)))
          (should (eq 'balanced-model (plist-get info :model))))
        (mevedel-preparation-test--abort fsm))))

  :doc "reentrant admission during readiness leaves the replacement running and old FSM settled"
  (mevedel-preparation-test--with-goal
    (let ((check (symbol-function 'mevedel-request-assert-target-ready))
          entered replacement request cancelled old)
      (cl-letf (((symbol-function 'mevedel-request-assert-target-ready)
                 (lambda (owner)
                   (prog1 (funcall check owner)
                     (unless entered
                       (setq entered t
                             replacement (mevedel-preparation-test--send)
                             request mevedel--current-request)
                       (setq-local gptel--fsm-last replacement)
                       (mevedel-request-push-canceller
                        request (lambda () (setq cancelled t))))))))
        (setq old (mevedel-preparation-test--send)))
      (should (eq 'ERRS (gptel-fsm-state old)))
      (should (plist-get (gptel-fsm-info old) :mevedel-turn-settled))
      (should-not (plist-get (gptel-fsm-info old) :mevedel-request-id))
      (should (equal (list replacement) sent))
      (should (eq request mevedel--current-request))
      (should (eq replacement (mevedel-request-fsm request)))
      (should (eq replacement gptel--fsm-last))
      (should-not (mevedel-request-cancelled-p request))
      (should-not cancelled)
      (should (eq 'active (mevedel-goal-status goal)))
      (mevedel-preparation-test--abort replacement)))

  :doc "ordinary Goals without accepted policy remain on their unrelated baseline path"
  (mevedel-preparation-test--with-goal
    (setf (mevedel-goal-implementation-policy goal) nil)
    (let* (resume
           (fsm (mevedel-preparation-test--send
                 (lambda (continue) (setq resume continue)))))
      (mevedel-goal-clear)
      (setq goal (mevedel-goal-create "Ordinary replacement" :session session))
      (funcall resume)
      (should (equal (list fsm) sent))
      (should (eq 'fast-model (plist-get (gptel-fsm-info fsm) :model)))
      (should-not (mevedel-request-implementation-policy mevedel--current-request))
      (should (equal (mevedel-goal-id goal) (plist-get (gptel-fsm-info fsm) :mevedel-goal-id)))
      (mevedel-preparation-test--abort fsm)))

  :doc "Direct policy is independent of Goal changes and replacement pending identity survives"
  (mevedel-preparation-test--with-goal
    (mevedel-goal-clear)
    (setq-local mevedel--pending-implementation-policy (copy-tree policy))
    (let* (resume
           (fsm (mevedel-preparation-test--send
                 (lambda (continue) (setq resume continue))))
           (new-pending (copy-tree mevedel--pending-implementation-policy)))
      (setq-local mevedel--pending-implementation-policy new-pending)
      (funcall resume)
      (should (eq 'ERRS (gptel-fsm-state fsm)))
      (should-not sent)
      (should (eq new-pending mevedel--pending-implementation-policy))
      (let* (next-resume
             (fresh (mevedel-preparation-test--send
                     (lambda (continue) (setq next-resume continue)))))
        (setq goal (mevedel-goal-create "Ordinary Goal" :session session))
        (funcall next-resume)
        (should (equal (list fresh) sent))
        (should (equal policy (mevedel-request-implementation-policy mevedel--current-request)))
        (should-not mevedel--pending-implementation-policy)
        (mevedel-preparation-test--abort fresh))))

  :doc "pending accepted Goal kickoff rejects a changed owner even with unchanged policy"
  (mevedel-preparation-test--with-goal
    (setq-local mevedel--pending-implementation-policy (copy-tree policy))
    (let* (resume
           (fsm (mevedel-preparation-test--send
                 (lambda (continue) (setq resume continue)))))
      (mevedel-goal-clear)
      (setq goal (mevedel-goal-create
                  "Replacement accepted Goal" :session session
                  :implementation-policy policy))
      (funcall resume)
      (should (eq 'ERRS (gptel-fsm-state fsm)))
      (should (plist-get (gptel-fsm-info fsm) :mevedel-turn-settled))
      (should-not sent)
      (should-not mevedel--current-request)
      (should-not mevedel--pending-implementation-policy)
      (should (eq 'active (mevedel-goal-status goal)))
      (let ((fresh (mevedel-preparation-test--send)))
        (should (equal (list fresh) sent))
        (should (eq 'balanced-model (plist-get (gptel-fsm-info fresh) :model)))
        (mevedel-preparation-test--abort fresh)))))

(provide 'test-mevedel-implementation-policy-admission)
;;; test-mevedel-implementation-policy-admission.el ends here
