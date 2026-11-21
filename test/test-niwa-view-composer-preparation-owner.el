;;; test-niwa-view-composer-preparation-owner.el -- Owned startup failure -*- lexical-binding: t -*-

;;; Commentary:

;; Real source-backed skill preparation and composer dispatch, with its prepared
;; callback held across an event-loop boundary.  Readiness races must not give
;; an older failure authority over a newer Goal, request or pending skill stash.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-engine-test-support"))
(require 'gptel-openai)
(require 'niwa-implementation-policy)
(require 'niwa-goal)
(require 'niwa-skills-core)
(require 'niwa-skills-plan)
(require 'niwa-view-composer)

(defvar niwa-journal-enabled)

(defmacro niwa-composer-owner-test--with-skill (&rest body)
  "Run BODY with a real skill, accepted Goal and held skill preparation."
  (declare (indent 0) (debug t))
  `(niwa-engine-test--with-session
     (niwa-request-end)
     (niwa-skills-test--with-model-backends
       (let* ((view (generate-new-buffer " *composer-owner-view*"))
              (skills-root (file-name-concat root ".niwa" "skills"))
              (niwa-skill-dirs (list skills-root))
              (niwa-skills-include-bundled nil)
              (niwa-skills-check-for-modifications nil)
              (niwa-journal-enabled nil)
              (policy '(:model-provider "Balanced:balanced-model"
                        :reasoning-effort nil :permission-mode edits))
              (input "$alpha inspect")
              (prepare (symbol-function 'niwa-skills-plan-prepare))
              (dispatch (symbol-function 'niwa-view--dispatch-prepared-outcome))
              (pause (symbol-function 'niwa-implementation-policy-pause-failure))
              resume prepared submission goal result failure failure-preparation)
         (unwind-protect
             (progn
               (niwa-model-set-session-provider
                session (niwa-model-resolve-provider "Fast:fast-model") buffer)
               (niwa-model-set-session-effort session 'high buffer)
               (niwa-permission-mode-set-raw 'ask)
               (setq-local niwa-goal-token-budget 4321 niwa-model-workloads nil)
               (setq goal (niwa-goal-create
                           "Original accepted Goal" :session session
                           :implementation-policy policy :token-budget 200))
               (niwa-skills-test--write-skill
                skills-root "alpha"
                "name: alpha\ndescription: Source-backed ownership fixture\ncontext: inline\nallowed-tools: Read\n"
                "Read evidence for the requested outcome.")
               (niwa-skills-install session buffer)
               (niwa-view--setup view buffer)
               (cl-letf (((symbol-function 'niwa-skills-plan-prepare)
                          (lambda (plan callback &optional cancelled-p)
                            (funcall prepare plan
                                     (lambda (value)
                                       (setq prepared value
                                             resume (lambda () (funcall callback value))))
                                     cancelled-p)))
                         ((symbol-function 'niwa-view--dispatch-prepared-outcome)
                          (lambda (&rest args)
                            (setq submission (car args)
                                  result (apply dispatch args))))
                         ((symbol-function 'niwa-implementation-policy-pause-failure)
                          (lambda (&rest args)
                            (setq failure (nth 1 args)
                                  failure-preparation (nth 2 args))
                            (apply pause args))))
                 (with-current-buffer view
                   (niwa-view-composer-insert input)
                   (niwa-view--submit-planned-input input))
                 (should resume)
                 (should (eq 'ok (plist-get prepared :status)))
                 (should (plist-get prepared :request-context))
                 (should-not submission)
                 (should-not niwa--current-request)
                 (niwa-test--with-captured-diagnostics nil ,@body))
               (should (equal "Fast:fast-model" (niwa-session-model-provider session)))
               (should (eq 'high (niwa-session-reasoning-effort session)))
               (should (eq 'ask (niwa-session-permission-mode session)))
               (should (eq 'fast-model gptel-model))
               (should (eq 'high gptel-reasoning-effort))
               (should (= 4321 niwa-goal-token-budget)))
           (when (buffer-live-p view) (kill-buffer view)))))))

(niwa-deftest niwa-view--dispatch-prepared-outcome/preparation-owner
  (:quiet t)
  ,test
  (test)
  :doc "real prepared-skill stale preflight preserves replacement Goal and equal-shaped newer stash"
  (dolist (change '(replace edit request))
    (niwa-composer-owner-test--with-skill
      (let ((check (symbol-function 'niwa-request-assert-target-ready))
            entered newer-goal newer-context newer-request cancelled sent)
        (cl-letf (((symbol-function 'niwa-request-assert-target-ready)
                   (lambda (owner)
                     (prog1 (funcall check owner)
                       (unless entered
                         (setq entered t)
                         (with-current-buffer buffer
                           (should niwa-skills--pending-request-context)
                           (setq newer-context
                                 (copy-tree niwa-skills--pending-request-context))
                           (pcase change
                             ('replace
                              (niwa-goal-clear)
                              (setq newer-goal
                                    (niwa-goal-create
                                     "Replacement accepted Goal" :session session
                                     :implementation-policy policy)))
                             ('edit
                              (niwa-goal-edit "Revised accepted Goal")
                              (setq newer-goal goal))
                             ('request
                              (setq newer-goal goal
                                    newer-request (niwa-request-begin session))
                              (niwa-request-push-canceller
                               newer-request (lambda () (setq cancelled t)))))
                           (setq-local niwa-skills--pending-request-context
                                       newer-context))))))
                  ((symbol-function 'gptel-send)
                   (lambda (&rest _) (setq sent t))))
          (funcall resume))
        (should submission)
        (should (eq 'niwa-stale-implementation-preparation (car failure)))
        (should (eq goal (plist-get failure-preparation :goal)))
        (should-not result)
        (should-not sent)
        (should (eq 'pending (niwa-prompt-submission-state submission)))
        (should (eq newer-goal (niwa-session-goal session)))
        (should (eq 'active (niwa-goal-status newer-goal)))
        (should-not (niwa-goal-reason newer-goal))
        (should (eq newer-context niwa-skills--pending-request-context))
        (should (eq newer-request niwa--current-request))
        (should-not cancelled)
        (when newer-request
          (should-not (niwa-request-cancelled-p newer-request))
          (niwa-request-end))
        (with-current-buffer view
          (should (equal input (niwa-view-composer-draft-text)))))))

  :doc "a generic readiness error after Goal replacement has no authority over the new owner"
  (niwa-composer-owner-test--with-skill
    (let ((check (symbol-function 'niwa-request-assert-target-ready))
          newer-goal newer-context)
      (cl-letf (((symbol-function 'niwa-request-assert-target-ready)
                 (lambda (owner)
                   (funcall check owner)
                   (with-current-buffer buffer
                     (setq newer-context (copy-tree niwa-skills--pending-request-context))
                     (niwa-goal-clear)
                     (setq newer-goal (niwa-goal-create
                                       "New unrelated accepted Goal" :session session
                                       :implementation-policy policy))
                     (setq-local niwa-skills--pending-request-context newer-context))
                   (user-error "Readiness callback failed after replacement"))))
        (funcall resume))
      (should-not result)
      (should (eq 'user-error (car failure)))
      (should (eq goal (plist-get failure-preparation :goal)))
      (should (eq newer-goal (niwa-session-goal session)))
      (should (eq 'active (niwa-goal-status newer-goal)))
      (should-not (niwa-goal-reason newer-goal))
      (should (eq newer-context niwa-skills--pending-request-context))
      (should-not niwa--current-request)))

  :doc "owned prepared-skill startup failure still durably pauses its original Goal"
  (niwa-composer-owner-test--with-skill
    (let ((check (symbol-function 'niwa-request-assert-target-ready)))
      (cl-letf (((symbol-function 'niwa-request-assert-target-ready)
                 (lambda (owner)
                   (funcall check owner)
                   (user-error "Owned readiness failure"))))
        (funcall resume)))
    (should-not result)
    (should (eq 'user-error (car failure)))
    (should (eq goal (plist-get failure-preparation :goal)))
    (should (eq goal (niwa-session-goal session)))
    (should (eq 'paused (niwa-goal-status goal)))
    (should (equal "Owned readiness failure" (niwa-goal-reason goal)))
    (should-not niwa-skills--pending-request-context)
    (should-not niwa--current-request)
    (let ((saved (read (decode-coding-string
                       (niwa-session-artifacts-read-artifact
                        session "session.meta.el" t) 'utf-8-unix))))
      (should (eq 'paused (plist-get (plist-get saved :goal) :status)))
      (should (equal "Owned readiness failure"
                     (plist-get (plist-get saved :goal) :reason))))))

(niwa-deftest niwa-implementation-policy-pause-failure/owned-preparation
  (:quiet t)
  ,test
  (test)
  :doc "stale refusal does not pause even when the original Goal is active again"
  (niwa-composer-owner-test--with-skill
    (let ((preparation (niwa-implementation-policy-capture-preparation))
          (publication (niwa-session-publication session)))
      (should-not
       (niwa-implementation-policy-pause-failure
        buffer '(niwa-stale-implementation-preparation) preparation))
      (should (eq 'active (niwa-goal-status goal)))
      (should (eq publication (niwa-session-publication session))))))

(provide 'test-niwa-view-composer-preparation-owner)
;;; test-niwa-view-composer-preparation-owner.el ends here
