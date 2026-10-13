;;; test-mevedel-view-composer-preparation-owner.el -- Owned startup failure -*- lexical-binding: t -*-

;;; Commentary:

;; Real source-backed skill preparation and composer dispatch, with its prepared
;; callback held across an event-loop boundary.  Readiness races must not give
;; an older failure authority over a newer Goal, request or pending skill stash.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-engine-test-support"))
(require 'gptel-openai)
(require 'mevedel-implementation-policy)
(require 'mevedel-goal)
(require 'mevedel-skills-core)
(require 'mevedel-skills-plan)
(require 'mevedel-view-composer)

(defvar mevedel-journal-enabled)

(defmacro mevedel-composer-owner-test--with-skill (&rest body)
  "Run BODY with a real skill, accepted Goal and held skill preparation."
  (declare (indent 0) (debug t))
  `(mevedel-engine-test--with-session
     (mevedel-request-end)
     (mevedel-skills-test--with-model-backends
       (let* ((view (generate-new-buffer " *composer-owner-view*"))
              (skills-root (file-name-concat root ".mevedel" "skills"))
              (mevedel-skill-dirs (list skills-root))
              (mevedel-skills-include-bundled nil)
              (mevedel-skills-check-for-modifications nil)
              (mevedel-journal-enabled nil)
              (policy '(:model-provider "Balanced:balanced-model"
                        :reasoning-effort nil :permission-mode edits))
              (input "$alpha inspect")
              (prepare (symbol-function 'mevedel-skills-plan-prepare))
              (dispatch (symbol-function 'mevedel-view--dispatch-prepared-outcome))
              (pause (symbol-function 'mevedel-implementation-policy-pause-failure))
              resume prepared submission goal result failure failure-preparation)
         (unwind-protect
             (progn
               (mevedel-model-set-session-provider
                session (mevedel-model-resolve-provider "Fast:fast-model") buffer)
               (mevedel-model-set-session-effort session 'high buffer)
               (mevedel-permission-mode-set-raw 'ask)
               (setq-local mevedel-goal-token-budget 4321 mevedel-model-workloads nil)
               (setq goal (mevedel-goal-create
                           "Original accepted Goal" :session session
                           :implementation-policy policy :token-budget 200))
               (mevedel-skills-test--write-skill
                skills-root "alpha"
                "name: alpha\ndescription: Source-backed ownership fixture\ncontext: inline\nallowed-tools: Read\n"
                "Read evidence for the requested outcome.")
               (mevedel-skills-install session buffer)
               (mevedel-view--setup view buffer)
               (cl-letf (((symbol-function 'mevedel-skills-plan-prepare)
                          (lambda (plan callback &optional cancelled-p)
                            (funcall prepare plan
                                     (lambda (value)
                                       (setq prepared value
                                             resume (lambda () (funcall callback value))))
                                     cancelled-p)))
                         ((symbol-function 'mevedel-view--dispatch-prepared-outcome)
                          (lambda (&rest args)
                            (setq submission (car args)
                                  result (apply dispatch args))))
                         ((symbol-function 'mevedel-implementation-policy-pause-failure)
                          (lambda (&rest args)
                            (setq failure (nth 1 args)
                                  failure-preparation (nth 2 args))
                            (apply pause args))))
                 (with-current-buffer view
                   (goto-char (mevedel-view--input-start))
                   (insert input)
                   (mevedel-view--submit-planned-input input))
                 (should resume)
                 (should (eq 'ok (plist-get prepared :status)))
                 (should (plist-get prepared :request-context))
                 (should-not submission)
                 (should-not mevedel--current-request)
                 (mevedel-test--with-captured-diagnostics nil ,@body))
               (should (equal "Fast:fast-model" (mevedel-session-model-provider session)))
               (should (eq 'high (mevedel-session-reasoning-effort session)))
               (should (eq 'ask (mevedel-session-permission-mode session)))
               (should (eq 'fast-model gptel-model))
               (should (eq 'high gptel-reasoning-effort))
               (should (= 4321 mevedel-goal-token-budget)))
           (when (buffer-live-p view) (kill-buffer view)))))))

(mevedel-deftest mevedel-view--dispatch-prepared-outcome/preparation-owner
  (:quiet t)
  ,test
  (test)
  :doc "real prepared-skill stale preflight preserves replacement Goal and equal-shaped newer stash"
  (dolist (change '(replace edit request))
    (mevedel-composer-owner-test--with-skill
      (let ((check (symbol-function 'mevedel-request-assert-target-ready))
            entered newer-goal newer-context newer-request cancelled sent)
        (cl-letf (((symbol-function 'mevedel-request-assert-target-ready)
                   (lambda (owner)
                     (prog1 (funcall check owner)
                       (unless entered
                         (setq entered t)
                         (with-current-buffer buffer
                           (should mevedel-skills--pending-request-context)
                           (setq newer-context
                                 (copy-tree mevedel-skills--pending-request-context))
                           (pcase change
                             ('replace
                              (mevedel-goal-clear)
                              (setq newer-goal
                                    (mevedel-goal-create
                                     "Replacement accepted Goal" :session session
                                     :implementation-policy policy)))
                             ('edit
                              (mevedel-goal-edit "Revised accepted Goal")
                              (setq newer-goal goal))
                             ('request
                              (setq newer-goal goal
                                    newer-request (mevedel-request-begin session))
                              (mevedel-request-push-canceller
                               newer-request (lambda () (setq cancelled t)))))
                           (setq-local mevedel-skills--pending-request-context
                                       newer-context))))))
                  ((symbol-function 'gptel-send)
                   (lambda (&rest _) (setq sent t))))
          (funcall resume))
        (should submission)
        (should (eq 'mevedel-stale-implementation-preparation (car failure)))
        (should (eq goal (plist-get failure-preparation :goal)))
        (should-not result)
        (should-not sent)
        (should (eq 'pending (mevedel-prompt-submission-state submission)))
        (should (eq newer-goal (mevedel-session-goal session)))
        (should (eq 'active (mevedel-goal-status newer-goal)))
        (should-not (mevedel-goal-reason newer-goal))
        (should (eq newer-context mevedel-skills--pending-request-context))
        (should (eq newer-request mevedel--current-request))
        (should-not cancelled)
        (when newer-request
          (should-not (mevedel-request-cancelled-p newer-request))
          (mevedel-request-end))
        (with-current-buffer view
          (should (equal input (mevedel-view--input-text)))))))

  :doc "a generic readiness error after Goal replacement has no authority over the new owner"
  (mevedel-composer-owner-test--with-skill
    (let ((check (symbol-function 'mevedel-request-assert-target-ready))
          newer-goal newer-context)
      (cl-letf (((symbol-function 'mevedel-request-assert-target-ready)
                 (lambda (owner)
                   (funcall check owner)
                   (with-current-buffer buffer
                     (setq newer-context (copy-tree mevedel-skills--pending-request-context))
                     (mevedel-goal-clear)
                     (setq newer-goal (mevedel-goal-create
                                       "New unrelated accepted Goal" :session session
                                       :implementation-policy policy))
                     (setq-local mevedel-skills--pending-request-context newer-context))
                   (user-error "Readiness callback failed after replacement"))))
        (funcall resume))
      (should-not result)
      (should (eq 'user-error (car failure)))
      (should (eq goal (plist-get failure-preparation :goal)))
      (should (eq newer-goal (mevedel-session-goal session)))
      (should (eq 'active (mevedel-goal-status newer-goal)))
      (should-not (mevedel-goal-reason newer-goal))
      (should (eq newer-context mevedel-skills--pending-request-context))
      (should-not mevedel--current-request)))

  :doc "owned prepared-skill startup failure still durably pauses its original Goal"
  (mevedel-composer-owner-test--with-skill
    (let ((check (symbol-function 'mevedel-request-assert-target-ready)))
      (cl-letf (((symbol-function 'mevedel-request-assert-target-ready)
                 (lambda (owner)
                   (funcall check owner)
                   (user-error "Owned readiness failure"))))
        (funcall resume)))
    (should-not result)
    (should (eq 'user-error (car failure)))
    (should (eq goal (plist-get failure-preparation :goal)))
    (should (eq goal (mevedel-session-goal session)))
    (should (eq 'paused (mevedel-goal-status goal)))
    (should (equal "Owned readiness failure" (mevedel-goal-reason goal)))
    (should-not mevedel-skills--pending-request-context)
    (should-not mevedel--current-request)
    (let ((saved (read (decode-coding-string
                       (mevedel-session-artifacts-read-artifact
                        session "session.meta.el" t) 'utf-8-unix))))
      (should (eq 'paused (plist-get (plist-get saved :goal) :status)))
      (should (equal "Owned readiness failure"
                     (plist-get (plist-get saved :goal) :reason))))))

(mevedel-deftest mevedel-implementation-policy-pause-failure/owned-preparation
  (:quiet t)
  ,test
  (test)
  :doc "stale refusal does not pause even when the original Goal is active again"
  (mevedel-composer-owner-test--with-skill
    (let ((preparation (mevedel-implementation-policy-capture-preparation))
          (publication (mevedel-session-publication session)))
      (should-not
       (mevedel-implementation-policy-pause-failure
        buffer '(mevedel-stale-implementation-preparation) preparation))
      (should (eq 'active (mevedel-goal-status goal)))
      (should (eq publication (mevedel-session-publication session))))))

(provide 'test-mevedel-view-composer-preparation-owner)
;;; test-mevedel-view-composer-preparation-owner.el ends here
