;;; niwa-implementation-policy.el -- Accepted implementation settings -*- lexical-binding: t -*-

;;; Commentary:

;; Accepted Plan settings belong to an implementation request or its durable
;; Goal, never to the session's ordinary selection.  This module owns the
;; serializable policy shape and strict model resolution.  Permission projection
;; and request admission live in their existing owners.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'niwa-structs)

;; `niwa-goal'
(declare-function niwa-goal-pause-runtime-failure
                  "niwa-goal" (buffer reason))
(autoload 'niwa-goal-pause-runtime-failure "niwa-goal")

;; `niwa-models'
(declare-function niwa-model-resolve-provider
                  "niwa-models" (spec &optional noerror))
(declare-function niwa-model-validate-effort
                  "niwa-models" (model effort))
(autoload 'niwa-model-resolve-provider "niwa-models")
(autoload 'niwa-model-validate-effort "niwa-models")

(define-error 'niwa-stale-implementation-preparation
  "Implementation context changed during preparation; submit again" 'user-error)

(defvar niwa-implementation-policy--admission-preparation nil
  "Preparation snapshot checked at policy capture during request admission.")

(defun niwa-implementation-policy-validate (policy)
  "Return serializable implementation POLICY, or signal for malformed data.
Nil means no override.  A present policy has exactly one provider, explicit
reasoning effort (nil means the model default), and canonical permission mode.
Provider availability and effort support are checked only at dispatch."
  (when policy
    (let ((keys '(:model-provider :reasoning-effort :permission-mode)))
      (unless (and (proper-list-p policy)
                   (= (length policy) (* 2 (length keys)))
                   (cl-every (lambda (key) (plist-member policy key)) keys)
                   (cl-loop for (key _) on policy by #'cddr
                            always (memq key keys))
                   (stringp (plist-get policy :model-provider))
                   (let ((label (plist-get policy :model-provider)))
                     (save-match-data
                       (and (string-match "\\`\\([^:\n]+\\):\\([^\n]+\\)\\'" label)
                            (not (string-blank-p (match-string 1 label)))
                            (not (string-blank-p (match-string 2 label))))))
                   (symbolp (plist-get policy :reasoning-effort))
                   (not (keywordp (plist-get policy :reasoning-effort)))
                   (memq (plist-get policy :permission-mode)
                         '(ask edits full-auto)))
        (error "Invalid implementation policy"))))
  policy)

(defun niwa-implementation-policy-from-selection (selection)
  "Return the implementation policy captured by accepted SELECTION."
  (niwa-implementation-policy-validate
   (list :model-provider (plist-get selection :model-provider)
         :reasoning-effort (plist-get selection :reasoning-effort)
         :permission-mode (plist-get selection :mode))))

(defun niwa-implementation-policy-model (policy)
  "Resolve POLICY's exact backend, model, and effort without fallback."
  (niwa-implementation-policy-validate policy)
  (when policy
    (let ((provider (niwa-model-resolve-provider
                     (plist-get policy :model-provider)))
          (effort (plist-get policy :reasoning-effort)))
      (niwa-model-validate-effort (plist-get provider :model) effort)
      (plist-put provider :effort effort))))

(defun niwa-implementation-policy-current (&optional session buffer)
  "Return the current request or pending root implementation policy.
SESSION must own BUFFER, which defaults to the current buffer.  Read buffer
locals without switching buffers so status redisplay stays cheap.
An admitted request keeps its captured
policy even after its Goal pauses.  Without a request or pending handoff, only
active ordinary root Goal turns inherit the Goal's policy; agents
and Plan requests do not."
  (setq buffer (or buffer (current-buffer)))
  (when (buffer-live-p buffer)
    (let* ((current (eq buffer (current-buffer)))
           (owner (if current niwa--session
                    (buffer-local-value 'niwa--session buffer)))
           (session (or session owner))
           (request (if current niwa--current-request
                      (buffer-local-value 'niwa--current-request buffer)))
           (pending (if current niwa--pending-implementation-policy
                      (buffer-local-value 'niwa--pending-implementation-policy buffer)))
           (invocation (if current niwa--agent-invocation
                         (buffer-local-value 'niwa--agent-invocation buffer))))
      (when (and session (eq session owner))
        (cond
         ((and request (eq session (niwa-request-session request)))
          (niwa-request-implementation-policy request))
         (pending)
         ((and (eq buffer (niwa-session-root-buffer session))
               (not invocation)
               (not (niwa-session-plan-mode session)))
          (when-let* ((goal (niwa-session-goal session))
                      ((eq (niwa-goal-status goal) 'active)))
            (niwa-goal-implementation-policy goal))))))))

(defun niwa-implementation-policy-capture-preparation ()
  "Capture an unadmitted ordinary root turn's implementation authority.
The record includes explicit nil policy and active accepted Goal identity.
Admitted, child and Plan requests keep their existing paths."
  (when (and niwa--session
             (eq (current-buffer) (niwa-session-root-buffer niwa--session))
             (not niwa--current-request)
             (not niwa--agent-invocation)
             (not (niwa-session-plan-mode niwa--session)))
    (let* ((policy (niwa-implementation-policy-current))
           (goal (niwa-session-goal niwa--session))
           (goal (and goal (eq (niwa-goal-status goal) 'active)
                      (niwa-goal-implementation-policy goal) goal)))
      (list :session niwa--session :policy (copy-tree policy)
            :pending niwa--pending-implementation-policy
            :goal goal :goal-id (and goal (niwa-goal-id goal))))))

(defun niwa-implementation-policy-assert-preparation (preparation)
  "Reject PREPARATION when its unadmitted root authority has changed.
Check in the source buffer.  Never replace or cancel a newer request."
  (let* ((niwa-implementation-policy--admission-preparation nil)
         (current (niwa-implementation-policy-capture-preparation)))
    (unless (and current
                 (eq (plist-get preparation :session) (plist-get current :session))
                 (equal (plist-get preparation :policy) (plist-get current :policy))
                 (eq (plist-get preparation :pending) (plist-get current :pending))
                 (eq (plist-get preparation :goal) (plist-get current :goal))
                 (equal (plist-get preparation :goal-id) (plist-get current :goal-id)))
      (signal 'niwa-stale-implementation-preparation nil))))

(cl-defun niwa-implementation-policy-pause-failure
    (buffer problem &optional (preparation nil preparation-supplied-p))
  "Pause BUFFER's active accepted-plan Goal after dispatch PROBLEM.
Read-only policy inspection does not call this helper.  Actual submission and
dispatch failures do, including failures before request admission.  Child
requests cannot pause an unrelated root Goal through this path.
When PREPARATION is supplied, only its unchanged unadmitted Goal owner may
pause.  Explicit nil owns no Goal.  Stale refusal never pauses a Goal."
  (when (and (buffer-live-p buffer)
             (not (memq 'niwa-stale-implementation-preparation
                        (get (car-safe problem) 'error-conditions))))
    (with-current-buffer buffer
      (when-let* ((session niwa--session)
                  ((eq buffer (niwa-session-root-buffer session)))
                  ((not niwa--agent-invocation))
                  (goal (niwa-session-goal session))
                  ((or (not preparation-supplied-p)
                       (and preparation
                            (not niwa--current-request)
                            (eq session (plist-get preparation :session))
                            (eq goal (plist-get preparation :goal))
                            (equal (niwa-goal-id goal)
                                   (plist-get preparation :goal-id))
                            (equal (niwa-goal-implementation-policy goal)
                                   (plist-get preparation :policy)))))
                  ((eq (niwa-goal-status goal) 'active))
                  ((niwa-goal-implementation-policy goal)))
        (niwa-goal-pause-runtime-failure
         buffer (if (stringp problem) problem (error-message-string problem)))))))

(provide 'niwa-implementation-policy)
;;; niwa-implementation-policy.el ends here
