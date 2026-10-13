;;; mevedel-implementation-policy.el -- Accepted implementation settings -*- lexical-binding: t -*-

;;; Commentary:

;; Accepted Plan settings belong to an implementation request or its durable
;; Goal, never to the session's ordinary selection.  This module owns the
;; serializable policy shape and strict model resolution.  Permission projection
;; and request admission live in their existing owners.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mevedel-structs)

;; `mevedel-goal'
(declare-function mevedel-goal-pause-runtime-failure
                  "mevedel-goal" (buffer reason))
(autoload 'mevedel-goal-pause-runtime-failure "mevedel-goal")

;; `mevedel-models'
(declare-function mevedel-model-resolve-provider
                  "mevedel-models" (spec &optional noerror))
(declare-function mevedel-model-validate-effort
                  "mevedel-models" (model effort))
(autoload 'mevedel-model-resolve-provider "mevedel-models")
(autoload 'mevedel-model-validate-effort "mevedel-models")

(define-error 'mevedel-stale-implementation-preparation
  "Implementation context changed during preparation; submit again" 'user-error)

(defvar mevedel-implementation-policy--admission-preparation nil
  "Preparation snapshot checked at policy capture during request admission.")

(defun mevedel-implementation-policy-validate (policy)
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

(defun mevedel-implementation-policy-from-selection (selection)
  "Return the implementation policy captured by accepted SELECTION."
  (mevedel-implementation-policy-validate
   (list :model-provider (plist-get selection :model-provider)
         :reasoning-effort (plist-get selection :reasoning-effort)
         :permission-mode (plist-get selection :mode))))

(defun mevedel-implementation-policy-model (policy)
  "Resolve POLICY's exact backend, model, and effort without fallback."
  (mevedel-implementation-policy-validate policy)
  (when policy
    (let ((provider (mevedel-model-resolve-provider
                     (plist-get policy :model-provider)))
          (effort (plist-get policy :reasoning-effort)))
      (mevedel-model-validate-effort (plist-get provider :model) effort)
      (plist-put provider :effort effort))))

(defun mevedel-implementation-policy-current (&optional session buffer)
  "Return the current request or pending root implementation policy.
SESSION must own BUFFER, which defaults to the current buffer.  Read buffer
locals without switching buffers so status redisplay stays cheap.
An admitted request keeps its captured
policy even after its Goal pauses.  Without a request or pending handoff, only
active ordinary root Goal turns inherit the Goal's policy; directives, agents,
and Plan requests do not."
  (setq buffer (or buffer (current-buffer)))
  (when (buffer-live-p buffer)
    (let* ((current (eq buffer (current-buffer)))
           (owner (if current mevedel--session
                    (buffer-local-value 'mevedel--session buffer)))
           (session (or session owner))
           (request (if current mevedel--current-request
                      (buffer-local-value 'mevedel--current-request buffer)))
           (pending (if current mevedel--pending-implementation-policy
                      (buffer-local-value 'mevedel--pending-implementation-policy buffer)))
           (invocation (if current mevedel--agent-invocation
                         (buffer-local-value 'mevedel--agent-invocation buffer)))
           (directive (if current mevedel--current-directive-uuid
                        (buffer-local-value 'mevedel--current-directive-uuid buffer))))
      (when (and session (eq session owner))
        (cond
         ((and request (eq session (mevedel-request-session request)))
          (mevedel-request-implementation-policy request))
         (pending)
         ((and (eq buffer (mevedel-session-root-buffer session))
               (not invocation)
               (not directive)
               (not (mevedel-session-plan-mode session)))
          (when-let* ((goal (mevedel-session-goal session))
                      ((eq (mevedel-goal-status goal) 'active)))
            (mevedel-goal-implementation-policy goal))))))))

(defun mevedel-implementation-policy-capture-preparation ()
  "Capture an unadmitted ordinary root turn's implementation authority.
The record includes explicit nil policy and active accepted Goal identity.
Admitted, child, directive and Plan requests keep their existing paths."
  (when (and mevedel--session
             (eq (current-buffer) (mevedel-session-root-buffer mevedel--session))
             (not mevedel--current-request)
             (not mevedel--agent-invocation)
             (not mevedel--current-directive-uuid)
             (not (mevedel-session-plan-mode mevedel--session)))
    (let* ((policy (mevedel-implementation-policy-current))
           (goal (mevedel-session-goal mevedel--session))
           (goal (and goal (eq (mevedel-goal-status goal) 'active)
                      (mevedel-goal-implementation-policy goal) goal)))
      (list :session mevedel--session :policy (copy-tree policy)
            :pending mevedel--pending-implementation-policy
            :goal goal :goal-id (and goal (mevedel-goal-id goal))))))

(defun mevedel-implementation-policy-assert-preparation (preparation)
  "Reject PREPARATION when its unadmitted root authority has changed.
Check in the source buffer.  Never replace or cancel a newer request."
  (let* ((mevedel-implementation-policy--admission-preparation nil)
         (current (mevedel-implementation-policy-capture-preparation)))
    (unless (and current
                 (eq (plist-get preparation :session) (plist-get current :session))
                 (equal (plist-get preparation :policy) (plist-get current :policy))
                 (eq (plist-get preparation :pending) (plist-get current :pending))
                 (eq (plist-get preparation :goal) (plist-get current :goal))
                 (equal (plist-get preparation :goal-id) (plist-get current :goal-id)))
      (signal 'mevedel-stale-implementation-preparation nil))))

(cl-defun mevedel-implementation-policy-pause-failure
    (buffer problem &optional (preparation nil preparation-supplied-p))
  "Pause BUFFER's active accepted-plan Goal after dispatch PROBLEM.
Read-only policy inspection does not call this helper.  Actual submission and
dispatch failures do, including failures before request admission.  Child and
directive requests cannot pause an unrelated root Goal through this path.
When PREPARATION is supplied, only its unchanged unadmitted Goal owner may
pause.  Explicit nil owns no Goal.  Stale refusal never pauses a Goal."
  (when (and (buffer-live-p buffer)
             (not (memq 'mevedel-stale-implementation-preparation
                        (get (car-safe problem) 'error-conditions))))
    (with-current-buffer buffer
      (when-let* ((session mevedel--session)
                  ((eq buffer (mevedel-session-root-buffer session)))
                  ((not mevedel--agent-invocation))
                  ((not mevedel--current-directive-uuid))
                  (goal (mevedel-session-goal session))
                  ((or (not preparation-supplied-p)
                       (and preparation
                            (not mevedel--current-request)
                            (eq session (plist-get preparation :session))
                            (eq goal (plist-get preparation :goal))
                            (equal (mevedel-goal-id goal)
                                   (plist-get preparation :goal-id))
                            (equal (mevedel-goal-implementation-policy goal)
                                   (plist-get preparation :policy)))))
                  ((eq (mevedel-goal-status goal) 'active))
                  ((mevedel-goal-implementation-policy goal)))
        (mevedel-goal-pause-runtime-failure
         buffer (if (stringp problem) problem (error-message-string problem)))))))

(provide 'mevedel-implementation-policy)
;;; mevedel-implementation-policy.el ends here
