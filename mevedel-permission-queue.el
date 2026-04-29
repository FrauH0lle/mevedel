;;; mevedel-permission-queue.el -- Session permission FIFO queue -*- lexical-binding: t -*-

;;; Commentary:

;; Spec 23 permission queue.  Heterogeneous FIFO on the session struct
;; holding generic permission, Bash, and Eval entries.  Render-head
;; dispatches on `:kind' so a single visible prompt covers all three
;; cases at any moment.  Coalesce on rule-creating outcomes
;; (`allow-session', `deny-session', `always-allow') re-evaluates
;; queued entries through the decision chain; protected paths skip
;; allow rules but coalesce on deny.  Per-agent terminal-state sweep
;; fires `'aborted' on entries owned by an agent that has unwound.
;;
;; The queue is transient runtime state: never persisted to the
;; sidecar, empty at every completed-turn boundary.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-structs)

(declare-function mevedel-permission--prompt-async "mevedel-tool-ui"
                  (tool-name path include-always cont))
(declare-function mevedel-check-permission "mevedel-permissions" t t)

(defvar mevedel--session)


(defun mevedel-permission-queue--get ()
  "Return the current session's permission-queue slot, or nil.
Caller must `setf' the slot through `mevedel-session-permission-queue'
to mutate."
  (when (and (boundp 'mevedel--session) mevedel--session)
    (mevedel-session-permission-queue mevedel--session)))

(defun mevedel-permission-queue--set (queue)
  "Set the current session's permission-queue slot to QUEUE."
  (when (and (boundp 'mevedel--session) mevedel--session)
    (setf (mevedel-session-permission-queue mevedel--session) queue)))

(defun mevedel-permission--enqueue (entry)
  "Append ENTRY (a plist) to the session permission queue.
If the queue was empty, render ENTRY as the visible head immediately.

When no session is in context (degraded mode, e.g. tests or
unusual dispatches that the pipeline already warns about), bypass
the queue entirely and render ENTRY directly — the queue's
ordering and coalesce semantics require a session struct to
attach to.  The user-visible behavior in the no-session path
matches the pre-spec-23 direct-prompt behavior.

ENTRY plist keys (per spec 23):
  :kind                  — `generic' / `bash' / `eval'
  :tool-name             — string (`generic' only)
  :args                  — keyword plist
  :specifier-value       — display path / pattern / domain
  :include-always        — boolean
  :workspace             — workspace struct or nil
  :origin                — \"main\" or canonical agent-id (leaf)
  :command               — string (`bash' only)
  :dangerous             — boolean (`bash' only)
  :expression            — string (`eval' only)
  :callback              — function: (lambda (outcome) ...)"
  (cond
   ((not (and (boundp 'mevedel--session) mevedel--session))
    ;; No session — render directly without queueing.
    (mevedel-permission-queue--render-entry entry))
   (t
    (let* ((q (mevedel-permission-queue--get))
           (was-empty (null q))
           (new-q (append q (list entry))))
      (mevedel-permission-queue--set new-q)
      (when was-empty
        (mevedel-permission-queue--render-head))))))

(defun mevedel-permission-queue--render-entry (entry)
  "Render ENTRY directly via the kind-specific dispatcher.
Used by the queue's render-head and by the no-session fallback."
  (pcase (plist-get entry :kind)
    ('generic (mevedel-permission-queue--render-generic entry))
    ('bash (mevedel-permission-queue--render-bash entry))
    ('eval (mevedel-permission-queue--render-eval entry))
    (_
     (display-warning
      'mevedel
      (format "permission-queue: unknown :kind %S, dropping"
              (plist-get entry :kind))
      :warning)
     (let ((cb (plist-get entry :callback)))
       (when (functionp cb)
         (condition-case _ (funcall cb 'aborted) (error nil)))))))

(defun mevedel-permission-queue--render-head ()
  "Render the current head of the permission queue into the interaction zone.
Dispatches on entry's `:kind' via `--render-entry'."
  (when-let* ((q (mevedel-permission-queue--get))
              (head (car q)))
    (mevedel-permission-queue--render-entry head)))

(defun mevedel-permission-queue--render-generic (entry)
  "Render a generic-kind permission ENTRY as the visible head."
  (let ((tool-name (plist-get entry :tool-name))
        (path (plist-get entry :specifier-value))
        (include-always (plist-get entry :include-always)))
    (mevedel-permission--prompt-async
     tool-name path include-always
     (lambda (outcome)
       (mevedel-permission-queue--on-head-outcome entry outcome)))))

(defun mevedel-permission-queue--render-bash (entry)
  "Render a bash-kind permission ENTRY as the visible head.
For now, defers to the generic prompt with the command as the
displayed path; specialized Bash UI lands when Bash handler is
migrated to the queue (Phase 4)."
  (let ((command (plist-get entry :command))
        (include-always (plist-get entry :include-always)))
    (mevedel-permission--prompt-async
     "Bash" command include-always
     (lambda (outcome)
       (mevedel-permission-queue--on-head-outcome entry outcome)))))

(defun mevedel-permission-queue--render-eval (entry)
  "Render an eval-kind permission ENTRY as the visible head.
For now, defers to the generic prompt; specialized Eval UI (with
feedback support, no session/persistent buttons) lands when Eval
handler is migrated to the queue (Phase 4)."
  (let ((expr (plist-get entry :expression)))
    (mevedel-permission--prompt-async
     "Eval" expr nil ; no include-always for Eval (no rule path)
     (lambda (outcome)
       (mevedel-permission-queue--on-head-outcome entry outcome)))))

(defun mevedel-permission-queue--on-head-outcome (entry outcome)
  "Settle ENTRY with OUTCOME, then advance the queue.
Coalesce on rule-creating outcomes (`allow-session',
`deny-session', `always-allow').  Then render the next head."
  ;; First, fire the head's own callback with the outcome.
  ;; The pipeline-side `apply-prompt-result' does the rule write;
  ;; the callback is what enqueue-callers passed in and is
  ;; expected to translate to their pipeline format.
  (let ((cb (plist-get entry :callback)))
    (when (functionp cb)
      (condition-case err
          (funcall cb outcome)
        (error
         (display-warning
          'mevedel
          (format "permission-queue: head callback error: %S" err)
          :warning)))))
  ;; Drop the head from the queue.
  (let ((q (mevedel-permission-queue--get)))
    (mevedel-permission-queue--set (cdr q)))
  ;; Coalesce queued siblings against the new rule, if any.
  (pcase outcome
    ((or 'allow-session 'deny-session 'always-allow)
     (mevedel-permission-queue--coalesce outcome)))
  ;; Render the next head, if any.
  (mevedel-permission-queue--render-head))

(defun mevedel-permission-queue--coalesce (rule-outcome)
  "Re-evaluate queued entries against the rule produced by RULE-OUTCOME.
Entries that resolve to a non-`ask' outcome via
`mevedel-check-permission' fire their callbacks with that outcome
and are removed from the queue; entries that still resolve to
`ask' stay in place.

The protected-path / deny-precedence nuance is handled inside
`mevedel-check-permission' itself: protected paths short-circuit
allow rules but not deny rules, so re-evaluation produces the
correct coalesce semantics without an explicit flag on the
entry."
  (ignore rule-outcome)
  (let ((q (mevedel-permission-queue--get))
        (kept nil))
    (dolist (entry q)
      (let ((resolved (mevedel-permission-queue--reevaluate entry)))
        (cond
         ((eq resolved 'ask)
          (push entry kept))
         (t
          (let ((cb (plist-get entry :callback)))
            (when (functionp cb)
              (condition-case err
                  (funcall cb resolved)
                (error
                 (display-warning
                  'mevedel
                  (format "permission-queue: coalesced callback error: %S" err)
                  :warning)))))))))
    (mevedel-permission-queue--set (nreverse kept))))

(defun mevedel-permission-queue--reevaluate (entry)
  "Re-evaluate ENTRY through `mevedel-check-permission' with current rules.
Returns one of `allow' / `deny' / `ask' (per the decision chain)."
  (let ((tool-name (or (plist-get entry :tool-name)
                       (pcase (plist-get entry :kind)
                         ('bash "Bash")
                         ('eval "Eval"))))
        (args (plist-get entry :args)))
    (condition-case _err
        (mevedel-check-permission tool-name args)
      (error 'ask))))

(defun mevedel-permission-queue-abort-all ()
  "Flush the queue, firing `'aborted' on every entry's callback.
Called from `mevedel-abort' / request-cancel-fn."
  (let ((q (mevedel-permission-queue--get)))
    (mevedel-permission-queue--set nil)
    (dolist (entry q)
      (let ((cb (plist-get entry :callback)))
        (when (functionp cb)
          (condition-case err
              (funcall cb 'aborted)
            (error
             (display-warning
              'mevedel
              (format "permission-queue: abort callback error: %S" err)
              :warning))))))))

(defun mevedel-permission-queue-sweep-agent (origin)
  "Fire `'aborted' on queued entries whose `:origin' matches ORIGIN.
Called when an agent enters a terminal state with entries it had
queued; the agent's FSM has unwound and nothing would consume the
answer."
  (let ((q (mevedel-permission-queue--get))
        (kept nil))
    (dolist (entry q)
      (cond
       ((equal (plist-get entry :origin) origin)
        (let ((cb (plist-get entry :callback)))
          (when (functionp cb)
            (condition-case err
                (funcall cb 'aborted)
              (error
               (display-warning
                'mevedel
                (format "permission-queue: sweep callback error: %S" err)
                :warning))))))
       (t
        (push entry kept))))
    (mevedel-permission-queue--set (nreverse kept))
    ;; If the head was swept, render the next head.
    (when (and q kept)
      (mevedel-permission-queue--render-head))))

(provide 'mevedel-permission-queue)

;;; mevedel-permission-queue.el ends here
