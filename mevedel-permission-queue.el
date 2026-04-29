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
(declare-function mevedel--prompt-user-for-bash-command "mevedel-tool-exec"
                  (command callback))
(declare-function mevedel--prompt-user-for-eval "mevedel-tool-exec"
                  (expression callback))
(declare-function mevedel-check-permission "mevedel-permissions" t t)
(declare-function mevedel-tools--check-bash-permission "mevedel-tool-exec"
                  (command &key trust-literal-p))
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)

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
  "Render a bash-kind permission ENTRY using the specialized Bash UI.
Calls `mevedel--prompt-user-for-bash-command' with the entry's
`:command'.  The UI returns one of `'approve' / `'deny' /
`(feedback . TEXT)' / `'aborted'; the queue passes these through
unchanged to the entry's callback (the bash slot adapter does the
final mapping to the pipeline `cont' shape)."
  (let ((command (plist-get entry :command)))
    (mevedel--prompt-user-for-bash-command
     command
     (lambda (outcome)
       (mevedel-permission-queue--on-head-outcome entry outcome)))))

(defun mevedel-permission-queue--render-eval (entry)
  "Render an eval-kind permission ENTRY using the specialized Eval UI.
Calls `mevedel--prompt-user-for-eval' with the entry's
`:expression'.  The UI returns one of `'approve' / `'deny' /
`(feedback . TEXT)' / `'aborted'; the queue passes these through
unchanged to the entry's callback (the eval slot adapter does the
final mapping)."
  (let ((expr (plist-get entry :expression)))
    (mevedel--prompt-user-for-eval
     expr
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
  "Re-evaluate ENTRY through the decision chain with current rules.
Returns one of `allow' / `deny' / `ask'.

Dispatches on `:kind':
- `generic'  — pass entry's `:specifier-value' as `:path' (or
  whatever specifier the tool uses; for v1 the pipeline only
  populates path-shaped specifiers).
- `bash'     — pass `:command' as `:pattern' to match Bash rules.
  Additionally re-run `mevedel-tools--check-bash-permission'
  (the safety classifier) so dangerous commands cannot silently
  resolve to allow even when a session pattern rule covers them.
- `eval'     — Eval has no rule path; always returns `ask'.

The eval-by-buffer asymmetry the second reviewer flagged is
mitigated by re-binding `mevedel--session' in the coalesce caller
to the entry's `:session' (when stored).  For v1 we read from the
ambient buffer-local `mevedel--session' which the coalesce caller
already enters via the FSM's continuation buffer."
  (pcase (plist-get entry :kind)
    ('generic
     (let ((tool-name (plist-get entry :tool-name))
           (path (plist-get entry :specifier-value)))
       (condition-case _err
           (mevedel-check-permission tool-name :path path)
         (error 'ask))))
    ('bash
     (let* ((command (plist-get entry :command))
            (rule-decision
             (condition-case _err
                 (mevedel-check-permission "Bash" :pattern command)
               (error 'ask))))
       ;; Safety classifier wins.  If pattern rules say allow but
       ;; the command is dangerous / has complex syntax, return
       ;; ask so the user re-confirms.  This cannot silently auto-
       ;; allow a queued sibling that's dangerous after an
       ;; unrelated allow-session of a benign command.
       (cond
        ((eq rule-decision 'deny) 'deny)
        ((eq rule-decision 'allow)
         (let ((safety
                (condition-case _err
                    (mevedel-tools--check-bash-permission
                     command :trust-literal-p nil)
                  (error 'ask))))
           (if (memq safety '(allow deny)) safety 'ask)))
        (t 'ask))))
    ('eval
     ;; Eval has no rule path; always re-asks.  Coalesce never
     ;; resolves Eval entries.
     'ask)
    (_ 'ask)))

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
