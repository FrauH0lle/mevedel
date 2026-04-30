;;; mevedel-permission-queue.el -- Session permission FIFO queue -*- lexical-binding: t -*-

;;; Commentary:

;; Heterogeneous FIFO on the session struct holding generic
;; permission, Bash, and Eval entries.  Render-head dispatches on
;; `:kind' so a single visible prompt covers all three cases at any
;; moment.  Coalesce on rule-creating outcomes
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
                  (expression callback &optional origin))
(declare-function mevedel-check-permission "mevedel-permissions" t t)
(declare-function mevedel-tools--check-bash-permission "mevedel-tool-exec"
                  (command &key trust-literal-p))
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

(defvar mevedel--session)


(defun mevedel-permission-queue--current-session ()
  "Resolve the session struct that owns the permission queue.
Reads `mevedel--session' from the current buffer, falling back
to `mevedel--data-buffer''s buffer-local binding when present
(view buffers expose the data buffer reference but not the
session struct)."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer) mevedel--data-buffer
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-permission-queue--get (&optional session)
  "Return SESSION's permission-queue slot, or nil.
SESSION defaults to the current session resolved via
`mevedel-permission-queue--current-session'.  Caller must `setf'
the slot through `mevedel-session-permission-queue' to mutate."
  (when-let* ((sess (or session
                        (mevedel-permission-queue--current-session))))
    (mevedel-session-permission-queue sess)))

(defun mevedel-permission-queue--set (queue &optional session)
  "Set SESSION's permission-queue slot to QUEUE.
SESSION defaults to the current session."
  (when-let* ((sess (or session
                        (mevedel-permission-queue--current-session))))
    (setf (mevedel-session-permission-queue sess) queue)))

(defun mevedel-permission--enqueue (entry)
  "Append ENTRY (a plist) to the session permission queue.
If the queue was empty, render ENTRY as the visible head immediately.

When no session is in context (degraded mode, e.g. tests or
unusual dispatches that the pipeline already warns about), bypass
the queue entirely and render ENTRY directly -- the queue's
ordering and coalesce semantics require a session struct to
attach to.  The user-visible behavior in the no-session path
matches the legacy direct-prompt behavior.

ENTRY plist keys:
  :kind                  -- `generic' / `bash' / `eval'
  :tool-name             -- string (`generic' only)
  :args                  -- keyword plist
  :specifier-key         -- `:path' / `:pattern' / `:domain' / `:name'
  :specifier-value       -- display path / pattern / domain
  :protected-path        -- non-nil when the original path is protected
  :include-always        -- boolean
  :workspace             -- workspace struct or nil
  :origin                -- \"main\" or canonical agent-id (leaf)
  :command               -- string (`bash' only)
  :dangerous             -- boolean (`bash' only)
  :expression            -- string (`eval' only)
  :callback              -- function: (lambda (outcome) ...)"
  (let ((session (mevedel-permission-queue--current-session)))
    (cond
     ((not session)
      ;; No session in context: render directly without queueing.
      ;; Preserve direct prompting for the degenerate case.
      (mevedel-permission-queue--render-entry entry))
     (t
      ;; Capture the session on the entry so settlement runs in the
      ;; correct context regardless of which buffer fires the user
      ;; keypress (overlays render in the view buffer, which doesn't
      ;; bind mevedel--session locally).
      (let* ((entry (plist-put entry :session session))
             (q (mevedel-permission-queue--get session))
             (was-empty (null q))
             (new-q (append q (list entry))))
        (mevedel-permission-queue--set new-q session)
        (when was-empty
          (mevedel-permission-queue--render-head session)))))))

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

(defun mevedel-permission-queue--render-head (&optional session)
  "Render the current head of SESSION's permission queue.
Dispatches on entry's `:kind' via `--render-entry'."
  (when-let* ((q (mevedel-permission-queue--get session))
              (head (car q)))
    (mevedel-permission-queue--render-entry head)))

(defun mevedel-permission-queue--render-generic (entry)
  "Render a generic-kind permission ENTRY as the visible head.
Routes through `mevedel-permission--prompt-async' (the
overlay primitive) which itself delegates to
`mevedel-permission--prompt-async-attributed' with origin=nil
when none is provided.  When an origin is set on the entry
(sub-agent dispatch), bypass the legacy entry point and call the
attributed variant directly so the attribution line renders."
  (let ((tool-name (plist-get entry :tool-name))
        (path (plist-get entry :specifier-value))
        (include-always (plist-get entry :include-always))
        (origin (plist-get entry :origin))
        (cb (lambda (outcome)
              (mevedel-permission-queue--on-head-outcome entry outcome))))
    (if (and origin (not (equal origin "main"))
             (fboundp 'mevedel-permission--prompt-async-attributed))
        (mevedel-permission--prompt-async-attributed
         tool-name path include-always origin cb)
      (mevedel-permission--prompt-async
       tool-name path include-always cb))))

(defun mevedel-permission-queue--render-bash (entry)
  "Render a bash-kind permission ENTRY using the 5-button UI.

Bash uses the same FIFO and 5-button machinery as generic
permissions, so `allow-session' / `always-allow' produce pattern
rules.  Falls back to the legacy approve/deny/feedback/abort
overlay (`mevedel--prompt-user-for-bash-command') when the
5-button helper isn't available -- defensive guard for buffers
where the new overlay primitive hasn't loaded."
  (let ((command (plist-get entry :command))
        (dangerous (plist-get entry :dangerous))
        (include-always (plist-get entry :include-always)))
    (cond
     ((fboundp 'mevedel-permission--prompt-async-bash)
      (mevedel-permission--prompt-async-bash
       command dangerous include-always (plist-get entry :origin)
       (lambda (outcome)
         (mevedel-permission-queue--on-head-outcome entry outcome))))
     (t
      (mevedel--prompt-user-for-bash-command
       command
       (lambda (outcome)
         (mevedel-permission-queue--on-head-outcome entry outcome)))))))

(defun mevedel-permission-queue--render-eval (entry)
  "Render an eval-kind permission ENTRY using the specialized Eval UI.
Calls `mevedel--prompt-user-for-eval' with the entry's
`:expression'.  The UI returns one of `'approve' / `'deny' /
`(feedback . TEXT)' / `'aborted'; the queue passes these through
unchanged to the entry's callback (the eval slot adapter does the
final mapping)."
  (let ((expr (plist-get entry :expression))
        (origin (plist-get entry :origin)))
    (mevedel--prompt-user-for-eval
     expr
     (lambda (outcome)
       (mevedel-permission-queue--on-head-outcome entry outcome))
     origin)))

(defun mevedel-permission-queue--on-head-outcome (entry outcome)
  "Settle ENTRY with OUTCOME, then advance ENTRY's session queue.
Coalesce on rule-creating outcomes (`allow-session',
`deny-session', `always-allow').  Then render the next head.

Uses the session reference captured on ENTRY at enqueue time
rather than reading the ambient `mevedel--session', so settlement
runs correctly regardless of which buffer fired the keypress."
  (let ((session (plist-get entry :session))
        (cb (plist-get entry :callback)))
    ;; Fire the head's callback with the outcome.
    (when (functionp cb)
      (condition-case err
          (funcall cb outcome)
        (error
         (display-warning
          'mevedel
          (format "permission-queue: head callback error: %S" err)
          :warning))))
    ;; Drop the head from the queue.
    (let ((q (mevedel-permission-queue--get session)))
      (mevedel-permission-queue--set (cdr q) session))
    ;; Coalesce queued siblings against the new rule, if any.
    (pcase outcome
      ((or 'allow-session 'deny-session 'always-allow)
       (mevedel-permission-queue--coalesce outcome session)))
    ;; Render the next head, if any.
    (mevedel-permission-queue--render-head session)))

(defun mevedel-permission-queue--translate-coalesce-outcome (kind resolved)
  "Translate RESOLVED (`'allow' / `'deny') into the vocabulary KIND expects.
Generic entries' callbacks were written to receive `'allow-once'
/ `'deny-once' / etc. from the prompt UI.  Bash and Eval slot
adapters expect `'approve' / `'deny'.  Without this translation,
a coalesced `'allow' from `mevedel-check-permission' falls
through to the adapter's catch-all `'deny' clause and silently
denies an entry the rules would otherwise auto-allow."
  (pcase kind
    ('generic
     ;; The pipeline's wrapper at mevedel-pipeline.el handles
     ;; `'allow' / `'deny' directly via the dispatch helper.
     resolved)
    ((or 'bash 'eval)
     (pcase resolved
       ('allow 'approve)
       ('deny 'deny)
       (_ resolved)))
    (_ resolved)))

(defun mevedel-permission-queue--coalesce (rule-outcome &optional session)
  "Re-evaluate SESSION's queued entries against the new rule.
Entries that resolve to a non-`ask' outcome via
`mevedel-check-permission' fire their callbacks with that outcome
(translated per kind) and are removed from the queue; entries
that still resolve to `ask' stay in place.

The protected-path / deny-precedence nuance is handled inside
`mevedel-check-permission' itself: protected paths short-circuit
allow rules but not deny rules, so re-evaluation produces the
correct coalesce semantics without an explicit flag on the
entry."
  (ignore rule-outcome)
  (let ((q (mevedel-permission-queue--get session))
        (kept nil))
    (dolist (entry q)
      (let ((resolved (mevedel-permission-queue--reevaluate entry)))
        (cond
         ((eq resolved 'ask)
          (push entry kept))
         (t
          (let ((cb (plist-get entry :callback))
                (kind (plist-get entry :kind)))
            (when (functionp cb)
              (condition-case err
                  (funcall cb
                           (mevedel-permission-queue--translate-coalesce-outcome
                            kind resolved))
                (error
                 (display-warning
                  'mevedel
                  (format "permission-queue: coalesced callback error: %S" err)
                  :warning)))))))))
    (mevedel-permission-queue--set (nreverse kept) session)))

(defun mevedel-permission-queue--reevaluate (entry)
  "Re-evaluate ENTRY through the decision chain with current rules.
Returns one of `allow' / `deny' / `ask'.

Dispatches on `:kind' (generic/bash/eval).

Critical: `mevedel-check-permission' consumes session-rules,
persistent-rules, mode, and workspace-root via keyword args; it
does NOT read `mevedel--session'.  An earlier draft only bound
`mevedel--session' inside this function and the just-created
session rule was invisible to queued sibling re-evaluation --
the FIFO queue's central rule-coalescing was effectively a
no-op.  This function now extracts the rule context from the
entry's captured :session and passes it explicitly.

For Bash, the same context binding flows into
`mevedel-tools--check-bash-permission' via `mevedel--session'
(that function reads it directly); we let-bind to make the
session visible to it as well."
  (let* ((session (plist-get entry :session))
         (session-rules
          (and session (mevedel-session-permission-rules session)))
         (mode
          (and session (mevedel-session-permission-mode session)))
         (workspace
          (and session (mevedel-session-workspace session)))
         (workspace-root
          (and workspace (mevedel-workspace-root workspace)))
         ;; Bind ambient mevedel--session so the Bash safety
         ;; classifier (which reads mevedel--session directly)
         ;; sees the captured context too.
         (mevedel--session (or session
                               (and (boundp 'mevedel--session)
                                    mevedel--session))))
    (pcase (plist-get entry :kind)
      ('generic
       (let ((tool-name (plist-get entry :tool-name))
             (spec-key (or (plist-get entry :specifier-key) :path))
             (spec-value (plist-get entry :specifier-value)))
         (condition-case _err
             (apply #'mevedel-check-permission
                    tool-name
                    (append
                     (and spec-key spec-value (list spec-key spec-value))
                     (list :session-rules session-rules
                           :mode mode
                           :workspace-root workspace-root)))
           (error 'ask))))
      ('bash
       (let* ((command (plist-get entry :command))
              (rule-decision
               (condition-case _err
                   (mevedel-check-permission
                    "Bash"
                    :pattern command
                    :session-rules session-rules
                    :mode mode
                    :workspace-root workspace-root)
                 (error 'ask))))
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
      ('eval 'ask)
      (_ 'ask))))

(defun mevedel-permission-queue-abort-all (&optional session)
  "Flush SESSION's queue, firing `'aborted' on every entry's callback.
Called from `mevedel-abort' / request-cancel-fn."
  (let ((q (mevedel-permission-queue--get session)))
    (mevedel-permission-queue--set nil session)
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

(defun mevedel-permission-queue-sweep-agent (origin &optional session)
  "Fire `'aborted' on queued entries whose `:origin' matches ORIGIN.
Called when an agent enters a terminal state with entries it had
queued; the agent's FSM has unwound and nothing would consume the
answer."
  (let* ((q-before (mevedel-permission-queue--get session))
         (head-before (car q-before))
         (kept nil))
    (dolist (entry q-before)
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
    (let ((kept-q (nreverse kept)))
      (mevedel-permission-queue--set kept-q session)
      ;; Only re-render when the head actually changed (the live
      ;; overlay otherwise stacks a duplicate on top of itself).
      (when (and kept-q
                 (not (eq head-before (car kept-q))))
        (mevedel-permission-queue--render-head session)))))

(provide 'mevedel-permission-queue)

;;; mevedel-permission-queue.el ends here
