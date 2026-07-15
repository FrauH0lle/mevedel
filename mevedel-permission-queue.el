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
(require 'mevedel-permission-log)
(require 'mevedel-queue)

;; `mevedel-permission-prompt'
(declare-function mevedel-permission--prompt-async-attributed
                  "mevedel-permission-prompt"
                  (tool-name path include-always origin cont
                             &optional count entry))
(declare-function mevedel-permission--prompt-async-bash
                  "mevedel-permission-prompt"
                  (command dangerous include-always origin cont
                           &optional count entry))

;; `mevedel-permissions'
(declare-function mevedel-check-permission "mevedel-permissions" t t)
(declare-function mevedel-permission--checker-args
                  "mevedel-permissions" (context))
(declare-function mevedel-permission--invocation-context
                  "mevedel-permissions" (&rest args))

;; `mevedel-structs'
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-tool-exec'
(declare-function mevedel--prompt-user-for-eval "mevedel-tool-exec"
                  (expression callback &optional origin count entry
                              mode preserve-ui))
(declare-function mevedel-tools--check-bash-permission "mevedel-tool-exec"
                  (command &rest args))

;; `mevedel-workspace'
(declare-function mevedel--all-allowed-roots
                  "mevedel-workspace" (&optional buffer))

(defvar mevedel-permission-queue--spec
  (mevedel-queue-spec--create
   :name 'permission-queue
   :get (lambda (session) (mevedel-session-permission-queue session))
   :set (lambda (session queue)
          (setf (mevedel-session-permission-queue session) queue))
   :render #'mevedel-permission-queue--render-entry
   :settle (lambda (entry outcome)
             (when-let* ((cb (plist-get entry :callback)))
               (funcall cb outcome)))
   :coalesce (lambda (outcome session)
               (when (memq outcome '(allow-session deny-session always-allow))
                 (mevedel-permission-queue--coalesce outcome session)))
   :render-error-outcome (lambda (entry _error)
                           (if (eq (plist-get entry :kind) 'bash)
                               '(deny . "Bash permission UI unavailable")
                             'aborted))
   :entry-origin (lambda (entry) (plist-get entry :origin)))
  "Shared FIFO spec for the permission queue.")


(defun mevedel-permission-queue--current-session ()
  "Resolve the session struct that owns the permission queue.

Reads `mevedel--session' from the current buffer, falling back
to `mevedel--data-buffer''s buffer-local binding when present.
View buffers expose the data buffer reference but not the session struct."
  (mevedel-queue--current-session))

(defun mevedel-permission-queue--get (&optional session)
  "Return SESSION's permission-queue slot, or nil.
SESSION defaults to the current session resolved via
`mevedel-permission-queue--current-session'.  Caller must `setf'
the slot through `mevedel-session-permission-queue' to mutate."
  (when-let* ((sess (or session
                        (mevedel-permission-queue--current-session))))
    (mevedel-queue--get mevedel-permission-queue--spec sess)))

(defun mevedel-permission-queue--set (queue &optional session)
  "Set SESSION's permission-queue slot to QUEUE.
SESSION defaults to the current session."
  (when-let* ((sess (or session
                        (mevedel-permission-queue--current-session))))
    (mevedel-queue--set mevedel-permission-queue--spec sess queue)))

(defun mevedel-permission-queue--log-props (entry &rest props)
  "Return sanitized permission diagnostic properties for ENTRY plus PROPS."
  (let ((base nil))
    (dolist (key '(:kind :tool-name :specifier-key :specifier-value
                   :protected-path :resource-access :origin :command-class
                   :mode :commands-summary))
      (when (plist-member entry key)
        (setq base (plist-put base key (plist-get entry key)))))
    (when-let* ((id (mevedel-queue--entry-metadata-get
                    entry :interaction-id)))
      (setq base (plist-put base :interaction-id id)))
    (append base props)))

(defun mevedel-permission-queue--log (event entry &optional session &rest props)
  "Log permission queue EVENT for ENTRY in SESSION with PROPS."
  (when-let* ((sess (or session
                        (plist-get entry :session)
                        (mevedel-permission-queue--current-session))))
    (apply #'mevedel-permission-log
           sess event
           (apply #'mevedel-permission-queue--log-props entry props))))

(defun mevedel-permission--enqueue (entry &optional session)
  "Append ENTRY (a plist) to the session permission queue.
If the queue was empty, render ENTRY as the visible head immediately.

When SESSION is non-nil, attach ENTRY to that session explicitly.
When no session is available, settle ENTRY as aborted; the queue's
ordering and coalesce semantics require a session struct.

ENTRY plist keys:
  :kind                  -- `generic' / `bash' / `eval'
  :tool-name             -- string (`generic' only)
  :args                  -- keyword plist
  :specifier-key         -- `:path' / `:pattern' / `:domain' / `:name'
  :specifier-value       -- display path / pattern / domain
  :protected-path        -- non-nil when the original path is protected
  :resource-access       -- `read' / `write' for exact filesystem grants
  :include-always        -- boolean
  :workspace             -- workspace struct or nil
  :origin                -- \"main\" or canonical agent-id (leaf)
  :command               -- string (`bash' only)
  :analysis              -- normalized Bash analysis (`bash' only)
  :command-class         -- Bash command class (`bash' only)
  :expression            -- string (`eval' only)
  :callback              -- function: (lambda (outcome) ...)"
  (let ((origin (plist-get entry :origin)))
    (unless (and (stringp origin)
                 (or (equal origin "main")
                     (string-match-p
                      "\\`[^[:space:]]+--[[:xdigit:]]\\{32\\}\\'"
                      origin)))
      (error "Invalid permission queue origin: %S" origin)))
  (let ((session (or session (mevedel-permission-queue--current-session))))
    (mevedel-permission-queue--log 'permission-enqueued entry session)
    (mevedel-queue--enqueue mevedel-permission-queue--spec entry session)))

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
  (mevedel-queue--render-head mevedel-permission-queue--spec
                              (or session
                                  (mevedel-permission-queue--current-session))))

(defun mevedel-permission-queue--render-generic (entry)
  "Render a generic-kind permission ENTRY as the visible head."
  (require 'mevedel-permission-prompt)
  (let ((tool-name (plist-get entry :tool-name))
        (path (plist-get entry :specifier-value))
        (include-always (plist-get entry :include-always))
        (count (length (mevedel-permission-queue--get
                        (plist-get entry :session))))
        (origin (plist-get entry :origin))
        (cb (lambda (outcome)
              (mevedel-permission-queue--on-head-outcome entry outcome))))
    (mevedel-permission--prompt-async-attributed
     tool-name path include-always origin cb count entry)))

(defun mevedel-permission-queue--render-bash (entry)
  "Render a bash-kind permission ENTRY using the Bash permission UI.

Bash uses the same FIFO machinery as generic permissions; for
non-dangerous commands, `allow-session' / `always-allow' produce
pattern rules.  If the helper is unavailable, signal so the shared
queue engine removes the head and returns the pinned tool-level
denial."
  (require 'mevedel-permission-prompt)
  (let ((command (plist-get entry :command))
        (command-class (plist-get entry :command-class))
        (include-always (plist-get entry :include-always))
        (count (length (mevedel-permission-queue--get
                        (plist-get entry :session)))))
    (unless (fboundp 'mevedel-permission--prompt-async-bash)
      (error "Bash permission UI unavailable"))
    (mevedel-permission--prompt-async-bash
     command command-class include-always (plist-get entry :origin)
     (lambda (outcome)
       (mevedel-permission-queue--on-head-outcome entry outcome))
     count entry)))

(defun mevedel-permission-queue--render-eval (entry)
  "Render an eval-kind permission ENTRY using the specialized Eval UI.
Calls `mevedel--prompt-user-for-eval' with the entry's
`:expression'.  The UI returns one of `'allow-once' / `'deny-once' /
`(feedback . TEXT)' / `'aborted'; the queue passes these through
unchanged to the entry's callback (the eval slot adapter does the
final mapping)."
  (let ((expr (plist-get entry :expression))
        (mode (plist-get entry :mode))
        (preserve-ui (plist-get entry :preserve-ui))
        (origin (plist-get entry :origin))
        (count (length (mevedel-permission-queue--get
                        (plist-get entry :session)))))
    (mevedel--prompt-user-for-eval
     expr
     (lambda (outcome)
       (mevedel-permission-queue--on-head-outcome entry outcome))
     origin count entry mode preserve-ui)))

(defun mevedel-permission-queue--on-head-outcome (entry outcome)
  "Settle ENTRY with OUTCOME, then advance ENTRY's session queue.
Coalesce on rule-creating outcomes (`allow-session',
`deny-session', `always-allow').  Then render the next head.

Uses the session reference captured on ENTRY at enqueue time
rather than reading the ambient `mevedel--session', so settlement
runs correctly regardless of which buffer fired the keypress."
  (mevedel-permission-queue--log
   'permission-resolved entry nil :outcome outcome)
  (mevedel-queue--pop mevedel-permission-queue--spec entry outcome))

(defun mevedel-permission-queue--translate-coalesce-outcome (kind resolved)
  "Translate RESOLVED (`'allow' / `'deny') into the vocabulary KIND expects.
Generic entries and Bash adapters can consume `'allow' / `'deny'
from coalescing directly.  Eval does not coalesce because Eval
always asks, but keep a defensive mapping to its authoritative
queue vocabulary."
  (pcase kind
    ((or 'generic 'bash)
     ;; The pipeline's wrapper at mevedel-pipeline.el handles
     ;; `'allow' / `'deny' directly; Bash's adapter does too.
     resolved)
    ('eval
     (pcase resolved
       ('allow 'allow-once)
       ('deny 'deny-once)
       (_ resolved)))
    (_ resolved)))

(defun mevedel-permission-queue--coalesce (_rule-outcome &optional session)
  "Re-evaluate SESSION's queued entries against newly stored authority.
Entries that resolve to a non-`ask' outcome via
`mevedel-check-permission' fire their callbacks with that outcome
translated for their kind and are removed from the queue.  Entries that
still resolve to `ask' stay in place.

Protected-path and deny precedence is handled inside
`mevedel-check-permission': a protected resource needs an exact grant,
while deny rules remain final."
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
                  (let ((outcome
                         (mevedel-permission-queue--translate-coalesce-outcome
                          kind resolved)))
                    (mevedel-permission-queue--log
                     'permission-coalesced entry session
                     :resolved resolved :outcome outcome)
                    (funcall cb outcome))
                (error
                 (display-warning
                  'mevedel
                  (format "permission-queue: coalesced callback error: %S" err)
                  :warning)))))))))
    (mevedel-permission-queue--set (nreverse kept) session)))

(defun mevedel-permission-queue--reevaluate (entry)
  "Re-evaluate ENTRY through the decision chain with current rules.
Return one of `allow' / `deny' / `ask'.

Dispatches on `:kind' (generic/bash/eval).

Critical: `mevedel-check-permission' consumes session-rules,
persistent-rules, mode, and workspace-root via keyword args; it
does not read `mevedel--session'.  An earlier draft only bound
`mevedel--session' inside this function and the just-created
session rule was invisible to queued sibling re-evaluation --
the FIFO queue's central rule-coalescing was effectively a
no-op.  This function now extracts the rule context from the
entry's captured :session and passes it explicitly.

For Bash, the same context binding flows into
`mevedel-tools--check-bash-permission' via `mevedel--session'.
That function reads it directly; we let-bind to make the session visible
to it as well."
  (let* ((session (plist-get entry :session))
         (workspace
          (and session (mevedel-session-workspace session)))
         (allowed-roots
          (when (and workspace (fboundp 'mevedel--all-allowed-roots))
            (ignore-errors (mevedel--all-allowed-roots))))
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
             (let ((context
                    (mevedel-permission--invocation-context
                     :tool-name tool-name
                     :session session
                     :workspace workspace
                     :allowed-roots allowed-roots
                     :path (and (eq spec-key :path) spec-value)
                     :pattern (and (eq spec-key :pattern) spec-value)
                     :domain (and (eq spec-key :domain) spec-value)
                     :name (and (eq spec-key :name) spec-value))))
               (when-let* ((access (plist-get entry :resource-access)))
                 (setq context
                       (plist-put context :resource-access access)))
               (apply #'mevedel-check-permission
                      tool-name
                      (mevedel-permission--checker-args context)))
           (error 'ask))))
      ('bash
       (let* ((command (plist-get entry :command))
              (context
               (mevedel-permission--invocation-context
                :tool-name "Bash"
                :session session
                :workspace workspace
                :allowed-roots allowed-roots
                :pattern command))
              (rule-decision
               (condition-case _err
                   (apply #'mevedel-check-permission
                          "Bash"
                          (mevedel-permission--checker-args context))
                 (error 'ask))))
         (cond
          ((eq rule-decision 'deny) 'deny)
          ((eq rule-decision 'allow)
           (let ((safety
                  (condition-case _err
                      (mevedel-tools--check-bash-permission
                       command :trust-literal-p nil
                       :permission-context context)
                    (error 'ask))))
             (if (memq safety '(allow deny)) safety 'ask)))
          (t 'ask))))
      ('eval 'ask)
      (_ 'ask))))

(defun mevedel-permission-queue-abort-all (&optional session)
  "Flush SESSION's queue, firing `'aborted' on every entry's callback.
Called from `mevedel-abort' / request-cancel-fn."
  (let* ((session (or session (mevedel-permission-queue--current-session)))
         (queue (and session (mevedel-permission-queue--get session))))
    (dolist (entry queue)
      (mevedel-permission-queue--log
       'permission-aborted entry session :outcome 'aborted))
    (mevedel-queue--abort-all mevedel-permission-queue--spec
                              'aborted session)))

(defun mevedel-permission-queue-sweep-origin
    (origin &optional session no-render)
  "Abort queued entries for ORIGIN in SESSION.

When NO-RENDER is non-nil, do not render the next head entry after
sweeping."
  (let* ((session (or session (mevedel-permission-queue--current-session)))
         (queue (and session (mevedel-permission-queue--get session))))
    (dolist (entry queue)
      (when (equal (plist-get entry :origin) origin)
        (mevedel-permission-queue--log
         'permission-swept entry session
         :outcome 'aborted :sweep-origin origin)))
    (mevedel-queue--sweep-origin
     mevedel-permission-queue--spec
     origin 'aborted session no-render)))

(provide 'mevedel-permission-queue)

;;; mevedel-permission-queue.el ends here
