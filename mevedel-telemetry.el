;;; mevedel-telemetry.el -- Durable session telemetry -*- lexical-binding: t -*-

;;; Commentary:

;; Records a versioned, append-only stream of bounded lifecycle events in each
;; materialized session.  Telemetry is diagnostic evidence rather than session
;; state: it is never read during resume and failures never block the workflow.
;; Payload-bearing fields are rejected at this boundary so prompts, commands,
;; tool arguments, model responses, hook output, and environment values cannot
;; leak into the log through an accidental call site.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `emacs'
(defvar default-file-modes)

;; `gptel'
(defvar gptel--log-buffer-name)
(defvar gptel-log-level)
(defvar gptel-version)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

;; `mevedel-sandbox'
(declare-function mevedel-sandbox-probe "mevedel-sandbox" ())

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-append-diagnostic
                  "mevedel-session-publication" (session path content))

;; `mevedel-structs'
(declare-function mevedel-goal-id "mevedel-structs" (cl-x))
(declare-function mevedel-goal-p "mevedel-structs" (cl-x))
(declare-function mevedel-goal-status "mevedel-structs" (cl-x))
(declare-function mevedel-goal-tokens-used "mevedel-structs" (cl-x))
(declare-function mevedel-goal-turns-run "mevedel-structs" (cl-x))
(declare-function mevedel-session-audit-target "mevedel-structs" (session))
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x))
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x))
(declare-function mevedel-session-sandbox-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x))
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-telemetry-pending
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x))
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(defvar mevedel--agent-invocation)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-view-render'
(defvar mevedel-view-render-debug)
(defvar mevedel-view-render-debug-buffer-name)

;; `profiler'
(declare-function profiler-cpu-profile "profiler" ())
(declare-function profiler-fixup-profile "profiler" (profile))
(declare-function profiler-memory-profile "profiler" ())
(declare-function profiler-report-setup-buffer "profiler" (profile))
(declare-function profiler-start "profiler" (mode))
(declare-function profiler-stop "profiler" ())
(defvar profiler-cpu-log)
(defvar profiler-max-stack-depth)
(defvar profiler-memory-log)


;;
;;; Customization

(defcustom mevedel-telemetry-enabled t
  "When non-nil, record bounded lifecycle telemetry for each session."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-telemetry-file-name "telemetry-log.el"
  "File name for the append-only per-session telemetry stream."
  :type 'string
  :group 'mevedel)

(defcustom mevedel-telemetry-max-string-length 512
  "Maximum number of characters retained in one telemetry string value."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-telemetry-profiler-fail-on-prompt t
  "When non-nil, profiler runs log and reject synchronous prompt functions.
This turns hidden compaction, file-conflict, and edit confirmations into an
explicit reproduction failure instead of unmeasured user-wait time."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-telemetry-profiler-stack-depth 64
  "Backtrace depth captured by a mevedel profiler run.

The native default of 16 truncates before a mevedel frame is reached: a turn
runs through the view, the FSM, gptel, and a TRAMP handler before it costs
anything measurable, so a truncated sample records that the time went to
`file-attributes' without recording which mevedel call site asked for it.
Attribution is the whole point of these artifacts.  Deeper samples make the
profile file larger and cost a little more per sample."
  :type 'integer
  :group 'mevedel)


;;
;;; Event representation

(defconst mevedel-telemetry-schema-version 1
  "Schema version written into every telemetry event.")

(defconst mevedel-telemetry--allowed-keys
  '(:abort-plan-approval :active-work-paused :additional-read-count
    :additional-write-count :admitted :after-confined-launch-failure
    :agent-id :agent-path :agent-type :aggressive :artifacts-directory
    :artifacts-local :backend :baseline-marker-position
    :baseline-request-id :blocked :boundary :bubblewrap-available :bucket
    :budget-kind :budget-status :buffer-chars-model-visible :buffer-chars-total
    :buffers :cache-identity :cached-tokens :call-source :captured-goal-id
    :chosen-active-context-tokens :chosen-source :chunk-bytes
    :command-class :command-hash :context :context-chars
    :context-deduplicated :continuation :conversation-scope
    :cumulative-usage :cumulative-usage-tokens :dequeue-goal-id
    :dirty-content-hash :dirty-file-count :dirty-state-hash :duration-ms
    :effective-wait-ms :effort :emacs-version :enqueue-goal-id
    :error-class :estimate :estimate-source :execution-id :exit-code
    :exit-status :failure-class :failure-stage :fallback-offered
    :fallback-possible :filesystem :first-byte-seen
    :fresh-visible-prompt-estimate :full-execution-approval-offered
    :git-head :goal-id :gptel-agent-commit :gptel-agent-file-hash
    :gptel-commit :gptel-file-hash :gptel-version :handler-count
    :handler-id :handler-source :handler-type :hook-event
    :ineligible-reason :input-p :input-tokens :interaction-id :issue-count
    :kind :lane :launch-failure-reason-class :launch-failure-stage
    :message-chars :message-hash :mode :model :model-context-window :modes
    :native-resource-capture :native-resource-report-bytes :nested-call-count
    :network
    :new-count :new-segment :old-segment :omitted-count :origin :outcome
    :output-bytes :output-limit :output-tokens :overlap-count :owner
    :parent-tool-use-id :parent-turn :pending-count :permission-mode
    :permission-mode-base
    :permission-mode-effective :preexisting-count :preparation-state
    :previous-owner :previous-status :proc :profile :profile-bytes-total
    :profile-file-names :prompt-chars :prompt-function :prompt-hash
    :protected-path-count :provider-context-model :provider-context-status
    :provider-context-tokens :provider-context-usage
    :provider-context-window :provider-status :purpose :queue-depth
    :queue-depth-before :queue-duration-ms :read-only :reason
    :reason-class :repair-count :report-bytes-total :report-file-names
    :request-id :requested-yield-time-ms :resolved-count :resource-access
    :restored :result-bytes :result-chars :retained :roster-chars :rounds
    :sandbox :sandbox-mode :sandbox-permissions :scope :settled
    :skill-count :skill-name :skill-names :skip-gates :span-id
    :specifier-key :stage :status :step :summary-threshold
    :system-configuration :target-model :target-origin :target-pressure
    :target-threshold :termination :test-scope :threshold :threshold-ms
    :timed-out :timeout-ms :token-source :tokens-after :tokens-before
    :tokens-used :tool-name :tool-use-id :trigger :tty :turns-run :via
    :workload :yield-time-ms)
  "Metadata keys telemetry may persist.

An allowlist, not a denylist: telemetry is durable and the data policy in
`docs/telemetry.md' permits only lifecycle metadata, sizes, classifications,
hashes, and bounded identifiers.  A denylist has to name every field that
could carry a prompt, a command, a path, or a tool result, so any caller
naming a field something new leaks by default.  Keys outside this list are
dropped and their names -- names only -- reported in `:dropped-keys', which
is what a new caller sees instead of silence.
`mevedel-execution-telemetry--audit-prop-keys' filters the same way for
forwarded audits.")

(defconst mevedel-telemetry--owned-keys
  '(:schema-version :time :elapsed-ms :sequence :event :session-id :turn
    :preset :goal-id :goal-cycle :goal-phase :goal-status :profiler-run-id
    :dropped-keys)
  "Envelope keys supplied by `mevedel-telemetry-record'.")

(defun mevedel-telemetry--monotonic-now ()
  "Return a monotonic process clock value in seconds.
Linux exposes the kernel monotonic clock through `/proc/uptime'.  Other
systems fall back to wall time; emitted elapsed values are still clamped so
they never move backwards within the process."
  (or (ignore-errors
        (with-temp-buffer
          (insert-file-contents-literally "/proc/uptime" nil 0 64)
          (goto-char (point-min))
          (when (looking-at "[0-9]+\\(?:\\.[0-9]+\\)?")
            (string-to-number (match-string 0)))))
      (float-time)))

(defvar mevedel-telemetry--origin (mevedel-telemetry--monotonic-now)
  "Process-relative monotonic origin for event elapsed-time values.")

(defvar mevedel-telemetry--last-elapsed-ms 0
  "Last emitted process-relative elapsed time, clamped nondecreasing.")

(defvar mevedel-telemetry--sequence 0
  "Process-local telemetry event sequence.")

(defvar mevedel-telemetry--profiler-session nil
  "Session owning the currently active mevedel profiler run.")

(defvar mevedel-telemetry--profiler-run-id nil
  "Identifier of the currently active mevedel profiler run.")

(defvar mevedel-telemetry--session-debug-marker nil
  "Marker delimiting the active session's gptel debug capture.")

(defvar mevedel-telemetry--session-debug-previous-log-level nil
  "Gptel log level to restore after session debugging.")

(defvar mevedel-telemetry--session-debug-view-marker nil
  "Marker delimiting the active session's view-render debug capture.")

(defvar mevedel-telemetry--session-debug-previous-view-debug nil
  "View-render debug state to restore after session debugging.")

(defvar mevedel-telemetry--prompt-advices nil
  "Alist of prompt functions and temporary profiler advice closures.")

(defun mevedel-telemetry-current-session (&optional buffer)
  "Return the session visible from BUFFER, or nil."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (or (and (boundp 'mevedel--session) mevedel--session)
            (and (boundp 'mevedel--data-buffer)
                 (buffer-live-p mevedel--data-buffer)
                 (buffer-local-value 'mevedel--session
                                     mevedel--data-buffer))
            (and (boundp 'mevedel--agent-invocation)
                 (fboundp 'mevedel-agent-invocation-p)
                 (mevedel-agent-invocation-p mevedel--agent-invocation)
                 (fboundp 'mevedel-agent-invocation-parent-session)
                 (mevedel-agent-invocation-parent-session
                  mevedel--agent-invocation)))))))

(defun mevedel-telemetry-detailed-p (session)
  "Return non-nil when SESSION is under active profiler capture."
  (and session
       (eq session mevedel-telemetry--profiler-session)
       mevedel-telemetry--profiler-run-id))

(defun mevedel-telemetry-path (session)
  "Return SESSION's telemetry path, or nil before materialization."
  (let ((save-path (and session
                        (ignore-errors
                          (mevedel-session-save-path session)))))
    (when save-path
      (file-name-concat save-path mevedel-telemetry-file-name))))

(defun mevedel-telemetry-profiler-directory (session)
  "Return the active profiler diagnostics directory for SESSION, or nil.

For a session saved on a target this is a local directory under
`temporary-file-directory', not `diagnostics/run-*' under the session.  A
profile measures this Emacs, and writing one to the target costs megabytes of
base64 through the connection -- the `ssh' method has no out-of-band copy at
any size -- for an artifact no resume consults.  The cost is real and the
portability is not: another client resuming the session simply finds no
diagnostics for a run profiled here.

The run identifier already carries a timestamp and a hash, so naming the
directory after it is unique per run without keeping any state to remember it
by, and every caller derives the same answer."
  (when (and (eq session mevedel-telemetry--profiler-session)
             mevedel-telemetry--profiler-run-id)
    (let ((save-path (mevedel-session-save-path session)))
      (when save-path
        (if (file-remote-p save-path)
            (file-name-concat
             temporary-file-directory
             (format "mevedel-profiler-%s"
                     mevedel-telemetry--profiler-run-id))
          (file-name-concat
           save-path "diagnostics"
           mevedel-telemetry--profiler-run-id))))))

(defvar mevedel-telemetry--dropped-keys nil
  "Names of keys the filter dropped while building the current event.
Bound by `mevedel-telemetry--envelope' so a nested drop is reported once on
the event rather than inside the structure it was removed from.")

(defun mevedel-telemetry--truncate-string (value)
  "Return VALUE bounded to `mevedel-telemetry-max-string-length'."
  (if (> (length value) mevedel-telemetry-max-string-length)
      (substring value 0 mevedel-telemetry-max-string-length)
    value))

(defun mevedel-telemetry--take-bounded (items count)
  "Return at most COUNT elements from ITEMS, marking a remaining tail."
  (let (taken)
    (while (and (consp items) (> count 0))
      (push (pop items) taken)
      (setq count (1- count)))
    (when items
      (push :truncated taken))
    (nreverse taken)))

(defun mevedel-telemetry--safe-value (value &optional depth)
  "Return a bounded disk-safe representation of VALUE.
DEPTH limits recursive collections.  Unsupported objects become their type
symbol instead of their printed representation."
  (let ((depth (or depth 0)))
    (cond
     ((or (null value) (eq value t) (numberp value) (symbolp value)) value)
     ((stringp value) (mevedel-telemetry--truncate-string value))
     ((>= depth 3) :truncated)
     ((vectorp value)
      (vconcat
       (mapcar (lambda (item)
                 (mevedel-telemetry--safe-value item (1+ depth)))
               (mevedel-telemetry--take-bounded (append value nil) 32))))
     ;; A keyword-headed list is a property list, so its keys are subject to
     ;; the same rule as the event's own: a nested field is the easy way to
     ;; smuggle a path or a command past a top-level check.
     ((and (consp value) (keywordp (car value)))
      (mevedel-telemetry--safe-props value (1+ depth)))
     ((consp value)
      (mapcar (lambda (item)
                (mevedel-telemetry--safe-value item (1+ depth)))
              (mevedel-telemetry--take-bounded value 32)))
     (t (intern (format ":%s" (type-of value)))))))

(defun mevedel-telemetry--safe-props (props &optional depth)
  "Return PROPS reduced to allowlisted metadata keys at DEPTH.
Keys outside `mevedel-telemetry--allowed-keys' are dropped; their names are
collected in `mevedel-telemetry--dropped-keys' so the omission is visible to
whoever added the caller.  Envelope keys are dropped silently: the envelope
supplies them itself."
  (let (safe)
    (while props
      (let ((key (pop props))
            (value (pop props)))
        (cond
         ((not (keywordp key)) nil)
         ((memq key mevedel-telemetry--owned-keys) nil)
         ((memq key mevedel-telemetry--allowed-keys)
          (setq safe
                (append safe
                        (list key
                              (mevedel-telemetry--safe-value value depth)))))
         (t (cl-pushnew key mevedel-telemetry--dropped-keys)))))
    safe))

(defun mevedel-telemetry--envelope (session event props)
  "Build the common telemetry envelope for SESSION, EVENT, and PROPS."
  (let* ((goal (ignore-errors (mevedel-session-goal session)))
         (preset (ignore-errors (mevedel-session-preset-name session)))
         (mevedel-telemetry--dropped-keys nil)
         ;; Filter first: the report of what was dropped is only complete
         ;; once every nested value has been walked.
         (safe (mevedel-telemetry--safe-props props))
         (dropped (nreverse mevedel-telemetry--dropped-keys)))
    (unless (and goal (mevedel-goal-p goal))
      (setq goal nil))
    (append
     (list :schema-version mevedel-telemetry-schema-version
           :time (format-time-string "%FT%T.%3N%z")
           :elapsed-ms
           (setq mevedel-telemetry--last-elapsed-ms
                 (max mevedel-telemetry--last-elapsed-ms
                      (round
                       (* 1000.0
                          (- (mevedel-telemetry--monotonic-now)
                             mevedel-telemetry--origin)))))
           :sequence (cl-incf mevedel-telemetry--sequence)
           :event event
           :session-id (ignore-errors
                         (mevedel-session-session-id session))
           :turn (or (ignore-errors
                       (mevedel-session-turn-count session))
                     0)
           :profiler-run-id
           (and (eq session mevedel-telemetry--profiler-session)
                mevedel-telemetry--profiler-run-id))
     (when preset (list :preset preset))
     (when goal
       (list :goal-id (mevedel-goal-id goal)
             :goal-status (mevedel-goal-status goal)
             :goal-tokens-used (mevedel-goal-tokens-used goal)
             :goal-turns-run (mevedel-goal-turns-run goal)))
     safe
     (when dropped (list :dropped-keys dropped)))))


;;
;;; Persistence

(defun mevedel-telemetry--remote-p (session)
  "Return non-nil when SESSION's execution target is remote."
  (when-let* ((target (mevedel-session-execution-target session)))
    (require 'mevedel-execution-target)
    (mevedel-execution-target-remote-p target)))

(defun mevedel-telemetry--entry-text (entry)
  "Return telemetry ENTRY in its durable line format."
  (let ((print-length nil)
        (print-level nil)
        (print-quoted t)
        (print-escape-newlines t))
    (concat (prin1-to-string entry) "\n")))

(defun mevedel-telemetry--persist-content (session content)
  "Append serialized telemetry CONTENT to SESSION's persistent stream."
  (when-let* ((file (and mevedel-telemetry-enabled
                         (mevedel-telemetry-path session))))
    (condition-case err
        (if (mevedel-telemetry--remote-p session)
            (progn
              (require 'mevedel-session-durability)
              (require 'mevedel-session-publication)
              (mevedel-session-publication-append-diagnostic
               session file content))
          (make-directory (file-name-directory file) t)
          (write-region content nil file t 'silent)
          t)
      (error
       (message "mevedel: telemetry persistence failed: %s"
                (error-message-string err))
       nil))))

(defun mevedel-telemetry--persist (session entry)
  "Append telemetry ENTRY to SESSION's persistent stream."
  (mevedel-telemetry--persist-content
   session (mevedel-telemetry--entry-text entry)))

(defun mevedel-telemetry-record (session event &rest props)
  "Record telemetry EVENT and PROPS for SESSION.
The event is buffered until SESSION has a persistent directory.  Raw payload
keys are always discarded.  Return the sanitized event plist."
  (when (and mevedel-telemetry-enabled session)
    (condition-case err
        (let ((entry (mevedel-telemetry--envelope session event props))
              (pending (mevedel-session-telemetry-pending session)))
          (if (and (null pending)
                   (mevedel-session-save-path session)
                   (not (mevedel-telemetry--remote-p session))
                   (mevedel-telemetry--persist session entry))
              nil
            ;; Newest first: appending kept the queue chronological but
            ;; cost the whole list per event; the flush restores order.
            (setf (mevedel-session-telemetry-pending session)
                  (cons entry pending)))
          entry)
      (error
       (message "mevedel: telemetry event failed: %s"
                (error-message-string err))
       nil))))

(defun mevedel-telemetry-forwarded-audit-p (session)
  "Return non-nil when SESSION audits into a distinct durable session."
  (let ((target (mevedel-session-audit-target session)))
    (and target (not (eq target session)))))

(defun mevedel-telemetry-record-audit (session event &rest props)
  "Record audit EVENT and PROPS on SESSION's durable audit target.

Transient conversations audit into a durable parent session (see
`mevedel-session-audit-target'); their events are tagged with a
`:conversation-scope' so consumers can distinguish forwarded events.
Callers own reducing PROPS to sanitized categorical values before
anything crosses into the durable target."
  (when-let* ((target (mevedel-session-audit-target session)))
    (apply #'mevedel-telemetry-record target event
           (if (eq target session)
               props
             (append '(:conversation-scope btw) props)))))

(defun mevedel-telemetry-flush (session)
  "Persist SESSION's queued telemetry, retaining failed entries."
  (when session
    ;; The queue is stored newest first; flush in recording order.
    (let ((pending (reverse (mevedel-session-telemetry-pending session))))
      (if (and pending (mevedel-telemetry--remote-p session))
          (when (mevedel-telemetry--persist-content
                 session (mapconcat #'mevedel-telemetry--entry-text
                                    pending ""))
            (setf (mevedel-session-telemetry-pending session) nil))
        (let (remaining)
          (dolist (entry pending)
            (unless (mevedel-telemetry--persist session entry)
              (push entry remaining)))
          (setf (mevedel-session-telemetry-pending session)
                remaining))))))


;;
;;; Asynchronous spans

(defun mevedel-telemetry--span-id (event)
  "Return a process-unique identifier for EVENT."
  (format "%s-%s"
          event
          (substring
           (secure-hash
            'sha1
            (format "%s:%s:%s:%s"
                    (emacs-pid) (float-time) mevedel-telemetry--sequence event))
           0 16)))

(defun mevedel-telemetry-start (session event &rest props)
  "Start an asynchronous SESSION span named EVENT with PROPS.
Return an opaque span plist accepted by `mevedel-telemetry-finish'."
  (let* ((span-id (mevedel-telemetry--span-id event))
         (started-at (mevedel-telemetry--monotonic-now)))
    (apply #'mevedel-telemetry-record
           session event :stage 'start :span-id span-id props)
    (list :session session :event event :span-id span-id
          :started-at started-at
          ;; The span's own copy is filtered for storage; the drops are
          ;; reported by the event recorded just above, not collected here.
          :props (let ((mevedel-telemetry--dropped-keys nil))
                   (mevedel-telemetry--safe-props props)))))

(defun mevedel-telemetry-finish (span &rest props)
  "Finish telemetry SPAN with PROPS and return the emitted event."
  (when span
    (let ((duration-ms
           (round (* 1000.0
                     (max 0.0
                          (- (mevedel-telemetry--monotonic-now)
                             (plist-get span :started-at)))))))
      (apply #'mevedel-telemetry-record
             (plist-get span :session)
             (plist-get span :event)
             :stage 'finish
             :span-id (plist-get span :span-id)
             :duration-ms duration-ms
             (append (plist-get span :props) props)))))


;;
;;; Profiler capture

(defun mevedel-telemetry--guard-prompt (function original &rest args)
  "Record synchronous prompt FUNCTION, then call ORIGINAL with ARGS or fail."
  (let* ((prompt (car args))
         (text (and (stringp prompt) prompt))
         (session mevedel-telemetry--profiler-session))
    (when session
      (mevedel-telemetry-record
       session 'interactive-prompt-opened
       :prompt-function function
       :prompt-hash (and text (secure-hash 'sha256 text))
       :prompt-chars (and text (length text))
       :blocked (and mevedel-telemetry-profiler-fail-on-prompt t)))
    (if mevedel-telemetry-profiler-fail-on-prompt
        (user-error "Interactive prompt blocked during telemetry run: %s"
                    function)
      (apply original args))))

(defun mevedel-telemetry--install-prompt-guard ()
  "Install temporary synchronous prompt observation advices."
  (dolist (function '(ask-user-about-supersession-threat
                      yes-or-no-p y-or-n-p))
    (when (fboundp function)
      (let* ((prompt-function function)
             (advice
             (lambda (original &rest args)
               (apply #'mevedel-telemetry--guard-prompt
                      prompt-function original args))))
        (advice-add function :around advice)
        (push (cons function advice) mevedel-telemetry--prompt-advices)))))

(defun mevedel-telemetry--remove-prompt-guard ()
  "Remove temporary synchronous prompt observation advices."
  (dolist (entry mevedel-telemetry--prompt-advices)
    (advice-remove (car entry) (cdr entry)))
  (setq mevedel-telemetry--prompt-advices nil))

(defun mevedel-telemetry--process-output (program &rest args)
  "Return trimmed PROGRAM output for ARGS, or nil on failure."
  (with-temp-buffer
    (when (zerop (apply #'process-file program nil t nil args))
      (string-trim (buffer-string)))))

(defun mevedel-telemetry--git-snapshot (directory)
  "Return a non-sensitive Git state snapshot for DIRECTORY."
  (let* ((default-directory directory)
         (head (ignore-errors
                 (mevedel-telemetry--process-output
                  "git" "rev-parse" "HEAD")))
         (status (ignore-errors
                   (mevedel-telemetry--process-output
                    "git" "status" "--short")))
         (diff (ignore-errors
                 (mevedel-telemetry--process-output
                  "git" "diff" "--binary" "HEAD")))
         (untracked (ignore-errors
                      (mevedel-telemetry--process-output
                       "git" "ls-files" "--others"
                       "--exclude-standard" "-z")))
         (untracked-hashes
          (mapcar
           (lambda (relative)
             (let ((file (expand-file-name relative directory)))
               (when (and (file-regular-p file) (file-readable-p file))
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally file)
                   (secure-hash
                    'sha256
                    (concat relative "\0"
                            (secure-hash 'sha256 (current-buffer))))))))
           (split-string (or untracked "") "\0" t)))
         (dirty-content
          (concat (or diff "") "\0"
                  (mapconcat #'identity (delq nil untracked-hashes) "\0"))))
    (list :git-head head
          :dirty-file-count
          (if (and status (not (string-empty-p status)))
              (length (split-string status "\n" t))
            0)
          :dirty-state-hash
          (and status (secure-hash 'sha256 status))
          :dirty-content-hash
          (secure-hash 'sha256 dirty-content))))

(defun mevedel-telemetry--library-snapshot (feature)
  "Return safe loaded-library identity fields for FEATURE."
  (when-let* ((file (locate-library (symbol-name feature)))
              ((file-readable-p file)))
    (let* ((root (locate-dominating-file file ".git"))
           (head (and root
                      (ignore-errors
                        (mevedel-telemetry--process-output
                         "git" "-C" root "rev-parse" "HEAD")))))
      (list :file-hash
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally file)
            (secure-hash 'sha256 (current-buffer)))
            :file-bytes (file-attribute-size (file-attributes file))
            :git-head head))))

(defun mevedel-telemetry--record-environment (session boundary)
  "Record reproduction environment for SESSION at BOUNDARY."
  (let* ((directory (mevedel-session-working-directory session))
         (git (and directory (mevedel-telemetry--git-snapshot directory)))
         (gptel (mevedel-telemetry--library-snapshot 'gptel))
         (gptel-agent (mevedel-telemetry--library-snapshot 'gptel-agent))
         (sandbox
          (when (require 'mevedel-sandbox nil t)
            (ignore-errors (mevedel-sandbox-probe)))))
    (apply #'mevedel-telemetry-record
           session 'reproduction-environment
           :boundary boundary
           :emacs-version emacs-version
           :system-configuration system-configuration
           :gptel-version (and (boundp 'gptel-version) gptel-version)
           :gptel-file-hash (plist-get gptel :file-hash)
           :gptel-commit (plist-get gptel :git-head)
           :gptel-agent-file-hash (plist-get gptel-agent :file-hash)
           :gptel-agent-commit (plist-get gptel-agent :git-head)
           :sandbox-mode (mevedel-session-sandbox-mode session)
           :bubblewrap-available (and sandbox
                                       (plist-get sandbox :available))
           git)))

;;;###autoload
(defvar mevedel-telemetry--profiler-prior-stack-depth nil
  "Value of `profiler-max-stack-depth' from before a mevedel run raised it.
A run has to set the variable globally, because the C log fixes its backtrace
width when profiling starts.  Restoring it is the run's job.")

(defun mevedel-telemetry--profiler-release ()
  "Release every global a profiler run owns."
  (mevedel-telemetry--remove-prompt-guard)
  (when mevedel-telemetry--profiler-prior-stack-depth
    (setq profiler-max-stack-depth
          mevedel-telemetry--profiler-prior-stack-depth
          mevedel-telemetry--profiler-prior-stack-depth nil))
  (setq mevedel-telemetry--profiler-session nil
        mevedel-telemetry--profiler-run-id nil))

(defun mevedel-telemetry-profiler-start (&optional mode)
  "Start Emacs profiling for the current session in MODE.
MODE defaults to `cpu+mem'.  Interactively with a prefix argument, prompt for
`cpu', `mem', or `cpu+mem'."
  (interactive
   (list (if current-prefix-arg
             (intern
              (completing-read "Profiler mode: "
                               '("cpu" "mem" "cpu+mem") nil t nil nil
                               "cpu+mem"))
           'cpu+mem)))
  (let ((session (mevedel-telemetry-current-session))
        (mode (or mode 'cpu+mem)))
    (unless session
      (user-error "No mevedel session in the current buffer"))
    (unless (mevedel-session-save-path session)
      (user-error "Materialize the mevedel session before profiling"))
    (when mevedel-telemetry--profiler-session
      (user-error "A mevedel profiler run is already active"))
    (require 'profiler)
    (setq mevedel-telemetry--profiler-run-id
          (format "run-%s-%s"
                  (format-time-string "%Y%m%dT%H%M%S")
                  (substring
                   (secure-hash 'sha1
                                (format "%s:%s" (emacs-pid) (float-time)))
                   0 8)))
    ;; The C log fixes its backtrace width when the profiler starts, so this
    ;; has to be set before the call, not around the report.
    (setq mevedel-telemetry--profiler-prior-stack-depth profiler-max-stack-depth
          profiler-max-stack-depth mevedel-telemetry-profiler-stack-depth)
    (profiler-start mode)
    ;; Past this line Emacs is profiling, so a failure in the rest of the
    ;; setup has to undo it: reporting that the run did not start while it
    ;; keeps sampling is the one outcome with no way back.
    (condition-case err
        (progn
          (setq mevedel-telemetry--profiler-session session)
          (mevedel-telemetry--install-prompt-guard)
          (mevedel-telemetry--record-environment session 'start)
          (mevedel-telemetry-record
           session 'profiler-started
           :mode mode
           :emacs-version emacs-version
           :system-configuration system-configuration)
          (message "mevedel: profiler started for session %s"
                   (or (mevedel-session-session-id session) "pending")))
      (error
       (with-demoted-errors "mevedel: profiler rollback failed: %S"
         (profiler-stop))
       (mevedel-telemetry--profiler-release)
       (signal (car err) (cdr err))))))

(defun mevedel-telemetry--write-profiler-artifacts (directory)
  "Write compact native profiler profiles and reports below DIRECTORY.

`profiler-write-profile' is four lines around a `write-file' that visits its
output, which drags `set-visited-file-name' and a version-control refresh onto
a diagnostics artifact nobody tracks.  A Git probe there is several round trips
on a target, and a failure aborts the whole stop and loses the profile.  The
`print' it wraps is the part that matters, so it is inlined here and written
the way the report two lines below already is.  Nothing below visits a file,
so version control never hears about any of it."
  (cl-labels
      ((write-one
        (mode profile)
        (let* ((profile-file
                (file-name-concat
                 directory (format "profiler-%s-profile.el" mode)))
               (report-file
                (file-name-concat
                 directory (format "profiler-%s-report.txt" mode)))
               (report-buffer (profiler-report-setup-buffer profile)))
          (unwind-protect
              (progn
                (with-temp-buffer
                  (let (print-level print-length)
                    (print (profiler-fixup-profile profile) (current-buffer)))
                  (write-region (point-min) (point-max)
                                profile-file nil 'silent))
                (with-current-buffer report-buffer
                  (write-region (point-min) (point-max)
                                report-file nil 'silent))
                (let ((profile-bytes
                       (file-attribute-size (file-attributes profile-file)))
                      (report-bytes
                       (file-attribute-size (file-attributes report-file))))
                  (unless (and (> profile-bytes 0) (> report-bytes 0))
                    (error "Profiler %s artifacts are incomplete" mode))
                  (list :mode mode
                        :profile-file profile-file
                        :report-file report-file
                        :profile-bytes profile-bytes
                        :report-bytes report-bytes)))
            (when (buffer-live-p report-buffer)
              (kill-buffer report-buffer))))))
    (let* ((artifacts
            (delq nil
                  (list
                   (when profiler-cpu-log
                     (write-one 'cpu (profiler-cpu-profile)))
                   (when profiler-memory-log
                     (write-one 'memory (profiler-memory-profile)))))))
      (unless artifacts
        (error "No profiler samples were recorded"))
      artifacts)))

;;;###autoload
(defun mevedel-telemetry-profiler-stop ()
  "Stop the active mevedel profiler and save its session artifacts."
  (interactive)
  (unless mevedel-telemetry--profiler-session
    (user-error "No mevedel profiler run is active"))
  (require 'profiler)
  (let* ((session mevedel-telemetry--profiler-session)
         (directory (mevedel-telemetry-profiler-directory session))
         (failure-stage 'stop))
    (unwind-protect
        (condition-case err
            (progn
              ;; Before anything that can fail, and before the snapshot: a
              ;; snapshot that signals used to leave Emacs profiling with the
              ;; handle to stop it already thrown away.  Stopping first also
              ;; keeps the snapshot's own Git and hashing work out of the
              ;; profile it is describing.
              (profiler-stop)
              (setq failure-stage 'environment)
              (mevedel-telemetry--record-environment session 'stop)
              (make-directory directory t)
              (setq failure-stage 'save-artifacts)
              (let ((artifacts
                     (mevedel-telemetry--write-profiler-artifacts directory)))
                (mevedel-telemetry-record
                 session 'profiler-stopped
                 :modes (mapcar (lambda (artifact)
                                  (plist-get artifact :mode))
                                artifacts)
                 ;; Absolute, and on this client: for a session saved on a
                 ;; target, a path relative to the session directory would
                 ;; send a reader looking on the wrong machine.
                 :artifacts-directory directory
                 :artifacts-local
                 (not (file-remote-p (mevedel-session-save-path session)))
                 :profile-file-names
                 (mapcar (lambda (artifact)
                           (file-name-nondirectory
                            (plist-get artifact :profile-file)))
                         artifacts)
                 :report-file-names
                 (mapcar (lambda (artifact)
                           (file-name-nondirectory
                            (plist-get artifact :report-file)))
                         artifacts)
                 :profile-bytes-total
                 (apply #'+ (mapcar (lambda (artifact)
                                      (plist-get artifact :profile-bytes))
                                    artifacts))
                 :report-bytes-total
                 (apply #'+ (mapcar (lambda (artifact)
                                      (plist-get artifact :report-bytes))
                                    artifacts)))
                (message "mevedel: profiler artifacts saved under %s"
                         directory)))
          (error
           (mevedel-telemetry-record
            session 'profiler-stop-failed
            :failure-stage failure-stage
            :failure-class (car err))
           (signal (car err) (cdr err))))
      (mevedel-telemetry--profiler-release))))

(defun mevedel-telemetry--redact-gptel-debug-buffer ()
  "Redact credential-bearing headers in the current gptel debug buffer."
  (let ((case-fold-search t)
        record-type)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line-end (line-end-position)))
        (cond
         ((looking-at
           (concat
            "[ \t]*\\(?:{[ \t]*\\)?\"gptel\"[ \t]*:[ \t]*\""
            "\\([^\"]+\\)\""))
          (setq record-type (downcase (match-string-no-properties 1))))
         ((member record-type '("request headers" "response headers"))
          (when (re-search-forward
                 (concat
                  "\\([ \t]*\""
                  "\\(?:Authorization\\|ChatGPT-Account-Id\\|Session-Id\\)"
                  "\"[ \t]*:[ \t]*\"\\)[^\"\n\r]*\\(\"[, \t]*\\)$")
                 line-end t)
            (replace-match "\\1<redacted>\\2" nil nil)))
         ((equal record-type "request config")
          (when (re-search-forward
                 (concat
                  "\\([ \t]*header[ \t]*=[ \t]*\""
                  "\\(?:Authorization\\|ChatGPT-Account-Id\\|Session-Id\\)"
                  ":[ \t]*\\)[^\"\n\r]*\\(\"[ \t]*\\)$")
                 line-end t)
            (replace-match "\\1<redacted>\\2" nil nil)))))
      (forward-line 1))))

;;;###autoload
(defun mevedel-session-debug ()
  "Toggle profiler and gptel debug-log capture for the current session.
Stopping writes the captured log suffix to gptel-debug.log in the profiler
run's diagnostics directory.  The log may contain raw request data and
connection settings."
  (interactive)
  (require 'gptel)
  (if mevedel-telemetry--session-debug-marker
      (let* ((marker mevedel-telemetry--session-debug-marker)
             (view-marker mevedel-telemetry--session-debug-view-marker)
             (session mevedel-telemetry--profiler-session))
        (unwind-protect
            (let* ((directory
                    (mevedel-telemetry-profiler-directory session))
                   (gptel-file
                    (file-name-concat directory "gptel-debug.log"))
                   (view-file
                    (file-name-concat directory "view-render-debug.log"))
                   (default-file-modes #o600))
              (unless (marker-buffer marker)
                (user-error "The active gptel debug log buffer was killed"))
              (make-directory directory t)
              (with-temp-buffer
                (insert-buffer-substring (marker-buffer marker) marker)
                (mevedel-telemetry--redact-gptel-debug-buffer)
                (write-region (point-min) (point-max)
                              gptel-file nil 'silent))
              (set-file-modes gptel-file #o600)
              (when (and (markerp view-marker)
                         (marker-buffer view-marker))
                (with-current-buffer (marker-buffer view-marker)
                  (write-region view-marker (point-max)
                                view-file nil 'silent))
                (set-file-modes view-file #o600))
              (mevedel-telemetry-profiler-stop)
              (message "mevedel: session debug artifacts saved under %s"
                       directory))
          (when mevedel-telemetry--profiler-session
            (ignore-errors (mevedel-telemetry-profiler-stop)))
          (setq gptel-log-level
                mevedel-telemetry--session-debug-previous-log-level
                mevedel-telemetry--session-debug-previous-log-level nil
                mevedel-view-render-debug
                mevedel-telemetry--session-debug-previous-view-debug
                mevedel-telemetry--session-debug-previous-view-debug nil)
          (set-marker marker nil)
          (when (markerp view-marker)
            (set-marker view-marker nil))
          (setq mevedel-telemetry--session-debug-marker nil
                mevedel-telemetry--session-debug-view-marker nil)))
    (require 'mevedel-view-render)
    (let ((previous-level gptel-log-level)
          (previous-view-debug mevedel-view-render-debug))
      (mevedel-telemetry-profiler-start)
      (setq mevedel-telemetry--session-debug-previous-log-level previous-level
            mevedel-telemetry--session-debug-marker
            (with-current-buffer (get-buffer-create gptel--log-buffer-name)
              (copy-marker (point-max)))
            mevedel-telemetry--session-debug-previous-view-debug
            previous-view-debug
            mevedel-telemetry--session-debug-view-marker
            (with-current-buffer
                (get-buffer-create mevedel-view-render-debug-buffer-name)
              (copy-marker (point-max)))
            gptel-log-level 'debug
            mevedel-view-render-debug t)
      (message "mevedel: session debug capture started"))))

(provide 'mevedel-telemetry)

;;; mevedel-telemetry.el ends here
