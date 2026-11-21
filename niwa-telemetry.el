;;; niwa-telemetry.el -- Durable session telemetry -*- lexical-binding: t -*-

;;; Commentary:

;; Records a versioned, append-only stream of bounded lifecycle events in each
;; materialized session.  Telemetry is diagnostic evidence rather than session
;; state: it is never read during resume and failures never block the workflow.
;; Payload-bearing fields are rejected at this boundary so prompts, commands,
;; tool arguments, model responses, hook output, and environment values cannot
;; leak into the log through an accidental call site.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'niwa-structs))

;; `emacs'
(defvar default-file-modes)

;; `gptel'
(defvar gptel--log-buffer-name)
(defvar gptel-log-level)
(defvar gptel-version)

;; `niwa-agents'
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-parent-session
                  "niwa-agents" (cl-x) t)

;; `niwa-execution-target'
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))

;; `niwa-native'
(declare-function niwa-native-load "niwa-native" ())
(autoload 'niwa-native-load "niwa-native")

;; `niwa-sandbox'
(declare-function niwa-sandbox-mode-effective
                  "niwa-sandbox" (&optional session permission-mode))
(declare-function niwa-sandbox-probe "niwa-sandbox" (&optional workdir))
(autoload 'niwa-sandbox-mode-effective "niwa-sandbox")
(autoload 'niwa-sandbox-probe "niwa-sandbox")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-save
                  "niwa-session-artifacts"
                  (session buffer &optional settled force))
(autoload 'niwa-session-artifacts-save "niwa-session-artifacts")

;; `niwa-session-control-fs'
(declare-function niwa-session-control-fs-program-value "niwa-session-control-fs"
                  (result))
(declare-function niwa-session-control-fs-run-program "niwa-session-control-fs"
                  (operations &optional lock-directory))
(autoload 'niwa-session-control-fs-program-value "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-run-program "niwa-session-control-fs")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-root-buffer-for-session
                  "niwa-session-persistence" (session &optional buffer))
(autoload 'niwa-session-persistence-root-buffer-for-session
  "niwa-session-persistence")

;; `niwa-session-publication'
(declare-function niwa-session-publication-append-diagnostic
                  "niwa-session-publication" (session path content))
(autoload 'niwa-session-publication-append-diagnostic
  "niwa-session-publication")

;; `niwa-structs'
(declare-function niwa-goal-id "niwa-structs" (cl-x))
(declare-function niwa-goal-p "niwa-structs" (cl-x))
(declare-function niwa-goal-status "niwa-structs" (cl-x))
(declare-function niwa-goal-tokens-incomplete-p "niwa-structs" (cl-x))
(declare-function niwa-goal-tokens-used "niwa-structs" (cl-x))
(declare-function niwa-goal-turns-run "niwa-structs" (cl-x))
(declare-function niwa-session-audit-target "niwa-structs" (session))
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-goal "niwa-structs" (cl-x))
(declare-function niwa-session-preset-name "niwa-structs" (cl-x))
(declare-function niwa-session-sandbox-mode "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x))
(declare-function niwa-session-session-id "niwa-structs" (cl-x))
(declare-function niwa-session-telemetry-pending
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-turn-count "niwa-structs" (cl-x))
(declare-function niwa-session-working-directory "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--agent-invocation)
(defvar niwa--data-buffer)
(defvar niwa--session)

;; `niwa-transport'
(defvar niwa-transport--depth)

;; `niwa-utilities'
(declare-function niwa--run-periodic-timer
                  "niwa-utilities" (scope period function &rest arguments))
(declare-function niwa--ui-timer-cancel "niwa-utilities" (timer))
(declare-function niwa-diagnostic-message "niwa-utilities" (scope format-string &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format-string &rest args))
(declare-function niwa-library-provenance "niwa-utilities" (file))
(autoload 'niwa--run-periodic-timer "niwa-utilities")
(autoload 'niwa--ui-timer-cancel "niwa-utilities")
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")
(autoload 'niwa-library-provenance "niwa-utilities")

;; `niwa-view-render'
(defvar niwa-view-render-debug)
(defvar niwa-view-render-debug-buffer-name)

;; `native/niwa-wait.c'
(declare-function niwa-native--condition-wait "ext:niwa-wait"
                  (condition seconds))
(declare-function niwa-native--timed-wait-available-p "ext:niwa-wait" ())

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

(require 'niwa-execution-target)


;;
;;; Customization

(defcustom niwa-telemetry-enabled t
  "When non-nil, record bounded lifecycle telemetry for each session."
  :type 'boolean
  :group 'niwa)

(defcustom niwa-telemetry-file-name "telemetry-log.el"
  "File name for the append-only per-session telemetry stream."
  :type 'string
  :group 'niwa)

(defcustom niwa-telemetry-max-string-length 512
  "Maximum number of characters retained in one telemetry string value."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-telemetry-profiler-fail-on-prompt t
  "When non-nil, profiler runs log and reject synchronous prompt functions.
This turns hidden compaction, file-conflict, and edit confirmations into an
explicit reproduction failure instead of unmeasured user-wait time."
  :type 'boolean
  :group 'niwa)

(defcustom niwa-telemetry-profiler-stack-depth 64
  "Backtrace depth captured by a niwa profiler run.

The native default of 16 truncates before a niwa frame is reached: a turn
runs through the view, the FSM, gptel, and a TRAMP handler before it costs
anything measurable, so a truncated sample records that the time went to
`file-attributes' without recording which niwa call site asked for it.
Attribution is the whole point of these artifacts.  Deeper samples make the
profile file larger and cost a little more per sample."
  :type 'integer
  :group 'niwa)


;;
;;; Event representation

(defconst niwa-telemetry-schema-version 1
  "Schema version written into every telemetry event.")

(defconst niwa-telemetry--allowed-keys
  '(:adapter-exit-code :adapter-signal :queued-handover :resend :since-end-turn-ms
    :since-send-ms
    :abort-plan-approval :active-work-paused :additional-read-count
    :additional-write-count :admitted :allocated-kb :approval-lifetime
    :application-duration-ms :application-recovery-round-trip-ms :artifact-count
    :agent-id :agent-path :agent-type :aggressive :artifacts-directory
    :artifacts-local :attempt-generation :backend :baseline-marker-position
    :backend-type :baseline-request-id :blocked :boundary :bubblewrap-available :bucket
    :budget-kind :budget-status :buffer-chars-total
    :buffers :cache-identity :cache-write-tokens :cached-tokens :call-id :call-source :calls :candidate-count :capture-id
    :captured-goal-id
    :chosen-active-context-tokens :chosen-source :chunk-bytes
    :command-class :command-hash :command-name :context :context-chars :cpu-ms
    :context-deduplicated :continuation :conversation-scope :covered-count
    :cumulative-usage :cumulative-usage-tokens :delay-ms :dequeue-goal-id
    :deleted-directory-count :deleted-file-count
    :dirty-content-hash :dirty-file-count :dirty-state-hash :duration-ms
    :effective-wait-ms :effort :emacs-version :enqueue-goal-id
    :error-class :estimate :estimate-source :eval-mode :execution-id :exit-code
    :exit-status :failure-class :failure-stage :fallback :filesystem :first-byte-seen :frames
    :fresh-visible-prompt-estimate
    :gc-count :gc-ms :git-head :goal-id
    :gptel-commit :gptel-file-hash :gptel-version :handler-count
    :handler-id :handler-source :handler-type :hook-event :image-cache-bytes
    :ineligible-reason :input-bytes :input-p :input-pending :input-tokens
    :investigation :investigation-command :investigation-notes :investigation-notes-omitted
    :investigation-path :investigation-pattern :investigation-root
    :interaction-id :issue-count
    :kind :lane :largest-buffer-chars :largest-buffer-hash :largest-buffer-mode
    :largest-buffer-process :limit
    :loaded-at
    :max-ms :niwa-commit :niwa-version :message-chars :message-hash :mode :model
    :model-context-source
    :model-context-window :modes
    :native-resource-capture :native-resource-report-bytes :nested-call-count
    :network
    :new-count :new-segment :note-cycle-attempted :note-cycle-count :note-cycle-deferred
    :note-cycle-id :note-cycle-remaining :note-cycle-status
    :offset :old-segment :omitted-count :operation-count :origin :outcome
    :output-bytes :output-estimated-tokens :output-limit :output-tokens
    :over-1000-ms :over-200-ms :over-500-ms :overlap-count :owner
    :parent-tool-use-id :parent-turn :partial :pass-id :pending-count :permission-id :permission-mode
    :permission-mode-base
    :permission-mode-effective :permission-via :preexisting-count
    :preparation-duration-ms :preparation-recovery-duration-ms :preparation-state
    :previous-owner :previous-status :proc :processes :profile :profile-bytes-total
    :profile-file-names :prompt-chars :prompt-function :prompt-hash :proposed-count
    :protected-path-count :provider-call-count :provider-context-model :provider-context-status
    :provider-context-tokens :provider-context-usage
    :provider-context-window :provider-duration-ms :provider-error-code :provider-error-type
    :provider-status :publication-duration-ms :purpose :queue-depth
    :queue-depth-before :queue-duration-ms :read-only :reason
    :reason-class :reasoning-bytes :remaining-count :repair-count :reply-bytes
    :report-bytes-total :report-file-names
    :request-id :requested-yield-time-ms :resolved-count :resource-access
    :restored :result-bytes :result-chars :result-hash :retained :retained-count :retirement-duration-ms
    :review-request-duration-ms :reviewed-count :roster-chars :rounds :rss-kb
    :sandbox :sandbox-mode :sandbox-permissions :scope :settled :settlement-source
    :skill-count :skill-name :skill-names :skip-gates :span-id
    :specifier-key :stage :status :step :summary-context-source :summary-context-window
    :summary-model :summary-reserve-tokens :summary-threshold :summary-usable-tokens
    :system-configuration :target-model :target-origin :target-pressure
    :target-reserve-tokens :target-threshold :target-truncated :target-usable-tokens
    :termination :test-scope :threads :threshold :threshold-ms :threshold-ratio
    :timed-out :timeout-ms :timer-callback :timer-gc-count :timer-gc-ms :timer-ms :token-source :tokens-after :tokens-before
    :tokens-used :tool-call-bytes :tool-call-count :tool-name :tool-use-id
    :trigger :truncated :tty :turns-run :updated-file-count :usage-status :via
    :workload :yield-time-ms)
  "Metadata keys telemetry may persist.

An allowlist, not a denylist: telemetry is durable and the data policy in
`docs/telemetry.md' permits only lifecycle metadata, sizes, classifications,
hashes, and bounded identifiers.  A denylist has to name every field that
could carry a prompt, a command, a path, or a tool result, so any caller
naming a field something new leaks by default.  Keys outside this list are
dropped and their names -- names only -- reported in `:dropped-keys', which
is what a new caller sees instead of silence.
`niwa-execution-telemetry--audit-prop-keys' filters the same way for
forwarded audits.")

(defconst niwa-telemetry--owned-keys
  '(:schema-version :time :elapsed-ms :sequence :event :turn
    :preset :goal-id :goal-cycle :goal-phase :goal-status :profiler-run-id
    :dropped-keys)
  "Envelope keys supplied by `niwa-telemetry-record'.")

(defun niwa-telemetry--monotonic-now ()
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

(defvar niwa-telemetry--origin (niwa-telemetry--monotonic-now)
  "Process-relative monotonic origin for event elapsed-time values.")

(defvar niwa-telemetry--last-elapsed-ms 0
  "Last emitted process-relative elapsed time, clamped nondecreasing.")

(defvar niwa-telemetry--sequence 0
  "Process-local telemetry event sequence.")

(defvar niwa-telemetry--profiler-session nil
  "Session owning the currently active niwa profiler run.")

(defvar niwa-telemetry--profiler-run-id nil
  "Identifier of the currently active niwa profiler run.")

(defvar niwa-telemetry--session-debug-marker nil
  "Marker delimiting the active session's gptel debug capture.")

(defvar niwa-telemetry--session-debug-previous-log-level nil
  "Gptel log level to restore after session debugging.")

(defvar niwa-telemetry--session-debug-view-marker nil
  "Marker delimiting the active session's view-render debug capture.")

(defvar niwa-telemetry--session-debug-previous-view-debug nil
  "View-render debug state to restore after session debugging.")

(defvar niwa-telemetry--prompt-advices nil
  "Alist of prompt functions and temporary profiler advice closures.")

(defun niwa-telemetry-current-session (&optional buffer)
  "Return the session visible from BUFFER, or nil."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (or (and (boundp 'niwa--session) niwa--session)
            (and (boundp 'niwa--data-buffer)
                 (buffer-live-p niwa--data-buffer)
                 (buffer-local-value 'niwa--session
                                     niwa--data-buffer))
            (and (boundp 'niwa--agent-invocation)
                 (fboundp 'niwa-agent-invocation-p)
                 (niwa-agent-invocation-p niwa--agent-invocation)
                 (fboundp 'niwa-agent-invocation-parent-session)
                 (niwa-agent-invocation-parent-session
                  niwa--agent-invocation)))))))

(defun niwa-telemetry-detailed-p (session)
  "Return non-nil when SESSION is under active profiler capture."
  (and session
       (eq session niwa-telemetry--profiler-session)
       niwa-telemetry--profiler-run-id))

(defun niwa-telemetry-path (session)
  "Return SESSION's telemetry path, or nil before materialization."
  (let ((save-path (and session
                        (ignore-errors
                          (niwa-session-save-path session)))))
    (when save-path
      (file-name-concat save-path niwa-telemetry-file-name))))

(defun niwa-telemetry-profiler-directory (session)
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
  (when (and (eq session niwa-telemetry--profiler-session)
             niwa-telemetry--profiler-run-id)
    (let ((save-path (niwa-session-save-path session)))
      (when save-path
        (if (file-remote-p save-path)
            (file-name-concat
             temporary-file-directory
             (format "niwa-profiler-%s"
                     niwa-telemetry--profiler-run-id))
          (file-name-concat
           save-path "diagnostics"
           niwa-telemetry--profiler-run-id))))))

(defvar niwa-telemetry--dropped-keys nil
  "Names of keys the filter dropped while building the current event.
Bound by `niwa-telemetry--envelope' so a nested drop is reported once on
the event rather than inside the structure it was removed from.")

(defvar niwa-telemetry--event-turn nil
  "Frozen turn for a delayed per-call event, dynamically bound by its emitter.
Ordinary event callers cannot override envelope identity through properties.")

(defun niwa-telemetry--truncate-string (value)
  "Return VALUE bounded to `niwa-telemetry-max-string-length'."
  (if (> (length value) niwa-telemetry-max-string-length)
      (substring value 0 niwa-telemetry-max-string-length)
    value))

(defun niwa-telemetry--take-bounded (items count)
  "Return at most COUNT elements from ITEMS, marking a remaining tail."
  (let (taken)
    (while (and (consp items) (> count 0))
      (push (pop items) taken)
      (setq count (1- count)))
    (when items
      (push :truncated taken))
    (nreverse taken)))

(defun niwa-telemetry--safe-value (value &optional depth)
  "Return a bounded disk-safe representation of VALUE.
DEPTH limits recursive collections.  Unsupported objects become their type
symbol instead of their printed representation."
  (let ((depth (or depth 0)))
    (cond
     ((or (numberp value) (symbolp value)) value)
     ((stringp value) (niwa-telemetry--truncate-string value))
     ((>= depth 3) :truncated)
     ((vectorp value)
      (vconcat
       (mapcar (lambda (item)
                 (niwa-telemetry--safe-value item (1+ depth)))
               ;; One extra element preserves the truncated-tail marker.
               (niwa-telemetry--take-bounded
                (append (substring value 0 (min 33 (length value))) nil)
                32))))
     ;; A keyword-headed list is a property list, so its keys are subject to
     ;; the same rule as the event's own: a nested field is the easy way to
     ;; smuggle a path or a command past a top-level check.
     ((and (consp value) (keywordp (car value)))
      (niwa-telemetry--safe-props value (1+ depth)))
     ((consp value)
      (mapcar (lambda (item)
                (niwa-telemetry--safe-value item (1+ depth)))
              (niwa-telemetry--take-bounded value 32)))
     (t (intern (format ":%s" (type-of value)))))))

(defun niwa-telemetry--safe-props (props &optional depth)
  "Return PROPS reduced to non-nil allowlisted metadata keys at DEPTH.
Keys outside `niwa-telemetry--allowed-keys' are dropped; their names are
collected in `niwa-telemetry--dropped-keys' so the omission is visible to
whoever added the caller.  Envelope keys are dropped silently: the envelope
supplies them itself."
  (let (safe)
    (while props
      (let ((key (pop props))
            (value (pop props)))
        (cond
         ((not (keywordp key)) nil)
         ((memq key niwa-telemetry--owned-keys) nil)
         ((memq key niwa-telemetry--allowed-keys)
          (when-let* ((value (niwa-telemetry--safe-value value depth)))
            (push key safe)
            (push value safe)))
         (t (cl-pushnew key niwa-telemetry--dropped-keys)))))
    (nreverse safe)))

(defun niwa-telemetry--envelope (session event props)
  "Build the common telemetry envelope for SESSION, EVENT, and PROPS."
  (let* ((goal (ignore-errors (niwa-session-goal session)))
         (preset (ignore-errors (niwa-session-preset-name session)))
         (niwa-telemetry--dropped-keys nil)
         ;; Filter first: the report of what was dropped is only complete
         ;; once every nested value has been walked.
         (safe (niwa-telemetry--safe-props props))
         (dropped (nreverse niwa-telemetry--dropped-keys)))
    (unless (and goal (niwa-goal-p goal))
      (setq goal nil))
    (append
     (list :schema-version niwa-telemetry-schema-version
           :time (format-time-string "%FT%T.%3N%z")
           :elapsed-ms
           (setq niwa-telemetry--last-elapsed-ms
                 (max niwa-telemetry--last-elapsed-ms
                      (round
                       (* 1000.0
                          (- (niwa-telemetry--monotonic-now)
                             niwa-telemetry--origin)))))
           :sequence (cl-incf niwa-telemetry--sequence)
           :event event
           :turn (or niwa-telemetry--event-turn
                     (ignore-errors (niwa-session-turn-count session))
                     0))
     (when (and (eq session niwa-telemetry--profiler-session)
                niwa-telemetry--profiler-run-id)
       (list :profiler-run-id niwa-telemetry--profiler-run-id))
     (when preset (list :preset preset))
     (when goal
       (list :goal-id (niwa-goal-id goal)
             :goal-status (niwa-goal-status goal)
             :goal-tokens-used (niwa-goal-tokens-used goal)
             :goal-turns-run (niwa-goal-turns-run goal)))
     (when (and goal (niwa-goal-tokens-incomplete-p goal))
       (list :goal-tokens-incomplete-p t))
     safe
     (when dropped (list :dropped-keys dropped)))))


;;
;;; Persistence

(defun niwa-telemetry--remote-p (session)
  "Return non-nil when SESSION's execution target is remote."
  (when-let* ((target (niwa-session-execution-target session)))
    (niwa-execution-target-remote-p target)))

(defun niwa-telemetry--entry-text (entry)
  "Return telemetry ENTRY in its durable line format."
  (let ((print-length nil)
        (print-level nil)
        (print-quoted t)
        (print-escape-newlines t))
    (concat (prin1-to-string entry) "\n")))

(defun niwa-telemetry--persist-content (session content)
  "Append serialized telemetry CONTENT to SESSION's persistent stream."
  (when-let* ((file (and niwa-telemetry-enabled
                         (niwa-telemetry-path session))))
    (condition-case err
        (if (niwa-telemetry--remote-p session)
            (niwa-session-publication-append-diagnostic
             session file content)
          ;; Session persistence owns the directory; a late event must not
          ;; undo deletion.  Pin the coding system on the hot path.
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region content nil file t 'silent))
          t)
      (error
       (niwa-diagnostic-message (niwa-diagnostic-scope session) "Telemetry persistence failed: %s"
                (error-message-string err))
       nil))))

(defun niwa-telemetry--persist (session entry)
  "Append telemetry ENTRY to SESSION's persistent stream."
  (niwa-telemetry--persist-content
   session (niwa-telemetry--entry-text entry)))

(defun niwa-telemetry-record (session event &rest props)
  "Record telemetry EVENT and PROPS for SESSION.
The event is buffered until SESSION has a persistent directory.  Raw payload
keys are always discarded.  Return the sanitized event plist."
  (when (and niwa-telemetry-enabled session)
    ;; Lag is a property of an interactive editor; batch Emacs has no event
    ;; loop a user waits on.
    (when (and (memq event '(request-start request-settled))
               (not noninteractive))
      (niwa-telemetry--lag-request session event (plist-get props :request-id)))
    (condition-case err
        (let ((entry (niwa-telemetry--envelope session event props))
              (pending (niwa-session-telemetry-pending session)))
          (if (and (null pending)
                   (niwa-session-save-path session)
                   (not (niwa-telemetry--remote-p session))
                   (niwa-telemetry--persist session entry))
              nil
            ;; Newest first: appending kept the queue chronological but
            ;; cost the whole list per event; the flush restores order.
            (setf (niwa-session-telemetry-pending session)
                  (cons entry pending)))
          entry)
      (error
       (niwa-diagnostic-message (niwa-diagnostic-scope session) "Telemetry event failed: %s"
                (error-message-string err))
       nil))))

(defvar niwa-telemetry--workspace-batch nil
  "Dynamically collected workspace rows for one heartbeat, or nil.
Keys are physical workspace root names; values hold workspace and row texts.")

(defun niwa-telemetry--append-workspace (workspace content)
  "Append bounded CONTENT to WORKSPACE with directory setup in one program."
  (niwa-telemetry--lag-observer-call
   (lambda ()
     (let* ((root (niwa-workspace-root workspace))
            (control (file-name-concat root ".niwa"))
            (state (file-name-concat control "state"))
            (directory (file-name-concat state "diagnostics")))
       (niwa-session-control-fs-program-value
        (car (last
              (niwa-session-control-fs-run-program
               (list (list :op 'make-directory :path control :optional t)
                     (list :op 'make-directory :path state :optional t)
                     (list :op 'make-directory :path directory :optional t)
                     (list :op 'append-rotating
                           :path (file-name-concat directory niwa-telemetry-file-name)
                           :content (concat (number-to-string (* 10 1024 1024)) "\n" content)))
               root))))))))

;;;###autoload
(defun niwa-telemetry-record-workspace (workspace event &rest props)
  "Append bounded EVENT and PROPS to WORKSPACE's target diagnostic stream.
This uses the ordinary telemetry envelope and privacy filter without a session.
Failures are diagnostic only and never alter the background operation."
  (when (and niwa-telemetry-enabled workspace)
    (condition-case err
        (let* ((entry (niwa-telemetry--envelope nil event props))
               (text (niwa-telemetry--entry-text entry)))
          (if niwa-telemetry--workspace-batch
              (let* ((root (file-name-as-directory (expand-file-name (niwa-workspace-root workspace))))
                     (batch (or (gethash root niwa-telemetry--workspace-batch)
                                ;; IO while another batch flushes may rebind
                                ;; the original workspace.  Keep this row's root.
                                (puthash root (list (niwa-workspace--create :root root))
                                         niwa-telemetry--workspace-batch))))
                (setcdr batch (cons text (cdr batch))))
            (niwa-telemetry--append-workspace workspace text))
          entry)
      (error
       (niwa-diagnostic-message (niwa-diagnostic-scope workspace) "Workspace telemetry failed: %s" (error-message-string err))
       nil))))

(defun niwa-telemetry-forwarded-audit-p (session)
  "Return non-nil when SESSION audits into a distinct durable session."
  (let ((target (niwa-session-audit-target session)))
    (and target (not (eq target session)))))

(defun niwa-telemetry-record-audit (session event &rest props)
  "Record audit EVENT and PROPS on SESSION's durable audit target.

Transient conversations audit into a durable parent session (see
`niwa-session-audit-target'); their events are tagged with a
`:conversation-scope' so consumers can distinguish forwarded events.
Callers own reducing PROPS to sanitized categorical values before
anything crosses into the durable target."
  (when-let* ((target (niwa-session-audit-target session)))
    (apply #'niwa-telemetry-record target event
           (if (eq target session)
               props
             (append '(:conversation-scope btw) props)))))

(defun niwa-telemetry-flush (session)
  "Persist SESSION's queued telemetry, retaining failed entries."
  (when session
    ;; The queue is stored newest first; flush in recording order.
    (let ((pending (reverse (niwa-session-telemetry-pending session))))
      (if (and pending (niwa-telemetry--remote-p session))
          (when (niwa-telemetry--persist-content
                 session (mapconcat #'niwa-telemetry--entry-text
                                    pending ""))
            (setf (niwa-session-telemetry-pending session) nil))
        (let (remaining)
          (dolist (entry pending)
            (unless (niwa-telemetry--persist session entry)
              (push entry remaining)))
          (setf (niwa-session-telemetry-pending session)
                remaining))))))


;;
;;; Event-loop lag

(defcustom niwa-telemetry-lag-threshold 0.5
  "Event-loop delay, in seconds, recorded as an `event-loop-lag' event.
Delays above `niwa-telemetry--lag-count-threshold' are only counted, in
the `event-loop-lag-summary' each settled request records."
  :type 'number
  :group 'niwa)

(defconst niwa-telemetry--lag-interval 0.1
  "Seconds between event-loop samples.
A native timed condition wait lets a worker sample without dispatching
callbacks or redisplaying.  Without it, the shared main-thread timer
preserves sampling and process ownership at the cost of redisplay.")

(defconst niwa-telemetry--lag-pgtk-interval 1
  "Seconds between fallback timer samples while a PGTK frame exists.
Stock PGTK presents a whole frame on every timer wakeup: a 100 ms timer
cost 2.8% editor CPU idle at scale 1 and 12.4% on HiDPI, against 0.3% for
a whole-second one.  Timer callbacks still time their own run time.")

(defconst niwa-telemetry--lag-count-threshold 0.2
  "Event-loop delay, in seconds, counted in a request's lag summary.")

(defconst niwa-telemetry--memory-interval 300
  "Seconds between `memory-sample' events while the heartbeat watches a session.")

(defvar niwa-telemetry--memory-due nil
  "Time the next `memory-sample' is due, or nil for the next heartbeat.
Kept across heartbeat restarts, so short requests do not sample more often.")

(defconst niwa-telemetry--lag-tail 120
  "Seconds the heartbeat keeps watching a session after its request settles.
Journal, collection and publication work follows settlement and can stall
the editor as much as the request did.")

(defvar niwa-telemetry--lag-thread nil
  "Heartbeat worker using native timed waits, or nil.")

(defvar niwa-telemetry--lag-timer nil
  "Coalesced fallback heartbeat timer, or nil.")

(defvar niwa-telemetry--lag-due nil
  "Float time the next heartbeat is due.")

(defvar niwa-telemetry--lag-since nil
  "Float time the previous heartbeat ran, or watching started.")

(defvar niwa-telemetry--lag-late 0
  "Longest stall seen since the previous heartbeat, in seconds.
A timer's lateness, or the run time of a timer callback or a blocking
remote operation.  Time the previous heartbeat already saw is not counted
again.")

(defvar niwa-telemetry--lag-slowest nil
  "The slowest timer callback since the last heartbeat, or nil.
A plist (:name NAME :seconds SECONDS :gc-seconds GC :gc-count COUNT), GC
and COUNT being the collection time and count inside that callback.")

(defvar niwa-telemetry--lag-gc nil
  "(GCS-DONE . GC-ELAPSED) when the previous heartbeat ran.")

(defvar niwa-telemetry--lag-cpu nil
  "Emacs CPU seconds when the previous heartbeat ran.")

(defvar niwa-telemetry--lag-input nil
  "`num-nonmacro-input-events' when the previous heartbeat ran.")

(defvar niwa-telemetry--lag-observer-start nil
  "Start time of the current workspace diagnostic append, or nil.
Shared with the native sampler so it cannot observe its own persistence.")

(defvar niwa-telemetry--lag-observer-count 0
  "Diagnostic calls active across all threads.")

(defvar niwa-telemetry--lag-observer-due nil
  "Heartbeat deadline when the current shared diagnostic interval began.")

(defvar niwa-telemetry--lag-observer-seconds 0
  "Cumulative workspace diagnostic time excluded from enclosing operations.")

(defvar niwa-telemetry--lag-observer-level 0
  "Dynamic nesting of diagnostic calls in this thread.")

(defvar niwa-telemetry--lag-remote-observer-level 0
  "Diagnostic nesting owning the currently running remote operation.")

(defconst niwa-telemetry--lag-dispatch-scan-limit 96
  "Maximum frames checked for dispatch inside an observed remote operation.")

(defun niwa-telemetry--lag-dispatched-in-observer-p ()
  "Return non-nil for a supported dispatch boundary inside the observer.
Read one frame's function at a time, retaining no arguments or stack dump.
The exact enclosing observer call ends the scan: waits below that owner do
not qualify.  Missing owners and exhausted scans conservatively retain
observer ownership.  This recognizes dispatch, not arbitrary callback intent."
  (let ((index 0) boundary found function)
    (while (and (not found) (< index niwa-telemetry--lag-dispatch-scan-limit)
                (setq function (cadr (backtrace-frame index))))
      (cond
       ((eq function 'niwa-telemetry--lag-observer-remote-call) (setq found t))
       ((memq function '(accept-process-output sit-for sleep-for
                        process-send-string process-send-region))
        (setq boundary t)))
      (setq index (1+ index)))
    (and found boundary)))

(defun niwa-telemetry--lag-observer-remote-call (original arguments)
  "Apply observer-owned ORIGINAL to ARGUMENTS, marking its exact frame."
  (let ((niwa-telemetry--lag-remote-observer-level niwa-telemetry--lag-observer-level))
    (apply original arguments)))

(defun niwa-telemetry--lag-observer-call (function)
  "Call diagnostic FUNCTION without feeding its own IO back into lag rows.
This excludes observer cost, not application work; synchronous diagnostic IO
still blocks input.  Two clock reads and scalar updates add no target IO."
  (if (not (or niwa-telemetry--lag-thread niwa-telemetry--lag-timer))
      (funcall function)
    (let ((start (float-time))
          (niwa-telemetry--lag-observer-level (1+ niwa-telemetry--lag-observer-level))
          (observed niwa-telemetry--lag-observer-seconds))
      (when (zerop niwa-telemetry--lag-observer-count)
        (setq niwa-telemetry--lag-observer-start start
              niwa-telemetry--lag-observer-due niwa-telemetry--lag-due))
      (cl-incf niwa-telemetry--lag-observer-count)
      (unwind-protect (funcall function)
        (let* ((end (float-time))
               ;; Charge nested diagnostic calls when they return, so a timer
               ;; dispatched during outer IO can exclude its own append too.
               (elapsed (max 0 (- end start (- niwa-telemetry--lag-observer-seconds observed)))))
          (cl-incf niwa-telemetry--lag-observer-seconds elapsed)
          (cl-decf niwa-telemetry--lag-observer-count)
          ;; Threads can finish in either order.  Only the last active call
          ;; closes the shared interval; restoring a captured start would
          ;; resurrect a marker whose owning thread has already returned.
          (when (zerop niwa-telemetry--lag-observer-count)
            (when niwa-telemetry--lag-since
              ;; A restarted sampler can begin inside the old observer union.
              ;; Exclude only its overlap with this sampler's baseline.
              (cl-incf niwa-telemetry--lag-since
                       (max 0 (- end (max niwa-telemetry--lag-observer-start
                                          niwa-telemetry--lag-since)))))
            (when niwa-telemetry--lag-due
              (setq niwa-telemetry--lag-due
                    (if (equal niwa-telemetry--lag-observer-due niwa-telemetry--lag-due)
                        (+ niwa-telemetry--lag-due (- end niwa-telemetry--lag-observer-start))
                      (+ end niwa-telemetry--lag-interval))))
            (setq niwa-telemetry--lag-observer-start nil
                  niwa-telemetry--lag-observer-due nil)))))))

(defun niwa-telemetry--cpu-seconds ()
  "Return the CPU seconds this Emacs has used."
  (let ((time (current-cpu-time)))
    (/ (float (car time)) (cdr time))))

(defvar niwa-telemetry--lag-windows nil
  "Alist from watched session to its lag window.
A window is (:request-id ID :until TIME :counts COUNTS).  UNTIL is nil
while the request runs and the end of the tail afterwards; COUNTS is a
plist of the delays counted for the running request.")

(defvar niwa-telemetry--lag-pass-windows nil
  "Workspace memory-pass windows sharing the session heartbeat.
Each entry is (WORKSPACE . (:pass-id ID :until TIME :counts COUNTS)).
Entries are opaque lifecycle handles, compared by identity, so overlapping
passes never replace one another or collide with session windows.")

(defconst niwa-telemetry--lag-closure-scan-limit 400
  "Most conses searched in an interpreted closure's body for its callee.")

(defconst niwa-telemetry--lag-closure-callee-count 3
  "Most callees naming a closure.
One callee named small wrappers rather than their work: the tool pipeline's
continuation was reported by its transport check, and the view spinner by
the timer cancellation it guards with.")

(defun niwa-telemetry--lag-closure-callees (function)
  "Return the functions closure FUNCTION calls that best name it.
A byte-compiled closure, native-compiled ones included, keeps its callees
among its constants; an interpreted one in its body, searched only up to
`niwa-telemetry--lag-closure-scan-limit' conses.  Return at most
`niwa-telemetry--lag-closure-callee-count' `niwa-' functions in
order, predicates only when nothing else names it, else the first
non-primitive function alone."
  (let ((budget niwa-telemetry--lag-closure-scan-limit)
        symbols)
    (cond
     ((byte-code-function-p function)
      (mapc (lambda (constant) (when (symbolp constant) (push constant symbols)))
            (aref function 2)))
     ((interpreted-function-p function)
      (let ((stack (list (aref function 1))))
        (while (and stack (> budget 0))
          (let ((form (pop stack)))
            (cond
             ((symbolp form) (push form symbols))
             ((consp form)
              (setq budget (1- budget))
              (push (cdr form) stack)
              (push (car form) stack))))))))
    (setq symbols
          (delete-dups
           (seq-filter (lambda (symbol)
                         (and symbol (functionp symbol)
                              (not (primitive-function-p (indirect-function symbol)))))
                       (nreverse symbols))))
    (let* ((own (seq-filter (lambda (symbol)
                              (string-prefix-p "niwa-" (symbol-name symbol)))
                            symbols))
           (work (seq-remove (lambda (symbol)
                               (string-suffix-p "-p" (symbol-name symbol)))
                             own)))
      (or (seq-take (or work own) niwa-telemetry--lag-closure-callee-count)
          (and symbols (list (car symbols)))))))

(defun niwa-telemetry--lag-function-name (function arguments)
  "Return a label for FUNCTION called with ARGUMENTS.
A closure is labelled `closure:' plus the functions it calls, which names
what it does rather than where it was created.  A named function whose
`niwa-telemetry-delegate' property is an argument index runs the
function at that index; its label continues with that function's, after
`>', since the trampoline alone does not say what work ran."
  (cond
   ((and (symbolp function) function)
    (let* ((index (get function 'niwa-telemetry-delegate))
           (delegate (and (natnump index) (nth index arguments))))
      (if (and delegate (functionp delegate))
          (concat (symbol-name function) ">"
                  (niwa-telemetry--lag-function-name delegate nil))
        (symbol-name function))))
   ((when-let* ((callees (niwa-telemetry--lag-closure-callees function)))
      (concat "closure:" (mapconcat #'symbol-name callees ","))))
   (t "anonymous")))

(defun niwa-telemetry--lag-callback-name (timer)
  "Return a label for TIMER's callback."
  (niwa-telemetry--lag-function-name (timer--function timer)
                                        (timer--args timer)))

(defun niwa-telemetry--lag-time-callback (original timer)
  "Run ORIGINAL on TIMER, counting its lateness and run time as lag.
A callback blocks input while it runs, and timers run most of niwa's
deferred work, so this times most stalls even between whole-second
fallback heartbeats, and names the slowest.  Collection inside the
callback is kept apart from its own work.  The heartbeat's own tick is
not timed.

A stall delays every timer due during it, so TIMER's lateness measures the
loop as well as a heartbeat would.  Idle timers are due relative to input,
not the clock, and are not counted."
  (if (memq (timer--function timer)
            '(niwa--coalesced-timer-tick niwa-telemetry--lag-tick))
      (funcall original timer)
    (let ((start (float-time))
          (observer niwa-telemetry--lag-observer-seconds)
          (gc-count gcs-done)
          (gc-seconds gc-elapsed))
      (when-let* (((not niwa-telemetry--lag-observer-start))
                  ((not (timer--idle-delay timer)))
                  (due (timer--time timer))
                  ((car due)))
        (setq niwa-telemetry--lag-late
              (max niwa-telemetry--lag-late
                   (- start (max (float-time due)
                                 (or niwa-telemetry--lag-since start))))))
      (unwind-protect (funcall original timer)
        (let ((elapsed (max 0 (- (float-time) start
                                (- niwa-telemetry--lag-observer-seconds observer)))))
          (setq niwa-telemetry--lag-late
                (max niwa-telemetry--lag-late
                     (min elapsed
                          (- (float-time) (max start (or niwa-telemetry--lag-since start))))))
          (when (> elapsed (or (plist-get niwa-telemetry--lag-slowest :seconds) 0))
            (setq niwa-telemetry--lag-slowest
                  (list :name (niwa-telemetry--lag-callback-name timer)
                        :seconds elapsed
                        :gc-seconds (- gc-elapsed gc-seconds)
                        :gc-count (- gcs-done gc-count)))))))))

(defun niwa-telemetry--lag-time-remote (original &rest arguments)
  "Apply ORIGINAL to ARGUMENTS, counting a blocking remote operation as lag.
TRAMP waits for a remote command in `accept-process-output' and `sit-for',
running timers, so the heartbeat runs on time while the editor
cannot run a command.  The outermost operation on the main thread is
therefore counted as late as it took, once it returns."
  (cond
   ((and niwa-telemetry--lag-observer-start
         (> niwa-telemetry--lag-observer-level 0)
         (or (> niwa-telemetry--lag-observer-level niwa-telemetry--lag-remote-observer-level)
             (not (niwa-telemetry--lag-dispatched-in-observer-p))))
    ;; Synchronous TRAMP handler nesting belongs to the diagnostic program.
    ;; A callback dispatched inside its wait is independently observable.
    (niwa-telemetry--lag-observer-remote-call original arguments))
   ((or (and (> niwa-transport--depth 0) (not niwa-telemetry--lag-observer-start))
        (not (eq (current-thread) main-thread)))
    (apply original arguments))
   (t
    (let ((start (float-time))
          (observer niwa-telemetry--lag-observer-seconds))
      (unwind-protect (apply original arguments)
        (setq niwa-telemetry--lag-late
              (max niwa-telemetry--lag-late
                   (- (float-time) start
                      (- niwa-telemetry--lag-observer-seconds observer)))))))))

(defun niwa-telemetry--lag-watch ()
  "Sample until this thread is replaced or stopped.
Native condition waits release the interpreter without dispatching process
sentinels or timers.  Ordinary `sleep-for' is unsafe in this worker.

The worker samples from its own hidden buffer.  A new thread keeps the buffer
that was current when it was made, usually a view or a memory pass's origin,
and each wakeup makes that buffer current again.  Its local face remappings,
such as those of `solaire-mode', would then apply to redisplay and repaint
other windows with the wrong background."
  (let* ((thread (current-thread))
         ;; Dynamic bindings are thread-local: native observer IO must not be
         ;; subtracted from unrelated main-thread callbacks or remote work.
         (niwa-telemetry--lag-observer-seconds 0)
         (mutex (make-mutex "niwa-lag"))
         (gate (make-condition-variable mutex "niwa-lag")))
    (with-current-buffer (get-buffer-create " *niwa-lag-heartbeat*" t)
      (setq default-directory temporary-file-directory))
    (set-buffer (get-buffer " *niwa-lag-heartbeat*"))
    (condition-case err
        (with-mutex mutex
          (while (eq thread niwa-telemetry--lag-thread)
            (let ((remaining (- niwa-telemetry--lag-due (float-time))))
              (if (> remaining 0)
                  (niwa-native--condition-wait gate remaining)
                (niwa-telemetry--lag-tick)))))
      ((error quit)
       (when (eq thread niwa-telemetry--lag-thread)
         (niwa-telemetry--lag-stop)
         (niwa-diagnostic-message 'global "Event-loop lag watching stopped: %s"
                  (error-message-string err)))))))

(defun niwa-telemetry--lag-start ()
  "Start callback timing and the lowest-overhead safe heartbeat available."
  (unless (or niwa-telemetry--lag-thread niwa-telemetry--lag-timer)
    ;; Resolve the optional module before starting the sampling clock.
    ;; Elsewhere a timer wakeup that changes nothing does not redraw.
    (let ((native (and (eq system-type 'gnu/linux) (featurep 'pgtk)
                       (niwa-native-load)
                       (niwa-native--timed-wait-available-p))))
      ;; Loading can wait for a compiler while another request starts.
      (unless (or niwa-telemetry--lag-thread niwa-telemetry--lag-timer)
        (setq niwa-telemetry--lag-since (float-time)
              niwa-telemetry--lag-late 0
              niwa-telemetry--lag-slowest nil
              niwa-telemetry--lag-gc (cons gcs-done gc-elapsed)
              niwa-telemetry--lag-cpu (niwa-telemetry--cpu-seconds)
              niwa-telemetry--lag-input num-nonmacro-input-events
              niwa-telemetry--lag-due (+ (float-time) niwa-telemetry--lag-interval))
        (when native
          (setq niwa-telemetry--lag-thread
                (condition-case nil
                    (make-thread #'niwa-telemetry--lag-watch
                                 "niwa-lag-heartbeat" 'silently)
                  (error nil))))
        (unless niwa-telemetry--lag-thread
          (setq niwa-telemetry--lag-timer
                (niwa--run-periodic-timer
                 'global
                 (if (seq-some (lambda (frame) (eq (framep frame) 'pgtk))
                               (frame-list))
                     niwa-telemetry--lag-pgtk-interval
                   niwa-telemetry--lag-interval)
                 #'niwa-telemetry--lag-tick)
                niwa-telemetry--lag-due
                (float-time (timer--time niwa-telemetry--lag-timer))))))
    (advice-add 'timer-event-handler :around
                #'niwa-telemetry--lag-time-callback)
    (advice-add 'niwa--coalesced-timer-call :around
                #'niwa-telemetry--lag-time-callback)
    (advice-add 'niwa-transport-call-as-remote-operation :around
                #'niwa-telemetry--lag-time-remote)))

(defun niwa-telemetry--lag-stop ()
  "Stop the heartbeat and callback timing, joining any sleeping worker."
  (let ((thread niwa-telemetry--lag-thread))
    (setq niwa-telemetry--lag-thread nil)
    (when (and (threadp thread) (thread-live-p thread)
               (not (eq thread (current-thread))))
      (thread-signal thread 'error '("Heartbeat stopped"))
      (ignore-errors (thread-join thread))))
  (when (timerp niwa-telemetry--lag-timer)
    (niwa--ui-timer-cancel niwa-telemetry--lag-timer))
  (setq niwa-telemetry--lag-timer nil
        niwa-telemetry--lag-since nil
        niwa-telemetry--lag-late 0
        niwa-telemetry--lag-gc nil
        niwa-telemetry--lag-cpu nil
        niwa-telemetry--lag-input nil
        niwa-telemetry--lag-windows nil
        niwa-telemetry--lag-pass-windows nil)
  (advice-remove 'timer-event-handler #'niwa-telemetry--lag-time-callback)
  (advice-remove 'niwa--coalesced-timer-call #'niwa-telemetry--lag-time-callback)
  (advice-remove 'niwa-transport-call-as-remote-operation
                 #'niwa-telemetry--lag-time-remote))

(defun niwa-telemetry--lag-request (session event request-id)
  "Open or close SESSION's lag window for request EVENT with REQUEST-ID."
  (pcase event
    ('request-start
     (setf (alist-get session niwa-telemetry--lag-windows nil nil #'eq)
           (list :request-id request-id :until nil :counts nil))
     (niwa-telemetry--lag-start))
    ('request-settled
     (when-let* ((window (alist-get session niwa-telemetry--lag-windows nil nil #'eq)))
       (unless (plist-get window :until)
         (let ((counts (plist-get window :counts)))
           (niwa-telemetry-record
            session 'event-loop-lag-summary
            :request-id (plist-get window :request-id)
            :over-200-ms (or (plist-get counts :over-200-ms) 0)
            :over-500-ms (or (plist-get counts :over-500-ms) 0)
            :over-1000-ms (or (plist-get counts :over-1000-ms) 0)
            :max-ms (or (plist-get counts :max-ms) 0)
            :input-pending (or (plist-get counts :input-pending) 0)))
         (setf (alist-get session niwa-telemetry--lag-windows nil nil #'eq)
               (list :request-id (plist-get window :request-id)
                     :until (+ (float-time) niwa-telemetry--lag-tail)
                     :counts nil)))))))

(defun niwa-telemetry--lag-count (counts delay input-pending)
  "Return COUNTS with DELAY seconds counted, INPUT-PENDING or not."
  (let ((ms (round (* 1000 delay))))
    (dolist (bucket '((0.2 . :over-200-ms) (0.5 . :over-500-ms) (1.0 . :over-1000-ms)))
      (when (> delay (car bucket))
        (setq counts (plist-put counts (cdr bucket)
                                (1+ (or (plist-get counts (cdr bucket)) 0))))))
    (when input-pending
      (setq counts (plist-put counts :input-pending
                              (1+ (or (plist-get counts :input-pending) 0)))))
    (plist-put counts :max-ms (max ms (or (plist-get counts :max-ms) 0)))))

(defun niwa-telemetry--lag-record (entry event &rest props)
  "Record lag EVENT and PROPS for window ENTRY's session or workspace."
  (if-let* ((pass-id (plist-get (cdr entry) :pass-id)))
      (apply #'niwa-telemetry-record-workspace (car entry) event
             :pass-id pass-id :workload 'memory props)
    (apply #'niwa-telemetry-record (car entry) event props)))

;;;###autoload
(defun niwa-telemetry-memory-pass-start (workspace pass-id)
  "Watch WORKSPACE's memory PASS-ID using the shared lag heartbeat.
Return an opaque handle for `niwa-telemetry-memory-pass-settle', or nil
when telemetry is disabled or Emacs is noninteractive.  PASS-ID must be a
string identifier, never evidence or focus text.  No session is required."
  (when (and niwa-telemetry-enabled (not noninteractive)
             workspace (stringp pass-id))
    (let ((handle (cons workspace
                        (list :pass-id (niwa-telemetry--truncate-string pass-id)
                              :until nil :counts nil)))
          started)
      (push handle niwa-telemetry--lag-pass-windows)
      (condition-case err
          (unwind-protect
              (progn
                (niwa-telemetry--lag-start)
                (setq started t)
                handle)
            (unless started
              (let ((inhibit-quit t))
                (setq niwa-telemetry--lag-pass-windows
                      (delq handle niwa-telemetry--lag-pass-windows))
                ;; Startup can allocate the sampler before a later failure.
                ;; Never stop another session's or pass's shared watcher.
                (unless (or niwa-telemetry--lag-windows
                            niwa-telemetry--lag-pass-windows)
                  (niwa-telemetry--lag-stop)))))
        (error
         (niwa-diagnostic-message (niwa-diagnostic-scope workspace) "Memory pass lag watching failed: %s"
                  (error-message-string err))
         nil)))))

;;;###autoload
(defun niwa-telemetry-memory-pass-settle (handle)
  "Summarize HANDLE once and retain its ordinary post-settlement lag tail.
HANDLE comes from `niwa-telemetry-memory-pass-start'.  Nil, already
settled, expired and stopped handles are harmless.  Return nil."
  (when (memq handle niwa-telemetry--lag-pass-windows)
    (if (not niwa-telemetry-enabled)
        (niwa-telemetry--lag-stop)
      (let ((window (cdr handle)))
        (unless (plist-get window :until)
          (let ((counts (plist-get window :counts)))
            ;; Close before appending: remote persistence may run timers.
            (setf (plist-get window :until)
                  (+ (float-time) niwa-telemetry--lag-tail)
                  (plist-get window :counts) nil)
            (niwa-telemetry--lag-record
             handle 'event-loop-lag-summary
             :over-200-ms (or (plist-get counts :over-200-ms) 0)
             :over-500-ms (or (plist-get counts :over-500-ms) 0)
             :over-1000-ms (or (plist-get counts :over-1000-ms) 0)
             :max-ms (or (plist-get counts :max-ms) 0)
             :input-pending (or (plist-get counts :input-pending) 0)))))))
  nil)

(defun niwa-telemetry--lag-publish-workspace-batch (batch)
  "Append BATCH's sampled workspace rows on the main thread."
  (if (not (eq (current-thread) main-thread))
      ;; A synchronous target program can enter GTK through Emacs's wait loop.
      (run-at-time 0.01 nil #'niwa-telemetry--lag-publish-workspace-batch batch)
    (maphash
     (lambda (_root rows)
       (when niwa-telemetry-enabled
         (condition-case err
             (niwa-telemetry--append-workspace
              (car rows) (apply #'concat (nreverse (cdr rows))))
           (error (niwa-diagnostic-message (niwa-diagnostic-scope (car rows)) "Workspace telemetry failed: %s"
                    (error-message-string err))))))
     batch)))

(defun niwa-telemetry--lag-tick ()
  "Record the loop's worst delay since the previous heartbeat.
The delay is the later of this heartbeat's own, any timer's and any
blocking remote operation's since then, once per heartbeat for every
watched session or memory pass.  Input since the previous sample is reported as
pending.  Disabling telemetry stops watching: the request it would close
never reports settling."
  (if (not niwa-telemetry-enabled)
      (niwa-telemetry--lag-stop)
    (if niwa-telemetry--lag-observer-start
        ;; A remote append can dispatch the fallback timer; a native sampler
        ;; can also wake while the append waits.  Neither observes its own IO.
        (setq niwa-telemetry--lag-due (+ (float-time) niwa-telemetry--lag-interval))
    (let* ((now (float-time))
           (delay (max (- now (or niwa-telemetry--lag-due now))
                       niwa-telemetry--lag-late))
           (slowest niwa-telemetry--lag-slowest)
           (gc (or niwa-telemetry--lag-gc (cons gcs-done gc-elapsed)))
           (cpu (niwa-telemetry--cpu-seconds))
           (cpu-before (or niwa-telemetry--lag-cpu cpu))
           (input-before (or niwa-telemetry--lag-input num-nonmacro-input-events))
           (niwa-telemetry--workspace-batch nil))
      (setq niwa-telemetry--lag-due
            (if niwa-telemetry--lag-timer
                (float-time (timer--time niwa-telemetry--lag-timer))
              (+ now niwa-telemetry--lag-interval))
            niwa-telemetry--lag-input num-nonmacro-input-events
            niwa-telemetry--lag-since now
            niwa-telemetry--lag-late 0
            niwa-telemetry--lag-slowest nil
            niwa-telemetry--lag-gc (cons gcs-done gc-elapsed)
            niwa-telemetry--lag-cpu cpu
            niwa-telemetry--lag-windows
            (seq-filter (lambda (entry)
                          (let ((until (plist-get (cdr entry) :until)))
                            (or (null until) (< now until))))
                        niwa-telemetry--lag-windows))
      (when niwa-telemetry--lag-pass-windows
        (setq niwa-telemetry--lag-pass-windows
              (seq-filter (lambda (entry)
                            (let ((until (plist-get (cdr entry) :until)))
                              (or (null until) (< now until))))
                          niwa-telemetry--lag-pass-windows)))
      (when (and niwa-telemetry--lag-pass-windows
                 (> delay niwa-telemetry-lag-threshold))
        (setq niwa-telemetry--workspace-batch (make-hash-table :test #'equal)))
      (when (> delay niwa-telemetry--lag-count-threshold)
        (let ((input-pending (> num-nonmacro-input-events input-before)))
          (dolist (entry (if niwa-telemetry--lag-pass-windows
                            (append niwa-telemetry--lag-windows
                                    niwa-telemetry--lag-pass-windows)
                          niwa-telemetry--lag-windows))
            (let ((window (cdr entry)))
              (unless (plist-get window :until)
                (setf (plist-get (cdr entry) :counts)
                      (niwa-telemetry--lag-count
                       (plist-get window :counts) delay input-pending)))
              (when (> delay niwa-telemetry-lag-threshold)
                (apply #'niwa-telemetry--lag-record
                       entry 'event-loop-lag
                       :request-id (plist-get window :request-id)
                       :delay-ms (round (* 1000 delay))
                       :input-pending input-pending
                       :settled (and (plist-get window :until) t)
                       :command-name (and (symbolp this-command) this-command
                                          (symbol-name this-command))
                       ;; Collection anywhere since the previous heartbeat:
                       ;; timers, process output, redisplay and commands.
                       :gc-count (- gcs-done (car gc))
                       :gc-ms (round (* 1000 (- gc-elapsed (cdr gc))))
                       ;; Near the delay for a busy editor, near zero for a
                       ;; suspended machine or a blocking wait on a child.
                       :cpu-ms (round (* 1000 (- cpu cpu-before)))
                       (and slowest
                            (list :timer-callback (plist-get slowest :name)
                                  :timer-ms (round (* 1000 (plist-get slowest :seconds)))
                                  :timer-gc-count (plist-get slowest :gc-count)
                                  :timer-gc-ms (round (* 1000 (plist-get slowest :gc-seconds)))))))))))
      (when niwa-telemetry--workspace-batch
        (let ((batch niwa-telemetry--workspace-batch)
              (niwa-telemetry--workspace-batch nil))
          (niwa-telemetry--lag-publish-workspace-batch batch)))
      (unless (or niwa-telemetry--lag-windows
                  niwa-telemetry--lag-pass-windows)
        (niwa-telemetry--lag-stop))
      (when (and niwa-telemetry--lag-windows
                 (>= now (or niwa-telemetry--memory-due 0)))
        (setq niwa-telemetry--memory-due (+ now niwa-telemetry--memory-interval))
        (niwa-telemetry--memory-sample (caar niwa-telemetry--lag-windows)))))))

(defun niwa-telemetry--memory-sample (session)
  "Record a `memory-sample' of this Emacs process for SESSION.
Separates resident memory into buffer text, image cache and the rest, which
garbage-collection timing cannot see.  Buffer names can carry titles and
paths, so the largest buffer is identified by mode and a name hash only."
  (let ((largest nil) (largest-size -1) (total 0) (count 0)
        ;; `process-attributes' follows a remote directory to its host.
        (default-directory temporary-file-directory))
    (dolist (buffer (buffer-list))
      (let ((size (buffer-size buffer)))
        (setq total (+ total size) count (1+ count))
        (when (> size largest-size)
          (setq largest buffer largest-size size))))
    (niwa-telemetry-record
     session 'memory-sample
     :rss-kb (alist-get 'rss (process-attributes (emacs-pid)))
     :buffers count :buffer-chars-total total
     :largest-buffer-chars (max largest-size 0)
     :largest-buffer-mode (and largest (buffer-local-value 'major-mode largest))
     :largest-buffer-process (and largest (get-buffer-process largest) t)
     :largest-buffer-hash (and largest (substring (secure-hash 'sha256 (buffer-name largest)) 0 16))
     :image-cache-bytes (and (fboundp 'image-cache-size) (image-cache-size))
     :frames (length (frame-list)) :threads (length (all-threads))
     :processes (length (process-list)))))


;;
;;; Measured synchronous work

(defvar niwa-telemetry--measuring-record nil
  "Non-nil while a measurement records its event.
A remote session appends telemetry through the same target programs a
measurement may wrap, so nothing is measured while one is recorded.")

(defun niwa-telemetry--allocated-bytes ()
  "Return an estimate of the bytes allocated so far in this Emacs.
Weights `memory-use-counts' by the object sizes of a 64-bit build; the
difference between two readings is what matters, not the absolute value."
  (let ((counts (memory-use-counts)))
    (+ (* 16 (nth 0 counts))            ; conses
       (* 16 (nth 1 counts))            ; floats
       (* 8 (nth 2 counts))             ; vector cells
       (* 48 (nth 3 counts))            ; symbols
       (nth 4 counts)                   ; string characters
       (* 56 (nth 5 counts))            ; intervals
       (* 32 (nth 6 counts)))))         ; strings

(defun niwa-telemetry-measure (session event min-ms function &rest props)
  "Call FUNCTION and record synchronous EVENT for SESSION; return its value.

The event carries PROPS plus `:duration-ms', the collections and their
time inside the call (`:gc-count', `:gc-ms'), an estimate of the bytes it
allocated (`:allocated-kb'), and `:outcome': the returned symbol, `ok'
for any other value, or `error' before the error propagates.  Allocation
matters separately from collection because `niwa--with-gc-batched'
defers collection past the call, into whatever runs next.

A nil SESSION records into every session the event-loop lag heartbeat
watches, and costs nothing when it watches none: sessionless work such as
a target program is only worth attributing to a pause.  Calls shorter than
MIN-MS milliseconds are not recorded."
  (let ((sessions (cond (niwa-telemetry--measuring-record nil)
                        (session (list session))
                        (t (mapcar #'car niwa-telemetry--lag-windows)))))
    (if (null sessions)
        (funcall function)
      (let ((start (float-time))
            (gc-count gcs-done)
            (gc-seconds gc-elapsed)
            (allocated (niwa-telemetry--allocated-bytes))
            (outcome 'error)
            value)
        (unwind-protect
            (progn
              (setq value (funcall function)
                    outcome (if (and value (symbolp value)) value 'ok))
              value)
          (let ((duration-ms (round (* 1000 (- (float-time) start)))))
            (when (>= duration-ms min-ms)
              (let ((niwa-telemetry--measuring-record t)
                    (gc-ms (round (* 1000 (- gc-elapsed gc-seconds))))
                    (gc-done (- gcs-done gc-count))
                    (allocated-kb
                     (/ (- (niwa-telemetry--allocated-bytes) allocated) 1024)))
                (dolist (target sessions)
                  (apply #'niwa-telemetry-record target event
                         :duration-ms duration-ms :gc-count gc-done :gc-ms gc-ms
                         :allocated-kb allocated-kb :outcome outcome props))))))))))


;;
;;; Asynchronous spans

(defun niwa-telemetry--span-id (event)
  "Return a process-unique identifier for EVENT."
  (format "%s-%s"
          event
          (substring
           (secure-hash
            'sha1
            (format "%s:%s:%s:%s"
                    (emacs-pid) (float-time) niwa-telemetry--sequence event))
           0 16)))

(defun niwa-telemetry-start (session event &rest props)
  "Start an asynchronous SESSION span named EVENT with PROPS.
Return an opaque span plist accepted by `niwa-telemetry-finish'."
  (let* ((span-id (niwa-telemetry--span-id event))
         (started-at (niwa-telemetry--monotonic-now)))
    (apply #'niwa-telemetry-record
           session event :stage 'start :span-id span-id props)
    (list :session session :event event :span-id span-id
          :started-at started-at
          ;; The span's own copy is filtered for storage; the drops are
          ;; reported by the event recorded just above, not collected here.
          :props (let ((niwa-telemetry--dropped-keys nil))
                   (niwa-telemetry--safe-props props)))))

(defun niwa-telemetry-finish (span &rest props)
  "Finish telemetry SPAN with PROPS and return the emitted event."
  (when span
    (let ((duration-ms
           (round (* 1000.0
                     (max 0.0
                          (- (niwa-telemetry--monotonic-now)
                             (plist-get span :started-at)))))))
      (apply #'niwa-telemetry-record
             (plist-get span :session)
             (plist-get span :event)
             :stage 'finish
             :span-id (plist-get span :span-id)
             :duration-ms duration-ms
             (append (plist-get span :props) props)))))


;;
;;; Profiler capture

(defun niwa-telemetry--guard-prompt (function original &rest args)
  "Record synchronous prompt FUNCTION, then call ORIGINAL with ARGS or fail."
  (let* ((prompt (car args))
         (text (and (stringp prompt) prompt))
         (session niwa-telemetry--profiler-session))
    (when session
      (niwa-telemetry-record
       session 'interactive-prompt-opened
       :prompt-function function
       :prompt-hash (and text (secure-hash 'sha256 text))
       :prompt-chars (and text (length text))
       :blocked (and niwa-telemetry-profiler-fail-on-prompt t)))
    (if niwa-telemetry-profiler-fail-on-prompt
        (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Interactive prompt blocked during telemetry run: %s"
                    function)
      (apply original args))))

(defun niwa-telemetry--install-prompt-guard ()
  "Install temporary synchronous prompt observation advices."
  (dolist (function '(ask-user-about-supersession-threat
                      yes-or-no-p y-or-n-p))
    (when (fboundp function)
      (let* ((prompt-function function)
             (advice
             (lambda (original &rest args)
               (apply #'niwa-telemetry--guard-prompt
                      prompt-function original args))))
        (advice-add function :around advice)
        (push (cons function advice) niwa-telemetry--prompt-advices)))))

(defun niwa-telemetry--remove-prompt-guard ()
  "Remove temporary synchronous prompt observation advices."
  (dolist (entry niwa-telemetry--prompt-advices)
    (advice-remove (car entry) (cdr entry)))
  (setq niwa-telemetry--prompt-advices nil))

(defun niwa-telemetry--process-output (program &rest args)
  "Return trimmed PROGRAM output for ARGS, or nil on failure."
  (with-temp-buffer
    (when (zerop (apply #'process-file program nil t nil args))
      (string-trim (buffer-string)))))

(defun niwa-telemetry--git-snapshot (directory)
  "Return a non-sensitive Git state snapshot for DIRECTORY."
  (let* ((default-directory directory)
         (head (ignore-errors
                 (niwa-telemetry--process-output
                  "git" "rev-parse" "HEAD")))
         (status (ignore-errors
                   (niwa-telemetry--process-output
                    "git" "status" "--short")))
         (diff (ignore-errors
                 (niwa-telemetry--process-output
                  "git" "diff" "--binary" "HEAD")))
         (untracked (ignore-errors
                      (niwa-telemetry--process-output
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

(defun niwa-telemetry--library-snapshot (feature)
  "Return current disk artifact identity and installed provenance for FEATURE.
Use loaded-feature history, never a different copy from `load-path'.
Hash and size describe the artifact's disk bytes, not its bytes at load time.
The commit comes from the source checkout, which the lookup reaches through
sibling source and symlinks, or from the package descriptor of a package.el
install; it does not establish source/bytecode equivalence.  Return nil for
unavailable artifacts, or a nil commit when only provenance is unavailable."
  (ignore-errors
    (when-let* (((featurep feature))
                (file (symbol-file feature 'provide))
                ((file-regular-p file))
                ((file-readable-p file)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (list :file-hash (secure-hash 'sha256 (current-buffer))
              :file-bytes (buffer-size)
              :git-head
              (plist-get (niwa-library-provenance file) :commit))))))

(defun niwa-telemetry--record-environment (session boundary)
  "Record reproduction environment for SESSION at BOUNDARY."
  (let* ((directory (niwa-session-working-directory session))
         (git (and directory (niwa-telemetry--git-snapshot directory)))
         (gptel (niwa-telemetry--library-snapshot 'gptel))
         (mode (niwa-sandbox-mode-effective session))
         (sandbox (and (not (eq mode 'off))
                       (ignore-errors (niwa-sandbox-probe directory)))))
    (apply #'niwa-telemetry-record
           session 'reproduction-environment
           :boundary boundary
           :emacs-version emacs-version
           :system-configuration system-configuration
           :gptel-version (and (boundp 'gptel-version) gptel-version)
           :gptel-file-hash (plist-get gptel :file-hash)
           :gptel-commit (plist-get gptel :git-head)
           :sandbox-mode mode
           :sandbox-preference (niwa-session-sandbox-mode session)
           :bubblewrap-available (and sandbox
                                       (plist-get sandbox :available))
           git)))

;;;###autoload
(defvar niwa-telemetry--profiler-prior-stack-depth nil
  "Value of `profiler-max-stack-depth' from before a niwa run raised it.
A run has to set the variable globally, because the C log fixes its backtrace
width when profiling starts.  Restoring it is the run's job.")

(defun niwa-telemetry--profiler-release ()
  "Release every global a profiler run owns."
  (niwa-telemetry--remove-prompt-guard)
  (when niwa-telemetry--profiler-prior-stack-depth
    (setq profiler-max-stack-depth
          niwa-telemetry--profiler-prior-stack-depth
          niwa-telemetry--profiler-prior-stack-depth nil))
  (setq niwa-telemetry--profiler-session nil
        niwa-telemetry--profiler-run-id nil))

(defun niwa-telemetry-profiler-start (&optional mode)
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
  (let ((session (niwa-telemetry-current-session))
        (mode (or mode 'cpu+mem)))
    (unless session
      (niwa-diagnostic-user-error 'unavailable "No niwa session in the current buffer"))
    (when niwa-telemetry--profiler-session
      (niwa-diagnostic-user-error (niwa-diagnostic-scope niwa-telemetry--profiler-session) "A niwa profiler run is already active"))
    ;; A cold session has no directory yet, so the run would have nowhere to
    ;; put its artifacts.  Materialize it here rather than making the user
    ;; send a throwaway turn first.
    (unless (niwa-session-save-path session)
      (niwa-session-artifacts-save
       session
       (or (niwa-session-persistence-root-buffer-for-session
            session (current-buffer))
           (current-buffer))
       nil t)
      (unless (niwa-session-save-path session)
        (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Could not materialize the niwa session for profiling")))
    (require 'profiler)
    (setq niwa-telemetry--profiler-run-id
          (format "run-%s-%s"
                  (format-time-string "%Y%m%dT%H%M%S")
                  (substring
                   (secure-hash 'sha1
                                (format "%s:%s" (emacs-pid) (float-time)))
                   0 8)))
    ;; The C log fixes its backtrace width when the profiler starts, so this
    ;; has to be set before the call, not around the report.
    (setq niwa-telemetry--profiler-prior-stack-depth profiler-max-stack-depth
          profiler-max-stack-depth niwa-telemetry-profiler-stack-depth)
    (profiler-start mode)
    ;; Past this line Emacs is profiling, so a failure in the rest of the
    ;; setup has to undo it: reporting that the run did not start while it
    ;; keeps sampling is the one outcome with no way back.
    (condition-case err
        (progn
          (setq niwa-telemetry--profiler-session session)
          (niwa-telemetry--install-prompt-guard)
          (niwa-telemetry--record-environment session 'start)
          (niwa-telemetry-record
           session 'profiler-started
           :mode mode
           :emacs-version emacs-version
           :system-configuration system-configuration)
          (message "niwa: profiler started for session %s"
                   (or (niwa-session-session-id session) "pending")))
      (error
       (condition-case-unless-debug rollback-error (profiler-stop)
         (error (niwa-diagnostic-message (niwa-diagnostic-scope session)
                                           "Profiler rollback failed: %S" rollback-error)))
       (niwa-telemetry--profiler-release)
       (signal (car err) (cdr err))))))

(defun niwa-telemetry--write-profiler-artifacts (directory)
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
(defun niwa-telemetry-profiler-stop ()
  "Stop the active niwa profiler and save its session artifacts."
  (interactive)
  (unless niwa-telemetry--profiler-session
    (niwa-diagnostic-user-error 'global "No niwa profiler run is active"))
  (let* ((session niwa-telemetry--profiler-session)
         (directory (niwa-telemetry-profiler-directory session))
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
              (niwa-telemetry--record-environment session 'stop)
              (make-directory directory t)
              (setq failure-stage 'save-artifacts)
              (let ((artifacts
                     (niwa-telemetry--write-profiler-artifacts directory)))
                (niwa-telemetry-record
                 session 'profiler-stopped
                 :modes (mapcar (lambda (artifact)
                                  (plist-get artifact :mode))
                                artifacts)
                 ;; Absolute, and on this client: for a session saved on a
                 ;; target, a path relative to the session directory would
                 ;; send a reader looking on the wrong machine.
                 :artifacts-directory directory
                 :artifacts-local
                 (not (file-remote-p (niwa-session-save-path session)))
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
                (message "niwa: profiler artifacts saved under %s"
                         directory)))
          (error
           (niwa-telemetry-record
            session 'profiler-stop-failed
            :failure-stage failure-stage
            :failure-class (car err))
           (signal (car err) (cdr err))))
      (niwa-telemetry--profiler-release))))

(defun niwa-telemetry--redact-gptel-debug-buffer ()
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

(defun niwa-telemetry--gptel-log-raw (orig data &optional type _no-json)
  "Append gptel log entries raw during a session debug capture.
ORIG is `gptel--log', which pretty-prints every DATA entry of TYPE.
On a debug capture the streamed response bodies are multi-megabyte
payloads in a log buffer that grows for the whole capture, and a
profiled capture spent a third of its CPU inside `json-pretty-print'.
Raw entries are also what reproduction tooling wants."
  (funcall orig data type t))

;;;###autoload
(defun niwa-session-debug ()
  "Toggle profiler and gptel debug-log capture for the current session.
Stopping writes the captured log suffix to gptel-debug.log in the profiler
run's diagnostics directory.  The log may contain raw request data and
connection settings."
  (interactive)
  (require 'gptel)
  (if niwa-telemetry--session-debug-marker
      (let* ((marker niwa-telemetry--session-debug-marker)
             (view-marker niwa-telemetry--session-debug-view-marker)
             (session niwa-telemetry--profiler-session))
        (unwind-protect
            (let* ((directory
                    (niwa-telemetry-profiler-directory session))
                   (gptel-file
                    (file-name-concat directory "gptel-debug.log"))
                   (view-file
                    (file-name-concat directory "view-render-debug.log"))
                   (default-file-modes #o600))
              (unless (marker-buffer marker)
                (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "The active gptel debug log buffer was killed"))
              (make-directory directory t)
              (with-temp-buffer
                (insert-buffer-substring (marker-buffer marker) marker)
                (niwa-telemetry--redact-gptel-debug-buffer)
                (write-region (point-min) (point-max)
                              gptel-file nil 'silent))
              (set-file-modes gptel-file #o600)
              (when (and (markerp view-marker)
                         (marker-buffer view-marker))
                (with-current-buffer (marker-buffer view-marker)
                  (write-region view-marker (point-max)
                                view-file nil 'silent))
                (set-file-modes view-file #o600))
              (niwa-telemetry-profiler-stop)
              (message "niwa: session debug artifacts saved under %s"
                       directory))
          (when niwa-telemetry--profiler-session
            (ignore-errors (niwa-telemetry-profiler-stop)))
          (advice-remove 'gptel--log #'niwa-telemetry--gptel-log-raw)
          (setq gptel-log-level
                niwa-telemetry--session-debug-previous-log-level
                niwa-telemetry--session-debug-previous-log-level nil
                niwa-view-render-debug
                niwa-telemetry--session-debug-previous-view-debug
                niwa-telemetry--session-debug-previous-view-debug nil)
          (set-marker marker nil)
          (when (markerp view-marker)
            (set-marker view-marker nil))
          (setq niwa-telemetry--session-debug-marker nil
                niwa-telemetry--session-debug-view-marker nil)))
    (require 'niwa-view-render)
    (let ((previous-level gptel-log-level)
          (previous-view-debug niwa-view-render-debug))
      (niwa-telemetry-profiler-start)
      (setq niwa-telemetry--session-debug-previous-log-level previous-level
            niwa-telemetry--session-debug-marker
            (with-current-buffer (get-buffer-create gptel--log-buffer-name)
              (copy-marker (point-max)))
            niwa-telemetry--session-debug-previous-view-debug
            previous-view-debug
            niwa-telemetry--session-debug-view-marker
            (with-current-buffer
                (get-buffer-create niwa-view-render-debug-buffer-name)
              (copy-marker (point-max)))
            gptel-log-level 'debug
            niwa-view-render-debug t)
      (advice-add 'gptel--log :around #'niwa-telemetry--gptel-log-raw)
      (message "niwa: session debug capture started"))))

(provide 'niwa-telemetry)

;;; niwa-telemetry.el ends here
