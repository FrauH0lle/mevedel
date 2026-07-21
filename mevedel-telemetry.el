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

;; `gptel'
(defvar gptel-version)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)

;; `mevedel-sandbox'
(declare-function mevedel-sandbox-probe "mevedel-sandbox" ())
(defvar mevedel-sandbox-mode)

;; `mevedel-structs'
(declare-function mevedel-goal-cycle "mevedel-structs" (cl-x))
(declare-function mevedel-goal-id "mevedel-structs" (cl-x))
(declare-function mevedel-goal-p "mevedel-structs" (cl-x))
(declare-function mevedel-goal-phase "mevedel-structs" (cl-x))
(declare-function mevedel-goal-status "mevedel-structs" (cl-x))
(declare-function mevedel-session-goal "mevedel-structs" (cl-x))
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x))
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-telemetry-pending
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x))
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(defvar mevedel--agent-invocation)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `profiler'
(declare-function profiler-cpu-profile "profiler" ())
(declare-function profiler-memory-profile "profiler" ())
(declare-function profiler-report-setup-buffer "profiler" (profile))
(declare-function profiler-start "profiler" (mode))
(declare-function profiler-stop "profiler" ())
(declare-function profiler-write-profile
                  "profiler" (profile filename &optional confirm))
(defvar profiler-cpu-log)
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


;;
;;; Event representation

(defconst mevedel-telemetry-schema-version 1
  "Schema version written into every telemetry event.")

(defconst mevedel-telemetry--payload-keys
  '(:args :command :content :detail :environment :expression :input
    :justification :output :prompt :response :stderr :stdout :tool-args
    :tool-result)
  "Keys that may contain raw user, model, process, or tool payloads.")

(defconst mevedel-telemetry--owned-keys
  '(:schema-version :time :elapsed-ms :sequence :event :session-id :turn
    :preset :goal-id :goal-cycle :goal-phase :goal-status :profiler-run-id)
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

(defun mevedel-telemetry-path (session)
  "Return SESSION's telemetry path, or nil before materialization."
  (let ((save-path (and session
                        (ignore-errors
                          (mevedel-session-save-path session)))))
    (when save-path
      (file-name-concat save-path mevedel-telemetry-file-name))))

(defun mevedel-telemetry-profiler-directory (session)
  "Return the active profiler diagnostics directory for SESSION, or nil."
  (when (and (eq session mevedel-telemetry--profiler-session)
             mevedel-telemetry--profiler-run-id
             (mevedel-session-save-path session))
    (file-name-concat (mevedel-session-save-path session)
                      "diagnostics" mevedel-telemetry--profiler-run-id)))

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
     ((consp value)
      (mapcar (lambda (item)
                (mevedel-telemetry--safe-value item (1+ depth)))
              (mevedel-telemetry--take-bounded value 32)))
     (t (intern (format ":%s" (type-of value)))))))

(defun mevedel-telemetry--safe-props (props)
  "Return telemetry-safe PROPS without owned or payload-bearing keys."
  (let (safe)
    (while props
      (let ((key (pop props))
            (value (pop props)))
        (when (and (keywordp key)
                   (not (memq key mevedel-telemetry--payload-keys))
                   (not (memq key mevedel-telemetry--owned-keys)))
          (setq safe
                (append safe
                        (list key (mevedel-telemetry--safe-value value)))))))
    safe))

(defun mevedel-telemetry--envelope (session event props)
  "Build the common telemetry envelope for SESSION, EVENT, and PROPS."
  (let* ((goal (ignore-errors (mevedel-session-goal session)))
         (preset (ignore-errors (mevedel-session-preset-name session))))
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
             :goal-cycle (mevedel-goal-cycle goal)
             :goal-phase (mevedel-goal-phase goal)
             :goal-status (mevedel-goal-status goal)))
     (mevedel-telemetry--safe-props props))))


;;
;;; Persistence

(defun mevedel-telemetry--persist (session entry)
  "Append telemetry ENTRY to SESSION's persistent stream."
  (when-let* ((file (and mevedel-telemetry-enabled
                         (mevedel-telemetry-path session))))
    (condition-case err
        (let ((print-length nil)
              (print-level nil)
              (print-quoted t)
              (print-escape-newlines t))
          (make-directory (file-name-directory file) t)
          (with-temp-buffer
            (prin1 entry (current-buffer))
            (insert "\n")
            (append-to-file (point-min) (point-max) file)))
      (error
       (message "mevedel: telemetry persistence failed: %s"
                (error-message-string err))))))

(defun mevedel-telemetry-record (session event &rest props)
  "Record telemetry EVENT and PROPS for SESSION.
The event is buffered until SESSION has a persistent directory.  Raw payload
keys are always discarded.  Return the sanitized event plist."
  (when (and mevedel-telemetry-enabled session)
    (condition-case err
        (let ((entry (mevedel-telemetry--envelope session event props)))
          (if (mevedel-session-save-path session)
              (mevedel-telemetry--persist session entry)
            (setf (mevedel-session-telemetry-pending session)
                  (append (mevedel-session-telemetry-pending session)
                          (list entry))))
          entry)
      (error
       (message "mevedel: telemetry event failed: %s"
                (error-message-string err))
       nil))))

(defun mevedel-telemetry-flush (session)
  "Persist and clear SESSION telemetry buffered before materialization."
  (when session
    (dolist (entry (mevedel-session-telemetry-pending session))
      (mevedel-telemetry--persist session entry))
    (setf (mevedel-session-telemetry-pending session) nil)))


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
          :started-at started-at :props (mevedel-telemetry--safe-props props))))

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
  (let* ((head (ignore-errors
                 (mevedel-telemetry--process-output
                  "git" "-C" directory "rev-parse" "HEAD")))
         (status (ignore-errors
                   (mevedel-telemetry--process-output
                    "git" "-C" directory "status" "--short")))
         (diff (ignore-errors
                 (mevedel-telemetry--process-output
                  "git" "-C" directory "diff" "--binary" "HEAD")))
         (untracked (ignore-errors
                      (mevedel-telemetry--process-output
                       "git" "-C" directory "ls-files" "--others"
                       "--exclude-standard" "-z")))
         (untracked-hashes
          (mapcar
           (lambda (relative)
             (let ((file (expand-file-name relative directory)))
               (when (and (file-regular-p file) (file-readable-p file))
                 (with-temp-buffer
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
           :sandbox-mode (and (boundp 'mevedel-sandbox-mode)
                              mevedel-sandbox-mode)
           :bubblewrap-available (and sandbox
                                       (plist-get sandbox :available))
           git)))

;;;###autoload
(defun mevedel-telemetry-profiler-start (&optional mode)
  "Start Emacs profiling for the current session in MODE.
MODE defaults to `cpu'.  Interactively with a prefix argument, prompt for
`cpu', `mem', or `cpu+mem'."
  (interactive
   (list (if current-prefix-arg
             (intern
              (completing-read "Profiler mode: "
                               '("cpu" "mem" "cpu+mem") nil t nil nil
                               "cpu"))
           'cpu)))
  (let ((session (mevedel-telemetry-current-session)))
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
    (profiler-start (or mode 'cpu))
    (setq mevedel-telemetry--profiler-session session)
    (mevedel-telemetry--install-prompt-guard)
    (mevedel-telemetry--record-environment session 'start)
    (mevedel-telemetry-record
     session 'profiler-started
     :mode (or mode 'cpu)
     :emacs-version emacs-version
     :system-configuration system-configuration)
    (message "mevedel: profiler started for session %s"
             (or (mevedel-session-session-id session) "pending"))))

(defun mevedel-telemetry--write-profiler-artifacts (directory)
  "Write compact native profiler profiles and reports below DIRECTORY."
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
                (profiler-write-profile profile profile-file nil)
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
    (let ((artifacts
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
         (failure-stage 'environment))
    (unwind-protect
        (condition-case err
            (progn
              (mevedel-telemetry--record-environment session 'stop)
              (setq failure-stage 'stop)
              (profiler-stop)
              (make-directory directory t)
              (setq failure-stage 'save-artifacts)
              (let ((artifacts
                     (mevedel-telemetry--write-profiler-artifacts directory)))
                (mevedel-telemetry-record
                 session 'profiler-stopped
                 :modes (mapcar (lambda (artifact)
                                  (plist-get artifact :mode))
                                artifacts)
                 :profile-relative-paths
                 (mapcar (lambda (artifact)
                           (file-relative-name
                            (plist-get artifact :profile-file)
                            (mevedel-session-save-path session)))
                         artifacts)
                 :report-relative-paths
                 (mapcar (lambda (artifact)
                           (file-relative-name
                            (plist-get artifact :report-file)
                            (mevedel-session-save-path session)))
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
      (mevedel-telemetry--remove-prompt-guard)
      (setq mevedel-telemetry--profiler-session nil
            mevedel-telemetry--profiler-run-id nil))))

(provide 'mevedel-telemetry)

;;; mevedel-telemetry.el ends here
