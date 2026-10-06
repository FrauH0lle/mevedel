;;; mevedel-memory-pass.el -- Workspace consolidation coordination -*- lexical-binding: t -*-

;;; Commentary:

;; Run one workspace-owned consolidation independently of a live conversation.
;; Target-native claims fence inference, accepted storage precedes publication,
;; and a successor recovers older results before selecting its own evidence.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-journal-index)
(require 'mevedel-journal-worker)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-review)
(require 'mevedel-memory-store)

;; `mevedel-memory-list'
(declare-function mevedel-memory-list--finished "mevedel-memory-list" (workspace result))
(autoload 'mevedel-memory-list--finished "mevedel-memory-list")

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record-workspace "mevedel-telemetry" (workspace event &rest props))
(autoload 'mevedel-telemetry-record-workspace "mevedel-telemetry")

(defcustom mevedel-memory-consolidation-mode 'propose
  "How memory consolidation is scheduled and proposals are handled.
Manual runs only on request.  Propose also runs automatically, at digest
publication, completed root turns, session opening and idle maintenance, and
leaves its proposals for approval.  Auto uses the same gate and applies fresh
memory and instruction proposals, including `AGENTS.md' changes, through the
checked decision path without prior review."
  :type '(choice (const manual) (const propose) (const auto))
  :group 'mevedel)

(defcustom mevedel-memory-consolidation-min-hours 24
  "Minimum hours between successful general memory reviews.
Focused reviews do not move this clock.  Explicit commands bypass the gate."
  :type '(number :tag "Hours") :group 'mevedel)

(defcustom mevedel-memory-consolidation-min-digests 5
  "Minimum eligible unreviewed digests needed for automatic memory review.
Completed digests from running sessions are eligible. One day before ordinary
journal expiry, a smaller backlog may be reviewed. Explicit commands bypass
this threshold."
  :type '(integer :tag "Digests") :group 'mevedel)

(defvar mevedel-memory-pass--inhibit-scheduling nil
  "Non-nil suppresses automatic opportunities during exit or isolated tests.")

(defvar mevedel-memory-pass--pending (make-hash-table :test #'eq)
  "Coalesced turn-completion timers, keyed by their workspace objects.")

(defvar mevedel-memory-pass--running (make-hash-table :test #'equal)
  "This client's active requests, keyed by physical pass ownership directory.")

(defun mevedel-memory-pass--last-general (workspace entries)
  "Return the latest successful general-review time from WORKSPACE ENTRIES.
Include retired completion metadata so expiry cannot erase the clock.  A
malformed retirement marker stops automatic admission for inspection."
  (let (latest)
    (dolist (entry entries)
      (when (and (eq (plist-get entry :kind) 'consolidation)
                 (string-empty-p (plist-get entry :focus)))
        (setq latest (max (or latest 0) (float-time (date-to-time (plist-get entry :created)))))))
    (dolist (path (mevedel-session-control-fs-list-directory
                   (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "retired-passes")
                   mevedel-journal-store-id-regexp))
      (let* ((text (mevedel-session-control-fs-read-file path 'utf-8-unix 513))
             (record (json-parse-string text))
             (scope (and (hash-table-p record) (gethash "scope" record)))
             (date (and (hash-table-p record) (gethash "created" record)))
             (hash (and (hash-table-p record) (gethash "sha256" record))))
        (unless (and (<= (string-bytes text) 512) (hash-table-p record)
                     (= 3 (hash-table-count record)) (member scope '("general" "focused"))
                     (stringp hash) (mevedel-journal-store-id-p hash)
                     (stringp date) (string-match-p (concat "\\`" mevedel-journal-store--time-regexp "\\'") date))
          (error "Invalid retired memory review clock"))
        (when (equal scope "general")
          (setq latest (max (or latest 0) (float-time (date-to-time date)))))))
    latest))

(defun mevedel-memory-pass--automatic-selection (workspace entries)
  "Return an eligible automatic batch from fresh WORKSPACE ENTRIES, or nil.
The caller owns consolidation admission and has recovered prior publications.
Check elapsed target time before selecting completed published evidence.
Cache only the next client-side opportunity, never coverage or write authority."
  (unless (and (numberp mevedel-memory-consolidation-min-hours)
               (>= mevedel-memory-consolidation-min-hours 0)
               (integerp mevedel-memory-consolidation-min-digests)
               (> mevedel-memory-consolidation-min-digests 0))
    (error "Invalid memory consolidation thresholds"))
  (let* ((last (mevedel-memory-pass--last-general workspace entries))
         (now (mevedel-session-control-fs-target-time (mevedel-workspace-root workspace)))
         (remaining (if last (- (+ last (* 3600 mevedel-memory-consolidation-min-hours)) now) 0)))
    (setf (mevedel-workspace-memory-schedule workspace)
          (list :after (+ (float-time) (if (> remaining 0) remaining 600))
                :hours mevedel-memory-consolidation-min-hours
                :digests mevedel-memory-consolidation-min-digests
                :age mevedel-journal-max-age-days))
    (when (<= remaining 0)
      (let ((selection (mevedel-memory-pass-select entries)))
        (when (or (>= (plist-get selection :eligible) mevedel-memory-consolidation-min-digests)
                  (and (plist-get selection :entries)
                       (integerp mevedel-journal-max-age-days)
                       (>= mevedel-journal-max-age-days 0)
                       (>= (- now (float-time (date-to-time
                                               (plist-get (car (plist-get selection :entries)) :created))))
                           (* (max 0 (1- mevedel-journal-max-age-days)) 86400))))
          selection)))))

;;;###autoload
(defun mevedel-memory-pass-schedule (workspace)
  "Offer automatic review at an activity boundary in WORKSPACE.
Manual mode, cached thresholds and pending opportunities need no target I/O.
Defer the cold observation until transport is idle; a pass rechecks admission
under target ownership. No completion recursively schedules another pass."
  (unless (or mevedel-memory-pass--inhibit-scheduling
              (eq mevedel-memory-consolidation-mode 'manual))
    (let ((cached (mevedel-workspace-memory-schedule workspace)) opportunity)
      (unless (or (gethash workspace mevedel-memory-pass--pending)
                  (and (equal (plist-get cached :hours) mevedel-memory-consolidation-min-hours)
                       (equal (plist-get cached :digests) mevedel-memory-consolidation-min-digests)
                       (equal (plist-get cached :age) mevedel-journal-max-age-days)
                       (< (float-time) (or (plist-get cached :after) 0))))
        (setf (mevedel-workspace-memory-schedule workspace)
              (setq opportunity
                    (list :after (+ (float-time) 600)
                          :hours mevedel-memory-consolidation-min-hours
                          :digests mevedel-memory-consolidation-min-digests
                          :age mevedel-journal-max-age-days)))
        (mevedel-transport-schedule-idle
         mevedel-memory-pass--pending workspace 'memory-pass (mevedel-workspace-root workspace)
         (lambda ()
           (unless (or mevedel-memory-pass--inhibit-scheduling
                       (eq mevedel-memory-consolidation-mode 'manual))
             (condition-case err
                 (when (mevedel-session-control-fs-path-exists-p
                        (mevedel-journal-store-directory (mevedel-workspace-root workspace)))
                   (unless (mevedel-memory-pass-start
                            workspace (lambda (result) (mevedel-memory-list--finished workspace result))
                            :automatic t)
                     ;; A busy owner is not an admission observation. Preserve
                     ;; any fresh gate result, but allow the next activity offer.
                     (when (eq opportunity (mevedel-workspace-memory-schedule workspace))
                       (setf (mevedel-workspace-memory-schedule workspace) nil))))
               (error (display-warning 'mevedel
                                       (format "Automatic memory review failed: %s" (error-message-string err))
                                       :warning))))))))))

(defun mevedel-memory-pass-stop-all ()
  "Cancel this client's queued and running memory reviews on exit."
  (mevedel-transport-cancel-idle mevedel-memory-pass--pending 'memory-pass)
  (maphash (lambda (_key state) (mevedel-memory-pass-cancel (plist-get state :workspace)))
           mevedel-memory-pass--running))

(defun mevedel-memory-pass-select (entries &optional focused)
  "Select at most twenty completed digests from validated public ENTRIES.
FOCUSED also admits previously reviewed evidence. Published digests are
immutable completed work, including from sessions that are still running.

Return :entries oldest first, :eligible count and :remaining eligible backlog.
Break equal timestamps by digest ID. This observation acquires no ownership,
pins no evidence and consumes no coverage; the caller coordinates publication."
  (let ((eligible (if focused
                      (seq-filter (lambda (entry) (eq (plist-get entry :kind) 'digest)) entries)
                    (mevedel-journal-index-unreviewed entries))))
    (setq eligible
          (sort eligible (lambda (left right)
                           (let ((first (plist-get left :created)) (second (plist-get right :created)))
                             (if (equal first second)
                                 (string-lessp (plist-get left :id) (plist-get right :id))
                               (string-lessp first second))))))
    (list :entries (seq-take eligible 20) :eligible (length eligible)
          :remaining (max 0 (- (length eligible) 20)))))

(defun mevedel-memory-pass-running (workspace)
  "Return this client's running consolidation state for WORKSPACE, or nil.
Its :request contains an inspectable :buffer. Other clients' ownership is
represented by the target claim and does not grant local cancellation rights."
  (gethash (mevedel-memory-store--claim-directory workspace) mevedel-memory-pass--running))

(defun mevedel-memory-pass--apply (state accepted)
  "Apply ACCEPTED memory proposals sequentially for completed pass STATE.
Each application reacquires workspace and original-target ownership, for
instruction proposals as for memory ones.  Stop before further proposals if
cancellation was requested, holding the rest.
Count distinct files only through this call's confirmed-write notification."
  (let ((workspace (plist-get state :workspace))
        (pass (plist-get (plist-get accepted :prepared) :id))
        (applied 0) (held 0) (unapplied 0) files)
    (dolist (proposal (plist-get accepted :proposals))
      (if (plist-get state :stop-apply)
          (cl-incf held)
        (condition-case nil
            (let ((decision
                   (mevedel-memory-decision-apply
                    workspace pass (plist-get proposal :id)
                    (lambda (paths) (cl-incf applied) (setq files (append paths files))))))
              (unless (mevedel-memory-decision-terminal-status-p (plist-get decision :status))
                (cl-incf unapplied)))
          (quit (plist-put state :stop-apply t) (cl-incf unapplied))
          (error (cl-incf unapplied)))))
    (list :updated-files (length (delete-dups files)) :applied-count applied
          :held-count held :unapplied-count unapplied)))

(cl-defun mevedel-memory-pass--finish (state result)
          "Settle STATE once from RESULT and publish only its accepted immutable reply."
          (unless (plist-get state :settled)
            (when (and (eq (plist-get result :outcome) 'success)
                       (not (plist-get state :publication-finished))
                       (mevedel-journal-worker-supported-p (plist-get state :workspace))
                       (not (cl-some (lambda (root) (file-remote-p (plist-get (cdr root) :dir)))
                                     (plist-get (plist-get (plist-get state :prepared) :scope) :roots))))
              (unless (plist-get state :publishing)
                (plist-put state :publishing t)
                (condition-case err
                    (plist-put state :worker
                               (mevedel-journal-worker-start
                                (plist-get state :workspace) 'memory-publish
                                (lambda (reply)
                                  (unless (plist-get state :settled)
                                    (plist-put state :worker nil)
                                    (plist-put state :publication-finished t)
                                    (plist-put state :accepted (plist-get reply :accepted))
                                    (plist-put state :entry (plist-get reply :entry))
                                    (mevedel-memory-pass--finish
                                     state (if (plist-get reply :entry) result
                                             (append (list :outcome 'error :error
                                                           (or (plist-get reply :error) "Memory publication returned no entry")) result)))))
                                nil (list :prepared (plist-get state :prepared)
                                          :reply (plist-get result :reply) :entries (plist-get result :entries)
                                          :model (mevedel-model--provider-label (plist-get result :policy))
                                          :references (plist-get (plist-get result :references) :references))))
                  (error
                   (plist-put state :publication-finished t)
                   (mevedel-memory-pass--finish
                    state (append (list :outcome 'error :error (error-message-string err)) result)))))
              (cl-return-from mevedel-memory-pass--finish nil))
            (plist-put state :settled t)
            (let* ((workspace (plist-get state :workspace))
                   (claim (plist-get state :claim))
                   (id (plist-get claim :owner))
                   (status (if (eq (plist-get result :outcome) 'aborted) 'cancelled 'failed))
                   (reported (if-let* ((read-usage (plist-get (plist-get state :request) :usage)))
                                 (funcall read-usage) result))
                   ;; Freeze diagnostics before storage can replace RESULT with an error.
                   ;; Cancellation has no review callback result, so use its live snapshot.
                   (usage (cl-loop for key in '(:input-tokens :cached-tokens :output-tokens
                                                              :output-bytes :output-estimated-tokens :result-bytes
                                                              :reasoning-bytes :reply-bytes :tool-call-bytes
                                                              :tool-call-count :rounds
                                                              :budget-kind :output-limit)
                                   append (list key (if (plist-member result key)
                                                        (plist-get result key)
                                                      (or (plist-get reported key)
                                                          (unless (memq key '(:budget-kind :output-limit)) 0))))))
                   (model (when (plist-get result :policy) (mevedel-model--provider-label (plist-get result :policy))))
                   (failure-class (cond ((plist-get usage :budget-kind) 'output-limit)
                                        ((plist-get state :publishing) 'publication)
                                        (t 'review)))
                   (accepted (plist-get state :accepted))
                   (entry (plist-get state :entry))
                   recovery-error applied)
              (when (timerp (plist-get state :timer)) (cancel-timer (plist-get state :timer)))
              (plist-put state :timer nil)
              (when-let* ((worker (plist-get state :worker)))
                (plist-put state :worker nil)
                (when (process-live-p worker) (delete-process worker)))
              (condition-case err
                  (when (eq (plist-get result :outcome) 'success)
                    (setq failure-class 'publication)
                    (unless (plist-get state :publication-finished)
                      (setq accepted
                            (mevedel-memory-store-accept
                             workspace (plist-get state :prepared) (plist-get result :reply)
                             (plist-get result :entries) (mevedel-model--provider-label (plist-get result :policy))
                             (plist-get (plist-get result :references) :references)))
                      (unless accepted (error "Memory pass lost ownership before accepting its result"))
                      (setq entry (mevedel-memory-store-publish workspace id)))
                    (when (string-empty-p (plist-get entry :focus))
                      (setf (mevedel-workspace-memory-schedule workspace) nil)))
                (error (setq result (list :outcome 'error :error (error-message-string err)))))
              (unless entry
                (condition-case err
                    (progn
                      (mevedel-journal-claim-settle claim status "")
                      (when (eq 'completed (plist-get (mevedel-journal-claim-outcome claim) :status))
                        (setq recovery-error "Accepted memory result requires publication verification"))
                      ;; A timeout cannot settle as the now-expired owner. Compete for
                      ;; the same immutable expiry outcome used by target-side takeover.
                      (when (and (not (mevedel-journal-claim-outcome claim))
                                 (>= (mevedel-session-control-fs-target-time (plist-get claim :directory))
                                     (plist-get claim :expires-at)))
                        (mevedel-journal-claim--finish claim 'expired ""))
                      (when (and (plist-get state :prepared)
                                 (memq (plist-get (mevedel-journal-claim-outcome claim) :status)
                                       '(failed cancelled expired)))
                        (mevedel-memory-store-release workspace id)))
                  (error (setq recovery-error (error-message-string err)))))
              (when (plist-get state :publishing)
                (setf (mevedel-workspace-journal-observation workspace) nil))
              (when entry
                (when (plist-get state :publication-finished)
                  (mevedel-journal-cleanup-schedule workspace t))
                (when (eq 'auto (plist-get (plist-get state :telemetry) :mode))
                  (setq applied (mevedel-memory-pass--apply state accepted)))
                (setf (mevedel-workspace-memory-observation workspace) nil))
              (plist-put state :result
                         (append (list :outcome (if entry 'success (plist-get result :outcome))
                                       :id id :entry entry :error (plist-get result :error)
                                       :mode (plist-get (plist-get state :telemetry) :mode)
                                       :recovery-required (or recovery-error (and accepted (not entry)))
                                       :remaining (if entry
                                                      (+ (or (plist-get (plist-get state :selection) :remaining) 0)
                                                         (or (plist-get result :omitted-digests) 0))
                                                    (or (plist-get (plist-get state :selection) :eligible) 0)))
                                 applied usage))
              (when (eq state (mevedel-memory-pass-running workspace))
                (remhash (plist-get claim :directory) mevedel-memory-pass--running))
              (apply #'mevedel-telemetry-record-workspace workspace
                     (cond (entry 'memory-consolidation-completed)
                           ((eq status 'cancelled) 'memory-consolidation-killed)
                           (t 'memory-consolidation-failed))
                     (append (plist-get state :telemetry) usage
                             (list :model model :outcome (plist-get (plist-get state :result) :outcome)
                                   :duration-ms (round (* 1000 (max 0 (- (float-time) (or (plist-get state :started-at) (float-time))))))
                                   :failure-class (and (not entry) (not (eq status 'cancelled)) failure-class)
                                   :reviewed-count (length (plist-get entry :digests))
                                   :covered-count (if (eq 'general (plist-get (plist-get state :telemetry) :scope))
                                                      (length (plist-get entry :digests)) 0)
                                   :proposed-count (length (plist-get entry :proposals))
                                   :updated-file-count (plist-get applied :updated-files)
                                   :remaining-count (plist-get (plist-get state :result) :remaining))))
              (when-let* ((callback (plist-get state :callback)))
                (funcall callback (plist-get state :result))))))

(defun mevedel-memory-pass-cancel (workspace)
  "Cancel only this client's running consolidation for WORKSPACE.
Fence its outcome before aborting transport, making late callbacks harmless."
  (when-let* ((state (mevedel-memory-pass-running workspace)))
    (plist-put state :stop-apply t)
    (unwind-protect
        (mevedel-memory-pass--finish state '(:outcome aborted))
      (when-let* ((cancel (plist-get (plist-get state :request) :cancel))) (funcall cancel)))
    t))

(defun mevedel-memory-pass--publish (workspace payload)
  "Accept and publish a validated review in WORKSPACE from frozen PAYLOAD.
The accepted bundle survives a publication failure for checked recovery."
  (let (accepted)
    (condition-case err
        (progn
          (setq accepted
                (mevedel-memory-store-accept
                 workspace (plist-get payload :prepared) (plist-get payload :reply)
                 (plist-get payload :entries) (plist-get payload :model)
                 (plist-get payload :references)))
          (unless accepted (error "Memory pass lost ownership before accepting its result"))
          (list :ok t :accepted accepted
                :entry (mevedel-memory-store-publish
                        workspace (plist-get (plist-get payload :prepared) :id))))
      (error (list :error (error-message-string err) :accepted accepted)))))

(defun mevedel-memory-pass--prepare (workspace payload)
  "Recover, select and prepare consolidation inputs from frozen PAYLOAD.
This may run in a storage child; no provider configuration is needed."
  (unless (equal (plist-get payload :client) (mevedel-workspace-identity-client))
    (error "Memory preparation belongs to another client"))
  (let* ((claim (plist-get payload :claim))
         (automatic (plist-get payload :automatic))
         (focus (plist-get payload :focus))
         (mevedel-memory-consolidation-min-hours (plist-get payload :min-hours))
         (mevedel-memory-consolidation-min-digests (plist-get payload :min-digests)))
    (mevedel-memory-store--assert-owned claim)
    (mevedel-memory-decision-recover workspace)
    (mevedel-memory-store-recover workspace)
    (let* ((selection
            (mevedel-memory-store--mutation
             workspace
             (lambda (_mutation)
               (mevedel-journal-cleanup-recover
                (mevedel-journal-store-directory (mevedel-workspace-root workspace)))
               (let ((entries (mevedel-journal-store-entries (mevedel-workspace-root workspace))))
                 (if automatic (mevedel-memory-pass--automatic-selection workspace entries)
                   (mevedel-memory-pass-select entries (not (string-empty-p focus))))))))
           (entries (plist-get selection :entries)))
      (if (and automatic (not selection))
          (list :skipped t :schedule (mevedel-workspace-memory-schedule workspace))
        (unless (or entries (plist-get payload :memory-only))
          (error "No eligible digests for consolidation"))
        (mevedel-memory-store--assert-owned claim)
        (let* ((scope (mevedel-memory-scope-capture
                       workspace nil (plist-get payload :configuration)))
               (prepared (mevedel-memory-store-prepare workspace claim scope entries focus)))
          (list :selection selection :schedule (mevedel-workspace-memory-schedule workspace)
                :record prepared :rejections (mevedel-memory-decision-rejections workspace)))))))

(defun mevedel-memory-pass--prepared (state result focus memory-only)
  "Start STATE's configured review from prepared RESULT, FOCUS and MEMORY-ONLY.
Cancelled or expired storage replies never start a model request."
  (unless (plist-get state :settled)
    (plist-put state :worker nil)
    (condition-case err
        (let* ((workspace (plist-get state :workspace))
               (claim (plist-get state :claim))
               (bundle (plist-get result :prepared))
               (prepared (plist-get bundle :record)))
          (when (plist-get result :error) (error "%s" (plist-get result :error)))
          (mevedel-memory-store--assert-owned claim)
          (setf (mevedel-workspace-memory-schedule workspace) (plist-get bundle :schedule)
                (mevedel-workspace-memory-observation workspace) nil
                (mevedel-workspace-journal-observation workspace) nil)
          (if (and (plist-get state :automatic) (plist-get bundle :skipped))
              (progn
                (mevedel-journal-claim-settle claim 'cancelled "")
                (when (timerp (plist-get state :timer)) (cancel-timer (plist-get state :timer)))
                (plist-put state :timer nil)
                (plist-put state :settled t)
                (plist-put state :skipped t)
                (remhash (plist-get claim :directory) mevedel-memory-pass--running))
            (unless (and prepared (equal claim (plist-get prepared :claim)))
              (error "Memory preparation returned another claim"))
            (plist-put state :selection (plist-get bundle :selection))
            (plist-put state :prepared prepared)
            (when (plist-get state :automatic)
              (apply #'mevedel-telemetry-record-workspace workspace 'memory-consolidation-fired
                     (plist-get state :telemetry)))
            (unless (buffer-live-p (plist-get state :origin-buffer))
              (error "Memory review caller buffer was closed"))
            (plist-put state :request
                       (with-current-buffer (plist-get state :origin-buffer)
                         (mevedel-memory-review-request
                          (plist-get prepared :scope) (plist-get prepared :entries)
                          (lambda (reply) (mevedel-memory-pass--finish state reply))
                          :focus focus :memory-only memory-only
                          :rejections (plist-get bundle :rejections)
                          :currentp (lambda ()
                                      (and (not (plist-get state :settled))
                                           (condition-case nil
                                               (progn (mevedel-memory-store--assert-owned claim) t)
                                             (error nil)))))))))
      (error (mevedel-memory-pass--finish
              state (list :outcome 'error :error (error-message-string err)))))))

(cl-defun mevedel-memory-pass-start (workspace callback &key (focus "") memory-only automatic)
  "Start one sessionless consolidation in WORKSPACE and return its state.
CALLBACK receives its settled outcome and diagnostics. Automatic admission can
settle the returned state as :skipped without a callback or model request.
Invalid focus signals before starting. Later errors settle through CALLBACK.
FOCUS selects a bounded focused review; MEMORY-ONLY allows an empty batch.
AUTOMATIC uses the general-review time/count gates after fenced recovery."
  (unless (and (stringp focus) (<= (string-bytes focus) 4096))
    (error "Memory focus exceeds its byte limit"))
  (when (and automatic (or mevedel-memory-pass--inhibit-scheduling
                           (eq mevedel-memory-consolidation-mode 'manual)))
    (cl-return-from mevedel-memory-pass-start nil))
  (when (and automatic (or memory-only (not (string-empty-p focus))))
    (error "Automatic review must use general digest admission"))
  (when (mevedel-memory-pass-running workspace)
    (if automatic (cl-return-from mevedel-memory-pass-start nil)
      (error "Memory consolidation is already running")))
  (mevedel-workspace-identity-ensure (mevedel-workspace-root workspace))
  (let* ((key (mevedel-memory-store--claim-directory workspace))
         (claim (or (mevedel-journal-claim-acquire key 180)
                    (if automatic (cl-return-from mevedel-memory-pass-start nil)
                      (error "Memory consolidation is busy"))))
         (state (list :workspace workspace :claim claim :callback callback :settled nil
                      :automatic automatic :origin-buffer (current-buffer)
                      :started-at (float-time)
                      :telemetry (list :pass-id (plist-get claim :owner)
                                       :attempt-generation (plist-get claim :generation)
                                       :mode mevedel-memory-consolidation-mode :workload 'memory
                                       :scope (if (string-empty-p focus) 'general 'focused)))))
    (puthash key state mevedel-memory-pass--running)
    (unless automatic
      (apply #'mevedel-telemetry-record-workspace workspace 'memory-consolidation-fired
             (plist-get state :telemetry)))
    (condition-case err
        (progn
          (plist-put state :timer
                     (run-at-time
                      (max 0 (- (plist-get claim :expires-at)
                                (mevedel-session-control-fs-target-time key))) nil
                      (lambda ()
                        (unwind-protect
                            (mevedel-memory-pass--finish state '(:outcome error :error "Memory pass timed out"))
                          (when-let* ((cancel (plist-get (plist-get state :request) :cancel)))
                            (funcall cancel))))))
          (let* ((roots (mevedel-system--memory-roots workspace))
                 (payload (list :claim claim :focus focus :automatic automatic :memory-only memory-only
                                :min-hours mevedel-memory-consolidation-min-hours
                                :min-digests mevedel-memory-consolidation-min-digests
                                :client (mevedel-workspace-identity-client)
                                :configuration (list :roots roots :instructions
                                                     (mevedel-system-workspace-config-files workspace))))
                 (ready (lambda (result) (mevedel-memory-pass--prepared state result focus memory-only))))
            (if (and (mevedel-journal-worker-supported-p workspace)
                     (not (cl-some (lambda (root) (file-remote-p (plist-get root :dir))) roots)))
                (plist-put state :worker
                           (mevedel-journal-worker-start workspace 'memory-prepare ready nil payload))
              (funcall ready (list :ok t :prepared (mevedel-memory-pass--prepare workspace payload))))))
      (error (mevedel-memory-pass--finish state (list :outcome 'error :error (error-message-string err)))))
    (unless (plist-get state :skipped) state)))

(provide 'mevedel-memory-pass)
;;; mevedel-memory-pass.el ends here
