;;; mevedel-memory-pass.el -- Workspace consolidation coordination -*- lexical-binding: t -*-

;;; Commentary:

;; Run one workspace-owned consolidation independently of a live conversation.
;; Target-native claims fence inference, accepted storage precedes publication,
;; and a successor recovers older results before selecting its own evidence.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-journal-index)
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
Manual runs only on request. Propose also runs at eligible workspace activation,
digest publication and completed root turns. Auto uses the same gate and applies
fresh memory proposals
with checked writes.
Instruction proposals always wait for explicit approval."
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
    (let ((cached (mevedel-workspace-memory-schedule workspace)))
      (unless (or (gethash workspace mevedel-memory-pass--pending)
                  (and (equal (plist-get cached :hours) mevedel-memory-consolidation-min-hours)
                       (equal (plist-get cached :digests) mevedel-memory-consolidation-min-digests)
                       (equal (plist-get cached :age) mevedel-journal-max-age-days)
                       (< (float-time) (or (plist-get cached :after) 0))))
        (setf (mevedel-workspace-memory-schedule workspace)
              (list :after (+ (float-time) 600)
                    :hours mevedel-memory-consolidation-min-hours
                    :digests mevedel-memory-consolidation-min-digests
                    :age mevedel-journal-max-age-days))
        (mevedel-transport-schedule-idle
         mevedel-memory-pass--pending workspace 'memory-pass (mevedel-workspace-root workspace)
         (lambda ()
           (unless (or mevedel-memory-pass--inhibit-scheduling
                       (eq mevedel-memory-consolidation-mode 'manual))
             (condition-case err
                 (when (mevedel-session-control-fs-path-exists-p
                        (mevedel-journal-store-directory (mevedel-workspace-root workspace)))
                   (mevedel-memory-pass-start
                    workspace (lambda (result) (mevedel-memory-list--finished workspace result))
                    :automatic t))
               (error (display-warning 'mevedel
                                       (format "Automatic memory review failed: %s" (error-message-string err))
                                       :warning))))))))))

(defun mevedel-memory-pass-stop-all ()
  "Cancel this client's queued and running memory reviews on exit."
  (mevedel-transport-cancel-idle mevedel-memory-pass--pending 'memory-pass #'mevedel-workspace-root)
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
Each application reacquires workspace and original-target ownership. Hold
instructions and stop before further proposals if cancellation was requested.
Count distinct files only through this call's confirmed-write notification."
  (let ((workspace (plist-get state :workspace))
        (pass (plist-get (plist-get accepted :prepared) :id))
        (applied 0) (held 0) (unapplied 0) files)
    (dolist (proposal (plist-get accepted :proposals))
      (if (or (plist-get state :stop-apply) (eq (plist-get proposal :action) 'instructions))
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

(defun mevedel-memory-pass--finish (state result)
  "Settle STATE once from RESULT and publish only its accepted immutable reply."
  (unless (plist-get state :settled)
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
           (failure-class (if (plist-get usage :budget-kind) 'output-limit 'review))
           accepted entry recovery-error applied)
      (when (timerp (plist-get state :timer)) (cancel-timer (plist-get state :timer)))
      (plist-put state :timer nil)
      (condition-case err
          (when (eq (plist-get result :outcome) 'success)
            (setq failure-class 'publication)
            (setq accepted
                  (mevedel-memory-store-accept
                   workspace (plist-get state :prepared) (plist-get result :reply)
                   (plist-get result :entries) (mevedel-model--provider-label (plist-get result :policy))
                   (plist-get (plist-get result :references) :references)))
            (unless accepted (error "Memory pass lost ownership before accepting its result"))
            (setq entry (mevedel-memory-store-publish workspace id))
            (when (string-empty-p (plist-get entry :focus))
              (setf (mevedel-workspace-memory-schedule workspace) nil)))
        (error (setq result (list :outcome 'error :error (error-message-string err)))))
      (unless entry
        (condition-case err
            (progn
              (mevedel-journal-claim-settle claim status "")
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
      (when entry
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

(cl-defun mevedel-memory-pass-start (workspace callback &key (focus "") memory-only automatic)
          "Start one sessionless consolidation in WORKSPACE and return its state.
CALLBACK receives one result with :outcome, published :entry, :remaining frozen
backlog count, review usage and output-budget diagnostics, and optional
:error/:recovery-required. Diagnostics survive cancellation and storage failure.
FOCUS never consumes general coverage; MEMORY-ONLY permits an explicitly
requested empty batch.
AUTOMATIC checks time and count after ownership and publication recovery; it
returns nil silently when busy or not due. Explicit calls bypass those gates.
Invalid focus signals before starting. Later errors settle through CALLBACK.
No request recursively drains a remaining backlog."
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
                    (when (and automatic (not selection))
                      (mevedel-journal-claim-settle claim 'cancelled "")
                      (remhash key mevedel-memory-pass--running)
                      (cl-return-from mevedel-memory-pass-start nil))
                    (when automatic
                      (apply #'mevedel-telemetry-record-workspace workspace 'memory-consolidation-fired
                             (plist-get state :telemetry)))
                    (plist-put state :selection selection)
                    (unless (or entries memory-only) (error "No eligible digests for consolidation"))
                    (let ((scope (mevedel-memory-scope-capture workspace)))
                      (plist-put state :prepared (mevedel-memory-store-prepare workspace claim scope entries focus))
                      (plist-put state :timer
                                 (run-at-time
                                  (max 0 (- (plist-get claim :expires-at)
                                            (mevedel-session-control-fs-target-time (plist-get claim :directory)))) nil
                                  (lambda ()
                                    (unwind-protect
                                        (mevedel-memory-pass--finish state '(:outcome error :error "Memory pass timed out"))
                                      (when-let* ((cancel (plist-get (plist-get state :request) :cancel))) (funcall cancel))))))
                      (plist-put state :request
                                 (mevedel-memory-review-request
                                  scope entries (lambda (result) (mevedel-memory-pass--finish state result))
                                  :focus focus :memory-only memory-only
                                  :rejections (mevedel-memory-decision-rejections workspace)
                                  :currentp (lambda ()
                                              (and (not (plist-get state :settled))
                                                   (condition-case nil
                                                       (progn (mevedel-memory-store--assert-owned claim) t)
                                                     (error nil)))))))))
              (error (mevedel-memory-pass--finish state (list :outcome 'error :error (error-message-string err)))))
            state))

(provide 'mevedel-memory-pass)
;;; mevedel-memory-pass.el ends here
