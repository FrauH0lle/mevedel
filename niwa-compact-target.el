;;; niwa-compact-target.el -- Compaction target transactions -*- lexical-binding: t -*-

;;; Commentary:

;; Owns root and agent transcript targets, archive publication, and application.
;; Evidence selection and asynchronous request settlement remain separate.

;;; Code:

;; `setf' on a slot of a struct defined elsewhere needs that
;; `cl-defstruct' at compile time: `declare-function' supplies the
;; accessor but not the setter, and without the expander the form
;; compiles to a call to a function that does not exist.
(eval-when-compile (require 'niwa-structs))

(require 'cl-lib)
(require 'subr-x)

;; `gptel'
(declare-function gptel--update-status "ext:gptel" (msg &optional face))

;; `niwa-agent-control'
(declare-function niwa-agent-record-parent-path "niwa-agent-control" (cl-x) t)
(defvar niwa-agent-control--wake-prompt)

;; `niwa-agent-conversation'
(declare-function niwa-agent-conversation-record-activity
                  "niwa-agent-conversation"
                  (invocation item &optional suppress-rerender))
(declare-function niwa-agent-conversation-save
                  "niwa-agent-conversation" (invocation &optional deferred))

;; `niwa-agent-persistence'
(declare-function niwa-agent-persistence-transcript-path-p
                  "niwa-agent-persistence" (path save-path))
(autoload 'niwa-agent-persistence-transcript-path-p
  "niwa-agent-persistence")

;; `niwa-agents'
(declare-function niwa-agent-invocation-buffer "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-parent-session
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-require-path
                  "niwa-agents" (invocation))
(declare-function niwa-agent-invocation-transcript-relative-path
                  "niwa-agents" (cl-x) t)

;; `niwa-chat'
(declare-function niwa--run-session-start-hooks "niwa-chat" (source))

;; `niwa-compact'
(declare-function niwa--compact-agent-terminal-failure
                  "niwa-compact" (target fsm err))
(declare-function niwa--compact-main-failure
                  "niwa-compact" (target fsm err))
(declare-function niwa--compact-main-resume-status
                  "niwa-compact" (target))
(declare-function niwa--compact-target-resume
                  "niwa-compact" (target fsm))

;; `niwa-compact-evidence'
(declare-function niwa-compact-evidence-agent-summary-bounds
                  "niwa-compact-evidence" (&optional invocation))
(declare-function niwa-compact-evidence-agent-task-heading
                  "niwa-compact-evidence" (&optional invocation))
(declare-function niwa-compact-evidence-body-start
                  "niwa-compact-evidence" ())
(declare-function niwa-compact-evidence-previous-summary
                  "niwa-compact-evidence" ())
(declare-function niwa-compact-evidence-turn-starts-before
                  "niwa-compact-evidence" (limit &optional body-start))
(autoload 'niwa-compact-evidence-agent-summary-bounds
  "niwa-compact-evidence")
(autoload 'niwa-compact-evidence-agent-task-heading
  "niwa-compact-evidence")
(autoload 'niwa-compact-evidence-body-start "niwa-compact-evidence")
(autoload 'niwa-compact-evidence-previous-summary
  "niwa-compact-evidence")
(autoload 'niwa-compact-evidence-turn-starts-before
  "niwa-compact-evidence")

;; `niwa-execution-transcript'
(declare-function niwa-execution-transcript-archive-text
                  "niwa-execution-transcript" (plan))
(declare-function niwa-execution-transcript-commit-archive
                  "niwa-execution-transcript" (data-buffer plan))
(autoload 'niwa-execution-transcript-archive-text
  "niwa-execution-transcript")
(autoload 'niwa-execution-transcript-commit-archive
  "niwa-execution-transcript")

;; `niwa-hooks'
(declare-function niwa-hooks-context-audit-records
                  "niwa-hooks" (decision event type &optional omit-context))
(declare-function niwa-hooks-take-session-context
                  "niwa-hooks" (session))

;; `niwa-reminders'
(declare-function niwa-reminders-rearm-plan-reference
                  "niwa-reminders" (session))
(autoload 'niwa-reminders-rearm-plan-reference "niwa-reminders")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-artifact-present-p
                  "niwa-session-artifacts"
                  (session logical &optional committed-only))
(declare-function niwa-session-artifacts-content-start
                  "niwa-session-artifacts" (buffer))
(declare-function niwa-session-artifacts-publish-text
                  "niwa-session-artifacts"
                  (session path content &optional coding))
(declare-function niwa-session-artifacts-rotate-segment
                  "niwa-session-artifacts"
                  (session buffer summary &rest keys))
(declare-function niwa-session-artifacts-segment-path
                  "niwa-session-artifacts" (save-path n))
(declare-function niwa-session-artifacts-strip-summary-handoff-prefix
                  "niwa-session-artifacts" (summary))
(declare-function niwa-session-artifacts-summary-block
                  "niwa-session-artifacts" (summary))
(autoload 'niwa-session-artifacts-artifact-present-p
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-publish-text "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-rotate-segment "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-segment-path "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-strip-summary-handoff-prefix
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-summary-block "niwa-session-artifacts")

;; `niwa-session-codec'
(declare-function niwa-session-codec-portable-authority-p "niwa-session-codec" (session))
(autoload 'niwa-session-codec-portable-authority-p "niwa-session-codec")

;; `niwa-structs'
(declare-function niwa-file-interaction-modified-turn
                  "niwa-structs" (cl-x) t)
(declare-function niwa-file-interaction-read-turn
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-agent-registry "niwa-structs" (cl-x) t)
(declare-function niwa-session-current-segment
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-enqueue-pending-reminder
                  "niwa-structs" (session body))
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(declare-function niwa-session-tool-results-directory
                  "niwa-structs" (session))
(declare-function niwa-session-touched-files "niwa-structs" (cl-x) t)
(declare-function niwa-session-turn-count "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace-instruction-hashes
                  "niwa-structs" (cl-x) t)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-transcript-audit'
(declare-function niwa--format-hook-audit-record
                  "niwa-transcript-audit" (record))
(declare-function niwa--strip-hook-audit-blocks
                  "niwa-transcript-audit" (text))
(autoload 'niwa--format-hook-audit-record "niwa-transcript-audit")
(autoload 'niwa--strip-hook-audit-blocks "niwa-transcript-audit")

;; `niwa-transcript'
(declare-function niwa-transcript--mailbox-any-block-at-point
                  "niwa-transcript" (limit))
(declare-function niwa-transcript-segments
                  "niwa-transcript" (start end))
(autoload 'niwa-transcript-segments "niwa-transcript")

;; `niwa-utilities'
(declare-function niwa--same-file-p "niwa-utilities" (a b))
(autoload 'niwa--same-file-p "niwa-utilities")

;; `niwa-view'
(declare-function niwa-view--full-rerender "niwa-view" ())

;; `niwa-view-agent'
(declare-function niwa-view-agent-live-transcript-start
                  "niwa-view-agent" (invocation))
(autoload 'niwa-view-agent-live-transcript-start "niwa-view-agent")

;; `niwa-view-disclosure'
(declare-function niwa-view-disclosure-compaction-tool-states
                  "niwa-view-disclosure" ())
(declare-function niwa-view-disclosure-rebase-compaction-tool-states
                  "niwa-view-disclosure" (states data-buffer))

;; `niwa-view-stream'
(declare-function niwa-view-stream-end-progress
                  "niwa-view-stream" (view))
(declare-function niwa-view-stream-show-progress
                  "niwa-view-stream" (view status &optional owner))
(autoload 'niwa-view-stream-end-progress "niwa-view-stream")
(autoload 'niwa-view-stream-show-progress "niwa-view-stream")

(defcustom niwa-compact-target-file-reference-reminder-limit 20
  "Maximum number of compacted file references to cite in one reminder."
  :type 'natnum
  :group 'niwa)

(defvar-local niwa-compact-target-current-request-reminder nil
  "Reminder body to inject into the current auto-compacted request.")

(defvar-local niwa-compact-target-current-request-hook-context nil
  "SessionStart context to inject into the current auto-compacted request.")

(defun niwa-compact-target-current-persisted-p ()
  "Return non-nil when current buffer can use segment rotation."
  (and (boundp 'niwa--session)
       niwa--session
       (niwa-session-save-path niwa--session)
       buffer-file-name
       (niwa--same-file-p
        buffer-file-name
        (niwa-session-artifacts-segment-path
         (niwa-session-save-path niwa--session)
         (niwa-session-current-segment niwa--session)))))

(defun niwa-compact-target--apply
    (summary &optional tail-text pending-text hook-audits archive-text)
  "Rotate the current segment with SUMMARY, TAIL-TEXT, and PENDING-TEXT.
HOOK-AUDITS are persisted next to the summary.  ARCHIVE-TEXT contains
hidden execution records replacing compacted tool rows."
  (let ((session (and (boundp 'niwa--session) niwa--session)))
    (unless (and session (niwa-compact-target-current-persisted-p))
      (user-error "Session is not materialized on disk"))
    (remove-text-properties 0 (length summary) '(gptel nil face nil) summary)
    (setq summary (niwa-compact-target--append-hook-audits summary hook-audits))
    (niwa-session-artifacts-rotate-segment
     session (current-buffer) summary
     :tail-text tail-text
     :pending-text pending-text
     :archive-text archive-text)))


(defun niwa-compact-target--omitted-file-references
    (session preserved-tail-turns auto)
  "Return touched files likely omitted from SESSION's compacted history.
PRESERVED-TAIL-TURNS is the actual number of complete recent user turns
retained after tail-budget and aggressive-compaction decisions.  AUTO
non-nil marks a mid-request compaction: files touched during the
in-flight turn carry the reserved turn number above the committed
count, and their evidence is exactly what a mid-request compaction
summarizes away, so they are included rather than assumed visible."
  (when-let* ((table (and session (niwa-session-touched-files session)))
              ((hash-table-p table)))
    (let* ((turn (or (niwa-session-turn-count session) 0))
           (cutoff (max 0 (- turn (max 0 (or preserved-tail-turns 0)))))
           files)
      (maphash
       (lambda (path interaction)
         (let ((last-turn
                (max (or (niwa-file-interaction-modified-turn interaction)
                         0)
                     (or (niwa-file-interaction-read-turn interaction) 0))))
          (when (or (< last-turn cutoff)
                    (and auto (> last-turn turn)))
            (push path files))))
      table)
      (sort files #'string<))))

(defun niwa-compact-target-file-reference-reminder-body
    (session preserved-tail-turns auto)
  "Return reminder body for SESSION file references omitted by compaction.
PRESERVED-TAIL-TURNS is the actual count returned by
`niwa-compact-evidence-preserved-tail-turn-count'.  AUTO non-nil
marks a mid-request compaction, which also lists files touched during
the in-flight turn."
  (when-let* ((files (niwa-compact-target--omitted-file-references
                      session preserved-tail-turns auto)))
    (let* ((limit niwa-compact-target-file-reference-reminder-limit)
           (shown (cl-subseq files 0 (min limit (length files))))
           (omitted (- (length files) (length shown))))
      (concat
       "Compaction omitted older transcript content for files you previously read or edited. Re-read any file before relying on exact contents, line numbers, or stale diffs.\n\n"
       (mapconcat (lambda (path) (format "- %s" path)) shown "\n")
       (when (> omitted 0)
         (format "\n- ... %d more file references omitted" omitted))))))

(defun niwa-compact-target-hook-audit-records (decision)
  "Return PreCompact audit records for hook DECISION."
  (niwa-hooks-context-audit-records decision 'PreCompact 'compact-context))

(defun niwa-compact-target--append-hook-audits (summary records)
  "Return SUMMARY followed by ignored PreCompact audit RECORDS."
  (if (and records (stringp summary))
      (concat summary
              (mapconcat #'niwa--format-hook-audit-record records ""))
    summary))


(defun niwa-compact-target--latest-assignment (invocation start)
  "Return INVOCATION's latest assignment after START, verbatim.
Direct idle follow-ups are task prompts; queued or in-flight steering is
parent MAIL.  Inspect canonical segments, never quoted control text or
inherited history.  A queued-mail wake prompt is not an assignment."
  (let* ((session (niwa-agent-invocation-parent-session invocation))
         (path (niwa-agent-invocation-require-path invocation))
         (record (cdr (assoc path (niwa-session-agent-registry session))))
         (parent (and record (niwa-agent-record-parent-path record)))
         latest)
    (save-restriction
      ;; The canonical scanner expands to complete property runs.  Clamp its
      ;; source so a restored user run cannot include the old task or summary.
      (narrow-to-region start (point-max))
      (save-excursion
        (dolist (segment (niwa-transcript-segments (point-min) (point-max)))
          (pcase-let ((`(,type ,begin ,end) segment))
            (pcase type
              ('user
               (let ((text (buffer-substring-no-properties begin end)))
                 (when (and (string-match
                             "\\`[ \t\n]*\\* Agent Task: [^\n]*\n" text)
                            (not (equal
                                  (string-trim (substring text (match-end 0)))
                                  niwa-agent-control--wake-prompt)))
                   (setq latest text))))
              ('mailbox
               (goto-char begin)
               (while (< (point) end)
                 (skip-chars-forward " \t\n" end)
                 (if-let* ((block (niwa-transcript--mailbox-any-block-at-point end)))
                     (progn
                       (when (and (eq 'agent-message (plist-get block :kind))
                                  (equal parent (plist-get block :id))
                                  (string-match-p
                                   "\\btype=\"MAIL\""
                                   (buffer-substring-no-properties
                                    (point) (plist-get block :open-end))))
                         (setq latest
                               (buffer-substring-no-properties
                                (point) (plist-get block :close-end))))
                       (goto-char (plist-get block :close-end)))
                   (goto-char end)))))))))
    latest))

(defun niwa-compact-target-agent-target (invocation)
  "Return the private compaction target for persisted INVOCATION, or nil."
  (when-let* (((niwa-agent-invocation-p invocation))
              (buffer (niwa-agent-invocation-buffer invocation))
              ((eq buffer (current-buffer)))
              (session (niwa-agent-invocation-parent-session invocation))
              (save-path (niwa-session-save-path session))
              (relative-path
               (niwa-agent-invocation-transcript-relative-path invocation))
              ((niwa-agent-persistence-transcript-path-p
                relative-path save-path))
              (canonical-path (expand-file-name relative-path save-path))
              (buffer-path buffer-file-name)
              ((niwa-session-codec-portable-authority-p session))
              ((equal (expand-file-name buffer-path) canonical-path))
              ((niwa-session-artifacts-artifact-present-p
                session relative-path))
              (task-heading (niwa-compact-evidence-agent-task-heading invocation))
              (first-output
               (cl-find-if
                (lambda (segment)
                  (memq (car segment) '(response reasoning tool)))
                (niwa-transcript-segments task-heading (point-max)))))
    (let* ((summary-bounds (niwa-compact-evidence-agent-summary-bounds invocation))
           (anchor-end (or (plist-get summary-bounds :begin)
                           (cadr first-output))))
      (when (<= task-heading anchor-end)
        (let ((previous-summary
               (when summary-bounds
                 (niwa-session-artifacts-strip-summary-handoff-prefix
                  (string-trim
                   (niwa--strip-hook-audit-blocks
                    (buffer-substring
                     (plist-get summary-bounds :body-begin)
                     (plist-get summary-bounds :body-end))))))))
          (list :buffer buffer
                :invocation invocation
                :session session
                :workspace (niwa-session-workspace session)
                :transcript-path canonical-path
                :origin (niwa-agent-invocation-require-path invocation)
                :history-prefix-regions
                (and (< (point-min) task-heading)
                     (list (cons (point-min) task-heading)))
                :anchor-text (buffer-substring task-heading anchor-end)
                :assignment-text
                (niwa-compact-target--latest-assignment
                 invocation (or (plist-get summary-bounds :end) anchor-end))
                :body-start (or (plist-get summary-bounds :end) anchor-end)
                :previous-summary previous-summary
                :prompt-session session
                :skill-agent-path
                (niwa-agent-invocation-require-path invocation)
                :tool-results-dir
                (niwa-session-tool-results-directory session)
                :eligible-p t
                :apply #'niwa-compact-target--agent-apply
                :start #'niwa-compact-target--agent-start
                :complete #'niwa-compact-target--agent-complete
                :resume #'niwa--compact-target-resume
                :fail #'niwa--compact-agent-terminal-failure
                :warn-on-completion nil))))))

(defun niwa-compact-target--agent-archive-path (session canonical-path)
  "Return SESSION's next unused archive for CANONICAL-PATH."
  (let* ((save-path
          (file-name-as-directory (niwa-session-save-path session)))
         (remote-p (file-remote-p save-path))
         (stem (if (string-suffix-p ".chat.org" canonical-path)
                   (string-remove-suffix ".chat.org" canonical-path)
                 canonical-path)))
    (cl-loop for number from 1
             for path = (format "%s.compact-%04d.chat.org" stem number)
             for logical =
             (and (string-prefix-p save-path path)
                  (substring path (length save-path)))
             unless logical
             do (error "Archive path escaped session: %s" path)
             unless (if remote-p
                        (niwa-session-artifacts-artifact-present-p
                         session logical)
                      (file-exists-p path))
             return path)))


(defun niwa-compact-target--commit-execution-row-archive (target)
  "Commit TARGET's prepared execution-row archive after compaction."
  (when-let* ((plan (plist-get target :execution-archive-plan)))
    (niwa-execution-transcript-commit-archive
     (plist-get target :buffer) plan)))

(defun niwa-compact-target--execution-row-archive-text (target)
  "Return TARGET's durable execution-row replacement records."
  (when-let* ((plan (plist-get target :execution-archive-plan)))
    (niwa-execution-transcript-archive-text plan)))

(defun niwa-compact-target--agent-apply
    (target summary tail-text pending-text hook-audits
            &optional _auto _preserved-tail-turns)
  "Apply agent TARGET compaction with SUMMARY, TAIL-TEXT, and PENDING-TEXT.
HOOK-AUDITS are stored beside SUMMARY.  Return the recovery archive path."
  (let* ((invocation (plist-get target :invocation))
         (session (plist-get target :session))
         (canonical-path (plist-get target :transcript-path))
         (archive-path
          (niwa-compact-target--agent-archive-path
           (plist-get target :session) canonical-path))
         (summary (niwa-compact-target--append-hook-audits summary hook-audits))
         (execution-archive-text
          (niwa-compact-target--execution-row-archive-text target)))
    (unless (niwa-agent-conversation-save invocation)
      (error "Could not persist agent transcript before compaction"))
    (niwa-session-artifacts-publish-text
     (plist-get target :session) archive-path
     (buffer-substring-no-properties (point-min) (point-max))
     'utf-8-unix)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (plist-get target :anchor-text))
      (unless (bolp) (insert "\n"))
      (insert (niwa-session-artifacts-summary-block summary))
      ;; Do not entrust the current assignment to a lossy model summary.
      ;; Keep its original transcript form so the next compaction can find it
      ;; again, including after save/resume.  Tail/pending copies already do so.
      (when-let* ((assignment (plist-get target :assignment-text))
                  ((not (equal
                         assignment
                         (with-temp-buffer
                           (insert (or tail-text "") (or pending-text ""))
                           (niwa-compact-target--latest-assignment
                            invocation (point-min)))))))
        (insert assignment)
        (unless (bolp) (insert "\n")))
      (when tail-text (insert tail-text))
      (when execution-archive-text (insert execution-archive-text))
      (when pending-text (insert pending-text))
      (set-buffer-modified-p t))
    ;; Native children reuse their canonical transcript.  Persist its rebased
    ;; submitted-input boundary together with the rewritten summary and tail.
    (when-let* ((record (plist-get target :native-history-record)))
      (plist-put record :input-boundary
                 (cons 0 (- (point-max)
                            (niwa-session-artifacts-content-start (current-buffer))))))
    (unless (niwa-agent-conversation-save invocation)
      (error "Could not persist compacted agent transcript"))
    (setf (niwa-session-workspace-instruction-hashes session)
          (cl-delete (plist-get target :origin)
                     (niwa-session-workspace-instruction-hashes session)
                     :key #'caar :test #'equal))
    (niwa-compact-target--commit-execution-row-archive target)
    archive-path))

(defun niwa-compact-target--main-apply
    (target summary tail-text pending-text hook-audits
            auto preserved-tail-turns)
  "Apply main-session TARGET compaction and arrange its file reminder."
  (let* ((session (plist-get target :session))
         (view-buffer (and (buffer-live-p niwa--view-buffer)
                           niwa--view-buffer))
         (tool-states
          (and view-buffer
               (with-current-buffer view-buffer
                 (niwa-view-disclosure-compaction-tool-states)))))
    (niwa-compact-target--apply
     summary tail-text pending-text hook-audits
     (niwa-compact-target--execution-row-archive-text target))
    (when (and view-buffer tool-states)
      (with-current-buffer view-buffer
        (niwa-view-disclosure-rebase-compaction-tool-states
         tool-states (plist-get target :buffer))))
    (niwa-compact-target--commit-execution-row-archive target)
    ;; The one-shot accepted-plan reference may have been delivered in
    ;; a turn this compaction just summarized away; resetting its fired
    ;; mark lets it re-surface the plan address once.  Its trigger
    ;; still gates on an accepted plan, so this is inert otherwise.
    (niwa-reminders-rearm-plan-reference session)
    (let ((reminder
           (niwa-compact-target-file-reference-reminder-body
            session preserved-tail-turns auto)))
      (cond
       (auto
        (setq niwa-compact-target-current-request-reminder reminder))
       (reminder
        (niwa-session-enqueue-pending-reminder session reminder))))))

(defun niwa-compact-target--main-start (_target)
  "Show main-session compaction progress for TARGET."
  (niwa-view-stream-show-progress
   niwa--view-buffer "Compacting..." 'compaction))

(defun niwa-compact-target--agent-start (target)
  "Show agent TARGET compaction progress."
  (niwa-agent-conversation-record-activity
   (plist-get target :invocation)
   '(:type status :summary "Compacting..."))
  (gptel--update-status " Compacting..." 'warning))

(defun niwa-compact-target-begin-root-context-epoch (target auto)
  "Begin TARGET's root compact epoch.
When AUTO is non-nil, attach the new context to the pending request."
  (when (plist-get target :begin-context-epoch)
    (let ((buffer (plist-get target :buffer))
          (session (plist-get target :session)))
      (when (and session (buffer-live-p buffer))
        (with-current-buffer buffer
          (niwa--run-session-start-hooks "compact")
          (when-let* ((context (and auto
                                    (niwa-hooks-take-session-context
                                     session))))
            (setq niwa-compact-target-current-request-hook-context context)
            (let ((inhibit-read-only t)
                  (start (point-max)))
              (goto-char start)
              (unless (bolp) (insert "\n"))
              (insert "\n" context "\n")
              (remove-text-properties start (point) '(gptel nil)))))))))

(defun niwa-compact-target--main-complete (_target auto)
  "Restore main-session display state after compaction.
AUTO is non-nil for automatic compaction."
  (when-let* ((view-buffer niwa--view-buffer)
              (_ (buffer-live-p view-buffer)))
    (with-current-buffer view-buffer
      (niwa-view--full-rerender)
      (unless auto
        (niwa-view-stream-end-progress view-buffer)))))

(defun niwa-compact-target--agent-complete (target _auto)
  "Restore ordinary continuation status for agent TARGET.
Open transcript views of the agent are rebuilt: compaction rewrote the
buffer under their live projection, as main compaction does for its view."
  (niwa-agent-conversation-record-activity
   (plist-get target :invocation)
   '(:type status :summary "waiting"))
  (niwa-view-agent-live-transcript-start (plist-get target :invocation))
  (gptel--update-status " Calling Agent..." 'font-lock-escape-face))

(defun niwa-compact-target-main-target ()
  "Return the private target adapter for the current main-session segment."
  (let ((session niwa--session))
    (list :buffer (current-buffer)
          :session session
          :workspace (and session (niwa-session-workspace session))
          :invocation nil
          :transcript-path (and session (niwa-session-save-path session))
          :origin "/root"
          :body-start (niwa-compact-evidence-body-start)
          :previous-summary (niwa-compact-evidence-previous-summary)
          :prompt-session session
          :skill-agent-path "/root"
          :tool-results-dir
          (niwa-session-tool-results-directory session)
          :eligible-p (niwa-compact-target-current-persisted-p)
          :begin-context-epoch t
          :apply #'niwa-compact-target--main-apply
          :start #'niwa-compact-target--main-start
          :complete #'niwa-compact-target--main-complete
          :resume #'niwa--compact-target-resume
          :resume-status #'niwa--compact-main-resume-status
          :fail #'niwa--compact-main-failure
          :warn-on-completion t)))

(defun niwa-compact-target-call (target operation &rest args)
  "Invoke TARGET OPERATION with TARGET followed by ARGS."
  (let ((function (plist-get target operation)))
    (unless (functionp function)
      (error "Compaction target lacks %s operation" operation))
    (apply function target args)))

(provide 'niwa-compact-target)

;;; niwa-compact-target.el ends here
