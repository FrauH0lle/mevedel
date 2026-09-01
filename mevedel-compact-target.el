;;; mevedel-compact-target.el -- Compaction target transactions -*- lexical-binding: t -*-

;;; Commentary:

;; Owns root and agent transcript targets, archive publication, and application.
;; Evidence selection and asynchronous request settlement remain separate.

;;; Code:

;; `setf' on a slot of a struct defined elsewhere needs that
;; `cl-defstruct' at compile time: `declare-function' supplies the
;; accessor but not the setter, and without the expander the form
;; compiles to a call to a function that does not exist.
(eval-when-compile (require 'mevedel-structs))

(require 'cl-lib)
(require 'subr-x)

;; `gptel'
(declare-function gptel--update-status "ext:gptel" (msg &optional face))

;; `mevedel-agent-conversation'
(declare-function mevedel-agent-conversation-record-activity
                  "mevedel-agent-conversation"
                  (invocation item &optional suppress-rerender))
(declare-function mevedel-agent-conversation-save
                  "mevedel-agent-conversation" (invocation &optional deferred))

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-transcript-path-p
                  "mevedel-agent-persistence" (path save-path))
(autoload 'mevedel-agent-persistence-transcript-path-p
  "mevedel-agent-persistence")

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-buffer "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-require-path
                  "mevedel-agents" (invocation))
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)

;; `mevedel-chat'
(declare-function mevedel--run-session-start-hooks "mevedel-chat" (source))

;; `mevedel-compact-evidence'
(declare-function mevedel-compact-evidence-agent-summary-bounds
                  "mevedel-compact-evidence" (&optional invocation))
(declare-function mevedel-compact-evidence-agent-task-heading
                  "mevedel-compact-evidence" (&optional invocation))
(declare-function mevedel-compact-evidence-body-start
                  "mevedel-compact-evidence" ())
(declare-function mevedel-compact-evidence-previous-summary
                  "mevedel-compact-evidence" ())
(declare-function mevedel-compact-evidence-turn-starts-before
                  "mevedel-compact-evidence" (limit &optional body-start))
(autoload 'mevedel-compact-evidence-agent-summary-bounds
  "mevedel-compact-evidence")
(autoload 'mevedel-compact-evidence-agent-task-heading
  "mevedel-compact-evidence")
(autoload 'mevedel-compact-evidence-body-start "mevedel-compact-evidence")
(autoload 'mevedel-compact-evidence-previous-summary
  "mevedel-compact-evidence")
(autoload 'mevedel-compact-evidence-turn-starts-before
  "mevedel-compact-evidence")

;; `mevedel-execution-transcript'
(declare-function mevedel-execution-transcript-archive-text
                  "mevedel-execution-transcript" (plan))
(declare-function mevedel-execution-transcript-commit-archive
                  "mevedel-execution-transcript" (data-buffer plan))
(autoload 'mevedel-execution-transcript-archive-text
  "mevedel-execution-transcript")
(autoload 'mevedel-execution-transcript-commit-archive
  "mevedel-execution-transcript")

;; `mevedel-hooks'
(declare-function mevedel-hooks-context-audit-records
                  "mevedel-hooks" (decision event type &optional omit-context))
(declare-function mevedel-hooks-take-session-context
                  "mevedel-hooks" (session))

;; `mevedel-reminders'
(declare-function mevedel-reminders-rearm-plan-reference
                  "mevedel-reminders" (session))
(autoload 'mevedel-reminders-rearm-plan-reference "mevedel-reminders")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-artifact-present-p
                  "mevedel-session-artifacts"
                  (session logical &optional committed-only))
(declare-function mevedel-session-artifacts-publish-text
                  "mevedel-session-artifacts"
                  (session path content &optional coding))
(declare-function mevedel-session-artifacts-rotate-segment
                  "mevedel-session-artifacts"
                  (session buffer summary &rest keys))
(declare-function mevedel-session-artifacts-segment-path
                  "mevedel-session-artifacts" (save-path n))
(declare-function mevedel-session-artifacts-strip-summary-handoff-prefix
                  "mevedel-session-artifacts" (summary))
(declare-function mevedel-session-artifacts-summary-block
                  "mevedel-session-artifacts" (summary))
(autoload 'mevedel-session-artifacts-artifact-present-p
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-publish-text "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-rotate-segment "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-segment-path "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-strip-summary-handoff-prefix
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-summary-block "mevedel-session-artifacts")

;; `mevedel-structs'
(declare-function mevedel-file-interaction-modified-turn
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-file-interaction-read-turn
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-current-segment
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-enqueue-pending-reminder
                  "mevedel-structs" (session body))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-tool-results-directory
                  "mevedel-structs" (session))
(declare-function mevedel-session-touched-files "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace-instruction-hashes
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-transcript-audit" (record))
(declare-function mevedel--strip-hook-audit-blocks
                  "mevedel-transcript-audit" (text))
(autoload 'mevedel--format-hook-audit-record "mevedel-transcript-audit")
(autoload 'mevedel--strip-hook-audit-blocks "mevedel-transcript-audit")

;; `mevedel-transcript'
(declare-function mevedel-transcript-segments
                  "mevedel-transcript" (start end))
(autoload 'mevedel-transcript-segments "mevedel-transcript")

;; `mevedel-utilities'
(declare-function mevedel--same-file-p "mevedel-utilities" (a b))
(autoload 'mevedel--same-file-p "mevedel-utilities")

;; `mevedel-view'
(declare-function mevedel-view--full-rerender "mevedel-view" ())

;; `mevedel-view-stream'
(declare-function mevedel-view--stop-request-progress
                  "mevedel-view-stream" ())
(declare-function mevedel-view--stop-spinner "mevedel-view-stream" ())
(declare-function mevedel-view--update-spinner
                  "mevedel-view-stream" (status &optional owner))
(defvar mevedel-view--status-owner-override)

(defcustom mevedel-compact-target-file-reference-reminder-limit 20
  "Maximum number of compacted file references to cite in one reminder."
  :type 'natnum
  :group 'mevedel)

(defvar-local mevedel-compact-target-current-request-reminder nil
  "Reminder body to inject into the current auto-compacted request.")

(defvar-local mevedel-compact-target-current-request-hook-context nil
  "SessionStart context to inject into the current auto-compacted request.")

(defun mevedel-compact-target-current-persisted-p ()
  "Return non-nil when current buffer can use segment rotation."
  (and (boundp 'mevedel--session)
       mevedel--session
       (mevedel-session-save-path mevedel--session)
       buffer-file-name
       (mevedel--same-file-p
        buffer-file-name
        (mevedel-session-artifacts-segment-path
         (mevedel-session-save-path mevedel--session)
         (mevedel-session-current-segment mevedel--session)))))

(defun mevedel-compact-target--apply
    (summary &optional tail-text pending-text hook-audits archive-text)
  "Rotate the current segment with SUMMARY, TAIL-TEXT, and PENDING-TEXT.
HOOK-AUDITS are persisted next to the summary.  ARCHIVE-TEXT contains
hidden execution records replacing compacted tool rows."
  (let ((session (and (boundp 'mevedel--session) mevedel--session)))
    (unless (and session (mevedel-compact-target-current-persisted-p))
      (user-error "Session is not materialized on disk"))
    (remove-text-properties 0 (length summary) '(gptel nil face nil) summary)
    (setq summary (mevedel-compact-target--append-hook-audits summary hook-audits))
    (mevedel-session-artifacts-rotate-segment
     session (current-buffer) summary
     :tail-text tail-text
     :pending-text pending-text
     :archive-text archive-text)))


(defun mevedel-compact-target--omitted-file-references
    (session preserved-tail-turns auto)
  "Return touched files likely omitted from SESSION's compacted history.
PRESERVED-TAIL-TURNS is the actual number of complete recent user turns
retained after tail-budget and aggressive-compaction decisions.  AUTO
non-nil marks a mid-request compaction: files touched during the
in-flight turn carry the reserved turn number above the committed
count, and their evidence is exactly what a mid-request compaction
summarizes away, so they are included rather than assumed visible."
  (when-let* ((table (and session (mevedel-session-touched-files session)))
              ((hash-table-p table)))
    (let* ((turn (or (mevedel-session-turn-count session) 0))
           (cutoff (max 0 (- turn (max 0 (or preserved-tail-turns 0)))))
           files)
      (maphash
       (lambda (path interaction)
         (let ((last-turn
                (max (or (mevedel-file-interaction-modified-turn interaction)
                         0)
                     (or (mevedel-file-interaction-read-turn interaction) 0))))
          (when (or (< last-turn cutoff)
                    (and auto (> last-turn turn)))
            (push path files))))
      table)
      (sort files #'string<))))

(defun mevedel-compact-target-file-reference-reminder-body
    (session preserved-tail-turns auto)
  "Return reminder body for SESSION file references omitted by compaction.
PRESERVED-TAIL-TURNS is the actual count returned by
`mevedel-compact-evidence-preserved-tail-turn-count'.  AUTO non-nil
marks a mid-request compaction, which also lists files touched during
the in-flight turn."
  (when-let* ((files (mevedel-compact-target--omitted-file-references
                      session preserved-tail-turns auto)))
    (let* ((limit mevedel-compact-target-file-reference-reminder-limit)
           (shown (cl-subseq files 0 (min limit (length files))))
           (omitted (- (length files) (length shown))))
      (concat
       "Compaction omitted older transcript content for files you previously read or edited. Re-read any file before relying on exact contents, line numbers, or stale diffs.\n\n"
       (mapconcat (lambda (path) (format "- %s" path)) shown "\n")
       (when (> omitted 0)
         (format "\n- ... %d more file references omitted" omitted))))))

(defun mevedel-compact-target-hook-audit-records (decision)
  "Return PreCompact audit records for hook DECISION."
  (mevedel-hooks-context-audit-records decision 'PreCompact 'compact-context))

(defun mevedel-compact-target--append-hook-audits (summary records)
  "Return SUMMARY followed by ignored PreCompact audit RECORDS."
  (if (and records (stringp summary))
      (concat summary
              (mapconcat #'mevedel--format-hook-audit-record records ""))
    summary))


(defun mevedel-compact-target-agent-target (invocation)
  "Return the private compaction target for persisted INVOCATION, or nil."
  (when-let* (((mevedel-agent-invocation-p invocation))
              (buffer (mevedel-agent-invocation-buffer invocation))
              ((eq buffer (current-buffer)))
              (session (mevedel-agent-invocation-parent-session invocation))
              (save-path (mevedel-session-save-path session))
              (relative-path
               (mevedel-agent-invocation-transcript-relative-path invocation))
              ((mevedel-agent-persistence-transcript-path-p
                relative-path save-path))
              (canonical-path (expand-file-name relative-path save-path))
              (buffer-path buffer-file-name)
              ((if (file-remote-p save-path)
                   (and (equal (expand-file-name buffer-path)
                               canonical-path)
                        (mevedel-session-artifacts-artifact-present-p
                         session relative-path))
                 (and (mevedel--same-file-p buffer-path canonical-path)
                      (file-regular-p canonical-path)
                      (not (file-symlink-p canonical-path))
                      (file-writable-p canonical-path)
                      (file-writable-p
                       (file-name-directory canonical-path)))))
              (task-heading (mevedel-compact-evidence-agent-task-heading invocation))
              (first-output
               (cl-find-if
                (lambda (segment)
                  (memq (car segment) '(response reasoning tool)))
                (mevedel-transcript-segments task-heading (point-max)))))
    (let* ((summary-bounds (mevedel-compact-evidence-agent-summary-bounds invocation))
           (anchor-end (or (plist-get summary-bounds :begin)
                           (cadr first-output))))
      (when (<= task-heading anchor-end)
        (let ((previous-summary
               (when summary-bounds
                 (mevedel-session-artifacts-strip-summary-handoff-prefix
                  (string-trim
                   (mevedel--strip-hook-audit-blocks
                    (buffer-substring
                     (plist-get summary-bounds :body-begin)
                     (plist-get summary-bounds :body-end))))))))
          (list :buffer buffer
                :invocation invocation
                :session session
                :workspace (mevedel-session-workspace session)
                :transcript-path canonical-path
                :origin (mevedel-agent-invocation-require-path invocation)
                :history-prefix-regions
                (and (< (point-min) task-heading)
                     (list (cons (point-min) task-heading)))
                :anchor-text (buffer-substring task-heading anchor-end)
                :body-start (or (plist-get summary-bounds :end) anchor-end)
                :previous-summary previous-summary
                :prompt-session session
                :skill-agent-path
                (mevedel-agent-invocation-require-path invocation)
                :tool-results-dir
                (mevedel-session-tool-results-directory session)
                :eligible-p t
                :apply #'mevedel-compact-target--agent-apply
                :start #'mevedel-compact-target--agent-start
                :complete #'mevedel-compact-target--agent-complete
                :resume #'mevedel--compact-target-resume
                :fail #'mevedel--compact-agent-terminal-failure
                :warn-on-completion nil))))))

(defun mevedel-compact-target--agent-archive-path (session canonical-path)
  "Return SESSION's next unused archive for CANONICAL-PATH."
  (let* ((save-path
          (file-name-as-directory (mevedel-session-save-path session)))
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
                        (mevedel-session-artifacts-artifact-present-p
                         session logical)
                      (file-exists-p path))
             return path)))


(defun mevedel-compact-target--commit-execution-row-archive (target)
  "Commit TARGET's prepared execution-row archive after compaction."
  (when-let* ((plan (plist-get target :execution-archive-plan)))
    (mevedel-execution-transcript-commit-archive
     (plist-get target :buffer) plan)))

(defun mevedel-compact-target--execution-row-archive-text (target)
  "Return TARGET's durable execution-row replacement records."
  (when-let* ((plan (plist-get target :execution-archive-plan)))
    (mevedel-execution-transcript-archive-text plan)))

(defun mevedel-compact-target--agent-apply
    (target summary tail-text pending-text hook-audits
            &optional _auto _preserved-tail-turns)
  "Apply agent TARGET compaction with SUMMARY, TAIL-TEXT, and PENDING-TEXT.
HOOK-AUDITS are stored beside SUMMARY.  Return the recovery archive path."
  (let* ((invocation (plist-get target :invocation))
         (session (plist-get target :session))
         (canonical-path (plist-get target :transcript-path))
         (archive-path
          (mevedel-compact-target--agent-archive-path
           (plist-get target :session) canonical-path))
         (summary (mevedel-compact-target--append-hook-audits summary hook-audits))
         (execution-archive-text
          (mevedel-compact-target--execution-row-archive-text target)))
    (unless (mevedel-agent-conversation-save invocation)
      (error "Could not persist agent transcript before compaction"))
    (mevedel-session-artifacts-publish-text
     (plist-get target :session) archive-path
     (buffer-substring-no-properties (point-min) (point-max))
     'utf-8-unix)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (plist-get target :anchor-text))
      (unless (bolp) (insert "\n"))
      (insert (mevedel-session-artifacts-summary-block summary))
      (when tail-text (insert tail-text))
      (when execution-archive-text (insert execution-archive-text))
      (when pending-text (insert pending-text))
      (set-buffer-modified-p t))
    (unless (mevedel-agent-conversation-save invocation)
      (error "Could not persist compacted agent transcript"))
    (setf (mevedel-session-workspace-instruction-hashes session)
          (cl-delete (plist-get target :origin)
                     (mevedel-session-workspace-instruction-hashes session)
                     :key #'caar :test #'equal))
    (mevedel-compact-target--commit-execution-row-archive target)
    archive-path))

(defun mevedel-compact-target--main-apply
    (target summary tail-text pending-text hook-audits
            auto preserved-tail-turns)
  "Apply main-session TARGET compaction and arrange its file reminder."
  (let ((session (plist-get target :session)))
    (mevedel-compact-target--apply
     summary tail-text pending-text hook-audits
     (mevedel-compact-target--execution-row-archive-text target))
    (mevedel-compact-target--commit-execution-row-archive target)
    ;; The one-shot accepted-plan reference may have been delivered in
    ;; a turn this compaction just summarized away; resetting its fired
    ;; mark lets it re-surface the plan address once.  Its trigger
    ;; still gates on an accepted plan, so this is inert otherwise.
    (mevedel-reminders-rearm-plan-reference session)
    (let ((reminder
           (mevedel-compact-target-file-reference-reminder-body
            session preserved-tail-turns auto)))
      (cond
       (auto
        (setq mevedel-compact-target-current-request-reminder reminder))
       (reminder
        (mevedel-session-enqueue-pending-reminder session reminder))))))

(defun mevedel-compact-target--main-start (_target)
  "Show main-session compaction progress for TARGET."
  (when-let* ((view-buffer mevedel--view-buffer)
              (_ (buffer-live-p view-buffer)))
    (with-current-buffer view-buffer
      (let ((mevedel-view--status-owner-override 'compaction))
        (mevedel-view--update-spinner "Compacting...")))))

(defun mevedel-compact-target--agent-start (target)
  "Show agent TARGET compaction progress."
  (mevedel-agent-conversation-record-activity
   (plist-get target :invocation)
   '(:type status :summary "Compacting..."))
  (gptel--update-status " Compacting..." 'warning))

(defun mevedel-compact-target-begin-root-context-epoch (target auto)
  "Begin TARGET's root compact epoch.
When AUTO is non-nil, attach the new context to the pending request."
  (when (plist-get target :begin-context-epoch)
    (let ((buffer (plist-get target :buffer))
          (session (plist-get target :session)))
      (when (and session (buffer-live-p buffer))
        (with-current-buffer buffer
          (mevedel--run-session-start-hooks "compact")
          (when-let* ((context (and auto
                                    (mevedel-hooks-take-session-context
                                     session))))
            (setq mevedel-compact-target-current-request-hook-context context)
            (let ((inhibit-read-only t)
                  (start (point-max)))
              (goto-char start)
              (unless (bolp) (insert "\n"))
              (insert "\n" context "\n")
              (remove-text-properties start (point) '(gptel nil)))))))))

(defun mevedel-compact-target--main-complete (_target auto)
  "Restore main-session display state after compaction.
AUTO is non-nil for automatic compaction."
  (when-let* ((view-buffer mevedel--view-buffer)
              (_ (buffer-live-p view-buffer)))
    (with-current-buffer view-buffer
      (mevedel-view--full-rerender)
      (unless auto
        (if (fboundp 'mevedel-view--stop-request-progress)
            (mevedel-view--stop-request-progress)
          (mevedel-view--stop-spinner))))))

(defun mevedel-compact-target--agent-complete (target _auto)
  "Restore ordinary continuation status for agent TARGET."
  (mevedel-agent-conversation-record-activity
   (plist-get target :invocation)
   '(:type status :summary "waiting"))
  (gptel--update-status " Calling Agent..." 'font-lock-escape-face))

(defun mevedel-compact-target-main-target ()
  "Return the private target adapter for the current main-session segment."
  (let ((session mevedel--session))
    (list :buffer (current-buffer)
          :session session
          :workspace (and session (mevedel-session-workspace session))
          :invocation nil
          :transcript-path (and session (mevedel-session-save-path session))
          :origin "/root"
          :body-start (mevedel-compact-evidence-body-start)
          :previous-summary (mevedel-compact-evidence-previous-summary)
          :prompt-session session
          :skill-agent-path "/root"
          :tool-results-dir
          (mevedel-session-tool-results-directory session)
          :eligible-p (mevedel-compact-target-current-persisted-p)
          :begin-context-epoch t
          :apply #'mevedel-compact-target--main-apply
          :start #'mevedel-compact-target--main-start
          :complete #'mevedel-compact-target--main-complete
          :resume #'mevedel--compact-target-resume
          :resume-status #'mevedel--compact-main-resume-status
          :fail #'mevedel--compact-main-failure
          :warn-on-completion t)))

(defun mevedel-compact-target-call (target operation &rest args)
  "Invoke TARGET OPERATION with TARGET followed by ARGS."
  (let ((function (plist-get target operation)))
    (unless (functionp function)
      (error "Compaction target lacks %s operation" operation))
    (apply function target args)))

(provide 'mevedel-compact-target)

;;; mevedel-compact-target.el ends here
