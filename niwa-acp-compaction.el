;;; niwa-acp-compaction.el --- Retained native summaries -*- lexical-binding: t -*-

;;; Commentary:
;; ACP compaction describes the external model's effective history.  Archive
;; the replaced transcript through ordinary segment publication, preserving
;; output received since compaction began as a recent tail.  It neither
;; starts another model request nor changes native context acknowledgements.

;;; Code:

(require 'niwa-engine)
(require 'niwa-compact-evidence)
(require 'niwa-compact-target)
(require 'niwa-execution-transcript)
(require 'niwa-session-artifacts)

;; `niwa-agents'
(declare-function niwa-agent-invocation-parent-session "niwa-agents" (invocation))

;; `niwa-reminders'
(declare-function niwa-reminders-make-pending-events "niwa-reminders" ())
(declare-function niwa-session-ensure-reminder "niwa-reminders" (session reminder))
(autoload 'niwa-reminders-make-pending-events "niwa-reminders")
(autoload 'niwa-session-ensure-reminder "niwa-reminders")

;; `niwa-view'
(declare-function niwa-view--full-rerender "niwa-view")

;; `niwa-view-core'
(defvar niwa--view-buffer)

(defun niwa-acp-compaction--apply (owner state summary)
  "Replace OWNER's compacted history according to STATE and SUMMARY."
  (let* ((info (niwa-engine-info owner))
         (root-p (niwa-request-p owner))
         (session (if root-p (niwa-request-session owner)
                    (niwa-agent-invocation-parent-session owner)))
         (start (plist-get state :start))
         (tail (buffer-substring start (point-max)))
         (plan (niwa-execution-transcript-prepare-archive
                (current-buffer)
                (niwa-compact-evidence-archived-tool-use-ids (point-min) start))))
    (setq summary
          (concat (format "%s's retained compaction summary follows. It may refer to context delivered through hooks.\n\n"
                          (if-let* ((backend (plist-get info :backend)))
                              (gptel-backend-name backend) "External agent"))
                  summary))
    (if root-p
        (progn
          (niwa-session-artifacts-rotate-segment
           session (current-buffer) summary
           :tail-text tail :archive-text (niwa-execution-transcript-archive-text plan))
          (niwa-execution-transcript-commit-archive (current-buffer) plan)
          ;; As after local compaction: the summary replaced file contents.
          (when-let* ((reminder (niwa-compact-target-file-reference-reminder-body
                                 session 0 t)))
            (niwa-session-enqueue-pending-reminder session reminder)
            (niwa-session-ensure-reminder session (niwa-reminders-make-pending-events))))
      (let ((target (or (niwa-compact-target-agent-target owner)
                        (error "The child transcript is not ready for compaction"))))
        (setq target (plist-put target :execution-archive-plan plan))
        (setq target (plist-put target :native-history-record
                                (plist-get info :niwa-claude-history)))
        (niwa-compact-target-call target :apply summary tail nil nil t 0)
        (niwa-compact-target-call target :complete t)))
    ;; Rotation replaces the buffer and collapses old stream markers.  Resume
    ;; insertion after the retained text, as gptel's own compaction does.
    (set-marker (plist-get info :position) (point-max) (current-buffer))
    (dolist (key '(:tracking-marker :reasoning-marker :tool-marker :reasoning-block))
      (setf (niwa-engine-info owner)
            (plist-put (niwa-engine-info owner) key nil)))
    (when (buffer-live-p niwa--view-buffer)
      (with-current-buffer niwa--view-buffer (niwa-view--full-rerender)))))

(defun niwa-acp-compaction-observe (owner update)
  "Apply an ACP compaction UPDATE for admitted OWNER in its owning buffer.
Completed identities remain recorded for this turn so terminal enrichment cannot
rotate twice. A terminal summary supersedes streamed chunks when both arrive."
  (let* ((info (niwa-engine-info owner))
         (id (alist-get 'compactionId update))
         (kind (alist-get 'sessionUpdate update))
         (status (alist-get 'status update))
         (states (plist-get info :niwa-acp-compactions))
         (state (alist-get id states nil nil #'equal)))
    (when (plist-get info :niwa-native-isolated)
      (error "Isolated compaction cannot replace root history"))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "ACP compaction has no identity"))
    (unless (plist-get state :terminal)
      (cond
       ((equal kind "compaction_summary_chunk")
        (when state
          (let ((content (alist-get 'content update)))
            (unless (and (equal "text" (alist-get 'type content))
                         (stringp (alist-get 'text content)))
              (error "ACP compaction summary is not text"))
            (plist-put state :chunks (cons (alist-get 'text content) (plist-get state :chunks))))))
       ((equal kind "compaction_update")
        (unless state
          (when (cl-some (lambda (entry) (not (plist-get (cdr entry) :terminal))) states)
            (error "ACP reported overlapping compactions"))
          (setq state (list :start (copy-marker (or (plist-get info :tracking-marker)
                                                  (point-max)) nil)
                            :chunks nil :terminal nil))
          (push (cons id state) states)
          (setf (niwa-engine-info owner)
                (plist-put info :niwa-acp-compactions states)))
        (pcase status
          ("in_progress" nil)
          ((or "cancelled" "failed")
           (plist-put state :terminal status)
           (set-marker (plist-get state :start) nil))
          ("completed"
           (let ((summary
                  (if-let* ((parts (alist-get 'summary update)))
                      (mapconcat
                       (lambda (part)
                         (unless (and (equal "text" (alist-get 'type part))
                                      (stringp (alist-get 'text part)))
                           (error "ACP compaction summary is not text"))
                         (alist-get 'text part)) parts "\n")
                    (apply #'concat (reverse (plist-get state :chunks))))))
             (when (string-empty-p (string-trim summary))
               (error "Agent completed compaction without a retained summary"))
             (niwa-acp-compaction--apply owner state summary)
             (plist-put state :terminal status)
             (plist-put state :chunks nil)
             (set-marker (plist-get state :start) nil)))
          (_ (error "Unknown ACP compaction status: %s" status))))))))

(provide 'niwa-acp-compaction)
;;; niwa-acp-compaction.el ends here
