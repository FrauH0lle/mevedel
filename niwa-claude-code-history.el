;;; niwa-claude-code-history.el --- Native history and recovery -*- lexical-binding: t -*-

;;; Commentary:
;; A native conversation's identity is durable before its prompt is sent; a
;; turn interrupted after that point is uncertain, never replayed.  Edited or
;; foreign history cannot resume.  Missing history can continue through
;; explicit, labelled transcript evidence.

;;; Code:

(require 'niwa-claude-code-backend)
(require 'niwa-engine)
(require 'niwa-session-artifacts)
(require 'niwa-transcript)
(require 'niwa-compact-evidence)

;; `niwa-agents'
(declare-function niwa-agent-invocation-parent-turn "niwa-agents" (invocation))

(defun niwa-claude-code-history--foreign-p (record)
  "Return non-nil when another machine or installation retained RECORD."
  (and (plist-get record :id)
       (not (and (eq 'claude-code (plist-get record :engine))
                 (equal (system-name) (plist-get record :host))
                 (equal (expand-file-name niwa-claude-code-directory)
                        (plist-get record :directory))))))

(defun niwa-claude-code-history-assert-current (record)
  "Reject native continuation of RECORD when it was edited or is foreign.
Displayed evidence edited after receipt, or a native identity retained by
another machine or installation directory, cannot be resumed."
  (when (eq 'diverged (plist-get record :state))
    (user-error "The transcript was edited after Claude received it; use M-x niwa-claude-code-recover-history before continuing"))
  (when (niwa-claude-code-history--foreign-p record)
    (user-error "This Claude history belongs to another installation; its transcript remains readable.  Use M-x niwa-claude-code-recover-history to continue from it")))

(defun niwa-claude-code-history-unresumable-p (record)
  "Return non-nil when RECORD's native history needs transcript recovery.
That is when it was refused as edited or foreign, or failed to start."
  (and (plist-get record :id)
       (or (eq 'diverged (plist-get record :state))
           (niwa-claude-code-history--foreign-p record)
           (plist-get record :start-failed))))

(defun niwa-claude-code-history-open (owner id &optional boundary prior-history)
  "Return OWNER's in-flight native record for conversation ID.
BOUNDARY is the submitted input's (SEGMENT . OFFSET), protecting it from
undetected edits.  Preserve PRIOR-HISTORY's exact native boundaries.
The record is attached to OWNER's turn context."
  (let ((record (list :engine 'claude-code :id id :host (system-name)
                      :directory (expand-file-name niwa-claude-code-directory)
                      :state 'in-flight)))
    (when boundary (plist-put record :input-boundary boundary))
    (when-let* ((boundaries (plist-get prior-history :boundaries)))
      (plist-put record :boundaries (copy-tree boundaries)))
    (setf (niwa-engine-info owner)
          (plist-put (niwa-engine-info owner) :niwa-claude-history record))
    record))

(defun niwa-claude-code-history-settle (owner history record outcome)
  "Record OWNER's native OUTCOME on RECORD, opened from previous HISTORY.
Without RECORD, a retained HISTORY failed to start; it is marked so recovery
can be offered, and its diagnosis names excerpt recovery.  Divergence
observed during the turn is preserved.  Successful answers retain the exact
native message boundary, replacing any checkpoint at the same root turn."
  (if (not record)
      (let ((info (niwa-engine-info owner)))
        (when (and (plist-get history :id)
                   (eq 'error (plist-get info :niwa-acp-outcome)))
          (plist-put history :start-failed t)
          (setf (niwa-engine-info owner)
                (plist-put info :error
                           (concat (if (listp (plist-get info :error))
                                       (plist-get (plist-get info :error) :message)
                                     (plist-get info :error))
                                   "; if native history is unavailable, use M-x niwa-claude-code-recover-history to continue from the retained transcript")))))
    (unless (eq 'diverged (plist-get record :state))
      (plist-put record :state
                 (if (eq 'success (plist-get outcome :status)) 'ready 'uncertain)))
    (when-let* (((eq 'success (plist-get outcome :status)))
                (id (plist-get (niwa-engine-info owner) :niwa-native-message-id)))
      (let ((turn (if (niwa-request-p owner) (niwa-request-turn owner)
                    (niwa-agent-invocation-parent-turn owner))))
        (plist-put record :boundaries
                   (append (cl-remove turn (plist-get record :boundaries)
                                      :key (lambda (entry) (plist-get entry :turn)))
                           (list (list :turn turn :id (plist-get record :id)
                                       :host (plist-get record :host)
                                       :directory (plist-get record :directory)
                                       :message-id id
                                       :input-boundary (copy-tree (plist-get record :input-boundary))))))))))

(defun niwa-claude-code-history-excerpt (session &optional child end)
  "Return labelled evidence before the current prompt for SESSION.
CHILD selects its private transcript; root evidence excludes isolated turns.
END is the current prompt's known start.  Without it, the prompt begins after
the last response, so an unanswered earlier prompt would merge into it.
The effective compaction summary is retained once from its canonical bounds.
Historical calls are evidence, never executable work or a native resume."
  (let* ((end (or end (niwa-transcript-prompt-transform-start)))
         (bounds (if child (niwa-compact-evidence-agent-summary-bounds)
                   (niwa-session-artifacts-segment-summary-bounds)))
         (bounds (and bounds (<= (plist-get bounds :end) end) bounds))
         (ranges (if bounds
                     (list (cons (point-min) (plist-get bounds :begin))
                           (cons (plist-get bounds :end) end))
                   (list (cons (point-min) end))))
         ;; Views omit the leading root summary from ordinary segmentation.
         ;; Read the authoritative body separately, including edits that inherit
         ;; the closing wrapper's sticky ignore property, and include it once.
         (summary (when bounds
                    (niwa-session-artifacts-strip-summary-handoff-prefix
                     (string-trim
                      (niwa--strip-hook-audit-blocks
                       (buffer-substring-no-properties
                        (plist-get bounds :body-begin)
                        (plist-get bounds :body-end)))))))
         (evidence
          (string-join
           (delq nil
                 (list (when (and summary (not (string-blank-p summary)))
                         (niwa-transcript--summary-evidence-item "compaction-summary" summary))
                       (niwa-transcript-project-evidence
                        (if child ranges
                          (niwa-compact-evidence--regions-without-isolated-turns ranges))
                        :tool-results-dir (niwa-session-tool-results-directory session))))
           "\n\n")))
    (unless (string-blank-p evidence)
      (concat "Excerpt continuation from niwa's current transcript segment. "
              "This starts a new Claude conversation, not an exact native resume. "
              "Historical tool calls/results are evidence of earlier work; do not replay their effects. "
              "Tool output may be shortened with explicit omission markers; earlier segments remain in session history.\n\n"
              evidence "\n\n--- End historical excerpt; current request follows ---\n\n"))))

(provide 'niwa-claude-code-history)
;;; niwa-claude-code-history.el ends here
