;;; mevedel-claude-code-history.el --- Native history and recovery -*- lexical-binding: t -*-

;;; Commentary:
;; A native conversation's admitted call identities survive process loss.  An
;; admission proves only that a call may have run, never that its effects or
;; result delivery completed.  Replayed identities cannot execute again.
;; Missing history can continue through explicit, labelled transcript evidence.

;;; Code:

(require 'mevedel-agents)
(require 'mevedel-engine)
(require 'mevedel-session-artifacts)
(require 'mevedel-transcript)
(require 'mevedel-compact-evidence)

(defun mevedel-claude-code-history-assert-current (record)
  "Reject native continuation when RECORD's displayed evidence was edited."
  (when (eq 'diverged (plist-get record :state))
    (user-error "The transcript was edited after Claude received it; use M-x mevedel-claude-code-recover-history before continuing")))

(defun mevedel-claude-code-history-unavailable (owner)
  "Add an actionable recovery option to OWNER's failed startup diagnosis."
  (let ((info (mevedel-engine-info owner)))
    (when (eq 'error (plist-get info :mevedel-acp-outcome))
      (setf (mevedel-engine-info owner)
            (plist-put info :error
                       (concat (plist-get info :error)
                               "; if native history is unavailable, use M-x mevedel-claude-code-recover-history to continue from the retained transcript"))))))

(defun mevedel-claude-code-history-excerpt (session &optional child)
  "Return labelled evidence before the current prompt for SESSION.
CHILD selects its private transcript; root evidence excludes isolated turns.
The effective compaction summary is retained once from its canonical bounds.
Historical calls are evidence, never executable work or a native resume."
  (let* ((end (mevedel-transcript-prompt-transform-start))
         (bounds (if child (mevedel-compact-evidence-agent-summary-bounds)
                   (mevedel-session-artifacts-segment-summary-bounds)))
         (bounds (and bounds (<= (plist-get bounds :end) end) bounds))
         (ranges (if bounds
                     (list (cons (point-min) (plist-get bounds :begin))
                           (cons (plist-get bounds :end) end))
                   (list (cons (point-min) end))))
         ;; Views omit the leading root summary from ordinary segmentation.
         ;; Read the authoritative body separately, including edits that inherit
         ;; the closing wrapper's sticky ignore property, and include it once.
         (summary (when bounds
                    (mevedel-session-artifacts-strip-summary-handoff-prefix
                     (string-trim
                      (mevedel--strip-hook-audit-blocks
                       (buffer-substring-no-properties
                        (plist-get bounds :body-begin)
                        (plist-get bounds :body-end)))))))
         (evidence
          (string-join
           (delq nil
                 (list (when (and summary (not (string-blank-p summary)))
                         (mevedel-transcript--summary-evidence-item "compaction-summary" summary))
                       (mevedel-transcript-project-evidence
                        (if child ranges
                          (mevedel-compact-evidence--regions-without-isolated-turns ranges))
                        :tool-results-dir (mevedel-session-tool-results-directory session))))
           "\n\n")))
    (unless (string-blank-p evidence)
      (concat "Excerpt continuation from mevedel's current transcript segment. "
              "This starts a new Claude conversation, not an exact native resume. "
              "Historical tool calls/results are evidence of earlier work; do not replay their effects. "
              "Tool output may be shortened with explicit omission markers; earlier segments remain in session history.\n\n"
              evidence "\n\n--- End historical excerpt; current request follows ---\n\n"))))

(defun mevedel-claude-code-history-admit (owner name id)
  "Persist OWNER's native call ID and NAME before entering the tool pipeline.
Reject identities admitted in any earlier turn of this native conversation.
Publication failure prevents effects; uncertain admission is never retried."
  (let* ((record (plist-get (mevedel-engine-info owner) :mevedel-claude-history))
         (session (if (mevedel-request-p owner) (mevedel-request-session owner)
                    (mevedel-agent-invocation-parent-session owner)))
         (calls (plist-get record :tool-calls)))
    (unless (and record (eq 'in-flight (plist-get record :state)))
      (error "Native tool call has no published conversation owner"))
    (when (assoc id calls)
      (error "Native tool call was already admitted; inspect current effects before continuing"))
    (plist-put record :tool-calls (cons (cons id name) calls))
    (mevedel-session-artifacts-publish-sidecar-state
     session (mevedel-session-root-buffer session))))

(provide 'mevedel-claude-code-history)
;;; mevedel-claude-code-history.el ends here
