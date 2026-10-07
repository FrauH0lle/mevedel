;;; mevedel-claude-code-history.el --- Native history and recovery -*- lexical-binding: t -*-

;;; Commentary:
;; A native conversation's identity is durable before its prompt is sent; a
;; turn interrupted after that point is uncertain, never replayed.  Edited or
;; foreign history cannot resume.  Missing history can continue through
;; explicit, labelled transcript evidence.

;;; Code:

(require 'mevedel-claude-code-backend)
(require 'mevedel-engine)
(require 'mevedel-session-artifacts)
(require 'mevedel-transcript)
(require 'mevedel-compact-evidence)

(defun mevedel-claude-code-history--foreign-p (record)
  "Return non-nil when another machine or installation retained RECORD."
  (and (plist-get record :id)
       (not (and (eq 'claude-code (plist-get record :engine))
                 (equal (system-name) (plist-get record :host))
                 (equal (expand-file-name mevedel-claude-code-directory)
                        (plist-get record :directory))))))

(defun mevedel-claude-code-history-assert-current (record)
  "Reject native continuation of RECORD when it was edited or is foreign.
Displayed evidence edited after receipt, or a native identity retained by
another machine or installation directory, cannot be resumed."
  (when (eq 'diverged (plist-get record :state))
    (user-error "The transcript was edited after Claude received it; use M-x mevedel-claude-code-recover-history before continuing"))
  (when (mevedel-claude-code-history--foreign-p record)
    (user-error "This Claude history belongs to another installation; its transcript remains readable.  Use M-x mevedel-claude-code-recover-history to continue from it")))

(defun mevedel-claude-code-history-unresumable-p (record)
  "Return non-nil when RECORD's native history needs transcript recovery.
That is when it was refused as edited or foreign, or failed to start."
  (and (plist-get record :id)
       (or (eq 'diverged (plist-get record :state))
           (mevedel-claude-code-history--foreign-p record)
           (plist-get record :start-failed))))

(defun mevedel-claude-code-history-open (owner id &optional boundary)
  "Return OWNER's in-flight native record for conversation ID.
BOUNDARY is the submitted input's (SEGMENT . OFFSET), protecting it from
undetected edits.  The record is attached to OWNER's turn context."
  (let ((record (list :engine 'claude-code :id id :host (system-name)
                      :directory (expand-file-name mevedel-claude-code-directory)
                      :state 'in-flight)))
    (when boundary (plist-put record :input-boundary boundary))
    (setf (mevedel-engine-info owner)
          (plist-put (mevedel-engine-info owner) :mevedel-claude-history record))
    record))

(defun mevedel-claude-code-history-settle (owner history record outcome)
  "Record OWNER's native OUTCOME on RECORD, opened from previous HISTORY.
Without RECORD, a retained HISTORY failed to start; it is marked so recovery
can be offered, and its diagnosis names excerpt recovery.  Divergence
observed during the turn is preserved."
  (if (not record)
      (let ((info (mevedel-engine-info owner)))
        (when (and (plist-get history :id)
                   (eq 'error (plist-get info :mevedel-acp-outcome)))
          (plist-put history :start-failed t)
          (setf (mevedel-engine-info owner)
                (plist-put info :error
                           (concat (if (listp (plist-get info :error))
                                       (plist-get (plist-get info :error) :message)
                                     (plist-get info :error))
                                   "; if native history is unavailable, use M-x mevedel-claude-code-recover-history to continue from the retained transcript")))))
    (unless (eq 'diverged (plist-get record :state))
      (plist-put record :state
                 (if (eq 'success (plist-get outcome :status)) 'ready 'uncertain)))))

(defun mevedel-claude-code-history-excerpt (session &optional child end)
  "Return labelled evidence before the current prompt for SESSION.
CHILD selects its private transcript; root evidence excludes isolated turns.
END is the current prompt's known start.  Without it, the prompt begins after
the last response, so an unanswered earlier prompt would merge into it.
The effective compaction summary is retained once from its canonical bounds.
Historical calls are evidence, never executable work or a native resume."
  (let* ((end (or end (mevedel-transcript-prompt-transform-start)))
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

(provide 'mevedel-claude-code-history)
;;; mevedel-claude-code-history.el ends here
