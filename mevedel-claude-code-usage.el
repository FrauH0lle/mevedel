;;; mevedel-claude-code-usage.el --- Native sample usage -*- lexical-binding: t -*-

;;; Commentary:
;; SDK message usage is cumulative within one sample, not across a turn.
;; Keep message identities so streamed and consolidated duplicates do not add
;; cost twice.  The adapter's final prompt total remains terminal authority.

;;; Code:

(require 'mevedel-engine)

(defun mevedel-claude-code-usage-observe (owner notification)
  "Record live top-level SDK sample usage from NOTIFICATION for OWNER.
Only nonnegative counters establish known usage.  Partial deltas retain prior
fields, and replayed or older snapshots cannot reduce a sample's counters.
Cache creation contributes to normalized input; cached reads stay separate."
  (when (equal "_claude/sdkMessage" (alist-get 'method notification))
    (let* ((message (alist-get 'message (alist-get 'params notification)))
           (type (alist-get 'type message))
           (event (alist-get 'event message))
           (kind (alist-get 'type event))
           (info (mevedel-engine-info owner))
           (rows (plist-get info :mevedel-claude-sample-usage))
           (current (plist-get info :mevedel-claude-sample-id))
           sample id usage)
      (unless (alist-get 'parent_tool_use_id message)
        (cond
         ((and (equal type "stream_event") (equal kind "message_start"))
          (setq sample (alist-get 'message event)
                current (unless (equal "<synthetic>" (alist-get 'model sample))
                          (alist-get 'id sample))
                id current usage (alist-get 'usage sample)))
         ((and (equal type "stream_event") (equal kind "message_delta"))
          (setq id current usage (alist-get 'usage event)))
         ((and (equal type "stream_event") (equal kind "message_stop"))
          (setq current nil))
         ((equal type "assistant")
          (setq sample (alist-get 'message message))
          (unless (equal "<synthetic>" (alist-get 'model sample))
            (setq id (alist-get 'id sample) usage (alist-get 'usage sample)))))
        (when (and (stringp id) (not (string-empty-p id)) usage
                   (not (member id (plist-get info :mevedel-claude-finished-samples))))
          (let ((row (alist-get id rows nil nil #'equal)))
            (dolist (key '(input_tokens output_tokens cache_creation_input_tokens cache_read_input_tokens))
              (when-let* ((value (alist-get key usage)) ((natnump value)))
                (setf (alist-get key row) (max value (or (alist-get key row) 0)))))
            (setf (alist-get id rows nil nil #'equal) row))
          (let (tokens)
            (dolist (mapping '((:input input_tokens cache_creation_input_tokens)
                               (:output output_tokens) (:cached cache_read_input_tokens)
                               (:cache cache_creation_input_tokens)))
              (when (cl-every (lambda (row)
                                (cl-every (lambda (key) (assq key (cdr row))) (cdr mapping)))
                              rows)
                (setq tokens
                      (plist-put tokens (car mapping)
                                 (cl-loop for row in rows sum
                                          (cl-loop for key in (cdr mapping)
                                                   sum (alist-get key (cdr row))))))))
            (when (plist-member info :mevedel-claude-usage-base)
              (let ((base (plist-get info :mevedel-claude-usage-base)) accumulated)
                (while tokens
                  (let ((key (pop tokens)) (value (pop tokens)))
                    (when (plist-member base key)
                      (setq accumulated (plist-put accumulated key (+ value (plist-get base key)))))))
                (setq tokens accumulated)))
            (setq info (plist-put info :tokens-full tokens))))
        (setf (mevedel-engine-info owner)
              (plist-put (plist-put info :mevedel-claude-sample-id current)
                         :mevedel-claude-sample-usage rows))))))

(defun mevedel-claude-code-usage-complete (owner outcome)
  "Merge native prompt totals from OUTCOME into OWNER's request usage.
Reported totals replace sample counters for this prompt.  Prior context
continuations contribute their frozen base exactly once."
  (let* ((info (mevedel-engine-info owner))
         (base (plist-get info :mevedel-claude-usage-base))
         (known (copy-sequence (plist-get info :tokens-full)))
         (tokens (plist-get outcome :tokens)))
    (while tokens
      (let ((key (pop tokens)) (value (pop tokens)))
        (if (or (not (plist-member info :mevedel-claude-usage-base)) (plist-member base key))
            (setq known (plist-put known key (+ value (or (plist-get base key) 0))))
          (cl-remf known key))))
    (setf (mevedel-engine-info owner) (plist-put info :tokens-full known))
    (plist-put outcome :tokens known)))

(provide 'mevedel-claude-code-usage)
;;; mevedel-claude-code-usage.el ends here
