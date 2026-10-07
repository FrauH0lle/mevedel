;;; mevedel-claude-code-usage.el --- Native sample usage -*- lexical-binding: t -*-

;;; Commentary:
;; SDK message usage is cumulative within one sample, not across a turn.
;; Keep message identities so streamed and consolidated duplicates do not add
;; cost twice.  The adapter's final prompt total remains terminal authority.

;;; Code:

(require 'mevedel-engine)

(defun mevedel-claude-code-usage--add-base (target base tokens)
  "Return TARGET with each TOKENS counter added to its BASE counter."
  (cl-loop for (key value) on tokens by #'cddr
           do (setq target (plist-put target key (+ value (or (plist-get base key) 0)))))
  target)

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
        (when (and (stringp id) (not (string-empty-p id))
                   (not (member id (plist-get info :mevedel-claude-finished-samples))))
          (let ((row (alist-get id rows nil nil #'equal)))
            (dolist (key '(input_tokens output_tokens cache_creation_input_tokens cache_read_input_tokens))
              (when-let* ((value (alist-get key usage)) ((natnump value)))
                (setf (alist-get key row) (max value (or (alist-get key row) 0)))))
            (setf (alist-get id rows nil nil #'equal) row))
          (let (tokens complete)
            (dolist (mapping '((:input input_tokens cache_creation_input_tokens)
                               (:output output_tokens) (:cached cache_read_input_tokens)
                               (:cache cache_creation_input_tokens)))
              (let ((known (cl-remove-if-not
                            (lambda (row)
                              (cl-some (lambda (key) (assq key (cdr row))) (cdr mapping)))
                            rows)))
                (when known
                  (let ((count (cl-loop for row in known sum
                                       (cl-loop for key in (cdr mapping)
                                                sum (or (alist-get key (cdr row)) 0)))))
                    (setq tokens (plist-put tokens (car mapping) count))
                    (when (cl-every (lambda (row)
                                      (cl-every (lambda (key) (assq key (cdr row))) (cdr mapping)))
                                    rows)
                      (setq complete (plist-put complete (car mapping) count)))))))
            (setq info (plist-put info :mevedel-claude-prompt-usage complete))
            (setq info (plist-put info :mevedel-usage-pending
                                  (not (and (natnump (plist-get complete :input))
                                            (natnump (plist-get complete :output))))))
            (when (plist-member info :mevedel-claude-usage-base)
              (let ((base (plist-get info :mevedel-claude-usage-base)))
                (setq tokens (mevedel-claude-code-usage--add-base (copy-sequence base) base tokens))))
            (setq info (plist-put info :tokens-full tokens))))
        (setf (mevedel-engine-info owner)
              (plist-put (plist-put info :mevedel-claude-sample-id current)
                         :mevedel-claude-sample-usage rows))))))

(defun mevedel-claude-code-usage-complete (owner outcome)
  "Merge native prompt totals from OUTCOME into OWNER's request usage.
Reported totals replace sample counters for this prompt.  Prior context
continuations contribute their frozen base exactly once.  Missing counters
retain a request-level incomplete marker even after later complete prompts."
  (let* ((info (mevedel-engine-info owner))
         (base (plist-get info :mevedel-claude-usage-base))
         (known (copy-sequence (plist-get info :tokens-full)))
         (tokens (plist-get outcome :tokens))
         (current (plist-get info :mevedel-claude-prompt-usage))
         (complete (cl-every (lambda (key)
                               (or (natnump (plist-get tokens key))
                                   (natnump (plist-get current key))))
                             '(:input :output))))
    (setq info (plist-put info :mevedel-usage-pending (not complete)))
    (unless complete
      (setq info (plist-put info :mevedel-usage-incomplete t)))
    (setq known (mevedel-claude-code-usage--add-base known base tokens))
    (setf (mevedel-engine-info owner) (plist-put info :tokens-full known))
    (plist-put outcome :tokens known)))

(provide 'mevedel-claude-code-usage)
;;; mevedel-claude-code-usage.el ends here
