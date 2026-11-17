;;; mevedel-claude-code-fork.el --- Native answer-boundary forks -*- lexical-binding: t -*-

;;; Commentary:
;; Prepare independent native histories before changing a published session.
;; Source histories stay intact for Redo.  Callers own the publication CAS and
;; discard only newly created forks when that transaction does not commit.

;;; Code:

(require 'mevedel-acp)
(require 'mevedel-claude-code)
(require 'mevedel-claude-code-history)
(require 'mevedel-utilities)

(declare-function mevedel-pending-inputs-schedule-held-input-delivery
                  "mevedel-pending-inputs" (&optional data-buffer))
(autoload 'mevedel-pending-inputs-schedule-held-input-delivery "mevedel-pending-inputs")
(declare-function mevedel-agent-control--schedule-agent-wakes
                  "mevedel-agent-control" (session))
(autoload 'mevedel-agent-control--schedule-agent-wakes "mevedel-agent-control")

(defvar mevedel-claude-code-fork--sessions nil
  "Sessions whose native history preparation currently excludes new turns.")

(defun mevedel-claude-code-fork-histories (histories complete &optional session)
  "Fork each native record in HISTORIES at its last recorded answer.
COMPLETE receives the independent histories, an error string or nil, and an
idempotent cleanup function deleting only the new forks.  Invoke cleanup when
publication does not commit; source sessions are never deleted.  SESSION,
when supplied, excludes root and child admission until COMPLETE returns.
The operation is asynchronous and never sends a model prompt."
  (when (and session (memq session mevedel-claude-code-fork--sessions))
    (user-error "Native history preparation is already pending"))
  (let ((remaining (copy-tree histories)) result created finished cleaned
        (source-ids
         (cl-loop for (_ . record) in histories
                  append (cons (plist-get record :id)
                               (mapcar (lambda (entry) (plist-get entry :id))
                                       (plist-get record :boundaries)))))
        (scope (mevedel-diagnostic-scope (or session (current-buffer))))
        (buffer (current-buffer)))
    (when session (push session mevedel-claude-code-fork--sessions))
    (cl-labels
        ((cleanup ()
           (unless cleaned
             (setq cleaned t)
             (dolist (entry created)
               (condition-case err
                   (mevedel-acp-session-operation
                    (car entry) 'delete (cdr entry)
                    (lambda (outcome)
                      (unless (eq 'success (plist-get outcome :status))
                        (mevedel-diagnostic-warning
                         scope 'mevedel
                         (format "Could not delete unused native fork %s: %s"
                                 (cdr entry) (plist-get outcome :message))))))
                 (error (mevedel-diagnostic-warning
                         scope 'mevedel (error-message-string err)))))))
         (deliver (error)
           (unless finished
             (setq finished t)
             (when error (cleanup))
             (unwind-protect
                 (funcall complete (unless error (nreverse result)) error #'cleanup)
               (when session
                 (setq mevedel-claude-code-fork--sessions
                       (delq session mevedel-claude-code-fork--sessions))
                 (when (buffer-live-p buffer)
                   (mevedel-pending-inputs-schedule-held-input-delivery buffer))
                 (mevedel-agent-control--schedule-agent-wakes session)))))
         (next ()
           (condition-case err
               (if (null remaining)
                   (deliver nil)
                 (let* ((entry (pop remaining))
                        (record (cdr entry))
                        (answer (car (last (plist-get record :boundaries)))))
                   (if (not (plist-get record :id))
                       (progn (push entry result) (next))
                     (unless (and (plist-get answer :message-id) (plist-get answer :id))
                       (user-error "No recorded native answer boundary for %s; recover its Claude history before continuing"
                                   (car entry)))
                     (setq record (plist-put record :host (plist-get answer :host))
                           record (plist-put record :directory (plist-get answer :directory)))
                     (mevedel-claude-code-history-assert-current record)
                     (unless (buffer-live-p buffer)
                       (user-error "Native history owner closed during preparation"))
                     (let* ((launch (with-current-buffer buffer
                                      (mevedel-claude-code-launch
                                       "Prepare native history boundary" [] "sonnet" nil)))
                            (meta (copy-tree (plist-get launch :meta))))
                       (setf (alist-get 'jetbrains meta)
                             `((air . ((fork . ((version . 1)
                                              (messageId . ,(plist-get answer :message-id))))))))
                       (setq launch (plist-put launch :meta meta))
                       (mevedel-acp-session-operation
                        launch 'fork (plist-get answer :id)
                        (lambda (outcome)
                          (cond
                           ((not (eq 'success (plist-get outcome :status)))
                            (deliver (format "Cannot fork native history for %s: %s"
                                             (car entry) (plist-get outcome :message))))
                           ((or (member (plist-get outcome :session-id) source-ids)
                                (cl-find (plist-get outcome :session-id) created
                                         :key #'cdr :test #'equal))
                            (deliver (format "Native fork returned an existing session ID for %s" (car entry))))
                           (t
                            (push (cons launch (plist-get outcome :session-id)) created)
                            (setq record (plist-put record :id (plist-get outcome :session-id))
                                  record (plist-put record :state
                                                    (if (eq 'uncertain (plist-get record :state))
                                                        'uncertain 'ready))
                                  record (plist-put record :start-failed nil)
                                  record (plist-put record :input-boundary
                                                    (copy-tree (plist-get answer :input-boundary))))
                            (push (cons (car entry) record) result)
                            (next)))))))))
             ((error quit) (deliver (error-message-string err))))))
      (next))
    :pending))

(provide 'mevedel-claude-code-fork)
;;; mevedel-claude-code-fork.el ends here
