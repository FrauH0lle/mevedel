;;; niwa-prompt-submission.el --- Accepted prompt transactions -*- lexical-binding: t -*-

;;; Commentary:

;; Owns accepted user prompts and their pending lifecycle context until a
;; transcript or retained-message boundary commits them.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'niwa-structs)

;; `niwa-hooks'
(declare-function niwa-hooks-consume-session-context
                  "niwa-hooks" (session entries))
(autoload 'niwa-hooks-consume-session-context "niwa-hooks")

;; `niwa-structs'
(declare-function niwa-session-hook-context-pending
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-set-hook-context-pending
                  "niwa-structs" (session entries))

(cl-defstruct
    (niwa-prompt-submission
     (:constructor niwa-prompt-submission-create))
  input
  display-text
  context
  audits
  session
  context-entries
  outcome
  (state 'pending))

(defun niwa-prompt-submission-accept
    (submission input context audits context-entries)
  "Record accepted hook INPUT and its pending ownership on SUBMISSION."
  (unless (eq (niwa-prompt-submission-state submission) 'pending)
    (error "Cannot accept inactive prompt submission"))
  (setf (niwa-prompt-submission-input submission) input
        (niwa-prompt-submission-context submission) context
        (niwa-prompt-submission-audits submission) audits
        (niwa-prompt-submission-context-entries submission) context-entries)
  submission)

(defun niwa-prompt-submission-cancel (submission)
  "Cancel pending SUBMISSION and return non-nil when it is inactive."
  (pcase (niwa-prompt-submission-state submission)
    ('cancelled t)
    ('pending
     (setf (niwa-prompt-submission-state submission) 'cancelled)
     t)
    (_ nil)))

(defun niwa-prompt-submission-commit (submission)
  "Commit SUBMISSION's pending context exactly once."
  (pcase (niwa-prompt-submission-state submission)
    ('committed t)
    ('reserved
     (setf (niwa-prompt-submission-state submission) 'committed)
     t)
    ('pending
     (let ((entries (niwa-prompt-submission-context-entries submission)))
       (when (and entries
                  (not (niwa-hooks-consume-session-context
                        (niwa-prompt-submission-session submission)
                        entries)))
         (error "Prompt context ownership changed before commit"))
       (setf (niwa-prompt-submission-state submission) 'committed)
       t))
    (_ (error "Invalid prompt submission state"))))

(defun niwa-prompt-submission-reserve (submission)
  "Transfer SUBMISSION's pending context into the submission itself."
  (pcase (niwa-prompt-submission-state submission)
    ('reserved t)
    ('pending
     (let ((entries (niwa-prompt-submission-context-entries submission)))
       (when (and entries
                  (not (niwa-hooks-consume-session-context
                        (niwa-prompt-submission-session submission)
                        entries)))
         (error "Prompt context ownership changed before reservation"))
       (setf (niwa-prompt-submission-state submission) 'reserved)
       t))
    (_ (error "Cannot reserve committed prompt submission"))))

(defun niwa-prompt-submission-set-outcome (submission outcome)
  "Attach prepared OUTCOME and its audits to SUBMISSION."
  (setf (niwa-prompt-submission-outcome submission) outcome
        (niwa-prompt-submission-audits submission)
        (plist-get outcome :hook-audits))
  submission)

(defun niwa-prompt-submission-restore (submission)
  "Restore a reserved SUBMISSION's context to its session."
  (when (eq (niwa-prompt-submission-state submission) 'reserved)
    (let ((session (niwa-prompt-submission-session submission)))
      (niwa-session-set-hook-context-pending
       session
       (append
        (niwa-prompt-submission-context-entries submission)
        (niwa-session-hook-context-pending session)))
      (setf (niwa-prompt-submission-state submission) 'pending))))

(provide 'niwa-prompt-submission)
;;; niwa-prompt-submission.el ends here
