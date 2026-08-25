;;; mevedel-prompt-submission.el --- Accepted prompt transactions -*- lexical-binding: t -*-

;;; Commentary:

;; Owns accepted user prompts and their pending lifecycle context until a
;; transcript or retained-message boundary commits them.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'mevedel-structs)

;; `mevedel-hooks'
(declare-function mevedel-hooks-consume-session-context
                  "mevedel-hooks" (session entries))
(autoload 'mevedel-hooks-consume-session-context "mevedel-hooks")

;; `mevedel-structs'
(declare-function mevedel-session-hook-context-pending
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-set-hook-context-pending
                  "mevedel-structs" (session entries))

(cl-defstruct
    (mevedel-prompt-submission
     (:constructor mevedel-prompt-submission-create))
  input
  display-text
  context
  audits
  session
  context-entries
  outcome
  (state 'pending))

(defun mevedel-prompt-submission-accept
    (submission input context audits context-entries)
  "Record accepted hook INPUT and its pending ownership on SUBMISSION."
  (unless (eq (mevedel-prompt-submission-state submission) 'pending)
    (error "Cannot accept inactive prompt submission"))
  (setf (mevedel-prompt-submission-input submission) input
        (mevedel-prompt-submission-context submission) context
        (mevedel-prompt-submission-audits submission) audits
        (mevedel-prompt-submission-context-entries submission) context-entries)
  submission)

(defun mevedel-prompt-submission-cancel (submission)
  "Cancel pending SUBMISSION and return non-nil when it is inactive."
  (pcase (mevedel-prompt-submission-state submission)
    ('cancelled t)
    ('pending
     (setf (mevedel-prompt-submission-state submission) 'cancelled)
     t)
    (_ nil)))

(defun mevedel-prompt-submission-commit (submission)
  "Commit SUBMISSION's pending context exactly once."
  (pcase (mevedel-prompt-submission-state submission)
    ('committed t)
    ('reserved
     (setf (mevedel-prompt-submission-state submission) 'committed)
     t)
    ('pending
     (let ((entries (mevedel-prompt-submission-context-entries submission)))
       (when (and entries
                  (not (mevedel-hooks-consume-session-context
                        (mevedel-prompt-submission-session submission)
                        entries)))
         (error "Prompt context ownership changed before commit"))
       (setf (mevedel-prompt-submission-state submission) 'committed)
       t))
    (_ (error "Invalid prompt submission state"))))

(defun mevedel-prompt-submission-reserve (submission)
  "Transfer SUBMISSION's pending context into the submission itself."
  (pcase (mevedel-prompt-submission-state submission)
    ('reserved t)
    ('pending
     (let ((entries (mevedel-prompt-submission-context-entries submission)))
       (when (and entries
                  (not (mevedel-hooks-consume-session-context
                        (mevedel-prompt-submission-session submission)
                        entries)))
         (error "Prompt context ownership changed before reservation"))
       (setf (mevedel-prompt-submission-state submission) 'reserved)
       t))
    (_ (error "Cannot reserve committed prompt submission"))))

(defun mevedel-prompt-submission-set-outcome (submission outcome)
  "Attach prepared OUTCOME and its audits to SUBMISSION."
  (setf (mevedel-prompt-submission-outcome submission) outcome
        (mevedel-prompt-submission-audits submission)
        (plist-get outcome :hook-audits))
  submission)

(defun mevedel-prompt-submission-restore (submission)
  "Restore a reserved SUBMISSION's context to its session."
  (when (eq (mevedel-prompt-submission-state submission) 'reserved)
    (let ((session (mevedel-prompt-submission-session submission)))
      (mevedel-session-set-hook-context-pending
       session
       (append
        (mevedel-prompt-submission-context-entries submission)
        (mevedel-session-hook-context-pending session)))
      (setf (mevedel-prompt-submission-state submission) 'pending))))

(provide 'mevedel-prompt-submission)
;;; mevedel-prompt-submission.el ends here
