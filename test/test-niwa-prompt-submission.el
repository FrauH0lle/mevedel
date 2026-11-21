;;; test-niwa-prompt-submission.el --- Prompt submission tests -*- lexical-binding: t -*-

;;; Commentary:

;; Accepted prompt context ownership tests.

;;; Code:

(require 'niwa-hooks)
(require 'niwa-prompt-submission)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-prompt-submission-commit ()
  ,test
  (test)
  :doc "consumes pending context once and commits reserved context in place"
  (let* ((entries '((:event SessionStart :body "context")))
         (session (niwa-session--create :hook-context-pending entries))
         (pending
          (niwa-prompt-submission-create
           :session session :context-entries entries)))
    (should (niwa-prompt-submission-commit pending))
    (should-not (niwa-session-hook-context-pending session))
    (should (niwa-prompt-submission-commit pending))
    (let ((reserved
           (niwa-prompt-submission-create :state 'reserved)))
      (should (niwa-prompt-submission-commit reserved))
      (should (eq 'committed
                  (niwa-prompt-submission-state reserved))))))

(niwa-deftest niwa-prompt-submission-accept ()
  ,test
  (test)
  :doc "records accepted hook data only while the submission is pending"
  (let ((submission (niwa-prompt-submission-create)))
    (should
     (eq submission
         (niwa-prompt-submission-accept
          submission "input" "context" '((:type rewrite)) '((:event hook)))))
    (should (equal "input" (niwa-prompt-submission-input submission)))
    (should (equal "context" (niwa-prompt-submission-context submission)))
    (niwa-prompt-submission-cancel submission)
    (should-error
     (niwa-prompt-submission-accept submission nil nil nil nil))))

(niwa-deftest niwa-prompt-submission-cancel ()
  ,test
  (test)
  :doc "cancels pending work idempotently but never cancels committed work"
  (let ((pending (niwa-prompt-submission-create))
        (committed (niwa-prompt-submission-create :state 'committed)))
    (should (niwa-prompt-submission-cancel pending))
    (should (niwa-prompt-submission-cancel pending))
    (should (eq 'cancelled (niwa-prompt-submission-state pending)))
    (should-not (niwa-prompt-submission-cancel committed))))

(niwa-deftest niwa-prompt-submission-reserve ()
  ,test
  (test)
  :doc "removes owned context from the session while queued"
  (let* ((entries '((:event SessionStart :body "context")))
         (session (niwa-session--create :hook-context-pending entries))
         (submission
          (niwa-prompt-submission-create
           :session session :context-entries entries)))
    (should (niwa-prompt-submission-reserve submission))
    (should-not (niwa-session-hook-context-pending session))
    (should (eq 'reserved (niwa-prompt-submission-state submission)))))

(niwa-deftest niwa-prompt-submission-restore ()
  ,test
  (test)
  :doc "returns reserved context before newer pending entries"
  (let* ((owned '((:event SessionStart :body "owned")))
         (newer '((:event UserPromptSubmit :body "newer")))
         (session (niwa-session--create :hook-context-pending newer))
         (submission
          (niwa-prompt-submission-create
           :session session :context-entries owned :state 'reserved)))
    (niwa-prompt-submission-restore submission)
    (should (equal (append owned newer)
                   (niwa-session-hook-context-pending session)))
    (should (eq 'pending (niwa-prompt-submission-state submission)))))

(niwa-deftest niwa-prompt-submission-set-outcome ()
  ,test
  (test)
  :doc "attaches one prepared outcome and its audit records"
  (let* ((submission (niwa-prompt-submission-create))
         (outcome '(:model-input "prompt" :hook-audits ((:type rewrite)))))
    (should (eq submission
                (niwa-prompt-submission-set-outcome submission outcome)))
    (should (eq outcome (niwa-prompt-submission-outcome submission)))
    (should (equal '((:type rewrite))
                   (niwa-prompt-submission-audits submission)))))

(provide 'test-niwa-prompt-submission)
;;; test-niwa-prompt-submission.el ends here
