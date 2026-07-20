;;; test-mevedel-prompt-submission.el --- Prompt submission tests -*- lexical-binding: t -*-

;;; Commentary:

;; Accepted prompt context ownership tests.

;;; Code:

(require 'mevedel-hooks)
(require 'mevedel-prompt-submission)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-prompt-submission-commit ()
  ,test
  (test)
  :doc "consumes pending context once and commits reserved context in place"
  (let* ((entries '((:event SessionStart :body "context")))
         (session (mevedel-session--create :hook-context-pending entries))
         (pending
          (mevedel-prompt-submission-create
           :session session :context-entries entries)))
    (should (mevedel-prompt-submission-commit pending))
    (should-not (mevedel-session-hook-context-pending session))
    (should (mevedel-prompt-submission-commit pending))
    (let ((reserved
           (mevedel-prompt-submission-create :state 'reserved)))
      (should (mevedel-prompt-submission-commit reserved))
      (should (eq 'committed
                  (mevedel-prompt-submission-state reserved))))))

(mevedel-deftest mevedel-prompt-submission-reserve ()
  ,test
  (test)
  :doc "removes owned context from the session while queued"
  (let* ((entries '((:event SessionStart :body "context")))
         (session (mevedel-session--create :hook-context-pending entries))
         (submission
          (mevedel-prompt-submission-create
           :session session :context-entries entries)))
    (should (mevedel-prompt-submission-reserve submission))
    (should-not (mevedel-session-hook-context-pending session))
    (should (eq 'reserved (mevedel-prompt-submission-state submission)))))

(mevedel-deftest mevedel-prompt-submission-restore ()
  ,test
  (test)
  :doc "returns reserved context before newer pending entries"
  (let* ((owned '((:event SessionStart :body "owned")))
         (newer '((:event UserPromptSubmit :body "newer")))
         (session (mevedel-session--create :hook-context-pending newer))
         (submission
          (mevedel-prompt-submission-create
           :session session :context-entries owned :state 'reserved)))
    (mevedel-prompt-submission-restore submission)
    (should (equal (append owned newer)
                   (mevedel-session-hook-context-pending session)))
    (should (eq 'pending (mevedel-prompt-submission-state submission)))))

(mevedel-deftest mevedel-prompt-submission-set-outcome ()
  ,test
  (test)
  :doc "attaches one prepared outcome and its audit records"
  (let* ((submission (mevedel-prompt-submission-create))
         (outcome '(:model-input "prompt" :hook-audits ((:type rewrite)))))
    (should (eq submission
                (mevedel-prompt-submission-set-outcome submission outcome)))
    (should (eq outcome (mevedel-prompt-submission-outcome submission)))
    (should (equal '((:type rewrite))
                   (mevedel-prompt-submission-audits submission)))))

(provide 'test-mevedel-prompt-submission)
;;; test-mevedel-prompt-submission.el ends here
