;;; niwa-plan-handoff-test-support.el --- Published handoff fixture -*- lexical-binding: t -*-

;;; Commentary:

;; Shared published-session fixture for handoff ownership tests.
;; Loading this support file defines no test cases.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-engine-test-support"))
(require 'niwa-plan-handoff)
(require 'niwa-bash-policy)
(require 'niwa-permissions)
(require 'niwa-view-composer)
(require 'niwa-journal-capture)
(require 'gptel-openai)

(defmacro niwa-handoff-owner-test--with-target (&rest body)
  "Run BODY with a durable session holding a Here Goal handoff.
A Worktree handoff runs the same Here handoff in its fork.
An initial :file-workspace FORM selects file storage for policy-only cases;
publication callbacks and durable owner races must use project storage."
  (declare (indent 0) (debug (body)))
  (let ((file-workspace (when (eq (car body) :file-workspace)
                         (pop body) (pop body))))
    `(niwa-engine-test--with-session :file-workspace ,file-workspace
       (niwa-request-end)
       (with-current-buffer buffer
         (niwa-skills-test--with-model-backends
           (niwa-model-set-session-provider
            session (niwa-model-resolve-provider "Fast:fast-model") buffer)
           (niwa-model-set-session-effort session 'high buffer)
           (niwa-permission-mode-set-raw 'ask)
           (let* ((selection (list :location 'here :context 'current :execution 'goal
                                   :mode 'full-auto :model-provider "Balanced:balanced-model"
                                   :reasoning-effort nil :goal-token-budget 200))
                  (policy (niwa-implementation-policy-from-selection selection))
                  (draft (niwa-plan-write-current "# Accepted outcome\nRead and verify."
                                                     session buffer))
                  (accepted (niwa-plan-archive-accepted draft session))
                  (record (list :step 'submit :goal-id "reserved-kickoff"
                                :selection selection :accepted accepted))
                  (submission (niwa-prompt-submission-create :session session))
                  (niwa-journal-enabled nil)
                  (gptel-use-curl t) (gptel-use-tools nil) (gptel-use-context nil)
                  (gptel-send--handlers
                   (niwa-preset--build-handlers (copy-tree gptel-send--handlers)))
                  (gptel-send--transitions
                   (niwa-preset--build-transitions (copy-tree gptel-send--transitions)))
                  (gptel-prompt-transform-functions
                   '(niwa-skills--transform-apply-request-model-policy)))
             (niwa-plan--metadata-put session :accepted-path (plist-get accepted :path))
             (niwa-plan--metadata-put session :accepted-hash (plist-get accepted :hash))
             (niwa-plan--metadata-put session :implementation-retry record)
             (niwa-prompt-submission-accept submission "Implement accepted outcome" nil nil nil)
             ,@body))))))

(provide 'niwa-plan-handoff-test-support)
;;; niwa-plan-handoff-test-support.el ends here
