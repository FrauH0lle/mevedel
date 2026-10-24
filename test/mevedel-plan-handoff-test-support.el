;;; mevedel-plan-handoff-test-support.el --- Published handoff fixture -*- lexical-binding: t -*-

;;; Commentary:

;; Shared published-session fixture for handoff ownership tests.
;; Loading this support file defines no test cases.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-engine-test-support"))
(require 'mevedel-plan-handoff)
(require 'mevedel-bash-policy)
(require 'mevedel-permissions)
(require 'mevedel-view-composer)
(require 'mevedel-journal-capture)
(require 'gptel-openai)

(defmacro mevedel-handoff-owner-test--with-target (&rest body)
  "Run BODY with a published session holding a Here Goal handoff.
A Worktree handoff runs the same Here handoff in its fork."
  (declare (indent 0) (debug (body)))
  `(mevedel-engine-test--with-session
     (mevedel-request-end)
     (with-current-buffer buffer
       (mevedel-skills-test--with-model-backends
         (mevedel-model-set-session-provider
          session (mevedel-model-resolve-provider "Fast:fast-model") buffer)
         (mevedel-model-set-session-effort session 'high buffer)
         (mevedel-permission-mode-set-raw 'ask)
         (let* ((selection (list :location 'here :context 'current :execution 'goal
                                 :mode 'full-auto :model-provider "Balanced:balanced-model"
                                 :reasoning-effort nil :goal-token-budget 200))
                (policy (mevedel-implementation-policy-from-selection selection))
                (draft (mevedel-plan-write-current "# Accepted outcome\nRead and verify."
                                                   session buffer))
                (accepted (mevedel-plan-archive-accepted draft session))
                (record (list :step 'submit :goal-id "reserved-kickoff"
                              :selection selection :accepted accepted))
                (submission (mevedel-prompt-submission-create :session session))
                (mevedel-journal-enabled nil)
                (gptel-use-curl t) (gptel-use-tools nil) (gptel-use-context nil)
                (gptel-send--handlers
                 (mevedel-preset--build-handlers (copy-tree gptel-send--handlers)))
                (gptel-send--transitions
                 (mevedel-preset--build-transitions (copy-tree gptel-send--transitions)))
                (gptel-prompt-transform-functions
                 '(mevedel-skills--transform-apply-request-model-policy)))
           (mevedel-plan--metadata-put session :accepted-path (plist-get accepted :path))
           (mevedel-plan--metadata-put session :accepted-hash (plist-get accepted :hash))
           (mevedel-plan--metadata-put session :implementation-retry record)
           (mevedel-prompt-submission-accept submission "Implement accepted outcome" nil nil nil)
           ,@body)))))

(provide 'mevedel-plan-handoff-test-support)
;;; mevedel-plan-handoff-test-support.el ends here
