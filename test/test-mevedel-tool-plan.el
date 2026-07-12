;;; test-mevedel-tool-plan.el --- Tests for mevedel-tool-plan.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-tool-registry)
(require 'mevedel-chat)
(require 'mevedel-tool-plan)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'mevedel-mentions)
(require 'mevedel-reminders)
(require 'mevedel-session-persistence)
(require 'mevedel-skills-ui)
(require 'mevedel-worktree)
(require 'gptel-request)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


(defun test-mevedel-tool-plan--raw-bytes (&rest bytes)
  "Return BYTES as an Emacs string of raw byte characters."
  (apply #'string (mapcar #'unibyte-char-to-multibyte bytes)))

(defun test-mevedel-tool-plan--raw-byte-string-p (string)
  "Return non-nil for STRING with raw byte characters."
  (catch 'found
    (dotimes (index (length string))
      (when (eq (char-charset (aref string index)) 'eight-bit)
        (throw 'found t)))
    nil))

(mevedel-deftest mevedel-plan-mode--prepare-worktree-outcome ()
  ,test
  (test)
  :doc "reuses a created worktree when a later preparation phase is retried"
  (with-temp-buffer
    (let* ((chat-buffer (current-buffer))
           (target-buffer (generate-new-buffer " *plan-prepare-target*"))
           (target-session (mevedel-session--create
                            :name "target" :workspace nil))
           (entry (list :body "# Plan"))
           (outcome '(:action implement-worktree :mode default))
           (create-count 0)
           (fail-write t)
           (worktree (list :buffer target-buffer
                           :branch "test"
                           :directory "/tmp/test")))
      (unwind-protect
          (progn
            (setq-local gptel-backend 'backend)
            (setq-local gptel-model 'model)
            (with-current-buffer target-buffer
              (setq-local mevedel--session target-session))
            (cl-letf (((symbol-function 'mevedel-worktree-create-session)
                       (lambda (&rest _)
                         (cl-incf create-count)
                         worktree))
                      ((symbol-function 'mevedel-plan-write-current)
                       (lambda (&rest _)
                         (if fail-write
                             (error "Write failed")
                           '(:absolute-path "/tmp/plan.md"))))
                      ((symbol-function 'mevedel-plan-archive-accepted)
                       (lambda (&rest _) '(:absolute-path "/tmp/accepted.md")))
                      ((symbol-function 'mevedel-plan-mark-approved)
                       #'ignore)
                      ((symbol-function 'mevedel-plan-mode--save-session-state)
                       #'ignore))
              (should-error
               (mevedel-plan-mode--prepare-worktree-outcome
                entry "# Plan" chat-buffer outcome))
              (setq fail-write nil)
              (let ((prepared
                     (mevedel-plan-mode--prepare-worktree-outcome
                      entry "# Plan" chat-buffer outcome)))
                (should (= 1 create-count))
                (should (eq worktree (plist-get prepared :worktree))))))
        (when (buffer-live-p target-buffer)
          (kill-buffer target-buffer))))))

;;
;;; Plan-mode presentation

(mevedel-deftest mevedel-plan-mode-present
  (:doc "persists the proposed plan and enqueues an approval prompt")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-mode-" t))
        (rendered nil))
    (unwind-protect
        (let* ((session (mevedel-session--create
                         :name "test"
                         :workspace nil
                         :save-path save-dir
                         :permission-rules nil
                         :permission-mode 'plan
                         :permission-queue nil
                         :plan-queue nil
                         :plan-metadata
                         '(:status approved
                           :verification-pending t
                           :approved-turn 2
                           :approved-at "old")
                         :turn-count 3))
               (mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-plan-queue--render-entry)
                     (lambda (entry)
                       (push (plist-get entry :body) rendered))))
            (mevedel-plan-mode-present "# Plan\n\nDo it."))
          (let ((path (file-name-concat save-dir "plans" "current.md")))
            (should (file-exists-p path))
            (with-temp-buffer
              (insert-file-contents path)
              (should (equal "# Plan\n\nDo it." (buffer-string)))))
          (should (equal '("# Plan\n\nDo it.") rendered))
          (should (= 1 (length (mevedel-session-plan-queue session))))
          (should (equal "plans/current.md"
                         (plist-get (mevedel-session-plan-metadata session)
                                    :path)))
          (should (eq 'presented
                      (plist-get (mevedel-session-plan-metadata session)
                                 :status)))
          (should-not (plist-get (mevedel-session-plan-metadata session)
                                 :verification-pending))
          (should-not (plist-get (mevedel-session-plan-metadata session)
                                 :approved-turn))
          (should (mevedel-plan-known-p
                   "# Plan\n\nDo it." session)))
      (delete-directory save-dir t)))

  :doc "normalizes raw UTF-8 bytes before persisting and queueing the plan"
  (let ((save-dir (make-temp-file "mevedel-plan-raw-" t))
        rendered)
    (unwind-protect
        (let* ((raw-quote
                (test-mevedel-tool-plan--raw-bytes
                 #xe2 #x80 #x9c ?x #xe2 #x80 #x9d))
               (raw-plan (concat "# Plan\n\nQuote: " raw-quote))
               (session (mevedel-session--create
                         :name "test"
                         :workspace nil
                         :save-path save-dir
                         :permission-rules nil
                         :permission-mode 'plan
                         :permission-queue nil
                         :plan-queue nil
                         :turn-count 3))
               (mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-plan-queue--render-entry)
                     (lambda (entry)
                       (push (plist-get entry :body) rendered))))
            (mevedel-plan-mode-present raw-plan))
          (let ((path (file-name-concat save-dir "plans" "current.md")))
            (with-temp-buffer
              (insert-file-contents path)
              (should (equal "# Plan\n\nQuote: “x”" (buffer-string)))))
          (should (equal '("# Plan\n\nQuote: “x”") rendered))
          (should-not
           (test-mevedel-tool-plan--raw-byte-string-p
            (mevedel-plan-current-body session))))
      (delete-directory save-dir t)))

  :doc "materializes the session before writing its plan"
  (let ((root (make-temp-file "mevedel-plan-workspace-" t))
        (rendered nil))
    (unwind-protect
        (let* ((workspace (mevedel-workspace--create
                           :type 'project
                           :id root
                           :root root
                           :name "workspace"))
               (session (mevedel-session--create
                         :name "test"
                         :workspace workspace
                         :save-path nil
                         :permission-rules nil
                         :permission-mode 'plan
                         :permission-queue nil
                         :plan-queue nil
                         :turn-count 3))
               (mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-plan-queue--render-entry)
                     (lambda (entry)
                       (push (plist-get entry :body) rendered))))
            (mevedel-plan-mode-present "# Workspace plan\n\nDo it."))
          (let* ((metadata (mevedel-session-plan-metadata session))
                 (path (plist-get metadata :absolute-path)))
            (should (mevedel-session-save-path session))
            (should (equal '("# Workspace plan\n\nDo it.") rendered))
            (should (file-exists-p path))
            (should (equal
                     (file-name-concat (mevedel-session-save-path session)
                                       "plans" "current.md")
                     path))
            (should (equal "# Workspace plan\n\nDo it."
                           (mevedel-plan-current-body session)))
            (should (mevedel-plan-current-exists-p session))))
      (delete-directory root t))))

(mevedel-deftest mevedel-plan-mode--post-response
  (:doc "renders an approval prompt after a proposed-plan response")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-post-" t))
        (data-buffer (generate-new-buffer " *mev-plan-data*"))
        (view-buffer (generate-new-buffer " *mev-plan-view*")))
    (unwind-protect
        (let ((session (mevedel-session--create
                        :name "test"
                        :workspace nil
                        :save-path save-dir
                        :permission-rules nil
                        :permission-mode 'plan
                        :permission-queue nil
                        :plan-queue nil
                        :turn-count 1)))
          (with-current-buffer data-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--view-buffer view-buffer)
            (insert "<proposed_plan>\n# Plan\n\nUse `raw` Markdown.\n\n```elisp\n(message \"hi\")\n```\n</proposed_plan>\n"))
          (with-current-buffer view-buffer
            (setq-local mevedel--data-buffer data-buffer)
            (setq-local mevedel--session nil)
            (setq-local mevedel-view--agent-transcript-p nil)
            (setq-local mevedel-view--interaction-marker
                        (copy-marker (point-min) t))
            (setq-local mevedel-view--input-marker
                        (copy-marker (point-min) nil))
            (setq-local mevedel-view--interaction-descriptors
                        (make-hash-table :test #'equal))
            (setq-local mevedel-view--interaction-overlays
                        (make-hash-table :test #'equal))
            (setq-local mevedel--prompt-overlays nil))
          (with-current-buffer data-buffer
            (mevedel-plan-mode--post-response (point-min) (point-max)))
          (with-current-buffer view-buffer
            (mevedel-view--interaction-rebuild))
          (should (= 1 (length (mevedel-session-plan-queue session))))
          (let ((plan "# Plan\n\nUse `raw` Markdown.\n\n```elisp\n(message \"hi\")\n```"))
            (should (equal plan
                           (plist-get (car (mevedel-session-plan-queue session))
                                      :body)))
            (with-temp-buffer
              (insert-file-contents
               (file-name-concat save-dir "plans" "current.md"))
              (should (equal plan (buffer-string)))))
          (with-current-buffer view-buffer
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should (string-match-p "# Plan" text))
              (should (string-match-p "`raw` Markdown" text))
              (should (string-match-p "```elisp" text))
              (should (string-match-p "Keys:" text)))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-mode--post-response@request-end
  (:doc "keeps the approval prompt after normal request cleanup")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-request-end-" t))
        (data-buffer (generate-new-buffer " *mev-plan-data*"))
        (view-buffer (generate-new-buffer " *mev-plan-view*")))
    (unwind-protect
        (let ((session (mevedel-session--create
                        :name "test"
                        :workspace nil
                        :save-path save-dir
                        :permission-rules nil
                        :permission-mode 'plan
                        :permission-queue nil
                        :plan-queue nil
                        :turn-count 1)))
          (with-current-buffer data-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--view-buffer view-buffer)
            (mevedel-request-begin session)
            (insert "<proposed_plan>\n# Plan\n\nDo it.\n</proposed_plan>\n"))
          (with-current-buffer view-buffer
            (setq-local mevedel--data-buffer data-buffer)
            (setq-local mevedel--session session)
            (setq-local mevedel-view--agent-transcript-p nil)
            (setq-local mevedel-view--interaction-marker
                        (copy-marker (point-min) t))
            (setq-local mevedel-view--input-marker
                        (copy-marker (point-min) nil))
            (setq-local mevedel-view--interaction-descriptors
                        (make-hash-table :test #'equal))
            (setq-local mevedel-view--interaction-overlays
                        (make-hash-table :test #'equal))
            (setq-local mevedel--prompt-overlays nil))
          (with-current-buffer data-buffer
            (mevedel-plan-mode--post-response (point-min) (point-max))
            (mevedel-request-end))
          (should (= 1 (length (mevedel-session-plan-queue session))))
          (with-current-buffer view-buffer
            (mevedel-view--interaction-rebuild)
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should (string-match-p "# Plan" text))
              (should (string-match-p "Keys:" text)))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
	      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
	      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-mode--approval-callback
  (:doc "persists approved plan metadata before implementation starts")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-approve-" t))
        (data-buffer (generate-new-buffer " *mev-plan-data*"))
        saved-metadata
        saved-status
        saved-mode
        implemented)
    (unwind-protect
        (let ((session (mevedel-session--create
                        :name "test"
                        :workspace nil
                        :save-path save-dir
                        :permission-rules nil
                        :permission-mode 'plan
                        :permission-queue nil
                        :plan-queue nil
                        :plan-metadata
                        '(:path "plans/current.md" :status presented)
                        :turn-count 1)))
          (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                     (lambda (session _buffer)
                       (setq saved-metadata
                             (copy-sequence
                              (mevedel-session-plan-metadata session)))
                       (setq saved-status (plist-get saved-metadata :status))
                       (setq saved-mode
                             (mevedel-session-permission-mode session))))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (&rest _)
                       (setq implemented t))))
            (with-current-buffer data-buffer
              (setq-local mevedel--session session)
              (mevedel-plan-mode--approval-callback
               "# Plan\n\nDo it." data-buffer 'implement))
            (should (eq saved-status 'approved))
            (should (eq saved-mode 'default))
            (should implemented)
            (let* ((current-path (file-name-concat save-dir "plans" "current.md"))
                   (accepted-path (plist-get saved-metadata :accepted-path))
                   (accepted-absolute-path
                    (plist-get saved-metadata :accepted-absolute-path)))
              (should (equal "plans/current.md"
                             (plist-get saved-metadata :path)))
              (should (equal current-path
                             (plist-get saved-metadata :absolute-path)))
              (should (file-exists-p current-path))
              (with-temp-buffer
                (insert-file-contents current-path)
                (should (equal "# Plan\n\nDo it." (buffer-string))))
              (should (string-match-p
                       "\\`plans/accepted-[0-9]\\{8\\}-[0-9]\\{6\\}\\.md\\'"
                       accepted-path))
              (should (equal (file-name-concat save-dir accepted-path)
                             accepted-absolute-path))
              (should (file-exists-p accepted-absolute-path))
              (with-temp-buffer
                (insert-file-contents accepted-absolute-path)
                (should (equal "# Plan\n\nDo it." (buffer-string)))))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t)))

  :doc "persists rejected plan metadata before inserting feedback draft"
  (let ((save-dir (make-temp-file "mevedel-plan-feedback-" t))
        (data-buffer (generate-new-buffer " *mev-plan-data*"))
        saved-status
        saved-pending
        sent-prompt
        draft-buffer
        draft-path
        draft-feedback)
    (unwind-protect
        (let ((session (mevedel-session--create
                        :name "test"
                        :workspace nil
                        :save-path save-dir
                        :permission-rules nil
                        :permission-mode 'plan
                        :permission-queue nil
                        :plan-queue nil
                        :plan-metadata
                        '(:path "plans/current.md"
                          :status presented
                          :verification-pending t)
                        :turn-count 1)))
          (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                     (lambda (session _buffer)
                       (let ((metadata
                              (mevedel-session-plan-metadata session)))
                         (setq saved-status (plist-get metadata :status))
                         (setq saved-pending
                               (plist-get metadata :verification-pending)))))
                    ((symbol-function 'mevedel-plan-mode--insert-and-send)
                     (lambda (prompt &rest _)
                       (setq sent-prompt prompt)))
                    ((symbol-function 'mevedel-plan-mode--insert-feedback-draft)
                     (lambda (buffer path &optional feedback)
                       (setq draft-buffer buffer)
                       (setq draft-path path)
                       (setq draft-feedback feedback))))
            (with-current-buffer data-buffer
              (setq-local mevedel--session session)
              (mevedel-plan-mode--approval-callback
               "# Plan\n\nDo it." data-buffer 'feedback-draft))
            (should (eq saved-status 'rejected))
            (should-not saved-pending)
            (should-not sent-prompt)
            (should (eq draft-buffer data-buffer))
            (should (equal (file-name-concat save-dir "plans" "current.md")
                           draft-path))
            (should-not draft-feedback)
            (let ((plans-dir (file-name-concat save-dir "plans")))
              (should-not (and (file-directory-p plans-dir)
                               (directory-files plans-dir nil
                                                "\\`accepted-"))))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t)))

  :doc "passes selected implementation permission mode to implementation"
  (let ((save-dir (make-temp-file "mevedel-plan-mode-impl-" t))
        (data-buffer (generate-new-buffer " *mev-plan-data*"))
        implemented)
    (unwind-protect
        (let ((session (mevedel-session--create
                        :name "test"
                        :workspace nil
                        :save-path save-dir
                        :permission-rules nil
                        :permission-mode 'plan
                        :permission-queue nil
                        :plan-queue nil
                        :plan-metadata
                        '(:path "plans/current.md" :status presented)
                        :turn-count 1)))
          (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                     #'ignore)
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (action)
                       (setq implemented action))))
            (with-current-buffer data-buffer
              (setq-local mevedel--session session)
              (mevedel-plan-mode--approval-callback
               "# Plan\n\nDo it." data-buffer
               '(:action implement :mode accept-edits)))
            (should (eq 'implement (plist-get implemented :action)))
            (should (eq 'accept-edits
                        (plist-get implemented :permission-mode)))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t)))

  :doc "passes explicit default implementation permission mode to implementation"
  (let ((save-dir (make-temp-file "mevedel-plan-mode-default-" t))
        (data-buffer (generate-new-buffer " *mev-plan-data*"))
        implemented)
    (unwind-protect
        (let ((session (mevedel-session--create
                        :name "test"
                        :workspace nil
                        :save-path save-dir
                        :permission-rules nil
                        :permission-mode 'plan
                        :permission-queue nil
                        :plan-queue nil
                        :plan-metadata
                        '(:path "plans/current.md" :status presented)
                        :turn-count 1)))
          (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                     #'ignore)
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (action)
                       (setq implemented action))))
            (with-current-buffer data-buffer
              (setq-local mevedel--session session)
              (mevedel-plan-mode--approval-callback
               "# Plan\n\nDo it." data-buffer
               '(:action implement :mode default)))
            (should (eq 'implement (plist-get implemented :action)))
            (should (eq 'default
                        (plist-get implemented :permission-mode)))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t)))

  :doc "hands worktree implementation and verification to the target session"
  (let ((source-dir (make-temp-file "mevedel-plan-source-" t))
        (target-dir (make-temp-file "mevedel-plan-target-" t))
        (source-buffer (generate-new-buffer " *mev-plan-source*"))
        (target-buffer (generate-new-buffer " *mev-plan-target*"))
        saved-sessions
        implementation-buffer
        implementation)
    (unwind-protect
        (let* ((source-session
                (mevedel-session--create
                 :name "main"
                 :workspace nil
                 :save-path source-dir
                 :permission-mode 'plan
                 :permission-rules nil
                 :permission-queue nil
                 :plan-queue nil
                 :plan-metadata
                 '(:path "plans/current.md" :status presented)
                 :turn-count 2))
               (target-session
                (mevedel-session--create
                 :name ".worktrees:main"
                 :workspace nil
                 :save-path target-dir
                 :permission-mode 'default
                 :permission-rules nil
                 :permission-queue nil
                 :plan-queue nil
                 :turn-count 0))
               (worktree
                (list :buffer target-buffer
                      :branch "worktree/main"
                      :directory "/tmp/project/.worktrees/main/"))
               (outcome
                (list :action 'implement-worktree
                      :mode 'trust-all
                      :worktree worktree)))
          (dolist (pair `((,source-buffer . ,source-session)
                          (,target-buffer . ,target-session)))
            (with-current-buffer (car pair)
              (org-mode)
              (setq-local gptel-response-separator "\n\n")
              (setq-local gptel-prompt-prefix-alist
                          '((org-mode . "* User\n")))
              (setq-local mevedel--session (cdr pair))))
          (with-current-buffer target-buffer
            (let* ((target-artifact
                    (mevedel-plan-write-current
                     "# Plan\n\nDo it." target-session target-buffer))
                   (target-accepted-plan
                    (mevedel-plan-archive-accepted
                     target-artifact target-session)))
              (mevedel-plan-mark-approved
               target-session target-artifact target-accepted-plan)
              (setq worktree
                    (plist-put worktree :plan-file
                               (plist-get target-artifact :absolute-path)))
              (setq outcome (plist-put outcome :worktree worktree))))
          (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                     (lambda (session _buffer)
                       (push session saved-sessions)))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (action)
                       (setq implementation-buffer (current-buffer))
                       (setq implementation action))))
            (with-current-buffer source-buffer
              (mevedel-plan-mode--approval-callback
               "# Plan\n\nDo it." source-buffer outcome)))
          (let ((source-metadata
                 (mevedel-session-plan-metadata source-session))
                (target-metadata
                 (mevedel-session-plan-metadata target-session)))
            (should (eq 'approved (plist-get source-metadata :status)))
            (should-not (plist-get source-metadata :verification-pending))
            (let ((handoff (plist-get source-metadata :execution-handoff)))
              (should (equal ".worktrees:main"
                             (plist-get handoff :session)))
              (should (equal (plist-get worktree :plan-file)
                             (plist-get handoff :plan-file))))
            (should (eq 'approved (plist-get target-metadata :status)))
            (should (plist-get target-metadata :verification-pending))
            (should (file-exists-p
                     (plist-get target-metadata :accepted-absolute-path)))
            (with-temp-buffer
              (insert-file-contents
               (plist-get target-metadata :accepted-absolute-path))
              (should (equal "# Plan\n\nDo it." (buffer-string)))))
          (with-current-buffer source-buffer
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should (string-match-p "worktree/main" text))
              (should (string-match-p
                       (regexp-quote "/tmp/project/.worktrees/main/") text))))
          (should (memq source-session saved-sessions))
          (should (eq target-buffer implementation-buffer))
          (should (eq 'implement-worktree
                      (plist-get implementation :action)))
          (should (eq 'trust-all
                      (plist-get implementation :permission-mode)))
          (should (string-prefix-p target-dir
                                   (plist-get implementation :plan-file))))
      (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
      (when (buffer-live-p target-buffer) (kill-buffer target-buffer))
      (delete-directory source-dir t)
      (delete-directory target-dir t)))

  :doc "records a retryable target error when worktree request startup fails"
  (let ((source-dir (make-temp-file "mevedel-plan-error-source-" t))
        (target-dir (make-temp-file "mevedel-plan-error-target-" t))
        (source-buffer (generate-new-buffer " *mev-plan-error-source*"))
        (target-buffer (generate-new-buffer " *mev-plan-error-target*"))
        target-saved)
    (unwind-protect
        (let* ((source-session
                (mevedel-session--create
                 :name "main"
                 :workspace nil
                 :save-path source-dir
                 :permission-mode 'plan
                 :permission-rules nil
                 :permission-queue nil
                 :plan-queue nil
                 :plan-metadata '(:status presented)))
               (target-session
                (mevedel-session--create
                 :name ".worktrees:main"
                 :workspace nil
                 :save-path target-dir
                 :permission-mode 'default
                 :permission-rules nil
                 :permission-queue nil
                 :plan-queue nil))
               target-path
               (worktree
                (list :buffer target-buffer
                      :branch "worktree/main"
                      :directory "/tmp/project/.worktrees/main/")))
          (dolist (pair `((,source-buffer . ,source-session)
                          (,target-buffer . ,target-session)))
            (with-current-buffer (car pair)
              (org-mode)
              (setq-local gptel-response-separator "\n\n")
              (setq-local gptel-prompt-prefix-alist
                          '((org-mode . "* User\n")))
              (setq-local mevedel--session (cdr pair))))
          (with-current-buffer target-buffer
            (let ((target-artifact
                   (mevedel-plan-write-current
                    "# Plan\n\nDo it." target-session target-buffer)))
              (setq target-path (plist-get target-artifact :absolute-path))
              (mevedel-plan-mark-approved
               target-session target-artifact
               (mevedel-plan-archive-accepted
                target-artifact target-session))))
          (setq worktree (plist-put worktree :plan-file target-path))
          (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                     (lambda (session _buffer)
                       (when (eq session target-session)
                         (setq target-saved t))))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (_action)
                       (error "No API key"))))
            (with-current-buffer source-buffer
              (mevedel-plan-mode--approval-callback
               "# Plan\n\nDo it." source-buffer
               (list :action 'implement-worktree
                     :mode 'default
                     :worktree worktree))))
          (with-current-buffer target-buffer
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should (string-match-p
                       "Implementation request failed to start: No API key"
                       text))
              (should (string-match-p "retry" text))))
          (should target-saved)
          (should (eq 'approved
                      (plist-get (mevedel-session-plan-metadata target-session)
                                 :status)))
          (should (plist-get (mevedel-session-plan-metadata target-session)
                             :verification-pending))
          (should (equal
                   (list :action 'implement-worktree
                         :plan-file target-path
                         :permission-mode 'default)
                   (plist-get (mevedel-session-plan-metadata target-session)
                              :implementation-retry))))
      (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
      (when (buffer-live-p target-buffer) (kill-buffer target-buffer))
      (delete-directory source-dir t)
      (delete-directory target-dir t)))

  :doc "persists cancelled plan metadata"
  (let ((save-dir (make-temp-file "mevedel-plan-cancel-" t))
        (data-buffer (generate-new-buffer " *mev-plan-data*"))
        saved-status
        saved-pending)
    (unwind-protect
        (let ((session (mevedel-session--create
                        :name "test"
                        :workspace nil
                        :save-path save-dir
                        :permission-rules nil
                        :permission-mode 'plan
                        :permission-queue nil
                        :plan-queue nil
                        :plan-metadata
                        '(:path "plans/current.md"
                          :status presented
                          :verification-pending t)
                        :turn-count 1)))
          (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                     (lambda (session _buffer)
                       (let ((metadata
                              (mevedel-session-plan-metadata session)))
                         (setq saved-status (plist-get metadata :status))
                         (setq saved-pending
                               (plist-get metadata :verification-pending))))))
            (with-current-buffer data-buffer
              (setq-local mevedel--session session)
              (mevedel-plan-mode--approval-callback
               "# Plan\n\nDo it." data-buffer 'aborted))
            (should (eq saved-status 'cancelled))
            (should-not saved-pending)
            (let ((plans-dir (file-name-concat save-dir "plans")))
              (should-not (and (file-directory-p plans-dir)
                               (directory-files plans-dir nil
                                                "\\`accepted-"))))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-retry-plan-implementation
  (:doc "retries the persisted action with its implementation preset and permission mode")
  ,test
  (test)
  (let ((data-buffer (generate-new-buffer " *mev-plan-retry-data*"))
        (view-buffer (generate-new-buffer " *mev-plan-retry-view*"))
        (save-dir (make-temp-file "mevedel-plan-retry-" t))
        implemented
        implementation-buffer
        saved)
    (unwind-protect
        (let* ((retry (list :action 'implement-worktree
                            :plan-file "/tmp/accepted-plan.md"
                            :permission-mode 'trust-all))
               (session
                (mevedel-session--create
                 :name "worktree"
                 :workspace nil
                 :save-path save-dir
                 :permission-mode 'default
                 :permission-rules nil
                 :permission-queue nil
                 :plan-queue nil
                 :plan-metadata (list :status 'approved
                                      :implementation-retry retry))))
          (with-current-buffer data-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--view-buffer view-buffer))
          (with-current-buffer view-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--data-buffer data-buffer)
            (cl-letf
                (((symbol-function 'mevedel--implement-plan)
                  (lambda (action)
                    (setq implemented action
                          implementation-buffer (current-buffer))))
                 ((symbol-function 'mevedel-session-persistence-save)
                  (lambda (_session buffer)
                    (setq saved buffer))))
              (mevedel-retry-plan-implementation))
            (should (equal retry implemented))
            (should (eq data-buffer implementation-buffer))
            (should-not (plist-get (mevedel-session-plan-metadata session)
                                   :implementation-retry))
            (should (eq data-buffer saved))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-queue--on-head-outcome
  (:doc "blocks implementation while queued user messages are pending")
  ,test
  (test)
  (let (settled-outcome)
    (let* ((session (mevedel-session--create
                     :name "test"
                     :workspace nil
                     :permission-mode 'default
                     :queued-user-messages
                     (list (list :input "please revise first"))
                     :plan-queue nil))
           (entry (list :body "# Plan"
                        :origin "main"
                        :session session
                        :callback
                        (lambda (outcome)
                          (setq settled-outcome outcome)))))
      (setf (mevedel-session-plan-queue session) (list entry))
      (should-error
       (mevedel-plan-queue--on-head-outcome
        entry '(:action implement :mode default))
       :type 'user-error)
      (should (eq entry (car (mevedel-session-plan-queue session))))
      (should-not settled-outcome)
      (mevedel-plan-queue--on-head-outcome entry 'feedback-draft)
      (should-not (mevedel-session-plan-queue session))
      (should (eq 'feedback-draft settled-outcome)))))

(mevedel-deftest mevedel-plan-mode-restore-pending-approval
  (:doc "restores a presented plan approval prompt from the session artifact")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-restore-" t))
        (data-buffer (generate-new-buffer " *mev-plan-data*"))
        (view-buffer (generate-new-buffer " *mev-plan-view*")))
    (unwind-protect
        (let* ((session (mevedel-session--create
                         :name "test"
                         :workspace nil
                         :save-path save-dir
                         :permission-rules nil
                         :permission-mode 'plan
                         :permission-queue nil
                         :plan-queue nil
                         :plan-metadata
                         '(:path "plans/current.md" :status presented)))
               (path (file-name-concat save-dir "plans" "current.md")))
          (make-directory (file-name-directory path) t)
          (write-region "# Restored plan\n\nDo it." nil path nil 'silent)
          (with-current-buffer data-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--view-buffer view-buffer))
          (with-current-buffer view-buffer
            (setq-local mevedel--data-buffer data-buffer)
            (setq-local mevedel--session session)
            (setq-local mevedel-view--agent-transcript-p nil)
            (setq-local mevedel-view--interaction-marker
                        (copy-marker (point-min) t))
            (setq-local mevedel-view--input-marker
                        (copy-marker (point-min) nil))
            (setq-local mevedel-view--interaction-descriptors
                        (make-hash-table :test #'equal))
            (setq-local mevedel-view--interaction-overlays
                        (make-hash-table :test #'equal)))
          (with-current-buffer data-buffer
            (mevedel-plan-mode-restore-pending-approval session data-buffer))
          (should (= 1 (length (mevedel-session-plan-queue session))))
          (with-current-buffer view-buffer
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should (string-match-p "# Restored plan" text))
              (should (string-match-p "Keys:" text)))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-mode--insert-feedback-draft
  (:doc "inserts editable feedback draft referencing the plan artifact")
  (let ((data-buffer (generate-new-buffer " *mev-plan-data*"))
        (view-buffer (generate-new-buffer " *mev-plan-view*"))
        (path "/tmp/mevedel/plans/current.md"))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (_buffer) view-buffer))
                  ((symbol-function 'mevedel-view--clear-input)
                   (lambda () (erase-buffer)))
                  ((symbol-function 'mevedel-view--input-start)
                   (lambda () (point-min))))
          (with-current-buffer view-buffer
            (insert "stale input"))
          (mevedel-plan-mode--insert-feedback-draft data-buffer path)
          (with-current-buffer view-buffer
            (should (string-match-p
                     "Plan feedback:\n\n\n\nRevise the proposed plan"
                     (buffer-string)))
            (should (string-match-p "reference-only" (buffer-string)))
            (should (string-match-p "do not edit it" (buffer-string)))
            (should (string-match-p
                     "full replacement <proposed_plan> block"
                     (buffer-string)))
            (should (string-match-p (regexp-quote path) (buffer-string)))
            (should-not (string-match-p "Original proposed plan"
                                        (buffer-string)))
            (should-not (string-match-p "# Plan" (buffer-string)))
            (should (= (point)
                       (+ 1 (length "Plan feedback:\n\n"))))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer)))))

(mevedel-deftest mevedel-plan-mode--insert-and-send
  (:doc "renders plan-mode initiated sends in the view")
  ,test
  (test)
  (let ((data-buffer (generate-new-buffer " *mev-plan-data*"))
        (view-buffer (generate-new-buffer " *mev-plan-view*"))
        sent)
    (unwind-protect
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&optional _arg) (setq sent t))))
          (with-current-buffer data-buffer
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel--view-buffer view-buffer)
            (setq-local mevedel-agent-runtime--fsms nil)
            (setq-local mevedel--agent-invocation nil))
          (mevedel-view--setup view-buffer data-buffer)
          (with-current-buffer data-buffer
            (mevedel-plan-mode--insert-and-send "Plan this"))
          (should sent)
          (with-current-buffer view-buffer
            (let ((text (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
              (should (string-match-p "You" text))
              (should (string-match-p "Plan this" text))
              (should mevedel-view--data-turn-start)
              (should mevedel-view--in-flight-turn-start))))
	      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
	      (when (buffer-live-p view-buffer) (kill-buffer view-buffer)))))

(mevedel-deftest mevedel-plan-mode-restore-reminders
  (:doc "restores Plan-mode workflow reminders for resumed Plan sessions")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-reminders-" t)))
    (unwind-protect
        (let* ((plan-path (file-name-concat save-dir "plans" "current.md"))
               (session (mevedel-session--create
                         :name "test"
                         :workspace nil
                         :save-path save-dir
                         :permission-mode 'plan
                         :reminders nil)))
          (make-directory (file-name-directory plan-path) t)
          (write-region "# Existing plan\n" nil plan-path nil 'silent)
          (mevedel-reminders-install-defaults session)
          (mevedel-plan-mode-restore-reminders session)
          (let ((types (mapcar #'mevedel-reminder-type
                               (mevedel-session-reminders session))))
            (should (memq 'plan-mode types))
            (should (memq 'plan-mode-reentry types))
            (should-not (memq 'plan-mode-exit types))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-mode-enter-exit
  (:doc "restores the effective prior permission mode when session slot was nil")
  ,test
  (test)
  (let ((saved (default-toplevel-value 'mevedel-permission-mode)))
    (unwind-protect
        (let ((session (mevedel-session--create
                        :name "test"
                        :workspace nil
                        :permission-mode nil
                        :permission-rules nil
                        :permission-queue nil
                        :plan-queue nil
                        :plan-metadata nil)))
          (set-default-toplevel-value 'mevedel-permission-mode 'accept-edits)
          (with-temp-buffer
            (setq-local mevedel--session session)
            (kill-local-variable 'mevedel-permission-mode)
            (cl-letf (((symbol-function
                        'mevedel-skills--refresh-view-input-prompt)
                       #'ignore))
              (mevedel-plan-mode-enter)
              (should (eq 'plan
                          (mevedel-session-permission-mode session)))
              (should (eq 'accept-edits
                          (plist-get
                           (mevedel-session-plan-metadata session)
                           :previous-permission-mode)))
              (mevedel-plan-mode-exit)
              (should (eq 'accept-edits
                          (mevedel-session-permission-mode session)))
              (should-not
               (plist-get (mevedel-session-plan-metadata session)
                          :previous-permission-mode)))))
      (set-default-toplevel-value 'mevedel-permission-mode saved))))

(mevedel-deftest mevedel-plan-queue--enqueue
  (:doc "plan approval queue renders only the FIFO head")
  ,test
  (test)
  :doc "first plan renders immediately; second waits"
  (let* ((session (mevedel-session--create
                   :name "test"
                   :workspace nil
                   :permission-rules nil
                   :permission-mode 'default
                   :permission-queue nil
                   :plan-queue nil))
         (mevedel--session session)
         (rendered nil))
    (cl-letf (((symbol-function 'mevedel-plan-queue--render-entry)
               (lambda (entry) (push (plist-get entry :body) rendered))))
      (mevedel-plan-queue--enqueue
       (list :body "plan 1" :chat-buffer (current-buffer)
             :callback #'ignore))
      (mevedel-plan-queue--enqueue
       (list :body "plan 2" :chat-buffer (current-buffer)
             :callback #'ignore)))
    (should (equal '("plan 1" "plan 1") rendered))
    (should (= 2 (length (mevedel-session-plan-queue session)))))

  :doc "settling the head renders the next plan"
  (let* ((session (mevedel-session--create
                   :name "test"
                   :workspace nil
                   :permission-rules nil
                   :permission-mode 'default
                   :permission-queue nil
                   :plan-queue nil))
         (mevedel--session session)
         (rendered nil)
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-plan-queue--render-entry)
               (lambda (entry) (push (plist-get entry :body) rendered))))
      (mevedel-plan-queue--enqueue
       (list :body "plan 1" :chat-buffer (current-buffer)
             :callback (lambda (o) (push o outcomes))))
      (mevedel-plan-queue--enqueue
       (list :body "plan 2" :chat-buffer (current-buffer)
             :callback (lambda (o) (push o outcomes))))
      (mevedel-plan-queue--on-head-outcome
       (car (mevedel-session-plan-queue session)) 'aborted))
    (should (equal '(aborted) outcomes))
    (should (member "plan 2" rendered))
    (should (= 1 (length (mevedel-session-plan-queue session)))))

  :doc "missing live view aborts the visible head"
  (with-temp-buffer
    (let* ((session (mevedel-session--create
                     :name "test"
                     :workspace nil
                     :permission-rules nil
                     :permission-mode 'default
                     :permission-queue nil
                     :plan-queue nil))
           (mevedel--session session)
           (outcome nil)
           (entry (list :body "# Plan"
                        :chat-buffer (current-buffer)
                        :session session
                        :callback (lambda (o) (setq outcome o)))))
      (setf (mevedel-session-plan-queue session) (list entry))
      (mevedel-plan-queue--render-head session)
      (should (eq 'aborted outcome))
      (should (null (mevedel-session-plan-queue session)))))

  :doc "confirmation controls render once after the plan body"
  (with-temp-buffer
    (let* ((chat-buffer (current-buffer))
           (target-buffer (generate-new-buffer " *plan-view*"))
           (session (mevedel-session--create
                     :name "test"
                     :workspace nil
                     :permission-rules nil
                     :permission-mode 'default
                     :permission-queue nil
                     :plan-queue nil))
           (mevedel--session session)
           (captured-body nil)
           (entry (list :body "# Plan\n\nDo the work."
                        :chat-buffer chat-buffer
                        :session session
                        :callback #'ignore)))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (&optional _data-buffer) target-buffer))
                    ((symbol-function 'mevedel-view--interaction-anchor)
                     (lambda () (point-min)))
                    ((symbol-function 'mevedel-view--interaction-register)
                     (lambda (descriptor)
                       (setq captured-body (plist-get descriptor :body))
                       (make-overlay (point-min) (point-min)
                                     (current-buffer) nil t)))
                    ((symbol-function 'mevedel--prompt--register-canceller)
                     #'ignore))
            (with-current-buffer target-buffer
              (setq-local mevedel--prompt-overlays nil))
            (setf (mevedel-session-plan-queue session) (list entry))
            (mevedel-plan-queue--render-entry entry)
            (should captured-body)
            (should (= 1
                       (cl-loop with start = 0
                                while (string-match "Keys: " captured-body start)
                                count t
                                do (setq start (match-end 0)))))
            (should (string-match-p "Do the work\\.\n\nKeys: "
                                    captured-body))
            (should (< (string-match-p "# Plan" captured-body)
                       (string-match-p "Keys: " captured-body))))
        (when (buffer-live-p target-buffer)
          (kill-buffer target-buffer)))))

  :doc "implementation mode key cycles and applies to approval"
  (with-temp-buffer
    (let* ((chat-buffer (current-buffer))
           (target-buffer (generate-new-buffer " *plan-view*"))
           (session (mevedel-session--create
                     :name "test"
                     :workspace nil
                     :permission-rules nil
                     :permission-mode 'default
                     :permission-queue nil
                     :plan-queue nil))
           (mevedel--session session)
           captured-body
           captured-keymap
           outcome
           (entry (list :body "# Plan\n\nDo the work."
                        :chat-buffer chat-buffer
                        :session session
                        :callback (lambda (o) (setq outcome o)))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (&optional _data-buffer) target-buffer))
                    ((symbol-function 'mevedel-view--interaction-anchor)
                     (lambda () (point-min)))
                    ((symbol-function 'mevedel-view--interaction-register)
                     (lambda (descriptor)
                       (setq captured-body (plist-get descriptor :body))
                       (setq captured-keymap (plist-get descriptor :keymap))
                       (make-overlay (point-min) (point-min)
                                     (current-buffer) nil t)))
                    ((symbol-function 'mevedel--prompt--settle)
                     (lambda (overlay outcome)
                       (funcall (overlay-get overlay 'mevedel--callback)
                                outcome)))
                    ((symbol-function 'mevedel--prompt--register-canceller)
                     #'ignore))
            (with-current-buffer target-buffer
              (setq-local mevedel--prompt-overlays nil)
              (setq-local mevedel-view--interaction-descriptors
                          (make-hash-table :test #'equal))
              (setq-local mevedel-view--interaction-overlays
                          (make-hash-table :test #'equal)))
            (setf (mevedel-session-plan-queue session) (list entry))
            (mevedel-plan-queue--render-entry entry)
            (should (string-match-p "mode: default" captured-body))
            (should (lookup-key captured-keymap (kbd "TAB")))
            (call-interactively (lookup-key captured-keymap (kbd "TAB")))
            (should (string-match-p "mode: edit" captured-body))
            (call-interactively (lookup-key captured-keymap (kbd "m")))
            (should (string-match-p "mode: auto" captured-body))
            (call-interactively (lookup-key captured-keymap (kbd "RET")))
            (should (equal '(:action implement :mode trust-all) outcome)))
        (when (buffer-live-p target-buffer)
          (kill-buffer target-buffer)))))

  :doc "implementation key leaves approval unsettled when queued messages exist"
  (with-temp-buffer
    (let* ((chat-buffer (current-buffer))
           (target-buffer (generate-new-buffer " *plan-view*"))
           (session (mevedel-session--create
                     :name "test"
                     :workspace nil
                     :permission-rules nil
                     :permission-mode 'default
                     :permission-queue nil
                     :queued-user-messages
                     (list (list :input "queued feedback"))
                     :plan-queue nil))
           (mevedel--session session)
           captured-keymap
           settled
           outcome
           (entry (list :body "# Plan\n\nDo the work."
                        :chat-buffer chat-buffer
                        :session session
                        :callback (lambda (o) (setq outcome o)))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (&optional _data-buffer) target-buffer))
                    ((symbol-function 'mevedel-view--interaction-anchor)
                     (lambda () (point-min)))
                    ((symbol-function 'mevedel-view--interaction-register)
                     (lambda (descriptor)
                       (setq captured-keymap (plist-get descriptor :keymap))
                       (make-overlay (point-min) (point-min)
                                     (current-buffer) nil t)))
                    ((symbol-function 'mevedel--prompt--settle)
                     (lambda (_overlay _outcome)
                       (setq settled t)))
                    ((symbol-function 'mevedel--prompt--register-canceller)
                     #'ignore))
            (with-current-buffer target-buffer
              (setq-local mevedel--prompt-overlays nil)
              (setq-local mevedel-view--interaction-descriptors
                          (make-hash-table :test #'equal))
              (setq-local mevedel-view--interaction-overlays
                          (make-hash-table :test #'equal)))
            (setf (mevedel-session-plan-queue session) (list entry))
            (mevedel-plan-queue--render-entry entry)
            (should-error
             (call-interactively (lookup-key captured-keymap (kbd "RET")))
             :type 'user-error)
            (should-not settled)
            (should-not outcome)
            (should (eq entry (car (mevedel-session-plan-queue session)))))
        (when (buffer-live-p target-buffer)
          (kill-buffer target-buffer)))))

  :doc "worktree key creates the target before settling approval"
  (with-temp-buffer
    (let* ((target-dir (make-temp-file "mevedel-plan-key-target-" t))
           (chat-buffer (current-buffer))
           (view-buffer (generate-new-buffer " *plan-view*"))
           (worktree-buffer (generate-new-buffer " *plan-worktree*"))
           (session (mevedel-session--create
                     :name "test"
                     :workspace nil
                     :permission-rules nil
                     :permission-mode 'default
                     :permission-queue nil
                     :plan-queue nil))
           (target-session (mevedel-session--create
                            :name "worktree"
                            :workspace nil
                            :save-path target-dir
                            :permission-mode 'default))
           (mevedel--session session)
           captured-keymap
           created-from
           fail-create
           outcome
           target-saved
           target-metadata-at-settle
           (worktree-result
            (list :buffer worktree-buffer
                  :branch "worktree/test"
                  :directory "/tmp/worktrees/test/"))
           (entry (list :body "# Plan\n\nDo the work."
                        :chat-buffer chat-buffer
                        :session session
                        :callback
                        (lambda (value)
                          (setq target-metadata-at-settle
                                (copy-sequence
                                 (mevedel-session-plan-metadata
                                  target-session)))
                          (setq outcome value)))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (&optional _data-buffer) view-buffer))
                    ((symbol-function 'mevedel-view--interaction-anchor)
                     (lambda () (point-min)))
                    ((symbol-function 'mevedel-view--interaction-register)
                     (lambda (descriptor)
                       (setq captured-keymap (plist-get descriptor :keymap))
                       (make-overlay (point-min) (point-min)
                                     (current-buffer) nil t)))
                    ((symbol-function 'mevedel--prompt--settle)
                     (lambda (overlay value)
                       (funcall (overlay-get overlay 'mevedel--callback)
                                value)))
                    ((symbol-function 'mevedel--prompt--register-canceller)
                     #'ignore)
                    ((symbol-function 'mevedel-plan-mode--save-session-state)
                     (lambda (saved-session _buffer)
                       (when (eq saved-session target-session)
                         (setq target-saved t))))
                    ((symbol-function 'mevedel-worktree-create-session)
                     (lambda (&rest _)
                       (setq created-from (current-buffer))
                       (if fail-create
                           (user-error "Worktree creation failed")
                         worktree-result))))
            (setq-local gptel-backend 'source-backend)
            (setq-local gptel-model 'source-model)
            (with-current-buffer view-buffer
              (setq-local mevedel--prompt-overlays nil)
              (setq-local mevedel-view--interaction-descriptors
                          (make-hash-table :test #'equal))
              (setq-local mevedel-view--interaction-overlays
                          (make-hash-table :test #'equal)))
            (with-current-buffer worktree-buffer
              (org-mode)
              (setq-local gptel-response-separator "\n\n")
              (setq-local gptel-prompt-prefix-alist
                          '((org-mode . "* User\n")))
              (setq-local mevedel--session target-session))
            (setf (mevedel-session-plan-queue session) (list entry))
            (mevedel-plan-queue--render-entry entry)
            (setq fail-create t)
            (should-error
             (call-interactively (lookup-key captured-keymap (kbd "w")))
             :type 'user-error)
            (should-not outcome)
            (should (eq entry (car (mevedel-session-plan-queue session))))
            (setq fail-create nil)
            (call-interactively (lookup-key captured-keymap (kbd "w")))
            (should (eq chat-buffer created-from))
            (should (eq 'implement-worktree (plist-get outcome :action)))
            (should (eq 'default (plist-get outcome :mode)))
            (should (eq 'approved
                        (plist-get target-metadata-at-settle :status)))
            (should target-saved)
            (should (plist-get target-metadata-at-settle
                               :verification-pending))
            (should (file-exists-p
                     (plist-get target-metadata-at-settle
                                :accepted-absolute-path)))
            (with-current-buffer worktree-buffer
              (should (eq 'source-backend gptel-backend))
              (should (eq 'source-model gptel-model))))
        (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
        (when (buffer-live-p worktree-buffer) (kill-buffer worktree-buffer))
        (delete-directory target-dir t))))

  :doc "implementation mode rerender preserves point and viewport inside the plan overlay"
  (let* ((data-buffer (generate-new-buffer " *plan-data*"))
         (target-buffer (generate-new-buffer " *plan-view*"))
         (session (mevedel-session--create
                   :name "test"
                   :workspace nil
                   :permission-rules nil
                   :permission-mode 'default
                   :permission-queue nil
                   :plan-queue nil))
         (plan-body
          (concat
           "# Plan\n\n"
           (mapconcat (lambda (n) (format "- Step %02d" n))
                      (number-sequence 1 80)
                      "\n")
           "\n\nDo the work."))
         keymap
         offset
         window-start-pos)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup target-buffer data-buffer)
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (&optional _data-buffer) target-buffer)))
            (let ((entry (list :body plan-body
                               :chat-buffer data-buffer
                               :session session
                               :callback #'ignore)))
              (setf (mevedel-session-plan-queue session) (list entry))
              (mevedel-plan-queue--render-entry entry)))
          (save-window-excursion
            (switch-to-buffer target-buffer)
            (delete-other-windows)
            (goto-char (point-min))
            (search-forward "- Step 40")
            (setq window-start-pos (match-beginning 0))
            (search-forward "mode: default")
            (goto-char (match-beginning 0))
            (set-window-point (selected-window) (point))
            (set-window-start (selected-window) window-start-pos t)
            (let ((overlay (get-text-property
                            (point) 'mevedel-view-interaction-overlay)))
              (setq offset (- (point) (overlay-start overlay)))
              (setq keymap (get-text-property (point) 'keymap)))
            (call-interactively (lookup-key keymap (kbd "m")))
            (should (looking-at-p "mode: edit"))
            (let ((overlay (get-text-property
                            (point) 'mevedel-view-interaction-overlay)))
              (should overlay)
              (should (= (point) (+ (overlay-start overlay) offset)))
              (should (= (window-point (selected-window)) (point)))
              (should (= (window-start) window-start-pos)))))
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))

  :doc "implementation mode rerender preserves unselected overlay window scroll"
  (let* ((data-buffer (generate-new-buffer " *plan-data*"))
         (target-buffer (generate-new-buffer " *plan-view*"))
         (session (mevedel-session--create
                   :name "test"
                   :workspace nil
                   :permission-rules nil
                   :permission-mode 'default
                   :permission-queue nil
                   :plan-queue nil))
         (plan-body
          (concat
           "# Plan\n\n"
           (mapconcat (lambda (n) (format "- Step %02d" n))
                      (number-sequence 1 80)
                      "\n")
           "\n\nDo the work."))
         keymap
         window-start-pos
         window-start-text)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup target-buffer data-buffer)
          (with-current-buffer target-buffer
            (setq-local after-change-functions nil))
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (&optional _data-buffer) target-buffer)))
            (let ((entry (list :body plan-body
                               :chat-buffer data-buffer
                               :session session
                               :callback #'ignore)))
              (setf (mevedel-session-plan-queue session) (list entry))
              (mevedel-plan-queue--render-entry entry)))
          (save-window-excursion
            (switch-to-buffer target-buffer)
            (delete-other-windows)
            (let ((input-window (selected-window))
                  (overlay-window (split-window-right)))
              (set-window-buffer overlay-window target-buffer)
              (select-window overlay-window)
              (goto-char (point-min))
              (search-forward "- Step 40")
              (setq window-start-pos (match-beginning 0))
              (search-forward "mode: default")
              (goto-char (match-beginning 0))
              (set-window-point overlay-window (point))
              (set-window-start overlay-window window-start-pos t)
              (setq keymap (get-text-property (point) 'keymap))
              (select-window input-window)
              (goto-char (point-max))
              (set-window-point input-window (point))
              (redisplay t)
              (setq window-start-pos (window-start overlay-window))
              (setq window-start-text
                    (with-current-buffer target-buffer
                      (save-excursion
                        (goto-char window-start-pos)
                        (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))))
              (cl-letf (((symbol-function
                          'mevedel-view--refresh-skill-argument-hint-after-change)
                         #'ignore))
                (call-interactively (lookup-key keymap (kbd "m"))))
              (should (get-text-property
                       (window-point overlay-window)
                       'mevedel-view-interaction-overlay))
              (should
               (equal window-start-text
                      (with-current-buffer target-buffer
                        (save-excursion
                          (goto-char (window-start overlay-window))
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))))))))
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(provide 'test-mevedel-tool-plan)
;;; test-mevedel-tool-plan.el ends here
