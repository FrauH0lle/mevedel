;;; test-mevedel-tool-plan.el --- Tests for mevedel-tool-plan.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-tool-registry)
(require 'mevedel-tool-plan)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'mevedel-mentions)
(require 'mevedel-reminders)
(require 'mevedel-session-persistence)
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
  "Return non-nil when STRING contains raw byte characters."
  (catch 'found
    (dotimes (index (length string))
      (when (eq (char-charset (aref string index)) 'eight-bit)
        (throw 'found t)))
    nil))


;;
;;; Proposed-plan blocks

(mevedel-deftest mevedel-plan-mode-extract-proposed-plan
  (:doc "extracts the body of a line-oriented proposed-plan block")
  ,test
  (test)
  (should (equal "# Plan\nDo it."
                 (mevedel-plan-mode-extract-proposed-plan
                  "Intro\n<proposed_plan>\n# Plan\nDo it.\n</proposed_plan>\nTail")))

  :doc "returns the last proposed-plan block when multiple are present"
  (should (equal "second"
                 (mevedel-plan-mode-extract-proposed-plan
                  "<proposed_plan>\nfirst\n</proposed_plan>\n<proposed_plan>\nsecond\n</proposed_plan>")))

  :doc "ignores inline tag-looking text"
  (should-not
   (mevedel-plan-mode-extract-proposed-plan
    "text <proposed_plan>\nnot a plan\n</proposed_plan>")))

(mevedel-deftest mevedel-plan-mode-strip-proposed-plans
  (:doc "removes proposed-plan blocks from visible assistant text")
  ,test
  (test)
  (should (equal "Intro\nTail"
                 (mevedel-plan-mode-strip-proposed-plans
                  "Intro\n<proposed_plan>\n# Plan\n</proposed_plan>\nTail")))

  :doc "removes an incomplete streaming proposed-plan block"
  (should (equal "Intro"
                 (mevedel-plan-mode-strip-proposed-plans
                  "Intro\n<proposed_plan>\n# Streaming plan\n"))))


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
          (should (mevedel-plan-mode-known-proposed-plan-p
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
            (mevedel-plan-mode--current-plan-body session))))
      (delete-directory save-dir t)))

  :doc "uses workspace plans directory when session persistence is disabled"
  (let ((root (make-temp-file "mevedel-plan-workspace-" t))
        (rendered nil)
        (mevedel-session-persistence nil)
        (mevedel-plans-directory (file-name-concat ".mevedel" "plans")))
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
            (should (null (mevedel-session-save-path session)))
            (should (equal '("# Workspace plan\n\nDo it.") rendered))
            (should (file-exists-p path))
            (should (string-prefix-p
                     (file-name-concat root ".mevedel" "plans")
                     path))
            (should (equal "# Workspace plan\n\nDo it."
                           (mevedel-plan-mode--current-plan-body session)))
            (should (mevedel-plan-mode--current-plan-exists-p session))))
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
                       (setq saved-status
                             (plist-get
                              (mevedel-session-plan-metadata session)
                              :status))
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
            (should implemented)))
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
            (should-not draft-feedback)))
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
            (should-not saved-pending)))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t))))

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
                     "Plan feedback:\n\n\n\nRevise the saved proposed plan"
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
            (setq-local mevedel-tools--agents-fsm nil)
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
            (call-interactively (lookup-key captured-keymap (kbd "m")))
            (should (string-match-p "mode: edit" captured-body))
            (call-interactively (lookup-key captured-keymap (kbd "m")))
            (should (string-match-p "mode: auto" captured-body))
            (call-interactively (lookup-key captured-keymap (kbd "RET")))
            (should (equal '(:action implement :mode trust-all) outcome)))
        (when (buffer-live-p target-buffer)
          (kill-buffer target-buffer)))))

  :doc "implementation mode rerender preserves point inside the plan overlay"
  (let* ((data-buffer (generate-new-buffer " *plan-data*"))
         (target-buffer (generate-new-buffer " *plan-view*"))
         (session (mevedel-session--create
                   :name "test"
                   :workspace nil
                   :permission-rules nil
                   :permission-mode 'default
                   :permission-queue nil
                   :plan-queue nil))
         keymap
         offset)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup target-buffer data-buffer)
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (&optional _data-buffer) target-buffer)))
            (let ((entry (list :body "# Plan\n\nDo the work."
                               :chat-buffer data-buffer
                               :session session
                               :callback #'ignore)))
              (setf (mevedel-session-plan-queue session) (list entry))
              (mevedel-plan-queue--render-entry entry)))
          (save-window-excursion
            (switch-to-buffer target-buffer)
            (delete-other-windows)
            (goto-char (point-min))
            (search-forward "mode: default")
            (goto-char (match-beginning 0))
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
              (should (= (window-point (selected-window)) (point))))))
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(provide 'test-mevedel-tool-plan)
;;; test-mevedel-tool-plan.el ends here
