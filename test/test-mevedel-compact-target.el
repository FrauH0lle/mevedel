;;; test-mevedel-compact-target.el -- Tests for compaction target -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'mevedel)
(require 'mevedel-agent-control)
(require 'mevedel-agent-exec)
(require 'mevedel-agent-runtime)
(require 'mevedel-compact)
(require 'mevedel-compact-estimation)
(require 'mevedel-compact-evidence)
(require 'mevedel-compact-run)
(require 'mevedel-compact-target)
(require 'mevedel-execution-transcript)
(require 'mevedel-models)
(require 'mevedel-hooks)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-system)
(require 'mevedel-tool-render-data)
(require 'mevedel-utilities)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-render)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-compact-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-compact-test-support"))

(defvar gptel--request-params)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-model)
(defvar gptel-reasoning-effort)

(mevedel-deftest mevedel-compact-target-file-reference-reminder-body
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "lists touched file references whose turns fall outside the preserved tail"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws)))
    (let ((mevedel-compact-evidence-tail-turns 2))
      (setf (mevedel-session-turn-count session) 10)
      (puthash "/tmp/p/old.el"
               (mevedel-file-interaction--create
                :path "/tmp/p/old.el" :read-turn 4)
               (mevedel-session-touched-files session))
      (puthash "/tmp/p/boundary.el"
               (mevedel-file-interaction--create
                :path "/tmp/p/boundary.el" :read-turn 8)
               (mevedel-session-touched-files session))
      (puthash "/tmp/p/recent.el"
               (mevedel-file-interaction--create
                :path "/tmp/p/recent.el" :read-turn 9)
               (mevedel-session-touched-files session))
      (let ((body (mevedel-compact-target-file-reference-reminder-body
                   session 2 nil)))
        (should (string-match-p "/tmp/p/old.el" body))
        (should-not (string-match-p "/tmp/p/boundary.el" body))
        (should-not (string-match-p "/tmp/p/recent.el" body)))))

  :doc "aggressive compaction lists even recent touched file references"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/q/" "/tmp/q/" "q"))
         (session (mevedel-session-create "main" ws)))
    (setf (mevedel-session-turn-count session) 10)
    (puthash "/tmp/q/recent.el"
             (mevedel-file-interaction--create
              :path "/tmp/q/recent.el" :read-turn 9)
             (mevedel-session-touched-files session))
    (let ((body (mevedel-compact-target-file-reference-reminder-body
                 session 0 nil)))
      (should (string-match-p "/tmp/q/recent.el" body))))

  :doc "auto compaction lists files stamped with the in-flight reserved turn"
  ;; Mid-request file access is stamped with the reserved turn, one
  ;; above the committed count; a mid-request compaction summarizes
  ;; exactly that evidence, so auto includes it and manual does not.
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-compact-in-flight-" t)))
         (path (file-name-concat root "in-flight.el"))
         (ws (mevedel-workspace-get-or-create 'project root root "r"))
         (session (mevedel-session-create "main" ws)))
    (unwind-protect
        (progn
          (with-temp-file path (insert "content\n"))
          (setf (mevedel-session-turn-count session) 10)
          ;; An older modification must not mask the later in-flight read.
          (puthash path
                   (mevedel-file-interaction--create
                    :path path :modified-turn 5)
                   (mevedel-session-touched-files session))
          (let ((mevedel--current-request
                 (mevedel-request--create :session session :turn 11)))
            (mevedel-session-record-file-access session path 'read))
          (let ((interaction
                 (gethash path (mevedel-session-touched-files session))))
            (should (= 5 (mevedel-file-interaction-modified-turn interaction)))
            (should (= 11 (mevedel-file-interaction-read-turn interaction))))
          (should (string-match-p
                   (regexp-quote path)
                   (mevedel-compact-target-file-reference-reminder-body
                    session 2 t)))
          (should-not (mevedel-compact-target-file-reference-reminder-body
                       session 2 nil)))
      (delete-directory root t))))

(mevedel-deftest mevedel-compact-target-agent-target ()
  ,test
  (test)
  :doc "builds a complete adapter only for the live canonical transcript"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((start (point)))
      (insert "Agent response.\n")
      (put-text-property start (point) 'gptel 'response))
    (basic-save-buffer)
    (let ((target (mevedel-compact-target-agent-target invocation)))
      (should (eq agent-buffer (plist-get target :buffer)))
      (should (eq invocation (plist-get target :invocation)))
      (should (eq session (plist-get target :session)))
      (should (eq session (plist-get target :prompt-session)))
      (should (equal "/root/explorer"
                     (plist-get target :skill-agent-path)))
      (should (string-suffix-p
               "/tool-results" (plist-get target :tool-results-dir)))
      (should (equal canonical-path (plist-get target :transcript-path)))
      (dolist (operation '(:apply :start :complete :resume :fail))
        (should (functionp (plist-get target operation)))))
    (setf (mevedel-agent-invocation-transcript-relative-path invocation)
          "/ssh:foreign:/tmp/agent.chat.org")
    (should-not (mevedel-compact-target-agent-target invocation))
    (setf (mevedel-agent-invocation-transcript-relative-path invocation)
          "agents/other.chat.org")
    (should-not (mevedel-compact-target-agent-target invocation)))

  :doc "anchors the marked child task after inherited nested-agent context"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (let ((parent
           (mevedel-agent-invocation--create :path "/root/parent")))
      (test-mevedel-compact--insert-agent-task
       parent "parent" "Inherited prompt."))
    (let ((start (point)))
      (insert "Inherited response.\n")
      (put-text-property start (point) 'gptel 'response))
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((start (point)))
      (insert "Agent response.\n")
      (put-text-property start (point) 'gptel 'response))
    (basic-save-buffer)
    (let ((target (mevedel-compact-target-agent-target invocation)))
      (should target)
      (should (string-match-p "Inherited prompt"
                              (buffer-substring-no-properties
                               (caar (plist-get
                                      target :history-prefix-regions))
                               (cdar (plist-get
                                      target :history-prefix-regions)))))
      (should-not (string-match-p "Inherited prompt"
                                  (plist-get target :anchor-text)))
      (should (string-match-p "Keep this task"
                              (plist-get target :anchor-text)))))

  :doc "accepts a tool-only agent turn after the marked task"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((tool-start (point)))
      (insert "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nresult\n")
      (put-text-property tool-start (point) 'gptel '(tool . "call-1"))
      (basic-save-buffer)
      (let ((target (mevedel-compact-target-agent-target invocation)))
        (should target)
        (should (= tool-start (plist-get target :body-start)))
        (should (string-match-p "Keep this task"
                                (plist-get target :anchor-text))))))

  :doc "uses publication membership for a remote canonical transcript"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((start (point)))
      (insert "Agent response.\n")
      (put-text-property start (point) 'gptel 'response))
    (basic-save-buffer)
    (delete-file canonical-path)
    (let ((save-path (mevedel-session-save-path session))
          seen)
      (cl-letf (((symbol-function 'file-remote-p)
                 (lambda (path &rest _)
                   (and (equal path save-path) "/mock:")))
                ((symbol-function
                  'mevedel-session-artifacts-artifact-present-p)
                 (lambda (seen-session logical)
                   (setq seen (list seen-session logical))
                   t)))
        (should (mevedel-compact-target-agent-target invocation))
        (should
         (equal (list session "agents/explorer-test.chat.org") seen)))
      (make-directory (file-name-directory canonical-path) t)
      (write-region "poisoned cache" nil canonical-path nil 'silent)
      (cl-letf (((symbol-function 'file-remote-p)
                 (lambda (path &rest _)
                   (and (equal path save-path) "/mock:")))
                ((symbol-function
                  'mevedel-session-artifacts-artifact-present-p)
                 (lambda (&rest _) nil)))
        (should-not (mevedel-compact-target-agent-target invocation))))))

(mevedel-deftest mevedel-compact-target--agent-archive-path ()
  ,test
  (test)
  :doc "selects the first unused numbered sibling archive"
  (let* ((tempdir (make-temp-file "mevedel-compact-archive-test-" t))
         (canonical (expand-file-name "agent.chat.org" tempdir))
         (first (expand-file-name "agent.compact-0001.chat.org" tempdir))
         (session
          (mevedel-session--create
           :name "main" :save-path (file-name-as-directory tempdir))))
    (unwind-protect
        (progn
          (write-region "canonical" nil canonical nil 'silent)
          (should (equal first
                         (mevedel-compact-target--agent-archive-path
                          session canonical)))
          (write-region "archive" nil first nil 'silent)
          (should
           (equal (expand-file-name "agent.compact-0002.chat.org" tempdir)
                  (mevedel-compact-target--agent-archive-path
                   session canonical)))
          (let ((save-path (mevedel-session-save-path session)))
            (cl-letf (((symbol-function 'file-remote-p)
                       (lambda (path &rest _)
                         (and (equal path save-path) "/mock:")))
                      ((symbol-function
                        'mevedel-session-artifacts-artifact-present-p)
                       (lambda (&rest _) nil)))
              (should
               (equal first
                      (mevedel-compact-target--agent-archive-path
                       session canonical))))
            (delete-file first)
            (let (seen-logicals)
              (cl-letf (((symbol-function 'file-remote-p)
                         (lambda (path &rest _)
                           (and (equal path save-path) "/mock:")))
                        ((symbol-function
                          'mevedel-session-artifacts-artifact-present-p)
                         (lambda (_seen-session logical)
                           (push logical seen-logicals)
                           (equal logical
                                  "agent.compact-0001.chat.org"))))
                (should
                 (equal (expand-file-name
                         "agent.compact-0002.chat.org" tempdir)
                        (mevedel-compact-target--agent-archive-path
                         session canonical)))
                (should
                 (equal '("agent.compact-0001.chat.org"
                          "agent.compact-0002.chat.org")
                        (nreverse seen-logicals)))))))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-compact-target--agent-apply (:quiet t)
  ,test
  (test)
  :doc "archives the full canonical transcript before rewriting it"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((start (point)))
      (insert "Old response.\n")
      (put-text-property start (point) 'gptel 'response))
    (basic-save-buffer)
    (setf (mevedel-session-workspace-instruction-hashes session)
          '((("/root" "/workspace/root/AGENTS.md") . "root")
            (("/root/explorer" "/workspace/nested/AGENTS.md") . "agent")))
    (let* ((record
            (mevedel-agent-record--create
             :id (mevedel-agent-invocation-agent-id invocation)
             :path "/root/explorer"
             :parent-path "/root"
             :activity 'running
             :conversation-buffer agent-buffer
             :conversation-location
             (mevedel-agent-invocation-transcript-relative-path invocation)
             :invocation invocation))
           (_ (setf (mevedel-session-agent-registry session)
                    (list (cons "/root/explorer" record))))
           (original (buffer-string))
           (original-publish
            (symbol-function 'mevedel-session-artifacts-publish-text))
           published
           (target (mevedel-compact-target-agent-target invocation))
           (archive
            (cl-letf (((symbol-function
                        'mevedel-session-artifacts-publish-text)
                       (lambda (seen-session path content &optional coding)
                         (push (list seen-session path content coding)
                               published)
                         (funcall original-publish
                                  seen-session path content coding))))
              (mevedel-compact-target--agent-apply
               target "## Goal\n- Continue" "Recent tail.\n"
               "Pending result.\n" nil))))
      ;; The archive publishes first; a portable session then publishes
      ;; the rewritten canonical transcript.
      (should (equal (list session archive
                           (substring-no-properties original) 'utf-8-unix)
                     (car (last published))))
      (should (file-exists-p archive))
      (should (equal original
                     (with-temp-buffer
                       (insert-file-contents archive)
                       (buffer-string))))
      (should (string-match-p "Keep this task" (buffer-string)))
      (should (string-match-p "Continue" (buffer-string)))
      (should (string-match-p "Recent tail" (buffer-string)))
      (should (string-match-p "Pending result" (buffer-string)))
      (should (eq record
                  (cdr (assoc "/root/explorer"
                              (mevedel-session-agent-registry session)))))
      (should (eq agent-buffer
                  (mevedel-agent-record-conversation-buffer record)))
      (should (equal (buffer-string)
                     (with-temp-buffer
                       (insert-file-contents canonical-path)
                       (buffer-string))))
      (should
       (equal (mevedel-session-workspace-instruction-hashes session)
              '((("/root" "/workspace/root/AGENTS.md") . "root"))))))

  :doc "leaves live and canonical transcripts unchanged on archive failure"
  (progn
    (skip-unless (not (eq system-type 'windows-nt)))
    (test-mevedel-compact--with-persisted-agent
        (agent-buffer invocation session canonical-path parent-buffer)
      (test-mevedel-compact--insert-agent-task
       invocation "inspect" "Keep this task.")
      (let ((start (point)))
        (insert "Old response.\n")
        (put-text-property start (point) 'gptel 'response))
      (basic-save-buffer)
      (let* ((target (mevedel-compact-target-agent-target invocation))
             (directory (file-name-directory canonical-path))
             (original-live (buffer-string))
             (original-canonical
              (with-temp-buffer
                (insert-file-contents canonical-path)
                (buffer-string))))
        (unwind-protect
            (progn
              (set-file-modes directory #o500)
              (skip-unless (not (file-writable-p directory)))
              (should-error
               (mevedel-compact-target--agent-apply
                target "## Goal\n- Continue" nil nil nil)
               :type 'file-error))
          (set-file-modes directory #o700))
        (should (equal original-live (buffer-string)))
        (should (equal original-canonical
                       (with-temp-buffer
                         (insert-file-contents canonical-path)
                         (buffer-string))))
        (should-not
         (directory-files directory nil
                          "\\.compact-[0-9]+\\.chat\\.org\\'")))))

  :doc "retains the full archive when canonical application later fails"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((start (point)))
      (insert "Old response.\n")
      (put-text-property start (point) 'gptel 'response))
    (basic-save-buffer)
    (let* ((original (buffer-string))
           (target (mevedel-compact-target-agent-target invocation)))
      (add-hook
       'before-change-functions
       (lambda (&rest _)
         (error "Stop compacted transcript application"))
       nil t)
      (should-error
       (mevedel-compact-target--agent-apply
        target "## Goal\n- Continue" nil nil nil)
       :type 'error)
      (let ((archives
             (directory-files
              (file-name-directory canonical-path) t
              "\\.compact-[0-9]+\\.chat\\.org\\'")))
        (should (= 1 (length archives)))
        (should (equal original
                       (with-temp-buffer
                         (insert-file-contents (car archives))
                         (buffer-string))))))))

(mevedel-deftest mevedel-compact-target--main-apply ()
  ,test
  (test)
  :doc "delegates rewriting and routes reminders by compaction mode"
  (let ((session 'session)
        applied queued reset)
    (cl-letf (((symbol-function 'mevedel-compact-target--apply)
               (lambda (&rest args) (setq applied args)))
              ((symbol-function
                'mevedel-compact-target-file-reference-reminder-body)
               (lambda (_session _turns _auto) "remember files"))
              ((symbol-function 'mevedel-reminders-rearm-plan-reference)
               (lambda (_session) (push 'plan-reference reset)))
              ((symbol-function 'mevedel-session-enqueue-pending-reminder)
               (lambda (_session reminder) (push reminder queued))))
      (let ((mevedel-compact-target-current-request-reminder nil))
        (mevedel-compact-target--main-apply
         (list :session session) "summary" "tail" "pending" nil t 2)
        (should (equal '("summary" "tail" "pending" nil nil) applied))
        (should (equal "remember files"
                       mevedel-compact-target-current-request-reminder))
        ;; The one-shot accepted-plan reference may re-fire after its
        ;; delivering turn was summarized away.
        (should (equal '(plan-reference) reset))
        (should-not queued)
        (setq mevedel-compact-target-current-request-reminder nil)
        (mevedel-compact-target--main-apply
         (list :session session) "summary" "tail" nil nil nil 2)
        (should (equal '("remember files") queued))
        (should-not mevedel-compact-target-current-request-reminder)))))

(mevedel-deftest mevedel-compact-target--main-start ()
  ,test
  (test)
  :doc "shows main-session compaction progress in the view"
  (let ((view-buffer (generate-new-buffer " *mevedel-compact-view*"))
        spinner)
    (unwind-protect
        (let ((mevedel--view-buffer view-buffer))
          (cl-letf (((symbol-function 'mevedel-view--update-spinner)
                     (lambda (text) (setq spinner text))))
            (mevedel-compact-target--main-start nil))
          (should (equal "Compacting..." spinner)))
      (kill-buffer view-buffer))))

(mevedel-deftest mevedel-compact-target--agent-start ()
  ,test
  (test)
  :doc "records and displays agent compaction progress"
  (let ((invocation (mevedel-agent-invocation-create
                     (mevedel-agent--create :name "explorer")))
        activity status)
    (cl-letf (((symbol-function 'mevedel-agent-conversation-record-activity)
               (lambda (_invocation value) (setq activity value)))
              ((symbol-function 'gptel--update-status)
               (lambda (value &optional _face) (setq status value))))
      (mevedel-compact-target--agent-start (list :invocation invocation)))
    (should (equal '(:type status :summary "Compacting...") activity))
    (should (equal " Compacting..." status))))

(mevedel-deftest mevedel-compact-target-begin-root-context-epoch ()
  ,test
  (test)
  :doc "manual compaction leaves compact-start context for the next input"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "compact-epoch" :root "/tmp"
                     :name "compact-epoch"))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *mevedel-compact-epoch*"))
         source)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "Transcript\n"))
          (cl-letf (((symbol-function 'mevedel--run-session-start-hooks)
                     (lambda (value)
                       (setq source value)
                       (mevedel-hooks-record-session-context
                        session '(:additional-context ("fresh context"))
                        'SessionStart))))
            (mevedel-compact-target-begin-root-context-epoch
             (list :buffer buffer :session session
                   :begin-context-epoch t)
             nil))
          (should (equal "compact" source))
          (should (mevedel-session-hook-context-pending session))
          (with-current-buffer buffer
            (should-not mevedel-compact-target-current-request-hook-context)
            (should-not (string-match-p "fresh context" (buffer-string)))))
      (kill-buffer buffer)))

  :doc "automatic compaction consumes compact-start context into its request"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "compact-auto-epoch" :root "/tmp"
                     :name "compact-auto-epoch"))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *mevedel-compact-auto-epoch*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "Pending prompt\n"))
          (cl-letf (((symbol-function 'mevedel--run-session-start-hooks)
                     (lambda (_source)
                       (mevedel-hooks-record-session-context
                        session '(:additional-context ("fresh context"))
                        'SessionStart))))
            (mevedel-compact-target-begin-root-context-epoch
             (list :buffer buffer :session session
                   :begin-context-epoch t)
             t))
          (should-not (mevedel-session-hook-context-pending session))
          (with-current-buffer buffer
            (should (string-match-p
                     "fresh context"
                     mevedel-compact-target-current-request-hook-context))
            (should (string-match-p "fresh context" (buffer-string)))))
      (kill-buffer buffer)))

  :doc "retained-agent compaction does not begin a root context epoch"
  (let ((buffer (generate-new-buffer " *mevedel-agent-compact-epoch*"))
        called)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--run-session-start-hooks)
                   (lambda (_source) (setq called t))))
          (mevedel-compact-target-begin-root-context-epoch
           (list :buffer buffer :session 'session :invocation 'agent) t)
          (should-not called))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-compact-target--main-complete ()
  ,test
  (test)
  :doc "rerenders the main view and stops manual request progress"
  (let ((view-buffer (generate-new-buffer " *mevedel-compact-view*"))
        (renders 0) (stops 0))
    (unwind-protect
        (let ((mevedel--view-buffer view-buffer))
          (cl-letf (((symbol-function 'mevedel-view--full-rerender)
                     (lambda () (cl-incf renders)))
                    ((symbol-function 'mevedel-view--stop-request-progress)
                     (lambda () (cl-incf stops))))
            (mevedel-compact-target--main-complete nil nil)
            (mevedel-compact-target--main-complete nil t))
          (should (= 2 renders))
          (should (= 1 stops)))
      (kill-buffer view-buffer)))

  :doc "real compaction redraw preserves a multiline leading-> draft and point"
  (mevedel-view-test--with-buffers
    (let ((draft "> quoted\nsecond line")
          (point-offset 4))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (mevedel-view-test--insert-data data-buf "Response.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft draft point-offset))
      (with-current-buffer data-buf
        (mevedel-compact-target--main-complete nil t))
      (with-current-buffer view-buf
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point)
                   (+ (mevedel-view--input-start) point-offset)))))))

(mevedel-deftest mevedel-compact-target--agent-complete ()
  ,test
  (test)
  :doc "restores ordinary agent continuation status"
  (let ((invocation (mevedel-agent-invocation-create
                     (mevedel-agent--create :name "explorer")))
        activity status)
    (cl-letf (((symbol-function 'mevedel-agent-conversation-record-activity)
               (lambda (_invocation value) (setq activity value)))
              ((symbol-function 'gptel--update-status)
               (lambda (value &optional _face) (setq status value))))
      (mevedel-compact-target--agent-complete (list :invocation invocation) t))
    (should (equal '(:type status :summary "waiting") activity))
    (should (equal " Calling Agent..." status))))

(mevedel-deftest mevedel-compact-target-main-target ()
  ,test
  (test)
  :doc "builds the complete adapter for the active persisted segment"
  (test-mevedel-compact--with-persisted-buffer (buffer session)
    (insert "Prompt.\n")
    (let ((start (point)))
      (insert "Response.\n")
      (put-text-property start (point) 'gptel 'response))
    (let ((target (mevedel-compact-target-main-target)))
      (should (eq buffer (plist-get target :buffer)))
      (should (eq session (plist-get target :session)))
      (should (plist-get target :eligible-p))
      (dolist (operation '(:apply :start :complete :resume :fail))
        (should (functionp (plist-get target operation)))))))

(mevedel-deftest mevedel-compact-target-call ()
  ,test
  (test)
  :doc "passes the target and arguments to its selected operation"
  (let* ((target (list :apply (lambda (self one two)
                                (list self one two))))
         (result (mevedel-compact-target-call target :apply 1 2)))
    (should (eq target (car result)))
    (should (equal '(1 2) (cdr result)))
    (should-error (mevedel-compact-target-call target :fail)
                  :type 'error)))

(mevedel-deftest mevedel-compact-target-current-persisted-p ()
  ,test
  (test)
  :doc "requires current buffer to be the session's active segment"
  (let* ((tempdir (make-temp-file "mevedel-compact-persisted-" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "compact-persisted" tempdir "compact-persisted"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) tempdir)
          (setf (mevedel-session-current-segment session) 2)
          (with-temp-buffer
            (setq buffer-file-name
                  (mevedel-session-artifacts-segment-path tempdir 2))
            (setq-local mevedel--session session)
            (should (mevedel-compact-target-current-persisted-p)))
          (with-temp-buffer
            (setq buffer-file-name
                  (mevedel-session-artifacts-segment-path tempdir 1))
            (setq-local mevedel--session session)
            (should-not (mevedel-compact-target-current-persisted-p)))
          (with-temp-buffer
            (setq buffer-file-name "aliased-segment")
            (setq-local mevedel--session session)
            (cl-letf (((symbol-function 'mevedel--same-file-p)
                       (lambda (_left _right) t)))
              (should (mevedel-compact-target-current-persisted-p)))))
      (mevedel-workspace-clear-registry)
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-compact-target--apply ()
  ,test
  (test)
  :doc "rotates without carrying Goal state and includes hook audits"
  (let* ((tempdir (make-temp-file "mevedel-compact-apply-" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "compact-apply" tempdir "compact-apply"))
         (session (mevedel-session-create "main" workspace))
         (steering
          (mevedel-session-enqueue-pending-input
           session 'steering '(:input "steer after compact")))
         (follow-up
          (mevedel-session-enqueue-pending-input
           session 'follow-up '(:input "turn after compact")))
         (execution-state (mevedel-execution--state-for-session session))
         (buffer (generate-new-buffer " *mevedel-compact-apply*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (insert "Original transcript\n")
          (let ((begin (point)))
            (insert "Running Bash\n")
            (insert
             (propertize
              (mevedel-tool-render-data-format
               '(:execution-id "exec-000001" :state running
                 :live-execution-p t)
               "archived-call")
              'gptel '(tool . "archived-call")))
            (put-text-property begin (point) 'gptel
                               '(tool . "archived-call")))
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session)
                (mevedel-goal--create
                 :id "goal-compact" :objective "Finish the work"
                 :status 'active :tokens-used 0 :time-used-seconds 0
                 :turns-run 0))
          (mevedel-session-set-pending-input-paused session t)
          (mevedel-session-set-pending-input-failure-paused session t)
          (mevedel-session-artifacts-ensure-files session buffer)
          (let* ((plan
                  (mevedel-execution-transcript-prepare-archive
                   buffer '("archived-call")))
                 (target
                  (list :buffer buffer :session session
                        :execution-archive-plan plan)))
            (mevedel-compact-target--main-apply
             target "summary" "tail" "pending"
             (list '(:type compact-context
                     :event "PreCompact"
                     :context "compact note"))
             nil 0))
          (should (eq execution-state
                      (mevedel-session-execution-state session)))
          (should (equal (list steering)
                         (mevedel-session-pending-steering session)))
          (should (equal (list follow-up)
                         (mevedel-session-pending-follow-ups session)))
          (should (mevedel-session-pending-input-paused session))
          (should
           (mevedel-session-pending-input-failure-paused session))
          (should (= 2 (mevedel-session-current-segment session)))
          (should (string-match-p "summary" (buffer-string)))
          (should (string-match "<!-- mevedel-hook-audit -->"
                                (buffer-string)))
          (should (eq 'mevedel-hook-audit
                      (get-text-property (match-beginning 0)
                                         'gptel (buffer-string))))
          (should (string-match-p "tail" (buffer-string)))
          (should (string-match-p "pending\n\\'" (buffer-string)))
          (should-not (mevedel-session-pending-reminders session))
          (let* ((ids
                  (mevedel-compact-evidence-archived-tool-use-ids
                   (point-min) (point-max)))
                 (plan
                  (mevedel-execution-transcript-prepare-archive
                   buffer ids))
                 (target
                  (list :buffer buffer :session session
                        :execution-archive-plan plan)))
            (should (equal '("archived-call") ids))
            (should (= 1 (length (plist-get plan :live))))
            (mevedel-compact-target--main-apply
             target "summary again" "tail again" "pending\n"
             nil nil 0))
          (should (= 3 (mevedel-session-current-segment session)))
          (ert-info ("archive survives repeated compaction")
            (should (= 1
                       (length
                        (mevedel-transcript-audit-records
                         (buffer-string) 'execution-archive)))))
          (mevedel-view-stream-handle-execution-event
           (list :type 'terminal :session session :data-buffer buffer
                 :owner "/root" :tool-use-id "archived-call"
                 :facts '(:state completed :outcome success :exit-code 0)
                 :whole-output "done"))
          (ert-info ("terminal event replaces the live archive")
            (should (= 1
                       (length
                        (mevedel-transcript-audit-records
                         (buffer-string) 'execution-completion)))))
          (let ((segment-path
                 (mevedel-session-artifacts-segment-path
                  (mevedel-session-save-path session) 3)))
            (should (file-exists-p segment-path))
            (with-temp-buffer
              (insert-file-contents segment-path)
              (org-mode)
              (mevedel-transcript-restore-properties)
              (should (string-match-p "summary again" (buffer-string)))
              (should (string-match-p "tail again" (buffer-string)))
              (should (= 1
                         (length
                          (mevedel-transcript-audit-records
                           (buffer-string) 'execution-completion))))
              (should-not (string-match-p "pending" (buffer-string))))))
      (mevedel-session-persistence-lock-release
       (mevedel-session-save-path session) session)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (mevedel-workspace-clear-registry)
      (delete-directory tempdir t))))

(provide 'test-mevedel-compact-target)

;;; test-mevedel-compact-target.el ends here
