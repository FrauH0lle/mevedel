;;; test-mevedel-tool-ui.el --- Tests for mevedel-tool-ui.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-tool-ui)
(require 'gptel)
(require 'mevedel-structs)
(require 'mevedel-agents)
(require 'mevedel-tools)
(require 'mevedel-view)
(require 'mevedel-mentions)
(require 'mevedel-skills)
(require 'mevedel-permission-log)
(require 'mevedel-pipeline)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defun test-tool-ui--read-permission-log (session)
  "Read SESSION's permission log entries."
  (let ((file (mevedel-permission-log-path session))
        entries)
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (push (read (current-buffer)) entries))
          (end-of-file nil))))
    (nreverse entries)))


;;
;;; Ask User

(mevedel-deftest mevedel-tools--ask-format-option ()
  ,test
  (test)
  :doc "plain option:
`mevedel-tools--ask-format-option' leaves options without suffix unchanged"
  (should (equal "Balanced"
                 (mevedel-tools--ask-format-option "Balanced")))
  :doc "terminal recommendation:
`mevedel-tools--ask-format-option' highlights the recommended suffix"
  (let* ((formatted (mevedel-tools--ask-format-option
                     "Balanced (Recommended)"))
         (start (string-match-p (regexp-quote " (Recommended)") formatted)))
    (should (equal "Balanced (Recommended)"
                   (substring-no-properties formatted)))
    (should (eq 'success (get-text-property start 'font-lock-face formatted))))
  :doc "object option:
`mevedel-tools--ask-format-option' formats the object label"
  (let* ((formatted (mevedel-tools--ask-format-option
                     '(:label "Project AGENTS.md (Recommended)"
                       :description "Shared guidance")))
         (start (string-match-p (regexp-quote " (Recommended)") formatted)))
    (should (equal "Project AGENTS.md (Recommended)"
                   (substring-no-properties formatted)))
    (should (eq 'success (get-text-property start 'font-lock-face formatted))))
  :doc "non-terminal recommendation text:
`mevedel-tools--ask-format-option' only treats terminal suffix as recommended"
  (let* ((formatted (mevedel-tools--ask-format-option
                     "Balanced (Recommended) maybe"))
         (start (string-match-p (regexp-quote " (Recommended)") formatted)))
    (should (equal "Balanced (Recommended) maybe"
                   (substring-no-properties formatted)))
    (should-not (get-text-property start 'font-lock-face formatted))))

(mevedel-deftest mevedel-tools--ask-completion-table ()
  ,test
  (test)
  :doc "preserves displayed Ask option order for completing-read frontends"
  (let* ((choices '("Skills + hooks (Recommended)"
                    "Skills only"
                    "Hooks only"
                    "Neither, just instructions"
                    "Custom input"))
         (table (mevedel-tools--ask-completion-table choices))
         (metadata (completion-metadata "" table nil)))
    (should (equal choices (all-completions "" table nil)))
    (should (eq #'identity
                (completion-metadata-get metadata 'display-sort-function)))
    (should (eq #'identity
                (completion-metadata-get metadata 'cycle-sort-function)))))


;;
;;; Agent result formatting

(mevedel-deftest mevedel-tools--agent-result-format ()
  ,test
  (test)
  :doc "escapes nested mailbox delimiters in the result body"
  (let ((body
         (mevedel-tools--agent-result-format
          "verifier--1" "verifier" "Verify"
          (concat "Before.\n"
                  "<agent-result agent-id=\"inner\">\ninner\n</agent-result>\n"
                  "<agent-message from=\"inner\">\nmsg\n</agent-message>\n"
                  "After."))))
    (should (string-match-p
             "\\`<agent-result agent-id=\"verifier--1\""
             body))
    (should (string-match-p "&lt;agent-result agent-id=\"inner\"" body))
    (should (string-match-p "&lt;/agent-result&gt;" body))
    (should (string-match-p "&lt;agent-message from=\"inner\"" body))
    (should (string-match-p "&lt;/agent-message&gt;" body))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (should (= 1 (how-many "<agent-result" (point-min) (point-max))))
      (goto-char (point-min))
      (should (= 1 (how-many "</agent-result>" (point-min)
                             (point-max)))))))

(mevedel-deftest mevedel-tools--ask-user ()
  ,test
  (test)
  :doc "advances after answers and submits from the review page"
  (let ((data-buffer (generate-new-buffer " *mev-ask-data*"))
        (view-buffer (generate-new-buffer " *mev-ask-view*"))
        (choices '("Yes" "No"))
        rendered-body
        rendered-keymap
        result)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--prompt--data-buffer)
                   (lambda () data-buffer))
                  ((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (&optional _data-buffer) view-buffer))
                  ((symbol-function 'mevedel-view--interaction-register)
                   (lambda (descriptor)
                     (setq rendered-body (plist-get descriptor :body))
                     (setq rendered-keymap (plist-get descriptor :keymap))
                     (make-overlay (point-min) (point-min)
                                   (current-buffer) nil t)))
                  ((symbol-function 'mevedel--prompt--register-canceller)
                   #'ignore)
                  ((symbol-function 'completing-read)
                   (lambda (&rest _args)
                     (pop choices))))
          (with-current-buffer view-buffer
            (setq-local mevedel--prompt-overlays nil))
          (mevedel-tools--ask-user
           (lambda (value) (setq result value))
           [(:question "Use cache?" :options ["Yes" "No"])
            (:question "Run tests?" :options ["Yes" "No"])])
          (should (string-match-p "Question 1/2" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "Question 2/2" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "Review Your Answers" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "Q1: Use cache\\?" result))
          (should (string-match-p "A1: Yes" result))
          (should (string-match-p "Q2: Run tests\\?" result))
          (should (string-match-p "A2: No" result)))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))))

  :doc "recommended option remains part of the selected Ask answer"
  (let ((data-buffer (generate-new-buffer " *mev-ask-rec-data*"))
        (view-buffer (generate-new-buffer " *mev-ask-rec-view*"))
        (choices '("Balanced (Recommended)"))
        rendered-body
        rendered-keymap
        result)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--prompt--data-buffer)
                   (lambda () data-buffer))
                  ((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (&optional _data-buffer) view-buffer))
                  ((symbol-function 'mevedel-view--interaction-register)
                   (lambda (descriptor)
                     (setq rendered-body (plist-get descriptor :body))
                     (setq rendered-keymap (plist-get descriptor :keymap))
                     (make-overlay (point-min) (point-min)
                                   (current-buffer) nil t)))
                  ((symbol-function 'mevedel--prompt--register-canceller)
                   #'ignore)
                  ((symbol-function 'completing-read)
                   (lambda (&rest _args)
                     (pop choices))))
          (with-current-buffer view-buffer
            (setq-local mevedel--prompt-overlays nil))
          (mevedel-tools--ask-user
           (lambda (value) (setq result value))
           [(:question "Choose risk profile"
                       :options ["Conservative" "Balanced (Recommended)" "Aggressive"])])
          (should (string-match-p "Balanced (Recommended)"
                                  (substring-no-properties rendered-body)))
          (let ((start (string-match-p (regexp-quote " (Recommended)")
                                       rendered-body)))
            (should (eq 'success
                        (get-text-property start 'font-lock-face rendered-body))))
          (should (string-match-p "Available options:" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "Review Your Answers" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "Q1: Choose risk profile" result))
          (should (string-match-p "A1: Balanced (Recommended)" result)))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))))

  :doc "object options display descriptions and previews but return labels"
  (let ((data-buffer (generate-new-buffer " *mev-ask-obj-data*"))
        (view-buffer (generate-new-buffer " *mev-ask-obj-view*"))
        (choices '("Project AGENTS.md (Recommended)"))
        rendered-body
        rendered-keymap
        result)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--prompt--data-buffer)
                   (lambda () data-buffer))
                  ((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (&optional _data-buffer) view-buffer))
                  ((symbol-function 'mevedel-view--interaction-register)
                   (lambda (descriptor)
                     (setq rendered-body (plist-get descriptor :body))
                     (setq rendered-keymap (plist-get descriptor :keymap))
                     (make-overlay (point-min) (point-min)
                                   (current-buffer) nil t)))
                  ((symbol-function 'mevedel--prompt--register-canceller)
                   #'ignore)
                  ((symbol-function 'completing-read)
                   (lambda (&rest _args)
                     (pop choices))))
          (with-current-buffer view-buffer
            (setq-local mevedel--prompt-overlays nil))
          (mevedel-tools--ask-user
           (lambda (value) (setq result value))
           [(:question "What should I write?"
                       :options [(:label "Project AGENTS.md (Recommended)"
                                  :description "Shared repo guidance"
                                  :preview "# Repository Guidelines\n- Run tests")
                                 (:label "Personal AGENTS.local.md"
                                  :description "Private notes")])])
          (should (string-match-p "Shared repo guidance" rendered-body))
          (should-not (string-match-p "# Repository Guidelines" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "Review Your Answers" rendered-body))
          (should (string-match-p "# Repository Guidelines" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "A1: Project AGENTS.md (Recommended)"
                                  result))
          (should-not (string-match-p "# Repository Guidelines" result)))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))))

  :doc "agent Ask prompt survives parent request cleanup but aborts with agent request"
  (let* ((session (mevedel-session--create :name "main"))
         (data-buffer (generate-new-buffer " *mev-ask-parent-data*"))
         (view-buffer (generate-new-buffer " *mev-ask-parent-view*"))
         (agent-buffer (generate-new-buffer " *mev-ask-agent-data*"))
         (agent (mevedel-agent--create :name "verifier"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "verifier--abc"
               :parent-session session
               :parent-data-buffer data-buffer
               :buffer agent-buffer))
         result)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session)
            (mevedel-request-begin session))
          (mevedel-view--setup view-buffer data-buffer)
          (with-current-buffer agent-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation inv)
            (setq-local mevedel--view-buffer view-buffer)
            (mevedel-request-begin session)
            (mevedel-tools--ask-user
             (lambda (value) (setq result value))
             [(:question "Proceed?" :options ["Yes" "No"])]))
          (with-current-buffer view-buffer
            (should (= 1 (length mevedel--prompt-overlays))))
          (with-current-buffer data-buffer
            (mevedel-request-end))
          (should-not result)
          (with-current-buffer view-buffer
            (should (= 1 (length mevedel--prompt-overlays))))
          (with-current-buffer agent-buffer
            (mevedel-request-end))
          (should (eq 'aborted result))
          (with-current-buffer view-buffer
            (should-not mevedel--prompt-overlays)))
      (when (buffer-live-p agent-buffer) (kill-buffer agent-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "sibling agent Ask prompts in the parent view are cancelled per request"
  (let* ((session (mevedel-session--create :name "main"))
         (data-buffer (generate-new-buffer " *mev-ask-siblings-data*"))
         (view-buffer (generate-new-buffer " *mev-ask-siblings-view*"))
         (agent-buffer-a (generate-new-buffer " *mev-ask-agent-a*"))
         (agent-buffer-b (generate-new-buffer " *mev-ask-agent-b*"))
         (agent (mevedel-agent--create :name "verifier"))
         (inv-a (mevedel-agent-invocation--create
                 :agent agent
                 :agent-id "verifier--a"
                 :parent-session session
                 :parent-data-buffer data-buffer
                 :buffer agent-buffer-a))
         (inv-b (mevedel-agent-invocation--create
                 :agent agent
                 :agent-id "verifier--b"
                 :parent-session session
                 :parent-data-buffer data-buffer
                 :buffer agent-buffer-b))
         result-a
         result-b)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buffer data-buffer)
          (with-current-buffer agent-buffer-a
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation inv-a)
            (setq-local mevedel--view-buffer view-buffer)
            (mevedel-request-begin session)
            (mevedel-tools--ask-user
             (lambda (value) (setq result-a value))
             [(:question "A?" :options ["Yes" "No"])]))
          (with-current-buffer agent-buffer-b
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation inv-b)
            (setq-local mevedel--view-buffer view-buffer)
            (mevedel-request-begin session)
            (mevedel-tools--ask-user
             (lambda (value) (setq result-b value))
             [(:question "B?" :options ["Yes" "No"])]))
          (with-current-buffer view-buffer
            (should (= 2 (length mevedel--prompt-overlays))))
          (with-current-buffer agent-buffer-a
            (mevedel-request-end))
          (should (eq 'aborted result-a))
          (should-not result-b)
          (with-current-buffer view-buffer
            (should (= 1 (length mevedel--prompt-overlays))))
          (with-current-buffer agent-buffer-b
            (mevedel-request-end))
          (should (eq 'aborted result-b))
          (with-current-buffer view-buffer
            (should-not mevedel--prompt-overlays)))
      (when (buffer-live-p agent-buffer-b) (kill-buffer agent-buffer-b))
      (when (buffer-live-p agent-buffer-a) (kill-buffer agent-buffer-a))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer)))))


;;
;;; RequestAccess

(mevedel-deftest mevedel-tools--request-access/logging
  ()
  ,test
  (test)
  :doc "writes create and resolve diagnostics for access prompts"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-access-log-" t)))
         (session (mevedel-session--create :name "main"
                                           :save-path dir))
         (data-buffer (generate-new-buffer " *mev-access-data*"))
         (root "/tmp/outside")
         captured-callback
         outcome)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--pending-access-requests nil))
          (cl-letf (((symbol-function
                      'mevedel-workspace--file-in-allowed-roots-p)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'mevedel--prompt-user-for-access)
                     (lambda (_root _reason callback)
                       (setq captured-callback callback)
                       (let ((ov (make-overlay (point-min) (point-min)
                                               (current-buffer) nil t)))
                         (overlay-put ov 'mevedel-view-interaction-id
                                      'request-id)
                         ov))))
            (with-current-buffer data-buffer
              (mevedel-tools--request-access
               root "inspect dependency"
               (lambda (value) (setq outcome value)))))
          (should captured-callback)
          (funcall captured-callback 'deny)
          (should (eq 'deny outcome))
          (let ((entries (test-tool-ui--read-permission-log session)))
            (should (= 2 (length entries)))
            (should (eq 'request-access-created
                        (plist-get (nth 0 entries) :event)))
            (should (eq 'request-access-resolved
                        (plist-get (nth 1 entries) :event)))
            (should (equal root (plist-get (nth 0 entries) :directory)))
            (should (eq 'request-id
                        (plist-get (nth 0 entries) :interaction-id)))
            (should (eq 'deny (plist-get (nth 1 entries) :outcome)))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))
      (delete-directory dir t)))
  :doc "full RequestAccess pipeline logs tool decision and access outcome"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-access-log-" t)))
         (workspace-root (file-name-as-directory
                          (make-temp-file "mevedel-access-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "mevedel-access-outside-" t)))
         (ws (mevedel-workspace--create
              :type 'project :id "root" :root workspace-root
              :name "root" :file-cache nil))
         (session (mevedel-session--create
                   :name "main" :workspace ws :save-path dir))
         (data-buffer (generate-new-buffer " *mev-access-pipeline-data*"))
         captured-callback
         result)
    (unwind-protect
        (with-current-buffer data-buffer
          (setq-local mevedel--session session)
          (cl-letf (((symbol-function
                      'mevedel-workspace--file-in-allowed-roots-p)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'mevedel--all-allowed-roots)
                     (lambda (&optional _buffer) (list workspace-root)))
                    ((symbol-function 'mevedel--prompt-user-for-access)
                     (lambda (_root _reason callback)
                       (setq captured-callback callback)
                       (make-overlay (point-min) (point-min)
                                     (current-buffer) nil t)))
                    ((symbol-function 'mevedel-add-project-root)
                     (lambda (&rest _args) nil)))
            (mevedel-pipeline-run-tool
             (mevedel-tool--create
              :name "RequestAccess"
              :handler #'mevedel-tool-ui--request-access
              :args '((directory string :required "Directory")
                      (reason string :required "Reason"))
              :async-p t
              :check-permission (lambda (_tool _args) 'allow)
              :get-path (lambda (args) (plist-get args :directory))
              :read-only-p t)
             (lambda (value) (setq result value))
             (list :directory outside :reason "inspect dependency"))
            (should captured-callback)
            (funcall captured-callback 'approve)
            (should (string-match-p "Access granted" result))
            (let ((entries (test-tool-ui--read-permission-log session)))
              (should (= 3 (length entries)))
              (should (eq 'permission-decision
                          (plist-get (nth 0 entries) :event)))
              (should (equal "RequestAccess"
                             (plist-get (nth 0 entries) :tool-name)))
              (should (eq 'allow (plist-get (nth 0 entries) :outcome)))
              (should (eq 'tool-slot (plist-get (nth 0 entries) :via)))
              (should (eq 'request-access-created
                          (plist-get (nth 1 entries) :event)))
              (should (eq 'request-access-resolved
                          (plist-get (nth 2 entries) :event)))
              (should (eq 'approve (plist-get (nth 2 entries) :outcome))))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))
      (when (file-directory-p dir)
        (delete-directory dir t))
      (when (file-directory-p workspace-root)
        (delete-directory workspace-root t))
      (when (file-directory-p outside)
        (delete-directory outside t)))))


;;
;;; Agent stop control

(mevedel-deftest mevedel-tools-stop-agent
  (:doc "stops a running background agent and resumes parent BWAIT")
  (let* ((session (mevedel-session--create :name "main"))
         (tempdir (file-name-as-directory
                   (make-temp-file "mevedel-stop-agent-bg" t)))
         (rel-path "agents/reviewer--stopped.chat.org")
         (abs-path (expand-file-name rel-path tempdir))
         (parent-buf (generate-new-buffer " *mev-stop-parent*"))
         (agent-buf (generate-new-buffer " *mev-stop-agent*"))
         (agent (mevedel-agent--create :name "reviewer"
                                       :description "Review"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "reviewer--735123142194f47363852069e3f42083"
               :description "review current diff"
               :parent-session session
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p t
               :transcript-relative-path rel-path
               :transcript-status 'running))
         (parent-fsm (gptel-make-fsm
                      :info (list :buffer parent-buf)
                      :handlers nil
                      :state 'BWAIT))
         (child-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv
                                 :callback
                                 (lambda (resp _info)
                                   (when (eq resp 'abort)
                                     (setf
                                      (mevedel-agent-invocation-transcript-status
                                       inv)
                                      'aborted)
                                     (mevedel-tools--complete-background-agent
                                      inv "abort callback body"))))
                     :handlers nil
                     :state 'WAIT))
         (gptel--request-alist
          (list (cons 'fake-process (cons child-fsm #'ignore))))
         aborted
         result)
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) tempdir)
          (make-directory (file-name-directory abs-path) t)
          (with-temp-file abs-path
            (insert "saved transcript"))
          (setf (mevedel-agent-invocation-parent-fsm inv) parent-fsm)
          (setf (mevedel-session-background-agents session)
                '("reviewer--735123142194f47363852069e3f42083"))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-tools--agents-fsm
                        `(("reviewer--735123142194f47363852069e3f42083"
                           . ,child-fsm))))
          (cl-letf (((symbol-function 'gptel-abort)
                     (lambda (&optional _buf)
                       (setq aborted t)
                       (funcall (plist-get (gptel-fsm-info child-fsm)
                                           :callback)
                                'abort (gptel-fsm-info child-fsm))))
                    ((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t))
                    ((symbol-function 'mevedel-agent-exec--handle-update)
                     (lambda (_invocation) nil))
                    ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                     (lambda (_invocation _status) nil))
                    ((symbol-function
                      'mevedel-session-persistence--update-transcript-entry)
                     (lambda (_session _agent-id _updates) nil))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t)))
            (setq result
                  (with-current-buffer parent-buf
                    (mevedel-tools-stop-agent
                     "reviewer--73512314" "stranded in BWAIT"))))
          (should (eq 'running (plist-get result :previous-status)))
          (should (eq 'aborted (plist-get result :status)))
          (should (plist-get result :resumed-bwait))
          (should (eq 'aborted
                      (mevedel-agent-invocation-transcript-status inv)))
          (should (equal "stranded in BWAIT"
                         (mevedel-agent-invocation-terminal-reason inv)))
          (should (null (mevedel-session-background-agents session)))
          (should (= 1 (length (mevedel-session-messages session))))
          (let ((body (plist-get (car (mevedel-session-messages session))
                                 :body)))
            (should (string-match-p "<agent-result agent-id=\"reviewer--735123142194f47363852069e3f42083\"" body))
            (should (string-match-p "was stopped" body))
            (should (string-match-p "stranded in BWAIT" body))
            (should (string-match-p (regexp-quote (format "Transcript: %s" abs-path))
                                    body))
            (should (string-match-p
                     (regexp-quote (format "Read(file_path=%S)" abs-path))
                     body)))
          (with-current-buffer parent-buf
            (should-not (assoc "reviewer--735123142194f47363852069e3f42083"
                               mevedel-tools--agents-fsm)))
          (should (eq 'WAIT (gptel-fsm-state parent-fsm)))
          (should aborted))
      (when (file-directory-p tempdir) (delete-directory tempdir t))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-tools-stop-agent/foreground-completes-parent-tool
  (:doc "stops a foreground agent by completing the parent Agent tool callback")
  (let* ((session (mevedel-session--create :name "main"))
         (tempdir (file-name-as-directory
                   (make-temp-file "mevedel-stop-agent-fg" t)))
         (rel-path "agents/verifier--stopped.chat.org")
         (abs-path (expand-file-name rel-path tempdir))
         (parent-buf (generate-new-buffer " *mev-stop-fg-parent*"))
         (agent-buf (generate-new-buffer " *mev-stop-fg-agent*"))
         (agent-id "verifier--foreground1234567890abcdef123456")
         (agent (mevedel-agent--create :name "verifier"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id agent-id
               :description "verify current diff"
               :parent-session session
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-relative-path rel-path
               :transcript-status 'running))
         (child-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv
                                 :callback #'ignore)
                     :handlers nil
                     :state 'WAIT))
         (gptel--request-alist nil)
         (callback-count 0)
         parent-result
         result)
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) tempdir)
          (make-directory (file-name-directory abs-path) t)
          (with-temp-file abs-path
            (insert "saved transcript"))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-tools--agents-fsm
                        `((,agent-id . ,child-fsm))))
          (setf
           (mevedel-agent-invocation-parent-tool-callback inv)
           (lambda (response &rest _)
             (unless
                 (mevedel-agent-invocation-foreground-result-reported-p
                  inv)
               (cl-incf callback-count)
               (setq parent-result response)
               (setf
                (mevedel-agent-invocation-foreground-result-reported-p
                 inv)
                t)
               (mevedel-tools--remove-agent-registry-entry
                inv agent-id parent-buf))))
          (cl-letf (((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t))
                    ((symbol-function 'mevedel-agent-exec--handle-update)
                     (lambda (_invocation) nil))
                    ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                     (lambda (_invocation _status) nil))
                    ((symbol-function
                      'mevedel-session-persistence--update-transcript-entry)
                     (lambda (_session _agent-id _updates) nil))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t)))
            (setq result
                  (with-current-buffer parent-buf
                    (mevedel-tools-stop-agent
                     agent-id "no longer needed"))))
          (should (= 1 callback-count))
          (should (string-match-p "was stopped" parent-result))
          (should (string-match-p "no longer needed" parent-result))
          (should (string-match-p (regexp-quote (format "Transcript: %s" abs-path))
                                  parent-result))
          (should (string-match-p
                   (regexp-quote (format "Read(file_path=%S)" abs-path))
                   parent-result))
          (should (plist-get result :completed-tool-callback))
          (should-not (plist-get result :resumed-bwait))
          (should (eq 'aborted
                      (mevedel-agent-invocation-transcript-status inv)))
          (should (null (mevedel-session-messages session)))
          (with-current-buffer parent-buf
            (should-not (assoc agent-id mevedel-tools--agents-fsm))))
      (when (file-directory-p tempdir) (delete-directory tempdir t))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-tools-stop-agent/foreground-no-transcript-inlines-partial
  (:doc "stopped foreground agent inlines recovered partial when transcript is absent")
  (let* ((session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-stop-fg-partial-parent*"))
         (agent-buf (generate-new-buffer " *mev-stop-fg-partial-agent*"))
         (agent-id "verifier--partial1234567890abcdef123456")
         (agent (mevedel-agent--create :name "verifier"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id agent-id
               :description "verify current diff"
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (child-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv
                                 :callback #'ignore)
                     :handlers nil
                     :state 'WAIT))
         (gptel--request-alist nil)
         parent-result)
    (unwind-protect
        (progn
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-tools--agents-fsm
                        `((,agent-id . ,child-fsm))))
          (setf
           (mevedel-agent-invocation-parent-tool-callback inv)
           (lambda (response &rest _)
             (setq parent-result response)
             (setf
              (mevedel-agent-invocation-foreground-result-reported-p inv)
              t)
             (mevedel-tools--remove-agent-registry-entry
              inv agent-id parent-buf)))
          (cl-letf (((symbol-function 'mevedel-agent-exec--final-response-text)
                     (lambda (_invocation)
                       "partial analysis from stopped verifier"))
                    ((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t))
                    ((symbol-function 'mevedel-agent-exec--handle-update)
                     (lambda (_invocation) nil))
                    ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                     (lambda (_invocation _status) nil))
                    ((symbol-function
                      'mevedel-session-persistence--update-transcript-entry)
                     (lambda (_session _agent-id _updates) nil))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t)))
            (with-current-buffer parent-buf
              (mevedel-tools-stop-agent agent-id "operator stop")))
          (should (string-match-p "Partial response recovered" parent-result))
          (should (string-match-p "partial analysis from stopped verifier"
                                  parent-result))
          (should-not (string-match-p "Transcript:" parent-result)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-tools-stop-agent/foreground-abort-race-is-once
  (:doc "foreground stop wins the abort race and reports to parent once")
  (let* ((session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-stop-fg-race-parent*"))
         (agent-buf (generate-new-buffer " *mev-stop-fg-race-agent*"))
         (agent-id "verifier--race1234567890abcdef1234567890")
         (agent (mevedel-agent--create :name "verifier"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id agent-id
               :description "verify current diff"
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (child-fsm nil)
         (gptel--request-alist nil)
         (callback-count 0)
         parent-result
         aborted
         result)
    (unwind-protect
        (progn
          (setf
           (mevedel-agent-invocation-parent-tool-callback inv)
           (lambda (response &rest _)
             (unless
                 (mevedel-agent-invocation-foreground-result-reported-p
                  inv)
               (cl-incf callback-count)
               (setq parent-result response)
               (setf
                (mevedel-agent-invocation-foreground-result-reported-p
                 inv)
                t)
               (mevedel-tools--remove-agent-registry-entry
                inv agent-id parent-buf))))
          (setq child-fsm
                (gptel-make-fsm
                 :info (list :buffer agent-buf
                             :mevedel-agent-invocation inv
                             :callback
                             (lambda (resp _info)
                               (when (eq resp 'abort)
                                 (funcall
                                  (mevedel-agent-invocation-parent-tool-callback
                                   inv)
                                  "Error: generic abort"))))
                 :handlers nil
                 :state 'WAIT))
          (setq gptel--request-alist
                (list (cons 'fake-process (cons child-fsm #'ignore))))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-tools--agents-fsm
                        `((,agent-id . ,child-fsm))))
          (cl-letf (((symbol-function 'gptel-abort)
                     (lambda (&optional _buf)
                       (setq aborted t)
                       (funcall (plist-get (gptel-fsm-info child-fsm)
                                           :callback)
                                'abort (gptel-fsm-info child-fsm))))
                    ((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t))
                    ((symbol-function 'mevedel-agent-exec--handle-update)
                     (lambda (_invocation) nil))
                    ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                     (lambda (_invocation _status) nil))
                    ((symbol-function
                      'mevedel-session-persistence--update-transcript-entry)
                     (lambda (_session _agent-id _updates) nil))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t)))
            (setq result
                  (with-current-buffer parent-buf
                    (mevedel-tools-stop-agent
                     agent-id "operator stop"))))
          (should aborted)
          (should (= 1 callback-count))
          (should (string-match-p "operator stop" parent-result))
          (should-not (string-match-p "generic abort" parent-result))
          (should (plist-get result :completed-tool-callback))
          (with-current-buffer parent-buf
            (should-not (assoc agent-id mevedel-tools--agents-fsm))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-tools-stop-agent/recovers-parent-fsm-from-request-alist
  (:doc "resumes parent BWAIT even when invocation lost its parent-fsm slot")
  (let* ((session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-stop-recover-parent*"))
         (agent-buf (generate-new-buffer " *mev-stop-recover-agent*"))
         (agent (mevedel-agent--create :name "verifier"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "verifier--cf9dca9d45d108008685cd1c40a86a09"
               :description "verify current diff"
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p t
               :transcript-status 'running))
         (parent-fsm (gptel-make-fsm
                      :info (list :buffer parent-buf)
                      :state 'BWAIT))
         (child-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv
                                 :callback #'ignore)
                     :state 'WAIT))
         (gptel--request-alist
          (list (cons 'parent-process (cons parent-fsm #'ignore))))
         result)
    (unwind-protect
        (progn
          (setf (mevedel-session-background-agents session)
                '("verifier--cf9dca9d45d108008685cd1c40a86a09"))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-tools--agents-fsm
                        `(("verifier--cf9dca9d45d108008685cd1c40a86a09"
                           . ,child-fsm))))
          (cl-letf (((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t))
                    ((symbol-function 'mevedel-agent-exec--handle-update)
                     (lambda (_invocation) nil))
                    ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                     (lambda (_invocation _status) nil))
                    ((symbol-function 'gptel--fsm-transition)
                     (lambda (fsm state)
                       (setf (gptel-fsm-state fsm) state)))
                    ((symbol-function
                      'mevedel-session-persistence--update-transcript-entry)
                     (lambda (_session _agent-id _updates) nil))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t)))
            (setq result
                  (with-current-buffer parent-buf
                    (mevedel-tools-stop-agent
                     "verifier--cf9dca9d" "recover parent"))))
          (should (eq parent-fsm
                      (mevedel-agent-invocation-parent-fsm inv)))
          (should (plist-get result :resumed-bwait))
          (should (eq 'WAIT (gptel-fsm-state parent-fsm)))
          (should (null (mevedel-session-background-agents session)))
          (should (= 1 (length (mevedel-session-messages session)))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-stop-agent/from-view-buffer
  (:doc "resolves the data-buffer registry when invoked from a view buffer")
  (let* ((session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-stop-view-parent*"))
         (view-buf (generate-new-buffer " *mev-stop-view*"))
         (agent-buf (generate-new-buffer " *mev-stop-view-agent*"))
         (agent (mevedel-agent--create :name "verifier"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "verifier--cf9dca9d45d108008685cd1c40a86a09"
               :description "verify current diff"
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p t
               :transcript-status 'running))
         (parent-fsm (gptel-make-fsm
                      :info (list :buffer parent-buf)
                      :state 'BWAIT))
         (child-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv
                                 :callback #'ignore)
                     :state 'WAIT))
         result)
    (unwind-protect
        (progn
          (setf (mevedel-agent-invocation-parent-fsm inv) parent-fsm)
          (setf (mevedel-session-background-agents session)
                '("verifier--cf9dca9d45d108008685cd1c40a86a09"))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-tools--agents-fsm
                        `(("verifier--cf9dca9d45d108008685cd1c40a86a09"
                           . ,child-fsm))))
          (with-current-buffer view-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--data-buffer parent-buf))
          (cl-letf (((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t))
                    ((symbol-function 'mevedel-agent-exec--handle-update)
                     (lambda (_invocation) nil))
                    ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                     (lambda (_invocation _status) nil))
                    ((symbol-function 'gptel--fsm-transition)
                     (lambda (fsm state)
                       (setf (gptel-fsm-state fsm) state)))
                    ((symbol-function
                      'mevedel-session-persistence--update-transcript-entry)
                     (lambda (_session _agent-id _updates) nil))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t)))
            (setq result
                  (with-current-buffer view-buf
                    (mevedel-stop-agent
                     "verifier--cf9dca9d" "stopped from view"))))
          (should (eq 'running (plist-get result :previous-status)))
          (should (eq 'aborted (plist-get result :status)))
          (should (plist-get result :resumed-bwait))
          (should (null (mevedel-session-background-agents session)))
          (should (= 1 (length (mevedel-session-messages session))))
          (with-current-buffer parent-buf
            (should-not (assoc "verifier--cf9dca9d45d108008685cd1c40a86a09"
                               mevedel-tools--agents-fsm)))
          (should (eq 'WAIT (gptel-fsm-state parent-fsm))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-tools--resolve-agent-stop-target
  (:doc "accepts exact ids and rejects ambiguous displayed short ids")
  (let* ((parent-buf (generate-new-buffer " *mev-stop-resolve-parent*"))
         (agent-buf-a (generate-new-buffer " *mev-stop-resolve-agent-a*"))
         (agent-buf-b (generate-new-buffer " *mev-stop-resolve-agent-b*"))
         (agent (mevedel-agent--create :name "reviewer"))
         (inv-a (mevedel-agent-invocation--create
                 :agent agent
                 :agent-id "reviewer--aaaaaaaa111111111111111111111111"
                 :buffer agent-buf-a
                 :transcript-status 'running))
         (inv-b (mevedel-agent-invocation--create
                 :agent agent
                 :agent-id "reviewer--aaaaaaaa222222222222222222222222"
                 :buffer agent-buf-b
                 :transcript-status 'running))
         (fsm-a (gptel-make-fsm
                 :info (list :buffer agent-buf-a
                             :mevedel-agent-invocation inv-a)
                 :state 'WAIT))
         (fsm-b (gptel-make-fsm
                 :info (list :buffer agent-buf-b
                             :mevedel-agent-invocation inv-b)
                 :state 'WAIT)))
    (unwind-protect
        (progn
          (with-current-buffer parent-buf
            (setq-local mevedel-tools--agents-fsm
                        `(("reviewer--aaaaaaaa111111111111111111111111"
                           . ,fsm-a)
                          ("reviewer--aaaaaaaa222222222222222222222222"
                           . ,fsm-b))))
	          (should (equal "reviewer--aaaaaaaa111111111111111111111111"
	                         (car (mevedel-tools--resolve-agent-stop-target
	                               "reviewer--aaaaaaaa111111111111111111111111"
	                               parent-buf))))
	          (should-error
	           (mevedel-tools--resolve-agent-stop-target
	            "reviewer--aaaaaaaa" parent-buf)))
	      (when (buffer-live-p agent-buf-a) (kill-buffer agent-buf-a))
	      (when (buffer-live-p agent-buf-b) (kill-buffer agent-buf-b))
	      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-tools--foreground-watchdog-expire ()
  ,test
  (test)
  :doc "stops a foreground agent when its progress snapshot is unchanged"
  (let* ((mevedel-tools--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent*"))
         (agent-id "verifier--fg-watchdog")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (clock 100.0)
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args) 'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-tools-stop-agent)
                     (lambda (id reason parent)
                       (setq stopped (list id reason parent)))))
            (mevedel-tools--foreground-watchdog-arm inv)
            (setq clock 110.0)
            (mevedel-tools--foreground-watchdog-expire agent-id))
          (should (equal agent-id (car stopped)))
          (should (string-match-p "no progress" (cadr stopped)))
          (should (eq parent-buf (caddr stopped)))
          (should-not (gethash agent-id
                               mevedel-tools--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "reschedules instead of stopping when buffer content progresses"
  (let* ((mevedel-tools--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-progress*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-progress*"))
         (agent-id "verifier--fg-progress")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (clock 100.0)
         (timer-count 0)
         delays
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (delay &rest _args)
                       (cl-incf timer-count)
                       (push delay delays)
                       'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-tools-stop-agent)
                     (lambda (&rest args) (setq stopped args))))
            (mevedel-tools--foreground-watchdog-arm inv)
            (with-current-buffer agent-buf
              (insert "\npartial response"))
            (setq clock 110.0)
            (mevedel-tools--foreground-watchdog-expire agent-id))
          (should-not stopped)
          (should (= 2 timer-count))
          (should (equal '(10 10) (nreverse delays)))
          (should (gethash agent-id mevedel-tools--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "reschedules from latest activity time instead of launch time"
  (let* ((mevedel-tools--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-activity*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-activity*"))
         (agent-id "verifier--fg-activity")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (clock 100.0)
         delays
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (delay &rest _args)
                       (push delay delays)
                       'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-tools-stop-agent)
                     (lambda (&rest args) (setq stopped args))))
            (mevedel-tools--foreground-watchdog-arm inv)
            (setf (mevedel-agent-invocation-activity inv)
                  '((:type tool-start :time 105.0)))
            (setq clock 109.0)
            (mevedel-tools--foreground-watchdog-expire agent-id))
          (should-not stopped)
          (should (equal '(10 6) (nreverse delays)))
          (should (gethash agent-id mevedel-tools--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "early foreground watchdog timer reschedules remaining grace"
  (let* ((mevedel-tools--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-early*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-early*"))
         (agent-id "verifier--fg-early")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (clock 100.0)
         delays
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (delay &rest _args)
                       (push delay delays)
                       'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-tools-stop-agent)
                     (lambda (&rest args) (setq stopped args))))
            (mevedel-tools--foreground-watchdog-arm inv)
            (setq clock 105.0)
            (mevedel-tools--foreground-watchdog-expire agent-id))
          (should-not stopped)
          (should (equal '(10 5) (nreverse delays)))
          (should (gethash agent-id mevedel-tools--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "records foreground watchdog reschedule reason in transcript metadata"
  (let* ((mevedel-tools--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-log*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-log*"))
         (agent-id "verifier--fg-log")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-session session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (clock 100.0)
         (timer-count 0)
         logged
         stopped)
    (unwind-protect
        (progn
          (setf (mevedel-session-agent-transcripts session)
                `((,agent-id :status running)))
          (with-current-buffer agent-buf
            (insert "prompt"))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args)
                       (cl-incf timer-count)
                       'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq logged (apply #'format fmt args))))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t))
                    ((symbol-function 'mevedel-tools-stop-agent)
                     (lambda (&rest args) (setq stopped args))))
            (mevedel-tools--foreground-watchdog-arm inv)
            (with-current-buffer agent-buf
              (insert "\npartial response"))
            (setq clock 110.0)
            (mevedel-tools--foreground-watchdog-expire agent-id))
          (should-not stopped)
          (should (= 2 timer-count))
          (should (string-match-p "rescheduled" logged))
          (let* ((entry (cdr (assoc agent-id
                                    (mevedel-session-agent-transcripts
                                     session))))
                 (watchdog (plist-get entry :watchdog)))
            (should (eq 'rescheduled (plist-get watchdog :state)))
            (should (eq 'progress (plist-get watchdog :reason)))
            (should (plist-member watchdog :current-size))
            (should (plist-member watchdog :last-size))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "reschedules while the foreground agent is waiting on child work"
  (let* ((mevedel-tools--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-child*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-child*"))
         (agent-id "coordinator--fg-child")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (clock 100.0)
         (timer-count 0)
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args)
                       (cl-incf timer-count)
                       'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-tools-stop-agent)
                     (lambda (&rest args) (setq stopped args))))
            (mevedel-tools--foreground-watchdog-arm inv)
            (mevedel-tools--ctx-push-background-agent inv "explorer--child")
            (setq clock 110.0)
            (mevedel-tools--foreground-watchdog-expire agent-id))
          (should-not stopped)
          (should (= 2 timer-count))
          (should (gethash agent-id mevedel-tools--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "stale tool-pending flag outside TOOL does not suppress watchdog stop"
  (let* ((mevedel-tools--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-stale-tool*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-stale-tool*"))
         (agent-id "verifier--fg-stale-tool")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (agent-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :tool-pending t
                                 :mevedel-agent-invocation inv)
                     :state 'WAIT))
         (clock 100.0)
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (with-current-buffer parent-buf
            (setq-local mevedel-tools--agents-fsm
                        `((,agent-id . ,agent-fsm))))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args) 'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-tools-stop-agent)
                     (lambda (id reason parent)
                       (setq stopped (list id reason parent)))))
            (mevedel-tools--foreground-watchdog-arm inv)
            (setq clock 110.0)
            (mevedel-tools--foreground-watchdog-expire agent-id))
          (should (equal agent-id (car stopped)))
          (should (string-match-p "no progress" (cadr stopped)))
          (should (eq parent-buf (caddr stopped)))
          (should-not (gethash agent-id
                               mevedel-tools--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "stops a foreground TOOL-state agent after no progress grace"
  (let* ((mevedel-tools--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-tool*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-tool*"))
         (agent-id "verifier--fg-tool")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (agent-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv)
                     :state 'TOOL))
         (clock 100.0)
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (with-current-buffer parent-buf
            (setq-local mevedel-tools--agents-fsm
                        `((,agent-id . ,agent-fsm))))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args) 'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-tools-stop-agent)
                     (lambda (id reason parent)
                       (setq stopped (list id reason parent)))))
            (mevedel-tools--foreground-watchdog-arm inv)
            (setq clock 110.0)
            (mevedel-tools--foreground-watchdog-expire agent-id))
          (should (equal agent-id (car stopped)))
          (should (string-match-p "no progress" (cadr stopped)))
          (should (eq parent-buf (caddr stopped)))
          (should-not (gethash agent-id
                               mevedel-tools--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-tools--bwait-watchdog-expire
  (:doc "running-agent warning advertises StopAgent recovery")
  (let* ((session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-watchdog-parent*"))
         (agent-buf (generate-new-buffer " *mev-watchdog-agent*"))
         (agent (mevedel-agent--create :name "reviewer"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "reviewer--WATCHDOG"
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p t
               :transcript-status 'running))
         (parent-fsm (gptel-make-fsm
                      :info (list :buffer parent-buf)
                      :state 'BWAIT))
         (child-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv)
                     :state 'WAIT))
         logged)
    (unwind-protect
        (progn
          (setf (mevedel-session-background-agents session)
                '("reviewer--WATCHDOG"))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-tools--agents-fsm
                        `(("reviewer--WATCHDOG" . ,child-fsm))))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq logged (apply #'format fmt args))))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _args) nil)))
            (let ((mevedel-agent-background-timeout 600))
              (mevedel-tools--bwait-watchdog-expire parent-fsm)))
          (should (string-match-p "StopAgent" logged))
          (should (string-match-p "mevedel-stop-agent" logged)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))


(provide 'test-mevedel-tool-ui)
;;; test-mevedel-tool-ui.el ends here
