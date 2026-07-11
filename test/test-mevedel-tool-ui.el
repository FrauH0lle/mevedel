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

(mevedel-deftest mevedel-tool-ui--deliver-result ()
  ,test
  (test)
  :doc "wraps raw results in the canonical envelope"
  (let (delivered)
    (mevedel-tool-ui--deliver-result
     (lambda (value) (setq delivered value))
     "done")
    (should (equal '(:result "done") delivered)))

  :doc "preserves result metadata envelopes"
  (let ((envelope '(:result "done" :render-data (:kind card)
                    :media ((:type image))))
        delivered)
    (mevedel-tool-ui--deliver-result
     (lambda (value) (setq delivered value))
     envelope)
    (should (eq envelope delivered))))





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

(mevedel-deftest mevedel-tool-ui--register/path-contract
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "declares RequestAccess.directory as a semantic path"
  (progn
    (mevedel-tool-ui--register)
    (should
     (eq 'path
         (cadr (assq 'directory
                     (mevedel-tool-args
                      (mevedel-tool-get "RequestAccess"))))))))

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
;;; Agent terminal recovery




;;
;;; Agent stop control











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
            (setq-local mevedel-agent-runtime--fsms
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
                               mevedel-agent-runtime--fsms)))
          (should (eq 'WAIT (gptel-fsm-state parent-fsm))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))








(provide 'test-mevedel-tool-ui)
;;; test-mevedel-tool-ui.el ends here
