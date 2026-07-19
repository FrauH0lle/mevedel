;;; test-mevedel-tool-ask.el --- Tests for mevedel-tool-ask.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-interaction-prompt)
(require 'mevedel-agent-runtime)
(require 'mevedel-tool-ask)
(require 'mevedel-structs)
(require 'mevedel-agents)
(require 'mevedel-mentions)
(require 'mevedel-skills-ui)
(require 'mevedel-tools)
(require 'mevedel-view)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


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

(mevedel-deftest mevedel-tool-ask--ask ()
  ,test
  (test)
  :doc "validates questions and wraps the questionnaire result"
  (let (delivered)
    (cl-letf (((symbol-function 'mevedel-tools--ask-user)
               (lambda (callback questions)
                 (should (vectorp questions))
                 (funcall callback "answers"))))
      (mevedel-tool-ask--ask
       (lambda (value) (setq delivered value))
       '(:questions [(:question "Proceed?" :options ["Yes" "No"])])))
    (should (equal '(:result "answers") delivered)))

  :doc "rejects a missing questions argument"
  (should-error (mevedel-tool-ask--ask #'ignore nil) :type 'error))

(mevedel-deftest mevedel-tool-ask--question-count ()
  ,test
  (test)
  :doc "counts each supported Ask question container shape"
  (progn
    (should (= 1 (mevedel-tool-ask--question-count
                  [(:question "A?")])))
    (should (= 2 (mevedel-tool-ask--question-count
                  '((:question "A?") (:question "B?")))))
    (should (= 1 (mevedel-tool-ask--question-count 'malformed)))
    (should (= 0 (mevedel-tool-ask--question-count nil)))))

(mevedel-deftest mevedel-tool-ask--result-status ()
  ,test
  (test)
  :doc "marks only string results with an error prefix"
  (progn
    (should (eq 'error
                (mevedel-tool-ask--result-status "Error: unavailable")))
    (should-not (mevedel-tool-ask--result-status "done"))
    (should-not (mevedel-tool-ask--result-status '(:result "done")))))

(mevedel-deftest mevedel-tool-ask--render ()
  ,test
  (test)
  :doc "renders an Ask result with its question count"
  (should
   (equal '(:header "Ask: 2 questions"
            :body "answers"
            :body-mode nil
            :status nil
            :initially-collapsed-p t)
          (mevedel-tool-ask--render
           "Ask"
           '(:questions [(:question "A?") (:question "B?")])
           "answers"
           nil))))




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
               :agent-id "verifier--abc" :path "/root/verifier"
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
                 :agent-id "verifier--a" :path "/root/worker/a"
                 :parent-session session
                 :parent-data-buffer data-buffer
                 :buffer agent-buffer-a))
         (inv-b (mevedel-agent-invocation--create
                 :agent agent
                 :agent-id "verifier--b" :path "/root/worker/b"
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
            (should (= 2 (length mevedel--prompt-overlays)))
            (let ((origins
                   (mapcar
                    (lambda (overlay)
                      (overlay-get overlay
                                   'mevedel-view-interaction-origin))
                    mevedel--prompt-overlays)))
              (should (member "/root/worker/a" origins))
              (should (member "/root/worker/b" origins))))
          (cl-letf
              (((symbol-function
                 'mevedel-agent-runtime--interrupted-agent-response)
                (lambda (_invocation _reason) "interrupted"))
               ((symbol-function 'mevedel-agent-runtime--finalize)
                (lambda (invocation status)
                  (when-let* ((buffer (mevedel-agent-invocation-buffer
                                      invocation))
                              ((buffer-live-p buffer)))
                    (with-current-buffer buffer
                      (mevedel-request-end)))
                  (setf (mevedel-agent-invocation-transcript-status invocation)
                        status))))
            (mevedel-agent-runtime-interrupt
             inv-a "interrupted by /root"))
          (should (eq 'aborted result-a))
          (should-not result-b)
          (with-current-buffer view-buffer
            (should (= 1 (length mevedel--prompt-overlays)))
            (should
             (equal "/root/worker/b"
                    (overlay-get
                     (car mevedel--prompt-overlays)
                     'mevedel-view-interaction-origin))))
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
;;; Registration

(mevedel-deftest mevedel-tool-ask-register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "registers the Ask handler and renderer"
  (progn
    (mevedel-tool-ask-register)
    (let ((tool (mevedel-tool-get "Ask")))
      (should tool)
      (should (eq #'mevedel-tool-ask--ask
                  (mevedel-tool-handler tool)))
      (should (eq #'mevedel-tool-ask--render
                  (mevedel-tool-renderer tool))))))

(provide 'test-mevedel-tool-ask)
;;; test-mevedel-tool-ask.el ends here
