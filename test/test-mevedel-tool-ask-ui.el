;;; test-mevedel-tool-ask-ui.el --- Tests for mevedel-tool-ask-ui.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the Ask questionnaire state, controllers, and presentation.

;;; Code:

(require 'mevedel-interaction-prompt)
(require 'mevedel-agent-runtime)
(require 'mevedel-tool-ask-ui)
(require 'mevedel-structs)
(require 'mevedel-agents)
(require 'mevedel-mentions)
(require 'mevedel-skills-ui)
(require 'mevedel-tools)
(require 'mevedel-view)
(require 'mevedel-view-interaction)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Ask User

(mevedel-deftest mevedel-tool-ask-ui--format-option ()
  ,test
  (test)
  :doc "plain option:
`mevedel-tool-ask-ui--format-option' leaves options without suffix unchanged"
  (should (equal "Balanced"
                 (mevedel-tool-ask-ui--format-option "Balanced")))
  :doc "terminal recommendation:
`mevedel-tool-ask-ui--format-option' highlights the recommended suffix"
  (let* ((formatted (mevedel-tool-ask-ui--format-option
                     "Balanced (Recommended)"))
         (start (string-match-p (regexp-quote " (Recommended)") formatted)))
    (should (equal "Balanced (Recommended)"
                   (substring-no-properties formatted)))
    (should (eq 'success (get-text-property start 'font-lock-face formatted))))
  :doc "object option:
`mevedel-tool-ask-ui--format-option' formats the object label"
  (let* ((formatted (mevedel-tool-ask-ui--format-option
                     '(:label "Project AGENTS.md (Recommended)"
                       :description "Shared guidance")))
         (start (string-match-p (regexp-quote " (Recommended)") formatted)))
    (should (equal "Project AGENTS.md (Recommended)"
                   (substring-no-properties formatted)))
    (should (eq 'success (get-text-property start 'font-lock-face formatted))))
  :doc "non-terminal recommendation text:
`mevedel-tool-ask-ui--format-option' only treats terminal suffix as recommended"
  (let* ((formatted (mevedel-tool-ask-ui--format-option
                     "Balanced (Recommended) maybe"))
         (start (string-match-p (regexp-quote " (Recommended)") formatted)))
    (should (equal "Balanced (Recommended) maybe"
                   (substring-no-properties formatted)))
    (should-not (get-text-property start 'font-lock-face formatted))))


(mevedel-deftest mevedel-tool-ask-ui--question-anchor ()
  ,test
  (test)
  :doc "anchors each question to its own block and the last to the form end"
  (let ((data-buffer (generate-new-buffer " *mev-ask-anchor-data*"))
        (view-buffer (generate-new-buffer " *mev-ask-anchor-view*"))
        rendered-body
        rendered-keymap)
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
                   #'ignore))
          (with-current-buffer view-buffer
            (setq-local mevedel--prompt-overlays nil))
          (mevedel-tool-ask-ui-show
           #'ignore
           [(:question "One?" :options ["Yes" "No"])
            (:question "Two?" :options ["Yes" "No"])
            (:question "Three?" :options ["Yes" "No"])])
          (ignore rendered-keymap)
          (with-temp-buffer
            (insert rendered-body)
            (let* ((overlay (make-overlay (point-min) (point-max)))
                   (start-0 (mevedel-tool-ask-ui--question-start overlay 0))
                   (start-1 (mevedel-tool-ask-ui--question-start overlay 1))
                   (anchor-0 (mevedel-tool-ask-ui--question-anchor overlay 0))
                   (anchor-1 (mevedel-tool-ask-ui--question-anchor overlay 1))
                   (anchor-2 (mevedel-tool-ask-ui--question-anchor overlay 2)))
              ;; Every block is findable, in order.
              (should start-0)
              (should start-1)
              (should (< start-0 start-1))
              ;; A question's anchor sits inside its own block, below the
              ;; options being compared and above the next question.
              (should (< start-0 anchor-0))
              (should (< anchor-0 start-1))
              (should (< anchor-0 anchor-1))
              ;; The last block has no successor, so it anchors to the
              ;; end of the whole form.
              (should (= anchor-2 (overlay-end overlay))))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer)))))

(mevedel-deftest mevedel-tool-ask-ui-show ()
  ,test
  (test)
  :doc "shows every question at once and submits the whole form"
  (let ((data-buffer (generate-new-buffer " *mev-ask-data*"))
        (view-buffer (generate-new-buffer " *mev-ask-view*"))
        rendered-body
        rendered-keymap
        registered-overlay
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
                     ;; One interaction id means one overlay across
                     ;; renders; a fresh one per keystroke would pile up
                     ;; in `mevedel--prompt-overlays'.
                     (setq registered-overlay
                           (or registered-overlay
                               (make-overlay (point-min) (point-min)
                                             (current-buffer) nil t)))))
                  ((symbol-function 'mevedel--prompt--register-canceller)
                   #'ignore))
          (with-current-buffer view-buffer
            (setq-local mevedel--prompt-overlays nil))
          (mevedel-tool-ask-ui-show
           (lambda (value) (setq result value))
           [(:question "Use cache?" :options ["Yes" "No"])
            (:question "Run tests?" :options ["Yes" "No"])])
          ;; Both questions are on screen from the very first render.
          (should (string-match-p "Use cache\\?" rendered-body))
          (should (string-match-p "Run tests\\?" rendered-body))
          (should (string-match-p "0 of 2 answered" rendered-body))
          ;; RET records the focused option and moves to the next
          ;; unanswered question.
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "1 of 2 answered" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "n")))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "2 of 2 answered" rendered-body))
          (with-current-buffer view-buffer
            (should (= 1 (length mevedel--prompt-overlays))))
          (call-interactively (lookup-key rendered-keymap (kbd "C-c C-c")))
          (should (string-match-p "Q1: Use cache\\?" result))
          (should (string-match-p "A1: Yes" result))
          (should (string-match-p "Q2: Run tests\\?" result))
          (should (string-match-p "A2: No" result))
          ;; The generic approve/deny/feedback surface keys off
          ;; `mevedel-user-request'; the questionnaire must not carry
          ;; it, or point inside the Ask settles it with an outcome the
          ;; questions never offered.
          (should-not (overlay-get registered-overlay
                                   'mevedel-user-request)))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))))

  :doc "moving back to an answered question reopens it on its answer"
  (let ((data-buffer (generate-new-buffer " *mev-ask-back-data*"))
        (view-buffer (generate-new-buffer " *mev-ask-back-view*"))
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
                   #'ignore))
          (with-current-buffer view-buffer
            (setq-local mevedel--prompt-overlays nil))
          (mevedel-tool-ask-ui-show
           (lambda (value) (setq result value))
           [(:question "Use cache?" :options ["Yes" "No"])
            (:question "Run tests?" :options ["Yes" "No"])])
          ;; Answer the first question, then walk the cursor back into
          ;; it: an answered question collapses to its answer until the
          ;; cursor returns, which is the only "edit" gesture there is.
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (call-interactively (lookup-key rendered-keymap (kbd "p")))
          ;; Digits move the cursor without answering, so the count is
          ;; unchanged until RET.
          (call-interactively (lookup-key rendered-keymap (kbd "2")))
          (should (string-match-p "1 of 2 answered" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (call-interactively (lookup-key rendered-keymap (kbd "C-c C-c")))
          (should (string-match-p "A1: No" result)))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))))

  :doc "an unanswered question submits as no preference, never nil"
  (let ((data-buffer (generate-new-buffer " *mev-ask-skip-data*"))
        (view-buffer (generate-new-buffer " *mev-ask-skip-view*"))
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
                   #'ignore))
          (with-current-buffer view-buffer
            (setq-local mevedel--prompt-overlays nil))
          (mevedel-tool-ask-ui-show
           (lambda (value) (setq result value))
           [(:question "Use cache?" :options ["Yes" "No"])
            (:question "Run tests?" :options ["Yes" "No"])])
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (call-interactively (lookup-key rendered-keymap (kbd "C-c C-c")))
          (should (string-match-p "A1: Yes" result))
          (should (string-match-p (regexp-quote
                                   mevedel-tool-ask-ui--no-preference)
                                  result))
          (should-not (string-match-p "A2: nil" result)))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))))

  :doc "recommended option remains part of the selected Ask answer"
  (let ((data-buffer (generate-new-buffer " *mev-ask-rec-data*"))
        (view-buffer (generate-new-buffer " *mev-ask-rec-view*"))
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
                   #'ignore))
          (with-current-buffer view-buffer
            (setq-local mevedel--prompt-overlays nil))
          (mevedel-tool-ask-ui-show
           (lambda (value) (setq result value))
           [(:question "Choose risk profile"
                       :options ["Conservative" "Balanced (Recommended)" "Aggressive"])])
          (should (string-match-p "Balanced (Recommended)"
                                  (substring-no-properties rendered-body)))
          (let ((start (string-match-p (regexp-quote " (Recommended)")
                                       rendered-body)))
            (should (eq 'success
                        (get-text-property start 'font-lock-face rendered-body))))
          (call-interactively (lookup-key rendered-keymap (kbd "2")))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (call-interactively (lookup-key rendered-keymap (kbd "C-c C-c")))
          (should (string-match-p "Q1: Choose risk profile" result))
          ;; The suffix travels with the answer: it is how the model
          ;; learns whether the user took its recommendation.
          (should (string-match-p "A1: Balanced (Recommended)" result)))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))))

  :doc "object options show descriptions inline and echo the chosen sample"
  (let ((data-buffer (generate-new-buffer " *mev-ask-obj-data*"))
        (view-buffer (generate-new-buffer " *mev-ask-obj-view*"))
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
                   #'ignore))
          (with-current-buffer view-buffer
            (setq-local mevedel--prompt-overlays nil))
          (mevedel-tool-ask-ui-show
           (lambda (value) (setq result value))
           [(:question "What should I write?"
                       :options [(:label "Project AGENTS.md (Recommended)"
                                  :description "Shared repo guidance"
                                  :sample "# Repository Guidelines\n- Run tests")
                                 (:label "Personal AGENTS.local.md"
                                  :description "Private notes"
                                  :sample "# Local notes")])])
          (should (string-match-p "Shared repo guidance" rendered-body))
          ;; The sample belongs to the frame beside the form, never to
          ;; the form itself.
          (should-not (string-match-p "# Repository Guidelines" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (call-interactively (lookup-key rendered-keymap (kbd "C-c C-c")))
          (should (string-match-p "A1: Project AGENTS.md (Recommended)"
                                  result))
          (should (string-match-p "Sample shown for A1:" result))
          (should (string-match-p "# Repository Guidelines" result)))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))))

  :doc "cursor movement does not re-announce the form to remote guests"
  (let ((data-buffer (generate-new-buffer " *mev-ask-announce-data*"))
        (view-buffer (generate-new-buffer " *mev-ask-announce-view*"))
        (mevedel-interaction-prompt-created-hook nil)
        (announcements 0)
        rendered-keymap
        result)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--prompt--data-buffer)
                   (lambda () data-buffer))
                  ((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (&optional _data-buffer) view-buffer))
                  ((symbol-function 'mevedel-view--interaction-register)
                   (lambda (descriptor)
                     (setq rendered-keymap (plist-get descriptor :keymap))
                     (make-overlay (point-min) (point-min)
                                   (current-buffer) nil t)))
                  ((symbol-function 'mevedel--prompt--register-canceller)
                   #'ignore))
          (add-hook 'mevedel-interaction-prompt-created-hook
                    (lambda (_overlay) (cl-incf announcements)))
          (with-current-buffer view-buffer
            (setq-local mevedel--prompt-overlays nil))
          (mevedel-tool-ask-ui-show
           (lambda (value) (setq result value))
           [(:question "Use cache?" :options ["Yes" "No"])])
          ;; The opening render announces once.
          (should (= 1 announcements))
          ;; A guest sees the whole form already, so host navigation is
          ;; not state worth re-sending -- and every announce rebuilds
          ;; the questionnaire payload, samples included.
          (call-interactively (lookup-key rendered-keymap (kbd "n")))
          (call-interactively (lookup-key rendered-keymap (kbd "p")))
          (call-interactively (lookup-key rendered-keymap (kbd "1")))
          (should (= 1 announcements))
          ;; An answer is a real change.
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (= 2 announcements)))
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
            (mevedel-tool-ask-ui-show
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
            (mevedel-tool-ask-ui-show
             (lambda (value) (setq result-a value))
             [(:question "A?" :options ["Yes" "No"])]))
          (with-current-buffer agent-buffer-b
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation inv-b)
            (setq-local mevedel--view-buffer view-buffer)
            (mevedel-request-begin session)
            (mevedel-tool-ask-ui-show
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
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "exposes the questionnaire remotely and adopts an atomic guest answer"
  (let ((data-buffer (generate-new-buffer " *mev-ask-remote-data*"))
        (view-buffer (generate-new-buffer " *mev-ask-remote-view*"))
        (mevedel-interaction-prompt-created-hook nil)
        (mevedel-interaction-prompt-settled-hook nil)
        overlay announced settled result)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--prompt--data-buffer)
                   (lambda () data-buffer))
                  ((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (&optional _data-buffer) view-buffer))
                  ((symbol-function 'mevedel-view--interaction-register)
                   (lambda (_descriptor)
                     (make-overlay (point-min) (point-min)
                                   (current-buffer) nil t)))
                  ((symbol-function 'mevedel--prompt--register-canceller)
                   #'ignore))
          (add-hook 'mevedel-interaction-prompt-created-hook
                    (lambda (ov) (setq announced t overlay ov)))
          (add-hook 'mevedel-interaction-prompt-settled-hook
                    (lambda (_ov) (setq settled t)))
          (with-current-buffer view-buffer
            (setq-local mevedel--prompt-overlays nil))
          (mevedel-tool-ask-ui-show
           (lambda (value) (setq result value))
           [(:question "Which approach?"
             :options ["MVP first (Recommended)"
                       (:label "Risk first" :description "slower"
                        :sample "# Risk first\n- spike the parser")])
            (:question "Run tests?" :options ["Yes" "No"])])
          (should announced)
          (should (functionp (plist-get (overlay-get overlay 'mevedel--remote)
                                        :answer)))
          (let ((questions (funcall (plist-get
                                     (overlay-get overlay 'mevedel--remote)
                                     :questions))))
            (should (= 2 (length questions)))
            (should (equal "Which approach?"
                           (cdr (assoc "question" (car questions)))))
            (should (equal "slower"
                           (cdr (assoc "description"
                                       (aref (cdr (assoc "options"
                                                         (car questions)))
                                             1)))))
            ;; A guest choosing between artifacts needs the artifacts,
            ;; not just their labels.
            (should (equal "# Risk first\n- spike the parser"
                           (cdr (assoc "sample"
                                       (aref (cdr (assoc "options"
                                                         (car questions)))
                                             1)))))
            ;; Nothing answered yet.
            (should-not (assoc "answer" (car questions))))
          ;; A wrong-length answer set is refused.
          (funcall (plist-get (overlay-get overlay 'mevedel--remote) :answer)
                   '("only"))
          (should-not result)
          ;; The atomic guest answer submits through the same path.
          (funcall (plist-get (overlay-get overlay 'mevedel--remote) :answer)
                   '("Risk first" ""))
          (should (string-match-p "A1: Risk first" result))
          (should (string-match-p
                   (concat "A2: " mevedel-tool-ask-ui--no-preference)
                   result))
          (should settled))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "settles one questionnaire exactly once across surfaces"
  (let* ((session (mevedel-session--create :name "main"))
         (data-buffer (generate-new-buffer " *mev-ask-once-data*"))
         (view-buffer (generate-new-buffer " *mev-ask-once-view*"))
         (settlements 0)
         (count-settlement (lambda (_overlay) (cl-incf settlements)))
         results overlay interaction-id)
    (unwind-protect
        (progn
          (add-hook 'mevedel-interaction-prompt-settled-hook count-settlement)
          (with-current-buffer data-buffer
            (setq-local mevedel--session session)
            (mevedel-request-begin session))
          (mevedel-view--setup view-buffer data-buffer)
          (with-current-buffer data-buffer
            (mevedel-tool-ask-ui-show
             (lambda (value) (push value results))
             [(:question "Which approach?" :options ["MVP" "Risk"])]))
          (with-current-buffer view-buffer
            (setq overlay (car mevedel--prompt-overlays)))
          (setq interaction-id
                (overlay-get overlay 'mevedel-view-interaction-id))
          (should interaction-id)
          (let ((answer (plist-get (overlay-get overlay 'mevedel--remote)
                                   :answer)))
            (funcall answer '("Risk"))
            ;; Deleting the overlay neither erases its properties nor
            ;; invalidates the closure a guest already holds.
            (funcall answer '("MVP")))
          ;; Request teardown is the production route into the gate; a
          ;; settled questionnaire must not be answered a second time by it.
          (with-current-buffer data-buffer
            (mevedel-request-end))
          (should (= 1 (length results)))
          (should (string-match-p "A1: Risk" (car results)))
          (should (= 1 settlements))
          (should (overlay-get overlay 'mevedel-settled))
          (with-current-buffer view-buffer
            ;; The shared gate owns view unregistration, not Ask.
            (should-not (gethash interaction-id
                                 mevedel-view--interaction-overlays))
            (should-not mevedel--prompt-overlays)))
      (remove-hook 'mevedel-interaction-prompt-settled-hook count-settlement)
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "quitting settles the questionnaire without ending the run"
  (let* ((session (mevedel-session--create :name "main"))
         (data-buffer (generate-new-buffer " *mev-ask-quit-data*"))
         (view-buffer (generate-new-buffer " *mev-ask-quit-view*"))
         (aborts 0)
         results overlay interaction-id)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-abort)
                   (lambda (&optional _buf) (cl-incf aborts))))
          (with-current-buffer data-buffer
            (setq-local mevedel--session session)
            (mevedel-request-begin session))
          (mevedel-view--setup view-buffer data-buffer)
          (with-current-buffer data-buffer
            (mevedel-tool-ask-ui-show
             (lambda (value) (push value results))
             [(:question "Which approach?" :options ["MVP" "Risk"])]))
          (with-current-buffer view-buffer
            (setq overlay (car mevedel--prompt-overlays)))
          (setq interaction-id
                (overlay-get overlay 'mevedel-view-interaction-id))
          (with-current-buffer view-buffer
            (call-interactively
             (lookup-key (overlay-get overlay 'keymap) (kbd "q"))))
          (should (equal '(aborted) results))
          ;; The handler turns this into an error result the model can
          ;; act on; a mis-hit key costs a tool call, not the run.
          (should (= 0 aborts))
          (with-current-buffer view-buffer
            (should-not (gethash interaction-id
                                 mevedel-view--interaction-overlays)))
          ;; A guest closure retained from before the quit cannot answer it.
          (funcall (plist-get (overlay-get overlay 'mevedel--remote) :answer)
                   '("MVP"))
          (should (equal '(aborted) results)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "the abort key cancels the questionnaire and the run"
  (let* ((session (mevedel-session--create :name "main"))
         (data-buffer (generate-new-buffer " *mev-ask-abort-data*"))
         (view-buffer (generate-new-buffer " *mev-ask-abort-view*"))
         (aborts 0)
         results overlay)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-abort)
                   (lambda (&optional _buf) (cl-incf aborts))))
          (with-current-buffer data-buffer
            (setq-local mevedel--session session)
            (mevedel-request-begin session))
          (mevedel-view--setup view-buffer data-buffer)
          (with-current-buffer data-buffer
            (mevedel-tool-ask-ui-show
             (lambda (value) (push value results))
             [(:question "Which approach?" :options ["MVP" "Risk"])]))
          (with-current-buffer view-buffer
            (setq overlay (car mevedel--prompt-overlays))
            (call-interactively
             (lookup-key (overlay-get overlay 'keymap) (kbd "C-c C-k"))))
          (should (equal '(aborted) results))
          (should (= 1 aborts)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer)))))

(provide 'test-mevedel-tool-ask-ui)
;;; test-mevedel-tool-ask-ui.el ends here
