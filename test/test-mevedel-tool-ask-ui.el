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


(defmacro test-mevedel-tool-ask-ui--with-form (&rest body)
  "Run BODY with native Ask buffers and captured rendering.
Bind data-buffer, view-buffer, rendered-body, rendered-keymap,
registered-overlay and result for questionnaire assertions."
  (declare (indent 0) (debug t))
  `(mevedel-view-test--with-buffers
     (let ((data-buffer data-buf)
           (view-buffer view-buf)
           (register (symbol-function 'mevedel-view--interaction-register))
           rendered-body rendered-keymap registered-overlay result)
       (cl-letf (((symbol-function 'mevedel-view--interaction-register)
                  (lambda (descriptor)
                    (setq rendered-body (plist-get descriptor :body)
                          rendered-keymap (plist-get descriptor :keymap)
                          registered-overlay (funcall register descriptor)))))
         (save-window-excursion
           (set-window-buffer (selected-window) view-buffer)
           (with-current-buffer view-buffer
             (goto-char (point-max)))
           (with-current-buffer data-buffer
             ,@body))))))


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
  (test-mevedel-tool-ask-ui--with-form
    (mevedel-tool-ask-ui-show
     #'ignore
     [(:question "One?" :options ["Yes" "No"])
      (:question "Two?" :options ["Yes" "No"])
      (:question "Three?" :options ["Yes" "No"])])
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
        (should (= anchor-2 (overlay-end overlay)))))))


(mevedel-deftest mevedel-tool-ask-ui--sample-show ()
  ,test
  (test)
  :doc "samples use the top-level parent from ordinary and nested question windows"
  (dolist (source '(top child grandchild))
    (with-temp-buffer
      (insert "Question?\n")
      (let ((overlay (make-overlay (point-min) (point-max)))
            (window (selected-window))
            (noninteractive nil)
            ensured placed shown)
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) window))
                  ((symbol-function 'pos-visible-in-window-p) (lambda (&rest _) t))
                  ((symbol-function 'window-frame) (lambda (_) source))
                  ((symbol-function 'frame-parent)
                   (lambda (frame) (cdr (assq frame '((grandchild . child) (child . top))))))
                  ((symbol-function 'mevedel-tool-ask-ui--sample-work-buffer)
                   #'current-buffer)
                  ((symbol-function 'mevedel-view--fontify-as)
                   (lambda (text _) text))
                  ((symbol-function 'mevedel-tool-ask-ui--sample-ensure-frame)
                   (lambda (parent) (setq ensured parent) 'sample))
                  ((symbol-function 'frame-root-window) (lambda (_) window))
                  ((symbol-function 'set-window-buffer) #'ignore)
                  ((symbol-function 'mevedel-tool-ask-ui--sample-place)
                   (lambda (frame parent source-window position)
                     (setq placed (list frame parent source-window position))))
                  ((symbol-function 'make-frame-visible)
                   (lambda (frame) (setq shown frame))))
          (mevedel-tool-ask-ui--sample-show "Sample" overlay 0)
          (should (eq ensured 'top))
          (should (equal placed (list 'sample 'top window 11)))
          (should (eq shown 'sample)))))))


(mevedel-deftest mevedel-tool-ask-ui--sample-place ()
  ,test
  (test)
  :doc "placement translates native coordinates through nested frames, including unavailable display origins"
  ;; Top-level, nested display coordinates, and Wayland-style child origins
  ;; all describe the same glyph at (140, 160) in the top-level native frame.
  (dolist (fixture '((top (1140 . 2160))
                     (child (1140 . 2160))
                     (grandchild (27 . 27))
                     (child (59 . 59))))
    (let* ((source (car fixture))
           (pixels (cadr fixture))
           (display-origin (equal pixels '(1140 . 2160)))
           (mevedel-ask-sample-frame-width 0.5)
           (mevedel-ask-sample-frame-height 5)
           size position visible)
      (cl-letf (((symbol-function 'window-absolute-pixel-position)
                 (lambda (&rest _) pixels))
                ((symbol-function 'window-frame) (lambda (_) source))
                ((symbol-function 'frame-parent)
                 (lambda (frame) (cdr (assq frame '((grandchild . child) (child . top))))))
                ((symbol-function 'frame-edges)
                 (lambda (frame kind)
                   (let ((origin (pcase frame
                                   ('top '(1000 2000))
                                   ('child (if display-origin '(1080 2100) '(-1 -1)))
                                   ('grandchild '(-2 -2)))))
                     ;; Children have a one-pixel external border.
                     (if (and (not (eq frame 'top)) (eq kind 'native-edges))
                         (mapcar #'1+ origin)
                       origin))))
                ((symbol-function 'frame-parameter)
                 (lambda (frame key)
                   (pcase (list frame key)
                     (`(child left) 80) (`(child top) 100)
                     (`(grandchild left) 30) (`(grandchild top) 30))))
                ((symbol-function 'frame-char-height) (lambda (_) 20))
                ((symbol-function 'frame-pixel-width) (lambda (_) 800))
                ((symbol-function 'frame-pixel-height) (lambda (_) 600))
                ((symbol-function 'set-frame-size)
                 (lambda (_ width height pixelwise) (setq size (list width height pixelwise))))
                ((symbol-function 'set-frame-position)
                 (lambda (_ x y) (setq position (list x y))))
                ((symbol-function 'fit-frame-to-buffer)
                 (lambda (&rest args) (setq visible args))))
        (should (mevedel-tool-ask-ui--sample-place 'sample 'top 'window 1))
        (should (equal size '(400 100 t)))
        (should (equal position '(140 180)))
        (should (eq (car (last visible)) 'vertically))))))


(mevedel-deftest mevedel-tool-ask-ui--entry-at-point ()
  ,test
  (test)
  :doc "entry identity covers descriptions and custom input, but not adjacent prose"
  (test-mevedel-tool-ask-ui--with-form
    (mevedel-tool-ask-ui-show
     #'ignore
     [(:question "Choose a layout\nwith room for a long description?"
       :options [(:label "Compact" :description "First line\nSecond line")
                 "Spacious"])])
    (with-current-buffer view-buffer
      (let* ((state (car mevedel-tool-ask-ui--states))
             (positions (mevedel-tool-ask-ui--entry-positions state)))
        (should (equal (mapcar #'car positions) '((0 . 0) (0 . 1) (0 . 2) submit)))
        (should (equal '(0 . 0) (mevedel-tool-ask-ui--entry-at-point state)))
        (should (get-text-property (point) 'mevedel-prompt-focus))
        (should (eq (char-after) ?○))
        (should (= (current-column) 5))
        (search-forward "Second line")
        (backward-char)
        (should (equal '(0 . 0) (mevedel-tool-ask-ui--entry-at-point state)))
        (should (equal "          " (get-text-property (point) 'wrap-prefix)))
        (mevedel-tool-ask-ui--goto-entry state '(0 . 2))
        (should (equal '(0 . 2) (mevedel-tool-ask-ui--entry-at-point state)))
        (should (eq (char-after) ?○))
        (goto-char (overlay-start registered-overlay))
        (should-not (mevedel-tool-ask-ui--entry-at-point state))
        (search-forward "Choose a layout")
        (should (equal "     " (get-text-property (point) 'wrap-prefix)))
        (goto-char (overlay-end registered-overlay))
        (should-not (mevedel-tool-ask-ui--entry-at-point state))))
    (should-not (string-match-p "▸\\|Ask ·" rendered-body))))


(mevedel-deftest mevedel-tool-ask-ui--post-command ()
  ,test
  (test)
  :doc "native motion synchronizes samples only on entry changes and leaving the form"
  (test-mevedel-tool-ask-ui--with-form
    (let (focused)
      (cl-letf (((symbol-function 'mevedel-tool-ask-ui--sync-sample)
                 (lambda (state)
                   (push (mevedel-tool-ask-ui--entry-at-point state) focused))))
        (mevedel-tool-ask-ui-show
         #'ignore
         [(:question "Layout?"
           :options [(:label "Compact" :description "A long description") "Spacious"])])
        (should (equal focused '((0 . 0))))
        (with-current-buffer view-buffer
          (let ((tick (buffer-chars-modified-tick))
                (state (car mevedel-tool-ask-ui--states)))
            (forward-char 3)
            (run-hooks 'post-command-hook)
            (should (equal focused '((0 . 0))))
            (search-forward "Spacious")
            (backward-char)
            (run-hooks 'post-command-hook)
            (should (equal focused '((0 . 1) (0 . 0))))
            (should (= 1 (mevedel-tool-ask-ui--state-focus-option state)))
            (goto-char (point-max))
            (run-hooks 'post-command-hook)
            (should (equal focused '(nil (0 . 1) (0 . 0))))
            (should-not (mevedel-tool-ask-ui--focused-option state))
            (should (= tick (buffer-chars-modified-tick)))))))))


(mevedel-deftest mevedel-tool-ask-ui--retire ()
  ,test
  (test)
  :doc "settling concurrent forms removes only their own motion tracking"
  (test-mevedel-tool-ask-ui--with-form
    (mevedel-tool-ask-ui-show #'ignore [(:question "First?" :options ["Yes"])])
    (let ((first registered-overlay))
      (mevedel-tool-ask-ui-show #'ignore [(:question "Second?" :options ["Yes"])])
      (with-current-buffer view-buffer
        (should (= 2 (length mevedel-tool-ask-ui--states)))
        (mevedel--prompt--settle first 'aborted)
        (should (= 1 (length mevedel-tool-ask-ui--states)))
        (should (memq #'mevedel-tool-ask-ui--post-command post-command-hook))
        (mevedel--prompt--settle registered-overlay 'aborted)
        (should-not mevedel-tool-ask-ui--states)
        (should-not mevedel-tool-ask-ui--active-state)
        (should-not (memq #'mevedel-tool-ask-ui--post-command post-command-hook))))))


(mevedel-deftest mevedel-tool-ask-ui--dispatch ()
  ,test
  (test)
  :doc "navigation never repaints and RET on a description answers its own row"
  (test-mevedel-tool-ask-ui--with-form
    (mevedel-tool-ask-ui-show
     (lambda (value) (setq result value))
     [(:question "Layout?"
       :options [(:label "Compact" :description "Dense text")
                 (:label "Spacious" :description "Room to breathe")])])
    (with-current-buffer view-buffer
      (let ((tick (buffer-chars-modified-tick))
            (body rendered-body))
        (dolist (key '("n" "p" "2" "C-n" "C-p" "<up>" "<down>"))
          (call-interactively (lookup-key rendered-keymap (kbd key))))
        (should (= tick (buffer-chars-modified-tick)))
        (should (eq body rendered-body)))
      (search-forward "Room to breathe")
      (backward-char)
      (call-interactively (lookup-key rendered-keymap (kbd "RET")))
      (should (eq 'submit (get-text-property (point) 'mevedel-ask-entry)))
      (should (eq (char-after) ?\[))
      (call-interactively (lookup-key rendered-keymap (kbd "p")))
      (should (eq (char-after) ?✓))
      (search-forward "Layout?")
      (should (= (- (current-column) (length "Layout?")) 5))
      (call-interactively (lookup-key rendered-keymap (kbd "n")))
      (call-interactively (lookup-key rendered-keymap (kbd "RET"))))
    (should (string-match-p "A1: Spacious" result)))

  :doc "custom answer drafts survive reopening and blank edits leave answers intact"
  (test-mevedel-tool-ask-ui--with-form
    (mevedel-tool-ask-ui-show
     (lambda (value) (setq result value))
     [(:question "Layout?" :options ["Compact" "Spacious"])])
    (let (initial)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt value &rest _)
                   (setq initial value)
                   "My layout")))
        (call-interactively (lookup-key rendered-keymap (kbd "c"))))
      (should-not initial)
      (call-interactively (lookup-key rendered-keymap (kbd "p")))
      (call-interactively (lookup-key rendered-keymap (kbd "RET")))
      (with-current-buffer view-buffer
        (should (equal '(0 . 2) (get-text-property (point) 'mevedel-ask-entry))))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt value &rest _)
                   (setq initial value)
                   "  ")))
        (call-interactively (lookup-key rendered-keymap (kbd "RET"))))
      (should (equal "My layout" initial)))
    (call-interactively (lookup-key rendered-keymap (kbd "C-c C-c")))
    (should (string-match-p "A1: My layout" result)))

  :doc "mouse activation selects the clicked row rather than the previous point"
  (test-mevedel-tool-ask-ui--with-form
    (mevedel-tool-ask-ui-show
     (lambda (value) (setq result value))
     [(:question "Layout?" :options ["Compact" "Spacious"])])
    (with-current-buffer view-buffer
      (let ((position (cdr (assoc '(0 . 1)
                                  (mevedel-tool-ask-ui--entry-positions
                                   (car mevedel-tool-ask-ui--states))))))
        (funcall (lookup-key rendered-keymap [mouse-1])
                 (list 'mouse-1 (list (selected-window) position '(0 . 0) 0))))
      (should (eq 'submit (get-text-property (point) 'mevedel-ask-entry)))
      (call-interactively (lookup-key rendered-keymap (kbd "RET"))))
    (should (string-match-p "A1: Spacious" result)))

  :doc "form changes preserve a multiline composer draft and restore its point"
  (test-mevedel-tool-ask-ui--with-form
    (with-current-buffer view-buffer
      (mevedel-view-test--insert-composer-draft "> quoted\nsecond line" 4))
    (mevedel-tool-ask-ui-show
     #'ignore [(:question "Layout?" :options ["Compact" "Spacious"])])
    (call-interactively (lookup-key rendered-keymap (kbd "RET")))
    (with-current-buffer view-buffer
      (should (equal "> quoted\nsecond line" (mevedel-view--input-text))))
    (call-interactively (lookup-key rendered-keymap (kbd "RET")))
    (with-current-buffer view-buffer
      (should (equal "> quoted\nsecond line" (mevedel-view--input-text)))
      (should (= (point) (+ (mevedel-view--input-start) 4))))))


(mevedel-deftest mevedel-tool-ask-ui-show ()
  ,test
  (test)
  :doc "shows every question at once and submits the whole form"
  (test-mevedel-tool-ask-ui--with-form
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
    (call-interactively (lookup-key rendered-keymap (kbd "<return>")))
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

  :doc "RET on an answered summary reopens it on its answer"
  (test-mevedel-tool-ask-ui--with-form
    (mevedel-tool-ask-ui-show
     (lambda (value) (setq result value))
     [(:question "Use cache?" :options ["Yes" "No"])
      (:question "Run tests?" :options ["Yes" "No"])])
    ;; Motion reaches the summary without expanding or repainting it.
    (call-interactively (lookup-key rendered-keymap (kbd "RET")))
    (call-interactively (lookup-key rendered-keymap (kbd "p")))
    (with-current-buffer view-buffer
      (should (equal '(0 . summary) (get-text-property (point) 'mevedel-ask-entry))))
    (call-interactively (lookup-key rendered-keymap (kbd "RET")))
    ;; Digits move the cursor without answering, so the count is
    ;; unchanged until RET.
    (call-interactively (lookup-key rendered-keymap (kbd "2")))
    (should (string-match-p "1 of 2 answered" rendered-body))
    (call-interactively (lookup-key rendered-keymap (kbd "RET")))
    (call-interactively (lookup-key rendered-keymap (kbd "C-c C-c")))
    (should (string-match-p "A1: No" result)))

  :doc "an unanswered question submits as no preference, never nil"
  (test-mevedel-tool-ask-ui--with-form
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

  :doc "recommended option remains part of the selected Ask answer"
  (test-mevedel-tool-ask-ui--with-form
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

  :doc "object options show descriptions inline and echo the chosen sample"
  (test-mevedel-tool-ask-ui--with-form
    (let (shown-sample)
      (cl-letf (((symbol-function 'mevedel-tool-ask-ui--sample-show)
                 (lambda (sample _overlay _question)
                   (setq shown-sample sample)))
                ((symbol-function 'mevedel-tool-ask-ui--sample-close)
                 (lambda () (setq shown-sample nil))))
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
        (should (equal "# Repository Guidelines\n- Run tests" shown-sample))
        (call-interactively (lookup-key rendered-keymap (kbd "n")))
        (should (equal "# Local notes" shown-sample))
        (call-interactively (lookup-key rendered-keymap (kbd "n")))
        (should-not shown-sample)
        (call-interactively (lookup-key rendered-keymap (kbd "1")))
        (should (equal "# Repository Guidelines\n- Run tests" shown-sample))
        (call-interactively (lookup-key rendered-keymap (kbd "RET")))
        (call-interactively (lookup-key rendered-keymap (kbd "C-c C-c")))
        (should (string-match-p "A1: Project AGENTS.md (Recommended)"
                                result))
        (should (string-match-p "Sample shown for A1:" result))
        (should (string-match-p "# Repository Guidelines" result)))))

  :doc "cursor movement does not re-announce the form to remote guests"
  (test-mevedel-tool-ask-ui--with-form
    (let ((mevedel-interaction-prompt-created-hook nil)
          (announcements 0))
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
      (dolist (key '("n" "p" "1" "C-n" "<down>" "C-p" "<up>"))
        (call-interactively (lookup-key rendered-keymap (kbd key))))
      (should (= 1 announcements))
      ;; An answer is a real change.
      (call-interactively (lookup-key rendered-keymap (kbd "RET")))
      (should (= 2 announcements))))

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
