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

;; `C source code' (graphical builds only)
(defvar overflow-newline-into-fringe)


(mevedel-deftest mevedel-tool-ask-ui/cold-compile
  (:doc "compiles without graphical variables and wraps rows in a terminal")
  (let* ((directory (make-temp-file "mevedel-ask-compile-" t))
         (source (file-name-concat directory "mevedel-tool-ask-ui.el")))
    (unwind-protect
        (progn
          (copy-file (locate-library "mevedel-tool-ask-ui") source)
          (with-temp-buffer
            (let ((status
                   (call-process
                    (expand-file-name invocation-name invocation-directory)
                    nil t nil "--batch" "-Q" "--eval"
                    (prin1-to-string
                     `(progn
                        (setq load-path ',load-path)
                        (require 'bytecomp)
                        ;; Terminal-only builds never define this symbol.
                        (unintern "overflow-newline-into-fringe" obarray)
                        (let ((byte-compile-error-on-warn t)
                              (byte-compile-verbose nil))
                          (unless (byte-compile-file ,source)
                            (error "Ask UI compilation failed")))
                        (load ,(concat source "c") nil t)
                        (with-temp-buffer
                          (set-window-buffer (selected-window) (current-buffer))
                          (let ((state (mevedel-tool-ask-ui--state-create
                                        :chat-buffer (current-buffer))))
                            (cl-assert (= (1- (window-body-width))
                                          (mevedel-tool-ask-ui--wrap-width state))))))))))
              (ert-info ((buffer-string))
                (should (eql status 0))
                (should (equal "" (buffer-string)))))))
      (delete-directory directory t))))


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

(defun test-mevedel-tool-ask-ui--show (callback questions)
  "Show QUESTIONS like `mevedel-tool-ask-ui-show', then visit them.
A new prompt never moves point, so the user visits it with \\`C-c C-j';
do the same when the selected window shows the prompt's view, including
the `post-command-hook' that follows the command."
  (mevedel-tool-ask-ui-show callback questions)
  (when-let* ((view (mevedel-view--interaction-target-buffer
                     (mevedel--prompt--data-buffer)))
              ((eq (window-buffer (selected-window)) view)))
    (with-current-buffer view
      (mevedel-view-jump-to-interaction)
      (run-hooks 'post-command-hook))))

(defun test-mevedel-tool-ask-ui--panel-rows (body)
  "Return BODY's sample panel rows as trimmed strings, top to bottom.
A panel row is a line whose text sits on the source block face."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (let (rows)
      (while (not (eobp))
        (when (< (+ (point) 5) (line-end-position))
          (when (memq 'mevedel-view-source-block
                      (ensure-list (get-text-property (+ (point) 5)
                                                      'font-lock-face)))
            (push (string-trim (buffer-substring-no-properties
                                (point) (line-end-position)))
                  rows)))
        (forward-line 1))
      (nreverse rows))))


;;
;;; Ask User

(mevedel-deftest mevedel-tool-ask-ui--option-label ()
  ,test
  (test)
  :doc "trim labels consistently for strings, objects, display and sample lookup"
  (progn
    (should (equal "A" (mevedel-tool-ask-ui--option-label " A ")))
    (should (equal "B" (mevedel-tool-ask-ui--option-label '(:label " B "))))
    (should (equal "A" (mevedel-tool-ask-ui--format-option " A ")))
    (should (equal "sample" (mevedel-tool-ask-ui--selected-sample
                             "B" '((:label " B " :sample "sample")))))))

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


(mevedel-deftest mevedel-tool-ask-ui--wrap-sample-line ()
  ,test
  (test)
  :doc "wrapping preserves text, properties and wide characters"
  (let* ((text (propertize "alpha beta 界界界界" 'font-lock-face 'bold))
         (rows (mevedel-tool-ask-ui--wrap-sample-line text 8)))
    (should (equal rows '("alpha " "↪ beta " "↪ 界界界" "↪ 界")))
    (should (cl-every (lambda (row) (<= (string-width row) 8)) rows))
    (should (eq 'bold (get-text-property 2 'font-lock-face (cadr rows))))
    (should (eq 'shadow (get-text-property 0 'font-lock-face (cadr rows)))))
  :doc "short and empty lines remain one row"
  (should (equal '("") (mevedel-tool-ask-ui--wrap-sample-line "" 8)))
  (should (equal '("short") (mevedel-tool-ask-ui--wrap-sample-line "short" 8)))
  :doc "tabs expand before wrapping even when narrower than a tab stop"
  (should (equal '("    " "↪   " "↪   " "↪ x")
                 (mevedel-tool-ask-ui--wrap-sample-line "\tx" 4)))
  :doc "oversized characters terminate even on a narrower continuation row"
  (let* ((byte (string-to-multibyte (unibyte-string 255)))
         (text (concat byte byte byte))
         (measure (symbol-function 'string-width))
         (calls 0))
    ;; Bound the old loop's width queries: a regression must fail, not hang.
    (cl-letf (((symbol-function 'string-width)
               (lambda (&rest args)
                 (when (> (cl-incf calls) 50)
                   (error "Wrapping failed to make progress"))
                 (apply measure args))))
      (should (equal (list byte (concat "↪ " byte) (concat "↪ " byte))
                     (mevedel-tool-ask-ui--wrap-sample-line text 4)))))
  :doc "composed emoji and combining marks never split across rows"
  (let* ((emoji (propertize "👍🏽" 'font-lock-face 'bold))
         (rows (mevedel-tool-ask-ui--wrap-sample-line
                (concat emoji emoji "é") 4)))
    (should (equal rows '("👍🏽" "↪ 👍🏽" "↪ é")))
    (should (eq 'bold (get-text-property 3 'font-lock-face (cadr rows)))))
  :doc "a space just beyond a full word stays before the line break"
  (should (equal '("abcdefgh " "↪ ijk")
                 (mevedel-tool-ask-ui--wrap-sample-line "abcdefgh ijk" 8))))

(mevedel-deftest mevedel-tool-ask-ui--sample-width ()
  ,test
  (test)
  :doc "panel width uses the view window and falls back to frame width"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show #'ignore [(:question "Q" :options ["A"])])
    (with-current-buffer view-buffer
      (let ((state (car mevedel-tool-ask-ui--states)))
        (should (= (mevedel-tool-ask-ui--sample-width state)
                   (max 4 (- (window-body-width (selected-window)) 7))))
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
          (should (= (mevedel-tool-ask-ui--sample-width state)
                     (max 4 (- (frame-width) 7)))))))))

(mevedel-deftest mevedel-tool-ask-ui--sample-lines ()
  ,test
  (test)
  :doc "samples are fontified once per form; rows are kept for the current width only"
  (let ((state (mevedel-tool-ask-ui--state-create))
        (fontify (symbol-function 'mevedel-view--fontify-as))
        (calls 0))
    (cl-letf (((symbol-function 'mevedel-view--fontify-as)
               (lambda (&rest args)
                 (cl-incf calls)
                 (apply fontify args))))
      (let ((rows (mevedel-tool-ask-ui--sample-lines state "abcdefghijk" 8)))
        (should (eq rows (mevedel-tool-ask-ui--sample-lines state "abcdefghijk" 8)))
        (should (= 1 calls))
        (should (equal '("abcdef" "↪ ghij" "↪ k")
                       (mevedel-tool-ask-ui--sample-lines state "abcdefghijk" 6)))
        ;; A new width re-wraps without fontifying again, and drops the
        ;; rows of the old width.
        (should (= 1 calls))
        (let ((cached (mevedel-tool-ask-ui--state-sample-rows state)))
          (should (= 6 (car cached)))
          (should (= 1 (hash-table-count (cdr cached)))))
        (should (equal rows (mevedel-tool-ask-ui--sample-lines
                             state "abcdefghijk" 8)))
        (should (= 1 calls)))))

  :doc "blank lines in several samples never share or corrupt cached rows"
  (let ((state (mevedel-tool-ask-ui--state-create)))
    (should (equal '("alpha" "" "omega")
                   (mevedel-tool-ask-ui--sample-lines state "alpha\n\nomega" 40)))
    (should (equal '("gamma" "" "delta")
                   (mevedel-tool-ask-ui--sample-lines state "gamma\n\ndelta" 40)))
    (should (equal '("one" "" "" "two")
                   (mevedel-tool-ask-ui--sample-lines state "one\n\n\ntwo" 40)))
    (should (equal '("alpha" "" "omega")
                   (mevedel-tool-ask-ui--sample-lines state "alpha\n\nomega" 40)))
    (should (equal '("") (mevedel-tool-ask-ui--wrap-sample-line "" 40))))

  :doc "a form whose samples contain blank lines opens and shows them"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
     #'ignore
     [(:question "First?" :options [(:label "A" :sample "alpha\n\nomega")
                                    (:label "B" :sample "gamma\n\ndelta")])
      (:question "Second?" :options [(:label "C" :sample "one\n\n\ntwo") "D"])])
    (should (equal (test-mevedel-tool-ask-ui--panel-rows rendered-body)
                   '("Sample · A" "alpha" "" "omega")))
    (call-interactively (lookup-key rendered-keymap (kbd "TAB")))
    (should (equal (test-mevedel-tool-ask-ui--panel-rows rendered-body)
                   '("Sample · C" "one" "" "" "two")))))

(mevedel-deftest mevedel-tool-ask-ui--wrap-sample-line-fresh ()
  ,test
  (test)
  :doc "every call returns a fresh list, so callers may splice it"
  (let ((first (mevedel-tool-ask-ui--wrap-sample-line "" 40))
        (second (mevedel-tool-ask-ui--wrap-sample-line "" 40)))
    (should-not (eq first second))
    (should-not (eq (mevedel-tool-ask-ui--wrap-sample-line "short" 40)
                    (mevedel-tool-ask-ui--wrap-sample-line "short" 40)))))

(mevedel-deftest mevedel-tool-ask-ui--sample-panel ()
  ,test
  (test)
  :doc "a question's panel is as tall as its longest sample and follows sample-bearing options at point"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
     (lambda (value) (setq result value))
     [(:question "Which file?"
       :options [(:label "Project" :sample "# Project\n- one\n- two\n")
                 (:label "Local" :sample "# Local")
                 "Neither"])
      (:question "Second?" :options ["Yes" "No"])])
    ;; The first sample shows before any motion; a trailing newline in the
    ;; sample does not add a row.
    (should (equal (test-mevedel-tool-ask-ui--panel-rows rendered-body)
                   '("Sample · Project" "# Project" "- one" "- two")))
    (call-interactively (lookup-key rendered-keymap (kbd "n")))
    ;; A shorter sample keeps the question's panel height, so nothing
    ;; below it moves while options are compared.
    (should (equal (test-mevedel-tool-ask-ui--panel-rows rendered-body)
                   '("Sample · Local" "# Local" "" "")))
    (with-current-buffer view-buffer
      (should (equal '(0 . 1) (get-text-property (point) 'mevedel-ask-entry)))
      (should (eq (char-after) ?○)))
    ;; Options without a sample, the other question, and returning to
    ;; the shown option leave the panel and the buffer untouched.
    (let ((body rendered-body)
          (tick (with-current-buffer view-buffer
                  (buffer-chars-modified-tick))))
      (dolist (key '("n" "n" "n" "p" "p" "p"))
        (call-interactively (lookup-key rendered-keymap (kbd key))))
      (with-current-buffer view-buffer
        (should (equal '(0 . 1) (get-text-property (point) 'mevedel-ask-entry)))
        (should (= tick (buffer-chars-modified-tick))))
      (should (eq body rendered-body)))
    ;; Answering collapses the question and its panel; the model is told
    ;; which sample the answer was chosen from.
    (call-interactively (lookup-key rendered-keymap (kbd "RET")))
    (should-not (test-mevedel-tool-ask-ui--panel-rows rendered-body))
    (call-interactively (lookup-key rendered-keymap (kbd "C-c C-c")))
    (should (string-match-p "A1: Local\nSample shown for A1:\n# Local\n" result)))

  :doc "wrapped samples keep a fixed row count and scroll by rows"
  (test-mevedel-tool-ask-ui--with-form
    (let ((mevedel-ask-sample-height 3))
      (cl-letf (((symbol-function 'mevedel-tool-ask-ui--sample-width)
                 (lambda (_) 40)))
        (test-mevedel-tool-ask-ui--show
         #'ignore
         (vector (list :question "Which?"
                       :options (vector (list :label (make-string 60 ?L)
                                              :sample (make-string 150 ?x))
                                        '(:label "Short" :sample "a")))))
        (let ((rows (test-mevedel-tool-ask-ui--panel-rows rendered-body)))
          (should (= 4 (length rows)))
          (should (cl-every (lambda (row) (<= (string-width row) 40)) rows))
          (should (string-prefix-p "↪ " (nth 2 rows)))
          (should (string-match-p "rows 1–3 of 4" (car rows))))
        (call-interactively (lookup-key rendered-keymap (kbd "C-v")))
        (let ((rows (test-mevedel-tool-ask-ui--panel-rows rendered-body)))
          (should (string-match-p "rows 2–4 of 4" (car rows)))
          (should (string-prefix-p "↪ " (cadr rows))))
        (call-interactively (lookup-key rendered-keymap (kbd "n")))
        (should (= 4 (length (test-mevedel-tool-ask-ui--panel-rows rendered-body))))
        (with-current-buffer view-buffer
          (let ((cache (mevedel-tool-ask-ui--state-sample-cache
                        (car mevedel-tool-ask-ui--states))))
            (should (= 2 (hash-table-count cache)))
            (call-interactively (lookup-key rendered-keymap (kbd "p")))
            (should (= 2 (hash-table-count cache))))))))

  :doc "a panel repaint triggered by motion preserves a multiline composer draft"
  (test-mevedel-tool-ask-ui--with-form
    (with-current-buffer view-buffer
      (mevedel-view-test--insert-composer-draft "> quoted\nsecond line" 4))
    (test-mevedel-tool-ask-ui--show
     #'ignore
     [(:question "Which?" :options [(:label "A" :sample "a") (:label "B" :sample "b")])])
    (with-current-buffer view-buffer
      (goto-char (cdr (assoc '(0 . 1) (mevedel-tool-ask-ui--entry-positions
                                      (car mevedel-tool-ask-ui--states)))))
      (run-hooks 'post-command-hook)
      (should (equal (test-mevedel-tool-ask-ui--panel-rows rendered-body)
                     '("Sample · B" "b")))
      (should (equal '(0 . 1) (get-text-property (point) 'mevedel-ask-entry)))
      (should (equal "> quoted\nsecond line" (mevedel-view--input-text))))))


(mevedel-deftest mevedel-tool-ask-ui--sample-scroll ()
  ,test
  (test)
  :doc "an overflowing panel pages with C-v and M-v and keeps point on its option"
  (test-mevedel-tool-ask-ui--with-form
    (let ((mevedel-ask-sample-height 3))
      (test-mevedel-tool-ask-ui--show
       #'ignore
       [(:question "Which?" :options [(:label "Long" :sample "a\nb\nc\nd\ne\nf")
                                      "Plain"])])
      (should (equal (test-mevedel-tool-ask-ui--panel-rows rendered-body)
                     '("Sample · Long  ·  rows 1–3 of 6  C-v/M-v" "a" "b" "c")))
      ;; A page keeps one line of context.
      (call-interactively (lookup-key rendered-keymap (kbd "C-v")))
      (should (equal (cdr (test-mevedel-tool-ask-ui--panel-rows rendered-body))
                     '("c" "d" "e")))
      ;; The last page stops at the sample's end.
      (call-interactively (lookup-key rendered-keymap (kbd "C-v")))
      (should (equal (test-mevedel-tool-ask-ui--panel-rows rendered-body)
                     '("Sample · Long  ·  rows 4–6 of 6  C-v/M-v" "d" "e" "f")))
      (call-interactively (lookup-key rendered-keymap (kbd "M-v")))
      (call-interactively (lookup-key rendered-keymap (kbd "M-v")))
      (should (equal (cdr (test-mevedel-tool-ask-ui--panel-rows rendered-body))
                     '("a" "b" "c")))
      (with-current-buffer view-buffer
        (should (equal '(0 . 0) (get-text-property (point) 'mevedel-ask-entry))))))

  :doc "collapsed answers scroll the window, not their hidden sample panel"
  (test-mevedel-tool-ask-ui--with-form
    (let ((mevedel-ask-sample-height 2)
          scrolled)
      (cl-letf (((symbol-function 'scroll-up-command)
                 (lambda () (interactive) (push 'up scrolled)))
                ((symbol-function 'scroll-down-command)
                 (lambda () (interactive) (push 'down scrolled))))
        (test-mevedel-tool-ask-ui--show
         #'ignore
         [(:question "Which?" :options [(:label "Long" :sample "a\nb\nc\nd")])])
        (call-interactively (lookup-key rendered-keymap (kbd "RET")))
        (with-current-buffer view-buffer
          (mevedel-tool-ask-ui--goto-entry
           (car mevedel-tool-ask-ui--states) '(0 . summary)))
        (should-not (test-mevedel-tool-ask-ui--panel-rows rendered-body))
        (call-interactively (lookup-key rendered-keymap (kbd "C-v")))
        (call-interactively (lookup-key rendered-keymap (kbd "M-v")))
        (should (equal scrolled '(down up))))))

  :doc "without an overflowing panel C-v and M-v scroll the window as usual"
  (test-mevedel-tool-ask-ui--with-form
    (let (scrolled)
      (cl-letf (((symbol-function 'scroll-up-command)
                 (lambda () (interactive) (push 'up scrolled)))
                ((symbol-function 'scroll-down-command)
                 (lambda () (interactive) (push 'down scrolled))))
        (test-mevedel-tool-ask-ui--show
         #'ignore
         [(:question "Which?" :options [(:label "Short" :sample "a") "Plain"])
          (:question "Second?" :options ["Yes" "No"])])
        (call-interactively (lookup-key rendered-keymap (kbd "C-v")))
        (with-current-buffer view-buffer
          (mevedel-tool-ask-ui--goto-entry (car mevedel-tool-ask-ui--states) '(1 . 0)))
        (call-interactively (lookup-key rendered-keymap (kbd "M-v")))
        (should (equal scrolled '(down up)))))))


(mevedel-deftest mevedel-tool-ask-ui--entry-at-point ()
  ,test
  (test)
  :doc "entry identity covers descriptions and custom input, but not adjacent prose"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
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
        ;; The newline ending a row still belongs to it.
        (end-of-line)
        (should (equal '(0 . 2) (mevedel-tool-ask-ui--entry-at-point state)))
        (forward-line 1)
        (should-not (mevedel-tool-ask-ui--entry-at-point state))
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
  :doc "native motion synchronizes samples only on entry changes, not on leaving the form"
  (test-mevedel-tool-ask-ui--with-form
    (let (focused)
      (cl-letf (((symbol-function 'mevedel-tool-ask-ui--sync-sample)
                 (lambda (_state entry) (push entry focused))))
        (test-mevedel-tool-ask-ui--show
         #'ignore
         [(:question "Layout?"
           :options [(:label "Compact" :description "A long description") "Spacious"])])
        (should (equal focused '((0 . 0))))
        (with-current-buffer view-buffer
          (let ((tick (buffer-chars-modified-tick)))
            (forward-char 3)
            (run-hooks 'post-command-hook)
            (should (equal focused '((0 . 0))))
            (search-forward "Spacious")
            (backward-char)
            (run-hooks 'post-command-hook)
            (should (equal focused '((0 . 1) (0 . 0))))
            (should (equal '(0 . 1) mevedel-tool-ask-ui--active-entry))
            (goto-char (point-max))
            (run-hooks 'post-command-hook)
            (should (equal focused '((0 . 1) (0 . 0))))
            (should-not mevedel-tool-ask-ui--active-entry)
            (should (= tick (buffer-chars-modified-tick)))))))))


(mevedel-deftest mevedel-tool-ask-ui--entry-option ()
  ,test
  (test)
  :doc "only option rows name an option; custom, summary and Submit do not"
  (let ((state (mevedel-tool-ask-ui--state-create
                :questions '((:question "Q?" :options ["A" (:label "B")])))))
    (should (equal "A" (mevedel-tool-ask-ui--entry-option state '(0 . 0))))
    (should (equal '(:label "B") (mevedel-tool-ask-ui--entry-option state '(0 . 1))))
    (should-not (mevedel-tool-ask-ui--entry-option state '(0 . 2)))
    (should-not (mevedel-tool-ask-ui--entry-option state '(0 . summary)))
    (should-not (mevedel-tool-ask-ui--entry-option state 'submit))
    (should-not (mevedel-tool-ask-ui--entry-option state nil))))


(mevedel-deftest mevedel-tool-ask-ui--question-open-p ()
  ,test
  (test)
  :doc "exactly the question named open is open, answered or not"
  (let ((state (mevedel-tool-ask-ui--state-create
                :answers (vector nil "A" "B") :open 2)))
    (should-not (mevedel-tool-ask-ui--question-open-p state 0))
    (should-not (mevedel-tool-ask-ui--question-open-p state 1))
    (should (mevedel-tool-ask-ui--question-open-p state 2))
    (setf (mevedel-tool-ask-ui--state-open state) nil)
    (should-not (mevedel-tool-ask-ui--question-open-p state 2))))


(mevedel-deftest mevedel-tool-ask-ui--open-question ()
  ,test
  (test)
  :doc "opening closes the previous question and lands on the answer or first option"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
     #'ignore
     [(:question "One?" :options ["A" "B"])
      (:question "Two?" :options ["C" "D"])])
    (with-current-buffer view-buffer
      (let ((state (car mevedel-tool-ask-ui--states)))
        (mevedel-tool-ask-ui--open-question state 1)
        (should (equal '(1 . 0) (get-text-property (point) 'mevedel-ask-entry)))
        (should-not (string-match-p "○ 1  A" rendered-body))
        (should (string-match-p "○ 1  C" rendered-body))
        ;; Opening the open question again only moves point.
        (let ((body rendered-body))
          (mevedel-tool-ask-ui--open-question state 1)
          (should (eq body rendered-body)))
        (call-interactively (lookup-key rendered-keymap (kbd "2")))
        (call-interactively (lookup-key rendered-keymap (kbd "RET")))
        (mevedel-tool-ask-ui--open-question state 1)
        (should (equal '(1 . 1) (get-text-property (point) 'mevedel-ask-entry)))))))


(mevedel-deftest mevedel-tool-ask-ui--text-rows ()
  ,test
  (test)
  :doc "counts word-wrapped screen rows, wrap prefixes and over-long words included"
  (dolist (case `(("" 10 1)
                  ("abc" 10 1)
                  ("aaaa bbbb" 9 1)
                  ("aaaa bbbb c" 9 2)
                  ("a\nb\n" 10 3)
                  (,(propertize "aaaa bbbb cccc" 'wrap-prefix "  ") 9 2)
                  (,(make-string 25 ?x) 10 3)
                  (,(propertize (concat "aa " (make-string 20 ?x))
                                'wrap-prefix "    ")
                   10 5)))
    (pcase-let ((`(,text ,width ,rows) case))
      (should (equal (list text rows)
                     (list text (mevedel-tool-ask-ui--text-rows text width)))))))


(mevedel-deftest mevedel-tool-ask-ui--form-body ()
  ,test
  (test)
  :doc "the form keeps one height whichever question is open, and none"
  (test-mevedel-tool-ask-ui--with-form
    (cl-letf (((symbol-function 'mevedel-tool-ask-ui--form-width) (lambda (_) 60))
              ((symbol-function 'mevedel-tool-ask-ui--wrap-width) (lambda (_) 59)))
      (test-mevedel-tool-ask-ui--show
       #'ignore
       [(:question "First, with a long sample?"
         :options [(:label "A" :sample "1\n2\n3\n4\n5\n6\n7\n8")
                   (:label "B" :description "A description long enough to wrap onto a second row here.")])
        (:question "Second?" :options ["C" "D"])])
      (let ((rows (lambda () (mevedel-tool-ask-ui--text-rows rendered-body 60)))
            heights)
        (push (funcall rows) heights)
        (dolist (key '("TAB" "<backtab>" "RET" "RET"))
          (call-interactively (lookup-key rendered-keymap (kbd key)))
          (push (funcall rows) heights))
        ;; Every layout, the last with nothing open, has the same height.
        (should (with-current-buffer view-buffer
                  (eq 'submit (get-text-property (point) 'mevedel-ask-entry))))
        (should (= 1 (length (delete-dups heights)))))))

  :doc "a row filling a terminal window wraps there, and the height still holds"
  (test-mevedel-tool-ask-ui--with-form
    (let* ((width (window-body-width))
           ;; Option descriptions start ten columns in; fill the row exactly.
           (description
            (let ((words (string-trim-right
                          (substring (apply #'concat (make-list width "word "))
                                     0 (- width 15)))))
              (concat words (make-string (- width 10 (length words)) ?x))))
           (screen-rows
            (lambda ()
              (with-current-buffer view-buffer
                (count-screen-lines (overlay-start registered-overlay)
                                    (overlay-end registered-overlay)))))
           heights)
      (should-not (display-graphic-p))
      (test-mevedel-tool-ask-ui--show
       #'ignore
       (vector (list :question "Full-width descriptions?"
                     :options (vector (list :label "One" :description description)
                                      (list :label "Two" :description description)))
               '(:question "Plain?"
                 :options [(:label "Alpha" :description "a")
                           (:label "Beta" :description "b")
                           (:label "Gamma" :description "c")
                           (:label "Delta" :description "d")])))
      (push (funcall screen-rows) heights)
      (dolist (key '("TAB" "<backtab>"))
        (call-interactively (lookup-key rendered-keymap (kbd key)))
        (push (funcall screen-rows) heights))
      (should (= 1 (length (delete-dups heights)))))))

(mevedel-deftest mevedel-tool-ask-ui--wrap-width ()
  ,test
  (test)
  :doc "a window without a right fringe loses the continuation column"
  (test-mevedel-tool-ask-ui--with-form
    (let ((state (mevedel-tool-ask-ui--state-create :chat-buffer view-buffer))
          (overflow-newline-into-fringe t))
      (should (= (1- (window-body-width))
                 (mevedel-tool-ask-ui--wrap-width state)))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'window-fringes) (lambda (&rest _) '(8 8 nil nil))))
        (should (= (window-body-width) (mevedel-tool-ask-ui--wrap-width state)))
        (let ((overflow-newline-into-fringe nil))
          (should (= (1- (window-body-width))
                     (mevedel-tool-ask-ui--wrap-width state)))))))

  :doc "every displaying window's own fringe policy sets the minimum capacity"
  (test-mevedel-tool-ask-ui--with-form
    (let* ((state (mevedel-tool-ask-ui--state-create :chat-buffer view-buffer))
           (overflow-newline-into-fringe t)
           (wide (selected-window))
           (narrow (split-window-right -30)))
      (set-window-buffer narrow view-buffer)
      (let ((narrow-width (window-body-width narrow))
            (wide-width (window-body-width wide)))
        (should (< narrow-width wide-width))
        ;; Fringes are injected per window; a graphical frame is simulated.
        (cl-flet ((capacity (fringed)
                    (cl-letf (((symbol-function 'display-graphic-p)
                               (lambda (&rest _) t))
                              ((symbol-function 'window-fringes)
                               (lambda (&optional window)
                                 (if (memq (or window (selected-window)) fringed)
                                     '(8 8 nil nil)
                                   '(0 0 nil nil)))))
                      (mevedel-tool-ask-ui--wrap-width state))))
          ;; The wide, fringed selected window must not grant the narrow,
          ;; fringeless one its last column.
          (should (= (1- narrow-width) (capacity (list wide))))
          ;; The reverse: the narrow fringed window holds all its columns.
          (should (= narrow-width (capacity (list narrow))))
          (should (= narrow-width (capacity (list wide narrow))))
          (should (= (1- narrow-width) (capacity nil)))
          ;; A wide window without a fringe is the limit when it is closer.
          (delete-window narrow)
          (should (= (window-body-width wide) (capacity (list wide))))))))

  :doc "a view without a window falls back to the selected window, fringeless"
  (test-mevedel-tool-ask-ui--with-form
    (let ((state (mevedel-tool-ask-ui--state-create :chat-buffer view-buffer)))
      (set-window-buffer (selected-window) (get-buffer-create " *ask-other*"))
      (unwind-protect
          (with-current-buffer view-buffer
            (should (= (1- (window-body-width))
                       (mevedel-tool-ask-ui--wrap-width state))))
        (kill-buffer " *ask-other*")))))

(mevedel-deftest mevedel-tool-ask-ui--width-changed ()
  ,test
  (test)
  :doc "a narrower window lays the form out again before the next keystroke"
  (test-mevedel-tool-ask-ui--with-form
    (with-current-buffer view-buffer
      (mevedel-view-test--insert-composer-draft "> draft\nnext" 3))
    (test-mevedel-tool-ask-ui--show
     #'ignore
     [(:question "Where should the long-form guidance for this project live?"
       :options [(:label "AGENTS.md" :description "Shared with every contributor and agent, loaded into every session automatically.")
                 (:label "docs/development.md" :description "Already owns the test commands and the compile checks.")])
      (:question "Ship?" :options ["Yes" "No"])])
    (with-current-buffer view-buffer
      (goto-char (+ (mevedel-view--input-start) 3)))
    (let* ((state (car (buffer-local-value 'mevedel-tool-ask-ui--states view-buffer)))
           (wide (car (mevedel-tool-ask-ui--state-height state))))
      (split-window-right 30)
      (with-current-buffer view-buffer
        (mevedel-view-zone--on-window-change nil))
      (let ((narrow (mevedel-tool-ask-ui--wrap-width state))
            ;; Batch `vertical-motion' ignores `wrap-prefix', so rows are
            ;; counted the way the form counts them, at the new width.
            (rows (lambda ()
                    (mevedel-tool-ask-ui--text-rows
                     rendered-body (mevedel-tool-ask-ui--wrap-width state))))
            heights)
        (should (< narrow wide))
        (should (eql narrow (car (mevedel-tool-ask-ui--state-height state))))
        (with-current-buffer view-buffer
          (should (equal "> draft\nnext" (mevedel-view--input-text)))
          (should (= (point) (+ (mevedel-view--input-start) 3))))
        (push (funcall rows) heights)
        (with-current-buffer view-buffer
          (mevedel-view-jump-to-interaction)
          (dolist (key '("TAB" "<backtab>"))
            (call-interactively (lookup-key rendered-keymap (kbd key)))
            (push (funcall rows) heights)))
        (should (= 1 (length (delete-dups heights))))))))


(mevedel-deftest mevedel-tool-ask-ui--closed-heading ()
  ,test
  (test)
  :doc "a closed question fits one line, keeps its answer whole, and says so on hover"
  (let ((state (mevedel-tool-ask-ui--state-create)))
    (cl-letf (((symbol-function 'mevedel-tool-ask-ui--form-width) (lambda (_) 40)))
      (let ((line (mevedel-tool-ask-ui--closed-heading
                   state "  1  " "A long question\nthat wraps onto two lines?" nil)))
        (should (<= (string-width line) 39))
        (should (string-suffix-p "…" line))
        (should-not (string-search "\n" line)))
      (let ((line (mevedel-tool-ask-ui--closed-heading
                   state "✓ 1  " "A long question that needs cutting?" "Yes")))
        (should (string-suffix-p "→ Yes" line))
        (should (<= (string-width line) 39)))
      ;; A long, multi-line custom answer is flattened and cut too, keeping
      ;; at least half of the room.
      (let ((line (mevedel-tool-ask-ui--closed-heading
                   state "✓ 1  " "Which layout?"
                   "A very long custom answer\nthat goes on and on and on")))
        (should-not (string-search "\n" line))
        (should (<= (string-width line) 39))
        (should (string-match-p "Which layout\\?  → A very long" line))
        (should (string-suffix-p "…" line)))
      (should (equal "  1  Short?"
                     (substring-no-properties
                      (mevedel-tool-ask-ui--closed-heading
                       state "  1  " "Short?" nil)))))))


(mevedel-deftest mevedel-tool-ask-ui--refresh ()
  ,test
  (test)
  :doc "a panel repaint keeps point on its row when a re-wrapped panel above it grew"
  (test-mevedel-tool-ask-ui--with-form
    (let ((width 60))
      (cl-letf (((symbol-function 'mevedel-tool-ask-ui--sample-width)
                 (lambda (_state) width)))
        (test-mevedel-tool-ask-ui--show
         #'ignore
         [(:question "First?"
           :options [(:label "Wide" :sample "one two three four five six seven eight nine ten eleven")
                     "Plain"])
          (:question "Second?"
           :options [(:label "A" :sample "a") (:label "B" :sample "b")])])
        (with-current-buffer view-buffer
          (let ((state (car mevedel-tool-ask-ui--states)))
            ;; Point is on the closed second question, below the open
            ;; first question's panel.
            (mevedel-tool-ask-ui--goto-entry state '(1 . summary))
            ;; The window narrows: that panel now wraps into more rows
            ;; above point.
            (setq width 20)
            (mevedel-tool-ask-ui--refresh state)
            (should (equal '(1 . summary)
                           (get-text-property (point) 'mevedel-ask-entry)))
            (should (looking-at-p "2  Second")))))))

  :doc "a settled form is never repainted"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show #'ignore [(:question "First?" :options ["A"])])
    (let ((state (with-current-buffer view-buffer (car mevedel-tool-ask-ui--states)))
          (registrations 0))
      (mevedel--prompt--settle registered-overlay 'aborted)
      (cl-letf (((symbol-function 'mevedel-view--interaction-register)
                 (lambda (&rest _) (cl-incf registrations))))
        (mevedel-tool-ask-ui--refresh state))
      (should (= 0 registrations))))

  :doc "point on a heading keeps its place when a re-wrapped panel above it grew"
  (test-mevedel-tool-ask-ui--with-form
    (let ((width 60))
      (cl-letf (((symbol-function 'mevedel-tool-ask-ui--sample-width)
                 (lambda (_state) width)))
        (test-mevedel-tool-ask-ui--show
         #'ignore
         [(:question "First?"
           :options [(:label "Wide" :sample "one two three four five six seven eight nine ten eleven")
                     "Plain"])
          (:question "Second heading?" :options ["A" "B"])])
        (with-current-buffer view-buffer
          (let ((state (car mevedel-tool-ask-ui--states)))
            (search-forward "Second heading?")
            (backward-char 3)
            (setq width 20)
            (mevedel-tool-ask-ui--refresh state)
            (should (looking-at-p "ng\\?"))
            (should (= 1 (get-text-property (point) 'mevedel-ask-question)))))))))


(mevedel-deftest mevedel-tool-ask-ui--anchor-at-point ()
  ,test
  (test)
  :doc "entries anchor at their glyph, headings and panels at their block, prose nowhere"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
     #'ignore
     [(:question "First?" :options [(:label "A" :sample "alpha")])])
    (with-current-buffer view-buffer
      (let* ((state (car mevedel-tool-ask-ui--states))
             (glyph (mevedel-tool-ask-ui--anchor-start state '(0 . 0)))
             (block (mevedel-tool-ask-ui--anchor-start state '(question . 0))))
        (should (equal '((0 . 0) . 0) (mevedel-tool-ask-ui--anchor-at-point state)))
        (should (< block glyph))
        (search-forward "alpha")
        (let ((anchor (mevedel-tool-ask-ui--anchor-at-point state)))
          (should (equal '(question . 0) (car anchor)))
          (should (= (point) (+ block (cdr anchor))))
          (should (mevedel-tool-ask-ui--anchor-p (car anchor) (point))))
        (should-not (mevedel-tool-ask-ui--anchor-p '(0 . 0) (point)))
        (goto-char (overlay-end registered-overlay))
        (should-not (mevedel-tool-ask-ui--anchor-at-point state))))))


(mevedel-deftest mevedel-tool-ask-ui--retire ()
  ,test
  (test)
  :doc "settling concurrent forms removes only their own motion tracking"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show #'ignore [(:question "First?" :options ["Yes"])])
    (let ((first registered-overlay))
      (test-mevedel-tool-ask-ui--show #'ignore [(:question "Second?" :options ["Yes"])])
      (with-current-buffer view-buffer
        (should (= 2 (length mevedel-tool-ask-ui--states)))
        (mevedel--prompt--settle first 'aborted)
        (should (= 1 (length mevedel-tool-ask-ui--states)))
        (should (memq #'mevedel-tool-ask-ui--post-command post-command-hook))
        (mevedel--prompt--settle registered-overlay 'aborted)
        (should-not mevedel-tool-ask-ui--states)
        (should-not mevedel-tool-ask-ui--active-state)
        (should-not (memq #'mevedel-tool-ask-ui--post-command post-command-hook))))))


(mevedel-deftest mevedel-tool-ask-ui--question-heads ()
  ,test
  (test)
  :doc "each question's first entry in form order, an answer's summary included, then Submit"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
     #'ignore
     [(:question "One?" :options ["A" "B"])
      (:question "Two?" :options ["C" "D"])])
    (call-interactively (lookup-key rendered-keymap (kbd "RET")))
    (with-current-buffer view-buffer
      (let ((heads (mevedel-tool-ask-ui--question-heads
                    (car mevedel-tool-ask-ui--states))))
        (should (equal (mapcar (lambda (head) (cons (car head) (cadr head))) heads)
                       '((0 . (0 . summary)) (1 . (1 . 0)) (submit . submit))))
        (should (apply #'< (mapcar #'cddr heads)))))))


(mevedel-deftest mevedel-tool-ask-ui--multi-p ()
  ,test
  (test)
  :doc "only true enables multi-select"
  (let ((state (mevedel-tool-ask-ui--state-create
                :questions '((:multiSelect t) (:multiSelect :json-false) nil))))
    (should (mevedel-tool-ask-ui--multi-p state 0))
    (should-not (mevedel-tool-ask-ui--multi-p state 1))
    (should-not (mevedel-tool-ask-ui--multi-p state 2))))

(mevedel-deftest mevedel-tool-ask-ui--normalize-answer ()
  ,test
  (test)
  :doc "normalization trims, orders and preserves blank multi as no preference"
  (let ((state (mevedel-tool-ask-ui--state-create
                :questions '((:options ["A" "B"] :multiSelect t)
                             (:options ["Yes"])))))
    (should (equal '("A" "B" "Extra")
                   (mevedel-tool-ask-ui--normalize-answer state 0 '("B" "Extra" "A"))))
    (should-not (mevedel-tool-ask-ui--normalize-answer state 0 nil))
    (should-not (mevedel-tool-ask-ui--normalize-answer state 0 '(" ")))
    (should (equal "Yes" (mevedel-tool-ask-ui--normalize-answer state 1 " Yes ")))
    (should-not (mevedel-tool-ask-ui--normalize-answer state 1 " "))))

(mevedel-deftest mevedel-tool-ask-ui--remote-questions ()
  ,test
  (test)
  :doc "wire flags and answer arrays survive actual JSON encoding including empty multi"
  (let* ((state (mevedel-tool-ask-ui--state-create
                 :questions '((:question "Multi" :multiSelect t :options ["A" "B"])
                              (:question "Empty" :multiSelect t :options ["C"])
                              (:question "Single" :options ["Yes"]))
                 :answers [("A" "B") nil "Yes"]))
         (decoded (json-parse-string
                   (json-encode (vconcat (mevedel-tool-ask-ui--remote-questions state)))
                   :object-type 'alist)))
    (should (eq t (alist-get 'multiSelect (aref decoded 0))))
    (should (equal ["A" "B"] (alist-get 'answer (aref decoded 0))))
    (should (equal [] (alist-get 'answer (aref decoded 1))))
    (should (equal "Yes" (alist-get 'answer (aref decoded 2))))))

(mevedel-deftest mevedel-tool-ask-ui--dispatch ()
  ,test
  (test)
  :doc "a single sampled option with whitespace keeps its sample after submission"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
     (lambda (value) (setq result value))
     [(:question "Single?" :options [(:label " A " :sample "alpha")])])
    (call-interactively (lookup-key rendered-keymap (kbd "RET")))
    (call-interactively (lookup-key rendered-keymap (kbd "C-c C-c")))
    (should (string-match-p "A1: A\nSample shown for A1:\nalpha" result)))

  :doc "trimmed single and multi labels retain selected samples and wire identities"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
     (lambda (value) (setq result value))
     [(:question "Single?" :options [(:label " A " :sample "alpha")])
      (:question "Multi?" :multiSelect t
       :options [(:label " B " :sample "beta") (:label " C " :sample "gamma")])])
    (dolist (key '("RET" "RET" "2" "RET"))
      (call-interactively (lookup-key rendered-keymap (kbd key))))
    (let* ((remote (overlay-get registered-overlay 'mevedel--remote))
           (questions (funcall (plist-get remote :questions))))
      (should (equal "A" (cdr (assoc "answer" (car questions)))))
      (should (equal ["B" "C"] (cdr (assoc "answer" (cadr questions))))))
    (call-interactively (lookup-key rendered-keymap (kbd "C-c C-c")))
    (should (string-match-p "A1: A\nSample shown for A1:\nalpha" result))
    (should (string-match-p "A2:\n- B\n- C\n" result))
    (should (string-match-p "beta" result))
    (should (string-match-p "gamma" result)))

  :doc "multi toggles stay open, keep geometry and drafts, and submit every selected sample"
  (test-mevedel-tool-ask-ui--with-form
    (with-current-buffer view-buffer
      (mevedel-view-test--insert-composer-draft "> quoted\nsecond line" 4))
    (test-mevedel-tool-ask-ui--show
     (lambda (value) (setq result value))
     [(:question "Which?" :multiSelect t
       :options [(:label "A" :sample "alpha") (:label "B" :sample "beta")])
      (:question "Single?" :options ["Yes" "No"])])
    (with-current-buffer view-buffer
      (let* ((state (car mevedel-tool-ask-ui--states))
             (height (mevedel-tool-ask-ui--text-rows rendered-body 79)))
        (dolist (key '("2" "RET" "1" "RET"))
          (call-interactively (lookup-key rendered-keymap (kbd key))))
        (should (equal '("A" "B") (aref (mevedel-tool-ask-ui--state-answers state) 0)))
        (should (eql 0 (mevedel-tool-ask-ui--state-open state)))
        (should (string-match-p "☑ 1  A" rendered-body))
        (should (string-match-p "☑ 2  B" rendered-body))
        (should (= height (mevedel-tool-ask-ui--text-rows rendered-body 79)))
        (call-interactively (lookup-key rendered-keymap (kbd "TAB")))
        (should (string-match-p "A, B" rendered-body))
        (call-interactively (lookup-key rendered-keymap (kbd "<backtab>")))
        (call-interactively (lookup-key rendered-keymap (kbd "RET")))
        (should (equal '("B") (aref (mevedel-tool-ask-ui--state-answers state) 0)))
        (call-interactively (lookup-key rendered-keymap (kbd "RET")))
        (should (equal "> quoted\nsecond line" (mevedel-view--input-text)))
        (call-interactively (lookup-key rendered-keymap (kbd "C-c C-c")))))
    (should (string-match-p "A1:\n- A\n- B\n" result))
    (should (string-match-p "alpha" result))
    (should (string-match-p "beta" result))
    (should (string-match-p "A2: (no preference" result)))

  :doc "multi custom edits replace or clear only custom text and preserve selected labels"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show #'ignore
     [(:question "Which?" :multiSelect t :options ["A" "B"])])
    (with-current-buffer view-buffer
      (let ((state (car mevedel-tool-ask-ui--states)) (initial nil))
        (call-interactively (lookup-key rendered-keymap (kbd "RET")))
        (dolist (text '("Extra" "Revised" "  "))
          (cl-letf (((symbol-function 'read-string)
                     (lambda (_prompt value &rest _)
                       (should (equal initial value)) text)))
            (call-interactively (lookup-key rendered-keymap (kbd "c"))))
          (setq initial text)
          (unless (string-blank-p text)
            (should (string-match-p (concat "Custom input… " text) rendered-body)))
          (should (equal (if (string-blank-p text) '("A") (list "A" text))
                         (aref (mevedel-tool-ask-ui--state-answers state) 0))))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "A")))
          (call-interactively (lookup-key rendered-keymap (kbd "c"))))
        (should (equal '("A") (aref (mevedel-tool-ask-ui--state-answers state) 0)))
        (call-interactively (lookup-key rendered-keymap (kbd "1")))
        (call-interactively (lookup-key rendered-keymap (kbd "RET")))
        (should-not (aref (mevedel-tool-ask-ui--state-answers state) 0))
        (should (string-match-p "0 of 1 answered" rendered-body)))))

  :doc "remote multi shape is validated atomically and normalized before submission"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show (lambda (value) (setq result value))
     [(:question "Single?" :options ["Yes"])
      (:question "Multi?" :multiSelect t :options ["A" "B"])
      (:question "Empty?" :multiSelect t :options ["C"])])
    (let ((answer (plist-get (overlay-get registered-overlay 'mevedel--remote) :answer)))
      (dolist (bad '(("Yes" "A" nil) (("Yes") ("A") nil)
                     ("Yes" ("A" 2) nil) ("Yes" ("X" "Y") nil)
                     ("Yes" ("A" "A") nil) ("Yes")))
        (should-error (funcall answer bad) :type 'user-error)
        (should-not result)
        (with-current-buffer view-buffer
          (should (equal [nil nil nil]
                         (mevedel-tool-ask-ui--state-answers
                          (car mevedel-tool-ask-ui--states))))))
      (funcall answer '("Yes" (" B " "Custom" "A") nil)))
    (should (string-match-p "A1: Yes\n" result))
    (should (string-match-p "A2:\n- A\n- B\n- Custom\n" result))
    (should (string-match-p "A3: (no preference" result)))

  :doc "TAB and S-TAB open the next or previous question; Submit is the last stop"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
     #'ignore
     [(:question "One?" :options ["A" "B"])
      (:question "Two?" :options [(:label "C" :description "Sea") "D"])
      (:question "Three?" :options ["E"])])
    (should (string-match-p "TAB question" rendered-body))
    ;; Closed questions stack without blank lines between them.
    (should (string-match-p "  2  Two\\?\n  3  Three\\?\n" rendered-body))
    (with-current-buffer view-buffer
      (let* ((entry (lambda () (get-text-property (point) 'mevedel-ask-entry)))
             (key (lambda (keys)
                    (call-interactively (lookup-key rendered-keymap (kbd keys)))))
             (open (lambda () (mevedel-tool-ask-ui--state-open
                               (car mevedel-tool-ask-ui--states)))))
        (funcall key "TAB")
        (should (equal '(1 . 0) (funcall entry)))
        (should (eql 1 (funcall open)))
        ;; From inside a question's description, TAB still means "next".
        (search-forward "Sea")
        (funcall key "TAB")
        (should (equal '(2 . 0) (funcall entry)))
        (funcall key "TAB")
        (should (eq 'submit (funcall entry)))
        ;; Reaching Submit leaves the last question open.
        (should (eql 2 (funcall open)))
        ;; Submit is the last stop.
        (funcall key "TAB")
        (should (eq 'submit (funcall entry)))
        (funcall key "<backtab>")
        (should (equal '(2 . 0) (funcall entry)))
        (funcall key "<backtab>")
        (funcall key "<backtab>")
        (should (equal '(0 . 0) (funcall entry)))
        (should (eql 0 (funcall open)))
        (funcall key "<backtab>")
        (should (equal '(0 . 0) (funcall entry))))))

  :doc "moving onto a closed question never opens it; RET or a click does"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
     #'ignore
     [(:question "One?" :options ["A"])
      (:question "Two?" :options ["C" "D"])])
    (with-current-buffer view-buffer
      (let ((tick (buffer-chars-modified-tick)))
        (dolist (key '("n" "n"))
          (call-interactively (lookup-key rendered-keymap (kbd key))))
        (should (equal '(1 . summary) (get-text-property (point) 'mevedel-ask-entry)))
        (should (= tick (buffer-chars-modified-tick)))
        ;; Digits and `c' act only inside the open question.
        (call-interactively (lookup-key rendered-keymap (kbd "2")))
        (should (equal '(1 . summary) (get-text-property (point) 'mevedel-ask-entry)))
        (should (= tick (buffer-chars-modified-tick)))
        (call-interactively (lookup-key rendered-keymap (kbd "RET")))
        (should (equal '(1 . 0) (get-text-property (point) 'mevedel-ask-entry)))
        (should (string-match-p "○ 1  C" rendered-body)))))

  :doc "a single question does not advertise question jumps"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show #'ignore [(:question "One?" :options ["A" "B"])])
    (should-not (string-match-p "TAB" rendered-body)))

  :doc "navigation never repaints and RET on a description answers its own row"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
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
    (test-mevedel-tool-ask-ui--show
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

  :doc "a custom answer that returns after the form was settled changes nothing"
  (test-mevedel-tool-ask-ui--with-form
    (let ((settled 0))
      (test-mevedel-tool-ask-ui--show
       (lambda (_value) (cl-incf settled))
       [(:question "Layout?" :options ["Compact" "Spacious"])
        (:question "Colour?" :options ["Red" "Blue"])])
      (let ((overlay registered-overlay))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _)
                     ;; A run abort settles the form while the minibuffer is open.
                     (mevedel--prompt--settle overlay 'aborted)
                     (setq rendered-body nil)
                     "Late answer")))
          (call-interactively (lookup-key rendered-keymap (kbd "c")))))
      (should (= 1 settled))
      ;; Nothing re-registered the settled form.
      (should-not rendered-body)
      (with-current-buffer view-buffer
        (should-not mevedel-tool-ask-ui--states))))

  :doc "n from the start of a row moves to the next row"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show #'ignore [(:question "Layout?" :options ["A" "B"])])
    (with-current-buffer view-buffer
      (beginning-of-line)
      (should (equal '(0 . 0) (get-text-property (point) 'mevedel-ask-entry)))
      (call-interactively (lookup-key rendered-keymap (kbd "n")))
      (should (equal '(0 . 1) (get-text-property (point) 'mevedel-ask-entry)))
      (beginning-of-line)
      (call-interactively (lookup-key rendered-keymap (kbd "p")))
      (should (equal '(0 . 0) (get-text-property (point) 'mevedel-ask-entry)))))

  :doc "digits focus options from the heading but never the custom row"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show #'ignore [(:question "Layout?" :options ["A" "B"])])
    (with-current-buffer view-buffer
      (search-backward "Layout?")
      (call-interactively (lookup-key rendered-keymap (kbd "2")))
      (should (equal '(0 . 1) (get-text-property (point) 'mevedel-ask-entry)))
      (call-interactively (lookup-key rendered-keymap (kbd "3")))
      (should (equal '(0 . 1) (get-text-property (point) 'mevedel-ask-entry)))))

  :doc "mouse activation selects the clicked row rather than the previous point"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
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
    (test-mevedel-tool-ask-ui--show
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
  :doc "an arriving form leaves point in the draft until the user visits it"
  (test-mevedel-tool-ask-ui--with-form
    (with-current-buffer view-buffer
      (mevedel-view-test--insert-composer-draft "> draft" 3))
    (mevedel-tool-ask-ui-show #'ignore [(:question "Use cache?" :options ["Yes" "No"])])
    (with-current-buffer view-buffer
      (should (= (point) (+ (mevedel-view--input-start) 3)))
      (mevedel-view-jump-to-interaction)
      (should (equal '(0 . 0) (get-text-property (point) 'mevedel-ask-entry)))))

  :doc "shows every question at once and submits the whole form"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
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
    (test-mevedel-tool-ask-ui--show
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
    (test-mevedel-tool-ask-ui--show
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
    (test-mevedel-tool-ask-ui--show
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

  :doc "object options show descriptions and samples inline and echo the chosen sample"
  (test-mevedel-tool-ask-ui--with-form
    (test-mevedel-tool-ask-ui--show
     (lambda (value) (setq result value))
     [(:question "What should I write?"
                 :options [(:label "Project AGENTS.md (Recommended)"
                            :description "Shared repo guidance"
                            :sample "# Repository Guidelines\n- Run tests")
                           (:label "Personal AGENTS.local.md"
                            :description "Private notes"
                            :sample "# Local notes")])])
    (should (string-match-p "Shared repo guidance" rendered-body))
    (should (equal (test-mevedel-tool-ask-ui--panel-rows rendered-body)
                   '("Sample · Project AGENTS.md (Recommended)"
                     "# Repository Guidelines" "- Run tests")))
    (call-interactively (lookup-key rendered-keymap (kbd "n")))
    (should (equal (cdr (test-mevedel-tool-ask-ui--panel-rows rendered-body))
                   '("# Local notes" "")))
    (call-interactively (lookup-key rendered-keymap (kbd "1")))
    (call-interactively (lookup-key rendered-keymap (kbd "RET")))
    (call-interactively (lookup-key rendered-keymap (kbd "C-c C-c")))
    (should (string-match-p "A1: Project AGENTS.md (Recommended)"
                            result))
    (should (string-match-p "Sample shown for A1:" result))
    (should (string-match-p "# Repository Guidelines" result)))

  :doc "cursor movement does not re-announce the form to remote guests"
  (test-mevedel-tool-ask-ui--with-form
    (let ((mevedel-interaction-prompt-created-hook nil)
          (announcements 0))
      (add-hook 'mevedel-interaction-prompt-created-hook
                (lambda (_overlay) (cl-incf announcements)))
      (with-current-buffer view-buffer
        (setq-local mevedel--prompt-overlays nil))
      (test-mevedel-tool-ask-ui--show
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
            (test-mevedel-tool-ask-ui--show
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
            (test-mevedel-tool-ask-ui--show
             (lambda (value) (setq result-a value))
             [(:question "A?" :options ["Yes" "No"])]))
          (with-current-buffer agent-buffer-b
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation inv-b)
            (setq-local mevedel--view-buffer view-buffer)
            (mevedel-request-begin session)
            (test-mevedel-tool-ask-ui--show
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
                (lambda (invocation status &optional _token)
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
          (test-mevedel-tool-ask-ui--show
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
          (should-error
           (funcall (plist-get (overlay-get overlay 'mevedel--remote) :answer)
                    '("only"))
           :type 'user-error)
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
            (test-mevedel-tool-ask-ui--show
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
            (test-mevedel-tool-ask-ui--show
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
            (test-mevedel-tool-ask-ui--show
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
