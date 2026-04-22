;;; test-mevedel-view.el -- Tests for mevedel-view -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)
(require 'mevedel-mentions)


;;
;;; Test helpers

(defmacro mevedel-view-test--with-buffers (&rest body)
  "Execute BODY with a data buffer and view buffer set up.
Binds `data-buf' and `view-buf' in scope.  Cleans up afterwards."
  (declare (indent 0) (debug t))
  `(let ((data-buf (generate-new-buffer " *test-data*"))
         (view-buf (generate-new-buffer " *test-view*")))
     (unwind-protect
         (progn
           ;; Set up data buffer as a minimal gptel-like buffer
           (with-current-buffer data-buf
             (org-mode)
             (setq-local gptel-response-separator "\n\n")
             (setq-local gptel-prompt-prefix-alist '((org-mode . "*** "))))
           ;; Wire up view buffer
           (mevedel-view--setup view-buf data-buf)
           ,@body)
       (when (buffer-live-p view-buf) (kill-buffer view-buf))
       (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(defun mevedel-view-test--insert-data (data-buf text props)
  "Insert TEXT into DATA-BUF with gptel text property PROPS.
PROPS is the value for the `gptel' property."
  (with-current-buffer data-buf
    (goto-char (point-max))
    (let ((start (point)))
      (insert text)
      (when props
        (put-text-property start (point) 'gptel props)))))


;;
;;; Segment extraction

(mevedel-deftest mevedel-view--extract-segments ()
  ,test
  (test)
  :doc "single user segment"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hello\n" nil)
    (with-current-buffer data-buf
      (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
        (should (= 1 (length segs)))
        (should (eq 'user (caar segs))))))

  :doc "user + response segments"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hello\n" nil)
    (mevedel-view-test--insert-data data-buf "Hi there\n" 'response)
    (with-current-buffer data-buf
      (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
        (should (= 2 (length segs)))
        (should (eq 'user (caar segs)))
        (should (eq 'response (caadr segs))))))

  :doc "response + tool + response segments"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Some response\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nresult\n"
     '(tool . "call_1"))
    (mevedel-view-test--insert-data data-buf "More response\n" 'response)
    (with-current-buffer data-buf
      (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
        (should (= 3 (length segs)))
        (should (eq 'response (caar segs)))
        (should (eq 'tool (caadr segs)))
        (should (eq 'response (car (caddr segs)))))))

  :doc "expands partial start/end to full gptel runs"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Some response\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     "\n(:name \"Grep\" :args (:pattern \"foo\"))\n\nmatch\n"
     '(tool . "call_1"))
    (mevedel-view-test--insert-data data-buf "More response\n" 'response)
    (with-current-buffer data-buf
      ;; Simulate incremental rerender entering in the middle of the tool run.
      (let* ((tool-start (next-single-property-change (point-min) 'gptel))
             (mid-start (+ tool-start 2))
             (mid-end (+ tool-start 10))
             (segs (mevedel-view--extract-segments mid-start mid-end)))
        (should (= 1 (length segs)))
        (pcase-let ((`(,kind ,seg-start ,_seg-end) (car segs)))
          (should (eq 'tool kind))
          (should (eq ?\n (char-after seg-start)))
          (should (string-prefix-p "\n(:name \"Grep\""
                                   (buffer-substring-no-properties seg-start (+ seg-start 20))))))))

  :doc "ignore segment"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "thinking...\n" 'ignore)
    (with-current-buffer data-buf
      (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
        (should (= 1 (length segs)))
        (should (eq 'ignore (caar segs)))))))


;;
;;; Turn grouping

(mevedel-deftest mevedel-view--group-into-turns ()
  ,test
  (test)
  :doc "single user turn"
  (let* ((segs '((user 1 10)))
         (turns (mevedel-view--group-into-turns segs)))
    (should (= 1 (length turns)))
    (should (eq 'user (plist-get (car turns) :role))))

  :doc "user then assistant turn"
  (let* ((segs '((user 1 10) (response 10 30) (tool 30 50)))
         (turns (mevedel-view--group-into-turns segs)))
    (should (= 2 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (= 2 (length (plist-get (cadr turns) :segments)))))

  :doc "multiple user-assistant pairs"
  (let* ((segs '((user 1 10) (response 10 20) (user 20 30) (response 30 40)))
         (turns (mevedel-view--group-into-turns segs)))
    (should (= 4 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (eq 'user (plist-get (caddr turns) :role)))
    (should (eq 'assistant (plist-get (cadddr turns) :role))))

  :doc "reasoning text (nil segments) inside assistant turn absorbed"
  (let* ((segs '((user 1 10) (ignore 10 20) (user 20 40) (tool 40 80)
                 (user 80 90) (ignore 90 100) (response 100 150)))
         (turns (mevedel-view--group-into-turns segs)))
    (should (= 2 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (= 6 (length (plist-get (cadr turns) :segments)))))

  :doc "mid-turn nil gap after response absorbed when next is ignore/tool"
  (let* ((segs '((user 1 10) (response 10 50) (user 50 60)
                 (ignore 60 80) (tool 80 120) (response 120 200)))
         (turns (mevedel-view--group-into-turns segs)))
    (should (= 2 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    ;; All 5 non-user segments belong to one assistant turn
    (should (= 5 (length (plist-get (cadr turns) :segments)))))

  :doc "nil gap after response starts new user turn when next is response"
  (let* ((segs '((user 1 10) (response 10 50) (user 50 60)
                 (response 60 100)))
         (turns (mevedel-view--group-into-turns segs)))
    ;; user | assistant(response) | user | assistant(response)
    (should (= 4 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (eq 'user (plist-get (caddr turns) :role)))
    (should (eq 'assistant (plist-get (cadddr turns) :role)))))


;;
;;; Tool one-liner generation

(mevedel-deftest mevedel-view--tool-one-liner ()
  ,test
  (test)
  :doc "Read tool summary"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/home/user/src/main.el\"))\n\nline 1\nline 2\nline 3\n"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((summary (mevedel-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (string-match-p "Read" summary))
        (should (string-match-p "main\\.el" summary))
        (should (string-match-p "3 lines" summary)))))

  :doc "Bash tool summary"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Bash\" :args (:command \"ls -la\"))\n\noutput\n"
     '(tool . "call_2"))
    (with-current-buffer data-buf
      (let ((summary (mevedel-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (string-match-p "Bash" summary))
        (should (string-match-p "ls -la" summary)))))

  :doc "fallback on unparseable content"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "not a valid sexp" '(tool . "call_3"))
    (with-current-buffer data-buf
      (let ((summary (mevedel-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (stringp summary))
        (should (> (length summary) 0))))))


;;
;;; Read-like tool detection

(mevedel-deftest mevedel-view--read-like-tool-p ()
  ,test
  (test)
  :doc "Read tool is read-like"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\ncontent\n" '(tool . "c1"))
    (with-current-buffer data-buf
      (should (mevedel-view--read-like-tool-p data-buf (point-min) (point-max)))))

  :doc "Bash tool is not read-like"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf "(:name \"Bash\" :args (:command \"ls\"))\n\noutput\n" '(tool . "c2"))
    (with-current-buffer data-buf
      (should-not (mevedel-view--read-like-tool-p data-buf (point-min) (point-max))))))


;;
;;; Rendering

(mevedel-deftest mevedel-view--render-response ()
  ,test
  (test)
  :doc "renders user + assistant turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hello world\n" nil)
    (mevedel-view-test--insert-data data-buf "Hi! How can I help?\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "You" text))
        (should (string-match-p "Assistant" text))
        (should (string-match-p "Hello world" text))
        (should (string-match-p "How can I help" text)))))

  :doc "renders tool calls as one-liners"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nfile content\n"
     '(tool . "call_1"))
    (mevedel-view-test--insert-data data-buf "Here is the file.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Read.*test\\.el" text))
        (should-not (string-match-p "file content" text))
        (should (string-match-p "Here is the file" text)))))

  :doc "does not render spurious user turn for gptel tool scaffolding"
  ;; gptel inserts `#+begin_tool ... ' and `#+end_tool' around the
  ;; propertised tool content with no `gptel' property, so the
  ;; separator text between a user prompt and the tool content shows
  ;; up as a `user' segment.  Must not render as a second "You" turn.
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "\n\n#+begin_tool (Read :file_path \"/tmp/test.el\")\n"
     nil)
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nfile content\n"
     '(tool . "call_1"))
    (mevedel-view-test--insert-data data-buf "\n#+end_tool\n" nil)
    (mevedel-view-test--insert-data data-buf "Here is the file.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let* ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker))
             (you-count (cl-count-if (lambda (line) (string= line "You"))
                                     (split-string text "\n"))))
        (should (= 0 you-count))
        (should-not (string-match-p "#\\+begin_tool" text))
        (should (string-match-p "Read.*test\\.el" text))
        (should (string-match-p "Here is the file" text)))))

  :doc "renders thinking blocks as summaries"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "line 1\nline 2\nline 3\n" 'ignore)
    (mevedel-view-test--insert-data data-buf "The answer is 42.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Thinking" text))
        (should-not (string-match-p "line 1" text))
        (should (string-match-p "42" text))))))


;;
;;; Spinner

(mevedel-deftest mevedel-view--start-spinner ()
  ,test
  (test)
  :doc "creates and removes spinner overlay"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Working...")
      (should mevedel-view--spinner-overlay)
      (should (overlay-buffer mevedel-view--spinner-overlay))
      (let ((text (buffer-substring-no-properties
                   (overlay-start mevedel-view--spinner-overlay)
                   (overlay-end mevedel-view--spinner-overlay))))
        (should (string-match-p "Working" text)))
      (mevedel-view--stop-spinner)
      (should-not mevedel-view--spinner-overlay)))

  :doc "update replaces spinner text"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (mevedel-view--update-spinner "Calling Read...")
      (let ((text (buffer-substring-no-properties
                   (overlay-start mevedel-view--spinner-overlay)
                   (overlay-end mevedel-view--spinner-overlay))))
        (should (string-match-p "Calling Read" text))
        (should-not (string-match-p "Thinking" text)))
      (mevedel-view--stop-spinner))))


;;
;;; View buffer setup

(mevedel-deftest mevedel-view--setup ()
  ,test
  (test)
  :doc "wires buffers together correctly"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (should (eq mevedel--data-buffer data-buf))
      (should mevedel-view--input-marker)
      (should (derived-mode-p 'mevedel-view-mode))
      (should-not buffer-read-only))
    (with-current-buffer data-buf
      (should (eq mevedel--view-buffer view-buf)))))


;;
;;; Input forwarding

(mevedel-deftest mevedel-view--input-text ()
  ,test
  (test)
  :doc "extracts text from input region"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (should (string-empty-p (mevedel-view--input-text)))
      (goto-char (mevedel-view--input-start))
      (insert "hello world")
      (should (equal "hello world" (mevedel-view--input-text)))))

  :doc "clear empties input region"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "hello world")
      (mevedel-view--clear-input)
      (should (string-empty-p (mevedel-view--input-text))))))


;;
;;; Full re-render

(mevedel-deftest mevedel-view--full-rerender ()
  ,test
  (test)
  :doc "rebuilds view from data buffer"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** What is 2+2?\n" nil)
    (mevedel-view-test--insert-data data-buf "The answer is 4.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text1 (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "What is 2\\+2" text1))
        (mevedel-view--full-rerender)
        (let ((text2 (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
          (should (string-match-p "What is 2\\+2" text2))
          (should (string-match-p "answer is 4" text2)))))))


;;
;;; Expand/collapse

(mevedel-deftest mevedel-view-toggle-section ()
  ,test
  (test)
  :doc "expands collapsed tool one-liners"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/f.el\"))\n\nfull content here\nline 2\n"
     '(tool . "call_1"))
    (mevedel-view-test--insert-data data-buf "Done.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      ;; Find the collapsed tool line
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found) (< (point) mevedel-view--input-marker))
          (if (eq (get-text-property (point) 'mevedel-view-collapsed) t)
              (setq found t)
            (goto-char (or (next-single-property-change
                            (point) 'mevedel-view-collapsed)
                           mevedel-view--input-marker))))
        (should found)
        ;; Content should be collapsed
        (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
          (should-not (string-match-p "full content here" text)))
        ;; Expand
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
          (should (string-match-p "full content here" text)))))))

(mevedel-deftest mevedel-view--section-bounds ()
  ,test
  (test)
  :doc "distinguishes equal-but-distinct source conses (regression)
Thinking-cons and turn-fallback-cons can have equal values but be
separate cons objects.  `section-bounds' must compare by `eq', not
`equal', or it will treat them as one run and expand/collapse over
the preceding header."
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "deep thoughts here\n"))
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            ;; Equal values, distinct objects — matches the real render
            ;; path where the thinking summary's source cons and the
            ;; turn-level fallback cons may print identically.
            (thinking-src (cons 1 20))
            (turn-src (cons 1 20)))
        (should (equal thinking-src turn-src))
        (should-not (eq thinking-src turn-src))
        (save-excursion
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((insert-start (point)))
                (insert (propertize "Assistant\n"
                                    'font-lock-face 'mevedel-view-assistant-header))
                (insert (propertize "Thinking... (1 lines)\n"
                                    'font-lock-face 'mevedel-view-thinking-summary
                                    'mevedel-view-type 'thinking-summary
                                    'mevedel-view-collapsed t
                                    'mevedel-view-source thinking-src))
                (insert (propertize "\n" 'font-lock-face 'mevedel-view-separator))
                (add-text-properties insert-start (point) '(read-only t))
                ;; Fill the header/separator gap with the turn-level
                ;; fallback cons, as `mevedel-view--render-turn' does.
                (let ((pos insert-start))
                  (while (< pos (point))
                    (if (get-text-property pos 'mevedel-view-source)
                        (setq pos (or (next-single-property-change
                                       pos 'mevedel-view-source nil (point))
                                      (point)))
                      (let ((next (or (next-single-property-change
                                       pos 'mevedel-view-source nil (point))
                                      (point))))
                        (put-text-property pos next
                                           'mevedel-view-source turn-src)
                        (setq pos next))))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        ;; Point at the exact start of the thinking run — the boundary
        ;; case where `previous-single-property-change' lands in the
        ;; preceding "Assistant\n" run.
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (let ((bounds (mevedel-view--section-bounds)))
          (should bounds)
          (should (eq (get-text-property (car bounds)
                                         'mevedel-view-source)
                      thinking-src))
          (should (eq (get-text-property (point) 'mevedel-view-source)
                      thinking-src))
          ;; The bounds must not reach into the Assistant header.
          (let ((header-text (buffer-substring-no-properties
                              (car bounds) (cdr bounds))))
            (should-not (string-match-p "Assistant" header-text))))))))

(mevedel-deftest mevedel-view-toggle-section/thinking-preserves-headers ()
  ,test
  (test)
  :doc "TAB expand then collapse on thinking keeps surrounding
headers intact (regression for the \"You/Assistant disappear\" bug
when thinking-cons and turn-cons had equal-but-distinct values)."
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "deep thoughts here\n"))
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (thinking-src (cons 1 20))
            (turn-src (cons 1 20)))
        ;; User section (no source, mirrors `--insert-user-message').
        (save-excursion
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((start (point)))
                (insert (propertize "You\n"
                                    'font-lock-face 'mevedel-view-user-header))
                (insert "Think about it.\n")
                (insert (propertize "\n" 'font-lock-face 'mevedel-view-separator))
                (add-text-properties start (point)
                                     '(read-only t mevedel-view-type user)))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        ;; Assistant turn: header + thinking summary + separator, with
        ;; the turn-level fallback source equal-but-not-eq to thinking.
        (save-excursion
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((insert-start (point)))
                (insert (propertize "Assistant\n"
                                    'font-lock-face 'mevedel-view-assistant-header))
                (insert (propertize "Thinking... (1 lines)\n"
                                    'font-lock-face 'mevedel-view-thinking-summary
                                    'mevedel-view-type 'thinking-summary
                                    'mevedel-view-collapsed t
                                    'mevedel-view-source thinking-src))
                (insert (propertize "\n" 'font-lock-face 'mevedel-view-separator))
                (add-text-properties insert-start (point) '(read-only t))
                (let ((pos insert-start))
                  (while (< pos (point))
                    (if (get-text-property pos 'mevedel-view-source)
                        (setq pos (or (next-single-property-change
                                       pos 'mevedel-view-source nil (point))
                                      (point)))
                      (let ((next (or (next-single-property-change
                                       pos 'mevedel-view-source nil (point))
                                      (point))))
                        (put-text-property pos next
                                           'mevedel-view-source turn-src)
                        (setq pos next))))))
            (set-marker-insertion-type mevedel-view--input-marker nil))))
      ;; Point at the exact start of the thinking line.
      (goto-char (point-min))
      (search-forward "Thinking...")
      (goto-char (match-beginning 0))
      ;; Expand.
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "^You$" text))
        (should (string-match-p "^Assistant$" text))
        (should (string-match-p "deep thoughts here" text))
        (should-not (string-match-p "^Thinking\\.\\.\\." text)))
      ;; Collapse back — the thinking summary must return and headers
      ;; must still be intact.
      (goto-char (point-min))
      (search-forward "deep thoughts here")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "^You$" text))
        (should (string-match-p "^Assistant$" text))
        (should (string-match-p "Thinking\\.\\.\\. (1 lines)" text))
        (should-not (string-match-p "deep thoughts here" text))))))

(mevedel-deftest mevedel-view-toggle-section/response ()
  ,test
  (test)
  :doc "response text is a collapsible section (regression for
earlier removal of the catch-all collapse branch that dropped
response folding along with a dangerous best-guess preview path)."
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "First line of reply.\nSecond line.\nThird line.\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "First line of reply")
      (goto-char (match-beginning 0))
      (should (eq (get-text-property (point) 'mevedel-view-type) 'response))
      (should (eq (get-text-property (point) 'mevedel-view-collapsed) nil))
      ;; Collapse.
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "First line of reply" text))
        (should-not (string-match-p "Second line" text))
        (should (string-match-p "(3 lines)" text)))
      ;; Expand back.
      (goto-char (point-min))
      (search-forward "First line of reply")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Second line" text))
        (should (string-match-p "Third line" text))))))

(mevedel-deftest mevedel-view-toggle-section/assistant-turn ()
  ,test
  (test)
  :doc "TAB on Assistant header folds the whole turn into a single
summary line; TAB again restores it exactly."
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hi\n" nil)
    (mevedel-view-test--insert-data
     data-buf
     "Here is the first line.\nSecond line.\nThird line.\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "Assistant")
      (goto-char (match-beginning 0))
      (should (eq (get-text-property (point) 'mevedel-view-type) 'turn-header))
      ;; Fold the turn.
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Assistant — Here is the first line" text))
        (should-not (string-match-p "^Second line" text))
        (should-not (string-match-p "^Third line" text))
        ;; The user turn is untouched.
        (should (string-match-p "^You$" text))
        (should (string-match-p "^Hi$" text)))
      ;; Expand back.
      (goto-char (point-min))
      (search-forward "Assistant — ")
      (goto-char (match-beginning 0))
      (should (eq (get-text-property (point) 'mevedel-view-type) 'turn-summary))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "^Assistant$" text))
        (should (string-match-p "Here is the first line" text))
        (should (string-match-p "Second line" text))
        (should (string-match-p "Third line" text))))))

(mevedel-deftest mevedel-view-toggle-section/user-turn ()
  ,test
  (test)
  :doc "multi-line user turn folds to first-line summary
Single-line user turns refuse to fold since they are already
compact."
  (mevedel-view-test--with-buffers
    ;; Multi-line user turn.
    (mevedel-view-test--insert-data
     data-buf
     "*** First prompt line.\nSecond prompt line.\nThird prompt line.\n"
     nil)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "You")
      (goto-char (match-beginning 0))
      (should (eq (get-text-property (point) 'mevedel-view-type) 'turn-header))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "First prompt line" text))
        (should-not (string-match-p "^Second prompt line" text))
        (should (string-match-p "(3 lines)" text)))
      ;; Expand.
      (goto-char (point-min))
      (search-forward "First prompt line")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "^You$" text))
        (should (string-match-p "Second prompt line" text))
        (should (string-match-p "Third prompt line" text)))))
  :doc "single-line user turn refuses to fold"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** One line only.\n" nil)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "You")
      (goto-char (match-beginning 0))
      (should-error (mevedel-view-toggle-section)
                    :type 'user-error))))

(mevedel-deftest mevedel-view-toggle-section/turn-preserves-inner-state ()
  ,test
  (test)
  :doc "folding and unfolding a turn preserves the expanded/collapsed
state of its inner sections"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert "deep thoughts live here\n")
        (put-text-property start (point) 'gptel 'ignore))
      (let ((start (point)))
        (insert "Visible response text.\n")
        (put-text-property start (point) 'gptel 'response))
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      ;; Expand the thinking section first.
      (goto-char (point-min))
      (search-forward "Thinking...")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "deep thoughts live here" text))
        (should-not (string-match-p "Thinking\\.\\.\\." text)))
      ;; Fold the whole turn.
      (goto-char (point-min))
      (search-forward "Assistant")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Assistant — " text))
        (should-not (string-match-p "deep thoughts live here" text)))
      ;; Unfold — the thinking section must still be EXPANDED.
      (goto-char (point-min))
      (search-forward "Assistant — ")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "^Assistant$" text))
        (should (string-match-p "deep thoughts live here" text))
        (should-not (string-match-p "Thinking\\.\\.\\." text))
        (should (string-match-p "Visible response text" text))))))


;;
;;; Rendering plist validation

(mevedel-deftest mevedel-view--rendering-plist-p ()
  ,test
  (test)
  :doc "accepts minimal plist with just :header"
  (should (mevedel-view--rendering-plist-p '(:header "h")))
  :doc "accepts full plist with string body and symbol mode"
  (should (mevedel-view--rendering-plist-p
           '(:header "h" :body "b" :body-mode diff-mode
                     :initially-collapsed-p t)))
  :doc "rejects missing :header"
  (should-not (mevedel-view--rendering-plist-p '(:body "b")))
  :doc "rejects non-string :header"
  (should-not (mevedel-view--rendering-plist-p '(:header 42)))
  :doc "rejects non-string :body"
  (should-not (mevedel-view--rendering-plist-p '(:header "h" :body 42)))
  :doc "rejects non-symbol :body-mode"
  (should-not (mevedel-view--rendering-plist-p
               '(:header "h" :body-mode "not-a-symbol"))))


;;
;;; Renderer invocation

(mevedel-deftest mevedel-view--invoke-renderer ()
  ,test
  (test)
  :doc "returns the renderer's plist on success"
  (let* ((tool (mevedel-tool--create
                :name "R1"
                :renderer (lambda (_name _args _result _data)
                            (list :header "ok"
                                  :body "b"
                                  :body-mode 'diff-mode)))))
    (should (equal '(:header "ok" :body "b" :body-mode diff-mode)
                   (mevedel-view--invoke-renderer
                    tool '(:kind diff) nil "result"))))
  :doc "invokes the renderer even when render-data is nil (output-driven renderers)"
  (let ((tool (mevedel-tool--create
               :name "R2"
               :renderer (lambda (_name _args _result _data)
                           (list :header "x")))))
    (should (equal '(:header "x")
                   (mevedel-view--invoke-renderer tool nil nil "ok"))))

  :doc "data-driven renderers can opt out by returning nil when render-data is absent"
  (let ((tool (mevedel-tool--create
               :name "R2-data"
               :renderer (lambda (_name _args _result data)
                           (and data (list :header "only with data"))))))
    (should (null (mevedel-view--invoke-renderer tool nil nil "ok"))))
  :doc "returns nil when tool has no renderer"
  (let ((tool (mevedel-tool--create :name "NoRend" :renderer nil)))
    (should (null (mevedel-view--invoke-renderer
                   tool '(:kind diff) nil "ok"))))
  :doc "renderer returning nil yields nil (silent fallback)"
  (let ((tool (mevedel-tool--create
               :name "Declines"
               :renderer (lambda (_name _args _result _data) nil))))
    (should (null (mevedel-view--invoke-renderer
                   tool '(:kind diff) nil "ok"))))
  :doc "renderer returning malformed plist yields nil and emits a warning"
  (let* ((tool (mevedel-tool--create
                :name "Bad"
                :renderer (lambda (_name _args _result _data)
                            '(:body "no header"))))
         (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (should (null (mevedel-view--invoke-renderer
                     tool '(:kind diff) nil "ok")))
      (should warnings)
      (should (eq 'mevedel (caar warnings)))
      (should (string-match-p "malformed" (cadar warnings)))))
  :doc "renderer signalling an error yields nil and emits a warning"
  (let* ((tool (mevedel-tool--create
                :name "Boom"
                :renderer (lambda (_name _args _result _data)
                            (error "oops"))))
         (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (should (null (mevedel-view--invoke-renderer
                     tool '(:kind diff) nil "ok")))
      (should warnings)
      (should (eq 'mevedel (caar warnings)))
      (should (string-match-p "failed" (cadar warnings))))))


;;
;;; Tool-call parsing with render-data

(mevedel-deftest mevedel-view--tool-call-parse ()
  ,test
  (test)
  :doc "extracts name, args, and result from a tool segment"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nplain result\n"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (mevedel-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "Read" (plist-get call :name)))
        (should (equal '(:file_path "/tmp/f") (plist-get call :args)))
        (should (string-match-p "plain result" (plist-get call :result)))
        (should (null (plist-get call :render-data))))))
  :doc "renderer path survives incremental rerender starting inside tool run"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "prefix\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     "\n(:name \"Grep\" :args (:pattern \"task\"))\n\nNo matches found\n"
     '(tool . "call_1"))
    (mevedel-view-test--insert-data data-buf "suffix\n" 'response)
    (with-current-buffer data-buf
      (let* ((tool-start (next-single-property-change (point-min) 'gptel))
             (mid-start (+ tool-start 2))
             (mid-end (+ tool-start 12))
             (segs (mevedel-view--extract-segments mid-start mid-end))
             (tool-seg (car segs))
             (call (mevedel-view--tool-call-parse
                    data-buf (cadr tool-seg) (caddr tool-seg))))
        (should (= 1 (length segs)))
        (should (equal "Grep" (plist-get call :name)))
        (should (equal '(:pattern "task") (plist-get call :args)))
        (should (string-match-p "No matches found" (plist-get call :result))))))
  :doc "decodes embedded render-data and strips it from :result"
  (mevedel-view-test--with-buffers
    (let* ((render-data '(:kind diff :patch "--- a\n+++ b\n+hi\n"
                          :path "/tmp/f" :rel-path "f"))
           (body (concat "visible body"
                         (mevedel-pipeline--format-render-data-block
                          render-data))))
      (mevedel-view-test--insert-data
       data-buf
       (concat "(:name \"Edit\" :args (:file_path \"/tmp/f\"))\n\n"
               body "\n")
       '(tool . "call_1"))
      (with-current-buffer data-buf
        (let ((call (mevedel-view--tool-call-parse
                     data-buf (point-min) (point-max))))
          (should (equal "Edit" (plist-get call :name)))
          (should (equal "visible body" (plist-get call :result)))
          (should (equal render-data (plist-get call :render-data)))))))
  :doc "returns nil on unreadable segments"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf "(:unclosed\n" '(tool . "call_1"))
    (with-current-buffer data-buf
      (should (null (mevedel-view--tool-call-parse
                     data-buf (point-min) (point-max)))))))


;;
;;; Re-render idempotence with renderer

(mevedel-deftest mevedel-view--renderer-idempotent ()
  ,test
  (test)
  :doc "invoking the renderer twice with identical inputs yields equal plists"
  (let* ((calls 0)
         (tool (mevedel-tool--create
                :name "Idem"
                :renderer (lambda (_name _args _result data)
                            (cl-incf calls)
                            (list :header (format "I:%s" (plist-get data :n))
                                  :body "b"
                                  :body-mode 'diff-mode))))
         (data '(:n 7))
         (args '(:x 1))
         (result "done"))
    (let ((first (mevedel-view--invoke-renderer tool data args result))
          (second (mevedel-view--invoke-renderer tool data args result)))
      (should (equal first second))
      (should (= 2 calls))))
  :doc "round-trip through serialization preserves render-data"
  (let* ((data '(:kind diff :patch "@@ @@\n+a\n" :path "/tmp/x"))
         (serialized (mevedel-pipeline--format-render-data-block data))
         (extract (mevedel-pipeline-extract-render-data
                   (concat "result" serialized))))
    (should (equal data (cdr extract)))))

(provide 'test-mevedel-view)

;;; test-mevedel-view.el ends here
