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
(require 'mevedel-skills)
(require 'mevedel-workspace)
(require 'mevedel-file-state)
(require 'mevedel-session-persistence)
(require 'mevedel-tool-ui)


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
        (should (string-match-p "42" text)))))

  :doc "tolerates detached status-marker without crashing"
  ;; A detached marker passes `markerp' but `marker-position' returns
  ;; nil; downstream uses (`<=', `delete-region', `apply-collapse-states')
  ;; would otherwise signal `wrong-type-argument: integer-or-marker-p, nil'.
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hello\n" nil)
    (mevedel-view-test--insert-data data-buf "Hi.\n" 'response)
    (with-current-buffer view-buf
      (set-marker mevedel-view--status-marker nil))
    (with-current-buffer data-buf
      (should
       (progn (mevedel-view--render-response (point-min) (point-max)) t))))

  :doc "tolerates nil status-marker without crashing"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hello\n" nil)
    (mevedel-view-test--insert-data data-buf "Hi.\n" 'response)
    (with-current-buffer view-buf
      (setq mevedel-view--status-marker nil))
    (with-current-buffer data-buf
      (should
       (progn (mevedel-view--render-response (point-min) (point-max)) t)))))


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
      (mevedel-view--stop-spinner)))

  :doc "stop tolerates a detached overlay without crashing"
  ;; A rerender that wipes the spinner's anchor region leaves the
  ;; overlay's `overlay-start' / `overlay-end' returning nil; without
  ;; the guard, `delete-region' would signal
  ;; `wrong-type-argument: integer-or-marker-p, nil'.
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (let ((ov mevedel-view--spinner-overlay))
        ;; Detach the overlay manually -- simulates a rerender that
        ;; wiped the anchor region.
        (delete-overlay ov)
        ;; Re-install on the variable so stop-spinner sees a detached
        ;; overlay (one whose `overlay-start' returns nil).
        (setq mevedel-view--spinner-overlay ov))
      (should (progn (mevedel-view--stop-spinner) t))
      (should-not mevedel-view--spinner-overlay))))


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
          (should (string-match-p "answer is 4" text2))))))
  :doc "header stays at top when rerendering (input-marker advances past it)"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Greetings\n" nil)
    (mevedel-view-test--insert-data data-buf "Hello back\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let* ((text (buffer-substring-no-properties
                    (point-min) mevedel-view--input-marker))
             (header (mevedel-view--header-string data-buf))
             (header-trim (string-trim-right header))
             (header-pos (string-search header-trim text))
             (greet-pos (string-search "Greetings" text)))
        (should header-pos)
        (should greet-pos)
        (should (< header-pos greet-pos)))))
  :doc "skips leading :PROPERTIES: drawer on data buffer"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      ;; Simulate gptel-org's state drawer at buffer start.
      (insert ":PROPERTIES:\n"
              ":GPTEL_MODEL: test\n"
              ":GPTEL_BOUNDS: ((response (100 200)))\n"
              ":END:\n\n"))
    (mevedel-view-test--insert-data data-buf "*** Actual prompt\n" nil)
    (mevedel-view-test--insert-data data-buf "Actual reply\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should-not (string-match-p ":GPTEL_MODEL:" text))
        (should-not (string-match-p ":PROPERTIES:" text))
        (should     (string-match-p "Actual prompt" text))
        (should     (string-match-p "Actual reply" text))))))

(mevedel-deftest mevedel-view--skip-leading-properties-drawer ()
  ,test
  (test)
  :doc "advances past a well-formed :PROPERTIES: drawer at POS"
  (with-temp-buffer
    (insert ":PROPERTIES:\n:GPTEL_MODEL: x\n:END:\nhello\n")
    (let ((after (mevedel-view--skip-leading-properties-drawer (point-min))))
      (should (> after (point-min)))
      (should (string= "hello\n" (buffer-substring-no-properties
                                  after (point-max))))))
  :doc "returns POS unchanged when no drawer is present"
  (with-temp-buffer
    (insert "no drawer here\n")
    (should (= (point-min)
               (mevedel-view--skip-leading-properties-drawer (point-min)))))
  :doc "returns POS unchanged when drawer is malformed (no :END:)"
  (with-temp-buffer
    (insert ":PROPERTIES:\n:GPTEL_MODEL: x\nstuff\n")
    (should (= (point-min)
               (mevedel-view--skip-leading-properties-drawer (point-min))))))


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

(mevedel-deftest mevedel-view-toggle-section/renderer-vtype ()
  ,test
  (test)
  :doc "agent handles expand and collapse through their renderer"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "(:name \"Agent\" :args (:subagent_type \"explore\"))\n\nraw launch payload\n"))
    (with-current-buffer view-buf
      (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
             (rendering '(:header "Agent: explore -- Find calls"
                          :body "rendered agent body\n"
                          :body-mode text-mode
                          :vtype agent-handle)))
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((start (point)))
                (insert "› Agent: explore -- Find calls\nrendered agent body\n")
                (add-text-properties
                 start (point)
                 `(font-lock-face mevedel-view-tool-summary
                   mevedel-view-type agent-handle
                   mevedel-view-collapsed nil
                   mevedel-view-source ,source
                   read-only t
                   keymap ,mevedel-view--display-map
                   front-sticky (read-only keymap)
                   rear-nonsticky (read-only keymap))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                   (lambda (buf start end)
                     (should (eq buf data-buf))
                     (should (= start (car source)))
                     (should (= end (cdr source)))
                     rendering)))
          (goto-char (point-min))
          (search-forward "rendered agent body")
          (mevedel-view-toggle-section)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "Agent: explore -- Find calls" text))
            (should-not (string-match-p "rendered agent body" text))
            (should-not (string-match-p "raw launch payload" text))
            (goto-char (point-min))
            (search-forward "Agent: explore")
            (goto-char (match-beginning 0))
            (should (eq (get-text-property (point) 'mevedel-view-type)
                        'agent-handle))
            (should (eq (get-text-property (point)
                                           'mevedel-view-collapsed)
                        t)))
          (goto-char (point-min))
          (search-forward "Agent: explore")
          (mevedel-view-toggle-section)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "rendered agent body" text))
            (should-not (string-match-p "raw launch payload" text))
            (goto-char (point-min))
            (search-forward "Agent: explore")
            (goto-char (match-beginning 0))
            (should (eq (get-text-property (point)
                                           'mevedel-view-collapsed)
                        nil))))))))

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
  :doc "parser skips leading org drawer and block scaffolding"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat ":PROPERTIES:\n:GPTEL_MODEL: x\n:END:\n"
             "#+begin_tool (Read :file_path \"/tmp/f\")\n"
             "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\n"
             "file body\n"
             "#+end_tool\n")
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (mevedel-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "Read" (plist-get call :name)))
        (should (string-match-p "file body" (plist-get call :result)))
        (should-not (string-match-p "GPTEL_MODEL"
                                    (plist-get call :result))))))
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


(mevedel-deftest mevedel-view--pending-tool-key
  (:doc "keys pending tool calls by call id when available")
  ,test
  (test)
  :doc "uses backend call id before name/args fingerprint"
  (should (equal "call-1"
                 (mevedel-view--pending-tool-key
                  '(:id "call-1" :name "Read" :args (:file_path "a")))))
  :doc "identical calls with distinct ids stay distinct"
  (should-not (equal
               (mevedel-view--pending-tool-key
                '(:id "call-1" :name "Read" :args (:file_path "a")))
               (mevedel-view--pending-tool-key
                '(:id "call-2" :name "Read" :args (:file_path "a"))))))


(mevedel-deftest mevedel-view--pending-tool-calls
  (:doc "tracks and renders the pending-tool live tail")
  ,test
  (test)

  :doc "pre/post hooks add and remove entries by call id"
  (mevedel-view-test--with-buffers
    (let ((render-count 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                 (lambda (&rest _) (cl-incf render-count))))
        (with-current-buffer data-buf
          (should-not
           (mevedel-view--pre-tool-hook
            '(:id "call-1" :name "Read" :args (:file_path "a"))))
          (should-not
           (mevedel-view--pre-tool-hook
            '(:id "call-2" :name "Grep" :args (:pattern "x"))))
          (with-current-buffer view-buf
            (should (equal '(("call-1" . "Read") ("call-2" . "Grep"))
                           mevedel-view--pending-tool-calls)))
          (should-not
           (mevedel-view--post-tool-hook
            '(:id "call-1" :name "Read" :args (:file_path "a"))))))
      (with-current-buffer view-buf
        (should (equal '(("call-2" . "Grep"))
                       mevedel-view--pending-tool-calls)))
      (should (= 3 render-count))))

  :doc "tool hooks do not return rendered agent-status strings to gptel"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min)))))
    (cl-letf (((symbol-function 'mevedel-view--render-incremental)
               (lambda (&rest _)
                 #(" ─── agents: 1 running [+] ─────────────────────────────────\n"
                   0 61 (font-lock-face mevedel-view-zone-separator)))))
      (with-current-buffer data-buf
        (should-not
         (mevedel-view--pre-tool-hook
          '(:id "call-1" :name "Read" :args (:file_path "a"))))
        (should-not
         (mevedel-view--post-tool-hook
          '(:id "call-1" :name "Read" :args (:file_path "a")))))))

  :doc "rendering caps visible calls and adds a truncation tail"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-pending-tools-visible-max 2))
        (setq mevedel-view--pending-tool-calls
              '(("1" . "Read") ("2" . "Grep") ("3" . "Bash")))
        (mevedel-view--insert-pending-tool-lines
         (cl-subseq mevedel-view--pending-tool-calls 0 2))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Calling Read" text))
          (should (string-match-p "Calling Grep" text))
          (should-not (string-match-p "Calling Bash" text))
          (should (string-match-p "1 more tools running" text)))))))


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


;;
;;; mevedel-view-send slash-fork integration

(defmacro mevedel-view-test--with-fork-skill (skill-form &rest body)
  "Wire data-buf with a session containing SKILL-FORM, then run BODY.
Binds `data-buf', `view-buf', and `session' in scope.  The skill is
attached via `mevedel-session-skills' so `mevedel-session-get-skill'
finds it during slash dispatch."
  (declare (indent 1) (debug t))
  `(mevedel-view-test--with-buffers
     (let* ((ws (mevedel-workspace--create
                 :type 'test :id "vf" :root "/tmp/vf" :name "vf"
                 :file-cache (mevedel-file-cache--create
                              :table (make-hash-table :test #'equal)
                              :order nil :total-bytes 0)))
            (session (mevedel-session-create "main" ws))
            (skill ,skill-form))
       (setf (mevedel-session-skills session) (list skill))
       (with-current-buffer data-buf
         (setq-local mevedel--session session))
       ,@body)))

(mevedel-deftest mevedel-view-send/skill-fork ()
  ,test
  (test)
  :doc "fork slash blocks input, captures the callback, and inserts the result"
  ;; Drive `mevedel-view-send' for a /myfork dispatch with a fork
  ;; skill installed on the session.  We mock `mevedel-skills-invoke'
  ;; to capture the callback so we can verify (a) the view-side spinner
  ;; and turn marker get armed before invocation returns, (b) the input
  ;; is cleared, (c) the eventual fork result is rendered into the data
  ;; buffer with `gptel response' text properties (the contract that
  ;; `--insert-fork-result' relies on for downstream rendering).
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myfork"
       :body "ignored"
       :context 'fork
       :agent "general-purpose"
       :user-invocable-p t)
    (let (captured-args save-called status-called)
      (cl-letf (((symbol-function 'mevedel-skills-invoke)
                 (lambda (skill args callback &rest kwargs)
                   (setq captured-args
                         (list :skill skill :args args
                               :callback callback :kwargs kwargs))))
                ((symbol-function 'mevedel-session-persistence-save)
                 (lambda (s b)
                   (setq save-called (list s b))
                   "saved"))
                ((symbol-function 'gptel--update-status)
                 (lambda (&rest args) (setq status-called args))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "/myfork run a thing")
          (mevedel-view-send)

          ;; The view armed the in-flight turn marker and spinner.
          (should (markerp mevedel-view--in-flight-turn-start))
          (should (marker-position mevedel-view--in-flight-turn-start))
          (should mevedel-view--spinner-overlay)

          ;; The user-message display text appeared in the view above
          ;; the input region.
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "/myfork run a thing" text))))
        (with-current-buffer data-buf
          (should mevedel--current-request))

        ;; mevedel-skills-invoke was called with the right shape.
        (should captured-args)
        (should (equal "myfork"
                       (mevedel-skill-name (plist-get captured-args :skill))))
        (should (equal "run a thing" (plist-get captured-args :args)))
        (should (eq 'user-slash
                    (plist-get (plist-get captured-args :kwargs) :trigger)))

        ;; Fire the callback with a fork outcome; expect the data buffer
        ;; to grow an assistant response carrying `gptel response'.
        (with-current-buffer data-buf
          (let ((before (buffer-size)))
            (funcall (plist-get captured-args :callback)
                     '(:status ok :kind fork
                               :result "FORK-RESULT-BODY"
                               :agent-id "myfork--1"))
            (should (> (buffer-size) before))
            (let ((text (buffer-string)))
              (should (string-match-p "FORK-RESULT-BODY" text)))
            ;; The inserted region carries `gptel response' so the view
            ;; renderer treats it as an assistant turn.
            (goto-char (point-max))
            (let ((response-pos
                   (text-property-any (point-min) (point-max)
                                      'gptel 'response)))
              (should response-pos)
              (should (string-match-p
                       "FORK-RESULT-BODY"
                       (buffer-substring-no-properties
                        response-pos (point-max)))))))
        (with-current-buffer data-buf
          (should-not mevedel--current-request)
          (should (= 1 (mevedel-session-turn-count session)))
          (should (equal (list session data-buf) save-called))
          (should (equal '(" Ready" success) status-called)))))

  :doc "fork error stops the spinner and does not insert a response"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myfork"
       :body "ignored"
       :context 'fork
       :agent "general-purpose"
       :user-invocable-p t)
    (let (captured-args)
      (cl-letf (((symbol-function 'mevedel-skills-invoke)
                 (lambda (_skill _args callback &rest _)
                   (setq captured-args (list :callback callback))))
                ((symbol-function 'message)
                 (lambda (&rest _))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "/myfork run")
          (mevedel-view-send))
        (with-current-buffer data-buf
          (should mevedel--current-request))
        ;; Drive the error branch.
        (with-current-buffer data-buf
          (let ((before (buffer-string)))
            (funcall (plist-get captured-args :callback)
                     '(:status error :reason boom :message "boom"))
            ;; No response inserted.
            (should (equal before (buffer-string)))
            (should-not mevedel--current-request)))
        ;; Spinner overlay was removed by `--stop-spinner'.
        (with-current-buffer view-buf
          (should-not mevedel-view--spinner-overlay)))))))

(mevedel-deftest mevedel-view--scaffolding-only-p ()
  ,test
  (test)
  :doc "marker-only and blank-only segments are scaffolding"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "\n#+end_tool\n\n#+begin_tool (Bash :command \"echo hi\")\n"))
    (should (mevedel-view--scaffolding-only-p
             data-buf (point-min) (with-current-buffer data-buf (point-max)))))

  :doc "real reasoning content is not scaffolding"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_reasoning\nLet me think about this.\n#+end_reasoning\n"))
    (should-not (mevedel-view--scaffolding-only-p
                 data-buf (point-min) (with-current-buffer data-buf (point-max))))))

(mevedel-deftest mevedel-view--user-turn-text/drawer-strip ()
  ,test
  (test)
  :doc "leading :PROPERTIES: drawer is stripped from user turn text"
  ;; Without the strip, gptel-org's per-buffer state drawer (system
  ;; prompt, model, GPTEL_BOUNDS) would render verbatim inside the
  ;; visible "You" turn on a full rerender that didn't pre-narrow past
  ;; the drawer.
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert ":PROPERTIES:\n:GPTEL_SYSTEM: hidden system prompt\n:END:\n")
      (insert "Real user prompt here.\n"))
    (let* ((seg (list 'user (point-min)
                      (with-current-buffer data-buf (point-max))))
           (text (mevedel-view--user-turn-text (list seg) data-buf)))
      (should (string-match-p "Real user prompt" text))
      (should-not (string-match-p "GPTEL_SYSTEM" text))
      (should-not (string-match-p "hidden system prompt" text)))))

(mevedel-deftest mevedel-view--render-mailbox-block
  (:doc "renders pure mailbox deliveries as message cards")
  ,test
  (test)

  :doc "pure agent-message turn renders without a You header"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "<agent-message from=\"explore--abc123\">\nhello\n</agent-message>\n"
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "✉ from explore--abc123" text))
        (should (string-match-p "hello" text))
        (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text)))))

  :doc "pure agent-result turn renders with the same mailbox card path"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "<agent-result agent-id=\"worker--xyz789\" type=\"worker\">\nresult\n</agent-result>\n"
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "✉ from worker--xyz789" text))
        (should (string-match-p "result" text))
        (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text))))))

(mevedel-deftest mevedel-view-open-agent-transcript-at-point
  (:doc "opens transcript at attribution targets")
  ,test
  (test)

  :doc "insert-attribution stamps clickable id with transcript property"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explore--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr"
                       :root temporary-file-directory
                       :name "attr"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-attr-session"))))
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explore--abc123.chat.org"
                          :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let* ((s (mevedel-view--insert-attribution agent-id))
               (pos (string-match-p "explore--abc123" s)))
          (should pos)
          (should (equal agent-id
                         (get-text-property pos 'mevedel-view-agent-id s)))))))

  :doc "running transcript attribution is clickable and reports still-running"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explore--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr-running"
                       :root temporary-file-directory
                       :name "attr-running"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-attr-running-session")))
           message-text)
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explore--abc123.chat.org"
                          :status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let* ((s (mevedel-view--insert-attribution agent-id nil 7))
               (pos (string-match-p "explore--abc123" s)))
          (should pos)
          (should (get-text-property pos 'keymap s))
          (should (equal agent-id
                         (get-text-property pos 'mevedel-view-agent-id s)))
          (let ((inhibit-read-only t)
                start)
            (goto-char mevedel-view--input-marker)
            (setq start (point))
            (insert s)
            (goto-char (+ start pos)))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (mevedel-view-open-agent-transcript-at-point))
          (should (string-match-p "still running" message-text))
          (should (string-match-p "7 tool calls" message-text)))))))

  :doc "read-only attach does not open running transcripts through attribution"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explore--abc123")
           (root (file-name-as-directory
                  (make-temp-file "mevedel-attr-readonly" t)))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr-readonly"
                       :root root
                       :name "attr-readonly"))
           (session (mevedel-session-create "main" workspace))
           opened
           message-text)
      (setf (mevedel-session-save-path session) root)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explore--abc123.chat.org"
                          :status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-session--read-only-mode t))
      (with-current-buffer view-buf
        (let* ((s (mevedel-view--insert-attribution agent-id nil 4))
               (pos (string-match-p "explore--abc123" s)))
          (let ((inhibit-read-only t)
                start)
            (goto-char mevedel-view--input-marker)
            (setq start (point))
            (insert s)
            (goto-char (+ start pos)))
          (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                     (lambda (&rest _) (setq opened t)))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
          (mevedel-view-open-agent-transcript-at-point))
        (should-not opened)
        (should (string-match-p "still running" message-text))))))

  :doc "survives display-region keymap overlay by using agent-id property"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explore--abc123")
           (opened nil)
           (s (copy-sequence "from explore--abc123")))
      (add-text-properties (length "from ") (length s)
                           `(mevedel-view-agent-id ,agent-id
                             keymap ,(make-sparse-keymap)
                             help-echo "Open transcript")
                           s)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((start (point)))
                (insert s)
                (add-text-properties
                 start (point)
                 `(read-only t keymap ,mevedel-view--display-map
                   front-sticky (read-only keymap)
                   rear-nonsticky (read-only keymap))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (goto-char (point-min))
        (search-forward "explore--abc123")
        (goto-char (match-beginning 0))
        (should (equal agent-id
                       (get-text-property (point) 'mevedel-view-agent-id)))
        (should (eq (get-text-property (point) 'keymap)
                    mevedel-view--display-map))
        (cl-letf (((symbol-function
                    'mevedel-view--open-agent-transcript-or-message)
                   (lambda (id &rest _) (setq opened id))))
          (mevedel-view-open-agent-transcript-at-point)
          (should (equal agent-id opened))))))

(mevedel-deftest mevedel-view--agent-transcript-setup
  (:doc "sets up transcript inspection views without chat input zones")
  ,test
  (test)

  :doc "transcript view has co-located hidden markers and no prompt"
  (let ((data-buf (generate-new-buffer " *test-agent-data*"))
        (view-buf (generate-new-buffer " *test-agent-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local default-directory temporary-file-directory))
          (mevedel-view--setup
           view-buf data-buf
           '(:agent-transcript-p t
             :agent-id "explore--abc123"
             :transcript-info (:agent-id "explore--abc123"
                               :status completed
                               :calls 2
                               :elapsed 1.5
                               :session-label "main")))
          (with-current-buffer view-buf
            (should mevedel-view--agent-transcript-p)
            (should (equal "explore--abc123" mevedel-view--agent-id))
            (should (= (point-min) (marker-position mevedel-view--input-marker)))
            (should (= (marker-position mevedel-view--status-marker)
                       (marker-position mevedel-view--input-marker)))
            (should (= (marker-position mevedel-view--interaction-marker)
                       (marker-position mevedel-view--input-marker)))
            (should-not (string-match-p "> " (buffer-string)))
            (should (string-match-p "Agent explore--abc123"
                                    (mevedel-view--agent-transcript-header-line)))
            (should-error (mevedel-view-send) :type 'user-error)
            (should-error (mevedel-view-abort) :type 'user-error)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view-agent-handle-activate
  (:doc "dispatches running handles to activity toggle and terminal handles to transcript open")
  ,test
  (test)

  :doc "running handle toggles activity expansion instead of opening transcript"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explore--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "handle"
                       :root temporary-file-directory
                       :name "handle"))
           (session (mevedel-session-create "main" workspace))
           opened)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "Agent line\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t)))
        (goto-char (point-min))
        (search-forward "Agent line")
        (goto-char (match-beginning 0))
        (should (eq 'running
                    (plist-get
                     (mevedel-view--lookup-transcript-entry agent-id)
                     :status)))
        (should (> mevedel-view-agent-activity-max 0))
        (mevedel-view--set-agent-expanded agent-id nil)
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (&rest _) (setq opened t)))
                  ((symbol-function 'mevedel-view--full-rerender)
                   (lambda () nil)))
          (mevedel-view-agent-handle-activate)
          (should-not opened)
          (should (plist-get (mevedel-view--agent-activity-state agent-id)
                             :expanded))))))

  :doc "terminal handle opens rendered transcript path"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explore--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "handle-terminal"
                       :root temporary-file-directory
                       :name "handle-terminal"))
           (session (mevedel-session-create "main" workspace))
           opened)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "Agent line\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t)))
        (goto-char (point-min))
        (search-forward "Agent line")
        (goto-char (match-beginning 0))
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (id) (setq opened id))))
          (mevedel-view-agent-handle-activate)
          (should (equal agent-id opened))))))

  :doc "terminal status wins when a running handle races completion"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explore--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "handle-race"
                       :root temporary-file-directory
                       :name "handle-race"))
           (session (mevedel-session-create "main" workspace))
           opened)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "Agent line\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t
                              'mevedel-view-agent-status 'running)))
        (goto-char (point-min))
        (search-forward "Agent line")
        (goto-char (match-beginning 0))
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (id) (setq opened id))))
          (mevedel-view-agent-handle-activate)
          (should (equal agent-id opened)))))))

(mevedel-deftest mevedel-view--agent-activity-state
  (:doc "tracks running activity expansion across block/unblock cycles")
  ,test
  (test)

  :doc "blocked auto-expands and unblock restores the pre-block state"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explore--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "activity-state"
                       :root temporary-file-directory
                       :name "activity-state"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (mevedel-view--set-agent-expanded agent-id nil)
        (setf (mevedel-session-permission-queue session)
              (list (list :origin agent-id)))
        (mevedel-view--agent-normalize-expansion-state agent-id 'running)
        (should (plist-get (mevedel-view--agent-activity-state agent-id)
                           :expanded))
        (setf (mevedel-session-permission-queue session) nil)
        (mevedel-view--agent-normalize-expansion-state agent-id 'running)
        (should-not (plist-get (mevedel-view--agent-activity-state agent-id)
                               :expanded)))))

  :doc "terminal transition clears stored expansion state"
  (mevedel-view-test--with-buffers
    (let ((agent-id "explore--abc123"))
      (with-current-buffer view-buf
        (mevedel-view--set-agent-expanded agent-id t)
        (mevedel-view--agent-normalize-expansion-state agent-id 'completed)
        (should-not (gethash agent-id mevedel-view--agent-activity-expanded))))))

(mevedel-deftest mevedel-view--agent-transcript-window
  (:doc "manages the singleton transcript side window")
  ,test
  (test)

  :doc "new transcript replaces the prior singleton and manual kill clears parent"
  (let ((parent (generate-new-buffer " *test-parent-view*"))
        (old-data (generate-new-buffer " *test-old-agent-data*"))
        (old-view (generate-new-buffer " *test-old-agent-view*"))
        (new-data (generate-new-buffer " *test-new-agent-data*"))
        (new-view (generate-new-buffer " *test-new-agent-view*"))
        displayed)
    (unwind-protect
        (progn
          (with-current-buffer old-data
            (org-mode))
          (mevedel-view--setup
           old-view old-data
           (list :agent-transcript-p t
                 :agent-id "explore--old"
                 :parent-view parent))
          (with-current-buffer new-data
            (org-mode))
          (mevedel-view--setup
           new-view new-data
           (list :agent-transcript-p t
                 :agent-id "explore--new"
                 :parent-view parent))
          (with-current-buffer parent
            (mevedel-view-mode)
            (setq-local mevedel-view--agent-transcript-window
                        (selected-window))
            (set-window-buffer (selected-window) old-view)
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (buf _action)
                         (setq displayed buf)
                         (set-window-buffer (selected-window) buf)
                         (selected-window))))
              (mevedel-view--display-agent-transcript-view new-view))
            (should (eq displayed new-view))
            (should-not (buffer-live-p old-view))
            (should (window-live-p mevedel-view--agent-transcript-window)))
          (kill-buffer new-view)
          (with-current-buffer parent
            (should-not mevedel-view--agent-transcript-window)))
      (when (buffer-live-p new-view) (kill-buffer new-view))
      (when (buffer-live-p new-data) (kill-buffer new-data))
      (when (buffer-live-p old-view) (kill-buffer old-view))
      (when (buffer-live-p old-data) (kill-buffer old-data))
      (when (buffer-live-p parent) (kill-buffer parent)))))

(mevedel-deftest mevedel-view-open-agent-transcript
  (:doc "opens terminal transcripts as rendered inspection views")
  ,test
  (test)

  :doc "terminal transcript opens rendered view and q kills view plus data buffer"
  (let* ((agent-id "explore--abc123")
         (root (file-name-as-directory
                (make-temp-file "mevedel-transcript-view" t)))
         (agents-dir (file-name-concat root "agents"))
         (rel-path "agents/explore--abc123.chat.org")
         (abs-path (file-name-concat root rel-path))
         (data-buf (generate-new-buffer " *test-parent-data*"))
         (view-buf (generate-new-buffer " *test-parent-view*"))
         agent-view
         agent-data)
    (make-directory agents-dir t)
    (with-temp-file abs-path
      (insert "*** Agent prompt\n"))
    (unwind-protect
        (let* ((workspace (mevedel-workspace--create
                           :type 'project :id "transcript-view"
                           :root root :name "transcript-view"))
               (session (mevedel-session-create "main" workspace)))
          (setf (mevedel-session-save-path session) root)
          (setf (mevedel-session-agent-transcripts session)
                (list (cons agent-id
                            (list :path rel-path
                                  :status 'completed
                                  :calls 3
                                  :elapsed 2.5))))
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local default-directory root))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (buf _action)
                         (set-window-buffer (selected-window) buf)
                         (selected-window))))
              (mevedel-view-open-agent-transcript agent-id)))
          (setq agent-view (get-buffer "*mevedel-agent:explore--abc123*"))
          (should (buffer-live-p agent-view))
          (with-current-buffer agent-view
            (setq agent-data mevedel--data-buffer)
            (should mevedel-view--agent-transcript-p)
            (should (string-match-p "3 calls"
                                    (mevedel-view--agent-transcript-header-line)))
            (mevedel-view-close-agent-transcript))
          (should-not (buffer-live-p agent-view))
          (should-not (buffer-live-p agent-data)))
      (when (buffer-live-p agent-view) (kill-buffer agent-view))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view--agent-status-collect
  (:doc "derives aggregate rows from current rendered handles and sidecar")
  ,test
  (test)

  :doc "counts sidecar running and terminal handles present in the current view"
  (mevedel-view-test--with-buffers
    (let* ((running-id "explore--run123")
           (done-id "explore--done123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-collect"
                       :root temporary-file-directory
                       :name "status-collect"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons running-id '(:status running :calls 1))
                  (cons done-id '(:status completed :calls 2))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "running\n"
                              'mevedel-view-agent-id running-id
                              'mevedel-view-agent-handle-p t))
          (insert (propertize "done\n"
                              'mevedel-view-agent-id done-id
                              'mevedel-view-agent-handle-p t)))
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (= 2 (length rows)))
          (should (cl-find 'running rows :key (lambda (row)
                                                (plist-get row :status))))
          (should (cl-find 'completed rows :key (lambda (row)
                                                  (plist-get row :status))))))))

  :doc "stale queue origins do not promote terminal handles to blocked"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explore--done123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-stale"
                       :root temporary-file-directory
                       :name "status-stale"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status completed))))
      (setf (mevedel-session-permission-queue session)
            (list (list :origin agent-id)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "done\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t)))
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (= 1 (length rows)))
          (should (eq 'completed (plist-get (car rows) :status))))))))

(mevedel-deftest mevedel-view-agent-status-activate-row
  (:doc "reveals aggregate rows without opening transcripts")
  ,test
  (test)

  :doc "running row reveals handle and expands activity"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explore--run123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-reveal"
                       :root temporary-file-directory
                       :name "status-reveal"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "handle\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t))
          (insert (mevedel-view--agent-status-row-string
                   (list :agent-id agent-id :status 'running))))
        (goto-char (point-min))
        (search-forward "explore--run123")
        (cl-letf (((symbol-function 'mevedel-view--full-rerender)
                   (lambda () nil)))
          (mevedel-view-agent-status-activate-row))
        (should (plist-get (mevedel-view--agent-activity-state agent-id)
                           :expanded)))))

  :doc "terminal row reveal does not open transcript"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explore--done123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-reveal-terminal"
                       :root temporary-file-directory
                       :name "status-reveal-terminal"))
           (session (mevedel-session-create "main" workspace))
           opened)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "handle\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t))
          (insert (mevedel-view--agent-status-row-string
                   (list :agent-id agent-id :status 'completed))))
        (goto-char (point-min))
        (search-forward "explore--done123")
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (&rest _) (setq opened t))))
          (mevedel-view-agent-status-activate-row))
        (should-not opened)))))

(mevedel-deftest mevedel-view--insert-attribution
  (:doc "builds transcript attribution fragments")
  ,test
  (test)

  :doc "uses short display label and no keymap when no sidecar entry exists"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((s (mevedel-view--insert-attribution
                 "explore--abcdef1234567890"))
             (pos (string-match-p "explore--abcdef12" s)))
        (should (string-match-p "from explore--abcdef12" s))
        (should pos)
        (should-not (get-text-property pos 'keymap s))
        (should (get-text-property pos 'help-echo s)))))

  :doc "completed transcript dispatches through the shared open command"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explore--abcdef1234567890")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr-open"
                       :root temporary-file-directory
                       :name "attr-open"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-attr-open-session")))
           opened)
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explore--abcdef12.chat.org"
                          :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (id) (setq opened id))))
          (mevedel-view--open-agent-transcript-or-message agent-id)
          (should (equal agent-id opened)))))))

(mevedel-deftest mevedel-view--tool-one-liner/scaffolding-prefix ()
  ,test
  (test)
  :doc "scaffolding prefix on the segment doesn't drop the tool name"
  ;; A boundary-expansion or patch can land seg-start on the
  ;; `#+begin_tool …' line (no gptel property) instead of the call
  ;; sexp.  The cleaner skips the marker so the parse still surfaces
  ;; `Bash: …' instead of bare `Tool'.
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool (Bash :command \"git status\")\n"
              "(:name \"Bash\" :args (:command \"git status\"))\n"
              "\nOK\n"))
    (let ((line (mevedel-view--tool-one-liner
                 data-buf (point-min)
                 (with-current-buffer data-buf (point-max)))))
      (should (string-match-p "Bash" line))
      (should-not (string-match-p "\\bTool\\b" line)))))


(provide 'test-mevedel-view)

;;; test-mevedel-view.el ends here
