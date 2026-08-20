;;; test-mevedel-view-disclosure.el -- Transcript disclosure tests -*- lexical-binding: t -*-

;;; Commentary:

;; Source-backed disclosure identity, state, and action coverage.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-structs)
(require 'mevedel-tool-registry)
(require 'mevedel-view)
(require 'mevedel-view-disclosure)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)

;; `mevedel-session-persistence'
(defvar mevedel-session--read-only-mode)


;;
;;; Disclosure

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
     (mevedel-view-stream-render-response (point-min) (point-max)))
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
         (should (string-match-p "Read: .*f\\.el" text))
         (should (string-match-p "full content here" text)))
       (search-forward "full content here")
       (let ((body-start (match-beginning 0)))
         (should (equal "    " (get-text-property body-start 'line-prefix)))
         (should (equal "    " (get-text-property body-start 'wrap-prefix)))
         (goto-char body-start)
         (should (looking-at-p "full content here"))))))

  :doc "fallback prompt disclosure keeps its header when expanded"
  (mevedel-view-test--with-buffers
   (with-current-buffer data-buf
     (insert ":PROMPT:\nExpanded prompt body.\n:END:\n"))
   (with-current-buffer view-buf
     (let ((inhibit-read-only t)
           (source (cons 1 (with-current-buffer data-buf (point-max)))))
       (goto-char mevedel-view--input-marker)
       (mevedel-view--insert-rendered-tool
        '(:header "Prompt"
                  :body "Expanded prompt body."
                  :body-mode markdown-mode
                  :vtype prompt-summary
                  :initially-collapsed-p t)
        source))
     (goto-char (point-min))
     (search-forward "Prompt")
     (goto-char (match-beginning 0))
     (mevedel-view-toggle-section)
     (let ((text (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker)))
       (should (string-match-p "^  ◆ Prompt$" text))
       (should (= 1 (mevedel-view-test--count-substring "Prompt" text)))
       (should (string-match-p "Expanded prompt body" text)))
     (goto-char (point-min))
     (search-forward "Expanded prompt body")
     (let ((body-start (match-beginning 0)))
       (should (equal "    " (get-text-property body-start 'line-prefix)))
       (should (equal "    " (get-text-property body-start 'wrap-prefix)))
       (goto-char body-start)
       (mevedel-view-toggle-section))
     (let ((text (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker)))
       (should (= 1 (mevedel-view-test--count-substring "Prompt" text)))
       (should-not (string-match-p "Expanded prompt body" text)))))

  :doc "embedded tool syntax cannot hijack a prompt disclosure"
  (mevedel-view-test--with-buffers
   (with-current-buffer data-buf
     (insert (concat ":PROMPT:\nExpanded follow-up prompt.\n"
                     ",#+begin_tool (Read :file_path \"/tmp/example.el\")\n"
                     "(:name \"Read\" :args "
                     "(:file_path \"/tmp/example.el\"))\n\n"
                     "Embedded tool output.\n,#+end_tool\n:END:\n")))
   (with-current-buffer view-buf
     (let ((inhibit-read-only t)
           (source (cons 1 (with-current-buffer data-buf (point-max)))))
       (goto-char mevedel-view--input-marker)
       (mevedel-view--insert-rendered-tool
        '(:header "Prompt"
                  :body "Expanded follow-up prompt."
                  :body-mode markdown-mode
                  :vtype prompt-summary
                  :initially-collapsed-p t)
        source))
     (goto-char (point-min))
     (search-forward "Prompt")
     (mevedel-view-toggle-section)
     (let ((text (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker)))
       (should (string-search "Expanded follow-up prompt." text)))
     (goto-char (point-min))
     (search-forward "Expanded follow-up prompt.")
     (mevedel-view-toggle-section)
     (let ((text (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker)))
       (should (string-search "◆ Prompt" text))
       (should-not (string-search "Read: /tmp/example.el" text)))))

  :doc "collapsed tools retain material sandbox disclosure"
  (mevedel-view-test--with-buffers
   (with-current-buffer data-buf
     (insert "sandboxed tool source\n"))
   (with-current-buffer view-buf
     (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
            (rendering
             '(:header "Bash: test"
                       :body "test output\n"
                       :body-mode text-mode
                       :sandbox-summary
                       (:attempt-count 1 :started-count 1 :refused-count 0
                                       :sandbox bubblewrap :filesystem workspace-write
                                       :network unrestricted :proc fresh
                                       :additional-read-count 0 :additional-write-count 1))))
       (let ((inhibit-read-only t))
         (goto-char mevedel-view--input-marker)
         (mevedel-view--render-expanded-body rendering source))
       (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                  (lambda (_buf _start _end &optional _collapsed-only)
                    rendering)))
         (goto-char (point-min))
         (search-forward "test output")
         (mevedel-view-toggle-section)
         (let ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker)))
           (should (string-match-p "Bash: test" text))
           (should (string-match-p
                    "Sandbox:.*network access allowed.*additional filesystem write access"
                    text))
           (should-not (string-match-p "test output" text)))))))

  :doc "non-expandable tool events remain non-toggleable and untracked"
  (mevedel-view-test--with-buffers
   (mevedel-tool-register
    (mevedel-tool--create
     :name "EventStateTool"
     :category "mevedel"
     :renderer (lambda (_name _args _result _data)
                 (list :header "EventStateTool: complete"
                       :expandable-p nil))))
   (mevedel-view-test--insert-data
    data-buf
    "(:name \"EventStateTool\" :args (:id 1))\n\nevent body hidden\n"
    '(tool . "call_event_state"))
   (with-current-buffer view-buf
     (let ((inhibit-read-only t))
       (goto-char mevedel-view--input-marker)
       (mevedel-view--render-tool-group
        (list (list 'tool 1 (with-current-buffer data-buf (point-max))))
        data-buf))
     (goto-char (point-min))
     (search-forward "EventStateTool: complete")
     (goto-char (match-beginning 0))
     (should (eq (get-text-property (point) 'mevedel-view-type)
                 'tool-event))
     (should-not (get-text-property (point) 'mevedel-view-source))
     (should-error (mevedel-view-toggle-section) :type 'user-error)
     (should (= 0 (hash-table-count mevedel-view-disclosure--source-states)))
     (should-not (search-forward "event body hidden"
                                 mevedel-view--input-marker t))))

  :doc "TAB toggles running and completed Agent rows; RET opens transcripts"
  (dolist (status '(running completed))
    (mevedel-view-test--with-buffers
     (with-current-buffer data-buf
       (insert "agent source data\n"))
     (let* ((agent-path (format "/root/%s" status))
            (source (cons 1 (with-current-buffer data-buf (point-max))))
            (rendering
             `(:header ,(format "Started %s" agent-path)
                       :body "Agent details\n"
                       :body-mode text-mode
                       :vtype agent-handle
                       :agent-path ,agent-path
                       :agent-status ,status
                       :initially-collapsed-p t))
            opened)
       (with-current-buffer view-buf
         (let ((inhibit-read-only t))
           (goto-char mevedel-view--input-marker)
           (mevedel-view--insert-rendered-tool rendering source))
         (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                    (lambda (_buf _start _end &optional _collapsed-only)
                      rendering))
                   ((symbol-function
                     'mevedel-view-open-agent-transcript-at-point)
                    (lambda (&optional _event)
                      (setq opened t))))
           (goto-char (point-min))
           (search-forward "Started")
           (mevedel-view-toggle-section)
           (should (search-forward "Agent details"
                                   mevedel-view--input-marker t))
           (should-not opened)
           (goto-char (point-min))
           (search-forward agent-path)
           (goto-char (match-beginning 0))
           (should (eq #'mevedel-view-toggle-section
                       (lookup-key (get-text-property (point) 'keymap)
                                   (kbd "TAB"))))
           (mevedel-view-toggle-section)
           (should-not (search-forward "Agent details"
                                       mevedel-view--input-marker t))
           (goto-char (point-min))
           (search-forward agent-path)
           (goto-char (match-beginning 0))
           (should (eq #'mevedel-view-activate-at-point
                       (lookup-key (get-text-property (point) 'keymap)
                                   (kbd "RET"))))
           (mevedel-view-activate-at-point)
           (should opened))))))

  :doc "prompt summaries expand and collapse through their renderer"
  (mevedel-view-test--with-buffers
   (with-current-buffer data-buf
     (insert "(:name \"Agent\" :args (:task_name \"explore\" :message \"Inspect.\"))\n\nraw launch payload\n"))
   (with-current-buffer view-buf
     (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
            (rendering '(:header "Agent: explorer -- Find calls"
                                 :body "rendered agent body\n"
                                 :body-mode text-mode
                                 :vtype prompt-summary)))
       (let ((inhibit-read-only t))
         (goto-char mevedel-view--input-marker)
         (set-marker-insertion-type mevedel-view--input-marker t)
         (unwind-protect
             (let ((start (point)))
               (insert "› Agent: explorer -- Find calls\nrendered agent body\n")
               (add-text-properties
                start (point)
                `(font-lock-face mevedel-view-tool-summary
                                 mevedel-view-type prompt-summary
                                 mevedel-view-collapsed nil
                                 mevedel-view-source ,source
                                 read-only t
                                 keymap ,mevedel-view--display-map
                                 front-sticky (read-only keymap)
                                 rear-nonsticky (read-only keymap))))
           (set-marker-insertion-type mevedel-view--input-marker nil)))
       (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                  (lambda (buf start end &optional _collapsed-only)
                    (should (eq buf data-buf))
                    (should (= start (car source)))
                    (should (= end (cdr source)))
                    rendering)))
         (goto-char (point-min))
         (search-forward "rendered agent body")
         (mevedel-view-toggle-section)
         (let ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker)))
           (should (string-match-p "Agent: explorer -- Find calls" text))
           (should-not (string-match-p "rendered agent body" text))
           (should-not (string-match-p "raw launch payload" text))
           (goto-char (point-min))
           (search-forward "Agent: explorer")
           (goto-char (match-beginning 0))
           (should (eq (get-text-property (point) 'mevedel-view-type)
                       'prompt-summary))
           (should (eq (get-text-property (point)
                                          'mevedel-view-collapsed)
                       t)))
         (goto-char (point-min))
         (search-forward "Agent: explorer")
         (mevedel-view-toggle-section)
         (let ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker)))
           (should (string-match-p "rendered agent body" text))
           (should-not (string-match-p "raw launch payload" text))
           (goto-char (point-min))
           (search-forward "Agent: explorer")
           (goto-char (match-beginning 0))
           (should (eq (get-text-property (point)
                                          'mevedel-view-collapsed)
                       nil)))))))

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
               (insert (propertize "  … Thinking... (1 lines)\n"
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
       (should (string-match-p
                (concat
                 "^"
                 (regexp-quote "  … Thinking... (1 lines)")
                 "$")
                text))
       (should (= 1 (mevedel-view-test--count-substring
                     "Thinking..." text)))
       (should (string-match-p "deep thoughts here" text))
       (goto-char (point-min))
       (search-forward "deep thoughts here")
       (let ((body-start (match-beginning 0)))
         (should (equal "    "
                        (get-text-property body-start 'line-prefix)))
         (should (equal "    "
                        (get-text-property body-start 'wrap-prefix)))))
     ;; Collapse back; the thinking summary must return and headers
     ;; must still be intact.
     (goto-char (point-min))
     (search-forward "deep thoughts here")
     (goto-char (match-beginning 0))
     (mevedel-view-toggle-section)
     (let ((text (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker)))
       (should (string-match-p "^You$" text))
       (should (string-match-p "^Assistant$" text))
       (should (= 1 (mevedel-view-test--count-substring
                     "Thinking... (1 lines)" text)))
       (should-not (string-match-p "deep thoughts here" text)))))

  :doc "response text is a collapsible section (regression for
earlier removal of the catch-all collapse branch that dropped
response folding along with a dangerous best-guess preview path)."
  (mevedel-view-test--with-buffers
   (mevedel-view-test--insert-data
    data-buf
    "First line of reply.\nSecond line.\nThird line.\n"
    'response)
   (with-current-buffer data-buf
     (mevedel-view-stream-render-response (point-min) (point-max)))
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
       (should (string-match-p "Third line" text))
       (should-not (string-match-p "(3 lines)" text)))))

  :doc "response collapse and expand keep complete proposed-plan blocks hidden"
  (mevedel-view-test--with-buffers
   (let* ((old-plan "# Hidden plan\n")
          (session (mevedel-session--create
                    :name "test"
                    :workspace nil
                    :permission-mode 'ask)))
     (with-current-buffer data-buf
       (setq-local mevedel--session session))
     (with-current-buffer view-buf
       (setq-local mevedel--session session))
     (mevedel-view-test--insert-data
      data-buf
      (concat "Visible lead.\n<proposed_plan>\n"
              old-plan
              "</proposed_plan>\nVisible tail.\n")
      'response)
     (with-current-buffer data-buf
       (mevedel-view-stream-render-response (point-min) (point-max)))
     (with-current-buffer view-buf
       (goto-char (point-min))
       (search-forward "Visible lead")
       (goto-char (match-beginning 0))
       (mevedel-view-toggle-section)
       (let ((collapsed (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
         (should-not (string-match-p "<proposed_plan>" collapsed))
         (should-not (string-match-p "# Hidden plan" collapsed)))
       (goto-char (point-min))
       (search-forward "Visible lead")
       (goto-char (match-beginning 0))
       (mevedel-view-toggle-section)
       (let ((expanded (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker)))
         (should (string-match-p "Visible tail" expanded))
         (should-not (string-match-p "<proposed_plan>" expanded))
         (should-not (string-match-p "# Hidden plan" expanded))))))

  :doc "response table collapse and expand does not leave duplicate rows"
  (mevedel-view-test--with-buffers
   (mevedel-view-test--insert-data
    data-buf
    "| Name | Role |\n|------|------|\n| Alice | Engineer |\n| Bob | Designer |\n| Carol | Product Manager |\n"
    'response)
   (with-current-buffer data-buf
     (mevedel-view-stream-render-response (point-min) (point-max)))
   (with-current-buffer view-buf
     (goto-char (point-min))
     (search-forward "Alice")
     (goto-char (match-beginning 0))
     (mevedel-view-toggle-section)
     (let ((collapsed (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
       (should (string-match-p "(5 lines)" collapsed))
       (should-not (string-match-p "^| Bob" collapsed)))
     (goto-char (point-min))
     (search-forward "Name | Role")
     (goto-char (match-beginning 0))
     (mevedel-view-toggle-section)
     (let ((expanded (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker)))
       (should (= 1 (mevedel-view-test--count-substring "Alice" expanded)))
       (should (= 1 (mevedel-view-test--count-substring "Bob" expanded)))
       (should (= 1 (mevedel-view-test--count-substring "Carol" expanded))))))

  :doc "TAB on Assistant header folds the whole turn into a single
summary line; TAB again restores it exactly."
  (mevedel-view-test--with-buffers
   (mevedel-view-test--insert-data data-buf "*** Hi\n" nil)
   (mevedel-view-test--insert-data
    data-buf
    "Here is the first line.\nSecond line.\nThird line.\n"
    'response)
   (with-current-buffer data-buf
     (mevedel-view-stream-render-response (point-min) (point-max)))
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
       (should (string-match-p "Third line" text)))))

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
     (mevedel-view-stream-render-response (point-min) (point-max)))
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
     (mevedel-view-stream-render-response (point-min) (point-max)))
   (with-current-buffer view-buf
     (goto-char (point-min))
     (search-forward "You")
     (goto-char (match-beginning 0))
     (should-error (mevedel-view-toggle-section)
                   :type 'user-error)))

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
     (mevedel-view-stream-render-response (point-min) (point-max)))
   (with-current-buffer view-buf
     ;; Expand the thinking section first.
     (goto-char (point-min))
     (search-forward "Thinking...")
     (goto-char (match-beginning 0))
     (mevedel-view-toggle-section)
     (let ((text (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker)))
       (should (string-match-p "deep thoughts live here" text))
       (should (string-match-p "Thinking\\.\\.\\." text)))
     ;; Fold the whole turn.
     (goto-char (point-min))
     (search-forward "Assistant")
     (goto-char (match-beginning 0))
     (mevedel-view-toggle-section)
     (let ((text (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker)))
       (should (string-match-p "Assistant — " text))
       (should-not (string-match-p "deep thoughts live here" text)))
     ;; Unfold; the thinking section must still be EXPANDED.
     (goto-char (point-min))
     (search-forward "Assistant — ")
     (goto-char (match-beginning 0))
     (mevedel-view-toggle-section)
     (let ((text (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker)))
       (should (string-match-p "^Assistant$" text))
       (should (string-match-p "deep thoughts live here" text))
       (should (string-match-p "Thinking\\.\\.\\." text))
       (should (string-match-p "Visible response text" text))))))

(mevedel-deftest mevedel-view-disclosure-section-bounds ()
  ,test
  (test)
  :doc "distinguishes equal-but-distinct source conses (regression)
Thinking-cons and turn-fallback-cons can have equal values but be
separate cons objects.  `section-bounds' must compare by `eq', not
`equal', or it will treat them as one run and expand/collapse over
the preceding header."
  (let ((mevedel-session--read-only-mode nil))
    (mevedel-view-test--with-buffers
     (with-current-buffer data-buf
       (insert "deep thoughts here\n"))
     (with-current-buffer view-buf
       (let ((inhibit-read-only t)
             ;; Equal values, distinct objects; this matches the real render
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
                 (insert (propertize "  … Thinking... (1 lines)\n"
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
         ;; Point at the exact start of the thinking run; the boundary
         ;; case where `previous-single-property-change' lands in the
         ;; preceding "Assistant\n" run.
         (goto-char (point-min))
         (search-forward "Thinking...")
         (goto-char (match-beginning 0))
         (let ((bounds (mevedel-view-disclosure-section-bounds)))
           (should bounds)
           (should (eq (get-text-property (car bounds)
                                          'mevedel-view-source)
                       thinking-src))
           (should (eq (get-text-property (point) 'mevedel-view-source)
                       thinking-src))
           ;; The bounds must not reach into the Assistant header.
           (let ((header-text (buffer-substring-no-properties
                               (car bounds) (cdr bounds))))
             (should-not (string-match-p "Assistant" header-text)))))))))


(provide 'test-mevedel-view-disclosure)
;;; test-mevedel-view-disclosure.el ends here
