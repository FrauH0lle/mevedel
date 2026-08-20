;;; test-mevedel-view-render.el --- View rendering tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel-agent-tools)
(require 'mevedel-agent-conversation)
(require 'mevedel-chat)
(require 'mevedel-directive)
(require 'mevedel-view)
(require 'mevedel-view-audit)
(require 'mevedel-view-render)
(require 'mevedel-view-segments)

;; Focused dependencies moved with the rendering tests.
(require 'mevedel-view-stream)
(require 'mevedel-menu)
(require 'mevedel-transcript)
(require 'mevedel-transcript-restore)
(require 'mevedel-structs)
(require 'mevedel-pipeline)
(require 'mevedel-tool-media)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-repair)
(require 'mevedel-mentions)
(require 'mevedel-skills-ui)
(require 'mevedel-workspace)
(require 'mevedel-file-state)
(require 'mevedel-session-persistence)
(require 'mevedel-tool-ui)
(require 'mevedel-permission-queue)
(require 'mevedel-tool-exec)
(require 'mevedel-goal)
(require 'mevedel-tool-task)
(require 'mevedel-agents)
(require 'mevedel-hooks)
(require 'mevedel-review)
(require 'mevedel-view-zone)
(require 'mevedel-view-history)



;;; Turn grouping

(defun mevedel-view-test--group-synthetic-segments (segments)
  "Group synthetic SEGMENTS against a sufficiently large data buffer."
  (with-temp-buffer
    (insert (make-string 300 ?x))
    (mevedel-view--group-into-turns segments (current-buffer))))

;;
;;; Turn grouping and tool summaries

(mevedel-deftest mevedel-view--tool-result-error-p ()
  ,test
  (test)
  :doc "recognizes canonical and legacy prose error prefixes"
  (should (mevedel-view--tool-result-error-p "Error: File is missing"))
  (should (mevedel-view--tool-result-error-p "Error writing file: denied"))
  (should-not (mevedel-view--tool-result-error-p "Writing file succeeded")))

(mevedel-deftest mevedel-view--group-into-turns ()
  ,test
  (test)
  :doc "single user turn"
  (let* ((segs '((user 1 10)))
         (turns (mevedel-view-test--group-synthetic-segments segs)))
    (should (= 1 (length turns)))
    (should (eq 'user (plist-get (car turns) :role))))

  :doc "user then assistant turn"
  (let* ((segs '((user 1 10) (response 10 30) (tool 30 50)))
         (turns (mevedel-view-test--group-synthetic-segments segs)))
    (should (= 2 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (= 2 (length (plist-get (cadr turns) :segments)))))

  :doc "multiple user-assistant pairs"
  (let* ((segs '((user 1 10) (response 10 20) (user 20 30) (response 30 40)))
         (turns (mevedel-view-test--group-synthetic-segments segs)))
    (should (= 4 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (eq 'user (plist-get (caddr turns) :role)))
    (should (eq 'assistant (plist-get (cadddr turns) :role))))

  :doc "reasoning segments inside assistant turn absorbed"
  (let* ((segs '((user 1 10) (ignored 10 20) (reasoning 20 40)
                 (tool 40 80) (reasoning 80 90) (ignored 90 100)
                 (response 100 150)))
         (turns (mevedel-view-test--group-synthetic-segments segs)))
    (should (= 2 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (= 6 (length (plist-get (cadr turns) :segments)))))

  :doc "mid-turn nil gap after response absorbed when next is ignored/tool"
  (with-temp-buffer
    (insert (make-string 200 ?\s))
    (let* ((segs '((user 1 10) (response 10 50) (user 50 60)
                   (ignored 60 80) (tool 80 120) (response 120 200)))
           (turns (mevedel-view--group-into-turns segs (current-buffer))))
      (should (= 2 (length turns)))
      (should (eq 'user (plist-get (car turns) :role)))
      (should (eq 'assistant (plist-get (cadr turns) :role)))
      ;; All 5 non-user segments belong to one assistant turn
      (should (= 5 (length (plist-get (cadr turns) :segments))))))

  :doc "nil gap after response starts new user turn when next is response"
  (let* ((segs '((user 1 10) (response 10 50) (user 50 60)
                 (response 60 100)))
         (turns (mevedel-view-test--group-synthetic-segments segs)))
    ;; user | assistant(response) | user | assistant(response)
    (should (= 4 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (eq 'user (plist-get (caddr turns) :role)))
    (should (eq 'assistant (plist-get (cadddr turns) :role))))

  :doc "blank nil gap between response ranges stays in assistant turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "First answer.\n" 'response)
    (mevedel-view-test--insert-data data-buf "\n\n" nil)
    (mevedel-view-test--insert-data data-buf "Second answer.\n" 'response)
    (with-current-buffer data-buf
      (let* ((segments (mevedel-transcript-segments (point-min) (point-max)))
             (turns (mevedel-view--group-into-turns segments data-buf)))
        (should (equal '(assistant)
                       (mapcar (lambda (turn) (plist-get turn :role))
                               turns)))
        (should (= 3 (length (plist-get (car turns) :segments)))))))

  :doc "real user prompt between response ranges remains a user turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "First answer.\n" 'response)
    (mevedel-view-test--insert-data data-buf "\n\nSecond prompt.\n\n" nil)
    (mevedel-view-test--insert-data data-buf "Second answer.\n" 'response)
    (with-current-buffer data-buf
      (let* ((segments (mevedel-transcript-segments (point-min) (point-max)))
             (turns (mevedel-view--group-into-turns segments data-buf)))
        (should (equal '(assistant user assistant)
                       (mapcar (lambda (turn) (plist-get turn :role))
                               turns))))))

  :doc "real user prompt after response is not absorbed before reasoning"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "First answer.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf "\n\nSecond prompt.\n\n#+begin_reasoning\n" nil)
    (mevedel-view-test--insert-data data-buf "thinking\n" 'ignore)
    (with-current-buffer data-buf
      (let* ((segments (mevedel-transcript-segments (point-min) (point-max)))
             (turns (mevedel-view--group-into-turns segments data-buf)))
        (should (equal '(assistant user assistant)
                       (mapcar (lambda (turn) (plist-get turn :role))
                               turns))))))

  :doc "real user prompt after closed reasoning starts a user turn"
  (let* ((segs '((reasoning 1 20) (user 20 40) (reasoning 40 60)))
         (turns (mevedel-view-test--group-synthetic-segments segs)))
    (should (equal '(assistant user assistant)
                   (mapcar (lambda (turn) (plist-get turn :role))
                           turns))))

  :doc "same-turn steering after a tool starts a user turn"
  (let* ((segs '((response 1 20) (tool 20 40)
                 (user 40 60) (reasoning 60 80)))
         (turns (mevedel-view-test--group-synthetic-segments segs)))
    (should (equal '(assistant user assistant)
                   (mapcar (lambda (turn) (plist-get turn :role))
                           turns))))

  :doc "retained agent task after tool activity starts a user turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Initial prompt.\n" nil)
    (mevedel-view-test--insert-data
     data-buf "Tool activity.\n" '(tool . "call_1"))
    (mevedel-view-test--insert-data
     data-buf
     "* Agent Task: follow_up\n\nSecond prompt.\n"
     nil)
    (mevedel-view-test--insert-data data-buf "Second answer.\n" 'response)
    (with-current-buffer data-buf
      (let* ((segments (mevedel-transcript-segments (point-min) (point-max)))
             (turns (mevedel-view--group-into-turns segments data-buf)))
        (should (equal '(user assistant user assistant)
                       (mapcar (lambda (turn) (plist-get turn :role))
                               turns))))))

  :doc "scaffolding-only gap after response is still absorbed"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "First answer.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf "\n\n#+begin_reasoning\n" nil)
    (mevedel-view-test--insert-data data-buf "thinking\n" 'ignore)
    (with-current-buffer data-buf
      (let* ((segments (mevedel-transcript-segments (point-min) (point-max)))
             (turns (mevedel-view--group-into-turns segments data-buf)))
        (should (= 1 (length turns)))
        (should (eq 'assistant (plist-get (car turns) :role)))))))

  :doc "task background is its own turn beside the authoritative Agent Task"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat "<task-background>\nAdvisory context.\n</task-background>\n"
             "\n* Agent Task: child\n\nDo the work.\n")
     nil)
    (with-current-buffer data-buf
      (let* ((segments (mevedel-transcript-segments (point-min) (point-max)))
             (turns (mevedel-view--group-into-turns segments data-buf)))
        (should (equal '(task-background user)
                       (mapcar (lambda (turn) (plist-get turn :role)) turns)))
        (should (eq 'task-background
                    (caar (plist-get (car turns) :segments)))))))


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
        (should (string-match-p "3 lines" summary))
        (should (string-match "✓" summary))
        (should (eq 'mevedel-view-tool-marker
                    (get-text-property (match-beginning 0)
                                       'font-lock-face summary)))
        (should (string-match "Read" summary))
        (should (eq 'mevedel-view-tool-name
                    (get-text-property (match-beginning 0)
                                       'font-lock-face summary))))))

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

  :doc "hook-blocked tools show the blocking event and reason"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Bash\" :args (:command \"rm -rf /tmp/x\"))\n\nError: blocked by PreToolUse: blocked rm -rf test\n"
     '(tool . "call_hook_block"))
    (with-current-buffer data-buf
      (let ((summary (mevedel-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (string-match-p "Bash" summary))
        (should (string-match-p "rm -rf /tmp/x" summary))
        (should (string-match-p "blocked by PreToolUse: blocked rm -rf test"
                                summary))
        (should (string-match "!" summary))
        (should (eq 'mevedel-view-tool-warning
                    (get-text-property (match-beginning 0)
                                       'font-lock-face summary))))))

  :doc "fallback on unparseable content"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "not a valid sexp" '(tool . "call_3"))
    (with-current-buffer data-buf
      (let ((summary (mevedel-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (stringp summary))
        (should (> (length summary) 0)))))

  :doc "fallback suppresses marker-only tool fragments"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "#+begin_tool (Read :file_path \"x\")\n" '(tool . "call_4"))
    (with-current-buffer data-buf
      (should-not (mevedel-view--tool-one-liner
                   data-buf (point-min) (point-max)))))

  :doc "tool-level errors use warning marker"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/missing.el\"))\n\nError: File does not exist\n"
     '(tool . "call_error"))
    (with-current-buffer data-buf
      (let ((summary (mevedel-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (string-match "!" summary))
        (should (eq 'mevedel-view-tool-warning
                    (get-text-property (match-beginning 0)
                                       'font-lock-face summary))))))

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


;;
;;; Navigation

(mevedel-deftest mevedel-view-user-query-navigation ()
  ,test
  (test)
  :doc "jumps between visible user query headers"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let (first second)
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--status-marker)
          (setq first (point))
          (insert (propertize "You\n"
                              'mevedel-view-turn-role 'user))
          (insert "first question\n")
          (insert (propertize "Assistant\n"
                              'mevedel-view-turn-role 'assistant))
          (insert "answer\n")
          (setq second (point))
          (insert (propertize "You\n"
                              'mevedel-view-turn-role 'user))
          (insert "second question\n")
          (set-marker mevedel-view--status-marker (point))
          (set-marker mevedel-view--interaction-marker (point)))
        (goto-char (point-min))
        (mevedel-view-next-user-query)
        (should (= (point) first))
        (mevedel-view-next-user-query)
        (should (= (point) second))
        (mevedel-view-next-user-query)
        (should (= (point) second))
        (goto-char (+ second (length "You\ns")))
        (mevedel-view-previous-user-query)
        (should (= (point) second))
        (mevedel-view-previous-user-query)
        (should (= (point) first))))))

(mevedel-deftest mevedel-view-fork-point-at-point ()
  ,test
  (test)
  :doc "resolves the exact settled assistant response at point"
  (mevedel-view-test--with-buffers
    (let ((session
           (mevedel-session--create
            :authority-mode 'pid-lock
            :name "fork-points"
            :current-segment 1)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (insert "First prompt\n")
        (insert (propertize "First response\n" 'gptel 'response))
        (insert
         (mevedel--format-hook-audit-record
          '(:type fork-point :fork-point-id "fork-point-1"
            :segment 1 :turn 1 :file-turn 1 :cum-turn 1)))
        (insert "Second prompt\n")
        (insert (propertize "Second response\n" 'gptel 'response))
        (insert
         (mevedel--format-hook-audit-record
          '(:type fork-point :fork-point-id "fork-point-2"
            :segment 1 :turn 2 :file-turn 2 :cum-turn 2))))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--full-rerender)
        (should-not (string-search "hook audit" (buffer-string)))
        (goto-char (point-min))
        (search-forward "Assistant")
        (search-forward "Assistant")
        (let ((target (mevedel-view-fork-point-at-point)))
          (should (equal "fork-point-2"
                         (plist-get target :fork-point-id)))
          (should (= 1 (plist-get target :segment)))
          (should (= 2 (plist-get target :cum-turn)))))))
  :doc "rewinds through the exact settled assistant response at point"
  (mevedel-view-test--with-buffers
    (let ((session
           (mevedel-session--create
            :authority-mode 'pid-lock
            :name "rewind-point"
            :current-segment 1))
          called-buffer called-target returned-latest)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (insert "Prompt\n")
        (insert (propertize "Response\n" 'gptel 'response))
        (insert
         (mevedel--format-hook-audit-record
          '(:type fork-point :fork-point-id "rewind-point-1"
            :segment 1 :turn 1 :file-turn 1 :cum-turn 1))))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Assistant")
        (cl-letf
            (((symbol-function 'mevedel-session-rewind-rewind)
              (lambda (buffer target)
                (setq called-buffer buffer
                      called-target target)
                t))
             ((symbol-function 'mevedel-view-return-to-latest-segment)
              (lambda (&optional _event)
                (setq returned-latest t))))
          (mevedel-view-rewind-at-point)))
      (should (eq data-buf called-buffer))
      (should returned-latest)
      (should (equal "rewind-point-1"
                     (plist-get called-target :fork-point-id))))))

(mevedel-deftest mevedel-view--conversation-variant-button ()
  ,test
  (test)
  :doc "renders the direct Source switch beside expanded and collapsed headers"
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-view-variants-" t)))
           (session
            (mevedel-session--create
             :authority-mode 'pid-lock
             :name "source"
             :session-id "source-id"
             :save-path (file-name-concat root "sessions/source/")
             :current-segment 1))
           (variants
            `((:save-path ,(file-name-concat root "sessions/source/")
               :variant-origin source
               :summary (:session-id "source-id"))
              (:save-path ,(file-name-concat root "sessions/child/")
               :variant-origin conversation
               :summary (:session-id "child-id")))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (insert (propertize "Response line one.\nResponse line two.\n"
                            'gptel 'response))
        (insert
         (mevedel--format-hook-audit-record
          '(:type fork-point :fork-point-id "fork-point-1"
            :segment 1 :turn 1 :file-turn 1 :cum-turn 1))))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (cl-letf
            (((symbol-function
               'mevedel-session-persistence-conversation-variants)
              (lambda (_session _fork-point-id &optional _sessions)
                variants)))
          (mevedel-view--full-rerender)
          (goto-char (point-min))
          (should (search-forward
                   "Assistant  [⇆ Source · 2 variants]" nil t))
          (should (functionp
                   (get-text-property
                    (1- (point)) 'mevedel-view-zone-activate)))
          (goto-char (point-min))
          (search-forward "Assistant")
          (mevedel-view--collapse-turn)
          (goto-char (point-min))
          (should (search-forward "[⇆ Source · 2 variants]" nil t))
          (should (functionp
                   (get-text-property
                    (1- (point)) 'mevedel-view-zone-activate)))
          (setq variants (list (car variants)))
          (mevedel-view--full-rerender)
          (goto-char (point-min))
          (should-not (search-forward "variants]" nil t))))))

  :doc "uses explicit live session context for an archived transcript"
  (with-temp-buffer
    (let ((session
           (mevedel-session--create
            :authority-mode 'pid-lock
            :name "source"
            :session-id "source-id"
            :save-path "/tmp/mevedel-view-sessions/source/"))
          (variants
           '((:variant-origin source
              :summary (:session-id "source-id"))
             (:variant-origin conversation
              :summary (:session-id "child-id")))))
      (insert (propertize "Archived response.\n" 'gptel 'response))
      (insert
       (mevedel--format-hook-audit-record
        '(:type fork-point :fork-point-id "fork-point-1"
          :segment 1 :turn 1 :file-turn 1 :cum-turn 1)))
      (let ((mevedel-view--conversation-variant-sessions variants))
        (cl-letf
            (((symbol-function
               'mevedel-session-persistence-conversation-variants)
              (lambda (_session _fork-point-id &optional _sessions)
                variants)))
          (should-not (bound-and-true-p mevedel--session))
          (should
           (string-search
            "[⇆ Source · 2 variants]"
            (mevedel-view--conversation-variant-button
             (current-buffer) (point-min) (point-max) session))))))))

(mevedel-deftest mevedel-view--render-turn ()
  ,test
  (test)
  :doc "does not scan fork points while incrementally rendering a live turn"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert (propertize "Streaming response.\n" 'gptel 'response)))
    (with-current-buffer view-buf
      (cl-letf
          (((symbol-function
             'mevedel-session-artifacts-fork-point-at-source)
            (lambda (&rest _)
              (ert-fail "Incremental render scanned fork points"))))
        (mevedel-view--render-turn
         '(:role assistant
           :segments ((response 1 21))
           :start 1 :end 21)
         data-buf))))

  :doc "renders task background collapsed without disturbing a composer draft"
  (mevedel-view-test--with-buffers
    (let (end)
      (with-current-buffer data-buf
        (insert "<task-background>\nAdvisory context.\n</task-background>\n")
        (setq end (point-max)))
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft "> quote\nsecond line" 3)
        (mevedel-view--render-turn
         (list :role 'task-background
               :segments (list (list 'task-background 1 end))
               :start 1 :end end)
         data-buf)
        (goto-char (point-min))
        (should (search-forward "Task background" nil t))
        (should (get-text-property (match-beginning 0)
                                   'mevedel-view-collapsed))
        (should-not (search-forward "Advisory context" nil t))
        (should (equal "> quote\nsecond line"
                       (mevedel-view--input-text)))))))

(mevedel-deftest mevedel-view-switch-conversation-variant ()
  ,test
  (test)
  :doc "opens the sole alternative, preserves drafts, and lands at the shared point"
  (mevedel-view-test--with-buffers
    (let* ((target-data (generate-new-buffer " *test-target-data*"))
           (target-view (generate-new-buffer " *test-target-view*"))
           (source-session
            (mevedel-session--create
             :authority-mode 'pid-lock
             :name "source"
             :session-id "source-id"
             :save-path "/tmp/mevedel-view-sessions/source/"
             :working-directory "/source/"
             :current-segment 1))
           (target-session
            (mevedel-session--create
             :authority-mode 'pid-lock
             :name "fork"
             :session-id "child-id"
             :save-path "/tmp/mevedel-view-sessions/child/"
             :working-directory "/worktree/"
             :forked-from-session-id "source-id"
             :forked-from-fork-point-id "fork-point-1"
             :fork-type 'worktree
             :current-segment 2
             :prompt-index
             '((1 . ((:fork-point-id "fork-point-1"
                      :cum-turn 1 :preview "shared"))))))
           (variants
            '((:save-path "/tmp/mevedel-view-sessions/source/"
               :variant-origin source
               :summary (:session-id "source-id"))
              (:save-path "/tmp/mevedel-view-sessions/child/"
               :variant-origin worktree
               :summary (:session-id "child-id"))))
           displayed
           targeted-segment)
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local mevedel--session source-session)
              (insert (propertize "Shared response.\n" 'gptel 'response))
              (insert
               (mevedel--format-hook-audit-record
                '(:type fork-point :fork-point-id "fork-point-1"
                  :segment 1 :turn 1 :file-turn 1 :cum-turn 1))))
            (with-current-buffer target-data
              (org-mode)
              (setq-local gptel-response-separator "\n\n")
              (setq-local gptel-prompt-prefix-alist
                          '((org-mode . "*** ")))
              (setq-local mevedel--session target-session)
              (insert (propertize "Shared response.\n" 'gptel 'response))
              (insert
               (mevedel--format-hook-audit-record
                '(:type fork-point :fork-point-id "fork-point-1"
                  :segment 1 :turn 1 :file-turn 1 :cum-turn 1))))
            (mevedel-view--setup target-view target-data)
            (cl-letf
                (((symbol-function
                   'mevedel-session-persistence-conversation-variants)
                  (lambda (_session _fork-point-id &optional _sessions)
                    variants))
                 ((symbol-function 'mevedel-session-persistence-restore)
                  (lambda (_save-path) target-data))
                 ((symbol-function 'mevedel-view-go-to-segment)
                  (lambda (number)
                    (setq targeted-segment number)))
                 ((symbol-function 'display-buffer)
                  (lambda (buffer &optional _action)
                    (setq displayed buffer)
                    nil)))
              (with-current-buffer target-view
                (setq-local mevedel--session target-session)
                (mevedel-view--full-rerender)
                (mevedel-view-test--insert-composer-draft
                 "> target draft\nsecond line"))
              (with-current-buffer view-buf
                (setq-local mevedel--session source-session)
                (mevedel-view--full-rerender)
                (mevedel-view-test--insert-composer-draft
                 "> source draft\nsecond line")
                (goto-char (point-min))
                (search-forward "[⇆ Source · 2 variants]")
                (backward-char 1)
                (mevedel-view-activate-at-point))
              (should (eq target-view displayed))
              (should (= 1 targeted-segment))
              (with-current-buffer view-buf
                (should (string-match-p
                         "> source draft\nsecond line"
                         (buffer-substring-no-properties
                          (mevedel-view--input-start) (point-max)))))
              (with-current-buffer target-view
                (should (eq 'turn-header
                            (get-text-property
                             (point) 'mevedel-view-type)))
                (should (string-match-p
                         "> target draft\nsecond line"
                         (buffer-substring-no-properties
                          (mevedel-view--input-start) (point-max)))))
              (should (equal "/source/"
                             (mevedel-session-working-directory
                              source-session)))
              (should (equal "/worktree/"
                             (mevedel-session-working-directory
                              target-session)))))
        (when (buffer-live-p target-view)
          (kill-buffer target-view))
        (when (buffer-live-p target-data)
          (kill-buffer target-data)))))
  :doc "opens the stable chooser when more than one alternative survives"
  (mevedel-view-test--with-buffers
    (let* ((session
            (mevedel-session--create
             :authority-mode 'pid-lock
             :name "source"
             :session-id "source-id"
             :save-path "/tmp/mevedel-view-sessions/source/"))
           (target-data (generate-new-buffer " *test-choice-data*"))
           (target-view (generate-new-buffer " *test-choice-view*"))
           (variants
            '((:save-path "/tmp/mevedel-view-sessions/source/"
               :variant-origin source
               :summary (:session-id "source-id"))
              (:save-path "/tmp/mevedel-view-sessions/child-1/"
               :variant-origin conversation
               :summary (:session-id "child-1"))
              (:save-path "/tmp/mevedel-view-sessions/child-2/"
               :variant-origin worktree
               :summary (:session-id "child-2"))))
           chosen)
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer target-data
              (setq-local mevedel--session
                          (mevedel-session--create
                           :authority-mode 'pid-lock
                           :name "target"
                           :session-id "child-2"))
              (setq-local mevedel--view-buffer target-view))
            (with-current-buffer target-view
              (setq-local mevedel--data-buffer target-data))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (cl-letf
                  (((symbol-function
                     'mevedel-session-persistence-conversation-variants)
                    (lambda (&rest _) variants))
                   ((symbol-function
                     'mevedel-session-persistence-choose-conversation-variant)
                    (lambda (choices current-id)
                      (setq chosen (list choices current-id))
                      (car (last choices))))
                   ((symbol-function 'mevedel-session-persistence-restore)
                    (lambda (_save-path) target-data))
                   ((symbol-function 'mevedel-view--full-rerender)
                    #'ignore)
                   ((symbol-function
                     'mevedel-view-goto-conversation-variant)
                    #'ignore)
                   ((symbol-function 'display-buffer)
                    #'ignore))
                (mevedel-view-switch-conversation-variant "fork-point-1")))
            (should (equal (list variants "source-id") chosen)))
        (when (buffer-live-p target-view)
          (kill-buffer target-view))
        (when (buffer-live-p target-data)
          (kill-buffer target-data))))))

(mevedel-deftest mevedel-view-switch-conversation-variant-at-point ()
  ,test
  (test)
  :doc "switches the conversation variant for the exact fork point at point"
  (mevedel-view-test--with-buffers
    (let (called)
      (with-current-buffer view-buf
        (cl-letf
            (((symbol-function 'mevedel-view-fork-point-at-point)
              (lambda () '(:fork-point-id "fork-point-1")))
             ((symbol-function 'mevedel-view-switch-conversation-variant)
              (lambda (fork-point-id)
                (setq called fork-point-id))))
          (mevedel-view-switch-conversation-variant-at-point)))
      (should (equal "fork-point-1" called)))))


;;
;;; Full rendering

(mevedel-deftest mevedel-view--debug-state ()
  ,test
  (test)
  :doc "does not repair drifted zone markers while observing state"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((prompt-start (marker-position mevedel-view--input-marker)))
        (goto-char (mevedel-view--input-start))
        (insert "> existing\nmultiline draft")
        (set-marker mevedel-view--input-marker (point-max))
        (let ((drifted (marker-position mevedel-view--input-marker))
              (mevedel-view-render-debug nil))
          (should-not (mevedel-view--debug-state data-buf))
          (should (= drifted
                     (marker-position mevedel-view--input-marker)))
          (let ((mevedel-view-render-debug t))
            (let ((state (mevedel-view--debug-state data-buf)))
              (should (plist-member state :point-in-composer))
              (should (= drifted (plist-get state :input)))
              (should (< prompt-start drifted))
              (should (= drifted
                         (marker-position
                          mevedel-view--input-marker))))))))))

(mevedel-deftest mevedel-view--full-rerender-reset ()
  ,test
  (test)
  :doc "clears stale display text and restores ordered zone markers"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (insert "stale display\n")
        (mevedel-view--full-rerender-reset
         data-buf data-buf nil nil))
      (should-not (string-match-p "stale display" (buffer-string)))
      (should (= (marker-position mevedel-view--status-marker)
                 (marker-position mevedel-view--input-marker)))
      (should (= (marker-position mevedel-view--interaction-marker)
                 (marker-position mevedel-view--input-marker))))))

(mevedel-deftest mevedel-view--full-rerender-project ()
  ,test
  (test)
  :doc "projects transcript turns and reports the last assistant turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
    (mevedel-view-test--insert-data data-buf "Response\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (mevedel-view--full-rerender-reset data-buf data-buf nil nil)
        (let ((rendering
               (mevedel-view--full-rerender-project
                data-buf data-buf view-buf nil nil nil)))
          (should (eq (plist-get rendering :view-buffer) view-buf))
          (should (eq (plist-get rendering :last-turn-role) 'assistant))
          (should (markerp
                   (plist-get rendering :last-assistant-turn-start)))))
      (should (string-match-p "Response" (buffer-string))))))

(mevedel-deftest mevedel-view--full-rerender-reanchor ()
  ,test
  (test)
  :doc "prefers the projected current assistant as the live anchor"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((assistant-start (copy-marker (point-min) nil)))
        (mevedel-view--full-rerender-reanchor
         data-buf
         (list :view-buffer view-buf
               :last-current-assistant-turn-start assistant-start
               :last-turn-role 'assistant)
         t 1 nil)
        (should (= (marker-position mevedel-view--in-flight-turn-start)
                   (marker-position assistant-start)))))))

(mevedel-deftest mevedel-view--full-rerender-finish ()
  ,test
  (test)
  :doc "rebuilds the input, status, interaction, and progress chrome"
  (mevedel-view-test--with-buffers
    (let (calls)
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view-refresh-input-prompt)
                   (lambda () (push 'input calls)))
                  ((symbol-function 'mevedel-view--render-status)
                   (lambda (_buffer) (push 'status calls)))
                  ((symbol-function 'mevedel-view--interaction-rebuild)
                   (lambda () (push 'interaction calls)))
                  ((symbol-function 'mevedel-view--ensure-request-progress)
                   (lambda (_buffer) (push 'progress calls))))
          (mevedel-view--full-rerender-finish
           data-buf data-buf (list :view-buffer view-buf) nil
           (float-time))))
      (should (equal (nreverse calls)
                     '(input status interaction progress))))))

(mevedel-deftest mevedel-view--strip-render-data-display-text ()
  ,test
  (test)
  :doc "preserves untrusted render-data delimiter lines as transcript text"
  (let* ((literal
          (substring-no-properties
           (mevedel-pipeline--format-render-data-block
            '(:kind request-summary :elapsed-seconds 9))))
         (reasoning (propertize literal 'gptel 'ignore)))
    (should (equal literal
                   (substring-no-properties
                    (mevedel-view--strip-render-data-display-text
                     reasoning))))))

(mevedel-deftest mevedel-view--full-rerender ()
  ,test
  (test)
  :doc "rebuilds view from data buffer"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** What is 2+2?\n" nil)
    (mevedel-view-test--insert-data data-buf "The answer is 4.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text1 (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "What is 2\\+2" text1))
        (mevedel-view--full-rerender)
        (let ((text2 (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
          (should (string-match-p "What is 2\\+2" text2))
          (should (string-match-p "answer is 4" text2))))))

  :doc "preserves an active transcript selection"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
    (mevedel-view-test--insert-data
     data-buf "Response with selected text.\n" 'response)
    (with-current-buffer view-buf
      (setq-local transient-mark-mode t)
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "selected text")
      (set-mark (match-beginning 0))
      (activate-mark)
      (mevedel-view--full-rerender)
      (should mark-active)
      (should (equal "selected text"
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))))))

  :doc "rolls back a destructive rebuild when projection fails"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
    (mevedel-view-test--insert-data data-buf "Response.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'mevedel-view--full-rerender-project)
                   (lambda (&rest _)
                     (error "Injected projection failure"))))
          (should-error (mevedel-view--full-rerender)))
        (should (equal before (buffer-string))))))

  :doc "records elapsed timing when render debug is enabled"
  (let ((mevedel-view-render-debug t)
        (mevedel-view-render-debug-buffer-name
         " *mevedel-view-full-rerender-test*"))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
          (mevedel-view-test--insert-data data-buf "Response\n" 'response)
          (with-current-buffer view-buf
            (mevedel-view--full-rerender))
          (with-current-buffer (get-buffer mevedel-view-render-debug-buffer-name)
            (goto-char (point-min))
            (should (search-forward "full-rerender-after-render" nil t))
            (should (search-forward ":elapsed" nil t))
            (should (search-forward ":window-point" nil t))
            (should (search-forward ":window-start" nil t))
            (goto-char (point-min))
            (should (search-forward ":point-in-composer" nil t))
            (should (search-forward ":point-composer-offset" nil t))
            (should (search-forward ":point-fragment" nil t))
            (should (search-forward "zone-reconcile-begin" nil t))
            (should (search-forward "zone-reconcile-end" nil t))
            (should (search-forward ":fragment-ids" nil t))))
      (when-let* ((buf (get-buffer mevedel-view-render-debug-buffer-name)))
        (kill-buffer buf))))
  :doc "keeps the newest turn expanded while folding older directive turns"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert
       (mevedel--format-hook-audit-record
        '(:type directive-turn-boundary :edge start
          :directive-id "directive-1" :action discuss :turn 3)))
      (insert "*** Earlier question :discuss:\n:PROMPT:\nFull request\n:END:\n")
      (insert (propertize "Earlier answer.\n" 'gptel 'response))
      (insert
       (mevedel--format-hook-audit-record
        '(:type directive-turn-boundary :edge end
          :directive-id "directive-1" :action discuss :turn 3
          :outcome success :activity-kind discussion :sequence 1)))
      (insert
       (mevedel--format-hook-audit-record
        '(:type directive-turn-boundary :edge start
          :directive-id "directive-1" :action discuss :turn 4)))
      (insert "*** Explain this :discuss:\n:PROMPT:\nFull request\n:END:\n")
      (insert (propertize "Complete answer.\n" 'gptel 'response))
      (insert
       (mevedel--format-hook-audit-record
        '(:type directive-turn-boundary :edge end
          :directive-id "directive-1" :action discuss :turn 4
          :outcome success :activity-kind discussion :sequence 1))))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should
         (string-search
          "◆ directi… · Discuss · T3 · Discussed · RET: actions"
          text))
        (should (string-search "Complete answer." text))
        (should-not (string-search "Earlier answer." text))
        (goto-char (point-min))
        (search-forward "RET: actions")
        (let ((face (get-text-property (match-beginning 0) 'face)))
          (should (eq 'shadow (plist-get face :inherit)))
          (should
           (equal (face-attribute 'mevedel-view-user-header
                                  :foreground nil 'default)
                  (plist-get face :overline))))
        (should (eq (get-text-property (line-beginning-position)
                                       'mouse-face)
                    'highlight))
        (should (eq (get-text-property (line-end-position) 'mouse-face)
                    'highlight))
        (should-not (get-text-property (1+ (line-end-position))
                                       'mouse-face))
        (goto-char (point-min))
        (search-forward "excluded from model context")
        (should-not (get-text-property (line-end-position) 'mouse-face)))
      (goto-char (point-min))
      (search-forward "T3")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-search "excluded from model context" text))
        (should (string-search "Prompt" text))
        (should (string-search "Earlier answer." text))
        (should (string-search "Complete answer." text))
        (should-not (string-search "\nAssistant\n" text)))
      (mevedel-view--full-rerender)
      (should (string-search "Earlier answer."
                             (buffer-substring-no-properties
                              (point-min) mevedel-view--input-marker)))
      (should (string-search "Complete answer."
                             (buffer-substring-no-properties
                              (point-min) mevedel-view--input-marker)))
      (should-error (mevedel-view-fork-point-at-point)
                    :type 'user-error)))

  :doc "dispatches state-correct actions from a rendered directive turn"
  (let* ((attempt
          (mevedel-directive-attempt--create
           :sequence 1 :action 'implement
           :checkpoint '(:session-id "main" :turn 3)))
         (workspace
          (mevedel-workspace--create
           :type 'file :id "directive-actions" :root "/tmp"
           :name "directive-actions"))
         (record
          (mevedel-directive--create
           :id "directive-1" :request "Explain this" :state 'discussed
           :anchor '(:state source-missing)
           :attempts (list attempt)
           :discussion
           (list
            (mevedel-directive-discussion-turn--create
             :sequence 2 :attempt-index 1))))
         (session (mevedel-session-create "main" workspace))
         dispatched)
    (mevedel-workspace-set-directives workspace (list record))
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (insert
         (mevedel--format-hook-audit-record
          '(:type directive-turn-boundary :edge start
            :directive-id "directive-1" :action discuss :turn 4)))
        (insert "*** Explain this :discuss:\n:PROMPT:\nFull request\n:END:\n")
        (insert (propertize "Complete answer.\n" 'gptel 'response))
        (insert
         (mevedel--format-hook-audit-record
          '(:type directive-turn-boundary :edge end
            :directive-id "directive-1" :action discuss :turn 4
            :outcome success :activity-kind discussion :sequence 2))))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (&rest _) '(?d "continue discussion")))
                  ((symbol-function 'mevedel-view-enter-directive-scope)
                   (lambda (&rest args) (setq dispatched args))))
          (mevedel-view--full-rerender)
          (goto-char (point-min))
          (search-forward "◆ directi…")
          (mevedel-view-activate-at-point)
          (should (equal (list record 'discuss 1 workspace)
                         dispatched))))))

  :doc "suppresses modification hooks while rebuilding rendered transcript"
  (mevedel-view-test--with-buffers
    (let ((changes 0))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (mevedel-view-test--insert-data data-buf "Response\n" 'response)
      (with-current-buffer view-buf
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--full-rerender)
        (should (= 0 changes))
        (should (string-match-p
                 "Response"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))))
  :doc "suppresses intermediate redisplay while rebuilding the transcript"
  (mevedel-view-test--with-buffers
    (let ((original (symbol-function 'mevedel-view--header-string))
          redisplay-inhibited)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view--header-string)
                   (lambda (buffer)
                     (setq redisplay-inhibited inhibit-redisplay)
                     (funcall original buffer))))
          (mevedel-view--full-rerender))
        (should redisplay-inhibited))))
  :doc "suppresses hooks while cleaning stale pending lines"
  (mevedel-view-test--with-buffers
    (let ((changes 0))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (mevedel-view-test--insert-data data-buf "Response\n" 'response)
      (with-current-buffer view-buf
        (let ((mevedel-view--pending-tool-calls
               (list (cons 'read "Calling Read…"))))
          (mevedel-view--insert-pending-tool-lines
           mevedel-view--pending-tool-calls))
        (setq mevedel-view--pending-tool-calls nil)
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--full-rerender)
        (should (= 0 changes))
        (should-not (text-property-any
                     (point-min) mevedel-view--input-marker
                     'mevedel-view-pending-tool-live t)))))
  :doc "restores task status fragment after full rerender"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'project
                :id "/tmp/view-task/"
                :root "/tmp/view-task/"
                :name "view-task"))
           (session (mevedel-session-create "main" ws)))
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "visible task" :status 'pending)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--view-buffer view-buf)
        (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
        (mevedel-view-test--insert-data data-buf "Response\n" 'response)
        (mevedel-tool-task--refresh-display))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (should (string-match-p "visible task" (buffer-string)))
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "visible task" text))
          (goto-char (point-min))
          (search-forward "visible task" mevedel-view--input-marker)
          (should (eq 'status (get-text-property
                               (1- (point))
                               'mevedel-view-zone-namespace)))
          (should (eq 'tasks (get-text-property
                              (1- (point))
                              'mevedel-view-zone-id)))))))
  :doc "rebuilds status and permission zones in order after full rerender"
  (mevedel-view-test--with-buffers
      (let* ((ws (mevedel-workspace--create
                  :type 'project
                  :id "/tmp/view-zones/"
                  :root "/tmp/view-zones/"
                  :name "view-zones"))
             (session (mevedel-session-create "main" ws))
             (permission-outcomes nil))
        (setf (mevedel-session-tasks session)
              (list (mevedel-task--create
                     :id 1 :subject "visible zone task"
                     :status 'pending)))
        (setf (mevedel-session-permission-queue session)
              (list (list :kind 'generic
                          :tool-name "Read"
                          :specifier-key :path
                          :specifier-value "/tmp/zones.txt"
                          :include-always t
                          :session session
                          :callback
                          (lambda (outcome)
                            (push outcome permission-outcomes)))))
        (with-current-buffer data-buf
          (setq-local mevedel--session session)
          (setq-local mevedel--view-buffer view-buf)
          (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
          (mevedel-view-test--insert-data data-buf "Response\n" 'response))
        (with-current-buffer view-buf
          (setq-local mevedel--session session)
          ;; Render task status before the full rerender so the fragment
          ;; region is rebuilt along with the other chrome zones.
          (mevedel-tool-task--refresh-display)
          (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                     (lambda ()
                       (list (list :path "/root/verifier"
                                   :status 'running)))))
            (mevedel-view--full-rerender))
          (should-not permission-outcomes)
          (let* ((text (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker))
                 (header (string-trim-right
                          (mevedel-view--header-string data-buf)))
                 (header-pos (string-search header text))
                 (task-pos (string-search "visible zone task" text))
                 (agent-pos (string-search
                             "Running /root/verifier" text))
                 (permission-pos (string-search "Permission Request"
                                                text)))
            (should header-pos)
            (should task-pos)
            (should agent-pos)
            (should permission-pos)
            (should (= 0 header-pos))
            (should (< header-pos task-pos))
            (should (< task-pos agent-pos))
            (should (< agent-pos permission-pos))
            (should (= 1 (how-many "Permission Request"
                                   (point-min)
                                   mevedel-view--input-marker)))))))
  :doc "does not restore task block after full rerender when all tasks are completed"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'project
                :id "/tmp/view-task-completed/"
                :root "/tmp/view-task-completed/"
                :name "view-task-completed"))
           (session (mevedel-session-create "main" ws)))
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "completed task" :status 'completed)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--view-buffer view-buf)
        (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
        (mevedel-view-test--insert-data data-buf "Response\n" 'response))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should-not (string-match-p "tasks" text))
          (should-not (string-match-p "completed task" text))))))
  :doc "header stays at top when rerendering (input-marker advances past it)"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Greetings\n" nil)
    (mevedel-view-test--insert-data data-buf "Hello back\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
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
  :doc "normalizes stale reasoning response prefixes during full rerender"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (reasoning-start nil-start response-start
            response-end)
        (setq reasoning-start (point))
        (insert "#+begin_reasoning\nThinking.\n#+end_reasoning\n")
        (insert "\n")
        (insert "Whi")
        (setq nil-start (point))
        (insert "l")
        (setq response-start (point))
        (insert "e the agents run, I'll test.\n")
        (setq response-end (point))
        (put-text-property reasoning-start nil-start 'gptel 'ignore)
        (put-text-property response-start response-end 'gptel 'response)))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "While the agents run, I'll test" text))
        (should-not (string-match-p "^le the agents run" text)))))
  :doc "normalizes restored props in read-only data buffers"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (prefix-start)
        (let ((inhibit-read-only t)
              reasoning-start nil-start response-start response-end)
          (setq reasoning-start (point))
          (insert "#+begin_reasoning\nThinking.\n#+end_reasoning\n\n")
          (setq prefix-start (point))
          (insert "Whi")
          (setq nil-start (point))
          (insert "l")
          (setq response-start (point))
          (insert "e the agents run, I'll test.\n")
          (setq response-end (point))
          (put-text-property reasoning-start nil-start 'gptel 'ignore)
          (put-text-property response-start response-end 'gptel 'response)
          (setq buffer-read-only t))
        (mevedel-transcript-restore-properties t)
        (should buffer-read-only)
        (should (eq (get-text-property prefix-start 'gptel) 'response)))))
  :doc "renders generated system reminders as compact control rows"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "<system-reminder>\nCRITICAL: verify only.\nReport findings.\n</system-reminder>\n"
     'ignore)
    (with-current-buffer view-buf
      (let (collapsed-header)
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "System reminder (2 lines)" text))
          (should-not (string-match-p "Thinking" text))
          (should-not (string-match-p "<system-reminder>" text)))
        (goto-char (point-min))
        (search-forward "System reminder")
        (setq collapsed-header
              (buffer-substring-no-properties
               (line-beginning-position) (line-end-position)))
        (should (equal "  ◇ System reminder (2 lines)"
                       collapsed-header))
        (goto-char (match-beginning 0))
        (should (eq (get-text-property (point) 'mevedel-view-type)
                    'system-reminder-summary))
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p
                   (concat "^" (regexp-quote collapsed-header) "$")
                   text))
          (should (= 1 (mevedel-view-test--count-substring
                        "System reminder" text)))
          (should (string-match-p "CRITICAL: verify only" text))
          (should (string-match-p "Report findings" text))
          (should-not (string-match-p "<system-reminder>" text))
          (should-not (string-match-p "</system-reminder>" text)))
        (goto-char (point-min))
        (search-forward "CRITICAL: verify only")
        (let ((body-start (match-beginning 0)))
          (should (equal "    " (get-text-property body-start 'line-prefix)))
          (should (equal "    " (get-text-property body-start 'wrap-prefix)))
          (goto-char body-start)
          (should (looking-at-p "CRITICAL: verify only"))
          (mevedel-view-toggle-section))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (= 1 (mevedel-view-test--count-substring
                        "System reminder" text)))
          (should-not (string-match-p "CRITICAL: verify only" text))
          (goto-char (point-min))
          (search-forward "System reminder")
          (should (equal collapsed-header
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))))))
  :doc "renders partial Worktree Fork disclosure as an expanded warning"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat
      "<system-reminder>\n"
      "Worktree Fork (partial restoration)\n"
      "Captured repository files restored: 1\n"
      "Unrestored captured files:\n"
      "- /repo/missing.el: backup unavailable\n"
      "</system-reminder>\n")
     'ignore)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Partial Worktree Fork" text))
        (should (string-match-p "/repo/missing.el" text)))
      (goto-char (point-min))
      (search-forward "Partial Worktree Fork")
      (should (equal "  ! Partial Worktree Fork (4 lines)"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
      (goto-char (match-beginning 0))
      (should-not (get-text-property (point) 'mevedel-view-collapsed))
      (should (eq 'mevedel-view-tool-warning
                  (get-text-property (point) 'font-lock-face)))
      (mevedel-view-toggle-section)
      (should-not
       (string-match-p
        "/repo/missing.el"
        (buffer-substring-no-properties
         (point-min) mevedel-view--input-marker)))
      (goto-char (point-min))
      (search-forward "Partial Worktree Fork")
      (should (equal "  ! Partial Worktree Fork (4 lines)"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
    (with-current-buffer data-buf
      (should (string-match-p "/repo/missing.el" (buffer-string)))))
  :doc "keeps generated system reminders separate from real thinking"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Answer first.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     "<system-reminder>\nUse verification mode.\n</system-reminder>\n"
     'ignore)
    (mevedel-view-test--insert-data data-buf "\n" nil)
    (mevedel-view-test--insert-data
     data-buf
     "#+begin_reasoning\nInspect the diff.\n#+end_reasoning\n"
     'ignore)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Answer first" text))
        (should (string-match-p "System reminder (1 line)" text))
        (should (string-match-p "Thinking... (1 lines)" text))
        (should-not (string-match-p "<system-reminder>" text)))))
  :doc "discloses the persisted expanded skill prompt after a full rerender"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Use $implement\n" nil)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind inline-skill
        :display-text "Use $implement"
        :expanded-prompt
        "Prepared implementation instructions.\n\nHook-generated context."))
     'ignore)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "You\nUse \\$implement" text))
        (should-not (string-match-p "Prepared implementation" text)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Prepared implementation instructions" text))
        (should (string-match-p "Hook-generated context" text)))))
  :doc "keeps fork reminder and elapsed footers with their assistant turns"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "First prompt.\n\n" nil)
    (mevedel-view-test--insert-data data-buf "First answer.\n\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind request-summary :elapsed-seconds 7))
     'ignore)
    (mevedel-view-test--insert-data
     data-buf
     "<system-reminder>\nConversation Fork\n</system-reminder>\n\n"
     'ignore)
    (mevedel-view-test--insert-data data-buf "Second prompt.\n\n" nil)
    (mevedel-view-test--insert-data data-buf "Second answer.\n\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind request-summary :elapsed-seconds 3))
     'ignore)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let* ((text (buffer-substring-no-properties
                    (point-min) mevedel-view--input-marker))
             (first-answer (string-search "First answer" text))
             (reminder (string-search "System reminder" text))
             (first-footer (string-search "Worked for 7s" text))
             (second-prompt (string-search "You\nSecond prompt" text))
             (second-answer (string-search "Second answer" text))
             (second-footer (string-search "Worked for 3s" text)))
        (should first-answer)
        (should reminder)
        (should first-footer)
        (should second-prompt)
        (should second-answer)
        (should second-footer)
        (should (< first-answer reminder first-footer
                   second-prompt second-answer second-footer))
        (should (= 2 (how-many "Worked for"
                               (point-min)
                               mevedel-view--input-marker))))))
  :doc "omits nested tool blocks from rendered reasoning text"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat "#+begin_reasoning\n"
             "I will inspect the file.\n"
             "#+begin_tool (Read :file_path \"a.el\")\n"
             "(:name \"Read\" :args (:file_path \"a.el\"))\n\n"
             "tool result line\n"
             "#+end_tool\n"
             "Then I will summarize.\n"
             "#+end_reasoning\n")
     'ignore)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (= 2 (how-many "Thinking\\.\\.\\. (1 lines)"
                                (point-min)
                                mevedel-view--input-marker)))
        (should (string-match-p "Read: a.el" text))
        (should-not (string-match-p "tool result line" text))
        (should-not (string-match-p "(:name \\\"Read\\\"" text)))
      (goto-char (point-min))
      (search-forward "Thinking...")
      (mevedel-view-toggle-section)
      (search-forward "Read: a.el")
      (search-forward "Thinking...")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "I will inspect the file" text))
        (should (string-match-p "Then I will summarize" text))
        (should-not (string-match-p "tool result line" text))
        (should-not (string-match-p "(:name \\\"Read\\\"" text)))))
  :doc "render-data-only segments after responses stay hidden"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Handoff\n\nBody.\n" nil)
    (mevedel-view-test--insert-data data-buf "Assistant answer.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind inline-skill :name "handoff" :arguments ""
              :display-text "/handoff"))
     'ignore)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Handoff" text))
        (should (string-match-p "Assistant answer" text))
        (should-not (string-match-p "mevedel-render-data" text))
        (should-not (string-match-p "inline-skill" text))
        (should-not (string-match-p "Thinking" text)))))
  :doc "separates response prose from following activity"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
    (mevedel-view-test--insert-data data-buf "First answer.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"a.el\"))\n\ncontents\n"
     '(tool . "call_visual"))
    (mevedel-view-test--insert-data data-buf "Second answer.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let (first-end tool-pos second-pos rule-pos)
        (goto-char (point-min))
        (search-forward "First answer")
        (setq first-end (point))
        (search-forward "Read")
        (setq tool-pos (match-beginning 0))
        (search-forward "Second answer")
        (setq second-pos (match-beginning 0))
        (let ((pos first-end))
          (while (and (< pos tool-pos) (not rule-pos))
            (when (eq (get-text-property pos 'font-lock-face)
                      'mevedel-view-activity-rule)
              (setq rule-pos pos))
            (setq pos (1+ pos))))
        (should rule-pos)
        (should (string-match-p "\n\n"
                                (buffer-substring-no-properties
                                 tool-pos second-pos))))))
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
        (should     (string-match-p "Actual reply" text)))))

  :doc "restores saved GPTEL_BOUNDS before rendering a persisted segment"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert ":PROPERTIES:\n"
              ":GPTEL_MODEL: test\n"
              ":GPTEL_BOUNDS: nil\n"
              ":END:\n\n"
              "*** Prompt\n\n"
              "Assistant intro.\n"
              "\n#+begin_tool (Read :file_path \"/tmp/a.png\")\n"
              "(:name \"Read\" :args (:file_path \"/tmp/a.png\"))\n\n"
              "<media-file>\n"
              "data:\n"
              "<native media block attached>\n"
              "</media-file>\n"
              "#+end_tool\n"
              "Assistant close.\n")
      ;; `org-entry-put' changes the drawer length, so recompute from
      ;; content anchors after each update until the stored positions are
      ;; aligned with the final buffer text.
      (dotimes (_ 3)
        (let (response-start response-end tool-start tool-end close-start
                             close-end)
          (goto-char (point-min))
          (search-forward "Assistant intro.")
          (setq response-start (match-beginning 0)
                response-end (line-end-position))
          (search-forward "#+begin_tool")
          (setq tool-start (match-beginning 0))
          (search-forward "#+end_tool")
          (setq tool-end (line-end-position))
          (search-forward "Assistant close.")
          (setq close-start (match-beginning 0)
                close-end (line-end-position))
          (org-entry-put
           (point-min) "GPTEL_BOUNDS"
           (prin1-to-string
            `((tool (,tool-start ,tool-end "call_1"))
              (response (,response-start ,response-end)
                        (,close-start ,close-end))))))))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Prompt" text))
        (should (string-match-p "Assistant intro" text))
        (should (string-match-p "Read: \\(?:/tmp/\\)?a\\.png" text))
        (should (string-match-p "Assistant close" text))
        (should-not (string-match-p "GPTEL_BOUNDS" text))
        (should-not (string-match-p ":PROPERTIES:" text))
        (should-not (string-match-p "<media-file>" text))
        (should-not (string-match-p "(:name \"Read\"" text)))))

  :doc "preserves tool and thinking fold state across full rerender"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"input.pdf\"))\n\nfirst\n"
     '(tool . "call_0"))
    (mevedel-view-test--insert-data data-buf "thinking\nmore thinking\n" 'ignore)
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"output.pdf\"))\n\nline\n"
     '(tool . "call_1"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (save-excursion
        (goto-char (point-min))
        (search-forward "Thinking...")
        (mevedel-view-toggle-section)
        (should-not (get-text-property (point) 'mevedel-view-collapsed))
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (search-forward "thinking"))
        (let ((body-start (match-beginning 0)))
          (should (equal "    " (get-text-property body-start 'line-prefix)))
          (should (equal "    " (get-text-property body-start 'wrap-prefix)))
          (goto-char body-start)
          (should (looking-at-p "thinking")))
        (goto-char (point-min))
        (search-forward "input.pdf")
        (mevedel-view-toggle-section)
        (should-not (get-text-property (point) 'mevedel-view-collapsed)))
      (mevedel-view--full-rerender)
      (save-excursion
        (goto-char (point-min))
        (search-forward "thinking")
        (should-not (get-text-property (point) 'mevedel-view-collapsed))
        (goto-char (point-min))
        (search-forward "input.pdf")
        (should-not (get-text-property (point) 'mevedel-view-collapsed))
        (mevedel-view-toggle-section)
        (should (get-text-property (point) 'mevedel-view-collapsed)))
      (mevedel-view--full-rerender)
      (save-excursion
        (goto-char (point-min))
        (search-forward "thinking")
        (should-not (get-text-property (point) 'mevedel-view-collapsed))
        (goto-char (point-min))
        (search-forward "input.pdf")
        (should (get-text-property (point) 'mevedel-view-collapsed)))))

  :doc "preserves source-backed agent handle state across full rerender"
  (mevedel-view-test--with-buffers
    (mevedel-tool-register
     (mevedel-tool--create
      :name "FullStateAgent"
      :category "mevedel"
      :renderer (lambda (_name _args result _data)
                  (list :header "Agent: verifier -- full state"
                        :body result
                        :body-mode 'text-mode
                        :vtype 'agent-handle
                        :initially-collapsed-p t))))
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"FullStateAgent\" :args (:task_name \"verify\"))\n\nfull rerender agent body\n"
     '(tool . "call_full_state_agent"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "Agent: verifier -- full state")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (should (search-forward "full rerender agent body"
                              mevedel-view--input-marker t))
      (let ((body-start (match-beginning 0)))
        (should (equal "    " (get-text-property body-start 'line-prefix)))
        (should (equal "    " (get-text-property body-start 'wrap-prefix))))
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "Agent: verifier -- full state")
      (goto-char (match-beginning 0))
      (should-not (get-text-property (point) 'mevedel-view-collapsed))
      (should (search-forward "full rerender agent body"
                              mevedel-view--input-marker t))))

  :doc "does not carry fold state to rewritten data at the same source start"
  (mevedel-view-test--with-buffers
    (mevedel-tool-register
     (mevedel-tool--create
      :name "RewriteStateTool"
      :category "mevedel"
      :renderer (lambda (_name _args result _data)
                  (list :header "RewriteStateTool: item"
                        :body result
                        :body-mode 'text-mode
                        :initially-collapsed-p t))))
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"RewriteStateTool\" :args (:id \"old\"))\n\nold expanded body\n"
     '(tool . "call_rewrite_old"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "RewriteStateTool: item")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (should (search-forward "old expanded body"
                              mevedel-view--input-marker t)))
    (with-current-buffer data-buf
      (erase-buffer)
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"RewriteStateTool\" :args (:id \"new\"))\n\nnew body must start collapsed\n"
       '(tool . "call_rewrite_new")))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "RewriteStateTool: item")
      (goto-char (match-beginning 0))
      (should (get-text-property (point) 'mevedel-view-collapsed))
      (should-not (search-forward "new body must start collapsed"
                                  mevedel-view--input-marker t))
      (should (= 0 (hash-table-count mevedel-view-disclosure--source-states)))))

  :doc "does not carry non-tool fold state to same-prefix rewritten data"
  (mevedel-view-test--with-buffers
    (let ((prefix (make-string 300 ?x)))
      (mevedel-view-test--insert-data
       data-buf (concat prefix "\nold thinking tail\n") 'ignore)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (should (search-forward "old thinking tail"
                                mevedel-view--input-marker t)))
      (with-current-buffer data-buf
        (erase-buffer)
        (mevedel-view-test--insert-data
         data-buf (concat prefix "\nnew thinking starts collapsed\n")
         'ignore))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (should (get-text-property (point) 'mevedel-view-collapsed))
        (should-not (search-forward "new thinking starts collapsed"
                                    mevedel-view--input-marker t)))))

  :doc "reanchors in-flight assistant after restoring earlier expanded fold"
  (mevedel-view-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-test--insert-data data-buf "*** First\n" nil)
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"Read\" :args (:file_path \"input.pdf\"))\n\nfirst\nsecond\nthird\n"
       '(tool . "call_0"))
      (mevedel-view-test--insert-data data-buf "\n\n*** Second\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (mevedel-view-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (save-excursion
          (goto-char (point-min))
          (search-forward "input.pdf")
          (mevedel-view-toggle-section)
          (should-not (get-text-property (point) 'mevedel-view-collapsed)))
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (mevedel-view--full-rerender)
        (should (markerp mevedel-view--in-flight-turn-start))
        (save-excursion
          (goto-char mevedel-view--in-flight-turn-start)
          (should (looking-at-p "Assistant"))
          (should (search-forward "Second response"
                                  mevedel-view--input-marker t)))
        (mevedel-view-test--insert-data data-buf "More text.\n" 'response)
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (= 2 (cl-count-if (lambda (line) (string= line "Assistant"))
                                    (split-string text "\n"))))
          (should (= 1 (cl-loop with start = 0
                                while (string-match "Second response" text start)
                                count t
                                do (setq start (match-end 0)))))))))

  :doc "turn fold preserves the in-flight assistant anchor"
  (mevedel-view-test--with-buffers
    (let (data-turn-start second-start)
      (mevedel-view-test--insert-data data-buf "*** First\n" nil)
      (mevedel-view-test--insert-data
       data-buf
       "First response line 1.\nFirst response line 2.\nFirst response line 3.\n"
       'response)
      (mevedel-view-test--insert-data data-buf "\n\n*** Second\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (mevedel-view-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (save-excursion
          (goto-char (point-min))
          (search-forward "Assistant")
          (search-forward "Assistant")
          (setq second-start (match-beginning 0)))
        (setq mevedel-view--data-turn-start data-turn-start)
        (mevedel-view-stream-set-in-flight-turn-start second-start)
        (save-excursion
          (goto-char (point-min))
          (search-forward "Assistant")
          (mevedel-view-toggle-section))
        (should (markerp mevedel-view--in-flight-turn-start))
        (mevedel-view-test--insert-data data-buf "More text.\n" 'response)
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (= 1 (cl-count-if (lambda (line) (string= line "Assistant"))
                                    (split-string text "\n"))))
          (should (= 1 (cl-loop with start = 0
                                while (string-match "Second response" text start)
                                count t
                                do (setq start (match-end 0)))))))))

  :doc "marks skipped leading compaction summary in rotated segment"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert "#+begin_summary mevedel-role=compaction-summary\n")
        (put-text-property start (point) 'gptel 'ignore))
      (insert "Summary should stay out of view.\n")
      (let ((start (point)))
        (insert "#+end_summary\n\n")
        (put-text-property start (point) 'gptel 'ignore)))
    (mevedel-view-test--insert-data data-buf "*** Actual prompt\n" nil)
    (mevedel-view-test--insert-data data-buf "Actual reply\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should-not (string-match-p "Summary should stay out of view" text))
        (should (= 1 (cl-loop with start = 0
                              while (string-match
                                     "conversation compacted"
                                     text start)
                              count t
                              do (setq start (match-end 0)))))
        (should (string-match-p "Actual prompt" text))
        (should (string-match-p "Actual reply" text)))))

  :doc "preserves in-flight live tail when data has no assistant replacement yet"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Read files\n" nil)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (start nil))
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (setq start (point))
        (insert "Assistant\n... Thinking... (1 lines)\nCalling Read...\n")
        (setq mevedel-view--in-flight-turn-start (copy-marker start nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Read files" text))
        (should (string-match-p "Assistant" text))
        (should (string-match-p "Calling Read" text)))))

  :doc "loads persisted session summaries once for a full transcript render"
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-view-summaries-" t)))
           (workspace
            (mevedel-workspace--create
             :type 'project :id "test" :root root :name "test"))
           (session
            (mevedel-session--create
             :name "source"
             :session-id "source-id"
             :save-path (file-name-concat root "sessions/source/")
             :workspace workspace
             :current-segment 1))
           (entries
            '((:save-path "/tmp/mevedel-view-sessions/source/"
               :summary
               (:session-id "source-id"
                :fork-point-ids ("fork-point-1" "fork-point-2")))))
           (calls 0))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (insert (propertize "First response.\n" 'gptel 'response))
        (insert
         (mevedel--format-hook-audit-record
          '(:type fork-point :fork-point-id "fork-point-1")))
        (insert "\nPrompt\n")
        (insert (propertize "Second response.\n" 'gptel 'response))
        (insert
         (mevedel--format-hook-audit-record
          '(:type fork-point :fork-point-id "fork-point-2"))))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (cl-letf
            (((symbol-function
               'mevedel-session-persistence-list-sessions)
              (lambda (_workspace &optional _cached)
                (cl-incf calls)
                entries)))
          (mevedel-view--full-rerender))
        (should (= 1 calls)))
      (delete-directory root t)))

  :doc "loads no session summaries for a transcript with no fork point"
  ;; The enumeration costs several target round trips per session in the
  ;; workspace, and nothing reads it unless a fork point exists to carry a
  ;; variant button.
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-view-summaries-" t)))
           (workspace
            (mevedel-workspace--create
             :type 'project :id "test" :root root :name "test"))
           (session
            (mevedel-session--create
             :name "source"
             :session-id "source-id"
             :save-path (file-name-concat root "sessions/source/")
             :workspace workspace
             :current-segment 1))
           (calls 0))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (insert "Prompt\n")
        (insert (propertize "First response.\n" 'gptel 'response)))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (cl-letf
            (((symbol-function
               'mevedel-session-persistence-list-sessions)
              (lambda (_workspace &optional _cached)
                (cl-incf calls)
                nil)))
          (mevedel-view--full-rerender))
        (should (= 0 calls)))
      (delete-directory root t))))


(mevedel-deftest mevedel-view--rebase-data-sources ()
  ,test
  (test)
  :doc "shifts rendered source ranges, collapse keys, and saved states"
  (with-temp-buffer
    (insert "rendered")
    (let* ((anchor '(tool "call-1"))
           (source (cons 10 20))
           (key (list 'source 'tool-summary 10 anchor)))
      (add-text-properties
       (point-min) (point-max)
       `(mevedel-view-source ,source
         mevedel-view-source-key ,key))
      (setq-local mevedel-view-disclosure--source-states
                  (make-hash-table :test #'equal))
      (puthash key t mevedel-view-disclosure--source-states)
      (let ((tick (buffer-chars-modified-tick)))
        (mevedel-view--rebase-data-sources 5)
        (should (= tick (buffer-chars-modified-tick))))
      (should (eq source
                  (get-text-property (point-min) 'mevedel-view-source)))
      (should (equal '(15 . 25) source))
      (should (eq key
                  (get-text-property
                   (point-min) 'mevedel-view-source-key)))
      (should (equal '(source tool-summary 15 (tool "call-1")) key))
      (should (gethash '(source tool-summary 15 (tool "call-1"))
                       mevedel-view-disclosure--source-states))))
  :doc "leaves marker coordinates to track data-buffer edits themselves"
  (let ((data (generate-new-buffer " *mevedel-rebase-data*")))
    (unwind-protect
        (with-temp-buffer
          (let ((start (with-current-buffer data (copy-marker (point-min))))
                (end (with-current-buffer data (copy-marker (point-max) t))))
            (insert "rendered")
            (put-text-property (point-min) (point-max)
                               'mevedel-view-source (cons start end))
            (mevedel-view--rebase-data-sources 5)
            (let ((source (get-text-property
                           (point-min) 'mevedel-view-source)))
              (should (eq start (car source)))
              (should (eq end (cdr source))))))
      (kill-buffer data))))


(mevedel-deftest mevedel-view--history-tail-position ()
  ,test
  (test)
  :doc "recovers the last history boundary by property runs"
  (with-temp-buffer
    (insert "Header\n")
    (let ((start (point)))
      (insert "Assistant answer\n")
      (put-text-property start (point) 'mevedel-view-type 'response))
    (let ((expected (point))
          (original
           (symbol-function 'mevedel-view--transcript-history-position-p))
          (calls 0))
      (insert (propertize "status\n" 'mevedel-view-type 'agent-status))
      (insert (make-string 100000 ?x))
      (cl-letf (((symbol-function
                  'mevedel-view--transcript-history-position-p)
                 (lambda (pos)
                   (cl-incf calls)
                   (funcall original pos))))
        (should (= expected (mevedel-view--history-tail-position)))
        (should (< calls 10))))))

(mevedel-deftest mevedel-view--full-rerender-live-tail ()
  ,test
  (test)
  :doc "does not append preserved live tail already rendered from data"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
    (mevedel-view-test--insert-data data-buf "Assistant answer.\n" 'response)
    (mevedel-view-test--insert-data data-buf "\n\n*** Follow-up\n" nil)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            start)
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (setq start (point))
        (insert "Assistant\nAssistant answer.\n")
        (setq mevedel-view--in-flight-turn-start (copy-marker start nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (mevedel-view--full-rerender)
      (mevedel-view--full-rerender)
      (let* ((text (buffer-substring-no-properties
                    (point-min) mevedel-view--input-marker))
             (assistant-count
              (cl-count-if (lambda (line) (string= line "Assistant"))
                           (split-string text "\n")))
             (answer-count
              (cl-loop with start = 0
                       while (string-match "Assistant answer" text start)
                       count t
                       do (setq start (match-end 0)))))
        (should (string-match-p "Follow-up" text))
        (should (= 1 assistant-count))
        (should (= 1 answer-count))
        (save-excursion
          (goto-char mevedel-view--in-flight-turn-start)
          (should (looking-at-p "Assistant"))))))

  :doc "matches preserved live tail by stable prefix when status lines differ"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
    (mevedel-view-test--insert-data
     data-buf
     "Assistant answer.\n(:name \"Read\" :args (:file_path \"a.el\"))\n\nok\n"
     'response)
    (mevedel-view-test--insert-data data-buf "\n\n*** Follow-up\n" nil)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            start)
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (setq start (point))
        (insert "Assistant\nAssistant answer.\n✓ Agent: reviewer done · 9.9s · 3 calls\n")
        (setq mevedel-view--in-flight-turn-start (copy-marker start nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (mevedel-view--full-rerender)
      (let* ((text (buffer-substring-no-properties
                    (point-min) mevedel-view--input-marker))
             (assistant-count
              (cl-count-if (lambda (line) (string= line "Assistant"))
                           (split-string text "\n")))
             (answer-count
              (cl-loop with start = 0
                       while (string-match "Assistant answer" text start)
                       count t
                       do (setq start (match-end 0)))))
        (should (string-match-p "Follow-up" text))
        (should (= 1 assistant-count))
        (should (= 1 answer-count))
        (should-not (string-match-p "reviewer done" text)))))

  :doc "task status rerender does not duplicate preserved live tail"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "view-task-rerender"
                       :root temporary-file-directory
                       :name "view-task-rerender"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "Inspect renderer" :status 'pending)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (mevedel-view-test--insert-data data-buf "Assistant answer.\n" 'response)
      (mevedel-view-test--insert-data data-buf "\n\n*** Follow-up\n" nil)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              start)
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (setq start (point))
          (insert "Assistant\nAssistant answer.\n")
          (setq mevedel-view--in-flight-turn-start (copy-marker start nil))
          (set-marker mevedel-view--status-marker (point))
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (set-marker-insertion-type mevedel-view--input-marker nil))
        (mevedel-view--full-rerender)
        (mevedel-view--full-rerender)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (assistant-count
                (cl-count-if (lambda (line) (string= line "Assistant"))
                             (split-string text "\n")))
               (task-count
                (cl-loop with start = 0
                         while (string-match "Inspect renderer" text start)
                         count t
                         do (setq start (match-end 0)))))
          (should (= 1 assistant-count))
          (should (= 1 task-count))))))

  :doc "full rerender recreates progress from request state"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker nil))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max) nil)))
      (mevedel-view--start-spinner "Thinking...")
      (mevedel-view--full-rerender)
      (should (mevedel-view--request-progress-visible-p))
      (mevedel-view--stop-spinner)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "Thinking" text)))))

  :doc "reanchors to current assistant when mailbox follows the in-flight turn"
  (mevedel-view-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        ;; Match the send-path marker shape: inside the user run, just
        ;; before the assistant response starts.
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (mevedel-view-test--insert-data data-buf "Assistant answer.\n" 'response)
      (mevedel-view-test--insert-data
       data-buf
       "\n<agent-message sender=\"/root/explorer\" recipient=\"/root\">\nhello\n</agent-message>\n"
       nil)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              start)
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (setq start (point))
          (insert "Assistant\nstale live tail\n")
          (setq mevedel-view--in-flight-turn-start (copy-marker start nil))
          (set-marker mevedel-view--status-marker (point))
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (set-marker-insertion-type mevedel-view--input-marker nil))
        (setq mevedel-view--data-turn-start data-turn-start)
        (mevedel-view--full-rerender)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (assistant-count
                (cl-count-if (lambda (line) (string= line "Assistant"))
                             (split-string text "\n"))))
          (should (string-match-p "Assistant answer" text))
          (should (string-match-p "hello" text))
          (should-not (string-match-p "stale live tail" text))
          (should (= 1 assistant-count))
          (save-excursion
            (goto-char mevedel-view--in-flight-turn-start)
            (should (looking-at-p "Assistant")))))))
)

(ert-deftest mevedel-view--full-rerender-in-flight-user-anchor/test ()
  "Full rerender during a new request keeps the in-flight anchor after `You'."
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** First\n" nil)
    (mevedel-view-test--insert-data data-buf "First response.\n" 'response)
    (mevedel-view-test--insert-data data-buf "\n\n*** Second\n" nil)
    (with-current-buffer data-buf
      (let ((data-turn-start (copy-marker (point-max) nil)))
        (with-current-buffer view-buf
          (setq mevedel-view--data-turn-start data-turn-start)
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker nil)))))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (should (string-match-p
               "Second"
               (buffer-substring-no-properties
                (point-min) mevedel-view--input-marker))))
    (mevedel-view-test--insert-data data-buf "Second response.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--render-incremental data-buf)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "First response" text))
        (should (string-match-p "Second" text))
        (should (string-match-p "Second response" text))))))


;;


;;
;;; Folding

(mevedel-deftest mevedel-view--render-request-summary-segment ()
  ,test
  (test)
  :doc "provider failures render expanded, retain the request ID, and fold"
  (mevedel-view-test--with-buffers
    (let ((provider-message "Processing failed."))
      (with-current-buffer data-buf
        (insert
         (mevedel-pipeline--format-render-data-block
          `(:kind request-summary
            :elapsed-seconds 7
            :outcome error
            :backend "Codex"
            :status "HTTP/2 200"
            :error-type "server_error"
            :error-code "server_error"
            :error-data (:type "server_error"
                         :code "server_error"
                         :message ,provider-message
                         :request_id "provider-request-123")
            :message ,provider-message
            :retry manual))))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (segment
               (list 'request-summary
                     1 (with-current-buffer data-buf (point-max)))))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--render-request-summary-segment
               segment data-buf)
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p
                   "Codex request failed · server_error" text))
          (should (string-match-p "HTTP/2 200" text))
          (should (string-match-p "provider-request-123" text))
          (should (string-match-p "Retry the request manually" text))
          (should (string-match-p "Worked for 7s" text)))
        (goto-char (point-min))
        (search-forward "Codex request failed")
        (goto-char (match-beginning 0))
        (should-not (get-text-property (point) 'mevedel-view-collapsed))
        (mevedel-view-toggle-section)
        (should-not (search-forward "provider-request-123"
                                    mevedel-view--input-marker t))
        (goto-char (point-min))
        (search-forward "Codex request failed")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (should (search-forward "provider-request-123"
                                mevedel-view--input-marker t)))))

  :doc "plain-string provider failures remain visible"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert
       (mevedel-pipeline--format-render-data-block
        '(:kind request-summary
          :elapsed-seconds 1
          :outcome error
          :backend "Provider"
          :status "Transport failure"
          :error-data "Raw provider failure"
          :message "Raw provider failure"
          :retry manual))))
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (segment
             (list 'request-summary
                   1 (with-current-buffer data-buf (point-max)))))
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (unwind-protect
            (mevedel-view--render-request-summary-segment segment data-buf)
          (set-marker-insertion-type mevedel-view--input-marker nil)))
      (goto-char (point-min))
      (should (search-forward "Raw provider failure"
                              mevedel-view--input-marker t)))))

(mevedel-deftest mevedel-view--render-incremental ()
  ,test
  (test)
  :doc "preserves an active selection while rebuilding the assistant turn"
  (mevedel-view-test--with-buffers
    (let (assistant-start)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (mevedel-view-test--insert-data
       data-buf "Response with selected text.\n" 'response)
      (with-current-buffer view-buf
        (setq-local transient-mark-mode t)
        (mevedel-view--full-rerender)
        (setq mevedel-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq mevedel-view--in-flight-turn-start
              (copy-marker (match-beginning 0) nil))
        (search-forward "selected text")
        (set-mark (match-beginning 0))
        (activate-mark))
      (mevedel-view-test--insert-data data-buf "Stream tail.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--render-incremental data-buf)
        (should mark-active)
        (should (equal "selected text"
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))))))))

(mevedel-deftest mevedel-view-collapse-state-survives-streaming ()
  ,test
  (test)
  :doc "expanded renderer-backed tool survives in-flight incremental render"
  (mevedel-view-test--with-buffers
    (let (assistant-start view-assistant-start)
      (mevedel-tool-register
       (mevedel-tool--create
        :name "StateTool"
        :category "mevedel"
        :renderer (lambda (_name _args result _data)
                    (list :header "StateTool: a.txt"
                          :body result
                          :body-mode 'text-mode
                          :initially-collapsed-p t))))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (mevedel-view-test--insert-data data-buf "Intro.\n" 'response)
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"StateTool\" :args (:path \"a.txt\"))\n\nexpanded tool body\n"
       '(tool . "call_state_tool"))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (setq mevedel-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq view-assistant-start (match-beginning 0))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker view-assistant-start nil))
        (search-forward "StateTool: a.txt")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (should (search-forward "expanded tool body"
                                mevedel-view--input-marker t)))
      (mevedel-view-test--insert-data data-buf "Stream tail.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "expanded tool body" text))
          (should (string-match-p "Stream tail" text))))))

  :doc "expanded short thinking survives in-flight incremental render"
  (mevedel-view-test--with-buffers
    (let (assistant-start view-assistant-start)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (mevedel-view-test--insert-data data-buf "short thought\n" 'ignore)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (setq mevedel-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq view-assistant-start (match-beginning 0))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker view-assistant-start nil))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (should (search-forward "short thought"
                                mevedel-view--input-marker t)))
      (mevedel-view-test--insert-data data-buf "more streamed thinking\n" 'ignore)
      (with-current-buffer view-buf
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "short thought" text))
          (should (string-match-p "more streamed thinking" text))
          (should (string-match-p "Thinking\\.\\.\\." text)))
        (when (markerp mevedel-view--data-turn-start)
          (set-marker mevedel-view--data-turn-start nil))
        (setq mevedel-view--data-turn-start nil)
        (when (markerp mevedel-view--in-flight-turn-start)
          (set-marker mevedel-view--in-flight-turn-start nil))
        (setq mevedel-view--in-flight-turn-start nil)
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "short thought" text))
          (should (string-match-p "more streamed thinking" text))
          (should (string-match-p "Thinking\\.\\.\\." text))))))

  :doc "expanded source-backed agent handle survives in-flight incremental render"
  (mevedel-view-test--with-buffers
    (let (assistant-start view-assistant-start)
      (mevedel-tool-register
       (mevedel-tool--create
        :name "StateAgent"
        :category "mevedel"
        :renderer (lambda (_name _args result _data)
                    (list :header "Agent: verifier -- check state"
                          :body result
                          :body-mode 'text-mode
                          :vtype 'agent-handle
                          :initially-collapsed-p t))))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"StateAgent\" :args (:task_name \"verify\"))\n\nagent body stays open\n"
       '(tool . "call_state_agent"))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (setq mevedel-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq view-assistant-start (match-beginning 0))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker view-assistant-start nil))
        (search-forward "Agent: verifier -- check state")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (should (search-forward "agent body stays open"
                                mevedel-view--input-marker t)))
      (mevedel-view-test--insert-data data-buf "Agent stream tail.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "agent body stays open" text))
          (should (string-match-p "Agent stream tail" text))))))

  :doc "expanded agent-result mailbox card survives in-flight incremental render"
  (let ((mevedel-view-mailbox-collapse-line-threshold 1))
    (mevedel-view-test--with-buffers
      (let (assistant-start view-assistant-start)
        (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
        (with-current-buffer data-buf
          (setq assistant-start (copy-marker (point) nil)))
        (mevedel-view-test--insert-data data-buf "Before result.\n" 'response)
        (mevedel-view-test--insert-data
         data-buf
         "\n<agent-result sender=\"/root/worker_state\" recipient=\"/root\">\nline one\nline two\n</agent-result>\n\n"
         nil)
        (with-current-buffer view-buf
          (mevedel-view--full-rerender)
          (setq mevedel-view--data-turn-start assistant-start)
          (goto-char (point-min))
          (search-forward "Assistant")
          (setq view-assistant-start (match-beginning 0))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker view-assistant-start nil))
          (search-forward "/root/worker_state")
          (goto-char (match-beginning 0))
          (mevedel-view-toggle-section)
          (goto-char (point-min))
          (search-forward "line two")
          (should-not (get-text-property (match-beginning 0) 'invisible)))
        (mevedel-view-test--insert-data data-buf "Result stream tail.\n" 'response)
        (with-current-buffer view-buf
          (mevedel-view--render-incremental data-buf)
          (goto-char (point-min))
          (search-forward "line two")
          (should-not (get-text-property (match-beginning 0) 'invisible))
          (should (search-forward "Result stream tail"
                                  mevedel-view--input-marker t))))))

  :doc "expanded task status survives streaming redraw and full rerender"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "task-state-stream"
                       :root temporary-file-directory
                       :name "task-state-stream"))
           (session (mevedel-session-create "main" workspace))
           assistant-start view-assistant-start)
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "active detail" :status 'pending)
                  (mevedel-task--create
                   :id 2 :subject "finished detail" :status 'completed)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--view-buffer view-buf))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (mevedel-view-test--insert-data data-buf "Task response.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "active detail")
        (mevedel-toggle-tasks)
        (should (search-forward "finished detail"
                                mevedel-view--input-marker t))
        (setq mevedel-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq view-assistant-start (match-beginning 0))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker view-assistant-start nil)))
      (mevedel-view-test--insert-data data-buf "Task stream tail.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "finished detail" text))
          (should (string-match-p "Task stream tail" text)))
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (should (search-forward "finished detail"
                                mevedel-view--input-marker t))))))



(mevedel-deftest mevedel-view-render-preserves-composer ()
  ,test
  (test)
  :doc "incremental redraw preserves a multiline leading-> composer draft"
  (mevedel-view-test--with-buffers
    (let ((draft "> quoted\nsecond line")
          data-turn-start)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (mevedel-view-test--insert-data data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4))
        (mevedel-view--render-incremental data-buf)
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (should-not (get-text-property (mevedel-view--input-start)
                                       'read-only)))))
  :doc "incremental redraw preserves composer point in every live window"
  (mevedel-view-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (mevedel-view-test--insert-data data-buf "Assistant text.\n" 'response)
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((first-window (selected-window))
            (second-window (split-window-right)))
        (unwind-protect
            (progn
              (set-window-buffer second-window view-buf)
              (with-current-buffer view-buf
                (setq mevedel-view--data-turn-start data-turn-start)
                (setq mevedel-view--in-flight-turn-start
                      (copy-marker mevedel-view--input-marker nil)))
              (with-selected-window first-window
                (goto-char (mevedel-view--input-start))
                (insert "draft")
                (goto-char (+ (mevedel-view--input-start) 2)))
              (with-selected-window second-window
                (goto-char (+ (mevedel-view--input-start) 3)))
              (with-selected-window first-window
                (mevedel-view--render-incremental data-buf)
                (should (= (window-point first-window)
                           (+ (mevedel-view--input-start) 2)))
                (should (= (window-point second-window)
                           (+ (mevedel-view--input-start) 3)))
                (should (string= "draft" (mevedel-view--input-text)))))
          (delete-other-windows)))))
  :doc "full redraw preserves a multiline leading-> composer draft"
  (mevedel-view-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (mevedel-view-test--insert-data data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4))
        (mevedel-view--full-rerender)
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (should-not (get-text-property (mevedel-view--input-start)
                                       'read-only))))))

(mevedel-deftest mevedel-view-render-navigation ()
  ,test
  (test)
  :doc "turn navigation crosses rendered source-backed turns"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** First\n" nil)
    (mevedel-view-test--insert-data data-buf "Answer one.\n" 'response)
    (mevedel-view-test--insert-data data-buf "\n*** Second\n" nil)
    (mevedel-view-test--insert-data data-buf "Answer two.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "You")
      (goto-char (match-beginning 0))
      (let ((first-source (get-text-property (point) 'mevedel-view-source)))
        (mevedel-view-next-turn)
        (should (get-text-property (point) 'mevedel-view-source))
        (should-not (equal first-source
                           (get-text-property (point) 'mevedel-view-source)))
        (mevedel-view-prev-turn)
        (should (equal first-source
                       (get-text-property (point) 'mevedel-view-source))))))
  :doc "transcript toggle selects the authoritative data buffer"
  (mevedel-view-test--with-buffers
    (switch-to-buffer view-buf)
    (mevedel-view-toggle-transcript)
    (should (eq (current-buffer) data-buf))))

(defun mevedel-view-render-test--owner (symbol)
  "Return the source feature basename that defines SYMBOL."
  (file-name-base (or (symbol-file symbol 'defun) "")))

(mevedel-deftest mevedel-view-render-ownership ()
  ,test
  (test)
  :doc "owns canonical turn grouping"
  (should (equal "mevedel-view-render"
                 (mevedel-view-render-test--owner
                  'mevedel-view--group-into-turns)))
  :doc "owns incremental and full transcript rendering"
  (dolist (symbol '(mevedel-view--render-incremental
                    mevedel-view--full-rerender))
    (should (equal "mevedel-view-render"
                   (mevedel-view-render-test--owner symbol))))
  :doc "delegates source-backed disclosure while retaining navigation"
  (dolist (symbol '(mevedel-view-toggle-section
                    mevedel-view-disclosure-section-bounds))
    (should (equal "mevedel-view-disclosure"
                   (mevedel-view-render-test--owner symbol))))
  (dolist (symbol '(mevedel-view-next-display
                    mevedel-view-next-turn))
    (should (equal "mevedel-view-render"
                   (mevedel-view-render-test--owner symbol)))))

(mevedel-deftest mevedel-view--fontify-response ()
  ,test
  (test)
  :doc "preserves Markdown response text instead of converting it to Org"
  (let ((text (mevedel-view--fontify-response
               "I’ll inspect `mevedel-review.el` now.\n\n```r\neval(f[[3]], df)\n```")))
    (should (string-match-p "mevedel-review\\.el" text))
    (should (string-match-p "```r" text))
    (should-not (string-match-p "#\\+begin_src" text)))

  :doc "preserves bracket indexing inside fenced code blocks"
  (let ((text (mevedel-view--fontify-response
               "```r\neval(f[[3]], df)\n```")))
    (should (string-match-p "```r" text))
    (should (string-match-p "eval(f\\[\\[3\\]\\], df)" text))
    (should-not (string-match-p "eval(f3, df)" text))
    (let ((pos (string-match "\\[\\[3\\]\\]" text)))
      (should pos)
      (should-not (get-text-property pos 'htmlize-link text))
      (should-not (get-text-property pos 'help-echo text))
      (should-not (get-text-property pos 'keymap text))
      (should-not (get-text-property pos 'mouse-face text))))

  :doc "removes source-block link properties after affiliated keywords"
  (let ((text (mevedel-view--fontify-response
               "#+NAME: rhs\n#+begin_src r\neval(f[[3]], df)\n#+end_src")))
    (should (string-match-p "#\\+NAME: rhs" text))
    (should (string-match-p "eval(f\\[\\[3\\]\\], df)" text))
    (let ((pos (string-match "\\[\\[3\\]\\]" text)))
      (should pos)
      (should-not (get-text-property pos 'htmlize-link text))
      (should-not (get-text-property pos 'help-echo text))
      (should-not (get-text-property pos 'keymap text))
      (should-not (get-text-property pos 'mouse-face text))))

  :doc "preserves bracket indexing inside inline code"
  (let ((text (mevedel-view--fontify-response
               "Use `f[[3]]` for the right-hand side.")))
    (should (string-match-p "f\\[\\[3\\]\\]" text))
    (should-not (string-match-p "f3" text)))

  :doc "still displays descriptive prose links"
  (let ((text (mevedel-view--fontify-response
               "See [site](https://example.com) and `items[[3]]`.")))
    (should (string-match-p "\\[site\\](https://example\\.com)" text))
    (should (string-match-p "items\\[\\[3\\]\\]" text)))

  :doc "caches repeated response fontification in view buffers"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((calls 0))
        (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
                   (lambda () 'mevedel-view-test-markdown-mode))
                  ((symbol-function 'mevedel-view-test-markdown-mode)
                   (lambda ()
                     (cl-incf calls)
                     (fundamental-mode))))
          (should (string-match-p "cached"
                                  (mevedel-view--fontify-response "cached")))
          (should (string-match-p "cached"
                                  (mevedel-view--fontify-response "cached")))
          (should (= 1 calls))))))

  :doc "suppresses arbitrary major-mode hooks in render temp buffers"
  (let* ((called nil)
         (hook (lambda ()
                 (setq called t))))
    (unwind-protect
        (progn
          (add-hook 'emacs-lisp-mode-hook hook)
          (mevedel-view--with-render-temp-buffer
            (emacs-lisp-mode))
          (should-not called))
      (remove-hook 'emacs-lisp-mode-hook hook))))


(mevedel-deftest mevedel-view--live-tail-lines-rendered-position ()
  ,test
  (test)

  :doc "finds live-tail lines separated by blank gaps"
  (with-temp-buffer
    (insert "before\nalpha\n\n   beta\nafter\n")
    (should (= 8 (mevedel-view--live-tail-lines-rendered-position
                  '("alpha" "beta") (point-max)))))

  :doc "does not build an overflowing regexp for long unmatched tails"
  (with-temp-buffer
    (dotimes (i 3000)
      (insert (format "line-%04d\n" i)))
    (let ((lines (mapcar (lambda (i) (format "missing-%04d" i))
                         (number-sequence 0 1500))))
      (should-not (mevedel-view--live-tail-lines-rendered-position
                   lines (point-max))))))

(mevedel-deftest mevedel-view--render-tool-group ()
  ,test
  (test)
  :doc "fallback one-liner buttonizes existing file paths"
  (let* ((root (make-temp-file "mevedel-view-fallback-linkify-" t))
         (file (file-name-concat root "mevedel-goal.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "fallback-linkify"
                     :root root :name "fallback-linkify"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (erase-buffer)
              (insert "(:name \"Edit\" :args (:file_path \"mevedel-goal.el\"))\n"
                      "Error: nope\n"))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                           (lambda (&rest _) nil)))
                  (mevedel-view--render-tool-group
                   (list (list 'tool 1 (with-current-buffer data-buf (point-max))))
                   data-buf)))
              (goto-char (point-min))
              (should (search-forward "! Edit: mevedel-goal.el (1 lines)" nil t))
              (goto-char (point-min))
              (search-forward "mevedel-goal.el")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'mevedel-view-path)))))))
      (delete-directory root t)))

  :doc "fallback one-liner leaves missing file paths plain"
  (let* ((root (make-temp-file "mevedel-view-fallback-missing-" t))
         (workspace (mevedel-workspace--create
                     :type 'project :id "fallback-missing"
                     :root root :name "fallback-missing"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (erase-buffer)
            (insert "(:name \"Edit\" :args (:file_path \"missing-file.el\"))\n"
                    "Error: nope\n"))
          (with-current-buffer view-buf
            (setq-local mevedel--session session)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                         (lambda (&rest _) nil)))
                (mevedel-view--render-tool-group
                 (list (list 'tool 1 (with-current-buffer data-buf (point-max))))
                 data-buf)))
            (goto-char (point-min))
            (should (search-forward "! Edit: missing-file.el (1 lines)" nil t))
            (goto-char (point-min))
            (search-forward "missing-file.el")
            (should-not (button-at (match-beginning 0)))))
      (delete-directory root t)))

  :doc "expanded saved state renders full body instead of collapsed cache"
  (mevedel-view-test--with-buffers
    (let (source)
      (mevedel-tool-register
       (mevedel-tool--create
        :name "CacheStateTool"
        :category "mevedel"
        :renderer (lambda (_name _args result _data)
                    (list :header "CacheStateTool: cached"
                          :body result
                          :body-mode 'text-mode
                          :initially-collapsed-p t))))
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"CacheStateTool\" :args (:path \"cached\"))\n\nbody must survive cache\n"
       '(tool . "call_cache_state"))
      (setq source (cons 1 (with-current-buffer data-buf (point-max))))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (mevedel-view--render-tool-group
           (list (list 'tool (car source) (cdr source))) data-buf))
        (goto-char (point-min))
        (search-forward "CacheStateTool: cached")
        (let ((header-pos (match-beginning 0)))
          (should-not (search-forward "body must survive cache"
                                      mevedel-view--input-marker t))
          (goto-char header-pos))
        (mevedel-view-toggle-section)
        (should (search-forward "body must survive cache"
                                mevedel-view--input-marker t))
        (let ((inhibit-read-only t))
          (delete-region (point-min) mevedel-view--input-marker)
          (goto-char mevedel-view--input-marker)
          (mevedel-view--render-tool-group
           (list (list 'tool (car source) (cdr source))) data-buf))
        (goto-char (point-min))
        (search-forward "CacheStateTool: cached")
        (should-not (get-text-property (match-beginning 0)
                                       'mevedel-view-collapsed))
        (should (search-forward "body must survive cache"
                                mevedel-view--input-marker t)))))

  :doc "adjacent visible rows with one key render only the final row and count"
  (mevedel-view-test--with-buffers
    (let (segments)
      (mevedel-tool-register
       (mevedel-tool--create
        :name "PollingTool"
        :category "mevedel"
        :renderer
        (lambda (_name args _result _data)
          (list :header (format "Polled %s" (plist-get args :value))
                :expandable-p nil
                :coalesce-key "polling-tool"))))
      (with-current-buffer data-buf
        (dolist (value '("first" "second"))
          (let ((start (point)))
            (insert (format
                     "(:name \"PollingTool\" :args (:value \"%s\"))\n\nok\n"
                     value))
            (put-text-property
             start (point) 'gptel
             (cons 'tool (format "call-%s" value)))
            (push (list 'tool start (point)) segments))))
      (setq segments (nreverse segments))
      (let ((data-before (with-current-buffer data-buf (buffer-string))))
        (with-current-buffer view-buf
          (let ((inhibit-read-only t))
            (goto-char mevedel-view--input-marker)
            (mevedel-view--render-tool-group segments data-buf))
          (let ((visible (buffer-substring-no-properties
                          (point-min) mevedel-view--input-marker)))
            (should-not (string-match-p "Polled first" visible))
            (should (string-match-p "Polled second ×2" visible))))
        (with-current-buffer data-buf
          (should (equal data-before (buffer-string)))))))

  :doc "retained-agent growth keeps adjacent collaboration disclosures exact"
  (mevedel-view-test--with-buffers
    (mevedel-tool-ui--register)
    (let* ((draft "> quoted\nsecond line")
           rerendered-p
           (invocation
            (mevedel-agent-invocation--create
             :agent-id "timer_patch_review--1"
             :path "/root/timer_patch_review"
             :parent-data-buffer data-buf
             :parent-tool-use-id "call-agent"
             :transcript-status 'running
             :activity
             '((:type tool-start :summary "Read a long profiler report")
               (:type tool-finish
                      :summary "Compared the adjacent interaction paths")))))
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft draft 4))
      (cl-labels
          ((insert-tool
            (id call result &optional render-data)
            (with-current-buffer data-buf
              (let ((start (point)))
                (insert call "\n\n" result)
                (when render-data
                  (insert (mevedel-pipeline--format-render-data-block
                           render-data id)))
                (put-text-property start (point) 'gptel (cons 'tool id))
                (list 'tool start (point)))))
           (render-tool
            (segment)
            (with-current-buffer view-buf
              (let ((inhibit-read-only t))
                (goto-char mevedel-view--input-marker)
                (set-marker-insertion-type mevedel-view--input-marker t)
                (unwind-protect
                    (mevedel-view--render-tool-group (list segment) data-buf)
                  (set-marker-insertion-type mevedel-view--input-marker nil))))))
        (render-tool
         (insert-tool
          "call-agent"
          "(:name \"Agent\" :args (:task_name \"timer_patch_review\" :message \"Review timer patch\"))"
          "{\"path\":\"/root/timer_patch_review\"}"
          '(:kind collaboration-event :event started
            :path "/root/timer_patch_review"
            :agent-id "timer_patch_review--1" :status running)))
        (render-tool
         (insert-tool
          "call-list"
          "(:name \"ListAgents\" :args nil)"
          "[{\"path\":\"/root/timer_patch_review\",\"role\":\"explorer\",\"activity\":\"running\"}]"))
        (render-tool
         (insert-tool
          "call-message"
          "(:name \"SendMessage\" :args (:target \"/root/timer_patch_review\" :message \"Detailed finding\"))"
          "Message queued"
          '(:kind collaboration-event :event interacted
            :path "/root/timer_patch_review"))))
      (with-current-buffer view-buf
        (goto-char (+ (mevedel-view--input-start) 4)))
      (require 'mevedel-transcript-restore)
      (cl-letf (((symbol-function 'mevedel-transcript-restore-properties)
                #'ignore)
                ((symbol-function 'mevedel-view-rerender)
                 (lambda (buffer)
                   (setq rerendered-p t)
                   (with-current-buffer buffer
                     (mevedel-view--full-rerender)))))
        (mevedel-agent-conversation-refresh invocation)
        (should rerendered-p)
        (let ((size (with-current-buffer data-buf (buffer-size))))
          (setf (mevedel-agent-invocation-transcript-status invocation)
                'blocked)
          (setq rerendered-p nil)
          (mevedel-agent-conversation-refresh invocation)
          (should rerendered-p)
          (should (= size (with-current-buffer data-buf (buffer-size))))))
      (with-current-buffer data-buf
        (should
         (equal '("Read a long profiler report"
                  "Compared the adjacent interaction paths")
                (mapcar
                 (lambda (item) (plist-get item :summary))
                 (plist-get
                  (mevedel-pipeline-tool-render-data data-buf "call-agent")
                  :activity)))))
      (with-current-buffer view-buf
        (should (equal draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (goto-char (point-min))
        (search-forward "ListAgents: session (1 agent)")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (should (search-forward "/root/timer_patch_review  explorer  running"
                                mevedel-view--input-marker t))
        (goto-char (point-min))
        (search-forward
         "SendMessage: /root/timer_patch_review (message queued)")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (should (search-forward "Detailed finding"
                                mevedel-view--input-marker t))
        (let ((visible (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker)))
          (should (= 1 (mevedel-view-test--count-substring
                        "Started /root/timer_patch_review" visible)))
          (should (= 1 (mevedel-view-test--count-substring
                        "ListAgents: session (1 agent)" visible)))
          (should (= 1 (mevedel-view-test--count-substring
                        "SendMessage: /root/timer_patch_review (message queued)"
                        visible)))))))

  :doc "an intervening visible tool ends the coalescing run"
  (mevedel-view-test--with-buffers
    (let (segments)
      (mevedel-tool-register
       (mevedel-tool--create
        :name "PollingTool"
        :category "mevedel"
        :renderer
        (lambda (_name args _result _data)
          (list :header (format "Polled %s" (plist-get args :value))
                :expandable-p nil
                :coalesce-key "polling-tool"))))
      (mevedel-tool-register
       (mevedel-tool--create
        :name "VisibleTool"
        :category "mevedel"
        :renderer
        (lambda (&rest _)
          '(:header "Visible separator" :expandable-p nil))))
      (with-current-buffer data-buf
        (dolist (spec '(("PollingTool" "first")
                        ("VisibleTool" "separator")
                        ("PollingTool" "second")))
          (let ((start (point)))
            (insert (format
                     "(:name \"%s\" :args (:value \"%s\"))\n\nok\n"
                     (car spec) (cadr spec)))
            (put-text-property
             start (point) 'gptel
             (cons 'tool (format "call-%s" (cadr spec))))
            (push (list 'tool start (point)) segments))))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (mevedel-view--render-tool-group
           (nreverse segments) data-buf))
        (let ((visible (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Polled first" visible))
          (should (string-match-p "Visible separator" visible))
          (should (string-match-p "Polled second" visible))
          (should-not (string-match-p "×2" visible)))))))

(mevedel-deftest mevedel-view--rendering-header-face
  (:doc "selects distinct faces for agent handle header states")
  ,test
  (test)

  :doc "running agent handles use the active running face"
  (should (eq 'mevedel-view-agent-running
              (mevedel-view--rendering-header-face
               '(:vtype agent-handle :agent-status running))))

  :doc "completed agent handles use the normal tool summary face"
  (should (eq 'mevedel-view-tool-summary
              (mevedel-view--rendering-header-face
               '(:vtype agent-handle :agent-status completed))))

  :doc "ordinary tool rows use the normal tool summary face"
  (should (eq 'mevedel-view-tool-summary
              (mevedel-view--rendering-header-face
               '(:vtype tool-summary)))))

(mevedel-deftest mevedel-view--rendering-header-line
  (:doc "styles renderer-provided tool headers like normal tool summaries")
  ,test
  (test)

  :doc "tool name and argument get distinct faces"
  (let ((line (mevedel-view--rendering-header-line
               '(:vtype tool-summary :header "ToolSearch: Eval"))))
    (should (string-match "ToolSearch" line))
    (should (eq 'mevedel-view-tool-name
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "Eval" line))
    (should (eq 'mevedel-view-tool-argument
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "trailing line counts get the metadata face"
  (let ((line (mevedel-view--rendering-header-line
               '(:vtype tool-summary
                 :header "Read: mevedel-tools.el (95 lines)"))))
    (should (string-match "Read" line))
    (should (eq 'mevedel-view-tool-name
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "mevedel-tools.el" line))
    (should (eq 'mevedel-view-tool-argument
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "(95 lines)" line))
    (should (eq 'mevedel-view-tool-metadata
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "trailing match counts get the metadata face"
  (let ((line (mevedel-view--rendering-header-line
               '(:vtype tool-summary
                 :header
                 "Grep: sanitize-gptel-bounds|GPTEL_BOUNDS (720 matches)"))))
    (should (string-match "Grep" line))
    (should (eq 'mevedel-view-tool-name
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "sanitize-gptel-bounds|GPTEL_BOUNDS" line))
    (should (eq 'mevedel-view-tool-argument
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "(720 matches)" line))
    (should (eq 'mevedel-view-tool-metadata
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "diff count metadata keeps added and removed counts distinct"
  (let ((line (mevedel-view--rendering-header-line
               '(:vtype tool-summary
                 :header "Edit: mevedel-view.el (+1 -0)"))))
    (should (string-match "Edit" line))
    (should (eq 'mevedel-view-tool-name
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "mevedel-view.el" line))
    (should (eq 'mevedel-view-tool-argument
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "(" line))
    (should (eq 'mevedel-view-tool-metadata
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "\\+1" line))
    (should (eq 'mevedel-view-tool-diff-added
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "-0" line))
    (should (eq 'mevedel-view-tool-diff-removed
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "incomplete agent handles do not use a success marker"
  (let ((line (mevedel-view--rendering-header-line
               '(:vtype agent-handle
                 :agent-status incomplete
                 :header "Agent: verifier"))))
    (should (string-match "…" line))
    (should (eq 'mevedel-view-tool-metadata
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "tool errors use a warning marker"
  (should (string-match-p
           "\\`  ! Bash:"
           (mevedel-view--rendering-header-line
            '(:header "Bash: npx test" :status error)))))

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
  :doc "accepts status and non-expandable marker"
  (should (mevedel-view--rendering-plist-p
           '(:header "h" :status error :expandable-p nil)))
  :doc "accepts a hidden rendering"
  (should (mevedel-view--rendering-plist-p
           '(:header "h" :hidden-p t)))
  :doc "accepts a string coalescing key"
  (should (mevedel-view--rendering-plist-p
           '(:header "h" :coalesce-key "poll:exec-1")))
  :doc "rejects missing :header"
  (should-not (mevedel-view--rendering-plist-p '(:body "b")))
  :doc "rejects non-string :header"
  (should-not (mevedel-view--rendering-plist-p '(:header 42)))
  :doc "rejects non-string :body"
  (should-not (mevedel-view--rendering-plist-p '(:header "h" :body 42)))
  :doc "rejects non-symbol :body-mode"
  (should-not (mevedel-view--rendering-plist-p
               '(:header "h" :body-mode "not-a-symbol")))
  :doc "rejects non-symbol :status"
  (should-not (mevedel-view--rendering-plist-p
               '(:header "h" :status "error")))
  :doc "rejects non-boolean :expandable-p"
  (should-not (mevedel-view--rendering-plist-p
               '(:header "h" :expandable-p maybe)))
  :doc "rejects non-boolean :hidden-p"
  (should-not (mevedel-view--rendering-plist-p
               '(:header "h" :hidden-p maybe)))
  :doc "rejects non-string :coalesce-key"
  (should-not (mevedel-view--rendering-plist-p
               '(:header "h" :coalesce-key poll))))


;;
;;; Renderer invocation

(mevedel-deftest mevedel-view--tool-render-status ()
  ,test
  (test)
  :doc "prefers structured error status over plain result text"
  (should (eq 'error
              (mevedel-view--tool-render-status
               "plain failure" '(:status error))))
  :doc "prefers structured success status over legacy error prose"
  (should (eq 'success
              (mevedel-view--tool-render-status
               "Error: visible text" '(:status success))))
  :doc "falls back to legacy result classification"
  (should (eq 'error
              (mevedel-view--tool-render-status "Error: legacy failure"))))

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
  :doc "structured status fills a custom renderer's omitted visual status"
  (let ((tool (mevedel-tool--create
               :name "StructuredVisual"
               :renderer (lambda (_name _args _result _data)
                           (list :header "x")))))
    (should (equal '(:header "x" :status error)
                   (mevedel-view--invoke-renderer
                    tool '(:status error) nil "plain failure"))))
  :doc "structured status overrides a conflicting custom visual status"
  (let ((tool (mevedel-tool--create
               :name "StructuredOverride"
               :renderer (lambda (_name _args _result _data)
                           (list :header "x" :status 'success)))))
    (should (equal '(:header "x" :status error)
                   (mevedel-view--invoke-renderer
                    tool '(:status error) nil "plain failure"))))
  :doc "legacy custom renderer status remains authoritative without structure"
  (let ((tool (mevedel-tool--create
               :name "LegacyVisual"
               :renderer (lambda (_name _args _result _data)
                           (list :header "x" :status 'error)))))
    (should (equal '(:header "x" :status error)
                   (mevedel-view--invoke-renderer tool nil nil "plain"))))

  :doc "data-driven renderers can opt out by returning nil when render-data is absent"
  (let ((tool (mevedel-tool--create
               :name "R2-data"
               :renderer (lambda (_name _args _result data)
                           (and data (list :header "only with data"))))))
    (should (null (mevedel-view--invoke-renderer tool nil nil "ok"))))
  :doc "renderer alist dispatches by success status"
  (let* ((success-fn (lambda (_name _args _result _data)
                       (list :header "success")))
         (error-fn (lambda (_name _args _result _data)
                     (list :header "error")))
         (tool (mevedel-tool--create
                :name "StatusDispatch"
                :renderer `((success . ,success-fn)
                            (error . ,error-fn)))))
    (should (equal '(:header "success")
                   (mevedel-view--invoke-renderer tool nil nil "ok"))))
  :doc "renderer alist dispatches by error status"
  (let* ((success-fn (lambda (_name _args _result _data)
                       (list :header "success")))
         (error-fn (lambda (_name _args _result _data)
                     (list :header "error")))
         (tool (mevedel-tool--create
                :name "StatusDispatchErr"
                :renderer `((success . ,success-fn)
                            (error . ,error-fn)))))
    (should (equal '(:header "error" :status error)
                   (mevedel-view--invoke-renderer
                    tool nil nil "Error: bad"))))
  :doc "renderer alist honors structured error status without failure prose"
  (let* ((success-fn (lambda (_name _args _result _data)
                       (list :header "success")))
         (error-fn (lambda (_name _args _result _data)
                     (list :header "error")))
         (tool (mevedel-tool--create
                :name "StructuredStatus"
                :renderer `((success . ,success-fn)
                            (error . ,error-fn)))))
    (should (equal '(:header "error" :status error)
                   (mevedel-view--invoke-renderer
                    tool '(:status error) nil "plain failure"))))
  :doc "renderer alist falls back to default status"
  (let* ((default-fn (lambda (_name _args _result _data)
                       (list :header "default")))
         (tool (mevedel-tool--create
                :name "StatusDefault"
                :renderer `((default . ,default-fn)))))
    (should (equal '(:header "default" :status error)
                   (mevedel-view--invoke-renderer
                    tool nil nil "Error: bad"))))
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
	                            (error "Oops"))))
         (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (should (null (mevedel-view--invoke-renderer
                     tool '(:kind diff) nil "ok")))
      (should warnings)
      (should (eq 'mevedel (caar warnings)))
      (should (string-match-p "failed" (cadar warnings))))))

(mevedel-deftest mevedel-view--tool-row-region
  (:doc "finds the source-backed row for one tool use")
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool (Custom :value \"x\")\n")
      (let ((start (point)))
        (insert "(:name \"Custom\" :args (:value \"x\"))\n\ninitial")
        (put-text-property start (point) 'gptel '(tool . "call-custom")))
      (insert "\n#+end_tool\n"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let* ((region (mevedel-view--tool-row-region
                      data-buf "call-custom"))
             (source (nth 2 region)))
        (should region)
        (should
         (equal source
                (with-current-buffer data-buf
                  (mevedel-pipeline--tool-segment-bounds "call-custom"))))
        (should
         (equal "call-custom"
                (get-text-property
                 (car region) 'mevedel-view-tool-use-id)))))))

(mevedel-deftest mevedel-view--refresh-tool-row
  (:doc "replaces one row while preserving collapse and composer state")
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool (Custom :value \"x\")\n")
      (let ((start (point)))
        (insert "(:name \"Custom\" :args (:value \"x\"))\n\ninitial")
        (put-text-property start (point) 'gptel '(tool . "call-custom")))
      (insert "\n#+end_tool\n"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (mevedel-view-test--insert-composer-draft "> quoted\nsecond line" 4)
      (let* ((region (mevedel-view--tool-row-region
                      data-buf "call-custom"))
             (collapsed
              (get-text-property (car region) 'mevedel-view-collapsed)))
        (with-current-buffer data-buf
          (goto-char (point-min))
          (search-forward "(:name")
          (search-forward ":value \"x\"")
          (replace-match ":value \"y\"" nil t))
        (should (mevedel-view--refresh-tool-row data-buf "call-custom"))
        (setq region
              (mevedel-view--tool-row-region data-buf "call-custom"))
        (should
         (eq collapsed
             (get-text-property (car region) 'mevedel-view-collapsed)))
        (should (equal "> quoted\nsecond line"
                       (mevedel-view--input-text)))
        (should
         (string-match-p
          "Custom: y"
          (buffer-substring-no-properties
           (point-min) (mevedel-view--input-start))))))))

(mevedel-deftest mevedel-view--segment-rendering/generic-fallback
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "renders registered tools without renderer through generic fallback"
  (progn
    (mevedel-tool-register
     (mevedel-tool--create
      :name "NoRenderer"
      :category "mevedel"
      :args '((path string :required "Path"))))
    (with-temp-buffer
      (insert "(:name \"NoRenderer\" :args (:path \"foo.el\"))\nline\n")
      (let ((rendering (mevedel-view--segment-rendering
                        (current-buffer) (point-min) (point-max))))
        (should (equal "NoRenderer: foo.el (1 line)"
                       (plist-get rendering :header)))
        (should (equal "line" (plist-get rendering :body))))))
  :doc "renders unregistered third-party-style tools through generic fallback"
  (with-temp-buffer
    (insert "(:name \"ThirdParty\" :args (:query \"thing\"))\nanswer\n")
    (let ((rendering (mevedel-view--segment-rendering
                      (current-buffer) (point-min) (point-max))))
      (should (equal "ThirdParty: thing (1 line)"
                     (plist-get rendering :header)))))
  :doc "malformed args in parseable tool calls fall back without signalling"
  (with-temp-buffer
    (insert "(:name \"ThirdParty\" :args \"not a plist\")\nanswer\n")
    (let ((rendering (mevedel-view--segment-rendering
                      (current-buffer) (point-min) (point-max))))
      (should (equal "ThirdParty (1 line)"
                     (plist-get rendering :header)))))
  :doc "renderer opt-out falls through to generic error rendering"
  (progn
    (mevedel-tool-register
     (mevedel-tool--create
      :name "Edit"
      :category "mevedel"
      :display-arg :file_path
      :renderer (lambda (_name _args _result _data) nil)))
    (with-temp-buffer
      (insert "(:name \"Edit\" :args (:file_path \"mevedel-goal.el\"))\n"
              "Error: Could not find old_string in file: x\n")
      (let ((rendering (mevedel-view--segment-rendering
                        (current-buffer) (point-min) (point-max))))
        (should (equal "Edit: mevedel-goal.el (error)"
                       (plist-get rendering :header)))
        (should (eq 'error (plist-get rendering :status)))
        (should (string-prefix-p "Error:" (plist-get rendering :body))))))
  :doc "generic rendering honors structured error status without failure prose"
  (with-temp-buffer
    (insert "(:name \"ThirdParty\" :args (:query \"thing\"))\nplain failure")
    (insert (mevedel-pipeline--format-render-data-block '(:status error)))
    (let ((rendering (mevedel-view--segment-rendering
                      (current-buffer) (point-min) (point-max))))
      (should (equal "ThirdParty: thing (error)"
                     (plist-get rendering :header)))
      (should (eq 'error (plist-get rendering :status)))
      (should (equal "plain failure" (plist-get rendering :body)))))
  :doc "custom Bash rendering marks permission failures without render data"
  (progn
    (mevedel-tool-register
     (mevedel-tool--create
      :name "Bash"
      :category "mevedel"
      :display-arg :command
      :renderer #'mevedel-tool-exec--render-bash))
    (with-temp-buffer
      (insert "(:name \"Bash\" :args (:command \"npx test\"))\n"
              "Error: Permission denied\n")
      (let ((rendering (mevedel-view--segment-rendering
                        (current-buffer) (point-min) (point-max))))
        (should (eq 'error (plist-get rendering :status))))))
  :doc "collapsed cached renderings omit bodies but expansion keeps them"
  (let ((mevedel-view--tool-rendering-cache (make-hash-table :test #'equal))
        (mevedel-view--render-cache-entries 0))
    (with-temp-buffer
      (insert "(:name \"ThirdParty\" :args (:query \"thing\"))\nlarge body\n")
      (let ((collapsed (mevedel-view--segment-rendering
                        (current-buffer) (point-min) (point-max) t))
            (expanded (mevedel-view--segment-rendering
                       (current-buffer) (point-min) (point-max))))
        (should (equal "ThirdParty: thing (1 line)"
                       (plist-get collapsed :header)))
        (should-not (plist-get collapsed :body))
        (should (equal "large body" (plist-get expanded :body))))))
  :doc "unrelated appends keep completed tool renderings cached"
  (let ((mevedel-view--tool-rendering-cache (make-hash-table :test #'equal))
        (mevedel-view--render-cache-entries 0)
        (calls 0))
    (mevedel-tool-register
     (mevedel-tool--create
      :name "CacheTool"
      :category "mevedel"
      :renderer (lambda (_name _args result _data)
                  (cl-incf calls)
                  (list :header (format "CacheTool: %s" result)
                        :body result
                        :initially-collapsed-p t))))
    (with-temp-buffer
      (insert "(:name \"CacheTool\" :args (:query \"thing\"))\none\n")
      (let* ((seg-start (point-min))
             (seg-end (point-max))
             (first (mevedel-view--segment-rendering
                     (current-buffer) seg-start seg-end t)))
        (goto-char (point-max))
        (insert "unrelated streaming text\n")
        (let ((second (mevedel-view--segment-rendering
                       (current-buffer) seg-start seg-end t)))
          (should (equal "CacheTool: one" (plist-get first :header)))
          (should (equal "CacheTool: one" (plist-get second :header)))
          (should (= 1 calls))))))
  :doc "tool text changes invalidate cached renderings"
  (let ((mevedel-view--tool-rendering-cache (make-hash-table :test #'equal))
        (mevedel-view--render-cache-entries 0)
        (calls 0))
    (mevedel-tool-register
     (mevedel-tool--create
      :name "CacheTool"
      :category "mevedel"
      :renderer (lambda (_name _args result _data)
                  (cl-incf calls)
                  (list :header (format "CacheTool: %s" result)
                        :body result
                        :initially-collapsed-p t))))
    (with-temp-buffer
      (insert "(:name \"CacheTool\" :args (:query \"thing\"))\none\n")
      (let ((seg-start (point-min))
            (seg-end (point-max)))
        (let ((first (mevedel-view--segment-rendering
                      (current-buffer) seg-start seg-end t)))
          (goto-char (point-min))
          (search-forward "one")
          (replace-match "two" nil t)
          (let ((second (mevedel-view--segment-rendering
                         (current-buffer) seg-start seg-end t)))
            (should (equal "CacheTool: one" (plist-get first :header)))
            (should (equal "CacheTool: two" (plist-get second :header)))
            (should (= 2 calls)))))))
  :doc "malformed tool text still returns nil"
  (with-temp-buffer
    (insert "not a tool")
    (should-not (mevedel-view--segment-rendering
                 (current-buffer) (point-min) (point-max)))))

(mevedel-deftest mevedel-view--sandbox-summary-line
  ()
  ,test
  (test)
  :doc "omits the default sandbox boundary"
  (should-not
   (mevedel-view--sandbox-summary-line
    '(:attempt-count 1 :started-count 1 :refused-count 0
      :sandbox bubblewrap :filesystem workspace-write
      :network isolated :proc fresh
      :additional-read-count 0 :additional-write-count 0)))
  :doc "omits additional read-only access"
  (should-not
   (mevedel-view--sandbox-summary-line
    '(:attempt-count 2 :started-count 2 :refused-count 0
      :sandbox bubblewrap :filesystem workspace-write
      :network isolated :proc fresh
      :additional-read-count 6 :additional-write-count 0)))
  :doc "describes material access in plain language"
  (dolist
      (case
       '(((:sandbox bubblewrap :filesystem workspace-write
           :network isolated :proc fresh
           :additional-read-count 0 :additional-write-count 1)
          . "additional filesystem write access")
         ((:sandbox bubblewrap :filesystem workspace-write
           :network unrestricted :proc fresh
           :additional-read-count 0 :additional-write-count 0)
          . "network access allowed")
         ((:sandbox escalated :filesystem unrestricted
           :network unrestricted :proc host
           :additional-read-count 0 :additional-write-count 0)
          . "full execution access")
         ((:sandbox unavailable :filesystem unrestricted
           :network unrestricted :proc host
           :additional-read-count 0 :additional-write-count 0)
          . "sandbox unavailable · ran without confinement")))
    (let ((line
           (mevedel-view--sandbox-summary-line
            (append
             '(:attempt-count 1 :started-count 1 :refused-count 0)
             (car case)))))
      (should (string-match-p "! Sandbox:" line))
      (should (string-match-p (regexp-quote (cdr case)) line))))
  :doc "keeps material access visible beside a partial refusal"
  (let ((line
         (mevedel-view--sandbox-summary-line
          '(:attempt-count 2 :started-count 1 :refused-count 1
            :sandbox refused :filesystem unrestricted
            :network unrestricted :proc host
            :additional-read-count 0 :additional-write-count 1))))
    (dolist (detail '("execution refused"
                      "unrestricted filesystem access"
                      "network access allowed"
                      "host /proc access"
                      "additional filesystem write access"))
      (should (string-match-p (regexp-quote detail) line))))
  :doc "shows a total refusal as a warning without a raw reason"
  (let ((line
         (mevedel-view--sandbox-summary-line
          '(:attempt-count 1 :started-count 0 :refused-count 1
            :sandbox refused :filesystem unavailable
            :network unavailable :proc nil
            :additional-read-count 0 :additional-write-count 0))))
    (should (string-match-p "! Sandbox:" line))
    (should
     (string-match-p "execution refused · no child started" line)))
  :doc "shows a queued child that never started without nil policy fields"
  (let ((line
         (mevedel-view--sandbox-summary-line
          '(:attempt-count 1 :started-count 0 :refused-count 0
            :additional-read-count 0 :additional-write-count 0))))
    (should (string-match-p "1 child did not start" line))
    (should-not (string-match-p "nil" line))))

(mevedel-deftest mevedel-view--rendering-header-block
  (:doc "hides persisted read-only sandbox metadata")
  (let ((block
         (mevedel-view--rendering-header-block
          '(:header "Read: file.pdf"
            :sandbox-summary
            (:attempt-count 1 :started-count 1 :refused-count 0
             :sandbox bubblewrap :filesystem workspace-write
             :network isolated :proc fresh
             :additional-read-count 1 :additional-write-count 0)))))
    (should (equal "  ✓ Read: file.pdf" block))))

(mevedel-deftest mevedel-view--render-expanded-body ()
  ,test
  (test)
  :doc "adds a display-only body inset inherited by source panels"
  (with-temp-buffer
    (mevedel-view-mode)
    (let ((inhibit-read-only t))
      (mevedel-view--render-expanded-body
       '(:header "Read: file.el"
         :body "body text\n```elisp\n(+ 1 2)\n```\n"
         :body-mode markdown-mode
         :sandbox-summary
         (:attempt-count 1 :started-count 1 :refused-count 0
          :sandbox bubblewrap :filesystem workspace-write
          :network isolated :proc fresh
          :additional-read-count 0 :additional-write-count 1))
       (cons 1 10)))
    (goto-char (point-min))
    (should (looking-at-p "  ✓ Read: file\\.el"))
    (search-forward "Sandbox:")
    (should-not (get-text-property (match-beginning 0) 'line-prefix))
    (search-forward "body text")
    (let ((body-start (match-beginning 0)))
      (should (equal "    " (get-text-property body-start 'line-prefix)))
      (should (equal "    " (get-text-property body-start 'wrap-prefix)))
      (should (equal '(line-prefix wrap-prefix)
                     (get-text-property body-start 'rear-nonsticky)))
      (goto-char body-start)
      (should (looking-at-p "body text")))
    (search-forward "elisp ⧉")
    (should (equal "    "
                   (get-text-property (match-beginning 0) 'line-prefix)))
    (search-forward "(+ 1 2)")
    (should (equal "    "
                   (get-text-property (match-beginning 0) 'line-prefix)))))

(mevedel-deftest mevedel-view--insert-rendered-tool/non-expandable ()
  ,test
  (test)
  :doc "non-expandable renderings do not carry source state"
  (with-temp-buffer
    (mevedel-view-mode)
    (let ((rendering
           '(:header "TaskCreate: Created 1 task"
             :expandable-p nil
             :sandbox-summary
             (:attempt-count 1 :started-count 0 :refused-count 1
              :sandbox refused :filesystem unavailable
              :network unavailable :proc nil
              :additional-read-count 0 :additional-write-count 0)))
          (inhibit-read-only t))
      (mevedel-view--insert-rendered-tool rendering (cons 1 10))
      (goto-char (point-min))
      (should (eq 'tool-event
                  (get-text-property (point) 'mevedel-view-type)))
      (should (string-match-p
               "Sandbox:.*execution refused · no child started"
               (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (get-text-property (point) 'mevedel-view-source))
      (let ((before (buffer-string)))
        (should-error (mevedel-view-toggle-section))
        (should (equal before (buffer-string)))))))

(mevedel-deftest mevedel-view--format-hook-context-audit ()
  ,test
  (test)
  :doc "formats one event with ordered handler attribution"
  (let* ((record '(:event "SubagentStart"
                   :handlers
                   ((:function ponytail-subagent
                     :source plugin
                     :plugin-name "ponytail"
                     :reason "PONYTAIL:FULL")
                    (:description "Inject project conventions"
                     :source project-file))))
         (collapsed (mevedel-view--format-hook-context-audit record nil))
         (expanded (mevedel-view--format-hook-context-audit record t)))
    (should (equal "  ◇ SubagentStart hook added context · 2 handlers\n"
                   collapsed))
    (should (< (string-match "1\\. ponytail plugin" expanded)
               (string-match "2\\. project hook" expanded)))
    (should (string-match-p "Handler: ponytail-subagent" expanded))
    (should (string-match-p "Reason: PONYTAIL:FULL" expanded))
    (should (string-match-p "Handler: Inject project conventions" expanded))))

(mevedel-deftest mevedel-view--insert-rendered-tool/hook-audits ()
  ,test
  (test)
  :doc "renders updated tool input and result audit details"
  (with-temp-buffer
    (mevedel-view-mode)
    (let ((rendering
           '(:header "Read: /tmp/file (1 line)"
                     :body "updated result"
                     :initially-collapsed-p t
                     :hook-audits
                     ((:type tool-input-rewrite
                             :event "PreToolUse"
                             :original-input (:file_path "/tmp/old")
                             :updated-input (:file_path "/tmp/new")
                             :reason "normalized")
                      (:type tool-result-rewrite
                             :event "PostToolUse"
                             :original-result "original result"
                             :updated-result "updated result"
                             :reason "redacted"))))
          (inhibit-read-only t))
      (mevedel-view--insert-rendered-tool rendering (cons 1 40))
      (let ((text (buffer-string)))
        (should (string-match-p "hook changed tool input" text))
        (should (string-match-p "hook changed tool result" text))
        (should-not (string-match-p "original result" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool input")
      (mevedel-view-toggle-section)
      (let ((text (buffer-string)))
        (should (string-match-p "Original input" text))
        (should (string-match-p ":file_path \"/tmp/old\"" text))
        (should (string-match-p "Updated input" text))
        (should (string-match-p ":file_path \"/tmp/new\"" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool result")
      (mevedel-view-toggle-section)
      (let ((text (buffer-string)))
        (should (string-match-p "Original result" text))
        (should (string-match-p "original result" text))
        (should (string-match-p "Updated result" text))
        (should (string-match-p "updated result" text)))))

  :doc "renders committed and abandoned repair audits with value-free details"
  (with-temp-buffer
    (mevedel-view-mode)
    (let ((rendering
           '(:header "Collect: completed"
                     :body "ordinary result"
                     :initially-collapsed-p t
                     :hook-audits
                     ((:type tool-input-repair :state committed
                             :repairs
                             ((:rule wrap-array-singleton :source generic
                                     :paths ((names))
                                     :before string :after array)))
                      (:type tool-input-repair :state abandoned
                             :repairs
                             ((:rule parse-json-value :source generic
                                     :paths ((count))
                                     :before string :after integer))))))
          (inhibit-read-only t))
      (mevedel-view--insert-rendered-tool rendering (cons 1 40))
      (let ((text (buffer-string)))
        (should (string-match-p "tool input repaired" text))
        (should (string-match-p "tool input repair abandoned" text))
        (should-not (string-match-p "Rule:" text)))
      (goto-char (point-min))
      (search-forward "tool input repaired")
      (mevedel-view-toggle-section)
      (let ((text (buffer-string)))
        (should (string-match-p "Rule: wrap-array-singleton" text))
        (should (string-match-p "Path: names" text))
        (should (string-match-p "Shape: string -> array" text))
        (should-not (string-match-p "alice\|ordinary result" text)))))

  :doc "malformed repair audit metadata falls back without exposing values"
  (with-temp-buffer
    (mevedel-view-mode)
    (let ((rendering
           '(:header "Collect: completed"
                     :body "ordinary result"
                     :initially-collapsed-p t
                     :hook-audits
                     ((:type tool-input-repair :state committed
                             :repairs
                             ((:rule "sentinel-secret" :source generic
                                     :paths ((/private/secret))
                                     :before "raw-value" :after array))))))
          (inhibit-read-only t))
      (mevedel-view--insert-rendered-tool rendering (cons 1 40))
      (let ((text (buffer-string)))
        (should (string-match-p "repair audit unavailable" text))
        (should-not (string-match-p "sentinel\|private\|raw-value" text)))
      (goto-char (point-min))
      (search-forward "repair audit unavailable")
      (mevedel-view-toggle-section)
      (should-not
       (string-match-p "sentinel\|private\|raw-value" (buffer-string)))))

  :doc "repair audit normalization errors use the unavailable fallback"
  (with-temp-buffer
    (mevedel-view-mode)
    (let ((rendering
           '(:header "Collect: completed"
                     :body "ordinary result"
                     :initially-collapsed-p t
                     :hook-audits
                     ((:type tool-input-repair :state committed
                             :repairs
                             ((:rule wrap-array-singleton :source generic
                                     :paths ((names))
                                     :before string :after array))))))
          (inhibit-read-only t))
      (cl-letf (((symbol-function
                  'mevedel-tool-repair-normalize-audit-record)
                 (lambda (&rest _) (error "private audit sentinel"))))
        (mevedel-view--insert-rendered-tool rendering (cons 1 40)))
      (should (string-match-p "repair audit unavailable" (buffer-string)))
      (should-not (string-match-p "private\|sentinel" (buffer-string)))))

  :doc "ordinary valid tools retain their undecorated compact rendering"
  (with-temp-buffer
    (mevedel-view-mode)
    (let ((inhibit-read-only t))
      (mevedel-view--insert-rendered-tool
       '(:header "Read: file.el" :body "ordinary result"
                 :initially-collapsed-p t)
       (cons 1 40))
      (should-not (string-match-p "repair" (buffer-string))))))

(mevedel-deftest mevedel-view--repair-audit-reconstruction
  ()
  ,test
  (test)

  :doc "reconstructs a committed audit beside a persisted handler error"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat
      "(:name \"Collect\" :args (:names [\"alice\"]))\n\n"
      "Error: handler exploded"
      (mevedel-tool-repair-format-audit-block
       'committed
       '((:rule wrap-array-singleton :source generic
               :paths ((names)) :before string :after array)))
      "\n")
     '(tool . "repair-error"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Collect" text))
        (should (string-match-p "tool input repaired" text))
        (should-not (string-match-p "alice\|handler exploded" text)))
      (goto-char (point-min))
      (search-forward "tool input repaired")
      (mevedel-view-toggle-section)
      (should (string-match-p "Rule: wrap-array-singleton"
                              (buffer-substring-no-properties
                               (point-min) mevedel-view--input-marker))))))


;;
;;; Tool-call parsing with render-data

(mevedel-deftest mevedel-view--compute-segment-rendering/cold-load ()
  ,test
  (test)
  :doc "loads pending execution data through its transcript owner"
  (let ((root
         (file-name-as-directory
          (file-name-directory (locate-library "mevedel"))))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (should
       (= 0
          (call-process
           emacs nil t nil "--batch" "-Q" "-L" root
           "--eval"
           (prin1-to-string
            '(progn
               (require 'cl-lib)
               (require 'mevedel-view-render)
               (with-temp-buffer
                 (cl-letf
                     (((symbol-function 'mevedel-view--tool-call-parse)
                       (lambda (&rest _)
                         '(:name "Bash" :args nil :tool-use-id "cold"
                                 :result "" :render-data nil)))
                      ((symbol-function 'mevedel-tool-get) #'ignore)
                      ((symbol-function
                        'mevedel-view--generic-tool-rendering)
                       (lambda (&rest _) '(:vtype tool))))
                   (mevedel-view--compute-segment-rendering
                    (current-buffer) (point-min) (point-max))))
               (unless (featurep 'mevedel-execution-transcript)
                 (error "Execution transcript owner was not loaded")))))))
      (should (string-empty-p (string-trim (buffer-string)))))))

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
        (should (equal "call_1" (plist-get call :tool-use-id)))
        (should (equal '(:file_path "/tmp/f") (plist-get call :args)))
        (should (string-match-p "plain result" (plist-get call :result)))
        (should (null (plist-get call :render-data))))))
  :doc "keeps the leading indentation of the first result line"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\n  1\tfirst\n  2\tsecond\n"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (mevedel-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "  1\tfirst\n  2\tsecond"
                       (plist-get call :result))))))
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
             (segs (mevedel-transcript-segments mid-start mid-end))
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
                          render-data "call_1"))))
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
  :doc "keeps numbered render markers in Read output as literal text"
  (mevedel-view-test--with-buffers
    (let ((literal
           (concat "158 " mevedel-pipeline--render-data-open
                   "\n159 (:kind user-display :text \"literal\")"
                   "\n160 " mevedel-pipeline--render-data-close)))
      (mevedel-view-test--insert-data
       data-buf
       (concat "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\n"
               literal "\n")
       '(tool . "call_1"))
      (with-current-buffer data-buf
        (let ((call (mevedel-view--tool-call-parse
                     data-buf (point-min) (point-max))))
          (should (string-match-p (regexp-quote literal)
                                  (plist-get call :result)))
          (should-not (plist-get call :render-data))
          (should (mevedel-view--compute-segment-rendering
                   data-buf (point-min) (point-max)))))))
  :doc "preserves literal trailing end-tool marker in unwrapped result"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nbody\n#+end_tool"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (mevedel-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "Read" (plist-get call :name)))
        (should (equal "body\n#+end_tool" (plist-get call :result))))))
  :doc "recovers render-data when restored bounds start inside the tool marker"
  (mevedel-view-test--with-buffers
    (let* ((render-data '(:kind diff :patch "--- a\n+++ b\n+hi\n"
                          :path "/tmp/f" :rel-path "f"))
           (block
            (concat "#+begin_tool (RecoverEdit :file_path \"/tmp/f\")\n"
                    "(:name \"RecoverEdit\" :args (:file_path \"/tmp/f\"))\n\n"
                    "visible body"
                    (mevedel-pipeline--format-render-data-block
                     render-data "call_1")
                    "#+end_tool\n")))
      (mevedel-view-test--insert-data data-buf block '(tool . "call_1"))
      (with-current-buffer data-buf
        (let (seg-start seg-end)
          (goto-char (point-min))
          (search-forward ":file_path")
          (setq seg-start (match-beginning 0))
          (search-forward "visible body")
          (setq seg-end (match-end 0))
          (let ((call (mevedel-view--tool-call-parse
                       data-buf seg-start seg-end)))
            (should (equal "RecoverEdit" (plist-get call :name)))
            (should (equal "visible body" (plist-get call :result)))
            (should (equal render-data (plist-get call :render-data))))
          (mevedel-tool-register
           (mevedel-tool--create
            :name "RecoverEdit"
            :category "mevedel"
            :renderer
            (lambda (_name _args _result data)
              (and (eq (plist-get data :kind) 'diff)
                   (list :header "Recovered diff"
                         :body (plist-get data :patch))))))
          (should (equal "Recovered diff"
                         (plist-get
                          (mevedel-view--segment-rendering
                           data-buf seg-start seg-end)
                          :header)))))))
  :doc "recovers full tool body when result contains marker-looking lines"
  (mevedel-view-test--with-buffers
    (let ((block (concat "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "before\n"
                "#+end_tool\n"
                         "After\n"
                         "#+end_tool\n")))
      (mevedel-view-test--insert-data data-buf block '(tool . "call_1"))
      (with-current-buffer data-buf
        (let (seg-start seg-end)
          (goto-char (point-min))
          (search-forward ":file_path")
          (setq seg-start (match-beginning 0))
          (search-forward "After")
          (setq seg-end (match-end 0))
          (let ((call (mevedel-view--tool-call-parse
                       data-buf seg-start seg-end)))
            (should (equal "RecoverRead" (plist-get call :name)))
            (should (equal "before\n#+end_tool\nAfter"
                           (plist-get call :result))))))))
  :doc "recovers full tool body when stale bounds stop before a literal close"
  (mevedel-view-test--with-buffers
    (let ((block (concat "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                         "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                         "before\n"
                         "#+end_tool\n"
                         "after\n"
                         "#+end_tool\n")))
      (mevedel-view-test--insert-data data-buf block '(tool . "call_1"))
      (with-current-buffer data-buf
        (let (seg-start seg-end)
          (goto-char (point-min))
          (search-forward ":file_path")
          (setq seg-start (match-beginning 0))
          (search-forward "before")
          (setq seg-end (match-end 0))
          (let ((call (mevedel-view--tool-call-parse
                       data-buf seg-start seg-end)))
            (should (equal "RecoverRead" (plist-get call :name)))
            (should (equal "before\n#+end_tool\nafter"
                           (plist-get call :result))))))))
  :doc "keeps persisted-looking tool text inside unwrapped tool results"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\n"
             "outer before\n"
             "#+begin_tool (Bash :command \"echo nested\")\n"
             "(:name \"Bash\" :args (:command \"echo nested\"))\n"
             "nested result\n"
             "#+end_tool\n"
             "outer after\n")
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (mevedel-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "Read" (plist-get call :name)))
        (should (string-match-p "outer before" (plist-get call :result)))
        (should (string-match-p
                 "#\\+begin_tool (Bash :command \"echo nested\")"
                 (plist-get call :result)))
        (should (string-match-p "outer after" (plist-get call :result))))))
  :doc "recovers full tool body when result contains persisted-looking tool text"
  (mevedel-view-test--with-buffers
    (let ((block (concat "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                         "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                         "outer before\n"
                         "#+begin_tool (Bash :command \"echo nested\")\n"
                         "(:name \"Bash\" :args (:command \"echo nested\"))\n"
                         "nested result\n"
                         "#+end_tool\n"
                         "outer after\n"
                         "#+end_tool\n")))
      (mevedel-view-test--insert-data data-buf block '(tool . "call_1"))
      (with-current-buffer data-buf
        (let (seg-start seg-end)
          (goto-char (point-min))
          (search-forward ":file_path")
          (setq seg-start (match-beginning 0))
          (search-forward "outer after")
          (setq seg-end (match-end 0))
          (let ((call (mevedel-view--tool-call-parse
                       data-buf seg-start seg-end)))
            (should (equal "RecoverRead" (plist-get call :name)))
            (should (string-match-p
                     "#\\+begin_tool (Bash :command \"echo nested\")"
                     (plist-get call :result)))
            (should (string-match-p
                     "outer after"
                     (plist-get call :result))))))))
  :doc "does not parse a previous quoted tool block as a later tool call"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (real-start real-end)
        (insert "#+begin_tool (Read :file_path \"/quoted\")\n"
                "(:name \"Read\" :args (:file_path \"/quoted\"))\n"
                "quoted body\n"
                "#+end_tool\n\n"
                "Normal assistant text before real tool.\n")
        (put-text-property (save-excursion
                             (goto-char (point-min))
                             (search-forward "Normal")
                             (match-beginning 0))
                           (point)
                           'gptel 'response)
        (setq real-start (point))
        (insert "#+begin_tool (Bash :command \"echo real\")\n"
                "(:name \"Bash\" :args (:command \"echo real\"))\n"
                "real output\n"
                "#+end_tool\n")
        (setq real-end (point))
        (put-text-property (+ real-start 20) (- real-end 12)
                           'gptel '(tool . "call_real"))
        (let ((call (mevedel-view--tool-call-parse
                     data-buf (+ real-start 20) (- real-end 12))))
          (should (equal "Bash" (plist-get call :name)))
          (should (equal "real output" (plist-get call :result)))))))
  :doc "unescapes org-mode tool result storage markers"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"WebFetch\" :args (:url \"https://example.com\"))\n\n,* Heading\n,,* Literal comma-star\n,#+begin_src text\nbody\n,#+end_src\n"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (mevedel-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "* Heading\n,* Literal comma-star\n#+begin_src text\nbody\n#+end_src"
                       (plist-get call :result))))))
  :doc "returns nil on unreadable segments"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf "(:unclosed\n" '(tool . "call_1"))
    (with-current-buffer data-buf
      (should (null (mevedel-view--tool-call-parse
                     data-buf (point-min) (point-max))))))

  :doc "splits compacted malformed tools before the next real tool block"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (second-start)
        (insert "#+begin_tool (Edit :file_path \"mevedel-chat.el\" :old_string \"...\")\n")
        (let ((bad-start (point)))
          (insert "(:name \"Edit\" :args (:file_path \"mevedel-chat.el\" :old_string \"unterminated\n")
          (insert "[mevedel: tool output truncated; omitted 8858 chars]\n")
          (put-text-property bad-start (point) 'gptel '(tool . "bad-edit")))
        (setq second-start (point))
        (insert "#+begin_tool (Read :file_path \"next.el\")\n"
                "(:name \"Read\" :args (:file_path \"next.el\"))\n\n"
                "body\n#+end_tool\n")
        (put-text-property second-start (point) 'gptel '(tool . "read"))
        (let ((tool-segs (cl-remove-if-not
                          (lambda (seg) (eq (car seg) 'tool))
                          (mevedel-transcript-segments
                           (point-min) (point-max)))))
          (should (= 2 (length tool-segs)))
          (let ((second-call (mevedel-view--tool-call-parse
                              data-buf (cadr (cadr tool-segs))
                              (caddr (cadr tool-segs)))))
            (should (equal "Read" (plist-get second-call :name)))))))))


(mevedel-deftest mevedel-view--tool-call-parse/malformed-without-marker ()
  ,test
  (test)
  :doc "splits malformed tools before the next real tool block without marker"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (second-start)
        (insert "#+begin_tool (Edit :file_path \"mevedel-chat.el\" :old_string \"...\")\n")
        (let ((bad-start (point)))
          (insert "(:name \"Edit\" :args (:file_path \"mevedel-chat.el\" :old_string \"unterminated\n")
          (put-text-property bad-start (point) 'gptel '(tool . "bad-edit")))
        (setq second-start (point))
        (insert "#+begin_tool (Read :file_path \"next.el\")\n"
                "(:name \"Read\" :args (:file_path \"next.el\"))\n\n"
                "body\n#+end_tool\n")
        (put-text-property second-start (point) 'gptel '(tool . "read"))
        (let ((tool-segs (cl-remove-if-not
                          (lambda (seg) (eq (car seg) 'tool))
                          (mevedel-transcript-segments
                           (point-min) (point-max)))))
          (should (= 2 (length tool-segs)))
          (let ((second-call (mevedel-view--tool-call-parse
                              data-buf (cadr (cadr tool-segs))
                              (caddr (cadr tool-segs)))))
            (should (equal "Read" (plist-get second-call :name)))))))))


(mevedel-deftest mevedel-view--tool-fallback-line ()
  ,test
  (test)
  :doc "uses org tool headers for malformed compact fallback summaries"
  (let* ((line (mevedel-view--tool-fallback-line
                "#+begin_tool (Edit :file_path \"mevedel-chat.el\" :old_string \"...\")\n(:name \"Edit\" :args (:file_path \"mevedel-chat.el\" :old_string \"unterminated\n[mevedel: tool output truncated; omitted 8858 chars]\n"))
         (plain (substring-no-properties line)))
    (should (string-match-p "Edit" plain))
    (should (string-match-p "mevedel-chat.el" plain))
    (should-not (string-match-p "#\\+begin_tool" plain))))


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

(mevedel-deftest mevedel-view--collaboration-event-render-data ()
  ,test
  (test)
  :doc "renders a direct workflow's canonical started event"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** /review current changes\n"
                                    nil)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind collaboration-event
        :event started
        :path "/root/review"
        :status running
        :body ""))
     'ignore)
    (mevedel-view-test--insert-data data-buf "No issues.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-search "/review current changes" text))
        (should (string-search "Started /root/review" text))
        (should (string-search "No issues." text))))))

(mevedel-deftest mevedel-view-directive-actions ()
  ,test
  (test)
  :doc "rewind resumes the real checkpoint session from a synthetic transcript"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "directive-rewind" :root "/tmp"
           :name "directive-rewind"))
         (attempt
          (mevedel-directive-attempt--create
           :sequence 1 :action 'implement
           :checkpoint '(:session-id "real" :turn 2)))
         (record
          (mevedel-directive--create
           :id "directive-1" :state 'implemented
           :attempts (list attempt)))
         captured)
    (with-temp-buffer
      (setq-local mevedel--data-buffer (current-buffer)
                  mevedel--session
                  (mevedel-session--create :authority-mode 'pid-lock :session-id "synthetic"))
      (cl-letf (((symbol-function 'mevedel-view--directive-metadata-context)
                 (lambda (_) (list record workspace attempt 1)))
                ((symbol-function 'read-multiple-choice)
                 (lambda (&rest _) '(?w "rewind")))
                ((symbol-function
                  'mevedel-session-rewind-rewind-checkpoint)
                 (lambda (&rest args) (setq captured args))))
        (mevedel-view-directive-actions
         '(:activity-kind attempt :sequence 1))
        (should (equal (list workspace '(:session-id "real" :turn 2) nil)
                       captured))))))

(mevedel-deftest mevedel-view--directive-turn-summary ()
  ,test
  (test)
  :doc "folded implementation summaries include patch statistics"
  (let ((attempt
         (mevedel-directive-attempt--create
          :patch "--- a/file\n+++ b/file\n-old\n+new\n+second\n")))
    (with-temp-buffer
      (insert "tool\n")
      (put-text-property (point-min) (point-max)
                         'mevedel-view-type 'tool-summary)
      (cl-letf (((symbol-function 'mevedel-view--directive-metadata-context)
                 (lambda (_directive) (list nil nil attempt 1))))
        (should
         (equal "◆ directi… · Implement · T2 · Implemented · 1 tool call · +2 −1 · RET: actions"
                (mevedel-view--directive-turn-summary
                 (point-min) (point-max)
                 '(:directive-id "directive-1" :action implement :turn 2
                   :outcome success :activity-kind attempt))))))))

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

(mevedel-deftest mevedel-view--response-summary ()
  ,test
  (test)
  :doc "keeps org source block markers in response summaries"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src\n"
     'response)
    (let ((summary (mevedel-view--response-summary
                    data-buf
                    (with-current-buffer data-buf (point-min))
                    (with-current-buffer data-buf (point-max)))))
      (should (string-match-p "#\\+begin_src emacs-lisp" summary))
      (should-not (string-match-p "```emacs-lisp" summary)))))

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
	  (should-not (string-match-p "hidden system prompt" text))))

  :doc "hook context blocks are stripped from visible user turn text"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "Real user prompt here.\n\n")
      (insert "<hook-context>\n")
      (insert "Model-only context.\n")
      (insert "</hook-context>\n"))
    (let* ((seg (list 'user (point-min)
                      (with-current-buffer data-buf (point-max))))
           (text (mevedel-view--user-turn-text (list seg) data-buf)))
      (should (string-match-p "Real user prompt" text))
      (should-not (string-match-p "hook-context" text))
      (should-not (string-match-p "Model-only context" text))))

  :doc "Goal context blocks are stripped from visible user turn text"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "<goal-context authority=\"session-sidecar\">\n")
      (insert "Phase: planning\n")
      (insert "</goal-context>\n\n")
      (insert "Planning instructions:\nDo nothing.\n")
      (let ((start (point)))
        (insert (mevedel-pipeline--format-render-data-block
                 '(:kind user-display :text "Dry-run Goal")))
        (add-text-properties start (point) '(gptel ignore))))
    (let* ((segments
            (with-current-buffer data-buf
              (mevedel-transcript-segments (point-min) (point-max))))
           (text (mevedel-view--user-turn-text segments data-buf)))
      (should (equal "Dry-run Goal" text))
      (should-not (string-match-p "Planning instructions" text))
      (should-not (string-match-p "goal-context" text))
      (should-not (string-match-p "Phase: planning" text))))

  :doc "hook context renders as a collapsible view-only disclosure"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat "Real user prompt here.\n\n"
             "<hook-context>\n"
             "<hook-event name=\"UserPromptSubmit\">\n"
             "Model-only context.\n"
             "</hook-event>\n"
             "</hook-context>\n")
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Real user prompt" text))
        (should (string-match-p "◇ hook context added" text))
        (should-not (string-match-p "Model-only context" text)))
      (goto-char (point-min))
      (search-forward "hook context added")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "◇ hook context added" text))
        (should (string-match-p "UserPromptSubmit" text))
        (should (string-match-p "Model-only context" text)))))

  :doc "event-tagged hook context renders one combined disclosure"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat
      "Real user prompt here.\n\n"
      "<hook-context>\n"
      "<hook-event name=\"SessionStart\">\n"
      "Startup context.\n"
      "</hook-event>\n"
      "<hook-event name=\"UserPromptSubmit\">\n"
      "Prompt context.\n"
      "</hook-event>\n"
      "</hook-context>\n")
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (= 1 (mevedel-view-test--count-substring
                      "hook context added" text)))
        (should (string-match-p "Real user prompt" text))
        (should-not (string-match-p "Startup context" text))
        (should-not (string-match-p "Prompt context" text)))
      (goto-char (point-min))
      (search-forward "hook context added")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "SessionStart" text))
        (should (string-match-p "Startup context" text))
        (should (string-match-p "UserPromptSubmit" text))
        (should (string-match-p "Prompt context" text))
        (should-not (string-match-p "<hook-event" text)))))

  :doc "escaped hook context preserves delimiter-looking body text"
  (mevedel-view-test--with-buffers
    (let ((body "literal </hook-event> & <tag>"))
      (mevedel-view-test--insert-data
       data-buf
       (concat
        "Real user prompt here.\n\n"
        (mevedel-hooks-format-context
         (list (list :event "UserPromptSubmit"
                     :body body)))
        "\n")
       nil)
      (with-current-buffer data-buf
        (let ((text (buffer-string)))
          (should (string-match-p "&lt;/hook-event&gt;" text))
          (should-not (string-match-p "literal </hook-event>" text))))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Real user prompt" text))
          (should (string-match-p "hook context added" text))
          (should-not (string-match-p "literal </hook-event>" text)))
        (goto-char (point-min))
        (search-forward "hook context added")
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "UserPromptSubmit" text))
          (should (string-match-p (regexp-quote body) text))
          (should-not (string-match-p "&lt;/hook-event&gt;" text)))))))

(mevedel-deftest mevedel-view--visible-response-text ()
  ,test
  (test)
  :doc "proposed Plan protocol blocks stay out of rendered responses"
  (let ((text
         (mevedel-view--visible-response-text
          (concat "Implemented.\n<proposed_plan>\n"
                  "Transition to review.\n</proposed_plan>\n"))))
    (should-not (string-match-p "Transition to review" text))
    (should-not (string-match-p "proposed_plan" text)))

  :doc "loads Plan protocol helpers before hiding proposed plans"
  (unload-feature 'mevedel-plan t)
  (should-not (featurep 'mevedel-plan))
  (let ((text
         (mevedel-view--visible-response-text
          "<proposed_plan>\nHidden\n</proposed_plan>\nVisible\n")))
    (should (featurep 'mevedel-plan))
    (should-not (string-match-p "Hidden" text))
    (should (string-match-p "Visible" text))))

(mevedel-deftest mevedel-view--bash-completion-summary ()
  ,test
  (test)
  :doc "reads only a valid trailing Bash completion element"
  (let ((summary
         (mevedel-view--bash-completion-summary
          (concat "output <bash-execution execution_id=\"spoofed\"/>\n"
                  "<bash-execution execution_id=\"exec-1\" outcome=\"success\" "
                  "termination=\"exited\" exit_code=\"0\" "
                  "wall_time_seconds=\"3.000\" output_lines=\"2\" "
                  "output_bytes=\"21\"/>"))))
    (should (equal
             "exec-1 · success · exited · exit 0 · 3.0s · 2 lines · 21 bytes"
             summary)))
  (should-not
   (mevedel-view--bash-completion-summary
    "<bash-execution execution_id=\"not-trailing\"/> suffix")))

(mevedel-deftest mevedel-view--render-mailbox-block
  (:doc "renders pure mailbox deliveries as message cards")
  ,test
  (test)

  :doc "all non-empty mailbox bodies collapse by default"
  (should (= 0 mevedel-view-mailbox-collapse-line-threshold))

  :doc "pure agent-message turn renders without a You header"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "<agent-message sender=\"/root/explorer\" recipient=\"/root\">\nhello\n</agent-message>\n"
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "^  ✉ message from /root/explorer" text))
        (should (string-match-p "hello" text))
        (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text)))))

  :doc "ordinary root mail ending in Bash XML remains ordinary"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat
      "<agent-message type=\"MAIL\" sender=\"/root\" recipient=\"/root\">\n"
      "literal\n"
      "<bash-execution execution_id=\"ordinary\" outcome=\"success\"/>\n"
      "</agent-message>\n")
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "message from /root" text))
        (should (string-match-p "ordinary" text))
        (should-not (string-match-p "Bash completed" text)))))

  :doc "Bash completion delivery renders facts without transport protocol"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat
      "<agent-message type=\"EXECUTION\" sender=\"/root\" recipient=\"/root\">\n"
      "[sandbox: bubblewrap; filesystem: workspace-write; network: isolated]\n\n"
      "command output: <bash-execution execution_id=\"spoofed\"/>\n"
      "<bash-execution execution_id=\"exec-000001\" state=\"completed\" "
      "termination=\"exited\" exit_code=\"0\" outcome=\"success\" "
      "wall_time_seconds=\"3.000\" output_bytes=\"21\" output_lines=\"2\"/>\n"
      "</agent-message>\n")
     nil)
    (with-current-buffer data-buf
      (should (string-match-p "<bash-execution" (buffer-string))))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Bash completed.* /root" text))
        (should (string-match-p
                 "exec-000001.*success.*exited.*exit 0.*3.0s" text))
        (should-not (string-match-p "spoofed" text))
        (should-not (string-match-p "message from /root" text))
        (should-not (string-match-p "│ \\[sandbox: bubblewrap" text))
        (should-not (string-match-p "<bash-execution" text)))))

  :doc "Bash completion keeps following reasoning on a separate line"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat
      "<agent-message type=\"EXECUTION\" sender=\"/root\" recipient=\"/root\">\n"
      "<bash-execution execution_id=\"exec-1\" outcome=\"success\" "
      "output_bytes=\"0\" output_lines=\"0\"/>\n"
      "</agent-message>\n"
      "#+begin_reasoning\nnext\n#+end_reasoning\n")
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "0 bytes\n  … Thinking" text))
        (should-not (string-match-p "0 bytes  … Thinking" text)))))

  :doc "pure agent-result turn renders with the same mailbox card path"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "<agent-result sender=\"/root/worker\" recipient=\"/root\">\nresult\n</agent-result>\n"
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "^  ✓ Finished /root/worker" text))
        (should (string-match-p "│ result" text))
        (should (string-match-p "result" text))
        (should (string-match-p "Assistant\n" text))
        (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text)))
      (goto-char (point-min))
      (search-forward "  ✓ Finished")
      (let ((header-start (match-beginning 0)))
        (should-not (get-text-property header-start 'font-lock-face))
        (should (eq (get-text-property (+ header-start 2) 'font-lock-face)
                    'mevedel-view-attribution)))
      (goto-char (point-min))
      (search-forward "│")
      (should (eq (get-text-property
                   (match-beginning 0) 'font-lock-face)
                  'mevedel-view-mailbox-gutter))
      (search-forward "result")
      (should (eq (get-text-property
                   (match-beginning 0) 'font-lock-face)
                  'mevedel-view-mailbox-body))))

  :doc "agent-result body may mention nested result blocks"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 200))
      (mevedel-view-test--insert-data
       data-buf
       (concat
        "<agent-result sender=\"/root/verifier\" recipient=\"/root\">\n"
        "Before nested example.\n"
        "```elisp\n"
        "(:body \"<agent-result>\n"
        "partial result\n"
        "</agent-result>\")\n"
        "```\n"
        "After nested example.\n"
        "</agent-result>\n")
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "✓ Finished /root/verifier" text))
          (should (string-match-p "Before nested example" text))
          (should (string-match-p "After nested example" text))
          (should (string-match-p "partial result" text))
          (should-not (string-match-p "<agent-result sender=\"/root/verifier\""
                                      text))
          (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text))))))

  :doc "mailbox blocks separated by prose render independently"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 200))
      (mevedel-view-test--insert-data
       data-buf
       (concat
        "<agent-result sender=\"/root/reviewer\" recipient=\"/root\">\n"
        "first result\n"
        "</agent-result>\n"
        "Assistant prose between mailbox cards.\n"
        "<agent-result sender=\"/root/verifier\" recipient=\"/root\">\n"
        "second result\n"
        "</agent-result>\n")
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (finished-count
                (cl-count-if (lambda (line)
                               (string-prefix-p "  ✓ Finished" line))
                             (split-string text "\n"))))
          (should (= 2 finished-count))
          (should (string-match-p "✓ Finished /root/reviewer" text))
          (should (string-match-p "✓ Finished /root/verifier" text))
          (should (string-match-p "Assistant prose between mailbox cards" text))
          (should-not (string-match-p "<agent-result" text))))))

  :doc "indented mailbox close line is removed structurally"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 200))
      (mevedel-view-test--insert-data
       data-buf
       "<agent-result sender=\"/root/worker\" recipient=\"/root\">\nresult\n  </agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "✓ Finished /root/worker" text))
          (should (string-match-p "result" text))
          (should-not (string-match-p "</agent-result>" text))))))

  :doc "expanded agent-result keeps gutter on blank body lines"
  (let ((mevedel-view-mailbox-collapse-line-threshold 200))
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data
       data-buf
       "<agent-result sender=\"/root/worker\" recipient=\"/root\">\nfirst\n\nsecond\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p
                   "✓ Finished /root/worker\n\n    │ first"
                   text))
          (should (string-match-p "│ first\n    │ \n    │ second" text))))))

  :doc "long agent-result delivery expands to the final response body"
  (mevedel-view-test--with-buffers
    (let* ((mevedel-view-mailbox-collapse-line-threshold 1)
           (root (file-name-as-directory
                  (make-temp-file "mevedel-mailbox-long-" t)))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "mailbox-long"
                       :root root
                       :name "mailbox-long"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-save-path session)
            (file-name-as-directory
             (file-name-concat root "session")))
      (setf (mevedel-session-agent-transcripts session)
            '(("storage-long" . (:agent-path "/root/worker"
                                  :path "agents/worker.chat.org"
                                  :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (mevedel-view-test--insert-data
       data-buf
       "<agent-result sender=\"/root/worker\" recipient=\"/root\">\nline one\nline two\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "✓ Finished /root/worker" text))
          (should (string-match-p
                   "✓ Finished /root/worker \\[[0-9]+ lines collapsed\\]"
                   text))
          (should-not (string-match-p
                       "✓ Finished /root/worker\n[[:space:]]+\\[[0-9]+ lines collapsed\\]"
                       text))
          (goto-char (point-min))
          (search-forward "line two")
          (should (eq (get-text-property (match-beginning 0) 'invisible)
                      'mevedel-view-mailbox-collapsed)))
        (goto-char (point-min))
        (search-forward "✓ Finished /root/worker")
        (goto-char (match-beginning 0))
        (search-forward "/root/worker")
        (goto-char (match-beginning 0))
        (let (opened)
          (cl-letf (((symbol-function
                      'mevedel-view-open-agent-transcript)
                     (lambda (id &rest _) (setq opened id))))
            (mevedel-view-open-agent-transcript-at-point))
          (should (equal "/root/worker" opened)))
        (goto-char (point-min))
        (search-forward "✓ Finished /root/worker")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (goto-char (point-min))
        (search-forward "line two")
        (should-not (get-text-property (match-beginning 0) 'invisible))
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p
                   "✓ Finished /root/worker \\[[0-9]+ lines collapsed\\]"
                   text))
          (should-not (string-match-p
                       "✓ Finished /root/worker\n[[:space:]]+\\[[0-9]+ lines collapsed\\]"
                       text))))
      (delete-directory root t)))

  :doc "collapsed agent-result counts non-empty payload lines"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 0))
      (mevedel-view-test--insert-data
       data-buf
       "<agent-result sender=\"/root/worker\" recipient=\"/root\">\nresult\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p
                   "✓ Finished /root/worker \\[1 line collapsed\\]"
                   text))
          (should-not (string-match-p "2 lines collapsed" text))))))

  :doc "mailbox decoration clears inherited agent-handle properties"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "mailbox-stale"
                       :root temporary-file-directory
                       :name "mailbox-stale"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-save-path session)
            (file-name-as-directory
             (file-name-concat temporary-file-directory
                               "mevedel-mailbox-stale-session")))
      (setf (mevedel-session-agent-transcripts session)
            '(("storage-stale" . (:agent-path "/root/explorer"
                                   :path "agents/explorer.chat.org"
                                   :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)))
    (with-current-buffer data-buf
      (insert "(:name \"Agent\" :args (:task_name \"explore\" :message \"Inspect.\"))\n\nlaunch\n"))
    (with-current-buffer view-buf
      (let* ((stale-source (cons 1 (with-current-buffer data-buf (point-max))))
             (start nil))
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (setq start (point))
                (insert "<agent-result sender=\"/root/explorer\" recipient=\"/root\">\nfinal body\n</agent-result>\n")
                (add-text-properties
                 start (point)
                 `(mevedel-view-source ,stale-source
                   mevedel-view-type agent-handle
                   mevedel-view-agent-path "/root/explorer"
                   mevedel-view-agent-handle-p t
                   mevedel-view-agent-status completed))
                (mevedel-view--decorate-agent-result-blocks start (point)))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (goto-char start)
        (search-forward "✓ Finished /root/explorer")
        (search-backward "/root/explorer")
        (should (eq (get-text-property (point) 'mevedel-view-type)
                    'mailbox-delivery))
        (should-not (get-text-property (point) 'mevedel-view-source))
        (should-not (get-text-property (point) 'mevedel-view-agent-handle-p))
        (should (equal "/root/explorer"
                       (get-text-property (point) 'mevedel-view-agent-path)))
        (search-forward "final body")
        (should-not (get-text-property (match-beginning 0)
                                       'mevedel-view-agent-path)))))

  :doc "mailbox delivery between response chunks stays in one assistant turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Before mailbox.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     "\n<agent-message sender=\"/root/explorer\" recipient=\"/root\">\nhello\n</agent-message>\n\n"
     nil)
    (mevedel-view-test--insert-data data-buf "After mailbox.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let* ((text (buffer-substring-no-properties
                    (point-min) mevedel-view--input-marker))
             (assistant-count
              (cl-count-if (lambda (line) (string= line "Assistant"))
                           (split-string text "\n"))))
        (should (= 1 assistant-count))
        (should (string-match-p "Before mailbox" text))
        (should (string-match-p "✉ message from /root/explorer" text))
        (should (string-match-p "hello" text))
        (should (string-match-p "After mailbox" text)))))

  :doc "agent-result after response does not render as a You turn"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 200))
      (mevedel-view-test--insert-data
       data-buf
       "Reviewer returned clean. Waiting on verifier.\n"
       'response)
      (mevedel-view-test--insert-data
       data-buf
       (concat
        "<agent-result sender=\"/root/verifier\" recipient=\"/root\">\n"
        "Output observed:\n"
        "```elisp\n"
        "(:body \"<agent-result sender=\\\"/root/explorer\\\">\n"
        "partial\n"
        "</agent-result>\")\n"
        "```\n"
        "VERDICT: FAIL\n"
        "</agent-result>\n")
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Reviewer returned clean" text))
          (should (string-match-p "✓ Finished /root/verifier" text))
          (should (string-match-p "VERDICT: FAIL" text))
          (should-not (string-match-p
                       "You\n✓ Finished /root/verifier"
                       text))))))

  :doc "mailbox toggle does not expand a preceding Agent source"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 1))
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"Agent\" :args (:task_name \"explorer\" :message \"Skim mevedel-queue.el\"))\n\n{\"path\":\"/root/explorer\"}\n"
       '(tool . "call_agent"))
      (mevedel-view-test--insert-data
       data-buf
       "Assistant text before mailbox.\n"
       'response)
      (mevedel-view-test--insert-data
       data-buf
       "\n<agent-message sender=\"/root/explorer\" recipient=\"/root\">\nHello from your Explorer Agent :)\n</agent-message>\n\n<agent-result sender=\"/root/explorer\" recipient=\"/root\">\nfinal line one\nfinal line two\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "✓ Finished /root/explorer")
        (goto-char (match-beginning 0))
        (should (eq (get-text-property (point) 'mevedel-view-type)
                    'mailbox-delivery))
        (should-not (get-text-property (point) 'mevedel-view-source))
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "✉ message from /root/explorer" text))
          (should (string-match-p "Hello from your Explorer Agent :)" text))
          (should (string-match-p "✓ Finished /root/explorer" text))
          (should (string-match-p "final line two" text))
          (should-not (string-match-p "Skim mevedel-queue.el (370 lines)"
                                      text)))))))


(defun mevedel-view-test--format-media-data-block
    (media session tool-use-id)
  "Format MEDIA for SESSION and TOOL-USE-ID."
  (mevedel-tool-media--format-media-data-block
   media
   (mevedel-pipeline--tool-results-dir session nil)
   tool-use-id))

(mevedel-deftest mevedel-view--tool-call-parse-media-fallback ()
  ,test
  (test)
  :doc "text Read keeps copied persisted media side-channel visible"
  (let* ((tmpdir (make-temp-file "mevedel-view-copied-media-" t))
         (ws (mevedel-workspace--create :root tmpdir))
         (save-path (file-name-as-directory
                     (file-name-concat tmpdir ".mevedel" "sessions" "main")))
         (session (mevedel-session--create
                   :name "main" :workspace ws :save-path save-path))
         (media '((:path "/tmp/a.png"
                   :mime "image/png"
                   :kind image
                   :data "captured")))
         (copied (substring-no-properties
                  (mevedel-view-test--format-media-data-block
                   media session "toolu_original"))))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (mevedel-view-test--insert-data
           data-buf
           (concat "(:name \"Read\" :args (:file_path \"/tmp/copied.txt\"))\n\n"
                   "plain text" copied)
           '(tool . "toolu_other"))
          (with-current-buffer data-buf
            (let ((parsed (mevedel-view--tool-call-parse
                           data-buf (point-min) (point-max))))
              (should (string-search mevedel-tool-media--data-open
                                     (plist-get parsed :result))))))
      (delete-directory tmpdir t)))

  :doc "media Read keeps copied side-channel visible without current tool id"
  (let* ((tmpdir (make-temp-file "mevedel-view-media-no-id-" t))
         (ws (mevedel-workspace--create :root tmpdir))
         (save-path (file-name-as-directory
                     (file-name-concat tmpdir ".mevedel" "sessions" "main")))
         (session (mevedel-session--create
                   :name "main" :workspace ws :save-path save-path))
         (media '((:path "/tmp/a.png"
                   :mime "image/png"
                   :kind image
                   :data "captured")))
         (copied (substring-no-properties
                  (mevedel-view-test--format-media-data-block
                   media session "toolu_original"))))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (mevedel-view-test--insert-data
           data-buf
           (concat "(:name \"Read\" :args (:file_path \"/tmp/a.png\"))\n\n"
                   "plain text" copied)
           '(tool . nil))
          (with-current-buffer data-buf
            (let ((parsed (mevedel-view--tool-call-parse
                           data-buf (point-min) (point-max))))
              (should (string-search mevedel-tool-media--data-open
                                     (plist-get parsed :result))))))
      (delete-directory tmpdir t)))

  :doc "media Read keeps copied side-channel before generated side-channel"
  (let* ((tmpdir (make-temp-file "mevedel-view-media-copied-prefix-" t))
         (ws (mevedel-workspace--create :root tmpdir))
         (save-path (file-name-as-directory
                     (file-name-concat tmpdir ".mevedel" "sessions" "main")))
         (session (mevedel-session--create
                   :name "main" :workspace ws :save-path save-path))
         (copied-media '((:path "/tmp/copied.png"
                          :mime "image/png"
                          :kind image
                          :data "copied")))
         (actual-media '((:path "/tmp/a.png"
                          :mime "image/png"
                          :kind image
                          :data "actual")))
         (copied (substring-no-properties
                  (mevedel-view-test--format-media-data-block
                   copied-media session "toolu_copied")))
         (actual (substring-no-properties
                  (mevedel-view-test--format-media-data-block
                   actual-media session "toolu_actual")))
         (result (concat "plain text" copied "\nbody tail" actual)))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (mevedel-view-test--insert-data
           data-buf
           (concat "(:name \"Read\" :args (:file_path \"/tmp/a.png\"))\n\n"
                   result)
           '(tool . "toolu_wrong"))
          (with-current-buffer data-buf
            (let ((parsed (mevedel-view--tool-call-parse
                           data-buf (point-min) (point-max))))
              (should (string-search mevedel-tool-media--data-open
                                     (plist-get parsed :result)))
              (should (string-search "body tail"
                                     (plist-get parsed :result))))))
      (delete-directory tmpdir t)))

  :doc "media Read can strip resumed duplicate block with wrong gptel id"
  (let* ((tmpdir (make-temp-file "mevedel-view-media-duplicate-" t))
         (ws (mevedel-workspace--create :root tmpdir))
         (save-path (file-name-as-directory
                     (file-name-concat tmpdir ".mevedel" "sessions" "main")))
         (session (mevedel-session--create
                   :name "main" :workspace ws :save-path save-path))
         (media '((:path "/tmp/a.png"
                   :mime "image/png"
                   :kind image
                   :data "captured")))
         (result (substring-no-properties
                  (concat "plain media"
                          (mevedel-view-test--format-media-data-block
                           media session "toolu_actual")))))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (mevedel-view-test--insert-data
           data-buf
           (concat "(:name \"Read\" :args (:file_path \"/tmp/a.png\"))\n\n"
                   result)
           '(tool . "toolu_wrong"))
          (with-current-buffer data-buf
            (let ((parsed (mevedel-view--tool-call-parse
                           data-buf (point-min) (point-max))))
              (should (equal "plain media" (plist-get parsed :result))))))
      (delete-directory tmpdir t))))

(provide 'test-mevedel-view-render)
;;; test-mevedel-view-render.el ends here
