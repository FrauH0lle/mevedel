;;; test-mevedel-view-render.el --- View rendering tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-audit)
(require 'mevedel-view-render)

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
(require 'mevedel-preview-mode)
(require 'mevedel-permission-queue)
(require 'mevedel-tool-exec)
(require 'mevedel-goal)
(require 'mevedel-tool-task)
(require 'mevedel-agents)
(require 'mevedel-agent-runtime)
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

  :doc "reasoning text (nil segments) inside assistant turn absorbed"
  (let* ((segs '((user 1 10) (ignored 10 20) (user 20 40) (tool 40 80)
                 (user 80 90) (ignored 90 100) (response 100 150)))
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

  :doc "guardian audit does not absorb the following Goal turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Planning complete.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind request-summary :elapsed-seconds 19))
     'ignore)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel--format-hook-audit-record
      '(:type goal-guardian :verdict approve :reason "Safe"))
     'ignore)
    (mevedel-view-test--insert-data
     data-buf "Implementation instructions:\nDo nothing.\n" nil)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind user-display :text "Implement accepted plan"))
     'ignore)
    (mevedel-view-test--insert-data data-buf "Implementation complete.\n"
                                    'response)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind request-summary :elapsed-seconds 7))
     'ignore)
    (with-current-buffer data-buf
      (let* ((segments (mevedel-transcript-segments (point-min) (point-max)))
             (turns (mevedel-view--group-into-turns segments data-buf)))
        (should (equal '(assistant user assistant)
                       (mapcar (lambda (turn) (plist-get turn :role))
                               turns)))
        (should (equal '(1 0 1)
                       (mapcar
                        (lambda (turn)
                          (cl-count 'request-summary
                                    (plist-get turn :segments) :key #'car))
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


;;
;;; Full rendering

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
            (should (search-forward ":elapsed" nil t))))
      (when-let* ((buf (get-buffer mevedel-view-render-debug-buffer-name)))
        (kill-buffer buf))))
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
                       (list (list :agent-id "verifier--zones123"
                                   :status 'running
                                   :agent-type "verifier"
                                   :description "verify zones"
                                   :calls 1)))))
            (mevedel-view--full-rerender))
          (should-not permission-outcomes)
          (let* ((text (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker))
                 (header (string-trim-right
                          (mevedel-view--header-string data-buf)))
                 (header-pos (string-search header text))
                 (task-pos (string-search "visible zone task" text))
                 (agent-pos (string-search
                             "Agent: verifier -- verify zones" text))
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
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "System reminder (2 lines)" text))
        (should-not (string-match-p "Thinking" text))
        (should-not (string-match-p "<system-reminder>" text)))
      (goto-char (point-min))
      (search-forward "System reminder")
      (goto-char (match-beginning 0))
      (should (eq (get-text-property (point) 'mevedel-view-type)
                  'system-reminder-summary))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "CRITICAL: verify only" text))
        (should (string-match-p "Report findings" text))
        (should-not (string-match-p "<system-reminder>" text))
        (should-not (string-match-p "</system-reminder>" text)))))
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
      (goto-char (point-min))
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
     "(:name \"FullStateAgent\" :args (:subagent_type \"verifier\"))\n\nfull rerender agent body\n"
     '(tool . "call_full_state_agent"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "Agent: verifier -- full state")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (should (search-forward "full rerender agent body"
                              mevedel-view--input-marker t))
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
      (should (= 0 (hash-table-count mevedel-view--source-collapse-states)))))

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
          (should (= 1 (cl-count-if (lambda (line) (string= line "Assistant"))
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
        (mevedel-view--set-in-flight-turn-start second-start)
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

  :doc "restores Goal display text and hides protocol wrapper tags"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat "<goal-context authority=\"session-sidecar\">\n"
             "Objective: Dry-run Goal\n"
             "</goal-context>\n\n"
             "Planning instructions:\nDo nothing.\n")
     nil)
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert (mevedel-pipeline--format-render-data-block
                 '(:kind user-display :text "Dry-run Goal")))
        (add-text-properties start (point) '(gptel ignore))))
    (mevedel-view-test--insert-data
     data-buf
     "<goal_review>\nverdict: complete\nsummary: Done.\n</goal_review>\n"
     'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Dry-run Goal" text))
        (should-not (string-match-p "Planning instructions" text))
        (should (string-match-p "verdict: complete" text))
        (should-not (string-match-p "goal_review" text))))))


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
       "\n<agent-message from=\"explorer\">\nhello\n</agent-message>\n"
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
          (should (string-match-p "full content here" text))))))

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
      (should (= 0 (hash-table-count mevedel-view--source-collapse-states)))
      (should-not (search-forward "event body hidden"
                                  mevedel-view--input-marker t)))))


(mevedel-deftest mevedel-view-toggle-section/renderer-vtype ()
  ,test
  (test)
  :doc "prompt summaries expand and collapse through their renderer"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "(:name \"Agent\" :args (:subagent_type \"explorer\"))\n\nraw launch payload\n"))
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
                        nil))))))))


(mevedel-deftest mevedel-view-toggle-section/agent-unavailable ()
  ,test
  (test)
  :doc "agent handles with missing saved transcripts report unavailable"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--missing")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "missing-transcript"
                       :root temporary-file-directory
                       :name "missing-transcript"))
           (session (mevedel-session-create "main" workspace))
           opened
           message-text)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (insert "(:name \"Agent\" :args (:subagent_type \"explorer\"))\n\nraw launch payload\n"))
      (with-current-buffer view-buf
        (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
               (rendering (mevedel-tool-ui--render-agent
                           "Agent"
                           '(:subagent_type "explorer"
                             :description "Missing transcript task")
                           "rendered missing transcript body\n"
                           `(:kind agent-transcript
                             :agent-id ,agent-id
                             :status completed))))
          (let ((inhibit-read-only t))
            (goto-char mevedel-view--input-marker)
            (set-marker-insertion-type mevedel-view--input-marker t)
            (unwind-protect
                (mevedel-view--insert-rendered-tool rendering source)
              (set-marker-insertion-type mevedel-view--input-marker nil)))
          (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                     (lambda (_buf _start _end) rendering))
                    ((symbol-function 'mevedel-view-open-agent-transcript)
                     (lambda (_id)
                       (setq opened t)
                       (user-error "Transcript file missing")))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (goto-char (point-min))
            (search-forward "Agent: explorer")
            (goto-char (match-beginning 0))
            (mevedel-view-toggle-section)
            (let ((text (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
              (should opened)
              (should (equal "Transcript file missing" message-text))
              (should-not (string-match-p
                           "rendered missing transcript body" text))
              (should-not (string-match-p "raw launch payload" text))))))))

  :doc "agent handles with stale live invocations report unavailable"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--stale")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "stale-live-transcript"
                       :root temporary-file-directory
                       :name "stale-live-transcript"))
           (session (mevedel-session-create "main" workspace))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :transcript-status 'running))
           opened
           message-text)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (insert "(:name \"Agent\" :args (:subagent_type \"explorer\"))\n\nraw launch payload\n"))
      (with-current-buffer view-buf
        (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
               (rendering (mevedel-tool-ui--render-agent
                           "Agent"
                           '(:subagent_type "explorer"
                             :description "Stale live task")
                           "rendered stale live body\n"
                           `(:kind agent-transcript
                             :agent-id ,agent-id
                             :status running))))
          (let ((inhibit-read-only t))
            (goto-char mevedel-view--input-marker)
            (set-marker-insertion-type mevedel-view--input-marker t)
            (unwind-protect
                (mevedel-view--insert-rendered-tool rendering source)
              (set-marker-insertion-type mevedel-view--input-marker nil)))
          (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                     (lambda (_buf _start _end) rendering))
                    ((symbol-function 'mevedel-view--agent-invocation)
                     (lambda (_id) inv))
                    ((symbol-function 'mevedel-view-open-agent-transcript)
                     (lambda (_id)
                       (setq opened t)
                       (user-error "Live buffer unavailable")))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (goto-char (point-min))
            (search-forward "Agent: explorer")
            (goto-char (match-beginning 0))
            (mevedel-view-toggle-section)
            (let ((text (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
              (should opened)
              (should (equal "Live buffer unavailable" message-text))
              (should-not (string-match-p "rendered stale live body" text))
              (should-not (string-match-p "raw launch payload" text))))))))
)

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
        (should (string-match-p "Third line" text)))))

  :doc "response collapse and expand keep known proposed-plan blocks hidden"
  (mevedel-view-test--with-buffers
    (let* ((old-plan "# Hidden plan\n")
           (session (mevedel-session--create
                     :name "test"
                     :workspace nil
                     :permission-mode 'ask
                     :plan-metadata
                     (list :presented-plan-hashes
                           (list (mevedel-plan-hash old-plan))))))
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
        (should (= 1 (mevedel-view-test--count-substring "Carol" expanded)))))))

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
          (should-not (string-match-p "Thinking\\.\\.\\." text)))
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
          (should-not (string-match-p "Thinking\\.\\.\\." text))))))

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
       "(:name \"StateAgent\" :args (:subagent_type \"verifier\"))\n\nagent body stays open\n"
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
         "\n<agent-result agent-id=\"worker--state\" type=\"worker\">\nline one\nline two\n</agent-result>\n\n"
         nil)
        (with-current-buffer view-buf
          (mevedel-view--full-rerender)
          (setq mevedel-view--data-turn-start assistant-start)
          (goto-char (point-min))
          (search-forward "Assistant")
          (setq view-assistant-start (match-beginning 0))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker view-assistant-start nil))
          (search-forward "worker--state")
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
  :doc "owns source-backed folding and navigation"
  (dolist (symbol '(mevedel-view-toggle-section
                    mevedel-view--section-bounds
                    mevedel-view-next-display
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

(mevedel-deftest mevedel-view--render-tool-group/fallback-linkifies-paths ()
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
      (delete-directory root t))))

(mevedel-deftest mevedel-view--render-tool-group/source-collapse-state ()
  ,test
  (test)
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
                                mevedel-view--input-marker t))))))

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
                                   'font-lock-face line)))))

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
               '(:header "h" :expandable-p maybe))))


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
    (should (equal '(:header "error")
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
    (should (equal '(:header "default")
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
  :doc "session-only blocked state invalidates cached Agent renderings"
  (let* ((mevedel-view--tool-rendering-cache (make-hash-table :test #'equal))
         (mevedel-view--render-cache-entries 0)
         (agent-id "explorer--blocked-cache")
         (workspace (mevedel-workspace--create
                     :type 'project
                     :id "blocked-cache"
                     :root temporary-file-directory
                     :name "blocked-cache"))
         (session (mevedel-session-create "main" workspace))
         (agent-tool (mevedel-tool--create
                      :name "Agent"
                      :renderer #'mevedel-tool-ui--render-agent)))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (insert "(:name \"Agent\" :args (:subagent_type \"explorer\" :description \"cache\"))\n"
              "Agent is running.\n"
              (mevedel-pipeline--format-render-data-block
               (list :kind 'agent-transcript
                     :agent-id agent-id
                     :status 'running
                     :calls 1)))
      (cl-letf (((symbol-function 'mevedel-tool-get)
                 (lambda (name &optional _category)
                   (and (equal name "Agent") agent-tool))))
        (let ((running (mevedel-view--segment-rendering
                        (current-buffer) (point-min) (point-max) t)))
          (should (string-match-p "\\[running · 1 calls\\]"
                                  (plist-get running :header))))
        (setf (mevedel-session-permission-queue session)
              (list (list :origin agent-id)))
        (let ((blocked (mevedel-view--segment-rendering
                        (current-buffer) (point-min) (point-max) t)))
          (should (string-match-p "\\[blocked · awaiting permission\\]"
                                  (plist-get blocked :header)))))))
  :doc "malformed tool text still returns nil"
  (with-temp-buffer
    (insert "not a tool")
    (should-not (mevedel-view--segment-rendering
                 (current-buffer) (point-min) (point-max)))))

(mevedel-deftest mevedel-view--insert-rendered-tool/non-expandable ()
  ,test
  (test)
  :doc "non-expandable renderings do not carry source state"
  (with-temp-buffer
    (mevedel-view-mode)
    (let ((rendering '(:header "TaskCreate: Created 1 task"
                       :expandable-p nil))
          (inhibit-read-only t))
      (mevedel-view--insert-rendered-tool rendering (cons 1 10))
      (goto-char (point-min))
      (should (eq 'tool-event
                  (get-text-property (point) 'mevedel-view-type)))
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
                    (mevedel-pipeline--format-render-data-block render-data)
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

(mevedel-deftest mevedel-view--agent-transcript-render-data ()
  ,test
  (test)
  :doc "renders hidden review progress handles without exposing user_action"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** /review current changes\n"
                                    nil)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind agent-transcript
              :agent-id "reviewer--abc"
              :agent-type "reviewer"
              :name "Review"
              :description "current changes"
              :progress-handle review
              :default-expanded t
              :status running
              :calls 1
              :body ""))
     'ignore)
    (mevedel-view-test--insert-data
     data-buf
     "<user_action>\n  <action>review</action>\n  <results>\n  hidden\n  </results>\n</user_action>\n"
     nil)
    (mevedel-view-test--insert-data data-buf "No issues.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-search "/review current changes" text))
        (should (string-search "Review: current changes" text))
        (should-not (string-search "(1 lines)" text))
        (should (string-search "[running" text))
        (should-not (string-search "… waiting" text))
        (should (string-search "No issues." text))
        (should-not (string-search "<user_action>" text))
        (should-not (string-search "hidden" text))
        (should (= 1 (cl-count-if (lambda (line) (string= line "You"))
                                  (split-string text "\n")))))))

  :doc "renders review prompt when synthetic action shares its user segment"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert "*** /review current changes\n")
        (insert (mevedel-pipeline--format-render-data-block
                 '(:kind agent-transcript
                         :agent-id "reviewer--abc"
                         :agent-type "reviewer"
                         :name "Review"
                         :description "current changes"
                         :progress-handle review
                         :default-expanded t
                         :status running
                         :calls 1
                         :body "")))
        (put-text-property (save-excursion
                             (search-backward "<!-- mevedel-render-data -->" start t))
                           (point)
                           'gptel 'ignore)
        (insert "<user_action>\n"
                "  <action>review</action>\n"
                "  <results>\n"
                "  hidden\n"
                "  </results>\n"
                "</user_action>\n")))
    (mevedel-view-test--insert-data data-buf "No issues.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-search "/review current changes" text))
        (should (string-search "Review: current changes" text))
        (should (string-search "No issues." text))
        (should-not (string-search "<user_action>" text))
        (should-not (string-search "hidden" text))
        (should (= 1 (cl-count-if (lambda (line) (string= line "You"))
                                  (split-string text "\n"))))))))

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
  :doc "Goal protocol wrapper tags stay out of rendered responses"
  (let ((text
         (mevedel-view--visible-response-text
          (concat "Implemented.\n<proposed_plan>\n"
                  "Transition to review.\n</proposed_plan>\n"
                  "<goal_review>\nverdict: complete\n"
                  "summary: Done.\n</goal_review>\n"))))
    (should (string-match-p "Transition to review" text))
    (should (string-match-p "verdict: complete" text))
    (should-not (string-match-p "proposed_plan" text))
    (should-not (string-match-p "goal_review" text))))

(mevedel-deftest mevedel-view--render-mailbox-block
  (:doc "renders pure mailbox deliveries as message cards")
  ,test
  (test)

  :doc "pure agent-message turn renders without a You header"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "<agent-message from=\"explorer--abc123\">\nhello\n</agent-message>\n"
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "✉ message from explorer--abc123" text))
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
        (should (string-match-p "✓ finished worker--xyz789" text))
        (should (string-match-p "│ result" text))
        (should (string-match-p "result" text))
        (should (string-match-p "Assistant\n" text))
        (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text)))
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
        "<agent-result agent-id=\"verifier--nested\" type=\"verifier\">\n"
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
          (should (string-match-p "✓ finished verifier--nested" text))
          (should (string-match-p "Before nested example" text))
          (should (string-match-p "After nested example" text))
          (should (string-match-p "partial result" text))
          (should-not (string-match-p "<agent-result agent-id=\"verifier--nested\""
                                      text))
          (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text))))))

  :doc "mailbox blocks separated by prose render independently"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 200))
      (mevedel-view-test--insert-data
       data-buf
       (concat
        "<agent-result agent-id=\"reviewer--one\" type=\"reviewer\">\n"
        "first result\n"
        "</agent-result>\n"
        "Assistant prose between mailbox cards.\n"
        "<agent-result agent-id=\"verifier--two\" type=\"verifier\">\n"
        "second result\n"
        "</agent-result>\n")
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (finished-count
                (cl-count-if (lambda (line)
                               (string-prefix-p "✓ finished" line))
                             (split-string text "\n"))))
          (should (= 2 finished-count))
          (should (string-match-p "✓ finished reviewer--one" text))
          (should (string-match-p "✓ finished verifier--two" text))
          (should (string-match-p "Assistant prose between mailbox cards" text))
          (should-not (string-match-p "<agent-result" text))))))

  :doc "indented mailbox close line is removed structurally"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 200))
      (mevedel-view-test--insert-data
       data-buf
       "<agent-result agent-id=\"worker--indented\" type=\"worker\">\nresult\n  </agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "✓ finished worker--indented" text))
          (should (string-match-p "result" text))
          (should-not (string-match-p "</agent-result>" text))))))

  :doc "expanded agent-result keeps gutter on blank body lines"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "<agent-result agent-id=\"worker--blank\" type=\"worker\">\nfirst\n\nsecond\n</agent-result>\n"
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p
                 "✓ finished worker--blank\n\n    │ first"
                 text))
        (should (string-match-p "│ first\n    │ \n    │ second" text)))))

  :doc "long agent-result delivery expands to the final response body"
  (mevedel-view-test--with-buffers
    (let* ((mevedel-view-mailbox-collapse-line-threshold 1)
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "mailbox-long"
                       :root temporary-file-directory
                       :name "mailbox-long"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-save-path session)
            (file-name-as-directory
             (file-name-concat temporary-file-directory
                               "mevedel-mailbox-long-session")))
      (setf (mevedel-session-agent-transcripts session)
            '(("worker--long" . (:path "agents/worker--long.chat.org"
                                :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (mevedel-view-test--insert-data
       data-buf
       "<agent-result agent-id=\"worker--long\" type=\"worker\">\nline one\nline two\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "✓ finished worker--long" text))
          (should (string-match-p
                   "✓ finished worker--long \\[[0-9]+ lines collapsed\\]"
                   text))
          (should-not (string-match-p
                       "✓ finished worker--long\n[[:space:]]+\\[[0-9]+ lines collapsed\\]"
                       text))
          (goto-char (point-min))
          (search-forward "line two")
          (should (eq (get-text-property (match-beginning 0) 'invisible)
                      'mevedel-view-mailbox-collapsed)))
        (goto-char (point-min))
        (search-forward "✓ finished worker--long")
        (goto-char (match-beginning 0))
        (search-forward "worker--long")
        (goto-char (match-beginning 0))
        (let (opened)
          (cl-letf (((symbol-function
                      'mevedel-view--open-agent-transcript-or-message)
                     (lambda (id &rest _) (setq opened id))))
            (mevedel-view-open-agent-transcript-at-point))
          (should (equal "worker--long" opened)))
        (goto-char (point-min))
        (search-forward "✓ finished worker--long")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (goto-char (point-min))
        (search-forward "line two")
        (should-not (get-text-property (match-beginning 0) 'invisible))
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p
                   "✓ finished worker--long \\[[0-9]+ lines collapsed\\]"
                   text))
          (should-not (string-match-p
                       "✓ finished worker--long\n[[:space:]]+\\[[0-9]+ lines collapsed\\]"
                       text))))))

  :doc "collapsed agent-result counts non-empty payload lines"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 0))
      (mevedel-view-test--insert-data
       data-buf
       "<agent-result agent-id=\"worker--one\" type=\"worker\">\nresult\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p
                   "✓ finished worker--one \\[1 line collapsed\\]"
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
            '(("explorer--stale" . (:path "agents/explorer--stale.chat.org"
                                  :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)))
    (with-current-buffer data-buf
      (insert "(:name \"Agent\" :args (:subagent_type \"explorer\"))\n\nlaunch\n"))
    (with-current-buffer view-buf
      (let* ((stale-source (cons 1 (with-current-buffer data-buf (point-max))))
             (start nil))
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (setq start (point))
                (insert "<agent-result agent-id=\"explorer--stale\" type=\"explorer\">\nfinal body\n</agent-result>\n")
                (add-text-properties
                 start (point)
                 `(mevedel-view-source ,stale-source
                   mevedel-view-type agent-handle
                   mevedel-view-agent-id "explorer--stale"
                   mevedel-view-agent-handle-p t
                   mevedel-view-agent-status completed))
                (mevedel-view--decorate-agent-result-blocks start (point)))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (goto-char start)
        (search-forward "✓ finished explorer--stale")
        (search-backward "explorer--stale")
        (should (eq (get-text-property (point) 'mevedel-view-type)
                    'mailbox-delivery))
        (should-not (get-text-property (point) 'mevedel-view-source))
        (should-not (get-text-property (point) 'mevedel-view-agent-handle-p))
        (should (equal "explorer--stale"
                       (get-text-property (point) 'mevedel-view-agent-id)))
        (search-forward "final body")
        (should-not (get-text-property (match-beginning 0)
                                       'mevedel-view-agent-id)))))

  :doc "mailbox delivery between response chunks stays in one assistant turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Before mailbox.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     "\n<agent-message from=\"explorer\">\nhello\n</agent-message>\n\n"
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
        (should (string-match-p "✉ message from explorer" text))
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
        "<agent-result agent-id=\"verifier--ar\" type=\"verifier\">\n"
        "Output observed:\n"
        "```elisp\n"
        "(:body \"<agent-result agent-id=\\\"explorer--A\\\">\n"
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
          (should (string-match-p "✓ finished verifier--ar" text))
          (should (string-match-p "VERDICT: FAIL" text))
          (should-not (string-match-p
                       "You\n✓ finished verifier--ar"
                       text))))))

  :doc "mailbox toggle does not expand a preceding Agent source"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 1))
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"Agent\" :args (:subagent_type \"explorer\" :description \"Skim mevedel-queue.el\"))\n\nAgent launched in background\n"
       '(tool . "call_agent"))
      (mevedel-view-test--insert-data
       data-buf
       "Assistant text before mailbox.\n"
       'response)
      (mevedel-view-test--insert-data
       data-buf
       "\n<agent-message from=\"explorer\">\nHello from your Explorer Agent :)\n</agent-message>\n\n<agent-result agent-id=\"explorer--33d949f0\" type=\"explorer\">\nfinal line one\nfinal line two\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "✓ finished explorer--33d949f0")
        (goto-char (match-beginning 0))
        (should (eq (get-text-property (point) 'mevedel-view-type)
                    'mailbox-delivery))
        (should-not (get-text-property (point) 'mevedel-view-source))
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "✉ message from explorer" text))
          (should (string-match-p "Hello from your Explorer Agent :)" text))
          (should (string-match-p "✓ finished explorer--33d949f0" text))
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

(mevedel-deftest mevedel-view--format-hook-audit-block ()
  ,test
  (test)
  :doc "renders Goal guardian decisions as compact expandable disclosures"
  (should
   (equal "  \u25c7 Goal guardian: approve\n"
          (mevedel-view--format-hook-audit-block
           '(:type goal-guardian :verdict approve :reason "Safe"
             :provider "P:M" :effort high :at "now")
           nil)))
  (let ((expanded
         (mevedel-view--format-hook-audit-block
          '(:type goal-guardian :verdict ask :reason "Need scope"
            :provider "P:M" :effort high :at "now")
          t)))
    (dolist (needle '("Goal guardian: ask" "Need scope" "P:M" "high" "now"))
      (should (string-match-p (regexp-quote needle) expanded)))))

(mevedel-deftest mevedel-view--render-assistant-turn/guardian-audit ()
  ,test
  (test)
  :doc "renders standalone guardian audit data without exposing it as reasoning"
  (let ((data-buf (generate-new-buffer " *guardian-render-data*"))
        audit-start)
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer data-buf
            (insert "Planning complete.\n")
            (setq audit-start (point))
              (insert
               (mevedel--format-hook-audit-record
                '(:type goal-guardian :verdict approve :reason "Safe"
                  :provider "P:M" :effort high :at "now"))))
          (mevedel-view--render-assistant-turn
           `((response 1 ,audit-start)
             (ignored ,audit-start
                      ,(with-current-buffer data-buf (point-max))))
           data-buf)
          (should (string-match-p "Goal guardian: approve" (buffer-string)))
          (should-not (string-match-p "Thinking" (buffer-string))))
      (kill-buffer data-buf))))

(provide 'test-mevedel-view-render)
;;; test-mevedel-view-render.el ends here
