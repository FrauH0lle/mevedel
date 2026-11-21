;;; test-niwa-view-render.el --- View rendering tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-agent-control)
(require 'niwa-agent-conversation)
(require 'niwa-notes)
(require 'niwa-persistence)
(require 'niwa-view)
(require 'niwa-view-agent)
(require 'niwa-view-audit)
(require 'niwa-view-composer)
(require 'niwa-view-render)
(require 'niwa-view-segments)

;; Focused dependencies moved with the rendering tests.
(require 'niwa-view-stream)
(require 'niwa-view-viewport)
(require 'niwa-transcript)
(require 'niwa-transcript-audit)
(require 'niwa-transcript-restore)
(require 'niwa-structs)
(require 'niwa-pipeline)
(require 'niwa-tool-media)
(require 'niwa-tool-patch)
(require 'niwa-tool-ptc)
(require 'niwa-tool-render-data)
(require 'niwa-tool-registry)
(require 'niwa-tool-repair-diagnostics)
(require 'niwa-session-persistence)
(require 'niwa-session-rewind)
(require 'niwa-tool-ui)
(require 'niwa-tool-exec)
(require 'niwa-tool-task)
(require 'niwa-agents)
(require 'niwa-hooks)

(defun niwa-view-render-test--view-text ()
  "Return the current view's transcript and zones, excluding the draft."
  (buffer-substring-no-properties (point-min) (niwa-view-transcript-tail)))

(defun niwa-view-render-test--draft-offset ()
  "Return the current view's composer caret offset from its draft start."
  (let ((start (niwa-view-composer-draft-start)))
    (with-current-buffer (niwa-view-composer-buffer)
      (- (point) start))))

(defun niwa-view-render-test--goto-draft-offset (offset)
  "Move the current view's composer caret to draft-relative OFFSET."
  (let ((start (niwa-view-composer-draft-start)))
    (with-current-buffer (niwa-view-composer-buffer)
      (goto-char (+ start offset)))))

;;; Turn grouping

(defun niwa-view-test--group-synthetic-segments (segments)
  "Group synthetic SEGMENTS against a sufficiently large data buffer."
  (with-temp-buffer
    (insert (make-string 300 ?x))
    (niwa-view--group-into-turns segments (current-buffer))))

;;
;;; Turn grouping and tool summaries

(niwa-deftest niwa-view--tool-result-error-p ()
  ,test
  (test)
  :doc "recognizes canonical and legacy prose error prefixes"
  (should (niwa-view--tool-result-error-p "Error: File is missing"))
  (should (niwa-view--tool-result-error-p "Error writing file: denied"))
  (should-not (niwa-view--tool-result-error-p "Writing file succeeded")))

(niwa-deftest niwa-view--group-into-turns/audit-classification ()
  (with-temp-buffer
    (let* ((audit (niwa--format-hook-audit-record
                   '(:type hook :event "UserPromptSubmit" :status success)))
           (classify (symbol-function 'niwa-view--scaffolding-only-text-p))
           (audit-cleanups 0)
           segments turns)
      (dolist (entry (list (cons 'user "Prompt\n")
                           (cons 'ignored audit)
                           (cons 'response "Answer\n")
                           (cons 'ignored audit)
                           (cons 'user "Next prompt\n")))
        (let ((start (point)))
          (insert (cdr entry))
          (push (list (car entry) start (point)) segments)))
      (cl-letf (((symbol-function 'niwa-view--scaffolding-only-text-p)
                 (lambda (text)
                   (when (equal text audit) (cl-incf audit-cleanups))
                   (funcall classify text))))
        (setq turns (niwa-view--group-into-turns
                     (nreverse segments) (current-buffer))))
      (should (= audit-cleanups 0))
      (should (equal '(user assistant user)
                     (mapcar (lambda (turn) (plist-get turn :role)) turns)))
      (should (equal '(user ignored) (mapcar #'car (plist-get (car turns) :segments))))
      (should (equal '(response ignored)
                     (mapcar #'car (plist-get (cadr turns) :segments)))))))

(niwa-deftest niwa-view--group-into-turns/non-audit-classification ()
  (let ((audit (niwa--format-hook-audit-record
                '(:type hook :event "UserPromptSubmit" :status success))))
    (dolist (text (list (substring-no-properties audit)
                        (concat audit "visible tail")
                        (propertize
                         (concat niwa--hook-audit-open "\ninvalid\n"
                                 niwa--hook-audit-close)
                         'niwa-hook-audit t)
                        "#+begin_reasoning\n#+end_reasoning\n"))
      (with-temp-buffer
        (insert text)
        (let ((classify (symbol-function 'niwa-view--scaffolding-only-text-p))
              (calls 0))
          (cl-letf (((symbol-function 'niwa-view--scaffolding-only-text-p)
                     (lambda (value)
                       (cl-incf calls)
                       (funcall classify value))))
            (niwa-view--group-into-turns
             (list (list 'ignored (point-min) (point-max))) (current-buffer)))
          (should (= calls 1)))))))

(niwa-deftest niwa-view--group-into-turns ()
  ,test
  (test)
  :doc "prompt attachments stay with their user turn until assistant activity"
  (dolist (attachment
           (list
            (list 'prompt ":PROMPT:\nPrepared prompt.\n:END:\n" t)
            (list 'hook-context
                  "<hook-context>\nContext.\n</hook-context>\n" t)
            (list 'ignored
                  (niwa--format-hook-audit-record
                   '(:type hook :event "UserPromptSubmit" :status success))
                  t)
            (list 'render-data
                  (niwa-tool-render-data-format
                   '(:kind user-display :text "Visible prompt")) t)
            (list 'ignored
                  (niwa-tool-render-data-format
                   '(:kind inline-skill :prompt "Prepared skill.")) t)
            (list 'render-data
                  (niwa-tool-render-data-format
                   '(:kind inline-skill :prompt "Prepared skill.")) t)
            (list 'render-data
                  (niwa-tool-render-data-format
                   '(:kind user-display :text 7)) nil)))
    (dolist (prefix '((user) (user response) (task-background) nil))
      (with-temp-buffer
        (let (segments)
          (dolist (type prefix)
            (let ((start (point)))
              (insert "Visible text.\n")
              (push (list type start (point)) segments)))
          (let ((start (point)))
            (insert (cadr attachment))
            (push (list (car attachment) start (point)) segments))
          (setq segments (nreverse segments))
          (let* ((original (copy-tree segments))
                 (text (buffer-string))
                 (turns (niwa-view--group-into-turns
                         segments (current-buffer)))
                 (user (cl-find 'user turns
                                :key (lambda (turn) (plist-get turn :role)))))
            (should (equal original segments))
            (should (equal-including-properties text (buffer-string)))
            (if (eq (car prefix) 'user)
                (let ((attached (and (equal prefix '(user))
                                     (nth 2 attachment))))
                  (should (equal (plist-get user :segments)
                                 (if attached segments (list (car segments)))))
                  (should (= (plist-get user :end)
                             (if attached (point-max) (caddar segments)))))
              (should-not user)))))))

  :doc "single user turn"
  (let* ((segs '((user 1 10)))
         (turns (niwa-view-test--group-synthetic-segments segs)))
    (should (= 1 (length turns)))
    (should (eq 'user (plist-get (car turns) :role))))

  :doc "drops leading assistant-side residue with nothing to render"
  ;; Rewinding to before the first turn leaves whitespace still carrying
  ;; tool and audit properties; it must not project an empty turn.
  (with-temp-buffer
    (insert "\n\n")
    (should-not
     (niwa-view--group-into-turns
      '((tool 1 2) (ignored 2 3)) (current-buffer))))

  :doc "keeps a leading assistant segment that has visible content"
  (with-temp-buffer
    (insert "tool output\n")
    (let ((turns (niwa-view--group-into-turns
                  '((tool 1 13)) (current-buffer))))
      (should (= 1 (length turns)))
      (should (eq 'assistant (plist-get (car turns) :role)))))

  :doc "user then assistant turn"
  (let* ((segs '((user 1 10) (response 10 30) (tool 30 50)))
         (turns (niwa-view-test--group-synthetic-segments segs)))
    (should (= 2 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (= 2 (length (plist-get (cadr turns) :segments)))))

  :doc "multiple user-assistant pairs"
  (let* ((segs '((user 1 10) (response 10 20) (user 20 30) (response 30 40)))
         (turns (niwa-view-test--group-synthetic-segments segs)))
    (should (= 4 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (eq 'user (plist-get (caddr turns) :role)))
    (should (eq 'assistant (plist-get (cadddr turns) :role))))

  :doc "reasoning segments inside assistant turn absorbed"
  (let* ((segs '((user 1 10) (ignored 10 20) (reasoning 20 40)
                 (tool 40 80) (reasoning 80 90) (ignored 90 100)
                 (response 100 150)))
         (turns (niwa-view-test--group-synthetic-segments segs)))
    (should (= 2 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (= 6 (length (plist-get (cadr turns) :segments)))))

  :doc "mid-turn nil gap after response absorbed when next is ignored/tool"
  (with-temp-buffer
    (insert (make-string 200 ?\s))
    (let* ((segs '((user 1 10) (response 10 50) (user 50 60)
                   (ignored 60 80) (tool 80 120) (response 120 200)))
           (turns (niwa-view--group-into-turns segs (current-buffer))))
      (should (= 2 (length turns)))
      (should (eq 'user (plist-get (car turns) :role)))
      (should (eq 'assistant (plist-get (cadr turns) :role)))
      ;; All 5 non-user segments belong to one assistant turn
      (should (= 5 (length (plist-get (cadr turns) :segments))))))

  :doc "nil gap after response starts new user turn when next is response"
  (let* ((segs '((user 1 10) (response 10 50) (user 50 60)
                 (response 60 100)))
         (turns (niwa-view-test--group-synthetic-segments segs)))
    ;; user | assistant(response) | user | assistant(response)
    (should (= 4 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (eq 'user (plist-get (caddr turns) :role)))
    (should (eq 'assistant (plist-get (cadddr turns) :role))))

  :doc "blank nil gap between response ranges stays in assistant turn"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "First answer.\n" 'response)
    (niwa-view-test--insert-data data-buf "\n\n" nil)
    (niwa-view-test--insert-data data-buf "Second answer.\n" 'response)
    (with-current-buffer data-buf
      (let* ((segments (niwa-transcript-segments (point-min) (point-max)))
             (turns (niwa-view--group-into-turns segments data-buf)))
        (should (equal '(assistant)
                       (mapcar (lambda (turn) (plist-get turn :role))
                               turns)))
        (should (= 3 (length (plist-get (car turns) :segments)))))))

  :doc "real user prompt between response ranges remains a user turn"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "First answer.\n" 'response)
    (niwa-view-test--insert-data data-buf "\n\nSecond prompt.\n\n" nil)
    (niwa-view-test--insert-data data-buf "Second answer.\n" 'response)
    (with-current-buffer data-buf
      (let* ((segments (niwa-transcript-segments (point-min) (point-max)))
             (turns (niwa-view--group-into-turns segments data-buf)))
        (should (equal '(assistant user assistant)
                       (mapcar (lambda (turn) (plist-get turn :role))
                               turns))))))

  :doc "real user prompt after response is not absorbed before reasoning"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "First answer.\n" 'response)
    (niwa-view-test--insert-data
     data-buf "\n\nSecond prompt.\n\n#+begin_reasoning\n" nil)
    (niwa-view-test--insert-data data-buf "thinking\n" 'ignore)
    (with-current-buffer data-buf
      (let* ((segments (niwa-transcript-segments (point-min) (point-max)))
             (turns (niwa-view--group-into-turns segments data-buf)))
        (should (equal '(assistant user assistant)
                       (mapcar (lambda (turn) (plist-get turn :role))
                               turns))))))

  :doc "real user prompt after closed reasoning starts a user turn"
  (let* ((segs '((reasoning 1 20) (user 20 40) (reasoning 40 60)))
         (turns (niwa-view-test--group-synthetic-segments segs)))
    (should (equal '(assistant user assistant)
                   (mapcar (lambda (turn) (plist-get turn :role))
                           turns))))

  :doc "same-turn steering after a tool starts a user turn"
  (let* ((segs '((response 1 20) (tool 20 40)
                 (user 40 60) (reasoning 60 80)))
         (turns (niwa-view-test--group-synthetic-segments segs)))
    (should (equal '(assistant user assistant)
                   (mapcar (lambda (turn) (plist-get turn :role))
                           turns))))

  :doc "retained agent task after tool activity starts a user turn"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Initial prompt.\n" nil)
    (niwa-view-test--insert-data
     data-buf "Tool activity.\n" '(tool . "call_1"))
    (niwa-view-test--insert-data
     data-buf
     "* Agent Task: follow_up\n\nSecond prompt.\n"
     nil)
    (niwa-view-test--insert-data data-buf "Second answer.\n" 'response)
    (with-current-buffer data-buf
      (let* ((segments (niwa-transcript-segments (point-min) (point-max)))
             (turns (niwa-view--group-into-turns segments data-buf)))
        (should (equal '(user assistant user assistant)
                       (mapcar (lambda (turn) (plist-get turn :role))
                               turns))))))

  :doc "scaffolding-only gap after response is still absorbed"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "First answer.\n" 'response)
    (niwa-view-test--insert-data
     data-buf "\n\n#+begin_reasoning\n" nil)
    (niwa-view-test--insert-data data-buf "thinking\n" 'ignore)
    (with-current-buffer data-buf
      (let* ((segments (niwa-transcript-segments (point-min) (point-max)))
             (turns (niwa-view--group-into-turns segments data-buf)))
        (should (= 1 (length turns)))
        (should (eq 'assistant (plist-get (car turns) :role))))))

  :doc "task background is its own turn beside the authoritative Agent Task"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     (concat "<task-background>\nAdvisory context.\n</task-background>\n"
             "\n* Agent Task: child\n\nDo the work.\n")
     nil)
    (with-current-buffer data-buf
      (let* ((segments (niwa-transcript-segments (point-min) (point-max)))
             (turns (niwa-view--group-into-turns segments data-buf)))
        (should (equal '(task-background user)
                       (mapcar (lambda (turn) (plist-get turn :role)) turns)))
        (should (eq 'task-background
                    (caar (plist-get (car turns) :segments))))))))


(niwa-deftest niwa-view--tool-one-liner ()
  ,test
  (test)
  :doc "a readable atom remains a malformed tool diagnostic"
  (with-temp-buffer
    (insert "encoded-payload")
    (let ((line (niwa-view--tool-one-liner
                 (current-buffer) (point-min) (point-max))))
      (should (string-match-p "encoded-payload" line))
      (should-not (string-match-p "Tool (0 lines)" line))))
  :doc "Read tool summary"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/home/user/src/main.el\"))\n\nline 1\nline 2\nline 3\n"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((summary (niwa-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (string-match-p "Read" summary))
        (should (string-match-p "main\\.el" summary))
        (should (string-match-p "3 lines" summary))
        (should (string-match "✓" summary))
        (should (eq 'niwa-view-tool-marker
                    (get-text-property (match-beginning 0)
                                       'font-lock-face summary)))
        (should (string-match "Read" summary))
        (should (eq 'niwa-view-tool-name
                    (get-text-property (match-beginning 0)
                                       'font-lock-face summary))))))

  :doc "Bash tool summary"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "(:name \"Bash\" :args (:command \"ls -la\"))\n\noutput\n"
     '(tool . "call_2"))
    (with-current-buffer data-buf
      (let ((summary (niwa-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (string-match-p "Bash" summary))
        (should (string-match-p "ls -la" summary)))))

  :doc "hook-blocked tools show the blocking event and reason"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "(:name \"Bash\" :args (:command \"rm -rf /tmp/x\"))\n\nError: blocked by PreToolUse: blocked rm -rf test\n"
     '(tool . "call_hook_block"))
    (with-current-buffer data-buf
      (let ((summary (niwa-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (string-match-p "Bash" summary))
        (should (string-match-p "rm -rf /tmp/x" summary))
        (should (string-match-p "blocked by PreToolUse: blocked rm -rf test"
                                summary))
        (should (string-match "×" summary))
        (should (eq 'error
                    (get-text-property (match-beginning 0)
                                       'font-lock-face summary))))))

  :doc "fallback on unparseable content"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "not a valid sexp" '(tool . "call_3"))
    (with-current-buffer data-buf
      (let ((summary (niwa-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (stringp summary))
        (should (> (length summary) 0)))))

  :doc "fallback suppresses marker-only tool fragments"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "#+begin_tool (Read :file_path \"x\")\n" '(tool . "call_4"))
    (with-current-buffer data-buf
      (should-not (niwa-view--tool-one-liner
                   data-buf (point-min) (point-max)))))

  :doc "tool-level errors use error marker"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/missing.el\"))\n\nError: File does not exist\n"
     '(tool . "call_error"))
    (with-current-buffer data-buf
      (let ((summary (niwa-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (string-match "×" summary))
        (should (eq 'error
                    (get-text-property (match-beginning 0)
                                       'font-lock-face summary))))))

  :doc "scaffolding prefix on the segment doesn't drop the tool name"
  ;; A boundary-expansion or patch can land seg-start on the
  ;; `#+begin_tool …' line (no gptel property) instead of the call
  ;; sexp.  The cleaner skips the marker so the parse still surfaces
  ;; `Bash: …' instead of bare `Tool'.
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool (Bash :command \"git status\")\n"
              "(:name \"Bash\" :args (:command \"git status\"))\n"
              "\nOK\n"))
    (let ((line (niwa-view--tool-one-liner
                 data-buf (point-min)
                 (with-current-buffer data-buf (point-max)))))
      (should (string-match-p "Bash" line))
      (should-not (string-match-p "\\bTool\\b" line)))))


;;
;;; Navigation

(niwa-deftest niwa-view-user-query-navigation ()
  ,test
  (test)
  :doc "jumps between visible user query headers"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let (first second)
        (let ((inhibit-read-only t))
          (goto-char niwa-view--status-marker)
          (setq first (point))
          (insert (propertize "You\n"
                              'niwa-view-turn-role 'user))
          (insert "first question\n")
          (insert (propertize "Assistant\n"
                              'niwa-view-turn-role 'assistant))
          (insert "answer\n")
          (setq second (point))
          (insert (propertize "You\n"
                              'niwa-view-turn-role 'user))
          (insert "second question\n")
          (set-marker niwa-view--status-marker (point))
          (set-marker niwa-view--interaction-marker (point)))
        (goto-char (point-min))
        (niwa-view-next-user-query)
        (should (= (point) first))
        (niwa-view-next-user-query)
        (should (= (point) second))
        (niwa-view-next-user-query)
        (should (= (point) second))
        (goto-char (+ second (length "You\ns")))
        (niwa-view-previous-user-query)
        (should (= (point) second))
        (niwa-view-previous-user-query)
        (should (= (point) first))))))

(niwa-deftest niwa-view-fork-point-at-point ()
  ,test
  (test)
  :doc "resolves the exact settled assistant response at point"
  (niwa-view-test--with-buffers
    (let ((session
           (niwa-session--create
            :name "fork-points"
            :current-segment 1)))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (insert "First prompt\n")
        (insert (propertize "First response\n" 'gptel 'response))
        (insert
         (niwa--format-hook-audit-record
          '(:type fork-point :fork-point-id "fork-point-1"
            :segment 1 :turn 1 :file-turn 1 :cum-turn 1)))
        (insert "Second prompt\n")
        (insert (propertize "Second response\n" 'gptel 'response))
        (insert
         (niwa--format-hook-audit-record
          '(:type fork-point :fork-point-id "fork-point-2"
            :segment 1 :turn 2 :file-turn 2 :cum-turn 2))))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--full-rerender)
        (should-not (string-search "hook audit" (niwa-view-render-test--view-text)))
        (goto-char (point-min))
        (search-forward "Assistant")
        (search-forward "Assistant")
        (let ((target (niwa-view-fork-point-at-point)))
          (should (equal "fork-point-2"
                         (plist-get target :fork-point-id)))
          (should (= 1 (plist-get target :segment)))
          (should (= 2 (plist-get target :cum-turn)))))))
  :doc "rewinds through the exact settled assistant response at point"
  (niwa-view-test--with-buffers
    (let ((session
           (niwa-session--create
            :name "rewind-point"
            :current-segment 1))
          called-buffer called-target called-boundary called-scope returned-latest)
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (insert "Prompt\n")
        (insert (propertize "Response\n" 'gptel 'response))
        (insert
         (niwa--format-hook-audit-record
          '(:type fork-point :fork-point-id "rewind-point-1"
            :segment 1 :turn 1 :file-turn 1 :cum-turn 1))))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Assistant")
        (cl-letf
            (((symbol-function 'niwa-session-rewind-rewind)
              (lambda (buffer target &optional boundary scope _confirm complete)
                (setq called-buffer buffer
                      called-target target
                      called-boundary boundary
                      called-scope scope)
                (funcall complete t nil)
                t))
             ((symbol-function 'niwa-view-return-to-latest-segment)
              (lambda (&optional _event)
                (setq returned-latest t))))
          (niwa-view-rewind-at-point)))
      (should (eq data-buf called-buffer))
      ;; Point names a response the user is looking at, so it survives.
      (should (eq 'after called-boundary))
      (should (eq 'choose called-scope))
      (should returned-latest)
      (should (equal "rewind-point-1"
                     (plist-get called-target :fork-point-id)))))
  :doc "waits for native completion before returning to the live segment"
  (with-temp-buffer
    (setq-local niwa--session (niwa-session--create :name "native"))
    (setq-local niwa--data-buffer (current-buffer))
    (let (pending returned-latest)
      (cl-letf (((symbol-function 'niwa-view--settled-response-at-point)
                 (lambda () '(:fork-point-id "native")))
                ((symbol-function 'niwa-session-rewind-rewind)
                 (lambda (_buffer _target _boundary _scope _confirm complete)
                   (setq pending complete)
                   :pending))
                ((symbol-function 'niwa-view-return-to-latest-segment)
                 (lambda (&optional _event) (setq returned-latest t))))
        (should (eq :pending (niwa-view-rewind-at-point)))
        (should-not returned-latest)
        (funcall pending t nil)
        (should returned-latest)))))

(niwa-deftest niwa-view--conversation-variant-button ()
  ,test
  (test)
  :doc "renders the direct Source switch beside expanded and collapsed headers"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-view-variants-" t))))
    (unwind-protect
        (niwa-view-test--with-buffers
          (let* ((session
                  (niwa-session--create
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
              (setq-local niwa--session session)
              (insert (propertize "Response line one.\nResponse line two.\n"
                                  'gptel 'response))
              (insert
               (niwa--format-hook-audit-record
                '(:type fork-point :fork-point-id "fork-point-1"
                  :segment 1 :turn 1 :file-turn 1 :cum-turn 1))))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (cl-letf
                  (((symbol-function
                     'niwa-session-persistence-conversation-variants)
                    (lambda (_session _fork-point-id &optional _sessions)
                      variants)))
                (niwa-view--full-rerender)
                (goto-char (point-min))
                (should (search-forward
                         "Assistant  [⇆ Source · 2 variants]" nil t))
                (should (functionp
                         (get-text-property
                          (1- (point)) 'niwa-view-zone-activate)))
                (goto-char (point-min))
                (search-forward "Assistant")
                (niwa-view--collapse-turn)
                (goto-char (point-min))
                (should (search-forward "[⇆ Source · 2 variants]" nil t))
                (should (functionp
                         (get-text-property
                          (1- (point)) 'niwa-view-zone-activate)))
                (setq variants (list (car variants)))
                (niwa-view--full-rerender)
                (goto-char (point-min))
                (should-not (search-forward "variants]" nil t))))))
      (delete-directory root t)))

  :doc "uses explicit live session context for an archived transcript"
  (with-temp-buffer
    (let ((session
           (niwa-session--create
            :name "source"
            :session-id "source-id"
            :save-path "/tmp/niwa-view-sessions/source/"))
          (variants
           '((:variant-origin source
              :summary (:session-id "source-id"))
             (:variant-origin conversation
              :summary (:session-id "child-id")))))
      (insert (propertize "Archived response.\n" 'gptel 'response))
      (insert
       (niwa--format-hook-audit-record
        '(:type fork-point :fork-point-id "fork-point-1"
          :segment 1 :turn 1 :file-turn 1 :cum-turn 1)))
      (let ((niwa-view--conversation-variant-sessions variants))
        (cl-letf
            (((symbol-function
               'niwa-session-persistence-conversation-variants)
              (lambda (_session _fork-point-id &optional _sessions)
                variants)))
          (should-not (bound-and-true-p niwa--session))
          (should
           (string-search
            "[⇆ Source · 2 variants]"
            (niwa-view--conversation-variant-button
             (current-buffer) (point-min) (point-max) session))))))))

(niwa-deftest niwa-view--render-turn ()
  ,test
  (test)
  :doc "does not scan fork points while incrementally rendering a live turn"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert (propertize "Streaming response.\n" 'gptel 'response)))
    (with-current-buffer view-buf
      (cl-letf
          (((symbol-function
             'niwa-session-artifacts-fork-point-at-source)
            (lambda (&rest _)
              (ert-fail "Incremental render scanned fork points"))))
        (niwa-view--render-turn
         '(:role assistant
           :segments ((response 1 21))
           :start 1 :end 21)
         data-buf))))

  :doc "renders task background collapsed without disturbing a composer draft"
  (niwa-view-test--with-buffers
    (let (end)
      (with-current-buffer data-buf
        (insert "<task-background>\nAdvisory context.\n</task-background>\n")
        (setq end (point-max)))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> quote\nsecond line" 3)
        (niwa-view--render-turn
         (list :role 'task-background
               :segments (list (list 'task-background 1 end))
               :start 1 :end end)
         data-buf)
        (goto-char (point-min))
        (should (search-forward "Task background" nil t))
        (should (get-text-property (match-beginning 0)
                                   'niwa-view-collapsed))
        (should-not (search-forward "Advisory context" nil t))
        (should (equal "> quote\nsecond line"
                       (niwa-view-composer-draft-text nil t)))))))

(niwa-deftest niwa-view-switch-conversation-variant ()
  ,test
  (test)
  :doc "opens the sole alternative, preserves drafts, and lands at the shared point"
  (niwa-view-test--with-buffers
    (let* ((root (niwa-test--placeholder-root))
           (source-path (file-name-concat root "source/"))
           (target-path (file-name-concat root "child/"))
           (target-data (generate-new-buffer " *test-target-data*"))
           (target-view (generate-new-buffer " *test-target-view*"))
           (source-session
            (niwa-session--create
             :name "source"
             :session-id "source-id"
             :save-path source-path
             :working-directory "/source/"
             :current-segment 1))
           (target-session
            (niwa-session--create
             :name "fork"
             :session-id "child-id"
             :save-path target-path
             :working-directory "/worktree/"
             :forked-from-session-id "source-id"
             :forked-from-fork-point-id "fork-point-1"
             :fork-type 'worktree
             :current-segment 2
             :prompt-index
             '((1 . ((:fork-point-id "fork-point-1"
                      :cum-turn 1 :preview "shared"))))))
           (variants
            `((:save-path ,source-path
               :variant-origin source
               :summary (:session-id "source-id"))
              (:save-path ,target-path
               :variant-origin worktree
               :summary (:session-id "child-id"))))
           displayed
           targeted-segment)
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local niwa--session source-session)
              (insert (propertize "Shared response.\n" 'gptel 'response))
              (insert
               (niwa--format-hook-audit-record
                '(:type fork-point :fork-point-id "fork-point-1"
                  :segment 1 :turn 1 :file-turn 1 :cum-turn 1))))
            (with-current-buffer target-data
              (niwa--transcript-org-mode)
              (setq-local gptel-response-separator "\n\n")
              (setq-local niwa--session target-session)
              (insert (propertize "Shared response.\n" 'gptel 'response))
              (insert
               (niwa--format-hook-audit-record
                '(:type fork-point :fork-point-id "fork-point-1"
                  :segment 1 :turn 1 :file-turn 1 :cum-turn 1))))
            (niwa-view--setup target-view target-data)
            (cl-letf
                (((symbol-function
                   'niwa-session-persistence-conversation-variants)
                  (lambda (_session _fork-point-id &optional _sessions)
                    variants))
                 ((symbol-function 'niwa-session-persistence-restore)
                  (lambda (_save-path) target-data))
                 ((symbol-function 'niwa-view-go-to-segment)
                  (lambda (number)
                    (setq targeted-segment number)))
                 ((symbol-function 'display-buffer)
                  (lambda (buffer &optional _action)
                    (setq displayed buffer)
                    nil)))
              (with-current-buffer target-view
                (setq-local niwa--session target-session)
                (niwa-view--full-rerender)
                (niwa-view-test--insert-composer-draft
                 "> target draft\nsecond line"))
              (with-current-buffer view-buf
                (setq-local niwa--session source-session)
                (niwa-view--full-rerender)
                (niwa-view-test--insert-composer-draft
                 "> source draft\nsecond line")
                (goto-char (point-min))
                (search-forward "[⇆ Source · 2 variants]")
                (backward-char 1)
                (niwa-view-activate-at-point))
              (should (eq target-view displayed))
              (should (= 1 targeted-segment))
              (with-current-buffer view-buf
                (should (string-match-p
                         "> source draft\nsecond line"
                         (niwa-view-composer-draft-text nil t))))
              (with-current-buffer target-view
                (should (eq 'turn-header
                            (get-text-property
                             (point) 'niwa-view-type)))
                (should (string-match-p
                         "> target draft\nsecond line"
                         (niwa-view-composer-draft-text nil t))))
              (should (equal "/source/"
                             (niwa-session-working-directory
                              source-session)))
              (should (equal "/worktree/"
                             (niwa-session-working-directory
                              target-session)))))
        (when (buffer-live-p target-view)
          (kill-buffer target-view))
        (when (buffer-live-p target-data)
          (kill-buffer target-data)))))
  :doc "opens the stable chooser when more than one alternative survives"
  (niwa-view-test--with-buffers
    (let* ((session
            (niwa-session--create
             :name "source"
             :session-id "source-id"
             :save-path "/tmp/niwa-view-sessions/source/"))
           (target-data (generate-new-buffer " *test-choice-data*"))
           (target-view (generate-new-buffer " *test-choice-view*"))
           (variants
            '((:save-path "/tmp/niwa-view-sessions/source/"
               :variant-origin source
               :summary (:session-id "source-id"))
              (:save-path "/tmp/niwa-view-sessions/child-1/"
               :variant-origin conversation
               :summary (:session-id "child-1"))
              (:save-path "/tmp/niwa-view-sessions/child-2/"
               :variant-origin worktree
               :summary (:session-id "child-2"))))
           chosen)
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (with-current-buffer target-data
              (setq-local niwa--session
                          (niwa-session--create
                           :name "target"
                           :session-id "child-2"))
              (setq-local niwa--view-buffer target-view))
            (with-current-buffer target-view
              (setq-local niwa--data-buffer target-data))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (cl-letf
                  (((symbol-function
                     'niwa-session-persistence-conversation-variants)
                    (lambda (&rest _) variants))
                   ((symbol-function
                     'niwa-session-persistence-choose-conversation-variant)
                    (lambda (choices current-id)
                      (setq chosen (list choices current-id))
                      (car (last choices))))
                   ((symbol-function 'niwa-session-persistence-restore)
                    (lambda (_save-path) target-data))
                   ((symbol-function 'niwa-view--full-rerender)
                    #'ignore)
                   ((symbol-function
                     'niwa-view-goto-conversation-variant)
                    #'ignore)
                   ((symbol-function 'display-buffer)
                    #'ignore))
                (niwa-view-switch-conversation-variant "fork-point-1")))
            (should (equal (list variants "source-id") chosen)))
        (when (buffer-live-p target-view)
          (kill-buffer target-view))
        (when (buffer-live-p target-data)
          (kill-buffer target-data))))))

(niwa-deftest niwa-view-switch-conversation-variant-at-point ()
  ,test
  (test)
  :doc "switches the conversation variant for the exact fork point at point"
  (niwa-view-test--with-buffers
    (let (called)
      (with-current-buffer view-buf
        (cl-letf
            (((symbol-function 'niwa-view-fork-point-at-point)
              (lambda () '(:fork-point-id "fork-point-1")))
             ((symbol-function 'niwa-view-switch-conversation-variant)
              (lambda (fork-point-id)
                (setq called fork-point-id))))
          (niwa-view-switch-conversation-variant-at-point)))
      (should (equal "fork-point-1" called)))))


;;
;;; Full rendering

(ert-deftest niwa-view--debug-state/transcript-tail ()
  "Render diagnostics report transcript coordinates, not the draft."
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((end (point-max)))
        (niwa-view-test--insert-composer-draft "> existing\nmultiline draft")
        (let ((niwa-view-render-debug nil))
          (should-not (niwa-view--debug-state data-buf)))
        (let ((niwa-view-render-debug t))
          (should (= end (plist-get (niwa-view--debug-state data-buf) :input))))))))

(niwa-deftest niwa-view--full-rerender-reset ()
  ,test
  (test)
  :doc "clears stale display text and restores ordered zone markers"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (insert "stale display\n")
        (niwa-view--full-rerender-reset
         data-buf data-buf nil nil))
      (should-not (string-match-p "stale display" (niwa-view-render-test--view-text)))
      (should (= (marker-position niwa-view--status-marker)
                 (niwa-view-transcript-tail)))
      (should (= (marker-position niwa-view--interaction-marker)
                 (niwa-view-transcript-tail))))))

(niwa-deftest niwa-view--full-rerender-plan ()
  ,test
  (test)
  :doc "preparation preserves the view and returns the canonical source turns"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data data-buf "Response\n" 'response)
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let* ((before (buffer-string))
             (draft (niwa-view-composer-draft-text nil t))
             (position (point))
             (plan (niwa-view--full-rerender-plan data-buf data-buf view-buf nil)))
        (should (equal before (buffer-string)))
        (should (= position (point)))
        (should (equal draft (niwa-view-composer-draft-text nil t)))
        (should (= 4 (niwa-view-render-test--draft-offset)))
        (should (equal '(user assistant)
                       (mapcar (lambda (turn) (plist-get turn :role))
                               (plist-get plan :turns))))
        (should (= (plist-get plan :end) (with-current-buffer data-buf (point-max))))
        (should-not (plist-get plan :summary-source)))))
  :doc "respects the view's source start without permanently narrowing the data"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Old response\n" 'response)
    (let ((start (with-current-buffer data-buf (copy-marker (point-max) nil))))
      (niwa-view-test--insert-data data-buf "New prompt\n" nil)
      (niwa-view-test--insert-data data-buf "New response\n" 'response)
      (with-current-buffer view-buf
        (setq-local niwa-view--transcript-start start)
        (let ((plan (niwa-view--full-rerender-plan data-buf data-buf view-buf t)))
          (should (= (plist-get plan :start) start))
          (should-not (plist-get plan :session))
          (should (= (with-current-buffer data-buf (point-min)) 1))
          (should (= (plist-get (car (plist-get plan :turns)) :start) start)))))))

(niwa-deftest niwa-view--full-rerender-project ()
  ,test
  (test)
  :doc "projects transcript turns and reports the last assistant turn"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data data-buf "Response\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (niwa-view--full-rerender-reset data-buf data-buf nil nil)
        (let ((rendering
               (niwa-view--full-rerender-project
                data-buf data-buf view-buf nil nil nil)))
          (should (eq (plist-get rendering :view-buffer) view-buf))
          (should (eq (plist-get rendering :last-turn-role) 'assistant))
          (should (markerp
                   (plist-get rendering :last-assistant-turn-start)))
          (should (markerp
                   (plist-get rendering :last-assistant-turn-end)))
          (should (< (plist-get rendering :last-assistant-turn-start)
                     (plist-get rendering :last-assistant-turn-end)))
          (should (integerp
                   (plist-get rendering :last-assistant-turn-data-start)))
          (should (with-current-buffer data-buf
                    (<= (point-min)
                        (plist-get rendering :last-assistant-turn-data-start)
                        (point-max))))))
      (should (string-match-p "Response" (niwa-view-render-test--view-text)))))

  :doc "restored truncated bounds do not leak render data or audit text"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert ":PROPERTIES:\n:END:\n\nPrompt\n")
      (insert (propertize "Visible answer.\n" 'gptel 'response))
      (insert (niwa-tool-render-data-format
               '(:kind request-summary :elapsed-seconds 49.0)))
      (insert (niwa--format-hook-audit-record
               '(:type fork-point :fork-point-id "test-point"
                 :segment 1 :turn 1 :file-turn 1 :cum-turn 1)))
      ;; Model the saved truncated list, with correct pre-repair offsets.
      (dotimes (_ 5)
        (let ((bounds (gptel--get-buffer-bounds)))
          (setcdr (assq 'response bounds)
                  (append (cdr (assq 'response bounds)) (list (intern "..."))))
          (org-entry-put (point-min) "GPTEL_BOUNDS" (prin1-to-string bounds))))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (erase-buffer)
        (insert text))
      (niwa-transcript-restore-properties))
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (niwa-view--full-rerender-reset data-buf data-buf nil nil)
        (niwa-view--full-rerender-project
         data-buf data-buf view-buf nil nil nil))
      (should (string-match-p "Visible answer" (niwa-view-render-test--view-text)))
      (should-not (string-match-p
                   "niwa-render-data\\|niwa-hook-audit\\|elapsed-seconds"
                   (niwa-view-render-test--view-text)))))

  :doc "reports the current assistant turn's data start for reanchoring"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data data-buf "Response\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (niwa-view--full-rerender-reset data-buf data-buf nil nil)
        (let ((rendering
               (niwa-view--full-rerender-project
                data-buf data-buf view-buf nil 1 nil)))
          (should
           (integerp
            (plist-get rendering :last-current-assistant-turn-data-start)))
          (should
           (= (plist-get rendering :last-current-assistant-turn-data-start)
              (plist-get rendering :last-assistant-turn-data-start))))))))

(niwa-deftest niwa-view--full-rerender-reanchor ()
  ,test
  (test)
  :doc "prefers the projected current assistant as the live anchor"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((assistant-start (copy-marker (point-min) nil)))
        (niwa-view--full-rerender-reanchor
         data-buf
         (list :view-buffer view-buf
               :last-current-assistant-turn-start assistant-start
               :last-turn-role 'assistant)
         t 1 nil)
        (should (= (marker-position niwa-view--in-flight-turn-start)
                   (marker-position assistant-start))))))

  :doc "repairs the data anchor to the projected turn's data start"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\nfiller\n" nil)
    (with-current-buffer view-buf
      ;; Simulate a whole-buffer rewrite that collapsed the marker.
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min) nil)))
      (let ((assistant-start (copy-marker (point-min) nil))
            (data-start (with-current-buffer data-buf (1- (point-max)))))
        (niwa-view--full-rerender-reanchor
         data-buf
         (list :view-buffer view-buf
               :last-current-assistant-turn-start assistant-start
               :last-current-assistant-turn-data-start data-start
               :last-turn-role 'assistant)
         t 1 nil)
        (should (markerp niwa-view--data-turn-start))
        (should (eq (marker-buffer niwa-view--data-turn-start) data-buf))
        (should (= (marker-position niwa-view--data-turn-start)
                   data-start)))))

  :doc "parks the data anchor at the data end when no assistant turn exists"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\nfiller\n" nil)
    (with-current-buffer view-buf
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min) nil)))
      (niwa-view--full-rerender-reanchor
       data-buf
       (list :view-buffer view-buf :last-turn-role 'user)
       t nil nil)
      (should (markerp niwa-view--data-turn-start))
      (should (= (marker-position niwa-view--data-turn-start)
                 (with-current-buffer data-buf (point-max)))))))

(niwa-deftest niwa-view--reanchor-data-turn-start ()
  ,test
  (test)
  :doc "points the data anchor at a position in the data buffer"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (with-current-buffer view-buf
      (niwa-view--reanchor-data-turn-start data-buf 3)
      (should (markerp niwa-view--data-turn-start))
      (should (eq (marker-buffer niwa-view--data-turn-start) data-buf))
      (should (= (marker-position niwa-view--data-turn-start) 3))))

  :doc "releases the previous marker and ignores a nil position"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((old (with-current-buffer data-buf (copy-marker 1 nil))))
        (setq niwa-view--data-turn-start old)
        (niwa-view--reanchor-data-turn-start data-buf nil)
        (should (eq niwa-view--data-turn-start old))
        (niwa-view--reanchor-data-turn-start data-buf 1)
        (should-not (marker-buffer old))))))

(niwa-deftest niwa-view--full-rerender-finish ()
  ,test
  (test)
  :doc "rebuilds the input, status, interaction, and progress chrome"
  (niwa-view-test--with-buffers
    (let (calls)
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'niwa-view-refresh-input-prompt)
                   (lambda () (push 'input calls)))
                  ((symbol-function 'niwa-view--render-status-now)
                   (lambda (_buffer) (push 'status calls)))
                  ((symbol-function 'niwa-view--interaction-rebuild)
                   (lambda (owned visits)
                     (should owned)
                     (should-not visits)
                     (push 'interaction calls)))
                  ((symbol-function 'niwa-view--ensure-request-progress)
                   (lambda (_buffer) (push 'progress calls))))
          (niwa-view--full-rerender-finish
           data-buf data-buf (list :view-buffer view-buf) nil
           (float-time))))
      (should (equal (nreverse calls)
                     '(input status interaction progress)))))

  :doc "in-flight calls return with a live render of the turn"
  (niwa-view-test--with-buffers
    (let (scheduled live)
      (with-current-buffer view-buf
        (setq niwa-view--pending-tool-calls
              (list (list (niwa-view--pending-tool-claim-key
                           '(:name "Read" :args (:file_path "a")))
                          "Read" "a")))
        (cl-letf (((symbol-function 'niwa-view--schedule-tool-boundary-render)
                   (lambda (buffer) (push buffer scheduled)))
                  ((symbol-function 'niwa-view-render-live-update)
                   (lambda (buffer) (push buffer live)))
                  ((symbol-function 'niwa-view-stream-in-flight-turn-start-position)
                   (lambda () 1)))
          ;; Without a projection writer, the boundary render brings them back.
          (niwa-view--full-rerender-finish
           data-buf data-buf (list :view-buffer view-buf) nil (float-time))
          (should (equal (list data-buf) scheduled))
          (should-not live)
          (setq scheduled nil)
          ;; Under a writer the live render is queued before redisplay.
          (let ((niwa-view-render--owner t)
                (niwa-view--data-turn-start
                 (with-current-buffer data-buf (copy-marker (point-min)))))
            (niwa-view--full-rerender-finish
             data-buf data-buf (list :view-buffer view-buf) nil (float-time)))
          (should (equal (list data-buf) live))
          (should-not scheduled)
          ;; A historical projection draws no live turn.
          (niwa-view--full-rerender-finish
           data-buf data-buf (list :view-buffer view-buf) t (float-time))
          (should-not scheduled)
          (should (equal (list data-buf) live)))
        (setq niwa-view--pending-tool-calls nil)))))

(niwa-deftest niwa-view--strip-render-data-display-text ()
  ,test
  (test)
  :doc "preserves untrusted render-data delimiter lines as transcript text"
  (let* ((literal
          (substring-no-properties
           (niwa-tool-render-data-format
            '(:kind request-summary :elapsed-seconds 9))))
         (reasoning (propertize literal 'gptel 'ignore)))
    (should (equal literal
                   (substring-no-properties
                    (niwa-view--strip-render-data-display-text
                     reasoning))))))

(niwa-deftest niwa-view--full-rerender ()
  ,test
  (test)
  :doc "rebuilds view from data buffer"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "What is 2+2?\n" nil)
    (niwa-view-test--insert-data data-buf "The answer is 4.\n" 'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text1 (niwa-view-render-test--view-text)))
        (should (string-match-p "What is 2\\+2" text1))
        (niwa-view--full-rerender)
        (let ((text2 (niwa-view-render-test--view-text)))
          (should (string-match-p "What is 2\\+2" text2))
          (should (string-match-p "answer is 4" text2))))))

  :doc "repairs a data anchor collapsed by a whole-buffer rewrite"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data data-buf "Response\n" 'response)
    (with-current-buffer view-buf
      ;; Compaction and segment rotation rewrite the data buffer through
      ;; `erase-buffer', which moves every marker to `point-min'.  A stale
      ;; anchor made every later incremental render re-render the entire
      ;; transcript, so the rebuild must re-derive it from the projection.
      (niwa-view-stream-set-in-flight-turn-start (point-min))
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min) nil)))
      (niwa-view--full-rerender)
      (should (markerp niwa-view--data-turn-start))
      (should (> (marker-position niwa-view--data-turn-start)
                 (with-current-buffer data-buf (point-min))))
      ;; The repaired anchor points at the assistant turn, so the
      ;; response body is inside the live range it delimits.
      (should (with-current-buffer data-buf
                (save-excursion
                  (goto-char (point-min))
                  (let ((response-at (search-forward "Response" nil t)))
                    (and response-at
                         (>= response-at
                             (marker-position
                              (buffer-local-value
                               'niwa-view--data-turn-start view-buf)))))))))
    (with-current-buffer view-buf
      (niwa-view-stream-stop)))

  :doc "preserves an active transcript selection"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data
     data-buf "Response with selected text.\n" 'response)
    (with-current-buffer view-buf
      (setq-local transient-mark-mode t)
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "selected text")
      (set-mark (match-beginning 0))
      (activate-mark)
      (niwa-view--full-rerender)
      (should mark-active)
      (should (equal "selected text"
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))))))

  :doc "rolls back a destructive rebuild when projection fails"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data data-buf "Response.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'niwa-view--full-rerender-project)
                   (lambda (&rest _)
                     (error "Injected projection failure"))))
          (should-error (niwa-view--full-rerender)))
        (should (equal before (buffer-string))))))

  :doc "records elapsed timing when render debug is enabled"
  (let ((niwa-view-render-debug t)
        (niwa-view-render-debug-buffer-name
         " *niwa-view-full-rerender-test*"))
    (unwind-protect
        (niwa-view-test--with-buffers
          (niwa-view-test--insert-data data-buf "Prompt\n" nil)
          (niwa-view-test--insert-data data-buf "Response\n" 'response)
          (with-current-buffer view-buf
            (niwa-view--full-rerender))
          (with-current-buffer (get-buffer niwa-view-render-debug-buffer-name)
            (goto-char (point-min))
            (should (search-forward "full-rerender-after-render" nil t))
            (should (search-forward ":elapsed" nil t))
            (should (search-forward ":window-point" nil t))
            (should (search-forward ":window-start" nil t))
            (goto-char (point-min))
            (should (re-search-forward ":point [0-9]+" nil t))
            (should (search-forward ":point-fragment" nil t))
            (should (search-forward "zone-reconcile-begin" nil t))
            (should (search-forward "zone-reconcile-end" nil t))
            (should (search-forward ":fragment-ids" nil t))))
      (when-let* ((buf (get-buffer niwa-view-render-debug-buffer-name)))
        (kill-buffer buf))))
  :doc "renders old directive turns as ordinary expanded turns"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert
       (niwa--format-hook-audit-record
        '(:type directive-turn-boundary :edge start
          :directive-id "directive-1" :action discuss :turn 3)))
      (insert "Earlier question :discuss:\n:PROMPT:\nFull request\n:END:\n")
      (insert (propertize "Earlier answer.\n" 'gptel 'response))
      (insert
       (niwa--format-hook-audit-record
        '(:type directive-turn-boundary :edge end
          :directive-id "directive-1" :action discuss :turn 3
          :outcome success :activity-kind discussion :sequence 1)))
      (insert
       (niwa--format-hook-audit-record
        '(:type directive-turn-boundary :edge start
          :directive-id "directive-1" :action discuss :turn 4)))
      (insert "Explain this :discuss:\n:PROMPT:\nFull request\n:END:\n")
      (insert (propertize "Complete answer.\n" 'gptel 'response))
      (insert
       (niwa--format-hook-audit-record
        '(:type directive-turn-boundary :edge end
          :directive-id "directive-1" :action discuss :turn 4
          :outcome success :activity-kind discussion :sequence 1))))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should-not (string-search "RET: actions" text))
        (should-not (string-search "excluded from model context" text))
        (should-not (string-search "directive-turn-boundary" text))
        (should-not (string-search "Full request" text))
        (should (string-search "Earlier answer." text))
        (should (string-search "Complete answer." text))
        (should (= 2 (cl-count-if (lambda (line) (equal line "You"))
                                  (split-string text "\n"))))
        (should-not (text-property-any (point-min) (niwa-view-transcript-tail)
                                       'niwa-view-turn-role 'directive)))))

  :doc "suppresses modification hooks while rebuilding rendered transcript"
  (niwa-view-test--with-buffers
    (let ((changes 0))
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (niwa-view-test--insert-data data-buf "Response\n" 'response)
      (with-current-buffer view-buf
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (niwa-view--full-rerender)
        (should (= 0 changes))
        (should (string-match-p
                 "Response"
                 (niwa-view-render-test--view-text))))))
  :doc "suppresses intermediate redisplay while rebuilding the transcript"
  (niwa-view-test--with-buffers
    (let ((original (symbol-function 'niwa-view--header-string))
          redisplay-inhibited)
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'niwa-view--header-string)
                   (lambda (buffer)
                     (setq redisplay-inhibited inhibit-redisplay)
                     (funcall original buffer))))
          (niwa-view--full-rerender))
        (should redisplay-inhibited))))
  :doc "suppresses hooks while replacing stale running tool rows"
  (niwa-view-test--with-buffers
    (let ((changes 0))
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (niwa-view-test--insert-data data-buf "Response\n" 'response)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (niwa-view--history-insertion-marker))
            (niwa-view--with-render-boundaries-advancing
              (niwa-view--insert-pending-tool-row
               (list 'read "Read" "a.el")))))
        (should (text-property-any
                 (point-min) (niwa-view-transcript-tail)
                 'niwa-view-pending-tool-live t))
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (niwa-view--full-rerender)
        (should (= 0 changes))
        (should-not (text-property-any
                     (point-min) (niwa-view-transcript-tail)
                     'niwa-view-pending-tool-live t)))))
  :doc "restores task status fragment after full rerender"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'project
                :id "/tmp/view-task/"
                :root "/tmp/view-task/"
                :name "view-task"))
           (session (niwa-session-create "main" ws)))
      (setf (niwa-session-tasks session)
            (list (niwa-task--create
                   :id 1 :subject "visible task" :status 'pending)))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--view-buffer view-buf)
        (niwa-view-test--insert-data data-buf "Prompt\n" nil)
        (niwa-view-test--insert-data data-buf "Response\n" 'response)
        (niwa-tool-task-refresh-display))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (should (string-match-p "visible task" (niwa-view-render-test--view-text)))
        (niwa-view--full-rerender)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "visible task" text))
          (goto-char (point-min))
          (search-forward "visible task" (niwa-view-transcript-tail))
          (should (eq 'status (get-text-property
                               (1- (point))
                               'niwa-view-zone-namespace)))
          (should (eq 'tasks (get-text-property
                              (1- (point))
                              'niwa-view-zone-id)))))))
  :doc "rebuilds status and permission zones in order after full rerender"
  (niwa-view-test--with-buffers
      (let* ((ws (niwa-workspace--create
                  :type 'project
                  :id "/tmp/view-zones/"
                  :root "/tmp/view-zones/"
                  :name "view-zones"))
             (session (niwa-session-create "main" ws))
             (permission-outcomes nil))
        (setf (niwa-session-tasks session)
              (list (niwa-task--create
                     :id 1 :subject "visible zone task"
                     :status 'pending)))
        (setf (niwa-session-permission-queue session)
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
          (setq-local niwa--session session)
          (setq-local niwa--view-buffer view-buf)
          (niwa-view-test--insert-data data-buf "Prompt\n" nil)
          (niwa-view-test--insert-data data-buf "Response\n" 'response))
        (with-current-buffer view-buf
          (setq-local niwa--session session)
          ;; Render task status before the full rerender so the fragment
          ;; region is rebuilt along with the other chrome zones.
          (niwa-tool-task-refresh-display)
          (cl-letf (((symbol-function 'niwa-view--agent-status-collect)
                     (lambda ()
                       (list (list :path "/root/verifier"
                                   :status 'running)))))
            (niwa-view--full-rerender))
          (should-not permission-outcomes)
          (let* ((text (niwa-view-render-test--view-text))
                 (header (string-trim-right
                          (niwa-view--header-string data-buf)))
                 (header-pos (string-search header text))
                 (task-pos (string-search "visible zone task" text))
                 (agent-pos (string-search
                             "Running   agent    /root/verifier" text))
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
                                   (niwa-view-transcript-tail))))))))
  :doc "does not restore task block after full rerender when all tasks are completed"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'project
                :id "/tmp/view-task-completed/"
                :root "/tmp/view-task-completed/"
                :name "view-task-completed"))
           (session (niwa-session-create "main" ws)))
      (setf (niwa-session-tasks session)
            (list (niwa-task--create
                   :id 1 :subject "completed task" :status 'completed)))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--view-buffer view-buf)
        (niwa-view-test--insert-data data-buf "Prompt\n" nil)
        (niwa-view-test--insert-data data-buf "Response\n" 'response))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--full-rerender)
        (let ((text (niwa-view-render-test--view-text)))
          (should-not (string-match-p "tasks" text))
          (should-not (string-match-p "completed task" text))))))
  :doc "header stays at top when rerendering (the zone boundary advances past it)"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Greetings\n" nil)
    (niwa-view-test--insert-data data-buf "Hello back\n" 'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let* ((text (niwa-view-render-test--view-text))
             (header (niwa-view--header-string data-buf))
             (header-trim (string-trim-right header))
             (header-pos (string-search header-trim text))
             (greet-pos (string-search "Greetings" text)))
        (should header-pos)
        (should greet-pos)
        (should (< header-pos greet-pos)))))
  :doc "normalizes stale reasoning response prefixes during full rerender"
  (niwa-view-test--with-buffers
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
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "While the agents run, I'll test" text))
        (should-not (string-match-p "^le the agents run" text)))))
  :doc "normalizes restored props in read-only data buffers"
  (niwa-view-test--with-buffers
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
        (niwa-transcript-restore-properties t)
        (should buffer-read-only)
        (should (eq (get-text-property prefix-start 'gptel) 'response)))))
  :doc "renders generated system reminders as compact control rows"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "<system-reminder>\nCRITICAL: verify only.\nReport findings.\n</system-reminder>\n"
     'ignore)
    (with-current-buffer view-buf
      (let (collapsed-header)
        (niwa-view--full-rerender)
        (let ((text (niwa-view-render-test--view-text)))
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
        (should (eq (get-text-property (point) 'niwa-view-type)
                    'system-reminder-summary))
        (niwa-view-toggle-section)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p
                   (concat "^" (regexp-quote collapsed-header) "$")
                   text))
          (should (= 1 (niwa-view-test--count-substring
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
          (niwa-view-toggle-section))
        (let ((text (niwa-view-render-test--view-text)))
          (should (= 1 (niwa-view-test--count-substring
                        "System reminder" text)))
          (should-not (string-match-p "CRITICAL: verify only" text))
          (goto-char (point-min))
          (search-forward "System reminder")
          (should (equal collapsed-header
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))))))
  :doc "renders a historical fork disclosure as a generic collapsed row"
  ;; Old transcripts carry durable fork disclosure blocks; new forks
  ;; deliver provenance as an ephemeral reminder instead, so the block
  ;; gets no special treatment any more.
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
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
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "System reminder")
      (should (equal "  ◇ System reminder (4 lines)"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
      (goto-char (match-beginning 0))
      (should (get-text-property (point) 'niwa-view-collapsed))
      (should-not
       (string-match-p
        "/repo/missing.el"
        (niwa-view-render-test--view-text)))))
  :doc "keeps generated system reminders separate from real thinking"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Answer first.\n" 'response)
    (niwa-view-test--insert-data
     data-buf
     "<system-reminder>\nUse verification mode.\n</system-reminder>\n"
     'ignore)
    (niwa-view-test--insert-data data-buf "\n" nil)
    (niwa-view-test--insert-data
     data-buf
     "#+begin_reasoning\nInspect the diff.\n#+end_reasoning\n"
     'ignore)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "Answer first" text))
        (should (string-match-p "System reminder (1 line)" text))
        (should (string-match-p "Thinking... (1 lines)" text))
        (should-not (string-match-p "<system-reminder>" text)))))
  :doc "discloses the persisted expanded skill prompt after a full rerender"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Use $implement\n" nil)
    (niwa-view-test--insert-data
     data-buf
     (niwa-tool-render-data-format
      '(:kind inline-skill
        :display-text "Use $implement"
        :prompt
        "Prepared implementation instructions.\n\nHook-generated context."))
     'ignore)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "You\nUse \\$implement" text))
        (should-not (string-match-p "Prepared implementation" text)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (niwa-view-toggle-section)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "Prepared implementation instructions" text))
        (should (string-match-p "Hook-generated context" text)))))
  :doc "rows the attached-skill reminders leading an expanded skill prompt"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Use $doc\n" nil)
    (niwa-view-test--insert-data
     data-buf
     (niwa-tool-render-data-format
      '(:kind inline-skill
        :display-text "Use $doc"
        :attachments
        ((:name "artifact"
          :body "Attached artifact.")
         (:name "artifact-design"
          :body "Attached artifact-design.\nExample:\n<system-reminder>\nliteral reminder\n</system-reminder>\nAttachment tail."))
        :prompt "Prepared document instructions."))
     'ignore)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p
                 "2 attached skills (artifact, artifact-design)" text))
        (should-not (string-match-p "<system-reminder>" text))
        (should-not (string-match-p "Attached artifact\\." text)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (niwa-view-toggle-section)
      (let ((text (niwa-view-render-test--view-text)))
        ;; The reminders stay in their own row, not in the prompt drawer.
        (should (string-match-p "Prepared document instructions" text))
        (should-not (string-match-p "Attached artifact\\." text))
        (should-not (string-match-p "Attachment tail" text)))
      (goto-char (point-min))
      (search-forward "attached skills")
      (niwa-view-toggle-section)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "Attached artifact\\." text))
        (should (string-match-p "Attached artifact-design\\." text))
        (should (string-match-p "literal reminder" text))
        (should (string-match-p "Attachment tail" text)))
      ;; Collapsing rebuilds the header from the source record.
      (goto-char (point-min))
      (search-forward "attached skills")
      (niwa-view-toggle-section)
      (should (string-match-p
               "2 attached skills (artifact, artifact-design)"
               (niwa-view-render-test--view-text)))))
  :doc "keeps fork reminder and elapsed footers with their assistant turns"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "First prompt.\n\n" nil)
    (niwa-view-test--insert-data data-buf "First answer.\n\n" 'response)
    (niwa-view-test--insert-data
     data-buf
     (niwa-tool-render-data-format
      '(:kind request-summary :elapsed-seconds 7))
     'ignore)
    (niwa-view-test--insert-data
     data-buf
     "<system-reminder>\nConversation Fork\n</system-reminder>\n\n"
     'ignore)
    (niwa-view-test--insert-data data-buf "Second prompt.\n\n" nil)
    (niwa-view-test--insert-data data-buf "Second answer.\n\n" 'response)
    (niwa-view-test--insert-data
     data-buf
     (niwa-tool-render-data-format
      '(:kind request-summary :elapsed-seconds 3))
     'ignore)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let* ((text (niwa-view-render-test--view-text))
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
                               (niwa-view-transcript-tail)))))))
  :doc "closes a turn with its request summary as the labelled rule"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "First prompt.\n\n" nil)
    (niwa-view-test--insert-data data-buf "First answer.\n\n" 'response)
    (niwa-view-test--insert-data
     data-buf
     (niwa-tool-render-data-format
      '(:kind request-summary :elapsed-seconds 7))
     'ignore)
    (niwa-view-test--insert-data data-buf "Second prompt.\n\n" nil)
    (niwa-view-test--insert-data data-buf "Second answer.\n\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (let* ((label (progn (search-forward "── Worked for 7s ")
                           (match-beginning 0)))
             (label-end (point))
             (second-prompt (progn (search-forward "You\nSecond prompt")
                                   (match-beginning 0)))
             (turn-rules nil))
        ;; Blank line above the label, which ends in the extended rule.
        (should (equal "First answer.\n\n"
                       (buffer-substring-no-properties
                        (- label 15) label)))
        (should (eq 'niwa-view-request-summary-rule
                    (get-text-property label-end 'font-lock-face)))
        (should (eq 'niwa-view-separator
                    (get-text-property label 'font-lock-face)))
        ;; One blank row before the next heading, and no second rule.
        (should (equal "\n\n" (buffer-substring-no-properties
                               label-end second-prompt)))
        (let ((pos (point-min)))
          (while (< pos (niwa-view-transcript-tail))
            (when (eq (get-text-property pos 'font-lock-face)
                      'niwa-view-turn-rule)
              (push pos turn-rules))
            (setq pos (1+ pos))))
        ;; Only the summary-less second turn closes with the plain rule.
        (should (= 1 (length turn-rules)))
        (should (< second-prompt (car turn-rules))))))
  :doc "omits nested tool blocks from rendered reasoning text"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
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
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (= 2 (how-many "Thinking\\.\\.\\. (1 lines)"
                                (point-min)
                                (niwa-view-transcript-tail))))
        (should (string-match-p "Read: a.el" text))
        (should (string-match-p "I will inspect the file" text))
        (should (string-match-p "Then I will summarize" text))
        (should-not (string-match-p "tool result line" text))
        (should-not (string-match-p "(:name \\\"Read\\\"" text)))
      (goto-char (point-min))
      (search-forward "Thinking...")
      (niwa-view-toggle-section)
      (should-not (string-search "I will inspect the file" (niwa-view-render-test--view-text)))
      (should (string-search "Then I will summarize" (niwa-view-render-test--view-text)))
      (goto-char (point-min))
      (search-forward "Thinking...")
      (niwa-view-toggle-section)
      (search-forward "Read: a.el")
      (search-forward "Thinking...")
      (niwa-view-toggle-section)
      (should-not (string-search "Then I will summarize" (niwa-view-render-test--view-text)))
      (niwa-view-toggle-section)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "I will inspect the file" text))
        (should (string-match-p "Then I will summarize" text))
        (should-not (string-match-p "tool result line" text))
        (should-not (string-match-p "(:name \\\"Read\\\"" text)))))
  :doc "render-data-only segments after responses stay hidden"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Handoff\n\nBody.\n" nil)
    (niwa-view-test--insert-data data-buf "Assistant answer.\n" 'response)
    (niwa-view-test--insert-data
     data-buf
     (niwa-tool-render-data-format
      '(:kind inline-skill :name "handoff" :arguments ""
              :display-text "/handoff"))
     'ignore)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "Handoff" text))
        (should (string-match-p "Assistant answer" text))
        (should-not (string-match-p "niwa-render-data" text))
        (should-not (string-match-p "inline-skill" text))
        (should-not (string-match-p "Thinking" text)))))
  :doc "closes activity with a quiet inset rule before the next prose"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data data-buf "First answer.\n" 'response)
    (niwa-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"a.el\"))\n\ncontents\n"
     '(tool . "call_visual"))
    (niwa-view-test--insert-data data-buf "Second answer.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let (first-end tool-pos second-pos rules)
        (goto-char (point-min))
        (search-forward "First answer")
        (setq first-end (point))
        (search-forward "Read")
        (setq tool-pos (match-beginning 0))
        (search-forward "Second answer")
        (setq second-pos (match-beginning 0))
        (let ((pos (point-min)))
          (while (< pos second-pos)
            (when (eq (get-text-property pos 'font-lock-face)
                      'niwa-view-activity-rule)
              (push pos rules))
            (setq pos (1+ pos))))
        ;; One rule, after the tool and before the prose it precedes.
        (should (= 1 (length rules)))
        (should (< tool-pos (car rules) second-pos))
        ;; The announcing prose keeps its tool: a blank line, no rule.
        (should (string-match-p
                 "\\`\\.\n\n[^\n]*\\'"
                 (buffer-substring-no-properties first-end tool-pos)))
        ;; End of the tool row, blank line, inset rule row, then the prose.
        (should (equal "\n\n  \n"
                       (buffer-substring-no-properties
                        (- (car rules) 4) second-pos)))
        ;; The inset columns stay undecorated.
        (should-not (get-text-property (- (car rules) 2) 'font-lock-face))
        (should (eq 'activity-separator
                    (get-text-property (car rules) 'niwa-view-type))))))
  :doc "skips leading :PROPERTIES: drawer on data buffer"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      ;; Simulate gptel-org's state drawer at buffer start.
      (insert ":PROPERTIES:\n"
              ":GPTEL_MODEL: test\n"
              ":GPTEL_BOUNDS: ((response (100 200)))\n"
              ":END:\n\n"))
    (niwa-view-test--insert-data data-buf "Actual prompt\n" nil)
    (niwa-view-test--insert-data data-buf "Actual reply\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should-not (string-match-p ":GPTEL_MODEL:" text))
        (should-not (string-match-p ":PROPERTIES:" text))
        (should     (string-match-p "Actual prompt" text))
        (should     (string-match-p "Actual reply" text)))))

  :doc "restores saved GPTEL_BOUNDS before rendering a persisted segment"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert ":PROPERTIES:\n"
              ":GPTEL_MODEL: test\n"
              ":GPTEL_BOUNDS: nil\n"
              ":END:\n\n"
              "Prompt\n\n"
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
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "Prompt" text))
        (should (string-match-p "Assistant intro" text))
        (should (string-match-p "Read: \\(?:/tmp/\\)?a\\.png" text))
        (should (string-match-p "Assistant close" text))
        (should-not (string-match-p "GPTEL_BOUNDS" text))
        (should-not (string-match-p ":PROPERTIES:" text))
        (should-not (string-match-p "<media-file>" text))
        (should-not (string-match-p "(:name \"Read\"" text)))))

  :doc "preserves tool and thinking fold state across full rerender"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"input.pdf\"))\n\nfirst\n"
     '(tool . "call_0"))
    (niwa-view-test--insert-data data-buf "thinking\nmore thinking\n" 'ignore)
    (niwa-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"output.pdf\"))\n\nline\n"
     '(tool . "call_1"))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (save-excursion
        (goto-char (point-min))
        (search-forward "Thinking...")
        (should-not (get-text-property (point) 'niwa-view-collapsed))
        (niwa-view-toggle-section)
        (should (get-text-property (point) 'niwa-view-collapsed))
        (niwa-view-toggle-section)
        (should-not (get-text-property (point) 'niwa-view-collapsed))
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
        (niwa-view-toggle-section)
        (should-not (get-text-property (point) 'niwa-view-collapsed)))
      (niwa-view--full-rerender)
      (save-excursion
        (goto-char (point-min))
        (search-forward "thinking")
        (should-not (get-text-property (point) 'niwa-view-collapsed))
        (goto-char (point-min))
        (search-forward "input.pdf")
        (should-not (get-text-property (point) 'niwa-view-collapsed))
        (niwa-view-toggle-section)
        (should (get-text-property (point) 'niwa-view-collapsed)))
      (niwa-view--full-rerender)
      (save-excursion
        (goto-char (point-min))
        (search-forward "thinking")
        (should-not (get-text-property (point) 'niwa-view-collapsed))
        (goto-char (point-min))
        (search-forward "input.pdf")
        (should (get-text-property (point) 'niwa-view-collapsed)))))

  :doc "preserves independent tool-audit fold state across full rerender"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     (concat
      "(:name \"Read\" :args (:file_path \"audit.el\"))\n\ncontents\n"
      (niwa--format-hook-audit-record
       '(:type tool-input-rewrite
               :event "PreToolUse"
               :original-input (:file_path "before.el")
               :updated-input (:file_path "audit.el")
               :reason "first audit detail"))
      (niwa--format-hook-audit-record
       '(:type tool-result-rewrite
               :event "PostToolUse"
               :original-result "raw result"
               :updated-result "contents"
               :reason "second audit detail")))
     '(tool . "call_audit_state"))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "hook changed tool result")
      (niwa-view-toggle-section)
      (should (string-match-p "second audit detail" (niwa-view-render-test--view-text)))
      (should-not (string-match-p "first audit detail" (niwa-view-render-test--view-text)))
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "hook changed tool input")
      (should (get-text-property (point) 'niwa-view-collapsed))
      (search-forward "hook changed tool result")
      (should-not (get-text-property (point) 'niwa-view-collapsed))
      (should (string-match-p "second audit detail" (niwa-view-render-test--view-text)))
      (should-not (string-match-p "first audit detail" (niwa-view-render-test--view-text)))
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "hook changed tool input")
      (should (get-text-property (point) 'niwa-view-collapsed))
      (search-forward "hook changed tool result")
      (should-not (get-text-property (point) 'niwa-view-collapsed))))

  :doc "deduplicates renderer audit fallbacks and preserves their fold state"
  (niwa-view-test--with-buffers
    (niwa-tool-register
     (niwa-tool--create
      :name "FallbackAuditTool"
      :category "niwa"
      :renderer
      (lambda (_name _args result _data)
        (list
         :header "FallbackAuditTool: complete"
         :body result
         :initially-collapsed-p t
         :hook-audits
         '((:type tool-input-rewrite
                  :event "PreToolUse"
                  :original-input (:value "before")
                  :updated-input (:value "after")
                  :reason "fallback first detail")
           (:type tool-result-rewrite
                  :event "PostToolUse"
                  :original-result "raw"
                  :updated-result "clean"
                  :reason "fallback second detail")
           (:type tool-result-rewrite
                  :event "PostToolUse"
                  :original-result "raw"
                  :updated-result "clean"
                  :reason "fallback second detail"))))))
    (niwa-view-test--insert-data
     data-buf
     "(:name \"FallbackAuditTool\" :args nil)\n\ncomplete\n"
     '(tool . "call_fallback_audit_state"))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (should (= 1 (how-many "hook changed tool input")))
      (goto-char (point-min))
      (should (= 1 (how-many "hook changed tool result")))
      (goto-char (point-min))
      (search-forward "hook changed tool result")
      (niwa-view-toggle-section)
      (should (string-match-p "fallback second detail" (niwa-view-render-test--view-text)))
      (should-not (string-match-p "fallback first detail" (niwa-view-render-test--view-text)))
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "hook changed tool input")
      (should (get-text-property (point) 'niwa-view-collapsed))
      (search-forward "hook changed tool result")
      (should-not (get-text-property (point) 'niwa-view-collapsed))
      (should (string-match-p "fallback second detail" (niwa-view-render-test--view-text)))
      (should-not (string-match-p "fallback first detail" (niwa-view-render-test--view-text)))
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "hook changed tool input")
      (should (get-text-property (point) 'niwa-view-collapsed))
      (search-forward "hook changed tool result")
      (should-not (get-text-property (point) 'niwa-view-collapsed))))

  :doc "preserves source-backed agent handle state across full rerender"
  (niwa-view-test--with-buffers
    (niwa-tool-register
     (niwa-tool--create
      :name "FullStateAgent"
      :category "niwa"
      :renderer (lambda (_name _args result _data)
                  (list :header "Agent: verifier -- full state"
                        :body result
                        :body-mode 'text-mode
                        :vtype 'agent-handle
                        :initially-collapsed-p t))))
    (niwa-view-test--insert-data
     data-buf
     "(:name \"FullStateAgent\" :args (:task_name \"verify\"))\n\nfull rerender agent body\n"
     '(tool . "call_full_state_agent"))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "Agent: verifier -- full state")
      (goto-char (match-beginning 0))
      (niwa-view-toggle-section)
      (should (search-forward "full rerender agent body"
                              (niwa-view-transcript-tail) t))
      (let ((body-start (match-beginning 0)))
        (should (equal "    " (get-text-property body-start 'line-prefix)))
        (should (equal "    " (get-text-property body-start 'wrap-prefix))))
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "Agent: verifier -- full state")
      (goto-char (match-beginning 0))
      (should-not (get-text-property (point) 'niwa-view-collapsed))
      (should (search-forward "full rerender agent body"
                              (niwa-view-transcript-tail) t))))

  :doc "does not carry fold state to rewritten data at the same source start"
  (niwa-view-test--with-buffers
    (niwa-tool-register
     (niwa-tool--create
      :name "RewriteStateTool"
      :category "niwa"
      :renderer (lambda (_name _args result _data)
                  (list :header "RewriteStateTool: item"
                        :body result
                        :body-mode 'text-mode
                        :initially-collapsed-p t))))
    (niwa-view-test--insert-data
     data-buf
     "(:name \"RewriteStateTool\" :args (:id \"old\"))\n\nold expanded body\n"
     '(tool . "call_rewrite_old"))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "RewriteStateTool: item")
      (goto-char (match-beginning 0))
      (niwa-view-toggle-section)
      (should (search-forward "old expanded body"
                              (niwa-view-transcript-tail) t)))
    (with-current-buffer data-buf
      (erase-buffer)
      (niwa-view-test--insert-data
       data-buf
       "(:name \"RewriteStateTool\" :args (:id \"new\"))\n\nnew body must start collapsed\n"
       '(tool . "call_rewrite_new")))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "RewriteStateTool: item")
      (goto-char (match-beginning 0))
      (should (get-text-property (point) 'niwa-view-collapsed))
      (should-not (search-forward "new body must start collapsed"
                                  (niwa-view-transcript-tail) t))
      ;; Unmatched identities may remain for children hidden by group folds.
      ;; An explicit transcript-source change discards the whole table.
      (niwa-view--full-rerender nil t)
      (should (= 0 (hash-table-count niwa-view-disclosure--source-states)))))

  :doc "does not carry non-tool fold state to same-prefix rewritten data"
  (niwa-view-test--with-buffers
    (let ((prefix (make-string 300 ?x)))
      (niwa-view-test--insert-data
       data-buf (concat prefix "\nold thinking tail\n") 'ignore)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (should (search-forward "old thinking tail"
                                (niwa-view-transcript-tail) t))
        (niwa-view-toggle-section)
        (should-not (string-search "old thinking tail" (niwa-view-render-test--view-text))))
      (with-current-buffer data-buf
        (erase-buffer)
        (niwa-view-test--insert-data
         data-buf (concat prefix "\nnew thinking starts expanded\n")
         'ignore))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (should-not (get-text-property (point) 'niwa-view-collapsed))
        (should (search-forward "new thinking starts expanded"
                                (niwa-view-transcript-tail) t)))))

  :doc "reanchors in-flight assistant after restoring earlier expanded fold"
  (niwa-view-test--with-buffers
    (let (data-turn-start)
      (niwa-view-test--insert-data data-buf "First\n" nil)
      (niwa-view-test--insert-data
       data-buf
       "(:name \"Read\" :args (:file_path \"input.pdf\"))\n\nfirst\nsecond\nthird\n"
       '(tool . "call_0"))
      (niwa-view-test--insert-data data-buf "\n\nSecond\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (niwa-view-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (save-excursion
          (goto-char (point-min))
          (search-forward "input.pdf")
          (niwa-view-toggle-section)
          (should-not (get-text-property (point) 'niwa-view-collapsed)))
        (setq niwa-view--data-turn-start data-turn-start)
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view-transcript-tail)))
        (niwa-view--full-rerender)
        (should (markerp niwa-view--in-flight-turn-start))
        (save-excursion
          (goto-char niwa-view--in-flight-turn-start)
          (should (looking-at-p "Assistant"))
          (should (search-forward "Second response"
                                  (niwa-view-transcript-tail) t)))
        (niwa-view-test--insert-data data-buf "More text.\n" 'response)
        (niwa-view-render-live-update data-buf)
        (let ((text (niwa-view-render-test--view-text)))
          (should (= 2 (cl-count-if (lambda (line) (string= line "Assistant"))
                                    (split-string text "\n"))))
          (should (= 1 (cl-loop with start = 0
                                while (string-match "Second response" text start)
                                count t
                                do (setq start (match-end 0)))))))))

  :doc "turn fold preserves the in-flight assistant anchor"
  (niwa-view-test--with-buffers
    (let (data-turn-start second-start)
      (niwa-view-test--insert-data data-buf "First\n" nil)
      (niwa-view-test--insert-data
       data-buf
       "First response line 1.\nFirst response line 2.\nFirst response line 3.\n"
       'response)
      (niwa-view-test--insert-data data-buf "\n\nSecond\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (niwa-view-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (save-excursion
          (goto-char (point-min))
          (search-forward "Assistant")
          (search-forward "Assistant")
          (setq second-start (match-beginning 0)))
        (setq niwa-view--data-turn-start data-turn-start)
        (niwa-view-stream-set-in-flight-turn-start second-start)
        (save-excursion
          (goto-char (point-min))
          (search-forward "Assistant")
          (niwa-view-toggle-section))
        (should (markerp niwa-view--in-flight-turn-start))
        (niwa-view-test--insert-data data-buf "More text.\n" 'response)
        (niwa-view-render-live-update data-buf)
        (let ((text (niwa-view-render-test--view-text)))
          (should (= 1 (cl-count-if (lambda (line) (string= line "Assistant"))
                                    (split-string text "\n"))))
          (should (= 1 (cl-loop with start = 0
                                while (string-match "Second response" text start)
                                count t
                                do (setq start (match-end 0)))))))))

  :doc "shows a rotated segment's compaction summary only when expanded"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert "#+begin_summary niwa-role=compaction-summary\n")
        (put-text-property start (point) 'gptel 'ignore))
      (insert "Summary should stay out of view.\n")
      (let ((start (point)))
        (insert "#+end_summary\n\n")
        (put-text-property start (point) 'gptel 'ignore)))
    (niwa-view-test--insert-data data-buf "Actual prompt\n" nil)
    (niwa-view-test--insert-data data-buf "Actual reply\n" 'response)
    (with-current-buffer view-buf
      (cl-flet ((text () (niwa-view-render-test--view-text))
                (toggle () (save-excursion
                             (goto-char (point-min))
                             (search-forward "conversation compacted")
                             (niwa-view-toggle-section)))
                (count-marker ()
                  (niwa-view-test--count-substring
                   "conversation compacted"
                   (niwa-view-render-test--view-text))))
        (niwa-view--full-rerender)
        ;; Collapsed by default: the marker alone, no body or wrapper lines.
        (should-not (string-match-p "Summary should stay out of view" (text)))
        (should (= 1 (count-marker)))
        (should (string-match-p "Actual prompt" (text)))
        (should (string-match-p "Actual reply" (text)))
        ;; The marker says the row expands, and flips while it is open.
        (should (string-match-p "conversation compacted --- ▸" (text)))
        (toggle)
        (should (string-match-p "Summary should stay out of view" (text)))
        (should (string-match-p "conversation compacted --- ▾" (text)))
        (should-not (string-match-p "begin_summary\\|end_summary" (text)))
        (should (= 1 (count-marker)))
        ;; An explicit expansion survives a rerender.
        (niwa-view--full-rerender)
        (should (string-match-p "Summary should stay out of view" (text)))
        (should (= 1 (count-marker)))
        (toggle)
        (should-not (string-match-p "Summary should stay out of view" (text)))
        (should (string-match-p "conversation compacted --- ▸" (text)))
        (should (= 1 (count-marker))))))

  :doc "an uncompacted transcript shows no compaction row"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Actual prompt\n" nil)
    (niwa-view-test--insert-data data-buf "Actual reply\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (should-not (string-match-p "conversation compacted" (niwa-view-render-test--view-text)))
      (should (string-match-p "Actual reply" (niwa-view-render-test--view-text)))))

  :doc "hook audits attached to the summary render once beside the row"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert (niwa-session-artifacts-summary-block "Audited body") "\n"))
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (audit (list :type 'tool-context :event "PreToolUse"
                         :handlers (list (list :source 'project-file
                                               :description "Project handler"
                                               :reason "Why"
                                               :contexts (list "Ctx"))))))
        (niwa-view--insert-compaction-indicator
         view-buf data-buf
         (with-current-buffer data-buf (cons (point-min) (point-max)))
         (list audit)))
      (cl-flet ((text () (niwa-view-render-test--view-text)))
        (should (= 1 (niwa-view-test--count-substring
                      "conversation compacted" (text))))
        (should (= 1 (niwa-view-test--count-substring
                      "PreToolUse hook added context" (text))))
        (should-not (string-search "Audited body" (text)))
        (save-excursion
          (goto-char (point-min))
          (search-forward "conversation compacted")
          (niwa-view-toggle-section))
        (should (string-search "Audited body" (text)))
        (should (= 1 (niwa-view-test--count-substring
                      "PreToolUse hook added context" (text)))))))

  :doc "preserves in-flight live tail when data has no assistant replacement yet"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf "Read files\nAssistant\nCalling Read...\n" nil)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (start nil))
        (goto-char (niwa-view-transcript-tail))
        (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
        (setq start (point))
        (insert "Assistant\nCalling Read...\nAgent: reviewer done\n")
        (setq niwa-view--in-flight-turn-start (copy-marker start nil))
        (set-marker niwa-view--status-marker (point))
        (set-marker niwa-view--interaction-marker (point))
        (set-marker (niwa-view-transcript-tail-marker) (point))
        (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil))
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "Read files" text))
        (should (string-match-p "Assistant" text))
        (should (string-match-p "Calling Read" text))
        (should (string-match-p "reviewer done" text)))))

  :doc "stable-prefix matching stays within the latest assistant turn"
  (niwa-view-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (niwa-view-test--insert-data data-buf "First\n" nil)
      (niwa-view-test--insert-data
       data-buf "Shared first.\nShared second.\n" 'response)
      (niwa-view-test--insert-data data-buf "\n\nSecond\n" nil)
      (niwa-view-test--insert-data
       data-buf "Newest answer.\n" 'response)
      (niwa-view-test--insert-data
       data-buf "\n\nFollow-up\nAssistant\nShared first.\n" nil)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              start)
          (goto-char (niwa-view-transcript-tail))
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
          (setq start (point))
          (insert (concat "Assistant\n"
                          "Shared first.\n"
                          "Shared second.\n"
                          "Agent: reviewer done\n"))
          (setq niwa-view--in-flight-turn-start (copy-marker start nil))
          (set-marker niwa-view--status-marker (point))
          (set-marker niwa-view--interaction-marker (point))
          (set-marker (niwa-view-transcript-tail-marker) (point))
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil))
        (niwa-view-test--insert-composer-draft draft 4)
        (niwa-view--full-rerender)
        (let ((text (niwa-view-render-test--view-text)))
          (should (= 3 (niwa-view-test--count-substring
                        "Shared first." text)))
          (should (string-match-p "reviewer done" text)))
        (should (equal draft (niwa-view-composer-draft-text nil t)))
        (should (= 4 (niwa-view-render-test--draft-offset))))))

  :doc "loads persisted session summaries once for a full transcript render"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-view-summaries-" t)))
           (workspace
            (niwa-workspace--create
             :type 'project :id "test" :root root :name "test"))
           (session
            (niwa-session--create
             :name "source"
             :session-id "source-id"
             :save-path (file-name-concat root "sessions/source/")
             :workspace workspace
             :current-segment 1))
           (entries
            '((:save-path "/tmp/niwa-view-sessions/source/"
               :summary
               (:session-id "source-id"
                :fork-point-ids ("fork-point-1" "fork-point-2")))))
           (calls 0))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (insert (propertize "First response.\n" 'gptel 'response))
        (insert
         (niwa--format-hook-audit-record
          '(:type fork-point :fork-point-id "fork-point-1")))
        (insert "\nPrompt\n")
        (insert (propertize "Second response.\n" 'gptel 'response))
        (insert
         (niwa--format-hook-audit-record
          '(:type fork-point :fork-point-id "fork-point-2"))))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (cl-letf
            (((symbol-function
               'niwa-session-persistence-list-sessions)
              (lambda (_workspace &optional _cached)
                (cl-incf calls)
                entries)))
          (niwa-view--full-rerender))
        (should (= 1 calls)))
      (delete-directory root t)))

  :doc "loads no session summaries for a transcript with no fork point"
  ;; The enumeration costs several target round trips per session in the
  ;; workspace, and nothing reads it unless a fork point exists to carry a
  ;; variant button.
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-view-summaries-" t)))
           (workspace
            (niwa-workspace--create
             :type 'project :id "test" :root root :name "test"))
           (session
            (niwa-session--create
             :name "source"
             :session-id "source-id"
             :save-path (file-name-concat root "sessions/source/")
             :workspace workspace
             :current-segment 1))
           (calls 0))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (insert "Prompt\n")
        (insert (propertize "First response.\n" 'gptel 'response)))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (cl-letf
            (((symbol-function
               'niwa-session-persistence-list-sessions)
              (lambda (_workspace &optional _cached)
                (cl-incf calls)
                nil)))
          (niwa-view--full-rerender))
        (should (= 0 calls)))
      (delete-directory root t))))


(niwa-deftest niwa-view--rebase-data-sources ()
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
       `(niwa-view-source ,source
         niwa-view-source-key ,key))
      (setq-local niwa-view-disclosure--source-states
                  (make-hash-table :test #'equal))
      (puthash key t niwa-view-disclosure--source-states)
      (let ((tick (buffer-chars-modified-tick)))
        (niwa-view--rebase-data-sources 5)
        (should (= tick (buffer-chars-modified-tick))))
      (should (eq source
                  (get-text-property (point-min) 'niwa-view-source)))
      (should (equal '(15 . 25) source))
      (should (eq key
                  (get-text-property
                   (point-min) 'niwa-view-source-key)))
      (should (equal '(source tool-summary 15 (tool "call-1")) key))
      (should (gethash '(source tool-summary 15 (tool "call-1"))
                       niwa-view-disclosure--source-states))))
  :doc "leaves marker coordinates to track data-buffer edits themselves"
  (let ((data (generate-new-buffer " *niwa-rebase-data*")))
    (unwind-protect
        (with-temp-buffer
          (let ((start (with-current-buffer data (copy-marker (point-min))))
                (end (with-current-buffer data (copy-marker (point-max) t))))
            (insert "rendered")
            (put-text-property (point-min) (point-max)
                               'niwa-view-source (cons start end))
            (niwa-view--rebase-data-sources 5)
            (let ((source (get-text-property
                           (point-min) 'niwa-view-source)))
              (should (eq start (car source)))
              (should (eq end (cdr source))))))
      (kill-buffer data))))


(niwa-deftest niwa-view--history-tail-position ()
  ,test
  (test)
  :doc "copied response properties in a quoted draft cannot become history"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Question\n\n" nil)
    (let ((response-start (with-current-buffer data-buf (point-max))))
      (niwa-view-test--insert-data
       data-buf "Do you recall using Undo?\n" 'response)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let ((history-end (marker-position niwa-view--status-marker))
              (kill-ring nil)
              (kill-ring-yank-pointer nil)
              (interprogram-cut-function nil)
              (interprogram-paste-function nil))
          (goto-char (point-min))
          (search-forward "Assistant\n")
          (setq niwa-view--in-flight-turn-start
                (copy-marker (match-beginning 0)))
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf (copy-marker response-start)))
          (search-forward "Do you recall using Undo?")
          (kill-new (filter-buffer-substring
                     (match-beginning 0) (match-end 0)))
          (niwa-view-test--insert-composer-draft "> ")
          (with-current-buffer (niwa-view-composer-buffer view-buf)
            (yank)
            (niwa-view-composer-insert "\nsecond line\n" (point) view-buf))
          ;; Exercise actual copy/yank, not a hand-authored property stub.
          (should (eq 'response
                      (get-text-property 2 'niwa-view-type
                                         (niwa-view-composer-draft-text))))
          (niwa-view-render-test--goto-draft-offset 5)
          (let ((draft (niwa-view-composer-draft-text nil t))
                (tail (niwa-view--history-tail-position)))
            (niwa-view--render-status)
            (niwa-view-render-live-update data-buf)
            (should (equal draft (niwa-view-composer-draft-text nil t)))
            (should (= 5 (niwa-view-render-test--draft-offset)))
            (should-not (get-char-property (point-min) 'line-prefix))
            (should (= history-end tail)))))))

  :doc "agent inspection has no composer and scans the complete transcript"
  (with-temp-buffer
    (setq-local niwa-view--agent-transcript-p t)
    (insert (propertize "Assistant\nAnswer\n" 'niwa-view-type 'response))
    (save-excursion
      (goto-char (point-min))
      (niwa-view-transcript-tail-initialize))
    (should (= (point-max) (niwa-view--history-tail-position))))

  :doc "recovers the last history boundary by property runs"
  (with-temp-buffer
    (insert "Header\n")
    (let ((start (point)))
      (insert "Assistant answer\n")
      (put-text-property start (point) 'niwa-view-type 'response))
    (let ((expected (point))
          (original
           (symbol-function 'niwa-view--transcript-history-position-p))
          (calls 0))
      (insert (propertize "status\n" 'niwa-view-type 'agent-status))
      (insert (make-string 100000 ?x))
      (cl-letf (((symbol-function
                  'niwa-view--transcript-history-position-p)
                 (lambda (pos)
                   (cl-incf calls)
                   (funcall original pos))))
        (should (= expected (niwa-view--history-tail-position)))
        (should (< calls 10))))))

(niwa-deftest niwa-view--full-rerender-live-tail ()
  ,test
  (test)
  :doc "does not append preserved live tail already rendered from data"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data data-buf "Assistant answer.\n" 'response)
    (niwa-view-test--insert-data data-buf "\n\nFollow-up\n" nil)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            start)
        (goto-char (niwa-view-transcript-tail))
        (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
        (setq start (point))
        (insert "Assistant\nAssistant answer.\n")
        (setq niwa-view--in-flight-turn-start (copy-marker start nil))
        (set-marker niwa-view--status-marker (point))
        (set-marker niwa-view--interaction-marker (point))
        (set-marker (niwa-view-transcript-tail-marker) (point))
        (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil))
      (niwa-view--full-rerender)
      (niwa-view--full-rerender)
      (let* ((text (niwa-view-render-test--view-text))
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
          (goto-char niwa-view--in-flight-turn-start)
          (should (looking-at-p "Assistant"))))))

  :doc "matches preserved live tail by stable prefix when status lines differ"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data
     data-buf
     "Assistant answer.\n(:name \"Read\" :args (:file_path \"a.el\"))\n\nok\n"
     'response)
    (niwa-view-test--insert-data data-buf "\n\nFollow-up\n" nil)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            start)
        (goto-char (niwa-view-transcript-tail))
        (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
        (setq start (point))
        (insert "Assistant\nAssistant answer.\n✓ Agent: reviewer done · 9.9s · 3 calls\n")
        (setq niwa-view--in-flight-turn-start (copy-marker start nil))
        (set-marker niwa-view--status-marker (point))
        (set-marker niwa-view--interaction-marker (point))
        (set-marker (niwa-view-transcript-tail-marker) (point))
        (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil))
      (niwa-view--full-rerender)
      (let* ((text (niwa-view-render-test--view-text))
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
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'project
                       :id "view-task-rerender"
                       :root temporary-file-directory
                       :name "view-task-rerender"))
           (session (niwa-session-create "main" workspace)))
      (setf (niwa-session-tasks session)
            (list (niwa-task--create
                   :id 1 :subject "Inspect renderer" :status 'pending)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session))
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (niwa-view-test--insert-data data-buf "Assistant answer.\n" 'response)
      (niwa-view-test--insert-data data-buf "\n\nFollow-up\n" nil)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              start)
          (goto-char (niwa-view-transcript-tail))
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
          (setq start (point))
          (insert "Assistant\nAssistant answer.\n")
          (setq niwa-view--in-flight-turn-start (copy-marker start nil))
          (set-marker niwa-view--status-marker (point))
          (set-marker niwa-view--interaction-marker (point))
          (set-marker (niwa-view-transcript-tail-marker) (point))
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil))
        (niwa-view--full-rerender)
        (niwa-view--full-rerender)
        (let* ((text (niwa-view-render-test--view-text))
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
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (with-current-buffer view-buf
      (setq niwa-view--in-flight-turn-start
            (copy-marker (niwa-view-transcript-tail)))
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max) nil)))
      (niwa-view--start-spinner "Render in progress")
      (niwa-view--full-rerender)
      (should (string-search "Render in progress" (niwa-view-test--rail-text)))
      (should-not (string-search "Render in progress" (buffer-string)))
      (niwa-view--stop-spinner)
      (should-not (string-search "Render in progress" (niwa-view-test--rail-text)))
      (should (string-search "Prompt" (niwa-view-render-test--view-text)))))

  :doc "reanchors to current assistant when mailbox follows the in-flight turn"
  (niwa-view-test--with-buffers
    (let (data-turn-start)
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        ;; Match the send-path marker shape: inside the user run, just
        ;; before the assistant response starts.
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (niwa-view-test--insert-data data-buf "Assistant answer.\n" 'response)
      (niwa-view-test--insert-data
       data-buf
       "\n<agent-message sender=\"/root/explorer\" recipient=\"/root\">\nhello\n</agent-message>\n"
       nil)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              start)
          (goto-char (niwa-view-transcript-tail))
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
          (setq start (point))
          (insert "Assistant\nstale live tail\n")
          (setq niwa-view--in-flight-turn-start (copy-marker start nil))
          (set-marker niwa-view--status-marker (point))
          (set-marker niwa-view--interaction-marker (point))
          (set-marker (niwa-view-transcript-tail-marker) (point))
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil))
        (setq niwa-view--data-turn-start data-turn-start)
        (niwa-view--full-rerender)
        (let* ((text (niwa-view-render-test--view-text))
               (assistant-count
                (cl-count-if (lambda (line) (string= line "Assistant"))
                             (split-string text "\n"))))
          (should (string-match-p "Assistant answer" text))
          (should (string-match-p "hello" text))
          (should-not (string-match-p "stale live tail" text))
          (should (= 1 assistant-count))
          (save-excursion
            (goto-char niwa-view--in-flight-turn-start)
            (should (looking-at-p "Assistant")))))))
)

(ert-deftest niwa-view--full-rerender-in-flight-user-anchor/test ()
  "Full rerender during a new request keeps the in-flight anchor after `You'."
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "First\n" nil)
    (niwa-view-test--insert-data data-buf "First response.\n" 'response)
    (niwa-view-test--insert-data data-buf "\n\nSecond\n" nil)
    (with-current-buffer data-buf
      (let ((data-turn-start (copy-marker (point-max) nil)))
        (with-current-buffer view-buf
          (setq niwa-view--data-turn-start data-turn-start)
          (setq niwa-view--in-flight-turn-start
                (copy-marker (niwa-view-transcript-tail))))))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (should (string-match-p
               "Second"
               (niwa-view-render-test--view-text))))
    (niwa-view-test--insert-data data-buf "Second response.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view-render-live-update data-buf)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "First response" text))
        (should (string-match-p "Second" text))
        (should (string-match-p "Second response" text))))))


;;


;;
;;; Folding

(niwa-deftest niwa-view--render-request-summary-segment ()
  ,test
  (test)
  :doc "provider failures render expanded, retain the request ID, and fold"
  (niwa-view-test--with-buffers
    (let ((provider-message "Processing failed."))
      (with-current-buffer data-buf
        (insert
         (niwa-tool-render-data-format
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
          (goto-char (niwa-view-transcript-tail))
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
          (unwind-protect
              (niwa-view--render-request-summary-segment
               segment data-buf)
            (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil)))
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p
                   "Codex request failed · server_error" text))
          (should (string-match-p "HTTP/2 200" text))
          (should (string-match-p "provider-request-123" text))
          (should (string-match-p "Retry replays the prepared request" text))
          (should (string-match-p "Worked for 7s" text)))
        (goto-char (point-min))
        (search-forward "Codex request failed")
        (goto-char (match-beginning 0))
        (should-not (get-text-property (point) 'niwa-view-collapsed))
        (niwa-view-toggle-section)
        (should-not (search-forward "provider-request-123"
                                    (niwa-view-transcript-tail) t))
        (goto-char (point-min))
        (search-forward "Codex request failed")
        (goto-char (match-beginning 0))
        (niwa-view-toggle-section)
        (should (search-forward "provider-request-123"
                                (niwa-view-transcript-tail) t)))))

  :doc "plain-string provider failures remain visible"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert
       (niwa-tool-render-data-format
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
        (goto-char (niwa-view-transcript-tail))
        (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
        (unwind-protect
            (niwa-view--render-request-summary-segment segment data-buf)
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil)))
      (goto-char (point-min))
      (should (search-forward "Raw provider failure"
                              (niwa-view-transcript-tail) t))))

  :doc "an agent process exit renders one block that states its message once"
  (niwa-view-test--with-buffers
    (let ((message (concat "Agent process ended before completing request: "
                           "exited abnormally with code 1; agent stderr is in "
                           "~/.niwa/agent-stderr.log")))
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      ;; The record ACP turns persist when the agent process exits mid-prompt.
      (niwa-view-test--insert-data
       data-buf
       (niwa-tool-render-data-format
        `(:kind request-summary :elapsed-seconds 4693 :outcome error
          :backend "Claude Code" :status nil :error-type nil :error-code -32603
          :error-data (:message ,message :code -32603)
          :message ,message :retry-errors nil :retry-exhausted nil
          :retry manual))
       'ignore)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let ((text (niwa-view-render-test--view-text))
              (count (lambda (needle text)
                       (let ((start 0) (n 0))
                         (while (setq start (string-search needle text start))
                           (setq n (1+ n) start (1+ start)))
                         n))))
          (should (= 1 (funcall count "Claude Code request failed" text)))
          (should (= 1 (funcall count message text)))
          (should (string-search "Code: -32603" text))
          ;; Provider data restating the message and code adds nothing.
          (should-not (string-search "Provider data" text))
          (should (= 1 (funcall count "Retry replays the prepared request" text)))))))

  :doc "a failure after partial prose hangs below it like other activity"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data data-buf "Partial prose.\n" 'response)
    (niwa-view-test--insert-data
     data-buf
     (niwa-tool-render-data-format
      '(:kind request-summary :elapsed-seconds 7 :outcome error
        :backend "Provider" :message "Connection lost" :retry manual))
     'ignore)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (dotimes (_ 3)
        (goto-char (point-min))
        (should (re-search-forward
                 "Partial prose\\.\n\n  [^\n]*Provider request failed"
                 (niwa-view-transcript-tail) t))
        (should (search-forward "── Worked for 7s" (niwa-view-transcript-tail) t))
        (goto-char (point-min))
        (search-forward "Provider request failed")
        (niwa-view-toggle-section)))))

(niwa-deftest niwa-view-render-live-update ()
  ,test
  (test)

  :doc "preserves an active selection while rebuilding the assistant turn"
  (niwa-view-test--with-buffers
    (let (assistant-start)
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (niwa-view-test--insert-data
       data-buf "Response with selected text.\n" 'response)
      (with-current-buffer view-buf
        (setq-local transient-mark-mode t)
        (niwa-view--full-rerender)
        (setq niwa-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq niwa-view--in-flight-turn-start
              (copy-marker (match-beginning 0) nil))
        (search-forward "selected text")
        (set-mark (match-beginning 0))
        (activate-mark))
      (niwa-view-test--insert-data data-buf "Stream tail.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view-render-live-update data-buf)
        (should mark-active)
        (should (equal "selected text"
                       (buffer-substring-no-properties
                        (region-beginning) (region-end)))))))

  :doc "keeps completed render units while extending the live tail"
  (niwa-view-test--with-buffers
    (let ((fontify (symbol-function 'niwa-view--fontify-response))
          (stable-fontify-count 0)
          assistant-start stable-marker stable-position)
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point-max) nil)))
      (niwa-view-test--insert-data
       data-buf "#+begin_reasoning\nold thought\n#+end_reasoning\n" 'ignore)
      (niwa-view-test--insert-data data-buf "First paragraph.\n\nTail" 'response)
      (with-current-buffer view-buf
        (setq niwa-view--data-turn-start assistant-start)
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view-transcript-tail)))
        (cl-letf (((symbol-function 'niwa-view--fontify-response)
                   (lambda (text)
                     (when (string-search "First paragraph." text)
                       (cl-incf stable-fontify-count))
                     (funcall fontify text))))
          (niwa-view-render-live-update data-buf))
        (goto-char (point-min))
        (search-forward "Thinking...")
        (setq stable-marker (copy-marker (match-beginning 0) nil)
              stable-position (marker-position stable-marker)))
      (niwa-view-test--insert-data data-buf " grows." 'response)
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'niwa-view--fontify-response)
                   (lambda (text)
                     (when (string-search "First paragraph." text)
                       (cl-incf stable-fontify-count))
                     (funcall fontify text))))
          (niwa-view-render-live-update data-buf))
        (should (= 1 stable-fontify-count))
        (should (= stable-position (marker-position stable-marker)))
        (goto-char stable-marker)
        (should (looking-at-p "Thinking\\.\\.\\."))
        (should (search-forward "Tail grows." (niwa-view-transcript-tail) t)))
      (with-current-buffer data-buf
        (save-excursion
          (goto-char (point-min))
          (search-forward "First paragraph")
          (subst-char-in-region (match-beginning 0) (match-end 0) ?F ?X)))
      (with-current-buffer view-buf
        (should-not niwa-view--live-data-tail-start)
        (niwa-view-render-live-update data-buf)
        (goto-char (point-min))
        (should (search-forward "Xirst paragraph"
                                (niwa-view-transcript-tail) t)))))

  :doc "streamed reasoning stays visible between tool runs and preserves the draft"
  (niwa-view-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (with-current-buffer view-buf
        (niwa-view-stream-begin-turn
         niwa-view--status-marker
         (with-current-buffer data-buf (copy-marker (point-min))))
        (niwa-view-test--insert-composer-draft draft 3))
      (dotimes (i 6)
        (niwa-view-test--insert-data
         data-buf
         (format "(:name \"Read\" :args (:file_path \"f%d.el\"))\n\ncontent %d\n" i i)
         `(tool . ,(format "call_%d" i)))
        (with-current-buffer view-buf (niwa-view-render-live-update data-buf))
        (niwa-view-test--insert-data
         data-buf "#+begin_reasoning\nNext file\n#+end_reasoning\n" 'ignore)
        (with-current-buffer view-buf
          (niwa-view-render-live-update data-buf)
          (should (equal draft (niwa-view-composer-draft-text nil t)))
          (should (= 3 (niwa-view-render-test--draft-offset)))))
      (with-current-buffer view-buf
        (should-not (text-property-any (point-min) (niwa-view-transcript-tail)
                                       'niwa-view-type 'tool-group))
        (should (= 6 (niwa-view-test--count-substring
                      "Next file" (niwa-view-render-test--view-text))))
        (goto-char (point-min))
        (dotimes (i 6)
          (should (search-forward (format "f%d.el" i)
                                  (niwa-view-transcript-tail) t))
          (should (search-forward "Next file" (niwa-view-transcript-tail) t))
          (should-not (invisible-p (match-beginning 0))))
        (niwa-view-render-test--goto-draft-offset 3)
        (niwa-view--full-rerender)
        (should (= 6 (niwa-view-test--count-substring
                      "Next file" (niwa-view-render-test--view-text))))
        (should (equal draft (niwa-view-composer-draft-text nil t)))
        (should (= 3 (niwa-view-render-test--draft-offset))))))

  :doc "streamed prose after a tool keeps one activity rule, as a full render does"
  (niwa-view-test--with-buffers
    (let ((draft "> quoted\nsecond line")
          live)
      (cl-flet ((rules ()
                  (with-current-buffer view-buf
                    (let ((pos (point-min)) (count 0))
                      (while (< pos (niwa-view-transcript-tail))
                        (when (eq (get-text-property pos 'font-lock-face)
                                  'niwa-view-activity-rule)
                          (cl-incf count))
                        (setq pos (1+ pos)))
                      count)))
                (history ()
                  (with-current-buffer view-buf
                    (buffer-substring-no-properties
                     (point-min) (niwa-view--history-insertion-marker))))
                (update ()
                  (with-current-buffer view-buf
                    (niwa-view-render-live-update data-buf)
                    (should (equal draft (niwa-view-composer-draft-text nil t)))
                    (should (= 3 (niwa-view-render-test--draft-offset))))))
        (with-current-buffer view-buf
          (niwa-view-stream-begin-turn
           niwa-view--status-marker
           (with-current-buffer data-buf (copy-marker (point-min))))
          (niwa-view-test--insert-composer-draft draft 3))
        (niwa-view-test--insert-data data-buf "Checking the file.\n\n" 'response)
        (update)
        (niwa-view-test--insert-data
         data-buf "(:name \"Read\" :args (:file_path \"a.el\"))\n\ncontents\n"
         '(tool . "call_a"))
        (update)
        ;; Activity that follows prose gets a blank line, not a rule.
        (should (= 0 (rules)))
        (dolist (chunk '("The answer" " grows" " in chunks.\n"))
          (niwa-view-test--insert-data data-buf chunk 'response)
          (update)
          (should (= 1 (rules))))
        (setq live (history))
        (should (string-match-p "  \nThe answer grows in chunks\\." live))
        (with-current-buffer view-buf
          (niwa-view--full-rerender)
          (should (equal draft (niwa-view-composer-draft-text nil t))))
        (should (= 1 (rules)))
        (should (equal live (history))))))

  :doc "repair audits preserve expanded groups, child tools, cursor and window start"
  (dolist (interleaved '(nil t))
    (save-window-excursion
      (niwa-view-test--with-buffers
        (dotimes (i (if interleaved 8 4))
          (niwa-view-test--insert-data
           data-buf
           (format "(:name \"Read\" :args (:file_path \"f%d.el\"))\n\ncontent %d\n" i i)
           `(tool . ,(format "call_%d" i)))
          (when (and interleaved (= i 3))
            (niwa-view-test--insert-data
             data-buf "#+begin_reasoning\nNext file\n#+end_reasoning\n" 'ignore)))
        (with-current-buffer view-buf
          (switch-to-buffer view-buf)
          (niwa-view-stream-begin-turn
           niwa-view--status-marker
           (with-current-buffer data-buf (copy-marker (point-min))))
          (niwa-view-render-live-update data-buf)
          (goto-char (point-min))
          (search-forward "read 4 files")
          (niwa-view-toggle-section)
          (goto-char (point-min))
          (search-forward "f0.el")
          (niwa-view-toggle-section)
          (goto-char (point-min))
          (search-forward "content 0")
          (set-window-start nil (line-beginning-position) t)
          ;; The audit arrives at the next tool boundary while the reader
          ;; is inspecting an earlier expanded child of the activity group.
          (niwa-view-test--insert-data
           data-buf "(:name \"Read\" :args (:file_path \"next.el\"))\n\nnext\n"
           '(tool . "call-next"))
          (niwa-view-test--insert-data
           data-buf
           (niwa-tool-repair-format-audit-block
            'committed
            '((:rule wrap-array-singleton :source generic
                     :paths ((names)) :before string :after array)))
           'ignore)
          (dolist (refresh '(niwa-view-render-live-update
                             niwa-view-render-live-update
                             niwa-view--full-rerender
                             niwa-view-render-live-update))
            (if (eq refresh 'niwa-view--full-rerender)
                (funcall refresh)
              (funcall refresh data-buf))
            (should (string-match-p "read 4 files" (niwa-view-render-test--view-text)))
            (should (string-match-p "tool input repaired" (niwa-view-render-test--view-text)))
            (should (equal "content 0"
                           (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))))
            (should (= 9 (current-column)))
            (should (= (window-point) (point)))
            (should (= (window-start) (line-beginning-position))))))))

  :doc "an audited child keeps its expansion and cursor when promoted out of a group"
  (dolist (name '("Read" "ApplyPatch"))
    (niwa-view-test--with-buffers
      (when (equal name "ApplyPatch")
        (niwa-tool-register
         (niwa-tool--create :name name :category "niwa"
                              :renderer #'niwa-tool-patch--render)))
      (dotimes (i 5)
        (niwa-view-test--insert-data
         data-buf
         (concat
          "#+begin_tool
"
          (format "(:name %S :args (:file_path \"f%d.el\"))\n\ncontent %d\n"
                  (if (= i 4) name "Read") i i)
          (when (and (= i 4) (equal name "ApplyPatch"))
            (niwa-tool-render-data-format
             '(:kind patch :applied 1 :total 1
                     :files ((:path "f4.el" :kind update :added 1 :deleted 0
                                    :diff "+content 4\n")))
             "call_4"))
          "#+end_tool
")
         `(tool . ,(format "call_%d" i))))
      (with-current-buffer view-buf
        (niwa-view-stream-begin-turn
         niwa-view--status-marker
         (with-current-buffer data-buf (copy-marker (point-min))))
        (niwa-view-render-live-update data-buf)
        (goto-char (point-min))
        (search-forward (if (equal name "Read") "read 5 files" "ApplyPatch"))
        (niwa-view-toggle-section)
        (goto-char (point-min))
        (search-forward (if (equal name "Read") "f4.el" "ApplyPatch:"))
        (niwa-view-toggle-section)
        (goto-char (point-min))
        (search-forward "content 4")
        (let ((column (current-column))
              (line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (niwa-view-test--insert-data
           data-buf
           (niwa-tool-repair-format-audit-block
            'committed
            '((:rule wrap-array-singleton :source generic
                     :paths ((names)) :before string :after array)))
           'ignore)
          (dolist (refresh '(niwa-view-render-live-update
                             niwa-view--full-rerender))
            (if (eq refresh 'niwa-view--full-rerender)
                (funcall refresh)
              (funcall refresh data-buf))
            (should (string-match-p "read 4 files" (niwa-view-render-test--view-text)))
            (should (string-match-p "tool input repaired" (niwa-view-render-test--view-text)))
            (should (= column (current-column)))
            (should (equal line (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position))))))))))

(niwa-deftest niwa-view-render-invalidate-live-tail ()
  ,test
  (test)
  :doc "clears retained state and detaches both markers"
  (with-temp-buffer
    (let ((data-marker (copy-marker (point-min)))
          (view-marker (copy-marker (point-min)))
          (change-hook #'ignore))
      (setq-local niwa-view--live-data-tail-start data-marker)
      (setq-local niwa-view--live-view-tail-start view-marker)
      (setq-local niwa-view--live-source-change-hook change-hook)
      (add-hook 'before-change-functions change-hook nil t)
      (niwa-view-render-invalidate-live-tail)
      (should-not niwa-view--live-data-tail-start)
      (should-not niwa-view--live-view-tail-start)
      (should-not (memq change-hook before-change-functions))
      (should-not (marker-buffer data-marker))
      (should-not (marker-buffer view-marker)))))

(niwa-deftest niwa-view-render-initialize ()
  ,test
  (test)
  :doc "detaches an earlier source observer before resetting renderer state"
  (let ((data-buf (generate-new-buffer " *test-render-init-data*"))
        (view-buf (generate-new-buffer " *test-render-init-view*"))
        (change-hook #'ignore))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (add-hook 'before-change-functions change-hook nil t))
          (with-current-buffer view-buf
            (setq-local niwa-view--live-source-change-hook change-hook)
            (setq-local niwa-view--live-data-tail-start
                        (with-current-buffer data-buf
                          (copy-marker (point-min))))
            (setq-local niwa-view--live-view-tail-start
                        (copy-marker (point-min)))
            (niwa-view-render-initialize)
            (should-not niwa-view--live-data-tail-start)
            (should-not niwa-view--live-view-tail-start)
            (should-not niwa-view--live-source-change-hook))
          (with-current-buffer data-buf
            (should-not (memq change-hook before-change-functions))))
      (kill-buffer data-buf)
      (kill-buffer view-buf))))

(niwa-deftest niwa-view--live-tail-valid-p ()
  ,test
  (test)
  :doc "accepts only attached markers owned by the supplied data and view"
  (let ((data-buf (generate-new-buffer " *test-live-data*"))
        (other-buf (generate-new-buffer " *test-live-other*"))
        (view-buf (generate-new-buffer " *test-live-view*")))
    (unwind-protect
        (with-current-buffer view-buf
          (setq-local niwa-view--live-data-tail-start
                      (with-current-buffer data-buf
                        (copy-marker (point-min))))
          (setq-local niwa-view--live-view-tail-start
                      (copy-marker (point-min)))
          (should (niwa-view--live-tail-valid-p data-buf))
          (should-not (niwa-view--live-tail-valid-p other-buf))
          (set-marker niwa-view--live-view-tail-start nil)
          (should-not (niwa-view--live-tail-valid-p data-buf)))
      (kill-buffer data-buf)
      (kill-buffer other-buf)
      (kill-buffer view-buf))))

(niwa-deftest niwa-view--note-source-change ()
  ,test
  (test)
  :doc "invalidates the observing agent view instead of its retained parent"
  (niwa-view-test--with-buffers
    (let ((parent-view (generate-new-buffer " *test-parent-view*"))
          assistant-start)
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local niwa--view-buffer parent-view))
            (niwa-view-test--insert-data data-buf "Prompt\n" nil)
            (with-current-buffer data-buf
              (setq assistant-start (copy-marker (point-max) nil)))
            (niwa-view-test--insert-data
             data-buf "Stable paragraph.\n\nMutable tail" 'response)
            (with-current-buffer view-buf
              (setq niwa-view--data-turn-start assistant-start
                    niwa-view--in-flight-turn-start
                    (copy-marker (niwa-view-transcript-tail)))
              (niwa-view-render-live-update data-buf)
              (should niwa-view--live-data-tail-start))
            (with-current-buffer data-buf
              (save-excursion
                (goto-char (point-min))
                (search-forward "Stable")
                (subst-char-in-region
                 (match-beginning 0) (match-end 0) ?S ?X)))
            (with-current-buffer view-buf
              (should-not niwa-view--live-data-tail-start)))
        (kill-buffer parent-view)))))

(niwa-deftest niwa-view-render-settle ()
  ,test
  (test)
  :doc "reconciles the complete turn once and releases retained-tail state"
  (niwa-view-test--with-buffers
    (let (assistant-start stable-marker)
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point-max) nil)))
      (niwa-view-test--insert-data
       data-buf "Stable paragraph.\n\nMutable tail" 'response)
      (with-current-buffer view-buf
        (setq niwa-view--data-turn-start assistant-start)
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker) nil))
        (niwa-view-render-live-update data-buf)
        (goto-char (point-min))
        (search-forward "Stable paragraph.")
        (setq stable-marker (copy-marker (match-beginning 0) nil)))
      (niwa-view-test--insert-data data-buf " complete." 'response)
      (with-current-buffer view-buf
        (niwa-view-render-settle
         data-buf assistant-start
         (with-current-buffer data-buf (point-max)))
        (should-not niwa-view--live-data-tail-start)
        (should-not niwa-view--live-view-tail-start)
        (goto-char stable-marker)
        (should-not (looking-at-p "Stable paragraph\\."))
        (let ((text (niwa-view-render-test--view-text)))
          (should (= 1 (cl-count-if
                        (lambda (line) (string= line "Assistant"))
                        (split-string text "\n"))))
          (should (string-match-p "Mutable tail complete\\." text)))))))

(niwa-deftest niwa-view-collapse-state-survives-streaming ()
  ,test
  (test)
  :doc "expanded renderer-backed tool survives in-flight incremental render"
  (niwa-view-test--with-buffers
    (let (assistant-start view-assistant-start)
      (niwa-tool-register
       (niwa-tool--create
        :name "StateTool"
        :category "niwa"
        :renderer (lambda (_name _args result _data)
                    (list :header "StateTool: a.txt"
                          :body result
                          :body-mode 'text-mode
                          :initially-collapsed-p t))))
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (niwa-view-test--insert-data data-buf "Intro.\n" 'response)
      (niwa-view-test--insert-data
       data-buf
       "(:name \"StateTool\" :args (:path \"a.txt\"))\n\nexpanded tool body\n"
       '(tool . "call_state_tool"))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (setq niwa-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq view-assistant-start (match-beginning 0))
        (setq niwa-view--in-flight-turn-start
              (copy-marker view-assistant-start nil))
        (search-forward "StateTool: a.txt")
        (goto-char (match-beginning 0))
        (niwa-view-toggle-section)
        (should (search-forward "expanded tool body"
                                (niwa-view-transcript-tail) t)))
      (niwa-view-test--insert-data data-buf "Stream tail.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view-render-live-update data-buf)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "expanded tool body" text))
          (should (string-match-p "Stream tail" text))))))

  :doc "expanded short thinking survives in-flight incremental render"
  (niwa-view-test--with-buffers
    (let (assistant-start view-assistant-start)
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (niwa-view-test--insert-data data-buf "short thought\n" 'ignore)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (setq niwa-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq view-assistant-start (match-beginning 0))
        (setq niwa-view--in-flight-turn-start
              (copy-marker view-assistant-start nil))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (should (search-forward "short thought" (niwa-view-transcript-tail) t))
        (niwa-view-toggle-section)
        (should-not (string-search "short thought" (niwa-view-render-test--view-text)))
        (niwa-view-toggle-section)
        (should (search-forward "short thought"
                                (niwa-view-transcript-tail) t)))
      (niwa-view-test--insert-data data-buf "more streamed thinking\n" 'ignore)
      (with-current-buffer view-buf
        (niwa-view-render-live-update data-buf)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "short thought" text))
          (should (string-match-p "more streamed thinking" text))
          (should (string-match-p "Thinking\\.\\.\\." text)))
        (when (markerp niwa-view--data-turn-start)
          (set-marker niwa-view--data-turn-start nil))
        (setq niwa-view--data-turn-start nil)
        (when (markerp niwa-view--in-flight-turn-start)
          (set-marker niwa-view--in-flight-turn-start nil))
        (setq niwa-view--in-flight-turn-start nil)
        (niwa-view--full-rerender)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "short thought" text))
          (should (string-match-p "more streamed thinking" text))
          (should (string-match-p "Thinking\\.\\.\\." text))))))

  :doc "expanded source-backed agent handle survives in-flight incremental render"
  (niwa-view-test--with-buffers
    (let (assistant-start view-assistant-start)
      (niwa-tool-register
       (niwa-tool--create
        :name "StateAgent"
        :category "niwa"
        :renderer (lambda (_name _args result _data)
                    (list :header "Agent: verifier -- check state"
                          :body result
                          :body-mode 'text-mode
                          :vtype 'agent-handle
                          :initially-collapsed-p t))))
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (niwa-view-test--insert-data
       data-buf
       "(:name \"StateAgent\" :args (:task_name \"verify\"))\n\nagent body stays open\n"
       '(tool . "call_state_agent"))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (setq niwa-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq view-assistant-start (match-beginning 0))
        (setq niwa-view--in-flight-turn-start
              (copy-marker view-assistant-start nil))
        (search-forward "Agent: verifier -- check state")
        (goto-char (match-beginning 0))
        (niwa-view-toggle-section)
        (should (search-forward "agent body stays open"
                                (niwa-view-transcript-tail) t)))
      (niwa-view-test--insert-data data-buf "Agent stream tail.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view-render-live-update data-buf)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "agent body stays open" text))
          (should (string-match-p "Agent stream tail" text))))))

  :doc "expanded agent-result mailbox card survives in-flight incremental render"
  (let ((niwa-view-mailbox-collapse-line-threshold 1))
    (niwa-view-test--with-buffers
      (let (assistant-start view-assistant-start)
        (niwa-view-test--insert-data data-buf "Prompt\n" nil)
        (with-current-buffer data-buf
          (setq assistant-start (copy-marker (point) nil)))
        (niwa-view-test--insert-data data-buf "Before result.\n" 'response)
        (niwa-view-test--insert-data
         data-buf
         "\n<agent-result sender=\"/root/worker_state\" recipient=\"/root\">\nline one\nline two\n</agent-result>\n\n"
         nil)
        (with-current-buffer view-buf
          (niwa-view--full-rerender)
          (setq niwa-view--data-turn-start assistant-start)
          (goto-char (point-min))
          (search-forward "Assistant")
          (setq view-assistant-start (match-beginning 0))
          (setq niwa-view--in-flight-turn-start
                (copy-marker view-assistant-start nil))
          (search-forward "/root/worker_state")
          (goto-char (match-beginning 0))
          (niwa-view-toggle-section)
          (goto-char (point-min))
          (search-forward "line two")
          (should-not (get-text-property (match-beginning 0) 'invisible)))
        (niwa-view-test--insert-data data-buf "Result stream tail.\n" 'response)
        (with-current-buffer view-buf
          (niwa-view-render-live-update data-buf)
          (goto-char (point-min))
          (search-forward "line two")
          (should-not (get-text-property (match-beginning 0) 'invisible))
          (should (search-forward "Result stream tail"
                                  (niwa-view-transcript-tail) t))))))

  :doc "expanded task status survives streaming redraw and full rerender"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'project
                       :id "task-state-stream"
                       :root temporary-file-directory
                       :name "task-state-stream"))
           (session (niwa-session-create "main" workspace))
           assistant-start view-assistant-start)
      (setf (niwa-session-tasks session)
            (list (niwa-task--create
                   :id 1 :subject "active detail" :status 'pending)
                  (niwa-task--create
                   :id 2 :subject "finished detail" :status 'completed)))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--view-buffer view-buf))
      (with-current-buffer view-buf
        (setq-local niwa--session session))
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (niwa-view-test--insert-data data-buf "Task response.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (goto-char (point-min))
        (search-forward "active detail")
        (niwa-toggle-tasks)
        (should (search-forward "finished detail"
                                (niwa-view-transcript-tail) t))
        (setq niwa-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq view-assistant-start (match-beginning 0))
        (setq niwa-view--in-flight-turn-start
              (copy-marker view-assistant-start nil)))
      (niwa-view-test--insert-data data-buf "Task stream tail.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view-render-live-update data-buf)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "finished detail" text))
          (should (string-match-p "Task stream tail" text)))
        (niwa-view--full-rerender)
        (goto-char (point-min))
        (should (search-forward "finished detail"
                                (niwa-view-transcript-tail) t))))))



(niwa-deftest niwa-view-render-preserves-composer ()
  ,test
  (test)
  :doc "incremental redraw preserves a multiline leading-> composer draft"
  (niwa-view-test--with-buffers
    (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry))
          (gptel--known-tools (copy-tree gptel--known-tools))
          (draft "> quoted\nsecond line")
          data-turn-start)
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (niwa-view-test--insert-data data-buf "Assistant text.\n" 'response)
      (niwa-tool-ptc--register)
      (niwa-view-test--insert-data
       data-buf
       (concat "(:name \"ToolCall\" :args (:expression \"(Read :file_path \\\"x\\\")\"))\n\nread result"
               (niwa-tool-render-data-format
                '(:kind ptc :direct-tool "Read" :outcome completed
                  :calls ((:id "draft-envelope/1" :tool "Read" :args (:file_path "x")
                           :status success :result "read result"))) "draft-envelope"))
       '(tool . "draft-envelope"))
      (niwa-view-test--insert-data
       data-buf
       (concat
        (niwa--format-hook-audit-record
         '(:type provider-tool-batch-start :id "draft-envelope"))
        (niwa--format-hook-audit-record
         '(:type provider-tool-batch :id "draft-envelope"
           :messages [(:opaque "hidden provider representation")]))
        (niwa--format-hook-audit-record
         '(:type injected-reminders :phase mid-turn
           :items ((:type fixture :body "Retained guidance.")))))
       'niwa-hook-audit)
      (with-current-buffer view-buf
        (setq niwa-view--data-turn-start data-turn-start)
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view-transcript-tail)))
        (niwa-view-composer-insert draft)
        (niwa-view-render-test--goto-draft-offset 4)
        (niwa-view-render-live-update data-buf)
        (should (string= draft (niwa-view-composer-draft-text nil t)))
        (should-not (string-search "hidden provider representation" (niwa-view-render-test--view-text)))
        (should (= 4 (niwa-view-render-test--draft-offset)))
        (should-not (get-text-property 0 'read-only
                                      (niwa-view-composer-draft-text))))))
  :doc "incremental redraw preserves composer point in every live window"
  (niwa-view-test--with-buffers
    (let (data-turn-start)
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (niwa-view-test--insert-data data-buf "Assistant text.\n" 'response)
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((first-window (selected-window))
            (second-window (split-window-right)))
        (unwind-protect
            (progn
              (set-window-buffer second-window view-buf)
              (with-current-buffer view-buf
                (setq niwa-view--data-turn-start data-turn-start)
                (setq niwa-view--in-flight-turn-start
                      (copy-marker (niwa-view-transcript-tail))))
              (let ((first-composer (niwa-view-composer-display first-window))
                    (second-composer (niwa-view-composer-display second-window)))
                (with-selected-window first-composer
                  (with-current-buffer view-buf
                    (niwa-view-test--insert-composer-draft "draft" 2)))
                (with-selected-window second-composer
                  (with-current-buffer view-buf
                    (niwa-view-render-test--goto-draft-offset 3)))
                (with-current-buffer view-buf
                  (niwa-view-render-live-update data-buf)
                  (let ((start (niwa-view-composer-draft-start)))
                    (should (= (window-point first-composer) (+ start 2)))
                    (should (= (window-point second-composer) (+ start 3))))
                  (should (string= "draft" (niwa-view-composer-draft-text nil t))))))
          (delete-other-windows)))))
  :doc "full redraw preserves a multiline leading-> composer draft"
  (niwa-view-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (niwa-view-test--insert-data data-buf "Prompt\n" nil)
      (niwa-view-test--insert-data data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view-composer-insert draft)
        (niwa-view-render-test--goto-draft-offset 4)
        (niwa-view--full-rerender)
        (should (string= draft (niwa-view-composer-draft-text nil t)))
        (should (= 4 (niwa-view-render-test--draft-offset)))
        (should-not (get-text-property 0 'read-only
                                      (niwa-view-composer-draft-text))))))
  :doc "path-verification rerender preserves a multiline leading-> draft"
  (let ((file (make-temp-file "niwa-view-path-" nil ".el")))
    (unwind-protect
        (niwa-view-test--with-buffers
          (let ((draft "> quoted\nsecond line")
                (niwa-view-rerender-debounce 0))
            (niwa-view-test--insert-data data-buf "Prompt\n" nil)
            (niwa-view-test--insert-data data-buf "Assistant text.\n" 'response)
            (with-current-buffer view-buf
              (niwa-view-composer-insert draft)
              (niwa-view-render-test--goto-draft-offset 4)
              (setq niwa-view--path-pending (list file))
              (cl-letf (((symbol-function 'niwa-transport-run-when-idle)
                         (lambda (_key _path thunk &optional _on-cancel)
                           (funcall thunk)
                           t)))
                (niwa-view--verify-paths view-buf))
              (should (string= draft (niwa-view-composer-draft-text nil t)))
              (should (= 4 (niwa-view-render-test--draft-offset)))
              (should-not (get-text-property 0 'read-only
                                            (niwa-view-composer-draft-text))))))
      (delete-file file))))

(niwa-deftest niwa-view-render-navigation ()
  ,test
  (test)
  :doc "turn navigation crosses rendered source-backed turns"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "First\n" nil)
    (niwa-view-test--insert-data data-buf "Answer one.\n" 'response)
    (niwa-view-test--insert-data data-buf "\nSecond\n" nil)
    (niwa-view-test--insert-data data-buf "Answer two.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "You")
      (goto-char (match-beginning 0))
      (let ((first-source (get-text-property (point) 'niwa-view-source)))
        (niwa-view-next-turn)
        (should (get-text-property (point) 'niwa-view-source))
        (should-not (equal first-source
                           (get-text-property (point) 'niwa-view-source)))
        (niwa-view-prev-turn)
        (should (equal first-source
                       (get-text-property (point) 'niwa-view-source))))))
  :doc "next turn never follows source properties pasted into the draft"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((draft "> copied source\nsecond line"))
        (niwa-view-composer-insert
         (propertize draft 'niwa-view-source '(1 . 2)))
        (goto-char (point-min))
        (niwa-view-next-turn)
        (should (= (point-max) (point)))
        (should (equal draft (niwa-view-composer-draft-text nil t))))))
  :doc "transcript toggle selects the authoritative data buffer"
  (niwa-view-test--with-buffers
    (switch-to-buffer view-buf)
    (niwa-view-toggle-transcript)
    (should (eq (current-buffer) data-buf))))

(defun niwa-view-render-test--owner (symbol)
  "Return the source feature basename that defines SYMBOL."
  (file-name-base (or (symbol-file symbol 'defun) "")))

(niwa-deftest niwa-view-render-ownership ()
  ,test
  (test)
  :doc "owns canonical turn grouping"
  (should (equal "niwa-view-render"
                 (niwa-view-render-test--owner
                  'niwa-view--group-into-turns)))
  :doc "owns incremental and full transcript rendering"
  (dolist (symbol '(niwa-view-render-live-update
                    niwa-view-render-settle
                    niwa-view--full-rerender))
    (should (equal "niwa-view-render"
                   (niwa-view-render-test--owner symbol))))
  :doc "delegates source-backed disclosure while retaining navigation"
  (dolist (symbol '(niwa-view-toggle-section
                    niwa-view-disclosure-section-bounds))
    (should (equal "niwa-view-disclosure"
                   (niwa-view-render-test--owner symbol))))
  (dolist (symbol '(niwa-view-next-display
                    niwa-view-next-turn))
    (should (equal "niwa-view-render"
                   (niwa-view-render-test--owner symbol)))))

(niwa-deftest niwa-view--fontify-response ()
  ,test
  (test)
  :doc "preserves Markdown response text instead of converting it to Org"
  (let ((text (niwa-view--fontify-response
               "I’ll inspect `niwa-review.el` now.\n\n```r\neval(f[[3]], df)\n```")))
    (should (string-match-p "niwa-review\\.el" text))
    (should (string-match-p "```r" text))
    (should-not (string-match-p "#\\+begin_src" text)))

  :doc "preserves bracket indexing inside fenced code blocks"
  (let ((text (niwa-view--fontify-response
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
  (let ((text (niwa-view--fontify-response
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
  (let ((text (niwa-view--fontify-response
               "Use `f[[3]]` for the right-hand side.")))
    (should (string-match-p "f\\[\\[3\\]\\]" text))
    (should-not (string-match-p "f3" text)))

  :doc "still displays descriptive prose links"
  (let ((text (niwa-view--fontify-response
               "See [site](https://example.com) and `items[[3]]`.")))
    (should (string-match-p "\\[site\\](https://example\\.com)" text))
    (should (string-match-p "items\\[\\[3\\]\\]" text)))

  :doc "caches repeated response fontification in view buffers"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((calls 0))
        (cl-letf (((symbol-function 'niwa-view--markdown-fontify-mode)
                   (lambda () 'niwa-view-test-markdown-mode))
                  ((symbol-function 'niwa-view-test-markdown-mode)
                   (lambda ()
                     (cl-incf calls)
                     (fundamental-mode))))
          (should (string-match-p "cached"
                                  (niwa-view--fontify-response "cached")))
          (should (string-match-p "cached"
                                  (niwa-view--fontify-response "cached")))
          (should (= 1 calls)))))))

(niwa-deftest niwa-view--set-hide-markdown-markup ()
  ,test
  (test)

  :doc "changing the setting drops what was rendered under the old one"
  ;; The `invisible' property is baked in while fontifying, so a cached
  ;; rendering would otherwise keep the old look until a session restart.
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((rerendered 0)
            (original niwa-view-hide-markdown-markup))
        (unwind-protect
            (cl-letf (((symbol-function 'niwa-view--markdown-fontify-mode)
                       (lambda () 'fundamental-mode))
                      ((symbol-function 'niwa-view--full-rerender)
                       (lambda (&rest _) (cl-incf rerendered))))
              (should (niwa-view--markdown-fontify-target))
              (puthash 'stale "stale" niwa-view--response-fontify-cache)
              (puthash 'stale "stale" niwa-view--tool-rendering-cache)
              (customize-set-variable 'niwa-view-hide-markdown-markup
                                      (not original))
              (should-not niwa-view--markdown-fontify-buffer)
              (should (zerop (hash-table-count
                              niwa-view--response-fontify-cache)))
              (should (zerop (hash-table-count
                              niwa-view--tool-rendering-cache)))
              (should (= 1 rerendered))
              ;; Setting the same value again rebuilds nothing.
              (should (niwa-view--markdown-fontify-target))
              (customize-set-variable 'niwa-view-hide-markdown-markup
                                      (not original))
              (should niwa-view--markdown-fontify-buffer)
              (should (= 1 rerendered)))
          (setq-default niwa-view-hide-markdown-markup original)
          (niwa-view--release-markdown-fontify-buffer))))))


(niwa-deftest niwa-view--live-tail-lines-rendered-position ()
  ,test
  (test)

  :doc "finds live-tail lines separated by blank gaps"
  (with-temp-buffer
    (insert "before\n   alpha\n\n   beta\nafter\n")
    (should (= 11 (niwa-view--live-tail-lines-rendered-position
                  '("alpha" "beta") (point-max)))))

  :doc "does not build an overflowing regexp for long unmatched tails"
  (with-temp-buffer
    (dotimes (i 3000)
      (insert (format "line-%04d\n" i)))
    (let ((lines (mapcar (lambda (i) (format "missing-%04d" i))
                         (number-sequence 0 1500))))
      (should-not (niwa-view--live-tail-lines-rendered-position
                   lines (point-max)))))

  :doc "requires complete literal lines rather than prefixes or substrings"
  (with-temp-buffer
    (insert "Assistant\nokay\nbroken token\n")
    (should-not (niwa-view--live-tail-lines-rendered-position
                 '("Assistant" "ok") (point-max)))
    (should-not (niwa-view--live-tail-lines-rendered-position
                 '("ok") (point-max)))))

(niwa-deftest niwa-view--tool-segment-entry
  (:doc "activity reuse observes source edits and leaves coalescing counts independent")
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf "(:name \"Read\" :args (:file_path \"one.el\"))\n\nbody\n" '(tool . "entry"))
    (with-current-buffer view-buf
      (let* ((niwa-view--activity-entry-cache (make-hash-table :test #'equal))
             (segment (list 'tool 1 (with-current-buffer data-buf (point-max))))
             (entry (niwa-view--tool-segment-entry segment data-buf)))
        (should entry)
        (plist-put entry :count 7)
        (let ((again (niwa-view--tool-segment-entry segment data-buf)))
          (should (= 1 (plist-get again :count)))
          (plist-put again :count 9)
          (should (= 7 (plist-get entry :count))))
        (with-current-buffer data-buf
          (goto-char (point-min))
          (search-forward "one.el")
          (replace-match "two.el" t t))
        (should (string-match-p
                 "two.el"
                 (plist-get (plist-get (niwa-view--tool-segment-entry segment data-buf)
                                      :rendering) :header)))))))

(niwa-deftest niwa-view--render-tool-activity ()
  ,test
  (test)
  :doc "classification and insertion share each tool entry within one activity"
  (niwa-view-test--with-buffers
    (dotimes (i 2)
      (niwa-view-test--insert-data
       data-buf (format "(:name \"Read\" :args (:file_path \"f%d.el\"))\n\nbody\n" i)
       `(tool . ,(format "call-%d" i)))
      (niwa-view-test--insert-data
       data-buf "#+begin_reasoning\nThinking.\n#+end_reasoning\n" 'ignore))
    (let ((render (symbol-function 'niwa-view--segment-rendering))
          (calls 0))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 3)
        (cl-letf (((symbol-function 'niwa-view--segment-rendering)
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply render args))))
          (dotimes (_ 2)
            (setq calls 0)
            (niwa-view--full-rerender)
            (should (= calls 2))
            (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
            (should (= 3 (niwa-view-render-test--draft-offset))))))))
  :doc "a coalescing row needs individual presentation and splits its tool run"
  (dolist (mixed '(nil t))
    (niwa-view-test--with-buffers
      (let ((niwa-view-tool-group-collapse-threshold 1)
            (poll (niwa-tool--create
                   :name "PollBoundary" :category "niwa"
                   :renderer (lambda (&rest _)
                               '(:header "PollBoundary: latest"
                                 :body "latest" :coalesce-key "poll"))))
            (get-tool (symbol-function 'niwa-tool-get)))
        (dotimes (i 3)
          (niwa-view-test--insert-data
           data-buf
           (if (= i 1)
               "(:name \"PollBoundary\" :args nil)\n\nlatest\n"
             (format "(:name \"Read\" :args (:file_path \"f%d.el\"))\n\nbody\n" i))
           `(tool . ,(format "call-%d" i)))
          (when (and mixed (= i 1))
            (niwa-view-test--insert-data
             data-buf "#+begin_reasoning\nConsider it.\n#+end_reasoning\n"
             'ignore)))
        (niwa-view-test--insert-data data-buf "Done.\n" 'response)
        (cl-letf (((symbol-function 'niwa-tool-get)
                   (lambda (name &optional category)
                     (if (equal name "PollBoundary") poll
                       (funcall get-tool name category)))))
          (with-current-buffer view-buf
            (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
            (niwa-view--full-rerender)
            (should-not (text-property-any
                         (point-min) (niwa-view-transcript-tail)
                         'niwa-view-type 'tool-group))
            (should (string-search "PollBoundary: latest" (niwa-view-render-test--view-text)))
            (should (string-search "f0.el" (niwa-view-render-test--view-text)))
            (should (string-search "f2.el" (niwa-view-render-test--view-text)))
            (when mixed
              (should (string-search "Consider it." (niwa-view-render-test--view-text))))
            (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
            (should (= 4 (niwa-view-render-test--draft-offset))))))))

  :doc "substantive reasoning separates coalescing runs even beside hidden rows"
  (dolist (mixed '(nil t))
    (niwa-view-test--with-buffers
      (let ((niwa-view-tool-group-collapse-threshold 0)
            (poll (niwa-tool--create
                   :name "PollBoundary" :category "niwa"
                   :renderer (lambda (&rest _)
                               '(:header "PollBoundary: latest"
                                 :body "latest" :coalesce-key "poll"))))
            (hidden (niwa-tool--create
                     :name "HiddenBoundary" :category "niwa"
                     :renderer (lambda (&rest _) '(:header "hidden" :hidden-p t))))
            (get-tool (symbol-function 'niwa-tool-get)))
        (cl-loop for name in '("PollBoundary" "HiddenBoundary" "PollBoundary")
                 for i from 0 do
                 (niwa-view-test--insert-data
                  data-buf (format "(:name %S :args nil)\n\nlatest\n" name)
                  `(tool . ,(format "call-%d" i)))
                 (when (and mixed (= i 1))
                   (niwa-view-test--insert-data
                    data-buf "#+begin_reasoning\nConsider it.\n#+end_reasoning\n"
                    'ignore)))
        (niwa-view-test--insert-data data-buf "Done.\n" 'response)
        (cl-letf (((symbol-function 'niwa-tool-get)
                   (lambda (name &optional category)
                     (pcase name
                       ("PollBoundary" poll)
                       ("HiddenBoundary" hidden)
                       (_ (funcall get-tool name category))))))
          (with-current-buffer view-buf
            (niwa-view--full-rerender)
            (should (= (if mixed 2 1)
                       (niwa-view-test--count-substring
                        "PollBoundary: latest" (niwa-view-render-test--view-text))))
            (should (eq (not mixed)
                        (and (string-search "×2" (niwa-view-render-test--view-text)) t)))
            (should-not (string-search "HiddenBoundary" (niwa-view-render-test--view-text)))))))))

(niwa-deftest niwa-view--render-tool-call ()
  ,test
  (test)

  :doc "direct labels retain canonical children, envelope IDs and hook audits"
  (progn
    (niwa-tool-register
     (niwa-tool--create :name "ToolCall" :category "niwa"
                           :renderer #'niwa-tool-ptc--render))
    (dolist (name '("Eval" "Read" "Grep" "Glob" "Bash" "WebFetch"
                    "WebSearch" "DisplayProbe"))
      (dolist (status '(success error))
        (with-temp-buffer
          (let* ((args '(:expression "(+ 1 2)"))
                 (audit '(:type tool-result-rewrite :event "PostToolUse"
                          :original-result "raw" :updated-result "3"))
                 (data (list :kind 'ptc :direct-tool name :status status
                             :outcome (if (eq status 'error) 'tool-error 'completed)
                             :calls (list (list :id "outer/1" :source 'ptc
                                                :tool name :status status
                                                :args args :result "audit preview"))))
                 (call (list :name "ToolCall" :args args :result "3"
                             :tool-use-id "outer" :render-data data
                             :hook-audits (list audit)))
                 (original (copy-tree call)))
            (dolist (collapsed-only '(nil t))
              (let* ((row (niwa-view--render-tool-call
                           call (current-buffer) collapsed-only))
                     (child (plist-get row :group-child)))
                (should (equal name (plist-get row :group-tool)))
                (should (equal "outer" (plist-get row :tool-use-id)))
                (should (equal (list audit) (plist-get row :hook-audits)))
                (should (equal "ToolCall" (plist-get child :tool)))
                (should (equal args (plist-get child :args)))
                (should (equal "3" (plist-get child :result)))
                (should (eq status (plist-get child :status)))
                (should (equal data (plist-get child :render-data)))))
            (should (equal original call)))))))

  :doc "composed, live, malformed and failed envelopes retain ToolCall labels"
  (progn
    (niwa-tool-register
     (niwa-tool--create :name "ToolCall" :category "niwa"
                           :renderer #'niwa-tool-ptc--render))
    (dolist (change
             '((:direct-tool . nil) (:live-p . t) (:kind . other)
               (:outcome . script-error) (:outcome . denied)
               (:outcome . interrupted) (:status . error)
               (:direct-tool . "Read") (:calls . nil)
               (:calls . ((:tool "Eval" :status success)
                          (:tool "Read" :status success)))))
      (with-temp-buffer
        (let* ((data (plist-put
                      (copy-tree '(:kind ptc :direct-tool "Eval"
                                   :outcome completed :status success
                                   :calls ((:id "outer/1" :tool "Eval"
                                            :status success))))
                      (car change) (cdr change)))
               (call (list :name "ToolCall" :args '(:expression "(+ 1 2)")
                           :tool-use-id "outer" :result "3" :render-data data))
               (original (copy-tree call)))
          (dolist (collapsed-only '(nil t))
            (let ((row (niwa-view--render-tool-call
                        call (current-buffer) collapsed-only)))
              (should (equal "ToolCall" (plist-get row :group-tool)))
              (should (string-prefix-p "ToolCall" (plist-get row :header)))
              (should (equal "outer" (plist-get row :tool-use-id)))
              (should (equal data (plist-get (plist-get row :group-child)
                                             :render-data)))))
          (should (equal original call)))))))

(niwa-deftest niwa-view--render-tool-activity/rows ()
  ,test
  (test)
  :doc "fallback one-liner buttonizes existing file paths"
  (let* ((root (make-temp-file "niwa-view-fallback-linkify-" t))
         (file (file-name-concat root "niwa-goal.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "fallback-linkify"
                     :root root :name "fallback-linkify"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (erase-buffer)
              (insert "(:name \"Edit\" :args (:file_path \"niwa-goal.el\"))\n"
                      "Error: nope\n"))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (cl-letf (((symbol-function 'niwa-view--segment-rendering)
                           (lambda (&rest _) nil)))
                  (niwa-view--render-tool-activity
                   (list (list 'tool 1 (with-current-buffer data-buf (point-max))))
                   data-buf)))
              (goto-char (point-min))
              (should (search-forward "× Edit: niwa-goal.el (1 lines)" nil t))
              (goto-char (point-min))
              (search-forward "niwa-goal.el")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'niwa-view-path)))))))
      (delete-directory root t)))

  :doc "fallback one-liner leaves missing file paths plain"
  (let* ((root (make-temp-file "niwa-view-fallback-missing-" t))
         (workspace (niwa-workspace--create
                     :type 'project :id "fallback-missing"
                     :root root :name "fallback-missing"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (erase-buffer)
            (insert "(:name \"Edit\" :args (:file_path \"missing-file.el\"))\n"
                    "Error: nope\n"))
          (with-current-buffer view-buf
            (setq-local niwa--session session)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (cl-letf (((symbol-function 'niwa-view--segment-rendering)
                         (lambda (&rest _) nil)))
                (niwa-view--render-tool-activity
                 (list (list 'tool 1 (with-current-buffer data-buf (point-max))))
                 data-buf)))
            (goto-char (point-min))
            (should (search-forward "× Edit: missing-file.el (1 lines)" nil t))
            (goto-char (point-min))
            (search-forward "missing-file.el")
            (should-not (button-at (match-beginning 0)))))
      (delete-directory root t)))

  :doc "expanded saved state renders full body instead of collapsed cache"
  (niwa-view-test--with-buffers
    (let (source)
      (niwa-tool-register
       (niwa-tool--create
        :name "CacheStateTool"
        :category "niwa"
        :renderer (lambda (_name _args result _data)
                    (list :header "CacheStateTool: cached"
                          :body result
                          :body-mode 'text-mode
                          :initially-collapsed-p t))))
      (niwa-view-test--insert-data
       data-buf
       "(:name \"CacheStateTool\" :args (:path \"cached\"))\n\nbody must survive cache\n"
       '(tool . "call_cache_state"))
      (setq source (cons 1 (with-current-buffer data-buf (point-max))))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char (niwa-view-transcript-tail))
          (niwa-view--with-render-boundaries-advancing
            (niwa-view--render-tool-activity
             (list (list 'tool (car source) (cdr source))) data-buf)))
        (goto-char (point-min))
        (search-forward "CacheStateTool: cached")
        (let ((header-pos (match-beginning 0)))
          (should-not (search-forward "body must survive cache"
                                      (niwa-view-transcript-tail) t))
          (goto-char header-pos))
        (niwa-view-toggle-section)
        (should (search-forward "body must survive cache"
                                (niwa-view-transcript-tail) t))
        (let ((inhibit-read-only t))
          (delete-region (point-min) (niwa-view-transcript-tail))
          (goto-char (niwa-view-transcript-tail))
          (niwa-view--with-render-boundaries-advancing
            (niwa-view--render-tool-activity
             (list (list 'tool (car source) (cdr source))) data-buf)))
        (goto-char (point-min))
        (search-forward "CacheStateTool: cached")
        (should-not (get-text-property (match-beginning 0)
                                       'niwa-view-collapsed))
        (should (search-forward "body must survive cache"
                                (niwa-view-transcript-tail) t)))))

  :doc "direct Eval mixed with Read and Grep survives cached redraw and disclosure"
  (niwa-view-test--with-buffers
    (niwa-tool-register
     (niwa-tool--create :name "ToolCall" :category "niwa"
                           :renderer #'niwa-tool-ptc--render))
    (niwa-tool-register
     (niwa-tool--create :name "Eval" :category "niwa"
                           :renderer #'niwa-tool-exec--render-eval))
    (let* ((niwa-view-tool-group-collapse-threshold 1)
           (data '(:kind ptc :direct-tool "Eval" :outcome completed
                   :status success
                   :calls ((:id "outer/1" :source ptc :tool "Eval"
                            :status success :args (:expression "(+ 1 2)")
                            :result "audit preview"))))
           (heading "read 1 file, searched 1 pattern, evaluated 1 form")
           eval-start eval-end original)
      (niwa-view-test--insert-data
       data-buf "(:name \"Read\" :args (:file_path \"file.el\"))\n\nfile body\n"
       '(tool . "read"))
      (niwa-view-test--insert-data
       data-buf "(:name \"Grep\" :args (:pattern \"defun\"))\n\nfile.el:1:match\n"
       '(tool . "grep"))
      (setq eval-start (with-current-buffer data-buf (point-max)))
      (niwa-view-test--insert-data
       data-buf
       (concat "#+begin_tool\n"
               "(:name \"ToolCall\" :args (:expression \"(Eval :expression \\\"(+ 1 2)\\\")\"))\n\n3\n"
               (niwa-tool-render-data-format data "outer")
               "#+end_tool\n")
       '(tool . "outer"))
      (setq eval-end (with-current-buffer data-buf (point-max)))
      (with-current-buffer data-buf
        (niwa-transcript-restore-properties)
        (setq original (buffer-string)))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (niwa-view--full-rerender)
        (dotimes (_ 3)
          ;; Settled group summaries must use the cached name, not the hidden
          ;; audit or result payload, even on a complete redraw.
          (cl-letf (((symbol-function 'niwa-view--prepare-tool-segment)
                     (lambda (&rest _) (ert-fail "Reparsed a cached tool"))))
            (niwa-view--full-rerender)
            (let (cached)
              (maphash (lambda (_key rendering)
                         (when (and (equal "outer" (plist-get rendering :tool-use-id))
                                    (null (plist-get rendering :body)))
                           (setq cached rendering)))
                       niwa-view--tool-rendering-cache)
              (should cached)
              (let ((entry (list :start eval-start :end eval-end
                                 :rendering cached)))
                (should (equal "Eval" (plist-get cached :group-tool)))
                (should-not (plist-get cached :group-child))
                (should (equal "Eval" (plist-get
                                       (niwa-view--tool-group-child
                                        entry data-buf 2 t) :tool))))))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
          (should (= 4 (niwa-view-render-test--draft-offset)))
          (goto-char (point-min))
          (search-forward heading)
          (goto-char (match-beginning 0))
          (should (get-text-property (point) 'niwa-view-collapsed))
          (niwa-view-toggle-section)
          (goto-char (point-min))
          (search-forward "Eval: live (+ 1 2)")
          (goto-char (match-beginning 0))
          (should (equal (niwa-view-disclosure-source-range
                          data-buf eval-start eval-end)
                         (get-text-property (point) 'niwa-view-source)))
          (should-not (search-forward "audit preview"
                                      (niwa-view-transcript-tail) t))
          (goto-char (point-min))
          (search-forward "Eval: live (+ 1 2)")
          (goto-char (match-beginning 0))
          (niwa-view-toggle-section)
          (should (search-forward "3" (niwa-view-transcript-tail) t))
          (niwa-view--full-rerender)
          (goto-char (point-min))
          (search-forward "Eval: live (+ 1 2)")
          (goto-char (match-beginning 0))
          (should-not (get-text-property (point) 'niwa-view-collapsed))
          (niwa-view-toggle-section)
          (goto-char (point-min))
          (let ((group (text-property-any
                        (point-min) (niwa-view-transcript-tail)
                        'niwa-view-type 'tool-group)))
            (should group)
            (goto-char group)
            (niwa-view-toggle-section))
          (niwa-view-render-test--goto-draft-offset 4))
        (niwa-view--full-rerender)
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
        (goto-char (point-min))
        (should (search-forward heading (niwa-view-transcript-tail) t)))
      (with-current-buffer data-buf
        (should (equal-including-properties original (buffer-string)))
        (should (equal data (niwa-tool-render-data-for-tool data-buf "outer"))))))

  :doc "coalesced rows retain every source-ordered hook audit"
  (niwa-view-test--with-buffers
    (niwa-tool-register
     (niwa-tool--create
      :name "PollingTool"
      :category "niwa"
      :renderer
      (lambda (_name args _result _data)
        (list :header (format "Polled %s" (plist-get args :value))
              :expandable-p nil
              :coalesce-key "polling-tool"))))
    (with-current-buffer data-buf
      (dolist (spec
               '(("first"
                  (:type tool-input-rewrite
                   :event "PreToolUse"
                   :original-input (:value "raw")
                   :updated-input (:value "first")))
                 ("second"
                  (:type tool-result-rewrite
                   :event "PostToolUse"
                   :original-result "raw"
                   :updated-result "second"))))
        (let ((start (point))
              (value (car spec)))
          (insert (format
                   "(:name \"PollingTool\" :args (:value \"%s\"))\n\nok\n"
                   value)
                  (niwa--format-hook-audit-record (cadr spec)))
          (put-text-property
           start (point) 'gptel
           (cons 'tool (format "call-%s" value))))))
    (let ((data-before (with-current-buffer data-buf (buffer-string))))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let* ((visible (niwa-view-render-test--view-text))
               (input-audit (string-match "hook changed tool input" visible))
               (result-audit (string-match "hook changed tool result" visible)))
          (should-not (string-match-p "Polled first" visible))
          (should (string-match-p "Polled second ×2" visible))
          (should input-audit)
          (should result-audit)
          (should (< input-audit result-audit))))
      (with-current-buffer data-buf
        (should (equal data-before (buffer-string))))))

  :doc "retained-agent growth keeps adjacent collaboration disclosures exact"
  (niwa-view-test--with-buffers
    (niwa-tool-ui--register)
    (let* ((draft "> quoted\nsecond line")
           (niwa-view-agent-refresh-delay 0)
           (invocation
            (niwa-agent-invocation--create
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
        (niwa-view-test--insert-composer-draft draft 4))
      (cl-labels
          ((insert-tool
            (id call result &optional render-data)
            (with-current-buffer data-buf
              (let ((start (point)))
                (insert call "\n\n" result)
                (when render-data
                  (insert (niwa-tool-render-data-format
                           render-data id)))
                (put-text-property start (point) 'gptel (cons 'tool id))
                (list 'tool start (point)))))
           (render-tool
            (segment)
            (with-current-buffer view-buf
              (let ((inhibit-read-only t))
                (goto-char (niwa-view-transcript-tail))
                (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
                (unwind-protect
                    (niwa-view--render-tool-activity (list segment) data-buf)
                  (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil))))))
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
        (niwa-view-render-test--goto-draft-offset 4))
      (cl-letf (((symbol-function 'niwa-transcript-restore-properties)
                #'ignore)
                ((symbol-function 'niwa-view-rerender)
                 (lambda (&rest _)
                   (ert-fail "Unnecessary full redraw"))))
        (niwa-agent-conversation-refresh invocation)
        (let ((size (with-current-buffer data-buf (buffer-size))))
          (setf (niwa-agent-invocation-transcript-status invocation)
                'blocked)
          (niwa-agent-conversation-refresh invocation)
          (should (= size (with-current-buffer data-buf (buffer-size))))))
      (with-current-buffer data-buf
        ;; The handle's metadata changed in place; its activity history is
        ;; not persisted.
        (let ((render-data
               (niwa-tool-render-data-for-tool data-buf "call-agent")))
          (should (eq 'blocked (plist-get render-data :status)))
          (should-not (plist-member render-data :activity))))
      (with-current-buffer view-buf
        (should (equal draft (niwa-view-composer-draft-text nil t)))
        (should (= 4 (niwa-view-render-test--draft-offset)))
        (let ((pos (car (car (niwa-view--agent-handle-refresh-points
                             "/root/timer_patch_review")))))
          (should pos)
          (should (eq 'blocked
                      (get-text-property pos 'niwa-view-agent-status))))
        (goto-char (point-min))
        (search-forward "ListAgents: session (1 agent)")
        (goto-char (match-beginning 0))
        (niwa-view-toggle-section)
        (should (search-forward "/root/timer_patch_review  explorer  running"
                                (niwa-view-transcript-tail) t))
        (goto-char (point-min))
        (search-forward
         "SendMessage: /root/timer_patch_review (message queued)")
        (goto-char (match-beginning 0))
        (niwa-view-toggle-section)
        (should (search-forward "Detailed finding"
                                (niwa-view-transcript-tail) t))
        (let ((visible (niwa-view-render-test--view-text)))
          (should (= 1 (niwa-view-test--count-substring
                        "Started /root/timer_patch_review" visible)))
          (should (= 1 (niwa-view-test--count-substring
                        "ListAgents: session (1 agent)" visible)))
          (should (= 1 (niwa-view-test--count-substring
                        "SendMessage: /root/timer_patch_review (message queued)"
                        visible)))))))

  :doc "an intervening visible tool ends the coalescing run"
  (niwa-view-test--with-buffers
    (let (segments)
      (niwa-tool-register
       (niwa-tool--create
        :name "PollingTool"
        :category "niwa"
        :renderer
        (lambda (_name args _result _data)
          (list :header (format "Polled %s" (plist-get args :value))
                :expandable-p nil
                :coalesce-key "polling-tool"))))
      (niwa-tool-register
       (niwa-tool--create
        :name "VisibleTool"
        :category "niwa"
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
          (goto-char (niwa-view-transcript-tail))
          (niwa-view--render-tool-activity
           (nreverse segments) data-buf))
        (let ((visible (niwa-view-render-test--view-text)))
          (should (string-match-p "Polled first" visible))
          (should (string-match-p "Visible separator" visible))
          (should (string-match-p "Polled second" visible))
          (should-not (string-match-p "×2" visible)))))))

(niwa-deftest niwa-view--rendering-header-face
  (:doc "selects distinct faces for agent handle header states")
  ,test
  (test)

  :doc "running agent handles use the active running face"
  (should (eq 'niwa-view-agent-running
              (niwa-view--rendering-header-face
               '(:vtype agent-handle :agent-status running))))

  :doc "completed agent handles use the normal tool summary face"
  (should (eq 'niwa-view-tool-summary
              (niwa-view--rendering-header-face
               '(:vtype agent-handle :agent-status completed))))

  :doc "ordinary tool rows use the normal tool summary face"
  (should (eq 'niwa-view-tool-summary
              (niwa-view--rendering-header-face
               '(:vtype tool-summary)))))

(niwa-deftest niwa-view--rendering-header-line
  (:doc "styles renderer-provided tool headers like normal tool summaries")
  ,test
  (test)

  :doc "tool name and argument get distinct faces"
  (let ((line (niwa-view--rendering-header-line
               '(:vtype tool-summary :header "ToolSearch: Eval"))))
    (should (string-match "ToolSearch" line))
    (should (eq 'niwa-view-tool-name
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "Eval" line))
    (should (eq 'niwa-view-tool-argument
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "trailing line counts get the metadata face"
  (let ((line (niwa-view--rendering-header-line
               '(:vtype tool-summary
                 :header "Read: niwa-tools.el (95 lines)"))))
    (should (string-match "Read" line))
    (should (eq 'niwa-view-tool-name
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "niwa-tools.el" line))
    (should (eq 'niwa-view-tool-argument
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "(95 lines)" line))
    (should (eq 'niwa-view-tool-metadata
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "trailing match counts get the metadata face"
  (let ((line (niwa-view--rendering-header-line
               '(:vtype tool-summary
                 :header
                 "Grep: sanitize-gptel-bounds|GPTEL_BOUNDS (720 matches)"))))
    (should (string-match "Grep" line))
    (should (eq 'niwa-view-tool-name
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "sanitize-gptel-bounds|GPTEL_BOUNDS" line))
    (should (eq 'niwa-view-tool-argument
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "(720 matches)" line))
    (should (eq 'niwa-view-tool-metadata
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "diff count metadata keeps added and removed counts distinct"
  (let ((line (niwa-view--rendering-header-line
               '(:vtype tool-summary
                 :header "Edit: niwa-view.el (+1 -0)"))))
    (should (string-match "Edit" line))
    (should (eq 'niwa-view-tool-name
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "niwa-view.el" line))
    (should (eq 'niwa-view-tool-argument
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "(" line))
    (should (eq 'niwa-view-tool-metadata
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "\\+1" line))
    (should (eq 'niwa-view-tool-diff-added
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "-0" line))
    (should (eq 'niwa-view-tool-diff-removed
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "incomplete agent handles do not use a success marker"
  (let ((line (niwa-view--rendering-header-line
               '(:vtype agent-handle
                 :agent-status incomplete
                 :header "Agent: verifier"))))
    (should (string-match "…" line))
    (should (eq 'niwa-view-tool-metadata
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "tool errors use an error marker"
  (should (string-match-p
           "\\`  × Bash:"
           (niwa-view--rendering-header-line
            '(:header "Bash: npx test" :status error))))

  :doc "tool warnings color only the marker"
  (dolist (vtype '(tool-group tool-child tool-summary))
    (let ((line (niwa-view--rendering-header-line
                 (list :vtype vtype :header "Bash: run tests (4 lines)"
                       :status 'warning))))
      (should (string-match "!" line))
      (should (eq 'niwa-view-tool-warning
                  (get-text-property (match-beginning 0) 'font-lock-face line)))
      (dolist (label '("Bash" "run tests" "4 lines"))
        (should (string-match label line))
        (should-not (eq 'niwa-view-tool-warning
                        (get-text-property (match-beginning 0)
                                           'font-lock-face line))))))

  :doc "started agent paths are highlighted and clickable"
  (let ((line (niwa-view--rendering-header-line
               '(:vtype agent-handle :header "Started /root/reviewer"
                 :agent-path "/root/reviewer"))))
    (should (string-match "/root/reviewer" line))
    (should (eq 'link (get-text-property (match-beginning 0)
                                       'font-lock-face line)))
    (should (keymapp (get-text-property (match-beginning 0) 'keymap line)))))

;;; Rendering plist validation

(niwa-deftest niwa-view--rendering-plist-p ()
  ,test
  (test)
  :doc "accepts minimal plist with just :header"
  (should (niwa-view--rendering-plist-p '(:header "h")))
  :doc "accepts full plist with string body and symbol mode"
  (should (niwa-view--rendering-plist-p
           '(:header "h" :body "b" :body-mode diff-mode
                     :initially-collapsed-p t)))
  :doc "accepts status and non-expandable marker"
  (should (niwa-view--rendering-plist-p
           '(:header "h" :status error :expandable-p nil)))
  :doc "accepts a hidden rendering"
  (should (niwa-view--rendering-plist-p
           '(:header "h" :hidden-p t)))
  :doc "accepts a string coalescing key"
  (should (niwa-view--rendering-plist-p
           '(:header "h" :coalesce-key "poll:exec-1")))
  :doc "rejects missing :header"
  (should-not (niwa-view--rendering-plist-p '(:body "b")))
  :doc "rejects non-string :header"
  (should-not (niwa-view--rendering-plist-p '(:header 42)))
  :doc "rejects non-string :body"
  (should-not (niwa-view--rendering-plist-p '(:header "h" :body 42)))
  :doc "rejects non-symbol :body-mode"
  (should-not (niwa-view--rendering-plist-p
               '(:header "h" :body-mode "not-a-symbol")))
  :doc "rejects non-symbol :status"
  (should-not (niwa-view--rendering-plist-p
               '(:header "h" :status "error")))
  :doc "rejects non-boolean :expandable-p"
  (should-not (niwa-view--rendering-plist-p
               '(:header "h" :expandable-p maybe)))
  :doc "rejects non-boolean :hidden-p"
  (should-not (niwa-view--rendering-plist-p
               '(:header "h" :hidden-p maybe)))
  :doc "rejects non-string :coalesce-key"
  (should-not (niwa-view--rendering-plist-p
               '(:header "h" :coalesce-key poll))))


;;
;;; Renderer invocation

(niwa-deftest niwa-view--tool-render-status ()
  ,test
  (test)
  :doc "prefers structured error status over plain result text"
  (should (eq 'error
              (niwa-view--tool-render-status
               "plain failure" '(:status error))))
  :doc "prefers structured success status over legacy error prose"
  (should (eq 'success
              (niwa-view--tool-render-status
               "Error: visible text" '(:status success))))
  :doc "falls back to legacy result classification"
  (should (eq 'error
              (niwa-view--tool-render-status "Error: legacy failure"))))

(niwa-deftest niwa-view--invoke-renderer ()
  ,test
  (test)
  :doc "renders a direct expression as its specialist while preserving script rendering"
  (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry)))
    (niwa-tool-register
     (niwa-tool--create
      :name "DisplayProbe" :category "niwa"
      :renderer (lambda (name args result data)
                  (list :header (concat name ": " (plist-get args :value))
                        :body result :status (plist-get data :status)))))
    (let* ((tool (niwa-tool--create :name "ToolCall"
                                     :renderer #'niwa-tool-ptc--render))
           (audit '(:kind ptc :outcome completed :direct-tool "DisplayProbe"
                    :calls ((:id "envelope/1" :tool "DisplayProbe"
                             :args (:value "input") :status success :result "answer"))))
           (rendered (niwa-view--invoke-renderer tool audit nil "answer")))
      (should (equal "DisplayProbe: input" (plist-get rendered :header)))
      (should (equal "answer" (plist-get rendered :body)))
      (should (equal "envelope/1" (plist-get (car (plist-get audit :calls)) :id)))
      (let ((failed (plist-put (copy-sequence audit) :outcome 'script-error)))
        (setq failed (plist-put failed :status 'error))
        (let ((failure (niwa-view--invoke-renderer tool failed nil "Error: checkpoint failed")))
          (should (eq 'error (plist-get failure :status)))
          (should (string-search "checkpoint failed" (plist-get failure :body)))))
      (let ((live (plist-put (copy-sequence audit) :live-p t)))
        (should (string-prefix-p "ToolCall:"
                                 (plist-get (niwa-view--invoke-renderer tool live nil "") :header))))
      (setq audit (plist-put (copy-sequence audit) :direct-tool nil))
      (should (string-prefix-p "ToolCall: 1 call"
                               (plist-get (niwa-view--invoke-renderer tool audit nil "answer") :header)))))
  :doc "returns the renderer's plist on success"
  (let* ((tool (niwa-tool--create
                :name "R1"
                :renderer (lambda (_name _args _result _data)
                            (list :header "ok"
                                  :body "b"
                                  :body-mode 'diff-mode)))))
    (should (equal '(:header "ok" :body "b" :body-mode diff-mode)
                   (niwa-view--invoke-renderer
                    tool '(:kind diff) nil "result"))))
  :doc "invokes the renderer even when render-data is nil (output-driven renderers)"
  (let ((tool (niwa-tool--create
               :name "R2"
               :renderer (lambda (_name _args _result _data)
                           (list :header "x")))))
    (should (equal '(:header "x")
                   (niwa-view--invoke-renderer tool nil nil "ok"))))
  :doc "structured status fills a custom renderer's omitted visual status"
  (let ((tool (niwa-tool--create
               :name "StructuredVisual"
               :renderer (lambda (_name _args _result _data)
                           (list :header "x")))))
    (should (equal '(:header "x" :status error)
                   (niwa-view--invoke-renderer
                    tool '(:status error) nil "plain failure"))))
  :doc "structured status overrides a conflicting custom visual status"
  (let ((tool (niwa-tool--create
               :name "StructuredOverride"
               :renderer (lambda (_name _args _result _data)
                           (list :header "x" :status 'success)))))
    (should (equal '(:header "x" :status error)
                   (niwa-view--invoke-renderer
                    tool '(:status error) nil "plain failure"))))
  :doc "legacy custom renderer status remains authoritative without structure"
  (let ((tool (niwa-tool--create
               :name "LegacyVisual"
               :renderer (lambda (_name _args _result _data)
                           (list :header "x" :status 'error)))))
    (should (equal '(:header "x" :status error)
                   (niwa-view--invoke-renderer tool nil nil "plain"))))

  :doc "data-driven renderers can opt out by returning nil when render-data is absent"
  (let ((tool (niwa-tool--create
               :name "R2-data"
               :renderer (lambda (_name _args _result data)
                           (and data (list :header "only with data"))))))
    (should (null (niwa-view--invoke-renderer tool nil nil "ok"))))
  :doc "renderer alist dispatches by success status"
  (let* ((success-fn (lambda (_name _args _result _data)
                       (list :header "success")))
         (error-fn (lambda (_name _args _result _data)
                     (list :header "error")))
         (tool (niwa-tool--create
                :name "StatusDispatch"
                :renderer `((success . ,success-fn)
                            (error . ,error-fn)))))
    (should (equal '(:header "success")
                   (niwa-view--invoke-renderer tool nil nil "ok"))))
  :doc "renderer alist dispatches by error status"
  (let* ((success-fn (lambda (_name _args _result _data)
                       (list :header "success")))
         (error-fn (lambda (_name _args _result _data)
                     (list :header "error")))
         (tool (niwa-tool--create
                :name "StatusDispatchErr"
                :renderer `((success . ,success-fn)
                            (error . ,error-fn)))))
    (should (equal '(:header "error" :status error)
                   (niwa-view--invoke-renderer
                    tool nil nil "Error: bad"))))
  :doc "renderer alist honors structured error status without failure prose"
  (let* ((success-fn (lambda (_name _args _result _data)
                       (list :header "success")))
         (error-fn (lambda (_name _args _result _data)
                     (list :header "error")))
         (tool (niwa-tool--create
                :name "StructuredStatus"
                :renderer `((success . ,success-fn)
                            (error . ,error-fn)))))
    (should (equal '(:header "error" :status error)
                   (niwa-view--invoke-renderer
                    tool '(:status error) nil "plain failure"))))
  :doc "renderer alist falls back to default status"
  (let* ((default-fn (lambda (_name _args _result _data)
                       (list :header "default")))
         (tool (niwa-tool--create
                :name "StatusDefault"
                :renderer `((default . ,default-fn)))))
    (should (equal '(:header "default" :status error)
                   (niwa-view--invoke-renderer
                    tool nil nil "Error: bad"))))
  :doc "returns nil when tool has no renderer"
  (let ((tool (niwa-tool--create :name "NoRend" :renderer nil)))
    (should (null (niwa-view--invoke-renderer
                   tool '(:kind diff) nil "ok"))))
  :doc "renderer returning nil yields nil (silent fallback)"
  (let ((tool (niwa-tool--create
               :name "Declines"
               :renderer (lambda (_name _args _result _data) nil))))
    (should (null (niwa-view--invoke-renderer
                   tool '(:kind diff) nil "ok"))))
  :doc "renderer returning malformed plist yields nil and emits a warning"
  (let* ((tool (niwa-tool--create
                :name "Bad"
                :renderer (lambda (_name _args _result _data)
                            '(:body "no header"))))
         (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (should (null (niwa-view--invoke-renderer
                     tool '(:kind diff) nil "ok")))
      (should warnings)
      (should (eq 'niwa (caar warnings)))
      (should (string-match-p "malformed" (cadar warnings)))))
  :doc "renderer signalling an error yields nil and emits a warning"
  (let* ((tool (niwa-tool--create
	                :name "Boom"
	                :renderer (lambda (_name _args _result _data)
	                            (error "Oops"))))
         (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (should (null (niwa-view--invoke-renderer
                     tool '(:kind diff) nil "ok")))
      (should warnings)
      (should (eq 'niwa (caar warnings)))
      (should (string-match-p "failed" (cadar warnings))))))

(niwa-deftest niwa-view--invoke-renderer/diagnostic-scope ()
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (setq-local niwa--workspace (niwa-workspace--create :root "/project A/same/")))
    (with-current-buffer view-buf
      (let ((tool (niwa-tool--create
                   :name "Refusal"
                   :renderer (lambda (&rest _)
                               (user-error "%s" "niwa [/project A/same/]: Refused"))))
            (niwa--warn-once-table (make-hash-table :test #'equal))
            warnings)
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (_type text &rest _) (push text warnings))))
          (should-not (niwa-view--invoke-renderer tool nil nil "ok"))
          (should (= 1 (length warnings)))
          (should (string-prefix-p "niwa [/project A/same/]: " (car warnings)))
          (should-not (string-match-p "niwa.*niwa" (car warnings))))))))

(niwa-deftest niwa-view--tool-row-start-p ()
  ,test
  (test)
  :doc "accepts only a source-backed summary row for the ID before the limit"
  (with-temp-buffer
    (insert (propertize "row" 'niwa-view-source '(1 . 9)
                        'niwa-view-type 'tool-summary
                        'niwa-view-tool-use-id "id")
            (propertize "body" 'niwa-view-source '(1 . 9)
                        'niwa-view-type 'body
                        'niwa-view-tool-use-id "id")
            (propertize "loose" 'niwa-view-type 'tool-summary
                        'niwa-view-tool-use-id "id"))
    (should (niwa-view--tool-row-start-p 1 (point-max) "id"))
    (should-not (niwa-view--tool-row-start-p 1 1 "id"))
    (should-not (niwa-view--tool-row-start-p 1 (point-max) "other"))
    (should-not (niwa-view--tool-row-start-p 4 (point-max) "id"))
    (should-not (niwa-view--tool-row-start-p 8 (point-max) "id"))))

(niwa-deftest niwa-view--tool-row-start ()
  ,test
  (test)
  :doc "remembers a found row and rescans when the hint no longer starts it"
  (with-temp-buffer
    (let ((row (lambda (id)
                 (propertize (format "%s row\n" id) 'niwa-view-source '(1 . 9)
                             'niwa-view-type 'tool-summary
                             'niwa-view-tool-use-id id))))
      (insert "header\n" (funcall row "a") (funcall row "b"))
      (let ((b (niwa-view--tool-row-start "b" (point-max))))
        (should (equal "b row\n" (buffer-substring-no-properties b (point-max))))
        (should (= b (marker-position (gethash "b" niwa-view--tool-row-starts))))
        ;; A row inserted before it moves the remembered marker with it.
        (goto-char (point-min))
        (insert "more header\n")
        (should (= (+ b 12) (niwa-view--tool-row-start "b" (point-max))))
        ;; A hint moved into the middle of the row is not trusted.
        (set-marker (gethash "b" niwa-view--tool-row-starts) (+ b 14))
        (should (= (+ b 12) (niwa-view--tool-row-start "b" (point-max))))
        (should-not (niwa-view--tool-row-start "b" (+ b 12)))
        (should-not (niwa-view--tool-row-start "missing" (point-max))))))
  :doc "bounds remembered rows"
  (with-temp-buffer
    (dotimes (index (1+ niwa-view--tool-row-starts-limit))
      (insert (propertize "row\n" 'niwa-view-source '(1 . 9)
                          'niwa-view-type 'tool-summary
                          'niwa-view-tool-use-id (format "id%d" index))))
    (dotimes (index (1+ niwa-view--tool-row-starts-limit))
      (should (niwa-view--tool-row-start (format "id%d" index) (point-max))))
    (should (<= (hash-table-count niwa-view--tool-row-starts)
                niwa-view--tool-row-starts-limit))))

(niwa-deftest niwa-view--forget-tool-row-starts ()
  ,test
  (test)
  :doc "detaches every remembered marker"
  (with-temp-buffer
    (insert (propertize "row\n" 'niwa-view-source '(1 . 9)
                        'niwa-view-type 'tool-summary
                        'niwa-view-tool-use-id "id"))
    (niwa-view--tool-row-start "id" (point-max))
    (let ((marker (gethash "id" niwa-view--tool-row-starts)))
      (niwa-view--forget-tool-row-starts)
      (should-not (marker-buffer marker))
      (should (= 0 (hash-table-count niwa-view--tool-row-starts)))))
  :doc "does nothing before any row was remembered"
  (with-temp-buffer
    (should-not (niwa-view--forget-tool-row-starts))))

(niwa-deftest niwa-view--tool-row-region ()
  ,test
  (test)
  :doc "finds the source-backed row for one tool use"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool (Custom :value \"x\")\n")
      (let ((start (point)))
        (insert "(:name \"Custom\" :args (:value \"x\"))\n\ninitial")
        (put-text-property start (point) 'gptel '(tool . "call-custom")))
      (insert "\n#+end_tool\n"))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let* ((region (niwa-view--tool-row-region
                      data-buf "call-custom"))
             (source (nth 2 region)))
        (should region)
        (with-current-buffer data-buf
          (let ((raw (niwa-tool-render-data-segment-bounds "call-custom")))
            ;; The canonical tool span includes its structural wrapper, not
            ;; just the narrower gptel property run recovered from disk.
            (should (equal source (cons (point-min) (point-max))))
            (should (< (car source) (car raw)))
            (should (< (cdr raw) (cdr source)))
            (let ((call (niwa-view--tool-call-parse
                         data-buf (car source) (cdr source))))
              (should (equal "call-custom" (plist-get call :tool-use-id)))
              (should (equal "Custom" (plist-get call :name)))
              (should (equal "initial" (plist-get call :result))))))
        (should
         (equal "call-custom"
                (get-text-property
                 (car region) 'niwa-view-tool-use-id))))))
  :doc "revalidates a remembered row start after the projection moves it"
  (niwa-view-test--with-buffers
    (let ((niwa-view-tool-group-collapse-threshold 100))
      (dolist (id '("first" "second"))
        (niwa-view-test--insert-data
         data-buf
         (format "#+begin_tool\n(:name \"Custom\" :args (:value %S))\n\nresult\n#+end_tool\n" id)
         (cons 'tool id)))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let ((region (niwa-view--tool-row-region data-buf "second")))
          (should (equal region (niwa-view--tool-row-region data-buf "second")))
          ;; A stale hint inside another row is not trusted.
          (set-marker (gethash "second" niwa-view--tool-row-starts)
                      (car (niwa-view--tool-row-region data-buf "first")))
          (should (equal region (niwa-view--tool-row-region data-buf "second"))))
        (with-current-buffer data-buf
          (goto-char (point-min))
          (insert (propertize "Earlier response.\n" 'gptel 'response)))
        (niwa-view--full-rerender)
        (let ((region (niwa-view--tool-row-region data-buf "second")))
          (should (equal "second"
                         (get-text-property (car region) 'niwa-view-tool-use-id)))
          (should (string-search
                   "Earlier response"
                   (buffer-substring-no-properties (point-min) (car region)))))))))

(niwa-deftest niwa-view--refresh-tool-row
  (:doc "replaces one row while preserving collapse and composer state")
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool (Custom :value \"x\")\n")
      (let ((start (point)))
        (insert "(:name \"Custom\" :args (:value \"x\"))\n\ninitial")
        (put-text-property start (point) 'gptel '(tool . "call-custom")))
      (insert "\n#+end_tool\n"))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4)
      (let* ((region (niwa-view--tool-row-region
                      data-buf "call-custom"))
             (collapsed
              (get-text-property (car region) 'niwa-view-collapsed)))
        (with-current-buffer data-buf
          (goto-char (point-min))
          (search-forward "(:name")
          (search-forward ":value \"x\"")
          (replace-match ":value \"y\"" nil t))
        (should (niwa-view--refresh-tool-row data-buf "call-custom"))
        (setq region
              (niwa-view--tool-row-region data-buf "call-custom"))
        (should
         (eq collapsed
             (get-text-property (car region) 'niwa-view-collapsed)))
        (should (equal "> quoted\nsecond line"
                       (niwa-view-composer-draft-text nil t)))
        (should
         (string-match-p
          "Custom: y"
          (niwa-view-render-test--view-text)))))))

(niwa-deftest niwa-view-render--refresh-tool-row-now ()
  ,test
  (test)
  :doc "copied tool rows in the composer never become refresh targets or undo edits"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "#+begin_tool\n(:name \"Read\" :args (:file_path \"a.el\"))\n\nfirst result\n#+end_tool\n"
     '(tool . "first"))
    (let ((niwa-view-tool-group-collapse-threshold 1))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let* ((row (niwa-view--tool-row-region data-buf "first"))
               (kill-ring nil)
               (kill-ring-yank-pointer nil)
               (interprogram-cut-function nil)
               (interprogram-paste-function nil))
          (should row)
          (kill-new (filter-buffer-substring (car row) (cadr row)))
          (niwa-view-composer-insert "> ")
          (with-current-buffer (niwa-view-composer-buffer view-buf)
            (yank)
            (niwa-view-composer-insert "\nsecond line" (point) view-buf))
          (niwa-view-test--insert-data
           data-buf
           "#+begin_tool\n(:name \"Read\" :args (:file_path \"b.el\"))\n\nsecond result\n#+end_tool\n"
           '(tool . "second"))
          (niwa-view--full-rerender)
          (let ((draft (niwa-view-composer-draft-text nil t)))
            ;; The original row now lives inside a collapsed activity group.
            (should (text-property-any (point-min) (niwa-view-transcript-tail)
                                       'niwa-view-type 'tool-group))
            (should-not (text-property-any (point-min) (niwa-view-transcript-tail)
                                           'niwa-view-type 'tool-summary))
            (let ((composer (niwa-view-composer-buffer view-buf))
                  (start (niwa-view-composer-draft-start view-buf))
                  (end (niwa-view-composer-draft-end view-buf)))
              (with-current-buffer composer
                (should (text-property-any start end 'niwa-view-type 'tool-summary))
                (buffer-enable-undo)
                (setq buffer-undo-list nil)
                (niwa-view-composer-insert "typed" end view-buf)
                (undo-boundary))
              (let ((undo (buffer-local-value 'buffer-undo-list composer)))
                (should-not (niwa-view--refresh-tool-row data-buf "first"))
                (should (eq undo (buffer-local-value 'buffer-undo-list composer))))
              (should-not (niwa-view--tool-row-region data-buf "first"))
              (should (equal (concat draft "typed") (niwa-view-composer-draft-text nil t)))
              (with-current-buffer composer
                (niwa-test--with-captured-messages nil (undo-only 1))))
            (should (equal draft (niwa-view-composer-draft-text nil t))))))))
  :doc "progress refresh retains unrelated completed tool renderings"
  (niwa-view-test--with-buffers
    (let ((stable-position nil)
          (stable-computations 0)
          (compute (symbol-function 'niwa-view--prepare-tool-segment)))
      (with-current-buffer data-buf
        (dolist (id '("changing" "stable"))
          (insert "#+begin_tool Custom\n")
          (let ((start (point)))
            (when (equal id "stable") (setq stable-position start))
            (insert (format "(:name \"Custom\" :args (:value %S))\n\nresult" id))
            (put-text-property start (point) 'gptel (cons 'tool id)))
          (insert "\n#+end_tool\n")))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4)
        (cl-letf (((symbol-function 'niwa-view--prepare-tool-segment)
                   (lambda (buffer start end &rest options)
                     (when (and (<= start stable-position) (< stable-position end))
                       (cl-incf stable-computations))
                     (apply compute buffer start end options))))
          (should (niwa-view--refresh-tool-row data-buf "changing"))
          (niwa-view--full-rerender)
          (should (= 0 stable-computations)))
        (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text nil t)))
        (should (= 4 (niwa-view-render-test--draft-offset))))))
  :doc "a growing row keeps readers above, inside and below it on their text"
  (niwa-view-test--with-buffers
    (let ((niwa-view-tool-group-collapse-threshold 100))
      (dolist (id '("above" "changing" "below"))
        (niwa-view-test--insert-data
         data-buf
         (format "#+begin_tool\n(:name \"Custom\" :args (:value %S))\n\nresult\n#+end_tool\n" id)
         (cons 'tool id)))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let* ((window (selected-window))
               (above (car (niwa-view--tool-row-region data-buf "above")))
               (changing (car (niwa-view--tool-row-region data-buf "changing")))
               (below (car (niwa-view--tool-row-region data-buf "below")))
               (row-text (lambda (pos)
                           (buffer-substring-no-properties pos (min (point-max) (+ pos 8))))))
          (set-window-buffer window view-buf)
          (set-window-start window below)
          (set-window-point window (+ changing 2))
          (goto-char (+ changing 2))
          (set-marker (mark-marker) (1+ above))
          (let ((below-text (funcall row-text below))
                (inside-text (funcall row-text (+ changing 2)))
                (above-text (funcall row-text (1+ above))))
            (with-current-buffer data-buf
              (goto-char (point-min))
              (search-forward ":value \"changing\"")
              (replace-match ":value \"changing into a much longer value\"" nil t))
            (should (niwa-view--refresh-tool-row data-buf "changing"))
            (should (string-search "much longer"
                                   (niwa-view-render-test--view-text)))
            (should (= (window-start window)
                       (car (niwa-view--tool-row-region data-buf "below"))))
            (should (equal below-text (funcall row-text (window-start window))))
            (should (equal inside-text (funcall row-text (point))))
            (should (equal "changing"
                           (get-text-property (point) 'niwa-view-tool-use-id)))
            (should (equal above-text (funcall row-text (mark t)))))))))
  :doc "expanding a refreshed row follows metadata growth before its source"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (dolist (id '("earlier" "following"))
        (insert "#+begin_tool Custom\n")
        (let ((start (point)))
          (insert (format "(:name \"Custom\" :args (:value %S))\n\n%s-result\n" id id)
                  (niwa-tool-render-data-format '(:status success) id))
          (put-text-property start (point) 'gptel (cons 'tool id)))
        (insert "\n#+end_tool\n")
        (when (equal id "earlier")
          (insert (niwa--format-hook-audit-record
                   (list :type 'provider-tool-batch :id id
                         :messages (make-string 5000 ?x)))))))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4)
      (should (niwa-view--refresh-tool-row data-buf "following"))
      (should (niwa-tool-render-data-update
               data-buf "earlier"
               (list :execution-output (make-string 2300 ?y))))
      (let ((region (niwa-view--tool-row-region data-buf "following")))
        (goto-char (car region))
        (niwa-view-toggle-section)
        (should (string-search "following-result" (niwa-view-render-test--view-text)))
        (should-not (string-search "eHh4eHh4eHh4" (niwa-view-render-test--view-text)))
        (let ((source (get-text-property (point) 'niwa-view-source)))
          (should (markerp (car source)))
          (should (markerp (cdr source)))))
      (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text nil t))))))

(niwa-deftest niwa-view--refresh-tool-row/audit-neighbors
  (:doc "progress leaves an expanded neighboring hook audit unchanged")
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool Custom\n"
              (propertize "(:name \"Custom\" :args (:value \"x\"))\n\nresult"
                          'gptel '(tool . "audited"))
              "\n#+end_tool\n"
              (niwa--format-hook-audit-record
               '(:type tool-context :event "PreToolUse"
                 :handlers ((:source project-file
                             :contexts ("Keep this audit expanded")))))))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4)
      (let (audit-text audit-key)
        (save-excursion
          (goto-char (text-property-any (point-min) (niwa-view-transcript-tail)
                                        'niwa-view-type 'hook-audit))
          (niwa-view-toggle-section)
          (let ((bounds (niwa-view-disclosure-section-bounds)))
            (setq audit-text (buffer-substring-no-properties
                              (car bounds) (cdr bounds))
                  audit-key (get-text-property (point) 'niwa-view-source-key))))
        (dotimes (_ 5)
          (should (niwa-view--refresh-tool-row data-buf "audited"))
          (should (= 1 (niwa-view-test--count-substring
                        "PreToolUse hook added context" (niwa-view-render-test--view-text))))
          (save-excursion
            (goto-char (text-property-any (point-min) (niwa-view-transcript-tail)
                                          'niwa-view-type 'hook-audit))
            (let ((bounds (niwa-view-disclosure-section-bounds)))
              (should (equal audit-text (buffer-substring-no-properties
                                        (car bounds) (cdr bounds))))
              (should-not (get-text-property (point) 'niwa-view-collapsed))
              (should (eq audit-key (get-text-property
                                    (point) 'niwa-view-source-key)))))
          (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text nil t)))
          (should (= 4 (niwa-view-render-test--draft-offset))))
        ;; The insertion copy must not strip audits from cached renderings.
        (niwa-view--full-rerender)
        (should (= 1 (niwa-view-test--count-substring
                      "PreToolUse hook added context" (niwa-view-render-test--view-text))))
        (save-excursion
          (goto-char (text-property-any (point-min) (niwa-view-transcript-tail)
                                        'niwa-view-type 'hook-audit))
          (let ((bounds (niwa-view-disclosure-section-bounds)))
            (should (equal audit-text (buffer-substring-no-properties
                                      (car bounds) (cdr bounds))))
            (should-not (get-text-property (point) 'niwa-view-collapsed))))))))

(niwa-deftest niwa-view--prepare-tool-segment/audited-span
  (:doc "a complete tool followed by its merged hook audit is parsed once")
  (niwa-view-test--with-buffers
    (let ((computations 0)
          (compute (symbol-function 'niwa-view--prepare-tool-segment)))
      (with-current-buffer data-buf
        (insert (propertize "#+begin_tool Custom\n" 'gptel 'ignore)
                (propertize "(:name \"Custom\" :args (:value \"x\"))\n\nresult"
                            'gptel '(tool . "audited"))
                (propertize "\n#+end_tool\n" 'gptel 'ignore)
                (niwa--format-hook-audit-record
                 '(:type provider-tool-batch :id "audited" :messages "m"))))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'niwa-view--prepare-tool-segment)
                   (lambda (&rest args)
                     (cl-incf computations)
                     (apply compute args))))
          (niwa-view--full-rerender)
          (should (string-search "Custom" (niwa-view-render-test--view-text)))
          (let ((first computations))
            (should (> first 0))
            (niwa-view--full-rerender)
            (should (= first computations))))))))

(niwa-deftest niwa-view-render-summary-expansion
  (:doc "a collapsed Bash row defers its body until the user's expansion")
  (niwa-view-test--with-buffers
    (let ((body-builds 0))
      (niwa-tool-register
       (niwa-tool--create
        :name "Bash" :category "niwa"
        :renderer (lambda (name args result data)
                    (let ((rendering (niwa-tool-exec--render-bash name args result data)))
                      (when (plist-get rendering :body) (cl-incf body-builds))
                      rendering))))
      (with-current-buffer data-buf
        (insert "#+begin_tool\n"
                (propertize "(:name \"Bash\" :args (:command \"printf output\"))\n\noutput\n"
                            'gptel '(tool . "deferred"))
                "#+end_tool\n"))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> draft\nsecond line")
        (niwa-view--full-rerender)
        (should (= 0 body-builds))
        (goto-char (point-min)) (search-forward "Bash: printf output")
        (niwa-view-toggle-section)
        (should (> body-builds 0))
        (should (string-search "$ printf output\n\noutput" (niwa-view-render-test--view-text)))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))))))

(niwa-deftest niwa-view--tool-segment-text
  (:doc "pre-read complete spans are reused while partial spans still recover their enclosing tool")
  (with-temp-buffer
    (insert "#+begin_tool\n"
            (propertize "(:name \"Read\" :args nil)\n\nbody\n"
                        'gptel '(tool . "partial"))
            "#+end_tool\n")
    (let ((raw (buffer-string)))
      (should (eq raw (niwa-view--tool-segment-text
                       (point-min) (point-max) raw)))
      (goto-char (point-min))
      (search-forward "body")
      (let* ((end (point)) (start (- end 4))
             (part (buffer-substring start end)))
        (should (equal raw (niwa-view--tool-segment-text start end part)))))))

(niwa-deftest niwa-view--segment-rendering ()
  ,test
  (test)
  :doc "a cold complete tool copies its source only once"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool\n"
              (propertize "(:name \"Read\" :args (:file_path \"one.el\"))\n\nbody\n"
                          'gptel '(tool . "cold"))
              "#+end_tool\n"))
    (with-current-buffer view-buf
      (let ((end (with-current-buffer data-buf (point-max)))
            (copies 0)
            (copy (symbol-function 'buffer-substring)))
        (cl-letf (((symbol-function 'buffer-substring)
                   (lambda (beg limit)
                     (when (and (eq (current-buffer) data-buf)
                                (= beg 1) (= limit end))
                       (cl-incf copies))
                     (funcall copy beg limit))))
          (should (string-match-p
                   "one.el" (plist-get
                             (niwa-view--segment-rendering data-buf 1 end t)
                             :header)))
          (should (= copies 1))))))
  :doc "a complete cached tool skips parsing but same-length edits invalidate it"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool\n"
              (propertize "(:name \"Read\" :args (:file_path \"one.el\"))\n\nbody\n"
                          'gptel '(tool . "cached"))
              "#+end_tool\n"))
    (with-current-buffer view-buf
      (let* ((end (with-current-buffer data-buf (point-max)))
             (first (niwa-view--segment-rendering data-buf 1 end t))
             (reads 0)
             (metadata-reads 0)
             (extract (symbol-function 'niwa-tool-render-data-extract))
             (read (symbol-function 'niwa-view--tool-readable-start)))
        (should first)
        (cl-letf (((symbol-function 'niwa-view--tool-readable-start)
                   (lambda (text) (cl-incf reads) (funcall read text)))
                  ((symbol-function 'niwa-tool-render-data-extract)
                   (lambda (&rest args)
                     (cl-incf metadata-reads)
                     (apply extract args))))
          (should (equal (plist-get first :header)
                         (plist-get (niwa-view--segment-rendering data-buf 1 end t)
                                    :header)))
          (should (= reads 0))
          (should (= metadata-reads 0))
          (with-current-buffer data-buf
            (goto-char (point-min))
            (search-forward "one.el")
            (replace-match "two.el" t t))
          (let ((changed (niwa-view--segment-rendering data-buf 1 end t)))
            (should (> reads 0))
            (should (string-match-p "two.el" (plist-get changed :header))))))))
  :doc "unrelated appends reuse completed tools without copying or hashing payloads"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool\n"
              (propertize "(:name \"Read\" :args (:file_path \"one.el\"))\n\nbody\n"
                          'gptel '(tool . "cached"))
              "#+end_tool\n\n"))
    (with-current-buffer view-buf
      (let* ((end (with-current-buffer data-buf (1- (point-max))))
             (first (niwa-view--segment-rendering data-buf 1 end t))
             (copies 0)
             (copy (symbol-function 'buffer-substring)))
        (with-current-buffer data-buf (goto-char (point-max)) (insert "next"))
        (cl-letf (((symbol-function 'buffer-substring)
                   (lambda (beg limit)
                     (when (and (eq (current-buffer) data-buf) (= beg 1))
                       (cl-incf copies))
                     (funcall copy beg limit))))
          (should (equal (plist-get first :header)
                         (plist-get (niwa-view--segment-rendering data-buf 1 end t)
                                    :header)))
          (should (= copies 0))))))
  :doc "a long activity run reuses completed tools after unrelated appends"
  (niwa-view-test--with-buffers
    (let (ranges)
      (with-current-buffer data-buf
        (dotimes (i 350)
          (let ((start (point)))
            (insert "#+begin_tool\n"
                    (propertize
                     (format "(:name \"Read\" :args (:file_path \"file-%d.el\"))\n\nbody\n" i)
                     'gptel (cons 'tool (format "cached-%d" i)))
                    "#+end_tool\n\n")
            (push (cons start (1- (point))) ranges))))
      (setq ranges (nreverse ranges))
      (with-current-buffer view-buf
        (dolist (range ranges)
          (should (niwa-view--segment-rendering data-buf (car range) (cdr range) t)))
        (with-current-buffer data-buf (goto-char (point-max)) (insert "next"))
        (let ((reads 0)
              (read (symbol-function 'niwa-view--prepare-tool-segment)))
          (cl-letf (((symbol-function 'niwa-view--prepare-tool-segment)
                     (lambda (&rest args) (cl-incf reads) (apply read args))))
            (cl-loop for range in ranges for i from 0
                     do (should
                         (string-match-p
                          (format "file-%d\\.el" i)
                          (plist-get (niwa-view--segment-rendering
                                      data-buf (car range) (cdr range) t)
                                     :header))))
            (should (= reads 0)))))))
  :doc "cold renders outside a projection share one boundary index"
  (niwa-view-test--with-buffers
    (let (ranges (scans 0) (builds 0))
      (with-current-buffer data-buf
        (dotimes (i 40)
          (let ((start (point)))
            (insert "#+begin_tool\n"
                    (propertize
                     (format "(:name \"Read\" :args (:file_path \"file-%d.el\"))\n\nbody\n" i)
                     'gptel (cons 'tool (format "cold-%d" i)))
                    "#+end_tool\n\n")
            (push (cons start (1- (point))) ranges))))
      (let ((scan (symbol-function 'niwa-transcript--tool-block-start-for-run))
            (build (symbol-function 'niwa-transcript--org-tool-blocks-overlapping)))
        (cl-letf (((symbol-function 'niwa-transcript--tool-block-start-for-run)
                   (lambda (&rest args) (cl-incf scans) (apply scan args)))
                  ((symbol-function 'niwa-transcript--org-tool-blocks-overlapping)
                   (lambda (&rest args) (cl-incf builds) (apply build args))))
          (with-current-buffer view-buf
            (cl-loop for range in (nreverse ranges) for i from 0
                     do (should (string-match-p
                                 (format "file-%d\\.el" i)
                                 (plist-get (niwa-view--segment-rendering
                                             data-buf (car range) (cdr range) t)
                                            :header)))))))
      ;; Each unindexed recovery scans all earlier history backward, which
      ;; made a cold render cubic in the number of tools.
      (should (= scans 0))
      (should (= builds 1))))
  :doc "property-only restoration of a request failure precedes cached tool display"
  (niwa-view-test--with-buffers
    (let (metadata-start end)
      (with-current-buffer data-buf
        (insert "#+begin_tool\n"
                (propertize "(:name \"Read\" :args nil)\n\nbody\n"
                            'gptel '(tool . "cached")))
        (setq metadata-start (point))
        (insert (substring-no-properties
                 (niwa-tool-render-data-format
                  '(:kind request-summary :outcome error :message "Failed"))))
        (setq end (point))
        (insert "#+end_tool\n"))
      (with-current-buffer view-buf
        (let ((limit (with-current-buffer data-buf (point-max))))
          (should (niwa-view--segment-rendering data-buf 1 limit t))
          (with-current-buffer data-buf
            (put-text-property metadata-start end 'niwa-render-data t))
          (should (eq 'request-failure
                      (plist-get (niwa-view--segment-rendering data-buf 1 limit t)
                                 :vtype))))))))

(niwa-deftest niwa-view--render-prepared-tool/retry-failure ()
  ,test
  (test)
  :doc "exhaustion and chronological attempt history stay in the existing disclosure"
  (with-temp-buffer
    (let* ((attempts
            (cl-loop for attempt from 1 to 11
                     collect (list :attempt attempt
                                   :http-status (if (= attempt 11) "200" "503")
                                   :type "server_error" :code "capacity"
                                   :message (format "Capacity failure %d" attempt))))
           (summary (list :kind 'request-summary :outcome 'error
                          :backend "Codex" :status "HTTP/2 200"
                          :error-type "server_error" :error-code "capacity"
                          :error-data '(:request_id "provider-id")
                          :message "Model at capacity"
                          :retry-errors attempts :retry-exhausted t)))
      (insert (niwa-tool-render-data-format summary))
      (let* ((rendering (niwa-view--segment-rendering
                         (current-buffer) (point-min) (point-max)))
             (body (plist-get rendering :body)))
        (should (eq 'request-failure (plist-get rendering :vtype)))
        (should (eq 'error (plist-get rendering :status)))
        (should-not (plist-get rendering :initially-collapsed-p))
        (should (string-search "retries exhausted" (plist-get rendering :header)))
        (should (string-search "Automatic retries exhausted after 11 attempts." body))
        (should (string-search "Provider data: (:request_id \"provider-id\")" body))
        (should (string-search "Model at capacity" body))
        (let ((previous -1))
          (dolist (attempt attempts)
            (let ((position (string-search
                             (format "Attempt %d · %s · server_error · capacity: Capacity failure %d"
                                     (plist-get attempt :attempt)
                                     (plist-get attempt :http-status)
                                     (plist-get attempt :attempt)) body)))
              (should position)
              (should (> position previous))
              (setq previous position))))
        (should (string-search "Retry replays the prepared request" body)))))
  :doc "legacy failures and nonexhausted retry history do not claim exhaustion"
  (with-temp-buffer
    (dolist (attempts '(nil ((:attempt 1 :http-status nil :type nil :code nil
                             :message "Connection interrupted"))))
      (let* ((rendering
              (niwa-view--render-prepared-tool
               (list :request-failure
                     (list :backend "Provider" :outcome 'error :message "Failed"
                           :retry-errors attempts))
               (current-buffer)))
             (body (plist-get rendering :body)))
        (should (equal "Provider request failed" (plist-get rendering :header)))
        (should-not (string-search "exhausted" body))
        (should (string-search "Failed" body))
        (if attempts
            (should (string-search "Attempt 1: Connection interrupted" body))
          (should-not (string-search "Attempt history" body)))))))

(niwa-deftest niwa-view--render-prepared-tool/diagnostic-scope ()
  (let ((source (generate-new-buffer " *diagnostic source*"))
        (summary (list :backend "Provider" :outcome 'error :message "Raw failure")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq-local niwa--workspace
                        (niwa-workspace--create :root "/project A/same/")))
          (with-temp-buffer
            (setq-local niwa--workspace
                        (niwa-workspace--create :root "/project B/same/"))
            (let ((rendering (niwa-view--render-prepared-tool
                              (list :request-failure summary) source)))
              (should (string-search "niwa [/project A/same/]: Raw failure"
                                     (plist-get rendering :body)))
              (should-not (string-search "/project B/" (plist-get rendering :body)))))
          (should (equal "Raw failure" (plist-get summary :message))))
      (kill-buffer source))))

(niwa-deftest niwa-view--request-failure-extra-data ()
  ,test
  (test)
  :doc "fields the block already states are dropped, leaving nothing to show"
  (should-not (niwa-view--request-failure-extra-data
               '(:message "Ended" :code -32603) "Ended" nil -32603))
  :doc "provider-only fields stay, without the restated ones"
  (should (equal '(:request_id "r-1")
                 (niwa-view--request-failure-extra-data
                  '(:type "server_error" :code "x" :message "Failed" :request_id "r-1")
                  "Failed" "server_error" "x")))
  :doc "a field that differs from the shown value is provider evidence"
  (should (equal '(:message "Raw detail")
                 (niwa-view--request-failure-extra-data
                  '(:message "Raw detail" :code 7) "Summary" nil 7)))
  :doc "data that is not a keyword plist is shown unchanged"
  (dolist (data '((("code" . 1)) (:message) ("a" "b")))
    (should (eq data (niwa-view--request-failure-extra-data data "a" nil nil)))))

(niwa-deftest niwa-view--segment-rendering/generic-fallback
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)
  :doc "renders registered tools without renderer through generic fallback"
  (progn
    (niwa-tool-register
     (niwa-tool--create
      :name "NoRenderer"
      :category "niwa"
      :args '((path string :required "Path"))))
    (with-temp-buffer
      (insert "(:name \"NoRenderer\" :args (:path \"foo.el\"))\nline\n")
      (let ((rendering (niwa-view--segment-rendering
                        (current-buffer) (point-min) (point-max))))
        (should (equal "NoRenderer: foo.el (1 line)"
                       (plist-get rendering :header)))
        (should (equal "line" (plist-get rendering :body))))))
  :doc "renders unregistered third-party-style tools through generic fallback"
  (with-temp-buffer
    (insert "(:name \"ThirdParty\" :args (:query \"thing\"))\nanswer\n")
    (let ((rendering (niwa-view--segment-rendering
                      (current-buffer) (point-min) (point-max))))
      (should (equal "ThirdParty: thing (1 line)"
                     (plist-get rendering :header)))))
  :doc "malformed args in parseable tool calls fall back without signalling"
  (with-temp-buffer
    (insert "(:name \"ThirdParty\" :args \"not a plist\")\nanswer\n")
    (let ((rendering (niwa-view--segment-rendering
                      (current-buffer) (point-min) (point-max))))
      (should (equal "ThirdParty (1 line)"
                     (plist-get rendering :header)))))
  :doc "renderer opt-out falls through to generic error rendering"
  (progn
    (niwa-tool-register
     (niwa-tool--create
      :name "Edit"
      :category "niwa"
      :display-arg :file_path
      :renderer (lambda (_name _args _result _data) nil)))
    (with-temp-buffer
      (insert "(:name \"Edit\" :args (:file_path \"niwa-goal.el\"))\n"
              "Error: Could not find old_string in file: x\n")
      (let ((rendering (niwa-view--segment-rendering
                        (current-buffer) (point-min) (point-max))))
        (should (equal "Edit: niwa-goal.el (error)"
                       (plist-get rendering :header)))
        (should (eq 'error (plist-get rendering :status)))
        (should (string-prefix-p "Error:" (plist-get rendering :body))))))
  :doc "generic rendering honors structured error status without failure prose"
  (with-temp-buffer
    (insert "(:name \"ThirdParty\" :args (:query \"thing\"))\nplain failure")
    (insert (niwa-tool-render-data-format '(:status error)))
    (let ((rendering (niwa-view--segment-rendering
                      (current-buffer) (point-min) (point-max))))
      (should (equal "ThirdParty: thing (error)"
                     (plist-get rendering :header)))
      (should (eq 'error (plist-get rendering :status)))
      (should (equal "plain failure" (plist-get rendering :body)))))
  :doc "custom Bash rendering marks permission failures without render data"
  (progn
    (niwa-tool-register
     (niwa-tool--create
      :name "Bash"
      :category "niwa"
      :display-arg :command
      :renderer #'niwa-tool-exec--render-bash))
    (with-temp-buffer
      (insert "(:name \"Bash\" :args (:command \"npx test\"))\n"
              "Error: Permission denied\n")
      (let ((rendering (niwa-view--segment-rendering
                        (current-buffer) (point-min) (point-max))))
        (should (eq 'error (plist-get rendering :status))))))
  :doc "collapsed cached renderings omit bodies but expansion keeps them"
  (let ((niwa-view--tool-rendering-cache (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert "(:name \"ThirdParty\" :args (:query \"thing\"))\nlarge body\n")
      (let ((collapsed (niwa-view--segment-rendering
                        (current-buffer) (point-min) (point-max) t))
            (expanded (niwa-view--segment-rendering
                       (current-buffer) (point-min) (point-max))))
        (should (equal "ThirdParty: thing (1 line)"
                       (plist-get collapsed :header)))
        (should-not (plist-get collapsed :body))
        (should (equal "large body" (plist-get expanded :body))))))
  :doc "summary context reaches renderers only for the collapsed projection"
  (let ((phases nil))
    (niwa-tool-register
     (niwa-tool--create
      :name "Bash" :category "niwa"
      :renderer (lambda (name args result data)
                  (push niwa-tool-render-summary-only phases)
                  (niwa-tool-exec--render-bash name args result data))))
    (with-temp-buffer
      (insert "(:name \"Bash\" :args (:command \"printf output\"))\noutput\n")
      (let ((summary (niwa-view--segment-rendering
                      (current-buffer) (point-min) (point-max) t))
            (expanded (niwa-view--segment-rendering
                       (current-buffer) (point-min) (point-max))))
        (should (equal '(nil t) phases))
        (should (equal (plist-get summary :header) (plist-get expanded :header)))
        (should-not (plist-get summary :body))
        (should (equal "$ printf output\n\noutput" (plist-get expanded :body)))))
    (should-not niwa-tool-render-summary-only))
  :doc "unrelated appends keep completed tool renderings cached"
  (let ((niwa-view--tool-rendering-cache (make-hash-table :test #'equal))
        (calls 0))
    (niwa-tool-register
     (niwa-tool--create
      :name "CacheTool"
      :category "niwa"
      :renderer (lambda (_name _args result _data)
                  (cl-incf calls)
                  (list :header (format "CacheTool: %s" result)
                        :body result
                        :initially-collapsed-p t))))
    (with-temp-buffer
      (insert "(:name \"CacheTool\" :args (:query \"thing\"))\none\n")
      (let* ((seg-start (point-min))
             (seg-end (point-max))
             (first (niwa-view--segment-rendering
                     (current-buffer) seg-start seg-end t)))
        (goto-char (point-max))
        (insert "unrelated streaming text\n")
        (let ((second (niwa-view--segment-rendering
                       (current-buffer) seg-start seg-end t)))
          (should (equal "CacheTool: one" (plist-get first :header)))
          (should (equal "CacheTool: one" (plist-get second :header)))
          (should (= 1 calls))))))
  :doc "tool text changes invalidate cached renderings"
  (let ((niwa-view--tool-rendering-cache (make-hash-table :test #'equal))
        (calls 0))
    (niwa-tool-register
     (niwa-tool--create
      :name "CacheTool"
      :category "niwa"
      :renderer (lambda (_name _args result _data)
                  (cl-incf calls)
                  (list :header (format "CacheTool: %s" result)
                        :body result
                        :initially-collapsed-p t))))
    (with-temp-buffer
      (insert "(:name \"CacheTool\" :args (:query \"thing\"))\none\n")
      (let ((seg-start (point-min))
            (seg-end (point-max)))
        (let ((first (niwa-view--segment-rendering
                      (current-buffer) seg-start seg-end t)))
          (goto-char (point-min))
          (search-forward "one")
          (replace-match "two" nil t)
          (let ((second (niwa-view--segment-rendering
                         (current-buffer) seg-start seg-end t)))
            (should (equal "CacheTool: one" (plist-get first :header)))
            (should (equal "CacheTool: two" (plist-get second :header)))
            (should (= 2 calls)))))))
  :doc "running renderings are not cached; settled ones are"
  ;; A live execution's header carries elapsed time from transient progress,
  ;; which the source-based cache key cannot see.
  (let ((niwa-view--tool-rendering-cache (make-hash-table :test #'equal))
        (status 'running)
        (calls 0))
    (niwa-tool-register
     (niwa-tool--create
      :name "CacheTool"
      :category "niwa"
      :renderer (lambda (_name _args _result _data)
                  (cl-incf calls)
                  (list :header (format "CacheTool: %d" calls)
                        :status status
                        :initially-collapsed-p t))))
    (with-temp-buffer
      (insert "(:name \"CacheTool\" :args (:query \"thing\"))\none\n")
      (let ((start (point-min)) (end (point-max)))
        (should (equal "CacheTool: 1" (plist-get (niwa-view--segment-rendering
                                                   (current-buffer) start end t)
                                                  :header)))
        (should (equal "CacheTool: 2" (plist-get (niwa-view--segment-rendering
                                                   (current-buffer) start end t)
                                                  :header)))
        (should (equal "CacheTool: 3" (plist-get (niwa-view--segment-rendering
                                                   (current-buffer) start end)
                                                  :header)))
        (should (= 0 (hash-table-count niwa-view--tool-rendering-cache)))
        (setq status 'success)
        (niwa-view--segment-rendering (current-buffer) start end t)
        (niwa-view--segment-rendering (current-buffer) start end t)
        (should (= 4 calls)))))
  :doc "malformed tool text still returns nil"
  (with-temp-buffer
    (insert "not a tool")
    (should-not (niwa-view--segment-rendering
                 (current-buffer) (point-min) (point-max)))))

(niwa-deftest niwa-view--sandbox-summary-line
  ()
  ,test
  (test)
  :doc "omits the default sandbox boundary"
  (should-not
   (niwa-view--sandbox-summary-line
    '(:attempt-count 1 :started-count 1 :refused-count 0
      :sandbox bubblewrap :filesystem workspace-write
      :network isolated :proc fresh
      :additional-read-count 0 :additional-write-count 0)))
  :doc "omits additional read-only access"
  (should-not
   (niwa-view--sandbox-summary-line
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
           (niwa-view--sandbox-summary-line
            (append
             '(:attempt-count 1 :started-count 1 :refused-count 0)
             (car case)))))
      (should (string-match-p "◇ Sandbox:" line))
      (should (string-match-p (regexp-quote (cdr case)) line))))
  :doc "keeps material access visible beside a partial refusal"
  (let ((line
         (niwa-view--sandbox-summary-line
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
         (niwa-view--sandbox-summary-line
          '(:attempt-count 1 :started-count 0 :refused-count 1
            :sandbox refused :filesystem unavailable
            :network unavailable :proc nil
            :additional-read-count 0 :additional-write-count 0))))
    (should (string-match-p "! Sandbox:" line))
    (should
     (string-match-p "execution refused · no child started" line)))
  :doc "shows a queued child that never started without nil policy fields"
  (let ((line
         (niwa-view--sandbox-summary-line
          '(:attempt-count 1 :started-count 0 :refused-count 0
            :additional-read-count 0 :additional-write-count 0))))
    (should (string-match-p "1 child did not start" line))
    (should-not (string-match-p "nil" line)))
  :doc "renders a granted boundary as a quiet note, not a warning"
  (dolist (sandbox '(unavailable off))
    (let ((line
           (niwa-view--sandbox-summary-line
            (list :attempt-count 2 :started-count 2 :refused-count 0
                  :sandbox sandbox :filesystem 'unrestricted
                  :network 'unrestricted :proc nil
                  :additional-read-count 0 :additional-write-count 0))))
      (should (string-match-p "◇ Sandbox:" line))
      (should (string-match-p "ran without confinement" line))
      (should (eq 'niwa-view-tool-metadata
                  (get-text-property (string-match "◇" line)
                                     'font-lock-face line)))))
  :doc "notes network and additional write access granted as configured"
  (let ((line
         (niwa-view--sandbox-summary-line
          '(:attempt-count 1 :started-count 1 :refused-count 0
            :sandbox bubblewrap :filesystem workspace-write
            :network unrestricted :proc fresh
            :additional-read-count 0 :additional-write-count 1))))
    (should (eq 'niwa-view-tool-metadata
                (get-text-property (string-match "◇" line)
                                   'font-lock-face line))))
  :doc "keeps a child that never started in the warning face"
  (let ((line
         (niwa-view--sandbox-summary-line
          '(:attempt-count 2 :started-count 1 :refused-count 0
            :sandbox unavailable :filesystem unrestricted
            :network unrestricted :proc nil
            :additional-read-count 0 :additional-write-count 0))))
    (should (string-match-p "! Sandbox:" line))
    (should-not (eq 'niwa-view-tool-warning
                    (get-text-property (string-match "Sandbox:" line)
                                       'font-lock-face line)))
    (should (eq 'niwa-view-tool-warning
                (get-text-property (string-match "!" line)
                                   'font-lock-face line)))))

(niwa-deftest niwa-view--rendering-header-block
  (:doc "hides persisted read-only sandbox metadata")
  (let ((block
         (niwa-view--rendering-header-block
          '(:header "Read: file.pdf"
            :sandbox-summary
            (:attempt-count 1 :started-count 1 :refused-count 0
             :sandbox bubblewrap :filesystem workspace-write
             :network isolated :proc fresh
             :additional-read-count 1 :additional-write-count 0)))))
    (should (equal "  ✓ Read: file.pdf" block))))

(niwa-deftest niwa-view--render-expanded-body ()
  ,test
  (test)
  :doc "adds a display-only body inset inherited by source panels"
  (with-temp-buffer
    (niwa-view-mode)
    (let ((inhibit-read-only t))
      (niwa-view--render-expanded-body
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

(niwa-deftest niwa-view--child-call-rendering
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)

  :doc "renders a nested call through the nested tool's own renderer"
  (progn
    (niwa-tool-register
     (niwa-tool--create
      :name "Grep"
      :category "niwa"
      :renderer (lambda (name args result _data)
                  (list :header (format "%s: %s (%d matches)"
                                        name (plist-get args :pattern)
                                        (length (split-string result "\n" t)))
                        :body result
                        :body-mode 'grep-mode
                        :initially-collapsed-p t))))
    (let ((rendering
           (niwa-view--child-call-rendering
            '(:id "ptc/1" :tool "Grep" :status success
              :args (:pattern "defcustom")
              :result "a.el:1\nb.el:2\n"))))
      (should (equal "Grep: defcustom (2 matches)"
                     (plist-get rendering :header)))
      (should (eq 'grep-mode (plist-get rendering :body-mode)))
      (should (eq 'tool-child (plist-get rendering :vtype)))
      (should (plist-get rendering :initially-collapsed-p))))

  :doc "renders a failed nested call collapsed and marked as an error"
  (let ((rendering
         (niwa-view--child-call-rendering
          '(:id "ptc/1" :tool "Bash" :status error
            :args (:command "git status")
            :result "Error: exit 128"))))
    (should (eq 'error (plist-get rendering :status)))
    (should (plist-get rendering :initially-collapsed-p)))

  :doc "keeps a nested compound call's own rows so it expands into them"
  (progn
    (niwa-tool-register
     (niwa-tool--create
      :name "ToolCall"
      :category "niwa"
      :renderer (lambda (_name _args result _data)
                  (list :header "ToolCall: 1 call (completed)"
                        :body result
                        :child-calls '((:id "x" :tool "Read"))
                        :coalesce-key "ptc"
                        :initially-collapsed-p t))))
    (let ((rendering
           (niwa-view--child-call-rendering
            '(:id "ptc/1" :tool "ToolCall" :status success
              :args (:expression "(Read :file_path \"a\")")
              :result "inner value"))))
      (should (equal '((:id "x" :tool "Read"))
                     (plist-get rendering :child-calls)))
      (should-not (plist-get rendering :coalesce-key))))

  :doc "keeps media references on the row without payload bytes"
  (let ((rendering
         (niwa-view--child-call-rendering
          '(:id "ptc/1" :tool "Read" :status success
            :args (:file_path "shot.png")
            :result "image"
            :media ((:mime "image/png" :kind image :path "shot.png"))))))
    (should (string-match-p "Media:.*shot\\.png"
                            (plist-get rendering :body))))

  :doc "carries the row's sandbox disclosure one fold deeper"
  (let ((rendering
         (niwa-view--child-call-rendering
          '(:id "ptc/1" :tool "Bash" :status success
            :args (:command "ls")
            :result "out"
            :render-data
            (:sandbox-summary
             (:attempt-count 1 :started-count 1 :refused-count 0
              :sandbox bubblewrap :filesystem workspace-write
              :network unrestricted :proc fresh
              :additional-read-count 0 :additional-write-count 1))))))
    (should (string-match-p
             "◇ Sandbox:.*network access allowed"
             (niwa-view--rendering-header-block rendering)))))

(niwa-deftest niwa-view--tool-group-rendering-from-source ()
  ,test
  (test)
  :doc "rebuilds only the folded run, not the response that follows it"
  (niwa-view-test--with-buffers
    (dotimes (i 4)
      (niwa-view-test--insert-data
       data-buf
       (format "\n(:name \"Read\" :args (:file_path \"/tmp/f%d\"))\n\nbody\n" i)
       (cons 'tool (format "call_%d" i))))
    (let (run-start run-end)
      (with-current-buffer data-buf
        (setq run-start (point-min))
        (setq run-end (point-max)))
      (niwa-view-test--insert-data
       data-buf "#+begin_reasoning\nFollowing thought.\n#+end_reasoning\n" 'ignore)
      (niwa-view-test--insert-data data-buf "Following response.\n" 'response)
      (let ((children
             (plist-get
              (niwa-view--tool-group-rendering-from-source
               data-buf run-start run-end)
              :child-calls)))
        (should (= 4 (length children)))
        (should-not (cl-some (lambda (child)
                               (eq (plist-get child :kind) 'reasoning))
                             children)))))
  :doc "an agent view's own completion mail neither splits nor shortens the run"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (setq-local niwa-view--agent-path "/root/worker"))
    (cl-flet ((reads (from to)
                (cl-loop for i from from below to
                         do (niwa-view-test--insert-data
                             data-buf
                             (format "\n(:name \"Read\" :args (:file_path \"/tmp/f%d\"))\n\nbody\n" i)
                             (cons 'tool (format "call_%d" i))))))
      (reads 0 3)
      (niwa-view-test--insert-data
       data-buf
       (concat "<agent-message type=\"EXECUTION\" sender=\"/root/worker\" "
               "recipient=\"/root/worker\" execution_id=\"exec-1\" "
               "description=\"own\" outcome=\"success\" exit_code=\"0\">\n"
               "out\n</agent-message>\n")
       nil)
      (reads 3 6)
      (let* ((range (with-current-buffer data-buf
                      (cons (point-min) (point-max))))
             (rendering
              (with-current-buffer view-buf
                (niwa-view--tool-group-rendering-from-source
                 data-buf (car range) (cdr range)))))
        (should (= 6 (length (plist-get rendering :child-calls))))
        (should (equal "6 calls: read 6 files"
                       (substring-no-properties
                        (plist-get rendering :header))))))))

(niwa-deftest niwa-view--tool-group-rendering ()
  ,test
  (test)
  :doc "a note-class sandbox disclosure leaves the group unmarked"
  (let* ((summary '(:attempt-count 1 :started-count 1 :refused-count 0
                    :sandbox bubblewrap :filesystem workspace-write
                    :network unrestricted :proc fresh
                    :additional-read-count 0 :additional-write-count 1))
         (rendering
          (niwa-view--tool-group-rendering
           (car (niwa-activity-group-partition
                 (list (niwa-view--activity-group-entry
                        (list :kind 'tool :count 1
                              :rendering (list :status 'success
                                               :group-tool "Bash"
                                               :sandbox-summary summary)
                              :group-child
                              (list :tool "Bash" :status 'success
                                    :render-data
                                    (list :sandbox-summary summary)))))
                 nil))
           (current-buffer))))
    (should (equal "1 call: ran 1 command" (plist-get rendering :header)))
    (should-not (plist-get rendering :status))
    (should-not (plist-get rendering :unconfined)))
  :doc "a call that ran without confinement marks the group once"
  (let* ((summary '(:attempt-count 1 :started-count 1 :refused-count 0
                    :sandbox off :filesystem unrestricted
                    :network unrestricted :proc nil
                    :additional-read-count 0 :additional-write-count 0))
         (entry (lambda (sandbox)
                  (niwa-view--activity-group-entry
                   (list :kind 'tool :count 1
                         :rendering (list :status 'success :group-tool "Bash"
                                          :sandbox-summary sandbox)
                         :group-child (list :tool "Bash" :status 'success)))))
         (rendering
          (niwa-view--tool-group-rendering
           (car (niwa-activity-group-partition
                 (list (funcall entry summary) (funcall entry nil)) nil))
           (current-buffer)))
         (line (substring-no-properties
                (niwa-view--rendering-header-line rendering))))
    (should (plist-get rendering :unconfined))
    (should-not (plist-get rendering :status))
    (should (string-suffix-p "2 calls: ran 2 commands · unconfined ▸" line))
    (should (= 1 (niwa-view-test--count-substring "unconfined" line))))
  :doc "a refused call did not run, so it is not unconfined"
  (should-not (niwa-view--sandbox-unconfined-p
               '(:attempt-count 1 :started-count 0 :refused-count 1 :sandbox off))))

(niwa-deftest niwa-view--insert-child-calls ()
  ,test
  (test)

  :doc "inserts one indented row per nested call, collapsed by default"
  (with-temp-buffer
    (niwa-view-mode)
    (let ((inhibit-read-only t))
      (niwa-view--render-expanded-body
       '(:header "ToolCall: 2 calls (completed)"
         :body "Returned:\nfinal"
         :child-calls ((:id "ptc/1" :tool "Read" :status success
                        :args (:file_path "a.el") :result "first")
                       (:id "ptc/2" :tool "Read" :status success
                        :args (:file_path "b.el") :result "second")))
       (cons 1 10)))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "ToolCall: 2 calls" text))
      (should (string-match-p "Read: a\\.el" text))
      (should (string-match-p "Read: b\\.el" text))
      ;; Collapsed rows show headers only.
      (should-not (string-match-p "first" text)))
    (goto-char (point-min))
    (search-forward "a.el")
    (should (eq 'tool-child
                (get-text-property (match-beginning 0) 'niwa-view-type)))
    (should (equal "  " (get-text-property (match-beginning 0) 'line-prefix)))
    ;; Each row is its own section, so the two carry distinct state keys.
    (let ((first-key (get-text-property (match-beginning 0)
                                        'niwa-view-source-key)))
      (search-forward "b.el")
      (should-not (equal first-key
                         (get-text-property (match-beginning 0)
                                            'niwa-view-source-key)))))

  :doc "expands one row without disturbing its siblings"
  (with-temp-buffer
    (niwa-view-mode)
    (let ((inhibit-read-only t))
      (niwa-view--render-expanded-body
       '(:header "ToolCall: 2 calls (completed)"
         :body "Returned:\nfinal"
         :child-calls ((:id "ptc/1" :tool "Read" :status success
                        :args (:file_path "a.el") :result "first output")
                       (:id "ptc/2" :tool "Read" :status success
                        :args (:file_path "b.el") :result "second output")))
       (cons 1 10)))
    (goto-char (point-min))
    (search-forward "a.el")
    (goto-char (match-beginning 0))
    (niwa-view-toggle-section)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "first output" text))
      (should-not (string-match-p "second output" text))
      (should (string-match-p "Read: b\\.el" text)))
    ;; Collapsing it again leaves one header per row.
    (goto-char (point-min))
    (search-forward "first output")
    (niwa-view-toggle-section)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (string-match-p "first output" text))
      (should (= 1 (niwa-view-test--count-substring "Read: a.el" text)))))

  :doc "a collapsed block shows no rows at all"
  (with-temp-buffer
    (niwa-view-mode)
    (let ((inhibit-read-only t))
      (niwa-view--insert-rendered-tool
       '(:header "ToolCall: 1 call (completed)"
         :body "Returned:\nfinal"
         :initially-collapsed-p t
         :child-calls ((:id "ptc/1" :tool "Read" :status success
                        :args (:file_path "a.el") :result "first")))
       (cons 1 10)))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "ToolCall: 1 call" text))
      (should-not (string-match-p "Read" text))))

  :doc "draws a concurrent gutter and keeps it across a toggle"
  (with-temp-buffer
    (niwa-view-mode)
    (let ((inhibit-read-only t))
      (niwa-view--render-expanded-body
       '(:header "ToolCall: 3 calls (completed)"
         :body "Returned:\nfinal"
         :child-calls ((:id "ptc/1" :tool "Grep" :status success
                        :args (:pattern "x") :result "hit")
                       (:id "ptc/2" :tool "Read" :status success
                        :batch 1 :args (:file_path "a.el") :result "first out")
                       (:id "ptc/3" :tool "Read" :status success
                        :batch 1 :args (:file_path "b.el") :result "second")))
       (cons 1 10)))
    (goto-char (point-min))
    (search-forward "Grep")
    (should (equal "    " (get-text-property (match-beginning 0) 'line-prefix)))
    (search-forward "a.el")
    (should (equal "  \u250c " (get-text-property (match-beginning 0) 'line-prefix)))
    ;; The glyph belongs on the row's own line, not on text wrapped from it.
    (should (equal "    " (get-text-property (match-beginning 0) 'wrap-prefix)))
    (search-forward "b.el")
    (should (equal "  \u2514 " (get-text-property (match-beginning 0) 'line-prefix)))
    ;; Expanding a batched row keeps its glyph and insets its body plainly.
    (goto-char (point-min))
    (search-forward "a.el")
    (goto-char (match-beginning 0))
    (niwa-view-toggle-section)
    (goto-char (point-min))
    (search-forward "a.el")
    (should (equal "  \u250c " (get-text-property (match-beginning 0) 'line-prefix)))
    (search-forward "first out")
    (should (equal "        "
                   (get-text-property (match-beginning 0) 'line-prefix)))))

(niwa-deftest niwa-view--child-call-prefixes ()
  ,test
  (test)

  :doc "handles empty, singleton, adjacent, and interrupted batches"
  (dolist (case '((nil nil)
                 ((nil) nil)
                 ((1) ("  \u2500 "))
                 ((nil 1 nil) ("    " "  \u2500 " "    "))
                 ((1 2 2 1) ("  \u2500 " "  \u250c " "  \u2514 " "  \u2500 "))
                 ((1 nil 1) ("  \u2500 " "    " "  \u2500 "))
                 (("join" "join") ("  \u250c " "  \u2514 "))))
    (let* ((children (mapcar (lambda (batch) (list :batch batch)) (car case)))
           (before (copy-tree children)))
      (should (equal (cadr case) (niwa-view--child-call-prefixes children)))
      (should (equal before children))))

  :doc "gives a sequence-only block no glyph gutter"
  (should-not (niwa-view--child-call-prefixes
               '((:id "a" :batch nil) (:id "b" :batch nil))))

  :doc "brackets each concurrent join and aligns the rows around it"
  (should (equal '("  \u250c " "  \u251c " "  \u2514 " "    " "  \u250c " "  \u2514 ")
                 (niwa-view--child-call-prefixes
                  '((:id "1" :batch 1) (:id "2" :batch 1) (:id "3" :batch 1)
                    (:id "4" :batch nil)
                    (:id "5" :batch 2) (:id "6" :batch 2)))))

  :doc "keeps every prefix the same width so markers stay in one column"
  (let ((prefixes (niwa-view--child-call-prefixes
                   '((:id "1" :batch 1) (:id "2" :batch 1)
                     (:id "3" :batch nil)))))
    (should (= 1 (length (delete-dups
                          (mapcar #'string-width prefixes)))))))

(niwa-deftest niwa-view--insert-rendered-tool/non-expandable ()
  ,test
  (test)
  :doc "non-expandable renderings do not carry source state"
  (with-temp-buffer
    (niwa-view-mode)
    (let ((rendering
           '(:header "TaskCreate: Created 1 task"
             :expandable-p nil
             :sandbox-summary
             (:attempt-count 1 :started-count 0 :refused-count 1
              :sandbox refused :filesystem unavailable
              :network unavailable :proc nil
              :additional-read-count 0 :additional-write-count 0)))
          (inhibit-read-only t))
      (niwa-view--insert-rendered-tool rendering (cons 1 10))
      (goto-char (point-min))
      (should (eq 'tool-event
                  (get-text-property (point) 'niwa-view-type)))
      (should (string-match-p
               "Sandbox:.*execution refused · no child started"
               (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (get-text-property (point) 'niwa-view-source))
      (let ((before (buffer-string)))
        (should-error (niwa-view-toggle-section))
        (should (equal before (buffer-string)))))))

(niwa-deftest niwa-view--format-hook-context-audit ()
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
         (collapsed (niwa-view--format-hook-context-audit record nil))
         (expanded (niwa-view--format-hook-context-audit record t)))
    (should (equal "  ◇ SubagentStart hook added context · 2 handlers\n"
                   collapsed))
    (should (< (string-match "1\\. ponytail plugin" expanded)
               (string-match "2\\. project hook" expanded)))
    (should (string-match-p "Handler: ponytail-subagent" expanded))
    (should (string-match-p "Reason: PONYTAIL:FULL" expanded))
    (should (string-match-p "Handler: Inject project conventions" expanded))))

(niwa-deftest niwa-view--user-turn-hook-audits ()
  ,test
  (test)
  :doc "user turns hide provider bookkeeping while retaining reminder disclosures"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Continue\n" nil)
    (niwa-view-test--insert-data
     data-buf
     (mapconcat
      #'niwa--format-hook-audit-record
      '((:type injected-reminders :phase turn-start
         :items ((:type verification-suggestion :body "Check the result.")))
        (:type provider-tool-batch-start :id "hidden-call")
        (:type provider-tool-batch :id "hidden-call" :messages [])
        (:type mailbox-receipt :id "hidden-receipt")
        (:type fork-point :fork-point-id "hidden-fork"))
      "")
     'niwa-hook-audit)
    (let* ((end (with-current-buffer data-buf (point-max)))
           (records (niwa-view--user-turn-hook-audits
                     (list (list 'user 1 end)) data-buf)))
      (should (equal '(injected-reminders)
                     (mapcar (lambda (record) (plist-get record :type)) records)))
      (let ((source (plist-get (car records) :source)))
        (should (equal '(injected-reminders)
                       (mapcar
                        (lambda (span) (plist-get (plist-get span :record) :type))
                        (niwa-transcript-audit-spans
                         (niwa-view-disclosure-data-substring
                          data-buf (car source) (cdr source) t)))))))
    (niwa-view-test--insert-data data-buf "Continuing.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4)
      (niwa-view--full-rerender)
      (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text nil t)))
      (should (= 4 (niwa-view-render-test--draft-offset)))
      (should (string-search "1 system reminder" (niwa-view-render-test--view-text)))
      (should-not (string-search "hook audit" (niwa-view-render-test--view-text)))
      (should-not (string-search "hidden-call" (niwa-view-render-test--view-text))))))

(niwa-deftest niwa-view-mailbox-receipt-hidden ()
  (dolist (restored '(nil t))
    (dolist (placement '(response tool mailbox))
      (niwa-view-test--with-buffers
        (niwa-view-test--insert-data data-buf "Plan the change.\n" nil)
        (pcase placement
          ('response
           (niwa-view-test--insert-data data-buf "Plan ready.\n" 'response))
          ('tool
           (niwa-view-test--insert-data
            data-buf "(:name \"WaitAgent\" :args nil)\n\nMailbox activity\n"
            '(tool . "wait-mail")))
          ('mailbox
           (niwa-view-test--insert-data
            data-buf
            (concat "<agent-result sender=\"/root/reviewer\" recipient=\"/root\" "
                    "outcome=\"completed\">\nPlan checked.\n</agent-result>\n")
            nil)))
        (niwa-view-test--insert-data
         data-buf
         (niwa-agent-control-message-receipt '(:id "hidden-receipt"))
         nil)
        (with-current-buffer data-buf
          (when restored
            (let ((bounds (gptel--get-buffer-bounds)))
              (set-text-properties (point-min) (point-max) nil)
              (gptel--restore-props bounds)
              (niwa-transcript-restore-properties t)))
          ;; Delivery recovery must still be able to consume the receipt.
          (should (equal '((:type mailbox-receipt :id "hidden-receipt"))
                         (niwa-transcript-audit-records
                          (buffer-string) 'mailbox-receipt))))
        (with-current-buffer view-buf
          (niwa-view-test--insert-composer-draft "Keep this draft")
          (niwa-view--full-rerender)
          (should (equal "Keep this draft" (niwa-view-composer-draft-text nil t)))
          (should-not (string-search "hook audit" (niwa-view-render-test--view-text)))
          (should-not (string-search "hidden-receipt" (niwa-view-render-test--view-text)))
          (should (string-search
                   (pcase placement
                     ('response "Plan ready.")
                     ('tool "WaitAgent")
                     ('mailbox "Agent /root/reviewer finished"))
                   (niwa-view-render-test--view-text))))))))

(niwa-deftest niwa-view--format-injected-reminders-audit ()
  ,test
  (test)
  :doc "formats a grouped count row with per-entry labels"
  (let* ((record '(:type injected-reminders
                   :phase turn-start
                   :items ((:type skills-delta :body "roster changed")
                           (:type (specialist . read) :body "use Xref"))))
         (collapsed (niwa-view--format-injected-reminders-audit
                     record nil))
         (expanded (niwa-view--format-injected-reminders-audit
                    record t)))
    (should (equal "  ◇ 2 system reminders (skills-delta, specialist:read)\n"
                   collapsed))
    (should-not (string-match-p "roster changed" collapsed))
    (should (< (string-match "1\\. skills-delta" expanded)
               (string-match "2\\. specialist:read" expanded)))
    (should (string-match-p "roster changed" expanded))
    (should (string-match-p "use Xref" expanded)))
  :doc "uses the singular form for one reminder"
  (should (string-prefix-p
           "  ◇ 1 system reminder (date-change)"
           (niwa-view--format-injected-reminders-audit
            '(:type injected-reminders
              :phase mid-turn
              :items ((:type date-change :body "today")))
            nil))))

(niwa-deftest niwa-view--insert-rendered-tool/hook-audits ()
  ,test
  (test)
  :doc "renders updated tool input and result audit details"
  (with-temp-buffer
    (niwa-view-mode)
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
      (niwa-view--insert-rendered-tool rendering (cons 1 40))
      (let ((text (buffer-string)))
        (should (string-match-p "hook changed tool input" text))
        (should (string-match-p "hook changed tool result" text))
        (should-not (string-match-p "original result" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool input")
      (niwa-view-toggle-section)
      (let ((text (buffer-string)))
        (should (string-match-p "Original input" text))
        (should (string-match-p ":file_path \"/tmp/old\"" text))
        (should (string-match-p "Updated input" text))
        (should (string-match-p ":file_path \"/tmp/new\"" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool result")
      (niwa-view-toggle-section)
      (let ((text (buffer-string)))
        (should (string-match-p "Original result" text))
        (should (string-match-p "original result" text))
        (should (string-match-p "Updated result" text))
        (should (string-match-p "updated result" text)))))

  :doc "renders committed and abandoned repair audits with value-free details"
  (with-temp-buffer
    (niwa-view-mode)
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
      (niwa-view--insert-rendered-tool rendering (cons 1 40))
      (let ((text (buffer-string)))
        (should (string-match-p "tool input repaired" text))
        (should (string-match-p "tool input repair abandoned" text))
        (should-not (string-match-p "Rule:" text)))
      (goto-char (point-min))
      (search-forward "tool input repaired")
      (niwa-view-toggle-section)
      (let ((text (buffer-string)))
        (should (string-match-p "Rule: wrap-array-singleton" text))
        (should (string-match-p "Path: names" text))
        (should (string-match-p "Shape: string -> array" text))
        (should-not (string-match-p "alice\|ordinary result" text)))))

  :doc "malformed repair audit metadata falls back without exposing values"
  (with-temp-buffer
    (niwa-view-mode)
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
      (niwa-view--insert-rendered-tool rendering (cons 1 40))
      (let ((text (buffer-string)))
        (should (string-match-p "repair audit unavailable" text))
        (should-not (string-match-p "sentinel\|private\|raw-value" text)))
      (goto-char (point-min))
      (search-forward "repair audit unavailable")
      (niwa-view-toggle-section)
      (should-not
       (string-match-p "sentinel\|private\|raw-value" (buffer-string)))))

  :doc "repair audit normalization errors use the unavailable fallback"
  (with-temp-buffer
    (niwa-view-mode)
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
                  'niwa-tool-repair-normalize-audit-record)
                 (lambda (&rest _) (error "private audit sentinel"))))
        (niwa-view--insert-rendered-tool rendering (cons 1 40)))
      (should (string-match-p "repair audit unavailable" (buffer-string)))
      (should-not (string-match-p "private\|sentinel" (buffer-string)))))

  :doc "ordinary valid tools retain their undecorated compact rendering"
  (with-temp-buffer
    (niwa-view-mode)
    (let ((inhibit-read-only t))
      (niwa-view--insert-rendered-tool
       '(:header "Read: file.el" :body "ordinary result"
                 :initially-collapsed-p t)
       (cons 1 40))
      (should-not (string-match-p "repair" (buffer-string))))))

(niwa-deftest niwa-view--repair-audit-reconstruction
  ()
  ,test
  (test)

  :doc "reconstructs a committed audit beside a persisted handler error"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     (concat
      "(:name \"Collect\" :args (:names [\"alice\"]))\n\n"
      "Error: handler exploded"
      (niwa-tool-repair-format-audit-block
       'committed
       '((:rule wrap-array-singleton :source generic
               :paths ((names)) :before string :after array)))
      "\n")
     '(tool . "repair-error"))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "Collect" text))
        (should (string-match-p "tool input repaired" text))
        (should-not (string-match-p "alice\|handler exploded" text)))
      (goto-char (point-min))
      (search-forward "tool input repaired")
      (niwa-view-toggle-section)
      (should (string-match-p "Rule: wrap-array-singleton"
                              (niwa-view-render-test--view-text))))))


;;
;;; Tool-call parsing with render-data

(niwa-deftest niwa-view--render-tool-call/cold-load ()
  ,test
  (test)
  :doc "loads pending execution data through its transcript owner"
  (let ((root
         (file-name-as-directory
          (file-name-directory (locate-library "niwa"))))
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
               (require 'niwa-view-render)
               (with-temp-buffer
                 (cl-letf
                     (((symbol-function 'niwa-tool-get) #'ignore)
                      ((symbol-function
                        'niwa-view--generic-tool-rendering)
                       (lambda (&rest _) '(:vtype tool))))
                   (niwa-view--render-tool-call
                    '(:name "Bash" :args nil :tool-use-id "cold"
                            :result "" :render-data nil)
                    (current-buffer))))
               (unless (featurep 'niwa-execution-transcript)
                 (error "Execution transcript owner was not loaded")))))))
      (should (string-empty-p (string-trim (buffer-string)))))))

(niwa-deftest niwa-view--tool-readable-start
  (:doc "reads the real call after scaffolding without copying its result")
  (dolist (prefix '("" "\n\n" "#+end_reasoning\n\n#+begin_tool\n"
                    ":PROPERTIES:\n:X: (:name \"Drawer\")\n:END:\n#+begin_tool\n"
                    "#+begin_tool (:name \"Marker\")\n"))
    (let* ((call "(:name \"Read\" :args (:file_path \"file.el\"))")
           (raw (concat prefix call "\n\n  result\n(:name \"Quoted\")\n#+end_tool\n"))
           (start (niwa-view--tool-readable-start raw))
           (parsed (read-from-string raw start)))
      (should (= start (length prefix)))
      (should (equal (plist-get (car parsed) :name) "Read"))
      (should (string-prefix-p "\n\n  result\n" (substring raw (cdr parsed)))))))

(niwa-deftest niwa-view--tool-call-parse ()
  ,test
  (test)
  :doc "cold-loads exact audit source ranges through the disclosure owner"
  (let ((root
         (file-name-as-directory
          (file-name-directory (locate-library "niwa"))))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (should
       (= 0
          (call-process
           emacs nil t nil "--batch" "-Q" "-L" root
           "--eval"
           (prin1-to-string
            '(progn
               (require 'niwa-view-render)
               (require 'niwa-transcript-audit)
               (require 'niwa-utilities)
               (with-temp-buffer
                 (niwa-view-render-initialize)
                 (insert
                  "(:name \"Read\" :args (:file_path \"audit.el\"))\n\n"
                  (niwa--format-hook-audit-record
                   '(:type tool-result-rewrite
                           :event "PostToolUse"
                           :original-result "raw"
                           :updated-result "clean")))
                 (add-text-properties
                  (point-min) (point-max) '(gptel (tool . "cold")))
                 (let* ((call (niwa-view--tool-call-parse
                               (current-buffer) (point-min) (point-max)))
                        (source (plist-get
                                 (car (plist-get call :hook-audits))
                                 :source)))
                   (unless (and call
                                (featurep 'niwa-view-disclosure)
                                (markerp (car-safe source))
                                (markerp (cdr-safe source)))
                     (error "Audit disclosure owner did not load")))))))))
      (should (string-empty-p (string-trim (buffer-string))))))
  :doc "extracts name, args, and result from a tool segment"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nplain result\n"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (niwa-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "Read" (plist-get call :name)))
        (should (equal "call_1" (plist-get call :tool-use-id)))
        (should (equal '(:file_path "/tmp/f") (plist-get call :args)))
        (should (string-match-p "plain result" (plist-get call :result)))
        (should (null (plist-get call :render-data))))))
  :doc "keeps the leading indentation of the first result line"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\n  1\tfirst\n  2\tsecond\n"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (niwa-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "  1\tfirst\n  2\tsecond"
                       (plist-get call :result))))))
  :doc "renderer path survives incremental rerender starting inside tool run"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "prefix\n" 'response)
    (niwa-view-test--insert-data
     data-buf
     "\n(:name \"Grep\" :args (:pattern \"task\"))\n\nNo matches found\n"
     '(tool . "call_1"))
    (niwa-view-test--insert-data data-buf "suffix\n" 'response)
    (with-current-buffer data-buf
      (let* ((tool-start (next-single-property-change (point-min) 'gptel))
             (mid-start (+ tool-start 2))
             (mid-end (+ tool-start 12))
             (segs (niwa-transcript-segments mid-start mid-end))
             (tool-seg (car segs))
             (call (niwa-view--tool-call-parse
                    data-buf (cadr tool-seg) (caddr tool-seg))))
        (should (= 1 (length segs)))
        (should (equal "Grep" (plist-get call :name)))
        (should (equal '(:pattern "task") (plist-get call :args)))
        (should (string-match-p "No matches found" (plist-get call :result))))))
  :doc "parser skips leading org drawer and block scaffolding"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     (concat ":PROPERTIES:\n:GPTEL_MODEL: x\n:END:\n"
             "#+begin_tool (Read :file_path \"/tmp/f\")\n"
             "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\n"
             "file body\n"
             "#+end_tool\n")
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (niwa-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "Read" (plist-get call :name)))
        (should (string-match-p "file body" (plist-get call :result)))
        (should-not (string-match-p "GPTEL_MODEL"
                                    (plist-get call :result))))))
  :doc "decodes embedded render-data and strips it from :result"
  (niwa-view-test--with-buffers
    (let* ((render-data '(:kind diff :patch "--- a\n+++ b\n+hi\n"
                          :path "/tmp/f" :rel-path "f"))
           (body (concat "visible body"
                         (niwa-tool-render-data-format
                          render-data "call_1"))))
      (niwa-view-test--insert-data
       data-buf
       (concat "(:name \"Edit\" :args (:file_path \"/tmp/f\"))\n\n"
               body "\n")
       '(tool . "call_1"))
      (with-current-buffer data-buf
        (let ((call (niwa-view--tool-call-parse
                     data-buf (point-min) (point-max))))
	        (should (equal "Edit" (plist-get call :name)))
	        (should (equal "visible body" (plist-get call :result)))
	        (should (equal render-data (plist-get call :render-data)))))))
  :doc "keeps numbered render markers in Read output as literal text"
  (niwa-view-test--with-buffers
    (let ((literal
           (concat "158 " niwa-tool-render-data-open
                   "\n159 (:kind user-display :text \"literal\")"
                   "\n160 " niwa-tool-render-data-close)))
      (niwa-view-test--insert-data
       data-buf
       (concat "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\n"
               literal "\n")
       '(tool . "call_1"))
      (with-current-buffer data-buf
        (let ((call (niwa-view--tool-call-parse
                     data-buf (point-min) (point-max))))
          (should (string-match-p (regexp-quote literal)
                                  (plist-get call :result)))
          (should-not (plist-get call :render-data))
          (should (niwa-view--segment-rendering
                   data-buf (point-min) (point-max)))))))
  :doc "preserves literal trailing end-tool marker in unwrapped result"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nbody\n#+end_tool"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (niwa-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "Read" (plist-get call :name)))
        (should (equal "body\n#+end_tool" (plist-get call :result))))))
  :doc "recovers render-data when restored bounds start inside the tool marker"
  (niwa-view-test--with-buffers
    (let* ((render-data '(:kind diff :patch "--- a\n+++ b\n+hi\n"
                          :path "/tmp/f" :rel-path "f"))
           (block
            (concat "#+begin_tool (RecoverEdit :file_path \"/tmp/f\")\n"
                    "(:name \"RecoverEdit\" :args (:file_path \"/tmp/f\"))\n\n"
                    "visible body"
                    (niwa-tool-render-data-format
                     render-data "call_1")
                    "#+end_tool\n")))
      (niwa-view-test--insert-data data-buf block '(tool . "call_1"))
      (with-current-buffer data-buf
        (let (seg-start seg-end)
          (goto-char (point-min))
          (search-forward ":file_path")
          (setq seg-start (match-beginning 0))
          (search-forward "visible body")
          (setq seg-end (match-end 0))
          (let ((call (niwa-view--tool-call-parse
                       data-buf seg-start seg-end)))
            (should (equal "RecoverEdit" (plist-get call :name)))
            (should (equal "visible body" (plist-get call :result)))
            (should (equal render-data (plist-get call :render-data))))
          (niwa-tool-register
           (niwa-tool--create
            :name "RecoverEdit"
            :category "niwa"
            :renderer
            (lambda (_name _args _result data)
              (and (eq (plist-get data :kind) 'diff)
                   (list :header "Recovered diff"
                         :body (plist-get data :patch))))))
          (should (equal "Recovered diff"
                         (plist-get
                          (niwa-view--segment-rendering
                           data-buf seg-start seg-end)
                          :header)))))))
  :doc "recovers full tool body when result contains marker-looking lines"
  (niwa-view-test--with-buffers
    (let ((block (concat "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "before\n"
                "#+end_tool\n"
                         "After\n"
                         "#+end_tool\n")))
      (niwa-view-test--insert-data data-buf block '(tool . "call_1"))
      (with-current-buffer data-buf
        (let (seg-start seg-end)
          (goto-char (point-min))
          (search-forward ":file_path")
          (setq seg-start (match-beginning 0))
          (search-forward "After")
          (setq seg-end (match-end 0))
          (let ((call (niwa-view--tool-call-parse
                       data-buf seg-start seg-end)))
            (should (equal "RecoverRead" (plist-get call :name)))
            (should (equal "before\n#+end_tool\nAfter"
                           (plist-get call :result))))))))
  :doc "recovers full tool body when stale bounds stop before a literal close"
  (niwa-view-test--with-buffers
    (let ((block (concat "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                         "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                         "before\n"
                         "#+end_tool\n"
                         "after\n"
                         "#+end_tool\n")))
      (niwa-view-test--insert-data data-buf block '(tool . "call_1"))
      (with-current-buffer data-buf
        (let (seg-start seg-end)
          (goto-char (point-min))
          (search-forward ":file_path")
          (setq seg-start (match-beginning 0))
          (search-forward "before")
          (setq seg-end (match-end 0))
          (let ((call (niwa-view--tool-call-parse
                       data-buf seg-start seg-end)))
            (should (equal "RecoverRead" (plist-get call :name)))
            (should (equal "before\n#+end_tool\nafter"
                           (plist-get call :result))))))))
  :doc "keeps persisted-looking tool text inside unwrapped tool results"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
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
      (let ((call (niwa-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "Read" (plist-get call :name)))
        (should (string-match-p "outer before" (plist-get call :result)))
        (should (string-match-p
                 "#\\+begin_tool (Bash :command \"echo nested\")"
                 (plist-get call :result)))
        (should (string-match-p "outer after" (plist-get call :result))))))
  :doc "recovers full tool body when result contains persisted-looking tool text"
  (niwa-view-test--with-buffers
    (let ((block (concat "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                         "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                         "outer before\n"
                         "#+begin_tool (Bash :command \"echo nested\")\n"
                         "(:name \"Bash\" :args (:command \"echo nested\"))\n"
                         "nested result\n"
                         "#+end_tool\n"
                         "outer after\n"
                         "#+end_tool\n")))
      (niwa-view-test--insert-data data-buf block '(tool . "call_1"))
      (with-current-buffer data-buf
        (let (seg-start seg-end)
          (goto-char (point-min))
          (search-forward ":file_path")
          (setq seg-start (match-beginning 0))
          (search-forward "outer after")
          (setq seg-end (match-end 0))
          (let ((call (niwa-view--tool-call-parse
                       data-buf seg-start seg-end)))
            (should (equal "RecoverRead" (plist-get call :name)))
            (should (string-match-p
                     "#\\+begin_tool (Bash :command \"echo nested\")"
                     (plist-get call :result)))
            (should (string-match-p
                     "outer after"
                     (plist-get call :result))))))))
  :doc "does not parse a previous quoted tool block as a later tool call"
  (niwa-view-test--with-buffers
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
        (let ((call (niwa-view--tool-call-parse
                     data-buf (+ real-start 20) (- real-end 12))))
          (should (equal "Bash" (plist-get call :name)))
          (should (equal "real output" (plist-get call :result)))))))
  :doc "unescapes org-mode tool result storage markers"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "(:name \"WebFetch\" :args (:url \"https://example.com\"))\n\n,* Heading\n,,* Literal comma-star\n,#+begin_src text\nbody\n,#+end_src\n"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (niwa-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "* Heading\n,* Literal comma-star\n#+begin_src text\nbody\n#+end_src"
                       (plist-get call :result))))))
  :doc "returns nil on unreadable segments"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf "(:unclosed\n" '(tool . "call_1"))
    (with-current-buffer data-buf
      (should (null (niwa-view--tool-call-parse
                     data-buf (point-min) (point-max))))))

  :doc "splits compacted malformed tools before the next real tool block"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (let (second-start)
        (insert "#+begin_tool (Edit :file_path \"niwa-chat.el\" :old_string \"...\")\n")
        (let ((bad-start (point)))
          (insert "(:name \"Edit\" :args (:file_path \"niwa-chat.el\" :old_string \"unterminated\n")
          (insert "[niwa: tool output truncated; omitted 8858 chars]\n")
          (put-text-property bad-start (point) 'gptel '(tool . "bad-edit")))
        (setq second-start (point))
        (insert "#+begin_tool (Read :file_path \"next.el\")\n"
                "(:name \"Read\" :args (:file_path \"next.el\"))\n\n"
                "body\n#+end_tool\n")
        (put-text-property second-start (point) 'gptel '(tool . "read"))
        (let ((tool-segs (cl-remove-if-not
                          (lambda (seg) (eq (car seg) 'tool))
                          (niwa-transcript-segments
                           (point-min) (point-max)))))
          (should (= 2 (length tool-segs)))
          (let ((second-call (niwa-view--tool-call-parse
                              data-buf (cadr (cadr tool-segs))
                              (caddr (cadr tool-segs)))))
            (should (equal "Read" (plist-get second-call :name)))))))))


(niwa-deftest niwa-view--tool-call-parse/malformed-without-marker ()
  ,test
  (test)
  :doc "splits malformed tools before the next real tool block without marker"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (let (second-start)
        (insert "#+begin_tool (Edit :file_path \"niwa-chat.el\" :old_string \"...\")\n")
        (let ((bad-start (point)))
          (insert "(:name \"Edit\" :args (:file_path \"niwa-chat.el\" :old_string \"unterminated\n")
          (put-text-property bad-start (point) 'gptel '(tool . "bad-edit")))
        (setq second-start (point))
        (insert "#+begin_tool (Read :file_path \"next.el\")\n"
                "(:name \"Read\" :args (:file_path \"next.el\"))\n\n"
                "body\n#+end_tool\n")
        (put-text-property second-start (point) 'gptel '(tool . "read"))
        (let ((tool-segs (cl-remove-if-not
                          (lambda (seg) (eq (car seg) 'tool))
                          (niwa-transcript-segments
                           (point-min) (point-max)))))
          (should (= 2 (length tool-segs)))
          (let ((second-call (niwa-view--tool-call-parse
                              data-buf (cadr (cadr tool-segs))
                              (caddr (cadr tool-segs)))))
            (should (equal "Read" (plist-get second-call :name)))))))))


(niwa-deftest niwa-view--tool-fallback-line ()
  ,test
  (test)
  :doc "uses org tool headers for malformed compact fallback summaries"
  (let* ((line (niwa-view--tool-fallback-line
                "#+begin_tool (Edit :file_path \"niwa-chat.el\" :old_string \"...\")\n(:name \"Edit\" :args (:file_path \"niwa-chat.el\" :old_string \"unterminated\n[niwa: tool output truncated; omitted 8858 chars]\n"))
         (plain (substring-no-properties line)))
    (should (string-match-p "Edit" plain))
    (should (string-match-p "niwa-chat.el" plain))
    (should-not (string-match-p "#\\+begin_tool" plain))))


(niwa-deftest niwa-view--renderer-idempotent ()
  ,test
  (test)
  :doc "invoking the renderer twice with identical inputs yields equal plists"
  (let* ((calls 0)
         (tool (niwa-tool--create
                :name "Idem"
                :renderer (lambda (_name _args _result data)
                            (cl-incf calls)
                            (list :header (format "I:%s" (plist-get data :n))
                                  :body "b"
                                  :body-mode 'diff-mode))))
         (data '(:n 7))
         (args '(:x 1))
         (result "done"))
    (let ((first (niwa-view--invoke-renderer tool data args result))
          (second (niwa-view--invoke-renderer tool data args result)))
      (should (equal first second))
      (should (= 2 calls))))
  :doc "round-trip through serialization preserves render-data"
  (let* ((data '(:kind diff :patch "@@ @@\n+a\n" :path "/tmp/x"))
         (serialized (niwa-tool-render-data-format data))
         (extract (niwa-tool-render-data-extract
                   (concat "result" serialized))))
    (should (equal data (cdr extract)))))

(niwa-deftest niwa-view--collaboration-event-render-data ()
  ,test
  (test)
  :doc "renders a direct workflow's canonical started event"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "/review current changes\n"
                                    nil)
    (niwa-view-test--insert-data
     data-buf
     (niwa-tool-render-data-format
      '(:kind collaboration-event
        :event started
        :path "/root/review"
        :status running
        :body ""))
     'ignore)
    (niwa-view-test--insert-data data-buf "No issues.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-search "/review current changes" text))
        (should (string-search "Started /root/review" text))
        (should (string-search "No issues." text))))))

(niwa-deftest niwa-view--scaffolding-only-p ()
  ,test
  (test)
  :doc "marker-only and blank-only segments are scaffolding"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "\n#+end_tool\n\n#+begin_tool (Bash :command \"echo hi\")\n"))
    (should (niwa-view--scaffolding-only-p
             data-buf (point-min) (with-current-buffer data-buf (point-max)))))

  :doc "real reasoning content is not scaffolding"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_reasoning\nLet me think about this.\n#+end_reasoning\n"))
    (should-not (niwa-view--scaffolding-only-p
                 data-buf (point-min) (with-current-buffer data-buf (point-max))))))

(niwa-deftest niwa-view--response-summary ()
  ,test
  (test)
  :doc "keeps org source block markers in response summaries"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src\n"
     'response)
    (let ((summary (niwa-view--response-summary
                    data-buf
                    (with-current-buffer data-buf (point-min))
                    (with-current-buffer data-buf (point-max)))))
      (should (string-match-p "#\\+begin_src emacs-lisp" summary))
      (should-not (string-match-p "```emacs-lisp" summary)))))

(niwa-deftest niwa-view-cleanup/no-op ()
  ,test
  (test)
  :doc "plain text retains its identity and properties"
  (let* ((case-fold-search nil)
         (text (propertize "Ordinary content\nwith properties" 'face 'bold)))
    (dolist (cleanup '(niwa-view--strip-tool-blocks-from-reasoning
                       niwa-view--strip-system-reminder-blocks
                       niwa-view--strip-model-context-blocks))
      (should (eq text (funcall cleanup text)))))
  :doc "optional reasoning and reminder text still accepts nil"
  (dolist (cleanup '(niwa-view--strip-tool-blocks-from-reasoning
                     niwa-view--strip-system-reminder-blocks))
    (should (equal "" (funcall cleanup nil))))
  :doc "matching and incomplete blocks retain their cleanup semantics"
  ;; A fresh temporary buffer inherits the default rather than a caller's
  ;; buffer-local setting.  Guards must not bypass uppercase blocks.
  (let ((case-fold-search t))
    (with-temp-buffer
      (setq-local case-fold-search nil)
      (should (equal "kept\n" (niwa-view--strip-tool-blocks-from-reasoning
                               "kept\n#+BEGIN_TOOL\nhidden\n#+END_TOOL\n")))
      (should (equal "kept\n" (niwa-view--strip-tool-blocks-from-reasoning
                               "kept\n#+begin_tool\nunfinished")))
      (should (equal "kept" (niwa-view--strip-system-reminder-blocks
                             "kept<SYSTEM-REMINDER>hidden</SYSTEM-REMINDER>")))
      (should (equal "kept<system-reminder>unfinished"
                     (niwa-view--strip-system-reminder-blocks
                      "kept<system-reminder>unfinished")))
      (should (equal "kept\n\nend"
                     (niwa-view--strip-model-context-blocks
                      "kept<GOAL-CONTEXT authority=\"session\">hidden</GOAL-CONTEXT>\n\n\nend")))
      (should (equal "kept\n\nend"
                     (niwa-view--strip-model-context-blocks "kept\n\n\nend")))
      (should (equal "kept<hook-context>unfinished"
                     (niwa-view--strip-model-context-blocks
                      "kept<hook-context>unfinished"))))))

(niwa-deftest niwa-view--user-turn-text/marker-cleanup ()
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert (propertize "kept " 'face 'bold)
              "#+begin_reasoning\n"
              "thinking\n#+end_reasoning\n"
              "#+begin_tool name\nbody\n#+end_tool\n"))
    (let ((text (niwa-view--user-turn-text
                 (list (list 'user 1 (with-current-buffer data-buf (point-max))))
                 data-buf)))
      (should (equal "kept thinking\nbody" text))
      (should (eq 'bold (get-text-property 0 'face text)))
      (aset text 0 ?X)
      (should (with-current-buffer data-buf (eq (char-after 1) ?k))))))

(niwa-deftest niwa-view-cleanup/default-case-folding ()
  (let ((old (default-value 'case-fold-search))
        (text "<SYSTEM-REMINDER>visible</SYSTEM-REMINDER><GOAL-CONTEXT>visible</GOAL-CONTEXT>\n#+BEGIN_TOOL\nvisible\n#+END_TOOL"))
    (unwind-protect
        (progn
          (set-default 'case-fold-search nil)
          (dolist (cleanup '(niwa-view--strip-tool-blocks-from-reasoning
                             niwa-view--strip-system-reminder-blocks
                             niwa-view--strip-model-context-blocks))
            (should (eq text (funcall cleanup text)))))
      (set-default 'case-fold-search old))))

(niwa-deftest niwa-view--user-turn-text/drawer-strip ()
  ,test
  (test)
  :doc "leading :PROPERTIES: drawer is stripped from user turn text"
  ;; Without the strip, gptel-org's per-buffer state drawer (system
  ;; prompt, model, GPTEL_BOUNDS) would render verbatim inside the
  ;; visible "You" turn on a full rerender that didn't pre-narrow past
  ;; the drawer.
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert ":PROPERTIES:\n:GPTEL_SYSTEM: hidden system prompt\n:END:\n")
      (insert "Real user prompt here.\n"))
    (let* ((seg (list 'user (point-min)
                      (with-current-buffer data-buf (point-max))))
           (text (niwa-view--user-turn-text (list seg) data-buf)))
	  (should (string-match-p "Real user prompt" text))
	  (should-not (string-match-p "GPTEL_SYSTEM" text))
	  (should-not (string-match-p "hidden system prompt" text))))

  :doc "a user-typed leading Org heading is prompt text"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "\n\n* Plan\n:PROPERTIES:\n:ID: 1\n:END:\nDo it.\n"))
    (let* ((seg (list 'user (point-min)
                      (with-current-buffer data-buf (point-max))))
           (text (niwa-view--user-turn-text (list seg) data-buf)))
      (should (equal "* Plan\n:PROPERTIES:\n:ID: 1\n:END:\nDo it." text))))

  :doc "an agent task heading after separator whitespace shows its title"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "\n\n* Agent Task: Verify\n\nCheck it.\n"))
    (let* ((seg (list 'user (point-min)
                      (with-current-buffer data-buf (point-max))))
           (text (niwa-view--user-turn-text (list seg) data-buf)))
      (should (equal "Agent Task: Verify\n\nCheck it." text))))

  :doc "an agent task heading's property drawer is stripped"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "* Agent Task: Verify\n:PROPERTIES:\n"
              ":NIWA_AGENT_PATH: /root/verify\n:END:\n\nCheck it.\n"))
    (let* ((seg (list 'user (point-min)
                      (with-current-buffer data-buf (point-max))))
           (text (niwa-view--user-turn-text (list seg) data-buf)))
      (should (equal "Agent Task: Verify\n\nCheck it." text))))

  :doc "hook context blocks are stripped from visible user turn text"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "Real user prompt here.\n\n")
      (insert "<hook-context>\n")
      (insert "Model-only context.\n")
      (insert "</hook-context>\n"))
    (let* ((seg (list 'user (point-min)
                      (with-current-buffer data-buf (point-max))))
           (text (niwa-view--user-turn-text (list seg) data-buf)))
      (should (string-match-p "Real user prompt" text))
      (should-not (string-match-p "hook-context" text))
      (should-not (string-match-p "Model-only context" text))))

  :doc "Goal context blocks are stripped from visible user turn text"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "<goal-context authority=\"session-sidecar\">\n")
      (insert "Phase: planning\n")
      (insert "</goal-context>\n\n")
      (insert "Planning instructions:\nDo nothing.\n")
      (let ((start (point)))
        (insert (niwa-tool-render-data-format
                 '(:kind user-display :text "Dry-run Goal")))
        (add-text-properties start (point) '(gptel ignore))))
    (let* ((segments
            (with-current-buffer data-buf
              (niwa-transcript-segments (point-min) (point-max))))
           (text (niwa-view--user-turn-text segments data-buf)))
      (should (equal "Dry-run Goal" text))
      (should-not (string-match-p "Planning instructions" text))
      (should-not (string-match-p "goal-context" text))
      (should-not (string-match-p "Phase: planning" text))))

  :doc "hook context renders as a collapsible view-only disclosure"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     (concat "Real user prompt here.\n\n"
             "<hook-context>\n"
             "<hook-event name=\"UserPromptSubmit\">\n"
             "Model-only context.\n"
             "</hook-event>\n"
             "</hook-context>\n")
     nil)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "Real user prompt" text))
        (should (string-match-p "◇ hook context added" text))
        (should-not (string-match-p "Model-only context" text)))
      (goto-char (point-min))
      (search-forward "hook context added")
      (niwa-view-toggle-section)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "◇ hook context added" text))
        (should (string-match-p "UserPromptSubmit" text))
        (should (string-match-p "Model-only context" text)))))

  :doc "event-tagged hook context renders one combined disclosure"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
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
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (= 1 (niwa-view-test--count-substring
                      "hook context added" text)))
        (should (string-match-p "Real user prompt" text))
        (should-not (string-match-p "Startup context" text))
        (should-not (string-match-p "Prompt context" text)))
      (goto-char (point-min))
      (search-forward "hook context added")
      (niwa-view-toggle-section)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "SessionStart" text))
        (should (string-match-p "Startup context" text))
        (should (string-match-p "UserPromptSubmit" text))
        (should (string-match-p "Prompt context" text))
        (should-not (string-match-p "<hook-event" text)))
      (niwa-view-test--insert-composer-draft "> keep\nsecond line" 3)
      (niwa-view--full-rerender)
      (should (= 1 (niwa-view-test--count-substring
                    "hook context added" (niwa-view-render-test--view-text))))
      (dotimes (_ 3)
        (save-excursion
          (goto-char (point-min))
          (search-forward "Prompt context")
          (let ((body-start (match-beginning 0)))
            (should (equal "    " (get-text-property body-start 'line-prefix)))
            (should (equal "    " (get-text-property body-start 'wrap-prefix)))
            (beginning-of-line)
            (should (looking-at-p "Prompt context"))))
        (niwa-view--full-rerender)
        (should (= 1 (niwa-view-test--count-substring
                      "hook context added" (niwa-view-render-test--view-text))))
        (should (equal "> keep\nsecond line"
                       (niwa-view-composer-draft-text nil t)))
        (should (= 3 (niwa-view-render-test--draft-offset))))))

  :doc "attributed hook context preserves escaped text and the composer draft"
  (niwa-view-test--with-buffers
    (let ((body "literal </hook-event> & <tag>")
          (source-file "/tmp/hook \"quoted\" & <local>.el")
          (plugin-name "test & <plugin>"))
      (niwa-view-test--insert-data
       data-buf
       (concat
        "Real user prompt here.\n\n"
        (niwa-hooks-format-context
         (list (list :event "UserPromptSubmit"
                     :source 'plugin :source-file source-file
                     :plugin-name plugin-name
                     :body body)))
        "\n")
       nil)
      (with-current-buffer data-buf
        (let ((text (buffer-string)))
          (should (string-match-p "&lt;/hook-event&gt;" text))
          (should-not (string-match-p "literal </hook-event>" text))))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 3)
        (niwa-view--full-rerender)
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "Real user prompt" text))
          (should (string-match-p "hook context added" text))
          (should-not (string-match-p "literal </hook-event>" text)))
        (goto-char (point-min))
        (search-forward "hook context added")
        (niwa-view-toggle-section)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "UserPromptSubmit" text))
          (should (string-match-p "source: plugin" text))
          (should (string-match-p (regexp-quote source-file) text))
          (should (string-match-p (regexp-quote plugin-name) text))
          (should (string-match-p (regexp-quote body) text))
          (should-not (string-match-p "&lt;/hook-event&gt;" text)))
        (should (equal "> draft\nsecond line"
                       (niwa-view-composer-draft-text nil t)))))))

(niwa-deftest niwa-view--visible-response-text ()
  ,test
  (test)
  :doc "proposed Plan protocol blocks stay out of rendered responses"
  (let ((text
         (niwa-view--visible-response-text
          (concat "Implemented.\n<proposed_plan>\n"
                  "Transition to review.\n</proposed_plan>\n"))))
    (should-not (string-match-p "Transition to review" text))
    (should-not (string-match-p "proposed_plan" text)))

  :doc "uses Plan protocol helpers loaded with the renderer"
  (should (featurep 'niwa-plan))
  (let ((text
         (niwa-view--visible-response-text
          "<proposed_plan>\nHidden\n</proposed_plan>\nVisible\n")))
    (should-not (string-match-p "Hidden" text))
    (should (string-match-p "Visible" text))))

(niwa-deftest niwa-view--permission-approval-reason ()
  ,test
  (test)
  :doc "accepts only reviewer allow-once facts and bounds the plain tooltip"
  (progn
    (dolist (records '(nil ((:source human :outcome allow-once :reason "Human"))
                          ((:source reviewer :outcome deny :reason "Denied"))
                          ((:source reviewer :outcome allow-once :reason 42))))
      (should-not (niwa-view--permission-approval-reason
                   (list :permission-approvals records))))
    (let ((reason (niwa-view--permission-approval-reason
                   (list :permission-approvals
                         (list (list :source 'reviewer :outcome 'allow-once
                                     :reason (propertize (make-string 2000 ?x)
                                                         'face 'error)))))))
      (should (<= (length reason) 1024))
      (should-not (text-properties-at 0 reason))))
  :doc "nested and direct ToolCall rows keep the child's hint off the envelope"
  (niwa-view-test--with-buffers
    (niwa-tool-register
     (niwa-tool--create :name "ToolCall" :category "niwa"
                           :renderer #'niwa-tool-ptc--render))
    (let* ((approved '(:permission-approvals
                      ((:source reviewer :outcome allow-once :reason "Child only"))))
           (child (list :id "outer/1" :tool "ApprovalProbe" :status 'running
                        :args nil :result "payload" :render-data approved))
           (data (list :kind 'ptc :status 'success :outcome 'completed :calls (list child))))
      (with-current-buffer view-buf
        (dolist (status '(running error success))
          (setf (plist-get child :status) status)
          (let* ((rendering (niwa-view--child-call-rendering child))
                 (line (niwa-view--rendering-header-line rendering)))
            (should (eq status (plist-get rendering :status)))
            (should (string-search "Auto-approved · once" line))
            (should (equal "Child only" (plist-get rendering :permission-approval-reason)))))
        (let ((outer (niwa-view--render-tool-call
                      (list :name "ToolCall" :args '(:expression "(progn (ApprovalProbe))")
                            :result "payload" :render-data data) data-buf)))
          (should-not (plist-get outer :permission-approval-reason)))
        (let ((direct (niwa-view--render-tool-call
                       (list :name "ToolCall" :args '(:expression "(ApprovalProbe)")
                             :result "payload" :render-data
                             (plist-put (copy-sequence data) :direct-tool "ApprovalProbe"))
                       data-buf)))
          (should (equal "Child only" (plist-get direct :permission-approval-reason)))))))
  :doc "original Bash progress retains event approvals, while a poll does not inherit them"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((records '((:source reviewer :outcome allow-once :reason "Original execution")))
             (facts '(:kind execution :execution-id "exec-approved" :state running))
             (niwa-view--execution-events (make-hash-table :test #'equal)))
        (puthash "original" (list :type 'progress :facts facts
                                   :permission-approvals records :output-tail "work")
                 niwa-view--execution-events)
        (let ((row (niwa-view--render-tool-call
                    '(:name "Bash" :tool-use-id "original" :args (:command "work")
                      :result "yielded") data-buf)))
          (should (equal "Original execution" (plist-get row :permission-approval-reason))))
        (niwa-tool-register
         (niwa-tool--create :name "ToolCall" :category "niwa"
                               :renderer #'niwa-tool-ptc--render))
        (let* ((child '(:id "original" :tool "Bash" :status success
                       :args (:command "work") :result "yielded"))
               (direct (list :name "ToolCall" :args '(:expression "(Bash :command \"work\")")
                             :result "yielded" :render-data
                             (list :kind 'ptc :outcome 'completed
                                   :direct-tool "Bash" :calls (list child)))))
          (dolist (row (list (niwa-view--child-call-rendering child)
                             (niwa-view--render-tool-call direct data-buf)))
            (should (equal "Original execution" (plist-get row :permission-approval-reason)))))
        (let ((row (niwa-view--render-tool-call
                    (list :name "WriteStdin" :tool-use-id "poll" :args '(:chars "")
                          :result "observed" :render-data
                          (append facts '(:execution-control poll :control-succeeded-p t)))
                    data-buf)))
          (should-not (plist-get row :permission-approval-reason)))))))

(ert-deftest niwa-view-permission-approval-restored-row ()
  "An accepted approval stays factual beside a failed tool after restoration."
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool\n"
              (propertize
               (concat "(:name \"ApprovalProbe\" :args (:value 1))\nError: failed\n"
                       (niwa-tool-render-data-format
                        '(:status error :permission-approvals
                          ((:source reviewer :outcome allow-once :reason "Read-only verification")))
                        "approval-row"))
               'gptel '(tool . "approval-row"))
              "#+end_tool\n")
      (niwa-session-artifacts-stabilize-gptel-bounds))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4))
    (dotimes (restored 2)
      (when (= restored 1)
        (with-current-buffer data-buf
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (erase-buffer)
            (insert text)
            (niwa-transcript-restore-properties))))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text nil t)))
        (should (= 4 (niwa-view-render-test--draft-offset)))
        (save-excursion
          (goto-char (point-min))
          (search-forward "× ApprovalProbe")
          (search-forward "Auto-approved · once")
          (should (equal "Read-only verification"
                         (get-text-property (match-beginning 0) 'help-echo)))
          (should (eq 'niwa-view-tool-metadata
                      (get-text-property (match-beginning 0) 'font-lock-face)))
          (should-not (get-text-property (match-beginning 0)
                                         'niwa-view-inline-spinner-frame)))))))

(ert-deftest niwa-view-agent-result-canonical-outcomes ()
  "Live and restored result cards use envelope outcomes, not payload prose."
  (dolist (case '(("errored" "failed" error)
                  ("completed" "finished" niwa-view-tool-metadata)
                  ("interrupted" "interrupted" niwa-view-tool-metadata)
                  (nil "incomplete" niwa-view-tool-metadata)
                  ("bogus" "incomplete" niwa-view-tool-metadata)))
    (ert-info ((format "Outcome %S" (car case)))
      (niwa-view-test--with-buffers
        (with-current-buffer view-buf
          (niwa-view--full-rerender)
          (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4))
        (niwa-view-test--insert-data
         data-buf
         (format (concat "<agent-result sender=\"/root/worker\"%s>\n"
                         "Success; quoted error: pipe closed.\n</agent-result>\n")
                 (if (car case) (format " outcome=\"%s\"" (car case)) ""))
         nil)
        (dotimes (restored 2)
          (when (= restored 1)
            (with-current-buffer data-buf
              (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                (erase-buffer)
                (insert text)
                (niwa-transcript-restore-properties))))
          (with-current-buffer view-buf
            (niwa-view--full-rerender)
            (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text nil t)))
            (should (= 4 (niwa-view-render-test--draft-offset)))
            (save-excursion
              (goto-char (point-min))
              (search-forward (concat "● Agent /root/worker " (cadr case)))
              (should (eq (nth 2 case)
                          (get-text-property (match-beginning 0) 'font-lock-face)))
              (search-forward "Success; quoted error: pipe closed.")
              (should (get-text-property (match-beginning 0)
                                         'niwa-view-mailbox-body)))))))))

(niwa-deftest niwa-view--decorate-mailbox-block
  (:doc "renders pure mailbox deliveries as message cards")
  ,test
  (test)

  :doc "all non-empty mailbox bodies collapse by default"
  (should (= 0 niwa-view-mailbox-collapse-line-threshold))

  :doc "pure agent-message turn renders without a You header"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "<agent-message type=\"MAIL\" sender=\"/root/explorer\" recipient=\"/root\">\nhello\n</agent-message>\n"
     nil)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "^  ✉ message from /root/explorer" text))
        (should (string-match-p "hello" text))
        (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text)))
      (goto-char (point-min))
      (search-forward "message from ")
      (should (eq 'niwa-view-mailbox-header
                  (get-text-property (- (point) (length "from "))
                                     'font-lock-face)))
      (search-forward "hello")
      (should (invisible-p (match-beginning 0)))))

  :doc "incomplete agent-message opener stays ordinary user text"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "<agent-message sender=\"/root/explorer\" recipient=\"/root\">\npartial\n"
     nil)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "You\n" text))
        (should (string-match-p "<agent-message sender=" text))
        (should (string-match-p "partial" text))
        (should-not (string-match-p "message from /root/explorer" text)))))

  :doc "ordinary root mail ending in Bash XML remains ordinary"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     (concat
      "<agent-message type=\"MAIL\" sender=\"/root\" recipient=\"/root\">\n"
      "literal\n"
      "<bash-execution execution_id=\"ordinary\" outcome=\"success\"/>\n"
      "</agent-message>\n")
     nil)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "message from /root" text))
        (should (string-match-p "ordinary" text))
        (should-not (string-match-p "Bash completed" text)))))

  :doc "forwarded Bash completion renders a compact notice with a collapsed body"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     (concat
      "<agent-message type=\"EXECUTION\" sender=\"/root/worker\" recipient=\"/root\" "
      "execution_id=\"exec-000001\" description=\"Run build\" command=\"make\" "
      "termination=\"exited\" outcome=\"success\" exit_code=\"0\">\n"
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
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p
                 "^  ● Background command \"Run build\" completed (exit code 0)$"
                 text))
        (should-not (string-match-p "lines? collapsed" text))
        (should-not (string-match-p "message from /root" text)))
      (goto-char (point-min))
      (search-forward "● Background command")
      (should (get-text-property (match-beginning 0)
                                 'niwa-view-mailbox-notice))
      (should (eq 'niwa-view-tool-metadata
                  (get-text-property (match-beginning 0) 'font-lock-face)))
      (search-forward "[sandbox: bubblewrap")
      (should (invisible-p (match-beginning 0)))
      (niwa-view-toggle-section)
      (goto-char (point-min))
      (search-forward "[sandbox: bubblewrap")
      (should-not (invisible-p (match-beginning 0)))))

  :doc "Bash notice falls back to the command and reports failure"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     (concat
      "<agent-message type=\"EXECUTION\" sender=\"/root/worker\" recipient=\"/root\" "
      "execution_id=\"exec-2\" command=\"make test\" termination=\"exited\" "
      "outcome=\"failure\" exit_code=\"2\">\nfailing output\n</agent-message>\n")
     nil)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "● Background command \"make test\" failed (exit code 2)")
      (should (eq 'error (get-text-property (match-beginning 0)
                                            'font-lock-face)))))

  :doc "review results and child Bash completions retain separate notices and draft"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (niwa-view-test--insert-composer-draft "> keep\ndraft" 3))
    (niwa-view-test--insert-data
     data-buf
     (concat
      "<agent-result sender=\"/root/review\" recipient=\"/root\" outcome=\"completed\">\n"
      "{\"findings\": []}\n</agent-result>\n\n"
      "<agent-message type=\"EXECUTION\" sender=\"/root/review\" recipient=\"/root\" "
      "execution_id=\"exec-review\" description=\"Run tests\" "
      "termination=\"exited\" outcome=\"success\" exit_code=\"0\">\n"
      "Bash test output\n"
      "<bash-execution execution_id=\"exec-review\" state=\"completed\" "
      "termination=\"exited\" exit_code=\"0\" outcome=\"success\"/>\n"
      "</agent-message>\n")
     nil)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (should (equal "> keep\ndraft" (niwa-view-composer-draft-text nil t)))
      (goto-char (point-min))
      (search-forward "findings")
      (let ((result-card (get-text-property (point) 'niwa-view-mailbox-card)))
        (should result-card)
        (search-forward "● Background command \"Run tests\" completed")
        (should-not (eq result-card
                        (get-text-property (match-beginning 0)
                                           'niwa-view-mailbox-card))))
      (search-forward "Bash test output")
      (should (invisible-p (match-beginning 0)))))

  :doc "Bash completion keeps following reasoning on a separate line"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     (concat
      "<agent-message type=\"EXECUTION\" sender=\"/root/worker\" recipient=\"/root\" "
      "termination=\"exited\" outcome=\"success\">\n"
      "<bash-execution execution_id=\"exec-1\" outcome=\"success\" "
      "output_bytes=\"0\" output_lines=\"0\"/>\n"
      "</agent-message>\n"
      "#+begin_reasoning\nnext\n#+end_reasoning\n")
     nil)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "● Background command \"command\" completed" text))
        (should (string-match-p "  … Thinking" text))
        (should-not (string-match-p "completed  … Thinking" text)))))

  :doc "pure agent-result turn renders with the same mailbox card path"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "<agent-result sender=\"/root/worker\" recipient=\"/root\" outcome=\"completed\">\nresult\n</agent-result>\n"
     nil)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "^  ● Agent /root/worker finished" text))
        (should-not (string-match-p "│" text))
        (should (string-match-p "result" text))
        (should (string-match-p "Assistant\n" text))
        (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text)))
      (goto-char (point-min))
      (search-forward "  ● Agent")
      (let ((header-start (match-beginning 0)))
        (should-not (get-text-property header-start 'font-lock-face))
        (should (eq (get-text-property (+ header-start 2) 'font-lock-face)
                    'niwa-view-tool-metadata))
        (should (eq (get-text-property (+ header-start 4) 'font-lock-face)
                    'niwa-view-mailbox-header))
        (should (eq 'unspecified
                    (face-attribute 'niwa-view-mailbox-header :weight))))
      (goto-char (point-min))
      (search-forward "result")
      (let* ((pos (match-beginning 0))
             (prefix (get-text-property pos 'line-prefix)))
        (should (equal "    │ " prefix))
        (should (equal "      " (get-text-property pos 'wrap-prefix)))
        (should (eq (get-text-property 4 'font-lock-face prefix)
                    'niwa-view-mailbox-gutter))
        (should (eq (get-text-property pos 'font-lock-face)
                    'niwa-view-mailbox-body)))))

  :doc "incomplete agent-result opener stays ordinary response text"
  (niwa-view-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (niwa-view-test--insert-data
       data-buf
       "<agent-result sender=\"/root/worker\" recipient=\"/root\">\npartial\n"
       'response)
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft draft 4)
        (niwa-view--full-rerender)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "Assistant\n" text))
          (should (string-match-p "<agent-result sender=" text))
          (should (string-match-p "partial" text))
          (should-not (string-match-p "Agent /root/worker" text)))
        (should (equal draft (niwa-view-composer-draft-text nil t)))
        (should (= 4 (niwa-view-render-test--draft-offset))))))

  :doc "agent-result body may mention nested result blocks"
  (niwa-view-test--with-buffers
    (let ((niwa-view-mailbox-collapse-line-threshold 200))
      (niwa-view-test--insert-data
       data-buf
       (concat
        "<agent-result sender=\"/root/verifier\" recipient=\"/root\" outcome=\"completed\">\n"
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
        (niwa-view--full-rerender)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "● Agent /root/verifier finished" text))
          (should (string-match-p "Before nested example" text))
          (should (string-match-p "After nested example" text))
          (should (string-match-p "partial result" text))
          (should-not (string-match-p "<agent-result sender=\"/root/verifier\""
                                      text))
          (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text))))))

  :doc "mailbox blocks separated by prose render independently"
  (niwa-view-test--with-buffers
    (let ((niwa-view-mailbox-collapse-line-threshold 200))
      (niwa-view-test--insert-data
       data-buf
       (concat
        "<agent-result sender=\"/root/reviewer\" recipient=\"/root\" outcome=\"completed\">\n"
        "first result\n"
        "</agent-result>\n"
        "Assistant prose between mailbox cards.\n"
        "<agent-result sender=\"/root/verifier\" recipient=\"/root\" outcome=\"completed\">\n"
        "second result\n"
        "</agent-result>\n")
       nil)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let* ((text (niwa-view-render-test--view-text))
               (finished-count
                (cl-count-if (lambda (line)
                               (string-prefix-p "  ● Agent" line))
                             (split-string text "\n"))))
          (should (= 2 finished-count))
          (should (string-match-p "● Agent /root/reviewer finished" text))
          (should (string-match-p "● Agent /root/verifier finished" text))
          (should (string-match-p "Assistant prose between mailbox cards" text))
          (should-not (string-match-p "<agent-result" text))))))

  :doc "indented mailbox close line is removed structurally"
  (niwa-view-test--with-buffers
    (let ((niwa-view-mailbox-collapse-line-threshold 200))
      (niwa-view-test--insert-data
       data-buf
       "<agent-result sender=\"/root/worker\" recipient=\"/root\" outcome=\"completed\">\nresult\n  </agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "● Agent /root/worker finished" text))
          (should (string-match-p "result" text))
          (should-not (string-match-p "</agent-result>" text))))))

  :doc "expanded agent-result keeps gutter on blank body lines"
  (let ((niwa-view-mailbox-collapse-line-threshold 200))
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data
       data-buf
       "<agent-result sender=\"/root/worker\" recipient=\"/root\" outcome=\"completed\">\nfirst\n\nsecond\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p
                   "● Agent /root/worker finished\n\nfirst"
                   text))
        (should (string-match-p "first\n\nsecond" text)))
        (goto-char (point-min))
        (search-forward "first")
        (beginning-of-line)
        (dotimes (_ 3)
          (should (equal "    │ " (get-text-property (point) 'line-prefix)))
          (should (equal "      " (get-text-property (point) 'wrap-prefix)))
          (forward-line 1)))))

  :doc "long agent-result delivery expands to the final response body"
  (niwa-view-test--with-buffers
    (let* ((niwa-view-mailbox-collapse-line-threshold 1)
           (root (file-name-as-directory
                  (make-temp-file "niwa-mailbox-long-" t)))
           (workspace (niwa-workspace--create
                       :type 'project
                       :id "mailbox-long"
                       :root root
                       :name "mailbox-long"))
           (session (niwa-session-create "main" workspace)))
      (setf (niwa-session-save-path session)
            (file-name-as-directory
             (file-name-concat root "session")))
      (setf (niwa-session-agent-transcripts session)
            '(("storage-long" . (:agent-path "/root/worker"
                                  :path "agents/worker.chat.org"
                                  :status completed))))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (niwa-view-test--insert-data
       data-buf
       "<agent-result sender=\"/root/worker\" recipient=\"/root\" outcome=\"completed\">\nline one\nline two\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 3)
        (niwa-view--full-rerender)
        (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text nil t)))
        (should (= 3 (niwa-view-render-test--draft-offset)))
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "● Agent /root/worker finished" text))
          (should-not (string-match-p "lines? collapsed" text))
          (goto-char (point-min))
          (search-forward "line two")
          (should (eq (get-text-property (match-beginning 0) 'invisible)
                      'niwa-view-mailbox-collapsed))
          (should (invisible-p (match-beginning 0)))
          (niwa-view-composer-set-historical-visible nil)
          (should (invisible-p (match-beginning 0)))
          (niwa-view-composer-set-historical-visible t)
          (should (invisible-p (match-beginning 0))))
        (goto-char (point-min))
        (search-forward "● Agent /root/worker finished")
        (goto-char (match-beginning 0))
        (search-forward "/root/worker")
        (goto-char (match-beginning 0))
        (let (opened)
          (cl-letf (((symbol-function
                      'niwa-view-open-agent-transcript)
                     (lambda (id &rest _) (setq opened id))))
            (niwa-view-open-agent-transcript-at-point))
          (should (equal "/root/worker" opened)))
        (goto-char (point-min))
        (search-forward "● Agent /root/worker finished")
        (goto-char (match-beginning 0))
        (niwa-view-toggle-section)
        (goto-char (point-min))
        (search-forward "line two")
        (should-not (get-text-property (match-beginning 0) 'invisible))
        (niwa-view-toggle-section)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "● Agent /root/worker finished" text))
          (should-not (string-match-p "lines? collapsed" text))
          (goto-char (point-min))
          (search-forward "line two")
          (should (invisible-p (match-beginning 0)))))
      (delete-directory root t)))

  :doc "background notice collapses regardless of threshold and shows no hint"
  (niwa-view-test--with-buffers
    (let ((niwa-view-mailbox-collapse-line-threshold 0))
      (niwa-view-test--insert-data
       data-buf
       "<agent-result sender=\"/root/worker\" recipient=\"/root\" outcome=\"completed\">\nresult\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "● Agent /root/worker finished$" text))
          (should-not (string-match-p "lines? collapsed" text))))))

  :doc "mailbox decoration clears inherited agent-handle properties"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'project
                       :id "mailbox-stale"
                       :root temporary-file-directory
                       :name "mailbox-stale"))
           (session (niwa-session-create "main" workspace)))
      (setf (niwa-session-save-path session)
            (file-name-as-directory
             (file-name-concat temporary-file-directory
                               "niwa-mailbox-stale-session")))
      (setf (niwa-session-agent-transcripts session)
            '(("storage-stale" . (:agent-path "/root/explorer"
                                   :path "agents/explorer.chat.org"
                                   :status completed))))
      (with-current-buffer data-buf
        (setq-local niwa--session session)))
    (with-current-buffer data-buf
      (insert "(:name \"Agent\" :args (:task_name \"explore\" :message \"Inspect.\"))\n\nlaunch\n"))
    (with-current-buffer view-buf
      (let* ((stale-source (cons 1 (with-current-buffer data-buf (point-max))))
             (start nil))
        (let ((inhibit-read-only t))
          (goto-char (niwa-view-transcript-tail))
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
          (unwind-protect
              (progn
                (setq start (point))
                (insert "<agent-result sender=\"/root/explorer\" recipient=\"/root\" outcome=\"completed\">\nfinal body\n</agent-result>\n")
                (add-text-properties
                 start (point)
                 `(niwa-view-source ,stale-source
                   niwa-view-type agent-handle
                   niwa-view-agent-path "/root/explorer"
                   niwa-view-agent-handle-p t
                   niwa-view-agent-status completed))
                (niwa-view--decorate-agent-result-blocks start (point)))
            (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil)))
        (goto-char start)
        (search-forward "● Agent /root/explorer finished")
        (search-backward "/root/explorer")
        (should (eq (get-text-property (point) 'niwa-view-type)
                    'mailbox-delivery))
        (should-not (get-text-property (point) 'niwa-view-source))
        (should-not (get-text-property (point) 'niwa-view-agent-handle-p))
        (should (equal "/root/explorer"
                       (get-text-property (point) 'niwa-view-agent-path)))
        (search-forward "final body")
        (should-not (get-text-property (match-beginning 0)
                                       'niwa-view-agent-path)))))

  :doc "mailbox delivery between response chunks stays in one assistant turn"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Before mailbox.\n" 'response)
    (niwa-view-test--insert-data
     data-buf
     "\n<agent-message sender=\"/root/explorer\" recipient=\"/root\">\nhello\n</agent-message>\n\n"
     nil)
    (niwa-view-test--insert-data data-buf "After mailbox.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let* ((text (niwa-view-render-test--view-text))
             (assistant-count
              (cl-count-if (lambda (line) (string= line "Assistant"))
                           (split-string text "\n"))))
        (should (= 1 assistant-count))
        (should (string-match-p "Before mailbox" text))
        (should (string-match-p "✉ message from /root/explorer" text))
        (should (string-match-p "hello" text))
        (should (string-match-p "After mailbox" text)))))

  :doc "agent-result after response does not render as a You turn"
  (niwa-view-test--with-buffers
    (let ((niwa-view-mailbox-collapse-line-threshold 200))
      (niwa-view-test--insert-data
       data-buf
       "Reviewer returned clean. Waiting on verifier.\n"
       'response)
      (niwa-view-test--insert-data
       data-buf
       (concat
        "<agent-result sender=\"/root/verifier\" recipient=\"/root\" outcome=\"completed\">\n"
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
        (niwa-view--full-rerender)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "Reviewer returned clean" text))
          (should (string-match-p "● Agent /root/verifier finished" text))
          (should (string-match-p "VERDICT: FAIL" text))
          (should-not (string-match-p
                       "You\n● Agent /root/verifier finished"
                       text))))))

  :doc "mailbox toggle does not expand a preceding Agent source"
  (niwa-view-test--with-buffers
    (let ((niwa-view-mailbox-collapse-line-threshold 1))
      (niwa-view-test--insert-data
       data-buf
       "(:name \"Agent\" :args (:task_name \"explorer\" :message \"Skim niwa-queue.el\"))\n\n{\"path\":\"/root/explorer\"}\n"
       '(tool . "call_agent"))
      (niwa-view-test--insert-data
       data-buf
       "Assistant text before mailbox.\n"
       'response)
      (niwa-view-test--insert-data
       data-buf
       "\n<agent-message sender=\"/root/explorer\" recipient=\"/root\">\nHello from your Explorer Agent :)\n</agent-message>\n\n<agent-result sender=\"/root/explorer\" recipient=\"/root\" outcome=\"completed\">\nfinal line one\nfinal line two\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (goto-char (point-min))
        (search-forward "● Agent /root/explorer finished")
        (goto-char (match-beginning 0))
        (should (eq (get-text-property (point) 'niwa-view-type)
                    'mailbox-delivery))
        (should-not (get-text-property (point) 'niwa-view-source))
        (niwa-view-toggle-section)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "✉ message from /root/explorer" text))
          (should (string-match-p "Hello from your Explorer Agent :)" text))
          (should (string-match-p "● Agent /root/explorer finished" text))
          (should (string-match-p "final line two" text))
          (should-not (string-match-p "Skim niwa-queue.el (370 lines)"
                                      text)))))))


(defun niwa-view-test--format-media-data-block
    (media session tool-use-id)
  "Format MEDIA for SESSION and TOOL-USE-ID."
  (niwa-tool-media--format-media-data-block
   media
   (niwa-pipeline-tool-results-dir session nil)
   tool-use-id))

(niwa-deftest niwa-view--tool-call-parse-media-fallback ()
  ,test
  (test)
  :doc "text Read keeps copied persisted media side-channel visible"
  (let* ((tmpdir (make-temp-file "niwa-view-copied-media-" t))
         (ws (niwa-workspace--create :root tmpdir))
         (save-path (file-name-as-directory
                     (file-name-concat tmpdir ".niwa" "sessions" "main")))
         (session (niwa-session--create
                   :name "main" :workspace ws :save-path save-path))
         (media '((:path "/tmp/a.png"
                   :mime "image/png"
                   :kind image
                   :data "captured")))
         (copied (substring-no-properties
                  (niwa-view-test--format-media-data-block
                   media session "toolu_original"))))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session))
          (niwa-view-test--insert-data
           data-buf
           (concat "(:name \"Read\" :args (:file_path \"/tmp/copied.txt\"))\n\n"
                   "plain text" copied)
           '(tool . "toolu_other"))
          (with-current-buffer data-buf
            (let ((parsed (niwa-view--tool-call-parse
                           data-buf (point-min) (point-max))))
              (should (string-search niwa-tool-media--data-open
                                     (plist-get parsed :result))))))
      (delete-directory tmpdir t)))

  :doc "media Read keeps copied side-channel visible without current tool id"
  (let* ((tmpdir (make-temp-file "niwa-view-media-no-id-" t))
         (ws (niwa-workspace--create :root tmpdir))
         (save-path (file-name-as-directory
                     (file-name-concat tmpdir ".niwa" "sessions" "main")))
         (session (niwa-session--create
                   :name "main" :workspace ws :save-path save-path))
         (media '((:path "/tmp/a.png"
                   :mime "image/png"
                   :kind image
                   :data "captured")))
         (copied (substring-no-properties
                  (niwa-view-test--format-media-data-block
                   media session "toolu_original"))))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session))
          (niwa-view-test--insert-data
           data-buf
           (concat "(:name \"Read\" :args (:file_path \"/tmp/a.png\"))\n\n"
                   "plain text" copied)
           '(tool . nil))
          (with-current-buffer data-buf
            (let ((parsed (niwa-view--tool-call-parse
                           data-buf (point-min) (point-max))))
              (should (string-search niwa-tool-media--data-open
                                     (plist-get parsed :result))))))
      (delete-directory tmpdir t)))

  :doc "media Read keeps copied side-channel before generated side-channel"
  (let* ((tmpdir (make-temp-file "niwa-view-media-copied-prefix-" t))
         (ws (niwa-workspace--create :root tmpdir))
         (save-path (file-name-as-directory
                     (file-name-concat tmpdir ".niwa" "sessions" "main")))
         (session (niwa-session--create
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
                  (niwa-view-test--format-media-data-block
                   copied-media session "toolu_copied")))
         (actual (substring-no-properties
                  (niwa-view-test--format-media-data-block
                   actual-media session "toolu_actual")))
         (result (concat "plain text" copied "\nbody tail" actual)))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session))
          (niwa-view-test--insert-data
           data-buf
           (concat "(:name \"Read\" :args (:file_path \"/tmp/a.png\"))\n\n"
                   result)
           '(tool . "toolu_wrong"))
          (with-current-buffer data-buf
            (let ((parsed (niwa-view--tool-call-parse
                           data-buf (point-min) (point-max))))
              (should (string-search niwa-tool-media--data-open
                                     (plist-get parsed :result)))
              (should (string-search "body tail"
                                     (plist-get parsed :result))))))
      (delete-directory tmpdir t)))

  :doc "media Read can strip resumed duplicate block with wrong gptel id"
  (let* ((tmpdir (make-temp-file "niwa-view-media-duplicate-" t))
         (ws (niwa-workspace--create :root tmpdir))
         (save-path (file-name-as-directory
                     (file-name-concat tmpdir ".niwa" "sessions" "main")))
         (session (niwa-session--create
                   :name "main" :workspace ws :save-path save-path))
         (media '((:path "/tmp/a.png"
                   :mime "image/png"
                   :kind image
                   :data "captured")))
         (result (substring-no-properties
                  (concat "plain media"
                          (niwa-view-test--format-media-data-block
                           media session "toolu_actual")))))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session))
          (niwa-view-test--insert-data
           data-buf
           (concat "(:name \"Read\" :args (:file_path \"/tmp/a.png\"))\n\n"
                   result)
           '(tool . "toolu_wrong"))
          (with-current-buffer data-buf
            (let ((parsed (niwa-view--tool-call-parse
                           data-buf (point-min) (point-max))))
              (should (equal "plain media" (plist-get parsed :result))))))
      (delete-directory tmpdir t))))

(niwa-deftest niwa-view--source-position
  (:doc "keys markers and integers identically")
  ,test
  (test)
  (should (= 3 (niwa-view--source-position 3)))
  (with-temp-buffer
    (insert "abcdef")
    (should (= 2 (niwa-view--source-position (copy-marker 2))))))

(niwa-deftest niwa-view--source-revision ()
  ,test
  (test)
  :doc "observed appends and properties retain character identities but text edits invalidate"
  (with-temp-buffer
    (insert "first second tail")
    (let ((first (niwa-view--source-revision 1 6))
          (second (niwa-view--source-revision 7 13)))
      (goto-char (point-max)) (insert " more")
      (should (eq first (niwa-view--source-revision 1 6)))
      (should (eq second (niwa-view--source-revision 7 13)))
      (put-text-property 7 8 'niwa-render-data t)
      (should (eq first (niwa-view--source-revision 1 6)))
      (should (eq second (niwa-view--source-revision 7 13)))
      (goto-char 2) (delete-char 1) (insert "X")
      (should-not (eq first (niwa-view--source-revision 1 6)))))
  :doc "missed modifications cannot be hidden by a later observed append"
  (dolist (property-only '(nil t))
    (with-temp-buffer
      (insert "first second tail")
      (let ((first (niwa-view--tool-cache-key (current-buffer) 1 6 t)))
        (let ((inhibit-modification-hooks t))
          (if property-only
              (put-text-property 1 3 'gptel 'response)
            (goto-char 2) (delete-char 1) (insert "X")))
        (goto-char (point-max)) (insert " more")
        (should-not (equal first (niwa-view--tool-cache-key (current-buffer) 1 6 t))))))
  :doc "silent property changes and removed hooks retire old identities"
  (with-temp-buffer
    (insert "first second tail")
    (let ((first (niwa-view--tool-cache-key (current-buffer) 1 6 t)))
      (with-silent-modifications (put-text-property 1 3 'gptel 'response))
      (should-not (equal first (niwa-view--tool-cache-key (current-buffer) 1 6 t))))
    (let ((first (niwa-view--source-revision 1 6)))
      (remove-hook 'before-change-functions #'niwa-view--source-before-change t)
      (should-not (eq first (niwa-view--source-revision 1 6)))))
  :doc "narrowing and an unrelated cache limit preserve range identities"
  (with-temp-buffer
    (insert "first second tail")
    (let ((first (niwa-view--source-revision 1 6)))
      (save-restriction
        (narrow-to-region 1 6)
        (should (eq first (niwa-view--source-revision 1 6)))))
    (let ((niwa-view-render-cache-max-entries 2))
      (niwa-view--source-revision 7 10)
      (niwa-view--source-revision 11 12)
      (should (= (hash-table-count niwa-view--source-revisions) 3)))))

(niwa-deftest niwa-view--retire-tool-renderings ()
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf (insert "first second tail"))
    (with-current-buffer view-buf
      (let ((earlier (niwa-view--tool-cache-key data-buf 1 6 t))
            (header (niwa-view--tool-cache-key data-buf 7 13 t))
            (body (niwa-view--tool-cache-key data-buf 7 13 nil)))
        (puthash earlier 'earlier niwa-view--tool-rendering-cache)
        (puthash header 'header niwa-view--tool-rendering-cache)
        (puthash body 'body niwa-view--tool-rendering-cache)
        (niwa-view--retire-tool-renderings data-buf 7 13)
        (should (eq 'earlier (gethash earlier niwa-view--tool-rendering-cache)))
        (should-not (gethash header niwa-view--tool-rendering-cache))
        (should-not (gethash body niwa-view--tool-rendering-cache))
        (should (= 1 (hash-table-count niwa-view--tool-rendering-cache)))))))

(niwa-deftest niwa-view--sweep-tool-rendering-cache ()
  ,test
  (test)
  :doc "a complete projection retains only visited tool keys and source ranges"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf (insert "first second tail"))
    (with-current-buffer view-buf
      (let* ((old (niwa-view--tool-cache-key data-buf 1 6 t))
             (niwa-view--tool-cache-used (make-hash-table :test #'equal))
             (kept (niwa-view--tool-cache-key data-buf 7 13 t)))
        (with-current-buffer data-buf (niwa-view--source-revision 14 18))
        (puthash old 'old niwa-view--tool-rendering-cache)
        (puthash kept 'kept niwa-view--tool-rendering-cache)
        (niwa-view--sweep-tool-rendering-cache niwa-view--tool-cache-used)
        (should-not (gethash old niwa-view--tool-rendering-cache))
        (should (eq 'kept (gethash kept niwa-view--tool-rendering-cache)))
        (should (= 1 (hash-table-count niwa-view--tool-rendering-cache)))
        (with-current-buffer data-buf
          (should (= 1 (hash-table-count niwa-view--source-revisions)))
          (should (eq (nth 3 kept) (niwa-view--source-revision 7 13))))))))

(niwa-deftest niwa-view--source-before-change ()
  (with-temp-buffer
    (insert "first second tail")
    (let ((first (niwa-view--source-revision 1 6))
          (second (niwa-view--source-revision 7 13)))
      (niwa-view--source-before-change 6 6)
      (should (eq first (gethash '(1 . 6) niwa-view--source-revisions)))
      (should (eq second (gethash '(7 . 13) niwa-view--source-revisions)))
      (should second))))

(niwa-deftest niwa-view--source-after-change ()
  (with-temp-buffer
    (insert "first second tail")
    (niwa-view--source-revision 1 6)
    (let ((inhibit-modification-hooks t)) (goto-char 10) (insert "x"))
    (niwa-view--source-after-change 10 11 0)
    (should (= 0 (hash-table-count niwa-view--source-revisions)))))

(niwa-deftest niwa-view--tool-source-properties ()
  (with-temp-buffer
    (insert "abcdef")
    (put-text-property 2 4 'gptel 'response)
    (put-text-property 4 6 'niwa-render-data t)
    (put-text-property 3 5 'face 'bold)
    (should (equal '((0 1 nil nil nil) (1 3 response nil nil)
                     (3 5 nil t nil) (5 6 nil nil nil))
                   (niwa-view--tool-source-properties 1 7)))))

(niwa-deftest niwa-view--tool-cache-key ()
  ,test
  (test)

  :doc "provenance changes distinguish identical text"
  (with-temp-buffer
    (insert "source")
    (dolist (property '(gptel niwa-render-data niwa-hook-audit))
      (let ((plain (niwa-view--tool-cache-key (current-buffer) 1 7 t)))
        (put-text-property 1 7 property t)
        (should-not (equal plain (niwa-view--tool-cache-key
                                 (current-buffer) 1 7 t))))))

  :doc "mutating a provenance cons cannot mutate an already stored key"
  (with-temp-buffer
    (insert "source")
    (let ((provenance (cons 'tool "first")))
      (put-text-property 1 7 'gptel provenance)
      (let ((before (niwa-view--tool-cache-key (current-buffer) 1 7 t)))
        (setcdr provenance "second")
        (should-not (equal before (niwa-view--tool-cache-key
                                  (current-buffer) 1 7 t))))))

  :doc "marker positions and integer positions build equal keys"
  (with-temp-buffer
    (insert "#+begin_tool (Read)\n#+end_tool\n")
    (let ((buf (current-buffer)))
      (should (equal (niwa-view--tool-cache-key buf 2 9 nil)
                     (niwa-view--tool-cache-key
                      buf (copy-marker 2) (copy-marker 9) nil)))
      :doc "collapsed-only still discriminates"
      (should-not (equal (niwa-view--tool-cache-key buf 2 9 nil)
                         (niwa-view--tool-cache-key buf 2 9 t))))))

(niwa-deftest niwa-view--session-render-state-fingerprint ()
  ,test
  (test)

  :doc "agent activity changes leave the fingerprint stable"
  ;; Agent status reaches renderings only through render-data text
  ;; patches; folding registry activity into the fingerprint made every
  ;; cached tool rendering miss on every agent tick.
  (let ((session (niwa-session--create :name "main"))
        (record (niwa-agent-record--create
                 :id "agent-1" :path "/root/worker" :parent-path "/root"
                 :role "default" :activity 'running)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (let ((before (niwa-view--session-render-state-fingerprint session)))
      (setf (niwa-agent-record-activity record) 'waiting)
      (should (equal before
                     (niwa-view--session-render-state-fingerprint
                      session)))
      :doc "permission queue entries change the fingerprint"
      (setf (niwa-session-permission-queue session)
            (list '(:origin "/root/worker")))
      (should-not (equal before
                         (niwa-view--session-render-state-fingerprint
                          session))))))

(niwa-deftest niwa-view--tool-block-bounds ()
  ,test
  (test)

  :doc "memoizes bounds per segment and buffer tick"
  (let* ((real (symbol-function 'niwa-transcript--tool-block-bounds-for-run))
         (computations 0))
    (cl-letf (((symbol-function 'niwa-transcript--tool-block-bounds-for-run)
               (lambda (start end)
                 (cl-incf computations)
                 (funcall real start end))))
      (with-temp-buffer
        (insert "#+begin_tool (Read :file_path \"x\")\nbody\n#+end_tool\n")
        (put-text-property (point-min) (point-max) 'gptel '(tool . "c1"))
        (let ((bounds (niwa-view--tool-block-bounds
                       (point-min) (point-max))))
          (should (equal bounds (niwa-view--tool-block-bounds
                                 (point-min) (point-max))))
          (should (= 1 computations))
          :doc "a text change recomputes"
          (goto-char (point-max))
          (insert "tail\n")
          (niwa-view--tool-block-bounds (point-min) (point-max))
          (should (= 2 computations))
          :doc "a property-only change bumps the tick and recomputes"
          (put-text-property (point-min) (1+ (point-min))
                             'niwa-test 'stamp)
          (niwa-view--tool-block-bounds (point-min) (point-max))
          (should (= 3 computations))))
      :doc "nil bounds memoize"
      (with-temp-buffer
        (insert "no tool block here\n")
        (let ((before computations))
          (should-not (niwa-view--tool-block-bounds
                       (point-min) (point-max)))
          (should-not (niwa-view--tool-block-bounds
                       (point-min) (point-max)))
          (should (= (1+ before) computations)))))))

(niwa-deftest niwa-view--render-cache-key
  (:doc "distinguishes same-length edits deep inside cached text")
  (let* ((text (make-string 1024 ?a))
         (changed (copy-sequence text)))
    (aset changed 512 ?b)
    (should-not (equal (niwa-view--render-cache-key text)
                       (niwa-view--render-cache-key changed)))
    (should (equal (niwa-view--render-cache-key text)
                   (niwa-view--render-cache-key
                    (propertize text 'face 'bold))))))

(niwa-deftest niwa-view--render-live-region ()
  ,test
  (test)
  :doc "decodes repeated trusted audits once per live projection"
  (niwa-view-test--with-buffers
    (let ((decode (symbol-function 'niwa-transcript-audit--decode))
          (decodes 0)
          (audit (niwa--format-hook-audit-record
                  '(:type prompt-rewrite :event "test"
                    :original "before" :submitted "after"))))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 3)
        (niwa-view-stream-begin-turn
         (niwa-view--history-insertion-marker)
         (with-current-buffer data-buf (copy-marker (point-max)))))
      (with-current-buffer data-buf
        (insert (propertize "First answer.\n" 'gptel 'response))
        (insert audit)
        (insert (propertize "More answer.\n" 'gptel 'response))
        (insert audit))
      (cl-letf (((symbol-function 'niwa-transcript-audit--decode)
                 (lambda (text)
                   (cl-incf decodes)
                   (funcall decode text))))
        (with-current-buffer view-buf
          (niwa-view--render-live-region data-buf nil)
          (should (= decodes 1))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
          (should (= 3 (niwa-view-render-test--draft-offset)))))))
  :doc "a retained tail update keeps history readers without counting history"
  (niwa-view-test--with-buffers
    (let ((run-end (symbol-function 'niwa-view--render-anchor-run-end))
          (window (selected-window))
          (scans 0))
      (with-current-buffer data-buf
        (dotimes (i 30)
          (insert (format "Prompt %d\n" i))
          (insert (propertize (format "Response %d\n" i) 'gptel 'response))))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (set-window-buffer window view-buf)
        (niwa-view-stream-begin-turn
         (niwa-view--history-insertion-marker)
         (with-current-buffer data-buf (copy-marker (point-max)))))
      (niwa-view-test--insert-data data-buf "Live answer." 'response)
      (with-current-buffer view-buf
        (niwa-view--render-live-region data-buf nil)
        (should (niwa-view--live-tail-valid-p data-buf))
        (goto-char (point-min))
        (search-forward "Response 3")
        (set-window-start window (save-excursion (search-backward "Prompt 2"))
                          t))
      (niwa-view-test--insert-data data-buf " More." 'response)
      (cl-letf (((symbol-function 'niwa-view--render-anchor-run-end)
                 (lambda (&rest args)
                   (cl-incf scans)
                   (apply run-end args))))
        (with-current-buffer view-buf
          (niwa-view--render-live-region data-buf nil)
          (should (string-search "Live answer. More." (niwa-view-render-test--view-text)))
          (should (looking-back "Response 3" (line-beginning-position)))))
      (with-current-buffer view-buf
        (save-excursion
          (goto-char (window-start window))
          (should (looking-at-p "Prompt 2"))))
      ;; Thirty settled turns hold at least sixty source runs.
      (should (< scans 30)))))

(niwa-deftest niwa-view--user-input-fold-p ()
  ,test
  (test)
  :doc "folds only past the line threshold"
  (let ((niwa-view-user-input-collapse-line-threshold 3))
    (should-not (niwa-view--user-input-fold-p "a\nb\nc"))
    (should (niwa-view--user-input-fold-p "a\nb\nc\nd")))
  :doc "zero disables folding"
  (let ((niwa-view-user-input-collapse-line-threshold 0))
    (should-not (niwa-view--user-input-fold-p
                 (mapconcat #'number-to-string (number-sequence 1 40) "\n"))))
  :doc "org block markers keep the text unfolded"
  (let ((niwa-view-user-input-collapse-line-threshold 1))
    (should-not (niwa-view--user-input-fold-p
                 "intro\n#+begin_src elisp\n(x)\n#+end_src\nmore\n"))))

(niwa-deftest niwa-view--user-input-fold-summary ()
  ,test
  (test)
  :doc "shows the first line and the hidden line count"
  (should (equal "first line (+2 lines)"
                 (substring-no-properties
                  (niwa-view--user-input-fold-summary
                   "first line\nsecond\nthird"))))
  :doc "uses the singular for one hidden line"
  (should (equal "only (+1 line)"
                 (substring-no-properties
                  (niwa-view--user-input-fold-summary "only\nrest"))))
  :doc "truncates a long first line"
  (let ((summary (substring-no-properties
                  (niwa-view--user-input-fold-summary
                   (concat (make-string 120 ?x) "\ny")))))
    (should (string-match-p "\\.\\.\\. (\\+1 line)\\'" summary))
    (should (< (length summary) 100))))

(niwa-deftest niwa-view-user-markdown
  (:before-each (niwa-view--release-markdown-fontify-buffer)
   :after-each (niwa-view--release-markdown-fontify-buffer)
   :doc "user echo, history, and expanded folds reuse Markdown fontification")
  (cl-letf (((symbol-function 'niwa-view--markdown-fontify-mode)
             (lambda () 'emacs-lisp-mode)))
    (dolist (path '(echo history fold shared))
      (niwa-view-test--with-buffers
        (let ((text "\"highlight me\"\nsecond line")
              (niwa-view-user-input-collapse-line-threshold
               (if (eq path 'fold) 1 0)))
          (with-current-buffer view-buf
            (pcase path
              ((or 'echo 'fold)
               (niwa-view--insert-user-message text))
              ('shared
               (let ((inhibit-read-only t))
                 (goto-char (niwa-view--history-insertion-marker))
                 (niwa-view--insert-shared-context
                  (list :text text :context "context" :label "Shared context"))))
              ('history
               (niwa-view-test--insert-data
                data-buf (concat text "\n") nil)
               (let ((inhibit-read-only t))
                 (goto-char (niwa-view--history-insertion-marker))
                 (niwa-view--render-user-turn
                  (with-current-buffer data-buf
                    (list (list 'user (point-min) (point-max)))) data-buf))))
            (goto-char (point-min))
            (search-forward "highlight me")
            (when (eq path 'fold)
              (niwa-view-render-toggle-user-input)
              (goto-char (point-min))
              (search-forward "highlight me"))
            (should (eq (get-text-property (match-beginning 0) 'font-lock-face)
                        'font-lock-string-face))))))))

(niwa-deftest niwa-view--insert-user-message ()
  ,test
  (test)
  :doc "mailbox blocks mixed with visible prose never become prompt previews"
  (niwa-view-test--with-buffers
    (let ((mailbox (concat
                    "<agent-result sender=\"/root/worker\" recipient=\"/root\">\n"
                    "Private agent result\n</agent-result>\n")))
      (with-current-buffer view-buf
        (niwa-view--insert-user-message
         (concat mailbox "Actual prompt\n"))
        (goto-char (point-min))
        (search-forward "You\n")
        (should (equal "Actual prompt"
                       (get-text-property
                        (match-beginning 0) 'niwa-view-prompt-preview))))
      (niwa-view-test--insert-data
       data-buf (concat "Actual prompt\n" mailbox) nil)
      (niwa-view-test--insert-data data-buf "Answer.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (goto-char (point-min))
        (search-forward "You\n")
        (should (equal "Actual prompt"
                       (get-text-property
                        (match-beginning 0) 'niwa-view-prompt-preview))))))

  :doc "send echo and full render keep the same visible prompt preview"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view--insert-user-message "Visible % question\nwith detail")
      (goto-char (point-min))
      (search-forward "You\n")
      (should (equal (get-text-property (match-beginning 0)
                                        'niwa-view-prompt-preview)
                     "Visible % question with detail")))
    (niwa-view-test--insert-data
     data-buf
     "Visible % question\nwith detail\n:PROMPT:\nModel-only context\n:END:\n"
     nil)
    (niwa-view-test--insert-data data-buf "Answer.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "You\n")
      (should (equal (get-text-property (match-beginning 0)
                                        'niwa-view-prompt-preview)
                     "Visible % question with detail")))))

(niwa-deftest niwa-view-render-toggle-user-input ()
  ,test
  (test)
  :doc "rejects another section type without changing its content"
  (with-temp-buffer
    (insert (propertize "Response text" 'niwa-view-type 'response))
    (goto-char (point-min))
    (let ((before (buffer-string)))
      (should-error (niwa-view-render-toggle-user-input) :type 'user-error)
      (should (equal-including-properties before (buffer-string)))
      (should (= (point) (point-min)))))
  :doc "a long rendered user turn folds, expands, and refolds"
  (let ((niwa-view-user-input-collapse-line-threshold 3))
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data
       data-buf "line one\nline two\nline three\nline four\nline five\n"
       nil)
      (niwa-view-test--insert-data data-buf "Done.\n" 'response)
      (with-current-buffer data-buf
        (niwa-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "You" text))
          (should (string-match-p "line one (\\+4 lines)" text))
          (should-not (string-match-p "line five" text)))
        (goto-char (point-min))
        (search-forward "line one")
        (goto-char (match-beginning 0))
        (should (eq 'user-input-summary
                    (get-text-property (point) 'niwa-view-type)))
        (niwa-view-toggle-section)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "line five" text)))
        (goto-char (point-min))
        (search-forward "line five")
        (goto-char (match-beginning 0))
        (niwa-view-toggle-section)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "line one (\\+4 lines)" text))
          (should-not (string-match-p "line five" text))))))
  :doc "the send-path echo folds without source coordinates"
  (let ((niwa-view-user-input-collapse-line-threshold 2))
    (niwa-view-test--with-buffers
      (with-current-buffer view-buf
        (niwa-view--insert-user-message "one\ntwo\nthree\nfour")
        (goto-char (point-min))
        (search-forward "one")
        (goto-char (match-beginning 0))
        (should (eq 'user-input-summary
                    (get-text-property (point) 'niwa-view-type)))
        (should-not (get-text-property (point) 'niwa-view-source))
        (niwa-view-toggle-section)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "four" text))))))
  :doc "a short prompt renders unfolded"
  (let ((niwa-view-user-input-collapse-line-threshold 15))
    (niwa-view-test--with-buffers
      (with-current-buffer view-buf
        (niwa-view--insert-user-message "one\ntwo")
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "one\ntwo" text)))))))

(niwa-deftest niwa-view--mailbox-activity-entry ()
  ,test
  (test)
  :doc "activity classification decodes a delivery once and observes source edits"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf "<agent-message type=\"MAIL\" sender=\"/root/a\">\nfirst\n</agent-message>\n" nil)
    (with-current-buffer view-buf
      (let ((niwa-view--activity-entry-cache (make-hash-table :test #'equal))
            (seg (with-current-buffer data-buf (list 'mailbox (point-min) (point-max))))
            (reads 0)
            (read (symbol-function 'niwa-view--user-turn-text)))
        (cl-letf (((symbol-function 'niwa-view--user-turn-text)
                   (lambda (&rest args) (cl-incf reads) (apply read args))))
          (let ((first (niwa-view--mailbox-activity-entry seg data-buf)))
            (plist-put first :count 10)
            (let ((second (niwa-view--mailbox-activity-entry seg data-buf)))
              (should-not (plist-get second :count))
              (should (string-match-p
                       "first" (plist-get (plist-get (plist-get second :group-child)
                                                       :rendering) :mailbox-text)))))
          (should (= reads 1))
          (with-current-buffer data-buf
            (goto-char (point-min)) (search-forward "first") (replace-match "other" t t))
          (let ((changed (niwa-view--mailbox-activity-entry seg data-buf)))
            (should (string-match-p
                     "other" (plist-get (plist-get (plist-get changed :group-child)
                                                     :rendering) :mailbox-text))))
          (should (= reads 2))))))
  :doc "forwarded execution deliveries project one collapsed notice at any threshold"
  (dolist (threshold '(0 1))
    (niwa-view-test--with-buffers
      (let ((niwa-view-mailbox-collapse-line-threshold threshold))
        (niwa-view-test--insert-data
         data-buf
         (concat "<agent-message type=\"EXECUTION\" sender=\"/root/worker\" "
                 "execution_id=\"exec-1\" description=\"exec one\" "
                 "outcome=\"success\" exit_code=\"0\">\n"
                 "raw output\nsecond line\n"
                 "<bash-execution execution_id=\"exec-1\" outcome=\"success\" "
                 "output_bytes=\"25\" output_lines=\"2\"/>\n</agent-message>\n")
         nil)
        (with-current-buffer view-buf
          (let* ((inhibit-read-only t)
                 (seg (with-current-buffer data-buf
                        (list 'mailbox (point-min) (point-max))))
                 (entry (niwa-view--mailbox-activity-entry seg data-buf))
                 (child (niwa-view--tool-group-child entry data-buf 0)))
            (should (plist-get entry :notice-p))
            (goto-char niwa-view--status-marker)
            (let ((start (point)))
              (niwa-view--insert-child-call-block
               child (plist-get child :source) 'derive "")
              (goto-char start)
              (search-forward "● Background command \"exec one\" completed (exit code 0)")
              (should-not (string-match-p "lines? collapsed" (niwa-view-render-test--view-text)))
              (search-forward "raw output")
              (should (invisible-p (match-beginning 0)))))))))

  :doc "own terminal deliveries have no activity entry in root or agent views"
  (dolist (owner '(nil "/root/worker"))
    (dolist (type '("EXECUTION" "MONITOR_TERMINAL"))
      (niwa-view-test--with-buffers
        (niwa-view-test--insert-data
         data-buf
         (format "<agent-message type=\"%s\" sender=\"%s\">\nterminal output\n</agent-message>\n"
                 type (or owner "/root"))
         nil)
        (with-current-buffer view-buf
          (setq-local niwa-view--agent-path owner)
          (let ((seg (with-current-buffer data-buf
                       (list 'mailbox (point-min) (point-max)))))
            (should-not (niwa-view--mailbox-activity-entry seg data-buf)))))))

  :doc "ordinary mail and notices are told apart for activity grouping"
  (niwa-view-test--with-buffers
    (dolist (case '(("<agent-message type=\"MAIL\" sender=\"/root/a\">\nhi\n</agent-message>\n" . nil)
                    ("<agent-message type=\"MONITOR\" sender=\"/root\" description=\"tick\">\ne1\n</agent-message>\n" . t)
                    ("<agent-result sender=\"/root/a\" outcome=\"completed\">\nok\n</agent-result>\n" . t)
                    ("<agent-result sender=\"/root\" type=\"EXECUTION\" outcome=\"completed\">\nok\n</agent-result>\n" . t)))
      (with-current-buffer data-buf (erase-buffer))
      (niwa-view-test--insert-data data-buf (car case) nil)
      (let ((seg (with-current-buffer data-buf
                   (list 'mailbox (point-min) (point-max)))))
        (should (eq (cdr case)
                    (and (plist-get (niwa-view--mailbox-activity-entry
                                     seg data-buf)
                                    :notice-p)
                         t)))))))

(niwa-deftest niwa-view--own-completion-activity ()
  ,test
  (test)
  :doc "own completion mail does not split tool groups in live or rebuilt views"
  (dolist (type '("EXECUTION" "MONITOR_TERMINAL"))
    (niwa-view-test--with-buffers
      (dotimes (i 4)
        (niwa-view-test--insert-data
         data-buf
         (format "(:name \"Read\" :args (:file_path \"/tmp/f%d.el\"))\n\ncontent %d\n"
                 i i)
         `(tool . ,(format "call_%d" i)))
        (when (= i 1)
          (niwa-view-test--insert-data
           data-buf
           (format "<agent-message type=\"%s\" sender=\"/root\">\nterminal output\n</agent-message>\n"
                   type)
           nil)))
      (niwa-view-test--insert-data data-buf "Done.\n" 'response)
      (with-current-buffer data-buf
        (niwa-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (dolist (rebuild '(nil t))
          (when rebuild (niwa-view--full-rerender))
          (let ((text (niwa-view-render-test--view-text)))
            (should (string-match-p "read 4 files" text))
            (should-not (string-match-p "terminal output\\|Background command\\|Monitor\\|received" text)))))
      (with-current-buffer data-buf
        (should (string-match-p "terminal output" (buffer-string)))))))

(niwa-deftest niwa-view--split-completion-entries ()
  ,test
  (test)
  :doc "breadcrumbs fold into an activity group and stay out of view when folded"
  (niwa-view-test--with-buffers
    (let ((crumb (lambda (id outcome code)
                   (niwa--format-hook-audit-record
                    (list :type 'execution-breadcrumb :execution-id id
                          :tool-use-id "call_0" :owner "/root"
                          :command (concat "make " id)
                          :facts (list :outcome outcome :termination 'exited
                                       :exit-code code))))))
      (dotimes (i 4)
        (niwa-view-test--insert-data
         data-buf
         (format
          "(:name \"Read\" :args (:file_path \"/tmp/f%d.el\"))\n\ncontent %d\n"
          i i)
         `(tool . ,(format "call_%d" i)))
        (when (memq i '(1 2))
          (with-current-buffer data-buf
            (goto-char (point-max))
            (insert (if (= i 1)
                        (funcall crumb "a" 'success 0)
                      (funcall crumb "b" 'failure 1))))))
      (niwa-view-test--insert-data data-buf "Done.\n" 'response)
      (with-current-buffer data-buf
        (niwa-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p
                   "read 4 files, 1 command completed, 1 failed" text))
          (should-not (string-match-p "Background command" text)))
        ;; The folded group still counts as projecting its completions.
        (should (niwa-view-audit-breadcrumb-present-p
                 '(:execution-id "b" :owner "/root")
                 (niwa-view-transcript-tail)))
        (goto-char (point-min))
        (search-forward "read 4 files")
        (niwa-view-toggle-section)
        (let ((text (niwa-view-render-test--view-text)))
          (should (string-match-p "● Background command \"make a\" completed (exit code 0)  \\[Show result\\]" text))
          (should (string-match-p "● Background command \"make b\" failed (exit code 1)  \\[Show result\\]"
                                  text)))
        (goto-char (point-min))
        (search-forward "read 4 files")
        (niwa-view-toggle-section)
        (should-not (string-match-p
                     "Background command" (niwa-view-render-test--view-text))))))

  :doc "collapsing a group removes a last call whose breadcrumb ends before it"
  ;; The breadcrumb's own source ends at its audit block, inside the call's
  ;; segment.  The group's source must still cover the whole call, or the
  ;; collapse leaves the call and its breadcrumb under the folded header.
  (niwa-view-test--with-buffers
    (dotimes (i 4)
      (niwa-view-test--insert-data
       data-buf
       (format "(:name \"Read\" :args (:file_path \"/tmp/f%d.el\"))\n\ncontent %d\n"
               i i)
       `(tool . ,(format "call_%d" i))))
    (with-current-buffer data-buf
      (goto-char (point-max))
      (insert (niwa--format-hook-audit-record
               (list :type 'execution-breadcrumb :execution-id "last"
                     :tool-use-id "call_3" :owner "/root" :command "make last"
                     :facts (list :outcome 'success :termination 'exited
                                  :exit-code 0)))
              "\n"))
    (niwa-view-test--insert-data data-buf "Done.\n" 'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((folded (niwa-view-render-test--view-text)))
        (goto-char (point-min))
        (search-forward "read 4 files")
        (niwa-view-toggle-section)
        (should (string-match-p "f3.el" (niwa-view-render-test--view-text)))
        (goto-char (point-min))
        (search-forward "read 4 files")
        (niwa-view-toggle-section)
        (let ((text (niwa-view-render-test--view-text)))
          (should-not (string-match-p "f3.el" text))
          (should-not (string-match-p "Background command" text))
          (should (equal folded text)))))))

(niwa-deftest niwa-view--rendering-header-line/running ()
  ,test
  (test)
  :doc "a still-running Bash command is marked running, not finished"
  (let ((running '(:state running :status success :execution-id "e1")))
    (should (string-match-p
             "● Bash: sleep 9 · running"
             (niwa-view--rendering-header-line
              (niwa-tool-exec--render-bash
               "Bash" '(:command "sleep 9") "" running))))
    (should-not (eq 'running
                    (plist-get (niwa-tool-exec--render-bash
                                "StopExecution" '(:execution_id "e1") ""
                                running)
                               :status)))))

(niwa-deftest niwa-view--tool-group-entry-p ()
  ,test
  (test)
  :doc "plain collapsed tool rows are groupable"
  (should (niwa-view--tool-group-entry-p
           '(:count 1 :rendering (:header "Read: f (1 line)"))))
  :doc "rows demanding individual presentation are not"
  (dolist (rendering
           '(nil
             (:header "h" :vtype agent-handle)
             (:header "h" :child-calls ((:id "1")))
             (:header "h" :hook-audits ((:type x)))
             (:header "h" :force-expanded-p t)
             (:header "h" :expandable-p nil)
             (:header "h" :initially-collapsed-p nil)))
    (should-not (niwa-view--tool-group-entry-p
                 (list :count 1 :rendering rendering))))
  :doc "failed calls and results still being prepared keep their run's grouping"
  (dolist (rendering
           '((:header "h" :status error)
             (:header "h" :status failed)
             (:header "h" :status blocked)
             (:header "Read: preparing result…" :vtype tool-preparing
              :status running :group-tool "Read" :initially-collapsed-p t)))
    (should (niwa-view--tool-group-entry-p
             (list :count 1 :rendering rendering))))
  :doc "coalesced rows are not groupable"
  (should-not (niwa-view--tool-group-entry-p
               '(:count 2 :rendering (:header "h"))))
  :doc "sandbox disclosures alone do not split a successful run"
  (dolist (summary
           '((:attempt-count 1 :started-count 1 :refused-count 0
              :sandbox unavailable :filesystem unrestricted
              :network unrestricted :proc nil
              :additional-read-count 0 :additional-write-count 0)
             (:attempt-count 1 :started-count 1 :refused-count 0
              :sandbox bubblewrap :filesystem workspace-write
              :network unrestricted :proc fresh
              :additional-read-count 0 :additional-write-count 1)
             (:attempt-count 1 :started-count 0 :refused-count 1
              :sandbox refused :filesystem unavailable
              :network unavailable :proc nil
              :additional-read-count 0 :additional-write-count 0)))
    (should (niwa-view--tool-group-entry-p
             (list :count 1 :rendering
                   (list :header "Bash: ls" :status 'success
                         :sandbox-summary summary))))))

(niwa-deftest niwa-view--fit-group-header ()
  ,test
  (test)
  :doc "keeps a folded group row to one line with room for its suffixes"
  (with-temp-buffer
    (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 50)))
      (let* ((header (concat "15 calls: read 5 files, searched 3 patterns, "
                             "ran 2 commands, fetched 1 page, queried 4 symbols"))
             (running (list :header header :vtype 'tool-group :pending-count 2))
             (settled (list :header header :vtype 'tool-group
                            :unconfined t)))
        (dolist (rendering (list running settled))
          (let ((fitted (niwa-view--fit-group-header header rendering))
                (line (substring-no-properties
                       (niwa-view--rendering-header-line rendering))))
            (should (string-suffix-p "…" fitted))
            (should (< (string-width line) 50))
            (should (string-suffix-p " ▸" line))))
        (should (string-match-p "· 2 running ▸\\'"
                                (niwa-view--rendering-header-line running)))
        (should (string-match-p "· unconfined ▸\\'"
                                (niwa-view--rendering-header-line settled))))))
  :doc "a narrow running row keeps failures that the summary cut off"
  (with-temp-buffer
    (let ((width 50))
     (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) width)))
      (let* ((tool (lambda (name &optional status)
                     (list :role 'member :kind 'tool :name name
                           :status (or status 'completed))))
             (header
              (lambda (&rest entries)
                (let* ((group (car (niwa-activity-group-partition entries nil)))
                       (line (niwa-view--rendering-header-line
                              (list :header (plist-get group :summary)
                                    :vtype 'tool-group :pending-count 1))))
                  (should (< (string-width line) width))
                  (should (string-suffix-p "· 1 running ▸"
                                           (substring-no-properties line)))
                  line)))
             (failed (lambda (line)
                       (cl-loop with start = 0
                                while (string-match "\\([0-9]+\\) failed" line start)
                                do (should (eq 'error
                                               (get-text-property
                                                (match-beginning 0)
                                                'font-lock-face line)))
                                sum (string-to-number (match-string 1 line))
                                do (setq start (match-end 0))))))
        ;; A failed Bash after the cut is still counted on the row.
        (let ((line (funcall header (funcall tool "Read") (funcall tool "Read")
                             (funcall tool "Grep") (funcall tool "Eval")
                             (funcall tool "Bash" 'failed)
                             (funcall tool "Read" 'running))))
          (should-not (string-match-p "ran 1 command" line))
          (should (string-match-p "… (1 failed) · 1 running ▸\\'"
                                  (substring-no-properties line)))
          (should (= 1 (funcall failed line))))
        ;; A visible failure keeps its own count; only hidden ones join the tail.
        (setq width 80)
        (let ((line (funcall header (funcall tool "Grep" 'failed) (funcall tool "Read")
                             (funcall tool "Eval") (funcall tool "WebFetch")
                             (funcall tool "Bash" 'failed)
                             (funcall tool "Bash" 'failed))))
          (should (string-match-p "searched 1 pattern (1 failed)" line))
          (should (string-match-p "… (2 failed) · 1 running ▸\\'"
                                  (substring-no-properties line)))
          (should (= 3 (funcall failed line))))
        ;; A cut never leaves a count half shown.
        (setq width 50)
        (dotimes (n 12)
          (let ((line (funcall header (funcall tool "Read")
                               (funcall tool (make-string (1+ n) ?X) 'failed)
                               (funcall tool "Grep") (funcall tool "Eval")
                               (funcall tool "WebFetch"))))
            (should (= 1 (funcall failed line)))
            (should-not (string-match-p "([0-9]*\\( fa?i?l?e?\\)?…" line))))))))
  :doc "an expanded group points its disclosure marker down"
  (with-temp-buffer
    (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 80)))
      (should (string-suffix-p
               "4 calls: read 4 files ▾"
               (substring-no-properties
                (niwa-view--rendering-header-line
                 '(:header "4 calls: read 4 files" :vtype tool-group
                   :disclosure-open t)))))))
  :doc "leaves a header that fits unchanged"
  (with-temp-buffer
    (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 80)))
      (should (equal "read 4 files"
                     (niwa-view--fit-group-header
                      "read 4 files" '(:vtype tool-group :pending-count 1)))))))

(niwa-deftest niwa-view--activity-group-entry ()
  ,test
  (test)
  :doc "plain tool rows are members named for the summary, with their status"
  (let ((entry (niwa-view--activity-group-entry
                '(:kind tool :count 1
                  :rendering (:status error :group-tool "Grep")))))
    (should (eq 'member (plist-get entry :role)))
    (should (equal "Grep" (plist-get entry :name)))
    (should (eq 'failed (plist-get entry :status))))
  :doc "rows carrying audits, coalescing rows and notices are boundaries"
  (dolist (entry '((:kind tool :count 1
                    :rendering (:hook-audits ((:type guest-prompt))))
                   (:kind tool :count 1 :rendering (:coalesce-key "poll"))
                   (:kind mailbox :notice-p t)
                   (:kind hook-audit)))
    (should (eq 'boundary
                (plist-get (niwa-view--activity-group-entry entry) :role))))
  :doc "messages and own completions are members keyed by their execution"
  (should (eq 'message (plist-get (niwa-view--activity-group-entry
                                   '(:kind mailbox))
                                  :kind)))
  (should (equal '(("e1" "/root") . "failed")
                 (plist-get
                  (niwa-view--activity-group-entry
                   '(:kind completion
                     :group-child
                     (:rendering (:completion-record
                                  (:execution-id "e1" :owner "/root"
                                   :facts (:outcome failure))))))
                  :completion))))

(niwa-deftest niwa-view--pending-activity-entry ()
  ,test
  (test)
  :doc "in-flight calls run; finished ones take their outcome"
  (should (equal '(running completed failed)
                 (mapcar (lambda (entry)
                           (plist-get (niwa-view--pending-activity-entry entry)
                                      :status))
                         '((k1 "Read" "a" nil)
                           (k2 "Read" "b" (:settled t))
                           (k3 "Grep" "x" (:settled error))))))
  (should (equal "Tool" (plist-get (niwa-view--pending-activity-entry
                                    '(k4 nil nil nil))
                                   :name))))

(niwa-deftest niwa-view--insert-tool-group ()
  ,test
  (test)
  :doc "unconfined rows fold like any row; the group names it once and keeps each disclosure"
  (niwa-view-test--with-buffers
    (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry))
          (niwa-view-tool-group-collapse-threshold 2)
          (summary '(:attempt-count 1 :started-count 1 :refused-count 0
                     :sandbox off :filesystem unrestricted :network unrestricted
                     :proc nil :additional-read-count 0 :additional-write-count 0))
          (sandbox "◇ Sandbox:  sandbox disabled · ran without confinement"))
      (niwa-tool-register
       (niwa-tool--create :name "Bash" :category "niwa"
                             :renderer #'niwa-tool-exec--render-bash))
      (cl-flet ((tool (id name args)
                  (with-current-buffer data-buf
                    (goto-char (point-max))
                    (insert "#+begin_tool\n"
                            (propertize (format "(:name %S :args %S)\n\nout\n" name args)
                                        'gptel (cons 'tool id)))
                    (when (equal name "Bash")
                      (let ((start (point)))
                        (insert (substring-no-properties
                                 (niwa-tool-render-data-format
                                  (list :kind 'bash :command (plist-get args :command)
                                        :state 'completed :termination 'exited
                                        :exit-code 0 :outcome 'success
                                        :wall-time-seconds 1.0 :status 'success
                                        :sandbox-summary summary)
                                  id)))
                        (put-text-property start (point) 'niwa-render-data t)))
                    (insert "#+end_tool\n"))))
        (tool "r1" "Read" '(:file_path "/tmp/a.el"))
        (tool "b1" "Bash" '(:command "echo one"))
        (tool "b2" "Bash" '(:command "echo two"))
        (niwa-view-test--insert-data data-buf "Then two more.\n\n" 'response)
        (tool "b3" "Bash" '(:command "echo three"))
        (tool "b4" "Bash" '(:command "echo four"))
        (niwa-view-test--insert-data data-buf "Done.\n" 'response))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 3))
      (with-current-buffer data-buf
        (niwa-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (dotimes (_ 2)
          (let ((lines (split-string (niwa-view-render-test--view-text)
                                     "\n")))
            (should (= 1 (cl-count
                          "  ✓ 3 calls: read 1 file, ran 2 commands · unconfined ▸"
                          lines :test #'equal)))
            ;; The run below the threshold keeps its rows and their notes.
            (should (equal (list "  ✓ Bash: echo three · finished · 1s" (concat "  " sandbox)
                                 "  ✓ Bash: echo four · finished · 1s" (concat "  " sandbox))
                           (seq-filter (lambda (line)
                                         (string-match-p "Bash:\\|Sandbox:" line))
                                       lines))))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
          (niwa-view--full-rerender))
        (goto-char (point-min))
        (search-forward "3 calls:")
        (niwa-view-toggle-section)
        (let ((lines (split-string (niwa-view-render-test--view-text)
                                   "\n")))
          (should (member "  ✓ 3 calls: read 1 file, ran 2 commands · unconfined ▾" lines))
          (should (= 4 (cl-count-if (lambda (line) (string-search sandbox line))
                                    lines))))
        (goto-char (point-min))
        (search-forward "echo one")
        (should (eq 'tool-child (get-text-property (point) 'niwa-view-type)))
        (forward-line 1)
        (should (string-search sandbox (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position))))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t))))))
  :doc "a run past the threshold folds into one expandable activity row"
  (niwa-view-test--with-buffers
    (dotimes (i 4)
      (niwa-view-test--insert-data
       data-buf
       (format
        "(:name \"Read\" :args (:file_path \"/tmp/f%d.el\"))\n\ncontent %d\n"
        i i)
       `(tool . ,(format "call_%d" i))))
    (niwa-view-test--insert-data data-buf "Done.\n" 'response)
    (with-current-buffer view-buf
      (clrhash niwa-view--tool-rendering-cache))
    (let ((parse (symbol-function 'niwa-view--tool-call-parse))
          (parse-count 0))
      (cl-letf (((symbol-function 'niwa-view--tool-call-parse)
                 (lambda (&rest arguments)
                   (cl-incf parse-count)
                   (apply parse arguments))))
        (with-current-buffer data-buf
          (niwa-view-stream-render-response (point-min) (point-max)))
        (should (<= 1 parse-count 4))
        ;; A warm collapsed group needs names, not its hidden tool bodies.
        (setq parse-count 0)
        (with-current-buffer view-buf
          (niwa-view-test--insert-composer-draft "> draft\nsecond line" 3)
          (niwa-view--full-rerender)
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
          (should (= 3 (niwa-view-render-test--draft-offset)))))
      (should (= parse-count 0)))
    (with-current-buffer view-buf
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "read 4 files" text))
        (should-not (string-match-p "f0\\.el" text)))
      ;; Expand the group: rows are compound-tool nested rows.
      (goto-char (point-min))
      (search-forward "read 4 files")
      (goto-char (match-beginning 0))
      (should (eq 'tool-group
                  (get-text-property (point) 'niwa-view-type)))
      (niwa-view-toggle-section)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "Read: .*f0\\.el" text))
        (should (string-match-p "Read: .*f3\\.el" text)))
      (goto-char (point-min))
      (search-forward "f0.el")
      (should (eq 'tool-child
                  (get-text-property (match-beginning 0)
                                     'niwa-view-type)))
      ;; Expand one row without disturbing its siblings.
      (goto-char (match-beginning 0))
      (niwa-view-toggle-section)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "content 0" text))
        (should-not (string-match-p "content 1" text)))
      ;; Collapsing the group takes every row with it.
      (goto-char (point-min))
      (search-forward "read 4 files")
      (goto-char (match-beginning 0))
      (niwa-view-toggle-section)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "read 4 files" text))
        (should-not (string-match-p "f0\\.el\\|content 0" text)))))
  :doc "thinking partitions long tool runs and stays visible through either group fold"
  (niwa-view-test--with-buffers
    (let ((thought "I recommend skipping a separate wiki and instead mapping its jobs onto niwa's existing systems: preferences go in Memory, decisions in ADRs/commit messages, follow-ups in docs/backlog.md, and in-flight coordination in the shared workstream folder—symlinking would break both memory indexing and consolidation. I'll proceed with this approach while awaiting any pushback, and now I'm setting up worktrees for the first wave of 11 planned workstreams.")
          thought-start thought-end)
      (dotimes (i 8)
        (niwa-view-test--insert-data
         data-buf
         (format
          "(:name \"Read\" :args (:file_path \"/tmp/f%d.el\"))\n\ncontent %d\n"
          i i)
         `(tool . ,(format "call_%d" i)))
        (when (= i 3)
          (setq thought-start (with-current-buffer data-buf (point-max)))
          (niwa-view-test--insert-data
           data-buf
           (concat "#+begin_reasoning\n" thought "\n#+end_reasoning\n")
           'ignore)
          (setq thought-end (with-current-buffer data-buf (point-max)))))
      (niwa-view-test--insert-data data-buf "Done.\n" 'response)
      (with-current-buffer data-buf
        (niwa-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> keep\nsecond line" 4)
        (cl-flet ((check-thinking ()
                    (save-excursion
                      (goto-char (point-min))
                      (should (search-forward "Thinking..." (niwa-view-transcript-tail) t))
                      (should (eq 'thinking-summary
                                  (get-text-property (match-beginning 0)
                                                     'niwa-view-type)))
                      (let ((source (get-text-property (match-beginning 0)
                                                       'niwa-view-source)))
                        (should (eq data-buf (marker-buffer (car source))))
                        (should (eq data-buf (marker-buffer (cdr source))))
                        (should (= thought-start (marker-position (car source))))
                        (should (= thought-end (marker-position (cdr source)))))
                      (should (search-forward thought (niwa-view-transcript-tail) t))
                      (should-not (invisible-p (match-beginning 0)))
                      (should-not (get-text-property (match-beginning 0)
                                                     'niwa-view-collapsed))
                      (should (= 1 (niwa-view-test--count-substring
                                    thought (niwa-view-render-test--view-text)))))))
          (check-thinking)
          (should (= 2 (niwa-view-test--count-substring
                        "read 4 files" (niwa-view-render-test--view-text))))
          (should-not (string-search "thought 1 time" (niwa-view-render-test--view-text)))
          (dolist (group '(0 1))
            (dotimes (_ 2)
              (save-excursion
                (goto-char (point-min))
                (dotimes (_ (1+ group)) (search-forward "read 4 files"))
                (niwa-view-toggle-section))
              (check-thinking)
              (should (equal "> keep\nsecond line" (niwa-view-composer-draft-text nil t)))
              (should (= 4 (niwa-view-render-test--draft-offset)))))
          ;; Expand both groups to prove every tool remains on its own side.
          (goto-char (point-min))
          (search-forward "read 4 files")
          (niwa-view-toggle-section)
          (search-forward thought)
          (search-forward "read 4 files")
          (niwa-view-toggle-section)
          (goto-char (point-min))
          (dotimes (i 8)
            (when (= i 4) (search-forward thought))
            (should (search-forward (format "f%d.el" i) (niwa-view-transcript-tail) t)))
          (dotimes (i 8)
            (should (= 1 (niwa-view-test--count-substring
                          (format "f%d.el" i) (niwa-view-render-test--view-text)))))
          (niwa-view-render-test--goto-draft-offset 4)
          (niwa-view--full-rerender)
          (check-thinking)
          (should (equal "> keep\nsecond line" (niwa-view-composer-draft-text nil t)))
          (should (= 4 (niwa-view-render-test--draft-offset)))))))
  :doc "a short interleaved run keeps source order"
  (niwa-view-test--with-buffers
    (dotimes (i 2)
      (niwa-view-test--insert-data
       data-buf
       (format
        "(:name \"Read\" :args (:file_path \"/tmp/f%d.el\"))\n\ncontent %d\n"
        i i)
       `(tool . ,(format "call_%d" i)))
      (when (= i 0)
        (niwa-view-test--insert-data
         data-buf
         "#+begin_reasoning\nshort thought\n#+end_reasoning\n"
         'ignore)))
    (niwa-view-test--insert-data data-buf "Done.\n" 'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "f0.el")
      (search-forward "Thinking")
      (should (search-forward "short thought" (niwa-view-transcript-tail) t))
      (should-not (invisible-p (match-beginning 0)))
      (search-forward "f1.el")
      (should-not (save-excursion
                    (goto-char (point-min))
                    (search-forward "read 2 files" nil t)))))
  :doc "hidden tool rows stay hidden inside mixed activity groups"
  (niwa-view-test--with-buffers
    (let* ((hidden-tool
            (niwa-tool--create
             :name "HiddenMixed"
             :category "niwa"
             :renderer (lambda (&rest _)
                         '(:header "HiddenMixed" :hidden-p t))))
           (get-tool (symbol-function 'niwa-tool-get)))
      (dotimes (i 4)
        (niwa-view-test--insert-data
         data-buf
         (format
          "(:name \"Read\" :args (:file_path \"/tmp/f%d.el\"))\n\ncontent %d\n"
          i i)
         `(tool . ,(format "call_%d" i)))
        (when (= i 1)
          (niwa-view-test--insert-data
           data-buf
           "#+begin_reasoning\nkeep going\n#+end_reasoning\n"
           'ignore)
          (niwa-view-test--insert-data
           data-buf
           "(:name \"HiddenMixed\" :args nil)\n\nhidden\n"
           '(tool . "call_hidden"))))
      (niwa-view-test--insert-data data-buf "Done.\n" 'response)
      (cl-letf (((symbol-function 'niwa-tool-get)
                 (lambda (name &optional category)
                   (if (equal name "HiddenMixed")
                       hidden-tool
                     (funcall get-tool name category)))))
        (with-current-buffer data-buf
          (niwa-view-stream-render-response (point-min) (point-max))))
      (with-current-buffer view-buf
        (should-not (text-property-any (point-min) (niwa-view-transcript-tail)
                                       'niwa-view-type 'tool-group))
        (should (string-search "keep going" (niwa-view-render-test--view-text)))
        (goto-char (point-min))
        (search-forward "keep going")
        (should-not (invisible-p (match-beginning 0)))
        (should-not (string-match-p "HiddenMixed" (niwa-view-render-test--view-text))))))
  :doc "a failed call stays in its run, counted in the folded header, through redraws"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "> draft\nsecond line"))
    (dotimes (i 9)
      (niwa-view-test--insert-data
       data-buf
       (if (= i 4)
           "(:name \"Bash\" :args (:command \"false\"))\n\nError: command failed\n"
         (format
          "(:name \"Read\" :args (:file_path \"/tmp/f%d.el\"))\n\ncontent %d\n"
          i i))
       `(tool . ,(format "call_%d" i)))
      (with-current-buffer data-buf
        (niwa-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))))
    (with-current-buffer view-buf
      (dotimes (_ 2)
        (let ((text (niwa-view-render-test--view-text)))
          (should (= 1 (cl-count-if
                        (lambda (line)
                          (string-match-p
                           "! 9 calls: read 8 files, ran 1 command (1 failed) ▸$" line))
                        (split-string text "\n"))))
          (should-not (string-match-p "Bash:" text))
          (should-not (string-match-p "Error: command failed" text)))
        (niwa-view--full-rerender)
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t))))
      (goto-char (point-min))
      (search-forward "read 8 files")
      (niwa-view-toggle-section)
      (goto-char (point-min))
      (search-forward "Bash:")
      (should (eq 'tool-child
                  (get-text-property (match-beginning 0) 'niwa-view-type)))
      (should (string-match-p "×" (buffer-substring-no-properties
                                   (line-beginning-position) (point))))
      (should-not (string-search "Error: command failed" (niwa-view-render-test--view-text)))
      (niwa-view-toggle-section)
      (should (string-search "Error: command failed" (niwa-view-render-test--view-text)))
      (niwa-view--full-rerender)
      (should (string-search "Error: command failed" (niwa-view-render-test--view-text)))))
  :doc "a short run and a zero threshold keep individual rows"
  (let ((niwa-view-tool-group-collapse-threshold 0))
    (niwa-view-test--with-buffers
      (dotimes (i 4)
        (niwa-view-test--insert-data
         data-buf
         (format
          "(:name \"Read\" :args (:file_path \"/tmp/f%d.el\"))\n\ncontent %d\n"
          i i)
         `(tool . ,(format "call_%d" i))))
      (niwa-view-test--insert-data data-buf "Done.\n" 'response)
      (with-current-buffer data-buf
        (niwa-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (let ((text (niwa-view-render-test--view-text)))
          (should-not (string-match-p "read 4 files" text))
          (should (string-match-p "Read: .*f0\\.el" text))
          (should (string-match-p "Read: .*f3\\.el" text))))))

  :doc "delivered messages join tool groups and preserve expansion and cursor on refresh"
  (niwa-view-test--with-buffers
    (dotimes (i 4)
      (niwa-view-test--insert-data
       data-buf
       (format "#+begin_tool\n(:name \"Read\" :args (:file_path \"f%d.el\"))\n\ncontent %d\n#+end_tool\n" i i)
       `(tool . ,(format "call_%d" i)))
      (when (= i 1)
        (niwa-view-test--insert-data
         data-buf
         "\n<agent-message sender=\"/root/reviewer\">\nReview complete.\nSecond line.\n</agent-message>\n"
         nil)))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "read 4 files, received 1 message")
      (niwa-view-toggle-section)
      (search-forward "message from /root/reviewer")
      (should (get-text-property (point) 'niwa-view-collapsed))
      (niwa-view-toggle-section)
      (search-forward "Review complete.")
      (should-not (invisible-p (match-beginning 0)))
      (let ((column (current-column)))
        (niwa-view--full-rerender)
        (should (= column (current-column)))
        (should (looking-back "Review complete\\." (line-beginning-position)))
        (should-not (invisible-p (point))))
      (goto-char (point-min))
      (search-forward "read 4 files, received 1 message")
      (niwa-view-toggle-section)
      (niwa-view--full-rerender)
      (should-not (string-match-p "Review complete" (niwa-view-render-test--view-text)))
      (niwa-view-toggle-section)
      (search-forward "Review complete.")
      (should-not (invisible-p (match-beginning 0)))))


  :doc "live deliveries keep separate folds, composer draft, and reading position"
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (niwa-view-test--insert-composer-draft "> keep\ndraft" 3)
      (niwa-view-stream-begin-turn
       niwa-view--status-marker
       (with-current-buffer data-buf (copy-marker (point-min))))
      (dotimes (i 4)
        (niwa-view-test--insert-data
         data-buf
         (format "#+begin_tool\n(:name \"Read\" :args (:file_path \"f%d.el\"))\n\ncontent %d\n#+end_tool\n" i i)
         `(tool . ,(format "call_%d" i))))
      (niwa-view-render-live-update data-buf)
      (niwa-view-test--insert-data
       data-buf
       (concat "\n<agent-message sender=\"/root/reviewer\">\nFirst delivery.\n</agent-message>\n"
               "<agent-message sender=\"/root/explorer\">\nSecond delivery.\n</agent-message>\n")
       nil)
      (niwa-view-render-live-update data-buf)
      (should (equal "> keep\ndraft" (niwa-view-composer-draft-text nil t)))
      (should (= 3 (niwa-view-render-test--draft-offset)))
      (goto-char (point-min))
      (search-forward "read 4 files, received 2 messages")
      (niwa-view-toggle-section)
      (search-forward "message from /root/reviewer")
      (should (eq 'link (get-text-property (1- (point)) 'font-lock-face)))
      (niwa-view-toggle-section)
      (search-forward "First delivery.")
      (let ((column (current-column)))
        (set-window-start (selected-window) (point-min))
        (niwa-view-test--insert-data
         data-buf
         "#+begin_tool\n(:name \"Read\" :args (:file_path \"f4.el\"))\n\nlast\n#+end_tool\n"
         '(tool . "call_4"))
        (dolist (refresh '(niwa-view-render-live-update
                           niwa-view--full-rerender))
          (if (eq refresh 'niwa-view--full-rerender)
              (funcall refresh)
            (funcall refresh data-buf))
          (should (= column (current-column)))
          (should (looking-back "First delivery\\." (line-beginning-position)))
          (should-not (invisible-p (1- (point))))
          (should (= (window-start) (point-min)))
          (should (equal "> keep\ndraft" (niwa-view-composer-draft-text nil t)))))
      (search-forward "Second delivery.")
      (should (invisible-p (match-beginning 0)))))

  :doc "background notices stay outside activity groups and keep their own fold"
  (niwa-view-test--with-buffers
    (dotimes (i 4)
      (niwa-view-test--insert-data
       data-buf
       (format "#+begin_tool\n(:name \"Read\" :args (:file_path \"f%d.el\"))\n\ncontent %d\n#+end_tool\n" i i)
       `(tool . ,(format "call_%d" i)))
      (when (= i 1)
        (niwa-view-test--insert-data
         data-buf
         "\n<agent-result sender=\"/root/reviewer\" outcome=\"completed\">\nReview complete.\n</agent-result>\n"
         nil)))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "● Agent /root/reviewer finished" text))
        (should-not (string-match-p "received [0-9] message" text)))
      (goto-char (point-min))
      (search-forward "Review complete.")
      (should (invisible-p (match-beginning 0)))))

  :doc "monitor events in a long tool run stay separate visible notices"
  (niwa-view-test--with-buffers
    (dotimes (i 4)
      (niwa-view-test--insert-data
       data-buf
       (format "#+begin_tool\n(:name \"Read\" :args (:file_path \"f%d.el\"))\n\ncontent %d\n#+end_tool\n" i i)
       `(tool . ,(format "call_%d" i)))
      (when (memq i '(1 2))
        (niwa-view-test--insert-data
         data-buf
         (format "\n<agent-message type=\"MONITOR\" sender=\"/root\" recipient=\"/root\" execution_id=\"mon-1\" description=\"ticker\">\nevent %d\n</agent-message>\n" i)
         nil)))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (niwa-view-render-test--view-text)))
        (should (string-match-p "^  ● Monitor event: event 1 · ticker$" text))
        (should (string-match-p "^  ● Monitor event: event 2 · ticker$" text))
        (should-not (string-match-p "received [0-9] message" text)))))

  :doc "forming a group keeps an already expanded delivery and its cursor visible"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf
     "#+begin_tool\n(:name \"Read\" :args (:file_path \"f0.el\"))\n\ncontent\n#+end_tool\n"
     '(tool . "call_0"))
    (niwa-view-test--insert-data
     data-buf
     "<agent-message sender=\"/root/reviewer\">\nReading this delivery.\n</agent-message>\n"
     nil)
    (with-current-buffer view-buf
      (niwa-view-stream-begin-turn
       niwa-view--status-marker
       (with-current-buffer data-buf (copy-marker (point-min))))
      (niwa-view-render-live-update data-buf)
      (goto-char (point-min))
      (search-forward "message from /root/reviewer")
      (niwa-view-toggle-section)
      (search-forward "Reading this delivery.")
      (let ((column (current-column)))
        (dotimes (i 3)
          (niwa-view-test--insert-data
           data-buf
           (format "#+begin_tool\n(:name \"Read\" :args (:file_path \"f%d.el\"))\n\ncontent\n#+end_tool\n" (1+ i))
           `(tool . ,(format "call_%d" (1+ i)))))
        (niwa-view-render-live-update data-buf)
        (should (= column (current-column)))
        (should (looking-back "Reading this delivery\\." (line-beginning-position)))
        (should-not (invisible-p (1- (point))))))))


(niwa-deftest niwa-view--insert-rendered-tool/preview-body ()
  ,test
  (test)
  :doc "the initial expansion shows the preview body, not the full body"
  (with-temp-buffer
    (niwa-view-mode)
    (let ((rendering '(:header "ApplyPatch: 1 file · 5 changes"
                       :body "FULL BODY"
                       :preview-body "PREVIEW BODY"
                       :expandable-p t
                       :initially-collapsed-p nil))
          (inhibit-read-only t))
      (niwa-view--insert-rendered-tool rendering (cons 1 10))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "PREVIEW BODY" text))
        (should-not (string-match-p "FULL BODY" text)))))
  :doc "a collapsed rendering ignores the preview body"
  (with-temp-buffer
    (niwa-view-mode)
    (let ((rendering '(:header "ApplyPatch: 1 file · 5 changes"
                       :body "FULL BODY"
                       :preview-body "PREVIEW BODY"
                       :expandable-p t
                       :initially-collapsed-p t))
          (inhibit-read-only t))
      (niwa-view--insert-rendered-tool rendering (cons 1 10))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (string-match-p "PREVIEW BODY\\|FULL BODY" text))))))

(niwa-deftest niwa-view--nonblank-line-count ()
  ,test
  (test)

  :doc "counts exactly what splitting into lines would have counted"
  ;; The summary needs only the number.  Building the list to take its
  ;; length allocated one string per line of a reasoning block running to
  ;; thousands of lines, on every redraw.
  (dolist (case '("" "\n" "a\nb\n" "  \n\ta\n \n b " "one" "\n\n\n"
                  "x\n \t \ny" "trail   "))
    (should (= (length (split-string case "\n" t "[ \t]+"))
               (niwa-view--nonblank-line-count case)))))

(niwa-deftest niwa-view--clean-reasoning-text
  (:before-each (setq niwa-view--clean-reasoning-cache nil)
   :after-each (setq niwa-view--clean-reasoning-cache nil))
  ,test
  (test)

  :doc "strips every scaffolding marker in one pass over the markers"
  (should (equal "keep\ntail\n"
                 (niwa-view--clean-reasoning-text
                  (concat "#+begin_reasoning x\nkeep\n#+end_reasoning\n"
                          "#+begin_tool y\n#+end_tool\ntail\n"))))

  :doc "leaves ordinary reasoning prose untouched"
  (should (equal "just thinking\n"
                 (niwa-view--clean-reasoning-text "just thinking\n")))

  :doc "trust properties distinguish generated audits from quoted audit text"
  (let* ((audit (niwa--format-hook-audit-record
                 '(:type provider-tool-batch-start :id "call-1")))
         (trusted (concat "#+begin_reasoning\n" audit
                          "keep\n#+end_reasoning\n"))
         (quoted (substring-no-properties trusted)))
    ;; Exercise both cache insertion orders with identical visible text.
    (dolist (inputs (list (list trusted quoted) (list quoted trusted)))
      (setq niwa-view--clean-reasoning-cache nil)
      (dolist (text inputs)
        (let ((cleaned (niwa-view--clean-reasoning-text text)))
          (should (string-match-p "keep" cleaned))
          (if (eq text trusted)
              (should-not (string-match-p "niwa-hook-audit" cleaned))
            (should (string-match-p "niwa-hook-audit" cleaned)))))))

  :doc "a repeat cleaning is served from the global cache"
  ;; A render walks every turn, so each tick re-cleans every completed
  ;; reasoning block as well as the live one.
  (with-temp-buffer
    (let ((text "#+begin_reasoning a\nkeep\n#+end_reasoning\ntail\n")
          (computed 0))
      (cl-letf* ((original
                  (symbol-function 'niwa-view--clean-reasoning-text-1))
                 ((symbol-function 'niwa-view--clean-reasoning-text-1)
                  (lambda (arg)
                    (setq computed (1+ computed))
                    (funcall original arg))))
        (should (equal (niwa-view--clean-reasoning-text-1 text)
                       (niwa-view--clean-reasoning-text text)))
        (setq computed 0)
        (dotimes (_ 20) (niwa-view--clean-reasoning-text text))
        (should (= 0 computed)))))

  :doc "the cache stays bounded and keeps the newest entry"
  (with-temp-buffer
    (dotimes (index 100)
      (niwa-view--clean-reasoning-text
       (format "#+begin_reasoning %d\ntext\n#+end_reasoning\n" index)))
    (should (= niwa-view--clean-reasoning-cache-size
               (length niwa-view--clean-reasoning-cache)))
    (should (equal "text\n"
                   (cdr (assoc
                         "#+begin_reasoning 99\ntext\n#+end_reasoning\n"
                         niwa-view--clean-reasoning-cache)))))

  :doc "successive live versions are cleaned but never retained"
  (progn
    (dotimes (index 100)
      (niwa-view--clean-reasoning-text
       (format "#+begin_reasoning live\n%s" (make-string index ?x))))
    (should-not niwa-view--clean-reasoning-cache))

  :doc "a cleaning answers in any buffer, since it depends only on the text"
  (let ((text "#+begin_reasoning cross\nkeep\n#+end_reasoning\n"))
    (setq niwa-view--clean-reasoning-cache nil)
    (with-temp-buffer (niwa-view--clean-reasoning-text text))
    (with-temp-buffer
      (should (equal "keep\n"
                     (cdr (assoc text
                                 niwa-view--clean-reasoning-cache)))))))

(niwa-deftest niwa-view--scaffolding-only-text-p
  (:before-each (clrhash niwa-view--scaffolding-only-cache)
   :after-each (clrhash niwa-view--scaffolding-only-cache))
  ,test
  (test)

  :doc "separates org glue from real content, and treats nil as glue"
  (with-temp-buffer
    (clrhash niwa-view--scaffolding-only-cache)
    (should (niwa-view--scaffolding-only-text-p nil))
    (should (niwa-view--scaffolding-only-text-p
             "#+begin_reasoning x\n#+end_reasoning\n"))
    (should-not (niwa-view--scaffolding-only-text-p
                 "#+begin_reasoning x\nactual thought\n#+end_reasoning\n"))
    (should (= 1 (hash-table-count
                  niwa-view--scaffolding-only-cache))))

  :doc "a repeat verdict never cleans the text again"
  ;; A render walks every segment and asks each whether it is glue; the
  ;; segments do not change between redraws.
  (with-temp-buffer
    (clrhash niwa-view--scaffolding-only-cache)
    (let ((glue "#+begin_reasoning repeat-case\n#+end_reasoning\n")
          (cleans 0))
      (cl-letf* ((original
                  (symbol-function 'niwa-view--clean-reasoning-text-1))
                 ((symbol-function 'niwa-view--clean-reasoning-text-1)
                  (lambda (text)
                    (setq cleans (1+ cleans))
                    (funcall original text))))
        (should (niwa-view--scaffolding-only-text-p glue))
        (should (= 1 cleans))
        (dotimes (_ 30) (niwa-view--scaffolding-only-text-p glue))
        (should (= 1 cleans)))))

  :doc "content ahead of every removed construct is judged without cleaning"
  (with-temp-buffer
    (let ((cleans 0))
      (cl-letf* ((original
                  (symbol-function 'niwa-view--clean-reasoning-text-1))
                 ((symbol-function 'niwa-view--clean-reasoning-text-1)
                  (lambda (text)
                    (setq cleans (1+ cleans))
                    (funcall original text))))
        (dolist (text '("plain answer\n"
                        "\n#+begin_reasoning x\nactual thought\n#+end_reasoning\n"))
          (should-not (niwa-view--scaffolding-only-text-p text))
          (insert text)
          (should (niwa-view--leading-content-p
                   (- (point) (length text)) (point))))
        (should (= 0 cleans))
        ;; Constructs that cleaning removes still take the full path.
        (dolist (text '("#+begin_tool x\nbody\n#+end_tool\n"
                        "<system-reminder>x</system-reminder>\n"))
          (should (niwa-view--scaffolding-only-text-p text)))
        (should (= 2 cleans)))))

  :doc "the verdict cache stays bounded"
  (progn
    (clrhash niwa-view--scaffolding-only-cache)
    (dotimes (index (* 2 niwa-view--scaffolding-only-cache-size))
      (niwa-view--scaffolding-only-text-p
       (format "#+begin_reasoning segment-%d\n#+end_reasoning\n" index)))
    (should (<= (hash-table-count niwa-view--scaffolding-only-cache)
                niwa-view--scaffolding-only-cache-size)))

  :doc "a real-content miss does not evict a full glue cache"
  (progn
    (dotimes (index niwa-view--scaffolding-only-cache-size)
      (niwa-view--scaffolding-only-text-p
       (format "#+begin_reasoning segment-%d\n#+end_reasoning\n" index)))
    (niwa-view--scaffolding-only-text-p
     "#+begin_reasoning live\nactual thought")
    (should (= niwa-view--scaffolding-only-cache-size
               (hash-table-count niwa-view--scaffolding-only-cache))))

  :doc "a verdict answers in any buffer, since it depends only on the text"
  ;; The render walks more than one buffer, so a per-buffer cache was
  ;; refilled from empty rather than reused -- which is why the earlier
  ;; buffer-local version reduced nothing.
  (let ((glue "#+begin_reasoning cross-buffer\n#+end_reasoning\n")
        (cleans 0))
    (clrhash niwa-view--scaffolding-only-cache)
    (cl-letf* ((original
                (symbol-function 'niwa-view--clean-reasoning-text-1))
               ((symbol-function 'niwa-view--clean-reasoning-text-1)
                (lambda (text)
                  (setq cleans (1+ cleans))
                  (funcall original text))))
      (with-temp-buffer (should (niwa-view--scaffolding-only-text-p glue)))
      (with-temp-buffer (should (niwa-view--scaffolding-only-text-p glue)))
      (should (= 1 cleans)))))

(niwa-deftest niwa-view--tool-call-parse/absent-audits
  (:doc "an audit-free tool does not copy its large source merely to look for audits")
  (with-temp-buffer
    (org-mode)
    (let* ((body (make-string 262144 ?x))
           (raw (concat "(:name \"Eval\" :args (:expression \"x\"))\n" body))
           (original (symbol-function 'buffer-substring)))
      (insert raw)
      (cl-letf (((symbol-function 'buffer-substring)
                 (lambda (start end)
                   (should (< (- end start) 262144))
                   (funcall original start end))))
        (let ((call (niwa-view--tool-call-parse
                     (current-buffer) (point-min) (point-max) raw)))
          (should (equal body (plist-get call :result)))
          (should-not (plist-get call :hook-audits)))))))

(niwa-deftest niwa-view--render-data-only-text-p ()
  ,test
  (test)
  :doc "recognizes text holding only render data, without copying other text"
  (let ((block (niwa-tool-render-data-format '(:kind ptc :outcome ok))))
    (should (niwa-view--render-data-only-text-p (concat "\n" block "\n")))
    (should-not (niwa-view--render-data-only-text-p (concat "answer" block)))
    (should-not (niwa-view--render-data-only-text-p "  \n"))
    (should-not (niwa-view--render-data-only-text-p nil))
    (cl-letf (((symbol-function 'niwa-view--strip-render-data-display-text)
               (lambda (_) (error "Stripped text without render data"))))
      (should-not (niwa-view--render-data-only-text-p "plain answer")))))


;;
;;; In-flight tool calls

(niwa-deftest niwa-view--take-render-pending ()
  ,test
  (test)
  :doc "hands the in-flight entries to exactly one claimant"
  (let ((cell (list '(("k" "Read" "a")))))
    (let ((niwa-view--render-pending cell))
      (should (equal '(("k" "Read" "a")) (niwa-view--take-render-pending)))
      (should-not (niwa-view--take-render-pending)))
    (should-not (car cell)))
  :doc "claims nothing outside a live render"
  (let ((niwa-view--render-pending nil))
    (should-not (niwa-view--take-render-pending))))

(niwa-deftest niwa-view--pending-marker ()
  ,test
  (test)
  :doc "glyph styles animate the marker; shimmer and static keep the dot"
  (let ((niwa-view-tool-spinner-style 'ascii))
    (let ((marker (niwa-view--pending-marker 'k)))
      (should (= 4 (length marker)))
      (should (eq 'k (get-text-property 2 'niwa-view-tool-animation-id
                                        marker)))
      (should (get-text-property 2 'niwa-view-inline-spinner-frame marker))))
  (dolist (style '(shimmer static))
    (let* ((niwa-view-tool-spinner-style style)
           (marker (niwa-view--pending-marker 'k)))
      (should (equal "  ● " (substring-no-properties marker)))
      (should-not (text-property-any 0 (length marker)
                                     'niwa-view-inline-spinner-frame t
                                     marker)))))

(niwa-deftest niwa-view--pending-label ()
  ,test
  (test)
  :doc "only shimmer animates the label"
  (let ((niwa-view-tool-spinner-style 'shimmer))
    (let ((label (niwa-view--pending-label "Read" 'k)))
      (should (equal "Read" (substring-no-properties label)))
      (should (get-text-property 0 'niwa-view-inline-spinner-frame label))
      (should (eq 'k (get-text-property 0 'niwa-view-tool-animation-id
                                        label))))
    ;; The sweep uses the label's own face, defaulting to the ephemeral one.
    (should (eq 'niwa-view-tool-name
                (get-text-property
                 0 'niwa-view-tool-animation-face
                 (niwa-view--pending-label
                  (propertize "Read" 'font-lock-face 'niwa-view-tool-name) 'k))))
    (should (eq 'niwa-view-ephemeral
                (get-text-property 0 'niwa-view-tool-animation-face
                                   (niwa-view--pending-label "Read" 'k)))))
  (let ((niwa-view-tool-spinner-style 'braille))
    (should-not (get-text-property
                 0 'niwa-view-inline-spinner-frame
                 (niwa-view--pending-label "Read" 'k)))))

(niwa-deftest niwa-view--running-header ()
  ,test
  (test)
  :doc "replaces the settled marker and appends the running count"
  (let* ((niwa-view-tool-spinner-style 'shimmer)
         (line (concat (propertize "  ✓ " 'font-lock-face 'niwa-view-tool-marker)
                       "Read 4 files"))
         (running (niwa-view--running-header line 2)))
    (should (equal "  ● Read 4 files · 2 running"
                   (substring-no-properties running)))
    (should (equal "  ✓ " (substring-no-properties
                           (get-text-property
                            0 'niwa-view-settled-marker running))))
    (should (get-text-property 0 'niwa-view-pending-tool-live running))
    (should-not (get-text-property 5 'niwa-view-pending-tool-live running))
    (should (get-text-property (1- (length running))
                               'niwa-view-pending-tool-live running)))
  :doc "a virtual group has no settled marker to restore"
  (let ((running (niwa-view--running-header "  ✓ Read 4 files" 4 t)))
    (should-not (get-text-property 0 'niwa-view-settled-marker running))))

(niwa-deftest niwa-view--group-header-text ()
  ,test
  (test)
  :doc "styles the call count like a tool name and failure counts as errors"
  (let* ((summary (plist-get
                   (car (niwa-activity-group-partition
                         (list '(:role member :kind tool :name "Bash" :status failed)
                               '(:role member :kind tool :name "Bash" :status completed)
                               '(:role member :kind tool :name "Read" :status completed))
                         nil))
                   :summary))
         (line (niwa-view--group-header-text "×" 'error summary nil))
         (face (lambda (text)
                 (get-text-property (string-search text line) 'font-lock-face line))))
    (should (equal "  × 3 calls: ran 2 commands (1 failed), read 1 file"
                   (substring-no-properties line)))
    (should (equal '(niwa-view-tool-name bold) (funcall face "3 calls")))
    (should (eq 'error (funcall face "1 failed")))
    (should (eq 'niwa-view-tool-metadata (funcall face "(1")))
    (should (eq 'niwa-view-tool-summary (funcall face "ran 2")))
    (should (eq 'niwa-view-tool-summary (funcall face "read 1")))
    (should (eq 'error (funcall face "×"))))
  :doc "adds the unconfined note once, in the metadata face"
  (let ((line (niwa-view--group-header-text "✓" 'niwa-view-tool-marker
                                               "2 calls: ran 2 commands" t)))
    (should (string-suffix-p "ran 2 commands · unconfined"
                             (substring-no-properties line)))
    (should (eq 'niwa-view-tool-metadata
                (get-text-property (string-search "unconfined" line)
                                   'font-lock-face line)))))

(niwa-deftest niwa-view--pending-children ()
  ,test
  (test)
  :doc "running calls become pending children named by their tool"
  (let ((children (niwa-view--pending-children
                   '(("a" "Read" "x") ("b" "Grep" nil)))))
    (should (equal '(pending pending)
                   (mapcar (lambda (child) (plist-get child :kind)) children)))
    (should (equal '("Read" "Grep")
                   (mapcar (lambda (child) (plist-get child :tool)) children)))))

(niwa-deftest niwa-view--insert-pending-tool-row ()
  ,test
  (test)
  :doc "draws a sourceless running row in the settled row grammar"
  (with-temp-buffer
    (let ((niwa-view-tool-spinner-style 'shimmer))
      (niwa-view--insert-pending-tool-row '("k" "Read" "a.el"))
      (niwa-view--insert-pending-tool-row '("j" "Grep" nil))
      (should (equal "  ● Read: a.el\n  ● Grep\n" (buffer-string)))
      (should-not (text-property-not-all (point-min) (point-max)
                                         'niwa-view-pending-tool-live t))
      (should-not (text-property-not-all (point-min) (point-max)
                                         'niwa-view-source nil)))))

(niwa-deftest niwa-view--insert-pending-run ()
  ,test
  (test)
  :doc "groups running calls above the threshold, as they will settle"
  (with-temp-buffer
    (let ((niwa-view-tool-spinner-style 'shimmer)
          (niwa-view-tool-group-collapse-threshold 2))
      (niwa-view--insert-pending-run
       '(("a" "Read" "x") ("b" "Read" "y") ("c" "Grep" "z")))
      (should (equal "  ● 3 calls: read 2 files, searched 1 pattern · 3 running ▸\n"
                     (buffer-string)))
      (should-not (text-property-not-all (point-min) (point-max)
                                         'niwa-view-pending-tool-live t))))
  :doc "keeps separate rows at or below the threshold"
  (with-temp-buffer
    (let ((niwa-view-tool-spinner-style 'shimmer)
          (niwa-view-tool-group-collapse-threshold 2))
      (niwa-view--insert-pending-run '(("a" "Read" "x") ("b" "Grep" "z")))
      (should (equal "  ● Read: x\n  ● Grep: z\n" (buffer-string))))))

(niwa-deftest niwa-view--retire-pending-decorations ()
  ,test
  (test)
  :doc "deletes virtual rows and restores a running group's settled marker"
  (with-temp-buffer
    (let ((niwa-view-tool-spinner-style 'braille)
          (source (cons (copy-marker 1) (copy-marker 2))))
      (insert "before\n")
      (let ((start (point)))
        (insert (niwa-view--running-header
                 (concat (propertize "  ✓ " 'font-lock-face 'niwa-view-tool-marker)
                         "Read 4 files")
                 1)
                "\n")
        (add-text-properties start (point)
                             `(niwa-view-source ,source
                               niwa-view-type tool-group)))
      (niwa-view--insert-pending-tool-row '("k" "Grep" "x"))
      (insert "after\n")
      (should (niwa-view--retire-pending-decorations (point-min) (point-max)))
      (should (equal "before\n  ✓ Read 4 files\nafter\n" (buffer-string)))
      (goto-char (point-min))
      (search-forward "✓")
      (let ((pos (match-beginning 0)))
        (should (eq 'niwa-view-tool-marker
                    (get-text-property pos 'font-lock-face)))
        (should (eq source (get-text-property pos 'niwa-view-source)))
        (should (eq 'tool-group (get-text-property pos 'niwa-view-type)))
        (should-not (get-text-property pos 'display)))
      (should-not (niwa-view--retire-pending-decorations
                   (point-min) (point-max))))))

(niwa-deftest niwa-view--strip-pending-decorations-from-string ()
  ,test
  (test)
  :doc "keeps ordinary text and drops running rows from a preserved tail"
  (let ((text (with-temp-buffer
                (insert "Assistant\n")
                (niwa-view--insert-pending-tool-row '("k" "Read" "a"))
                (buffer-string))))
    (should (equal "Assistant\n"
                   (niwa-view--strip-pending-decorations-from-string text))))
  :doc "an only-running tail leaves nothing"
  (should-not (niwa-view--strip-pending-decorations-from-string
               (with-temp-buffer
                 (niwa-view--insert-pending-tool-row '("k" "Read" "a"))
                 (buffer-string))))
  (should-not (niwa-view--strip-pending-decorations-from-string nil)))

(niwa-deftest niwa-view--insert-assistant-header ()
  ,test
  (test)
  :doc "inserts the heading with its turn properties and extra properties"
  (with-temp-buffer
    (niwa-view--insert-assistant-header nil '(niwa-view-pending-tool-live t))
    (should (equal "Assistant\n" (buffer-string)))
    (should (eq 'turn-header (get-text-property 1 'niwa-view-type)))
    (should (eq 'assistant (get-text-property 1 'niwa-view-turn-role)))
    (should (get-text-property 1 'niwa-view-pending-tool-live)))
  :doc "places a variant button beside the label"
  (with-temp-buffer
    (niwa-view--insert-assistant-header "[2/3]")
    (should (equal "Assistant  [2/3]\n" (buffer-string)))))

(niwa-deftest niwa-view--live-turn-needs-header-p ()
  ,test
  (test)
  :doc "needs a heading unless the assistant's heading ends the turn"
  (cl-flet ((header (role label)
              (propertize label 'niwa-view-type 'turn-header
                          'niwa-view-turn-role role)))
    (let ((data (generate-new-buffer " *needs-header-data*")))
      (unwind-protect
          (with-temp-buffer
            (with-current-buffer data
              (setq-local niwa--current-request nil))
            ;; an empty turn needs a heading
            (should (niwa-view--live-turn-needs-header-p
                     (point-min) (point-max) data))
            ;; a turn whose last heading is the assistant's needs none
            (insert (header 'assistant "Assistant\n") "text\n")
            (should-not (niwa-view--live-turn-needs-header-p
                         (point-min) (point-max) data))
            ;; a delivery after the assistant's heading needs a new one
            (insert (header 'user "You\n") "delivered\n")
            (should (niwa-view--live-turn-needs-header-p
                     (point-min) (point-max) data)))
        (kill-buffer data)))))


;;
;;; Activity rules

(defun niwa-view-test--activity-rule-buffer (&rest parts)
  "Insert PARTS, alternating text and `niwa-view-type', then return point.
A part of the form (TEXT . FACE) carries `font-lock-face' FACE instead."
  (while parts
    (let ((part (pop parts)))
      (if (consp part)
          (insert (propertize (car part) 'font-lock-face (cdr part)))
        (insert (propertize part 'niwa-view-type (pop parts))))))
  (point))

(niwa-deftest niwa-view--activity-before-point-p ()
  ,test
  (test)
  :doc "activity rows of the same turn count, across blank lines"
  (with-temp-buffer
    (niwa-view-test--activity-rule-buffer
     "Assistant\n" 'turn-header "  ✓ Read a.el\n" 'tool-summary "\n" nil)
    (should (niwa-view--activity-before-point-p)))
  :doc "any non-prose row is activity: reminders and mailbox cards too"
  (dolist (type '(hook-audit mailbox-delivery thinking-summary agent-handle))
    (with-temp-buffer
      (niwa-view-test--activity-rule-buffer "row\n" type)
      (should (niwa-view--activity-before-point-p))))
  :doc "prose, headings, existing rules and empty buffers are not activity"
  (dolist (type '(response turn-header activity-separator nil))
    (with-temp-buffer
      (niwa-view-test--activity-rule-buffer "row\n" type "\n" nil)
      (should-not (niwa-view--activity-before-point-p))))
  (with-temp-buffer
    (should-not (niwa-view--activity-before-point-p)))
  :doc "turn-closing chrome stops the search at the turn boundary"
  (dolist (face '(niwa-view-separator
                  niwa-view-turn-rule
                  niwa-view-request-summary-rule))
    (with-temp-buffer
      ;; A user turn's reminder, then its spacer: a directive response
      ;; without its own heading must not take a rule from it.
      (niwa-view-test--activity-rule-buffer
       "  ◇ 1 system reminder" 'hook-audit (cons "\n" face))
      (should-not (niwa-view--activity-before-point-p)))))

(niwa-deftest niwa-view--insert-activity-rule-before-response ()
  ,test
  (test)
  :doc "inserts a blank line and an inset rule row after activity"
  (with-temp-buffer
    (let ((start (niwa-view-test--activity-rule-buffer
                  "  ✓ Read a.el\n" 'tool-summary)))
      (niwa-view--insert-activity-rule-before-response)
      (should (equal "\n  \n" (buffer-substring-no-properties
                               start (point))))
      (should-not (get-text-property (+ start 1) 'font-lock-face))
      (should (eq 'niwa-view-activity-rule
                  (get-text-property (1- (point)) 'font-lock-face)))
      (should (eq 'activity-separator
                  (get-text-property (+ start 1) 'niwa-view-type)))))
  :doc "inserts nothing after prose or a heading"
  (dolist (type '(response turn-header))
    (with-temp-buffer
      (let ((start (niwa-view-test--activity-rule-buffer "row\n" type)))
        (niwa-view--insert-activity-rule-before-response)
        (should (= start (point)))))))

(niwa-deftest niwa-view--ensure-blank-line-after-response ()
  ,test
  (test)
  :doc "separates activity from the prose above it by a blank line only"
  (with-temp-buffer
    (let ((start (niwa-view-test--activity-rule-buffer
                  "Prose.\n" 'response)))
      (niwa-view--ensure-blank-line-after-response)
      (should (equal "\n" (buffer-substring-no-properties start (point))))
      (should-not (text-property-not-all start (point) 'font-lock-face nil))))
  :doc "is a no-op after activity, so repeated calls add nothing"
  (with-temp-buffer
    (let ((start (niwa-view-test--activity-rule-buffer
                  "  ✓ Read a.el\n" 'tool-summary)))
      (niwa-view--ensure-blank-line-after-response)
      (should (= start (point))))))


(provide 'test-niwa-view-render)
;;; test-niwa-view-render.el ends here
