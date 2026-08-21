;;; test-mevedel-compact-evidence.el -- Tests for compaction evidence -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'mevedel)
(require 'mevedel-agent-control)
(require 'mevedel-agent-exec)
(require 'mevedel-agent-runtime)
(require 'mevedel-compact)
(require 'mevedel-compact-estimation)
(require 'mevedel-compact-evidence)
(require 'mevedel-compact-run)
(require 'mevedel-compact-target)
(require 'mevedel-execution-transcript)
(require 'mevedel-models)
(require 'mevedel-hooks)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-system)
(require 'mevedel-utilities)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-render)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-compact-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-compact-test-support"))

(defvar gptel--request-params)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-model)
(defvar gptel-reasoning-effort)

(mevedel-deftest mevedel-compact-evidence-previous-summary ()
  ,test
  (test)
  :doc "strips the persisted handoff prefix before reusing a summary"
  (with-temp-buffer
    (insert "#+begin_summary\n"
            mevedel-session-artifacts--summary-handoff-prefix
            "## Goal\n- Continue\n"
            (mevedel--format-hook-audit-record
             '(:type compact-context
               :event "PreCompact"
               :context "private audit"))
            "#+end_summary\n")
    (should (equal "## Goal\n- Continue"
                   (mevedel-compact-evidence-previous-summary)))))

(mevedel-deftest mevedel-compact-evidence-agent-task-heading ()
  ,test
  (test)
  :doc "finds the first initial-task marker for the exact agent path"
  (with-temp-buffer
    (org-mode)
    (let ((other
           (mevedel-agent-invocation--create :path "/root/other"))
          (invocation
           (mevedel-agent-invocation--create :path "/root/inspect")))
      (test-mevedel-compact--insert-agent-task
       other "other" "Other task.")
      (insert "* Agent Task: unmarked\n\nUnmarked task.\n")
      (let ((initial-heading (point)))
        (test-mevedel-compact--insert-agent-task
         invocation "inspect" "Initial task.")
        (test-mevedel-compact--insert-agent-task
         invocation "follow-up" "Later task.")
        (should (= initial-heading
                   (mevedel-compact-evidence-agent-task-heading invocation))))))

  :doc "returns nil when no task marker matches the agent path"
  (with-temp-buffer
    (org-mode)
    (let ((other
           (mevedel-agent-invocation--create :path "/root/other"))
          (invocation
           (mevedel-agent-invocation--create :path "/root/missing")))
      (test-mevedel-compact--insert-agent-task
       other "other" "Other task.")
      (insert "* Agent Task: unmarked\n\nUnmarked task.\n")
      (should-not (mevedel-compact-evidence-agent-task-heading invocation)))))

(mevedel-deftest mevedel-compact-evidence-agent-summary-bounds ()
  ,test
  (test)
  :doc "finds the anchored summary body in an agent transcript"
  (with-temp-buffer
    (org-mode)
    (let ((invocation
           (mevedel-agent-invocation--create :path "/root/inspect")))
      (setq-local mevedel--agent-invocation invocation)
      (test-mevedel-compact--insert-agent-task
       invocation "inspect" "Keep this task.")
      (insert "#+begin_summary\n## Goal\n- Continue\n#+end_summary\n"
              "Recent tail.\n")
      (let ((bounds (mevedel-compact-evidence-agent-summary-bounds invocation)))
      (should (equal "#+begin_summary\n## Goal\n- Continue\n#+end_summary"
                     (buffer-substring-no-properties
                      (plist-get bounds :begin) (plist-get bounds :end))))
      (should (equal "## Goal\n- Continue\n"
                     (buffer-substring-no-properties
                      (plist-get bounds :body-begin)
                        (plist-get bounds :body-end)))))))

  :doc "uses the path marker to ignore inherited task headings and summaries"
  (with-temp-buffer
    (org-mode)
    (let ((parent
           (mevedel-agent-invocation--create :path "/root/parent"))
          (child
           (mevedel-agent-invocation--create :path "/root/parent/child")))
      (test-mevedel-compact--insert-agent-task
       parent "parent" "Parent task.")
      (insert "#+begin_summary\nParent summary.\n#+end_summary\n")
      (setq-local mevedel--agent-invocation child)
      (test-mevedel-compact--insert-agent-task
       child "child" "Child task.")
      (insert "#+begin_summary\nAgent summary.\n#+end_summary\n")
      (let ((bounds (mevedel-compact-evidence-agent-summary-bounds child)))
        (should (equal "Agent summary.\n"
                       (buffer-substring-no-properties
                        (plist-get bounds :body-begin)
                        (plist-get bounds :body-end))))))))

(mevedel-deftest mevedel-compact-evidence-archived-tool-use-ids ()
  ,test
  (test)
  :doc "collects each concrete tool row removed by the compacted prefix"
  (with-temp-buffer
    (insert (propertize "first" 'gptel '(tool . "call-1")))
    (insert " plain ")
    (insert (propertize "again" 'gptel '(tool . "call-1")))
    (insert (propertize "second" 'gptel '(tool . "call-2")))
    (insert
     (mevedel--format-hook-audit-record
      '(:type execution-archive :tool-use-id "call-archived"
        :render-data (:execution-id "exec-archived" :state running
                      :live-execution-p t))))
    (should
     (equal '("call-1" "call-2" "call-archived")
            (mevedel-compact-evidence-archived-tool-use-ids
             (point-min) (point-max))))))

(mevedel-deftest mevedel-compact-evidence-current-tool-batch-start ()
  ,test
  (test)
  :doc "includes reasoning and prose before the active continuation tools"
  (with-temp-buffer
    (let ((body-start (point)))
      (insert (propertize "old tool\n" 'gptel '(tool . "call-old")))
      (let ((batch-start (point)))
        (insert "#+begin_reasoning\nthinking\n#+end_reasoning\n")
        (insert (propertize "assistant preface\n" 'gptel 'response))
        (insert (propertize "current tool one\n"
                            'gptel '(tool . "call-current-1")))
        (insert (propertize "current tool two\n"
                            'gptel '(tool . "call-current-2")))
        (should
         (= batch-start
            (mevedel-compact-evidence-current-tool-batch-start
             (list :tool-use
                   '((:id "call-current-1") (:id "call-current-2")))
             body-start)))))))

(mevedel-deftest mevedel-compact-evidence-select
  (:doc "projects the same aggressive source selection for external consumers")
  ,test
  (test)
  (with-temp-buffer
    (insert "Inherited context.\n")
    (let ((prefix-end (point)))
      (insert "Stable anchor.\n")
      (let ((body-start (point)))
        (insert "Current body.\n")
        (let ((selection
               (mevedel-compact-evidence-select
                (list :body-start body-start
                      :history-prefix-regions
                      (list (cons (point-min) prefix-end)))
                (point-max) t)))
          (should (string-match-p "Inherited context"
                                  (plist-get selection :content)))
          (should (string-match-p "Current body"
                                  (plist-get selection :content)))
          (should-not (string-match-p "Stable anchor"
                                      (plist-get selection :content)))
          (should (= 0 (plist-get selection :preserved-tail-turns))))))))

(mevedel-deftest mevedel-compact-evidence-turn-starts-before ()
  ,test
  (test)
  :doc "scans the block prefix linearly across a pass, not per turn"
  ;; Each prefix scan restarted at point-min, so a pass cost time quadratic in
  ;; the transcript, and a live segment is bounded only by the compaction
  ;; threshold.  Assert the characters scanned, not a duration: a pass may
  ;; cover the buffer a small number of times, never once per turn.
  (with-temp-buffer
    (let ((turns 20)
          (scanned 0)
          expected)
      (dotimes (i turns)
        (insert (format "u%d\n" i))
        (let ((response-start (point)))
          (insert (format "#+begin_tool\ncall %d\n#+end_tool\na%d\n" i i))
          (put-text-property response-start (point) 'gptel 'response)))
      (setq expected (mevedel-compact-evidence-turn-starts-before (point-max)))
      (should (= turns (length expected)))
      (cl-letf* ((real (symbol-function 're-search-forward))
                 ((symbol-function 're-search-forward)
                  (lambda (&rest args)
                    (cl-incf scanned)
                    (apply real args))))
        (should (equal expected
                       (mevedel-compact-evidence-turn-starts-before
                        (point-max)))))
      ;; A pass costs searches proportional to the markers it passes, not to
      ;; markers times turns: twenty turns of one block each cost 491 searches
      ;; before the prefix count was carried and 149 after.
      (should (< scanned (* 10 turns)))))

  :doc "ignores leading org metadata when finding turn starts"
  (with-temp-buffer
    (insert ":PROPERTIES:\n:foo: bar\n:END:\n")
    (let ((u1-start (point)))
      (insert "u1\n")
      (let ((a1-start (point)))
        (insert "a1\n")
        (put-text-property a1-start (point) 'gptel 'response))
      (let ((u2-start (point)))
        (insert "u2\n")
        (let ((a2-start (point)))
          (insert "a2\n")
          (put-text-property a2-start (point) 'gptel 'response))
        (should (equal (mevedel-compact-evidence-turn-starts-before (point-max))
                       (list u1-start u2-start))))))

  :doc "does not count prompt text after limit in a widened user span"
  (with-temp-buffer
    (let ((u1-start (point)))
      (insert "u1\n")
      (let ((a1-start (point)))
        (insert "a1\n")
        (put-text-property a1-start (point) 'gptel 'response))
      (insert "\n  ")
      (let ((limit (point)))
        (insert "u2\n")
        (should (equal (mevedel-compact-evidence-turn-starts-before limit)
                       (list u1-start))))))
  :doc "skips unpropertized reasoning and tool scaffolding before prompt"
  (with-temp-buffer
    (let ((u1-start (point)))
      (insert "u1\n")
      (let ((a1-start (point)))
        (insert "a1\n")
        (put-text-property a1-start (point) 'gptel 'response))
      (insert "#+begin_reasoning\nThinking text.\n")
      (insert "#+begin_tool (WebFetch :url \"https://example.com\")\n")
      (let ((tool-start (point)))
        (insert "(:name \"WebFetch\" :args (:url \"https://example.com\"))\n\n"
                "body\n")
        (put-text-property tool-start (point) 'gptel '(tool . "call_1")))
      (insert "#+end_tool\nMore thinking.\n#+end_reasoning\n")
      (let ((u2-start (point)))
        (insert "u2\n")
        (let ((a2-start (point)))
          (insert "a2\n")
          (put-text-property a2-start (point) 'gptel 'response))
        (should (equal (mevedel-compact-evidence-turn-starts-before (point-max))
                       (list u1-start u2-start))))))
  :doc "keeps user-authored org block marker as turn start"
  (with-temp-buffer
    (let ((u1-start (point)))
      (insert "u1\n")
      (let ((a1-start (point)))
        (insert "a1\n")
        (put-text-property a1-start (point) 'gptel 'response))
      (let ((u2-start (point)))
        (insert "#+begin_src emacs-lisp\n")
        (insert "(message \"hello\")\n")
        (insert "#+end_src\n")
        (let ((a2-start (point)))
          (insert "a2\n")
          (put-text-property a2-start (point) 'gptel 'response))
        (should (equal (mevedel-compact-evidence-turn-starts-before (point-max))
                       (list u1-start u2-start)))))))

(mevedel-deftest mevedel-compact-evidence-context-snapshot ()
  ,test
  (test)

  :doc "forks all effective live context with text properties"
  (with-temp-buffer
    (insert "#+begin_summary\nOld turns summarized.\n#+end_summary\n")
    (insert "Recent prompt.\n")
    (let ((response-start (point)))
      (insert "Recent response.\n")
      (put-text-property response-start (point) 'gptel 'response))
    (let ((snapshot (mevedel-compact-evidence-context-snapshot 'all)))
      (should (equal (buffer-string) snapshot))
      (should (eq 'response
                  (get-text-property
                   (string-match "Recent response" snapshot) 'gptel snapshot)))))

  :doc "forks no context"
  (with-temp-buffer
    (insert "Parent history.\n")
    (should (equal "" (mevedel-compact-evidence-context-snapshot 'none))))

  :doc "forks an anchored summary and only the requested recent turns"
  (with-temp-buffer
    (insert "#+begin_summary\nArchived raw text summarized.\n#+end_summary\n")
    (insert "First live prompt.\n")
    (let ((response-start (point)))
      (insert "First live response.\n")
      (put-text-property response-start (point) 'gptel 'response))
    (insert "Second live prompt.\n")
    (let ((response-start (point)))
      (insert "Second live response.\n")
      (put-text-property response-start (point) 'gptel 'response))
    (let ((snapshot (mevedel-compact-evidence-context-snapshot 1)))
      (should (string-match-p "Archived raw text summarized" snapshot))
      (should-not (string-match-p "First live prompt" snapshot))
      (should (string-match-p "Second live prompt" snapshot))))

  :doc "keeps an agent task anchor with its summary"
  (with-temp-buffer
    (org-mode)
    (let ((invocation
           (mevedel-agent-invocation--create :path "/root/parent")))
      (setq-local mevedel--agent-invocation invocation)
      (test-mevedel-compact--insert-agent-task
       invocation "parent" "Original task.")
      (insert "#+begin_summary\nEarlier work summarized.\n#+end_summary\n"
              "Recent prompt.\n"))
    (let ((response-start (point)))
      (insert "Recent response.\n")
      (put-text-property response-start (point) 'gptel 'response))
    (let ((snapshot (mevedel-compact-evidence-context-snapshot 1)))
      (should (string-prefix-p "* Agent Task: parent" snapshot))
      (should (string-match-p "Earlier work summarized" snapshot))
      (should (string-match-p "Recent prompt" snapshot)))))

(mevedel-deftest mevedel-compact-evidence-summary-context-evidence ()
  ,test
  (test)
  :doc "freezes realized parent evidence without the triggering Agent call"
  (with-temp-buffer
    (org-mode)
    (insert "Parent requirement.\n")
    (let ((start (point)))
      (insert "(:name \"Read\" :args (:file_path \"a.el\"))\n\n"
              "Sibling evidence.\n")
      (put-text-property start (point) 'gptel '(tool . "call_read")))
    (let ((start (point)))
      (insert "(:name \"Agent\" :args (:task_name \"child\"))\n\n"
              "Triggering tool placeholder.\n")
      (put-text-property start (point) 'gptel '(tool . "call_agent")))
    (let ((evidence
           (mevedel-compact-evidence-summary-context-evidence "call_agent")))
      (erase-buffer)
      (insert "Later parent text.")
      (should (string-match-p "Parent requirement" evidence))
      (should (string-match-p "Sibling evidence" evidence))
      (should-not (string-match-p "Triggering tool" evidence))
      (should-not (string-match-p "task_name" evidence))))

  :doc "includes only skill provenance from the delegating conversation"
  (with-temp-buffer
    (org-mode)
    (let* ((session (mevedel-session--create :name "main" :turn-count 4))
           (invocation
            (mevedel-agent-invocation--create :path "/root/parent"))
           (parent-skill
            (mevedel-skill-invocation-record--create
             :name "parent-skill" :role 'command :origin 'model
             :agent-path "/root/parent" :turn 4))
           (root-skill
            (mevedel-skill-invocation-record--create
             :name "root-skill" :role 'command :origin 'user
             :agent-path "/root" :turn 3)))
      (setf (mevedel-session-invoked-skills session)
            (list root-skill parent-skill))
      (setq-local mevedel--session session
                  mevedel--agent-invocation invocation)
      (insert "Delegating evidence.\n")
      (let ((evidence
             (mevedel-compact-evidence-summary-context-evidence "call_agent")))
        (should (string-match-p "parent-skill" evidence))
        (should-not (string-match-p "root-skill" evidence))))))

(mevedel-deftest mevedel-compact-evidence-tail-start ()
  ,test
  (test)
  :doc "keeps configured recent response turns when budget allows"
  (with-temp-buffer
    (insert "u1\n")
    (let ((a1-start (point)))
      (insert "a1\n")
      (put-text-property a1-start (point) 'gptel 'response))
    (let ((after-a1 (point)))
      (insert "u2\n")
      (let ((a2-start (point)))
        (insert "a2\n")
        (put-text-property a2-start (point) 'gptel 'response))
      (insert "u3\n")
      (let ((a3-start (point)))
        (insert "a3\n")
        (put-text-property a3-start (point) 'gptel 'response))
      (let ((mevedel-model-context-limit 200000)
            (gptel-model nil)
            (mevedel-compact-evidence-tail-turns 2)
            (mevedel-compact-evidence-tail-budget 0.25))
        (should (= (mevedel-compact-evidence-tail-start (point-max) nil)
                   after-a1)))))

  :doc "drops older preserved turns when tail budget would be exceeded"
  (with-temp-buffer
    (insert "u1\n")
    (let ((a1-start (point)))
      (insert "a1\n")
      (put-text-property a1-start (point) 'gptel 'response))
    (insert "u2\n")
    (let ((a2-start (point)))
      (insert (make-string 40 ?a) "\n")
      (put-text-property a2-start (point) 'gptel 'response))
    (let ((after-a2 (point)))
      (insert "u3\n")
      (let ((a3-start (point)))
        (insert (make-string 40 ?b) "\n")
        (put-text-property a3-start (point) 'gptel 'response))
      (let ((mevedel-model-context-limit 100)
            (mevedel-model-reserve-tokens 20)
            (gptel-model nil)
            (mevedel-compact-evidence-tail-turns 2)
            (mevedel-compact-evidence-tail-budget 0.01))
        (should (= (mevedel-compact-evidence-tail-start (point-max) nil)
                   after-a2)))))

  :doc "drops older turn even when session has only target turn count"
  (with-temp-buffer
    (insert "u1\n")
    (let ((a1-start (point)))
      (insert (make-string 40 ?a) "\n")
      (put-text-property a1-start (point) 'gptel 'response))
    (let ((after-a1 (point)))
      (insert "u2\n")
      (let ((a2-start (point)))
        (insert (make-string 40 ?b) "\n")
        (put-text-property a2-start (point) 'gptel 'response))
      (let ((mevedel-model-context-limit 100)
            (mevedel-model-reserve-tokens 20)
            (gptel-model nil)
            (mevedel-compact-evidence-tail-turns 2)
            (mevedel-compact-evidence-tail-budget 0.01))
        (should (= (mevedel-compact-evidence-tail-start (point-max) nil)
                   after-a1)))))

  :doc "keeps tool-using response chunks inside the same turn"
  (with-temp-buffer
    (insert "u1\n")
    (let ((a1-start (point)))
      (insert "a1\n")
      (put-text-property a1-start (point) 'gptel 'response))
    (let ((u2-start (point)))
      (insert "u2\n")
      (let ((a2a-start (point)))
        (insert "a2 part 1\n")
        (put-text-property a2a-start (point) 'gptel 'response))
      (let ((tool-start (point)))
        (insert "tool result\n")
        (put-text-property tool-start (point) 'gptel '(tool . result)))
      (let ((a2b-start (point)))
        (insert "a2 part 2\n")
        (put-text-property a2b-start (point) 'gptel 'response))
      (insert "u3\n")
      (let ((a3-start (point)))
        (insert "a3\n")
        (put-text-property a3-start (point) 'gptel 'response))
      (let ((mevedel-model-context-limit 200000)
            (gptel-model nil)
            (mevedel-compact-evidence-tail-turns 2)
            (mevedel-compact-evidence-tail-budget 0.25))
        (should (= (mevedel-compact-evidence-tail-start (point-max) nil)
                   u2-start)))))

  :doc "retains a directive boundary pair when its turn enters the tail"
  (with-temp-buffer
    (insert "u1\n")
    (let ((response-start (point)))
      (insert "a1\n")
      (put-text-property response-start (point) 'gptel 'response))
    (let ((boundary-start (point)))
      (insert (mevedel--format-hook-audit-record
               '(:type directive-turn-boundary :edge start
                 :directive-id "d1" :action discuss :turn 2)))
      (insert "directive prompt\n")
      (let ((response-start (point)))
        (insert "directive answer\n")
        (put-text-property response-start (point) 'gptel 'response))
      (insert (mevedel--format-hook-audit-record
               '(:type directive-turn-boundary :edge end
                 :directive-id "d1" :action discuss :turn 2
                 :outcome success :sequence 1)))
      (insert "u3\n")
      (let ((response-start (point)))
        (insert "a3\n")
        (put-text-property response-start (point) 'gptel 'response))
      (let ((mevedel-model-context-limit 200000)
            (gptel-model nil)
            (mevedel-compact-evidence-tail-turns 2)
            (mevedel-compact-evidence-tail-budget 0.25))
        (should (= boundary-start
                   (mevedel-compact-evidence-tail-start (point-max) nil)))))))

(mevedel-deftest mevedel-compact-evidence-rebuild-prompt-buffer ()
  ,test
  (test)
  :doc "reapplies directive context projection after rebuilding the prompt"
  (let ((source (generate-new-buffer " *compact-source*"))
        (prompt (generate-new-buffer " *compact-prompt*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "ordinary\n")
            (insert (mevedel--format-hook-audit-record
                     '(:type directive-turn-boundary :edge start
                       :directive-id "d1" :action discuss :turn 2)))
            (insert "directive body\n")
            (insert (mevedel--format-hook-audit-record
                     '(:type directive-turn-boundary :edge end
                       :directive-id "d1" :action discuss :turn 2
                       :outcome success :sequence 1))))
          (with-current-buffer prompt
            (mevedel-compact-evidence-rebuild-prompt-buffer
             prompt source nil nil nil)
            (goto-char (point-min))
            (should-not (get-text-property (point) 'gptel))
            (search-forward "directive body")
            (should (eq 'ignore (get-text-property (match-beginning 0)
                                                    'gptel)))))
      (kill-buffer source)
      (kill-buffer prompt))))

(mevedel-deftest mevedel-compact-evidence-pending-text-from-prompt-buffer ()
  ,test
  (test)
  :doc "uses prompt-buffer response boundary after inserted reminders"
  (with-temp-buffer
    (insert "old user\n")
    (let ((response-start (point)))
      (insert "old response\n")
      (put-text-property response-start (point) 'gptel 'response))
    (insert "<system-reminder>\nexpanded reminder\n</system-reminder>\n")
    (insert "new user prompt\n")
    (let ((text (mevedel-compact-evidence-pending-text-from-prompt-buffer)))
      (should (string-prefix-p "<system-reminder>" text))
      (should (string-match-p "expanded reminder" text))
      (should (string-match-p "new user prompt" text)))))

(mevedel-deftest mevedel-compact-evidence-region-with-tool-output-cap ()
  ,test
  (test)
  :doc "caps tool output spans while preserving surrounding text"
  (with-temp-buffer
    (insert "before\n")
    (let ((tool-start (point)))
      (insert "abcdef")
      (put-text-property tool-start (point) 'gptel '(tool . "call-1")))
    (insert "\nafter\n")
    (let ((text (mevedel-compact-evidence-region-with-tool-output-cap
                 (point-min) (point-max) 3 t)))
      (should (string-match-p "before" text))
      (should (string-match-p "abc" text))
      (should-not (string-match-p "def" text))
      (should (string-match-p "omitted 3 chars" text))
      (should (string-match-p "after" text))))

  :doc "restores tool property only on parseable org tool sexp and result"
  (with-temp-buffer
    (insert "#+begin_tool (Bash :command \"date\")\n")
    (let ((tool-start (point)))
      (insert "(:name \"Bash\" :args (:command \"date\"))\n\nresult\n")
      (let ((tool-end (point)))
        (insert "#+end_tool\n")
        (put-text-property tool-start tool-end 'gptel '(tool . "call-date"))))
    (let ((text (mevedel-compact-evidence-region-with-tool-output-cap
                 (point-min) (point-max) 1000 nil)))
      (with-temp-buffer
        (insert text)
        (let* ((sexp-start (progn
                             (goto-char (point-min))
                             (search-forward "(:name")
                             (match-beginning 0)))
               (suffix-start (progn
                               (goto-char (point-min))
                               (search-forward "#+end_tool")
                               (match-beginning 0))))
          (should-not (eq (car-safe (get-text-property (point-min) 'gptel))
                          'tool))
          (should (equal (get-text-property sexp-start 'gptel)
                         '(tool . "call-date")))
          (should-not (eq (car-safe (get-text-property suffix-start 'gptel))
                          'tool))
          (goto-char sexp-start)
          (should (equal "Bash" (plist-get (read (current-buffer)) :name)))))))

  :doc "does not restore tool property over unparseable org tool scaffolding"
  (with-temp-buffer
    (let ((tool-start (point)))
      (insert "#+begin_tool (Bash :command \"date\")\nnot a sexp\n#+end_tool\n")
      (put-text-property tool-start (point) 'gptel '(tool . "call-bad")))
    (let ((text (mevedel-compact-evidence-region-with-tool-output-cap
                 (point-min) (point-max) 1000 nil)))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (not (eobp))
          (should-not (eq (car-safe (get-text-property (point) 'gptel))
                          'tool))
          (goto-char (next-single-property-change (point) 'gptel nil
                                                  (point-max)))))))

  :doc "does not restore tool property over unparseable Lisp-looking spans"
  (with-temp-buffer
    (let ((tool-start (point)))
      (insert "(:name \"Bash\" :args")
      (put-text-property tool-start (point) 'gptel '(tool . "call-bad")))
    (let ((text (mevedel-compact-evidence-region-with-tool-output-cap
                 (point-min) (point-max) 1000 nil)))
      (with-temp-buffer
        (insert text)
        (should-not (eq (car-safe (get-text-property (point-min) 'gptel))
                        'tool)))))

  :doc "ignores stray readable sexps inside malformed org tool result text"
  (with-temp-buffer
    (let ((tool-start (point)))
      (insert "#+begin_tool (Bash :command \"date\")\n"
              "result mentions (:name \"Fake\")\n"
              "#+end_tool\n")
      (put-text-property tool-start (point) 'gptel '(tool . "call-bad")))
    (let ((text (mevedel-compact-evidence-region-with-tool-output-cap
                 (point-min) (point-max) 1000 nil)))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (not (eobp))
          (should-not (eq (car-safe (get-text-property (point) 'gptel))
                          'tool))
          (goto-char (next-single-property-change (point) 'gptel nil
                                                  (point-max)))))))

  :doc "keeps large Edit tool arguments readable when truncation lands inside args"
  (with-temp-buffer
    (let ((large-arg (make-string 2000 ?x)))
      (insert "#+begin_tool (Edit :file_path \"mevedel-chat.el\" :old_string \"...\")\n")
      (let ((tool-start (point)))
        (insert (prin1-to-string
                 (list :name "Edit"
                       :args (list :file_path "mevedel-chat.el"
                                   :old_string large-arg
                                   :new_string large-arg))))
        (insert "\n\nEdited mevedel-chat.el (+1 -1)\n#+end_tool\n")
        (put-text-property tool-start (point) 'gptel '(tool . "call-edit"))))
    (let* ((text (mevedel-compact-evidence-region-with-tool-output-cap
                  (point-min) (point-max) 200 t))
           (sexp-start (string-match "(:name" text))
           (read-result (read-from-string text sexp-start))
           (sexp (car read-result)))
      (should (equal "Edit" (plist-get sexp :name)))
      (should (equal "mevedel-chat.el" (plist-get (plist-get sexp :args)
                                                  :file_path)))
      (should (string-match-p "string argument truncated" text))
      (should (string-match-p "^#\\+end_tool" text))))

  :doc "keeps the org tool close marker after truncating a large result body"
  (with-temp-buffer
    (insert "#+begin_tool (Read :file_path \"big.txt\")\n")
    (let ((tool-start (point)))
      (insert "(:name \"Read\" :args (:file_path \"big.txt\"))\n\n")
      (insert (make-string 500 ?r))
      (insert "\n#+end_tool\n")
      (put-text-property tool-start (point) 'gptel '(tool . "call-read")))
    (let ((text (mevedel-compact-evidence-region-with-tool-output-cap
                 (point-min) (point-max) 80 t)))
      (should (string-match-p "tool output truncated" text))
      (should (string-match-p "\n#\\+end_tool\n\\'" text))))

  :doc "shortens large args in unpropertied org tool headers"
  (with-temp-buffer
    (let* ((large-arg (make-string 2000 ?x))
           (header-form (list 'Edit :file_path "mevedel-chat.el"
                              :old_string large-arg)))
      (insert "#+begin_tool " (prin1-to-string header-form) "\n")
      (let ((tool-start (point)))
        (insert "(:name \"Edit\" :args (:file_path \"mevedel-chat.el\"))\n\n")
        (insert "Edited mevedel-chat.el (+1 -1)\n#+end_tool\n")
        (put-text-property tool-start (point) 'gptel '(tool . "call-edit")))
      (let* ((text (mevedel-compact-evidence-region-with-tool-output-cap
                    (point-min) (point-max) 200 t))
             (header-start (string-match "#\\+begin_tool " text))
             (header (car (read-from-string text (match-end 0)))))
        (should header-start)
        (should (eq 'Edit (car header)))
        (should (equal "mevedel-chat.el" (plist-get (cdr header) :file_path)))
        (should (string-match-p "string argument truncated"
                                (plist-get (cdr header) :old_string))))))

  :doc "escapes nested-looking tool markers in truncated result bodies"
  (with-temp-buffer
    (insert "#+begin_tool (Read :file_path \"outer.txt\")\n")
    (let ((tool-start (point)))
      (insert "(:name \"Read\" :args (:file_path \"outer.txt\"))\n\n")
      (insert "outer before\n")
      (insert "#+begin_tool (Bash :command \"echo nested\")\n")
      (insert "(:name \"Bash\" :args (:command \"echo nested\"))\n")
      (insert "nested result\n#+end_tool\nouter after\n#+end_tool\n")
      (put-text-property tool-start (point) 'gptel '(tool . "call-read")))
    (let* ((text (mevedel-compact-evidence-region-with-tool-output-cap
                  (point-min) (point-max) 80 t))
           (begin-count (cl-loop with pos = 0
                                 while (string-match "^#\\+begin_tool" text pos)
                                 count t
                                 do (setq pos (match-end 0))))
           (end-count (cl-loop with pos = 0
                               while (string-match "^#\\+end_tool" text pos)
                               count t
                               do (setq pos (match-end 0)))))
      (should (= 1 begin-count))
      (should (= 1 end-count))
      (should (string-match-p "# [+]begin_tool" text))
      (should (string-match-p "tool output truncated" text))))

  :doc "escapes nested-looking tool markers in retained result bodies"
  (with-temp-buffer
    (insert "#+begin_tool (Read :file_path \"outer.txt\")\n")
    (let ((tool-start (point)))
      (insert "(:name \"Read\" :args (:file_path \"outer.txt\"))\n\n")
      (insert "short before\n#+begin_tool (Bash :command \"nested\")\nshort after\n")
      (insert "#+end_tool\n")
      (put-text-property tool-start (point) 'gptel '(tool . "call-read")))
    (let* ((text (mevedel-compact-evidence-region-with-tool-output-cap
                  (point-min) (point-max) 10000 t))
           (begin-count (cl-loop with pos = 0
                                 while (string-match "^#\\+begin_tool" text pos)
                                 count t
                                 do (setq pos (match-end 0))))
           (end-count (cl-loop with pos = 0
                               while (string-match "^#\\+end_tool" text pos)
                               count t
                               do (setq pos (match-end 0)))))
      (should (= 1 begin-count))
      (should (= 1 end-count))
      (should (string-match-p "# [+]begin_tool" text))
      (should-not (string-match-p "tool output truncated" text)))))

(mevedel-deftest mevedel-compact-evidence--skill-provenance ()
  ,test
  (test)
  :doc "returns no provenance when no session"
  (should-not (mevedel-compact-evidence--skill-provenance nil 0))

  :doc "returns no provenance when the session has no skill records"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "c1" :root "/tmp/c1" :name "c1"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (should-not (mevedel-compact-evidence--skill-provenance session 0)))

  :doc "lists invoked skills with name, args, role, origin, and turn"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "c2" :root "/tmp/c2" :name "c2"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (rec1 (mevedel-skill-invocation-record--create
                :name "grill-me" :args "spec 22"
                :role 'command :origin 'user :agent-path "/root" :turn 3
                :source-path "/skills/grill-me/SKILL.md"
                :prepared-body "Body 1"))
         (rec2 (mevedel-skill-invocation-record--create
                :name "review-spec" :args nil
                :role 'command :origin 'model
                :agent-path "/root/reviewer" :turn 7
                :source-path "/skills/review-spec/SKILL.md"
                :prepared-body "Body 2")))
    (setf (mevedel-session-invoked-skills session) (list rec1 rec2))
    (setf (mevedel-session-turn-count session) 9)
    (let ((items (mevedel-compact-evidence--skill-provenance session 0)))
      (should (= 2 (length items)))
      (should (string-match-p "\\$grill-me spec 22" (car items)))
      (should (string-match-p "role: command, origin: user" (car items)))
      (should (string-match-p "turn: 3" (car items)))
      (should (string-match-p "\\$review-spec" (cadr items)))
      (should (string-match-p "role: command, origin: model"
                              (cadr items))))
    (let ((items (mevedel-compact-evidence--skill-provenance session 3)))
      (should (= 1 (length items)))
      (should (string-match-p "\\$grill-me" (car items))))
    (let ((items (mevedel-compact-evidence--skill-provenance
                  session 0 "/root/reviewer")))
      (should (= 1 (length items)))
      (should (string-match-p "\\$review-spec" (car items))))))

(provide 'test-mevedel-compact-evidence)

;;; test-mevedel-compact-evidence.el ends here
