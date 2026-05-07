;;; test-mevedel-compact.el --- Tests for mevedel-compact.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'mevedel-compact)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar gptel-model)
(defvar gptel-max-tokens)
(defvar gptel-backend)
(defvar gptel--request-params)

(mevedel-deftest mevedel--file-local-variables-start ()
  ,test
  (test)
  :doc "returns nil when no file-local variables"
  (with-temp-buffer
    (insert "Hello world\n")
    (should (null (mevedel--file-local-variables-start))))

  :doc "detects elisp-style file-local variables"
  (with-temp-buffer
    (insert "Content here\n\n")
    (let ((start (point)))
      (insert ";; Local Variables:\n")
      (insert ";; gptel-model: \"test\"\n")
      (insert ";; End:\n")
      (should (= (mevedel--file-local-variables-start) start))))

  :doc "detects markdown-style file-local variables"
  (with-temp-buffer
    (insert "# Markdown content\n\n")
    (let ((start (point)))
      (insert "<!-- Local Variables: -->\n")
      (insert "<!-- gptel-model: \"test\" -->\n")
      (insert "<!-- End: -->\n")
      (should (= (mevedel--file-local-variables-start) start))))

  :doc "finds first Local Variables block when multiple exist"
  (with-temp-buffer
    (insert "Content here\n\n")
    (let ((first-start (point)))
      (insert ";; Local Variables:\n")
      (insert ";; gptel-model: \"test\"\n")
      (insert ";; End:\n\n")
      (insert ";; Local Variables:\n")
      (insert ";; gptel-model: \"test2\"\n")
      (insert ";; End:\n")
      (should (= (mevedel--file-local-variables-start) first-start)))))

(mevedel-deftest mevedel--estimate-tokens ()
  ,test
  (test)
  :doc "counts tokens without file-local variables"
  (with-temp-buffer
    (insert "Hello world")  ; 11 chars / 4 = 2 tokens
    (should (= (mevedel--estimate-tokens) 2)))

  :doc "excludes elisp-style file-local variables from count"
  (with-temp-buffer
    (insert "Hello world\n\n")  ; 13 chars / 4 = 3 tokens
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"claude-sonnet-4\"\n")
    (insert ";; End:\n")
    (should (= (mevedel--estimate-tokens) 3)))

  :doc "excludes markdown-style file-local variables from count"
  (with-temp-buffer
    (insert "# Title\n\nContent\n\n")  ; 18 chars / 4 = 4 tokens
    (insert "<!-- Local Variables: -->\n")
    (insert "<!-- gptel-model: \"test\" -->\n")
    (insert "<!-- End: -->\n")
    (should (= (mevedel--estimate-tokens) 4)))

  :doc "respects gptel ignore property"
  (with-temp-buffer
    (insert "Hello ")
    (let ((start (point)))
      (insert "ignored ")
      (put-text-property start (point) 'gptel 'ignore))
    (insert "world")
    (should (= (mevedel--estimate-tokens) 2)))  ; "Hello world" = 11 chars / 4 = 2 tokens

  :doc "combines gptel ignore property and file-local variables exclusion"
  (with-temp-buffer
    (insert "Hello ")
    (let ((start (point)))
      (insert "ignored ")
      (put-text-property start (point) 'gptel 'ignore))
    (insert "world\n\n")
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"test\"\n")
    (insert ";; End:\n")
    (should (= (mevedel--estimate-tokens) 3)))  ; "Hello world\n\n" = 13 chars / 4 = 3 tokens

  :doc "excludes all file-local variables when multiple blocks exist"
  (with-temp-buffer
    (insert "Hello world\n\n")  ; 13 chars / 4 = 3 tokens
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"test\"\n")
    (insert ";; End:\n\n")
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"test2\"\n")
    (insert ";; End:\n")
    (should (= (mevedel--estimate-tokens) 3))))

(mevedel-deftest mevedel--compact-record-token-baseline ()
  ,test
  (test)
  :doc "records gptel-reported tokens as estimate baseline"
  (with-temp-buffer
    (insert "abcd")
    (let* ((chat-buffer (current-buffer))
           (fsm (gptel-make-fsm
                 :info (list :buffer chat-buffer
                             :tokens-full '(:input 10
                                            :cached 5
                                            :output 7)))))
      (mevedel--compact-record-token-baseline fsm)
      (should (= (plist-get mevedel--known-token-baseline :tokens) 22))
      (should (= (plist-get mevedel--known-token-baseline :input-tokens) 15))
      (should (= (plist-get mevedel--known-token-baseline :output-tokens) 7))
      (goto-char (point-max))
      (insert "abcdefgh")
      (should (= (mevedel--estimate-tokens) 24))))

  :doc "ignores compaction request token usage"
  (with-temp-buffer
    (insert "abcd")
    (let* ((chat-buffer (current-buffer))
           (fsm (gptel-make-fsm
                 :info (list :buffer chat-buffer
                             :context '(:mevedel-compaction t)
                             :tokens-full '(:input 10 :output 7)))))
      (mevedel--compact-record-token-baseline fsm)
      (should (null mevedel--known-token-baseline)))))

(mevedel-deftest mevedel--compact-transform-auto ()
  ,test
  (test)
  :doc "successful auto-compaction preserves later prompt-buffer transforms"
  (let ((source-buf (generate-new-buffer " *mevedel-compact-source*"))
        (prompt-buf (generate-new-buffer " *mevedel-compact-prompt*"))
        (continued nil))
    (unwind-protect
        (progn
          (with-current-buffer source-buf
            (org-mode)
            (setq-local mevedel--compaction-in-flight nil)
            (setq-local mevedel--view-buffer nil)
            (insert "Old prompt\n")
            (insert (propertize "Old response\n" 'gptel 'response))
            (insert "Pending prompt\n"))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert-buffer-substring source-buf))
          (let ((fsm (gptel-make-fsm
                      :info (list :buffer source-buf))))
            (cl-letf (((symbol-function 'mevedel--compact-should-compact-p)
                       (lambda (&optional _token-estimate) t))
                      ((symbol-function 'mevedel--compact-run)
                       (lambda (&rest args)
                         (with-current-buffer prompt-buf
                           (goto-char (point-min))
                           (insert "Late prefix\n")
                           (when-let* ((start (mevedel--compact-find-boundary)))
                             (goto-char start)
                             (insert "Late context\n\n")))
                         (with-current-buffer source-buf
                           (let ((inhibit-read-only t))
                             (erase-buffer)
                             (insert "#+begin_summary\nSummary\n#+end_summary\n")
                             (insert "Tail prompt\n")
                             (insert (propertize "Tail response\n"
                                                 'gptel 'response))
                             (insert "Pending prompt\n")))
                         (funcall (plist-get args :callback) nil))))
              (with-current-buffer prompt-buf
                (let ((gptel-backend nil)
                      (gptel-model nil)
                      (gptel-max-tokens nil)
                      (gptel--request-params nil))
                  (mevedel--compact-transform-auto
                   (lambda () (setq continued t))
                   fsm)))))
          (with-current-buffer prompt-buf
            (let ((text (buffer-string)))
              (should continued)
              (should (string-match-p "Summary" text))
              (should (string-match-p "Late prefix" text))
              (should (string-match-p "Late context" text))
              (should-not (string-match-p "Old prompt" text))
              (should (string-match-p "Pending prompt" text)))))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf)))))

(mevedel-deftest mevedel--compact-transform-auto-threshold ()
  ,test
  (test)
  :doc "auto threshold uses transformed prompt buffer size"
  (let ((source-buf (generate-new-buffer " *mevedel-compact-source*"))
        (prompt-buf (generate-new-buffer " *mevedel-compact-prompt*"))
        (ran nil)
        (continued nil))
    (unwind-protect
        (progn
          (with-current-buffer source-buf
            (org-mode)
            (setq-local mevedel--compaction-in-flight nil)
            (insert "Old prompt\n")
            (insert (propertize "Old response\n" 'gptel 'response))
            (insert "Small pending\n"))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert-buffer-substring source-buf)
            (insert (make-string 400 ?x)))
          (let ((fsm (gptel-make-fsm
                      :info (list :buffer source-buf))))
            (cl-letf (((symbol-function 'mevedel--compact-auto-eligible-p)
                       (lambda () t))
                      ((symbol-function 'mevedel--compact-run)
                       (lambda (&rest args)
                         (setq ran t)
                         (funcall (plist-get args :callback) :skip))))
              (with-current-buffer prompt-buf
                (let ((mevedel-compact-token-threshold 50)
                      (gptel-backend nil)
                      (gptel-model nil)
                      (gptel-max-tokens nil)
                      (gptel--request-params nil))
                  (mevedel--compact-transform-auto
                   (lambda () (setq continued t))
                   fsm)))))
          (should ran)
          (should continued))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf)))))

(mevedel-deftest mevedel--compact-context-limit ()
  ,test
  (test)
  :doc "prefers explicit override"
  (let ((mevedel-compact-context-limit 12345)
        (gptel-model 'mevedel-test-model))
    (put 'mevedel-test-model :context-window 200)
    (should (= (mevedel--compact-context-limit) 12345)))

  :doc "uses gptel model context-window in thousands of tokens"
  (let ((mevedel-compact-context-limit nil)
        (gptel-model 'mevedel-test-model))
    (put 'mevedel-test-model :context-window 8.192)
    (should (= (mevedel--compact-context-limit) 8192)))

  :doc "falls back when model has no context-window"
  (let ((mevedel-compact-context-limit nil)
        (gptel-model 'mevedel-test-model-no-context))
    (put 'mevedel-test-model-no-context :context-window nil)
    (should (= (mevedel--compact-context-limit) 200000))))

(mevedel-deftest mevedel--compact-current-persisted-p ()
  ,test
  (test)
  :doc "requires current buffer to be the session's active segment"
  (let* ((tempdir (make-temp-file "mevedel-compact-persisted-" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "compact-persisted" tempdir "compact-persisted"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) tempdir)
          (setf (mevedel-session-current-segment session) 2)
          (with-temp-buffer
            (setq buffer-file-name
                  (mevedel-session-persistence--segment-path tempdir 2))
            (setq-local mevedel--session session)
            (should (mevedel--compact-current-persisted-p)))
          (with-temp-buffer
            (setq buffer-file-name
                  (mevedel-session-persistence--segment-path tempdir 1))
            (setq-local mevedel--session session)
            (should-not (mevedel--compact-current-persisted-p))))
      (mevedel-workspace-clear-registry)
      (delete-directory tempdir t))))

(mevedel-deftest mevedel--compact-threshold-tokens ()
  ,test
  (test)
  :doc "integer threshold is absolute"
  (let ((mevedel-compact-token-threshold 150000))
    (should (= (mevedel--compact-threshold-tokens) 150000)))

  :doc "float threshold applies to usable context"
  (let ((mevedel-compact-context-limit 200000)
        (mevedel-compact-reserve-tokens 20000)
        (mevedel-compact-token-threshold 0.8)
        (gptel-max-tokens nil))
    (should (= (mevedel--compact-threshold-tokens) 144000)))

  :doc "reserve is capped on small context windows"
  (let ((mevedel-compact-context-limit 8000)
        (mevedel-compact-reserve-tokens 20000)
        (mevedel-compact-token-threshold 0.8)
        (gptel-max-tokens nil))
    (should (= (mevedel--compact-usable-tokens) 4000))
    (should (= (mevedel--compact-threshold-tokens) 3200))))

(mevedel-deftest mevedel--compact-tail-start ()
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
      (let ((mevedel-compact-context-limit 200000)
            (mevedel-compact-tail-turns 2)
            (mevedel-compact-tail-budget 0.25))
        (should (= (mevedel--compact-tail-start (point-max) nil)
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
      (let ((mevedel-compact-context-limit 100)
            (mevedel-compact-reserve-tokens 20)
            (mevedel-compact-tail-turns 2)
            (mevedel-compact-tail-budget 0.01))
        (should (= (mevedel--compact-tail-start (point-max) nil)
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
	  (let ((mevedel-compact-context-limit 100)
	        (mevedel-compact-reserve-tokens 20)
	        (mevedel-compact-tail-turns 2)
	        (mevedel-compact-tail-budget 0.01))
	    (should (= (mevedel--compact-tail-start (point-max) nil)
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
      (let ((mevedel-compact-context-limit 200000)
            (mevedel-compact-tail-turns 2)
            (mevedel-compact-tail-budget 0.25))
        (should (= (mevedel--compact-tail-start (point-max) nil)
                   u2-start))))))

(mevedel-deftest mevedel--compact-pending-text-from-prompt-buffer ()
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
    (let ((text (mevedel--compact-pending-text-from-prompt-buffer)))
      (should (string-prefix-p "<system-reminder>" text))
      (should (string-match-p "expanded reminder" text))
      (should (string-match-p "new user prompt" text)))))

(mevedel-deftest mevedel--compact-region-with-tool-output-cap ()
  ,test
  (test)
  :doc "caps tool output spans while preserving surrounding text"
  (with-temp-buffer
    (insert "before\n")
    (let ((tool-start (point)))
      (insert "abcdef")
      (put-text-property tool-start (point) 'gptel '(tool . "call-1")))
    (insert "\nafter\n")
    (let ((text (mevedel--compact-region-with-tool-output-cap
                 (point-min) (point-max) 3 t)))
      (should (string-match-p "before" text))
      (should (string-match-p "abc" text))
      (should-not (string-match-p "def" text))
      (should (string-match-p "omitted 3 chars" text))
      (should (string-match-p "after" text)))))

(mevedel-deftest mevedel--compact-prompt ()
  ,test
  (test)
  :doc "creates anchored summary prompt with required sections"
  (let ((prompt (mevedel--compact-prompt nil nil nil)))
    (should (string-match-p "Create a new anchored summary" prompt))
    (should (string-match-p "## Skills Invoked" prompt))
    (should (string-match-p "- (none)" prompt))
    (should (string-match-p "Do NOT call any tools" prompt)))

  :doc "updates with previous summary and manual instructions"
  (let ((prompt (mevedel--compact-prompt "old summary" "focus tests" nil)))
    (should (string-match-p "Update the anchored summary" prompt))
    (should (string-match-p "previous summary is authoritative retained context" prompt))
    (should (string-match-p "Do NOT replace it with only the recent conversation" prompt))
    (should (string-match-p "Do not discard previous-summary details" prompt))
    (should (string-match-p "<previous-summary>" prompt))
    (should (string-match-p "old summary" prompt))
    (should (string-match-p "## Additional Instructions" prompt))
    (should (string-match-p "focus tests" prompt))))

(mevedel-deftest mevedel--compact-skills-section ()
  ,test
  (test)
  :doc "returns none marker when no session"
  (should (equal (mevedel--compact-skills-section nil) "- (none)"))

  :doc "returns none marker when session has no invoked-skills records"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "c1" :root "/tmp/c1" :name "c1"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (should (equal (mevedel--compact-skills-section session) "- (none)")))

  :doc "lists invoked skills with name, args, trigger, turn"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "c2" :root "/tmp/c2" :name "c2"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (rec1 (mevedel-skill-invocation-record--create
                :name "grill-me" :args "spec 22"
                :trigger 'user-slash :turn 3
                :source-path "/skills/grill-me/SKILL.md"
                :prepared-body "Body 1"))
         (rec2 (mevedel-skill-invocation-record--create
                :name "review-spec" :args nil
                :trigger 'model-skill :turn 7
                :source-path "/skills/review-spec/SKILL.md"
                :prepared-body "Body 2")))
    (setf (mevedel-session-invoked-skills session) (list rec1 rec2))
    (let ((section (mevedel--compact-skills-section session)))
      (should (string-match-p "/grill-me spec 22" section))
      (should (string-match-p "user-slash" section))
      (should (string-match-p "turn: 3" section))
      (should (string-match-p "/review-spec" section))
      (should (string-match-p "model-skill" section)))))

(provide 'test-mevedel-compact)
;;; test-mevedel-compact.el ends here
