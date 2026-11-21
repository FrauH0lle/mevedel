;;; test-niwa-view-render-audits.el --- Activity audit projection -*- lexical-binding: t -*-

;;; Commentary:

;; Standalone audits between deliveries and tools keep their own semantics.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-render)
(require 'niwa-view-composer)
(require 'niwa-view-disclosure)
(require 'niwa-view-audit)
(require 'niwa-view-agent)
(require 'niwa-view-segments)
(require 'niwa-view-stream)
(require 'niwa-transcript)
(require 'niwa-session-artifacts)
(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name)) "niwa-session-test-support"))
(require 'niwa-tool-registry)
(require 'niwa-tool-exec)
(require 'niwa-tool-ptc)
(require 'niwa-execution-transcript)

(defun niwa-view-audit-test--publish (session &rest paths)
  "Publish explicit archive PATHS for read-only audit lookup in SESSION."
  (niwa-session-publication-publish
   session (append (mapcar (lambda (path)
                             (list :path path :content
                                   (niwa-session-artifacts-read-file-raw path))) paths)
                   '((:path "session.meta.el" :content "(:fixture archive-only)" :commit-marker t))) t))

(niwa-deftest niwa-view--insert-hook-audit-block/display-insets ()
  ,test
  (test)
  :doc "prompt and grouped-context audit insets survive toggles and canonical redraws"
  (let ((long-line (concat "  Authored payload " (make-string 160 ?x))))
    (dolist
        (spec
         (list
          (list
           (list :type 'prompt-rewrite :event "UserPromptSubmit"
                 :reason "Rewrite explanation\ncontinued explanation"
                 :original "  Original prompt\n    Authored continuation"
                 :submitted long-line)
           "  ◇ hook changed prompt\n"
           (list '(4 . "Event: UserPromptSubmit")
                 '(4 . "Reason: Rewrite explanation")
                 '(4 . "continued explanation")
                 '(4 . "Original prompt:")
                 '(6 . "  Original prompt")
                 '(6 . "    Authored continuation")
                 '(4 . "Submitted prompt:")
                 (cons 6 long-line)))
          (list
           (list :type 'tool-context :event "PreToolUse"
                 :handlers
                 (list (list :source 'project-file
                             :description "Project handler\ncontinued handler"
                             :reason "Context explanation"
                             :contexts
                             (list (concat long-line "\n    Authored context")))))
           "  ◇ PreToolUse hook added context · 1 handler\n"
           (list '(4 . "1. project hook")
                 '(7 . "Handler: Project handler")
                 '(7 . "continued handler")
                 '(7 . "Reason: Context explanation")
                 '(7 . "Context:")
                 (cons 9 long-line)
                 '(9 . "    Authored context")))))
      (niwa-view-test--with-buffers
        (niwa-view-test--insert-data data-buf "Continue\n" nil)
        (niwa-view-test--insert-data
         data-buf (niwa--format-hook-audit-record (car spec))
         'niwa-hook-audit)
        (niwa-view-test--insert-data data-buf "Continuing.\n" 'response)
        (with-current-buffer view-buf
          (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
          (niwa-view--full-rerender)
          (save-excursion
            (goto-char (point-min))
            (search-forward (cadr spec))
            (goto-char (match-beginning 0))
            (niwa-view-toggle-section))
          (dotimes (phase 3)
            (when (> phase 0)
              (niwa-view--full-rerender))
            (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
            (should (= (+ (niwa-view-composer-draft-start) 4)
                       (with-current-buffer (niwa-view-composer-buffer) (point))))
            (save-excursion
              (goto-char (point-min))
              (search-forward (cadr spec))
              (goto-char (match-beginning 0))
              (should (= 1 (niwa-view-test--count-substring
                            (cadr spec) (buffer-string))))
              (should-not (get-text-property (point) 'niwa-view-collapsed))
              (should-not (get-text-property (point) 'line-prefix))
              (should-not (get-text-property (point) 'wrap-prefix))
              (let ((end (cdr (niwa-view-disclosure-section-bounds))))
                (forward-line 1)
                (let* ((body-start (point))
                       (rows (caddr spec))
                       (expected (concat (mapconcat #'cdr rows "\n") "\n")))
                  ;; Copying removes no authored indentation and adds no padding.
                  (should (equal expected
                                 (substring-no-properties
                                  (niwa-view--buffer-substring-filter
                                   body-start end))))
                  (dolist (row rows)
                    (let ((inset (make-string (car row) ?\s)))
                      (should (equal (cdr row)
                                     (buffer-substring-no-properties
                                      (line-beginning-position) (line-end-position))))
                      (should (equal inset (get-text-property (point) 'line-prefix)))
                      (should (equal inset (get-text-property (point) 'wrap-prefix)))
                      (should (eq 'niwa-view-hook-audit
                                  (get-text-property (point) 'font-lock-face))))
                    (forward-line 1)))))))))))

(niwa-deftest niwa-view-audit--history-render-data-owner ()
  ,test
  (test)
  :doc "history lookup finds the outer owner beyond quoted nested closing delimiters"
  (with-temp-buffer
    (insert "prefix\n")
    (insert (niwa-tool-render-data-format
             (list :kind 'bash :execution-output
                   (concat niwa-tool-render-data-close "\n"
                           (niwa-tool-render-data-format '(:kind nested) "fake")
                           "find-execution-here")) "actual"))
    (insert "suffix")
    (goto-char (point-min))
    (search-forward "find-execution-here")
    (let ((pos (point)))
      (should (equal "actual" (niwa-view-audit--history-render-data-owner pos)))
      (should (= pos (point))))
    (should-not (niwa-view-audit--history-render-data-owner (point-max)))))

(niwa-deftest niwa-view-audit--breadcrumb-label ()
  ,test
  (test)
  :doc "terminal facts determine one compact breadcrumb without output"
  (should (equal "  ● Background command \"./run-tests\" completed  "
                 (niwa-view-audit--breadcrumb-label
                  '(:command "./run-tests" :facts (:outcome success)))))
  (should (equal "  ● Background command \"./run-tests\" failed (exit code 1)  "
                 (niwa-view-audit--breadcrumb-label
                  '(:command "./run-tests"
                    :facts (:outcome failure :exit-code 1)))))
  (should (string-match-p "\"sleep\" stopped"
                          (niwa-view-audit--breadcrumb-label
                           '(:command "sleep" :facts (:termination owner-stopped)))))
  (let ((label (niwa-view-audit--breadcrumb-label
                '(:command "sleep" :facts (:termination stopped
                                          :exit-code 15 :outcome failure)))))
    (should-not (string-match-p "exit 15" label))
    (should-not (string-match-p "signal 15" label)))
  (let ((label (niwa-view-audit--breadcrumb-label
                '(:command "sleep" :facts (:termination signaled
                                          :exit-code 13 :outcome failure)))))
    (should (string-match-p "\"sleep\" failed (signal 13)" label))
    (should (eq 'error (get-text-property 2 'font-lock-face label)))
    (should-not (string-match-p "exit 13" label)))
  (let ((label (niwa-view-audit--breadcrumb-label
                '(:command "sleep" :facts (:termination interrupted
                                          :exit-code 2 :outcome failure)))))
    (should (string-match-p "\"sleep\" stopped (signal 2)" label))
    (should-not (string-match-p "exit 2" label)))
  (should-not (string-match-p "\n"
                              (niwa-view-audit--breadcrumb-label
                               '(:command "printf first\nsecond"
                                 :facts (:outcome success)))))
  :doc "a completion without an outcome reads ended, not failed or completed"
  (should (equal "Ended" (niwa-view-audit-breadcrumb-status
                          '(:facts (:kind monitor :termination exited)))))
  (should (equal "  ● Monitor \"ticker\" ended · 2 events  "
                 (niwa-view-audit--breadcrumb-label
                  '(:facts (:kind monitor :description "ticker"
                            :termination exited :event-count 2)))))
  (let ((label (niwa-view-audit--breadcrumb-label
                '(:command "sleep" :facts (:termination lost)))))
    (should (string-match-p "\"sleep\" lost" label))
    (should (eq 'niwa-view-tool-metadata (get-text-property 2 'font-lock-face label))))
  (should (equal '("Stopped" "Timed out" "Lost" "Completed")
                 (mapcar (lambda (facts)
                           (niwa-view-audit-breadcrumb-status
                            (list :facts facts)))
                         '((:termination stopped) (:termination timed-out)
                           (:termination lost) (:outcome no-match)))))
  :doc "monitor completion reports event counts rather than an exit code"
  (dolist (count '(0 1 2))
    (let ((label (niwa-view-audit--breadcrumb-label
                  (list :command "watch" :facts
                        (list :kind 'monitor :description "ticker"
                              :termination 'stopped :outcome 'failure
                              :exit-code 15 :event-count count)))))
      (should (equal (format "  ● Monitor \"ticker\" stopped · %d event%s  "
                             count (if (= count 1) "" "s"))
                     label))
      (should (eq 'niwa-view-tool-metadata
                  (get-text-property 2 'font-lock-face label)))))
  :doc "forwarded child work names its agent except in that agent's own view"
  (should (string-match-p
           "Background command \"make test\" completed · /root/worker  "
           (niwa-view-audit--breadcrumb-label
            '(:owner "/root/worker" :command "make test"
              :facts (:outcome success)))))
  (let ((niwa-view--agent-path "/root/worker"))
    (should-not (string-match-p
                 "/root/worker"
                 (niwa-view-audit--breadcrumb-label
                  '(:owner "/root/worker" :command "make test"
                    :facts (:outcome success)))))))

(niwa-deftest niwa-view-audit-mailbox-breadcrumb ()
  ,test
  (test)
  :doc "forwarded execution uses opener facts and retains body as opaque evidence"
  (let* ((record (niwa-view-audit-mailbox-breadcrumb
                  "output <bash-execution execution_id=\"fake\" outcome=\"success\"/>"
                  "/root/worker"
                  (concat "<agent-message type=\"EXECUTION\" execution_id=\"real\" "
                          "command=\"make &gt; test\" description=\"Run tests\" "
                          "output_path=\"artifact://result\" "
                          "outcome=\"failure\" exit_code=\"1\" "
                          "output_bytes=\"128\" omitted_output_bytes=\"64\">"))))
    (should (equal "real" (plist-get record :execution-id)))
    (should (equal "make > test" (plist-get record :command)))
    (should (equal 'failure (plist-get (plist-get record :facts) :outcome)))
    (should (string-match-p "Background command \"Run tests\" failed"
                            (niwa-view-audit--breadcrumb-label record)))
    (should (equal "artifact://result" (plist-get (plist-get record :facts)
                                                    :output-path)))
    (should (equal "/root/worker" (plist-get record :owner)))
    (should (equal 1 (plist-get (plist-get record :facts) :exit-code)))
    (should (equal 128 (plist-get (plist-get record :facts) :output-bytes)))
    (should (equal 64 (plist-get (plist-get record :facts) :omitted-output-bytes)))
    (should (string-match-p "output" (plist-get (plist-get record :facts)
                                                 :execution-output))))
  :doc "mailbox-only omitted bytes warn when the artifact is unavailable"
  (let ((record (niwa-view-audit-mailbox-breadcrumb
                 "bounded output"
                 "/root/worker"
                 (concat "<agent-message type=\"EXECUTION\" execution_id=\"mailbox\" "
                         "output_path=\"artifact://missing\" "
                         "omitted_output_bytes=\"20\">"))))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
          (let ((result (niwa-view-audit--evidence record)))
            (with-current-buffer result
              (should (string-match-p "Output truncated:.*retained preview"
                                      (buffer-string)))
              (should (string-match-p "bounded output" (buffer-string))))))
      (when-let* ((buffer (get-buffer "*niwa execution result*")))
        (kill-buffer buffer)))))

(niwa-deftest niwa-view-audit-mailbox-completion ()
  ,test
  (test)
  :doc "structural opener supplies identity and body metadata stays opaque"
  (let* ((body (concat "quoted output\n"
                       "<bash-execution execution_id=\"spoof\" outcome=\"success\"/>"))
         (record (niwa-view-audit-mailbox-completion
                  (concat "<agent-message type=\"EXECUTION\" sender=\"/root/child\" "
                          "execution_id=\"real\" command=\"printf &gt; file\" "
                          "description=\"Write file\" termination=\"stopped\" "
                          "outcome=\"failure\" exit_code=\"15\">\n"
                          body "\n</agent-message>\n"))))
    (should (equal "real" (plist-get record :execution-id)))
    (should (equal "/root/child" (plist-get record :owner)))
    (should (equal "printf > file" (plist-get record :command)))
    (should (equal body (string-trim (plist-get record :delivery-body))))
    (should (equal (plist-get record :delivery-body)
                   (plist-get (plist-get record :facts) :execution-output)))
    (should (equal 'stopped (plist-get (plist-get record :facts) :termination))))
  :doc "a body cannot supply missing opener identity or change ordinary mail's type"
  (dolist (open '("<agent-message type=\"EXECUTION\" sender=\"/root\">"
                  "<agent-message type=\"MAIL\" sender=\"/root\" execution_id=\"spoof\">"))
    (should-not
     (niwa-view-audit-mailbox-completion
      (concat open "\n<bash-execution execution_id=\"spoof\"/>\n</agent-message>"))))
  :doc "malformed openers or incomplete mailbox blocks are not execution evidence"
  (should-not (niwa-view-audit-mailbox-breadcrumb "output" "/root" "not XML"))
  (should-not (niwa-view-audit-mailbox-completion
               "<agent-message type=\"EXECUTION\" execution_id=\"exec\">unfinished")))

(niwa-deftest niwa-view-audit--insert-breadcrumb ()
  ,test
  (test)
  :doc "durable audit draws a linked line and does not surface archives"
  (let* ((record '(:type execution-breadcrumb :execution-id "exec-1"
                   :tool-use-id "tool-1" :command "./test"
                   :facts (:outcome success)))
         (text (concat (niwa--format-hook-audit-record
                        '(:type execution-completion :tool-use-id "old"))
                       (niwa--format-hook-audit-record record))))
    (with-temp-buffer
      (should (= 1 (length (niwa-view--hook-audit-records-from-text text))))
      (niwa-view--insert-hook-audit-block record)
      (should (equal 'execution-breadcrumb
                     (get-text-property (point-min) 'niwa-view-type)))
      (goto-char (point-min))
      (search-forward "[Show result]")
      (should (functionp (get-text-property (match-beginning 0)
                                            'niwa-view-zone-activate))))))

(niwa-deftest niwa-view-audit-mailbox-completion/producer-metadata ()
  ,test
  (test)
  :doc "production mailbox delivery preserves execution and output artifact facts"
  (require 'niwa-tools)
  (let* ((record
          (niwa-view-audit-mailbox-completion
           (niwa-tools--message-delivery-block
            '(:type EXECUTION :sender "/root/worker" :recipient "/root"
              :payload "bounded output"
              :meta (:execution-id "exec-artifact"
                     :description "Build & test" :command "make > build.log"
                     :outcome failure :termination exited :exit-code 2
                     :output-path "artifact://execution/build-output.txt"
                     :output-bytes 1024 :omitted-output-bytes 768)))))
         (facts (plist-get record :facts)))
    (should (equal "exec-artifact" (plist-get record :execution-id)))
    (should (equal "/root/worker" (plist-get record :owner)))
    (should (equal "make > build.log" (plist-get record :command)))
    (should (equal "Build & test" (plist-get facts :description)))
    (should (eq 'failure (plist-get facts :outcome)))
    (should (eq 'exited (plist-get facts :termination)))
    (should (equal 2 (plist-get facts :exit-code)))
    (should (equal "artifact://execution/build-output.txt"
                   (plist-get facts :output-path)))
    (should (equal 1024 (plist-get facts :output-bytes)))
    (should (equal 768 (plist-get facts :omitted-output-bytes)))
    (should (equal "bounded output"
                   (string-trim (plist-get facts :execution-output))))))

(niwa-deftest niwa-view--decorate-agent-message-blocks ()
  ,test
  (test)
  :doc "execution mail becomes an expandable notice, not an origin breadcrumb"
  (with-temp-buffer
    (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\" "
            "execution_id=\"exec-1\" outcome=\"failure\" exit_code=\"2\">\n"
            "test output\n"
            "<bash-execution execution_id=\"exec-1\" outcome=\"error\" "
            "exit_code=\"2\"/>\n</agent-message>\n")
    (niwa-view--decorate-agent-message-blocks (point-min) (point-max))
    (should (string-match-p "Background command.*failed (exit code 2)"
                            (buffer-string)))
    (goto-char (point-min))
    (search-forward "test output")
    (should (get-text-property (match-beginning 0) 'invisible))
    (should (get-text-property (match-beginning 0) 'niwa-view-collapsed))
    (should (get-text-property (point-min) 'niwa-view-mailbox-notice))
    (should-not (text-property-not-all (point-min) (point-max)
                                      'niwa-view-execution-breadcrumb nil))))

(niwa-deftest niwa-view--own-completion-mail-p ()
  ,test
  (test)
  :doc "own terminal mail disappears in root and agent transcripts"
  (dolist (owner '(nil "/root/worker"))
    (dolist (type '("EXECUTION" "MONITOR_TERMINAL"))
      (with-temp-buffer
        (setq-local niwa-view--agent-path owner)
        (let* ((sender (or owner "/root"))
               (opener (format "<agent-message type=\"%s\" sender=\"%s\">"
                               type sender)))
          (should (niwa-view--own-completion-mail-p opener sender))
          (insert "before\n" opener "\nterminal output\n</agent-message>\nafter\n")
          (niwa-view--decorate-agent-message-blocks (point-min) (point-max))
          (should (equal "before\nafter\n" (buffer-string)))))))
  :doc "Monitor events and agent results from the owner remain visible"
  (dolist (owner '(nil "/root/worker"))
    (dolist (case '(("agent-message" "MONITOR" "Monitor event: tick")
                    ("agent-result" "EXECUTION" "Agent %s finished")))
      (with-temp-buffer
        (setq-local niwa-view--agent-path owner)
        (let ((sender (or owner "/root")))
          (insert (format "<%s type=\"%s\" sender=\"%s\" outcome=\"completed\">\ntick\n</%s>\n"
                          (car case) (cadr case) sender (car case)))
          (niwa-view--decorate-agent-message-blocks (point-min) (point-max))
          (niwa-view--decorate-agent-result-blocks (point-min) (point-max))
          (should (string-match-p (regexp-quote (format (caddr case) sender))
                                  (buffer-string)))
          (should (get-text-property (point-min) 'niwa-view-mailbox-notice)))))))

(niwa-deftest niwa-view--decorate-agent-message-blocks/point-after ()
  ,test
  (test)
  :doc "point stays after the notice so callers stamp and append past it"
  (with-temp-buffer
    (insert "<agent-message type=\"EXECUTION\" sender=\"/root/worker\">\n"
            "<bash-execution execution_id=\"exec-1\" outcome=\"error\" "
            "exit_code=\"2\"/>\n</agent-message>\n")
    (niwa-view--decorate-agent-message-blocks (point-min) (point))
    (should (= (point) (point-max)))
    (should (get-text-property (1- (point))
                               'niwa-view-mailbox-notice))))

(niwa-deftest niwa-view--render-tool-call/preview-truncation ()
  ,test
  (test)
  :doc "the collapsed original Bash row warns when whole output exceeds its preview"
  (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry)))
    (niwa-tool-register
     (niwa-tool--create :name "Bash" :category "niwa"
                           :renderer #'niwa-tool-exec--render-bash))
    (with-temp-buffer
      (let* ((data (niwa-execution-transcript-terminal-render-data
                    '(:facts (:state completed :outcome success
                              :omitted-output-bytes 0)
                      :whole-output "head\n[niwa: tool output truncated; omitted 1000 chars]\ntail"
                      :whole-output-truncated-p t)))
             (row (niwa-view--render-tool-call
                   (list :name "Bash" :args '(:command "produce-output")
                         :result "" :render-data data)
                   (current-buffer))))
        (should (plist-get row :initially-collapsed-p))
        (should (string-match-p "output truncated"
                                (niwa-view--rendering-header-block row)))
        (should (string-match-p "omitted 1000 chars" (plist-get row :body)))))))

(niwa-deftest niwa-view--render-tool-call/direct-terminal ()
  ,test
  (test)
  :doc "a running Bash row retains its material sandbox summary on progress"
  (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry)))
    (niwa-tool-register
     (niwa-tool--create :name "Bash" :category "niwa"
                           :renderer #'niwa-tool-exec--render-bash))
    (with-temp-buffer
      (setq-local niwa-view--execution-events (make-hash-table :test 'equal))
      (puthash "direct-progress"
               '(:type progress :facts (:state running :wall-time-seconds 2)
                 :output-tail "1\n2\n3\n4\n5\n6\n7")
               niwa-view--execution-events)
      (let ((row (niwa-view--render-tool-call
                  '(:name "Bash" :tool-use-id "direct-progress"
                    :args (:command "seq 7; sleep 30") :result "initial"
                    :render-data
                    (:status success :state running
                     :sandbox-summary
                     (:attempt-count 1 :started-count 1 :sandbox unavailable
                      :filesystem unrestricted :network unrestricted)))
                  (current-buffer))))
        (should (equal 'unavailable
                       (plist-get (plist-get row :sandbox-summary) :sandbox)))
        (should (string-match-p "1\n2\n3\n4\n5\n6\n7"
                                (plist-get row :body)))
        (should-not (string-match-p "output truncated" (plist-get row :header)))
        (should (string-match-p "Sandbox:.*without confinement"
                                (niwa-view--rendering-header-block row))))))
  :doc "direct ToolCall Bash reconciles retained failure and output with its child ID"
  (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry)))
    (niwa-tool-register
     (niwa-tool--create :name "Bash" :category "niwa"
                           :renderer #'niwa-tool-exec--render-bash))
    (niwa-tool-register
     (niwa-tool--create :name "ToolCall" :category "niwa"
                           :renderer #'niwa-tool-ptc--render))
    (with-temp-buffer
      (let* ((data (current-buffer))
             (call '(:name "ToolCall" :tool-use-id "outer"
                     :result "INITIAL OUTPUT"
                     :render-data
                     (:kind ptc :direct-tool "Bash" :outcome completed
                      :calls ((:id "outer/1" :tool "Bash" :status success
                               :args (:command "make test")
                               :render-data (:execution-id "exec-1"
                                             :state running))))))
             (before (niwa-view--render-tool-call call data)))
        (should (string-match-p "running" (plist-get before :header)))
        (niwa-execution-transcript-handle-event
         (list :type 'terminal :data-buffer data :tool-use-id "outer/1"
               :owner "/root"
               :whole-output "FINAL FAILURE\n<bash-execution marker=\"USER_OUTPUT\"/>\n"
               :facts '(:execution-id "exec-1" :command "make test"
                        :state completed :outcome failure :exit-code 2)))
        (let ((after (niwa-view--render-tool-call call data)))
          (should (string-match-p "failed" (plist-get after :header)))
          (should-not (string-match-p "running" (plist-get after :header)))
          (should (string-match-p "FINAL FAILURE" (plist-get after :body)))
          (should (string-match-p "<bash-execution marker=\"USER_OUTPUT\"/>"
                                  (plist-get after :body)))
          (should-not (string-match-p "INITIAL OUTPUT" (plist-get after :body)))
          (should (equal "exec-1"
                         (plist-get
                          (plist-get (car (plist-get
                                           (plist-get (plist-get after :group-child)
                                                      :render-data)
                                           :calls)) :render-data)
                          :execution-id)))))))
  :doc "a terminal launch failure stays visible in direct and nested Bash rows"
  (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry)))
    (niwa-tool-register
     (niwa-tool--create :name "Bash" :category "niwa"
                           :renderer #'niwa-tool-exec--render-bash))
    (with-temp-buffer
      (let* ((data (current-buffer))
             (niwa--data-buffer data)
             (facts '(:state completed :termination spawn-failed
                      :outcome failure :exit-code -1))
             (observation
              (list :facts facts :output ""
                    :error "spawning child process: no such file: bash"))
             (envelope (niwa-tool-exec--observation-envelope observation)))
        (dolist (tool-id '("direct-fail" "outer/1"))
          (niwa-execution-transcript-handle-event
           (list :type 'terminal :data-buffer data :tool-use-id tool-id
                 :owner "/root" :whole-output "" :observation observation
                 :facts facts)))
        (let* ((direct
                (niwa-view--render-tool-call
                 (list :name "Bash" :tool-use-id "direct-fail"
                       :args '(:command "printf hello")
                       :result (plist-get envelope :result)
                       :render-data (plist-get envelope :render-data)) data))
               (nested
                (niwa-view--child-call-rendering
                 (list :id "outer/1" :tool "Bash" :status 'error
                       :args '(:command "printf hello")
                       :result (plist-get envelope :result)
                       :render-data (plist-get envelope :render-data)))))
          (dolist (row (list direct nested))
            (should (string-match-p "Failed to start process: spawning child process"
                                    (plist-get row :body)))
            (should-not (string-match-p "execution_id=" (plist-get row :body))))))))
  :doc "a direct Bash keeps its terminal sandbox disclosure on the outer row"
  (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry)))
    (niwa-tool-register
     (niwa-tool--create :name "Bash" :category "niwa"
                           :renderer #'niwa-tool-exec--render-bash))
    (niwa-tool-register
     (niwa-tool--create :name "ToolCall" :category "niwa"
                           :renderer #'niwa-tool-ptc--render))
    (with-temp-buffer
      (let* ((data (current-buffer))
             (summary '(:attempt-count 1 :started-count 1 :refused-count 0
                        :sandbox direct :filesystem unrestricted
                        :network unrestricted))
             (call '(:name "ToolCall" :tool-use-id "outer"
                     :result "INITIAL OUTPUT" :render-data
                     (:kind ptc :direct-tool "Bash" :outcome completed
                      :calls ((:id "outer/1" :tool "Bash" :status success
                               :args (:command "make test")
                               :render-data (:execution-id "exec-1"
                                             :state running)))))))
        (niwa-execution-transcript-handle-event
         (list :type 'terminal :data-buffer data :tool-use-id "outer/1"
               :owner "/root" :whole-output "FINAL OUTPUT"
               :facts (list :execution-id "exec-1" :command "make test"
                            :state 'completed :outcome 'success :exit-code 0
                            :sandbox-summary summary)))
        (let ((after (niwa-view--render-tool-call call data)))
          (should (equal summary (plist-get after :sandbox-summary)))
          (should (string-match-p "Sandbox:.*unrestricted"
                                  (niwa-view--rendering-header-block after)))))))
  :doc "a live direct Bash child uses the cached progress tail and elapsed time"
  (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry)))
    (niwa-tool-register
     (niwa-tool--create :name "Bash" :category "niwa"
                           :renderer #'niwa-tool-exec--render-bash))
    (niwa-tool-register
     (niwa-tool--create :name "ToolCall" :category "niwa"
                           :renderer #'niwa-tool-ptc--render))
    (with-temp-buffer
      (let ((data (current-buffer)))
        (with-temp-buffer
          (setq-local niwa-view--execution-events (make-hash-table :test 'equal))
          (puthash "outer/1"
                   '(:type progress :facts (:execution-id "exec-1" :state running
                                            :wall-time-seconds 7)
                     :output-tail "NEW OUTPUT")
                   niwa-view--execution-events)
          (let ((row (niwa-view--render-tool-call
                      '(:name "ToolCall" :tool-use-id "outer"
                        :result "OLD OUTPUT" :render-data
                        (:kind ptc :direct-tool "Bash" :outcome completed
                         :sandbox-summary
                         (:attempt-count 1 :started-count 1 :sandbox unavailable
                          :filesystem unrestricted :network unrestricted)
                         :calls ((:id "outer/1" :tool "Bash" :status success
                                  :args (:command "make test")
                                  :render-data (:state running
                                                :wall-time-seconds 0.25)))))
                      data)))
            (should (string-match-p "running · 7s" (plist-get row :header)))
            (should (string-match-p "NEW OUTPUT" (plist-get row :body)))
            (should (equal 'unavailable
                           (plist-get (plist-get row :sandbox-summary) :sandbox)))
            (should-not (string-match-p "OLD OUTPUT" (plist-get row :body)))))))))

(niwa-deftest niwa-view-audit-breadcrumb-present-p ()
  ,test
  (test)
  :doc "each delivery remains a notice and cannot suppress the origin breadcrumb"
  (with-temp-buffer
    (insert (concat
             "<agent-message type=\"EXECUTION\" sender=\"/root/child\" execution_id=\"one\" outcome=\"success\">\n"
             "<bash-execution execution_id=\"one\" outcome=\"success\"/>\n"
             "</agent-message>\n"
             "<agent-message type=\"EXECUTION\" sender=\"/root/child\" execution_id=\"one\" outcome=\"success\">\n"
             "<bash-execution execution_id=\"one\" outcome=\"success\"/>\n"
             "</agent-message>\n"
             "<agent-message type=\"EXECUTION\" sender=\"/root/child\" execution_id=\"two\" outcome=\"success\">\n"
             "<bash-execution execution_id=\"two\" outcome=\"success\"/>\n"
             "</agent-message>\n"))
    (niwa-view--decorate-agent-message-blocks (point-min) (point-max))
    (should (= 3 (how-many "Background command" (point-min) (point-max))))
    (let ((record '(:type execution-breadcrumb :execution-id "one"
                    :owner "/root/child" :facts (:outcome success))))
      (should-not (niwa-view-audit-breadcrumb-present-p record (point-max)))
      (goto-char (point-max))
      (niwa-view-audit--insert-breadcrumb record nil)
      (should (niwa-view-audit-breadcrumb-present-p record (point-max))))
    (should (= 1 (how-many "\\[Show result\\]" (point-min) (point-max))))
    (should-not (string-match-p "<agent-message" (buffer-string)))))

(niwa-deftest niwa-view-child-call-retained-completion ()
  ,test
  (test)
  :doc "nested ToolCall Bash uses the child completion after reload"
  (with-temp-buffer
    (insert (propertize "(:name \"ToolCall\")" 'gptel '(tool . "outer")))
    (let ((data (current-buffer)) received)
      (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                 (lambda () data))
                ((symbol-function 'niwa-tool-get) (lambda (_name) t))
                ((symbol-function 'niwa-execution-transcript-pending-render-data)
                 (lambda (_buffer id)
                   (setq received id)
                   '(:execution-id "exec-1" :execution-output "final output"
                     :status error)))
                ((symbol-function 'niwa-view--invoke-renderer)
                 (lambda (_tool facts _args result)
                   (list :header (format "%s: %s" (plist-get facts :status)
                                         result)))))
        (let ((rendering (niwa-view--child-call-rendering
                          '(:id "outer/1" :tool "Bash" :status running
                            :result "stale output"))))
          (should (equal "outer/1" received))
          (should (equal "outer/1" (plist-get rendering :tool-use-id)))
          (should (equal "exec-1" (plist-get rendering :execution-id)))
          (should (equal "error: final output" (plist-get rendering :header)))))))
  :doc "direct Bash nested in a ToolCall child reconciles the deepest result"
  (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry)))
    (niwa-tool-register
     (niwa-tool--create :name "Bash" :category "niwa"
                           :renderer #'niwa-tool-exec--render-bash))
    (niwa-tool-register
     (niwa-tool--create :name "ToolCall" :category "niwa"
                           :renderer #'niwa-tool-ptc--render))
    (with-temp-buffer
      (let ((data (current-buffer)))
        (niwa-execution-transcript-handle-event
         (list :type 'terminal :data-buffer data :tool-use-id "outer/1/1"
               :owner "/root" :whole-output "DEEP FAILURE"
               :facts '(:execution-id "exec-deep" :command "make test"
                        :state completed :outcome failure :exit-code 2)))
        (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                   (lambda () data)))
          (let ((row (niwa-view--child-call-rendering
                      '(:id "outer/1" :tool "ToolCall" :status success
                        :result "OLD OUTPUT"
                        :render-data
                        (:kind ptc :outcome completed :direct-tool "Bash"
                         :calls ((:id "outer/1/1" :tool "Bash" :status success
                                  :args (:command "make test")
                                  :render-data (:state running))))))))
            (should (string-match-p "failed" (plist-get row :header)))
            (should (string-match-p "DEEP FAILURE" (plist-get row :body)))
            (should-not (string-match-p "OLD OUTPUT" (plist-get row :body)))
            (should (equal "outer/1/1" (plist-get row :tool-use-id))))))))
  :doc "running Bash inside ToolCall uses cached output instead of its initial result"
  (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry)))
    (niwa-tool-register
     (niwa-tool--create :name "Bash" :category "niwa"
                           :renderer #'niwa-tool-exec--render-bash))
    (with-temp-buffer
      (let ((data (current-buffer)))
        (with-temp-buffer
          (setq-local niwa-view--execution-events (make-hash-table :test 'equal))
          (puthash "outer/1"
                   '(:type progress :facts (:execution-id "exec-1" :state running
                                            :wall-time-seconds 7)
                     :output-tail "NEW OUTPUT")
                   niwa-view--execution-events)
          (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                     (lambda () data)))
            (let ((row (niwa-view--child-call-rendering
                        '(:id "outer/1" :tool "Bash" :status success
                          :args (:command "make test") :result "OLD OUTPUT"
                          :render-data (:execution-id "exec-1" :state running
                                        :sandbox-summary
                                        (:attempt-count 1 :started-count 1
                                         :sandbox unavailable :filesystem unrestricted
                                         :network unrestricted)
                                        :wall-time-seconds 0.25)))))
              (should (string-match-p "running · 7s" (plist-get row :header)))
              (should (string-match-p "NEW OUTPUT" (plist-get row :body)))
              (should (equal 'unavailable
                             (plist-get (plist-get row :sandbox-summary) :sandbox)))
              (should-not (string-match-p "OLD OUTPUT" (plist-get row :body))))))))))

(niwa-deftest niwa-view-child-call-rendering-retained-output ()
  ,test
  (test)
  :doc "a reloaded nested Bash row uses its retained output, not a stale result"
  (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry)))
    (niwa-tool-register
     (niwa-tool--create :name "Bash" :category "niwa"
                           :renderer #'niwa-tool-exec--render-bash))
    (niwa-tool-register
     (niwa-tool--create :name "ToolCall" :category "niwa"
                           :renderer #'niwa-tool-ptc--render))
    (with-temp-buffer
      (let ((data (current-buffer)))
        (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                   (lambda () data)))
          (let ((row (niwa-view--child-call-rendering
                      '(:id "outer/1" :tool "ToolCall" :status success
                        :result "STALE MODEL RESULT"
                        :render-data
                        (:kind ptc :outcome completed :direct-tool "Bash"
                         :calls ((:id "outer/1/1" :tool "Bash" :status success
                                  :args (:command "make test")
                                  :render-data
                                  (:execution-id "exec-deep"
                                   :state completed :outcome success
                                   :execution-output "CANONICAL FINAL"))))))))
            (should (string-match-p "CANONICAL FINAL" (plist-get row :body)))
            (should-not (string-match-p "STALE MODEL RESULT"
                                        (plist-get row :body)))))))))

(niwa-deftest niwa-view-direct-toolcall-bash-history ()
  ,test
  (test)
  :doc "expanded direct Bash keeps history access through its child execution ID"
  (let ((data (generate-new-buffer " *direct Bash source*"))
        opened)
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (propertize "(:name \"ToolCall\")\nINITIAL OUTPUT"
                                'gptel '(tool . "outer")))
            (insert (niwa-tool-render-data-format
                     '(:kind ptc :direct-tool "Bash" :outcome completed
                       :calls ((:id "outer/1" :tool "Bash" :status success
                                :args (:command "make test")
                                :render-data (:execution-id "exec-1"
                                              :state running))))
                     "outer")))
          (let ((call '(:name "ToolCall" :tool-use-id "outer"
                        :result "INITIAL OUTPUT" :render-data
                        (:kind ptc :direct-tool "Bash" :outcome completed
                         :calls ((:id "outer/1" :tool "Bash" :status success
                                  :args (:command "make test")
                                  :render-data (:execution-id "exec-1"
                                                :state running)))))))
            (with-temp-buffer
              (setq-local niwa--data-buffer data)
              (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                         (lambda () data))
                        ((symbol-function 'niwa-view-audit-show-history)
                         (lambda (id) (setq opened id))))
                (should (niwa-view-audit--history-records "exec-1"))
                (dolist (terminal '(nil t))
                  (when terminal
                    (niwa-execution-transcript-handle-event
                     (list :type 'terminal :data-buffer data
                           :tool-use-id "outer/1" :owner "/root"
                           :whole-output "FINAL OUTPUT"
                           :facts '(:execution-id "exec-1" :command "make test"
                                    :state completed :outcome success
                                    :exit-code 0))))
                  (erase-buffer)
                  (niwa-view--render-expanded-body
                   (niwa-view--render-tool-call call data)
                   (cons 1 (with-current-buffer data (point-max))))
                  (goto-char (point-min))
                  (should (search-forward "[Execution history]" nil t))
                  (funcall (get-text-property (match-beginning 0)
                                              'niwa-view-zone-activate))
                  (should (equal "exec-1" opened)))))))
      (kill-buffer data))))

(niwa-deftest niwa-view-child-call-history ()
  ,test
  (test)
  :doc "an expanded nested Bash row links its real execution's retained history"
  (let ((data (generate-new-buffer " *nested history source*"))
        opened)
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (propertize "(:name \"ToolCall\")"
                                'gptel '(tool . "outer"))))
          (with-temp-buffer
            (setq-local niwa-view--data-turn-start nil)
            (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                       (lambda () data))
                      ((symbol-function 'niwa-view-audit-show-history)
                       (lambda (id) (setq opened id))))
              (niwa-view--render-expanded-body
               '(:vtype tool-child :tool-use-id "outer/1"
                 :execution-id "exec-child" :header "Bash: printf child"
                 :body "$ printf child\n\nchild")
               (cons 1 (with-current-buffer data (point-max))))
              (goto-char (point-min))
              (should (search-forward "[Execution history]" nil t))
              (should (save-excursion (goto-char (match-beginning 0)) (bolp)))
              (should-not (get-text-property (match-end 0) 'mouse-face))
              (funcall (get-text-property (match-beginning 0)
                                          'niwa-view-zone-activate))
              (should (equal "exec-child" opened)))))
      (kill-buffer data))))

(defun test-niwa-view-audit--insert-history-fixture (id execution-id)
  "Insert Bash call ID that started EXECUTION-ID at point."
  (insert (propertize
           (format "(:name \"Bash\" :args (:command \"make %s\"))\n\nOUT\n" id)
           'gptel (cons 'tool id))
          (niwa-tool-render-data-format
           (list :execution-id execution-id :state 'completed) id)))

(defun test-niwa-view-audit--insert-history-delivery (execution-id)
  "Insert the mailbox delivery of EXECUTION-ID at point."
  (insert (format (concat "<agent-message type=\"EXECUTION\" sender=\"/root\" "
                          "execution_id=\"%s\" outcome=\"success\" exit_code=\"0\">\n"
                          "TAIL\n"
                          "</agent-message>\n")
                  execution-id)))

(niwa-deftest niwa-view-audit--history-records ()
  ,test
  (test)
  :doc "only calls and deliveries naming the execution are collected, in order"
  (let ((data (generate-new-buffer " *history records source*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (test-niwa-view-audit--insert-history-fixture "t1" "exec-1")
            (insert (propertize "I will poll exec-1 next.\n" 'gptel 'response))
            (insert (propertize
                     "(:name \"WriteStdin\" :args (:execution_id \"exec-1\"))\n\nok\n"
                     'gptel '(tool . "t2")))
            (test-niwa-view-audit--insert-history-fixture "t3" "exec-2")
            (test-niwa-view-audit--insert-history-delivery "exec-1"))
          (with-temp-buffer
            (setq-local niwa--data-buffer nil)
            (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                       (lambda () data)))
              (should (equal '("Bash" "WriteStdin" "Execution delivery")
                             (mapcar (lambda (call) (plist-get call :name))
                                     (niwa-view-audit--history-records
                                      "exec-1")))))))
      (kill-buffer data))))

(niwa-deftest niwa-view-audit--history-records/origin-stops-walk ()
  ,test
  (test)
  :doc "segments older than the execution's starting call are never read"
  (let ((live (generate-new-buffer " *history live*"))
        read)
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq-local niwa--session 'session)
            (test-niwa-view-audit--insert-history-delivery "exec-1"))
          (with-temp-buffer
            (setq-local niwa--data-buffer live)
            (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                       (lambda () live))
                      ((symbol-function
                        'niwa-session-artifacts-transcript-segments)
                       (lambda (&rest _)
                         '((:number 1 :status readable)
                           (:number 2 :status readable)
                           (:status readable :current-p t))))
                      ((symbol-function
                        'niwa-session-artifacts-read-transcript-segment)
                       (lambda (_session descriptor)
                         (push (plist-get descriptor :number) read)
                         (with-current-buffer (generate-new-buffer " *older*")
                           (when (= 2 (plist-get descriptor :number))
                             (test-niwa-view-audit--insert-history-fixture
                              "t1" "exec-1"))
                           (current-buffer)))))
              (should (equal '("Bash" "Execution delivery")
                             (mapcar (lambda (call) (plist-get call :name))
                                     (niwa-view-audit--history-records
                                      "exec-1"))))
              (should (equal '(2) read)))))
      (kill-buffer live))))

(niwa-deftest niwa-view-audit-show-history ()
  ,test
  (test)
  :doc "arguments and execution facts render as readable lines"
  (cl-letf (((symbol-function 'niwa-view-audit--history-records)
             (lambda (_)
               (list (list :name "Bash" :args '(:command "make test")
                           :result (concat "OUT\n<bash-execution execution_id="
                                           "\"exec-1\" exit_code=\"1\"/>")))))
            ((symbol-function 'display-buffer) #'ignore))
    (unwind-protect
        (progn
          (niwa-view-audit-show-history "exec-1")
          (with-current-buffer "*niwa execution history*"
            (should (string-match-p "^Bash\n  command: make test\n\nOUT\n\n"
                                    (buffer-string)))
            (should (string-match-p "^  exit_code: 1$" (buffer-string)))
            (should-not (string-match-p "<bash-execution\\|(:command"
                                        (buffer-string)))))
      (when (get-buffer "*niwa execution history*")
        (kill-buffer "*niwa execution history*")))))

(niwa-deftest niwa-view-audit-show-control-result ()
  ,test
  (test)
  :doc "input and stop headers link to the original deepest Bash result"
  (let ((data (generate-new-buffer " *control result source*"))
        opened)
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (propertize
                     "(:name \"WriteStdin\")\ncollected"
                     'gptel '(tool . "poll")))
            (insert (niwa-tool-render-data-format
                     '(:execution-id "exec-deep" :state complete) "poll"))
            (insert (propertize
                     "(:name \"ToolCall\")\noriginal result"
                     'gptel '(tool . "outer")))
            (insert (niwa-tool-render-data-format
                     '(:kind ptc :calls
                       ((:id "outer/1" :tool "ToolCall" :status success
                         :render-data
                         (:kind ptc :direct-tool "Bash"
                          :calls ((:id "outer/1/1" :tool "Bash"
                                   :status success :args (:command "cat")
                                   :render-data (:execution-id "exec-deep"
                                                 :state running)))))))
                     "outer")))
          (should (niwa-tool-render-data-for-tool data "outer"))
          (with-temp-buffer
            (setq-local niwa--data-buffer data)
            (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                       (lambda () data))
                      ((symbol-function 'niwa-view-audit-show-result)
                       (lambda (record) (setq opened record))))
              (dolist (control '(("WriteStdin" (:execution_id "exec-deep"
                                                 :chars "secret\n") input)
                                 ("StopExecution" (:execution_id "exec-deep")
                                  stop)))
                (erase-buffer)
                (let* ((name (nth 0 control))
                       (args (nth 1 control))
                       (row (niwa-tool-exec--render-bash
                             name args "ok"
                             (list :execution-id "exec-deep" :status 'success
                                   :execution-control (nth 2 control)))))
                  (insert (niwa-view--rendering-header-line row))
                  (goto-char (point-min))
                  (should (search-forward "[Show result]" nil t))
                  (should-not (string-match-p "secret" (buffer-string)))
                  (funcall (get-text-property (match-beginning 0)
                                              'niwa-view-zone-activate))
                  (should (equal "outer/1/1"
                                 (plist-get opened :tool-use-id)))))))
      (kill-buffer data)))))

(niwa-deftest niwa-view-audit-show-control-result-missing ()
  ,test
  (test)
  :doc "control link without its original row reports missing retained evidence"
  (let ((data (generate-new-buffer " *missing control source*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--data-buffer data)
          (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                     (lambda () data))
                    ((symbol-function 'display-buffer) #'identity))
            (let ((evidence (niwa-view-audit-show-control-result
                             "exec-absent")))
              (with-current-buffer evidence
                (should (derived-mode-p 'special-mode))
                (should (string-match-p
                         "Original execution row and retained output are unavailable"
                         (buffer-string)))))))
      (kill-buffer data)
      (when-let* ((buffer (get-buffer "*niwa execution result*")))
        (kill-buffer buffer)))))

(niwa-deftest niwa-view-audit-show-control-result-retained ()
  ,test
  (test)
  :doc "a control link retains trusted completion output after its source row is gone"
  (let ((data (generate-new-buffer " *retained control source*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (niwa--format-hook-audit-record
                     '(:type execution-breadcrumb :execution-id "exec-deep"
                       :tool-use-id "outer/1/1" :command "cat"
                       :facts (:execution-output "RETAINED FINAL OUTPUT")))))
          (with-temp-buffer
            (setq-local niwa--data-buffer data)
            (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                       (lambda () data))
                      ((symbol-function 'display-buffer) #'identity))
              (let ((evidence (niwa-view-audit-show-control-result
                               "exec-deep")))
                (with-current-buffer evidence
                  (should (derived-mode-p 'special-mode))
                  (should (string-match-p "RETAINED FINAL OUTPUT"
                                          (buffer-string))))))))
      (kill-buffer data)
      (when-let* ((buffer (get-buffer "*niwa execution result*")))
        (kill-buffer buffer)))))

(niwa-deftest niwa-view-audit-show-control-result-running-archive ()
  ,test
  (test)
  :doc "archived running Bash retains source identity and output for control links"
  (let ((data (generate-new-buffer " *running archived control source*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (niwa--format-hook-audit-record
                     '(:type execution-archive :tool-use-id "original"
                       :render-data (:execution-id "exec-1" :command "sleep 30"
                                     :state running
                                     :execution-output "RETAINED RUNNING OUTPUT")))))
          (with-temp-buffer
            (setq-local niwa--data-buffer data)
            (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                       (lambda () data))
                      ((symbol-function 'display-buffer) #'identity))
              (let ((evidence (niwa-view-audit-show-control-result "exec-1")))
                (with-current-buffer evidence
                  (should (derived-mode-p 'special-mode))
                  (should (string-match-p "RETAINED RUNNING OUTPUT"
                                          (buffer-string))))))))
      (kill-buffer data)
      (when-let* ((buffer (get-buffer "*niwa execution result*")))
        (kill-buffer buffer)))))

(niwa-deftest niwa-view-audit-show-control-result-older-segment ()
  ,test
  (test)
  :doc "input and stop result links find retained output in an older segment"
  (let* ((root (make-temp-file "niwa-control-older-" t))
         (archive (niwa-session-artifacts-segment-path root 2))
         (live (generate-new-buffer " *control latest segment*"))
         (session (test-niwa-session-persistence--leased-session root
                                           :current-segment 3)))
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (niwa-session-artifacts-segment-path root 3))
            (setq-local niwa--session session))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (niwa--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "original"
                       :render-data (:execution-id "exec-older"
                                     :execution-output "RETAINED OLDER OUTPUT"))))
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) archive nil 'silent))
          (niwa-view-audit-test--publish session archive)
          (should (equal '(missing readable readable)
                         (mapcar (lambda (entry) (plist-get entry :status))
                                 (niwa-session-artifacts-transcript-segments
                                  session live))))
          (with-temp-buffer
            (setq-local niwa--data-buffer live)
            (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                       (lambda () live))
                      ((symbol-function 'display-buffer) #'identity))
              (dolist (control '(("WriteStdin" (:execution_id "exec-older"
                                                 :chars "input\n") input)
                                 ("StopExecution" (:execution_id "exec-older")
                                  stop)))
                (let ((inhibit-read-only t)) (erase-buffer))
                (insert (niwa-view--rendering-header-line
                         (niwa-tool-exec--render-bash
                          (nth 0 control) (nth 1 control) "ok"
                          (list :execution-id "exec-older" :status 'success
                                :execution-control (nth 2 control)))))
                (goto-char (point-min))
                (should (search-forward "[Show result]" nil t))
                (let ((before (buffer-list)))
                  (let ((evidence (funcall (get-text-property
                                            (match-beginning 0)
                                            'niwa-view-zone-activate))))
                    (with-current-buffer evidence
                      (should (derived-mode-p 'special-mode))
                      (should (string-match-p "RETAINED OLDER OUTPUT"
                                              (buffer-string)))))
                  (should-not (cl-set-difference (buffer-list)
                                                 (cons (get-buffer
                                                        "*niwa execution result*")
                                                       before)))))))
      (when (buffer-live-p live) (kill-buffer live))
      (when-let* ((buffer (get-buffer "*niwa execution result*")))
        (kill-buffer buffer))
      (niwa-session-durability-lease-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t)))))

(niwa-deftest niwa-view-audit-show-control-result-older-source ()
  ,test
  (test)
  :doc "a control link identifies a nested Bash source row in an older segment"
  (let* ((root (make-temp-file "niwa-control-source-" t))
         (archive (niwa-session-artifacts-segment-path root 1))
         (live (generate-new-buffer " *control source latest*"))
         (session (test-niwa-session-persistence--leased-session root
                                           :current-segment 3))
         opened)
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (niwa-session-artifacts-segment-path root 3))
            (setq-local niwa--session session))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (propertize "(:name \"ToolCall\")\noriginal result"
                                'gptel '(tool . "outer")))
            (insert (niwa-tool-render-data-format
                     '(:kind ptc :calls
                       ((:id "outer/1" :tool "Bash" :status success
                         :render-data (:execution-id "exec-older"
                                       :execution-output "ORIGINAL OUTPUT"))))
                     "outer"))
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) archive nil 'silent))
          (niwa-view-audit-test--publish session archive)
          (with-temp-buffer
            (setq-local niwa--data-buffer live)
            (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                       (lambda () live))
                      ((symbol-function 'niwa-view-audit-show-result)
                       (lambda (record) (setq opened record))))
              (niwa-view-audit-show-control-result "exec-older")
              (should (equal "outer/1" (plist-get opened :tool-use-id)))
              (should (equal "exec-older" (plist-get opened :execution-id))))))
      (when (buffer-live-p live) (kill-buffer live))
      (niwa-session-durability-lease-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-view-audit-show-control-result-agent-archive ()
  ,test
  (test)
  :doc "an agent's compacted source still opens its retained Bash output"
  (let* ((root (make-temp-file "niwa-control-agent-" t))
         (agent-dir (file-name-concat root "agents"))
         (archive (file-name-concat agent-dir
                                    "worker.compact-0001.chat.org"))
         (later (file-name-concat agent-dir
                                  "worker.compact-0003.chat.org"))
         (live (generate-new-buffer " *agent control current*"))
         (view (generate-new-buffer " *agent control view*"))
         (niwa-tool--registry (copy-hash-table niwa-tool--registry))
         (session (test-niwa-session-persistence--leased-session root
                                           :current-segment 7)))
    (unwind-protect
        (progn
          (make-directory agent-dir t)
          (with-current-buffer live
            (setq buffer-file-name (file-name-concat agent-dir
                                                      "worker.chat.org"))
            (setq-local niwa--session session)
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert "#+begin_tool (ToolCall)\n")
            (insert (propertize
                     (concat "(:name \"ToolCall\")\nMODEL-ENVELOPE"
                             (niwa-tool-render-data-format
                     '(:kind ptc :direct-tool "Bash" :outcome completed
                       :calls
                       ((:id "outer/1" :tool "Bash" :status success
                         :args (:command "run agent job")
                         :render-data (:execution-id "exec-agent-old"
                                       :state completed :outcome success
                                       :command "run agent job"
                                       :execution-output "AGENT-OUTPUT"))))
                     "outer"))
                     'gptel '(tool . "outer")))
            (insert "\n#+end_tool\n")
            (insert "#+begin_tool (ToolCall)\n")
            (insert (propertize
                     (concat "(:name \"ToolCall\")\nNESTED-MODEL-ENVELOPE"
                             (niwa-tool-render-data-format
                              '(:kind ptc :outcome completed
                                :calls
                                ((:id "nested/1" :tool "ToolCall" :status success
                                  :result "INNER-MODEL-ENVELOPE"
                                  :render-data
                                  (:kind ptc :direct-tool "Bash"
                                   :outcome completed
                                   :calls
                                   ((:id "nested/1/1" :tool "Bash"
                                     :status success
                                     :args (:command "run deep job")
                                     :render-data
                                     (:execution-id "exec-agent-deep"
                                      :state completed :outcome success
                                      :execution-output "DEEP-CANONICAL-OUTPUT")))))))
                              "nested"))
                     'gptel '(tool . "nested")))
            (insert "\n#+end_tool\n")
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) archive nil 'silent))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (propertize "SECOND-ARCHIVE\n" 'gptel 'response))
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) later nil 'silent))
          (niwa-view-audit-test--publish session archive later)
          (niwa-view--setup view live
                               '(:agent-transcript-p t :agent-path "/root/worker"))
          (niwa-tool-register
           (niwa-tool--create :name "Bash" :category "niwa"
                                 :renderer #'niwa-tool-exec--render-bash))
          (niwa-tool-register
           (niwa-tool--create :name "ToolCall" :category "niwa"
                                 :renderer #'niwa-tool-ptc--render))
          (with-current-buffer view
            (niwa-view--full-rerender)
            (let ((project (symbol-function 'niwa-view-render-project-segment)))
              (cl-letf (((symbol-function 'niwa-view-render-project-segment)
                         (lambda (source state direction)
                           (if source
                               (error "Archive projection failed")
                             (funcall project source state direction)))))
                (should-error (niwa-view-go-to-segment 1)
                              :type 'error)))
            (should-not (niwa-view-segments-current-number))
            (should buffer-read-only)
            (niwa-view-go-to-segment 1)
            (should-not (string-search "AGENT-OUTPUT" (buffer-string)))
            (niwa-view-return-to-latest-segment)
            (should (equal '((readable . nil) (readable . nil) (readable . t))
                           (mapcar (lambda (descriptor)
                                     (cons (plist-get descriptor :status)
                                           (plist-get descriptor :current-p)))
                                   (niwa-session-artifacts-transcript-segments
                                    session live))))
            ;; The Markdown fontification buffer is a reusable cache created on
            ;; first use when grammars are available; create it before the
            ;; snapshot so the leak checks below count only audit buffers.
            (niwa-view--markdown-fontify-target)
            (let ((before (buffer-list)))
              (let* ((control
                      (niwa-view--rendering-header-line
                       (niwa-tool-exec--render-bash
                        "WriteStdin"
                        '(:execution_id "exec-agent-old" :chars "input\n")
                        "ok"
                        '(:execution-id "exec-agent-old" :status success
                          :execution-control input :control-succeeded-p t))))
                     (link (string-match "\\[Show result\\]" control)))
                (should link)
                (funcall (get-text-property
                          link 'niwa-view-zone-activate control)))
              (should (= 1 (niwa-view-segments-current-number)))
              (should buffer-read-only)
              (should (string-search "Bash: run agent job" (buffer-string)))
              (should (string-match-p "AGENT-OUTPUT" (buffer-string)))
              (should-not (string-match-p "MODEL-ENVELOPE" (buffer-string)))
              (should-not (get-buffer "*niwa execution result*"))
              (should (= 1 (length (cl-set-difference (buffer-list) before))))
              (let* ((control
                      (niwa-view--rendering-header-line
                       (niwa-tool-exec--render-bash
                        "WriteStdin"
                        '(:execution_id "exec-agent-deep" :chars "input\n")
                        "ok"
                        '(:execution-id "exec-agent-deep" :status success
                          :execution-control input :control-succeeded-p t))))
                     (link (string-match "\\[Show result\\]" control)))
                (should link)
                (funcall (get-text-property
                          link 'niwa-view-zone-activate control)))
              (should (= 1 (niwa-view-segments-current-number)))
              (should (string-search "Bash: run deep job" (buffer-string)))
              (should (string-search "DEEP-CANONICAL-OUTPUT" (buffer-string)))
              (should-not (string-search "INNER-MODEL-ENVELOPE" (buffer-string)))
              (should-not (get-buffer "*niwa execution result*"))
              (niwa-view-next-segment)
              (should (= 3 (niwa-view-segments-current-number)))
              (should (string-match-p "SECOND-ARCHIVE" (buffer-string)))
              (niwa-view-go-to-segment 1)
              (should (= 1 (niwa-view-segments-current-number)))
              (cl-letf (((symbol-function 'niwa-view-render-project-segment)
                         (lambda (&rest _args)
                           (error "Projection and recovery failed"))))
                (should-error (niwa-view-return-to-latest-segment)
                              :type 'error))
              (should (= 1 (niwa-view-segments-current-number)))
              (should buffer-read-only)
              (with-current-buffer live
                (insert (propertize "(:name \"ToolCall\")\n"
                                    'gptel '(tool . "outer"))))
              (niwa-view-audit--select-source
               '(:tool-use-id "outer/1"))
              (should-not (niwa-view-segments-current-number))
              (should buffer-read-only)
              (niwa-view-previous-segment)
              (should (= 3 (niwa-view-segments-current-number)))
              (niwa-view-return-to-latest-segment)
              (should buffer-read-only)
              (should-not (cl-set-difference (buffer-list) before)))))
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p live) (kill-buffer live))
      (when-let* ((buffer (get-buffer "*niwa execution result*")))
        (kill-buffer buffer))
      (niwa-session-durability-lease-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-view-audit-show-result-forwarded-agent-archive ()
  ,test
  (test)
  :doc "a forwarded completion opens its agent's surviving source row"
  (save-window-excursion
    (let* ((root (make-temp-file "niwa-forwarded-result-" t))
           (agent-dir (file-name-concat root "agents"))
           (live-path (file-name-concat agent-dir "worker.chat.org"))
           (archive (file-name-concat agent-dir
                                      "worker.compact-0001.chat.org"))
           (niwa-tool--registry (copy-hash-table niwa-tool--registry))
           (session
            (test-niwa-session-persistence--leased-session root
             :agent-transcripts
             '(("worker-id" . (:agent-path "/root/worker"
                               :path "agents/worker.chat.org")))))
           agent-view agent-data)
      (unwind-protect
          (progn
            (make-directory agent-dir t)
            (with-temp-buffer
              (org-mode)
              (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
              (insert "#+begin_tool (ToolCall)\n")
              (insert (propertize
                       (concat "(:name \"ToolCall\")\nMODEL-ENVELOPE"
                               (niwa-tool-render-data-format
                       '(:kind ptc :direct-tool "Bash" :outcome completed
                         :calls
                         ((:id "forwarded/1" :tool "Bash" :status success
                           :args (:command "run forwarded job")
                           :render-data
                           (:execution-id "exec-forwarded" :state completed
                            :execution-output "FORWARDED-OUTPUT"))))
                       "forwarded"))
                       'gptel '(tool . "forwarded")))
              (insert "\n#+end_tool\n")
              (niwa-session-artifacts-stabilize-gptel-bounds)
              (write-region (point-min) (point-max) archive nil 'silent))
            (with-temp-file live-path
              (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"))
            (niwa-view-audit-test--publish session archive live-path)
            (niwa-tool-register
             (niwa-tool--create :name "Bash" :category "niwa"
                                   :renderer #'niwa-tool-exec--render-bash))
            (niwa-tool-register
             (niwa-tool--create :name "ToolCall" :category "niwa"
                                   :renderer #'niwa-tool-ptc--render))
            (niwa-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local niwa--session session)
                (insert (propertize "Parent transcript\n" 'gptel 'response))
                (insert (niwa--format-hook-audit-record
                         '(:type execution-breadcrumb
                           :owner "/root/worker"
                           :execution-id "exec-forwarded"
                           :tool-use-id "forwarded/1"
                           :command "run forwarded job"
                           :facts (:outcome success)))))
              (with-current-buffer view-buf
                (switch-to-buffer view-buf)
                (niwa-view--full-rerender)
                (goto-char (point-min))
                (should (search-forward "[Show result]" nil t))
                (funcall (get-text-property
                          (match-beginning 0) 'niwa-view-zone-activate))
                (setq agent-view (window-buffer (selected-window)))
                (should-not (eq agent-view view-buf))
                (with-current-buffer agent-view
                  (setq agent-data niwa--data-buffer)
                  (should niwa-view--agent-transcript-p)
                  (should (eq session (buffer-local-value
                                       'niwa--session agent-data)))
                  (should (= 1 (niwa-view-segments-current-number)))
                  (should buffer-read-only)
                  (should-not (get-text-property (point)
                                                 'niwa-view-collapsed))
                  (should (string-search "Bash: run forwarded job"
                                         (buffer-string)))
                  (should (string-search "FORWARDED-OUTPUT" (buffer-string)))
                  (should-not (string-search "MODEL-ENVELOPE" (buffer-string)))
                  (should-not (get-buffer "*niwa execution result*"))))))
        (when (buffer-live-p agent-view) (kill-buffer agent-view))
        (when (buffer-live-p agent-data)
          (with-current-buffer agent-data (set-buffer-modified-p nil))
          (kill-buffer agent-data))
        (when-let* ((evidence (get-buffer "*niwa execution result*")))
          (kill-buffer evidence))
        (niwa-session-durability-lease-release root session)
        (niwa-workspace-clear-registry)
        (delete-directory root t)))))

(niwa-deftest niwa-view-audit-archived-terminal-rendering ()
  ,test
  (test)
  :doc "opening an archived running row reads the later live completion"
  (let ((live (generate-new-buffer " *terminal live segment*"))
        (archive (generate-new-buffer " *original archived segment*")))
    (unwind-protect
        (progn
          (with-current-buffer live
            (insert (niwa--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "original"
                       :render-data (:execution-id "exec-1" :status error
                                     :outcome failure :exit-code 2
                                     :execution-output "FINAL OUTPUT")))))
          (with-temp-buffer
            (setq-local niwa--data-buffer live)
            (let ((rendering (niwa-view--render-tool-call
                              '(:name "Bash" :tool-use-id "original"
                                :args (:command "sleep 10")
                                :result "OLD OUTPUT"
                                :render-data (:execution-id "exec-1"
                                              :state running
                                              :execution-output "OLD OUTPUT"))
                              archive)))
              (should (string-match-p "FINAL OUTPUT"
                                      (plist-get rendering :body)))
              (should-not (string-match-p "OLD OUTPUT"
                                          (plist-get rendering :body)))
              (should (string-match-p "failed\\|error"
                                      (plist-get rendering :header))))))
      (kill-buffer live)
      (kill-buffer archive))))

(niwa-deftest niwa-view-audit-intermediate-terminal-rendering ()
  ,test
  (test)
  :doc "a later compaction still renders an archived row with its terminal facts"
  (let* ((root (make-temp-file "niwa-terminal-segments-" t))
         (live (generate-new-buffer " *third segment*"))
         (archive (generate-new-buffer " *first source segment*"))
         (first-path (niwa-session-artifacts-segment-path root 1))
         (middle-path (niwa-session-artifacts-segment-path root 2))
         (session (test-niwa-session-persistence--leased-session root
                                           :current-segment 3)))
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (niwa-session-artifacts-segment-path root 3))
            (setq-local niwa--session session))
          (with-current-buffer archive
            (insert "# original running Bash segment\n")
            (write-region (point-min) (point-max) first-path nil 'silent))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (niwa--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "original"
                       :render-data (:execution-id "exec-1" :status error
                                     :outcome failure :exit-code 2
                                     :execution-output "FINAL AFTER ROTATION"))))
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) middle-path nil 'silent))
          (niwa-view-audit-test--publish session first-path middle-path)
          (with-temp-buffer
            (setq-local niwa--data-buffer live)
            (let ((descriptors (niwa-session-artifacts-transcript-segments
                                session live)))
              (should (equal '(readable readable readable)
                             (mapcar (lambda (entry) (plist-get entry :status))
                                     descriptors)))
              (let ((restored (niwa-session-artifacts-read-transcript-segment
                               session (nth 1 descriptors))))
                (unwind-protect
                    (with-current-buffer restored
                      (let ((facts (plist-get
                                    (car (niwa-transcript-audit-records
                                          (buffer-string) 'execution-completion))
                                    :render-data)))
                        (should (equal "FINAL AFTER ROTATION"
                                       (plist-get facts :execution-output)))
                        (should (eq 'error (plist-get facts :status)))
                        (should (= 2 (plist-get facts :exit-code)))))
                  (kill-buffer restored))))
            (let ((before (buffer-list)))
              (let ((rendering (niwa-view--render-tool-call
                                '(:name "Bash" :tool-use-id "original"
                                  :args (:command "sleep 10")
                                  :result "OLD OUTPUT"
                                  :render-data (:execution-id "exec-1"
                                                :state running
                                                :execution-output "OLD OUTPUT"))
                                archive)))
                (should (string-match-p "FINAL AFTER ROTATION"
                                        (plist-get rendering :body)))
                (should-not (string-match-p "OLD OUTPUT"
                                            (plist-get rendering :body))))
              (should-not (cl-set-difference (buffer-list) before)))))
      (when (buffer-live-p live) (kill-buffer live))
      (when (buffer-live-p archive) (kill-buffer archive))
      (niwa-session-durability-lease-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-view-audit-intermediate-terminal-evidence ()
  ,test
  (test)
  :doc "missing original row uses terminal evidence from an older segment"
  (let* ((root (make-temp-file "niwa-terminal-evidence-" t))
         (middle-path (niwa-session-artifacts-segment-path root 2))
         (live (generate-new-buffer " *latest without Bash*"))
         (session (test-niwa-session-persistence--leased-session root
                                           :current-segment 3)))
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (niwa-session-artifacts-segment-path root 3))
            (setq-local niwa--session session))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (niwa--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "original"
                       :render-data (:execution-id "exec-1"
                                     :execution-output "RETAINED FINAL S2"))))
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) middle-path nil 'silent))
          (niwa-view-audit-test--publish session middle-path)
          (with-temp-buffer
            (setq-local niwa--data-buffer live)
            (should (equal '(missing readable readable)
                           (mapcar (lambda (entry) (plist-get entry :status))
                                   (niwa-session-artifacts-transcript-segments
                                    session live))))
            (let ((before (buffer-list)))
              (cl-letf (((symbol-function 'display-buffer) #'identity))
                (let ((evidence (niwa-view-audit-show-result
                                 '(:owner "/root" :execution-id "exec-1"
                                   :tool-use-id "original"))))
                  (with-current-buffer evidence
                    (should (derived-mode-p 'special-mode))
                    (should (string-match-p "RETAINED FINAL S2"
                                            (buffer-string))))
                  (should-not (cl-set-difference (buffer-list)
                                                 (cons evidence before))))))))
      (when (buffer-live-p live) (kill-buffer live))
      (when-let* ((buffer (get-buffer "*niwa execution result*")))
        (kill-buffer buffer))
      (niwa-session-durability-lease-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-view-audit-agent-compact-archive-evidence ()
  ,test
  (test)
  :doc "a child result remains inspectable after two agent compactions"
  (let* ((root (make-temp-file "niwa-child-archives-" t))
         (agents (file-name-concat root "agents"))
         (canonical (file-name-concat agents "worker.chat.org"))
         (archive-1 (file-name-concat agents "worker.compact-0001.chat.org"))
         (archive-2 (file-name-concat agents "worker.compact-0002.chat.org"))
         (data (generate-new-buffer " *agent current after compaction*"))
         (session (test-niwa-session-persistence--leased-session root
                                           :current-segment 1)))
    (unwind-protect
        (progn
          (make-directory agents)
          (with-temp-file archive-1 (insert "# older empty agent archive\n"))
          (with-temp-buffer
            (org-mode)
            (require 'gptel)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (niwa--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "original"
                       :render-data (:execution-id "child-exec"
                                     :execution-output "FINAL CHILD ARCHIVE"))))
            (should (eq 'niwa-hook-audit
                        (get-text-property (1- (point)) 'gptel)))
            (should (gptel--get-buffer-bounds))
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (should (string-match-p "niwa-hook-audit"
                                    (or (org-entry-get (point-min)
                                                       "GPTEL_BOUNDS") "")))
            (write-region (point-min) (point-max) archive-2 nil 'silent))
          (niwa-view-audit-test--publish session archive-1 archive-2)
          (with-current-buffer data
            (setq buffer-file-name canonical)
            (setq-local niwa--session session)
            (let* ((descriptors (niwa-session-artifacts-transcript-segments
                                 session data))
                   (older (niwa-session-artifacts-read-transcript-segment
                           session (cadr descriptors))))
              (should (= 3 (length descriptors)))
              (unwind-protect
                  (with-current-buffer older
                    (should (niwa-transcript-audit-records
                             (buffer-string) 'execution-completion))
                    (should (equal "FINAL CHILD ARCHIVE"
                                   (plist-get
                                    (niwa-execution-transcript-pending-render-data
                                     older "original") :execution-output))))
                (kill-buffer older))))
          (with-temp-buffer
            (setq-local niwa--data-buffer data)
            (setq-local niwa-view--agent-transcript-p t)
            (setq-local niwa-view--agent-path "/root/worker")
            (cl-letf (((symbol-function 'display-buffer) #'identity))
              (let ((evidence (niwa-view-audit-show-result
                               '(:owner "/root/worker" :tool-use-id "original"
                                 :execution-id "child-exec" :command "child"))))
                (with-current-buffer evidence
                  (should (string-match-p "FINAL CHILD ARCHIVE"
                                          (buffer-string))))
              (niwa-session-publication-publish
               session '((:path "agents/worker.compact-0001.chat.org" :delete t)
                         (:path "session.meta.el" :content "(:fixture archive-only)"
                          :commit-marker t)) t)
              (should (= 2 (length
                            (niwa-session-artifacts-transcript-segments
                             session data))))
              (with-current-buffer (niwa-view-audit-show-result
                                    '(:owner "/root/worker"
                                      :tool-use-id "original"
                                      :execution-id "child-exec" :command "child"))
                (should (string-match-p "FINAL CHILD ARCHIVE"
                                        (buffer-string))))))))
      (when (buffer-live-p data) (kill-buffer data))
      (when-let* ((evidence (get-buffer "*niwa execution result*")))
        (kill-buffer evidence))
      (niwa-session-durability-lease-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-view-audit-retained-child-evidence ()
  ,test
  (test)
  :doc "missing nested source row opens its durable terminal output read-only"
  (with-temp-buffer
    (insert (niwa--format-hook-audit-record
             '(:type execution-completion :tool-use-id "outer/0"
               :render-data (:execution-output "final child output"))))
    (let ((niwa--data-buffer (current-buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
            (let ((result (niwa-view-audit--evidence
                           '(:tool-use-id "outer/0" :command "echo child"))))
              (with-current-buffer result
                (should (derived-mode-p 'special-mode))
                (should (string-match-p "final child output" (buffer-string))))))
        (when-let* ((buffer (get-buffer "*niwa execution result*")))
          (kill-buffer buffer))))))

(niwa-deftest niwa-view-audit-retained-archived-evidence ()
  ,test
  (test)
  :doc "missing archived row opens retained completion output without an artifact"
  (with-temp-buffer
    (insert (niwa--format-hook-audit-record
             '(:type execution-completion :tool-use-id "old-call"
               :render-data (:execution-output "archived output"))))
    (let ((niwa--data-buffer (current-buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
            (let ((result (niwa-view-audit--evidence
                           '(:tool-use-id "old-call" :command "echo archived"))))
              (with-current-buffer result
                (should (derived-mode-p 'special-mode))
                (should (string-match-p "archived output" (buffer-string))))))
        (when-let* ((buffer (get-buffer "*niwa execution result*")))
          (kill-buffer buffer))))))

(niwa-deftest niwa-view-audit-retained-disclosures ()
  ,test
  (test)
  :doc "read-only preview warns about missing bytes and preserves sandbox boundary"
  (with-temp-buffer
    (insert (niwa--format-hook-audit-record
             '(:type execution-completion :tool-use-id "bounded-call"
               :render-data (:execution-output "partial output"
                             :output-preview-truncated-p t
                             :omitted-output-bytes 123
                             :sandbox-summary (:attempt-count 1 :started-count 1
                                               :sandbox off :filesystem unrestricted
                                               :network unrestricted)))))
    (let ((niwa--data-buffer (current-buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
            (let ((result (niwa-view-audit--evidence
                           '(:tool-use-id "bounded-call" :command "echo many"))))
              (with-current-buffer result
                (should (derived-mode-p 'special-mode))
                (should (string-match-p "Sandbox:.*ran without confinement"
                                        (buffer-string)))
                (should (string-match-p "Output truncated:.*retained preview"
                                        (buffer-string)))
                (should (string-match-p "partial output" (buffer-string))))))
        (when-let* ((buffer (get-buffer "*niwa execution result*")))
          (kill-buffer buffer)))))
  :doc "a readable artifact hides preview warnings, but not spool limit warnings"
  (let ((physical (make-temp-file "niwa-complete-output-"))
        (data (generate-new-buffer " *complete execution source*")))
    (unwind-protect
        (progn
          (with-temp-file physical (insert "complete output"))
          (with-current-buffer data
            (setq-local niwa--session (niwa-session--create :name "complete")))
          (with-temp-buffer
            (let ((niwa--data-buffer data))
              (cl-letf (((symbol-function 'niwa-resource-prepare)
                         (lambda (&rest _) (list :physical-path physical)))
                        ((symbol-function 'display-buffer) (lambda (buffer) buffer)))
                (let ((result (niwa-view-audit--evidence
                               '(:command "cat complete"
                                 :facts (:output-path "artifact://executions/complete"
                                         :output-bytes 15
                                         :output-preview-truncated-p t
                                         :execution-output "partial")))))
                  (with-current-buffer result
                    (should (string-match-p "complete output" (buffer-string)))
                    (should-not (string-match-p "Output truncated"
                                                (buffer-string)))))
                (let ((result (niwa-view-audit--evidence
                               '(:command "cat stale"
                                 :facts (:output-path "artifact://executions/stale"
                                         :output-bytes 30
                                         :output-preview-truncated-p t
                                         :execution-output "newer partial")))))
                  (with-current-buffer result
                    (should (string-match-p "Output truncated:.*retained artifact"
                                            (buffer-string)))
                    (should (string-match-p "complete output" (buffer-string)))))
                (let ((result (niwa-view-audit--evidence
                               '(:command "cat capped"
                                 :facts (:output-path "artifact://executions/capped"
                                         :termination output-limit
                                         :execution-output "partial")))))
                  (with-current-buffer result
                    (should (string-match-p "Output truncated:.*output limit"
                                            (buffer-string)))
                    (should (string-match-p "complete output"
                                            (buffer-string)))))))))
      (when-let* ((buffer (get-buffer "*niwa execution result*")))
        (kill-buffer buffer))
      (kill-buffer data)
      (delete-file physical))))

(niwa-deftest niwa-view-audit-retained-empty-evidence ()
  ,test
  (test)
  :doc "empty terminal output is retained evidence, not missing output"
  (with-temp-buffer
    (insert (niwa--format-hook-audit-record
             '(:type execution-completion :tool-use-id "old-call"
               :render-data (:state completed :outcome success :exit-code 0
                             :execution-output ""))))
    (let ((niwa--data-buffer (current-buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
            (let ((result (niwa-view-audit--evidence
                           '(:tool-use-id "old-call" :command "sleep .3; true"))))
              (with-current-buffer result
                (should (derived-mode-p 'special-mode))
                (should (equal "sleep .3; true\n\nExecution produced no output.\n"
                               (buffer-string))))))
        (when-let* ((buffer (get-buffer "*niwa execution result*")))
          (kill-buffer buffer)))))
  :doc "an empty retained output artifact also shows a zero-output result"
  (let ((physical (make-temp-file "niwa-empty-execution-output-"))
        (data (generate-new-buffer " *empty execution source*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local niwa--session (niwa-session--create :name "empty")))
          (with-temp-buffer
            (let ((niwa--data-buffer data))
              (cl-letf (((symbol-function 'niwa-resource-prepare)
                         (lambda (&rest _) (list :physical-path physical)))
                        ((symbol-function 'display-buffer) (lambda (buffer) buffer)))
                (let ((result (niwa-view-audit--evidence
                               '(:command "true"
                                 :facts (:output-path "artifact://executions/empty")))))
                  (with-current-buffer result
                    (should (equal "true\n\nExecution produced no output.\n"
                                   (buffer-string))))))))
      (when-let* ((buffer (get-buffer "*niwa execution result*")))
        (kill-buffer buffer))
      (kill-buffer data)
      (delete-file physical))))

  :doc "read-only fallback discloses a retained launch error without stdout"
  (with-temp-buffer
    (insert (niwa--format-hook-audit-record
             '(:type execution-completion :tool-use-id "failed-call"
               :render-data (:termination spawn-failed :execution-output ""
                             :execution-error "no such file: bash"))))
    (let ((niwa--data-buffer (current-buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
            (let ((result (niwa-view-audit--evidence
                           '(:tool-use-id "failed-call" :command "printf hello"))))
              (with-current-buffer result
                (should (derived-mode-p 'special-mode))
                (should (string-match-p "Failed to start process: no such file: bash"
                                        (buffer-string)))
                (should-not (string-match-p "Execution produced no output"
                                            (buffer-string))))))
        (when-let* ((buffer (get-buffer "*niwa execution result*")))
          (kill-buffer buffer)))))

  :doc "read-only fallback labels a post-launch diagnostic without claiming spawn failure"
  (with-temp-buffer
    (insert (niwa--format-hook-audit-record
             '(:type execution-completion :tool-use-id "lost-call"
               :render-data (:termination unknown :execution-output ""
                             :execution-error "remote output write failed"))))
    (let ((niwa--data-buffer (current-buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
            (let ((result (niwa-view-audit--evidence
                           '(:tool-use-id "lost-call" :command "remote-write"))))
              (with-current-buffer result
                (should (string-match-p "Execution error: remote output write failed"
                                        (buffer-string)))
                (should-not (string-match-p "Failed to start process"
                                            (buffer-string))))))
        (when-let* ((buffer (get-buffer "*niwa execution result*")))
          (kill-buffer buffer))))))

(niwa-deftest niwa-view-audit-show-result/folded-turn ()
  ,test
  (test)
  :doc "a folded turn reveals its canonical Bash row instead of retained fallback"
  (let ((data (generate-new-buffer " *folded result source*"))
        (view (generate-new-buffer " *folded result view*"))
        (record '(:type execution-breadcrumb :tool-use-id "original"
                  :owner "/root" :execution-id "exec-1"
                  :command "printf result")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (propertize "(:name \"Bash\" :args (:command \"printf result\"))\n\nRESULT"
                                'gptel '(tool . "original"))))
          (with-current-buffer view
            (setq-local niwa--data-buffer data)
            (let ((source (cons 1 (with-current-buffer data (point-max)))))
              (insert "Assistant\n")
              (niwa-view--insert-rendered-tool
               (niwa-tool-exec--render-bash
                "Bash" '(:command "printf result") "RESULT"
                '(:execution-id "exec-1" :state completed :outcome success))
               source)
              (niwa-view--insert-hook-audit-block record)
              (let ((inhibit-read-only t))
                (add-text-properties 1 (point-max)
                                     '(niwa-view-turn-id 1
                                       niwa-view-turn-role assistant)))
              (niwa-view-transcript-tail-initialize)
              (goto-char (point-min))
              (niwa-view--collapse-turn)
              (should (get-text-property 1 'niwa-view-stash))
              (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                         (lambda () data))
                        ((symbol-function 'niwa-view-audit--evidence)
                         (lambda (&rest _)
                           (ert-fail "Folded canonical row was not revealed"))))
                (niwa-view-audit-show-result record))
              (should-not (get-text-property 1 'niwa-view-stash))
              (should (get-text-property (point) 'niwa-view-tool-use-id))
              (should (equal "original"
                             (get-text-property (point)
                                                'niwa-view-tool-use-id))))))
      (kill-buffer view)
      (kill-buffer data))))

(niwa-deftest niwa-view-audit-breadcrumb-folded-retry ()
  ,test
  (test)
  :doc "completion retry never duplicates a breadcrumb stashed by turn folding"
  (let ((data (generate-new-buffer " *folded retry source*"))
        (view (generate-new-buffer " *folded retry view*"))
        (facts '(:execution-id "exec-1" :state completed :outcome success))
        (record '(:type execution-breadcrumb :tool-use-id "original"
                  :owner "/root" :execution-id "exec-1"
                  :command "printf result"
                  :facts (:execution-id "exec-1" :state completed
                          :outcome success))))
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (propertize "(:name \"Bash\" :args (:command \"printf result\"))\n\nRESULT"
                                'gptel '(tool . "original")))
            (insert (niwa--format-hook-audit-record record)))
          (with-current-buffer view
            (setq-local niwa--data-buffer data)
            (let ((source (cons 1 (with-current-buffer data (point-max)))))
              (insert "Assistant\n")
              (niwa-view--insert-rendered-tool
               (niwa-tool-exec--render-bash
                "Bash" '(:command "printf result") "RESULT" facts)
               source)
              (niwa-view--insert-hook-audit-block record)
              (let ((inhibit-read-only t))
                (add-text-properties 1 (point-max)
                                     '(niwa-view-turn-id 1
                                       niwa-view-turn-role assistant)))
              (niwa-view-transcript-tail-initialize)
              (goto-char (point-min))
              (niwa-view--collapse-turn)
              (setq-local niwa-view--status-marker
                          (copy-marker (niwa-view-transcript-tail)))
              (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                         (lambda () data))
                        ((symbol-function 'niwa-view-stream--execution-view-buffer)
                         (lambda (_) view)))
                (should-not (niwa-view-audit-breadcrumb-present-p
                             '(:execution-id "exec-2" :owner "/root")
                             (niwa-view-transcript-tail)))
                (should-not (niwa-view-audit-breadcrumb-present-p
                             '(:execution-id "exec-1" :owner "/root/child")
                             (niwa-view-transcript-tail)))
                (niwa-view-stream--terminal-breadcrumb
                 (list :data-buffer data :tool-use-id "original" :facts facts)))
              (goto-char (point-min))
              (niwa-view--expand-turn)
              (should (= 1 (how-many "Background command \"printf result\" completed"
                                     (point-min) (niwa-view-transcript-tail))))
              (with-current-buffer data
                (should (= 1 (length (niwa-transcript-audit-records
                                      (buffer-string) 'execution-breadcrumb))))))))
      (kill-buffer view)
      (kill-buffer data))))

(niwa-deftest niwa-view-audit--result-position ()
  ,test
  (test)
  :doc "folded enclosing group locates source and nested child locates its own row"
  (let ((data (generate-new-buffer " *breadcrumb source*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (propertize "(:name \"ToolCall\")"
                                'gptel '(tool . "outer"))))
          (with-temp-buffer
            (let ((source (cons 1 (with-current-buffer data (point-max)))))
              (insert "group\nchild\n")
              (add-text-properties
               1 7 `(niwa-view-type tool-group niwa-view-source ,source
                                       niwa-view-collapsed t))
              (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                         (lambda () data)))
                (should (= 1 (niwa-view-audit--result-position "outer/1")))
                (add-text-properties
                 7 (point-max)
                 '(niwa-view-type tool-child
                   niwa-view-tool-child (:id "outer/1" :tool "Bash")))
                (should (= 7 (niwa-view-audit--result-position "outer/1")))))))
      (kill-buffer data)))
  :doc "direct ToolCall Bash resolves to its own folded summary instead of a missing child"
  (let ((data (generate-new-buffer " *direct Bash source*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (propertize
                     (concat "(:name \"ToolCall\")\n\noutput"
                             (niwa-tool-render-data-format
                              '(:kind ptc :outcome completed :direct-tool "Bash"
                                :calls ((:id "outer/1" :tool "Bash"
                                         :status success)))
                              "outer"))
                     'gptel '(tool . "outer"))))
          (with-temp-buffer
            (insert "Bash: command\n")
            (add-text-properties
             (point-min) (point-max)
             `(niwa-view-type tool-summary
               niwa-view-source
               ,(cons (with-current-buffer data (point-min))
                      (with-current-buffer data (point-max)))
               niwa-view-collapsed t))
            (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                       (lambda () data)))
              (should (= 1 (niwa-view-audit--result-position "outer/1"))))))
      (kill-buffer data)))
  :doc "a direct Bash in a nested ToolCall uses the enclosing child row"
  (let ((data (generate-new-buffer " *nested direct source*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (propertize "(:name \"ToolCall\")"
                                'gptel '(tool . "outer"))))
          (with-temp-buffer
            (insert "Bash: command\n")
            (add-text-properties
             (point-min) (point-max)
             `(niwa-view-type tool-child
               niwa-view-source
               ,(cons (with-current-buffer data (point-min))
                      (with-current-buffer data (point-max)))
               niwa-view-collapsed t
               niwa-view-tool-child
               (:id "outer/1" :tool "ToolCall"
                :render-data (:kind ptc :outcome completed :direct-tool "Bash"
                              :calls ((:id "outer/1/1" :tool "Bash"
                                       :status success))))))
            (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                       (lambda () data)))
              (should (= 1 (niwa-view-audit--result-position
                            "outer/1/1"))))))
      (kill-buffer data))))

(niwa-deftest niwa-view-audit-show-result/nested-group ()
  ,test
  (test)
  :doc "a full-ID Bash child is revealed through a folded group and ToolCall"
  (let ((data (generate-new-buffer " *nested group source*"))
        (view (generate-new-buffer " *nested group view*"))
        (expanded nil))
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (propertize "(:name \"ToolCall\")"
                                'gptel '(tool . "outer"))))
          (with-current-buffer view
            (let ((source (cons 1 (with-current-buffer data (point-max)))))
              (insert "group\n")
              (add-text-properties
               1 (point-max)
               `(niwa-view-type tool-group niwa-view-source ,source
                 niwa-view-collapsed t))
              (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                         (lambda () data))
                        ((symbol-function 'niwa-view-audit--evidence)
                         (lambda (_record)
                           (error "Found no nested result after %S: %S" expanded
                                  (buffer-string))))
                        ((symbol-function 'niwa-view-toggle-section)
                         (lambda ()
                           (let ((kind (get-text-property
                                        (point) 'niwa-view-type)))
                             (push kind expanded)
                             (put-text-property
                              (point) (1+ (point)) 'niwa-view-collapsed nil)
                             (unless (equal
                                      (plist-get (get-text-property
                                                  (point) 'niwa-view-tool-child)
                                                 :id)
                                      "outer/1/1")
                               (save-excursion
                                 (forward-line 1)
                                 (let ((start (point)))
                                   (insert (if (eq kind 'tool-group)
                                               "toolcall\n" "bash\n"))
                                   (add-text-properties
                                    start (point)
                                    `(niwa-view-type tool-child
                                      niwa-view-source
                                      ,(cons (car source) (cdr source))
                                      niwa-view-collapsed t
                                      niwa-view-tool-child
                                      ,(if (eq kind 'tool-group)
                                           '(:id "outer/1" :tool "ToolCall")
                                         '(:id "outer/1/1" :tool "Bash")))))))))))
                (niwa-view-audit-show-result
                 '(:tool-use-id "outer/1/1" :owner "/root"))
                (should (equal '(tool-child tool-child tool-group)
                               expanded))
                (should (equal "outer/1/1"
                               (plist-get (get-text-property
                                           (point) 'niwa-view-tool-child)
                                          :id)))
                (should-not (get-text-property
                             (point) 'niwa-view-collapsed))))))
      (kill-buffer view)
      (kill-buffer data))))

(niwa-deftest niwa-view-audit--select-source ()
  ,test
  (test)
  :doc "compacted completion locates archived row, not the later event target"
  (let* ((root (make-temp-file "niwa-breadcrumb-source-" t))
         (live (generate-new-buffer " *breadcrumb live*"))
         (archive (niwa-session-artifacts-segment-path root 1))
         (session (test-niwa-session-persistence--leased-session root
                                           :current-segment 3))
         selected)
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (niwa-session-artifacts-segment-path root 3))
            (setq-local niwa--session session))
          (with-temp-buffer
            (let ((niwa--data-buffer live))
              (dolist (case '(("original" "Bash" "original")
                              ("outer/1/1" "ToolCall" "outer")))
                (with-temp-buffer
                  (org-mode)
                  (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
                  (insert (propertize (format "(:name %S)\n" (nth 1 case))
                                      'gptel (cons 'tool (nth 2 case))))
                  (niwa-session-artifacts-stabilize-gptel-bounds)
                  (write-region (point-min) (point-max) archive nil 'silent))
                (niwa-view-audit-test--publish session archive)
                (should (equal '(readable missing readable)
                               (mapcar (lambda (entry) (plist-get entry :status))
                                       (niwa-session-artifacts-transcript-segments
                                        session live))))
                (setq selected nil)
                (let ((before (buffer-list)))
                  (cl-letf (((symbol-function 'niwa-view-go-to-segment)
                             (lambda (number) (setq selected number))))
                    (niwa-view-audit--select-source
                     (list :tool-use-id (car case)
                           :source-target "/later.chat.org")))
                  (should (= 1 selected))
                  (should-not (cl-set-difference (buffer-list) before)))))))
      (when (buffer-live-p live) (kill-buffer live))
      (niwa-session-durability-lease-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-view-audit--history-records-nested ()
  ,test
  (test)
  :doc "nested Bash input and stop interactions remain inspectable in history"
  (let ((data (generate-new-buffer " *nested execution history*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (propertize "(:name \"ToolCall\")\nouter result"
                                'gptel '(tool . "outer")))
            (insert (niwa-tool-render-data-format
                     '(:kind ptc :calls
                       ((:id "outer/1" :tool "ToolCall" :status success
                         :render-data
                         (:kind ptc :calls
                          ((:id "outer/1/1" :tool "Bash" :status success
                            :args (:command "cat")
                            :render-data (:execution-id "exec-deep"))
                           (:id "outer/1/2" :tool "WriteStdin" :status success
                            :args (:execution_id "exec-deep" :chars "input"))
                           (:id "outer/1/3" :tool "StopExecution" :status success
                            :args (:execution_id "exec-deep")))))))
                     "outer")))
          (with-temp-buffer
            (let ((niwa--data-buffer data))
              (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                         (lambda () data)))
                (with-current-buffer data
                  (let* ((segment
                          (cl-find-if (lambda (entry) (eq (car entry) 'tool))
                                      (niwa-transcript-segments
                                       (point-min) (point-max))))
                         (call (and segment
                                    (niwa-view--tool-call-parse
                                     data (cadr segment) (caddr segment)))))
                    (should call)
                    (should (niwa-tool-render-data-for-tool
                             data (plist-get call :tool-use-id)))))
                (should (equal '("Bash" "WriteStdin" "StopExecution")
                               (mapcar (lambda (call) (plist-get call :name))
                                       (niwa-view-audit--history-records
                                        "exec-deep"))))))))
      (kill-buffer data))))

(niwa-deftest niwa-view-audit-breadcrumb-archived-delivery ()
  ,test
  (test)
  :doc "an archived notice cannot suppress a later origin breadcrumb"
  (let* ((root (make-temp-file "niwa-mailbox-delivery-" t))
         (data (generate-new-buffer " *next mailbox segment*"))
         (archive (niwa-session-artifacts-segment-path root 1))
         (session (test-niwa-session-persistence--leased-session root
                                           :current-segment 3)))
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq buffer-file-name
                  (niwa-session-artifacts-segment-path root 3))
            (setq-local niwa--session session))
          (with-temp-file archive
            (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\" execution_id=\"exec-1\" outcome=\"success\">\n"
                    "<bash-execution execution_id=\"exec-1\" outcome=\"success\"/>\n"
                    "</agent-message>\n"))
          (niwa-view-audit-test--publish session archive)
          (with-temp-buffer
            (setq-local niwa--data-buffer data)
            (should (equal '(readable missing readable)
                           (mapcar (lambda (entry) (plist-get entry :status))
                                   (niwa-session-artifacts-transcript-segments
                                    session data))))
            (let ((before (buffer-list)))
              (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                         (lambda () data)))
                (should-not (niwa-view-audit-breadcrumb-present-p
                         '(:execution-id "exec-1" :owner "/root/child")
                         (point-max))))
              (should-not (cl-set-difference (buffer-list) before)))))
      (when (buffer-live-p data) (kill-buffer data))
      (niwa-session-durability-lease-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-view-audit--archived-breadcrumbs ()
  ,test
  (test)
  :doc "an unchanged archive is read once; rewriting it is read again"
  (let* ((root (make-temp-file "niwa-archive-cache-" t))
         (data (generate-new-buffer " *archive cache live*"))
         (archive (niwa-session-artifacts-segment-path root 1))
         (session (test-niwa-session-persistence--leased-session root
                                           :current-segment 2))
         (read (symbol-function
                'niwa-session-artifacts-read-transcript-segment))
         (reads 0)
         (delivery (lambda (id)
                     (niwa--format-hook-audit-record
                      (list :type 'execution-breadcrumb :execution-id id
                            :owner "/root/child" :facts '(:outcome success))))))
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq buffer-file-name
                  (niwa-session-artifacts-segment-path root 2))
            (setq-local niwa--session session))
          (with-temp-file archive
            (delay-mode-hooks (org-mode))
            (insert (funcall delivery "exec-1"))
            (niwa-session-artifacts-stabilize-gptel-bounds))
          (niwa-view-audit-test--publish session archive)
          (with-temp-buffer
            (setq-local niwa--data-buffer data)
            (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                       (lambda () data))
                      ((symbol-function
                        'niwa-session-artifacts-read-transcript-segment)
                       (lambda (&rest args)
                         (cl-incf reads)
                         (apply read args))))
              (dotimes (_ 3)
                (should (niwa-view-audit-breadcrumb-present-p
                         '(:execution-id "exec-1" :owner "/root/child")
                         (point-max)))
                (should-not (niwa-view-audit-breadcrumb-present-p
                             '(:execution-id "exec-2" :owner "/root/child")
                             (point-max))))
              (should (= 1 reads))
              ;; Same-second rewrites still change the identity by size.
              (with-temp-file archive
                (delay-mode-hooks (org-mode))
                (insert (funcall delivery "exec-2") (funcall delivery "exec-3"))
                (niwa-session-artifacts-stabilize-gptel-bounds))
              (niwa-view-audit-test--publish session archive)
              (should (niwa-view-audit-breadcrumb-present-p
                       '(:execution-id "exec-2" :owner "/root/child")
                       (point-max)))
              (should-not (niwa-view-audit-breadcrumb-present-p
                           '(:execution-id "exec-1" :owner "/root/child")
                           (point-max)))
              (should (= 2 reads)))))
      (when (buffer-live-p data) (kill-buffer data))
      (niwa-session-durability-lease-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-view-audit-breadcrumb-archived-projection ()
  ,test
  (test)
  :doc "historical projection consults earlier, not later, receiver segments"
  (let* ((root (make-temp-file "niwa-mailbox-projection-" t))
         (live (generate-new-buffer " *live mailbox session*"))
         (session (test-niwa-session-persistence--leased-session root
                                           :current-segment 4))
         earlier)
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (niwa-session-artifacts-segment-path root 4))
            (setq-local niwa--session session))
          (cl-loop for number from 1 to 3
                   do (with-temp-file
                          (niwa-session-artifacts-segment-path root number)
                        (delay-mode-hooks (org-mode))
                        (when (<= number 2)
                          (insert (niwa--format-hook-audit-record
                                   '(:type execution-breadcrumb :execution-id "old"
                                     :owner "/root/child" :facts (:outcome success)))))
                        (when (>= number 2)
                          (insert (niwa--format-hook-audit-record
                                   '(:type execution-breadcrumb :execution-id "new"
                                     :owner "/root/child" :facts (:outcome success)))))
                        (niwa-session-artifacts-stabilize-gptel-bounds)))
          (apply #'niwa-view-audit-test--publish session
                 (mapcar (lambda (number) (niwa-session-artifacts-segment-path root number)) '(1 2 3)))
          (let ((descriptors (niwa-session-artifacts-transcript-segments
                              session live)))
            (should (equal '(readable readable readable readable)
                           (mapcar (lambda (entry) (plist-get entry :status))
                                   descriptors)))
            (setq earlier (niwa-session-artifacts-read-transcript-segment
                           session (nth 1 descriptors))))
          (with-temp-buffer
            (setq-local niwa--data-buffer live)
            (let ((before (buffer-list)))
              (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                         (lambda () earlier))
                        ((symbol-function 'niwa-view-segments-current-number)
                         (lambda () 2)))
                (dolist (id '("old" "new"))
                  (let ((record (list :type 'execution-breadcrumb :execution-id id
                                      :owner "/root/child" :facts '(:outcome success))))
                    (unless (niwa-view-audit-breadcrumb-present-p record (point-max))
                      (niwa-view-audit--insert-breadcrumb record nil))))
                (should (= 1 (how-many "\\[Show result\\]"
                                       (point-min) (point-max))))
                (let* ((pos (text-property-any
                             (point-min) (point-max)
                             'niwa-view-type 'execution-breadcrumb))
                       (record (and pos (get-text-property
                                         pos 'niwa-view-execution-breadcrumb))))
                  (should (equal "new" (plist-get record :execution-id)))))
              (should-not (cl-set-difference (buffer-list) before)))))
      (when (buffer-live-p live) (kill-buffer live))
      (when (buffer-live-p earlier) (kill-buffer earlier))
      (niwa-session-durability-lease-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-view-audit--history-records-mailbox ()
  ,test
  (test)
  :doc "execution history retains independent child delivery details"
  (with-temp-buffer
    (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\" execution_id=\"exec-1\" outcome=\"success\">\n"
            "INDEPENDENT DELIVERY\n"
            "<bash-execution execution_id=\"exec-1\" outcome=\"success\"/>\n"
            "</agent-message>\n")
    (let ((niwa--data-buffer (current-buffer)))
      (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                 (lambda () niwa--data-buffer)))
        (let ((records (niwa-view-audit--history-records "exec-1")))
          (should (equal '("Execution delivery")
                         (mapcar (lambda (record) (plist-get record :name))
                                 records)))
          (should (string-match-p "INDEPENDENT DELIVERY"
                                  (plist-get (car records) :result)))
          (should (equal "/root/child"
                         (plist-get (plist-get (car records) :args)
                                    :sender)))))))
  :doc "metadata-looking body text cannot attribute a delivery to another execution"
  (with-temp-buffer
    (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\" "
            "execution_id=\"real\" outcome=\"failure\">\n"
            "<bash-execution execution_id=\"spoof\" outcome=\"success\"/>\n"
            "</agent-message>\n")
    (let ((niwa--data-buffer (current-buffer)))
      (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                 (lambda () niwa--data-buffer)))
        (should-not (niwa-view-audit--history-records "spoof"))
        (should (= 1 (length (niwa-view-audit--history-records "real"))))))))

(niwa-deftest niwa-view-audit--history-records-mailbox/linear-scan ()
  ,test
  (test)
  :doc "history scans each mailbox block once, including ignored Monitor events"
  (with-temp-buffer
    (require 'niwa-tools)
    (let ((blocks 0)
          (parses 0)
          (parser (symbol-function 'niwa-transcript--mailbox-any-block-at-point)))
      (dolist (type (append '(MAIL EXECUTION) (make-list 40 'MONITOR)
                           '(MAIL EXECUTION)))
        (insert (niwa-tools--message-delivery-block
                 (list :type type :sender "/root/child" :recipient "/root"
                       :execution-id "exec-1" :description "Watch build"
                       :meta '(:execution-id "exec-1" :outcome success)
                       :payload (format "record %d: exec-1 exec-1" blocks)))
                "\nplain text mentions exec-1\n")
        (cl-incf blocks))
      (let ((niwa--data-buffer (current-buffer)))
        (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                   (lambda () niwa--data-buffer))
                  ((symbol-function 'niwa-transcript--mailbox-any-block-at-point)
                   (lambda (limit)
                     (cl-incf parses)
                     (funcall parser limit))))
          (let ((records (niwa-view-audit--history-records "exec-1")))
            (should (equal '("record 1: exec-1 exec-1"
                             "record 43: exec-1 exec-1")
                           (mapcar (lambda (record)
                                     (string-trim (plist-get record :result)))
                                   records)))
            ;; One structural scan and one opener-facts parse per block at most.
            (should (<= parses (* 2 blocks)))))))))

(niwa-deftest niwa-view-audit--history-records-across-segments ()
  ,test
  (test)
  :doc "an archived Bash row retains later input, polls and delivery in order"
  (dolist (agent-p '(nil t))
    (let* ((root (make-temp-file "niwa-execution-history-" t))
           (agents (file-name-concat root "agents"))
           (live (generate-new-buffer " *execution history live*"))
           (archive-path (if agent-p
                             (file-name-concat
                              agents "child.compact-0001.chat.org")
                           (niwa-session-artifacts-segment-path root 1)))
           (later-path (when agent-p
                         (file-name-concat
                          agents "child.compact-0003.chat.org")))
           (session (test-niwa-session-persistence--leased-session root
                                             :current-segment 3
                                             :name "execution history")))
      (unwind-protect
          (progn
            (when agent-p
              (make-directory agents)
              (with-temp-file later-path (insert "# empty later archive\n")))
            (with-current-buffer live
              (setq buffer-file-name
                    (if agent-p
                        (file-name-concat agents "child.chat.org")
                      (niwa-session-artifacts-segment-path root 3)))
              (setq-local niwa--session session)
              (insert (propertize
                       "(:name \"WriteStdin\" :args (:execution_id \"exec-1\" :chars \"\"))\npolled\n"
                       'gptel '(tool . "poll")))
              (insert (propertize
                       "(:name \"WriteStdin\" :args (:execution_id \"exec-1\" :chars \"hi\"))\ninput sent\n"
                       'gptel '(tool . "input")))
              (insert "<agent-message type=\"EXECUTION\" sender=\"/root\" execution_id=\"exec-1\" outcome=\"success\">\n"
                      "DELIVERED\n"
                      "<bash-execution execution_id=\"exec-1\" outcome=\"success\"/>\n"
                      "</agent-message>\n")
              (should (equal '("WriteStdin" "WriteStdin")
                             (cl-loop for segment in (niwa-transcript-segments
                                                       (point-min) (point-max))
                                      when (eq (car segment) 'tool)
                                      collect (plist-get
                                               (niwa-view--tool-call-parse
                                                live (cadr segment) (caddr segment))
                                               :name)))))
            (with-temp-buffer
              (org-mode)
              (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
              (insert (propertize
                       (concat "(:name \"Bash\" :args (:command \"sleep 1\"))\n"
                               "yielded"
                               (niwa-tool-render-data-format
                                '(:execution-id "exec-1" :state running) "bash"))
                       'gptel '(tool . "bash")))
              (niwa-session-artifacts-stabilize-gptel-bounds)
              (write-region (point-min) (point-max) archive-path nil 'silent))
            (apply #'niwa-view-audit-test--publish session
                   (delq nil (list archive-path later-path)))
            (with-temp-buffer
              (setq-local niwa--data-buffer live)
              (let* ((descriptors
                      (niwa-session-artifacts-transcript-segments session live))
                     (before (buffer-list)))
                (should (equal (if agent-p '(1 3 nil) '(1 2 3))
                               (mapcar (lambda (entry) (plist-get entry :number))
                                       descriptors)))
                (should (equal (if agent-p '(readable readable readable)
                                 '(readable missing readable))
                               (mapcar (lambda (entry) (plist-get entry :status))
                                       descriptors)))
                (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                           (lambda () live)))
                  (let ((records (niwa-view-audit--history-records "exec-1")))
                    (should (equal '("Bash" "WriteStdin" "WriteStdin"
                                     "Execution delivery")
                                   (mapcar (lambda (record) (plist-get record :name))
                                           records)))
                    (should (equal "hi"
                                   (plist-get (plist-get (nth 2 records) :args) :chars)))
                    (should (string-match-p "DELIVERED"
                                            (plist-get (nth 3 records) :result)))))
                (should-not (cl-set-difference (buffer-list) before))
                (should (buffer-live-p live)))))
        (when (buffer-live-p live) (kill-buffer live))
        (niwa-session-durability-lease-release root session)
        (niwa-workspace-clear-registry)
        (delete-directory root t)))))

(niwa-deftest niwa-view-audit--owner-record ()
  ,test
  (test)
  :doc "forwarded child completion resolves its tool ID in an older segment"
  (let* ((root (make-temp-file "niwa-owner-segments-" t))
         (live (generate-new-buffer " *child current*"))
         (archive (niwa-session-artifacts-segment-path root 1))
         (session (test-niwa-session-persistence--leased-session root
                                           :current-segment 3))
         (forwarded '(:execution-id "child-exec" :owner "/root/child")))
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (niwa-session-artifacts-segment-path root 3))
            (setq-local niwa--session session))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (niwa--format-hook-audit-record
                     '(:type execution-breadcrumb :execution-id "child-exec"
                       :owner "/root/child" :tool-use-id "original-call"
                       :command "echo child")))
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) archive nil 'silent))
          (niwa-view-audit-test--publish session archive)
          (should (equal '(readable missing readable)
                         (mapcar (lambda (entry) (plist-get entry :status))
                                 (niwa-session-artifacts-transcript-segments
                                  session live))))
          (let ((before (buffer-list)))
            (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                       (lambda () live)))
              (should (equal "original-call"
                             (plist-get (niwa-view-audit--owner-record forwarded)
                                        :tool-use-id))))
            (should-not (cl-set-difference (buffer-list) before))))
      (when (buffer-live-p live) (kill-buffer live))
      (niwa-session-durability-lease-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-view-audit-show-result-nested-agent-owner ()
  ,test
  (test)
  :doc "a parent agent view opens its child's owning transcript for a result"
  (with-temp-buffer
    (setq-local niwa-view--agent-transcript-p t)
    (setq-local niwa-view--agent-path "/root/worker")
    (let (opened)
      (cl-letf (((symbol-function 'niwa-view-open-agent-transcript)
                 (lambda (owner) (setq opened owner)))
                ((symbol-function 'niwa-view-audit--select-source)
                 #'ignore)
                ((symbol-function 'niwa-view-audit--result-position)
                 #'ignore)
                ((symbol-function 'niwa-view-audit--evidence)
                 (lambda (_) 'missing-row)))
        (should (eq 'missing-row
                    (niwa-view-audit-show-result
                     '(:owner "/root/worker/child" :tool-use-id "child-tool"))))
        (should (equal "/root/worker/child" opened))))))

(niwa-deftest niwa-view--render-assistant-turn ()
  ,test
  (test)
  :doc "standalone audits after deliveries never become tool disclosures"
  (dolist (threshold '(1 100))
    (dolist (record '((:type provider-tool-batch-start :id "next")
                      (:type tool-result-rewrite :event "PostToolUse"
                       :original-result "raw" :updated-result "updated")))
      (with-temp-buffer
        (org-mode)
        (insert "#+begin_tool (Bash)\n"
                (propertize "(:name \"Bash\" :args nil)\n\nDone\n"
                            'gptel '(tool . "previous"))
                "#+end_tool\n"
                "<agent-message type=\"EXECUTION\" sender=\"/root\" recipient=\"/root\">\n"
                "Ready.\n</agent-message>\n"
                (niwa--format-hook-audit-record record)
                "#+begin_tool (Read)\n"
                (propertize "(:name \"Read\" :args (:file_path \"file.el\"))\n\nContents\n"
                            'gptel '(tool . "next"))
                "#+end_tool\n")
        (let ((data (current-buffer))
              (segments (niwa-transcript-segments (point-min) (point-max)))
              (niwa-view-tool-group-collapse-threshold threshold))
          (should (equal '(tool mailbox ignored tool) (mapcar #'car segments)))
          (with-temp-buffer
            (let ((inhibit-read-only t))
              (niwa-view--render-assistant-turn segments data))
            (let ((text (buffer-string)))
              (should-not (string-match-p "Tool (" text))
              (should-not (string-match-p "niwa-hook-audit" text))
              (when (eq (plist-get record :type) 'tool-result-rewrite)
                (should (string-match-p "hook changed tool result" text))))))))))

(provide 'test-niwa-view-render-audits)
;;; test-niwa-view-render-audits.el ends here
