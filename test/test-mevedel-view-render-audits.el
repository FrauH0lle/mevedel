;;; test-mevedel-view-render-audits.el --- Activity audit projection -*- lexical-binding: t -*-

;;; Commentary:

;; Standalone audits between deliveries and tools keep their own semantics.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-view-composer)
(require 'mevedel-view-disclosure)
(require 'mevedel-view-audit)
(require 'mevedel-view-agent)
(require 'mevedel-view-stream)
(require 'mevedel-transcript)
(require 'mevedel-session-artifacts)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-ptc)
(require 'mevedel-execution-transcript)

(mevedel-deftest mevedel-view-audit--breadcrumb-label ()
  ,test
  (test)
  :doc "terminal facts determine one compact breadcrumb without output"
  (should (equal "  ↳ Finished: ./run-tests  "
                 (mevedel-view-audit--breadcrumb-label
                  '(:command "./run-tests" :facts (:outcome success)))))
  (should (equal "  ↳ Failed: ./run-tests · exit 1  "
                 (mevedel-view-audit--breadcrumb-label
                  '(:command "./run-tests"
                    :facts (:outcome error :exit-code 1)))))
  (should (string-match-p "Stopped:"
                          (mevedel-view-audit--breadcrumb-label
                           '(:command "sleep" :facts (:termination owner-stopped)))))
  (let ((label (mevedel-view-audit--breadcrumb-label
                '(:command "sleep" :facts (:termination stopped
                                          :exit-code 15 :outcome failure)))))
    (should-not (string-match-p "exit 15" label))
    (should-not (string-match-p "signal 15" label)))
  (let ((label (mevedel-view-audit--breadcrumb-label
                '(:command "sleep" :facts (:termination signaled
                                          :exit-code 13 :outcome failure)))))
    (should (string-match-p "Signaled:.*signal 13" label))
    (should-not (string-match-p "exit 13" label)))
  (let ((label (mevedel-view-audit--breadcrumb-label
                '(:command "sleep" :facts (:termination interrupted
                                          :exit-code 2 :outcome failure)))))
    (should (string-match-p "Stopped:.*signal 2" label))
    (should-not (string-match-p "exit 2" label)))
  (should-not (string-match-p "\n"
                              (mevedel-view-audit--breadcrumb-label
                               '(:command "printf first\nsecond"
                                 :facts (:outcome success)))))
  :doc "forwarded child work names its agent except in that agent's own view"
  (should (string-match-p
           "Finished: make test · /root/worker  "
           (mevedel-view-audit--breadcrumb-label
            '(:owner "/root/worker" :command "make test"
              :facts (:outcome success)))))
  (let ((mevedel-view--agent-path "/root/worker"))
    (should-not (string-match-p
                 "/root/worker"
                 (mevedel-view-audit--breadcrumb-label
                  '(:owner "/root/worker" :command "make test"
                    :facts (:outcome success)))))))

(mevedel-deftest mevedel-view-audit-mailbox-breadcrumb ()
  ,test
  (test)
  :doc "forwarded execution uses final XML facts and retains bounded evidence"
  (let* ((record (mevedel-view-audit-mailbox-breadcrumb
                  (concat "output <bash-execution execution_id=\"fake\"/>\n"
                          "<bash-execution execution_id=\"real\" "
                          "command=\"make test\" output_path=\"artifact://result\" "
                          "outcome=\"error\" exit_code=\"1\" "
                          "output_bytes=\"128\" omitted_output_bytes=\"64\"/>")
                  "/root/worker")))
    (should (equal "real" (plist-get record :execution-id)))
    (should (equal "make test" (plist-get record :command)))
    (should (equal "artifact://result" (plist-get (plist-get record :facts)
                                                    :output-path)))
    (should (equal "/root/worker" (plist-get record :owner)))
    (should (equal 1 (plist-get (plist-get record :facts) :exit-code)))
    (should (equal 128 (plist-get (plist-get record :facts) :output-bytes)))
    (should (equal 64 (plist-get (plist-get record :facts) :omitted-output-bytes)))
    (should (string-match-p "output" (plist-get (plist-get record :facts)
                                                 :execution-output))))
  :doc "mailbox-only omitted bytes warn when the artifact is unavailable"
  (let ((record (mevedel-view-audit-mailbox-breadcrumb
                 (concat "bounded output\n<bash-execution execution_id=\"mailbox\" "
                         "output_path=\"artifact://missing\" "
                         "omitted_output_bytes=\"20\"/>")
                 "/root/worker")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
          (let ((result (mevedel-view-audit--evidence record)))
            (with-current-buffer result
              (should (string-match-p "Output truncated:.*retained preview"
                                      (buffer-string)))
              (should (string-match-p "bounded output" (buffer-string))))))
      (when-let* ((buffer (get-buffer "*mevedel execution result*")))
        (kill-buffer buffer)))))

(mevedel-deftest mevedel-view-audit--insert-breadcrumb ()
  ,test
  (test)
  :doc "durable audit draws a linked line and does not surface archives"
  (let* ((record '(:type execution-breadcrumb :execution-id "exec-1"
                   :tool-use-id "tool-1" :command "./test"
                   :facts (:outcome success)))
         (text (concat (mevedel--format-hook-audit-record
                        '(:type execution-completion :tool-use-id "old"))
                       (mevedel--format-hook-audit-record record))))
    (with-temp-buffer
      (should (= 1 (length (mevedel-view--hook-audit-records-from-text text))))
      (mevedel-view--insert-hook-audit-block record)
      (should (equal 'execution-breadcrumb
                     (get-text-property (point-min) 'mevedel-view-type)))
      (goto-char (point-min))
      (search-forward "[Show result]")
      (should (functionp (get-text-property (match-beginning 0)
                                            'mevedel-view-zone-activate))))))

(mevedel-deftest mevedel-view--decorate-agent-message-blocks ()
  ,test
  (test)
  :doc "execution mail becomes one linked breadcrumb without duplicated output"
  (with-temp-buffer
    (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
            "test output\n"
            "<bash-execution execution_id=\"exec-1\" outcome=\"error\" "
            "exit_code=\"2\"/>\n</agent-message>\n")
    (mevedel-view--decorate-agent-message-blocks (point-min) (point-max))
    (should (string-match-p "↳ Failed: Bash · exit 2 · /root/child  \\[Show result\\]"
                            (buffer-string)))
    (should-not (string-match-p "Bash completed\\|test output\\|<bash-execution"
                                (buffer-string)))
    (goto-char (point-min))
    (search-forward "[Show result]")
    (should (equal "exec-1"
                   (plist-get (get-text-property (match-beginning 0)
                                                 'mevedel-view-execution-breadcrumb)
                              :execution-id)))))

(mevedel-deftest mevedel-view--render-tool-call/preview-truncation ()
  ,test
  (test)
  :doc "the collapsed original Bash row warns when whole output exceeds its preview"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "Bash" :category "mevedel"
                           :renderer #'mevedel-tool-exec--render-bash))
    (with-temp-buffer
      (let* ((data (mevedel-execution-transcript-terminal-render-data
                    '(:facts (:state completed :outcome success
                              :omitted-output-bytes 0)
                      :whole-output "head\n[mevedel: tool output truncated; omitted 1000 chars]\ntail"
                      :whole-output-truncated-p t)))
             (row (mevedel-view--render-tool-call
                   (list :name "Bash" :args '(:command "produce-output")
                         :result "" :render-data data)
                   (current-buffer))))
        (should (plist-get row :initially-collapsed-p))
        (should (string-match-p "output truncated"
                                (mevedel-view--rendering-header-block row)))
        (should (string-match-p "omitted 1000 chars" (plist-get row :body)))))))

(mevedel-deftest mevedel-view--render-tool-call/direct-terminal ()
  ,test
  (test)
  :doc "a running Bash row retains its material sandbox summary on progress"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "Bash" :category "mevedel"
                           :renderer #'mevedel-tool-exec--render-bash))
    (with-temp-buffer
      (setq-local mevedel-view--execution-events (make-hash-table :test 'equal))
      (puthash "direct-progress"
               '(:type progress :facts (:state running :wall-time-seconds 2)
                 :output-tail "1\n2\n3\n4\n5\n6\n7")
               mevedel-view--execution-events)
      (let ((row (mevedel-view--render-tool-call
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
                                (mevedel-view--rendering-header-block row))))))
  :doc "direct ToolCall Bash reconciles retained failure and output with its child ID"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "Bash" :category "mevedel"
                           :renderer #'mevedel-tool-exec--render-bash))
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                           :renderer #'mevedel-tool-ptc--render))
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
             (before (mevedel-view--render-tool-call call data)))
        (should (string-match-p "running" (plist-get before :header)))
        (mevedel-execution-transcript-handle-event
         (list :type 'terminal :data-buffer data :tool-use-id "outer/1"
               :owner "/root"
               :whole-output "FINAL FAILURE\n<bash-execution marker=\"USER_OUTPUT\"/>\n"
               :facts '(:execution-id "exec-1" :command "make test"
                        :state completed :outcome failure :exit-code 2)))
        (let ((after (mevedel-view--render-tool-call call data)))
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
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "Bash" :category "mevedel"
                           :renderer #'mevedel-tool-exec--render-bash))
    (with-temp-buffer
      (let* ((data (current-buffer))
             (mevedel--data-buffer data)
             (facts '(:state completed :termination spawn-failed
                      :outcome failure :exit-code -1))
             (observation
              (list :facts facts :output ""
                    :error "spawning child process: no such file: bash"))
             (envelope (mevedel-tool-exec--observation-envelope observation)))
        (dolist (tool-id '("direct-fail" "outer/1"))
          (mevedel-execution-transcript-handle-event
           (list :type 'terminal :data-buffer data :tool-use-id tool-id
                 :owner "/root" :whole-output "" :observation observation
                 :facts facts)))
        (let* ((direct
                (mevedel-view--render-tool-call
                 (list :name "Bash" :tool-use-id "direct-fail"
                       :args '(:command "printf hello")
                       :result (plist-get envelope :result)
                       :render-data (plist-get envelope :render-data)) data))
               (nested
                (mevedel-view--child-call-rendering
                 (list :id "outer/1" :tool "Bash" :status 'error
                       :args '(:command "printf hello")
                       :result (plist-get envelope :result)
                       :render-data (plist-get envelope :render-data)))))
          (dolist (row (list direct nested))
            (should (string-match-p "Failed to start process: spawning child process"
                                    (plist-get row :body)))
            (should-not (string-match-p "execution_id=" (plist-get row :body))))))))
  :doc "a direct Bash keeps its terminal sandbox disclosure on the outer row"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "Bash" :category "mevedel"
                           :renderer #'mevedel-tool-exec--render-bash))
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                           :renderer #'mevedel-tool-ptc--render))
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
        (mevedel-execution-transcript-handle-event
         (list :type 'terminal :data-buffer data :tool-use-id "outer/1"
               :owner "/root" :whole-output "FINAL OUTPUT"
               :facts (list :execution-id "exec-1" :command "make test"
                            :state 'completed :outcome 'success :exit-code 0
                            :sandbox-summary summary)))
        (let ((after (mevedel-view--render-tool-call call data)))
          (should (equal summary (plist-get after :sandbox-summary)))
          (should (string-match-p "Sandbox:.*unrestricted"
                                  (mevedel-view--rendering-header-block after)))))))
  :doc "a live direct Bash child uses the cached progress tail and elapsed time"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "Bash" :category "mevedel"
                           :renderer #'mevedel-tool-exec--render-bash))
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                           :renderer #'mevedel-tool-ptc--render))
    (with-temp-buffer
      (let ((data (current-buffer)))
        (with-temp-buffer
          (setq-local mevedel-view--execution-events (make-hash-table :test 'equal))
          (puthash "outer/1"
                   '(:type progress :facts (:execution-id "exec-1" :state running
                                            :wall-time-seconds 7)
                     :output-tail "NEW OUTPUT")
                   mevedel-view--execution-events)
          (let ((row (mevedel-view--render-tool-call
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
            (should (string-match-p "running · 7.0s" (plist-get row :header)))
            (should (string-match-p "NEW OUTPUT" (plist-get row :body)))
            (should (equal 'unavailable
                           (plist-get (plist-get row :sandbox-summary) :sandbox)))
            (should-not (string-match-p "OLD OUTPUT" (plist-get row :body)))))))))

(mevedel-deftest mevedel-view-audit-breadcrumb-present-p ()
  ,test
  (test)
  :doc "duplicate forwarded deliveries project once per receiving transcript"
  (with-temp-buffer
    (insert (concat
             "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
             "<bash-execution execution_id=\"one\" outcome=\"success\"/>\n"
             "</agent-message>\n"
             "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
             "<bash-execution execution_id=\"one\" outcome=\"success\"/>\n"
             "</agent-message>\n"
             "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
             "<bash-execution execution_id=\"two\" outcome=\"success\"/>\n"
             "</agent-message>\n"))
    (mevedel-view--decorate-agent-message-blocks (point-min) (point-max))
    (should (= 2 (how-many "\\[Show result\\]" (point-min) (point-max))))
    (should-not (string-match-p "Bash completed\\|<agent-message" (buffer-string)))))

(mevedel-deftest mevedel-view-child-call-retained-completion ()
  ,test
  (test)
  :doc "nested ToolCall Bash uses the child completion after reload"
  (with-temp-buffer
    (insert (propertize "(:name \"ToolCall\")" 'gptel '(tool . "outer")))
    (let ((data (current-buffer)) received)
      (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                 (lambda () data))
                ((symbol-function 'mevedel-tool-get) (lambda (_name) t))
                ((symbol-function 'mevedel-execution-transcript-pending-render-data)
                 (lambda (_buffer id)
                   (setq received id)
                   '(:execution-id "exec-1" :execution-output "final output"
                     :status error)))
                ((symbol-function 'mevedel-view--invoke-renderer)
                 (lambda (_tool facts _args result)
                   (list :header (format "%s: %s" (plist-get facts :status)
                                         result)))))
        (let ((rendering (mevedel-view--child-call-rendering
                          '(:id "outer/1" :tool "Bash" :status running
                            :result "stale output"))))
          (should (equal "outer/1" received))
          (should (equal "outer/1" (plist-get rendering :tool-use-id)))
          (should (equal "exec-1" (plist-get rendering :execution-id)))
          (should (equal "error: final output" (plist-get rendering :header)))))))
  :doc "direct Bash nested in a ToolCall child reconciles the deepest result"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "Bash" :category "mevedel"
                           :renderer #'mevedel-tool-exec--render-bash))
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                           :renderer #'mevedel-tool-ptc--render))
    (with-temp-buffer
      (let ((data (current-buffer)))
        (mevedel-execution-transcript-handle-event
         (list :type 'terminal :data-buffer data :tool-use-id "outer/1/1"
               :owner "/root" :whole-output "DEEP FAILURE"
               :facts '(:execution-id "exec-deep" :command "make test"
                        :state completed :outcome failure :exit-code 2)))
        (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                   (lambda () data)))
          (let ((row (mevedel-view--child-call-rendering
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
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "Bash" :category "mevedel"
                           :renderer #'mevedel-tool-exec--render-bash))
    (with-temp-buffer
      (let ((data (current-buffer)))
        (with-temp-buffer
          (setq-local mevedel-view--execution-events (make-hash-table :test 'equal))
          (puthash "outer/1"
                   '(:type progress :facts (:execution-id "exec-1" :state running
                                            :wall-time-seconds 7)
                     :output-tail "NEW OUTPUT")
                   mevedel-view--execution-events)
          (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                     (lambda () data)))
            (let ((row (mevedel-view--child-call-rendering
                        '(:id "outer/1" :tool "Bash" :status success
                          :args (:command "make test") :result "OLD OUTPUT"
                          :render-data (:execution-id "exec-1" :state running
                                        :sandbox-summary
                                        (:attempt-count 1 :started-count 1
                                         :sandbox unavailable :filesystem unrestricted
                                         :network unrestricted)
                                        :wall-time-seconds 0.25)))))
              (should (string-match-p "running · 7.0s" (plist-get row :header)))
              (should (string-match-p "NEW OUTPUT" (plist-get row :body)))
              (should (equal 'unavailable
                             (plist-get (plist-get row :sandbox-summary) :sandbox)))
              (should-not (string-match-p "OLD OUTPUT" (plist-get row :body))))))))))

(mevedel-deftest mevedel-view-child-call-rendering-retained-output ()
  ,test
  (test)
  :doc "a reloaded nested Bash row uses its retained output, not a stale result"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "Bash" :category "mevedel"
                           :renderer #'mevedel-tool-exec--render-bash))
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                           :renderer #'mevedel-tool-ptc--render))
    (with-temp-buffer
      (let ((data (current-buffer)))
        (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                   (lambda () data)))
          (let ((row (mevedel-view--child-call-rendering
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

(mevedel-deftest mevedel-view-direct-toolcall-bash-history ()
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
            (insert (mevedel-tool-render-data-format
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
              (setq-local mevedel--data-buffer data)
              (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                         (lambda () data))
                        ((symbol-function 'mevedel-view-audit-show-history)
                         (lambda (id) (setq opened id))))
                (should (mevedel-view-audit--history-records "exec-1"))
                (dolist (terminal '(nil t))
                  (when terminal
                    (mevedel-execution-transcript-handle-event
                     (list :type 'terminal :data-buffer data
                           :tool-use-id "outer/1" :owner "/root"
                           :whole-output "FINAL OUTPUT"
                           :facts '(:execution-id "exec-1" :command "make test"
                                    :state completed :outcome success
                                    :exit-code 0))))
                  (erase-buffer)
                  (mevedel-view--render-expanded-body
                   (mevedel-view--render-tool-call call data)
                   (cons 1 (with-current-buffer data (point-max))))
                  (goto-char (point-min))
                  (should (search-forward "[Execution history]" nil t))
                  (funcall (get-text-property (match-beginning 0)
                                              'mevedel-view-zone-activate))
                  (should (equal "exec-1" opened)))))))
      (kill-buffer data))))

(mevedel-deftest mevedel-view-child-call-history ()
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
            (setq-local mevedel-view--data-turn-start nil)
            (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                       (lambda () data))
                      ((symbol-function 'mevedel-view-audit-show-history)
                       (lambda (id) (setq opened id))))
              (mevedel-view--render-expanded-body
               '(:vtype tool-child :tool-use-id "outer/1"
                 :execution-id "exec-child" :header "Bash: printf child"
                 :body "$ printf child\n\nchild")
               (cons 1 (with-current-buffer data (point-max))))
              (goto-char (point-min))
              (should (search-forward "[Execution history]" nil t))
              (funcall (get-text-property (match-beginning 0)
                                          'mevedel-view-zone-activate))
              (should (equal "exec-child" opened)))))
      (kill-buffer data))))

(mevedel-deftest mevedel-view-audit-show-control-result ()
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
            (insert (mevedel-tool-render-data-format
                     '(:execution-id "exec-deep" :state complete) "poll"))
            (insert (propertize
                     "(:name \"ToolCall\")\noriginal result"
                     'gptel '(tool . "outer")))
            (insert (mevedel-tool-render-data-format
                     '(:kind ptc :calls
                       ((:id "outer/1" :tool "ToolCall" :status success
                         :render-data
                         (:kind ptc :direct-tool "Bash"
                          :calls ((:id "outer/1/1" :tool "Bash"
                                   :status success :args (:command "cat")
                                   :render-data (:execution-id "exec-deep"
                                                 :state running)))))))
                     "outer")))
          (should (mevedel-tool-render-data-for-tool data "outer"))
          (with-temp-buffer
            (setq-local mevedel--data-buffer data)
            (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                       (lambda () data))
                      ((symbol-function 'mevedel-view-audit-show-result)
                       (lambda (record) (setq opened record))))
              (dolist (control '(("WriteStdin" (:execution_id "exec-deep"
                                                 :chars "secret\n") input)
                                 ("StopExecution" (:execution_id "exec-deep")
                                  stop)))
                (erase-buffer)
                (let* ((name (nth 0 control))
                       (args (nth 1 control))
                       (row (mevedel-tool-exec--render-bash
                             name args "ok"
                             (list :execution-id "exec-deep" :status 'success
                                   :execution-control (nth 2 control)))))
                  (insert (mevedel-view--rendering-header-line row))
                  (goto-char (point-min))
                  (should (search-forward "[Show result]" nil t))
                  (should-not (string-match-p "secret" (buffer-string)))
                  (funcall (get-text-property (match-beginning 0)
                                              'mevedel-view-zone-activate))
                  (should (equal "outer/1/1"
                                 (plist-get opened :tool-use-id)))))))
      (kill-buffer data)))))

(mevedel-deftest mevedel-view-audit-show-control-result-missing ()
  ,test
  (test)
  :doc "control link without its original row reports missing retained evidence"
  (let ((data (generate-new-buffer " *missing control source*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--data-buffer data)
          (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                     (lambda () data))
                    ((symbol-function 'display-buffer) #'identity))
            (let ((evidence (mevedel-view-audit-show-control-result
                             "exec-absent")))
              (with-current-buffer evidence
                (should (derived-mode-p 'special-mode))
                (should (string-match-p
                         "Original execution row and retained output are unavailable"
                         (buffer-string)))))))
      (kill-buffer data)
      (when-let* ((buffer (get-buffer "*mevedel execution result*")))
        (kill-buffer buffer)))))

(mevedel-deftest mevedel-view-audit-show-control-result-retained ()
  ,test
  (test)
  :doc "a control link retains trusted completion output after its source row is gone"
  (let ((data (generate-new-buffer " *retained control source*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-breadcrumb :execution-id "exec-deep"
                       :tool-use-id "outer/1/1" :command "cat"
                       :facts (:execution-output "RETAINED FINAL OUTPUT")))))
          (with-temp-buffer
            (setq-local mevedel--data-buffer data)
            (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                       (lambda () data))
                      ((symbol-function 'display-buffer) #'identity))
              (let ((evidence (mevedel-view-audit-show-control-result
                               "exec-deep")))
                (with-current-buffer evidence
                  (should (derived-mode-p 'special-mode))
                  (should (string-match-p "RETAINED FINAL OUTPUT"
                                          (buffer-string))))))))
      (kill-buffer data)
      (when-let* ((buffer (get-buffer "*mevedel execution result*")))
        (kill-buffer buffer)))))

(mevedel-deftest mevedel-view-audit-show-control-result-running-archive ()
  ,test
  (test)
  :doc "archived running Bash retains source identity and output for control links"
  (let ((data (generate-new-buffer " *running archived control source*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-archive :tool-use-id "original"
                       :render-data (:execution-id "exec-1" :command "sleep 30"
                                     :state running
                                     :execution-output "RETAINED RUNNING OUTPUT")))))
          (with-temp-buffer
            (setq-local mevedel--data-buffer data)
            (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                       (lambda () data))
                      ((symbol-function 'display-buffer) #'identity))
              (let ((evidence (mevedel-view-audit-show-control-result "exec-1")))
                (with-current-buffer evidence
                  (should (derived-mode-p 'special-mode))
                  (should (string-match-p "RETAINED RUNNING OUTPUT"
                                          (buffer-string))))))))
      (kill-buffer data)
      (when-let* ((buffer (get-buffer "*mevedel execution result*")))
        (kill-buffer buffer)))))

(mevedel-deftest mevedel-view-audit-show-control-result-older-segment ()
  ,test
  (test)
  :doc "input and stop result links find retained output in an older segment"
  (let* ((root (make-temp-file "mevedel-control-older-" t))
         (archive (mevedel-session-artifacts-segment-path root 2))
         (live (generate-new-buffer " *control latest segment*"))
         (session (mevedel-session--create :save-path root
                                           :authority-mode 'pid-lock
                                           :current-segment 3)))
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (mevedel-session-artifacts-segment-path root 3))
            (setq-local mevedel--session session))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "original"
                       :render-data (:execution-id "exec-older"
                                     :execution-output "RETAINED OLDER OUTPUT"))))
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) archive nil 'silent))
          (should (equal '(missing readable readable)
                         (mapcar (lambda (entry) (plist-get entry :status))
                                 (mevedel-session-artifacts-transcript-segments
                                  session live))))
          (with-temp-buffer
            (setq-local mevedel--data-buffer live)
            (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                       (lambda () live))
                      ((symbol-function 'display-buffer) #'identity))
              (dolist (control '(("WriteStdin" (:execution_id "exec-older"
                                                 :chars "input\n") input)
                                 ("StopExecution" (:execution_id "exec-older")
                                  stop)))
                (let ((inhibit-read-only t)) (erase-buffer))
                (insert (mevedel-view--rendering-header-line
                         (mevedel-tool-exec--render-bash
                          (nth 0 control) (nth 1 control) "ok"
                          (list :execution-id "exec-older" :status 'success
                                :execution-control (nth 2 control)))))
                (goto-char (point-min))
                (should (search-forward "[Show result]" nil t))
                (let ((before (buffer-list)))
                  (let ((evidence (funcall (get-text-property
                                            (match-beginning 0)
                                            'mevedel-view-zone-activate))))
                    (with-current-buffer evidence
                      (should (derived-mode-p 'special-mode))
                      (should (string-match-p "RETAINED OLDER OUTPUT"
                                              (buffer-string)))))
                  (should-not (cl-set-difference (buffer-list)
                                                 (cons (get-buffer
                                                        "*mevedel execution result*")
                                                       before)))))))
      (when (buffer-live-p live) (kill-buffer live))
      (when-let* ((buffer (get-buffer "*mevedel execution result*")))
        (kill-buffer buffer))
      (delete-directory root t)))))

(mevedel-deftest mevedel-view-audit-show-control-result-older-source ()
  ,test
  (test)
  :doc "a control link identifies a nested Bash source row in an older segment"
  (let* ((root (make-temp-file "mevedel-control-source-" t))
         (archive (mevedel-session-artifacts-segment-path root 1))
         (live (generate-new-buffer " *control source latest*"))
         (session (mevedel-session--create :save-path root
                                           :authority-mode 'pid-lock
                                           :current-segment 3))
         opened)
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (mevedel-session-artifacts-segment-path root 3))
            (setq-local mevedel--session session))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (propertize "(:name \"ToolCall\")\noriginal result"
                                'gptel '(tool . "outer")))
            (insert (mevedel-tool-render-data-format
                     '(:kind ptc :calls
                       ((:id "outer/1" :tool "Bash" :status success
                         :render-data (:execution-id "exec-older"
                                       :execution-output "ORIGINAL OUTPUT"))))
                     "outer"))
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) archive nil 'silent))
          (with-temp-buffer
            (setq-local mevedel--data-buffer live)
            (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                       (lambda () live))
                      ((symbol-function 'mevedel-view-audit-show-result)
                       (lambda (record) (setq opened record))))
              (mevedel-view-audit-show-control-result "exec-older")
              (should (equal "outer/1" (plist-get opened :tool-use-id)))
              (should (equal "exec-older" (plist-get opened :execution-id))))))
      (when (buffer-live-p live) (kill-buffer live))
      (delete-directory root t))))

(mevedel-deftest mevedel-view-audit-show-control-result-agent-archive ()
  ,test
  (test)
  :doc "an agent's compacted source still opens its retained Bash output"
  (let* ((root (make-temp-file "mevedel-control-agent-" t))
         (agent-dir (file-name-concat root "agents"))
         (archive (file-name-concat agent-dir
                                    "worker.compact-0001.chat.org"))
         (later (file-name-concat agent-dir
                                  "worker.compact-0003.chat.org"))
         (live (generate-new-buffer " *agent control current*"))
         (view (generate-new-buffer " *agent control view*"))
         (mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
         (session (mevedel-session--create :save-path root
                                           :authority-mode 'pid-lock
                                           :current-segment 7)))
    (unwind-protect
        (progn
          (make-directory agent-dir t)
          (with-current-buffer live
            (setq buffer-file-name (file-name-concat agent-dir
                                                      "worker.chat.org"))
            (setq-local mevedel--session session)
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert "#+begin_tool (ToolCall)\n")
            (insert (propertize
                     (concat "(:name \"ToolCall\")\nMODEL-ENVELOPE"
                             (mevedel-tool-render-data-format
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
                             (mevedel-tool-render-data-format
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
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) archive nil 'silent))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (propertize "SECOND-ARCHIVE\n" 'gptel 'response))
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) later nil 'silent))
          (mevedel-view--setup view live
                               '(:agent-transcript-p t :agent-path "/root/worker"))
          (mevedel-tool-register
           (mevedel-tool--create :name "Bash" :category "mevedel"
                                 :renderer #'mevedel-tool-exec--render-bash))
          (mevedel-tool-register
           (mevedel-tool--create :name "ToolCall" :category "mevedel"
                                 :renderer #'mevedel-tool-ptc--render))
          (with-current-buffer view
            (mevedel-view--full-rerender)
            (let ((project (symbol-function 'mevedel-view-render-project-segment)))
              (cl-letf (((symbol-function 'mevedel-view-render-project-segment)
                         (lambda (source state direction)
                           (if source
                               (error "Archive projection failed")
                             (funcall project source state direction)))))
                (should-error (mevedel-view-go-to-segment 1)
                              :type 'error)))
            (should-not (mevedel-view-segments-current-number))
            (should buffer-read-only)
            (mevedel-view-go-to-segment 1)
            (should-not (string-search "AGENT-OUTPUT" (buffer-string)))
            (mevedel-view-return-to-latest-segment)
            (should (equal '((readable . nil) (readable . nil) (readable . t))
                           (mapcar (lambda (descriptor)
                                     (cons (plist-get descriptor :status)
                                           (plist-get descriptor :current-p)))
                                   (mevedel-session-artifacts-transcript-segments
                                    session live))))
            (let ((before (buffer-list)))
              (let* ((control
                      (mevedel-view--rendering-header-line
                       (mevedel-tool-exec--render-bash
                        "WriteStdin"
                        '(:execution_id "exec-agent-old" :chars "input\n")
                        "ok"
                        '(:execution-id "exec-agent-old" :status success
                          :execution-control input :control-succeeded-p t))))
                     (link (string-match "\\[Show result\\]" control)))
                (should link)
                (funcall (get-text-property
                          link 'mevedel-view-zone-activate control)))
              (should (= 1 (mevedel-view-segments-current-number)))
              (should buffer-read-only)
              (should (string-search "Bash: run agent job" (buffer-string)))
              (should (string-match-p "AGENT-OUTPUT" (buffer-string)))
              (should-not (string-match-p "MODEL-ENVELOPE" (buffer-string)))
              (should-not (get-buffer "*mevedel execution result*"))
              (should (= 1 (length (cl-set-difference (buffer-list) before))))
              (let* ((control
                      (mevedel-view--rendering-header-line
                       (mevedel-tool-exec--render-bash
                        "WriteStdin"
                        '(:execution_id "exec-agent-deep" :chars "input\n")
                        "ok"
                        '(:execution-id "exec-agent-deep" :status success
                          :execution-control input :control-succeeded-p t))))
                     (link (string-match "\\[Show result\\]" control)))
                (should link)
                (funcall (get-text-property
                          link 'mevedel-view-zone-activate control)))
              (should (= 1 (mevedel-view-segments-current-number)))
              (should (string-search "Bash: run deep job" (buffer-string)))
              (should (string-search "DEEP-CANONICAL-OUTPUT" (buffer-string)))
              (should-not (string-search "INNER-MODEL-ENVELOPE" (buffer-string)))
              (should-not (get-buffer "*mevedel execution result*"))
              (mevedel-view-next-segment)
              (should (= 3 (mevedel-view-segments-current-number)))
              (should (string-match-p "SECOND-ARCHIVE" (buffer-string)))
              (mevedel-view-go-to-segment 1)
              (should (= 1 (mevedel-view-segments-current-number)))
              (cl-letf (((symbol-function 'mevedel-view-render-project-segment)
                         (lambda (&rest _args)
                           (error "Projection and recovery failed"))))
                (should-error (mevedel-view-return-to-latest-segment)
                              :type 'error))
              (should (= 1 (mevedel-view-segments-current-number)))
              (should buffer-read-only)
              (with-current-buffer live
                (insert (propertize "(:name \"ToolCall\")\n"
                                    'gptel '(tool . "outer"))))
              (mevedel-view-audit--select-source
               '(:tool-use-id "outer/1"))
              (should-not (mevedel-view-segments-current-number))
              (should buffer-read-only)
              (mevedel-view-previous-segment)
              (should (= 3 (mevedel-view-segments-current-number)))
              (mevedel-view-return-to-latest-segment)
              (should buffer-read-only)
              (should-not (cl-set-difference (buffer-list) before)))))
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p live) (kill-buffer live))
      (when-let* ((buffer (get-buffer "*mevedel execution result*")))
        (kill-buffer buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-view-audit-show-result-forwarded-agent-archive ()
  ,test
  (test)
  :doc "a forwarded completion opens its agent's surviving source row"
  (save-window-excursion
    (let* ((root (make-temp-file "mevedel-forwarded-result-" t))
           (agent-dir (file-name-concat root "agents"))
           (live-path (file-name-concat agent-dir "worker.chat.org"))
           (archive (file-name-concat agent-dir
                                      "worker.compact-0001.chat.org"))
           (mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
           (session
            (mevedel-session--create
             :authority-mode 'pid-lock :save-path root
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
                               (mevedel-tool-render-data-format
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
              (mevedel-session-artifacts-stabilize-gptel-bounds)
              (write-region (point-min) (point-max) archive nil 'silent))
            (with-temp-file live-path
              (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"))
            (mevedel-tool-register
             (mevedel-tool--create :name "Bash" :category "mevedel"
                                   :renderer #'mevedel-tool-exec--render-bash))
            (mevedel-tool-register
             (mevedel-tool--create :name "ToolCall" :category "mevedel"
                                   :renderer #'mevedel-tool-ptc--render))
            (mevedel-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local mevedel--session session)
                (insert (propertize "Parent transcript\n" 'gptel 'response))
                (insert (mevedel--format-hook-audit-record
                         '(:type execution-breadcrumb
                           :owner "/root/worker"
                           :execution-id "exec-forwarded"
                           :tool-use-id "forwarded/1"
                           :command "run forwarded job"
                           :facts (:outcome success)))))
              (with-current-buffer view-buf
                (switch-to-buffer view-buf)
                (mevedel-view--full-rerender)
                (goto-char (point-min))
                (should (search-forward "[Show result]" nil t))
                (funcall (get-text-property
                          (match-beginning 0) 'mevedel-view-zone-activate))
                (setq agent-view (window-buffer (selected-window)))
                (should-not (eq agent-view view-buf))
                (with-current-buffer agent-view
                  (setq agent-data mevedel--data-buffer)
                  (should mevedel-view--agent-transcript-p)
                  (should (eq session (buffer-local-value
                                       'mevedel--session agent-data)))
                  (should (= 1 (mevedel-view-segments-current-number)))
                  (should buffer-read-only)
                  (should-not (get-text-property (point)
                                                 'mevedel-view-collapsed))
                  (should (string-search "Bash: run forwarded job"
                                         (buffer-string)))
                  (should (string-search "FORWARDED-OUTPUT" (buffer-string)))
                  (should-not (string-search "MODEL-ENVELOPE" (buffer-string)))
                  (should-not (get-buffer "*mevedel execution result*"))))))
        (when (buffer-live-p agent-view) (kill-buffer agent-view))
        (when (buffer-live-p agent-data)
          (with-current-buffer agent-data (set-buffer-modified-p nil))
          (kill-buffer agent-data))
        (when-let* ((evidence (get-buffer "*mevedel execution result*")))
          (kill-buffer evidence))
        (delete-directory root t)))))

(mevedel-deftest mevedel-view-audit-archived-terminal-rendering ()
  ,test
  (test)
  :doc "opening an archived running row reads the later live completion"
  (let ((live (generate-new-buffer " *terminal live segment*"))
        (archive (generate-new-buffer " *original archived segment*")))
    (unwind-protect
        (progn
          (with-current-buffer live
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "original"
                       :render-data (:execution-id "exec-1" :status error
                                     :outcome failure :exit-code 2
                                     :execution-output "FINAL OUTPUT")))))
          (with-temp-buffer
            (setq-local mevedel--data-buffer live)
            (let ((rendering (mevedel-view--render-tool-call
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

(mevedel-deftest mevedel-view-audit-intermediate-terminal-rendering ()
  ,test
  (test)
  :doc "a later compaction still renders an archived row with its terminal facts"
  (let* ((root (make-temp-file "mevedel-terminal-segments-" t))
         (live (generate-new-buffer " *third segment*"))
         (archive (generate-new-buffer " *first source segment*"))
         (first-path (mevedel-session-artifacts-segment-path root 1))
         (middle-path (mevedel-session-artifacts-segment-path root 2))
         (session (mevedel-session--create :save-path root
                                           :authority-mode 'pid-lock
                                           :current-segment 3)))
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (mevedel-session-artifacts-segment-path root 3))
            (setq-local mevedel--session session))
          (with-current-buffer archive
            (insert "# original running Bash segment\n")
            (write-region (point-min) (point-max) first-path nil 'silent))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "original"
                       :render-data (:execution-id "exec-1" :status error
                                     :outcome failure :exit-code 2
                                     :execution-output "FINAL AFTER ROTATION"))))
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) middle-path nil 'silent))
          (with-temp-buffer
            (setq-local mevedel--data-buffer live)
            (let ((descriptors (mevedel-session-artifacts-transcript-segments
                                session live)))
              (should (equal '(readable readable readable)
                             (mapcar (lambda (entry) (plist-get entry :status))
                                     descriptors)))
              (let ((restored (mevedel-session-artifacts-read-transcript-segment
                               session (nth 1 descriptors))))
                (unwind-protect
                    (with-current-buffer restored
                      (let ((facts (plist-get
                                    (car (mevedel-transcript-audit-records
                                          (buffer-string) 'execution-completion))
                                    :render-data)))
                        (should (equal "FINAL AFTER ROTATION"
                                       (plist-get facts :execution-output)))
                        (should (eq 'error (plist-get facts :status)))
                        (should (= 2 (plist-get facts :exit-code)))))
                  (kill-buffer restored))))
            (let ((before (buffer-list)))
              (let ((rendering (mevedel-view--render-tool-call
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
      (delete-directory root t))))

(mevedel-deftest mevedel-view-audit-intermediate-terminal-evidence ()
  ,test
  (test)
  :doc "missing original row uses terminal evidence from an older segment"
  (let* ((root (make-temp-file "mevedel-terminal-evidence-" t))
         (middle-path (mevedel-session-artifacts-segment-path root 2))
         (live (generate-new-buffer " *latest without Bash*"))
         (session (mevedel-session--create :save-path root
                                           :authority-mode 'pid-lock
                                           :current-segment 3)))
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (mevedel-session-artifacts-segment-path root 3))
            (setq-local mevedel--session session))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "original"
                       :render-data (:execution-id "exec-1"
                                     :execution-output "RETAINED FINAL S2"))))
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) middle-path nil 'silent))
          (with-temp-buffer
            (setq-local mevedel--data-buffer live)
            (should (equal '(missing readable readable)
                           (mapcar (lambda (entry) (plist-get entry :status))
                                   (mevedel-session-artifacts-transcript-segments
                                    session live))))
            (let ((before (buffer-list)))
              (cl-letf (((symbol-function 'display-buffer) #'identity))
                (let ((evidence (mevedel-view-audit-show-result
                                 '(:owner "/root" :execution-id "exec-1"
                                   :tool-use-id "original"))))
                  (with-current-buffer evidence
                    (should (derived-mode-p 'special-mode))
                    (should (string-match-p "RETAINED FINAL S2"
                                            (buffer-string))))
                  (should-not (cl-set-difference (buffer-list)
                                                 (cons evidence before))))))))
      (when (buffer-live-p live) (kill-buffer live))
      (when-let* ((buffer (get-buffer "*mevedel execution result*")))
        (kill-buffer buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-view-audit-agent-compact-archive-evidence ()
  ,test
  (test)
  :doc "a child result remains inspectable after two agent compactions"
  (let* ((root (make-temp-file "mevedel-child-archives-" t))
         (agents (file-name-concat root "agents"))
         (canonical (file-name-concat agents "worker.chat.org"))
         (archive-1 (file-name-concat agents "worker.compact-0001.chat.org"))
         (archive-2 (file-name-concat agents "worker.compact-0002.chat.org"))
         (data (generate-new-buffer " *agent current after compaction*"))
         (session (mevedel-session--create :save-path root
                                           :authority-mode 'pid-lock
                                           :current-segment 1)))
    (unwind-protect
        (progn
          (make-directory agents)
          (with-temp-file archive-1 (insert "# older empty agent archive\n"))
          (with-temp-buffer
            (org-mode)
            (require 'gptel)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "original"
                       :render-data (:execution-id "child-exec"
                                     :execution-output "FINAL CHILD ARCHIVE"))))
            (should (eq 'mevedel-hook-audit
                        (get-text-property (1- (point)) 'gptel)))
            (should (gptel--get-buffer-bounds))
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (should (string-match-p "mevedel-hook-audit"
                                    (or (org-entry-get (point-min)
                                                       "GPTEL_BOUNDS") "")))
            (write-region (point-min) (point-max) archive-2 nil 'silent))
          (with-current-buffer data
            (setq buffer-file-name canonical)
            (setq-local mevedel--session session)
            (let* ((descriptors (mevedel-session-artifacts-transcript-segments
                                 session data))
                   (older (mevedel-session-artifacts-read-transcript-segment
                           session (cadr descriptors))))
              (should (= 3 (length descriptors)))
              (unwind-protect
                  (with-current-buffer older
                    (should (mevedel-transcript-audit-records
                             (buffer-string) 'execution-completion))
                    (should (equal "FINAL CHILD ARCHIVE"
                                   (plist-get
                                    (mevedel-execution-transcript-pending-render-data
                                     older "original") :execution-output))))
                (kill-buffer older))))
          (with-temp-buffer
            (setq-local mevedel--data-buffer data)
            (setq-local mevedel-view--agent-transcript-p t)
            (setq-local mevedel-view--agent-path "/root/worker")
            (cl-letf (((symbol-function 'display-buffer) #'identity))
              (let ((evidence (mevedel-view-audit-show-result
                               '(:owner "/root/worker" :tool-use-id "original"
                                 :execution-id "child-exec" :command "child"))))
                (with-current-buffer evidence
                  (should (string-match-p "FINAL CHILD ARCHIVE"
                                          (buffer-string))))
              (delete-file archive-1)
              (should (= 2 (length
                            (mevedel-session-artifacts-transcript-segments
                             session data))))
              (with-current-buffer (mevedel-view-audit-show-result
                                    '(:owner "/root/worker"
                                      :tool-use-id "original"
                                      :execution-id "child-exec" :command "child"))
                (should (string-match-p "FINAL CHILD ARCHIVE"
                                        (buffer-string))))))))
      (when (buffer-live-p data) (kill-buffer data))
      (when-let* ((evidence (get-buffer "*mevedel execution result*")))
        (kill-buffer evidence))
      (delete-directory root t))))

(mevedel-deftest mevedel-view-audit-retained-child-evidence ()
  ,test
  (test)
  :doc "missing nested source row opens its durable terminal output read-only"
  (with-temp-buffer
    (insert (mevedel--format-hook-audit-record
             '(:type execution-completion :tool-use-id "outer/0"
               :render-data (:execution-output "final child output"))))
    (let ((mevedel--data-buffer (current-buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
            (let ((result (mevedel-view-audit--evidence
                           '(:tool-use-id "outer/0" :command "echo child"))))
              (with-current-buffer result
                (should (derived-mode-p 'special-mode))
                (should (string-match-p "final child output" (buffer-string))))))
        (when-let* ((buffer (get-buffer "*mevedel execution result*")))
          (kill-buffer buffer))))))

(mevedel-deftest mevedel-view-audit-retained-archived-evidence ()
  ,test
  (test)
  :doc "missing archived row opens retained completion output without an artifact"
  (with-temp-buffer
    (insert (mevedel--format-hook-audit-record
             '(:type execution-completion :tool-use-id "old-call"
               :render-data (:execution-output "archived output"))))
    (let ((mevedel--data-buffer (current-buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
            (let ((result (mevedel-view-audit--evidence
                           '(:tool-use-id "old-call" :command "echo archived"))))
              (with-current-buffer result
                (should (derived-mode-p 'special-mode))
                (should (string-match-p "archived output" (buffer-string))))))
        (when-let* ((buffer (get-buffer "*mevedel execution result*")))
          (kill-buffer buffer))))))

(mevedel-deftest mevedel-view-audit-retained-disclosures ()
  ,test
  (test)
  :doc "read-only preview warns about missing bytes and preserves sandbox boundary"
  (with-temp-buffer
    (insert (mevedel--format-hook-audit-record
             '(:type execution-completion :tool-use-id "bounded-call"
               :render-data (:execution-output "partial output"
                             :output-preview-truncated-p t
                             :omitted-output-bytes 123
                             :sandbox-summary (:attempt-count 1 :started-count 1
                                               :sandbox off :filesystem unrestricted
                                               :network unrestricted)))))
    (let ((mevedel--data-buffer (current-buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
            (let ((result (mevedel-view-audit--evidence
                           '(:tool-use-id "bounded-call" :command "echo many"))))
              (with-current-buffer result
                (should (derived-mode-p 'special-mode))
                (should (string-match-p "Sandbox:.*ran without confinement"
                                        (buffer-string)))
                (should (string-match-p "Output truncated:.*retained preview"
                                        (buffer-string)))
                (should (string-match-p "partial output" (buffer-string))))))
        (when-let* ((buffer (get-buffer "*mevedel execution result*")))
          (kill-buffer buffer)))))
  :doc "a readable artifact hides preview warnings, but not spool limit warnings"
  (let ((physical (make-temp-file "mevedel-complete-output-"))
        (data (generate-new-buffer " *complete execution source*")))
    (unwind-protect
        (progn
          (with-temp-file physical (insert "complete output"))
          (with-current-buffer data
            (setq-local mevedel--session (mevedel-session--create :name "complete")))
          (with-temp-buffer
            (let ((mevedel--data-buffer data))
              (cl-letf (((symbol-function 'mevedel-resource-prepare)
                         (lambda (&rest _) (list :physical-path physical)))
                        ((symbol-function 'display-buffer) (lambda (buffer) buffer)))
                (let ((result (mevedel-view-audit--evidence
                               '(:command "cat complete"
                                 :facts (:output-path "artifact://executions/complete"
                                         :output-bytes 15
                                         :output-preview-truncated-p t
                                         :execution-output "partial")))))
                  (with-current-buffer result
                    (should (string-match-p "complete output" (buffer-string)))
                    (should-not (string-match-p "Output truncated"
                                                (buffer-string)))))
                (let ((result (mevedel-view-audit--evidence
                               '(:command "cat stale"
                                 :facts (:output-path "artifact://executions/stale"
                                         :output-bytes 30
                                         :output-preview-truncated-p t
                                         :execution-output "newer partial")))))
                  (with-current-buffer result
                    (should (string-match-p "Output truncated:.*retained artifact"
                                            (buffer-string)))
                    (should (string-match-p "complete output" (buffer-string)))))
                (let ((result (mevedel-view-audit--evidence
                               '(:command "cat capped"
                                 :facts (:output-path "artifact://executions/capped"
                                         :termination output-limit
                                         :execution-output "partial")))))
                  (with-current-buffer result
                    (should (string-match-p "Output truncated:.*output limit"
                                            (buffer-string)))
                    (should (string-match-p "complete output"
                                            (buffer-string)))))))))
      (when-let* ((buffer (get-buffer "*mevedel execution result*")))
        (kill-buffer buffer))
      (kill-buffer data)
      (delete-file physical))))

(mevedel-deftest mevedel-view-audit-retained-empty-evidence ()
  ,test
  (test)
  :doc "empty terminal output is retained evidence, not missing output"
  (with-temp-buffer
    (insert (mevedel--format-hook-audit-record
             '(:type execution-completion :tool-use-id "old-call"
               :render-data (:state completed :outcome success :exit-code 0
                             :execution-output ""))))
    (let ((mevedel--data-buffer (current-buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
            (let ((result (mevedel-view-audit--evidence
                           '(:tool-use-id "old-call" :command "sleep .3; true"))))
              (with-current-buffer result
                (should (derived-mode-p 'special-mode))
                (should (equal "sleep .3; true\n\nExecution produced no output.\n"
                               (buffer-string))))))
        (when-let* ((buffer (get-buffer "*mevedel execution result*")))
          (kill-buffer buffer)))))
  :doc "an empty retained output artifact also shows a zero-output result"
  (let ((physical (make-temp-file "mevedel-empty-execution-output-"))
        (data (generate-new-buffer " *empty execution source*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local mevedel--session (mevedel-session--create :name "empty")))
          (with-temp-buffer
            (let ((mevedel--data-buffer data))
              (cl-letf (((symbol-function 'mevedel-resource-prepare)
                         (lambda (&rest _) (list :physical-path physical)))
                        ((symbol-function 'display-buffer) (lambda (buffer) buffer)))
                (let ((result (mevedel-view-audit--evidence
                               '(:command "true"
                                 :facts (:output-path "artifact://executions/empty")))))
                  (with-current-buffer result
                    (should (equal "true\n\nExecution produced no output.\n"
                                   (buffer-string))))))))
      (when-let* ((buffer (get-buffer "*mevedel execution result*")))
        (kill-buffer buffer))
      (kill-buffer data)
      (delete-file physical))))

  :doc "read-only fallback discloses a retained launch error without stdout"
  (with-temp-buffer
    (insert (mevedel--format-hook-audit-record
             '(:type execution-completion :tool-use-id "failed-call"
               :render-data (:termination spawn-failed :execution-output ""
                             :execution-error "no such file: bash"))))
    (let ((mevedel--data-buffer (current-buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
            (let ((result (mevedel-view-audit--evidence
                           '(:tool-use-id "failed-call" :command "printf hello"))))
              (with-current-buffer result
                (should (derived-mode-p 'special-mode))
                (should (string-match-p "Failed to start process: no such file: bash"
                                        (buffer-string)))
                (should-not (string-match-p "Execution produced no output"
                                            (buffer-string))))))
        (when-let* ((buffer (get-buffer "*mevedel execution result*")))
          (kill-buffer buffer)))))

  :doc "read-only fallback labels a post-launch diagnostic without claiming spawn failure"
  (with-temp-buffer
    (insert (mevedel--format-hook-audit-record
             '(:type execution-completion :tool-use-id "lost-call"
               :render-data (:termination unknown :execution-output ""
                             :execution-error "remote output write failed"))))
    (let ((mevedel--data-buffer (current-buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer)))
            (let ((result (mevedel-view-audit--evidence
                           '(:tool-use-id "lost-call" :command "remote-write"))))
              (with-current-buffer result
                (should (string-match-p "Execution error: remote output write failed"
                                        (buffer-string)))
                (should-not (string-match-p "Failed to start process"
                                            (buffer-string))))))
        (when-let* ((buffer (get-buffer "*mevedel execution result*")))
          (kill-buffer buffer))))))

(mevedel-deftest mevedel-view-audit-show-result/folded-turn ()
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
            (setq-local mevedel--data-buffer data)
            (let ((source (cons 1 (with-current-buffer data (point-max)))))
              (insert "Assistant\n")
              (mevedel-view--insert-rendered-tool
               (mevedel-tool-exec--render-bash
                "Bash" '(:command "printf result") "RESULT"
                '(:execution-id "exec-1" :state completed :outcome success))
               source)
              (mevedel-view--insert-hook-audit-block record)
              (let ((inhibit-read-only t))
                (add-text-properties 1 (point-max)
                                     '(mevedel-view-turn-id 1
                                       mevedel-view-turn-role assistant)))
              (setq-local mevedel-view--input-marker (copy-marker (point-max)))
              (goto-char (point-min))
              (mevedel-view--collapse-turn)
              (should (get-text-property 1 'mevedel-view-stash))
              (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                         (lambda () data))
                        ((symbol-function 'mevedel-view-audit--evidence)
                         (lambda (&rest _)
                           (ert-fail "Folded canonical row was not revealed"))))
                (mevedel-view-audit-show-result record))
              (should-not (get-text-property 1 'mevedel-view-stash))
              (should (get-text-property (point) 'mevedel-view-tool-use-id))
              (should (equal "original"
                             (get-text-property (point)
                                                'mevedel-view-tool-use-id))))))
      (kill-buffer view)
      (kill-buffer data))))

(mevedel-deftest mevedel-view-audit-breadcrumb-folded-retry ()
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
            (insert (mevedel--format-hook-audit-record record)))
          (with-current-buffer view
            (setq-local mevedel--data-buffer data)
            (let ((source (cons 1 (with-current-buffer data (point-max)))))
              (insert "Assistant\n")
              (mevedel-view--insert-rendered-tool
               (mevedel-tool-exec--render-bash
                "Bash" '(:command "printf result") "RESULT" facts)
               source)
              (mevedel-view--insert-hook-audit-block record)
              (let ((inhibit-read-only t))
                (add-text-properties 1 (point-max)
                                     '(mevedel-view-turn-id 1
                                       mevedel-view-turn-role assistant)))
              (setq-local mevedel-view--input-marker (copy-marker (point-max)))
              (goto-char (point-min))
              (mevedel-view--collapse-turn)
              (setq-local mevedel-view--status-marker (copy-marker (point-max)))
              (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                         (lambda () data))
                        ((symbol-function 'mevedel-view-stream--execution-view-buffer)
                         (lambda (_) view)))
                (should-not (mevedel-view-audit-breadcrumb-present-p
                             '(:execution-id "exec-2" :owner "/root")
                             (point-max)))
                (should-not (mevedel-view-audit-breadcrumb-present-p
                             '(:execution-id "exec-1" :owner "/root/child")
                             (point-max)))
                (mevedel-view-stream--terminal-breadcrumb
                 (list :data-buffer data :tool-use-id "original" :facts facts)))
              (goto-char (point-min))
              (mevedel-view--expand-turn)
              (should (= 1 (how-many "Finished: printf result"
                                     (point-min) (point-max))))
              (with-current-buffer data
                (should (= 1 (length (mevedel-transcript-audit-records
                                      (buffer-string) 'execution-breadcrumb))))))))
      (kill-buffer view)
      (kill-buffer data))))

(mevedel-deftest mevedel-view-audit--result-position ()
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
               1 7 `(mevedel-view-type tool-group mevedel-view-source ,source
                                       mevedel-view-collapsed t))
              (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                         (lambda () data)))
                (should (= 1 (mevedel-view-audit--result-position "outer/1")))
                (add-text-properties
                 7 (point-max)
                 '(mevedel-view-type tool-child
                   mevedel-view-tool-child (:id "outer/1" :tool "Bash")))
                (should (= 7 (mevedel-view-audit--result-position "outer/1")))))))
      (kill-buffer data)))
  :doc "direct ToolCall Bash resolves to its own folded summary instead of a missing child"
  (let ((data (generate-new-buffer " *direct Bash source*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (propertize
                     (concat "(:name \"ToolCall\")\n\noutput"
                             (mevedel-tool-render-data-format
                              '(:kind ptc :outcome completed :direct-tool "Bash"
                                :calls ((:id "outer/1" :tool "Bash"
                                         :status success)))
                              "outer"))
                     'gptel '(tool . "outer"))))
          (with-temp-buffer
            (insert "Bash: command\n")
            (add-text-properties
             (point-min) (point-max)
             `(mevedel-view-type tool-summary
               mevedel-view-source
               ,(cons (with-current-buffer data (point-min))
                      (with-current-buffer data (point-max)))
               mevedel-view-collapsed t))
            (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                       (lambda () data)))
              (should (= 1 (mevedel-view-audit--result-position "outer/1"))))))
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
             `(mevedel-view-type tool-child
               mevedel-view-source
               ,(cons (with-current-buffer data (point-min))
                      (with-current-buffer data (point-max)))
               mevedel-view-collapsed t
               mevedel-view-tool-child
               (:id "outer/1" :tool "ToolCall"
                :render-data (:kind ptc :outcome completed :direct-tool "Bash"
                              :calls ((:id "outer/1/1" :tool "Bash"
                                       :status success))))))
            (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                       (lambda () data)))
              (should (= 1 (mevedel-view-audit--result-position
                            "outer/1/1"))))))
      (kill-buffer data))))

(mevedel-deftest mevedel-view-audit-show-result/nested-group ()
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
               `(mevedel-view-type tool-group mevedel-view-source ,source
                 mevedel-view-collapsed t))
              (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                         (lambda () data))
                        ((symbol-function 'mevedel-view-audit--evidence)
                         (lambda (_record)
                           (error "Found no nested result after %S: %S" expanded
                                  (buffer-string))))
                        ((symbol-function 'mevedel-view-toggle-section)
                         (lambda ()
                           (let ((kind (get-text-property
                                        (point) 'mevedel-view-type)))
                             (push kind expanded)
                             (put-text-property
                              (point) (1+ (point)) 'mevedel-view-collapsed nil)
                             (unless (equal
                                      (plist-get (get-text-property
                                                  (point) 'mevedel-view-tool-child)
                                                 :id)
                                      "outer/1/1")
                               (save-excursion
                                 (forward-line 1)
                                 (let ((start (point)))
                                   (insert (if (eq kind 'tool-group)
                                               "toolcall\n" "bash\n"))
                                   (add-text-properties
                                    start (point)
                                    `(mevedel-view-type tool-child
                                      mevedel-view-source
                                      ,(cons (car source) (cdr source))
                                      mevedel-view-collapsed t
                                      mevedel-view-tool-child
                                      ,(if (eq kind 'tool-group)
                                           '(:id "outer/1" :tool "ToolCall")
                                         '(:id "outer/1/1" :tool "Bash")))))))))))
                (mevedel-view-audit-show-result
                 '(:tool-use-id "outer/1/1" :owner "/root"))
                (should (equal '(tool-child tool-child tool-group)
                               expanded))
                (should (equal "outer/1/1"
                               (plist-get (get-text-property
                                           (point) 'mevedel-view-tool-child)
                                          :id)))
                (should-not (get-text-property
                             (point) 'mevedel-view-collapsed))))))
      (kill-buffer view)
      (kill-buffer data))))

(mevedel-deftest mevedel-view-audit--select-source ()
  ,test
  (test)
  :doc "compacted completion locates archived row, not the later event target"
  (let* ((root (make-temp-file "mevedel-breadcrumb-source-" t))
         (live (generate-new-buffer " *breadcrumb live*"))
         (archive (mevedel-session-artifacts-segment-path root 1))
         (session (mevedel-session--create :save-path root
                                           :authority-mode 'pid-lock
                                           :current-segment 3))
         selected)
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (mevedel-session-artifacts-segment-path root 3))
            (setq-local mevedel--session session))
          (with-temp-buffer
            (let ((mevedel--data-buffer live))
              (dolist (case '(("original" "Bash" "original")
                              ("outer/1/1" "ToolCall" "outer")))
                (with-temp-buffer
                  (org-mode)
                  (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
                  (insert (propertize (format "(:name %S)\n" (nth 1 case))
                                      'gptel (cons 'tool (nth 2 case))))
                  (mevedel-session-artifacts-stabilize-gptel-bounds)
                  (write-region (point-min) (point-max) archive nil 'silent))
                (should (equal '(readable missing readable)
                               (mapcar (lambda (entry) (plist-get entry :status))
                                       (mevedel-session-artifacts-transcript-segments
                                        session live))))
                (setq selected nil)
                (let ((before (buffer-list)))
                  (cl-letf (((symbol-function 'mevedel-view-go-to-segment)
                             (lambda (number) (setq selected number))))
                    (mevedel-view-audit--select-source
                     (list :tool-use-id (car case)
                           :source-target "/later.chat.org")))
                  (should (= 1 selected))
                  (should-not (cl-set-difference (buffer-list) before)))))))
      (when (buffer-live-p live) (kill-buffer live))
      (delete-directory root t))))

(mevedel-deftest mevedel-view-audit--history-records-nested ()
  ,test
  (test)
  :doc "nested Bash input and stop interactions remain inspectable in history"
  (let ((data (generate-new-buffer " *nested execution history*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (insert (propertize "(:name \"ToolCall\")\nouter result"
                                'gptel '(tool . "outer")))
            (insert (mevedel-tool-render-data-format
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
            (let ((mevedel--data-buffer data))
              (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                         (lambda () data)))
                (with-current-buffer data
                  (let* ((segment
                          (cl-find-if (lambda (entry) (eq (car entry) 'tool))
                                      (mevedel-transcript-segments
                                       (point-min) (point-max))))
                         (call (and segment
                                    (mevedel-view--tool-call-parse
                                     data (cadr segment) (caddr segment)))))
                    (should call)
                    (should (mevedel-tool-render-data-for-tool
                             data (plist-get call :tool-use-id)))))
                (should (equal '("Bash" "WriteStdin" "StopExecution")
                               (mapcar (lambda (call) (plist-get call :name))
                                       (mevedel-view-audit--history-records
                                        "exec-deep"))))))))
      (kill-buffer data))))

(mevedel-deftest mevedel-view-audit-breadcrumb-archived-delivery ()
  ,test
  (test)
  :doc "a later mailbox retry cannot repeat an archived receiver breadcrumb"
  (let* ((root (make-temp-file "mevedel-mailbox-delivery-" t))
         (data (generate-new-buffer " *next mailbox segment*"))
         (archive (mevedel-session-artifacts-segment-path root 1))
         (session (mevedel-session--create :save-path root
                                           :authority-mode 'pid-lock
                                           :current-segment 3)))
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq buffer-file-name
                  (mevedel-session-artifacts-segment-path root 3))
            (setq-local mevedel--session session))
          (with-temp-file archive
            (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                    "<bash-execution execution_id=\"exec-1\" outcome=\"success\"/>\n"
                    "</agent-message>\n"))
          (with-temp-buffer
            (setq-local mevedel--data-buffer data)
            (should (equal '(readable missing readable)
                           (mapcar (lambda (entry) (plist-get entry :status))
                                   (mevedel-session-artifacts-transcript-segments
                                    session data))))
            (let ((before (buffer-list)))
              (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                         (lambda () data)))
                (should (mevedel-view-audit-breadcrumb-present-p
                         '(:execution-id "exec-1" :owner "/root/child")
                         (point-max))))
              (should-not (cl-set-difference (buffer-list) before)))))
      (when (buffer-live-p data) (kill-buffer data))
      (delete-directory root t))))

(mevedel-deftest mevedel-view-audit-breadcrumb-archived-projection ()
  ,test
  (test)
  :doc "historical projection consults earlier, not later, receiver segments"
  (let* ((root (make-temp-file "mevedel-mailbox-projection-" t))
         (live (generate-new-buffer " *live mailbox session*"))
         (session (mevedel-session--create :save-path root
                                           :authority-mode 'pid-lock
                                           :current-segment 4))
         earlier)
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (mevedel-session-artifacts-segment-path root 4))
            (setq-local mevedel--session session))
          (cl-loop for number from 1 to 3
                   do (with-temp-file
                          (mevedel-session-artifacts-segment-path root number)
                        (when (<= number 2)
                          (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                                  "<bash-execution execution_id=\"old\" outcome=\"success\"/>\n"
                                  "</agent-message>\n"))
                        (when (>= number 2)
                          (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                                  "<bash-execution execution_id=\"new\" outcome=\"success\"/>\n"
                                  "</agent-message>\n"))))
          (let ((descriptors (mevedel-session-artifacts-transcript-segments
                              session live)))
            (should (equal '(readable readable readable readable)
                           (mapcar (lambda (entry) (plist-get entry :status))
                                   descriptors)))
            (setq earlier (mevedel-session-artifacts-read-transcript-segment
                           session (nth 1 descriptors))))
          (with-temp-buffer
            (setq-local mevedel--data-buffer live)
            (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                    "<bash-execution execution_id=\"old\" outcome=\"success\"/>\n"
                    "</agent-message>\n"
                    "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                    "<bash-execution execution_id=\"new\" outcome=\"success\"/>\n"
                    "</agent-message>\n")
            (let ((before (buffer-list)))
              (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                         (lambda () earlier))
                        ((symbol-function 'mevedel-view-segments-current-number)
                         (lambda () 2)))
                (mevedel-view--decorate-agent-message-blocks
                 (point-min) (point-max))
                (should (= 1 (how-many "\\[Show result\\]"
                                       (point-min) (point-max))))
                (let* ((pos (text-property-any
                             (point-min) (point-max)
                             'mevedel-view-type 'execution-breadcrumb))
                       (record (and pos (get-text-property
                                         pos 'mevedel-view-execution-breadcrumb))))
                  (should (equal "new" (plist-get record :execution-id)))))
              (should-not (cl-set-difference (buffer-list) before)))))
      (when (buffer-live-p live) (kill-buffer live))
      (when (buffer-live-p earlier) (kill-buffer earlier))
      (delete-directory root t))))

(mevedel-deftest mevedel-view-audit--history-records-mailbox ()
  ,test
  (test)
  :doc "execution history retains independent child delivery details"
  (with-temp-buffer
    (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
            "INDEPENDENT DELIVERY\n"
            "<bash-execution execution_id=\"exec-1\" outcome=\"success\"/>\n"
            "</agent-message>\n")
    (let ((mevedel--data-buffer (current-buffer)))
      (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                 (lambda () mevedel--data-buffer)))
        (let ((records (mevedel-view-audit--history-records "exec-1")))
          (should (equal '("Execution delivery")
                         (mapcar (lambda (record) (plist-get record :name))
                                 records)))
          (should (string-match-p "INDEPENDENT DELIVERY"
                                  (plist-get (car records) :result)))
          (should (equal "/root/child"
                         (plist-get (plist-get (car records) :args)
                                    :sender))))))))

(mevedel-deftest mevedel-view-audit--history-records-across-segments ()
  ,test
  (test)
  :doc "an archived Bash row retains later input, polls and delivery in order"
  (dolist (agent-p '(nil t))
    (let* ((root (make-temp-file "mevedel-execution-history-" t))
           (agents (file-name-concat root "agents"))
           (live (generate-new-buffer " *execution history live*"))
           (archive-path (if agent-p
                             (file-name-concat
                              agents "child.compact-0001.chat.org")
                           (mevedel-session-artifacts-segment-path root 1)))
           (later-path (when agent-p
                         (file-name-concat
                          agents "child.compact-0003.chat.org")))
           (session (mevedel-session--create :save-path root
                                             :authority-mode 'pid-lock
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
                      (mevedel-session-artifacts-segment-path root 3)))
              (setq-local mevedel--session session)
              (insert (propertize
                       "(:name \"WriteStdin\" :args (:execution_id \"exec-1\" :chars \"\"))\npolled\n"
                       'gptel '(tool . "poll")))
              (insert (propertize
                       "(:name \"WriteStdin\" :args (:execution_id \"exec-1\" :chars \"hi\"))\ninput sent\n"
                       'gptel '(tool . "input")))
              (insert "<agent-message type=\"EXECUTION\" sender=\"/root\">\n"
                      "DELIVERED\n"
                      "<bash-execution execution_id=\"exec-1\" outcome=\"success\"/>\n"
                      "</agent-message>\n")
              (should (equal '("WriteStdin" "WriteStdin")
                             (cl-loop for segment in (mevedel-transcript-segments
                                                       (point-min) (point-max))
                                      when (eq (car segment) 'tool)
                                      collect (plist-get
                                               (mevedel-view--tool-call-parse
                                                live (cadr segment) (caddr segment))
                                               :name)))))
            (with-temp-buffer
              (org-mode)
              (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
              (insert (propertize
                       (concat "(:name \"Bash\" :args (:command \"sleep 1\"))\n"
                               "yielded"
                               (mevedel-tool-render-data-format
                                '(:execution-id "exec-1" :state running) "bash"))
                       'gptel '(tool . "bash")))
              (mevedel-session-artifacts-stabilize-gptel-bounds)
              (write-region (point-min) (point-max) archive-path nil 'silent))
            (with-temp-buffer
              (setq-local mevedel--data-buffer live)
              (let* ((descriptors
                      (mevedel-session-artifacts-transcript-segments session live))
                     (before (buffer-list)))
                (should (equal (if agent-p '(1 3 nil) '(1 2 3))
                               (mapcar (lambda (entry) (plist-get entry :number))
                                       descriptors)))
                (should (equal (if agent-p '(readable readable readable)
                                 '(readable missing readable))
                               (mapcar (lambda (entry) (plist-get entry :status))
                                       descriptors)))
                (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                           (lambda () live)))
                  (let ((records (mevedel-view-audit--history-records "exec-1")))
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
        (delete-directory root t)))))

(mevedel-deftest mevedel-view-audit--owner-record ()
  ,test
  (test)
  :doc "forwarded child completion resolves its tool ID in an older segment"
  (let* ((root (make-temp-file "mevedel-owner-segments-" t))
         (live (generate-new-buffer " *child current*"))
         (archive (mevedel-session-artifacts-segment-path root 1))
         (session (mevedel-session--create :save-path root
                                           :authority-mode 'pid-lock
                                           :current-segment 3))
         (forwarded '(:execution-id "child-exec" :owner "/root/child")))
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq buffer-file-name
                  (mevedel-session-artifacts-segment-path root 3))
            (setq-local mevedel--session session))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-breadcrumb :execution-id "child-exec"
                       :owner "/root/child" :tool-use-id "original-call"
                       :command "echo child")))
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) archive nil 'silent))
          (should (equal '(readable missing readable)
                         (mapcar (lambda (entry) (plist-get entry :status))
                                 (mevedel-session-artifacts-transcript-segments
                                  session live))))
          (let ((before (buffer-list)))
            (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                       (lambda () live)))
              (should (equal "original-call"
                             (plist-get (mevedel-view-audit--owner-record forwarded)
                                        :tool-use-id))))
            (should-not (cl-set-difference (buffer-list) before))))
      (when (buffer-live-p live) (kill-buffer live))
      (delete-directory root t))))

(mevedel-deftest mevedel-view-audit-show-result-nested-agent-owner ()
  ,test
  (test)
  :doc "a parent agent view opens its child's owning transcript for a result"
  (with-temp-buffer
    (setq-local mevedel-view--agent-transcript-p t)
    (setq-local mevedel-view--agent-path "/root/worker")
    (let (opened)
      (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                 (lambda (owner) (setq opened owner)))
                ((symbol-function 'mevedel-view-audit--select-source)
                 #'ignore)
                ((symbol-function 'mevedel-view-audit--result-position)
                 #'ignore)
                ((symbol-function 'mevedel-view-audit--evidence)
                 (lambda (_) 'missing-row)))
        (should (eq 'missing-row
                    (mevedel-view-audit-show-result
                     '(:owner "/root/worker/child" :tool-use-id "child-tool"))))
        (should (equal "/root/worker/child" opened))))))

(mevedel-deftest mevedel-view--render-assistant-turn ()
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
                (mevedel--format-hook-audit-record record)
                "#+begin_tool (Read)\n"
                (propertize "(:name \"Read\" :args (:file_path \"file.el\"))\n\nContents\n"
                            'gptel '(tool . "next"))
                "#+end_tool\n")
        (let ((data (current-buffer))
              (segments (mevedel-transcript-segments (point-min) (point-max)))
              (mevedel-view-tool-group-collapse-threshold threshold))
          (should (equal '(tool mailbox ignored tool) (mapcar #'car segments)))
          (with-temp-buffer
            (let ((inhibit-read-only t))
              (mevedel-view--render-assistant-turn segments data))
            (let ((text (buffer-string)))
              (should-not (string-match-p "Tool (" text))
              (should-not (string-match-p "mevedel-hook-audit" text))
              (when (eq (plist-get record :type) 'tool-result-rewrite)
                (should (string-match-p "hook changed tool result" text))))))))))

(provide 'test-mevedel-view-render-audits)
;;; test-mevedel-view-render-audits.el ends here
