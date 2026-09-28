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
                          "outcome=\"error\" exit_code=\"1\"/>")
                  "/root/worker")))
    (should (equal "real" (plist-get record :execution-id)))
    (should (equal "make test" (plist-get record :command)))
    (should (equal "artifact://result" (plist-get (plist-get record :facts)
                                                    :output-path)))
    (should (equal "/root/worker" (plist-get record :owner)))
    (should (equal 1 (plist-get (plist-get record :facts) :exit-code)))
    (should (string-match-p "output" (plist-get (plist-get record :facts)
                                                 :execution-output)))))

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

(mevedel-deftest mevedel-view--render-tool-call/direct-terminal ()
  ,test
  (test)
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
               :owner "/root" :whole-output "FINAL FAILURE"
               :facts '(:execution-id "exec-1" :command "make test"
                        :state completed :outcome failure :exit-code 2)))
        (let ((after (mevedel-view--render-tool-call call data)))
          (should (string-match-p "failed" (plist-get after :header)))
          (should-not (string-match-p "running" (plist-get after :header)))
          (should (string-match-p "FINAL FAILURE" (plist-get after :body)))
          (should-not (string-match-p "INITIAL OUTPUT" (plist-get after :body)))
          (should (equal "exec-1"
                         (plist-get
                          (plist-get (car (plist-get
                                           (plist-get (plist-get after :group-child)
                                                      :render-data)
                                           :calls)) :render-data)
                          :execution-id)))))))
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
                         :calls ((:id "outer/1" :tool "Bash" :status success
                                  :args (:command "make test")
                                  :render-data (:state running
                                                :wall-time-seconds 0.25)))))
                      data)))
            (should (string-match-p "running · 7.0s" (plist-get row :header)))
            (should (string-match-p "NEW OUTPUT" (plist-get row :body)))
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
                                        :wall-time-seconds 0.25)))))
              (should (string-match-p "running · 7.0s" (plist-get row :header)))
              (should (string-match-p "NEW OUTPUT" (plist-get row :body)))
              (should-not (string-match-p "OLD OUTPUT" (plist-get row :body))))))))))

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
  (let ((live (generate-new-buffer " *third segment*"))
        (middle (generate-new-buffer " *terminal middle segment*"))
        (archive (generate-new-buffer " *first source segment*")))
    (unwind-protect
        (progn
          (with-current-buffer live (setq-local mevedel--session t))
          (with-current-buffer middle
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "original"
                       :render-data (:execution-id "exec-1" :status error
                                     :outcome failure :exit-code 2
                                     :execution-output "FINAL AFTER ROTATION")))))
          (with-temp-buffer
            (setq-local mevedel--data-buffer live)
            (cl-letf (((symbol-function 'mevedel-session-artifacts-transcript-segments)
                       (lambda (_session _data)
                         '((:number 1 :status readable :current-p nil)
                           (:number 2 :status readable :current-p nil)
                           (:number 3 :status readable :current-p t))))
                      ((symbol-function 'mevedel-session-artifacts-read-transcript-segment)
                       (lambda (_session descriptor)
                         (if (= (plist-get descriptor :number) 2) middle archive))))
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
                                            (plist-get rendering :body)))
                (should-not (buffer-live-p middle))))))
      (when (buffer-live-p live) (kill-buffer live))
      (when (buffer-live-p middle) (kill-buffer middle))
      (when (buffer-live-p archive) (kill-buffer archive)))))

(mevedel-deftest mevedel-view-audit-intermediate-terminal-evidence ()
  ,test
  (test)
  :doc "missing original row uses terminal evidence from an older segment"
  (let ((live (generate-new-buffer " *latest without Bash*"))
        (middle (generate-new-buffer " *intermediate retained Bash*")))
    (unwind-protect
        (progn
          (with-current-buffer live (setq-local mevedel--session t))
          (with-current-buffer middle
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-completion :tool-use-id "original"
                       :render-data (:execution-id "exec-1"
                                     :execution-output "RETAINED FINAL S2")))))
          (with-temp-buffer
            (setq-local mevedel--data-buffer live)
            (cl-letf (((symbol-function 'mevedel-session-artifacts-transcript-segments)
                       (lambda (_session _data)
                         '((:number 1 :status missing :current-p nil)
                           (:number 2 :status readable :current-p nil)
                           (:number 3 :status readable :current-p t))))
                      ((symbol-function 'mevedel-session-artifacts-read-transcript-segment)
                       (lambda (_session _number)
                         (let ((copy (generate-new-buffer " *read terminal*")))
                           (with-current-buffer copy
                             (insert (with-current-buffer middle
                                       (buffer-string))))
                           copy)))
                      ((symbol-function 'display-buffer) #'identity))
              (let ((evidence (mevedel-view-audit-show-result
                               '(:owner "/root" :execution-id "exec-1"
                                 :tool-use-id "original"))))
                (with-current-buffer evidence
                  (should (derived-mode-p 'special-mode))
                  (should (string-match-p "RETAINED FINAL S2"
                                          (buffer-string))))))))
      (kill-buffer live)
      (kill-buffer middle)
      (when-let* ((buffer (get-buffer "*mevedel execution result*")))
        (kill-buffer buffer)))))

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
      (delete-file physical)))))

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
  (let ((live (generate-new-buffer " *breadcrumb live*"))
        (archive (generate-new-buffer " *breadcrumb archive*"))
        selected)
    (unwind-protect
        (progn
          (with-current-buffer live (setq-local mevedel--session t))
          (with-current-buffer archive
            (insert (propertize "(:name \"Bash\")"
                                'gptel '(tool . "original"))))
          (with-temp-buffer
            (let ((mevedel--data-buffer live))
              (cl-letf (((symbol-function 'mevedel-session-artifacts-transcript-segments)
                         (lambda (_session _data)
                           '((:number 1 :status readable :current-p nil)
                             (:number 2 :status readable :current-p t))))
                        ((symbol-function 'mevedel-session-artifacts-read-transcript-segment)
                         (lambda (_session _number) archive))
                        ((symbol-function 'mevedel-view-go-to-segment)
                         (lambda (number) (setq selected number))))
                (mevedel-view-audit--select-source
                 '(:tool-use-id "original" :source-target "/later.chat.org"))
                (should (= 1 selected))
                (should-not (buffer-live-p archive))
                (setq archive (generate-new-buffer " *nested archive*"))
                (with-current-buffer archive
                  (insert (propertize "(:name \"ToolCall\")"
                                      'gptel '(tool . "outer"))))
                (setq selected nil)
                (mevedel-view-audit--select-source
                 '(:tool-use-id "outer/1/1" :source-target "/later.chat.org"))
                (should (= 1 selected))
                (should-not (buffer-live-p archive))))))
      (when (buffer-live-p live) (kill-buffer live))
      (when (buffer-live-p archive) (kill-buffer archive)))))

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
  (let ((data (generate-new-buffer " *next mailbox segment*"))
        (archive (generate-new-buffer " *previous mailbox segment*")))
    (unwind-protect
        (progn
          (with-current-buffer data (setq-local mevedel--session t))
          (with-current-buffer archive
            (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                    "<bash-execution execution_id=\"exec-1\" outcome=\"success\"/>\n"
                    "</agent-message>\n"))
          (with-temp-buffer
            (setq-local mevedel--data-buffer data)
            (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                       (lambda () data))
                      ((symbol-function 'mevedel-session-artifacts-transcript-segments)
                       (lambda (_session _buffer)
                         '((:number 1 :status readable :current-p nil)
                           (:number 2 :status readable :current-p t))))
                      ((symbol-function 'mevedel-session-artifacts-read-transcript-segment)
                       (lambda (_session _number) archive)))
              (should (mevedel-view-audit-breadcrumb-present-p
                       '(:execution-id "exec-1" :owner "/root/child")
                       (point-max)))
              (should-not (buffer-live-p archive)))))
      (when (buffer-live-p data) (kill-buffer data))
      (when (buffer-live-p archive) (kill-buffer archive)))))

(mevedel-deftest mevedel-view-audit-breadcrumb-archived-projection ()
  ,test
  (test)
  :doc "historical projection consults earlier, not later, receiver segments"
  (let ((live (generate-new-buffer " *live mailbox session*"))
        (earlier (generate-new-buffer " *earlier mailbox*"))
        (later (generate-new-buffer " *later mailbox*")))
    (unwind-protect
        (progn
          (with-current-buffer live (setq-local mevedel--session t))
          (with-current-buffer earlier
            (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                    "<bash-execution execution_id=\"old\" outcome=\"success\"/>\n"
                    "</agent-message>\n"))
          (with-current-buffer later
            (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                    "<bash-execution execution_id=\"new\" outcome=\"success\"/>\n"
                    "</agent-message>\n"))
          (with-temp-buffer
            (setq-local mevedel--data-buffer live)
            (insert "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                    "<bash-execution execution_id=\"old\" outcome=\"success\"/>\n"
                    "</agent-message>\n"
                    "<agent-message type=\"EXECUTION\" sender=\"/root/child\">\n"
                    "<bash-execution execution_id=\"new\" outcome=\"success\"/>\n"
                    "</agent-message>\n")
            (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                       (lambda () earlier))
                      ((symbol-function 'mevedel-view-segments-current-number)
                       (lambda () 2))
                      ((symbol-function 'mevedel-session-artifacts-transcript-segments)
                       (lambda (_session _buffer)
                         '((:number 1 :status readable :current-p nil)
                           (:number 2 :status readable :current-p nil)
                           (:number 3 :status readable :current-p nil)
                           (:number 4 :status readable :current-p t))))
                      ((symbol-function 'mevedel-session-artifacts-read-transcript-segment)
                       (lambda (_session descriptor)
                         (let ((copy (generate-new-buffer " *mailbox inspection*")))
                           (with-current-buffer copy
                             (insert (with-current-buffer
                                         (if (= (plist-get descriptor :number) 1)
                                             earlier later)
                                       (buffer-string))))
                           copy))))
              (mevedel-view--decorate-agent-message-blocks
               (point-min) (point-max))
              (should (= 1 (how-many "\\[Show result\\]"
                                     (point-min) (point-max))))
              (let* ((pos (text-property-any
                           (point-min) (point-max)
                           'mevedel-view-type 'execution-breadcrumb))
                     (record (and pos (get-text-property
                                       pos 'mevedel-view-execution-breadcrumb))))
                (should (equal "new" (plist-get record :execution-id)))))))
      (kill-buffer live)
      (kill-buffer earlier)
      (kill-buffer later))))

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
    (let ((live (generate-new-buffer " *execution history live*"))
          (archive (generate-new-buffer " *execution history archived*"))
          (session (mevedel-session--create :authority-mode 'pid-lock
                                            :name "execution history"))
          opened)
      (unwind-protect
          (progn
            (with-current-buffer live
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
            (with-current-buffer archive
              (insert (propertize
                       (concat "(:name \"Bash\" :args (:command \"sleep 1\"))\n"
                               "yielded"
                               (mevedel-tool-render-data-format
                                '(:execution-id "exec-1" :state running) "bash"))
                       'gptel '(tool . "bash"))))
            (with-temp-buffer
              (setq-local mevedel--data-buffer live)
              (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                         (lambda () archive))
                        ((symbol-function 'mevedel-session-artifacts-transcript-segments)
                         (lambda (_session _buffer)
                           (list (append '(:number 1 :status readable :current-p nil)
                                         (when agent-p '(:logical "agents/child.compact-0001.chat.org")))
                                 '(:number 2 :status missing :current-p nil)
                                 '(:number 3 :status readable :current-p t))))
                        ((symbol-function 'mevedel-session-artifacts-read-transcript-segment)
                         (lambda (_session descriptor)
                           (should (= 1 (plist-get descriptor :number)))
                           (let ((copy (generate-new-buffer " *history inspection*")))
                             (push copy opened)
                             (with-current-buffer copy
                               (insert-buffer-substring archive))
                             copy))))
                (let ((records (mevedel-view-audit--history-records "exec-1")))
                  (should (equal '("Bash" "WriteStdin" "WriteStdin"
                                   "Execution delivery")
                                 (mapcar (lambda (record) (plist-get record :name))
                                         records)))
                  (should (equal "hi"
                                 (plist-get (plist-get (nth 2 records) :args) :chars)))
                  (should (string-match-p "DELIVERED"
                                          (plist-get (nth 3 records) :result)))
                  (should-not (cl-some #'buffer-live-p opened))
                  (should (buffer-live-p archive))))))
        (dolist (buffer (append opened (list archive live)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(mevedel-deftest mevedel-view-audit--owner-record ()
  ,test
  (test)
  :doc "forwarded child completion resolves its tool ID in an older segment"
  (let ((live (generate-new-buffer " *child current*"))
        (archive (generate-new-buffer " *child older*"))
        (forwarded '(:execution-id "child-exec" :owner "/root/child")))
    (unwind-protect
        (progn
          (with-current-buffer live (setq-local mevedel--session t))
          (with-current-buffer archive
            (insert (mevedel--format-hook-audit-record
                     '(:type execution-breadcrumb :execution-id "child-exec"
                       :owner "/root/child" :tool-use-id "original-call"
                       :command "echo child"))))
          (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                     (lambda () live))
                    ((symbol-function 'mevedel-session-artifacts-transcript-segments)
                     (lambda (_session _buffer)
                       '((:number 1 :status readable :current-p nil)
                         (:number 2 :status readable :current-p t))))
                    ((symbol-function 'mevedel-session-artifacts-read-transcript-segment)
                     (lambda (_session _number) archive)))
            (should (equal "original-call"
                           (plist-get (mevedel-view-audit--owner-record forwarded)
                                      :tool-use-id)))
            (should-not (buffer-live-p archive))))
      (when (buffer-live-p live) (kill-buffer live))
      (when (buffer-live-p archive) (kill-buffer archive)))))

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
