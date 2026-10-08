;;; test-mevedel-view-render-outcomes.el --- Tool outcomes -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise status presentation through the shared renderer and tool roster.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-tools)
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-view-segments)
(require 'mevedel-view-stream)
(require 'mevedel-skills-invoke)
(require 'mevedel-collaboration-projection)

(mevedel-deftest mevedel-view-tool-outcomes/registry ()
  ,test
  (test)
  :doc "every native tool presents failures without success counts"
  (progn
    (mevedel-tools-register)
    (should (> (length (mevedel-tool-all)) 30))
    (dolist (tool (mevedel-tool-all))
      (let ((name (mevedel-tool-name tool)))
        (ert-info ((format "Tool: %s" name))
          (dolist (result '("Error: Path is unreadable" "Path is unreadable"))
            (let* ((data '(:status error))
                   (rendering
                    (or (mevedel-view--invoke-renderer tool data nil result)
                        (mevedel-view--generic-tool-rendering name nil result nil data)))
                   (line (mevedel-view--rendering-header-line rendering)))
              (should (eq 'error (plist-get rendering :status)))
              (should (string-match "×" line))
              (should (eq 'error (get-text-property (match-beginning 0) 'font-lock-face line)))
              (should-not (string-match-p "[0-9]+ \\(matches\\|files\\|symbols\\|chars\\|lines\\)" line))
              (should (string-search result (plist-get rendering :body))))))))
    (dolist (tool (mevedel-tool-all))
      (let* ((name (mevedel-tool-name tool))
             (data '(:status success))
             (result "Error: quoted text in a successful result")
             (rendering (or (mevedel-view--invoke-renderer tool data nil result)
                            (mevedel-view--generic-tool-rendering name nil result nil data))))
        (ert-info ((format "Successful tool: %s" name))
          (should (eq 'success (mevedel-view--rendering-status rendering)))))))
  :doc "empty searches are successful and error-like query text is not an outcome"
  (progn
    (mevedel-tools-register)
    (let* ((rendering (mevedel-view--invoke-renderer
                       (mevedel-tool-get "Grep") '(:status success :count 0)
                       '(:pattern "Warning|Error:|FAILED") "No matches found"))
           (line (mevedel-view--rendering-header-line rendering)))
      (should (string-match-p "✓ Grep:.*(0 matches)" line))
      (should (mevedel-view--tool-group-entry-p (list :count 1 :rendering rendering))))))

(mevedel-deftest mevedel-view-tool-outcomes/execution ()
  ,test
  (test)
  :doc "failed execution headers stay compact while details retain the exit code"
  (let* ((data '(:status error :state completed :outcome failure
                         :termination exited :exit-code 1 :wall-time-seconds 311.7))
         (rendering (mevedel-tool-exec--render-bash "Bash" '(:command "test runner") "failed" data)))
    (should (equal "Bash: test runner · failed · 5m 11s"
                   (plist-get rendering :header)))
    (should (string-search "failure · exit 1 · 5m 11s"
                           (plist-get rendering :body))))
  :doc "expected nonzero outcomes stay successful"
  (let ((data '(:status success :state completed :outcome no-match
                        :termination exited :exit-code 1)))
    (let ((rendering (mevedel-tool-exec--render-bash
                      "Bash" '(:command "grep needle file") "" data)))
      (should (equal "Bash: grep needle file · finished"
                     (plist-get rendering :header)))
      (should (string-search "Details: no-match · exit 1"
                             (plist-get rendering :body)))))
  :doc "accepted input is successful even when the Bash process fails"
  (progn
    (mevedel-tools-register)
    (let* ((data '(:status error :execution-control input
                           :control-succeeded-p t :execution-id "exec-1"
                           :state completed :outcome failure
                           :termination exited :exit-code 2))
           (rendering (mevedel-view--invoke-renderer
                       (mevedel-tool-get "WriteStdin") data
                       '(:execution_id "exec-1" :chars "answer\n")
                       "failed command"))
           (line (mevedel-view--rendering-header-line rendering)))
      (should (eq 'success (mevedel-view--rendering-status rendering data)))
      (should (string-match-p "✓ WriteStdin: sent input" line))))
  :doc "abnormal termination remains visible without an exit code"
  (should (equal "failure · timeout"
                 (mevedel-tool-exec-format-execution-metadata
                  '(:state completed :outcome failure :termination timeout)))))

(mevedel-deftest mevedel-view-tool-outcomes/compound ()
  ,test
  (test)
  :doc "handled child errors warn on the parent without changing its execution outcome"
  (progn
    (mevedel-tools-register)
    (let* ((data '(:kind ptc :status success :outcome completed
                         :calls ((:tool "Read" :id "1" :status error
                                        :args (:file_path "missing.log") :result "Unreadable file"))))
           (rendering (mevedel-view--invoke-renderer
                       (mevedel-tool-get "ToolCall") data nil "Handled"))
           (child (mevedel-view--child-call-rendering (car (plist-get data :calls)))))
      (should (eq 'warning (plist-get rendering :status)))
      (should (eq 'success (plist-get data :status)))
      (should (eq 'error (plist-get child :status)))
      (should (string-match-p "Read: missing.log (error)" (plist-get child :header)))
      (should (plist-get child :initially-collapsed-p))))
  :doc "warning and lifecycle child statuses do not become errors"
  (dolist (status '(warning running cancelled))
    (let ((rendering (mevedel-view--child-call-rendering
                      (list :tool "External" :status status :result "usable result"))))
      (should (eq status (plist-get rendering :status)))))
  :doc "sandbox warnings on successful children warn on the completed parent"
  (let ((rendering (mevedel-tool-ptc--render
                    "ToolCall" nil "Usable result"
                    '(:kind ptc :outcome completed
                            :calls ((:tool "Bash" :status success :render-data
                                           (:sandbox-summary
                                            (:attempt-count 2 :started-count 1 :refused-count 1))))))))
    (should (eq 'warning (plist-get rendering :status))))
  :doc "unknown and MCP-style tools use the same generic status rules"
  (dolist (status '(success error))
    (let* ((data (list :status status))
           (rendering (mevedel-view--generic-tool-rendering
                       "mcp_lookup" '(:query "topic") "Error: quoted" nil data))
           (child (mevedel-view--child-call-rendering
                   (list :tool "mcp_lookup" :status status :result "Error: quoted"))))
      (should (eq status (plist-get rendering :status)))
      (should (eq status (plist-get child :status))))))

(mevedel-deftest mevedel-view-tool-outcomes/recovered-nested-execution
  ()
  ,test
  (test)
  :doc "stale success metadata cannot mark a recovered lost Bash as successful"
  (progn
    (mevedel-tools-register)
    (let* ((repaired (car (mevedel-tool-render-data--reconcile-running
                         '(:kind ptc :status success :outcome completed
                                 :direct-tool "Bash"
                                 :calls ((:id "outer/1" :tool "Bash" :status success
                                          :args (:command "sleep 60") :result "initial"
                                          :render-data (:execution-id "exec-lost"
                                                        :state running)))) nil)))
         (recovered (car (plist-get repaired :calls)))
         (direct (mevedel-view--render-tool-call
                  (list :name "ToolCall" :args '(:expression "Bash")
                        :result "initial" :render-data repaired) nil))
         (child (mevedel-view--child-call-rendering recovered)))
    (should (equal "outer/1" (plist-get recovered :id)))
    (dolist (row (list direct child))
      (should (eq 'error (plist-get row :status)))
      (should (string-match-p "lost execution" (plist-get row :header)))
      (should (string-match-p "×" (mevedel-view--rendering-header-line row))))
      (should (eq 'error (plist-get (plist-get direct :group-child) :status))))))

(mevedel-deftest mevedel-view-tool-outcomes/wrapped ()
  ,test
  (test)
  :doc "wrapped tool exceptions reach the shared error presentation"
  (let* ((source (gptel-make-tool :name "mcp_fixture" :category "mcp"
                                  :description "Test tool" :args nil
                                  :function (lambda () (error "Service unavailable"))))
         (handler (mevedel-tool--call-wrapped-handler source))
         result)
    (funcall handler (lambda (value) (setq result value)) nil)
    (let ((rendering (mevedel-view--generic-tool-rendering
                      "mcp_fixture" nil (plist-get result :result))))
      (should (eq 'error (plist-get rendering :status)))
      (should (string-match-p "× mcp_fixture (error)"
                              (mevedel-view--rendering-header-line rendering)))
      (should (string-search "Service unavailable" (plist-get rendering :body))))))

(mevedel-deftest mevedel-view--rendering-status ()
  ,test
  (test)
  :doc "errors outrank warning disclosures and lifecycle states remain distinct"
  (dolist (status '(error failed denied blocked))
    (should (eq 'error (mevedel-view--rendering-status
                        (list :status status :sandbox-summary
                              '(:attempt-count 1 :started-count 0 :refused-count 1))))))
  (dolist (status '(running cancelled success warning))
    (should (eq status (mevedel-view--rendering-status (list :status status)))))
  (should (eq 'warning (mevedel-view--rendering-status
                        '(:status success :sandbox-summary
                                  (:attempt-count 2 :started-count 1 :refused-count 1)))))
  :doc "warnings and errors color only their marker in every tool row shape"
  (dolist (vtype '(tool-summary tool-child tool-group))
    (dolist (case '((error "×" error) (warning "!" mevedel-view-tool-warning)))
      (let ((line (mevedel-view--rendering-header-line
                   (list :vtype vtype :header "Read: file.log (details)"
                         :status (car case)))))
        (should (string-match (cadr case) line))
        (should (eq (caddr case) (get-text-property (match-beginning 0) 'font-lock-face line)))
        (dolist (label '("Read" "file.log" "details"))
          (should (string-match label line))
          (should-not (memq (get-text-property (match-beginning 0) 'font-lock-face line)
                            '(error mevedel-view-tool-warning))))))))

(mevedel-deftest mevedel-view-tool-outcomes/grouping ()
  ,test
  (test)
  :doc "warnings group across streaming and redraw without losing child state or drafts"
  (mevedel-view-test--with-buffers
    (mevedel-tools-register)
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "> draft\nsecond line"))
    (dotimes (i 4)
      (mevedel-view-test--insert-data
       data-buf
       (concat "#+begin_tool (Grep)\n"
               (format "(:name \"Grep\" :args (:pattern \"needle%d\"))\n\nfile:1:matched\n" i)
               (mevedel-tool-render-data-format
                (if (= i 1)
                    '(:status success :count 1 :sandbox-summary
                              (:attempt-count 2 :started-count 1 :refused-count 1))
                  '(:status success :count 1))
                (format "call_%d" i))
               "\n#+end_tool\n")
       `(tool . ,(format "call_%d" i)))
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response (point-min) (point-max))))
    (with-current-buffer view-buf
      (dotimes (_ 2)
        (should (string-search "! Searched 4 patterns" (buffer-string)))
        (should-not (string-search "needle1" (buffer-string)))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
        (mevedel-view--full-rerender))
      (goto-char (point-min))
      (search-forward "Searched 4 patterns")
      (mevedel-view-toggle-section)
      (should (string-search "! Grep: needle1" (buffer-string)))
      (search-forward "needle1")
      (mevedel-view-toggle-section)
      (should (string-search "file:1:matched" (buffer-string)))
      (mevedel-view--full-rerender)
      (should (string-search "file:1:matched" (buffer-string)))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))

(mevedel-deftest mevedel-view-tool-outcomes/browser ()
  ,test
  (test)
  :doc "browser warnings and errors agree with Emacs and failures stay collapsed"
  (progn
    (mevedel-tools-register)
    (dolist (case '((success "warning") (error "failed")))
      (let* ((data (list :status (car case) :sandbox-summary
                         '(:attempt-count 2 :started-count 1 :refused-count 1)))
             (display (mevedel-collaboration-tool-presentation
                       (list :name "Grep" :args '(:pattern "needle")
                             :result "Partial output" :render-data data))))
        (should (equal (cadr case) (plist-get display :status)))
        (should (eq t (plist-get display :collapsed))))))
  :doc "browser nested warnings, lifecycle states and denials retain their meaning"
  (progn
    (mevedel-tools-register)
    (dolist (case '((warning "warning") (running "running")
                   (cancelled "cancelled") (denied "denied") (error "failed")))
      (let* ((child (list :id "env/1" :tool "External" :status (car case)
                          :result "Result details"))
             (display (mevedel-collaboration-tool-presentation
                       (list :name "ToolCall" :result "Handled"
                             :render-data (list :kind 'ptc :outcome 'completed
                                                :calls (list child)))))
             (nested (aref (plist-get display :children) 0)))
        (should (equal (cadr case) (plist-get nested :status)))
        (when (memq (car case) '(denied error))
          (should (string-search "(error)" (plist-get nested :header))))))))

(provide 'test-mevedel-view-render-outcomes)
;;; test-mevedel-view-render-outcomes.el ends here
