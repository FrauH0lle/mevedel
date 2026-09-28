;;; test-mevedel-collaboration-tool-presentation.el --- Tool display tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise browser tool presentation through canonical collaboration records.

;;; Code:

(require 'json)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-collaboration-projection)
(require 'mevedel-execution-transcript)
(require 'mevedel-skills-invoke)
(require 'mevedel-session-artifacts)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-ptc)
(require 'mevedel-transcript-restore)

(mevedel-deftest mevedel-collaboration-tool-presentation
  ()
  ,test
  (test)
  :doc "direct Skill displays its prepared body without changing envelope identity"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "Skill" :category "mevedel"
                          :renderer #'mevedel-skills--render-skill-tool))
    (let* ((parsed '(:name "ToolCall" :tool-use-id "call-1"
                     :args (:expression "(Skill :name \"artifact-dashboard\")")
                     :result "<system-reminder>Dependency</system-reminder>\n# Dashboard"
                     :render-data
                     (:kind ptc :direct-tool "Skill" :outcome completed
                      :calls ((:id "call-1/1" :tool "Skill" :status success
                               :args (:name "artifact-dashboard")
                               :render-data (:kind skill-invocation
                                             :prompt "# Dashboard"
                                             :attachments ("artifact")))))))
           (original (copy-tree parsed))
           (record (mevedel-collaboration--tool-record parsed "fixture"))
           (display (plist-get record :presentation)))
      (should (equal "ToolCall" (plist-get record :name)))
      (should (equal "tool-call-1" (plist-get record :id)))
      (should (equal "Skill" (plist-get display :name)))
      (should (equal "artifact-dashboard" (plist-get display :detail)))
      (should (equal "# Dashboard" (plist-get display :body)))
      (should (equal "markdown" (plist-get display :format)))
      (should (equal original parsed))))

  :doc "dependency bodies and composed children survive the JSON allowlist"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "Skill" :category "mevedel"
                          :renderer #'mevedel-skills--render-skill-tool))
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                          :renderer #'mevedel-tool-ptc--render))
    (let* ((record (mevedel-collaboration--tool-record
                    '(:name "Skill" :args (:name "dashboard") :result "Model guidance"
                      :render-data (:kind skill-invocation :prompt "# Dashboard"
                                    :attachments ("base" "missing")
                                    :attachment-bodies (("base" . "# Base\nDelivered body"))))
                    "fixture"))
           (wire (json-encode
                  (mevedel-collaboration--json-record record)))
           (display (plist-get record :presentation))
           (dependencies (plist-get display :attachments)))
      (should (string-search "presentation" wire))
      (should (= 2 (length dependencies)))
      (should (equal "# Base\nDelivered body" (plist-get (aref dependencies 0) :body)))
      (should (string-search "unavailable" (plist-get (aref dependencies 1) :body))))
    (let* ((record (mevedel-collaboration--tool-record
                    '(:name "ToolCall" :result "done"
                      :render-data (:kind ptc :outcome completed
                                    :calls ((:id "env/1" :tool "Read" :args (:file_path "a.el")
                                             :status success :batch 0 :result "A")
                                            (:id "env/2" :tool "Read" :args (:file_path "b.el")
                                             :status error :batch 0 :result "Error: missing"))))
                    "fixture"))
           (display (plist-get record :presentation))
           (children (plist-get display :children)))
      (should (equal "ToolCall" (plist-get display :name)))
      (should (= 2 (length children)))
      (should (equal "a.el" (plist-get (aref children 0) :detail)))
      (should (equal "0" (plist-get (aref children 0) :batch)))
      (should (equal "failed" (plist-get (aref children 1) :status)))
      (should (eq t (plist-get (aref children 1) :collapsed)))))

  :doc "budgets the whole display and falls back for malformed metadata"
  (let* ((mevedel-collaboration--max-tool-result-bytes 2048)
         (parsed (list :name "Read" :args '(:file_path "a.el")
                       :result (make-string 10000 ?x)))
         (display (mevedel-collaboration-tool-presentation parsed)))
    (should (plist-get display :truncated))
    (should (< (length (plist-get display :body)) 2048))
    (should (< (string-bytes (json-encode display)) 4096))
    (mevedel-test--with-captured-diagnostics nil
      (should-not (mevedel-collaboration-tool-presentation
                   '(:name "Skill" :result "Visible fallback"
                     :render-data (:attachments malformed))))))

  :doc "envelope failure remains ToolCall and child denial is truthful"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                          :renderer #'mevedel-tool-ptc--render))
    (let* ((data '(:kind ptc :direct-tool "Read" :outcome script-error :status error
                   :calls ((:id "env/1" :tool "Read" :status success :result "child OK"))))
           (display (mevedel-collaboration-tool-presentation
                     (list :name "ToolCall" :result "Error: envelope failed" :render-data data))))
      (should (equal "ToolCall" (plist-get display :name)))
      (should (equal "failed" (plist-get display :status)))
      (should (string-search "envelope failed" (plist-get display :body))))
    (let ((display (mevedel-collaboration-tool-presentation
                    '(:name "ToolCall" :result "Permission denied"
                      :render-data (:kind ptc :outcome tool-error
                                    :calls ((:id "env/1" :tool "Read" :status denied
                                             :result "Permission denied")))))))
      (should (equal "denied"
                     (plist-get (aref (plist-get display :children) 0) :status)))))

  :doc "real wrapped patch retains artifact cards and pending execution identity"
  (let* ((root (make-temp-file "mevedel-present-artifact-" t))
         (path (file-name-concat root "artifacts" "example.html")))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session (mevedel-session--create :name "fixture" :save-path root))
          (make-directory (file-name-directory path) t)
          (write-region "<h1>Fixture</h1>" nil path nil 'silent)
          (insert (propertize
                   "(:name \"ToolCall\" :args (:expression \"(ApplyPatch :patch ...)\"))\nApplied patch"
                   'gptel '(tool . "wrapped-patch")))
          (insert (mevedel-tool-render-data-format
                   `(:kind ptc :outcome completed :direct-tool "ApplyPatch"
                     :calls ((:id "wrapped-patch/1" :tool "ApplyPatch" :status success
                              :args (:patch "patch")
                              :render-data (:kind patch :files ((:kind add :added 1 :deleted 0 :diff "" :path ,path))))))
                   "wrapped-patch"))
          (put-text-property (point-min) (point-max)
                             'gptel '(tool . "wrapped-patch"))
          (let* ((room (list :data-buffer (current-buffer)
                             :pending-tools
                             (list (list :id "pending-id" :kind "tool" :name "ToolCall"
                                         :status "running" :pending t
                                         :baseline-tool-count 0 :baseline-record-count 0))))
                 (records (mevedel-collaboration--project-records room)))
            (should (= 1 (length records)))
            (should (equal "pending-id" (plist-get (car records) :id)))
            (should (equal "example.html" (plist-get (car records) :artifact)))
            (should-not (plist-get room :pending-tools))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory root t))))

(mevedel-deftest mevedel-collaboration--routine-poll-p
  () ,test (test)
  :doc "hide routine polls, including ones delivering output and terminal failure"
  (should (mevedel-collaboration--routine-poll-p
           "WriteStdin" '(:execution_id "exec-1" :chars "")
           '(:state completed :outcome failure :status success) 'success))
  (should (mevedel-collaboration--routine-poll-p
           "WriteStdin" '(:execution_id "exec-1" :chars "")
           '(:state completed :outcome failure :status error
             :control-succeeded-p t) 'error))
  (should (mevedel-collaboration--routine-poll-p
           "WriteStdin" '(:execution_id "exec-1")
           '(:state running :observation-output-p t) nil))
  :doc "do not hide input, denied operations, or a failed control operation"
  (should-not (mevedel-collaboration--routine-poll-p
               "WriteStdin" '(:chars "hello") nil nil))
  (should-not (mevedel-collaboration--routine-poll-p
               "WriteStdin" '(:chars "") '(:status denied) nil))
  (should-not (mevedel-collaboration--routine-poll-p
               "WriteStdin" '(:chars "")
               '(:status success :control-succeeded-p nil) 'success))
  (should-not (mevedel-collaboration--routine-poll-p
               "WriteStdin" '(:chars "") nil 'error)))

(mevedel-deftest mevedel-collaboration--tool-record-execution
  () ,test (test)
  :doc "Bash whole output and terminal status replace stale initial result"
  (let* ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
         (_ (mevedel-tool-register
             (mevedel-tool--create :name "Bash" :category "mevedel"
                                  :renderer #'mevedel-tool-exec--render-bash)))
         (record (mevedel-collaboration--tool-record
                  '(:name "Bash" :args (:command "echo hello")
                    :result "initial output"
                    :render-data (:kind execution :state completed
                                  :outcome failure :status error
                                  :execution-output "initial output\nfinal output"))
                  "tool")))
    (should (equal "failed" (plist-get record :status)))
    (should (equal "initial output\nfinal output" (plist-get record :result)))
    (should (equal "$ echo hello\n\ninitial output\nfinal output\n\nDetails: failure"
                   (plist-get (plist-get record :presentation) :body))))
  :doc "routine poll disappears; errors and real input remain inspectable"
  (should-not (mevedel-collaboration--tool-record
               '(:name "WriteStdin" :args (:execution_id "exec-1")
                 :result "new output"
                 :render-data (:execution-control poll :status error
                               :control-succeeded-p t
                               :state completed :outcome failure)) "poll"))
  (should (equal "failed"
                 (plist-get (mevedel-collaboration--tool-record
                             '(:name "WriteStdin" :args (:chars "")
                               :result "Error: invalid handle"
                               :render-data (:status error)) "control")
                            :status)))
  (should (mevedel-collaboration--tool-record
           '(:name "WriteStdin" :args (:chars "hi") :result "sent") "input")))

(mevedel-deftest mevedel-collaboration--tool-segment-records-pending-terminal
  (:doc "completion before original row insertion uses durable pending terminal facts")
  (with-temp-buffer
    (insert "tool")
    (cl-letf (((symbol-function 'mevedel-view--tool-call-parse)
               (lambda (&rest _)
                 '(:name "Bash" :tool-use-id "bash-1"
                   :args (:command "exit 1") :result "yielded"
                   :render-data (:state running :status success))))
              ((symbol-function 'mevedel-execution-transcript-pending-render-data)
               (lambda (_buffer id)
                 (should (equal "bash-1" id))
                 '(:state completed :outcome failure :status error
                   :execution-output "failed after yield"))))
      (let ((record (car (mevedel-collaboration--tool-segment-records
                          (current-buffer) '(tool 1 5)))))
        (should (equal "tool-bash-1" (plist-get record :id)))
        (should (equal "failed" (plist-get record :status)))
        (should (equal "failed after yield" (plist-get record :result)))))))

(mevedel-deftest mevedel-collaboration-tool-presentation-poll
  () ,test (test)
  :doc "nested successful polls vanish without altering raw returned value"
  (let* ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
         (_ (mevedel-tool-register
             (mevedel-tool--create :name "ToolCall" :category "mevedel"
                                  :renderer #'mevedel-tool-ptc--render)))
         (_bash (mevedel-tool-register
                 (mevedel-tool--create :name "Bash" :category "mevedel"
                                      :renderer #'mevedel-tool-exec--render-bash)))
         (parsed '(:name "ToolCall" :result "raw model value"
                   :render-data
                   (:kind ptc :outcome completed
                    :calls ((:id "c/1" :tool "Bash" :args (:command "echo hi")
                             :status success :result "first"
                             :render-data (:execution-id "exec-1" :state completed
                                           :outcome failure :status error
                                           :execution-output "first\nlast"))
                            (:id "c/2" :tool "WriteStdin"
                             :args (:execution_id "exec-1" :chars "")
                             :status error :result "last"
                             :render-data (:execution-control poll :status error
                                           :control-succeeded-p t))
                            (:id "c/3" :tool "WriteStdin"
                             :args (:execution_id "missing" :chars "")
                             :status error :result "Error: invalid handle")))))
         (record (mevedel-collaboration--tool-record parsed "raw"))
         (children (plist-get (plist-get record :presentation) :children)))
    (should (equal "raw model value" (plist-get record :result)))
    (should (= 2 (length children)))
    (should (equal "$ echo hi\n\nfirst\nlast\n\nDetails: failure · exec-1"
                   (plist-get (aref children 0) :body)))
    (should (equal "failed" (plist-get (aref children 0) :status)))
    (should (equal "failed" (plist-get (aref children 1) :status)))
    (should (equal "ToolCall" (plist-get record :name)))))

(mevedel-deftest mevedel-collaboration--direct-tool-poll
  () ,test (test)
  :doc "single direct ToolCall poll disappears from guest projection without mutating model result"
  (let* ((parsed '(:name "ToolCall" :result "raw model value"
                   :render-data
                   (:kind ptc :direct-tool "WriteStdin" :outcome completed
                    :calls ((:id "c/1" :tool "WriteStdin" :status success
                             :args (:execution_id "exec-1" :chars "")
                             :result "polled output")))))
         (original (copy-tree parsed)))
    (should-not (mevedel-collaboration--tool-record parsed "tool"))
    (should (equal original parsed)))
  :doc "direct failed control operation remains visible with its error facts"
  (let ((parsed '(:name "ToolCall" :result "Error: invalid handle"
                  :render-data
                  (:kind ptc :direct-tool "WriteStdin" :outcome tool-error
                   :calls ((:tool "WriteStdin" :status success
                            :args (:execution_id "missing" :chars "")
                            :render-data (:status error)
                            :result "Error: invalid handle"))))))
    (should (mevedel-collaboration--tool-record parsed "control"))))

(mevedel-deftest mevedel-collaboration--direct-facts
  (:doc "terminal Bash facts override the copied child success of a direct ToolCall")
  (let* ((facts '(:state completed :status error :outcome failure
                  :execution-output "final output"))
         (data (list :calls (list (list :tool "Bash" :status 'success
                                  :render-data facts))))
         (direct '(:tool "Bash" :status success
                   :render-data (:state completed :status success))))
    (should (equal facts
                   (mevedel-collaboration--direct-facts data direct)))))

(mevedel-deftest mevedel-collaboration--live-bash-data
  (:doc "running execution tail belongs to original Bash projection, not poll row")
  (let* ((mevedel--session 'fixture)
         (mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
         (_ (mevedel-tool-register
             (mevedel-tool--create :name "Bash" :category "mevedel"
                                  :renderer #'mevedel-tool-exec--render-bash)))
         (parsed '(:name "Bash" :result "yielded"
                   :args (:command "sleep 10")
                   :render-data (:execution-id "exec-1" :state running
                                 :status success :execution-output "stale output")))
         (original (copy-tree parsed)))
    (cl-letf (((symbol-function 'mevedel-execution-list-user)
               (lambda (_) '((:execution-id "exec-1" :state running
                            :output-tail "fresh poll output")))))
      (let ((live (mevedel-collaboration--live-bash-data parsed)))
        (should (equal "fresh poll output"
                       (mevedel-collaboration--execution-output
                        (plist-get live :render-data))))
        (should (eq 'running (plist-get (plist-get live :render-data) :status)))
        (should (= 1 (cl-count :status (plist-get live :render-data))))
        (let ((record (mevedel-collaboration--tool-record live "tool")))
          (should (equal "running" (plist-get record :status)))
          (should (equal "fresh poll output" (plist-get record :result)))
          (should (equal "$ sleep 10\n\nfresh poll output\n\nDetails: running · exec-1"
                         (plist-get (plist-get record :presentation) :body))))
        (should (equal "yielded" (plist-get live :result)))
        (should (equal original parsed))))))

(mevedel-deftest mevedel-collaboration--suppressed-tool-landed-p
  (:doc "settled direct poll clears only its matching pending ToolCall, not a later identical one")
  (with-temp-buffer
    (insert (make-string 40 ?x))
    (let* ((buffer (current-buffer))
           (parsed '(:name "ToolCall" :args (:expression "(WriteStdin :execution_id \"e\")")
                     :result "raw"
                     :render-data
                     (:kind ptc :direct-tool "WriteStdin" :outcome completed
                      :calls ((:tool "WriteStdin" :status success
                               :args (:execution_id "e" :chars "")
                               :result "observed")))))
           (key (mevedel-collaboration--tool-call-key parsed)))
      (cl-letf (((symbol-function 'mevedel-transcript-segments)
                 (lambda (&rest _) '((tool 3 8))))
                ((symbol-function 'mevedel-view--tool-call-parse)
                 (lambda (&rest _) parsed)))
        (should (mevedel-collaboration--suppressed-tool-landed-p
                 buffer (list :call-key key :baseline-buffer-end 3)))
        (should-not (mevedel-collaboration--suppressed-tool-landed-p
                     buffer (list :call-key key :baseline-buffer-end 9)))
        (should-not (mevedel-collaboration--suppressed-tool-landed-p
                     buffer (list :call-key "other" :baseline-buffer-end 3)))))))

(mevedel-deftest mevedel-collaboration--reconcile-ptc-children
  () ,test (test)
  :doc "nested Bash terminal facts after a poll replace stale child display, not outer model result"
  (with-temp-buffer
    (insert "tool")
    (let* ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
           (_ (mevedel-tool-register
               (mevedel-tool--create :name "ToolCall" :category "mevedel"
                                    :renderer #'mevedel-tool-ptc--render)))
           (_bash (mevedel-tool-register
                   (mevedel-tool--create :name "Bash" :category "mevedel"
                                        :renderer #'mevedel-tool-exec--render-bash)))
           (parsed '(:name "ToolCall" :tool-use-id "outer"
                     :args (:expression "(progn (Bash ...) (WriteStdin ...))")
                     :result "raw model value"
                     :render-data
                     (:kind ptc :outcome completed
                      :calls ((:id "outer/1" :tool "Bash" :status success
                               :args (:command "exit 1") :result "initial"
                               :render-data (:execution-id "exec-1" :state running))
                              (:id "outer/2" :tool "WriteStdin" :status error
                               :args (:execution_id "exec-1" :chars "")
                               :render-data (:control-succeeded-p t :status error)
                               :result "poll output")))))
           (original (copy-tree parsed)))
      (cl-letf (((symbol-function 'mevedel-view--tool-call-parse)
                 (lambda (&rest _) parsed))
                ((symbol-function 'mevedel-execution-transcript-pending-render-data)
                 (lambda (_buffer id)
                   (when (equal id "outer/1")
                     '(:execution-id "exec-1" :state completed :status error
                       :outcome failure :execution-output "initial\nfinal")))))
        (let* ((record (car (mevedel-collaboration--tool-segment-records
                             (current-buffer) '(tool 1 5))))
               (children (plist-get (plist-get record :presentation) :children)))
          (should (equal "raw model value" (plist-get record :result)))
          (should (= 1 (length children)))
          (should (equal "Bash" (plist-get (aref children 0) :name)))
          (should (equal "failed" (plist-get (aref children 0) :status)))
          (should (equal "$ exit 1\n\ninitial\nfinal\n\nDetails: failure · exec-1"
                         (plist-get (aref children 0) :body)))
          (should (equal original parsed))))))
  :doc "direct Bash also projects pending terminal output and status without rewriting returned value"
  (with-temp-buffer
    (insert "tool")
    (let* ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
           (_ (mevedel-tool-register
               (mevedel-tool--create :name "ToolCall" :category "mevedel"
                                    :renderer #'mevedel-tool-ptc--render)))
           (_bash (mevedel-tool-register
                   (mevedel-tool--create :name "Bash" :category "mevedel"
                                        :renderer #'mevedel-tool-exec--render-bash)))
           (parsed '(:name "ToolCall" :tool-use-id "outer"
                     :args (:expression "(Bash :command \"exit 1\")")
                     :result "raw returned value"
                     :render-data
                     (:kind ptc :direct-tool "Bash" :outcome completed
                      :calls ((:id "outer/1" :tool "Bash" :status success
                               :args (:command "exit 1") :result "initial"
                               :render-data (:execution-id "exec-1" :state running))))))
           (original (copy-tree parsed)))
      (cl-letf (((symbol-function 'mevedel-view--tool-call-parse)
                 (lambda (&rest _) parsed))
                ((symbol-function 'mevedel-execution-transcript-pending-render-data)
                 (lambda (_buffer id)
                   (when (equal id "outer/1")
                     '(:execution-id "exec-1" :state completed :status error
                       :outcome failure :execution-output "initial\nfinal")))))
        (let* ((record (car (mevedel-collaboration--tool-segment-records
                             (current-buffer) '(tool 1 5))))
               (presentation (plist-get record :presentation)))
          (should (equal "raw returned value" (plist-get record :result)))
          (should (equal "Bash" (plist-get presentation :name)))
          (should (equal "failed" (plist-get presentation :status)))
          (should (equal "$ exit 1\n\ninitial\nfinal\n\nDetails: failure · exec-1"
                         (plist-get presentation :body)))
          (should (equal original parsed))))))
  :doc "Bash terminal evidence survives another ToolCall layer"
  (with-temp-buffer
    (let* ((data (current-buffer))
           (mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
           (parsed '(:name "ToolCall" :tool-use-id "outer"
                     :result "original returned value"
                     :render-data
                     (:kind ptc :outcome completed
                      :calls ((:id "outer/1" :tool "ToolCall" :status success
                               :result "old output"
                               :render-data
                               (:kind ptc :outcome completed :direct-tool "Bash"
                                :calls ((:id "outer/1/1" :tool "Bash"
                                         :status success :args (:command "exit 2")
                                         :result "old output"
                                         :render-data (:state running)))))))))
           (original (copy-tree parsed)))
      (mevedel-tool-register
       (mevedel-tool--create :name "ToolCall" :category "mevedel"
                             :renderer #'mevedel-tool-ptc--render))
      (mevedel-tool-register
       (mevedel-tool--create :name "Bash" :category "mevedel"
                             :renderer #'mevedel-tool-exec--render-bash))
      (mevedel-execution-transcript-handle-event
       (list :type 'terminal :data-buffer data :tool-use-id "outer/1/1"
             :owner "/root" :whole-output "FINAL FAILURE"
             :facts '(:execution-id "exec-deep" :command "exit 2"
                      :state completed :outcome failure :exit-code 2)))
      (let* ((projection (mevedel-collaboration-tool-presentation
                          (mevedel-collaboration--reconcile-ptc-children
                           data parsed)))
             (child (aref (plist-get projection :children) 0)))
        (should (equal "Bash" (plist-get child :name)))
        (should (equal "failed" (plist-get child :status)))
        (should (string-match-p "FINAL FAILURE" (plist-get child :body)))
        (should-not (string-match-p "old output" (plist-get child :body)))
        (should (equal original parsed)))))
  :doc "unmatched terminal identities do not rewrite other nested executions"
  (let* ((parsed '(:name "ToolCall" :render-data
                   (:kind ptc :calls ((:id "outer/1" :tool "Bash" :status success
                                      :render-data (:execution-id "exec-1"))))))
         (original (copy-tree parsed)))
    (cl-letf (((symbol-function 'mevedel-execution-transcript-pending-render-data)
               (lambda (_buffer _id) nil)))
      (should (eq parsed (mevedel-collaboration--reconcile-ptc-children
                          (current-buffer) parsed)))
      (should (equal original parsed))))
  :doc "nested terminal output and failure survive closing and reopening the transcript"
  (let* ((path (make-temp-file "mevedel-guest-ptc-reload-"))
         (session (mevedel-session--create :authority-mode 'pid-lock
                                           :name "guest-ptc-reload"))
         (buffer (find-file-noselect path))
         (mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (unwind-protect
        (progn
          (mevedel-tool-register
           (mevedel-tool--create :name "ToolCall" :category "mevedel"
                                :renderer #'mevedel-tool-ptc--render))
          (mevedel-tool-register
           (mevedel-tool--create :name "Bash" :category "mevedel"
                                :renderer #'mevedel-tool-exec--render-bash))
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (mevedel-transcript-enable-gptel-mode)
            (setq-local mevedel--session session)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (let ((start (point)))
              (insert "(:name \"ToolCall\" :args (:expression \"(Bash :command \\\"exit 1\\\")\"))\nraw model value")
              (put-text-property start (point) 'gptel '(tool . "outer")))
            (insert (mevedel-tool-render-data-format
                     '(:kind ptc :outcome completed
                       :calls ((:id "outer/1" :tool "Bash" :status success
                                :args (:command "exit 1") :result "initial"
                                :render-data (:execution-id "exec-1" :state running))))
                     "outer"))
            (insert "\n")
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (should (org-entry-get (point-min) "GPTEL_BOUNDS"))
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime)
            (mevedel-execution-transcript-handle-event
             (list :type 'terminal :data-buffer buffer :tool-use-id "outer/1"
                   :owner "/root" :whole-output "initial\nfinal"
                   :facts '(:execution-id "exec-1" :command "exit 1"
                            :state completed :outcome failure :exit-code 1))))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)
          (setq buffer nil)
          (with-temp-buffer
            (insert-file-contents path)
            (delay-mode-hooks (org-mode))
            (mevedel-transcript-restore-properties)
            (should-not mevedel-execution-transcript--pending-terminals)
            (goto-char (point-min))
            (should (search-forward "(:name \"ToolCall\"" nil t))
            (let ((tool-start (match-beginning 0)))
              (should (search-forward mevedel-tool-render-data-close nil t))
              (let* ((tool-end (point))
                     (parsed (mevedel-view--tool-call-parse
                              (current-buffer) tool-start tool-end))
                     (record (car (mevedel-collaboration--tool-segment-records
                                   (current-buffer) (list 'tool tool-start tool-end))))
                     (child (aref (plist-get (plist-get record :presentation) :children)
                                  0)))
                (should (plist-get parsed :render-data))
                (should (equal "raw model value" (plist-get record :result)))
                (should (equal "failed" (plist-get child :status)))
                (should (equal "$ exit 1\n\ninitial\nfinal\n\nDetails: failure · exit 1 · exec-1"
                               (plist-get child :body)))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file path))))

(mevedel-deftest mevedel-collaboration--canonical-records-archived-bash
  (:doc "current guest transcript does not duplicate the archived Bash result")
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
      (mevedel-tool-register
       (mevedel-tool--create :name "Bash" :category "mevedel"
                            :renderer #'mevedel-tool-exec--render-bash))
      (let ((start (point)))
        (insert "(:name \"Bash\" :args (:command \"exit 1\"))\n\ninitial output")
        (insert (mevedel-tool-render-data-format
                 '(:execution-id "exec-archived" :command "exit 1"
                   :state running :status success :live-execution-p t)
                 "bash-archived"))
        (put-text-property start (point) 'gptel '(tool . "bash-archived")))
      (let ((plan (mevedel-execution-transcript-prepare-archive
                   (current-buffer) '("bash-archived"))))
        (should (equal "bash-archived" (caar (plist-get plan :live))))
        (erase-buffer)
        (insert (mevedel-execution-transcript-archive-text plan))
        (mevedel-execution-transcript-commit-archive (current-buffer) plan))
      (mevedel-execution-transcript-handle-event
       (list :type 'terminal :data-buffer (current-buffer)
             :tool-use-id "bash-archived" :owner "/root"
             :whole-output "initial output\nfinal output"
             :facts '(:execution-id "exec-archived" :command "exit 1"
                      :state completed :outcome failure :exit-code 1)))
      (let ((start (point-max)))
        (goto-char start)
        (insert "\n(:name \"WriteStdin\" :args (:execution_id \"exec-archived\" :chars \"\"))\n\nfinal output")
        (put-text-property (1+ start) (point) 'gptel '(tool . "poll-1")))
      (should (= 1 (length (mevedel-transcript-audit-records
                            (buffer-string) 'execution-completion))))
      (should-not (mevedel-collaboration--canonical-records (current-buffer))))))

(mevedel-deftest mevedel-collaboration--canonical-records-present-bash
  (:doc "existing Bash row and nested child completions do not become duplicate cards")
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (let ((start (point)))
      (insert "(:name \"Bash\" :args (:command \"true\"))\n\noriginal result")
      (insert (mevedel-tool-render-data-format
               '(:execution-id "exec-present" :state running) "bash-present"))
      (put-text-property start (point) 'gptel '(tool . "bash-present")))
    (insert (mevedel--format-hook-audit-record
             '(:type execution-completion :tool-use-id "bash-present"
                     :render-data (:execution-id "exec-present" :command "true"
                                   :state completed :status success
                                   :outcome success :execution-output "whole output")))
            (mevedel--format-hook-audit-record
             '(:type execution-completion :tool-use-id "outer/1"
                     :render-data (:execution-id "exec-child" :command "true"
                                   :state completed :status success
                                   :outcome success :execution-output "child output"))))
    (let ((records (mevedel-collaboration--canonical-records (current-buffer))))
      (should (= 1 (length records)))
      (should (equal "tool-bash-present" (plist-get (car records) :id)))
      (should (equal "whole output" (plist-get (car records) :result))))))

(mevedel-deftest mevedel-collaboration--missing-bash-data
  (:doc "only running archived work with missing later evidence becomes unknown")
  (let* ((completions (make-hash-table :test #'equal))
         (running '(:execution-id "exec-1" :state running :status success)))
    (should (eq running (mevedel-collaboration--missing-bash-data
                         running completions)))
    (puthash :gap t completions)
    (let ((unknown (mevedel-collaboration--missing-bash-data
                    running completions)))
      (should (eq 'unknown (plist-get unknown :state)))
      (should (eq 'warning (plist-get unknown :status)))
      (should (eq 'running (plist-get running :state))))
    (should (eq 'completed
                (plist-get (mevedel-collaboration--missing-bash-data
                            '(:state completed :status success) completions)
                           :state)))))

(mevedel-deftest mevedel-collaboration--reconcile-ptc-children-missing
  (:doc "nested and direct Bash show warning without changing the raw result")
  (with-temp-buffer
    (let* ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
           (completions (make-hash-table :test #'equal))
           (parsed '(:name "ToolCall" :result "raw model value"
                     :render-data
                     (:kind ptc :outcome completed
                      :calls ((:id "outer/1" :tool "Bash" :status success
                               :args (:command "echo hi") :result "initial"
                               :render-data (:execution-id "exec-1" :state running
                                             :status success)))))))
      (puthash :gap t completions)
      (mevedel-tool-register
       (mevedel-tool--create :name "ToolCall" :category "mevedel"
                             :renderer #'mevedel-tool-ptc--render))
      (mevedel-tool-register
       (mevedel-tool--create :name "Bash" :category "mevedel"
                             :renderer #'mevedel-tool-exec--render-bash))
      (dolist (direct '(nil "Bash"))
        (let* ((raw (copy-tree parsed))
               (_ (when direct
                    (setf (plist-get (plist-get raw :render-data) :direct-tool)
                          direct)))
               (projected (mevedel-collaboration--reconcile-ptc-children
                           (current-buffer) raw nil completions))
               (record (mevedel-collaboration--tool-record projected "original"))
               (display (mevedel-collaboration-tool-presentation projected))
               (bash (if direct display
                       (aref (plist-get display :children) 0))))
          (should (equal "raw model value" (plist-get projected :result)))
          (should (equal "raw model value" (plist-get record :result)))
          (should (equal "warning" (plist-get bash :status)))
          (when direct (should (equal "warning" (plist-get record :status))))
          (should (string-match-p "completion unavailable"
                                  (plist-get bash :header)))
          (should (string-match-p "initial" (plist-get bash :body)))
          (should (equal "running"
                         (symbol-name (plist-get
                                       (plist-get (car (plist-get
                                                        (plist-get raw :render-data)
                                                        :calls)) :render-data)
                                       :state)))))))))

(provide 'test-mevedel-collaboration-tool-presentation)
;;; test-mevedel-collaboration-tool-presentation.el ends here
