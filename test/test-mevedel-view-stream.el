;;; test-mevedel-view-stream.el --- Streaming view tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-animation)
(require 'mevedel-view-native)
(require 'mevedel-view-power)
(require 'mevedel-view-stream)
(require 'mevedel-agent-control)
(require 'mevedel-execution-transcript)
(require 'mevedel-plugin-registry)
(require 'mevedel-structs)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-ptc)
(require 'mevedel-tool-render-data)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-repair-diagnostics)
(require 'mevedel-transcript-audit)
(require 'mevedel-view-zone)
(eval-when-compile (require 'tramp))

(defmacro mevedel-view-stream-test--with-buffers (&rest body)
  "Create paired data and view buffers, then evaluate BODY."
  (declare (indent 0) (debug t))
  `(let ((data-buf (generate-new-buffer " *mevedel-stream-data*"))
         (view-buf (generate-new-buffer " *mevedel-stream-view*"))
         (mevedel-permission-mode 'ask)
         (mevedel-user-dir
          (file-name-as-directory
           (make-temp-file "mevedel-view-stream-user-" t)))
         (mevedel-plugin-extra-roots nil))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (mevedel--transcript-org-mode)
             (setq-local mevedel--current-request nil)
             (setq-local mevedel--session nil)
             (setq-local gptel-response-separator "\n\n")
             ;; These buffers own no root lifecycle, so teardown drains the
             ;; queues without the session save a fixture cannot complete.
             (setq-local mevedel-view--abort-function
                         #'mevedel-view-test--abort-interactions))
           (mevedel-view--setup view-buf data-buf)
           ,@body)
       (when (buffer-live-p view-buf)
         (with-current-buffer view-buf
           (mevedel-view-stream-stop))
         (kill-buffer view-buf))
       (when (buffer-live-p data-buf)
         (kill-buffer data-buf))
       (when (file-directory-p mevedel-user-dir)
         (delete-directory mevedel-user-dir t)))))

(defun mevedel-view-stream-test--insert-data (buffer text property)
  "Insert TEXT with gptel PROPERTY into BUFFER."
  (with-current-buffer buffer
    (let ((start (point-max)))
      (goto-char start)
      (insert text)
      (put-text-property start (point) 'gptel property))))

(defun mevedel-view-stream-test--insert-composer-draft
    (draft &optional point-offset)
  "Insert DRAFT into the editable composer and move point by POINT-OFFSET."
  (let ((start (mevedel-view--input-start))
        (inhibit-read-only t))
    (goto-char start)
    (insert draft)
    (remove-text-properties
     start (point)
     '(read-only nil
       mevedel-view-prompt nil
       font-lock-face nil
       face nil
       front-sticky nil
       rear-nonsticky nil))
    (goto-char (+ start (or point-offset (length draft))))))

(defun mevedel-view-stream-test--insert-repair-audited-tool (data-buf)
  "Insert one completed repair-audited tool call into DATA-BUF."
  (with-current-buffer data-buf
    (let ((start (point)))
      (insert "(:name \"Collect\" :args (:names [\"alice\"]))\n\ncompleted\n")
      (put-text-property start (point) 'gptel '(tool . "repair-call")))
    (let ((start (point)))
      (insert
       (mevedel-tool-repair-format-audit-block
        'committed
        '((:rule wrap-array-singleton :source generic
                :paths ((names)) :before string :after array))))
      (put-text-property start (point) 'gptel 'ignore))))

(defun mevedel-view-stream-test--count-substring (needle text)
  "Return the number of non-overlapping NEEDLE occurrences in TEXT."
  (let ((count 0)
        (start 0)
        position)
    (while (setq position (string-search needle text start))
      (cl-incf count)
      (setq start (+ position (length needle))))
    count))

(defmacro mevedel-view-stream-test--with-visible-view (&rest body)
  "Display the fixture view while evaluating BODY."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (switch-to-buffer view-buf)
     (redisplay t)
     (with-current-buffer view-buf ,@body)))

(mevedel-deftest mevedel-view-stream-begin-turn ()
  ,test
  (test)
  :doc "records independent active-turn markers and cleanup releases them"
  (mevedel-view-stream-test--with-buffers
    (let ((data-start (with-current-buffer data-buf
                        (copy-marker (point-max) nil))))
      (with-current-buffer view-buf
        (let ((view-start (copy-marker mevedel-view--input-marker nil)))
          (mevedel-view-stream-begin-turn view-start data-start)
          (should (markerp mevedel-view--in-flight-turn-start))
          (should (markerp mevedel-view--data-turn-start))
          (should-not (eq view-start mevedel-view--in-flight-turn-start))
          (should-not (eq data-start mevedel-view--data-turn-start))
          (mevedel-view-stream-stop)
          (should-not mevedel-view--in-flight-turn-start)
          (should-not mevedel-view--data-turn-start))))))

(mevedel-deftest mevedel-view-stream-schedule ()
  ,test
  (test)
  :doc "async streaming redraw preserves multiline leading-> draft and point"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "Prompt\n" nil)
    (let ((data-start (with-current-buffer data-buf
                        (copy-marker (point-min) nil)))
          (draft "> quoted\nsecond line"))
      (mevedel-view-stream-test--insert-data
       data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--data-turn-start data-start)
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay repeat callback &rest args)
                     (unless repeat
                       (apply callback args))
                     'scheduled)))
          (with-current-buffer data-buf
            (mevedel-view-stream-schedule)))
        (setq mevedel-view--render-timer nil)
        (should (string-match-p
                 "Assistant text"
                 (buffer-substring-no-properties
                  (point-min) (mevedel-view--input-start))))
        (should (equal draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))))))

(mevedel-deftest mevedel-view--render-stream-update
  (:doc "rolls back failed agent observer renders")
  ,test
  (test)
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq-local mevedel-view--agent-transcript-p t)
      (let ((before (buffer-string))
            warned)
        (cl-letf (((symbol-function 'mevedel-view-render-live-update)
                   (lambda (&rest _)
                     (let ((inhibit-read-only t))
                       (delete-region (point-min) (point-max)))
                     (error "Observer render failed")))
                  ((symbol-function 'display-warning)
                   (lambda (&rest _) (setq warned t))))
          (mevedel-view--render-stream-update data-buf))
        (should warned)
        (should (equal before (buffer-string)))))))

(mevedel-deftest mevedel-view-stream-handle-execution-event ()
  ,test
  (test)
  :doc "the pending live line stays plain while the row carries the metadata"
  ;; The pending line is keyed by gptel's tool-call name and arguments,
  ;; and an execution event arrives keyed by the provider's call id, so
  ;; the two never met.  What the user sees mid-run is the plain pending
  ;; line; the execution metadata and output tail belong to the row.
  (mevedel-view-stream-test--with-buffers
    (let* ((session (mevedel-session--create :name "execution-pending"))
           (draft "> quoted\nsecond line")
           (bash-tool
            (mevedel-tool--create
             :name "Bash" :renderer #'mevedel-tool-exec--render-bash)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-max))))
        (mevedel-view-stream-test--insert-composer-draft draft 3))
      (let ((point-offset
             (with-current-buffer view-buf
               (- (point) (mevedel-view--input-start)))))
        (cl-letf (((symbol-function 'mevedel-tool-get)
                   (lambda (name) (and (equal name "Bash") bash-tool)))
                  ((symbol-function 'mevedel-view-render-live-update)
                   #'ignore))
          (with-current-buffer data-buf
            ;; Exactly the plist gptel passes: name and arguments, no id.
            (mevedel-view-stream-pre-tool
             (list :name "Bash" :args '(:command "printf run")
                   :buffer (buffer-name data-buf)))
            (should-not
             (mevedel-view-stream-handle-execution-event
              (list :type 'progress :session session :data-buffer data-buf
                    :owner "main"
                    :tool-use-id "call-live"
                    :tool-args '(:command "printf run")
                    :output-tail "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7"
                    :facts '(:execution-id "exec-000001" :state running
                             :wall-time-seconds 2.5 :output-bytes 9
                             :output-lines 1 :omitted-output-bytes 0)))))
          (with-current-buffer view-buf
            (should (equal 1 (length mevedel-view--pending-tool-calls)))
            (let ((label (cdar mevedel-view--pending-tool-calls)))
              (should (equal "Calling Bash..." label))
              (should-not (string-match-p "exec-000001" label))
              (should-not (string-match-p "line 7" label)))
            ;; The tail did reach the execution cache the row renders from.
            (should (equal "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7"
                           (plist-get (gethash "call-live"
                                               mevedel-view--execution-events)
                                      :output-tail)))
            (should-not (plist-get (gethash "call-live"
                                            mevedel-view--execution-events)
                                   :output-preview-truncated-p))
            (should (equal draft
                           (buffer-substring-no-properties
                            (mevedel-view--input-start) (point-max))))
            (should (= point-offset
                       (- (point) (mevedel-view--input-start)))))))))

  :doc "progress stays transient while terminal rows persist across cache turnover"
  (mevedel-view-stream-test--with-buffers
    (let* ((session (mevedel-session--create :name "execution-view"))
           (draft "> quoted\nsecond line")
           (bash-tool
           (mevedel-tool--create
             :name "Bash" :renderer #'mevedel-tool-exec--render-bash))
           (mevedel-view-render-cache-max-entries 1)
           (rerenders 0)
           data-before)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (insert "#+begin_tool (Bash :command \"printf run\")\n")
        (let ((start (point))
              (block
               (mevedel-tool-render-data-format
                '(:status success :state running :live-execution-p t)
                "call-live")))
          (insert
           (concat "(:name \"Bash\" :args (:command \"printf run\"))\n\ninitial"
                   block))
          (put-text-property start (point) 'gptel '(tool . "call-live")))
        (insert "#+end_tool\n"
                "#+begin_tool (Bash :command \"printf new\")\n")
        (let ((start (point)))
          (insert "(:name \"Bash\" :args (:command \"printf new\"))\n\nnew\n")
          (put-text-property start (point) 'gptel '(tool . "call-new")))
        (insert "#+end_tool\n")
        (setq data-before (buffer-string)))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--full-rerender)
        (mevedel-view-stream-test--insert-composer-draft draft 3)
        (let ((point-offset (- (point) (mevedel-view--input-start))))
          (cl-letf (((symbol-function 'mevedel-tool-get)
                     (lambda (name)
                       (and (equal name "Bash") bash-tool)))
                    ((symbol-function 'mevedel-view-rerender)
                     (lambda (&optional _buffer)
                       (cl-incf rerenders)
                       (mevedel-view--full-rerender)
                       'rerender-timer)))
            (should-not
             (mevedel-view-stream-handle-execution-event
              (list :type 'progress :session session :data-buffer data-buf
                    :owner "main"
                    :tool-use-id "call-live"
                    :tool-args '(:command "printf run")
                    :output-tail "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7"
                    :output-preview-truncated-p t
                    :facts '(:execution-id "exec-000001" :state running
                             :wall-time-seconds 2.5 :output-bytes 9
                             :output-lines 1 :omitted-output-bytes 0))))
            (should (zerop rerenders))
            (let* ((bounds
                    (with-current-buffer data-buf
                      (mevedel-tool-render-data-segment-bounds "call-live")))
                   (rendering
                    (mevedel-view--render-tool-call
                     (mevedel-view--tool-call-parse data-buf (car bounds) (cdr bounds))
                     data-buf)))
              (should (string-prefix-p
                       "$ printf run\n\nline 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7"
                       (plist-get rendering :body)))
              (should (string-search "Details: running · 2s"
                                     (plist-get rendering :body)))
              (should (string-search "output truncated"
                                     (plist-get rendering :header)))
              (should-not (plist-get rendering :force-expanded-p)))
            (let ((cached (gethash "call-live"
                                   mevedel-view--execution-events)))
              (should (equal "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7"
                             (plist-get cached :output-tail)))
              (should (plist-get cached :output-preview-truncated-p))
              (should-not (plist-member cached :owner-context))
              (should-not (plist-member cached :observation)))
            (with-current-buffer data-buf
              (should (equal data-before (buffer-string))))
            (should-not (string-match-p
                         "line 7"
                         (buffer-substring-no-properties
                          (point-min) (mevedel-view--input-start))))
            (should-not (string-match-p
                         "line 1"
                         (buffer-substring-no-properties
                          (point-min) (mevedel-view--input-start))))
            (should (string-match-p
                     "Bash: printf run · running · 2s"
                     (buffer-substring-no-properties
                      (point-min) (mevedel-view--input-start))))
            (should-not
             (mevedel-view-stream-handle-execution-event
              (list :type 'terminal :delivery 'mailbox :session session
                    :data-buffer data-buf
                    :owner "main"
                    :tool-use-id "call-live"
                    :tool-args '(:command "printf run")
                    :whole-output "whole head\nwhole tail"
                    :observation
                    '(:sandbox-summary
                      (:attempt-count 1 :started-count 1 :refused-count 0
                       :sandbox bubblewrap :filesystem workspace-write
                       :network isolated :proc fresh
                       :additional-read-count 0
                       :additional-write-count 1))
                    :facts '(:execution-id "exec-000001" :state completed
                             :termination exited :exit-code 0 :outcome success
                             :wall-time-seconds 3.0 :output-bytes 21
                             :output-lines 2 :omitted-output-bytes 0))))
            (should (zerop rerenders))
            (should-not (gethash "call-live" mevedel-view--execution-events))
            (let ((parsed
                   (with-current-buffer data-buf
                     (mevedel-tool-render-data-extract
                      (buffer-substring-no-properties
                       (point-min) (point-max))
                      nil "call-live"))))
              (should (equal "whole head\nwhole tail"
                             (plist-get (cdr parsed) :execution-output)))
              (should
               (= 1
                  (plist-get
                   (plist-get (cdr parsed) :sandbox-summary)
                   :additional-write-count))))
            (let ((visible (buffer-substring-no-properties
                            (point-min) (mevedel-view--input-start))))
              (should-not (string-match-p "whole head" visible))
              (should-not (string-match-p "whole tail" visible))
              (should (string-match-p
                       "Sandbox:.*additional filesystem write access"
                       visible))
              (should (string-match-p "Bash: printf run · finished · 3s"
                                      visible)))
            (save-excursion
              (goto-char (point-min))
              (search-forward "Bash: printf run")
              (goto-char (match-beginning 0))
              (should (get-text-property (point) 'mevedel-view-collapsed))
              (mevedel-view-toggle-section)
              (let ((visible (buffer-substring-no-properties
                              (point-min) (mevedel-view--input-start))))
                (should (string-match-p "whole head" visible))
                (should (string-match-p "whole tail" visible))
                (should (string-match-p "success · exit 0" visible))))
            (should-not
             (mevedel-view-stream-handle-execution-event
              (list :type 'progress :session session :data-buffer data-buf
                    :owner "main"
                    :tool-use-id "call-new"
                    :tool-args '(:command "printf new")
                    :output-tail "new live tail"
                    :facts '(:execution-id "exec-000002" :state running
                             :wall-time-seconds 2.1 :output-bytes 13
                             :output-lines 1 :omitted-output-bytes 0))))
            (should (gethash "call-new" mevedel-view--execution-events))
            (should-not (gethash "call-live" mevedel-view--execution-events))
            (let ((visible (buffer-substring-no-properties
                            (point-min) (mevedel-view--input-start))))
              (should (string-match-p "whole head" visible))
              (should (string-match-p "whole tail" visible))
              (should (string-match-p "Bash: printf new · running · 2s"
                                      visible))
              (should-not (string-match-p "new live tail" visible)))
            (should (equal draft (mevedel-view--input-text)))
            (should (= point-offset
                       (- (point) (mevedel-view--input-start)))))))
      (with-current-buffer data-buf
        (should (= 1 (length (mevedel-transcript-audit-records
                              (buffer-string) 'execution-breadcrumb))))
        (should (equal (substring-no-properties
                        (mevedel-tool-render-data-strip
                         data-before "call-live"))
                       (substring-no-properties
                        (mevedel--strip-hook-audit-blocks
                         (mevedel-tool-render-data-strip
                          (buffer-string) "call-live"))))))))
  :doc "retains a terminal event until its parallel tool row is inserted"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool (Read :file_path \"other\")\n")
      (let ((start (point)))
        (insert "(:name \"Read\" :args (:file_path \"other\"))\n\nwaiting\n")
        (put-text-property start (point) 'gptel '(tool . "call-other")))
      (insert "#+end_tool\n"))
    (mevedel-view-stream-handle-execution-event
     (list :type 'terminal :delivery 'mailbox :data-buffer data-buf
           :owner "main" :tool-use-id "call-late"
           :tool-args '(:command "printf late")
           :whole-output "late terminal output"
           :facts '(:execution-id "exec-000003" :state completed
                    :termination exited :exit-code 0 :outcome success
                    :wall-time-seconds 2.4 :output-bytes 20
                    :output-lines 1 :omitted-output-bytes 0)))
    (with-current-buffer data-buf
      (should (gethash "call-late"
                       mevedel-execution-transcript--pending-terminals))
      (insert "#+begin_tool (Bash :command \"printf late\")\n")
      (let ((start (point)))
        (insert "(:name \"Bash\" :args (:command \"printf late\"))\n\nyielded\n")
        (put-text-property start (point) 'gptel '(tool . "call-late")))
      (insert "#+end_tool\n")
      (mevedel-execution-transcript-retry-pending-terminals data-buf)
      (should-not (gethash
                   "call-late"
                   mevedel-execution-transcript--pending-terminals))
      (let ((parsed
             (mevedel-tool-render-data-extract
              (buffer-substring-no-properties (point-min) (point-max))
              nil "call-late")))
        (should (equal "late terminal output"
                       (plist-get (cdr parsed) :execution-output))))))
  :doc "finalizes a headless agent row at the data-buffer response boundary"
  (let ((agent-data (generate-new-buffer " *execution-headless-agent*")))
    (unwind-protect
        (progn
          (mevedel-view-stream-handle-execution-event
           (list :type 'terminal :delivery 'mailbox
                 :data-buffer agent-data :owner "explorer--headless"
                 :tool-use-id "call-headless"
                 :tool-args '(:command "printf headless")
                 :whole-output "headless terminal output"
                 :facts '(:execution-id "exec-000004" :state completed
                          :termination exited :exit-code 0 :outcome success
                          :wall-time-seconds 2.6 :output-bytes 24
                          :output-lines 1 :omitted-output-bytes 0)))
          (with-current-buffer agent-data
            (should-not (bound-and-true-p mevedel--view-buffer))
            (should (gethash
                     "call-headless"
                     mevedel-execution-transcript--pending-terminals))
            (insert "#+begin_tool (Bash :command \"printf headless\")\n")
            (let ((start (point)))
              (insert "(:name \"Bash\" :args (:command \"printf headless\"))\n\nyielded\n")
              (put-text-property start (point) 'gptel
                                 '(tool . "call-headless")))
            (insert "#+end_tool\n")
            (mevedel-execution-transcript-retry-terminals)
            (should-not
             (gethash
              "call-headless"
              mevedel-execution-transcript--pending-terminals))
            (let ((parsed
                   (mevedel-tool-render-data-extract
                    (buffer-substring-no-properties
                     (point-min) (point-max))
                    nil "call-headless")))
              (should
               (equal "headless terminal output"
                      (plist-get (cdr parsed) :execution-output))))))
      (kill-buffer agent-data)))
  :doc "routes same-session progress to the matching owner view"
  (let ((session (mevedel-session--create :name "execution-routing"))
        (main-data (generate-new-buffer " *execution-main-data*"))
        (main-view (generate-new-buffer " *execution-main-view*"))
        (agent-data (generate-new-buffer " *execution-agent-data*"))
        (agent-view (generate-new-buffer " *execution-agent-view*"))
        targets)
    (unwind-protect
        (progn
          (dolist (buffer (list main-data agent-data))
            (with-current-buffer buffer
              (setq-local mevedel--session session)))
          (with-current-buffer main-view
            (setq-local mevedel--data-buffer main-data)
            (setq-local mevedel-view--agent-transcript-p nil)
            (setq-local mevedel-view--pending-tool-calls nil))
          (with-current-buffer agent-view
            (setq-local mevedel--data-buffer agent-data)
            (setq-local mevedel-view--agent-transcript-p t)
            (setq-local mevedel-view--pending-tool-calls nil))
          (cl-letf
              (((symbol-function
                 'mevedel-view--refresh-tool-row)
                (lambda (_data-buffer _tool-use-id)
                  (push (current-buffer) targets)
                  t)))
            (mevedel-view-stream-handle-execution-event
             (list :type 'progress :session session :data-buffer agent-data
                   :owner "explorer--owned"
                   :tool-use-id "call-agent"
                   :facts nil :tool-args nil))
            (mevedel-view-stream-handle-execution-event
             (list :type 'progress :session session :data-buffer main-data
                   :owner "main"
                   :tool-use-id "call-main"
                   :facts nil :tool-args nil)))
          (should (equal (list main-view agent-view) targets)))
      (dolist (buffer (list main-data main-view agent-data agent-view))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(mevedel-deftest mevedel-view-stream-terminal-breadcrumb-live ()
  ,test
  (test)
  :doc "terminal event projects its durable audit immediately in an idle open view"
  (mevedel-view-stream-test--with-buffers
    (let ((session (mevedel-session--create :name "live-breadcrumb"))
          (draft "> quoted\nsecond line")
          (rerenders 0)
          (event (list :type 'terminal :delivery 'mailbox
                       :owner "main" :tool-use-id "call-live"
                       :tool-args '(:command "printf run")
                       :whole-output "done"
                       :facts '(:execution-id "exec-live" :command "printf run"
                                :state completed :termination exited
                                :exit-code 0 :outcome success))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (insert "#+begin_tool (Bash :command \"printf run\")\n")
        (let ((start (point)))
          (insert "(:name \"Bash\" :args (:command \"printf run\"))\n\nyielded\n")
          (put-text-property start (point) 'gptel '(tool . "call-live")))
        (insert "#+end_tool\n"))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--full-rerender)
        (mevedel-view-stream-test--insert-composer-draft draft 4)
        (set-mark (+ (mevedel-view--input-start) 1))
        (setq mark-active t))
      (setq event (plist-put event :data-buffer data-buf)
            event (plist-put event :session session))
      (cl-letf (((symbol-function 'mevedel-view--full-rerender)
                 (lambda () (cl-incf rerenders))))
        (mevedel-view-stream-handle-execution-event event)
        (with-current-buffer view-buf
          (let ((visible (buffer-substring-no-properties
                          (point-min) (mevedel-view--input-start))))
            (should (= 1 (mevedel-view-stream-test--count-substring
                          "[Show result]" visible)))
            (should (string-match-p "↳ Finished: printf run" visible))
            (should (equal draft (mevedel-view--input-text)))
            (should (= 4 (- (point) (mevedel-view--input-start))))
            (should mark-active)
            (should (= 1 (- (mark) (mevedel-view--input-start))))
            (save-excursion
              (goto-char (point-min))
              (search-forward "[Show result]")
              (let ((at (match-beginning 0)))
                (should (eq 'execution-breadcrumb
                            (get-text-property at 'mevedel-view-type)))
                (should (get-text-property at 'mevedel-view-source))
                (should (equal "exec-live"
                               (plist-get
                                (get-text-property
                                 at 'mevedel-view-execution-breadcrumb)
                                :execution-id)))))))
        (mevedel-view-stream-handle-execution-event event)
        (with-current-buffer view-buf
          (should (= 1 (mevedel-view-stream-test--count-substring
                        "[Show result]" (buffer-string))))
          (should (zerop rerenders))))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (should (= 1 (mevedel-view-stream-test--count-substring
                      "[Show result]" (buffer-string))))
        (should (equal draft (mevedel-view--input-text)))
        (should (= 4 (- (point) (mevedel-view--input-start)))))
      (with-current-buffer data-buf
        (should (= 1 (length (mevedel-transcript-audit-records
                              (buffer-string) 'execution-breadcrumb))))))))

(mevedel-deftest mevedel-view-stream-terminal-breadcrumb-projection ()
  ,test
  (test)
  :doc "headless completions reconstruct; historical views stay on their segment"
  (mevedel-view-stream-test--with-buffers
    (let* ((session (mevedel-session--create :name "deferred-breadcrumb"))
           (event (list :type 'terminal :delivery 'mailbox :session session
                        :data-buffer data-buf :owner "main"
                        :tool-use-id "call-late"
                        :tool-args '(:command "printf late")
                        :whole-output "done"
                        :facts '(:execution-id "exec-late"
                                 :command "printf late" :state completed
                                 :termination exited :exit-code 0
                                 :outcome success))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (insert "#+begin_tool (Bash :command \"printf late\")\n")
        (let ((start (point)))
          (insert "(:name \"Bash\" :args (:command \"printf late\"))\n\nyielded\n")
          (put-text-property start (point) 'gptel '(tool . "call-late")))
        (insert "#+end_tool\n"))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--data-buffer nil))
      (mevedel-view-stream-handle-execution-event event)
      (with-current-buffer view-buf
        (setq-local mevedel--data-buffer data-buf))
      (let ((archive (generate-new-buffer " *breadcrumb historical source*")))
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-view-segments-display-buffer)
                       (lambda () archive)))
              (mevedel-view-stream-handle-execution-event event)
              (with-current-buffer view-buf
                (should (= 0 (mevedel-view-stream-test--count-substring
                              "[Show result]" (buffer-string))))))
          (kill-buffer archive)))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (should (= 1 (mevedel-view-stream-test--count-substring
                      "[Show result]" (buffer-string))))))))

(mevedel-deftest mevedel-view-stream-handle-tool-progress ()
  ,test
  (test)
  :doc "preserves a multiline composer draft for generic tool progress"
  (mevedel-view-stream-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (with-current-buffer view-buf
        (mevedel-view-stream-test--insert-composer-draft draft 3))
      (let ((point-offset
             (with-current-buffer view-buf
               (- (point) (mevedel-view--input-start)))))
        (cl-letf (((symbol-function 'mevedel-view--refresh-tool-row)
                   (lambda (&rest _) nil))
                  ((symbol-function
                    'mevedel-view-stream--schedule-execution-row-recovery)
                   #'ignore))
          (mevedel-view-stream-handle-tool-progress
           (list :type 'progress :data-buffer data-buf
                 :tool-use-id "ptc-live"
                 :facts '(:kind ptc :live-p t :active-tool "Read"
                          :completed-count 2))))
        (with-current-buffer view-buf
          (should (eq 'ptc
                      (plist-get
                       (plist-get (gethash "ptc-live"
                                           mevedel-view--execution-events)
                                  :facts)
                       :kind)))
          (should (equal draft
                         (buffer-substring-no-properties
                          (mevedel-view--input-start) (point-max))))
          (should (= point-offset
                     (- (point) (mevedel-view--input-start)))))
        (mevedel-view-stream-handle-tool-progress
         (list :type 'terminal :data-buffer data-buf
               :tool-use-id "ptc-live"))
        (with-current-buffer view-buf
          (should-not (gethash "ptc-live" mevedel-view--execution-events))))))

  :doc "an unattended view coalesces tool progress without rebuilding history"
  (mevedel-view-stream-test--with-buffers
    (let ((refreshed 0)
          (unattended t)
          (draft "> quoted\nsecond line"))
      (with-current-buffer view-buf
        (mevedel-view-stream-test--insert-composer-draft draft 3))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--unattended-p)
                     (lambda (&rest _) unattended))
                    ((symbol-function 'mevedel-view--refresh-tool-row)
                     (lambda (&rest _) (cl-incf refreshed) t)))
            (mevedel-view-stream-handle-tool-progress
             (list :type 'progress :data-buffer data-buf
                   :tool-use-id "bash-live"
                   :facts '(:kind bash) :output-tail "line"))
            (with-current-buffer view-buf
              (should (= 0 refreshed))
              (should (gethash "bash-live" mevedel-view--execution-events))
              (should (eq 'tools mevedel-view--pending-render-kind))
              (should (eq data-buf mevedel-view--pending-render-data-buffer))
              (should (equal draft
                             (buffer-substring-no-properties
                              (mevedel-view--input-start) (point-max))))
              (mevedel-view--schedule-render 'tools data-buf 0)
              (should-not mevedel-view--render-timer))
            (dotimes (_ 3)
              (mevedel-view-stream-handle-tool-progress
               (list :type 'progress :data-buffer data-buf
                     :tool-use-id "bash-live"
                     :facts '(:kind bash) :output-tail "latest line")))
            (with-current-buffer view-buf
              (should (equal '("bash-live") mevedel-view--pending-tool-rows))
              (should (equal "latest line"
                             (plist-get (gethash "bash-live"
                                                 mevedel-view--execution-events)
                                        :output-tail))))
            (mevedel-view-stream-handle-tool-progress
             (list :type 'terminal :data-buffer data-buf
                   :tool-use-id "bash-live"))
            (with-current-buffer view-buf
              (should-not (gethash "bash-live" mevedel-view--execution-events))
              (should (equal '("bash-live") mevedel-view--pending-tool-rows)))
            (setq unattended nil)
            (mevedel-view--resume-attended-views)
            (with-current-buffer view-buf
              (should (mevedel--timer-pending-p mevedel-view--render-timer))
              (mevedel-view--schedule-render 'tools data-buf 0)
              (should (= 1 refreshed))
              (should-not mevedel-view--pending-render-kind)
              (should (equal draft (mevedel-view--input-text)))
              (should (= 3 (- (point) (mevedel-view--input-start))))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render))))))

(mevedel-deftest mevedel-view-stream-nested-bash-progress ()
  ,test (test)
  :doc "a nested Bash event refreshes its source-owning row and preserves an open draft"
  (mevedel-view-stream-test--with-buffers
    (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
          (draft "> quoted\nsecond line"))
      (mevedel-tool-register
       (mevedel-tool--create :name "ToolCall" :category "mevedel"
                             :renderer #'mevedel-tool-ptc--render))
      (mevedel-tool-register
       (mevedel-tool--create :name "Bash" :category "mevedel"
                             :renderer #'mevedel-tool-exec--render-bash))
      (mevedel-view-stream-test--insert-data
       data-buf
       (concat
        "(:name \"ToolCall\" :args (:expression \"(Bash :command \\\"sleep 10\\\")\"))\n\nOLD OUTPUT"
        (mevedel-tool-render-data-format
         '(:kind ptc :outcome completed :direct-tool "Bash"
           :calls ((:id "outer/1" :tool "Bash" :status success
                    :args (:command "sleep 10") :result "OLD OUTPUT"
                    :render-data (:execution-id "exec-1" :state running))))
         "outer") "\n")
       '(tool . "outer"))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (source (cons 1 (with-current-buffer data-buf (point-max)))))
          (goto-char (point-min))
          (mevedel-view--insert-rendered-tool
           (mevedel-view--render-tool-call
            (mevedel-view--tool-call-parse
             data-buf (car source) (cdr source)) data-buf)
           source))
        (goto-char (point-min))
        (should (search-forward "Bash: sleep 10" nil t))
        (mevedel-view-toggle-section)
        (should (string-match-p "OLD OUTPUT" (buffer-string)))
        (mevedel-view-stream-test--insert-composer-draft draft 3))
      (mevedel-view-stream-handle-tool-progress
       (list :type 'progress :data-buffer data-buf :tool-use-id "outer/1"
             :facts '(:execution-id "exec-1" :command "sleep 10"
                      :state running :wall-time-seconds 7)
             :output-tail "NEW OUTPUT"))
      (with-current-buffer view-buf
        (should (string-match-p "Bash: sleep 10 · running · 7s"
                                (buffer-string)))
        (should (string-match-p "NEW OUTPUT" (buffer-string)))
        (should-not (string-match-p "OLD OUTPUT" (buffer-string)))
        (should (equal draft (mevedel-view--input-text)))
        (should (= 3 (- (point) (mevedel-view--input-start))))
        (cl-letf (((symbol-function 'mevedel-view-audit--evidence)
                   (lambda (&rest _) (ert-fail "Source-backed Bash row missing"))))
          (mevedel-view-audit-show-control-result "exec-1")
          (should (looking-at-p ".*Bash: sleep 10"))
          (should (equal draft (mevedel-view--input-text))))))
  :doc "compound ToolCall refresh replaces its expanded child instead of duplicating it"
  (mevedel-view-stream-test--with-buffers
    (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
          (draft "> quoted\nsecond line"))
      (mevedel-tool-register
       (mevedel-tool--create :name "ToolCall" :category "mevedel"
                             :renderer #'mevedel-tool-ptc--render))
      (mevedel-tool-register
       (mevedel-tool--create :name "Bash" :category "mevedel"
                             :renderer #'mevedel-tool-exec--render-bash))
      (mevedel-view-stream-test--insert-data
       data-buf
       (concat
        "(:name \"ToolCall\" :args (:expression \"(list (Bash :command \\\"sleep 10\\\"))\"))\n\nreturned"
        (mevedel-tool-render-data-format
         '(:kind ptc :outcome completed
           :calls ((:id "outer/1" :tool "Bash" :status success
                    :args (:command "sleep 10") :result "OLD OUTPUT"
                    :render-data (:execution-id "exec-1" :state running))))
         "outer") "\n")
       '(tool . "outer"))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (source (cons 1 (with-current-buffer data-buf (point-max)))))
          (goto-char (point-min))
          (mevedel-view--insert-rendered-tool
           (mevedel-view--render-tool-call
            (mevedel-view--tool-call-parse
             data-buf (car source) (cdr source)) data-buf)
           source))
        (goto-char (point-min))
        (mevedel-view-toggle-section)
        (goto-char (text-property-any (point-min) (point-max)
                                      'mevedel-view-type 'tool-child))
        (mevedel-view-render-toggle-child-call)
        (should (= 1 (how-many "Bash: sleep 10" (point-min) (point-max))))
        (mevedel-view-stream-test--insert-composer-draft draft 3))
      (dolist (output '("NEW OUTPUT" "LATEST OUTPUT"))
        (mevedel-view-stream-handle-tool-progress
         (list :type 'progress :data-buffer data-buf :tool-use-id "outer/1"
               :facts '(:execution-id "exec-1" :state running
                        :wall-time-seconds 7)
               :output-tail output)))
      (with-current-buffer view-buf
        (should (= 1 (how-many "Bash: sleep 10" (point-min) (point-max))))
        (should (= 1 (how-many "LATEST OUTPUT" (point-min) (point-max))))
        (should-not (string-match-p "OLD OUTPUT\\|NEW OUTPUT" (buffer-string)))
        (should (equal draft (mevedel-view--input-text)))
        (should (= 3 (- (point) (mevedel-view--input-start)))))))))

(mevedel-deftest mevedel-view--spinner-tick ()
  ,test
  (test)
  :doc "progress ticks preserve selection without reactivating mark hooks"
  (dolist (active '(nil t))
    (mevedel-view-stream-test--with-buffers
      (with-current-buffer view-buf
        (setq-local transient-mark-mode t)
        (mevedel-view--start-spinner "Working...")
        (mevedel-view-stream-test--insert-composer-draft
         "> quoted\nsecond line" 3)
        (set-mark (+ (mevedel-view--input-start) 8))
        (setq mark-active active deactivate-mark nil)
        (let* ((activations 0)
               (activate-mark-hook (list (lambda () (cl-incf activations)))))
          (cl-letf (((symbol-function 'mevedel-view--unattended-p)
                     (lambda (&rest _) nil)))
            (dotimes (_ 3) (mevedel-view--spinner-tick))
            (mevedel-view--update-spinner "Checking completed work..."))
          (should (= activations 0))
          (should (eq active mark-active))
          (should-not deactivate-mark)
          (should (= 3 (- (point) (mevedel-view--input-start))))
          (should (= 8 (- (mark) (mevedel-view--input-start))))
          (should (equal "> quoted\nsecond line" (mevedel-view--input-text))))
        (mevedel-view--stop-spinner))))

  :doc "an unattended tick leaves text, properties, and the composer untouched"
  (mevedel-view-stream-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (with-current-buffer view-buf
        (mevedel-view--start-spinner "Working...")
        (mevedel-view-stream-test--insert-composer-draft draft 3)
        (let* ((before (buffer-string))
               (point-before (point))
               (frame-pos (text-property-any
                           (point-min) (point-max)
                           'mevedel-view-spinner-frame t))
               (frame (get-text-property frame-pos 'display)))
          (cl-letf (((symbol-function 'mevedel-view--unattended-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'mevedel-view--animation-seconds)
                     (lambda () 0.48)))
            (mevedel-view--spinner-tick))
          (should (equal-including-properties before (buffer-string)))
          (should (= point-before (point)))
          (should (equal frame (get-text-property frame-pos 'display)))
          (should (equal draft
                         (buffer-substring-no-properties
                          (mevedel-view--input-start) (point-max)))))
        (mevedel-view--stop-spinner))))

  :doc "a visible attended tick samples elapsed time instead of an index"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full))
        (mevedel-view--start-spinner "Working...")
        (let* ((pos (text-property-any (point-min) (point-max)
                                       'mevedel-view-spinner-frame t))
               (initial (get-text-property pos 'display)))
          (should (equal "- Working..." initial))
          (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                     (lambda () 0.24)))
            (mevedel-view--spinner-tick))
          (should (equal "\\ Working..." (get-text-property pos 'display))))
        (mevedel-view--stop-spinner))))

  :doc "visible high-frequency frames touch only registered display spans"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view-tool-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read..."))))
        (mevedel-view--start-spinner "Working...")
        (mevedel-view--refresh-pending-tool-lines)
        (mevedel-view-stream-test--insert-composer-draft
         "> quoted\nsecond line" 3)
        (let* ((main (text-property-any (point-min) (point-max)
                                        'mevedel-view-spinner-frame t))
               (tool (text-property-any (point-min) (point-max)
                                        'mevedel-view-inline-spinner-frame t))
               (before (buffer-substring-no-properties
                        (point-min) (point-max)))
               (point-before (point))
               (undo-before buffer-undo-list)
               (modified-before (buffer-modified-p))
               (mevedel-view--spinner-last-second (floor (float-time)))
               (original-framerate
                (symbol-function 'mevedel-view-power-framerate))
               (rates 0))
          (should main)
          (should tool)
          (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                     (lambda () 0.24))
                    ((symbol-function 'mevedel-view-power-framerate)
                     (lambda (&rest args)
                       (cl-incf rates)
                       (apply original-framerate args)))
                    ((symbol-function 'mevedel-view-zone-reconcile)
                     (lambda (&rest _) (ert-fail "tick reconciled a zone")))
                    ((symbol-function 'mevedel-view--spinner-display-status)
                     (lambda (&rest _) (ert-fail "tick rebuilt metadata")))
                    ((symbol-function 'mevedel-view--pending-tool-fragments)
                     (lambda (&rest _) (ert-fail "tick rebuilt tool rows")))
                    ((symbol-function 'mevedel-view-animation--colors)
                     (lambda (&rest _) (ert-fail "tick resolved colors")))
                    ((symbol-function 'mevedel-view-power--current-state)
                     (lambda (&rest _) (ert-fail "tick queried power")))
                    ((symbol-function 'mevedel-view--input-text)
                     (lambda (&rest _) (ert-fail "tick copied the draft"))))
            (mevedel-view--spinner-tick))
          (should (> rates 0))
          (should (equal "\\ Working..." (get-text-property main 'display)))
          (should (equal "\\ " (get-text-property tool 'display)))
          (should (equal before (buffer-substring-no-properties
                                 (point-min) (point-max))))
          (should (= point-before (point)))
          (should (eq undo-before buffer-undo-list))
          (should (eq modified-before (buffer-modified-p)))
          (should (equal "> quoted\nsecond line" (mevedel-view--input-text))))))))

(mevedel-deftest mevedel-view-stream-ensure-progress-for-fsm ()
  ,test
  (test)
  :doc "direct top-level FSM anchors both sides and requests progress"
  (mevedel-view-stream-test--with-buffers
    (let* ((position (with-current-buffer data-buf
                       (copy-marker (point-max) nil)))
           (fsm (gptel-make-fsm
                 :info (list :buffer data-buf :position position)))
           ensured)
      (cl-letf (((symbol-function 'mevedel-view--agent-fsm-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-view--ensure-request-progress)
                 (lambda (buffer &optional _status)
                   (setq ensured buffer))))
        (mevedel-view-stream-ensure-progress-for-fsm fsm))
      (with-current-buffer view-buf
        (should (eq ensured data-buf))
        (should (markerp mevedel-view--data-turn-start))
        (should (= (marker-position mevedel-view--data-turn-start)
                   (marker-position position)))
        (should (markerp mevedel-view--in-flight-turn-start))))))

;;
;;; Pending tool lifecycle

(mevedel-deftest mevedel-view--pending-tool-fingerprint
  (:doc "keys pending tool calls by the arguments gptel actually sends")
  ,test
  (test)
  :doc "fingerprints the tool name and its arguments"
  (should (equal '("Read" . "(:file_path \"a\")")
                 (mevedel-view--pending-tool-fingerprint
                  '(:name "Read" :args (:file_path "a")))))

  :doc "different arguments fingerprint differently"
  (should-not (equal
               (mevedel-view--pending-tool-fingerprint
                '(:name "Read" :args (:file_path "a")))
               (mevedel-view--pending-tool-fingerprint
                '(:name "Read" :args (:file_path "b")))))

  :doc "identical calls share a fingerprint"
  ;; gptel passes no call id, so two identical parallel calls cannot be
  ;; told apart here; the pending list keeps one entry per call instead.
  (should (equal
           (mevedel-view--pending-tool-fingerprint
            '(:name "Read" :args (:file_path "a")))
           (mevedel-view--pending-tool-fingerprint
            '(:name "Read" :args (:file_path "a"))))))

(mevedel-deftest mevedel-view--routine-pending-poll-p ()
  ,test
  (test)
  :doc "only empty-input WriteStdin observations are routine pending polls"
  (should (mevedel-view--routine-pending-poll-p
           '(:name "WriteStdin" :args (:execution_id "exec-1" :chars ""))))
  (should (mevedel-view--routine-pending-poll-p
           '(:name "WriteStdin" :args (:execution_id "exec-1"))))
  (dolist (args '((:execution_id "" :chars "")
                  (:chars "")
                  (:execution_id 13 :chars "")
                  (:execution_id "exec-1" :chars ""
                                 :yield_time_ms "not-a-duration")
                  (:execution_id "exec-1" :chars ""
                                 :yield_time_ms 0)
                  (:execution_id "exec-1" :chars "" :bogus 1)
                  (:execution_id "exec-1" :chars "" :chars "yes")))
    (should-not (mevedel-view--routine-pending-poll-p
                 (list :name "WriteStdin" :args args))))
  (should (mevedel-view--routine-pending-poll-p
           '(:name "ToolCall"
             :args (:expression "(WriteStdin :execution_id \"exec-1\" :chars \"\" :yield_time_ms 5000)"))))
  (should (mevedel-view--routine-pending-poll-p
           '(:name "ToolCall"
             :args (:expression "(WriteStdin :execution_id \"exec-1\")"))))
  (should-not (mevedel-view--routine-pending-poll-p
               '(:name "ToolCall"
                 :args (:expression "(WriteStdin :execution_id \"exec-1\" :chars \"\")"
                        :bogus 1))))
  (should-not (mevedel-view--routine-pending-poll-p
               '(:name "WriteStdin"
                 :args (:execution_id "exec-1" :chars "\C-c"))))
  (dolist (script '("(WriteStdin :execution_id \"exec-1\" :chars \"yes\\n\")"
                    "(WriteStdin :execution_id \"exec-1\" :chars nil)"
                    "(WriteStdin :execution_id \"exec-1\" :chars \"\") (Read :file_path \"a\")"
                    "(WriteStdin :execution_id \"exec-1\" :chars \"\" :chars \"\")"
                    "(WriteStdin :execution_id \"exec-1\" :chars \"\" :yield_time_ms (identity 5000))"
                    "(WriteStdin :execution_id \"exec-1\" :chars \"\" :yield_time_ms 0)"
                    "(WriteStdin :execution_id \"exec-1\" :chars \"\""))
    (should-not (mevedel-view--routine-pending-poll-p
                 (list :name "ToolCall" :args (list :expression script)))))
  (should-not (mevedel-view--routine-pending-poll-p
               '(:name "StopExecution" :args (:execution_id "exec-1")))))

(mevedel-deftest mevedel-view-stream-spinner-hook ()
  ,test
  (test)
  :doc "routine polls do not create a Calling row before an in-flight turn"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (mevedel-view-stream-spinner-hook
       '(:name "WriteStdin" :args (:execution_id "exec-1" :chars "")))
      (mevedel-view-stream-spinner-hook
       '(:name "ToolCall"
         :args (:expression "(WriteStdin :execution_id \"exec-1\" :chars \"\")"))))
    (with-current-buffer view-buf
      (should-not (string-match-p "Calling WriteStdin" (buffer-string)))
      (should-not (string-match-p "Calling ToolCall" (buffer-string))))
    (with-current-buffer data-buf
      (mevedel-view-stream-spinner-hook
       '(:name "WriteStdin"
         :args (:execution_id "exec-1" :chars "yes\n"))))
    (with-current-buffer view-buf
      (should (string-match-p "Calling WriteStdin" (buffer-string))))))


(mevedel-deftest mevedel-view--pending-tool-calls
  (:doc "tracks and renders the pending-tool live tail")
  ,test
  (test)

  :doc "routine polls show no pending row while input and stops stay visible"
  (mevedel-view-stream-test--with-buffers
    (let ((mevedel-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'mevedel-view-render-live-update) #'ignore))
        (with-current-buffer data-buf
          (dolist (poll '((:name "WriteStdin"
                          :args (:execution_id "exec-1" :chars ""))
                         (:name "WriteStdin"
                          :args (:execution_id "exec-2"))
                         (:name "ToolCall"
                          :args (:expression "(WriteStdin :execution_id \"exec-3\" :chars \"\")"))))
            (mevedel-view-stream-spinner-hook poll)
            (mevedel-view-stream-pre-tool poll))
          (with-current-buffer view-buf
            (should-not mevedel-view--pending-tool-calls)
            (should-not (string-match-p "Calling WriteStdin" (buffer-string)))
            (should-not (string-match-p "Calling ToolCall" (buffer-string)))
            (should (mevedel-view--request-progress-visible-p)))
          (mevedel-view-stream-pre-tool
           '(:name "WriteStdin" :args (:execution_id "exec-1"
                                         :chars "yes\n")))
          (mevedel-view-stream-pre-tool
           '(:name "StopExecution" :args (:execution_id "exec-2")))
          (with-current-buffer view-buf
            (should (= 2 (length mevedel-view--pending-tool-calls)))
            (should (string-match-p "Calling WriteStdin" (buffer-string)))
            (should (string-match-p "Calling StopExecution" (buffer-string))))
          (mevedel-view-stream-post-tool
           '(:name "WriteStdin" :args (:execution_id "exec-1" :chars "")))
          (with-current-buffer view-buf
            (should (= 2 (length mevedel-view--pending-tool-calls))))))))

  :doc "invalid empty-input controls keep pending error indicators"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min)))))
    (with-current-buffer data-buf
      (mevedel-view-stream-pre-tool
       '(:name "WriteStdin" :args (:execution_id "" :chars "")))
      (mevedel-view-stream-pre-tool
       '(:name "WriteStdin"
         :args (:execution_id "exec-1" :chars ""
                :yield_time_ms "not-a-duration")))
      (mevedel-view-stream-pre-tool
       '(:name "WriteStdin"
         :args (:execution_id "exec-1" :chars "" :bogus 1)))
      (mevedel-view-stream-pre-tool
       '(:name "ToolCall"
         :args (:expression "(WriteStdin :execution_id \"exec-1\" :chars \"\")"
                :bogus 1))))
    (with-current-buffer view-buf
      (should (= 4 (length mevedel-view--pending-tool-calls)))
      (should (string-match-p "Calling WriteStdin" (buffer-string)))))

  :doc "pre/post hooks add and remove entries for the arguments gptel sends"
  ;; gptel builds its tool-hook arguments from the name, the arguments,
  ;; and the result, so there is no call id to key on.
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'mevedel-view-render-live-update)
                 (lambda (&rest _) (cl-incf render-count))))
        (with-current-buffer data-buf
          (should-not
           (mevedel-view-stream-pre-tool
            '(:name "Read" :args (:file_path "a"))))
          (should-not
           (mevedel-view-stream-pre-tool
            '(:name "Grep" :args (:pattern "x"))))
          (with-current-buffer view-buf
            (should (equal '(((("Read" . "(:file_path \"a\")") . 1)
                              . "Calling Read: a...")
                             ((("Grep" . "(:pattern \"x\")") . 2)
                              . "Calling Grep: x..."))
                           mevedel-view--pending-tool-calls)))
          (should-not
           (mevedel-view-stream-post-tool
            '(:name "Read" :args (:file_path "a"))))))
      (with-current-buffer view-buf
        (should (equal '(((("Grep" . "(:pattern \"x\")") . 2)
                          . "Calling Grep: x..."))
                       mevedel-view--pending-tool-calls)))
      (should (= 3 render-count))))

  :doc "identical parallel calls each keep their own live line"
  ;; Two identical calls share a fingerprint, so collapsing them dropped
  ;; the line as soon as the first one finished, while the second was
  ;; still running.
  (mevedel-view-stream-test--with-buffers
    (let ((mevedel-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'mevedel-view-render-live-update) #'ignore))
        (with-current-buffer data-buf
          (dotimes (_ 2)
            (mevedel-view-stream-pre-tool
             '(:name "Read" :args (:file_path "a"))))
          (with-current-buffer view-buf
            (should (= 2 (length mevedel-view--pending-tool-calls))))
          (mevedel-view-stream-post-tool
           '(:name "Read" :args (:file_path "a")))
          (with-current-buffer view-buf
            (should (= 1 (length mevedel-view--pending-tool-calls))))
          (mevedel-view-stream-post-tool
           '(:name "Read" :args (:file_path "a")))
          (with-current-buffer view-buf
            (should-not mevedel-view--pending-tool-calls))))))

  :doc "tool-boundary renders are debounced and coalesced"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 0.02))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'mevedel-view-render-live-update)
                 (lambda (&rest _) (cl-incf render-count))))
        (with-current-buffer data-buf
          (should-not
           (mevedel-view-stream-pre-tool
            '(:name "Read" :args (:file_path "a"))))
          (should-not
           (mevedel-view-stream-pre-tool
            '(:name "Grep" :args (:pattern "x")))))
        (should (= 0 render-count))
        (let ((deadline (+ (float-time) 1.0)))
          (while (and (= render-count 0)
                      (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        (should (= 1 render-count)))))

  :doc "immediate pending live tail inserts above status-zone content"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker status-start)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--status-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view-render-live-update)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp calling))
                (should (numberp status))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render)))))

  :doc "pending live tail recovers above status-zone content when status marker detaches"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view-render-live-update)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp calling))
                (should (numberp status))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render)))))

  :doc "pending live tail stays above status-zone content after immediate render"
  (mevedel-view-stream-test--with-buffers
    (let ((mevedel-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp calling))
                (should (numberp status))
                (should (< calling status))
                (should (= 1 (cl-loop with start = 0
                                      while (string-match "Calling Read: a"
                                                          text start)
                                      count t
                                      do (setq start (match-end 0))))))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render)))))

  :doc "pending live tail recovers after existing history when status marker detaches"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (mevedel-view--insert-user-message "Previous turn")
          (goto-char (marker-position mevedel-view--status-marker))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view-render-live-update)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render)))))

  :doc "pending live tail stays after existing live assistant text"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
          (insert (propertize "Assistant\n"
                              'mevedel-view-type 'turn-header
                              'mevedel-view-turn-role 'assistant))
          (insert (propertize "Existing response\n"
                              'mevedel-view-type 'response
                              'mevedel-view-source '(1 . 2)))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view-render-live-update)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (existing (string-match-p "Existing response" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp existing))
                (should (numberp calling))
                (should (numberp status))
                (should (< existing calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render)))))

  :doc "pending live tail stays above propertized status rows"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (mevedel-view--insert-user-message "Previous turn")
          (goto-char (marker-position mevedel-view--status-marker))
          (insert (propertize "AGENT STATUS\n"
                              'mevedel-view-type 'agent-handle
                              'mevedel-view-agent-status t))
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view-render-live-update)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "AGENT STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render)))))

  :doc "pending live tail ignores stale attached status marker"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (mevedel-view--insert-user-message "Previous turn")
          (goto-char (marker-position mevedel-view--status-marker))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker (point-min))
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view-render-live-update)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (header (string-match-p "mevedel" text))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp header))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< header previous))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render)))))

  :doc "pending live tail recovers after collapsed turn summaries"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (insert (propertize "Previous turn\n"
                              'mevedel-view-type 'turn-summary))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view-render-live-update)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render)))))

  :doc "incremental render preserves status rows when status marker detaches"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position mevedel-view--status-marker)))
        (goto-char status-start)
        (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker nil)
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read: a")))
        (should (progn (mevedel-view-render-live-update data-buf) t))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (response (string-match-p "assistant text" text))
               (calling (string-match-p "Calling Read: a" text))
               (status (string-match-p "TASK STATUS" text)))
          (should (numberp response))
          (should (numberp calling))
          (should (numberp status))
          (should (< response calling))
          (should (< calling status))))))

  :doc "incremental render recovers stale in-flight marker"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position mevedel-view--status-marker)))
        (goto-char status-start)
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker nil)
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (mevedel-view-stream-set-in-flight-turn-start (point-min))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read: a")))
        (should (progn (mevedel-view-render-live-update data-buf) t))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (calling (string-match-p "Calling Read: a" text))
               (status (string-match-p "TASK STATUS" text)))
          (should-not old)
          (should (numberp response))
          (should (numberp calling))
          (should (numberp status))
          (should (< response calling))
          (should (< calling status))))))

  :doc "incremental render prefers source recovery over stale in-flight marker"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (history-start (mevedel-view--after-header-position)))
        (goto-char (marker-position mevedel-view--status-marker))
        (insert (propertize "Previous turn\n"
                            'mevedel-view-type 'turn-summary))
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker nil)
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (mevedel-view-stream-set-in-flight-turn-start history-start)
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (cl-letf (((symbol-function 'mevedel-view--render-agent-status)
                   (lambda () nil))
                  ((symbol-function 'mevedel-view--interaction-rebuild)
                   (lambda () nil)))
          (should (progn (mevedel-view-render-live-update data-buf) t)))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (previous (string-match-p "Previous turn" text))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (status (string-match-p "TASK STATUS" text)))
          (should (numberp previous))
          (should-not old)
          (should (numberp response))
          (should (numberp status))
          (should (< previous response))
          (should (< response status))))))

  :doc "incremental render ignores stale attached status marker"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position mevedel-view--status-marker)))
        (goto-char status-start)
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker (point-min))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (mevedel-view-stream-set-in-flight-turn-start (point-min))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read: a")))
        (should (progn (mevedel-view-render-live-update data-buf) t))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (header (string-match-p "mevedel" text))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (calling (string-match-p "Calling Read: a" text))
               (status (string-match-p "TASK STATUS" text)))
          (should (numberp header))
          (should-not old)
          (should (numberp response))
          (should (numberp calling))
          (should (numberp status))
          (should (< header response))
          (should (< response calling))
          (should (< calling status))))))

  :doc "incremental render ignores status marker before existing history"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (history-start (mevedel-view--after-header-position)))
        (goto-char (marker-position mevedel-view--status-marker))
        (insert (propertize "Previous turn\n"
                            'mevedel-view-type 'turn-summary))
        (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker history-start)
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (cl-letf (((symbol-function 'mevedel-view--render-agent-status)
                   (lambda () nil))
                  ((symbol-function 'mevedel-view--interaction-rebuild)
                   (lambda () nil)))
          (should (progn (mevedel-view-render-live-update data-buf) t)))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (previous (string-match-p "Previous turn" text))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (status (string-match-p "TASK STATUS" text)))
          (should (numberp previous))
          (should-not old)
          (should (numberp response))
          (should (numberp status))
          (should (< previous response))
          (should (< response status))))))

  :doc "incremental render ignores status marker inside status row"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position mevedel-view--status-marker)))
        (goto-char status-start)
        (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (let ((row-start (point)))
          (insert (propertize "TASK STATUS\n"
                              'mevedel-view-type 'agent-handle
                              'display "TASK STATUS"
                              'keymap mevedel-view--display-map
                              'read-only t))
          (set-marker mevedel-view--status-marker (+ row-start 5)))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (cl-letf (((symbol-function 'mevedel-view--render-agent-status)
                   (lambda () nil))
                  ((symbol-function 'mevedel-view--interaction-rebuild)
                   (lambda () nil)))
          (should (progn (mevedel-view-render-live-update data-buf) t)))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (status (string-match-p "TASK STATUS" text)))
          (should-not old)
          (should (numberp response))
          (should (numberp status))
          (should (< response status))))))

  :doc "incremental render restores aggregate status when status marker detaches"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((row '(:path "/root/verifier" :status running)))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda () (list row))))
          (mevedel-view--render-agent-status)
          (let ((inhibit-read-only t))
            (goto-char (marker-position mevedel-view--status-marker))
            (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
            (insert (propertize "Assistant\n"
                                'mevedel-view-type 'turn-header
                                'mevedel-view-turn-role 'assistant))
            (insert (propertize "old text\n"
                                'mevedel-view-type 'response
                                'mevedel-view-source '(1 . 2)))
            (set-marker mevedel-view--status-marker nil)
            (setq mevedel-view--data-turn-start
                  (with-current-buffer data-buf
                    (copy-marker (point-min)))))
          (should (progn (mevedel-view-render-live-update data-buf) t))
          (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                 (old (string-match-p "old text" text))
                 (response (string-match-p "assistant text" text))
                 (status (string-match-p "Running /root/verifier" text))
                 (prompt (string-match-p "^> " text)))
            (should-not old)
            (should (numberp response))
            (should (numberp status))
            (should (numberp prompt))
            (should (< response status))
            (should (< status prompt)))))))

  :doc "aggregate status stays below pending live tail with detached status marker"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((row '(:path "/root/verifier" :status running)))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda () (list row))))
          (let ((inhibit-read-only t))
            (goto-char (marker-position mevedel-view--status-marker))
            (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
            (insert (propertize "Assistant\n"
                                'mevedel-view-type 'turn-header
                                'mevedel-view-turn-role 'assistant))
            (insert (propertize "old text\n"
                                'mevedel-view-type 'response
                                'mevedel-view-source '(1 . 2)))
            (set-marker mevedel-view--status-marker nil)
            (set-marker mevedel-view--interaction-marker (point))
            (set-marker mevedel-view--input-marker (point))
            (setq mevedel-view--data-turn-start
                  (with-current-buffer data-buf
                    (copy-marker (point-min))))
            (setq mevedel-view--pending-tool-calls
                  (list (cons "call-1" "Calling Read: a"))))
          (should (progn (mevedel-view-render-live-update data-buf) t))
          (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                 (old (string-match-p "old text" text))
                 (response (string-match-p "assistant text" text))
                 (calling (string-match-p "Calling Read: a" text))
                 (status (string-match-p "Running /root/verifier" text)))
            (should-not old)
            (should (numberp response))
            (should (numberp calling))
            (should (numberp status))
            (should (< response calling))
            (should (< calling status)))))))

  :doc "aggregate status ignores stale attached status marker"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((row '(:path "/root/verifier" :status running)))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda () (list row))))
          (let ((inhibit-read-only t))
            (goto-char (marker-position mevedel-view--status-marker))
            (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
            (insert (propertize "Assistant\n"
                                'mevedel-view-type 'turn-header
                                'mevedel-view-turn-role 'assistant))
            (insert (propertize "old text\n"
                                'mevedel-view-type 'response
                                'mevedel-view-source '(1 . 2)))
            (insert "TASK STATUS\n")
            (set-marker mevedel-view--status-marker (point-min))
            (set-marker mevedel-view--interaction-marker (point))
            (set-marker mevedel-view--input-marker (point))
            (setq mevedel-view--data-turn-start
                  (with-current-buffer data-buf
                    (copy-marker (point-min)))))
          (should (progn (mevedel-view-render-live-update data-buf) t))
          (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                 (header (string-match-p "mevedel" text))
                 (old (string-match-p "old text" text))
                 (response (string-match-p "assistant text" text))
                 (status (string-match-p "Running /root/verifier" text)))
            (should (numberp header))
            (should-not old)
            (should (numberp response))
            (should (numberp status))
            (should (< header response))
            (should (< response status)))))))

  :doc "pending live tail ignores stale input marker"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (mevedel-view--insert-user-message "Previous turn")
          (goto-char (marker-position mevedel-view--status-marker))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--input-marker (point-min))
          (mevedel-view-stream-set-in-flight-turn-start (point-min))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view-render-live-update)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render)))))

  :doc "tool hooks do not return rendered agent-status strings to gptel"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min)))))
    (let ((mevedel-view-tool-boundary-render-delay 0))
      (cl-letf (((symbol-function 'mevedel-view-render-live-update)
                 (lambda (&rest _)
                   #(" ─── agents: 1 running [+] ─────────────────────────────────\n"
                     0 61 (font-lock-face mevedel-view-zone-separator)))))
        (with-current-buffer data-buf
          (should-not
           (mevedel-view-stream-pre-tool
            '(:name "Read" :args (:file_path "a"))))
          (should-not
           (mevedel-view-stream-post-tool
            '(:name "Read" :args (:file_path "a"))))))))

  :doc "Agent pre-tool hook does not add a duplicate pending Calling Agent line"
  (mevedel-view-stream-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'mevedel-view-render-live-update)
                 (lambda (&rest _) (cl-incf render-count))))
        (with-current-buffer data-buf
          (should-not
           (mevedel-view-stream-pre-tool
            '(:id "call-1" :name "Agent"
              :args (:task_name "explore"
                     :message "Inspect the repository."))))))
      (with-current-buffer view-buf
        (should-not mevedel-view--pending-tool-calls)
        (should (= 1 render-count)))))

  :doc "rendering caps visible calls and adds a truncation tail"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-pending-tools-visible-max 2))
        (setq mevedel-view--pending-tool-calls
              '(("1" . "Calling Read...")
                ("2" . "Calling Grep...")
                ("3" . "Calling Bash...")))
        (mevedel-view--insert-pending-tool-lines
         (cl-subseq mevedel-view--pending-tool-calls 0 2))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Calling Read" text))
          (should (string-match-p "Calling Grep" text))
          (should-not (string-match-p "Calling Bash" text))
          (should (string-match-p "1 more tools running" text))))))

  :doc "post-tool hook removes only the completed pending fragment"
  (mevedel-view-stream-test--with-buffers
    (let ((mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-max))))
        (setq mevedel-view--pending-tool-calls
              (list (cons (mevedel-view--pending-tool-claim-key
                           '(:name "Read" :args (:file_path "a")))
                          "Calling Read...")
                    (cons (mevedel-view--pending-tool-claim-key
                           '(:name "Grep" :args (:pattern "x")))
                          "Calling Grep...")))
        (mevedel-view--refresh-pending-tool-lines))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (mevedel-view-stream-post-tool
               '(:name "Read" :args (:file_path "a"))))
            (with-current-buffer view-buf
              (let ((text (buffer-substring-no-properties
                           (point-min) (point-max))))
                (should-not (string-match-p "Calling Read" text))
                (should (string-match-p "Calling Grep" text))
                (goto-char (point-min))
                (should (search-forward "Calling Grep" nil t))
                (let ((grep-pos (match-beginning 0)))
                  (should (eq 'history-live
                              (get-text-property
                               grep-pos 'mevedel-view-zone-namespace)))
                  ;; The fragment is identified by the pending key,
                  ;; which stays put when an earlier call finishes.
                  (should (equal (car (car mevedel-view--pending-tool-calls))
                                 (get-text-property
                                  grep-pos 'mevedel-view-zone-id))))
                (should (= 1 (length mevedel-view--pending-tool-calls)))
                (should (equal "Calling Grep..."
                               (cdar mevedel-view--pending-tool-calls))))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render)))))

  :doc "repair audit redraw preserves a single-line composer and point"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-repair-audited-tool data-buf)
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min))))
      (mevedel-view-stream-test--insert-composer-draft "keep drafting" 5))
    (let ((mevedel-view-tool-boundary-render-delay 0))
      (with-current-buffer data-buf
        (mevedel-view-stream-post-tool
         '(:id "repair-call" :name "Collect"
               :args (:names ["alice"])))))
    (with-current-buffer view-buf
      (should (equal "keep drafting" (mevedel-view--input-text)))
      (should (= (point) (+ (mevedel-view--input-start) 5)))
      (should (string-match-p
               "tool input repaired"
               (buffer-substring-no-properties
                (point-min) mevedel-view--input-marker)))))

  :doc "repair audit redraw preserves a multiline > composer and point"
  (mevedel-view-stream-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (mevedel-view-stream-test--insert-repair-audited-tool data-buf)
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min))))
        (mevedel-view-stream-test--insert-composer-draft draft 4))
      (let ((mevedel-view-tool-boundary-render-delay 0))
        (with-current-buffer data-buf
          (mevedel-view-stream-post-tool
           '(:id "repair-call" :name "Collect"
                 :args (:names ["alice"])))))
      (with-current-buffer view-buf
        (should (equal draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (should (string-match-p
                 "tool input repaired"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))))

  :doc "post-tool hook deletes the live tail when no replacement text is ready"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (setq mevedel-view--pending-tool-calls
            (list (cons (mevedel-view--pending-tool-claim-key
                         '(:name "Agent"
                           :args (:task_name "explore"
                                  :message "Inspect.")))
                        "Calling Agent: explorer...")))
      (mevedel-view--insert-pending-tool-lines
       mevedel-view--pending-tool-calls)
      (should (string-match-p "Calling Agent"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))
    (with-current-buffer data-buf
      (mevedel-view-stream-post-tool
       '(:name "Agent"
         :args (:task_name "explore" :message "Inspect."))))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should-not (string-match-p "Calling Agent" text))
        (should-not mevedel-view--pending-tool-calls))))

  :doc "final response render clears pending live tail before rendering"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "final answer\n" 'response)
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min))))
      (setq mevedel-view--pending-tool-calls
            '(("call-1" . "Calling Agent: explorer...")))
      (mevedel-view--insert-pending-tool-lines
       mevedel-view--pending-tool-calls))
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (string-match-p "final answer" text))
        (should-not (string-match-p "Calling Agent" text))
        (should-not mevedel-view--pending-tool-calls))))

  :doc "full rerender preserves ordinary calling text and recreates pending fragments"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--pending-tool-calls
            '(("call-1" . "Calling Read...")))
      (let ((inhibit-read-only t)
            (tail-start (marker-position mevedel-view--input-marker)))
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (insert "Assistant\n| Calling Read...\n")
        (setq mevedel-view--in-flight-turn-start
              (copy-marker tail-start nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (= 2 (mevedel-view-stream-test--count-substring
                      "Calling Read" text))))
      (let (plain-seen fragment-pos)
        (goto-char (point-min))
        (while (search-forward "Calling Read" nil t)
          (let ((pos (match-beginning 0)))
            (if (eq 'history-live
                    (get-text-property pos 'mevedel-view-zone-namespace))
                (setq fragment-pos pos)
              (setq plain-seen t))))
        (should plain-seen)
        (should fragment-pos)
        (should (equal "call-1"
                       (get-text-property
                        fragment-pos 'mevedel-view-zone-id)))
        (should (eq (mevedel-view-zone-region 'history-live)
                    (get-text-property
                     fragment-pos 'mevedel-view-zone-region))))))

  :doc "cleanup ignores stale pending regions without live fragments"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            start
            stale-region)
        (goto-char mevedel-view--input-marker)
        (setq start (point))
        (insert "Calling Read...\n")
        (setq stale-region
              (make-overlay start (point) (current-buffer) nil nil))
        (overlay-put stale-region 'mevedel-view-zone 'history-live))
      (mevedel-view--delete-pending-tool-live-lines)
      (should (string-match-p "Calling Read"
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
      (should-not (mevedel-view-zone-region 'history-live))))

  :doc "incremental render preserves live tail when no replacement content is ready"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (start nil))
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (setq start (point))
        (insert "Assistant\n› Calling Read: mevedel-pipeline.el...\n")
        (setq mevedel-view--in-flight-turn-start (copy-marker start nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (setq mevedel-view--pending-tool-calls nil)
      (mevedel-view-render-live-update data-buf)
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (string-match-p "Calling Read: mevedel-pipeline.el" text)))))

  :doc "explicit response bounds do not blank live tail without replacement content"
  (mevedel-view-stream-test--with-buffers
    (let (start end)
      (with-current-buffer data-buf
        (setq start (point-max))
        (setq end (point-max)))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (tail-start nil))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (setq tail-start (point))
          (insert "Assistant\n... Thinking... (1 lines)\nCalling Read...\n")
          (setq mevedel-view--in-flight-turn-start
                (copy-marker tail-start nil))
          (set-marker mevedel-view--status-marker (point))
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (set-marker-insertion-type mevedel-view--input-marker nil))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-max))))
        (setq mevedel-view--pending-tool-calls nil)
        (mevedel-view-render-settle data-buf start end)
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p "Assistant" text))
          (should (string-match-p "Calling Read" text))))))

  :doc "incremental render keeps progress row beside pending tool details"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker nil))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (mevedel-view--start-spinner "Thinking...")
      (setq mevedel-view--pending-tool-calls
            '(("call-1" . "Calling Read...")))
      (mevedel-view-render-live-update data-buf)
      (should (mevedel-view--request-progress-visible-p))
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (string-match-p "Working" text))
        (should (string-match-p "Calling Read" text))))))


;;
;;; Re-render idempotence with renderer


;;
;;; Request progress

(mevedel-deftest mevedel-view--render-request-progress ()
  ,test
  (test)
  :doc "stream, full render and spinner ticks keep progress spacing and draft point"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "Hello\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view-stream-begin-turn
       mevedel-view--status-marker
       (with-current-buffer data-buf (copy-marker (point-min))))
      (mevedel-view-stream-test--insert-composer-draft "> quoted\nsecond line" 4)
      (mevedel-view-render-live-update data-buf)
      (let ((row (save-excursion
                   (goto-char (point-min))
                   (search-forward "Working...")
                   (line-number-at-pos))))
        (dolist (refresh '(mevedel-view--full-rerender
                           mevedel-view--spinner-tick
                           mevedel-view--full-rerender))
          (funcall refresh)
          (should (equal "> quoted\nsecond line" (mevedel-view--input-text)))
          (should (= (point) (+ (mevedel-view--input-start) 4)))
          (save-excursion
            (goto-char (point-min))
            (search-forward "Working...")
            (should (= row (line-number-at-pos)))))
        (should (= (mevedel-view-zone-start 'progress)
                   (plist-get (mevedel-view-zone-fragment-bounds
                               'progress 'request)
                              :start)))))))

(mevedel-deftest mevedel-view--start-spinner ()
  ,test
  (test)
  :doc "creates and removes progress/request fragment"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Working...")
      (goto-char (point-min))
      (should (search-forward "Working" mevedel-view--input-marker t))
      (goto-char (match-beginning 0))
      (should (eq 'progress
                  (get-text-property
                   (point) 'mevedel-view-zone-namespace)))
      (should (eq 'request
                  (get-text-property
                   (point) 'mevedel-view-zone-id)))
      (should (eq (mevedel-view-zone-region 'progress)
                  (get-text-property
                   (point) 'mevedel-view-zone-region)))
      (let ((zone-text (buffer-substring-no-properties
                        (point)
                        (mevedel-view--input-start))))
        (should (string-match-p "Working[^\n]*\n\n> \\'" zone-text)))
      (mevedel-view--stop-spinner)
      (should-not (text-property-any
                   (point-min) mevedel-view--input-marker
                   'mevedel-view-zone-namespace 'progress))))

  :doc "request spinner stays below queued interaction text"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-queued"
                       :root temporary-file-directory
                       :name "spinner-queued"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--start-spinner "Working...")
        (setf (mevedel-session-pending-follow-ups session)
              (list (list :input "queued while busy"
                          :display-text "queued while busy")))
        (mevedel-view--interaction-rebuild)
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (queued (string-match-p "queued while busy" text))
               (working (string-match-p "Working" text))
               (prompt (string-match-p "\n> " text working)))
          (should queued)
          (should working)
          (should prompt)
          (should (< queued working))
          (should (< working prompt)))
        (mevedel-view--stop-spinner))))

  :doc "queued interaction rebuild suppresses modification hooks"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-queued-hooks"
                       :root temporary-file-directory
                       :name "spinner-queued-hooks"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--start-spinner "Working...")
        (setf (mevedel-session-pending-follow-ups session)
              (list (list :input "queued while busy"
                          :display-text "queued while busy")))
        (let ((changes 0))
          (add-hook 'after-change-functions
                    (lambda (&rest _ignore)
                      (cl-incf changes))
                    nil t)
          (mevedel-view--interaction-rebuild)
          (should (= 0 changes)))
        (mevedel-view--stop-spinner))))

  :doc "pending tool rows stay above queued text and request spinner"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-pending-queued"
                       :root temporary-file-directory
                       :name "spinner-pending-queued"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--start-spinner "Working...")
        (mevedel-view--insert-pending-tool-lines
         (list (cons "call-1" "Calling Read: a")))
        (setf (mevedel-session-pending-follow-ups session)
              (list (list :input "queued while busy"
                          :display-text "queued while busy")))
        (mevedel-view--interaction-rebuild)
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (calling (string-match-p "Calling Read: a" text))
               (queued (string-match-p "queued while busy" text))
               (working (string-match-p "Working" text))
               (prompt (string-match-p "\n> " text working)))
          (should calling)
          (should queued)
          (should working)
          (should prompt)
          (should (< calling queued))
          (should (< queued working))
          (should (< working prompt)))
        (mevedel-view--stop-spinner))))

  :doc "pending tool refresh suppresses modification hooks"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((changes 0))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read…")))
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--refresh-pending-tool-lines)
        (should (= 0 changes))
        (let ((pos (text-property-any
                    (point-min) mevedel-view--input-marker
                    'mevedel-view-pending-tool-live t)))
          (should pos)
          (should (eq 'history-live
                      (get-text-property
                       pos 'mevedel-view-zone-namespace)))
          (should (equal "call-1"
                         (get-text-property
                          pos 'mevedel-view-zone-id)))
          (should (eq (mevedel-view-zone-region 'history-live)
                      (get-text-property
                       pos 'mevedel-view-zone-region)))
          (should (< pos (marker-position mevedel-view--status-marker)))))))

  :doc "pending tool refresh preserves composer text and point"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view-stream-test--insert-composer-draft "draft\n> keep typing" 8)
      (let ((input-offset (- (point) (mevedel-view--input-start))))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read…")))
        (mevedel-view--refresh-pending-tool-lines)
        (should (equal "draft\n> keep typing"
                       (mevedel-view--input-text)))
        (should (= (- (point) (mevedel-view--input-start))
                   input-offset)))))

  :doc "request progress render preserves selected-window history point"
  (mevedel-view-stream-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (insert (propertize "Earlier answer\n"
                              'mevedel-view-type 'response
                              'mevedel-view-source '(1 . 2)))
          (set-marker-insertion-type mevedel-view--input-marker nil)
          (set-marker mevedel-view--status-marker
                      mevedel-view--input-marker)
          (set-marker mevedel-view--interaction-marker
                      mevedel-view--input-marker))
        (goto-char (point-min))
        (search-forward "Earlier")
        (goto-char (match-beginning 0))
        (let ((point-before (point)))
          (setq mevedel-view--spinner-start-time (current-time))
          (mevedel-view--ensure-request-progress data-buf)
          (should (= (window-point (selected-window)) point-before))
          (should (= (point) point-before))
          (should (looking-at-p "Earlier"))))))

  :doc "pre-tool render preserves selected-window composer point"
  (mevedel-view-stream-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((mevedel-view-spinner-style 'ascii))
        (with-current-buffer view-buf
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker nil))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf (copy-marker (point-min))))
          (mevedel-view-stream-test--insert-composer-draft
           "> quoted\nsecond line" 4))
        (let ((input-offset
               (with-current-buffer view-buf
                 (- (window-point (selected-window))
                    (mevedel-view--input-start)))))
          (with-current-buffer data-buf
            (mevedel-view-stream-pre-tool
             '(:name "Read" :args (:file_path "foo.el"))))
          (with-current-buffer view-buf
            (should (mevedel-view--position-in-input-region-p
                     (window-point (selected-window))))
            (should (= (- (window-point (selected-window))
                          (mevedel-view--input-start))
                       input-offset))
            (should (= (point) (window-point (selected-window))))
            (should (equal "> quoted\nsecond line"
                           (mevedel-view--input-text)))
            (should (looking-at-p "oted")))))))

  :doc "WaitAgent pending redraw shows waiting text and preserves a leading-> draft"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker nil))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min))))
      (mevedel-view-stream-test--insert-composer-draft
       "> quoted\nsecond line" 4))
    (with-current-buffer data-buf
      (mevedel-view-stream-pre-tool
       '(:id "wait-1" :name "WaitAgent" :args (:timeout_ms 10000))))
    (with-current-buffer view-buf
      (should (equal "> quoted\nsecond line" (mevedel-view--input-text)))
      (should (string-match-p
               "Waiting for agents"
               (buffer-substring-no-properties (point-min) (point-max))))))

  :doc "pending tool refresh keeps ordinary calling rows and adds a fragment"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (goto-char mevedel-view--input-marker)
        (insert (propertize "Calling Read...\n"
                            'mevedel-view-source '(1 . 2)
                            'mevedel-view-type 'response))
        (set-marker-insertion-type mevedel-view--input-marker t)
        (insert "| Calling Read...\n")
        (setq mevedel-view--in-flight-turn-start
              (copy-marker (point-min) nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (setq mevedel-view--pending-tool-calls
            (list (cons "call-1" "Calling Read...")))
      (mevedel-view--refresh-pending-tool-lines)
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (= 3 (mevedel-view-stream-test--count-substring
                      "Calling Read" text))))
      (let (occurrences)
        (goto-char (point-min))
        (while (search-forward "Calling Read" nil t)
          (push (list :source (get-text-property
                               (match-beginning 0) 'mevedel-view-source)
                      :namespace (get-text-property
                                  (match-beginning 0)
                                  'mevedel-view-zone-namespace))
                occurrences))
        (should (cl-some (lambda (entry) (plist-get entry :source))
                         occurrences))
        (should (cl-some (lambda (entry)
                           (eq 'history-live (plist-get entry :namespace)))
                         occurrences))
        (should (cl-some (lambda (entry)
                           (and (not (plist-get entry :source))
                                (not (plist-get entry :namespace))))
                         occurrences)))))

  :doc "update replaces spinner text"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (mevedel-view--update-spinner "Calling Read...")
      (let ((text (buffer-substring-no-properties
                   (overlay-start
                    (mevedel-view-zone-region 'progress))
                   (overlay-end
                    (mevedel-view-zone-region 'progress)))))
        (should (string-match-p "Calling Read" text))
        (should-not (string-match-p "Thinking" text)))
      (mevedel-view--stop-spinner)))

  :doc "spinner freezes elapsed time and decorative motion while input is pending"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-agents"
                       :root temporary-file-directory
                       :name "spinner-agents"))
           (session (mevedel-session-create "main" workspace))
           (request
            (mevedel-request--create
             :session session
             :started-at
             (time-subtract (current-time) (seconds-to-time 12))))
           (mevedel-view-spinner-style 'ascii)
           (mevedel-view-spinner-power-policy 'full))
      (setf (mevedel-session-agent-registry session)
            (list (cons "/root/spin"
                        (mevedel-agent-record--create
                         :id "worker--spin" :path "/root/spin"
                         :parent-path "/root" :activity 'running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request request))
      (mevedel-view-stream-test--with-visible-view
        (mevedel-view--start-spinner "Thinking...")
        (mevedel-view--interaction-register
         '(:kind ask :id ask :origin "/root" :body "ask"))
        (let* ((pause-started
                (mevedel-request-active-work-pause-started-at request))
               (region (mevedel-view-zone-region 'progress))
               (frame-position
                (text-property-any
                 (overlay-start region) (overlay-end region)
                 'mevedel-view-spinner-frame t))
               (frame (get-text-property frame-position 'display))
               (text (buffer-substring-no-properties
                      (overlay-start region) (overlay-end region))))
          (should (string-match-p "Waiting for input" text))
          (should (string-match-p "[0-9]+s" text))
          (should (string-match-p "1 agent running" text))
          (should
           (< (abs
               (- (mevedel-request-active-elapsed-seconds
                   request pause-started)
                  (mevedel-request-active-elapsed-seconds
                   request
                   (time-add pause-started (seconds-to-time 5)))))
              0.001))
          ;; Neither elapsed time nor glyphs advance during the prompt,
          ;; even if an already delivered callback reaches the tick function.
          (dolist (seconds '(0.24 0.48 0.72))
            (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                       (lambda () seconds)))
              (mevedel-view--spinner-tick))
            (should (equal frame (get-text-property frame-position 'display))))
          (should (equal text (buffer-substring-no-properties
                               (overlay-start region) (overlay-end region)))))
        (mevedel-view--interaction-unregister 'ask)
        (let ((text (buffer-substring-no-properties
                     (overlay-start (mevedel-view-zone-region 'progress))
                     (overlay-end (mevedel-view-zone-region 'progress)))))
          (should (string-match-p "Working\\.\\.\\." text)))
        (mevedel-view--stop-spinner))))

  :doc "spinner ticks replace dynamic metadata instead of appending it"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-no-pileup"
                       :root temporary-file-directory
                       :name "spinner-no-pileup"))
           (session (mevedel-session-create "main" workspace))
           (started (time-subtract (current-time) (seconds-to-time 12))))
      (setf (mevedel-session-agent-registry session)
            (list (cons "/root/spin"
                        (mevedel-agent-record--create
                         :id "worker--spin" :path "/root/spin"
                         :parent-path "/root" :activity 'running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :session session
                     :started-at started)))
      (with-current-buffer view-buf
        (mevedel-view--start-spinner "Thinking...")
        (dotimes (_ 3)
          (mevedel-view--spinner-tick))
        (let ((text (buffer-substring-no-properties
                     (overlay-start
                      (mevedel-view-zone-region 'progress))
                     (overlay-end
                      (mevedel-view-zone-region 'progress)))))
          (should (= 1 (cl-loop with start = 0
                                while (string-match "agent running" text start)
                                count t
                                do (setq start (match-end 0)))))
          (should-not (string-match-p
                       "agent running.*agent running" text)))
        (mevedel-view--stop-spinner))))

  :doc "spinner tick suppresses modification hooks"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read...")))
            (changes 0))
        (mevedel-view--start-spinner "Thinking...")
        (mevedel-view--insert-pending-tool-lines
         mevedel-view--pending-tool-calls)
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--spinner-tick)
        (should (= 0 changes))
        (mevedel-view--stop-spinner))))

  :doc "spinner ticks update frames without rewriting unchanged status text"
  (mevedel-view-stream-test--with-buffers
    (let ((now (seconds-to-time 1000))
          (reconciles 0)
          (original-reconcile (symbol-function 'mevedel-view-zone-reconcile)))
      (cl-letf (((symbol-function 'current-time) (lambda () now))
                ((symbol-function 'mevedel-view-zone-reconcile)
                 (lambda (zone start end fragments)
                   (when (eq zone 'progress)
                     (cl-incf reconciles))
                   (funcall original-reconcile zone start end fragments))))
        (with-current-buffer view-buf
          (let ((mevedel-view-spinner-style 'ascii))
            (mevedel-view--start-spinner "Working...")
            (setq reconciles 0)
            (dotimes (_ 10)
              (mevedel-view--spinner-tick))
            (should (= 0 reconciles))
            (let* ((region (mevedel-view-zone-region 'progress))
                   (frame-pos
                    (text-property-any
                     (overlay-start region) (overlay-end region)
                     'mevedel-view-spinner-frame t)))
              (should frame-pos)
              (should (equal (get-text-property frame-pos 'display)
                             "- Working...")))
            (mevedel-view--stop-spinner))))))

  :doc "decorated spinner status is normalized to its base label"
  (should (equal "Working..."
                 (mevedel-view--spinner-base-status
                  "Working... · 14s · 1 agent running · 21s · 2 agents running")))

  :doc "spinner marker refresh keeps the live line outside composer input"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (goto-char (point-max))
      (insert "/edits")
      (should (equal "/edits" (mevedel-view--input-text)))
      (mevedel-view--spinner-tick)
      (should (equal "/edits" (mevedel-view--input-text)))
      (mevedel-view--stop-spinner)))

  :doc "spinner tick preserves composer point while drafting"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (mevedel-view-stream-test--insert-composer-draft "/edits")
      (let ((point-before (point)))
        (mevedel-view--spinner-tick)
        (should (equal "/edits" (mevedel-view--input-text)))
        (should (= (point) point-before)))
      (mevedel-view--stop-spinner)))

  :doc "spinner tick preserves point on a visible permission prompt"
  (let ((mevedel-view-spinner-animate nil))
    (mevedel-view-stream-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (insert (make-string 10 ?\n))))
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "Permission Request\nTool: Read\nPath: .git/HEAD\n"
               :keymap (make-sparse-keymap)
               :entry 'permission-entry :activate #'ignore))
        (mevedel-view--start-spinner "Working...")
        (goto-char (overlay-start (mevedel-view-zone-region 'interaction)))
        (search-forward "Read")
        (let ((permission-point (point)))
          (save-excursion
            (goto-char (point-min))
            (forward-line 2)
            (set-window-start (selected-window) (point) t))
          (goto-char permission-point)
          (set-window-point (selected-window) permission-point))
        (redisplay t)
        (should (> (window-start) (point-min)))
        (should (>= (window-end nil t) (point-max)))
        (let ((point-before (point)))
          (mevedel-view--spinner-tick)
          (should (= point-before (point))))
        (mevedel-view--stop-spinner))))

  :doc "input read excludes request progress fragment"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (goto-char (point-max))
      (insert "/edits")
      (should (equal "/edits" (mevedel-view--input-text)))))

  :doc "ASCII fallback frames can be selected"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full))
        (mevedel-view--start-spinner "Working...")
        (let* ((region (mevedel-view-zone-region 'progress))
               (pos (text-property-any
                     (overlay-start region) (overlay-end region)
                     'mevedel-view-spinner-frame t)))
          (should (string-match-p "Working"
                                  (buffer-substring-no-properties
                                   (overlay-start region) (overlay-end region))))
          (should (equal "- Working..." (get-text-property pos 'display))))
        (mevedel-view--stop-spinner))))

  :doc "spinner tick updates pending tool frame spans"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-tool-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read..."))))
        (mevedel-view--refresh-pending-tool-lines)
        (let ((frame-pos (text-property-any
                          (point-min) (point-max)
                          'mevedel-view-inline-spinner-frame t)))
          (should frame-pos)
          (should (equal (get-text-property frame-pos 'display) "- "))
          (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                     (lambda () 0.24)))
            (mevedel-view--spinner-tick))
          (should (equal (get-text-property frame-pos 'display) "\\ "))))))

  :doc "recapturing spans detaches the markers they replace"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-power-policy 'full))
        (unwind-protect
            (progn
              (mevedel-view--start-spinner "Thinking...")
              (let ((old mevedel-view--spinner-label-target))
                (should (marker-buffer (car old)))
                (mevedel-view--capture-request-animation-target)
                (should-not (marker-buffer (car old)))
                (should-not (marker-buffer (cdr old)))
                (should (marker-buffer
                         (car mevedel-view--spinner-label-target)))))
          (mevedel-view--stop-spinner)))))

  :doc "text streamed in at the label's start stays outside its span"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-power-policy 'full))
        (unwind-protect
            (progn
              (mevedel-view--start-spinner "Thinking...")
              (let* ((target mevedel-view--spinner-label-target)
                     (label (buffer-substring-no-properties
                             (car target) (cdr target))))
                (let ((inhibit-read-only t))
                  (save-excursion
                    (goto-char (car target))
                    (insert "streamed words\n")))
                (should (equal label (buffer-substring-no-properties
                                      (car target) (cdr target))))
                (should (eq t (get-text-property
                               (car target) 'mevedel-view-spinner-frame)))))
          (mevedel-view--stop-spinner)))))

  :doc "a request waiting for input holds its indicators still"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-power-policy 'full)
            (request (mevedel-request--create :started-at (current-time))))
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-view--spinner-request)
                       (lambda () request)))
              (mevedel-view--start-spinner "Thinking...")
              (should (mevedel--ui-timer-pending-p mevedel-view--spinner-timer))
              (mevedel-request-set-active-work-paused request t)
              (mevedel-view--start-spinner-timer)
              (should-not mevedel-view--spinner-timer)
              (mevedel-request-set-active-work-paused request nil)
              (mevedel-view--start-spinner-timer)
              (should (mevedel--ui-timer-pending-p mevedel-view--spinner-timer)))
          (mevedel-view--stop-spinner)))))

  :doc "a semantic tick with an unchanged plan rearms the delivered timer"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-power-policy 'full))
        (unwind-protect
            (progn
              (mevedel-view--start-spinner "Thinking...")
              (let ((timer mevedel-view--spinner-timer))
                (should (mevedel--ui-timer-pending-p timer))
                ;; Deliver it as Emacs does: off the list, then its callback.
                (mevedel--ui-timer-cancel timer)
                (setq mevedel-view--spinner-last-second nil)
                (funcall (timer--function timer))
                (should (eq timer mevedel-view--spinner-timer))
                (should (mevedel--ui-timer-pending-p timer))))
          (mevedel-view--stop-spinner)))))

  :doc "shimmer tool rows animate their verb and tool name"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-tool-spinner-style 'shimmer)
            (mevedel-view-spinner-power-policy 'full)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read: notes.org..."))))
        (mevedel-view--refresh-pending-tool-lines)
        (let* ((start (text-property-any (point-min) (point-max)
                                         'mevedel-view-inline-spinner-frame t))
               (end (next-single-property-change
                     start 'mevedel-view-inline-spinner-frame)))
          (should (equal "Calling Read" (buffer-substring-no-properties start end)))
          (should (string-search "Calling Read" (get-text-property start 'display)))
          (should (equal ": notes.org..."
                         (buffer-substring-no-properties
                          end (save-excursion (goto-char end)
                                              (line-end-position)))))))))

  :doc "probes once when a new span is not yet within the drawn window"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-power-policy 'full)
            (mevedel-view--spinner-probed-targets nil))
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-view--animation-visible-p) #'ignore)
                      ((symbol-function 'mevedel-view--animation-target-visible-p)
                       #'ignore)
                      ((symbol-function 'mevedel-view--animation-window-attended-p)
                       (lambda (_) t)))
              (mevedel-view--start-spinner "Thinking...")
              (should-not mevedel-view--spinner-timer)
              (let ((probe mevedel-view--spinner-probe-timer))
                (should (mevedel--ui-timer-pending-p probe))
                ;; The same spans are not probed again.
                (mevedel--ui-timer-cancel probe)
                (setq mevedel-view--spinner-probe-timer nil)
                (mevedel-view--start-spinner-timer)
                (should-not mevedel-view--spinner-probe-timer)))
          (mevedel-view--stop-spinner)))))

  :doc "the probe rearms the spinner once the span has been drawn"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-power-policy 'full)
            (mevedel-view--spinner-probed-targets nil)
            (drawn nil))
        (unwind-protect
            (cl-letf* ((visible (symbol-function
                                 'mevedel-view--animation-target-visible-p))
                       ((symbol-function 'mevedel-view--animation-target-visible-p)
                        (lambda (&rest args) (and drawn (apply visible args))))
                       ((symbol-function 'mevedel-view--animation-window-attended-p)
                        (lambda (_) t)))
              (mevedel-view--start-spinner "Thinking...")
              (should-not mevedel-view--spinner-timer)
              (setq drawn t)
              (funcall (timer--function mevedel-view--spinner-probe-timer))
              (should (mevedel--ui-timer-pending-p mevedel-view--spinner-timer)))
          (mevedel-view--stop-spinner)))))

  :doc "spinner tick does not move point on pending tool frame"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-tool-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read..."))))
        (mevedel-view--refresh-pending-tool-lines)
        (goto-char (text-property-any
                    (point-min) (point-max)
                    'mevedel-view-inline-spinner-frame t))
        (let ((point-before (point)))
          (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                     (lambda () 0.24)))
            (mevedel-view--spinner-tick))
          (should (= (point) point-before))))))

  :doc "spinner tick updates only history-live and preserves the composer"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-tool-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read..."))))
        (mevedel-view--refresh-pending-tool-lines)
        (let ((real-pos
               (text-property-any
                (point-min) (point-max)
                'mevedel-view-inline-spinner-frame t))
              (decoy-pos (point-min))
              (inhibit-read-only t))
          (put-text-property
           decoy-pos (1+ decoy-pos)
           'mevedel-view-inline-spinner-frame t)
          (put-text-property decoy-pos (1+ decoy-pos) 'display "decoy")
          (mevedel-view-stream-test--insert-composer-draft
           "> quoted\nsecond line" 4)
          (let ((point-before (point))
                (draft-before (mevedel-view--input-text)))
            (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                       (lambda () 0.24)))
              (mevedel-view--spinner-tick))
            (should (equal "\\ " (get-text-property real-pos 'display)))
            (should (equal "decoy"
                           (get-text-property decoy-pos 'display)))
            (should (= point-before (point)))
            (should (equal draft-before (mevedel-view--input-text))))))))

  :doc "incremental response keeps request progress row visible"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (mevedel-view-stream-test--insert-data data-buf "Partial answer.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Prompt")
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (mevedel-view--start-spinner "Thinking...")
        (mevedel-view-render-live-update data-buf)
        (should (mevedel-view--request-progress-visible-p))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Partial answer" text))
          (should (string-match-p "Working" text))))))

  :doc "direct data-buffer request begin starts request progress"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "direct-progress"
                       :root temporary-file-directory
                       :name "direct-progress"))
           (session (mevedel-session-create "main" workspace))
           position fsm)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :session session
                     :started-at (current-time)))
        (setq position (copy-marker (point-max) nil))
        (setq fsm (gptel-make-fsm
                   :info (list :buffer data-buf :position position))))
      (with-current-buffer view-buf
        (setq mevedel-view--request-progress-suppressed t))
      (mevedel-view-stream-ensure-progress-for-fsm fsm)
      (with-current-buffer view-buf
        (should-not mevedel-view--request-progress-suppressed)
        (should (mevedel-view--request-progress-visible-p))
        (should (markerp mevedel-view--data-turn-start))
        (should (= (marker-position mevedel-view--data-turn-start)
                   (marker-position position)))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Working" text))))))

  :doc "pre-tool render keeps request progress and adds pending line"
  (mevedel-view-stream-test--with-buffers
    (let ((mevedel-view-spinner-style 'ascii))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (with-current-buffer data-buf
        (mevedel-view-stream-spinner-hook
         '(:name "Read" :args (:file_path "foo.el")))
        (mevedel-view-stream-pre-tool
         '(:name "Read" :args (:file_path "foo.el"))))
      (with-current-buffer view-buf
        (should (mevedel-view--request-progress-visible-p))
        (should (text-property-any
                 (point-min) (point-max)
                 'mevedel-view-spinner-frame t))
        (should (text-property-any
                 (point-min) (point-max)
                 'mevedel-view-inline-spinner-frame t))
        (should (text-property-any
                 (point-min) (point-max)
                 'mevedel-view-pending-tool-live t))
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p "Working" text))
          (should (string-match-p "Calling Read: foo.el" text))
          (should (= 1 (cl-loop with start = 0
                                while (string-match "Calling Read" text start)
                                count t
                                do (setq start (match-end 0)))))))))

  :doc "disabled motion keeps a visible one-second metadata timer and live status"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view-tool-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full)
            (mevedel-view-spinner-animate nil)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read..."))))
        (mevedel-view--start-spinner "Working...")
        (mevedel-view--refresh-pending-tool-lines)
        (let* ((main (text-property-any (point-min) (point-max)
                                        'mevedel-view-spinner-frame t))
               (tool (text-property-any (point-min) (point-max)
                                        'mevedel-view-inline-spinner-frame t))
               (main-frame (get-text-property main 'display))
               (tool-frame (get-text-property tool 'display)))
          (should (timerp mevedel-view--spinner-timer))
          (should (= 1.0 mevedel-view--spinner-timer-period))
          (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                     (lambda () 0.24)))
            (mevedel-view--spinner-tick))
          (should (equal main-frame (get-text-property main 'display)))
          (should (equal tool-frame (get-text-property tool 'display)))
          (mevedel-view--update-spinner "Checking completed work...")
          (should (string-match-p "Checking completed work"
                                  (buffer-string)))
          (should (= 1.0 mevedel-view--spinner-timer-period))))))

  :doc "real pending calls preserve current request status without a window timer"
  (mevedel-view-stream-test--with-buffers
    (let ((mevedel-view-spinner-style 'ascii)
          (mevedel-view-tool-spinner-style 'ascii)
          (mevedel-view-spinner-power-policy 'full)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (should-not (get-buffer-window view-buf t))
        (mevedel-view-stream-begin-turn
         mevedel-view--input-marker
         (with-current-buffer data-buf (copy-marker (point-max))))
        (mevedel-view--update-spinner "Checking completed work...")
        (mevedel-view-stream-test--insert-composer-draft
         "> quoted\nsecond line" 3))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (mevedel-view-stream-pre-tool
               '(:name "Read" :args (:file_path "a")))
              (mevedel-view-stream-pre-tool
               '(:name "Grep" :args (:pattern "x"))))
            (with-current-buffer view-buf
              (should (equal "Checking completed work..."
                             mevedel-view--spinner-status))
              (should (mevedel-view--request-progress-visible-p))
              (should (= 2 (length mevedel-view--spinner-tool-targets)))
              (should-not (timerp mevedel-view--spinner-timer))
              (should-not mevedel-view--spinner-timer-period)
              (let ((text (buffer-substring-no-properties
                           (point-min) (point-max))))
                (should (string-match-p "Checking completed work" text))
                (should (string-match-p "Calling Read: a" text))
                (should (string-match-p "Calling Grep: x" text))
                (should (< (string-match-p "Calling Grep: x" text)
                           (string-match-p "Checking completed work" text)))))
            (mevedel-view-stream-test--with-visible-view
              (mevedel-view--start-spinner-timer)
              (should (timerp mevedel-view--spinner-timer))
              (should (= 0.24 mevedel-view--spinner-timer-period))
              (should (equal "> quoted\nsecond line"
                             (mevedel-view--input-text))))
            (with-current-buffer view-buf
              (mevedel-view--start-spinner-timer)
              (should-not (timerp mevedel-view--spinner-timer))
              (should-not mevedel-view--spinner-timer-period))
            (with-current-buffer data-buf
              (mevedel-view-stream-post-tool
               '(:name "Read" :args (:file_path "a"))))
            (with-current-buffer view-buf
              (should (equal "Checking completed work..."
                             mevedel-view--spinner-status))
              (should (equal "Calling Grep: x..."
                             (cdar mevedel-view--pending-tool-calls)))
              (should (= 1 (length mevedel-view--spinner-tool-targets)))
              (should-not (string-match-p "Calling Read: a"
                                          (buffer-string)))
              (should (equal "> quoted\nsecond line"
                             (mevedel-view--input-text)))
              (should (= 3 (- (point) (mevedel-view--input-start))))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render)))))

  :doc "spinner hook does not duplicate pending tool status in flight"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min)))))
    (with-current-buffer data-buf
      (mevedel-view-stream-spinner-hook
       '(:name "Agent"
         :args (:task_name "explore" :message "Inspect."))))
    (with-current-buffer view-buf
      (should-not (mevedel-view--request-progress-visible-p))
      (should-not (string-match-p "Calling Agent"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))))

  :doc "Agent pre-tool keeps request progress without duplicate pending line"
  (mevedel-view-stream-test--with-buffers
    (let ((mevedel-view-spinner-style 'ascii))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (with-current-buffer data-buf
        (mevedel-view-stream-spinner-hook
         '(:name "Agent"
           :args (:task_name "verify" :message "Verify.")))
        (mevedel-view-stream-pre-tool
         '(:id "agent-1" :name "Agent"
           :args (:task_name "verify" :message "Verify."))))
      (with-current-buffer view-buf
        (should (mevedel-view--request-progress-visible-p))
        (should-not mevedel-view--pending-tool-calls)
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p "Working" text))
          (should-not (string-match-p "Calling Agent" text))))))
)


;;
;;; Final response rendering

(mevedel-deftest mevedel-view--claim-spinner-status
  (:doc "claims only the unchanged request-progress generation")
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((owner (gensym "owner-")))
        (mevedel-view--start-spinner "Working...")
        (let ((snapshot (mevedel-view--spinner-status-snapshot)))
          (should (mevedel-view--claim-spinner-status
                   snapshot "Running hook..." owner))
          (should (equal "Running hook..." mevedel-view--spinner-status))
          (should (eq owner mevedel-view--spinner-owner)))
        (let ((snapshot (mevedel-view--spinner-status-snapshot)))
          (mevedel-view--update-spinner "Calling Read..." 'request)
          (should-not (mevedel-view--claim-spinner-status
                       snapshot "Stale hook..." owner))
          (should (equal "Calling Read..." mevedel-view--spinner-status))
          (should (eq 'request mevedel-view--spinner-owner)))))))

(mevedel-deftest mevedel-view--restore-spinner-status
  (:doc "restores only progress still owned by the caller")
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((owner (gensym "owner-")))
        (mevedel-view--start-spinner "Working...")
        (let ((snapshot (mevedel-view--spinner-status-snapshot)))
          (mevedel-view--claim-spinner-status snapshot "Running hook..." owner)
          (should (mevedel-view--restore-spinner-status owner snapshot))
          (should (equal "Working..." mevedel-view--spinner-status))
          (should (eq 'request mevedel-view--spinner-owner)))
        (let ((snapshot (mevedel-view--spinner-status-snapshot)))
          (mevedel-view--claim-spinner-status snapshot "Running hook..." owner)
          (mevedel-view--update-spinner "Calling Bash..." 'request)
          (should-not (mevedel-view--restore-spinner-status owner snapshot))
          (should (equal "Calling Bash..." mevedel-view--spinner-status))
          (should (eq 'request mevedel-view--spinner-owner)))))))

(mevedel-deftest mevedel-view-stream-render-response ()
  ,test
  (test)
  :doc "renders user + assistant turn"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "Hello world\n" nil)
    (mevedel-view-stream-test--insert-data data-buf "Hi! How can I help?\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "You" text))
        (should (string-match-p "Assistant" text))
        (should (string-match-p "Hello world" text))
        (should (string-match-p "How can I help" text)))))

  :doc "clears stale compaction lock on final response"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "Hello world\n" nil)
    (mevedel-view-stream-test--insert-data data-buf "Hi!\n" 'response)
    (with-current-buffer data-buf
      (setq-local mevedel-compact-run-in-flight t)
      (mevedel-view-stream-render-response (point-min) (point-max))
      (should-not mevedel-compact-run-in-flight)))

  :doc "final response renders durable worked footer and hides side channel"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "worked-footer"
                       :root temporary-file-directory
                       :name "worked-footer"))
           (session (mevedel-session-create "main" workspace))
           (started (time-subtract (current-time) (seconds-to-time 390)))
           data-turn-start response-start response-end)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :session session
                     :started-at started)))
      (mevedel-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (1- (point)) nil))
        (setq response-start (point)))
      (mevedel-view-stream-test--insert-data data-buf "Done.\n" 'response)
      (with-current-buffer data-buf
        (setq response-end (point)))
      (with-current-buffer view-buf
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (mevedel-view--start-spinner "Thinking..."))
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response response-start response-end)
        (should (string-search "request-summary"
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
      (with-current-buffer view-buf
        (should-not (mevedel-view--request-progress-visible-p))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Done" text))
          (should (string-match-p "Worked for 6m" text))
          (should-not (string-match-p "Working\\.\\.\\." text))
          (should-not (string-match-p "mevedel-render-data" text)))
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Worked for 6m" text))
          (should-not (string-match-p "mevedel-render-data" text))))))

  :doc "final response moves stale request summary after later streamed text"
  (mevedel-view-stream-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "worked-footer-tail"
                       :root temporary-file-directory
                       :name "worked-footer-tail"))
           (session (mevedel-session-create "main" workspace))
           (started (time-subtract (current-time) (seconds-to-time 9)))
           response-start)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :session session
                     :started-at started)))
      (mevedel-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq response-start (point)))
      (mevedel-view-stream-test--insert-data
       data-buf
       "```python\nprint('one')\n```\n"
       'response)
      (mevedel-view-stream-test--insert-data
       data-buf
       (mevedel-tool-render-data-format
        '(:kind request-summary :elapsed-seconds 4))
       'ignore)
      (mevedel-view-stream-test--insert-data
       data-buf
       "```javascript\nconsole.log('two');\n```\n"
       'response)
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response response-start (point-max)))
      (with-current-buffer data-buf
        (let* ((data (buffer-substring-no-properties
                      (point-min) (point-max)))
               (summary (string-match-p "request-summary" data))
               (javascript (string-match-p "```javascript" data)))
          (should summary)
          (should javascript)
          (should (> summary javascript))
          (should-not (string-match-p ":elapsed-seconds 4" data))))
      (with-current-buffer view-buf
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (worked (string-match-p "Worked for" text))
               (javascript (string-match-p "javascript ⧉" text)))
          (should javascript)
          (should worked)
          (should (> worked javascript))
          (should-not (string-match-p "mevedel-render-data" text))))))

  :doc "renders tool calls as one-liners"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nfile content\n"
     '(tool . "call_1"))
    (mevedel-view-stream-test--insert-data data-buf "Here is the file.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Read.*test\\.el" text))
        (should-not (string-match-p "file content" text))
        (should (string-match-p "Here is the file" text)))))

  :doc "full rerender preserves tool result rewrite audit details"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     (concat
      "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\n"
      "updated result"
      (mevedel--format-hook-audit-record
       '(:type tool-result-rewrite
               :event "PostToolUse"
               :original-result "original result"
               :updated-result "updated result"
               :reason "redacted"))
      "\n")
     '(tool . "call_1"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Read.*test\\.el" text))
        (should (string-match-p "hook changed tool result" text))
        (should-not (string-match-p "original result" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool result")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Original result" text))
        (should (string-match-p "original result" text))
        (should (string-match-p "Updated result" text))
        (should (string-match-p "updated result" text)))))

  :doc "full rerender preserves ignored tool result audit segments"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nupdated")
        (put-text-property start (point) 'gptel '(tool . "call_1")))
      (let ((start (point)))
        (insert
         (mevedel--format-hook-audit-record
          '(:type tool-result-rewrite
                  :event "PostToolUse"
                  :original-result "original result"
                  :updated-result "updated result"
                  :reason "redacted")))
        (put-text-property start (point) 'gptel 'ignore))
      (let ((start (point)))
        (insert " result\n")
        (put-text-property start (point) 'gptel '(tool . "call_1"))))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Read.*test\\.el" text))
        (should (string-match-p "hook changed tool result" text))
        (should-not (string-match-p "original result" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool result")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Original result" text))
        (should (string-match-p "original result" text))
        (should (string-match-p "Updated result" text))
        (should (string-match-p "updated result" text)))))

  :doc "decorates agent-result blocks inside assistant responses"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     (concat "Review update.\n"
             "<agent-result sender=\"/root/reviewer\" recipient=\"/root\" outcome=\"completed\">\n"
             "{\"findings\":[]}\n"
             "</agent-result>\n"
             "Final answer.\n")
     'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Review update" text))
        (should (string-match-p "✓ Finished /root/reviewer" text))
        (should (string-match-p "{\"findings\":\\[\\]}" text))
        (should (string-match-p "Final answer" text))
        (should-not (string-match-p "<agent-result" text)))
      (goto-char (point-min))
      (search-forward "Review update")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (goto-char (point-min))
      (search-forward "Review update")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "✓ Finished /root/reviewer" text))
        (should (string-match-p "{\"findings\":\\[\\]}" text))
        (should-not (string-match-p "<agent-result" text)))))

  :doc "renders completed Markdown source blocks as view panels"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     "Here is `code`:\n```emacs-lisp\n(message \"hi\")\n```\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer data-buf
      (should (string-match-p
               "```emacs-lisp"
               (buffer-substring-no-properties (point-min) (point-max)))))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Here is `code`" text))
        (should (string-match-p "emacs-lisp ⧉" text))
        (should (string-match-p "(message \"hi\")" text))
        (should-not (string-match-p "```emacs-lisp" text))
        (should-not (string-match-p "#\\+begin_src" text)))))

  :doc "keeps incomplete streaming Markdown source blocks raw"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     "```emacs-lisp\n(message \"hi\")\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "```emacs-lisp" text))
        (should (string-match-p "(message \"hi\")" text)))))

  :doc "assistant prose file line reference is buttonized"
  (let* ((root (make-temp-file "mevedel-view-response-line-" t))
         (file (file-name-concat root "mevedel-session-persistence.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "response-line"
                     :root root :name "response-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (mevedel-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-stream-test--insert-data
             data-buf
             "See mevedel-session-persistence.el:187.\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "mevedel-session-persistence.el:187")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'mevedel-view-path)))
                (should (= 187 (button-get button 'mevedel-view-line)))))))
      (delete-directory root t)))

  :doc "assistant inline code file line reference is buttonized"
  (let* ((root (make-temp-file "mevedel-view-response-inline-line-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "response-inline-line"
                     :root root :name "response-inline-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "inline\n"))
          (mevedel-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-stream-test--insert-data
             data-buf
             "See `file.el:42`.\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "file.el:42")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'mevedel-view-path)))
                (should (= 42 (button-get button 'mevedel-view-line)))))))
      (delete-directory root t)))

  :doc "assistant source block file line reference is not buttonized"
  (let* ((root (make-temp-file "mevedel-view-response-src-line-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "response-src-line"
                     :root root :name "response-src-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "src\n"))
          (mevedel-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-stream-test--insert-data
             data-buf
             "```emacs-lisp\nfile.el:42\n```\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "file.el:42")
              (should-not (button-at (match-beginning 0))))))
      (delete-directory root t)))

  :doc "expanded assistant response preserves file line buttons"
  (let* ((root (make-temp-file "mevedel-view-response-expand-line-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "response-expand-line"
                     :root root :name "response-expand-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "expand\n"))
          (mevedel-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-stream-test--insert-data
             data-buf
             "See file.el:42.\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "file.el:42")
              (goto-char (match-beginning 0))
              (mevedel-view-toggle-section)
              (goto-char (point-min))
              (search-forward "See")
              (goto-char (match-beginning 0))
              (mevedel-view-toggle-section)
              (goto-char (point-min))
              (search-forward "file.el:42")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'mevedel-view-path)))
                (should (= 42 (button-get button 'mevedel-view-line)))))))
      (delete-directory root t)))

  :doc "renders bracket indexing literally inside response code blocks"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     "```r\neval(f[[3]], df)\n```\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "r ⧉" text))
        (should (string-match-p "eval(f\\[\\[3\\]\\], df)" text))
        (should-not (string-match-p "```r" text))
        (should-not (string-match-p "eval(f3, df)" text)))
      (let ((pos (save-excursion
                   (goto-char (point-min))
                   (when (search-forward "[[3]]" mevedel-view--input-marker t)
                     (match-beginning 0)))))
        (should pos)
        (should-not (get-text-property pos 'htmlize-link))
        (should-not (get-text-property pos 'help-echo))
        (should-not (get-text-property pos 'mouse-face)))))

  :doc "does not render spurious user turn for gptel tool scaffolding"
  ;; gptel inserts `#+begin_tool ... ' and `#+end_tool' around the
  ;; propertised tool content with no `gptel' property, so the
  ;; separator text between a user prompt and the tool content shows
  ;; up as a `user' segment.  Must not render as a second "You" turn.
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     "\n\n#+begin_tool (Read :file_path \"/tmp/test.el\")\n"
     nil)
    (mevedel-view-stream-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nfile content\n"
     '(tool . "call_1"))
    (mevedel-view-stream-test--insert-data data-buf "\n#+end_tool\n" nil)
    (mevedel-view-stream-test--insert-data data-buf "Here is the file.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let* ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker))
             (you-count (cl-count-if (lambda (line) (string= line "You"))
                                     (split-string text "\n"))))
        (should (= 0 you-count))
        (should-not (string-match-p "#\\+begin_tool" text))
        (should (string-match-p "Read.*test\\.el" text))
        (should (string-match-p "Here is the file" text)))))

  :doc "permission retry keeps consecutive Bash handles distinct"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     "#+begin_tool (Bash :command \"rg TODO ..\")\n"
     nil)
    (mevedel-view-stream-test--insert-data
     data-buf
     (concat "(:name \"Bash\" :args (:command \"rg TODO ..\"))\n\n"
             "Error: Filesystem authority required\n")
     '(tool . "call-1"))
    (mevedel-view-stream-test--insert-data data-buf "#+end_tool\n" nil)
    (mevedel-view-stream-test--insert-data
     data-buf
     "#+begin_tool (Bash :command \"pwd\")\n"
     nil)
    (mevedel-view-stream-test--insert-data
     data-buf
     "(:name \"Bash\" :args (:command \"pwd\"))\n\nsuccess output\n"
     '(tool . "call-2"))
    (mevedel-view-stream-test--insert-data data-buf "#+end_tool\n" nil)
    (mevedel-view-stream-test--insert-data data-buf "Done.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (= 2 (mevedel-view-stream-test--count-substring
                      "Bash:" text))))
      (goto-char (point-min))
      (search-forward "Bash: pwd")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "success output" text))
        (should-not (string-match-p "Filesystem authority required" text))
        (should-not (string-match-p "#\\+begin_tool\\|#\\+end_tool"
                                    text)))))

  :doc "complete same-id Bash blocks render as separate handles"
  (mevedel-view-stream-test--with-buffers
    (dolist (call '(("test 1 = 2" . "<bash-execution exit_code=\"1\" outcome=\"false\"/>")
                    ("rg missing mevedel.el" . "<bash-execution exit_code=\"1\" outcome=\"no-match\"/>")
                    ("diff /dev/null mevedel.el" . "Error: Filesystem authority required")))
      (mevedel-view-stream-test--insert-data
       data-buf
       (format "#+begin_tool (Bash :command %S)\n" (car call))
       nil)
      (mevedel-view-stream-test--insert-data
       data-buf
       (format "(:name \"Bash\" :args (:command %S))\n\n%s\n"
               (car call) (cdr call))
       '(tool . "duplicate-call-id"))
      (mevedel-view-stream-test--insert-data data-buf "#+end_tool\n" nil))
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (= 3 (mevedel-view-stream-test--count-substring
                      "Bash:" text)))
        (dolist (command '("test 1 = 2"
                           "rg missing mevedel.el"
                           "diff /dev/null mevedel.el"))
          (should (string-match-p (regexp-quote command) text))))))

  :doc "restored stale tool bounds render and expand without garbled fragments"
  (mevedel-view-stream-test--with-buffers
    (mevedel-tool-register
     (mevedel-tool--create
      :name "RecoverRead"
      :category "mevedel"
      :renderer (lambda (_name _args result _data)
                  (list :header "RecoverRead: /tmp/f"
                        :body result
                        :initially-collapsed-p t))))
    (with-current-buffer data-buf
      (let (block-start block-end response-start)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "file body\n"
                "#+end_tool\n")
        (setq block-end (point))
        (put-text-property (+ block-start 20) (- block-end 12)
                           'gptel '(tool . "call_1"))
        (insert "Fixed the byte-compilation warning.\n")
        (save-excursion
          (search-backward "ation warning")
          (setq response-start (point)))
        (put-text-property response-start (point) 'gptel 'response))
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "RecoverRead: /tmp/f" text))
        (should (string-match-p
                 "Fixed the byte-compilation warning" text))
        (should-not (string-match-p "#\\+begin_tool\\|#\\+end_tool\\|n_tool" text))
        (should-not (string-match-p "^You$\\|Thinking" text)))
      (goto-char (point-min))
      (search-forward "RecoverRead: /tmp/f")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "file body" text))
        (should-not (string-match-p
                     "#\\+begin_tool\\|#\\+end_tool\\|n_tool" text)))))

  :doc "renders repeated read calls as individual tool rows"
  (let ((mevedel-view-tool-group-collapse-threshold 0))
    (mevedel-view-stream-test--with-buffers
      (dotimes (i 4)
        (mevedel-view-stream-test--insert-data
         data-buf
         (format "(:name \"Read\" :args (:file_path \"/tmp/file%d.el\"))\n\ncontent %d\n"
                 i i)
         `(tool . ,(format "call_%d" i))))
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should-not (string-match-p "Reading 4 files" text))
          (dolist (file '("file0.el" "file1.el" "file2.el" "file3.el"))
            (should (string-match-p
                     (format "Read: .*%s" (regexp-quote file))
                     text)))))))

  :doc "renders user turn when in-flight marker outlives echoed user block"
  (mevedel-view-stream-test--with-buffers
    (let (start end)
      (mevedel-view-stream-test--insert-data data-buf "First\n" nil)
      (mevedel-view-stream-test--insert-data data-buf "First response.\n" 'response)
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response (point-min) (point-max))
        (setq start (point-max)))
      (mevedel-view-stream-test--insert-data data-buf "Second\n" nil)
      (mevedel-view-stream-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer data-buf
        (setq end (point-max)))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response start end))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Second" text))
          (should (string-match-p "Second response" text))))))

  :doc "does not duplicate visible send-path user echo after flag is consumed"
  (mevedel-view-stream-test--with-buffers
    (let (start end)
      (with-current-buffer data-buf
        (setq start (point-max)))
      (mevedel-view-stream-test--insert-data data-buf "Second\n" nil)
      (mevedel-view-stream-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer data-buf
        (setq end (point-max)))
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Second")
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response start end))
      (with-current-buffer view-buf
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (you-count (cl-count-if (lambda (line) (string= line "You"))
                                       (split-string text "\n"))))
          (should (= 1 you-count))
          (should (string-match-p "Second response" text))))))

  :doc "final response includes reasoning before hook start"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start response-start response-end)
      (mevedel-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point) nil)))
      (mevedel-view-stream-test--insert-data
       data-buf
       "(:name \"Bash\" :args (:command \"true\"))\n\nok\n"
       '(tool . "call_1"))
      (mevedel-view-stream-test--insert-data
       data-buf
       "#+begin_reasoning\nroot cause thought\n#+end_reasoning\n"
       'ignore)
      (with-current-buffer data-buf
        (setq response-start (point)))
      (mevedel-view-stream-test--insert-data data-buf "Final answer.\n" 'response)
      (with-current-buffer data-buf
        (setq response-end (point)))
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Prompt")
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response response-start response-end))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Bash" text))
          (should (string-match-p "Thinking" text))
          (should (string-match-p "Final answer" text))
          (should-not (string-match-p "root cause thought" text)))
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "root cause thought" text))))))

  :doc "final response widening does not duplicate visible user echo"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start response-start response-end)
      (mevedel-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (mevedel-view-stream-test--insert-data
       data-buf
       "#+begin_reasoning\nlate thought\n#+end_reasoning\n"
       'ignore)
      (with-current-buffer data-buf
        (setq response-start (point)))
      (mevedel-view-stream-test--insert-data data-buf "Final answer.\n" 'response)
      (with-current-buffer data-buf
        (setq response-end (point)))
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Prompt")
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response response-start response-end))
      (with-current-buffer view-buf
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (you-count (cl-count-if (lambda (line) (string= line "You"))
                                       (split-string text "\n"))))
          (should (= 1 you-count))
          (should (string-match-p "Thinking" text))
          (should (string-match-p "Final answer" text))))))

  :doc "second-turn incremental render stays above interaction zone without duplication"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-stream-test--insert-data data-buf "First\n" nil)
      (mevedel-view-stream-test--insert-data data-buf "First response.\n" 'response)
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response (point-min) (point-max))
        (goto-char (point-max))
        (insert "\n\nSecond\n")
        (setq data-turn-start (copy-marker (point) nil)))
      (mevedel-view-stream-test--insert-data data-buf "Partial response.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--interaction-register
         (list :kind 'permission
               :id 'permission
               :count 1
               :body "\npermission\n"
               :keymap (make-sparse-keymap)
               :help-echo "Permission"
               :entry 'permission-entry
               :activate #'ignore))
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (mevedel-view--insert-user-message "Second"))
        (mevedel-view--start-spinner "Thinking...")
        (mevedel-view-render-live-update data-buf)
        (mevedel-view-render-live-update data-buf)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (assistant-count
                (cl-count-if (lambda (line) (string= line "Assistant"))
                             (split-string text "\n")))
               (partial-count
                (cl-loop with start = 0
                         while (string-match "Partial response" text start)
                         count t
                         do (setq start (match-end 0))))
               (second-pos (string-match "You\nSecond" text))
               (partial-pos (string-match "Partial response" text)))
          (should (= 2 assistant-count))
          (should (= 1 partial-count))
          (should second-pos)
          (should partial-pos)
          (should (< second-pos partial-pos))))))

  :doc "incremental render suppresses modification hooks"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start
          (changes 0))
      (mevedel-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (mevedel-view-stream-test--insert-data data-buf "Partial response.\n" 'response)
      (with-current-buffer view-buf
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--pending-tool-calls
              (list (cons 'read "Calling Read…")))
        (mevedel-view--insert-pending-tool-lines
         mevedel-view--pending-tool-calls)
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view-render-live-update data-buf)
        (should (= 0 changes))
        (should (string-match-p
                 "Partial response"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))))

  :doc "does not duplicate the original user turn after mailbox insertion"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        ;; Simulate the real send-path marker landing inside the nil
        ;; user-property run, which `--extract-segments' expands
        ;; backward to the beginning of the prompt.
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (mevedel-view-stream-test--insert-data data-buf "Thinking\n" 'ignore)
      (mevedel-view-stream-test--insert-data data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Prompt")
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (insert "✉ from explorer\nhi\n\n")
                (setq mevedel-view--in-flight-turn-start
                      (copy-marker (point) nil))
                (insert "Assistant\nold live tail\n")
                (set-marker mevedel-view--status-marker (point))
                (set-marker mevedel-view--interaction-marker (point))
                (set-marker mevedel-view--input-marker (point)))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--user-pre-rendered nil)
        (mevedel-view-render-live-update data-buf)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (you-count (cl-count-if (lambda (line) (string= line "You"))
                                       (split-string text "\n"))))
          (should (= 1 you-count))
          (should (string-match-p "Assistant text" text))))))

  :doc "renders thinking blocks as summaries"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "line 1\nline 2\nline 3\n" 'ignore)
    (mevedel-view-stream-test--insert-data data-buf "The answer is 42.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Thinking" text))
        (should-not (string-match-p "line 1" text))
        (should (string-match-p "42" text)))))

  :doc "trims contaminated thinking source to structural reasoning block"
  (mevedel-view-stream-test--with-buffers
    (let (reasoning-start)
      (mevedel-view-stream-test--insert-data
       data-buf
       "(:name \"Bash\" :args (:command \"true\"))\n\nok\n"
       '(tool . "call_1"))
      (mevedel-view-stream-test--insert-data
       data-buf
       "**Output observed:**\n  `ok`\n\nVERDICT: PASS\n</agent-result>\n"
       nil)
      (with-current-buffer data-buf
        (setq reasoning-start (point)))
      (mevedel-view-stream-test--insert-data
       data-buf
       "#+begin_reasoning\nreal thought\n#+end_reasoning\n"
       'ignore)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (let ((source (get-text-property (point) 'mevedel-view-source))
              (line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (should (= (marker-position (car source)) reasoning-start))
          (should (string-match-p "Thinking\\.\\.\\. (1 lines)" line)))
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "real thought" text))
          (should-not (string-match-p "VERDICT: PASS" text))
          (should-not (string-match-p "</agent-result>" text))))))

  :doc "hides complete proposed-plan protocol outside Goal planning"
  (mevedel-view-stream-test--with-buffers
    (let ((session (mevedel-session--create
                    :name "test"
                    :workspace nil
                    :permission-mode 'ask)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (mevedel-view-stream-test--insert-data
       data-buf
       "Normal\n<proposed_plan>\n# Plan\n</proposed_plan>\nAfter\n"
       'response)
      (with-current-buffer data-buf
        (mevedel-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should-not (string-match-p "<proposed_plan>" text))
          (should-not (string-match-p "# Plan" text))
          (should (string-match-p "Normal" text))
          (should (string-match-p "After" text))))))

  :doc "keeps historical proposed-plan protocol hidden after Goal planning"
  (let ((tmp (make-temp-file "mevedel-view-plan-" t)))
    (unwind-protect
        (mevedel-view-stream-test--with-buffers
          (let* ((plan-path (file-name-concat tmp "local" "plans" "current.md"))
                 (session (mevedel-session--create
                           :name "test"
                           :workspace nil
                           :save-path tmp
                           :permission-mode 'ask
                           :plan-metadata
                           (list :path "local/plans/current.md"
                                 :status 'accepted))))
            (make-directory (file-name-directory plan-path) t)
            (write-region "# Current plan\n" nil plan-path nil 'silent)
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-stream-test--insert-data
             data-buf
             "Normal\n<proposed_plan>\n# Old plan\n</proposed_plan>\nAfter\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (let ((text (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
                (should-not (string-match-p "<proposed_plan>" text))
                (should-not (string-match-p "# Old plan" text))
                (should (string-match-p "Normal" text))
                (should (string-match-p "After" text))))))
      ;; Killing the view buffers records telemetry under the session's
      ;; save path, so the directory goes only after them.
      (delete-directory tmp t)))

  :doc "renders ignored directive PROMPT drawer as collapsed user section"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "Change alpha :implement:\n" nil)
    (mevedel-view-stream-test--insert-data
     data-buf
     ":PROMPT:\n## TASK\nFull hidden prompt.\n:END:\n"
     'ignore)
    (mevedel-view-stream-test--insert-data data-buf "Done.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Implement: Change alpha" text))
        (should-not (string-match-p "Change alpha :implement:" text))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "Full hidden prompt" text))
        (should (string-match-p "Done" text)))
      (goto-char (point-min))
      (search-forward "Implement:")
      (should (eq 'mevedel-view-directive-action
                  (get-text-property (match-beginning 0)
                                     'font-lock-face)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (mevedel-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Full hidden prompt" expanded)))))

  :doc "renders inline skills as compact invocation with collapsed prompt"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     "You are helping with this user request:\n\nSay hi!\n"
     nil)
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert
         (mevedel-tool-render-data-format
          '(:kind inline-skill
                  :name "emacs-context-snapshot"
                  :arguments "Say hi!"
                  :display-text
                  "$emacs-context-snapshot\nSay hi!"
                  :prompt
                  "You are helping with this user request:\n\nSay hi!\n")))
        (put-text-property start (point) 'gptel 'ignore)))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p
                 "\\$emacs-context-snapshot\nSay hi!"
                 text))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "You are helping with this user request"
                                    text)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (mevedel-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
        (should (string-match-p "You are helping with this user request"
                                expanded)))))

  :doc "renders nil-property inline skill metadata as compact invocation"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     (concat
      "# Green Loop\n\nRun the loop.\n\nARGUMENTS: current changes"
      (mevedel-tool-render-data-format
       '(:kind inline-skill
               :name "green-loop"
               :arguments "current changes"
               :display-text "$green-loop current changes"
               :prompt
               "# Green Loop\n\nRun the loop.\n\nARGUMENTS: current changes")))
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "\\$green-loop current changes" text))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "# Green Loop" text))
        (should-not (string-match-p "Run the loop" text))
        (should-not (string-match-p "mevedel-render-data" text)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (mevedel-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
        (should (string-match-p "# Green Loop" expanded))
        (should (string-match-p "Run the loop" expanded)))))

  :doc "request summary before inline skill does not absorb user turn"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data data-buf "Previous answer.\n" 'response)
    (mevedel-view-stream-test--insert-data
     data-buf
     (mevedel-tool-render-data-format
      '(:kind request-summary :elapsed-seconds 120))
     'ignore)
    (mevedel-view-stream-test--insert-data
     data-buf
     "# Green Loop\n\nRun the loop.\n\nARGUMENTS: current changes"
     nil)
    (mevedel-view-stream-test--insert-data
     data-buf
     (mevedel-tool-render-data-format
      '(:kind inline-skill
              :name "green-loop"
              :arguments "current changes"
              :display-text "$green-loop current changes"))
     'ignore)
    (mevedel-view-stream-test--insert-data data-buf "Restarting checks.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Previous answer" text))
        (should (string-match-p "Worked for 2m" text))
        (should (string-match-p "You\n\\$green-loop current changes" text))
        (should (string-match-p "Restarting checks" text))
        (should-not (string-match-p "# Green Loop" text))
        (should-not (string-match-p "mevedel-render-data" text)))))

  :doc "expanded inline skill prompt omits saved org property drawer"
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--insert-data
     data-buf
     ":PROPERTIES:\n:GPTEL_PRESET: mevedel-implement\n:GPTEL_MODEL: gpt-5.5\n:GPTEL_BOUNDS: ((ignore (1 2)))\n:END:\n\nSkill prompt body\n\nVisible model prompt.\n"
     nil)
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert
         (mevedel-tool-render-data-format
          '(:kind inline-skill
                  :name "green-loop"
                  :arguments "commits a b"
                  :display-text
                  "$green-loop\ncommits a b"
                  :prompt
                  "Skill prompt body\n\nVisible model prompt.\n")))
        (put-text-property start (point) 'gptel 'ignore)))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "Prompt")
      (mevedel-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Visible model prompt" expanded))
        (should-not (string-match-p ":PROPERTIES:" expanded))
        (should-not (string-match-p "GPTEL_BOUNDS" expanded)))))

  :doc "expanded external Prompt survives in-flight incremental render"
  (mevedel-view-stream-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-stream-test--insert-data data-buf "Change alpha :implement:\n" nil)
      (mevedel-view-stream-test--insert-data
       data-buf
       ":PROMPT:\n## TASK\nFull hidden prompt.\n:END:\n"
       'ignore)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point) nil)))
      (with-current-buffer view-buf
        (mevedel-view--begin-external-turn
         "Implement: Change alpha" data-turn-start 'directive)
        (goto-char (point-min))
        (search-forward "Prompt")
        (mevedel-view-toggle-section)
        (should (string-match-p
                 "Full hidden prompt"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))
      (mevedel-view-stream-test--insert-data data-buf "Assistant answer.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view-render-live-update data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Full hidden prompt" text))
          (should (string-match-p "Assistant answer" text))))))

  :doc "target-native response link opens only through its session target"
  (let* ((host "view-link-target")
         (other-host "view-link-other")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-view-link-target-" t)))
         (remote-root (format "/mevedelmock:%s:%s" host local-root))
         (native-file (file-name-concat local-root "src" "main.el"))
         (remote-file (concat (file-remote-p remote-root)
                              native-file))
         (client-file (make-temp-file
                       "mevedel-view-link-client-" nil ".el" "client\n"))
         (client-path (concat "/:" client-file))
         (other-path (format "/mevedelmock:%s:%s" other-host client-file))
         opened)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host other-host)
          (make-directory (file-name-directory remote-file) t)
          (write-region "target\n" nil remote-file nil 'silent)
          (let* ((workspace
                  (mevedel-workspace--create
                   :type 'project :id "view-link-target"
                   :root remote-root :name "view-link-target"))
                 (session (mevedel-session-create "main" workspace)))
            (mevedel-view-stream-test--with-buffers
              (dolist (buffer (list data-buf view-buf))
                (with-current-buffer buffer
                  (setq-local mevedel--session session)))
              (mevedel-view-stream-test--insert-data
               data-buf "Show paths\n" nil)
              (mevedel-view-stream-test--insert-data
               data-buf
               (format "Open %s\nIgnore %s\nIgnore %s\n"
                       native-file other-path client-path)
               'response)
              (with-current-buffer data-buf
                (mevedel-view-stream-render-response
                 (point-min) (point-max)))
              ;; Decoration runs from a redraw timer and so never stats the
              ;; target; a remote path is linked only once the verification
              ;; pass has recorded it.  Drive that pass and redraw here
              ;; rather than waiting on its idle timer.
              (with-current-buffer view-buf
                (cl-letf (((symbol-function 'mevedel-transport-run-when-idle)
                           (lambda (_key _path thunk &optional _on-cancel)
                             (funcall thunk)
                             t)))
                  (mevedel-view--verify-paths view-buf)))
              (with-current-buffer data-buf
                (mevedel-view-stream-render-response
                 (point-min) (point-max)))
              (with-current-buffer view-buf
                (goto-char (point-min))
                (search-forward native-file)
                (let ((button (button-at (match-beginning 0))))
                  (should button)
                  (should (equal remote-file
                                 (button-get button 'mevedel-view-path)))
                  (cl-letf (((symbol-function 'find-file-other-window)
                             (lambda (path)
                               (setq opened (find-file-noselect path)))))
                    (button-activate button)))
                (goto-char (point-min))
                (search-forward other-path)
                (should-not (button-at (1- (point))))
                (search-forward client-path)
                (should-not (button-at (1- (point)))))))
          (should (buffer-live-p opened))
          (with-current-buffer opened
            (should (equal remote-file buffer-file-name))
            (should (equal "target\n" (buffer-string))))
          ;; The buffer visits a target file, so it has to be killed while
          ;; the mock method still resolves.
          (with-current-buffer opened
            (set-buffer-modified-p nil))
          (kill-buffer opened))
      (when (buffer-live-p opened)
        (with-current-buffer opened
          (set-buffer-modified-p nil))
        (kill-buffer opened))
      (when (file-exists-p client-file)
        (delete-file client-file))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "releases the turn, warns, and reschedules when projection fails"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (insert "ask\n\nreply\n"))
    (mevedel-view-stream-test--with-visible-view
      (mevedel-view-stream-begin-turn
       (point-min)
       (with-current-buffer data-buf (copy-marker (point-min) nil)))
      (setq mevedel-view--pending-tool-calls
            (list (list :id "call-1" :name "Read")))
      (should (timerp mevedel-view--spinner-timer))
      (let (warning later-ran)
        (mevedel-test--with-captured-diagnostics warning
          (cl-letf (((symbol-function 'mevedel-view--render-live-region)
                     (lambda (&rest _) (error "Projection failed"))))
            (with-current-buffer data-buf
              ;; Through the hook, because the point of not signalling is
              ;; that the observers after this one still run.
              (let ((gptel-post-response-functions
                     (list #'mevedel-view-stream-render-response
                           (lambda (&rest _) (setq later-ran t)))))
                (run-hook-with-args 'gptel-post-response-functions
                                    (point-min) (point-max))))))
        (should (string-match-p "Projection failed" warning))
        (should later-ran))
      (should-not mevedel-view--pending-tool-calls)
      (should-not (timerp mevedel-view--spinner-timer))
      (should-not mevedel-view--in-flight-turn-start)
      (should-not mevedel-view--data-turn-start)
      (should (eq 'full mevedel-view--pending-render-kind))))

  :doc "releases the turn when stopping the progress row fails"
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (insert "ask\n\nreply\n"))
    (mevedel-view-stream-test--with-visible-view
      (mevedel-view-stream-begin-turn
       (point-min)
       (with-current-buffer data-buf (copy-marker (point-min) nil)))
      (setq mevedel-view--pending-tool-calls
            (list (list :id "call-1" :name "Read")))
      (should (timerp mevedel-view--spinner-timer))
      (let (warning)
        (mevedel-test--with-captured-diagnostics warning
          (cl-letf (((symbol-function 'mevedel-view--stop-request-progress)
                     (lambda (&rest _) (error "Zone reconcile failed"))))
            (with-current-buffer data-buf
              (mevedel-view-stream-render-response
               (point-min) (point-max)))))
        (should (string-match-p "Zone reconcile failed" warning)))
      (should-not mevedel-view--pending-tool-calls)
      (should-not (timerp mevedel-view--spinner-timer))
      (should-not mevedel-view--in-flight-turn-start)
      (should-not mevedel-view--data-turn-start))))

(mevedel-deftest mevedel-view-animation-frozen-metadata
  (:doc "Elapsed metadata redraws never move disabled or zero-fps glyphs.")
  (dolist (setting '(disabled zero-fps))
    (mevedel-view-stream-test--with-buffers
      (mevedel-view-stream-test--with-visible-view
        (let ((mevedel-view-spinner-style 'ascii)
              (mevedel-view-tool-spinner-style 'ascii)
              (mevedel-view-spinner-animate (eq setting 'zero-fps))
              (mevedel-view-spinner-power-policy
               (if (eq setting 'zero-fps) 'save 'full))
              (mevedel-view-spinner-battery-framerate 0)
              (seconds 0))
          (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                     (lambda () seconds)))
            (mevedel-view--start-spinner "Working...")
            (let* ((target mevedel-view--spinner-label-target)
                   (initial (get-text-property
                             (marker-position (car target)) 'display)))
              (should (= mevedel-view--spinner-timer-period 1.0))
              (dolist (elapsed '(3 6))
                (setq seconds elapsed
                      mevedel-view--spinner-start-time
                      (time-subtract (current-time) (seconds-to-time elapsed))
                      mevedel-view--spinner-last-second nil)
                (mevedel-view--spinner-tick)
                (should (equal initial
                               (get-text-property
                                (marker-position
                                 (car mevedel-view--spinner-label-target))
                                'display)))
                (should (string-match-p
                         (format " · %ds" elapsed) (buffer-string)))))))))))

(mevedel-deftest mevedel-view-animation-freeze-resume-phase
  (:doc "Zero fps holds the last displayed sample, not the next clock phase.")
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full)
            (mevedel-view-spinner-battery-framerate 0)
            (seconds 0.119))
        (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                   (lambda () seconds)))
          (mevedel-view--start-spinner "Working...")
          (let ((phase mevedel-view--spinner-phase-start)
                (frame (get-text-property
                        (marker-position (car mevedel-view--spinner-label-target))
                        'display)))
            (setq seconds 0.241)
            ;; A focus/scroll rearm at the new clock phase does not render a
            ;; new sample and must not overwrite the last displayed one.
            (mevedel-view--start-spinner-timer)
            (should (equal frame
                           (get-text-property
                            (marker-position
                             (car mevedel-view--spinner-label-target))
                            'display)))
            (setq mevedel-view-spinner-power-policy 'save)
            (mevedel-view--refresh-animation-options)
            (should (equal frame
                           (get-text-property
                            (marker-position
                             (car mevedel-view--spinner-label-target))
                            'display)))
            (setq seconds 3.48
                  mevedel-view--spinner-start-time
                  (time-subtract (current-time) (seconds-to-time 3))
                  mevedel-view--spinner-last-second nil)
            (mevedel-view--spinner-tick)
            (should (equal frame
                           (get-text-property
                            (marker-position
                             (car mevedel-view--spinner-label-target))
                            'display)))
            (setq mevedel-view-spinner-power-policy 'full)
            (mevedel-view--refresh-animation-options)
            (should (equal (mevedel-view-animation-frame
                            'ascii "Working..." seconds 'mevedel-view-spinner
                            (selected-frame))
                           (get-text-property
                            (marker-position
                             (car mevedel-view--spinner-label-target))
                            'display)))
            ;; A real rendered frame supersedes the old frozen sample before
            ;; the next transition, without a metadata/second redraw.
            (mevedel-view--spinner-tick)
            (let ((resumed (get-text-property
                            (marker-position
                             (car mevedel-view--spinner-label-target))
                            'display)))
              (setq mevedel-view-spinner-power-policy 'save
                    seconds 3.601)
              (mevedel-view--start-spinner-timer)
              (setq mevedel-view--spinner-last-second nil)
              (mevedel-view--spinner-tick)
              (should (equal resumed (get-text-property
                                      (marker-position
                                       (car mevedel-view--spinner-label-target))
                                      'display))))
            (should (= phase mevedel-view--spinner-phase-start))))))))

(mevedel-deftest mevedel-view-animation-freeze-tool-row
  (:doc "Power changes keep the last displayed tool glyph, not its initial frame.")
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view-tool-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full)
            (mevedel-view-spinner-battery-framerate 0)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read...")))
            (seconds 0.72))
        (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                   (lambda () seconds)))
          (mevedel-view--start-spinner "Working...")
          (mevedel-view--refresh-pending-tool-lines)
          (mevedel-view--spinner-tick)
          (let* ((target (car mevedel-view--spinner-tool-targets))
                 (display (get-text-property
                           (marker-position (car target)) 'display)))
            (should (equal display "/ "))
            (setq seconds 0.961
                  mevedel-view-spinner-power-policy 'save)
            (mevedel-view--refresh-animation-options)
            (should (equal display (get-text-property
                                    (marker-position (car target)) 'display)))
            (setq seconds 3.6
                  mevedel-view--spinner-start-time
                  (time-subtract (current-time) (seconds-to-time 3))
                  mevedel-view--spinner-last-second nil)
            (mevedel-view--spinner-tick)
            (should (equal display (get-text-property
                                    (marker-position (car target)) 'display)))
            (setq mevedel-view--pending-tool-calls
                  '(("call-1" . "Calling Read...")
                    ("call-2" . "Calling Grep...")))
            (mevedel-view--refresh-pending-tool-lines)
            (should (equal display
                           (get-text-property
                            (marker-position
                             (caar mevedel-view--spinner-tool-targets))
                            'display)))
            (let ((second-display
                   (get-text-property
                    (marker-position
                     (car (cadr mevedel-view--spinner-tool-targets)))
                    'display)))
              (setq mevedel-view--pending-tool-calls
                    '(("call-2" . "Calling Grep...")))
              (mevedel-view--refresh-pending-tool-lines)
              (should (equal second-display
                             (get-text-property
                              (marker-position
                               (caar mevedel-view--spinner-tool-targets))
                              'display))))
            (setq mevedel-view-tool-spinner-style 'dots)
            (mevedel-view--refresh-animation-options)
            (should (equal "*... "
                           (get-text-property
                            (marker-position
                             (caar mevedel-view--spinner-tool-targets))
                            'display)))))))))

(mevedel-deftest mevedel-view-animation-freeze-independent-tool-phase
  (:doc "A paused tool keeps its own displayed phase on rearm and glyph fallback.")
  (dolist (main '(none static))
    (dolist (style '(braille dots))
      (dolist (freeze '(global battery))
        (let ((seconds 0.65)
              (supported t)
              (mevedel-view-animation--dots-cache nil))
          (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                     (lambda () seconds))
                    ((symbol-function 'char-displayable-p)
                     (lambda (char &optional _frame)
                       (or supported (not (memq char '(?⠋ ?● ?·)))))))
            (mevedel-view-stream-test--with-buffers
              (mevedel-view-stream-test--with-visible-view
                (let ((mevedel-view-spinner-style 'static)
                      (mevedel-view-tool-spinner-style style)
                      (mevedel-view-spinner-power-policy 'full)
                      (mevedel-view-spinner-battery-framerate 0)
                      (mevedel-view-spinner-animate t)
                      (mevedel-view--pending-tool-calls
                       '(("call-1" . "Calling Read..."))))
                  (when (eq main 'static)
                    (mevedel-view--start-spinner "Working..."))
                  (mevedel-view--refresh-pending-tool-lines)
                  (mevedel-view--spinner-tick)
                  (let* ((target (car mevedel-view--spinner-tool-targets))
                         (position (marker-position (car target)))
                         (display (get-text-property position 'display)))
                    (should (equal-including-properties
                             display
                             (mevedel-view-animation-frame
                              style "" 0.65 'mevedel-view-ephemeral
                              (mevedel-view--animation-target-frame
                               target (eq style 'dots)))))
                    (setq seconds 1.1)
                    (if (eq freeze 'global)
                        (setq mevedel-view-spinner-animate nil)
                      (setq mevedel-view-spinner-power-policy 'save))
                    (mevedel-view--start-spinner-timer)
                    (when (eq main 'none)
                      (should-not mevedel-view--spinner-timer))
                    (mevedel-view--resume-on-window-change (selected-window))
                    (should (equal-including-properties
                             display (get-text-property position 'display)))
                    (setq supported nil)
                    (run-hook-with-args 'enable-theme-functions
                                        'mevedel-test-theme)
                    (should (equal-including-properties
                             (mevedel-view-animation-frame
                              style "" 0.65 'mevedel-view-ephemeral
                              (mevedel-view--animation-target-frame
                               target (eq style 'dots)))
                             (get-text-property position 'display)))
                    (setq supported t)
                    (mevedel-view--resume-on-window-change (selected-window))
                    (should (equal-including-properties
                             display (get-text-property position 'display)))
                    (setq mevedel-view--pending-tool-calls
                          '(("call-1" . "Calling Read...")
                            ("call-2" . "Calling Grep...")))
                    (mevedel-view--refresh-pending-tool-lines)
                    (let* ((first (car mevedel-view--spinner-tool-targets))
                           (second (cadr mevedel-view--spinner-tool-targets)))
                      (should (equal-including-properties
                               display (get-text-property
                                        (marker-position (car first)) 'display)))
                      (mevedel-view--resume-on-window-change
                       (selected-window))
                      (should (equal-including-properties
                               display (get-text-property
                                        (marker-position (car first)) 'display)))
                      (should (equal-including-properties
                               (mevedel-view-animation-frame
                                style "" 0 'mevedel-view-ephemeral
                                (mevedel-view--animation-target-frame
                                 second (eq style 'dots)))
                               (get-text-property
                                (marker-position (car second)) 'display))))
                    (when (eq main 'none)
                      (should-not mevedel-view--spinner-timer))))))))))))

(mevedel-deftest mevedel-view-animation-incremental-tool-phase
  (:doc "An incremental transcript projection retains a frozen tool sample.")
  (dolist (style '(ascii braille dots))
    (mevedel-view-stream-test--with-buffers
      (mevedel-view-stream-test--with-visible-view
        (let ((seconds 0.72)
              (mevedel-view-spinner-style 'static)
              (mevedel-view-tool-spinner-style style)
              (mevedel-view-spinner-power-policy 'full)
              (mevedel-view-spinner-battery-framerate 0)
              (mevedel-view--pending-tool-calls
               '(("call-1" . "Calling Read..."))))
          (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                     (lambda () seconds)))
            (setq mevedel-view--in-flight-turn-start
                  (copy-marker mevedel-view--input-marker)
                  mevedel-view--data-turn-start
                  (with-current-buffer data-buf (copy-marker (point-max))))
            (mevedel-view--start-spinner "Working...")
            (mevedel-view--refresh-pending-tool-lines)
            (mevedel-view--spinner-tick)
            (let ((display
                   (get-text-property
                    (marker-position (caar mevedel-view--spinner-tool-targets))
                    'display)))
              (setq seconds 0.961
                    mevedel-view-spinner-power-policy 'save)
              (mevedel-view--start-spinner-timer)
              (mevedel-view--render-live-region data-buf nil)
              (let ((position
                     (marker-position
                      (caar mevedel-view--spinner-tool-targets))))
                (should (equal-including-properties
                         display (get-text-property position 'display)))
                (should (= 0.72 (mevedel-view--tool-sample-seconds position)))
                (mevedel-view--resume-on-window-change (selected-window))
                (should (equal-including-properties
                         display (get-text-property position 'display)))))))))))

(mevedel-deftest mevedel-view-animation-full-rerender-tool-phase
  (:doc "A full projection retains each surviving frozen tool's displayed phase.")
  (dolist (style '(ascii braille dots))
    (dolist (freeze '(global battery))
      (mevedel-view-stream-test--with-buffers
        (mevedel-view-stream-test--with-visible-view
          (let ((seconds 0.72)
                (mevedel-view-spinner-style 'static)
                (mevedel-view-tool-spinner-style style)
                (mevedel-view-spinner-power-policy 'full)
                (mevedel-view-spinner-battery-framerate 0)
                (mevedel-view-spinner-animate t)
                (mevedel-view--pending-tool-calls
                 '(("call-1" . "Calling Read..."))))
            (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                       (lambda () seconds)))
              (setq mevedel-view--in-flight-turn-start
                    (copy-marker mevedel-view--input-marker)
                    mevedel-view--data-turn-start
                    (with-current-buffer data-buf (copy-marker (point-max))))
              (mevedel-view--start-spinner "Working...")
              (mevedel-view--refresh-pending-tool-lines)
              (mevedel-view--spinner-tick)
              (let ((display
                     (get-text-property
                      (marker-position (caar mevedel-view--spinner-tool-targets))
                      'display)))
                (setq seconds 0.961)
                (if (eq freeze 'global)
                    (setq mevedel-view-spinner-animate nil)
                  (setq mevedel-view-spinner-power-policy 'save))
                (mevedel-view--start-spinner-timer)
                (mevedel-view--full-rerender)
                (let ((position
                       (marker-position
                        (caar mevedel-view--spinner-tool-targets))))
                  (should (equal-including-properties
                           display (get-text-property position 'display)))
                  (should (= 0.72 (mevedel-view--tool-sample-seconds position)))
                  (mevedel-view--resume-on-window-change (selected-window))
                  (should (equal-including-properties
                           display (get-text-property position 'display))))))))))))

(mevedel-deftest mevedel-view-animation-resume-lifecycle
  (:doc "Scroll/focus only rearm frames; mode change releases power and timers.")
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'auto)
            (reconciles 0))
        (mevedel-view--start-spinner "Working...")
        (should (timerp mevedel-view--spinner-timer))
        (should (gethash view-buf mevedel-view-power--watchers))
        (let ((timer mevedel-view--spinner-timer))
          (cl-letf (((symbol-function 'mevedel-view--render-request-progress)
                     (lambda (&rest _) (cl-incf reconciles)))
                    ((symbol-function 'mevedel-view--refresh-pending-tool-lines)
                     (lambda (&rest _) (cl-incf reconciles))))
            (mevedel-view--resume-on-window-layout (selected-window) (point-min))
            (mevedel-view--resume-render-if-attended view-buf)
            (should (= 0 reconciles)))
          (fundamental-mode)
          (should-not (mevedel--timer-pending-p timer))
          (should-not (gethash view-buf mevedel-view-power--watchers)))))))

(mevedel-deftest mevedel-view--resume-on-window-layout
  (:doc "Scrolling or resizing defers the decision until the new range is drawn.")
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full))
        (unwind-protect
            (progn
              (mevedel-view--start-spinner "Working...")
              (let ((timer mevedel-view--spinner-timer))
                ;; Inside the hook the old range would read as hidden.
                (cl-letf (((symbol-function 'mevedel-view--animation-visible-p)
                           #'ignore))
                  (mevedel-view--resume-on-window-layout (selected-window)
                                                         (point-min)))
                (should (mevedel--ui-timer-pending-p timer))
                (should (mevedel--ui-timer-pending-p
                         mevedel-view--spinner-probe-timer))
                (should (memq #'mevedel-view--resume-on-window-layout
                              window-size-change-functions))))
          (mevedel-view--stop-spinner))))))

(mevedel-deftest mevedel-view-animation-window-departure
  (:doc "Hiding the last frozen tool view releases power monitoring immediately.")
  (let ((queries 0)
        (battery-status-function (lambda () (cl-incf queries) nil))
        (mevedel-view-power--watchers (make-hash-table :test #'eq))
        (mevedel-view-power--timer nil)
        (mevedel-view-power--state 'unknown)
        (mevedel-view-power--sample-time nil)
        (mevedel-view-power--last-query nil)
        (battery-update-functions (copy-sequence battery-update-functions))
        (window-state-change-functions
         (copy-sequence window-state-change-functions))
        (mevedel-view-spinner-power-policy 'auto)
        (mevedel-view-spinner-battery-framerate 0))
    (unwind-protect
        (mevedel-view-stream-test--with-buffers
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer view-buf)
            (redisplay t)
            (with-current-buffer view-buf
              (setq mevedel-view--pending-tool-calls
                    '(("call-1" . "Calling Read...")))
              (mevedel-view--refresh-pending-tool-lines)
              (should (gethash view-buf mevedel-view-power--watchers))
              (should (timerp mevedel-view-power--timer))
              (should-not mevedel-view--spinner-timer))
            ;; The first departure must not unsubscribe while a second window
            ;; still displays the target.
            (let ((other (split-window-right))
                  (last-power mevedel-view-power--timer))
              (set-window-buffer other view-buf)
              (redisplay t)
              (switch-to-buffer data-buf)
              (redisplay t)
              ;; Batch Emacs has no live redisplay loop; deliver the departing
              ;; buffer's local callback with that buffer current.
              (with-current-buffer view-buf
                (should (memq #'mevedel-view--resume-on-window-change
                              window-buffer-change-functions))
                (run-hook-with-args 'window-buffer-change-functions
                                    (selected-window)))
              (should (gethash view-buf mevedel-view-power--watchers))
              (set-window-buffer other data-buf)
              (redisplay t)
              (with-current-buffer view-buf
                (run-hook-with-args 'window-buffer-change-functions other))
              (should-not (gethash view-buf mevedel-view-power--watchers))
              (should-not mevedel-view-power--timer)
              (should-not (memq #'mevedel-view-power--sample
                                battery-update-functions))
              (mevedel-view-power--poll last-power)
              (mevedel-view-power--poll last-power)
              (should (zerop queries))
              (should-not mevedel-view-power--timer)
              ;; Reopening the same view restores its observer without
              ;; restarting or changing the pending tool call.
              (set-window-buffer other view-buf)
              (redisplay t)
              (with-current-buffer view-buf
                (run-hook-with-args 'window-buffer-change-functions other))
              (should (gethash view-buf mevedel-view-power--watchers))
              (should (equal (with-current-buffer view-buf
                               mevedel-view--pending-tool-calls)
                             '(("call-1" . "Calling Read..."))))
              ;; Deleting the last view window has no departing buffer-local
              ;; callback.  A shared window-state observer must do the cleanup.
              (delete-window other)
              (run-hook-with-args 'window-state-change-functions
                                  (selected-frame))
              (should-not (gethash view-buf mevedel-view-power--watchers))
              (should-not mevedel-view-power--timer)
              (should-not (memq #'mevedel-view-power--sample
                                battery-update-functions))
              (should-not (memq #'mevedel-view-power--on-window-state-change
                                window-state-change-functions)))))
      (when mevedel-view-power--timer
        (cancel-timer mevedel-view-power--timer)))))

(mevedel-deftest mevedel-view-animation-power-cadence
  (:doc "The actual view timer changes cadence without restarting the phase.")
  (let ((mevedel-view-animation--cache nil))
    (cl-letf (((symbol-function 'mevedel-view-animation--colors)
               (lambda (_face _frame) '("#ff0000" . "#ffffff"))))
      (mevedel-view-stream-test--with-buffers
        (mevedel-view-stream-test--with-visible-view
          (let ((mevedel-view-spinner-style 'shimmer)
                (mevedel-view-spinner-framerate 60)
                (mevedel-view-spinner-battery-framerate 30)
                (mevedel-view-spinner-power-policy 'full))
            (mevedel-view--start-spinner "Working...")
            (should (< (abs (- mevedel-view--spinner-timer-period (/ 1.0 60)))
                       1e-7))
            (let ((phase mevedel-view--spinner-phase-start)
                  (full-timer mevedel-view--spinner-timer))
              (setq mevedel-view-spinner-power-policy 'save)
              (mevedel-view--start-spinner-timer)
              (should (< (abs (- mevedel-view--spinner-timer-period (/ 1.0 30)))
                         1e-7))
              (should-not (mevedel--timer-pending-p full-timer))
              (should (= phase mevedel-view--spinner-phase-start))
              (setq mevedel-view-spinner-battery-framerate 0)
              (mevedel-view--start-spinner-timer)
              (should (= 1.0 mevedel-view--spinner-timer-period))
              (should (= phase mevedel-view--spinner-phase-start))
              (setq mevedel-view-spinner-style 'static)
              (mevedel-view--start-spinner-timer)
              (should (= 1.0 mevedel-view--spinner-timer-period)))))))))

(mevedel-deftest mevedel-view-animation-customize-live
  (:doc "Customize replaces the active timer and display without resetting a turn.")
  (let ((mevedel-view-animation--cache nil)
        (original-style (default-value 'mevedel-view-spinner-style))
        (original-policy (default-value 'mevedel-view-spinner-power-policy))
        (original-rate (default-value 'mevedel-view-spinner-framerate)))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-view-animation--colors)
                   (lambda (_face _frame) '("#ff0000" . "#ffffff"))))
          (customize-set-variable 'mevedel-view-spinner-power-policy 'full)
          (customize-set-variable 'mevedel-view-spinner-style 'ascii)
          (customize-set-variable 'mevedel-view-spinner-framerate 60)
          (mevedel-view-stream-test--with-buffers
            (mevedel-view-stream-test--with-visible-view
              (mevedel-view--start-spinner "Working...")
              (should (= 0.24 mevedel-view--spinner-timer-period))
              (let ((phase mevedel-view--spinner-phase-start)
                    (old-timer mevedel-view--spinner-timer))
                (customize-set-variable 'mevedel-view-spinner-style 'shimmer)
                (should (< (abs (- mevedel-view--spinner-timer-period
                                   (/ 1.0 60))) 1e-7))
                (should-not (mevedel--timer-pending-p old-timer))
                (should (= phase mevedel-view--spinner-phase-start))
                (let* ((target mevedel-view--spinner-label-target)
                       (sample (get-text-property
                                (marker-position (car target)) 'display)))
                  (should (get-text-property 0 'face sample)))
                (customize-set-variable 'mevedel-view-spinner-framerate 30)
                (should (< (abs (- mevedel-view--spinner-timer-period
                                   (/ 1.0 30))) 1e-7))
                (should (= phase mevedel-view--spinner-phase-start))
                (customize-set-variable 'mevedel-view-spinner-style 'static)
                (should (= 1.0 mevedel-view--spinner-timer-period))
                (should (= phase mevedel-view--spinner-phase-start))))))
      (set-default 'mevedel-view-spinner-style original-style)
      (set-default 'mevedel-view-spinner-power-policy original-policy)
      (set-default 'mevedel-view-spinner-framerate original-rate))))

(mevedel-deftest mevedel-view-animation-timer-suspension
  (:doc "Suspended timers rearm from their old phase without replaying missed frames.")
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full)
            (now 1000.0))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional _) now)))
          (mevedel-view--start-spinner "Working...")
          (should (= 0.24 mevedel-view--spinner-timer-period))
          (let ((old-timer mevedel-view--spinner-timer)
                (phase mevedel-view--spinner-phase-start))
            ;; TRAMP can remove timers from timer-list while their buffer-local
            ;; references still point to timer objects.  A focus event rearms.
            (cancel-timer old-timer)
            (setq now 1234.37)
            (mevedel-view--resume-render-if-attended view-buf)
            (should (timerp mevedel-view--spinner-timer))
            (should-not (eq old-timer mevedel-view--spinner-timer))
            (should (= phase mevedel-view--spinner-phase-start))
            (mevedel-view--spinner-tick)
            (let* ((target mevedel-view--spinner-label-target)
                   (position (marker-position (car target))))
              (should (equal (get-text-property position 'display)
                             (mevedel-view-animation-frame
                              'ascii "Working..." (- now phase)
                              'mevedel-view-spinner (selected-frame)))))))))))

(mevedel-deftest mevedel-view-animation-tramp-timer-ownership
  (:doc "TRAMP suspension preserves one view timer and cancels it on stop.")
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full))
        (mevedel-view--start-spinner "Working...")
        (let ((original mevedel-view--spinner-timer)
              (phase mevedel-view--spinner-phase-start))
          (should (memq original timer-list))
          (with-tramp-suspended-timers
            (dotimes (_ 5) (mevedel-view--start-spinner-timer))
            (should (eq original mevedel-view--spinner-timer))
            (should (mevedel--ui-timer-pending-p original))
            (should-not (memq original timer-list)))
          (should (memq original timer-list))
          (with-tramp-suspended-timers
            ;; A cadence change must replace rather than strand the old poll.
            (setq mevedel-view-spinner-style 'static)
            (mevedel-view--start-spinner-timer)
            (let ((replacement mevedel-view--spinner-timer))
              (should-not (eq original replacement))
              (should-not (memq original
                                (default-toplevel-value 'timer-list)))
              (should (memq replacement
                            (default-toplevel-value 'timer-list)))
              (should (= phase mevedel-view--spinner-phase-start))
              (setq mevedel-view-spinner-style 'ascii)
              (mevedel-view--start-spinner-timer)
              (should-not (memq replacement
                                    (default-toplevel-value 'timer-list)))))
          (let ((active mevedel-view--spinner-timer))
            (should (memq active timer-list))
            (with-tramp-suspended-timers
              (mevedel-view-stream-stop)
              (should-not (memq active
                                    (default-toplevel-value 'timer-list))))
            (should-not (memq active timer-list))
            (should-not mevedel-view--spinner-timer)
            ;; Stale delivery must not restart a stopped view.
            (funcall (timer--function active))
            (should-not mevedel-view--spinner-timer)))))))

(mevedel-deftest mevedel-view-animation-auto-power-timer-suspension
  (:doc "Resuming a visible view restores the shared auto-power poll too.")
  (let* ((mevedel-view-power--watchers (make-hash-table :test #'eq))
         (mevedel-view-power--state 'unknown)
         (mevedel-view-power--sample-time nil)
         (mevedel-view-power--last-query nil)
         (mevedel-view-power--timer nil)
         (battery-update-functions (copy-sequence battery-update-functions))
         (window-state-change-functions
          (copy-sequence window-state-change-functions))
         (queries 0)
         (battery-status-function
          (lambda ()
            (cl-incf queries)
            '((?L . "AC") (?B . "high")))))
    (mevedel-view-stream-test--with-buffers
      (mevedel-view-stream-test--with-visible-view
        (let ((mevedel-view-spinner-style 'ascii)
              (mevedel-view-spinner-power-policy 'auto)
              (mevedel-view-spinner-battery-framerate 0))
          ;; Both UI-host timers survive TRAMP's disposable binding.
          (let ((timer-list nil) (timer-idle-list nil))
            (mevedel-view--start-spinner "Working...")
            (should (= 1.0 mevedel-view--spinner-timer-period))
            (should (memq mevedel-view--spinner-timer
                          (default-toplevel-value 'timer-list)))
            (should (memq mevedel-view-power--timer
                          (default-toplevel-value 'timer-list))))
          (let ((original mevedel-view-power--timer)
                (phase mevedel-view--spinner-phase-start))
            (should (mevedel--timer-pending-p original))
            (mevedel-view--resume-render-if-attended view-buf)
            (should (mevedel--timer-pending-p mevedel-view-power--timer))
            (should (eq original mevedel-view-power--timer))
            (should (= 0 queries))
            (should (= 1.0 mevedel-view--spinner-timer-period))
            (mevedel--ui-timer-cancel original)
            (mevedel-view-power--poll original)
            (should (= 1 queries))
            (should (= 60 (mevedel-view-power-framerate 60 0 'auto t)))
            (should (= 0.24 mevedel-view--spinner-timer-period))
            (should (= phase mevedel-view--spinner-phase-start))
            (mevedel-view-stream-stop)
            (should (zerop (hash-table-count mevedel-view-power--watchers)))
            (should-not mevedel-view-power--timer)))))))

(mevedel-deftest mevedel-view-animation-overdue-timer
  (:doc "An overdue one-shot does not replay missed callbacks or allocate timers.")
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full)
            (ticks 0))
        (mevedel-view--start-spinner "Working...")
        (let ((timer mevedel-view--spinner-timer)
              (phase mevedel-view--spinner-phase-start))
          (should-not (timer--repeat-delay timer))
          ;; Deliver one deadline after a simulated long stall.  Emacs's
          ;; repeating timer would requeue overdue callbacks in a burst.
          (timer-set-time timer
                          (time-subtract (current-time) (seconds-to-time 3)))
          (cl-letf (((symbol-function 'mevedel-view--spinner-tick)
                     (lambda () (cl-incf ticks))))
            (timer-event-handler timer)
            (should (= ticks 1))
            (should (eq timer mevedel-view--spinner-timer))
            (should (= phase mevedel-view--spinner-phase-start))
            (should (mevedel--timer-pending-p timer))
            (should (< (timer-until timer nil) -0.06))
            (should-not (timer--repeat-delay timer))))))))

(mevedel-deftest mevedel-view-animation-auto-transition
  ()
  ,test
  (test)
  :doc "Battery notifications rearm the view, preserving phase and color bank."
  (cl-letf (((symbol-function 'mevedel-view-animation--colors)
             (lambda (_face _frame) '("#ff0000" . "#ffffff"))))
    (let ((mevedel-view-power--state 'unknown)
          (mevedel-view-power--sample-time nil)
          (mevedel-view-animation--cache nil))
      (mevedel-view-stream-test--with-buffers
        (mevedel-view-stream-test--with-visible-view
          (let ((mevedel-view-spinner-style 'shimmer)
                (mevedel-view-spinner-power-policy 'auto)
                (mevedel-view-spinner-framerate 60)
                (mevedel-view-spinner-battery-framerate 30))
            (mevedel-view--start-spinner "Working...")
            (should (gethash view-buf mevedel-view-power--watchers))
            (should (< (abs (- mevedel-view--spinner-timer-period (/ 1.0 30)))
                       1e-7))
            (let ((phase mevedel-view--spinner-phase-start)
                  (bank mevedel-view-animation--cache))
              (mevedel-view-power--sample '((?L . "AC") (?B . "high")))
              (should (< (abs (- mevedel-view--spinner-timer-period (/ 1.0 60)))
                         1e-7))
              (should (= phase mevedel-view--spinner-phase-start))
              (should (eq bank mevedel-view-animation--cache))
              (mevedel-view-power--sample '((?L . "BAT") (?B . "low")))
              (should (< (abs (- mevedel-view--spinner-timer-period (/ 1.0 30)))
                         1e-7))
              (should (= phase mevedel-view--spinner-phase-start))
              (should (eq bank mevedel-view-animation--cache))))))))

  :doc "An expired AC sample followed by unknown notification slows the real timer"
  (let ((mevedel-view-power--watchers (make-hash-table :test #'eq))
        (mevedel-view-power--timer nil)
        (mevedel-view-power--state 'unknown)
        (mevedel-view-power--sample-time nil)
        (mevedel-view-power--last-query nil)
        (battery-update-functions (copy-sequence battery-update-functions))
        (window-state-change-functions
         (copy-sequence window-state-change-functions))
        (now 1000.5)
        (queries 0))
    (let ((battery-status-function (lambda () (cl-incf queries) nil)))
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
        (mevedel-view-stream-test--with-buffers
          (mevedel-view-stream-test--with-visible-view
            (let ((mevedel-view-spinner-style 'ascii)
                  (mevedel-view-spinner-power-policy 'auto)
                  (mevedel-view-spinner-framerate 60)
                  (mevedel-view-spinner-battery-framerate 1))
              (mevedel-view--start-spinner "Working...")
              (mevedel-view-power--sample '((?L . "AC") (?B . "high")))
              (should (= 0.24 mevedel-view--spinner-timer-period))
              (let ((fast mevedel-view--spinner-timer)
                    (phase mevedel-view--spinner-phase-start))
                (setq now 1060.5)
                (should (eq 'unknown (mevedel-view-power--current-state)))
                (should (mevedel--ui-timer-pending-p fast))
                (run-hook-with-args 'battery-update-functions
                                    '((?L . "N/A") (?B . "N/A") (?p . "N/A")))
                (should (= 1.0 mevedel-view--spinner-timer-period))
                (should-not (eq fast mevedel-view--spinner-timer))
                (should-not (mevedel--ui-timer-pending-p fast))
                (should (mevedel--ui-timer-pending-p
                         mevedel-view--spinner-timer))
                (should (= phase mevedel-view--spinner-phase-start))
                (should (= 0 queries))))))))))

(mevedel-deftest mevedel-view-animation-expired-external
  (:doc "An expired AC reading with a failed backend drops the actual timer rate.")
  (cl-letf (((symbol-function 'mevedel-view-animation--colors)
             (lambda (_face _frame) '("#ff0000" . "#ffffff"))))
    (let ((mevedel-view-animation--cache nil)
          (now 1000.0)
          (mevedel-view-power--state 'unknown)
          (mevedel-view-power--sample-time nil)
          (mevedel-view-power--last-query nil)
          (battery-status-function (lambda () (error "Battery backend unavailable"))))
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
        (mevedel-view-stream-test--with-buffers
          (mevedel-view-stream-test--with-visible-view
            (let ((mevedel-view-spinner-style 'shimmer)
                  (mevedel-view-spinner-power-policy 'auto)
                  (mevedel-view-spinner-framerate 60)
                  (mevedel-view-spinner-battery-framerate 0))
              (mevedel-view--start-spinner "Working...")
              (mevedel-view-power--sample '((?L . "AC") (?B . "high")))
              (should (< (abs (- mevedel-view--spinner-timer-period
                                 (/ 1.0 60))) 1e-7))
              (let ((fast mevedel-view--spinner-timer))
                (setq now 1060.0)
                (when mevedel-view-power--timer
                  (mevedel--ui-timer-cancel mevedel-view-power--timer))
                (mevedel-view-power--poll mevedel-view-power--timer)
                (should-not (mevedel--timer-pending-p fast))
                (should (= 1.0 mevedel-view--spinner-timer-period))))))))))

(mevedel-deftest mevedel-view-animation-tool-display-frame
  (:doc "Tool frames use portable initial glyphs and their own target frame.")
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-tool-spinner-style 'braille)
            (mevedel-view-spinner-power-policy 'full)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read...")))
            (original (symbol-function 'mevedel-view-animation-frame))
            frames)
        (cl-letf (((symbol-function 'char-displayable-p)
                   (lambda (_character) t))
                  ((symbol-function 'mevedel-view-animation-frame)
                   (lambda (style label seconds face &optional frame)
                     (when (eq face 'mevedel-view-ephemeral)
                       (push frame frames))
                     (funcall original style label seconds face frame))))
          (mevedel-view--refresh-pending-tool-lines)
          (should (eq (car frames) :multiple))
          (let ((pos (text-property-any
                      (point-min) (point-max)
                      'mevedel-view-inline-spinner-frame t)))
            (should (equal "- " (get-text-property pos 'display)))
            (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                       (lambda () 0.24)))
              (mevedel-view--spinner-tick))
            (should (eq (car frames) (selected-frame)))
            (should (equal "⠙ " (get-text-property pos 'display)))))))))

(mevedel-deftest mevedel-view-animation-main-initial-frame
  (:doc "An undisplayed status uses a portable frame, not the selected frame.")
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-style 'braille))
        (cl-letf (((symbol-function 'char-displayable-p)
                   (lambda (_character) t)))
          (let ((line (mevedel-view--format-spinner-line "Working...")))
            (should (equal "- Working..."
                           (get-text-property 0 'display line)))))))))

(mevedel-deftest mevedel-view-animation-multiple-frame-glyph-style
  (:doc "A multi-frame status retains dots, ellipsis, and ASCII styles.")
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (dolist (style '(dots ellipsis ascii))
        (let ((mevedel-view-spinner-style style)
              (mevedel-view-tool-spinner-style 'static)
              (mevedel-view-spinner-power-policy 'full))
          (mevedel-view--start-spinner "Working...")
          (let* ((target mevedel-view--spinner-label-target)
                 (pos (marker-position (car target)))
                 (initial (get-text-property pos 'display)))
            (setq mevedel-view--spinner-last-second (floor (float-time)))
            (cl-letf (((symbol-function 'mevedel-view--animation-target-frame)
                       (lambda (_target &optional _all) :multiple))
                      ((symbol-function 'mevedel-view--animation-span-in-window-p)
                       (lambda (_start _end _window) t))
                      ((symbol-function 'mevedel-view--animation-seconds)
                       (lambda () 0.96)))
              (mevedel-view--spinner-tick))
            (should (equal
                     (mevedel-view-animation-frame
                      style "Working..." 0.96 'mevedel-view-spinner :multiple)
                     (get-text-property pos 'display)))
            (should (= (string-width initial)
                       (string-width (get-text-property pos 'display)))))
          (mevedel-view--stop-spinner))))))

(mevedel-deftest mevedel-view-animation-dots-visible-target-frames
  (:doc "Dots ignore other windows not actually displaying the indicator.")
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (insert "Working...")
      (let* ((target (cons (copy-marker (point-min))
                           (copy-marker (point-max))))
             (visible-frame (selected-frame))
             (hidden-frame 'hidden)
             (scrolled-frame 'scrolled)
             (mevedel-view-animation--dots-cache nil))
        (cl-letf (((symbol-function 'get-buffer-window-list)
                   (lambda (&rest _) '(visible hidden scrolled)))
                  ((symbol-function 'window-frame)
                   (lambda (window)
                     (pcase window
                       ('visible visible-frame)
                       ('hidden hidden-frame)
                       (_ scrolled-frame))))
                  ((symbol-function 'frame-visible-p)
                   (lambda (frame) (not (eq frame hidden-frame))))
                  ((symbol-function 'window-start)
                   (lambda (window) (if (eq window 'scrolled) (point-max)
                                      (point-min))))
                  ((symbol-function 'window-end)
                   (lambda (_window) (point-max)))
                  ((symbol-function 'window-hscroll)
                   (lambda (_window) 0))
                  ;; Fake window symbols have no redisplay positions; this
                  ;; case checks which frame receives the dots bank.
                  ((symbol-function 'mevedel-view--animation-span-in-window-p)
                   (lambda (_start _end window)
                     (eq window 'visible)))
                  ((symbol-function 'mevedel-view-animation--dots-frame-supported-p)
                   (lambda (frame) (eq frame visible-frame))))
          (let ((frames (mevedel-view--animation-target-frame target t)))
            (should (equal frames (list visible-frame)))
            (should (equal
                     (mevedel-view-animation-frame
                      'dots "Working" 0 'default frames)
                     "●··· Working"))))))))

(mevedel-deftest mevedel-view-animation-colorless-glyph-cadence
  (:doc "Colorless views use the glyph timer cadence without frame-time queries.")
  (let ((mevedel-view-animation--cache nil)
        (color-queries 0))
    (cl-letf (((symbol-function 'mevedel-view-animation--colors)
               (lambda (_face _frame)
                 (cl-incf color-queries)
                 nil)))
      (mevedel-view-stream-test--with-buffers
        (mevedel-view-stream-test--with-visible-view
          (let ((mevedel-view-spinner-style 'shimmer)
                (mevedel-view-tool-spinner-style 'static)
                (mevedel-view-spinner-power-policy 'full))
            (mevedel-view--start-spinner "Working...")
            (should (= 0.24 mevedel-view--spinner-timer-period))
            (let ((queries color-queries))
              (setq mevedel-view--spinner-last-second (floor (float-time)))
              (mevedel-view--spinner-tick)
              (should (= queries color-queries))
              (should (= 0.24 mevedel-view--spinner-timer-period)))))))))

(mevedel-deftest mevedel-view-animation-attended-target-window
  (:doc "A focused offscreen window cannot animate an unfocused visible target.")
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full)
            (auto-hscroll-mode nil))
        (setq-local truncate-lines t)
        (mevedel-view--start-spinner "Working...")
        (let* ((foreground (selected-window))
               (background (split-window-right))
               (focused (window-frame foreground))
               (background-frame 'background-animation-test-frame)
               (original-window-frame (symbol-function 'window-frame))
               (target mevedel-view--spinner-label-target)
               (position (marker-position (car target)))
               (initial (get-text-property position 'display)))
          (set-window-buffer background view-buf)
          (redisplay t)
          (set-window-hscroll foreground 50)
          (cl-letf (((symbol-function 'window-frame)
                     (lambda (window)
                       (if (eq window background) background-frame
                         (funcall original-window-frame window))))
                    ((symbol-function 'frame-parent) (lambda (_frame) nil))
                    ((symbol-function 'frame-visible-p) (lambda (_frame) t))
                    ((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
                    ((symbol-function 'frame-focus-state)
                     (lambda (frame) (eq frame focused)))
                    ((symbol-function 'mevedel-view--animation-span-in-window-p)
                     (lambda (_start _end window)
                       (eq window background))))
            (should-not (mevedel-view--animation-visible-p))
            (should-not (mevedel-view--spinner-metadata-visible-p))
            (mevedel-view--start-spinner-timer)
            (should-not mevedel-view--spinner-timer-period)
            (should-not mevedel-view--spinner-timer)
            (let ((mevedel-view--spinner-last-second (floor (float-time))))
              (mevedel-view--spinner-tick))
            (should (equal-including-properties
                     initial (get-text-property position 'display)))
            ;; The focus-change hook rearms the window showing the label.
            (cl-letf (((symbol-function 'frame-focus-state)
                       (lambda (frame) (eq frame background-frame))))
              (should (mevedel-view--animation-visible-p))
              (mevedel-view--resume-attended-views)
              (should (= 0.24 mevedel-view--spinner-timer-period))
              (cl-letf (((symbol-function 'mevedel-view--animation-seconds)
                         (lambda () 0.48)))
                (setq mevedel-view--spinner-last-second (floor (float-time)))
                (mevedel-view--spinner-tick))
              (should-not (equal-including-properties
                           initial (get-text-property position 'display))))))))))

(mevedel-deftest mevedel-view-animation-clipped-row
  (:doc "Clipped rows keep progress current without glyph layout or draft edits.")
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-power-policy 'full)
            (auto-hscroll-mode nil)
            (draft "> quoted\nsecond line"))
        (setq-local truncate-lines t)
        (mevedel-view-stream-test--insert-composer-draft draft 4)
        (dolist (style '(ascii ellipsis shimmer static))
          (let ((mevedel-view-spinner-style style))
            (mevedel-view--start-spinner "Working...")
            (set-window-hscroll (selected-window) 100)
            (let ((cursor (point)))
              (cl-letf (((symbol-function 'posn-at-point)
                         (lambda (&rest _) (ert-fail "Animation queried glyphs")))
                        ((symbol-function 'posn-at-x-y)
                         (lambda (&rest _) (ert-fail "Animation queried pixels"))))
                (mevedel-view--start-spinner-timer)
                (should mevedel-view--spinner-timer-period)
                (mevedel-view--spinner-tick))
              (should (equal draft (mevedel-view--input-text)))
              (should (= cursor (point))))))))))

(mevedel-deftest mevedel-view-animation-metadata-middle-visible
  (:doc "A suffix starting above the viewport keeps elapsed time current.")
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'static)
            (window (selected-window)))
        (setq-local truncate-lines t)
        (mevedel-view--start-spinner "Working...")
        (let* ((start (marker-position
                       (car mevedel-view--spinner-metadata-target)))
               (end (marker-position
                     (cdr mevedel-view--spinner-metadata-target)))
               (window-start-position (1+ start)))
          (cl-letf (((symbol-function 'window-start)
                     (lambda (_window) window-start-position))
                    ((symbol-function 'window-end)
                     (lambda (_window &optional _update) (1+ end))))
            (should (mevedel-view--spinner-metadata-visible-p))
            (mevedel-view--start-spinner-timer)
            (should (= 1.0 mevedel-view--spinner-timer-period))
            (setq mevedel-view--spinner-start-time
                  (time-subtract (current-time) (seconds-to-time 3))
                  mevedel-view--spinner-last-second nil)
            (mevedel-view--spinner-tick)
            (should (string-match-p "Working\\.\\.\\. · [3-9]s"
                                    (buffer-substring-no-properties
                                     (mevedel-view-zone-start 'progress)
                                     (overlay-end
                                      (mevedel-view-zone-region 'progress)))))))))))

(mevedel-deftest mevedel-view-animation-low-color-terminal-cadence
  (:doc "A resolved 8-color terminal never schedules color-rate callbacks.")
  (let ((mevedel-view-animation--cache nil))
    (cl-letf (((symbol-function 'display-color-p) (lambda (&optional _frame) t))
              ((symbol-function 'display-graphic-p) (lambda (&optional _frame) nil))
              ((symbol-function 'display-color-cells)
               (lambda (&optional _frame) 8))
              ((symbol-function 'face-foreground)
               (lambda (&rest _) "#ffffff"))
              ((symbol-function 'face-background)
               (lambda (&rest _) "#000000")))
      (mevedel-view-stream-test--with-buffers
        (mevedel-view-stream-test--with-visible-view
          (dolist (style '(shimmer breathe bounce))
            (let ((mevedel-view-spinner-style style)
                  (mevedel-view-tool-spinner-style 'static)
                  (mevedel-view-spinner-power-policy 'full))
              (mevedel-view--start-spinner "Working...")
              (should-not mevedel-view--spinner-main-color-p)
              (should (= 0.24 mevedel-view--spinner-timer-period))
              (should-not (get-text-property
                           0 'face
                           (get-text-property
                            (marker-position
                             (car mevedel-view--spinner-label-target))
                            'display))))))))))

(mevedel-deftest mevedel-view-animation-theme-prepares-off-frame-path
  (:doc "Theme invalidation defers color preparation to semantic maintenance.")
  (let ((mevedel-view-animation--cache nil)
        (color-queries 0))
    (cl-letf (((symbol-function 'mevedel-view-animation--colors)
               (lambda (_face _frame)
                 (cl-incf color-queries)
                 '("#ff0000" . "#ffffff"))))
      (mevedel-view-stream-test--with-buffers
        (mevedel-view-stream-test--with-visible-view
          (let ((mevedel-view-spinner-style 'shimmer)
                (mevedel-view-tool-spinner-style 'static)
                (mevedel-view-spinner-power-policy 'full))
            (mevedel-view--start-spinner "Working...")
            (should (> color-queries 0))
            (mevedel-view-animation-invalidate)
            (let ((queries color-queries))
              (setq mevedel-view--spinner-last-second (floor (float-time)))
              (mevedel-view--spinner-tick)
              (should (= queries color-queries))
              (should-not mevedel-view-animation--cache)
              (setq mevedel-view--spinner-last-second nil)
              (mevedel-view--spinner-tick)
              (should (> color-queries queries))
              (should mevedel-view-animation--cache))))))))

(mevedel-deftest mevedel-view-animation-theme-refreshes-frozen-status
  (:doc "Theme changes repaint a paused zero-fps color sample without a timer.")
  (let ((mevedel-view-animation--cache nil)
        (foreground "#ff0000"))
    (cl-letf (((symbol-function 'mevedel-view-animation--colors)
               (lambda (_face _frame) (cons foreground "#000000"))))
      (mevedel-view-stream-test--with-buffers
        (with-current-buffer data-buf
          (setq-local mevedel--current-request
                      (mevedel-request--create :started-at (current-time)))
          (mevedel-request-set-active-work-paused
           mevedel--current-request t))
        (mevedel-view-stream-test--with-visible-view
          (let ((mevedel-view-spinner-style 'shimmer)
                (mevedel-view-spinner-power-policy 'save)
                (mevedel-view-spinner-battery-framerate 0))
            (mevedel-view--start-spinner "Working...")
            (let* ((target mevedel-view--spinner-label-target)
                   (start (marker-position (car target)))
                   (before (get-text-property start 'display))
                   (phase mevedel-view--spinner-phase-start)
                   (frozen mevedel-view--spinner-frozen-seconds)
                   (render-state mevedel-view--spinner-rendered-state)
                   (was-modified (buffer-modified-p))
                   (undo buffer-undo-list)
                   (here (point)))
              (should (get-text-property 0 'face before))
              (should-not mevedel-view--spinner-timer)
              (dolist (hook '(enable-theme-functions disable-theme-functions))
                (setq foreground (if (eq hook 'enable-theme-functions)
                                     "#00ff00" "#0000ff"))
                (run-hook-with-args hook 'mevedel-test-theme)
                (should-not (equal-including-properties
                             before (get-text-property start 'display)))
                (should (equal-including-properties
                         (mevedel-view-animation-frame
                          'shimmer "Waiting for input" frozen
                          'mevedel-view-spinner (selected-frame))
                         (get-text-property start 'display)))
                (should-not mevedel-view--spinner-timer)
                (should (= phase mevedel-view--spinner-phase-start))
                (should (= frozen mevedel-view--spinner-frozen-seconds))
                (should (equal render-state mevedel-view--spinner-rendered-state))
                (should (eq was-modified (buffer-modified-p)))
                (should (eq undo buffer-undo-list))
                (should (= here (point)))
                (setq before (get-text-property start 'display)))
              (mevedel-view--spinner-tick)
              (should (equal-including-properties
                       before (get-text-property start 'display)))
              ;; The same event must not paint an undisplayed buffer for an
              ;; unrelated frame; its frozen phase repaints on visibility.
              (set-window-buffer (selected-window) data-buf)
              (setq foreground "#ffff00")
              (run-hook-with-args 'enable-theme-functions 'mevedel-test-theme)
              (should mevedel-view--spinner-theme-stale-p)
              (should (equal-including-properties
                       before (get-text-property start 'display)))
              (should-not mevedel-view--spinner-timer)
              (set-window-buffer (selected-window) view-buf)
              (mevedel-view--resume-on-window-change (selected-window))
              (should-not mevedel-view--spinner-theme-stale-p)
              (should (equal-including-properties
                       (mevedel-view-animation-frame
                        'shimmer "Waiting for input" frozen
                        'mevedel-view-spinner (selected-frame))
                       (get-text-property start 'display)))
              (should (= frozen mevedel-view--spinner-frozen-seconds))
              (should-not mevedel-view--spinner-timer))))))))

(mevedel-deftest mevedel-view-animation-frame-refreshes-frozen-status
  (:doc "A paused zero-fps label changes palettes when its display frame changes.")
  (let ((display-frame :first))
    (cl-letf (((symbol-function 'mevedel-view--animation-buffer-frame)
               (lambda () display-frame))
              ((symbol-function 'mevedel-view--animation-target-frame)
               (lambda (&rest _) display-frame))
              ((symbol-function 'mevedel-view-animation-color-available-p)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-view-animation-frame)
               (lambda (_style label _seconds _face frame)
                 (propertize label 'face
                             (list :foreground
                                   (if (eq frame :first)
                                       "#ff0000" "#00ff00"))))))
      (mevedel-view-stream-test--with-buffers
        (with-current-buffer data-buf
          (setq-local mevedel--current-request
                      (mevedel-request--create :started-at (current-time)))
          (mevedel-request-set-active-work-paused
           mevedel--current-request t))
        (mevedel-view-stream-test--with-visible-view
          (let ((mevedel-view-spinner-style 'shimmer)
                (mevedel-view-spinner-power-policy 'save)
                (mevedel-view-spinner-battery-framerate 0))
            (mevedel-view--start-spinner "Working...")
            (let* ((start (marker-position
                           (car mevedel-view--spinner-label-target)))
                   (before (get-text-property start 'display))
                   (phase mevedel-view--spinner-phase-start)
                   (frozen mevedel-view--spinner-frozen-seconds)
                   (render-state mevedel-view--spinner-rendered-state)
                   (modified (buffer-modified-p))
                   (undo buffer-undo-list)
                   (here (point)))
              (should (eq mevedel-view--spinner-sample-frame :first))
              (should-not mevedel-view--spinner-timer)
              (setq display-frame :second)
              (mevedel-view--resume-on-window-change (selected-window))
              (should-not (equal-including-properties
                           before (get-text-property start 'display)))
              (should (equal (plist-get
                              (get-text-property 0 'face
                                                 (get-text-property start 'display))
                              :foreground)
                             "#00ff00"))
              (should (eq mevedel-view--spinner-sample-frame :second))
              (should-not mevedel-view--spinner-theme-stale-p)
              (should-not mevedel-view--spinner-timer)
              (should (= phase mevedel-view--spinner-phase-start))
              (should (= frozen mevedel-view--spinner-frozen-seconds))
              (should (equal render-state mevedel-view--spinner-rendered-state))
              (should (eq modified (buffer-modified-p)))
              (should (eq undo buffer-undo-list))
              (should (= here (point)))
              (setq before (get-text-property start 'display))
              (mevedel-view--resume-on-window-change (selected-window))
              (should (equal-including-properties
                       before (get-text-property start 'display)))
              (should-not mevedel-view--spinner-timer)
              ;; The old sample must stay untouched while its window is
              ;; hidden; returning to an attended frame repairs it.
              (set-window-buffer (selected-window) data-buf)
              (setq display-frame :first)
              (mevedel-view--start-spinner-timer)
              (should (eq mevedel-view--spinner-sample-frame :second))
              (should (equal-including-properties
                       before (get-text-property start 'display)))
              (set-window-buffer (selected-window) view-buf)
              (mevedel-view--resume-on-window-change (selected-window))
              (should (eq mevedel-view--spinner-sample-frame :first))
              (should (equal-including-properties
                       (get-text-property start 'display)
                       (propertize "Waiting for input" 'face
                                   '(:foreground "#ff0000"))))
              (should (= phase mevedel-view--spinner-phase-start))
              (should (= frozen mevedel-view--spinner-frozen-seconds))
              (should-not mevedel-view--spinner-timer))))))))

(mevedel-deftest mevedel-view-animation-glyph-fallback-refreshes-frozen-status
  (:doc "Theme/capability changes repaint frozen main and tool glyphs without timers.")
  (dolist (style '(braille dots))
    (dolist (freeze '(global battery))
      (let ((supported t)
            (mevedel-view-animation--dots-cache nil))
        (cl-letf (((symbol-function 'char-displayable-p)
                   (lambda (char &optional _frame)
                     (or supported (not (memq char '(?⠋ ?● ?·)))))))
          (mevedel-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--current-request
                          (mevedel-request--create :started-at (current-time))))
            (mevedel-view-stream-test--with-visible-view
              (let ((mevedel-view-spinner-style style)
                    (mevedel-view-tool-spinner-style style)
                    (mevedel-view-spinner-power-policy 'full)
                    (mevedel-view-spinner-battery-framerate 30)
                    (mevedel-view-spinner-animate t)
                    (mevedel-view--pending-tool-calls
                     '(("call-1" . "Calling Read..."))))
                (mevedel-view--start-spinner "Working...")
                (mevedel-view--refresh-pending-tool-lines)
                (mevedel-view--spinner-tick)
                (mevedel-view--interaction-register
                 '(:kind ask :id ask :origin "/root" :body "Question"))
                (let* ((main (car mevedel-view--spinner-label-target))
                       (tool (caar mevedel-view--spinner-tool-targets))
                       (before-main (get-text-property (marker-position main)
                                                       'display))
                       (before-tool (get-text-property (marker-position tool)
                                                       'display)))
                  (should (mevedel-view--animation-visible-p))
                  (should (if (eq style 'braille)
                              (string-prefix-p "⠋ " before-main)
                            (string-prefix-p "●" before-main)))
                  (should (if (eq style 'braille)
                              (string-prefix-p "⠋ " before-tool)
                            (string-prefix-p "●" before-tool)))
                  (if (eq freeze 'global)
                      (setq mevedel-view-spinner-animate nil)
                    (setq mevedel-view-spinner-power-policy 'save
                          mevedel-view-spinner-battery-framerate 0))
                  (mevedel-view--start-spinner-timer)
                  (let ((phase mevedel-view--spinner-phase-start)
                        (frozen mevedel-view--spinner-frozen-seconds)
                        (modified (buffer-modified-p))
                        (undo buffer-undo-list)
                        (here (point)))
                    (should-not mevedel-view--spinner-timer)
                    (setq supported nil)
                    (run-hook-with-args 'enable-theme-functions 'mevedel-test-theme)
                    (let ((expected-main
                           (mevedel-view-animation-frame
                            style "Waiting for input" frozen
                            'mevedel-view-spinner
                            (mevedel-view--animation-target-frame
                             mevedel-view--spinner-label-target
                             (eq style 'dots))))
                          (expected-tool
                           (mevedel-view-animation-frame
                            style "" frozen 'mevedel-view-ephemeral
                            (mevedel-view--animation-target-frame
                             (car mevedel-view--spinner-tool-targets)
                             (eq style 'dots)))))
                      (should-not (equal-including-properties
                                   before-main (get-text-property
                                                (marker-position main) 'display)))
                      (should-not (equal-including-properties
                                   before-tool (get-text-property
                                                (marker-position tool) 'display)))
                      (should (equal-including-properties
                               expected-main (get-text-property
                                              (marker-position main) 'display)))
                      (should (equal-including-properties
                               expected-tool (get-text-property
                                              (marker-position tool) 'display))))
                    (setq mevedel-view--spinner-last-second nil)
                    (mevedel-view--spinner-tick)
                    (should-not mevedel-view--spinner-timer)
                    (should (= phase mevedel-view--spinner-phase-start))
                    (should (= frozen mevedel-view--spinner-frozen-seconds))
                    (should (eq modified (buffer-modified-p)))
                    (should (eq undo buffer-undo-list))
                    (should (= here (point)))
                    ;; A font capability change need not run a theme hook.
                    ;; Visibility rearming retries the cached dots support.
                    (setq supported t)
                    (mevedel-view--resume-on-window-change (selected-window))
                    (should (equal-including-properties
                             (mevedel-view-animation-frame
                              style "Waiting for input" frozen
                              'mevedel-view-spinner
                              (mevedel-view--animation-target-frame
                               mevedel-view--spinner-label-target
                               (eq style 'dots)))
                             (get-text-property (marker-position main)
                                                'display)))
                    (should (equal-including-properties
                             (mevedel-view-animation-frame
                              style "" frozen 'mevedel-view-ephemeral
                              (mevedel-view--animation-target-frame
                               (car mevedel-view--spinner-tool-targets)
                               (eq style 'dots)))
                             (get-text-property (marker-position tool)
                                                'display)))
                    (let ((visible-main (get-text-property
                                         (marker-position main) 'display))
                          (visible-tool (get-text-property
                                         (marker-position tool) 'display)))
                      (set-window-buffer (selected-window) data-buf)
                      (setq supported nil)
                      (run-hook-with-args 'enable-theme-functions
                                          'mevedel-test-theme)
                      (should mevedel-view--spinner-theme-stale-p)
                      (should (equal-including-properties
                               visible-main (get-text-property
                                             (marker-position main) 'display)))
                      (should (equal-including-properties
                               visible-tool (get-text-property
                                             (marker-position tool) 'display)))
                      (set-window-buffer (selected-window) view-buf)
                      (mevedel-view--resume-on-window-change
                       (selected-window))
                      (should-not mevedel-view--spinner-theme-stale-p)
                      (should-not (equal-including-properties
                                   visible-main (get-text-property
                                                 (marker-position main)
                                                 'display)))
                      (should-not (equal-including-properties
                                   visible-tool (get-text-property
                                                 (marker-position tool)
                                                 'display))))
                    (should-not mevedel-view--spinner-timer)))))))))))

(mevedel-deftest mevedel-view-animation-face-refreshes-frozen-status
  (:doc "Customize and direct face edits repaint paused color without a timer.")
  (let ((mevedel-view-animation--cache nil)
        (original (face-attribute 'mevedel-view-spinner :foreground nil))
        (already-installed
         (advice-member-p #'mevedel-view--refresh-animation-on-face
                          'set-face-attribute)))
    (unwind-protect
        (progn
          (advice-add 'set-face-attribute :after
                      #'mevedel-view--refresh-animation-on-face)
          (set-face-attribute 'mevedel-view-spinner nil :foreground "#ff0000")
          (cl-letf (((symbol-function 'mevedel-view-animation--colors)
                     (lambda (_face _frame)
                       (cons (face-foreground 'mevedel-view-spinner nil t)
                             "#000000"))))
            (mevedel-view-stream-test--with-buffers
              (with-current-buffer data-buf
                (setq-local mevedel--current-request
                            (mevedel-request--create :started-at (current-time)))
                (mevedel-request-set-active-work-paused
                 mevedel--current-request t))
              (mevedel-view-stream-test--with-visible-view
                (let ((mevedel-view-spinner-style 'shimmer)
                      (mevedel-view-spinner-power-policy 'save)
                      (mevedel-view-spinner-battery-framerate 0))
                  (mevedel-view--start-spinner "Working...")
                  (let* ((start (marker-position
                                 (car mevedel-view--spinner-label-target)))
                         (before (get-text-property start 'display))
                         (phase mevedel-view--spinner-phase-start)
                         (frozen mevedel-view--spinner-frozen-seconds)
                         (modified (buffer-modified-p))
                         (undo buffer-undo-list)
                         (here (point)))
                    (should-not mevedel-view--spinner-timer)
                    (set-face-attribute 'mevedel-view-spinner nil
                                        :foreground "#00ff00")
                    (should-not (equal-including-properties
                                 before (get-text-property start 'display)))
                    (should (equal-including-properties
                             (mevedel-view-animation-frame
                              'shimmer "Waiting for input" frozen
                              'mevedel-view-spinner (selected-frame))
                             (get-text-property start 'display)))
                    (should-not mevedel-view--spinner-timer)
                    (should (= phase mevedel-view--spinner-phase-start))
                    (should (= frozen mevedel-view--spinner-frozen-seconds))
                    (should (eq modified (buffer-modified-p)))
                    (should (eq undo buffer-undo-list))
                    (should (= here (point)))
                    (let ((visible (get-text-property start 'display)))
                      (set-window-buffer (selected-window) data-buf)
                      (set-face-attribute 'mevedel-view-spinner nil
                                          :foreground "#0000ff")
                      (should mevedel-view--spinner-theme-stale-p)
                      (should (equal-including-properties
                               visible (get-text-property start 'display)))
                      (should-not mevedel-view--spinner-timer)
                      (set-window-buffer (selected-window) view-buf)
                      (mevedel-view--resume-on-window-change (selected-window))
                      (should-not mevedel-view--spinner-theme-stale-p)
                      (should-not (equal-including-properties
                                   visible (get-text-property start 'display)))
                      (should-not mevedel-view--spinner-timer))))))))
      (set-face-attribute 'mevedel-view-spinner nil :foreground original)
      (unless already-installed
        (advice-remove 'set-face-attribute
                       #'mevedel-view--refresh-animation-on-face)))))

(mevedel-deftest mevedel-view-animation-inherited-face-refreshes-frozen-status
  (:doc "Changing an inherited face repaints a timer-free paused label.")
  (let ((original-spinner
         (face-attribute 'mevedel-view-spinner :foreground nil))
        (original-inherit
         (face-attribute 'mevedel-view-spinner :inherit nil))
        (original-parent
         (face-attribute 'font-lock-comment-face :foreground nil))
        (alias 'mevedel-view-test-spinner-comment-alias)
        (old-alias (get 'mevedel-view-test-spinner-comment-alias 'face-alias))
        (installed (advice-member-p #'mevedel-view--refresh-animation-on-face
                                    'set-face-attribute)))
    (unwind-protect
        (progn
          (advice-add 'set-face-attribute :after
                      #'mevedel-view--refresh-animation-on-face)
          (set-face-attribute 'mevedel-view-spinner nil
                              :foreground 'unspecified)
          (put alias 'face-alias 'font-lock-comment-face)
          (dolist (variant (list (cons 'font-lock-comment-face
                                       'font-lock-comment-face)
                                 (cons alias 'font-lock-comment-face)
                                 (cons 'font-lock-comment-face alias)))
            (set-face-attribute 'mevedel-view-spinner nil
                                :inherit (list 'bold (car variant)))
            (set-face-attribute 'font-lock-comment-face nil
                                :foreground "#ff0000")
            (should (mevedel-view--spinner-inherits-face-p
                     'font-lock-comment-face (selected-frame)))
            (cl-letf (((symbol-function 'mevedel-view-animation--colors)
                       (lambda (_face _frame)
                         (cons (face-foreground 'mevedel-view-spinner nil t)
                               "#000000"))))
              (mevedel-view-stream-test--with-buffers
                (with-current-buffer data-buf
                  (setq-local mevedel--current-request
                              (mevedel-request--create :started-at (current-time)))
                  (mevedel-request-set-active-work-paused
                   mevedel--current-request t))
                (mevedel-view-stream-test--with-visible-view
                  (let ((mevedel-view-spinner-style 'shimmer)
                        (mevedel-view-spinner-power-policy 'save)
                        (mevedel-view-spinner-battery-framerate 0))
                    (mevedel-view--start-spinner "Working...")
                    (let* ((start (marker-position
                                   (car mevedel-view--spinner-label-target)))
                           (before (get-text-property start 'display))
                           (phase mevedel-view--spinner-phase-start)
                           (frozen mevedel-view--spinner-frozen-seconds))
                      (should-not mevedel-view--spinner-timer)
                      (set-face-attribute (cdr variant) nil
                                          :foreground "#00ff00")
                      (should-not (equal-including-properties
                                   before (get-text-property start 'display)))
                      (should (equal-including-properties
                               (mevedel-view-animation-frame
                                'shimmer "Waiting for input" frozen
                                'mevedel-view-spinner (selected-frame))
                               (get-text-property start 'display)))
                      (should (= phase mevedel-view--spinner-phase-start))
                      (should (= frozen mevedel-view--spinner-frozen-seconds))
                      (should-not mevedel-view--spinner-timer)))))))
      (set-face-attribute 'font-lock-comment-face nil
                          :foreground original-parent)
      (set-face-attribute 'mevedel-view-spinner nil
                          :foreground original-spinner :inherit original-inherit)
      (put alias 'face-alias old-alias)
      (unless installed
        (advice-remove 'set-face-attribute
                       #'mevedel-view--refresh-animation-on-face))))))

(mevedel-deftest mevedel-view-animation-shimmer-overflow
  (:doc "Frozen shimmer retains phase but displays the current overflow count.")
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-animate nil)
            (mevedel-view-tool-spinner-style 'shimmer)
            (mevedel-view-pending-tools-visible-max 1))
        (mevedel-view-stream-test--insert-composer-draft "> draft\nsecond line")
        (setq mevedel-view--pending-tool-calls
              '(("1" . "Calling Read: a") ("2" . "Calling Bash: sleep")))
        (mevedel-view--refresh-pending-tool-lines)
        (push '("3" . "Calling Grep: x") mevedel-view--pending-tool-calls)
        (mevedel-view--refresh-pending-tool-lines)
        (let* ((target (car (last mevedel-view--spinner-tool-targets)))
               (start (marker-position (car target))))
          (should (string-match-p
                   "2 more tools running"
                   (get-text-property start 'display))))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text)))))))

(mevedel-deftest mevedel-view-animation-shimmer-tool-banks
  (:doc "All distinct shimmer rows survive cache eviction and display changes.")
  (let ((mevedel-view-animation--cache nil))
    (mevedel-view-stream-test--with-buffers
      (mevedel-view-stream-test--with-visible-view
        (let ((mevedel-view-spinner-style 'static)
              (mevedel-view-tool-spinner-style 'shimmer)
              (mevedel-view-spinner-power-policy 'full)
              (mevedel-view-pending-tools-visible-max 8))
          (cl-letf (((symbol-function 'mevedel-view-animation--colors)
                     (lambda (_face frame)
                       (unless (eq frame :multiple)
                         '("#ffffff" . "#000000"))))
                    ((symbol-function 'mevedel-view--animation-span-in-window-p)
                     (lambda (&rest _) t)))
            (setq mevedel-view--pending-tool-calls
                  (cl-loop for i below 9
                           collect (cons (number-to-string i)
                                         (format "Calling Tool%d: argument" i))))
            (mevedel-view--refresh-pending-tool-lines)
            ;; A new display needs different banks.  The semantic scheduler
            ;; must prepare every row, even with no main color indicator.
            (cl-letf (((symbol-function 'mevedel-view--animation-target-frame)
                       (lambda (&rest _) :multiple)))
              (mevedel-view--start-spinner-timer t)
              ;; Another view may evict the entire shared reuse cache.
              (setq mevedel-view-animation--cache nil)
              (dolist (target mevedel-view--spinner-tool-targets)
                (should (mevedel-view-animation-color-ready-p
                         'shimmer (mevedel-view--tool-animation-label target)
                         'mevedel-view-ephemeral :multiple))))))))))

(mevedel-deftest mevedel-view--spinner-next-delay
  (:doc "Combines sweep, portable glyph and elapsed cadences without polling rest.")
  ,test
  (test)
  (should (= 2.6 (mevedel-view--spinner-next-delay '((shimmer . 0.04)) 2.0)))
  (should (= 1.0 (mevedel-view--spinner-next-delay
                  '((shimmer . 0.04) (metadata . 1.0)) 2.0)))
  (should (= 0.24 (mevedel-view--spinner-next-delay
                   '((shimmer . 0.04) (glyph . 0.24)) 2.0)))
  (should (= 0.04 (mevedel-view--spinner-next-delay
                   '((shimmer . 0.04) (glyph . 0.24)) 1.0))))

(mevedel-deftest mevedel-view-animation-shimmer-mixed-displays
  (:doc "A portable tool glyph continues during another tool's color rest.")
  (let ((mevedel-view-animation--cache nil))
    (mevedel-view-stream-test--with-buffers
      (mevedel-view-stream-test--with-visible-view
        (let ((mevedel-view-spinner-style 'static)
              (mevedel-view-tool-spinner-style 'shimmer)
              (mevedel-view-spinner-power-policy 'full))
          (cl-letf (((symbol-function 'mevedel-view-animation--colors)
                     (lambda (_face frame)
                       (unless (eq frame :multiple)
                         '("#ffffff" . "#000000"))))
                    ((symbol-function 'mevedel-view--animation-span-in-window-p)
                     (lambda (&rest _) t)))
            (setq mevedel-view--pending-tool-calls
                  '(("1" . "Calling Read: a") ("2" . "Calling Bash: sleep")))
            (mevedel-view--refresh-pending-tool-lines)
            (cl-letf (((symbol-function 'mevedel-view--animation-target-frame)
                       (lambda (target &optional _all)
                         (if (eq target (car mevedel-view--spinner-tool-targets))
                             (selected-frame) :multiple))))
              (mevedel-view--start-spinner-timer t)
              (should (assq 'shimmer mevedel-view--spinner-timer-plan))
              (should (equal '(glyph . 0.24)
                             (assq 'glyph mevedel-view--spinner-timer-plan)))
              (should (= 0.24 (mevedel-view--spinner-next-delay
                               mevedel-view--spinner-timer-plan 2.0))))))))))

(mevedel-deftest mevedel-view-animation-shimmer-frozen-tool-display
  (:doc "A frozen tool-only row adopts the destination display without a timer.")
  (let ((mevedel-view-animation--cache nil))
    (mevedel-view-stream-test--with-buffers
      (mevedel-view-stream-test--with-visible-view
        (let ((mevedel-view-spinner-style 'static)
              (mevedel-view-tool-spinner-style 'shimmer)
              (mevedel-view-spinner-animate nil))
          (cl-letf (((symbol-function 'mevedel-view-animation--colors)
                     (lambda (_face frame)
                       (unless (eq frame :multiple)
                         '("#ffffff" . "#000000"))))
                    ((symbol-function 'mevedel-view--animation-span-in-window-p)
                     (lambda (&rest _) t)))
            (setq mevedel-view--pending-tool-calls '(("1" . "Calling Read: a")))
            (mevedel-view--refresh-pending-tool-lines)
            (let ((start (marker-position
                          (caar mevedel-view--spinner-tool-targets))))
              (should (equal "Calling Read" (get-text-property start 'display)))
              (cl-letf (((symbol-function 'mevedel-view--animation-target-frame)
                         (lambda (&rest _) :multiple)))
                (mevedel-view--start-spinner-timer t)
                (should (equal "- Calling Read"
                               (get-text-property start 'display)))
                (should-not mevedel-view--spinner-timer)))))))))

(mevedel-deftest mevedel-view-animation-interaction-pause
  (:doc "Registering and answering an Ask stops and resumes the real scheduler.")
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (setq-local mevedel--current-request
                  (mevedel-request--create :started-at (current-time))))
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'ascii)
            (mevedel-view-spinner-power-policy 'full))
        (cl-letf (((symbol-function 'mevedel-view--animation-span-in-window-p)
                   (lambda (&rest _) t)))
          (mevedel-view-stream-test--insert-composer-draft "> draft\nsecond line")
          (mevedel-view--start-spinner "Working...")
          (should (timerp mevedel-view--spinner-timer))
          (mevedel-view--interaction-register
           '(:kind ask :id ask :origin "/root" :body "Question"))
          (should-not mevedel-view--spinner-timer)
          (mevedel-view--interaction-unregister 'ask)
          (should (timerp mevedel-view--spinner-timer))
          (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))))

(mevedel-deftest mevedel-view-animation-paused-shimmer-rearm
  (:doc "Visibility and theme refresh retain paused tool phases at ordinary fps.")
  (let ((mevedel-view-animation--cache nil))
    (mevedel-view-stream-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--current-request
                    (mevedel-request--create :started-at (current-time))))
      (mevedel-view-stream-test--with-visible-view
        (let ((mevedel-view-spinner-style 'shimmer)
              (mevedel-view-tool-spinner-style 'shimmer)
              (mevedel-view-spinner-power-policy 'full)
              (seconds 0.9))
          (cl-letf (((symbol-function 'mevedel-view-animation--colors)
                     (lambda (&rest _) '("#ffffff" . "#000000")))
                    ((symbol-function 'mevedel-view--animation-span-in-window-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'mevedel-view--animation-seconds)
                     (lambda () seconds)))
            (mevedel-view--start-spinner "Working...")
            (setq mevedel-view--pending-tool-calls '(("1" . "Calling Read: a")))
            (mevedel-view--refresh-pending-tool-lines)
            (mevedel-view--spinner-tick)
            (mevedel-view--interaction-register
             '(:kind ask :id ask :origin "/root" :body "Question"))
            (let* ((start (marker-position
                           (caar mevedel-view--spinner-tool-targets)))
                   (display (get-text-property start 'display))
                   (phase (mevedel-view--tool-sample-seconds start)))
              (setq seconds 1.3)
              (mevedel-view--start-spinner-timer t)
              (should (equal-including-properties
                       display (get-text-property start 'display)))
              (should (= phase (mevedel-view--tool-sample-seconds start)))
              (should-not mevedel-view--spinner-timer))))))))

(mevedel-deftest mevedel-view--animation-frozen-p
  (:doc "Input pause and reduced motion independently freeze presentation.")
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-animate t)
            (mevedel-view-spinner-power-policy 'full)
            (request (mevedel-request--create :started-at (current-time))))
        (with-current-buffer data-buf (setq mevedel--current-request request))
        (should-not (mevedel-view--animation-frozen-p))
        (mevedel-request-set-active-work-paused request t)
        (should (mevedel-view--animation-frozen-p))
        (mevedel-request-set-active-work-paused request nil)
        (should-not (mevedel-view--animation-frozen-p))
        (let ((mevedel-view-spinner-animate nil))
          (should (mevedel-view--animation-frozen-p)))))))

(mevedel-deftest mevedel-view--capture-native-animation
  (:doc "Freezing uses the native submitted phase instead of stale Lisp display text.")
  (mevedel-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-style 'ascii))
        (mevedel-view--start-spinner "Working...")
        (setq mevedel-view--native-animation-targets
              (list mevedel-view--spinner-label-target)
              mevedel-view--spinner-last-sample-seconds 0.0)
        (cl-letf (((symbol-function 'mevedel-view-native-sample) (lambda (_) 0.72)))
          (mevedel-view--capture-native-animation)
          (should (= 0.72 mevedel-view--spinner-last-sample-seconds))
          (let ((mevedel-view-spinner-animate nil))
            (should (= 0.72 (mevedel-view--animation-display-seconds)))))))))

(mevedel-deftest mevedel-view--sync-native-animation
  (:doc "Offloaded labels leave only metadata wakeups and preserve the composer.")
  (mevedel-view-stream-test--with-buffers
    (mevedel-view-stream-test--with-visible-view
      (let ((mevedel-view-spinner-style 'bounce)
            (mevedel-view-spinner-power-policy 'full)
            (mevedel-view-spinner-framerate 30))
        (mevedel-view--start-spinner "Working...")
        (goto-char (mevedel-view--input-start))
        (insert "> retained
second line")
        (let ((draft (mevedel-view--input-text)) captured)
          (cl-letf (((symbol-function 'mevedel-view-native-sync)
                     (lambda (specs _seconds _rearm)
                       (setq captured specs)
                       (mapcar #'car specs)))
                    ((symbol-function 'mevedel-view-native-sample) (lambda (_) 0.5)))
            (mevedel-view--start-spinner-timer)
            (should (eq (cadar captured) 'bounce))
            (should (= (/ 1.0 30) (nth 4 (car captured))))
            (should (equal mevedel-view--spinner-timer-plan '((metadata . 1.0))))
            (let* ((start (marker-position (car mevedel-view--spinner-label-target)))
                   (sample (get-text-property start 'display)))
              (mevedel-view--spinner-tick)
              (should (equal-including-properties sample (get-text-property start 'display))))
            (should (equal draft (mevedel-view--input-text)))))))))

(provide 'test-mevedel-view-stream)
;;; test-mevedel-view-stream.el ends here
