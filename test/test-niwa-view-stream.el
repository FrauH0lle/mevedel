;;; test-niwa-view-stream.el --- Streaming view tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-utilities)
(require 'niwa-view)
(require 'niwa-view-agent)
(require 'niwa-view-animation)
(require 'niwa-view-composer)
(require 'niwa-view-interaction)
(require 'niwa-view-native)
(require 'niwa-view-power)
(require 'niwa-view-render)
(require 'niwa-view-segments)
(require 'niwa-view-stream)
(require 'niwa-telemetry)
(require 'niwa-agent-control)
(require 'niwa-collaboration)
(require 'niwa-collaboration-transport)
(require 'niwa-collaboration-guest)
(require 'niwa-execution-transcript)
(require 'niwa-plugin-registry)
(require 'niwa-permission-review)
(require 'niwa-structs)
(require 'niwa-tool-exec)
(require 'niwa-tool-ptc)
(require 'niwa-tool-render-data)
(require 'niwa-tool-registry)
(require 'niwa-tool-repair-diagnostics)
(require 'niwa-transport)
(require 'niwa-transcript-audit)
(require 'niwa-view-zone)
(eval-when-compile (require 'tramp))

;; `battery'
(defvar battery-status-function)
(defvar battery-update-functions)

(niwa-deftest niwa-view-stream-ensure-progress-for-fsm/cold-load ()
  ,test
  (test)
  :doc "request progress loads its agent predicate before any composer exists"
  (with-temp-buffer
    (let ((status
           (call-process
            (expand-file-name invocation-name invocation-directory)
            nil t nil "--batch" "-Q" "--eval"
            (prin1-to-string
             `(progn
                (setq load-path ',load-path load-prefer-newer t)
                (require 'ert)
                (require 'gptel-request)
                (require 'niwa-view-stream)
                (should-not (featurep 'niwa-view-composer))
                (with-temp-buffer
                  (setq-local niwa--view-buffer nil
                              niwa--agent-invocation nil)
                  (let ((fsm (gptel-make-fsm
                              :info (list :buffer (current-buffer)))))
                    (should-not
                     (niwa-view-stream-ensure-progress-for-fsm fsm))
                    (should (featurep 'niwa-view-composer)))))))))
      (ert-info ((buffer-string))
        (should (equal 0 status))
        (should (equal "" (buffer-string)))))))

(defmacro niwa-view-stream-test--without-battery-load (&rest body)
  "Run BODY with `battery' treated as loaded and other features delegated.
A power poll requires `battery'; loading it for real initializes D-Bus
and selects the host's backend, so cases bind `battery-status-function'
instead."
  (declare (indent 0) (debug t))
  (let ((original (make-symbol "require")))
    `(let ((,original (symbol-function 'require)))
       (cl-letf (((symbol-function 'require)
                  (lambda (feature &rest args)
                    (if (eq feature 'battery) 'battery
                      (apply ,original feature args)))))
         ,@body))))

(defmacro niwa-view-stream-test--with-buffers (&rest body)
  "Create paired data and view buffers, then evaluate BODY."
  (declare (indent 0) (debug t))
  `(let ((data-buf (generate-new-buffer " *niwa-stream-data*"))
         (view-buf (generate-new-buffer " *niwa-stream-view*"))
         (niwa-permission-mode 'ask)
         (niwa-user-dir
          (file-name-as-directory
           (make-temp-file "niwa-view-stream-user-" t)))
         (niwa-plugin-extra-roots nil))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (niwa--transcript-org-mode)
             (setq-local niwa--current-request nil)
             (setq-local niwa--session nil)
             (setq-local gptel-response-separator "\n\n")
             ;; These buffers own no root lifecycle, so teardown drains the
             ;; queues without the session save a fixture cannot complete.
             (setq-local niwa-view--abort-function
                         #'niwa-view-test--abort-interactions))
           (niwa-view--setup view-buf data-buf)
           ,@body)
       (when (buffer-live-p view-buf)
         (with-current-buffer view-buf
           (niwa-view-stream-stop))
         (kill-buffer view-buf))
       (when (buffer-live-p data-buf)
         (kill-buffer data-buf))
       (when (file-directory-p niwa-user-dir)
         (delete-directory niwa-user-dir t)))))

(defun niwa-view-stream-test--insert-data (buffer text property)
  "Insert TEXT with gptel PROPERTY into BUFFER."
  (with-current-buffer buffer
    (let ((start (point-max)))
      (goto-char start)
      (insert text)
      (put-text-property start (point) 'gptel property))))

(defun niwa-view-stream-test--draft-offset ()
  "Return the composer caret offset for the current view."
  (let ((start (niwa-view-composer-draft-start)))
    (with-current-buffer (niwa-view-composer-buffer)
      (- (point) start))))

(defun niwa-view-stream-test--rail-activity (&optional property)
  "Read PROPERTY (default `display') from the composer's activity cell."
  (with-current-buffer (niwa-view-composer-buffer)
    (get-text-property (1+ (point-min)) (or property 'display))))

(defun niwa-view-stream-test--leave-row-for-draft (offset)
  "Leave the visited transcript row and place the draft caret at OFFSET."
  (let ((start (niwa-view-composer-draft-start)))
    (goto-char (point-min))
    (with-current-buffer (niwa-view-composer-buffer)
      (goto-char (+ start offset)))))

(defun niwa-view-stream-test--draw-running-rows ()
  "Draw `niwa-view--pending-tool-calls' as a live render draws them.
Earlier running rows are replaced; their displayed samples carry over."
  (let ((previous (niwa-view--snapshot-tool-animation-targets))
        (inhibit-read-only t)
        (boundary (niwa-view--history-insertion-marker)))
    (niwa-view--retire-pending-decorations (point-min) boundary)
    (let ((start (marker-position boundary)))
      (when niwa-view--pending-tool-calls
        (save-excursion
          (goto-char boundary)
          (niwa-view--with-render-boundaries-advancing
            (niwa-view--insert-pending-run
             niwa-view--pending-tool-calls))))
      (niwa-view--adopt-tool-animation-targets
       start (marker-position boundary) previous))))

(defun niwa-view-stream-test--batch (data-buf calls &optional settle)
  "Start the parallel tool CALLS in DATA-BUF, and with SETTLE finish them.
Each call is a gptel hook plist.  Settling writes every result before the
post-tool hooks run, so a zero boundary delay renders what a debounced
render would see once gptel has written the batch."
  (with-current-buffer data-buf
    (if (not settle)
        (mapc #'niwa-view-stream-pre-tool calls)
      (dolist (call calls)
        (niwa-view-stream-test--insert-data
         data-buf (format "%S\n\nresult\n" (list :name (plist-get call :name)
                                                 :args (plist-get call :args)))
         (cons 'tool (format "%S" (plist-get call :args)))))
      (mapc #'niwa-view-stream-post-tool calls))))

(defun niwa-view-stream-test--begin-live-turn (view-buf data-buf)
  "Anchor an in-flight turn in VIEW-BUF at the end of DATA-BUF."
  (with-current-buffer view-buf
    (setq niwa-view--in-flight-turn-start
          (copy-marker (niwa-view--history-insertion-marker)))
    (setq niwa-view--data-turn-start
          (with-current-buffer data-buf (copy-marker (point-max))))))

(defun niwa-view-stream-test--history (view-buf)
  "Return VIEW-BUF's transcript text, excluding managed chrome."
  (with-current-buffer view-buf
    (buffer-substring-no-properties
     (point-min) (niwa-view--history-insertion-marker))))

(defun niwa-view-stream-test--insert-repair-audited-tool (data-buf)
  "Insert one completed repair-audited tool call into DATA-BUF."
  (with-current-buffer data-buf
    (let ((start (point)))
      (insert "(:name \"Collect\" :args (:names [\"alice\"]))\n\ncompleted\n")
      (put-text-property start (point) 'gptel '(tool . "repair-call")))
    (let ((start (point)))
      (insert
       (niwa-tool-repair-format-audit-block
        'committed
        '((:rule wrap-array-singleton :source generic
                :paths ((names)) :before string :after array))))
      (put-text-property start (point) 'gptel 'ignore))))

(defun niwa-view-stream-test--count-substring (needle text)
  "Return the number of non-overlapping NEEDLE occurrences in TEXT."
  (let ((count 0)
        (start 0)
        position)
    (while (setq position (string-search needle text start))
      (cl-incf count)
      (setq start (+ position (length needle))))
    count))

(defmacro niwa-view-stream-test--with-visible-view (&rest body)
  "Display the fixture view and its composer while evaluating BODY."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (switch-to-buffer view-buf)
     (should (niwa-view-composer-display (selected-window)))
     (redisplay t)
     (with-current-buffer view-buf ,@body)))

(ert-deftest niwa-view-stream-approval-invocation-identity ()
  "Out-of-order approvals and settlements never decorate an identical sibling."
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (call '(:name "Read" :args (:file_path "same"))))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4))
      (niwa-view-stream-test--batch data-buf (list call call))
      (cl-labels ((event (type id &rest extra)
                    (run-hook-with-args
                     'niwa-pipeline-tool-event-hook
                     (append (list :type type :tool-use-id id :data-buffer data-buf)
                             call extra))))
        (event 'started "first")
        (event 'started "second")
        (let ((keys (with-current-buffer view-buf
                      (mapcar #'car niwa-view--pending-tool-calls))))
          (event 'permission-approved "second"
                 :permission-approvals
                 '((:source reviewer :outcome allow-once :reason "Second call only")))
          (with-current-buffer view-buf
            (should (equal keys (mapcar #'car niwa-view--pending-tool-calls)))
            (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
            (should (= 4 (niwa-view-stream-test--draft-offset)))
            (save-excursion
              (goto-char (point-min))
              (search-forward "Read: same")
              (should-not (search-forward "Auto-approved" (line-end-position) t))
              (forward-line 1)
              (search-forward "Read: same")
              (search-forward "Auto-approved · once" (line-end-position))
              (should (equal "Second call only"
                             (get-text-property (match-beginning 0) 'help-echo)))
              (should-not (get-text-property (match-beginning 0)
                                             'niwa-view-inline-spinner-frame))))
          (event 'settled "second")
          ;; The ID-less post hook must not consume the still-running first call.
          (with-current-buffer data-buf (niwa-view-stream-post-tool call))
          (should (= 1 (with-current-buffer view-buf
                         (length niwa-view--pending-tool-calls))))
          (should-not (string-match-p "Auto-approved"
                                      (niwa-view-stream-test--history view-buf)))
          ;; Late approval and a nested child event cannot resurrect a row.
          (event 'permission-approved "second" :permission-approvals
                 '((:source reviewer :outcome allow-once :reason "Late")))
          (event 'started "child" :parent-tool-use-id "first")
          (event 'permission-approved "child" :parent-tool-use-id "first"
                 :permission-approvals
                 '((:source reviewer :outcome allow-once :reason "Nested")))
          (should-not (string-match-p "Auto-approved"
                                      (niwa-view-stream-test--history view-buf)))
          (event 'settled "first")
          ;; Settlement marks the call finished; its result is still due,
          ;; so it keeps its place in the run until the post-tool hook.
          (with-current-buffer view-buf
            (should (= 1 (length niwa-view--pending-tool-calls)))
            (should (plist-get (nth 3 (car niwa-view--pending-tool-calls))
                               :settled)))
          (let ((text (niwa-view-stream-test--history view-buf)))
            (should (string-match-p "✓ Read: same" text))
            (should-not (string-match-p "●" text)))
          (with-current-buffer data-buf (niwa-view-stream-post-tool call))
          (should-not (with-current-buffer view-buf
                        niwa-view--pending-tool-calls)))))))

(niwa-deftest niwa-view-stream-handle-pipeline-event (:quiet t)
  ,test
  (test)
  :doc "real dispatch binds identical pre-hook rows before reverse reviewer callbacks"
  (niwa-view-stream-test--with-buffers
    (let* ((niwa-view-tool-boundary-render-delay 0)
          (niwa-permission-reviewer 'auto)
          (niwa-permission-rules '(("ApprovalProbe" :action ask)))
          (room (list :data-buffer data-buf :guests (make-hash-table :test #'eql)
                      :tool-call-occurrences (make-hash-table :test #'equal)))
          (niwa-collaboration--rooms (niwa-test-room-registry room))
          replies handlers outcomes)
      (with-current-buffer data-buf
        (insert "Run ApprovalProbe for this task.\n")
        (setq-local niwa--session
                    (niwa-session--create :permission-mode 'ask :root-buffer data-buf)))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4))
      (niwa-view-stream-test--batch data-buf
                                      '((:name "ApprovalProbe") (:name "ApprovalProbe")))
      (cl-letf (((symbol-function 'niwa-permission-review--model)
                 (lambda (_ callback &optional _stream) (push callback replies) #'ignore))
                ((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _) (funcall callback nil))))
        (let ((tool (niwa-tool--create
                     :name "ApprovalProbe" :read-only-p t :async-p t
                     :handler (lambda (callback _)
                                (push (cons (niwa-pipeline-active-tool-use-id) callback)
                                      handlers)))))
          (with-current-buffer data-buf
            (dolist (id '("first" "second"))
              (niwa-pipeline-run-tool-outcome
               tool (lambda (value) (push value outcomes)) nil (list :tool-use-id id))))
          (funcall (car replies) "{\"decision\":\"allow-once\",\"reason\":\"Second only\"}")
          (let ((records (niwa-collaboration--tool-records (plist-get room :records))))
            (should (= 2 (length records)))
            (should-not (plist-get (plist-get (car records) :presentation) :approval))
            (should (equal "Second only"
                           (plist-get (plist-get (cadr records) :presentation) :approval))))
          (with-current-buffer view-buf
            (should (= 2 (length niwa-view--pending-tool-calls)))
            (should-not (niwa-view--permission-approval-reason
                         (nth 3 (car niwa-view--pending-tool-calls))))
            (should (equal "Second only" (niwa-view--permission-approval-reason
                                          (nth 3 (cadr niwa-view--pending-tool-calls)))))
            (should (string-search "Auto-approved · once" (buffer-string))))
          (funcall (cdr (assoc "second" handlers)) '(:result "Error: failed" :status error))
          (let ((records (niwa-collaboration--tool-records (plist-get room :records))))
            (should (equal "running" (plist-get (car records) :status)))
            (should (equal "failed" (plist-get (cadr records) :status)))
            (should (equal "Second only"
                           (plist-get (plist-get (cadr records) :presentation) :approval))))
          (with-current-buffer view-buf
            ;; The failed call keeps its place, marked failed, until its
            ;; result reaches the transcript; its approval stays factual.
            (should (= 2 (length niwa-view--pending-tool-calls)))
            (should (eq 'error (plist-get (nth 3 (cadr niwa-view--pending-tool-calls))
                                          :settled)))
            (should (string-search "× ApprovalProbe  Auto-approved · once"
                                   (buffer-string)))
            (should (string-search "● ApprovalProbe" (buffer-string))))
          (funcall (cadr replies) "{\"decision\":\"allow-once\",\"reason\":\"First only\"}")
          (funcall (cdr (assoc "first" handlers)) '(:result "done"))))
      (should (= 2 (length outcomes)))
      ;; Both calls are finished; gptel's post-tool hooks retire them.
      (with-current-buffer view-buf
        (should (cl-every (lambda (entry) (plist-get (nth 3 entry) :settled))
                          niwa-view--pending-tool-calls)))
      (with-current-buffer data-buf
        (dotimes (_ 2) (niwa-view-stream-post-tool '(:name "ApprovalProbe"))))
      (with-current-buffer view-buf
        (should-not niwa-view--pending-tool-calls)
        (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-stream-test--draft-offset))))))

  :doc "binds hook rows to pipeline args that list omitted optionals in spec order"
  (niwa-view-stream-test--with-buffers
    (let* ((niwa-view-tool-boundary-render-delay 0)
           (spec '((file_path string) (offset integer) (limit integer)))
           ;; gptel's hooks see the model's plist; the pipeline sees the
           ;; positional values converted back in spec order, nils included.
           (first '(:name "Read" :args (:limit 5 :file_path "a")))
           (second '(:name "Read" :args (:file_path "b"))))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (niwa-view-stream-test--batch data-buf (list first second))
      (cl-labels ((event (type id call &rest extra)
                    (run-hook-with-args
                     'niwa-pipeline-tool-event-hook
                     (append (list :type type :tool-use-id id :data-buffer data-buf
                                   :name "Read"
                                   :args (niwa-tool--positional-to-plist
                                          (mapcar (lambda (arg)
                                                    (plist-get (plist-get call :args)
                                                               (intern (format ":%s" (car arg)))))
                                                  spec)
                                          spec))
                             extra))))
        (event 'started "first" first)
        (event 'started "second" second)
        (with-current-buffer view-buf
          (should (equal '("first" "second")
                         (mapcar (lambda (entry) (plist-get (nth 3 entry) :tool-use-id))
                                 niwa-view--pending-tool-calls))))
        ;; One running row per call, not a bound twin beside each hook row.
        (should (= 2 (niwa-view-stream-test--count-substring
                      "● Read" (niwa-view-stream-test--history view-buf))))
        (event 'settled "second" second :status 'success)
        (with-current-buffer data-buf (niwa-view-stream-post-tool second))
        (with-current-buffer view-buf
          (should (equal '("first")
                         (mapcar (lambda (entry) (plist-get (nth 3 entry) :tool-use-id))
                                 niwa-view--pending-tool-calls))))
        (event 'settled "first" first :status 'success)
        (with-current-buffer data-buf (niwa-view-stream-post-tool first))
        (with-current-buffer view-buf
          (should-not niwa-view--pending-tool-calls))))))

(niwa-deftest niwa-view-stream-begin-turn ()
  ,test
  (test)
  :doc "records independent active-turn markers and cleanup releases them"
  (niwa-view-stream-test--with-buffers
    (let ((data-start (with-current-buffer data-buf
                        (copy-marker (point-max) nil))))
      (with-current-buffer view-buf
        (let ((view-start (copy-marker (niwa-view--history-insertion-marker) nil)))
          (niwa-view-stream-begin-turn view-start data-start)
          (should (markerp niwa-view--in-flight-turn-start))
          (should (markerp niwa-view--data-turn-start))
          (should-not (eq view-start niwa-view--in-flight-turn-start))
          (should-not (eq data-start niwa-view--data-turn-start))
          (niwa-view-stream-stop)
          (should-not niwa-view--in-flight-turn-start)
          (should-not niwa-view--data-turn-start))))))

(niwa-deftest niwa-view-stream-schedule ()
  ,test
  (test)
  :doc "async streaming redraw preserves multiline leading-> draft and point"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "Prompt\n" nil)
    (let ((data-start (with-current-buffer data-buf
                        (copy-marker (point-min) nil)))
          (draft "> quoted\nsecond line"))
      (niwa-view-stream-test--insert-data
       data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft draft 4)
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker) nil))
        (setq niwa-view--data-turn-start data-start)
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay repeat callback &rest args)
                     (unless repeat
                       (apply callback args))
                     'scheduled)))
          (with-current-buffer data-buf
            (niwa-view-stream-schedule)))
        (setq niwa-view--render-timer nil)
        (should (string-match-p
                 "Assistant text"
                 (buffer-substring-no-properties
                  (point-min) (niwa-view-transcript-tail))))
        (should (equal draft (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-stream-test--draft-offset)))))))

(niwa-deftest niwa-view--render-stream-update
  (:doc "rolls back failed agent observer renders")
  ,test
  (test)
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq-local niwa-view--agent-transcript-p t)
      (let ((before (buffer-string))
            warned)
        (cl-letf (((symbol-function 'niwa-view-render-live-update)
                   (lambda (&rest _)
                     (let ((inhibit-read-only t))
                       (delete-region (point-min) (point-max)))
                     (error "Observer render failed")))
                  ((symbol-function 'display-warning)
                   (lambda (&rest _) (setq warned t))))
          (niwa-view--render-stream-update data-buf))
        (should warned)
        (should (equal before (buffer-string)))))))

(niwa-deftest niwa-view-stream-handle-execution-event ()
  ,test
  (test)
  :doc "a Monitor batch neither refreshes its row nor schedules a render"
  (niwa-view-stream-test--with-buffers
    (let (touched)
      (cl-letf (((symbol-function 'niwa-view--refresh-tool-row)
                 (lambda (&rest _) (push 'refresh touched) t))
                ((symbol-function 'niwa-view--schedule-render)
                 (lambda (&rest _) (push 'render touched)))
                ((symbol-function 'niwa-view-stream--schedule-execution-row-recovery)
                 (lambda (&rest _) (push 'recovery touched))))
        (should-not
         (niwa-view-stream-handle-execution-event
          (list :type 'monitor-batch :data-buffer data-buf :owner "/root"
                :tool-use-id "call-monitor" :body "line\n"
                :facts '(:execution-id "exec-1" :kind monitor :state running))))
        (should-not touched)
        ;; The same event as ordinary progress does reach the row.
        (niwa-view-stream-handle-execution-event
         (list :type 'progress :data-buffer data-buf :owner "/root"
               :tool-use-id "call-monitor"
               :facts '(:execution-id "exec-1" :state running)))
        (should touched))))

  :doc "the pending live line stays plain while the row carries the metadata"
  ;; The pending line is keyed by gptel's tool-call name and arguments,
  ;; and an execution event arrives keyed by the provider's call id, so
  ;; the two never met.  What the user sees mid-run is the plain pending
  ;; line; the execution metadata and output tail belong to the row.
  (niwa-view-stream-test--with-buffers
    (let* ((session (niwa-session--create :name "execution-pending"))
           (draft "> quoted\nsecond line")
           (bash-tool
            (niwa-tool--create
             :name "Bash" :renderer #'niwa-tool-exec--render-bash)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-max))))
        (niwa-view-test--insert-composer-draft draft 3))
      (let ((point-offset
             (with-current-buffer view-buf
               (niwa-view-stream-test--draft-offset))))
        (cl-letf (((symbol-function 'niwa-tool-get)
                   (lambda (name) (and (equal name "Bash") bash-tool)))
                  ((symbol-function 'niwa-view-render-live-update)
                   #'ignore))
          (with-current-buffer data-buf
            ;; Exactly the plist gptel passes: name and arguments, no id.
            (niwa-view-stream-pre-tool
             (list :name "Bash" :args '(:command "printf run")
                   :buffer (buffer-name data-buf)))
            (should-not
             (niwa-view-stream-handle-execution-event
              (list :type 'progress :session session :data-buffer data-buf
                    :owner "main"
                    :tool-use-id "call-live"
                    :tool-args '(:command "printf run")
                    :output-tail "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7"
                    :facts '(:execution-id "exec-000001" :state running
                             :wall-time-seconds 2.5 :output-bytes 9
                             :output-lines 1 :omitted-output-bytes 0)))))
          (with-current-buffer view-buf
            (should (equal 1 (length niwa-view--pending-tool-calls)))
            (let ((label (format "%S" (cdar niwa-view--pending-tool-calls))))
              (should (equal '("Bash" nil) (cdar niwa-view--pending-tool-calls)))
              (should-not (string-match-p "exec-000001" label))
              (should-not (string-match-p "line 7" label)))
            ;; The tail did reach the execution cache the row renders from.
            (should (equal "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7"
                           (plist-get (gethash "call-live"
                                               niwa-view--execution-events)
                                      :output-tail)))
            (should-not (plist-get (gethash "call-live"
                                            niwa-view--execution-events)
                                   :output-preview-truncated-p))
            (should (equal draft
                           (niwa-view-composer-draft-text nil t)))
            (should (= point-offset
                       (niwa-view-stream-test--draft-offset))))))))

  :doc "progress stays transient while terminal rows persist across cache turnover"
  (niwa-view-stream-test--with-buffers
    (let* ((session (niwa-session--create :name "execution-view"))
           (draft "> quoted\nsecond line")
           (bash-tool
           (niwa-tool--create
             :name "Bash" :renderer #'niwa-tool-exec--render-bash))
           (niwa-view-render-cache-max-entries 1)
           (rerenders 0)
           data-before)
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (insert "#+begin_tool (Bash :command \"printf run\")\n")
        (let ((start (point))
              (block
               (niwa-tool-render-data-format
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
        (setq-local niwa--session session)
        (niwa-view--full-rerender)
        (niwa-view-test--insert-composer-draft draft 3)
        (let ((point-offset (niwa-view-stream-test--draft-offset)))
          (cl-letf (((symbol-function 'niwa-tool-get)
                     (lambda (name)
                       (and (equal name "Bash") bash-tool)))
                    ((symbol-function 'niwa-view-rerender)
                     (lambda (&optional _buffer)
                       (cl-incf rerenders)
                       (niwa-view--full-rerender)
                       'rerender-timer)))
            (should-not
             (niwa-view-stream-handle-execution-event
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
                      (niwa-tool-render-data-segment-bounds "call-live")))
                   (rendering
                    (niwa-view--render-tool-call
                     (niwa-view--tool-call-parse data-buf (car bounds) (cdr bounds))
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
                                   niwa-view--execution-events)))
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
                          (point-min) (niwa-view-transcript-tail))))
            (should-not (string-match-p
                         "line 1"
                         (buffer-substring-no-properties
                          (point-min) (niwa-view-transcript-tail))))
            (should (string-match-p
                     "Bash: printf run · running · 2s"
                     (buffer-substring-no-properties
                      (point-min) (niwa-view-transcript-tail))))
            (should-not
             (niwa-view-stream-handle-execution-event
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
            (should-not (gethash "call-live" niwa-view--execution-events))
            (let ((parsed
                   (with-current-buffer data-buf
                     (niwa-tool-render-data-extract
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
                            (point-min) (niwa-view-transcript-tail))))
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
              (should (get-text-property (point) 'niwa-view-collapsed))
              (niwa-view-toggle-section)
              (let ((visible (buffer-substring-no-properties
                              (point-min) (niwa-view-transcript-tail))))
                (should (string-match-p "whole head" visible))
                (should (string-match-p "whole tail" visible))
                (should (string-match-p "success · exit 0" visible))))
            (should-not
             (niwa-view-stream-handle-execution-event
              (list :type 'progress :session session :data-buffer data-buf
                    :owner "main"
                    :tool-use-id "call-new"
                    :tool-args '(:command "printf new")
                    :output-tail "new live tail"
                    :facts '(:execution-id "exec-000002" :state running
                             :wall-time-seconds 2.1 :output-bytes 13
                             :output-lines 1 :omitted-output-bytes 0))))
            (should (gethash "call-new" niwa-view--execution-events))
            (should-not (gethash "call-live" niwa-view--execution-events))
            (let ((visible (buffer-substring-no-properties
                            (point-min) (niwa-view-transcript-tail))))
              (should (string-match-p "whole head" visible))
              (should (string-match-p "whole tail" visible))
              (should (string-match-p "Bash: printf new · running · 2s"
                                      visible))
              (should-not (string-match-p "new live tail" visible)))
            (should (equal draft (niwa-view-composer-draft-text)))
            (should (= point-offset
                       (niwa-view-stream-test--draft-offset))))))
      (with-current-buffer data-buf
        (should (= 1 (length (niwa-transcript-audit-records
                              (buffer-string) 'execution-breadcrumb))))
        (should (equal (substring-no-properties
                        (niwa-tool-render-data-strip
                         data-before "call-live"))
                       (substring-no-properties
                        (niwa--strip-hook-audit-blocks
                         (niwa-tool-render-data-strip
                          (buffer-string) "call-live"))))))))
  :doc "retains a terminal event until its parallel tool row is inserted"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool (Read :file_path \"other\")\n")
      (let ((start (point)))
        (insert "(:name \"Read\" :args (:file_path \"other\"))\n\nwaiting\n")
        (put-text-property start (point) 'gptel '(tool . "call-other")))
      (insert "#+end_tool\n"))
    (niwa-view-stream-handle-execution-event
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
                       niwa-execution-transcript--pending-terminals))
      (insert "#+begin_tool (Bash :command \"printf late\")\n")
      (let ((start (point)))
        (insert "(:name \"Bash\" :args (:command \"printf late\"))\n\nyielded\n")
        (put-text-property start (point) 'gptel '(tool . "call-late")))
      (insert "#+end_tool\n")
      (niwa-execution-transcript-retry-pending-terminals data-buf)
      (should-not (gethash
                   "call-late"
                   niwa-execution-transcript--pending-terminals))
      (let ((parsed
             (niwa-tool-render-data-extract
              (buffer-substring-no-properties (point-min) (point-max))
              nil "call-late")))
        (should (equal "late terminal output"
                       (plist-get (cdr parsed) :execution-output))))))
  :doc "finalizes a headless agent row at the data-buffer response boundary"
  (let ((agent-data (generate-new-buffer " *execution-headless-agent*")))
    (unwind-protect
        (progn
          (niwa-view-stream-handle-execution-event
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
            (should-not (bound-and-true-p niwa--view-buffer))
            (should (gethash
                     "call-headless"
                     niwa-execution-transcript--pending-terminals))
            (insert "#+begin_tool (Bash :command \"printf headless\")\n")
            (let ((start (point)))
              (insert "(:name \"Bash\" :args (:command \"printf headless\"))\n\nyielded\n")
              (put-text-property start (point) 'gptel
                                 '(tool . "call-headless")))
            (insert "#+end_tool\n")
            (niwa-execution-transcript-retry-terminals)
            (should-not
             (gethash
              "call-headless"
              niwa-execution-transcript--pending-terminals))
            (let ((parsed
                   (niwa-tool-render-data-extract
                    (buffer-substring-no-properties
                     (point-min) (point-max))
                    nil "call-headless")))
              (should
               (equal "headless terminal output"
                      (plist-get (cdr parsed) :execution-output))))))
      (kill-buffer agent-data)))
  :doc "routes same-session progress to the matching owner view"
  (let ((session (niwa-session--create :name "execution-routing"))
        (main-data (generate-new-buffer " *execution-main-data*"))
        (main-view (generate-new-buffer " *execution-main-view*"))
        (agent-data (generate-new-buffer " *execution-agent-data*"))
        (agent-view (generate-new-buffer " *execution-agent-view*"))
        targets)
    (unwind-protect
        (progn
          (dolist (buffer (list main-data agent-data))
            (with-current-buffer buffer
              (niwa--transcript-org-mode)
              (setq-local niwa--session session)))
          (niwa-view--setup main-view main-data)
          (niwa-view--setup agent-view agent-data '(:agent-transcript-p t))
          (cl-letf
              (((symbol-function
                 'niwa-view--refresh-tool-row)
                (lambda (_data-buffer _tool-use-id)
                  (push (current-buffer) targets)
                  t)))
            (niwa-view-stream-handle-execution-event
             (list :type 'progress :session session :data-buffer agent-data
                   :owner "explorer--owned"
                   :tool-use-id "call-agent"
                   :facts nil :tool-args nil))
            (niwa-view-stream-handle-execution-event
             (list :type 'progress :session session :data-buffer main-data
                   :owner "main"
                   :tool-use-id "call-main"
                   :facts nil :tool-args nil)))
          (should (equal (list main-view agent-view) targets)))
      (dolist (buffer (list main-data main-view agent-data agent-view))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun niwa-view-stream-test--check-mid-turn-breadcrumb (threshold expected)
  "Check a completion during a running turn with group THRESHOLD.
EXPECTED is the view text that must show the completion once, before the
turn's later text, live and after reconstruction."
  (niwa-view-stream-test--with-buffers
    (let ((session (niwa-session--create :name "mid-turn-breadcrumb"))
          (niwa-view-tool-group-collapse-threshold threshold)
          (draft "> quoted\nsecond line")
          data-turn-start tracking info)
      (dolist (buffer (list data-buf view-buf))
        (with-current-buffer buffer
          (setq-local niwa--session session)))
      (niwa-view-stream-test--insert-data data-buf "Run the checks\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point) nil)))
      (with-current-buffer view-buf
        (niwa-view--begin-external-turn "Run the checks" data-turn-start))
      (niwa-view-stream-test--insert-data
       data-buf "Starting both checks.\n" 'response)
      (with-current-buffer data-buf
        (dolist (call '(("call-slow" . "make slow") ("call-fast" . "make fast")))
          (insert (format "#+begin_tool (Bash :command %S)\n" (cdr call)))
          (let ((start (point)))
            (insert (format "(:name \"Bash\" :args (:command %S))\n\nyielded\n"
                            (cdr call)))
            (put-text-property start (point) 'gptel (cons 'tool (car call))))
          (insert "#+end_tool\n"))
        ;; gptel keeps a newline after its response marker while streaming.
        (setq tracking (copy-marker (point) nil))
        (insert "\n")
        (setq info (list :buffer data-buf :position (copy-marker tracking)
                         :tracking-marker tracking))
        (setq-local niwa--current-request
                    (niwa-request--create :fsm (gptel-make-fsm :info info))))
      (with-current-buffer view-buf
        (niwa-view-render-live-update data-buf)
        (niwa-view-test--insert-composer-draft draft 4))
      (niwa-view-stream-test--with-visible-view
        (niwa-view-stream-handle-execution-event
         (list :type 'terminal :delivery 'mailbox :session session
               :data-buffer data-buf :owner "main" :tool-use-id "call-slow"
               :tool-args '(:command "make slow") :whole-output "done"
               :facts '(:execution-id "exec-slow" :command "make slow"
                        :state completed :termination exited :exit-code 0
                        :outcome success)))
        (let ((visible (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))
          (ert-info ((format "View after completion:\n%s" visible))
            (should-not (string-search "Background command \"make slow\" completed" visible))))
        ;; Flush only what the completion itself scheduled.
        (niwa-view--flush-scheduled-render view-buf t)
        (let ((visible (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))
          (ert-info ((format "View after its render:\n%s" visible))
            (should (= 1 (niwa-view-stream-test--count-substring
                          expected visible)))))
        ;; The completion follows the still-running call in source.  Local
        ;; progress refreshes must replace only that call, not append another
        ;; copy of the separately projected completion.  Interleave tail
        ;; rebuilds to cover the reported grow/reset/grow sequence too.
        (dotimes (n 15)
          (niwa-view-stream-handle-tool-progress
           (list :type 'progress :session session :data-buffer data-buf
                 :owner "main" :tool-use-id "call-fast"
                 :tool-args '(:command "make fast")
                 :facts '(:execution-id "exec-fast" :command "make fast"
                          :state running)))
          (when (zerop (% (1+ n) 5))
            (niwa-view-render-live-update data-buf))
          (ert-info ((format "Completion after progress update %d" n))
            (should (= 1 (niwa-view-stream-test--count-substring
                          expected
                          (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))))
          (should (equal draft (niwa-view-composer-draft-text)))
          (should (= 4 (niwa-view-stream-test--draft-offset))))
        (with-current-buffer data-buf
          (should (= 1 (length (niwa-transcript-audit-records
                               (buffer-string) 'execution-breadcrumb)))))
        (with-current-buffer data-buf
          (save-excursion
            (goto-char tracking)
            (let ((start (point)))
              (insert "Both checks are done.")
              (put-text-property start (point) 'gptel 'response)
              (set-marker tracking (point)))))
        (niwa-view-render-live-update data-buf)
        (let ((visible (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))
          (ert-info ((format "View:\n%s" visible))
            (should (= 1 (niwa-view-stream-test--count-substring
                          expected visible)))
            ;; A folded completion is not repeated as its own line.
            (should (= (if (string-prefix-p "Background command" expected) 1 0)
                       (niwa-view-stream-test--count-substring
                        "Background command \"make slow\" completed" visible)))
            (should (< (string-search expected visible)
                       (string-search "Both checks are done." visible))))
          (when (string-search "[Show result]" expected)
            (save-excursion
              (goto-char (point-min))
              (search-forward "[Show result]")
              (let ((at (match-beginning 0)))
                (should (eq 'execution-breadcrumb
                            (get-text-property at 'niwa-view-type)))
                (should (equal "exec-slow"
                               (plist-get
                                (get-text-property
                                 at 'niwa-view-execution-breadcrumb)
                                :execution-id))))))
          (should (equal draft (niwa-view-composer-draft-text)))
          (should (= 4 (niwa-view-stream-test--draft-offset))))
        (niwa-view--cancel-scheduled-render))
      (with-current-buffer data-buf
        (setq-local niwa--current-request nil))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let ((visible (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))
          (ert-info ((format "Reconstructed view:\n%s" visible))
            (should (= 1 (niwa-view-stream-test--count-substring
                          expected visible)))
            (should (< (string-search expected visible)
                       (string-search "Both checks are done." visible)))))))))

(niwa-deftest niwa-view-stream-terminal-breadcrumb-live ()
  ,test
  (test)
  :doc "terminal event projects its durable audit immediately in an idle open view"
  (niwa-view-stream-test--with-buffers
    (let ((session (niwa-session--create :name "live-breadcrumb"))
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
        (setq-local niwa--session session)
        (insert "#+begin_tool (Bash :command \"printf run\")\n")
        (let ((start (point)))
          (insert "(:name \"Bash\" :args (:command \"printf run\"))\n\nyielded\n")
          (put-text-property start (point) 'gptel '(tool . "call-live")))
        (insert "#+end_tool\n"))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--full-rerender)
        (niwa-view-test--insert-composer-draft draft 4)
        (let ((start (niwa-view-composer-draft-start)))
          (with-current-buffer (niwa-view-composer-buffer)
            (set-mark (1+ start))
            (setq mark-active t))))
      (setq event (plist-put event :data-buffer data-buf)
            event (plist-put event :session session))
      (cl-letf (((symbol-function 'niwa-view--full-rerender)
                 (lambda () (cl-incf rerenders))))
        (niwa-view-stream-handle-execution-event event)
        (with-current-buffer view-buf
          (let ((visible (buffer-substring-no-properties
                          (point-min) (niwa-view-transcript-tail))))
            (should (= 1 (niwa-view-stream-test--count-substring
                          "[Show result]" visible)))
            (should (string-match-p
                     "● Background command \"printf run\" completed (exit code 0)"
                     visible))
            (should (equal draft (niwa-view-composer-draft-text)))
            (should (= 4 (niwa-view-stream-test--draft-offset)))
            (let ((start (niwa-view-composer-draft-start)))
              (with-current-buffer (niwa-view-composer-buffer)
                (should mark-active)
                (should (= 1 (- (mark) start)))))
            (save-excursion
              (goto-char (point-min))
              (search-forward "[Show result]")
              (let ((at (match-beginning 0)))
                (should (eq 'execution-breadcrumb
                            (get-text-property at 'niwa-view-type)))
                (should (get-text-property at 'niwa-view-source))
                (should (equal "exec-live"
                               (plist-get
                                (get-text-property
                                 at 'niwa-view-execution-breadcrumb)
                                :execution-id)))))))
        (niwa-view-stream-handle-execution-event event)
        (with-current-buffer view-buf
          (should (= 1 (niwa-view-stream-test--count-substring
                        "[Show result]" (buffer-string))))
          (should (zerop rerenders))))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (should (= 1 (niwa-view-stream-test--count-substring
                      "[Show result]" (buffer-string))))
        (should (equal draft (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-stream-test--draft-offset))))
      (with-current-buffer data-buf
        (should (= 1 (length (niwa-transcript-audit-records
                              (buffer-string) 'execution-breadcrumb)))))))
  :doc "a mid-turn completion folds into its turn's activity, before later text"
  (niwa-view-stream-test--check-mid-turn-breadcrumb 1 "1 command completed")
  :doc "an ungrouped mid-turn completion is one line before later text"
  (niwa-view-stream-test--check-mid-turn-breadcrumb
   0 "Background command \"make slow\" completed (exit code 0) · main  [Show result]"))

(niwa-deftest niwa-view-stream-terminal-breadcrumb-projection ()
  ,test
  (test)
  :doc "headless completions reconstruct; historical views stay on their segment"
  (niwa-view-stream-test--with-buffers
    (let* ((session (niwa-session--create :name "deferred-breadcrumb"))
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
        (setq-local niwa--session session)
        (insert "#+begin_tool (Bash :command \"printf late\")\n")
        (let ((start (point)))
          (insert "(:name \"Bash\" :args (:command \"printf late\"))\n\nyielded\n")
          (put-text-property start (point) 'gptel '(tool . "call-late")))
        (insert "#+end_tool\n"))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (setq-local niwa--data-buffer nil))
      (niwa-view-stream-handle-execution-event event)
      (with-current-buffer view-buf
        (setq-local niwa--data-buffer data-buf))
      (let ((archive (generate-new-buffer " *breadcrumb historical source*")))
        (unwind-protect
            (cl-letf (((symbol-function 'niwa-view-segments-display-buffer)
                       (lambda () archive)))
              (niwa-view-stream-handle-execution-event event)
              (with-current-buffer view-buf
                (should (= 0 (niwa-view-stream-test--count-substring
                              "[Show result]" (buffer-string))))))
          (kill-buffer archive)))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (should (= 1 (niwa-view-stream-test--count-substring
                      "[Show result]" (buffer-string))))))))

(niwa-deftest niwa-view-stream-handle-tool-progress ()
  ,test
  (test)
  :doc "preserves a multiline composer draft for generic tool progress"
  (niwa-view-stream-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft draft 3))
      (let ((point-offset
             (with-current-buffer view-buf
               (niwa-view-stream-test--draft-offset))))
        (cl-letf (((symbol-function 'niwa-view--refresh-tool-row)
                   (lambda (&rest _) nil))
                  ((symbol-function
                    'niwa-view-stream--schedule-execution-row-recovery)
                   #'ignore))
          (niwa-view-stream-handle-tool-progress
           (list :type 'progress :data-buffer data-buf
                 :tool-use-id "ptc-live"
                 :facts '(:kind ptc :live-p t :active-tool "Read"
                          :completed-count 2))))
        (with-current-buffer view-buf
          (should (eq 'ptc
                      (plist-get
                       (plist-get (gethash "ptc-live"
                                           niwa-view--execution-events)
                                  :facts)
                       :kind)))
          (should (equal draft
                         (niwa-view-composer-draft-text nil t)))
          (should (= point-offset
                     (niwa-view-stream-test--draft-offset))))
        (niwa-view-stream-handle-tool-progress
         (list :type 'terminal :data-buffer data-buf
               :tool-use-id "ptc-live"))
        (with-current-buffer view-buf
          (should-not (gethash "ptc-live" niwa-view--execution-events))))))

  :doc "an unattended view coalesces tool progress without rebuilding history"
  (niwa-view-stream-test--with-buffers
    (let ((refreshed 0)
          (unattended t)
          (draft "> quoted\nsecond line"))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft draft 3))
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-view--unattended-p)
                     (lambda (&rest _) unattended))
                    ((symbol-function 'niwa-view--refresh-tool-row)
                     (lambda (&rest _) (cl-incf refreshed) t)))
            (niwa-view-stream-handle-tool-progress
             (list :type 'progress :data-buffer data-buf
                   :tool-use-id "bash-live"
                   :facts '(:kind bash) :output-tail "line"))
            (with-current-buffer view-buf
              (should (= 0 refreshed))
              (should (gethash "bash-live" niwa-view--execution-events))
              (should (eq 'tools niwa-view--pending-render-kind))
              (should (eq data-buf niwa-view--pending-render-data-buffer))
              (should (equal draft
                             (niwa-view-composer-draft-text nil t)))
              (niwa-view--schedule-render 'tools data-buf 0)
              (should-not niwa-view--render-timer))
            (dotimes (_ 3)
              (niwa-view-stream-handle-tool-progress
               (list :type 'progress :data-buffer data-buf
                     :tool-use-id "bash-live"
                     :facts '(:kind bash) :output-tail "latest line")))
            (with-current-buffer view-buf
              (should (equal '("bash-live") niwa-view--pending-tool-rows))
              (should (equal "latest line"
                             (plist-get (gethash "bash-live"
                                                 niwa-view--execution-events)
                                        :output-tail))))
            (niwa-view-stream-handle-tool-progress
             (list :type 'terminal :data-buffer data-buf
                   :tool-use-id "bash-live"))
            (with-current-buffer view-buf
              (should-not (gethash "bash-live" niwa-view--execution-events))
              (should (equal '("bash-live") niwa-view--pending-tool-rows)))
            ;; A later turn settling before the view is seen projects only
            ;; its own range; the older row still needs its refresh.
            (with-current-buffer data-buf
              (niwa-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (should (equal '("bash-live") niwa-view--pending-tool-rows))
              (should (eq 'tools niwa-view--pending-render-kind))
              (should-not niwa-view--render-timer))
            (setq unattended nil)
            (niwa-view--resume-attended-views)
            (with-current-buffer view-buf
              (should (niwa--timer-pending-p niwa-view--render-timer))
              (niwa-view--schedule-render 'tools data-buf 0)
              (should (= 1 refreshed))
              (should-not niwa-view--pending-render-kind)
              (should (equal draft (niwa-view-composer-draft-text)))
              (should (= 3 (niwa-view-stream-test--draft-offset)))))
        (with-current-buffer view-buf
          (niwa-view--cancel-scheduled-render)))))

  :doc "retains progress without recovery redraws until its source row exists"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((draft "> quoted\nsecond line")
            (event (list :type 'progress :data-buffer data-buf
                         :tool-use-id "new-call/0/1" :facts '(:kind bash)
                         :output-tail "retained output")))
        (with-current-buffer view-buf
          (niwa-view-test--insert-composer-draft draft 3)
          (niwa-view-stream-set-in-flight-turn-start (point-min))
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf (copy-marker (point-min)))))
        (unwind-protect
            (progn
              (dotimes (_ 3) (niwa-view-stream-handle-tool-progress event))
              (with-current-buffer view-buf
                (should-not niwa-view--render-timer)
                (should (equal "retained output"
                               (plist-get (gethash "new-call/0/1"
                                                   niwa-view--execution-events)
                                          :output-tail)))
                (should (equal draft (niwa-view-composer-draft-text)))
                (should (= 3 (niwa-view-stream-test--draft-offset))))
              ;; Once its outer ToolCall source arrives, the missing view row
              ;; is recoverable and must still get an incremental projection.
              (niwa-view-stream-test--insert-data
               data-buf "tool source" '(tool . "new-call"))
              (with-current-buffer data-buf
                (save-restriction
                  (narrow-to-region (point-min) (point-min))
                  (niwa-view-stream-handle-tool-progress event)
                  (should (= (point-min) (point-max)))))
              (with-current-buffer view-buf
                (should (niwa--timer-pending-p niwa-view--render-timer))
                (should (eq 'incremental niwa-view--pending-render-kind))))
          (with-current-buffer view-buf
            (niwa-view--cancel-scheduled-render)))))))

(niwa-deftest niwa-view-stream-nested-bash-progress ()
  ,test (test)
  :doc "a nested Bash event refreshes its source-owning row and preserves an open draft"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry))
          (draft "> quoted\nsecond line"))
      (niwa-tool-register
       (niwa-tool--create :name "ToolCall" :category "niwa"
                             :renderer #'niwa-tool-ptc--render))
      (niwa-tool-register
       (niwa-tool--create :name "Bash" :category "niwa"
                             :renderer #'niwa-tool-exec--render-bash))
      (niwa-view-stream-test--insert-data
       data-buf
       (concat
        "(:name \"ToolCall\" :args (:expression \"(Bash :command \\\"sleep 10\\\")\"))\n\nOLD OUTPUT"
        (niwa-tool-render-data-format
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
          (niwa-view--insert-rendered-tool
           (niwa-view--render-tool-call
            (niwa-view--tool-call-parse
             data-buf (car source) (cdr source)) data-buf)
           source))
        (goto-char (point-min))
        (should (search-forward "Bash: sleep 10" nil t))
        (niwa-view-toggle-section)
        (should (string-match-p "OLD OUTPUT" (buffer-string)))
        (niwa-view-test--insert-composer-draft draft 3))
      (niwa-view-stream-handle-tool-progress
       (list :type 'progress :data-buffer data-buf :tool-use-id "outer/1"
             :facts '(:execution-id "exec-1" :command "sleep 10"
                      :state running :wall-time-seconds 7)
             :output-tail "NEW OUTPUT"))
      (with-current-buffer view-buf
        (should (string-match-p "Bash: sleep 10 · running · 7s"
                                (buffer-string)))
        (should (string-match-p "NEW OUTPUT" (buffer-string)))
        (should-not (string-match-p "OLD OUTPUT" (buffer-string)))
        (should (equal draft (niwa-view-composer-draft-text)))
        (should (= 3 (niwa-view-stream-test--draft-offset)))
        (cl-letf (((symbol-function 'niwa-view-audit--evidence)
                   (lambda (&rest _) (ert-fail "Source-backed Bash row missing"))))
          (niwa-view-audit-show-control-result "exec-1")
          (should (looking-at-p ".*Bash: sleep 10"))
          (should (equal draft (niwa-view-composer-draft-text))))))
  :doc "compound ToolCall refresh replaces its expanded child instead of duplicating it"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry))
          (draft "> quoted\nsecond line"))
      (niwa-tool-register
       (niwa-tool--create :name "ToolCall" :category "niwa"
                             :renderer #'niwa-tool-ptc--render))
      (niwa-tool-register
       (niwa-tool--create :name "Bash" :category "niwa"
                             :renderer #'niwa-tool-exec--render-bash))
      (niwa-view-stream-test--insert-data
       data-buf
       (concat
        "(:name \"ToolCall\" :args (:expression \"(list (Bash :command \\\"sleep 10\\\"))\"))\n\nreturned"
        (niwa-tool-render-data-format
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
          (niwa-view--insert-rendered-tool
           (niwa-view--render-tool-call
            (niwa-view--tool-call-parse
             data-buf (car source) (cdr source)) data-buf)
           source))
        (goto-char (point-min))
        (niwa-view-toggle-section)
        (goto-char (text-property-any (point-min) (point-max)
                                      'niwa-view-type 'tool-child))
        (niwa-view-render-toggle-child-call)
        (should (= 1 (how-many "Bash: sleep 10" (point-min) (point-max))))
        (niwa-view-test--insert-composer-draft draft 3))
      (dolist (output '("NEW OUTPUT" "LATEST OUTPUT"))
        (niwa-view-stream-handle-tool-progress
         (list :type 'progress :data-buffer data-buf :tool-use-id "outer/1"
               :facts '(:execution-id "exec-1" :state running
                        :wall-time-seconds 7)
               :output-tail output)))
      (with-current-buffer view-buf
        (should (= 1 (how-many "Bash: sleep 10" (point-min) (point-max))))
        (should (= 1 (how-many "LATEST OUTPUT" (point-min) (point-max))))
        (should-not (string-match-p "OLD OUTPUT\\|NEW OUTPUT" (buffer-string)))
        (should (equal draft (niwa-view-composer-draft-text)))
        (should (= 3 (niwa-view-stream-test--draft-offset))))))))

(niwa-deftest niwa-view--spinner-tick ()
  ,test
  (test)
  :doc "an owned elapsed callback cannot requeue itself during mouse tracking"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq niwa-view--spinner-last-second nil)
      (let ((niwa-view-render--owner t)
            (track-mouse 'drag-tracking))
        (niwa-view--spinner-tick))
      (should (numberp niwa-view--spinner-last-second))
      (should-not niwa-view-render--pending)))
  :doc "progress ticks preserve selection without reactivating mark hooks"
  (dolist (active '(nil t))
    (niwa-view-stream-test--with-buffers
      (with-current-buffer view-buf
        (niwa-view--start-spinner "Working...")
        (niwa-view-test--insert-composer-draft
         "> quoted\nsecond line" 3)
        (let ((start (niwa-view-composer-draft-start)))
          (with-current-buffer (niwa-view-composer-buffer)
            (setq-local transient-mark-mode t)
            (set-mark (+ start 8))
            (setq mark-active active deactivate-mark nil)
            (let* ((activations 0)
                   (activate-mark-hook (list (lambda () (cl-incf activations)))))
              (with-current-buffer view-buf
                (cl-letf (((symbol-function 'niwa-view--unattended-p)
                           (lambda (&rest _) nil)))
                  (dotimes (_ 3) (niwa-view--spinner-tick))
                  (niwa-view--update-spinner "Checking completed work...")))
              (should (= activations 0))
              (should (eq active mark-active))
              (should-not deactivate-mark)
              (should (= 8 (- (mark) (niwa-view-composer-draft-start view-buf))))))
          (should (= 3 (niwa-view-stream-test--draft-offset)))
          (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text))))
        (niwa-view--stop-spinner))))

  :doc "an unattended tick leaves text, properties, and the composer untouched"
  (niwa-view-stream-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (with-current-buffer view-buf
        (niwa-view--start-spinner "Working...")
        (niwa-view-test--insert-composer-draft draft 3)
        (let* ((before (buffer-string))
               (point-before (point))
               (draft-before (niwa-view-composer-draft-text))
               (draft-offset (niwa-view-stream-test--draft-offset))
               (frame (niwa-view-stream-test--rail-activity)))
          (cl-letf (((symbol-function 'niwa-view--unattended-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'niwa-view--animation-seconds)
                     (lambda () 0.48)))
            (niwa-view--spinner-tick))
          (should (equal-including-properties before (buffer-string)))
          (should (= point-before (point)))
          (should (equal frame (niwa-view-stream-test--rail-activity)))
          (should (= draft-offset (niwa-view-stream-test--draft-offset)))
          (should (equal-including-properties
                   draft-before (niwa-view-composer-draft-text)))
          (should (equal draft
                         (niwa-view-composer-draft-text nil t))))
        (niwa-view--stop-spinner))))

  :doc "a visible attended tick samples elapsed time instead of an index"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full))
        (niwa-view--start-spinner "Working...")
        (let ((initial (niwa-view-stream-test--rail-activity)))
          (should (equal "- Working..." initial))
          (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                     (lambda () 0.24)))
            (niwa-view--spinner-tick))
          (should (equal "\\ Working..." (niwa-view-stream-test--rail-activity))))
        (niwa-view--stop-spinner))))

  :doc "visible high-frequency frames touch only registered display spans"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-tool-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full)
            (niwa-view--pending-tool-calls
             '(("call-1" "Read" nil))))
        (niwa-view--start-spinner "Working...")
        (niwa-view-stream-test--draw-running-rows)
        (niwa-view-test--insert-composer-draft
         "> quoted\nsecond line" 3)
        (let* ((tool (text-property-any (point-min) (point-max)
                                        'niwa-view-inline-spinner-frame t))
               (before (buffer-substring-no-properties
                        (point-min) (point-max)))
               (point-before (point))
               (draft-offset (niwa-view-stream-test--draft-offset))
               (undo-before buffer-undo-list)
               (modified-before (buffer-modified-p))
               (niwa-view--spinner-last-second (floor (float-time)))
               (original-framerate
                (symbol-function 'niwa-view-power-framerate))
               (rates 0))
          (should (niwa-view-stream-test--rail-activity 'niwa-view-spinner-frame))
          (should tool)
          (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                     (lambda () 0.24))
                    ((symbol-function 'niwa-view-power-framerate)
                     (lambda (&rest args)
                       (cl-incf rates)
                       (apply original-framerate args)))
                    ((symbol-function 'niwa-view-zone-reconcile)
                     (lambda (&rest _) (ert-fail "tick reconciled a zone")))
                    ((symbol-function 'niwa-view--spinner-display-status)
                     (lambda (&rest _) (ert-fail "tick rebuilt metadata")))
                    ((symbol-function 'niwa-view--insert-pending-run)
                     (lambda (&rest _) (ert-fail "tick rebuilt tool rows")))
                    ((symbol-function 'niwa-view-render-live-update)
                     (lambda (&rest _) (ert-fail "tick rendered the live turn")))
                    ((symbol-function 'niwa-view-animation--colors)
                     (lambda (&rest _) (ert-fail "tick resolved colors")))
                    ((symbol-function 'niwa-view-power--current-state)
                     (lambda (&rest _) (ert-fail "tick queried power")))
                    ((symbol-function 'niwa-view-composer-draft-text)
                     (lambda (&rest _) (ert-fail "tick copied the draft"))))
            (niwa-view--spinner-tick))
          (should (> rates 0))
          (should (equal "\\ Working..." (niwa-view-stream-test--rail-activity)))
          (should (equal "\\ " (get-text-property tool 'display)))
          (should (equal before (buffer-substring-no-properties
                                 (point-min) (point-max))))
          (should (= point-before (point)))
          (should (= draft-offset (niwa-view-stream-test--draft-offset)))
          (should (eq undo-before buffer-undo-list))
          (should (eq modified-before (buffer-modified-p)))
          (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text))))))))

(niwa-deftest niwa-view-stream-ensure-progress-for-fsm ()
  ,test
  (test)
  :doc "direct top-level FSM anchors both sides and requests progress"
  (niwa-view-stream-test--with-buffers
    (let* ((position (with-current-buffer data-buf
                       (copy-marker (point-max) nil)))
           (fsm (gptel-make-fsm
                 :info (list :buffer data-buf :position position)))
           ensured)
      (cl-letf (((symbol-function 'niwa-view--agent-fsm-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'niwa-view--ensure-request-progress)
                 (lambda (buffer &optional _status)
                   (setq ensured buffer))))
        (niwa-view-stream-ensure-progress-for-fsm fsm))
      (with-current-buffer view-buf
        (should (eq ensured data-buf))
        (should (markerp niwa-view--data-turn-start))
        (should (= (marker-position niwa-view--data-turn-start)
                   (marker-position position)))
        (should (markerp niwa-view--in-flight-turn-start))))))

;;
;;; Pending tool lifecycle

(niwa-deftest niwa-view--pending-tool-fingerprint
  (:doc "keys pending tool calls by the arguments gptel actually sends")
  ,test
  (test)
  :doc "fingerprints the tool name and its arguments"
  (should (equal '("Read" (:file_path . "a"))
                 (niwa-view--pending-tool-fingerprint
                  '(:name "Read" :args (:file_path "a")))))

  :doc "different arguments fingerprint differently"
  (should-not (equal
               (niwa-view--pending-tool-fingerprint
                '(:name "Read" :args (:file_path "a")))
               (niwa-view--pending-tool-fingerprint
                '(:name "Read" :args (:file_path "b")))))

  :doc "identical calls share a fingerprint"
  ;; gptel passes no call id, so two identical parallel calls cannot be
  ;; told apart here; the pending list keeps one entry per call instead.
  (should (equal
           (niwa-view--pending-tool-fingerprint
            '(:name "Read" :args (:file_path "a")))
           (niwa-view--pending-tool-fingerprint
            '(:name "Read" :args (:file_path "a")))))

  :doc "the hook's model plist and the pipeline's spec-order plist agree"
  (should (equal
           (niwa-view--pending-tool-fingerprint
            '(:name "Read" :args (:limit 5 :file_path "a")))
           (niwa-view--pending-tool-fingerprint
            '(:name "Read" :args (:file_path "a" :offset nil :limit 5)))))

  :doc "a non-nil false value still distinguishes calls"
  (should-not (equal
               (niwa-view--pending-tool-fingerprint
                '(:name "Grep" :args (:pattern "x" :multiline :json-false)))
               (niwa-view--pending-tool-fingerprint
                '(:name "Grep" :args (:pattern "x")))))

  :doc "arguments past the printer's limits still distinguish calls"
  (should-not (equal
               (niwa-view--pending-tool-fingerprint
                `(:name "Edit" :args (:edits ,(append (make-list 40 1) '(a)))))
               (niwa-view--pending-tool-fingerprint
                `(:name "Edit" :args (:edits ,(append (make-list 40 1) '(b))))))))

(niwa-deftest niwa-view--routine-pending-poll-p ()
  ,test
  (test)
  :doc "only empty-input WriteStdin observations are routine pending polls"
  (should (niwa-view--routine-pending-poll-p
           '(:name "WriteStdin" :args (:execution_id "exec-1" :chars ""))))
  (should (niwa-view--routine-pending-poll-p
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
    (should-not (niwa-view--routine-pending-poll-p
                 (list :name "WriteStdin" :args args))))
  (should (niwa-view--routine-pending-poll-p
           '(:name "ToolCall"
             :args (:expression "(WriteStdin :execution_id \"exec-1\" :chars \"\" :yield_time_ms 5000)"))))
  (should (niwa-view--routine-pending-poll-p
           '(:name "ToolCall"
             :args (:expression "(WriteStdin :execution_id \"exec-1\")"))))
  (should-not (niwa-view--routine-pending-poll-p
               '(:name "ToolCall"
                 :args (:expression "(WriteStdin :execution_id \"exec-1\" :chars \"\")"
                        :bogus 1))))
  (should-not (niwa-view--routine-pending-poll-p
               '(:name "WriteStdin"
                 :args (:execution_id "exec-1" :chars "\C-c"))))
  (dolist (script '("(WriteStdin :execution_id \"exec-1\" :chars \"yes\\n\")"
                    "(WriteStdin :execution_id \"exec-1\" :chars nil)"
                    "(WriteStdin :execution_id \"exec-1\" :chars \"\") (Read :file_path \"a\")"
                    "(WriteStdin :execution_id \"exec-1\" :chars \"\" :chars \"\")"
                    "(WriteStdin :execution_id \"exec-1\" :chars \"\" :yield_time_ms (identity 5000))"
                    "(WriteStdin :execution_id \"exec-1\" :chars \"\" :yield_time_ms 0)"
                    "(WriteStdin :execution_id \"exec-1\" :chars \"\""))
    (should-not (niwa-view--routine-pending-poll-p
                 (list :name "ToolCall" :args (list :expression script)))))
  (should-not (niwa-view--routine-pending-poll-p
               '(:name "StopExecution" :args (:execution_id "exec-1")))))

(niwa-deftest niwa-view-stream-spinner-hook ()
  ,test
  (test)
  :doc "routine polls do not change the activity label before an in-flight turn"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (niwa-view-stream-spinner-hook
       '(:name "WriteStdin" :args (:execution_id "exec-1" :chars "")))
      (niwa-view-stream-spinner-hook
       '(:name "ToolCall"
         :args (:expression "(WriteStdin :execution_id \"exec-1\" :chars \"\")"))))
    (with-current-buffer view-buf
      (should-not (string-search "Calling WriteStdin" (niwa-view-test--rail-text)))
      (should-not (string-search "Calling ToolCall" (niwa-view-test--rail-text))))
    (with-current-buffer data-buf
      (niwa-view-stream-spinner-hook
       '(:name "WriteStdin"
         :args (:execution_id "exec-1" :chars "yes\n"))))
    (with-current-buffer view-buf
      (should (string-search "Calling WriteStdin" (niwa-view-test--rail-text))))))


(niwa-deftest niwa-view--pending-tool-calls
  (:doc "tracks and renders the pending-tool live tail")
  ,test
  (test)

  :doc "a running tool row rejects typing at every position"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min))))
        (niwa-view-test--insert-composer-draft "> keep\nthis" 3))
      (with-current-buffer data-buf
        (niwa-view-stream-pre-tool
         '(:name "Bash" :args (:command "cd backend && cargo add polars"
                               :description "Add polars"))))
      (with-current-buffer view-buf
        (let* ((text (buffer-string))
               (row (string-match "● Bash: " text))
               (row-start (and row (+ (point-min) row
                                      (- (length (match-string 0 text))
                                         (length "● Bash: ")))))
               (row-end (and row-start
                             (save-excursion
                               (goto-char row-start)
                               (line-end-position)))))
          (should row-start)
          (let ((before (buffer-string)))
            (cl-loop for pos from (save-excursion
                                    (goto-char row-start)
                                    (line-beginning-position))
                     to row-end
                     do (goto-char pos)
                     (should (eq 'text-read-only
                                 (car (condition-case err
                                          (progn (insert "x") '(inserted))
                                        (text-read-only err))))))
            (should (equal before (buffer-string))))
          (should (equal "> keep\nthis" (niwa-view-composer-draft-text)))))))

  :doc "routine polls show no pending row while input and stops stay visible"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (progn
        (with-current-buffer data-buf
          (dolist (poll '((:name "WriteStdin"
                          :args (:execution_id "exec-1" :chars ""))
                         (:name "WriteStdin"
                          :args (:execution_id "exec-2"))
                         (:name "ToolCall"
                          :args (:expression "(WriteStdin :execution_id \"exec-3\" :chars \"\")"))))
            (niwa-view-stream-spinner-hook poll)
            (niwa-view-stream-pre-tool poll))
          (with-current-buffer view-buf
            (should-not niwa-view--pending-tool-calls)
            (should-not (string-match-p "● WriteStdin" (buffer-string)))
            (should-not (string-match-p "● ToolCall" (buffer-string)))
            (should (niwa-view-stream-test--rail-activity 'niwa-view-spinner-frame)))
          (niwa-view-stream-pre-tool
           '(:name "WriteStdin" :args (:execution_id "exec-1"
                                         :chars "yes\n")))
          (niwa-view-stream-pre-tool
           '(:name "StopExecution" :args (:execution_id "exec-2")))
          (with-current-buffer view-buf
            (should (= 2 (length niwa-view--pending-tool-calls)))
            (should (string-match-p "● WriteStdin" (buffer-string)))
            (should (string-match-p "● StopExecution" (buffer-string))))
          (niwa-view-stream-post-tool
           '(:name "WriteStdin" :args (:execution_id "exec-1" :chars "")))
          (with-current-buffer view-buf
            (should (= 2 (length niwa-view--pending-tool-calls))))))))

  :doc "invalid empty-input controls keep pending error indicators"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq niwa-view--in-flight-turn-start
            (copy-marker (niwa-view--history-insertion-marker)))
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min)))))
    (let ((niwa-view-tool-boundary-render-delay 0))
     (with-current-buffer data-buf
      (niwa-view-stream-pre-tool
       '(:name "WriteStdin" :args (:execution_id "" :chars "")))
      (niwa-view-stream-pre-tool
       '(:name "WriteStdin"
         :args (:execution_id "exec-1" :chars ""
                :yield_time_ms "not-a-duration")))
      (niwa-view-stream-pre-tool
       '(:name "WriteStdin"
         :args (:execution_id "exec-1" :chars "" :bogus 1)))
      (niwa-view-stream-pre-tool
       '(:name "ToolCall"
         :args (:expression "(WriteStdin :execution_id \"exec-1\" :chars \"\")"
                :bogus 1)))))
    (with-current-buffer view-buf
      (should (= 4 (length niwa-view--pending-tool-calls)))
      ;; Four running calls exceed the grouping threshold: one row.
      (should (string-match-p "WriteStdin ×3, ToolCall · 4 running"
                              (buffer-string)))))

  :doc "pre/post hooks add and remove entries for the arguments gptel sends"
  ;; gptel builds its tool-hook arguments from the name, the arguments,
  ;; and the result, so there is no call id to key on.
  (niwa-view-stream-test--with-buffers
    (let ((render-count 0)
          (niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'niwa-view-render-live-update)
                 (lambda (&rest _) (cl-incf render-count))))
        (with-current-buffer data-buf
          (should-not
           (niwa-view-stream-pre-tool
            '(:name "Read" :args (:file_path "a"))))
          (should-not
           (niwa-view-stream-pre-tool
            '(:name "Grep" :args (:pattern "x"))))
          (with-current-buffer view-buf
            (should (equal '(((("Read" (:file_path . "a")) . 1) "Read" "a")
                             ((("Grep" (:pattern . "x")) . 2) "Grep" "x"))
                           niwa-view--pending-tool-calls)))
          (should-not
           (niwa-view-stream-post-tool
            '(:name "Read" :args (:file_path "a"))))))
      (with-current-buffer view-buf
        (should (equal '(((("Grep" (:pattern . "x")) . 2) "Grep" "x"))
                       niwa-view--pending-tool-calls)))
      (should (= 3 render-count))))

  :doc "identical parallel calls each keep their own live line"
  ;; Two identical calls share a fingerprint, so collapsing them dropped
  ;; the line as soon as the first one finished, while the second was
  ;; still running.
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'niwa-view-render-live-update) #'ignore))
        (with-current-buffer data-buf
          (dotimes (_ 2)
            (niwa-view-stream-pre-tool
             '(:name "Read" :args (:file_path "a"))))
          (with-current-buffer view-buf
            (should (= 2 (length niwa-view--pending-tool-calls))))
          (niwa-view-stream-post-tool
           '(:name "Read" :args (:file_path "a")))
          (with-current-buffer view-buf
            (should (= 1 (length niwa-view--pending-tool-calls))))
          (niwa-view-stream-post-tool
           '(:name "Read" :args (:file_path "a")))
          (with-current-buffer view-buf
            (should-not niwa-view--pending-tool-calls))))))

  :doc "tool-boundary renders are debounced and coalesced"
  (niwa-view-stream-test--with-buffers
    (let ((render-count 0)
          (niwa-view-tool-boundary-render-delay 0.02))
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'niwa-view-render-live-update)
                 (lambda (&rest _) (cl-incf render-count))))
        (with-current-buffer data-buf
          (should-not
           (niwa-view-stream-pre-tool
            '(:name "Read" :args (:file_path "a"))))
          (should-not
           (niwa-view-stream-pre-tool
            '(:name "Grep" :args (:pattern "x")))))
        (should (= 0 render-count))
        ;; Debouncing/coalescing is asserted by the counts, not wall latency.
        (niwa-test--await 5 "tool-boundary render settles" (> render-count 0))
        (should (= 1 render-count)))))

  :doc "immediate pending live tail inserts above status-zone content"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position niwa-view--status-marker)))
          (goto-char status-start)
          (insert "TASK STATUS\n")
          (set-marker niwa-view--status-marker status-start)
          (set-marker niwa-view--interaction-marker (point))
          (set-marker (niwa-view-transcript-tail-marker) (point))
          (setq niwa-view--in-flight-turn-start
                (copy-marker niwa-view--status-marker))
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (should-not
               (niwa-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (calling (string-match-p "● Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp calling))
                (should (numberp status))
                (should (< calling status))
                (should-not (string-match-p "Calling" text)))))
        (with-current-buffer view-buf
          (niwa-view--cancel-scheduled-render)))))

  :doc "pending live tail recovers above status-zone content when status marker detaches"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position niwa-view--status-marker)))
          (goto-char status-start)
          (insert "TASK STATUS\n")
          (set-marker niwa-view--status-marker nil)
          (set-marker niwa-view--interaction-marker (point))
          (set-marker (niwa-view-transcript-tail-marker) (point))
          (setq niwa-view--in-flight-turn-start
                (copy-marker (niwa-view-transcript-tail)))
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (should-not
               (niwa-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (calling (string-match-p "● Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp calling))
                (should (numberp status))
                (should (< calling status))
                (should-not (string-match-p "Calling" text)))))
        (with-current-buffer view-buf
          (niwa-view--cancel-scheduled-render)))))

  :doc "pending live tail stays above status-zone content after immediate render"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position niwa-view--status-marker)))
          (goto-char status-start)
          (insert "TASK STATUS\n")
          (set-marker niwa-view--status-marker nil)
          (set-marker niwa-view--interaction-marker (point))
          (set-marker (niwa-view-transcript-tail-marker) (point))
          (setq niwa-view--in-flight-turn-start
                (copy-marker (niwa-view-transcript-tail)))
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (should-not
               (niwa-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (calling (string-match-p "● Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp calling))
                (should (numberp status))
                (should (< calling status))
                (should (= 1 (cl-loop with start = 0
                                      while (string-match "● Read: a"
                                                          text start)
                                      count t
                                      do (setq start (match-end 0))))))))
        (with-current-buffer view-buf
          (niwa-view--cancel-scheduled-render)))))

  :doc "pending live tail recovers after existing history when status marker detaches"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (niwa-view--insert-user-message "Previous turn")
          (goto-char (marker-position niwa-view--status-marker))
          (insert "TASK STATUS\n")
          (set-marker niwa-view--status-marker nil)
          (set-marker niwa-view--interaction-marker (point))
          (set-marker (niwa-view-transcript-tail-marker) (point))
          (setq niwa-view--in-flight-turn-start
                (copy-marker (niwa-view-transcript-tail)))
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (should-not
               (niwa-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "● Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should-not (string-match-p "Calling" text)))))
        (with-current-buffer view-buf
          (niwa-view--cancel-scheduled-render)))))

  :doc "a running row follows the live assistant text it answers"
  (niwa-view-stream-test--with-buffers
    ;; The live render owns the in-flight turn, so its text comes from the
    ;; transcript, not from text placed in the view.
    (niwa-view-stream-test--insert-data
     data-buf "Existing response\n" 'response)
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position niwa-view--status-marker)))
          (goto-char status-start)
          (setq niwa-view--in-flight-turn-start (copy-marker (point)))
          (insert "TASK STATUS\n")
          (set-marker niwa-view--status-marker nil)
          (set-marker niwa-view--interaction-marker (point))
          (set-marker (niwa-view-transcript-tail-marker) (point))
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (should-not
               (niwa-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (existing (string-match-p "Existing response" text))
                     (calling (string-match-p "● Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp existing))
                (should (numberp calling))
                (should (numberp status))
                (should (< existing calling))
                (should (< calling status))
                (should-not (string-match-p "Calling" text)))))
        (with-current-buffer view-buf
          (niwa-view--cancel-scheduled-render)))))

  :doc "pending live tail stays above propertized status rows"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (niwa-view--insert-user-message "Previous turn")
          (goto-char (marker-position niwa-view--status-marker))
          (insert (propertize "AGENT STATUS\n"
                              'niwa-view-type 'agent-handle
                              'niwa-view-agent-status t))
          (set-marker niwa-view--status-marker nil)
          (set-marker niwa-view--interaction-marker (point))
          (set-marker (niwa-view-transcript-tail-marker) (point))
          (setq niwa-view--in-flight-turn-start
                (copy-marker (niwa-view-transcript-tail)))
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (should-not
               (niwa-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "● Read: a" text))
                     (status (string-match-p "AGENT STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should-not (string-match-p "Calling" text)))))
        (with-current-buffer view-buf
          (niwa-view--cancel-scheduled-render)))))

  :doc "pending live tail ignores stale attached status marker"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (niwa-view--insert-user-message "Previous turn")
          (goto-char (marker-position niwa-view--status-marker))
          (insert "TASK STATUS\n")
          (set-marker niwa-view--status-marker (point-min))
          (set-marker niwa-view--interaction-marker (point))
          (set-marker (niwa-view-transcript-tail-marker) (point))
          (setq niwa-view--in-flight-turn-start
                (copy-marker (niwa-view-transcript-tail)))
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (should-not
               (niwa-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (header (string-match-p "niwa" text))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "● Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp header))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< header previous))
                (should (< previous calling))
                (should (< calling status))
                (should-not (string-match-p "Calling" text)))))
        (with-current-buffer view-buf
          (niwa-view--cancel-scheduled-render)))))

  :doc "pending live tail recovers after collapsed turn summaries"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position niwa-view--status-marker)))
          (goto-char status-start)
          (insert (propertize "Previous turn\n"
                              'niwa-view-type 'turn-summary))
          (insert "TASK STATUS\n")
          (set-marker niwa-view--status-marker nil)
          (set-marker niwa-view--interaction-marker (point))
          (set-marker (niwa-view-transcript-tail-marker) (point))
          (setq niwa-view--in-flight-turn-start
                (copy-marker (niwa-view-transcript-tail)))
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (should-not
               (niwa-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "● Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should-not (string-match-p "Calling" text)))))
        (with-current-buffer view-buf
          (niwa-view--cancel-scheduled-render)))))

  :doc "incremental render preserves status rows when status marker detaches"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position niwa-view--status-marker)))
        (goto-char status-start)
        (setq niwa-view--in-flight-turn-start (copy-marker (point)))
        (insert (propertize "Assistant\n"
                            'niwa-view-type 'turn-header
                            'niwa-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'niwa-view-type 'response
                            'niwa-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker niwa-view--status-marker nil)
        (set-marker niwa-view--interaction-marker (point))
        (set-marker (niwa-view-transcript-tail-marker) (point))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (setq niwa-view--pending-tool-calls
              (list (list "call-1" "Read" "a")))
        (should (progn (niwa-view-render-live-update data-buf) t))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (response (string-match-p "assistant text" text))
               (calling (string-match-p "● Read: a" text))
               (status (string-match-p "TASK STATUS" text)))
          (should (numberp response))
          (should (numberp calling))
          (should (numberp status))
          (should (< response calling))
          (should (< calling status))))))

  :doc "incremental render recovers stale in-flight marker"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position niwa-view--status-marker)))
        (goto-char status-start)
        (insert (propertize "Assistant\n"
                            'niwa-view-type 'turn-header
                            'niwa-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'niwa-view-type 'response
                            'niwa-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker niwa-view--status-marker nil)
        (set-marker niwa-view--interaction-marker (point))
        (set-marker (niwa-view-transcript-tail-marker) (point))
        (niwa-view-stream-set-in-flight-turn-start (point-min))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (setq niwa-view--pending-tool-calls
              (list (list "call-1" "Read" "a")))
        (should (progn (niwa-view-render-live-update data-buf) t))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (calling (string-match-p "● Read: a" text))
               (status (string-match-p "TASK STATUS" text)))
          (should-not old)
          (should (numberp response))
          (should (numberp calling))
          (should (numberp status))
          (should (< response calling))
          (should (< calling status))))))

  :doc "incremental render prefers source recovery over stale in-flight marker"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (history-start (niwa-view--after-header-position)))
        (goto-char (marker-position niwa-view--status-marker))
        (insert (propertize "Previous turn\n"
                            'niwa-view-type 'turn-summary))
        (insert (propertize "Assistant\n"
                            'niwa-view-type 'turn-header
                            'niwa-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'niwa-view-type 'response
                            'niwa-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker niwa-view--status-marker nil)
        (set-marker niwa-view--interaction-marker (point))
        (set-marker (niwa-view-transcript-tail-marker) (point))
        (niwa-view-stream-set-in-flight-turn-start history-start)
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (cl-letf (((symbol-function 'niwa-view--render-agent-status)
                   (lambda () nil))
                  ((symbol-function 'niwa-view--interaction-rebuild)
                   (lambda () nil)))
          (should (progn (niwa-view-render-live-update data-buf) t)))
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
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position niwa-view--status-marker)))
        (goto-char status-start)
        (insert (propertize "Assistant\n"
                            'niwa-view-type 'turn-header
                            'niwa-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'niwa-view-type 'response
                            'niwa-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker niwa-view--status-marker (point-min))
        (set-marker niwa-view--interaction-marker (point))
        (set-marker (niwa-view-transcript-tail-marker) (point))
        (niwa-view-stream-set-in-flight-turn-start (point-min))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (setq niwa-view--pending-tool-calls
              (list (list "call-1" "Read" "a")))
        (should (progn (niwa-view-render-live-update data-buf) t))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (header (string-match-p "niwa" text))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (calling (string-match-p "● Read: a" text))
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
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (history-start (niwa-view--after-header-position)))
        (goto-char (marker-position niwa-view--status-marker))
        (insert (propertize "Previous turn\n"
                            'niwa-view-type 'turn-summary))
        (setq niwa-view--in-flight-turn-start (copy-marker (point)))
        (insert (propertize "Assistant\n"
                            'niwa-view-type 'turn-header
                            'niwa-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'niwa-view-type 'response
                            'niwa-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker niwa-view--status-marker history-start)
        (set-marker niwa-view--interaction-marker (point))
        (set-marker (niwa-view-transcript-tail-marker) (point))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (cl-letf (((symbol-function 'niwa-view--render-agent-status)
                   (lambda () nil))
                  ((symbol-function 'niwa-view--interaction-rebuild)
                   (lambda () nil)))
          (should (progn (niwa-view-render-live-update data-buf) t)))
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
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position niwa-view--status-marker)))
        (goto-char status-start)
        (setq niwa-view--in-flight-turn-start (copy-marker (point)))
        (insert (propertize "Assistant\n"
                            'niwa-view-type 'turn-header
                            'niwa-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'niwa-view-type 'response
                            'niwa-view-source '(1 . 2)))
        (let ((row-start (point)))
          (insert (propertize "TASK STATUS\n"
                              'niwa-view-type 'agent-handle
                              'display "TASK STATUS"
                              'keymap niwa-view--display-map
                              'read-only t))
          (set-marker niwa-view--status-marker (+ row-start 5)))
        (set-marker niwa-view--interaction-marker (point))
        (set-marker (niwa-view-transcript-tail-marker) (point))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (cl-letf (((symbol-function 'niwa-view--render-agent-status)
                   (lambda () nil))
                  ((symbol-function 'niwa-view--interaction-rebuild)
                   (lambda () nil)))
          (should (progn (niwa-view-render-live-update data-buf) t)))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (status (string-match-p "TASK STATUS" text)))
          (should-not old)
          (should (numberp response))
          (should (numberp status))
          (should (< response status))))))

  :doc "incremental render restores aggregate status when status marker detaches"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((row '(:path "/root/verifier" :status running)))
        (cl-letf (((symbol-function 'niwa-view--agent-status-collect)
                   (lambda () (list row))))
          (niwa-view--render-agent-status)
          (let ((inhibit-read-only t))
            (goto-char (marker-position niwa-view--status-marker))
            (setq niwa-view--in-flight-turn-start (copy-marker (point)))
            (insert (propertize "Assistant\n"
                                'niwa-view-type 'turn-header
                                'niwa-view-turn-role 'assistant))
            (insert (propertize "old text\n"
                                'niwa-view-type 'response
                                'niwa-view-source '(1 . 2)))
            (set-marker niwa-view--status-marker nil)
            (setq niwa-view--data-turn-start
                  (with-current-buffer data-buf
                    (copy-marker (point-min)))))
          (should (progn (niwa-view-render-live-update data-buf) t))
          (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                 (old (string-match-p "old text" text))
                 (response (string-match-p "assistant text" text))
                 (status (string-match-p "Running   agent    /root/verifier" text)))
            (should-not old)
            (should (numberp response))
            (should (numberp status))
            (should (< response status))
            (should (< status (- (niwa-view-transcript-tail) (point-min)))))))))

  :doc "aggregate status stays below pending live tail with detached status marker"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((row '(:path "/root/verifier" :status running)))
        (cl-letf (((symbol-function 'niwa-view--agent-status-collect)
                   (lambda () (list row))))
          (let ((inhibit-read-only t))
            (goto-char (marker-position niwa-view--status-marker))
            (setq niwa-view--in-flight-turn-start (copy-marker (point)))
            (insert (propertize "Assistant\n"
                                'niwa-view-type 'turn-header
                                'niwa-view-turn-role 'assistant))
            (insert (propertize "old text\n"
                                'niwa-view-type 'response
                                'niwa-view-source '(1 . 2)))
            (set-marker niwa-view--status-marker nil)
            (set-marker niwa-view--interaction-marker (point))
            (set-marker (niwa-view-transcript-tail-marker) (point))
            (setq niwa-view--data-turn-start
                  (with-current-buffer data-buf
                    (copy-marker (point-min))))
            (setq niwa-view--pending-tool-calls
                  (list (list "call-1" "Read" "a"))))
          (should (progn (niwa-view-render-live-update data-buf) t))
          (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                 (old (string-match-p "old text" text))
                 (response (string-match-p "assistant text" text))
                 (calling (string-match-p "● Read: a" text))
                 (status (string-match-p "Running   agent    /root/verifier" text)))
            (should-not old)
            (should (numberp response))
            (should (numberp calling))
            (should (numberp status))
            (should (< response calling))
            (should (< calling status)))))))

  :doc "aggregate status ignores stale attached status marker"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((row '(:path "/root/verifier" :status running)))
        (cl-letf (((symbol-function 'niwa-view--agent-status-collect)
                   (lambda () (list row))))
          (let ((inhibit-read-only t))
            (goto-char (marker-position niwa-view--status-marker))
            (setq niwa-view--in-flight-turn-start (copy-marker (point)))
            (insert (propertize "Assistant\n"
                                'niwa-view-type 'turn-header
                                'niwa-view-turn-role 'assistant))
            (insert (propertize "old text\n"
                                'niwa-view-type 'response
                                'niwa-view-source '(1 . 2)))
            (insert "TASK STATUS\n")
            (set-marker niwa-view--status-marker (point-min))
            (set-marker niwa-view--interaction-marker (point))
            (set-marker (niwa-view-transcript-tail-marker) (point))
            (setq niwa-view--data-turn-start
                  (with-current-buffer data-buf
                    (copy-marker (point-min)))))
          (should (progn (niwa-view-render-live-update data-buf) t))
          (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                 (header (string-match-p "niwa" text))
                 (old (string-match-p "old text" text))
                 (response (string-match-p "assistant text" text))
                 (status (string-match-p "Running   agent    /root/verifier" text)))
            (should (numberp header))
            (should-not old)
            (should (numberp response))
            (should (numberp status))
            (should (< header response))
            (should (< response status)))))))

  :doc "pending live tail ignores a stale transcript tail marker"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (niwa-view--insert-user-message "Previous turn")
          (goto-char (marker-position niwa-view--status-marker))
          (insert "TASK STATUS\n")
          (set-marker niwa-view--status-marker nil)
          (set-marker (niwa-view-transcript-tail-marker) (point-min))
          (niwa-view-stream-set-in-flight-turn-start (point-min))
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (should-not
               (niwa-view-stream-pre-tool
                '(:name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "● Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should-not (string-match-p "Calling" text)))))
        (with-current-buffer view-buf
          (niwa-view--cancel-scheduled-render)))))

  :doc "tool hooks do not return rendered agent-status strings to gptel"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq niwa-view--in-flight-turn-start
            (copy-marker (niwa-view--history-insertion-marker)))
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min)))))
    (let ((niwa-view-tool-boundary-render-delay 0))
      (cl-letf (((symbol-function 'niwa-view-render-live-update)
                 (lambda (&rest _)
                   #(" ─── agents: 1 running [+] ─────────────────────────────────\n"
                     0 61 (font-lock-face niwa-view-zone-separator)))))
        (with-current-buffer data-buf
          (should-not
           (niwa-view-stream-pre-tool
            '(:name "Read" :args (:file_path "a"))))
          (should-not
           (niwa-view-stream-post-tool
            '(:name "Read" :args (:file_path "a"))))))))

  :doc "Agent pre-tool hook does not add a duplicate pending Calling Agent line"
  (niwa-view-stream-test--with-buffers
    (let ((render-count 0)
          (niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'niwa-view-render-live-update)
                 (lambda (&rest _) (cl-incf render-count))))
        (with-current-buffer data-buf
          (should-not
           (niwa-view-stream-pre-tool
            '(:id "call-1" :name "Agent"
              :args (:task_name "explore"
                     :message "Inspect the repository."))))))
      (with-current-buffer view-buf
        (should-not niwa-view--pending-tool-calls)
        (should (= 1 render-count)))))

  :doc "a parallel batch above the threshold runs and settles as one row"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (calls '((:name "Read" :args (:file_path "a"))
                   (:name "Read" :args (:file_path "b"))
                   (:name "Read" :args (:file_path "c"))
                   (:name "Grep" :args (:pattern "x"))
                   (:name "Grep" :args (:pattern "y")))))
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min))))
        (niwa-view-test--insert-composer-draft "> keep\nthis" 3)
        (goto-char (point-min)))
      (with-current-buffer data-buf
        (mapc #'niwa-view-stream-pre-tool calls))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should (= 1 (niwa-view-stream-test--count-substring
                        "5 calls: read 3 files, searched 2 patterns · 5 running" text)))
          (should-not (string-match-p "Read: a" text))))
      ;; gptel writes every result after the batch's post-tool hooks.
      (with-current-buffer data-buf
        (dolist (call calls)
          (niwa-view-stream-test--insert-data
           data-buf (format "%S\n\nresult\n" call)
           (cons 'tool (format "%s" (plist-get call :args)))))
        (mapc #'niwa-view-stream-post-tool calls))
      (with-current-buffer view-buf
        (let ((settled (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))
          (should (string-match-p "✓ 5 calls: read 3 files, searched 2 patterns" settled))
          (should-not (string-match-p "running" settled))
          (should-not (text-property-any (point-min) (niwa-view-transcript-tail)
                                         'niwa-view-pending-tool-live t))
          (should (equal "> keep\nthis" (niwa-view-composer-draft-text)))
          (should (= 3 (niwa-view-stream-test--draft-offset)))
          ;; The settled live row is what a cold projection draws.
          (niwa-view--full-rerender)
          (should (equal settled
                         (buffer-substring-no-properties
                          (point-min) (niwa-view-transcript-tail))))))))

  :doc "a finished call leaves the other parallel call running"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-max)))))
      (with-current-buffer data-buf
        (niwa-view-stream-pre-tool '(:name "Read" :args (:file_path "a")))
        (niwa-view-stream-pre-tool '(:name "Grep" :args (:pattern "x"))))
      (with-current-buffer view-buf
        (let ((text (buffer-string)))
          (should (string-match-p "● Read: a" text))
          (should (string-match-p "● Grep: x" text))))
      (with-current-buffer data-buf
        (niwa-view-stream-post-tool '(:name "Read" :args (:file_path "a"))))
      (with-current-buffer view-buf
        (let ((text (buffer-string)))
          (should-not (string-match-p "Read: a" text))
          (should (= 1 (niwa-view-stream-test--count-substring
                        "● Grep: x" text))))
        (should (equal '("Grep" "x") (cdar niwa-view--pending-tool-calls))))))

  :doc "repair audit redraw preserves a single-line composer and point"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-repair-audited-tool data-buf)
    (with-current-buffer view-buf
      (setq niwa-view--in-flight-turn-start
            (copy-marker (niwa-view--history-insertion-marker)))
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min))))
      (niwa-view-test--insert-composer-draft "keep drafting" 5))
    (let ((niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer data-buf
        (niwa-view-stream-post-tool
         '(:id "repair-call" :name "Collect"
               :args (:names ["alice"])))))
    (with-current-buffer view-buf
      (should (equal "keep drafting" (niwa-view-composer-draft-text)))
      (should (= 5 (niwa-view-stream-test--draft-offset)))
      (should (string-match-p
               "tool input repaired"
               (buffer-substring-no-properties
                (point-min) (niwa-view-transcript-tail))))))

  :doc "repair audit redraw preserves a multiline > composer and point"
  (niwa-view-stream-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (niwa-view-stream-test--insert-repair-audited-tool data-buf)
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min))))
        (niwa-view-test--insert-composer-draft draft 4))
      (let ((niwa-view-tool-boundary-render-delay 0))
        (with-current-buffer data-buf
          (niwa-view-stream-post-tool
           '(:id "repair-call" :name "Collect"
                 :args (:names ["alice"])))))
      (with-current-buffer view-buf
        (should (equal draft (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-stream-test--draft-offset)))
        (should (string-match-p
                 "tool input repaired"
                 (buffer-substring-no-properties
                  (point-min) (niwa-view-transcript-tail)))))))

  :doc "post-tool hook retires a running row when no replacement text is ready"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq niwa-view--in-flight-turn-start
            (copy-marker (niwa-view--history-insertion-marker)))
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (setq niwa-view--pending-tool-calls
            (list (list (niwa-view--pending-tool-claim-key
                         '(:name "Agent"
                           :args (:task_name "explore"
                                  :message "Inspect.")))
                        "Agent" "explorer")))
      (niwa-view-stream-test--draw-running-rows)
      (should (string-match-p "● Agent: explorer"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))
    (let ((niwa-view-tool-boundary-render-delay 0))
     (with-current-buffer data-buf
      (niwa-view-stream-post-tool
       '(:name "Agent"
         :args (:task_name "explore" :message "Inspect.")))))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should-not (string-match-p "Agent: explorer" text))
        (should-not niwa-view--pending-tool-calls))))

  :doc "final response render settles without running rows"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "final answer\n" 'response)
    (with-current-buffer view-buf
      (setq niwa-view--in-flight-turn-start
            (copy-marker (niwa-view--history-insertion-marker)))
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min))))
      (setq niwa-view--pending-tool-calls
            '(("call-1" "Agent" "explorer")))
      (niwa-view-stream-test--draw-running-rows))
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (string-match-p "final answer" text))
        (should-not (string-match-p "Agent: explorer" text))
        (should-not (text-property-any (point-min) (point-max)
                                       'niwa-view-pending-tool-live t))
        (should-not niwa-view--pending-tool-calls))))

  :doc "full rerender redraws the live turn with its running rows at once"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq niwa-view--pending-tool-calls
            '(("call-1" "Read" nil)))
      (let ((inhibit-read-only t)
            (tail-start (marker-position (niwa-view-transcript-tail-marker))))
        (goto-char (niwa-view-transcript-tail-marker))
        (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
        (insert "Assistant\n| Calling Read...\n")
        (setq niwa-view--in-flight-turn-start
              (copy-marker tail-start nil))
        (set-marker niwa-view--status-marker (point))
        (set-marker niwa-view--interaction-marker (point))
        (set-marker (niwa-view-transcript-tail-marker) (point))
        (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil))
      (niwa-view-stream-test--draw-running-rows)
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (should (string-match-p "● Read" (buffer-string)))
      (let ((niwa-view-tool-boundary-render-delay 60))
        (niwa-view--full-rerender)
        ;; No timer: the live render queued behind the projection already
        ;; replaced the view-only tail with the turn and its running row.
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (= 0 (niwa-view-stream-test--count-substring
                        "Calling Read" text)))
          (should (= 1 (niwa-view-stream-test--count-substring
                        "● Read" text)))
          (should (text-property-any (point-min) (point-max)
                                     'niwa-view-pending-tool-live t))))))

  :doc "incremental render preserves live tail when no replacement content is ready"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (start nil))
        (goto-char (niwa-view-transcript-tail-marker))
        (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
        (setq start (point))
        (insert "Assistant\n› Calling Read: niwa-pipeline.el...\n")
        (setq niwa-view--in-flight-turn-start (copy-marker start nil))
        (set-marker niwa-view--status-marker (point))
        (set-marker niwa-view--interaction-marker (point))
        (set-marker (niwa-view-transcript-tail-marker) (point))
        (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil))
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (setq niwa-view--pending-tool-calls nil)
      (niwa-view-render-live-update data-buf)
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (string-match-p "Calling Read: niwa-pipeline.el" text)))))

  :doc "explicit response bounds do not blank live tail without replacement content"
  (niwa-view-stream-test--with-buffers
    (let (start end)
      (with-current-buffer data-buf
        (setq start (point-max))
        (setq end (point-max)))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (tail-start nil))
          (goto-char (niwa-view-transcript-tail-marker))
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
          (setq tail-start (point))
          (insert "Assistant\n... Thinking... (1 lines)\nCalling Read...\n")
          (setq niwa-view--in-flight-turn-start
                (copy-marker tail-start nil))
          (set-marker niwa-view--status-marker (point))
          (set-marker niwa-view--interaction-marker (point))
          (set-marker (niwa-view-transcript-tail-marker) (point))
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-max))))
        (setq niwa-view--pending-tool-calls nil)
        (niwa-view-render-settle data-buf start end)
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p "Assistant" text))
          (should (string-match-p "Calling Read" text))))))

  :doc "incremental render keeps the progress row beside running calls"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq niwa-view--in-flight-turn-start
            (copy-marker (niwa-view--history-insertion-marker) nil))
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (niwa-view--start-spinner "Thinking...")
      (setq niwa-view--pending-tool-calls
            '(("call-1" "Read" nil)))
      (niwa-view-render-live-update data-buf)
      (should (niwa-view-stream-test--rail-activity 'niwa-view-spinner-frame))
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (string-search "Working" (niwa-view-test--rail-text)))
        (should (string-match-p "● Read" text)))))

  :doc "running calls are separate rows until the run outgrows the threshold"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3)
          (first '((:name "Read" :args (:file_path "a"))
                   (:name "Grep" :args (:pattern "x"))))
          (second '((:name "Read" :args (:file_path "b"))
                    (:name "Read" :args (:file_path "c")))))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (with-current-buffer view-buf (goto-char (point-min)))
      (niwa-view-stream-test--batch data-buf first)
      (let ((text (niwa-view-stream-test--history view-buf)))
        (should (string-match-p "● Read: a" text))
        (should (string-match-p "● Grep: x" text)))
      (niwa-view-stream-test--batch data-buf first t)
      (let ((text (niwa-view-stream-test--history view-buf)))
        (should (string-match-p "✓ Read: a" text))
        (should (string-match-p "✓ Grep: x" text))
        (should-not (string-match-p "●" text)))
      ;; Two settled and two running calls: four rows become one group.
      (niwa-view-stream-test--batch data-buf second)
      (let ((text (niwa-view-stream-test--history view-buf)))
        (should (= 1 (niwa-view-stream-test--count-substring
                      "● 4 calls: read 3 files, searched 1 pattern · 2 running" text)))
        (should-not (string-match-p "Read: a" text)))
      (niwa-view-stream-test--batch data-buf second t)
      (let ((text (niwa-view-stream-test--history view-buf)))
        (should (= 1 (niwa-view-stream-test--count-substring
                      "✓ 4 calls: read 3 files, searched 1 pattern" text)))
        (should-not (string-match-p "running" text)))))

  :doc "identical parallel calls each count as running"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (call '(:name "Read" :args (:file_path "a"))))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (niwa-view-stream-test--batch data-buf (list call call))
      (should (= 2 (niwa-view-stream-test--count-substring
                    "● Read: a" (niwa-view-stream-test--history view-buf))))
      (with-current-buffer data-buf
        (niwa-view-stream-post-tool call))
      (should (= 1 (niwa-view-stream-test--count-substring
                    "● Read: a" (niwa-view-stream-test--history view-buf))))))

  :doc "a run folding under the reader stays open and keeps the reader's row"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3)
          (calls '((:name "Read" :args (:file_path "a"))
                   (:name "Read" :args (:file_path "b"))
                   (:name "Read" :args (:file_path "c"))))
          (fourth '((:name "Read" :args (:file_path "d")))))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (dolist (call calls)
        (niwa-view-stream-test--batch data-buf (list call))
        (niwa-view-stream-test--batch data-buf (list call) t))
      (save-window-excursion
        (set-window-buffer (selected-window) view-buf)
        (with-current-buffer view-buf
          (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4)
          (goto-char (point-min))
          (search-forward "Read: c")
          (set-window-point (selected-window) (point)))
        (niwa-view-stream-test--batch data-buf fourth)
        (with-current-buffer view-buf
          (let ((text (niwa-view-stream-test--history view-buf)))
            (should (string-match-p "4 calls: read 4 files · 1 running" text))
            (should (string-match-p "Read: c" text)))
          (should (< (point) (niwa-view--history-insertion-marker)))
          (should (string-match-p
                   "Read: c"
                   (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
          (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
          ;; Leaving the group does not fold it under the next reader.
          (niwa-view-stream-test--leave-row-for-draft 4)
          (set-window-point (selected-window) (point)))
        (niwa-view-stream-test--batch data-buf fourth t)
        (let ((text (niwa-view-stream-test--history view-buf)))
          (should (string-match-p "✓ 4 calls: read 4 files" text))
          (should (string-match-p "Read: c" text))))))

  :doc "a running-only group keeps its visited member and remembers expansion"
  (dolist (leave-before-settlement '(nil t))
    (niwa-view-stream-test--with-buffers
      (let ((niwa-view-tool-boundary-render-delay 0)
            (niwa-view-tool-group-collapse-threshold 3)
            (calls '((:name "Read" :args (:file_path "a"))
                     (:name "Read" :args (:file_path "b"))
                     (:name "Read" :args (:file_path "c"))
                     (:name "Read" :args (:file_path "d")))))
        (niwa-view-stream-test--begin-live-turn view-buf data-buf)
        (when leave-before-settlement
          (niwa-view-stream-test--insert-data data-buf "Let me check.\n" 'response))
        (dolist (call (butlast calls))
          (niwa-view-stream-test--batch data-buf (list call))
          (niwa-view-stream-handle-pipeline-event
           (append (list :type 'started :data-buffer data-buf
                         :tool-use-id (plist-get (plist-get call :args) :file_path))
                   call)))
        (save-window-excursion
          (set-window-buffer (selected-window) view-buf)
          (with-current-buffer view-buf
            (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4)
            (goto-char (point-min))
            (search-forward "Read: c")
            (set-window-point (selected-window) (point)))
          (niwa-view-stream-test--batch data-buf (last calls))
          (with-current-buffer view-buf
            (should (string-match-p "4 calls: read 4 files · 4 running"
                                    (niwa-view-stream-test--history view-buf)))
            (should (string-suffix-p
                     "Read: c" (buffer-substring-no-properties
                                (line-beginning-position) (point))))
            (should (equal "c" (get-text-property
                                (point) 'niwa-view-pending-tool-use-id)))
            (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
            (when leave-before-settlement
              (niwa-view-stream-test--leave-row-for-draft 4)
              (set-window-point (selected-window) (point))))
          ;; A second render after leaving must not hide the running rows.
          (niwa-view-render-live-update data-buf)
          (should (string-match-p "Read: c"
                                  (niwa-view-stream-test--history view-buf)))
          ;; Results use the pipeline id in their gptel property, as gptel does.
          ;; Debouncing removes every pending entry before the next redraw.
          (let ((niwa-view-tool-boundary-render-delay 60))
            (dolist (call calls)
              (let ((id (plist-get (plist-get call :args) :file_path)))
                (when leave-before-settlement
                  (niwa-view-stream-test--insert-data
                   data-buf (concat (when (equal id "a") "\n\n")
                                    "#+begin_tool Read\n") nil))
                (niwa-view-stream-test--insert-data
                 data-buf (format "%S\n\nresult\n" call) (cons 'tool id))
                (when leave-before-settlement
                  (niwa-view-stream-test--insert-data data-buf "#+end_tool\n" nil))
                (with-current-buffer data-buf
                  (niwa-view-stream-post-tool
                   (if (equal id "d") call
                     (append (list :tool-use-id id) call)))))))
          (niwa-view--flush-scheduled-render view-buf t)
          (with-current-buffer view-buf
            (should (string-match-p "✓ 4 calls: read 4 files"
                                    (niwa-view-stream-test--history view-buf)))
            (should (string-match-p "Read: c"
                                    (niwa-view-stream-test--history view-buf)))
            (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
            (if leave-before-settlement
                (should (= 4 (niwa-view-stream-test--draft-offset)))
              (should (string-match-p
                       "Read: c" (buffer-substring-no-properties
                                  (line-beginning-position) (line-end-position)))))
            (niwa-view-stream-test--leave-row-for-draft 4)
            (set-window-point (selected-window) (point)))
          (niwa-view-render-live-update data-buf)
          (with-current-buffer view-buf
            (should (string-match-p "Read: c"
                                    (niwa-view-stream-test--history view-buf)))
            (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
            (should (= 4 (niwa-view-stream-test--draft-offset))))))))

  :doc "a running-only reader expansion belongs to its turn, not its data position"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3)
          (calls '((:name "Read" :args (:file_path "a"))
                   (:name "Read" :args (:file_path "b"))
                   (:name "Read" :args (:file_path "c"))
                   (:name "Read" :args (:file_path "d")))))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (niwa-view-stream-test--batch data-buf (butlast calls))
      (save-window-excursion
        (set-window-buffer (selected-window) view-buf)
        (with-current-buffer view-buf
          (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4)
          (goto-char (point-min))
          (search-forward "Read: c")
          (set-window-point (selected-window) (point)))
        (niwa-view-stream-test--batch data-buf (last calls))
        (with-current-buffer view-buf
          (should (string-suffix-p
                   "Read: c" (buffer-substring-no-properties
                              (line-beginning-position) (point))))
          (niwa-view-stream-stop)
          (niwa-view-stream-test--leave-row-for-draft 4)
          (set-window-point (selected-window) (point)))
        ;; No result arrived: the next turn reuses precisely the same data end.
        (niwa-view-stream-test--begin-live-turn view-buf data-buf)
        (niwa-view-stream-test--batch data-buf calls)
        (with-current-buffer view-buf
          (let ((text (niwa-view-stream-test--history view-buf)))
            (should (string-match-p "4 calls: read 4 files · 4 running" text))
            (should-not (string-match-p "Read: c" text)))
          (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
          (should (= 4 (niwa-view-stream-test--draft-offset)))))))

  :doc "a transient default fold is not remembered over a row opened later"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3)
          (calls '((:name "Read" :args (:file_path "a"))
                   (:name "Read" :args (:file_path "b"))
                   (:name "Read" :args (:file_path "c")))))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      ;; Results land before their post-tool hooks run, so for a moment the
      ;; run counts past the threshold and folds by default.
      (niwa-view-stream-test--batch data-buf calls)
      (niwa-view-stream-test--batch data-buf calls t)
      (with-current-buffer view-buf
        (should (string-match-p "✓ Read: c"
                                (niwa-view-stream-test--history view-buf)))
        (goto-char (point-min))
        (search-forward "Read: c")
        (niwa-view-toggle-section)
        (should (string-match-p "result" (niwa-view-stream-test--history view-buf)))
        (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4))
      (niwa-view-stream-test--batch
       data-buf '((:name "Read" :args (:file_path "d"))))
      (let ((text (niwa-view-stream-test--history view-buf)))
        (should (string-match-p "4 calls: read 4 files · 1 running" text))
        (should (string-match-p "Read: c" text))
        (should (string-match-p "result" text)))
      (with-current-buffer view-buf
        (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text))))))

  :doc "a running group keeps its single row when one of its calls fails"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3)
          (calls '((:name "Read" :args (:file_path "a"))
                   (:name "Read" :args (:file_path "b"))
                   (:name "Read" :args (:file_path "c"))
                   (:name "Grep" :args (:pattern "x")))))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4)
        (goto-char (point-min)))
      (niwa-view-stream-test--batch data-buf calls)
      (dolist (call (butlast calls))
        (niwa-view-stream-test--batch data-buf (list call) t))
      (with-current-buffer data-buf
        (niwa-view-stream-test--insert-data
         data-buf "(:name \"Grep\" :args (:pattern \"x\"))\n\nError: tool failed\n"
         (cons 'tool "grep-x"))
        (niwa-view-stream-post-tool (car (last calls))))
      (let ((text (niwa-view-stream-test--history view-buf)))
        (should (string-match-p
                 "! 4 calls: read 3 files, searched 1 pattern (1 failed) ▸$" text))
        (should-not (string-match-p "Read: a\\|Grep: x" text)))
      (with-current-buffer view-buf
        (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-stream-test--draft-offset))))))

  :doc "calls after assistant text start their own run"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (niwa-view-stream-test--batch
       data-buf (list '(:name "Read" :args (:file_path "a"))
                      '(:name "Read" :args (:file_path "b"))
                      '(:name "Read" :args (:file_path "c")))
       t)
      (niwa-view-stream-test--insert-data
       data-buf "Now the tests.\n" 'response)
      (niwa-view-stream-test--batch
       data-buf (list '(:name "Grep" :args (:pattern "x"))))
      (let* ((text (niwa-view-stream-test--history view-buf))
             (prose (string-match-p "Now the tests" text))
             (running (string-match-p "● Grep: x" text)))
        (should prose)
        (should running)
        (should (< prose running))
        ;; The earlier run keeps its own rows; nothing crosses the prose.
        (should (string-match-p "✓ Read: a" text))
        (should-not (string-match-p "running" text)))))

  :doc "stopping a turn redraws its run from the transcript, counting only calls that ran"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3)
          (settled '((:name "Read" :args (:file_path "a"))
                     (:name "Read" :args (:file_path "b"))
                     (:name "Read" :args (:file_path "c"))
                     (:name "Read" :args (:file_path "d")))))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (niwa-view-stream-test--batch data-buf settled t)
      (niwa-view-stream-test--batch
       data-buf (list '(:name "Grep" :args (:pattern "x"))))
      (should (string-match-p "● 5 calls: read 4 files, searched 1 pattern · 1 running"
                              (niwa-view-stream-test--history view-buf)))
      (with-current-buffer view-buf
        (niwa-view-stream-stop)
        (let ((text (niwa-view-stream-test--history view-buf)))
          ;; The Grep never reached the transcript, so it no longer counts.
          (should (string-match-p "✓ 4 calls: read 4 files ▸\n" text))
          (should-not (string-match-p "searched" text))
          (should-not (string-match-p "running" text))
          (should-not (text-property-any (point-min) (point-max)
                                         'niwa-view-pending-tool-live t))
          (goto-char (point-min))
          (search-forward "✓ 4 calls: read 4 files")
          ;; The restored marker still belongs to the group's section.
          (should (eq 'tool-group
                      (get-text-property (match-beginning 0)
                                         'niwa-view-type)))
          (should-not niwa-view--spinner-tool-targets)))))

  :doc "a group formed only by running calls returns to plain rows on stop"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4))
      (niwa-view-stream-test--batch
       data-buf (list '(:name "Read" :args (:file_path "a"))
                      '(:name "Read" :args (:file_path "b")))
       t)
      (niwa-view-stream-test--batch
       data-buf (list '(:name "Grep" :args (:pattern "x"))
                      '(:name "Grep" :args (:pattern "y"))))
      (should (string-match-p "· 2 running"
                              (niwa-view-stream-test--history view-buf)))
      (with-current-buffer view-buf
        (niwa-view-stream-stop)
        (let ((text (niwa-view-stream-test--history view-buf)))
          (should (string-match-p "✓ Read: a" text))
          (should (string-match-p "✓ Read: b" text))
          (should-not (string-match-p "Grep\\|running\\|files" text)))
        (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-stream-test--draft-offset))))))

  :doc "an expanded running group lists its running calls and stays open"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3)
          (running '((:name "Grep" :args (:pattern "x"))
                     (:name "Grep" :args (:pattern "y")))))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (niwa-view-stream-test--batch
       data-buf (list '(:name "Read" :args (:file_path "a"))
                      '(:name "Read" :args (:file_path "b"))
                      '(:name "Read" :args (:file_path "c"))
                      '(:name "Read" :args (:file_path "d")))
       t)
      (with-current-buffer view-buf
        (goto-char (point-min))
        (search-forward "read 4 files")
        (niwa-view-toggle-section))
      (niwa-view-stream-test--batch data-buf running)
      (let ((text (niwa-view-stream-test--history view-buf)))
        (should (string-match-p "6 calls: read 4 files, searched 2 patterns · 2 running"
                                text))
        ;; Settled children and the running calls are all listed.
        (should (string-match-p "Read: a" text))
        (should (string-match-p "● Grep: x" text))
        (should (string-match-p "● Grep: y" text)))
      (niwa-view-stream-test--batch data-buf running t)
      (let ((text (niwa-view-stream-test--history view-buf)))
        (should (string-match-p "✓ 6 calls: read 4 files, searched 2 patterns" text))
        (should-not (string-match-p "running" text))
        ;; Still expanded: the settled calls are listed as children.
        (should (string-match-p "Grep: y" text))
        (should (string-match-p "Read: a" text)))))

  :doc "a folded running group stays folded while it grows"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (niwa-view-stream-test--batch
       data-buf (list '(:name "Read" :args (:file_path "a"))
                      '(:name "Read" :args (:file_path "b"))
                      '(:name "Read" :args (:file_path "c"))
                      '(:name "Read" :args (:file_path "d")))
       t)
      (niwa-view-stream-test--batch
       data-buf (list '(:name "Grep" :args (:pattern "x"))))
      (let ((text (niwa-view-stream-test--history view-buf)))
        (should (string-match-p "· 1 running" text))
        (should-not (string-match-p "Read: a" text))
        (should-not (string-match-p "Grep: x" text)))))

  :doc "calls opening a response or following its prose keep their layout"
  (dolist (case (list (list nil '((:name "Read" :args (:file_path "a"))
                                  (:name "Grep" :args (:pattern "x"))))
                      (list nil '((:name "Read" :args (:file_path "a"))
                                  (:name "Read" :args (:file_path "b"))
                                  (:name "Read" :args (:file_path "c"))
                                  (:name "Grep" :args (:pattern "x"))))
                      (list "Let me look.\n"
                            '((:name "Read" :args (:file_path "a"))
                              (:name "Grep" :args (:pattern "x"))))))
    (niwa-view-stream-test--with-buffers
      (let ((prose (car case))
            (calls (cadr case))
            (niwa-view-tool-boundary-render-delay 0)
            (niwa-view-tool-group-collapse-threshold 3))
        (niwa-view-stream-test--insert-data data-buf "Prompt\n" nil)
        (with-current-buffer view-buf
          (niwa-view--insert-user-message "Prompt"))
        (niwa-view-stream-test--begin-live-turn view-buf data-buf)
        (when prose
          (niwa-view-stream-test--insert-data data-buf prose 'response))
        (niwa-view-stream-test--batch data-buf calls)
        (let ((running (niwa-view-stream-test--history view-buf)))
          (should (= 1 (niwa-view-stream-test--count-substring
                        "Assistant\n" running)))
          (should (string-match-p (if prose "Let me look." "Assistant\n  ● ")
                                  running))
          (niwa-view-stream-test--batch data-buf calls t)
          ;; Settling flips the marker and drops the count; the heading,
          ;; rows and the progress row below keep their lines.  Settled
          ;; rows may gain result metadata.
          (cl-flet ((layout (text)
                      (replace-regexp-in-string
                       " · [0-9]+ running\\| ([^()\n]*)$\\| · [0-9]+s" ""
                       (string-replace "●" "✓" text))))
            (should (equal (layout running)
                           (layout (niwa-view-stream-test--history
                                    view-buf)))))))))

  :doc "a turn stopped before any result removes its interim heading"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (niwa-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer view-buf
        (niwa-view--insert-user-message "Prompt"))
      (let ((before (niwa-view-stream-test--history view-buf)))
        (niwa-view-stream-test--begin-live-turn view-buf data-buf)
        (niwa-view-stream-test--batch
         data-buf (list '(:name "Read" :args (:file_path "a"))))
        (should (string-match-p "Assistant\n  ● Read: a"
                                (niwa-view-stream-test--history view-buf)))
        (with-current-buffer view-buf
          (niwa-view-stream-stop))
        (should (equal before (niwa-view-stream-test--history view-buf))))))

  :doc "calls after the turn's prose share its single heading"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (niwa-view-stream-test--insert-data
       data-buf "Let me look.\n" 'response)
      (niwa-view-stream-test--batch
       data-buf (list '(:name "Read" :args (:file_path "a"))))
      (should (= 1 (niwa-view-stream-test--count-substring
                    "Assistant\n"
                    (niwa-view-stream-test--history view-buf))))))

  :doc "a call reported finished before its batch keeps the run's shape"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3)
          (calls (mapcar (lambda (file) (list :name "Read" :args (list :file_path file)))
                         '("a" "b" "c" "d"))))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (with-current-buffer view-buf (goto-char (point-min)))
      (niwa-view-stream-test--batch data-buf calls)
      (cl-flet ((event (type call id)
                  (run-hook-with-args
                   'niwa-pipeline-tool-event-hook
                   (append (list :type type :tool-use-id id :data-buffer data-buf)
                           call))))
        (cl-loop for call in calls for i from 1
                 do (event 'started call (format "id-%d" i)))
        (should (string-match-p "● 4 calls: read 4 files · 4 running"
                                (niwa-view-stream-test--history view-buf)))
        ;; The pipeline reports one call finished; gptel writes the
        ;; batch's results only once all four are done.
        (event 'settled (car calls) "id-1")
        (let ((text (niwa-view-stream-test--history view-buf)))
          (should (string-match-p "● 4 calls: read 4 files · 3 running" text))
          (should-not (string-match-p "Read: b" text)))
        (dolist (call (cdr calls))
          (event 'settled call (format "id-%d" (1+ (cl-position call calls)))))
        ;; Every call finished, results still due: a finished group row.
        (let ((text (niwa-view-stream-test--history view-buf)))
          (should (string-match-p "✓ 4 calls: read 4 files ▸\n" text))
          (should-not (string-match-p "running\\|●" text)))
        (niwa-view-stream-test--batch data-buf calls t)
        (with-current-buffer view-buf
          (should-not niwa-view--pending-tool-calls))
        (should (= 1 (niwa-view-stream-test--count-substring
                      "✓ 4 calls: read 4 files" (niwa-view-stream-test--history
                                        view-buf)))))))

  :doc "a folded running group counts calls that failed before their results"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3)
          (calls (mapcar (lambda (file) (list :name "Read" :args (list :file_path file)))
                         '("a" "b" "c" "d"))))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (with-current-buffer view-buf (goto-char (point-min)))
      (niwa-view-stream-test--batch data-buf calls)
      (cl-flet ((event (type call id &rest extra)
                  (run-hook-with-args
                   'niwa-pipeline-tool-event-hook
                   (append (list :type type :tool-use-id id :data-buffer data-buf)
                           extra call))))
        (cl-loop for call in calls for i from 1
                 do (event 'started call (format "id-%d" i)))
        (event 'settled (car calls) "id-1" :status 'error)
        (should (string-match-p "● 4 calls: read 4 files (1 failed) · 3 running ▸\n"
                                (niwa-view-stream-test--history view-buf)))
        (cl-loop for call in (cdr calls) for i from 2
                 do (event 'settled call (format "id-%d" i)))
        ;; Nothing left running: the failure takes the marker.
        (let ((text (niwa-view-stream-test--history view-buf)))
          (should (string-match-p "× 4 calls: read 4 files (1 failed) ▸\n" text))
          (should-not (string-match-p "running\\|●" text)))
        ;; A stop restores the group's own header.
        (with-current-buffer view-buf (niwa-view-stream-stop))
        (should-not (string-match-p "failed\\|running"
                                    (niwa-view-stream-test--history view-buf))))))

  :doc "a call reported finished draws a still finished row below the threshold"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (calls '((:name "Read" :args (:file_path "a"))
                   (:name "Grep" :args (:pattern "x")))))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (niwa-view-stream-test--batch data-buf calls)
      (run-hook-with-args 'niwa-pipeline-tool-event-hook
                          (append (list :type 'started :tool-use-id "r"
                                        :data-buffer data-buf)
                                  (car calls)))
      (run-hook-with-args 'niwa-pipeline-tool-event-hook
                          (append (list :type 'settled :tool-use-id "r"
                                        :data-buffer data-buf)
                                  (car calls)))
      (let ((text (niwa-view-stream-test--history view-buf)))
        (should (string-match-p "  ✓ Read: a\n  ● Grep: x" text)))
      (with-current-buffer view-buf
        ;; Only the running call is animated.
        (should (= 1 (length niwa-view--spinner-tool-targets))))))

  :doc "folding and unfolding a running group keeps its running calls"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3)
          (header "● 6 calls: read 4 files, searched 2 patterns · 2 running"))
      (niwa-view-stream-test--begin-live-turn view-buf data-buf)
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4))
      (niwa-view-stream-test--batch
       data-buf (mapcar (lambda (file) (list :name "Read" :args (list :file_path file)))
                        '("a" "b" "c" "d"))
       t)
      (with-current-buffer view-buf
        (goto-char (point-min))
        (search-forward "read 4 files")
        (niwa-view-toggle-section))
      (niwa-view-stream-test--batch
       data-buf '((:name "Grep" :args (:pattern "x"))
                  (:name "Grep" :args (:pattern "y"))))
      (cl-flet ((toggle ()
                  (with-current-buffer view-buf
                    (goto-char (point-min))
                    (search-forward "read 4 files")
                    (niwa-view-toggle-section)
                    (niwa-view-stream-test--history view-buf))))
        ;; Fold: one running header, no running rows left behind.
        (let ((text (toggle)))
          (should (= 1 (niwa-view-stream-test--count-substring header text)))
          (should-not (string-match-p "Grep: x\\|Read: a" text)))
        ;; Unfold: settled children and running calls are listed again.
        (let ((text (toggle)))
          (should (= 1 (niwa-view-stream-test--count-substring header text)))
          (should (string-match-p "Read: a" text))
          (should (= 1 (niwa-view-stream-test--count-substring "● Grep: x" text)))
          (should (= 1 (niwa-view-stream-test--count-substring "● Grep: y" text)))))
      (with-current-buffer view-buf
        (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
        (should (= 2 (length niwa-view--pending-tool-calls))))))

  :doc "an archived segment on display never receives the live turn"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-tool-boundary-render-delay 0)
          (niwa-view-tool-group-collapse-threshold 3)
          (archive (generate-new-buffer " *archived-segment*")))
      (unwind-protect
          (progn
            (with-current-buffer archive
              (niwa--transcript-org-mode)
              (setq-local gptel-response-separator "\n\n")
              (setq-local niwa--view-buffer view-buf))
            (niwa-view-stream-test--insert-data archive "Old prompt\n" nil)
            (dolist (file '("a" "b" "c" "d"))
              (niwa-view-stream-test--insert-data
               archive (format "%S\n\nresult\n"
                               (list :name "Read" :args (list :file_path file)))
               (cons 'tool (concat "old-" file))))
            (niwa-view-stream-test--insert-data archive "Old answer.\n" 'response)
            (niwa-view-stream-test--begin-live-turn view-buf data-buf)
            (niwa-view-stream-test--batch
             data-buf '((:name "Grep" :args (:pattern "x"))))
            (with-current-buffer view-buf
              (setq niwa-view-segments--number 1
                    niwa-view-segments--buffer archive)
              (cl-letf (((symbol-function 'niwa-view-segments-banner)
                         (lambda () "Viewing archived segment\n\n")))
                (niwa-view-render--full-now)
                (let ((before (niwa-view-stream-test--history view-buf)))
                  (should (string-match-p "read 4 files" before))
                  ;; Toggle a section of the archive while calls still run.
                  (goto-char (point-min))
                  (search-forward "read 4 files")
                  (niwa-view-toggle-section)
                  (niwa-view-toggle-section)
                  ;; A live update arriving now leaves the archive alone too.
                  (niwa-view-render-live-update data-buf)
                  (let ((after (niwa-view-stream-test--history view-buf)))
                    (should (equal before after))
                    (should-not (string-match-p "Grep: x" after)))))))
        (with-current-buffer view-buf
          (setq niwa-view-segments--number nil
                niwa-view-segments--buffer nil))
        (kill-buffer archive)))))


;;
;;; Re-render idempotence with renderer


;;
;;; Request progress

(niwa-deftest niwa-view--render-request-progress ()
  ,test
  (test)
  :doc "stream, full render and spinner ticks keep the rail at the draft's top"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "Hello\n" 'response)
    (with-current-buffer view-buf
      (niwa-view-stream-begin-turn
       niwa-view--status-marker
       (with-current-buffer data-buf (copy-marker (point-min))))
      (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4)
      (niwa-view-render-live-update data-buf)
      (let ((start (niwa-view-composer-draft-start))
            (rail-start (car niwa-view--spinner-label-target)))
        (dolist (refresh '(niwa-view--full-rerender
                           niwa-view--spinner-tick
                           niwa-view--full-rerender))
          (funcall refresh)
          (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
          (should (= 4 (niwa-view-stream-test--draft-offset)))
          (should (= start (niwa-view-composer-draft-start)))
          (should (string-search "Working..." (niwa-view-test--rail-text)))
          (should-not (string-search "Working..." (buffer-string)))
          (should (eq rail-start (car niwa-view--spinner-label-target)))
          (with-current-buffer (niwa-view-composer-buffer)
            (should (eq (marker-buffer rail-start) (current-buffer)))
            (should (= 1 (line-number-at-pos rail-start)))))))))

(niwa-deftest niwa-view-stream-rail-isolation ()
  ,test
  (test)
  :doc "changing the permanent rail edits neither transcript nor composer text"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "Hello\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let* ((composer (niwa-view-composer-buffer))
             (contents (buffer-string))
             (draft-tick (with-current-buffer composer (buffer-chars-modified-tick)))
             (idle (niwa-view-test--rail-text)))
        (should (string-search "Hello" contents))
        (niwa-view--start-spinner "Working...")
        (should (string-search "Working..." (niwa-view-test--rail-text)))
        (should (equal-including-properties contents (buffer-string)))
        (niwa-view--stop-spinner)
        (should (equal idle (niwa-view-test--rail-text)))
        (should (equal-including-properties contents (buffer-string)))
        (with-current-buffer composer
          (should (= draft-tick (buffer-chars-modified-tick))))))))

(niwa-deftest niwa-view-stream-spinner-reader-state ()
  (niwa-view-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "Readable response.\n" 'response)
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer view-buf)
      (let ((niwa-view-spinner-animate nil))
        (niwa-view--full-rerender)
        (niwa-view--start-spinner "Thinking...")
        (niwa-view-test--insert-composer-draft "> unsent draft\nsecond line" 4)
        (let ((reader (selected-window))
              (composer (niwa-view-composer-display (selected-window))))
          (set-window-point composer (+ (niwa-view-composer-draft-start) 4))
          (goto-char (point-min))
          (search-forward "Readable")
          (set-mark (+ (point) 3))
          (setq mark-active t deactivate-mark nil)
          (let ((reader-point (point)) (reader-mark (mark)))
            ,action
            (should (= (point) reader-point))
            (should (= (mark) reader-mark))
            (should mark-active)
            (should-not deactivate-mark)
            (should (= (window-point reader) (point)))
            (should (= (- (window-point composer)
                          (niwa-view-composer-draft-start))
                       4))
            (should (equal "> unsent draft\nsecond line"
                           (niwa-view-composer-draft-text))))))))
  (action)
  :doc "direct progress rendering preserves the reader's selection and draft"
  (progn
    (setq niwa-view--spinner-status "Waiting for tools...")
    (niwa-view--render-request-progress))
  :doc "starting progress preserves the reader's selection and draft"
  (niwa-view--start-spinner "Waiting for tools...")
  :doc "updating progress preserves the reader's selection and draft"
  (niwa-view--update-spinner "Waiting for tools...")
  :doc "stopping progress preserves the reader's selection and draft"
  (niwa-view--stop-spinner))

(niwa-deftest niwa-view--start-spinner ()
  ,test
  (test)
  :doc "starts and clears activity on the composer rail"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view--start-spinner "Working...")
      (should (string-search "Working" (niwa-view-test--rail-text)))
      (should-not (string-search "Working" (buffer-string)))
      (should (eq t (niwa-view-stream-test--rail-activity 'niwa-view-spinner-frame)))
      (niwa-view--stop-spinner)
      (should (string-prefix-p "[ask]" (niwa-view-test--rail-text)))
      (should-not (niwa-view-stream-test--rail-activity 'niwa-view-spinner-frame))
      (should-not niwa-view--spinner-status)
      (should-not niwa-view--spinner-timer)))

  :doc "queued interaction text stays in the transcript while activity stays on the rail"
  (niwa-view-stream-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'project
                       :id "spinner-queued"
                       :root temporary-file-directory
                       :name "spinner-queued"))
           (session (niwa-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--start-spinner "Working...")
        (setf (niwa-session-pending-follow-ups session)
              (list (list :input "queued while busy"
                          :display-text "queued while busy")))
        (niwa-view--interaction-rebuild)
        (should (string-search "queued while busy" (buffer-string)))
        (should-not (string-search "Working" (buffer-string)))
        (should (string-search "Working" (niwa-view-test--rail-text)))
        (niwa-view--stop-spinner))))

  :doc "queued interaction rebuild suppresses modification hooks"
  (niwa-view-stream-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'project
                       :id "spinner-queued-hooks"
                       :root temporary-file-directory
                       :name "spinner-queued-hooks"))
           (session (niwa-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--start-spinner "Working...")
        (setf (niwa-session-pending-follow-ups session)
              (list (list :input "queued while busy"
                          :display-text "queued while busy")))
        (let ((changes 0))
          (add-hook 'after-change-functions
                    (lambda (&rest _ignore)
                      (cl-incf changes))
                    nil t)
          (niwa-view--interaction-rebuild)
          (should (= 0 changes)))
        (niwa-view--stop-spinner))))

  :doc "pending tool rows stay above queued text, separate from the request spinner"
  (niwa-view-stream-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'project
                       :id "spinner-pending-queued"
                       :root temporary-file-directory
                       :name "spinner-pending-queued"))
           (session (niwa-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--start-spinner "Working...")
        (setq niwa-view--pending-tool-calls
              (list (list "call-1" "Read" "a")))
        (niwa-view-stream-test--draw-running-rows)
        (setf (niwa-session-pending-follow-ups session)
              (list (list :input "queued while busy"
                          :display-text "queued while busy")))
        (niwa-view--interaction-rebuild)
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (calling (string-match-p "● Read: a" text))
               (queued (string-match-p "queued while busy" text)))
          (should calling)
          (should queued)
          (should (< calling queued))
          (should-not (string-search "Working" text))
          (should (string-search "Working" (niwa-view-test--rail-text))))
        (niwa-view--stop-spinner))))

  :doc "a live render of running calls suppresses modification hooks"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((changes 0))
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-max))))
        (setq niwa-view--pending-tool-calls
              (list (list "call-1" "Read" nil)))
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (niwa-view-render-live-update data-buf)
        (should (= 0 changes))
        (let ((pos (text-property-any
                    (point-min) (niwa-view-transcript-tail)
                    'niwa-view-pending-tool-live t)))
          (should pos)
          ;; A running call has no transcript source to disclose.
          (should-not (get-text-property pos 'niwa-view-source))
          (should (< pos (marker-position niwa-view--status-marker)))))))

  :doc "pending tool refresh preserves composer text and point"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "draft\n> keep typing" 8)
      (let ((input-offset (niwa-view-stream-test--draft-offset)))
        (setq niwa-view--pending-tool-calls
              (list (list "call-1" "Read" nil)))
        (niwa-view-stream-test--draw-running-rows)
        (should (equal "draft\n> keep typing"
                       (niwa-view-composer-draft-text)))
        (should (= (niwa-view-stream-test--draft-offset)
                   input-offset)))))

  :doc "request progress render preserves selected-window history point"
  (niwa-view-stream-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char (niwa-view--history-insertion-marker))
          (niwa-view--with-render-boundaries-advancing
            (insert (propertize "Earlier answer\n"
                                'niwa-view-type 'response
                                'niwa-view-source '(1 . 2)))))
        (goto-char (point-min))
        (search-forward "Earlier")
        (goto-char (match-beginning 0))
        (let ((point-before (point)))
          (setq niwa-view--spinner-start-time (current-time))
          (niwa-view--ensure-request-progress data-buf)
          (should (= (window-point (selected-window)) point-before))
          (should (= (point) point-before))
          (should (looking-at-p "Earlier"))))))

  :doc "pre-tool render preserves selected-window composer point"
  (niwa-view-stream-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((niwa-view-spinner-style 'ascii)
            (composer (niwa-view-composer-buffer view-buf)))
        (with-current-buffer view-buf
          (setq niwa-view--in-flight-turn-start
                (copy-marker (niwa-view--history-insertion-marker) nil))
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf (copy-marker (point-min))))
          (niwa-view-test--insert-composer-draft
           "> quoted\nsecond line" 4))
        (unless (eq composer view-buf)
          (let ((window (split-window-below)))
            (set-window-buffer window composer)
            (select-window window)))
        (let* ((window (selected-window))
               (input-offset (- (window-point window)
                                (niwa-view-composer-draft-start view-buf))))
          (with-current-buffer data-buf
            (niwa-view-stream-pre-tool
             '(:name "Read" :args (:file_path "foo.el"))))
          (should (eq window (selected-window)))
          (should (eq composer (window-buffer window)))
          (with-current-buffer composer
            (should (niwa-view-composer-in-draft-p (point-marker) view-buf))
            (should (= (- (window-point window)
                          (niwa-view-composer-draft-start view-buf))
                       input-offset))
            (should (= (point) (window-point window)))
            (should (equal "> quoted\nsecond line"
                           (niwa-view-composer-draft-text view-buf)))
            (should (looking-at-p "oted")))))))

  :doc "WaitAgent pending redraw shows waiting text and preserves a leading-> draft"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq niwa-view--in-flight-turn-start
            (copy-marker (niwa-view--history-insertion-marker) nil))
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min))))
      (niwa-view-test--insert-composer-draft
       "> quoted\nsecond line" 4))
    (with-current-buffer data-buf
      (let ((niwa-view-tool-boundary-render-delay 0))
        (niwa-view-stream-pre-tool
         '(:id "wait-1" :name "WaitAgent" :args (:timeout_ms 10000)))))
    (with-current-buffer view-buf
      (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
      ;; The running row reads like the settled `WaitAgent: agents' row.
      (should (string-match-p
               "● WaitAgent: agents"
               (niwa-view-stream-test--history view-buf)))))

  :doc "a running row joins ordinary text that merely reads like one"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (goto-char (niwa-view-transcript-tail-marker))
        (insert (propertize "Calling Read...\n"
                            'niwa-view-source '(1 . 2)
                            'niwa-view-type 'response))
        (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
        (insert "| Calling Read...\n")
        (setq niwa-view--in-flight-turn-start
              (copy-marker (point-min) nil))
        (set-marker niwa-view--status-marker (point))
        (set-marker niwa-view--interaction-marker (point))
        (set-marker (niwa-view-transcript-tail-marker) (point))
        (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil))
      (setq niwa-view--pending-tool-calls
            (list (list "call-1" "Read" nil)))
      (niwa-view-stream-test--draw-running-rows)
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (= 2 (niwa-view-stream-test--count-substring
                      "Calling Read" text)))
        (should (= 1 (niwa-view-stream-test--count-substring
                      "● Read" text))))
      ;; Retiring the running row leaves the look-alike text alone.
      (niwa-view--retire-pending-tool-rows)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 2 (niwa-view-stream-test--count-substring
                      "Calling Read" text)))
        (should-not (string-match-p "● Read" text)))))

  :doc "update replaces spinner text"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view--start-spinner "Thinking...")
      (niwa-view--update-spinner "Calling Read...")
      (let ((text (niwa-view-test--rail-text)))
        (should (string-match-p "Calling Read" text))
        (should-not (string-match-p "Working" text)))
      (niwa-view--stop-spinner)))

  :doc "spinner freezes elapsed time and decorative motion while input is pending"
  (niwa-view-stream-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'project
                       :id "spinner-agents"
                       :root temporary-file-directory
                       :name "spinner-agents"))
           (session (niwa-session-create "main" workspace))
           (request
            (niwa-request--create
             :session session
             :started-at
             (time-subtract (current-time) (seconds-to-time 12))))
           (niwa-view-spinner-style 'ascii)
           (niwa-view-spinner-power-policy 'full))
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/spin"
                        (niwa-agent-record--create
                         :id "worker--spin" :path "/root/spin"
                         :parent-path "/root" :activity 'running))))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--current-request request))
      (niwa-view-stream-test--with-visible-view
        (niwa-view--start-spinner "Thinking...")
        (niwa-view--interaction-register
         '(:kind ask :id ask :origin "/root" :body "ask"))
        (let* ((pause-started
                (niwa-request-active-work-pause-started-at request))
               (frame (niwa-view-stream-test--rail-activity))
               (text (niwa-view-test--rail-text)))
          (should (string-match-p "Waiting for input" text))
          (should (string-match-p "[0-9]+s" text))
          ;; The background-work panel counts agents; the footer does not.
          (should-not (string-match-p "agent" text))
          (should
           (< (abs
               (- (niwa-request-active-elapsed-seconds
                   request pause-started)
                  (niwa-request-active-elapsed-seconds
                   request
                   (time-add pause-started (seconds-to-time 5)))))
              0.001))
          ;; Neither elapsed time nor glyphs advance during the prompt,
          ;; even if an already delivered callback reaches the tick function.
          (dolist (seconds '(0.24 0.48 0.72))
            (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                       (lambda () seconds)))
              (niwa-view--spinner-tick))
            (should (equal frame (niwa-view-stream-test--rail-activity))))
          (should (equal text (niwa-view-test--rail-text))))
        (niwa-view--interaction-unregister 'ask)
        (should (string-search "Working..." (niwa-view-test--rail-text)))
        (niwa-view--stop-spinner))))

  :doc "spinner ticks replace dynamic metadata instead of appending it"
  (niwa-view-stream-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'project
                       :id "spinner-no-pileup"
                       :root temporary-file-directory
                       :name "spinner-no-pileup"))
           (session (niwa-session-create "main" workspace))
           (started (time-subtract (current-time) (seconds-to-time 12))))
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/spin"
                        (niwa-agent-record--create
                         :id "worker--spin" :path "/root/spin"
                         :parent-path "/root" :activity 'running))))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--current-request
                    (niwa-request--create
                     :session session
                     :started-at started)))
      (with-current-buffer view-buf
        (niwa-view--start-spinner "Thinking...")
        (dotimes (_ 3)
          (niwa-view--spinner-tick))
        (let ((text (niwa-view-test--rail-text)))
          (should (= 1 (cl-loop with start = 0
                                while (string-match " · [0-9]+s" text start)
                                count t
                                do (setq start (match-end 0)))))
          (should-not (string-match-p "agent" text)))
        (niwa-view--stop-spinner))))

  :doc "spinner tick suppresses modification hooks"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view--pending-tool-calls
             '(("call-1" "Read" nil)))
            (changes 0))
        (niwa-view--start-spinner "Thinking...")
        (niwa-view-stream-test--draw-running-rows)
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (niwa-view--spinner-tick)
        (should (= 0 changes))
        (niwa-view--stop-spinner))))

  :doc "spinner ticks update frames without rewriting unchanged status text"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii))
        (niwa-view--start-spinner "Working...")
        (let ((niwa-view--spinner-last-second (floor (float-time))))
          (cl-letf (((symbol-function 'niwa-view--write-request-rail)
                     (lambda (&rest _) (ert-fail "Frame tick rewrote the rail")))
                    ((symbol-function 'niwa-view--animation-seconds)
                     (lambda () 0.24)))
            (dotimes (_ 10) (niwa-view--spinner-tick))))
        (should (equal "\\ Working..." (niwa-view-stream-test--rail-activity)))
        (niwa-view--stop-spinner))))

  :doc "decorated spinner status is normalized to its base label"
  (should (equal "Working..."
                 (niwa-view--spinner-base-status
                  "Working... · 14s · 21s")))

  :doc "spinner marker refresh keeps the live line outside composer input"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view--start-spinner "Thinking...")
      (niwa-view-composer-insert "/edits")
      (should (equal "/edits" (niwa-view-composer-draft-text)))
      (niwa-view--spinner-tick)
      (should (equal "/edits" (niwa-view-composer-draft-text)))
      (niwa-view--stop-spinner)))

  :doc "spinner tick preserves composer point while drafting"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view--start-spinner "Thinking...")
      (niwa-view-test--insert-composer-draft "/edits")
      (let ((point-before (niwa-view-stream-test--draft-offset)))
        (niwa-view--spinner-tick)
        (should (equal "/edits" (niwa-view-composer-draft-text)))
        (should (= (niwa-view-stream-test--draft-offset) point-before)))
      (niwa-view--stop-spinner)))

  :doc "spinner tick preserves point on a visible permission prompt"
  (let ((niwa-view-spinner-animate nil))
    (niwa-view-stream-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (insert (make-string 10 ?\n))))
        (niwa-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "Permission Request\nTool: Read\nPath: .git/HEAD\n"
               :keymap (make-sparse-keymap)
               :entry 'permission-entry :activate #'ignore))
        (niwa-view--start-spinner "Working...")
        (goto-char (overlay-start (niwa-view-zone-region 'interaction)))
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
          (niwa-view--spinner-tick)
          (should (= point-before (point))))
        (niwa-view--stop-spinner))))

  :doc "input read excludes request progress fragment"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view--start-spinner "Thinking...")
      (niwa-view-composer-insert "/edits")
      (should (equal "/edits" (niwa-view-composer-draft-text)))))

  :doc "ASCII fallback frames can be selected"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full))
        (niwa-view--start-spinner "Working...")
        (should (equal "- Working..." (niwa-view-stream-test--rail-activity)))
        (niwa-view--stop-spinner))))

  :doc "spinner tick updates pending tool frame spans"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-tool-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full)
            (niwa-view--pending-tool-calls
             '(("call-1" "Read" nil))))
        (niwa-view-stream-test--draw-running-rows)
        (let ((frame-pos (text-property-any
                          (point-min) (point-max)
                          'niwa-view-inline-spinner-frame t)))
          (should frame-pos)
          (should (equal (get-text-property frame-pos 'display) "- "))
          (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                     (lambda () 0.24)))
            (niwa-view--spinner-tick))
          (should (equal (get-text-property frame-pos 'display) "\\ "))))))

  :doc "semantic updates reuse targets without changing composer text"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-power-policy 'full))
        (unwind-protect
            (progn
              (niwa-view--start-spinner "Thinking...")
              (let* ((old niwa-view--spinner-label-target)
                     (composer (niwa-view-composer-buffer))
                     (tick (with-current-buffer composer (buffer-chars-modified-tick))))
                (niwa-view--update-spinner "Checking completed work...")
                (should (eq old niwa-view--spinner-label-target))
                (should (eq composer (marker-buffer (car old))))
                (should (eq composer (marker-buffer (cdr old))))
                (with-current-buffer composer
                  (should (= tick (buffer-chars-modified-tick))))))
          (niwa-view--stop-spinner)))))

  :doc "streamed text stays outside the stable composer activity span"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-power-policy 'full))
        (unwind-protect
            (progn
              (niwa-view--start-spinner "Thinking...")
              (let ((target niwa-view--spinner-label-target))
                (niwa-view-stream-test--begin-live-turn view-buf data-buf)
                (niwa-view-stream-test--insert-data data-buf "streamed words\n" 'response)
                (niwa-view-render-live-update data-buf)
                (should (string-search "streamed words" (buffer-string)))
                (should-not (string-search "streamed words" (niwa-view-test--rail-text)))
                (should (eq target niwa-view--spinner-label-target))
                (should (niwa-view-stream-test--rail-activity 'niwa-view-spinner-frame))))
          (niwa-view--stop-spinner)))))

  :doc "a request waiting for input holds its indicators still"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-power-policy 'full)
            (request (niwa-request--create :started-at (current-time))))
        (unwind-protect
            (cl-letf (((symbol-function 'niwa-view--spinner-request)
                       (lambda () request)))
              (niwa-view--start-spinner "Thinking...")
              (should (niwa--ui-timer-pending-p niwa-view--spinner-timer))
              (niwa-request-set-active-work-paused request t)
              (niwa-view--start-spinner-timer)
              (should-not niwa-view--spinner-timer)
              (niwa-request-set-active-work-paused request nil)
              (niwa-view--start-spinner-timer)
              (should (niwa--ui-timer-pending-p niwa-view--spinner-timer)))
          (niwa-view--stop-spinner)))))

  :doc "a semantic tick with an unchanged plan rearms the delivered timer"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-power-policy 'full))
        (unwind-protect
            (progn
              (niwa-view--start-spinner "Thinking...")
              (let ((timer niwa-view--spinner-timer))
                (should (niwa--ui-timer-pending-p timer))
                ;; Deliver it as Emacs does: off the list, then its callback.
                (niwa--ui-timer-cancel timer)
                (setq niwa-view--spinner-last-second nil)
                (funcall (timer--function timer))
                (should (eq timer niwa-view--spinner-timer))
                (should (niwa--ui-timer-pending-p timer))))
          (niwa-view--stop-spinner)))))

  :doc "shimmer running rows animate their tool name, not its argument"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-tool-spinner-style 'shimmer)
            (niwa-view-spinner-power-policy 'full)
            (niwa-view--pending-tool-calls
             '(("call-1" "Read" "notes.org"))))
        (niwa-view-stream-test--draw-running-rows)
        (let* ((start (text-property-any (point-min) (point-max)
                                         'niwa-view-inline-spinner-frame t))
               (end (next-single-property-change
                     start 'niwa-view-inline-spinner-frame)))
          (should (equal "Read" (buffer-substring-no-properties start end)))
          (should (string-search "Read" (get-text-property start 'display)))
          ;; The sweep keeps the name's settled color.
          (should (eq 'niwa-view-tool-name
                      (niwa-view--tool-animation-face
                       (car niwa-view--spinner-tool-targets))))
          (should (equal ": notes.org"
                         (buffer-substring-no-properties
                          end (save-excursion (goto-char end)
                                              (line-end-position)))))))))

  :doc "probes once when a new span is not yet within the drawn window"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-power-policy 'full)
            (niwa-view--spinner-probed-targets nil))
        (unwind-protect
            (cl-letf (((symbol-function 'niwa-view--animation-visible-p) #'ignore)
                      ((symbol-function 'niwa-view--animation-target-visible-p)
                       #'ignore)
                      ((symbol-function 'niwa-view--window-attended-p)
                       (lambda (_) t)))
              (niwa-view--start-spinner "Thinking...")
              (should-not niwa-view--spinner-timer)
              (let ((probe niwa-view--spinner-probe-timer))
                (should (niwa--ui-timer-pending-p probe))
                ;; The same spans are not probed again.
                (niwa--ui-timer-cancel probe)
                (setq niwa-view--spinner-probe-timer nil)
                (niwa-view--start-spinner-timer)
                (should-not niwa-view--spinner-probe-timer)))
          (niwa-view--stop-spinner)))))

  :doc "the probe rearms the spinner once the span has been drawn"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-power-policy 'full)
            (niwa-view--spinner-probed-targets nil)
            (drawn nil))
        (unwind-protect
            (cl-letf* ((visible (symbol-function
                                 'niwa-view--animation-target-visible-p))
                       ((symbol-function 'niwa-view--animation-target-visible-p)
                        (lambda (&rest args) (and drawn (apply visible args))))
                       ((symbol-function 'niwa-view--window-attended-p)
                        (lambda (_) t)))
              (niwa-view--start-spinner "Thinking...")
              (should-not niwa-view--spinner-timer)
              (setq drawn t)
              (funcall (timer--function niwa-view--spinner-probe-timer))
              (should (niwa--ui-timer-pending-p niwa-view--spinner-timer)))
          (niwa-view--stop-spinner)))))

  :doc "spinner tick does not move point on pending tool frame"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-tool-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full)
            (niwa-view--pending-tool-calls
             '(("call-1" "Read" nil))))
        (niwa-view-stream-test--draw-running-rows)
        (goto-char (text-property-any
                    (point-min) (point-max)
                    'niwa-view-inline-spinner-frame t))
        (let ((point-before (point)))
          (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                     (lambda () 0.24)))
            (niwa-view--spinner-tick))
          (should (= (point) point-before))))))

  :doc "spinner tick updates only running rows and preserves the composer"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-tool-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full)
            (niwa-view--pending-tool-calls
             '(("call-1" "Read" nil))))
        (niwa-view-stream-test--draw-running-rows)
        (let ((real-pos
               (text-property-any
                (point-min) (point-max)
                'niwa-view-inline-spinner-frame t))
              (decoy-pos (point-min))
              (inhibit-read-only t))
          (put-text-property
           decoy-pos (1+ decoy-pos)
           'niwa-view-inline-spinner-frame t)
          (put-text-property decoy-pos (1+ decoy-pos) 'display "decoy")
          (niwa-view-test--insert-composer-draft
           "> quoted\nsecond line" 4)
          (let ((cursor (niwa-view-stream-test--draft-offset))
                (draft-before (niwa-view-composer-draft-text)))
            (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                       (lambda () 0.24)))
              (niwa-view--spinner-tick))
            (should (equal "\\ " (get-text-property real-pos 'display)))
            (should (equal "decoy"
                           (get-text-property decoy-pos 'display)))
            (should (= cursor (niwa-view-stream-test--draft-offset)))
            (should (equal draft-before (niwa-view-composer-draft-text))))))))

  :doc "incremental response keeps request progress row visible"
  (niwa-view-stream-test--with-buffers
    (let (data-turn-start)
      (niwa-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (niwa-view-stream-test--insert-data data-buf "Partial answer.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view--insert-user-message "Prompt")
        (setq niwa-view--data-turn-start data-turn-start)
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker) nil))
        (niwa-view--start-spinner "Thinking...")
        (niwa-view-render-live-update data-buf)
        (should (niwa-view-stream-test--rail-activity 'niwa-view-spinner-frame))
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should (string-match-p "Partial answer" text))
          (should (string-search "Working" (niwa-view-test--rail-text)))))))

  :doc "direct data-buffer request begin starts request progress"
  (niwa-view-stream-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'project
                       :id "direct-progress"
                       :root temporary-file-directory
                       :name "direct-progress"))
           (session (niwa-session-create "main" workspace))
           position fsm)
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--current-request
                    (niwa-request--create
                     :session session
                     :started-at (current-time)))
        (setq position (copy-marker (point-max) nil))
        (setq fsm (gptel-make-fsm
                   :info (list :buffer data-buf :position position))))
      (with-current-buffer view-buf
        (setq niwa-view--request-progress-suppressed t))
      (niwa-view-stream-ensure-progress-for-fsm fsm)
      (with-current-buffer view-buf
        (should-not niwa-view--request-progress-suppressed)
        (should (niwa-view-stream-test--rail-activity 'niwa-view-spinner-frame))
        (should (markerp niwa-view--data-turn-start))
        (should (= (marker-position niwa-view--data-turn-start)
                   (marker-position position)))
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should (string-search "Working" (niwa-view-test--rail-text)))))))

  :doc "pre-tool render keeps request progress and adds a running row"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-spinner-style 'ascii)
          (niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (with-current-buffer data-buf
        (niwa-view-stream-spinner-hook
         '(:name "Read" :args (:file_path "foo.el")))
        (niwa-view-stream-pre-tool
         '(:name "Read" :args (:file_path "foo.el"))))
      (with-current-buffer view-buf
        (should (niwa-view-stream-test--rail-activity 'niwa-view-spinner-frame))
        (should (niwa-view-stream-test--rail-activity 'niwa-view-spinner-frame))
        (should (text-property-any
                 (point-min) (point-max)
                 'niwa-view-inline-spinner-frame t))
        (should (text-property-any
                 (point-min) (point-max)
                 'niwa-view-pending-tool-live t))
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-search "Working" (niwa-view-test--rail-text)))
          (should (= 1 (niwa-view-stream-test--count-substring
                        "● Read: foo.el" text)))))))

  :doc "disabled motion keeps a visible one-second metadata timer and live status"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-tool-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full)
            (niwa-view-spinner-animate nil)
            (niwa-view--pending-tool-calls
             '(("call-1" "Read" nil))))
        (niwa-view--start-spinner "Working...")
        (niwa-view-stream-test--draw-running-rows)
        (let* ((tool (text-property-any (point-min) (point-max)
                                        'niwa-view-inline-spinner-frame t))
               (main-frame (niwa-view-stream-test--rail-activity))
               (tool-frame (get-text-property tool 'display)))
          (should (timerp niwa-view--spinner-timer))
          (should (= 1.0 niwa-view--spinner-timer-period))
          (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                     (lambda () 0.24)))
            (niwa-view--spinner-tick))
          (should (equal main-frame (niwa-view-stream-test--rail-activity)))
          (should (equal tool-frame (get-text-property tool 'display)))
          (niwa-view--update-spinner "Checking completed work...")
          (should (string-search "Checking completed work" (niwa-view-test--rail-text)))
          (should (= 1.0 niwa-view--spinner-timer-period))))))

  :doc "real pending calls preserve current request status without a window timer"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-spinner-style 'ascii)
          (niwa-view-tool-spinner-style 'ascii)
          (niwa-view-spinner-power-policy 'full)
          (niwa-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (should-not (get-buffer-window view-buf t))
        (niwa-view-stream-begin-turn
         (niwa-view--history-insertion-marker)
         (with-current-buffer data-buf (copy-marker (point-max))))
        (niwa-view--update-spinner "Checking completed work...")
        (niwa-view-test--insert-composer-draft
         "> quoted\nsecond line" 3))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (niwa-view-stream-pre-tool
               '(:name "Read" :args (:file_path "a")))
              (niwa-view-stream-pre-tool
               '(:name "Grep" :args (:pattern "x"))))
            (with-current-buffer view-buf
              (should (equal "Checking completed work..."
                             niwa-view--spinner-status))
              (should (niwa-view-stream-test--rail-activity 'niwa-view-spinner-frame))
              (should (= 2 (length niwa-view--spinner-tool-targets)))
              (should-not (timerp niwa-view--spinner-timer))
              (should-not niwa-view--spinner-timer-period)
              (let ((text (buffer-substring-no-properties
                           (point-min) (point-max))))
                (should (string-search "Checking completed work" (niwa-view-test--rail-text)))
                (should (string-match-p "- Read: a" text))
                (should (string-match-p "- Grep: x" text))
                (should-not (string-search "Checking completed work" text))))
            (niwa-view-stream-test--with-visible-view
              (niwa-view--start-spinner-timer)
              (should (timerp niwa-view--spinner-timer))
              (should (= 0.24 niwa-view--spinner-timer-period))
              (should (equal "> quoted\nsecond line"
                             (niwa-view-composer-draft-text))))
            (with-current-buffer view-buf
              (niwa-view--start-spinner-timer)
              (should-not (timerp niwa-view--spinner-timer))
              (should-not niwa-view--spinner-timer-period))
            (with-current-buffer data-buf
              (niwa-view-stream-post-tool
               '(:name "Read" :args (:file_path "a"))))
            (with-current-buffer view-buf
              (should (equal "Checking completed work..."
                             niwa-view--spinner-status))
              (should (equal '("Grep" "x")
                             (cdar niwa-view--pending-tool-calls)))
              (should (= 1 (length niwa-view--spinner-tool-targets)))
              (should-not (string-match-p "- Read: a"
                                          (buffer-string)))
              (should (equal "> quoted\nsecond line"
                             (niwa-view-composer-draft-text)))
              (should (= 3 (niwa-view-stream-test--draft-offset)))))
        (with-current-buffer view-buf
          (niwa-view--cancel-scheduled-render)))))

  :doc "spinner hook does not duplicate pending tool status in flight"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (setq niwa-view--in-flight-turn-start
            (copy-marker (niwa-view--history-insertion-marker)))
      (setq niwa-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min)))))
    (with-current-buffer data-buf
      (niwa-view-stream-spinner-hook
       '(:name "Agent"
         :args (:task_name "explore" :message "Inspect."))))
    (with-current-buffer view-buf
      (should (string-prefix-p "[ask]" (niwa-view-test--rail-text)))
      (should-not niwa-view--spinner-status)
      (should-not (string-match-p "Calling Agent"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))))

  :doc "Agent pre-tool keeps request progress without duplicate pending line"
  (niwa-view-stream-test--with-buffers
    (let ((niwa-view-spinner-style 'ascii))
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (with-current-buffer data-buf
        (niwa-view-stream-spinner-hook
         '(:name "Agent"
           :args (:task_name "verify" :message "Verify.")))
        (niwa-view-stream-pre-tool
         '(:id "agent-1" :name "Agent"
           :args (:task_name "verify" :message "Verify."))))
      (with-current-buffer view-buf
        (should (niwa-view-stream-test--rail-activity 'niwa-view-spinner-frame))
        (should-not niwa-view--pending-tool-calls)
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-search "Working" (niwa-view-test--rail-text)))
          (should-not (string-match-p "Calling Agent" text))))))
)


;;
;;; Final response rendering

(niwa-deftest niwa-view--claim-spinner-status
  (:doc "claims only the unchanged request-progress generation")
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((owner (gensym "owner-")))
        (niwa-view--start-spinner "Working...")
        (let ((snapshot (niwa-view--spinner-status-snapshot)))
          (should (niwa-view--claim-spinner-status
                   snapshot "Running hook..." owner))
          (should (equal "Running hook..." niwa-view--spinner-status))
          (should (eq owner niwa-view--spinner-owner)))
        (let ((snapshot (niwa-view--spinner-status-snapshot)))
          (niwa-view--update-spinner "Calling Read..." 'request)
          (should-not (niwa-view--claim-spinner-status
                       snapshot "Stale hook..." owner))
          (should (equal "Calling Read..." niwa-view--spinner-status))
          (should (eq 'request niwa-view--spinner-owner)))))))

(niwa-deftest niwa-view--restore-spinner-status
  (:doc "restores only progress still owned by the caller")
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((owner (gensym "owner-")))
        (niwa-view--start-spinner "Working...")
        (let ((snapshot (niwa-view--spinner-status-snapshot)))
          (niwa-view--claim-spinner-status snapshot "Running hook..." owner)
          (should (niwa-view--restore-spinner-status owner snapshot))
          (should (equal "Working..." niwa-view--spinner-status))
          (should (eq 'request niwa-view--spinner-owner)))
        (let ((snapshot (niwa-view--spinner-status-snapshot)))
          (niwa-view--claim-spinner-status snapshot "Running hook..." owner)
          (niwa-view--update-spinner "Calling Bash..." 'request)
          (should-not (niwa-view--restore-spinner-status owner snapshot))
          (should (equal "Calling Bash..." niwa-view--spinner-status))
          (should (eq 'request niwa-view--spinner-owner)))))))

(niwa-deftest niwa-view-stream-render-response ()
  ,test
  (test)
  :doc "renders user + assistant turn"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "Hello world\n" nil)
    (niwa-view-stream-test--insert-data data-buf "Hi! How can I help?\n" 'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "You" text))
        (should (string-match-p "Assistant" text))
        (should (string-match-p "Hello world" text))
        (should (string-match-p "How can I help" text)))))

  :doc "clears stale compaction lock on final response"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "Hello world\n" nil)
    (niwa-view-stream-test--insert-data data-buf "Hi!\n" 'response)
    (with-current-buffer data-buf
      (setq-local niwa-compact-run-in-flight t)
      (niwa-view-stream-render-response (point-min) (point-max))
      (should-not niwa-compact-run-in-flight)))

  :doc "final response renders durable worked footer and hides side channel"
  (niwa-view-stream-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'project
                       :id "worked-footer"
                       :root temporary-file-directory
                       :name "worked-footer"))
           (session (niwa-session-create "main" workspace))
           (started (time-subtract (current-time) (seconds-to-time 390)))
           data-turn-start response-start response-end)
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--current-request
                    (niwa-request--create
                     :session session
                     :started-at started)))
      (niwa-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (1- (point)) nil))
        (setq response-start (point)))
      (niwa-view-stream-test--insert-data data-buf "Done.\n" 'response)
      (with-current-buffer data-buf
        (setq response-end (point)))
      (with-current-buffer view-buf
        (setq niwa-view--data-turn-start data-turn-start)
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker) nil))
        (niwa-view--start-spinner "Thinking..."))
      (with-current-buffer data-buf
        (niwa-view-stream-render-response response-start response-end)
        (should (string-search "request-summary"
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
      (with-current-buffer view-buf
        (should (string-prefix-p "[ask]" (niwa-view-test--rail-text)))
        (should-not niwa-view--spinner-status)
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should (string-match-p "Done" text))
          (should (string-match-p "Worked for 6m" text))
          (should-not (string-match-p "Working\\.\\.\\." text))
          (should-not (string-match-p "niwa-render-data" text)))
        (niwa-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should (string-match-p "Worked for 6m" text))
          (should-not (string-match-p "niwa-render-data" text))))))

  :doc "final response moves stale request summary after later streamed text"
  (niwa-view-stream-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'project
                       :id "worked-footer-tail"
                       :root temporary-file-directory
                       :name "worked-footer-tail"))
           (session (niwa-session-create "main" workspace))
           (started (time-subtract (current-time) (seconds-to-time 9)))
           response-start)
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--current-request
                    (niwa-request--create
                     :session session
                     :started-at started)))
      (niwa-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq response-start (point)))
      (niwa-view-stream-test--insert-data
       data-buf
       "```python\nprint('one')\n```\n"
       'response)
      (niwa-view-stream-test--insert-data
       data-buf
       (niwa-tool-render-data-format
        '(:kind request-summary :elapsed-seconds 4))
       'ignore)
      (niwa-view-stream-test--insert-data
       data-buf
       "```javascript\nconsole.log('two');\n```\n"
       'response)
      (with-current-buffer data-buf
        (niwa-view-stream-render-response response-start (point-max)))
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
                      (point-min) (niwa-view-transcript-tail)))
               (worked (string-match-p "Worked for" text))
               (javascript (string-match-p "javascript ⧉" text)))
          (should javascript)
          (should worked)
          (should (> worked javascript))
          (should-not (string-match-p "niwa-render-data" text))))))

  :doc "renders tool calls as one-liners"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nfile content\n"
     '(tool . "call_1"))
    (niwa-view-stream-test--insert-data data-buf "Here is the file.\n" 'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "Read.*test\\.el" text))
        (should-not (string-match-p "file content" text))
        (should (string-match-p "Here is the file" text)))))

  :doc "full rerender preserves tool result rewrite audit details"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data
     data-buf
     (concat
      "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\n"
      "updated result"
      (niwa--format-hook-audit-record
       '(:type tool-result-rewrite
               :event "PostToolUse"
               :original-result "original result"
               :updated-result "updated result"
               :reason "redacted"))
      "\n")
     '(tool . "call_1"))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "Read.*test\\.el" text))
        (should (string-match-p "hook changed tool result" text))
        (should-not (string-match-p "original result" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool result")
      (niwa-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "Original result" text))
        (should (string-match-p "original result" text))
        (should (string-match-p "Updated result" text))
        (should (string-match-p "updated result" text)))))

  :doc "full rerender preserves ignored tool result audit segments"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nupdated")
        (put-text-property start (point) 'gptel '(tool . "call_1")))
      (let ((start (point)))
        (insert
         (niwa--format-hook-audit-record
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
      (niwa-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "Read.*test\\.el" text))
        (should (string-match-p "hook changed tool result" text))
        (should-not (string-match-p "original result" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool result")
      (niwa-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "Original result" text))
        (should (string-match-p "original result" text))
        (should (string-match-p "Updated result" text))
        (should (string-match-p "updated result" text)))))

  :doc "decorates agent-result blocks inside assistant responses"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data
     data-buf
     (concat "Review update.\n"
             "<agent-result sender=\"/root/reviewer\" recipient=\"/root\" outcome=\"completed\">\n"
             "{\"findings\":[]}\n"
             "</agent-result>\n"
             "Final answer.\n")
     'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "Review update" text))
        (should (string-match-p "● Agent /root/reviewer finished" text))
        (should (string-match-p "{\"findings\":\\[\\]}" text))
        (should (string-match-p "Final answer" text))
        (should-not (string-match-p "<agent-result" text)))
      (goto-char (point-min))
      (search-forward "Review update")
      (goto-char (match-beginning 0))
      (niwa-view-toggle-section)
      (goto-char (point-min))
      (search-forward "Review update")
      (goto-char (match-beginning 0))
      (niwa-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "● Agent /root/reviewer finished" text))
        (should (string-match-p "{\"findings\":\\[\\]}" text))
        (should-not (string-match-p "<agent-result" text)))))

  :doc "renders completed Markdown source blocks as view panels"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data
     data-buf
     "Here is `code`:\n```emacs-lisp\n(message \"hi\")\n```\n"
     'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer data-buf
      (should (string-match-p
               "```emacs-lisp"
               (buffer-substring-no-properties (point-min) (point-max)))))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "Here is `code`" text))
        (should (string-match-p "emacs-lisp ⧉" text))
        (should (string-match-p "(message \"hi\")" text))
        (should-not (string-match-p "```emacs-lisp" text))
        (should-not (string-match-p "#\\+begin_src" text)))))

  :doc "keeps incomplete streaming Markdown source blocks raw"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data
     data-buf
     "```emacs-lisp\n(message \"hi\")\n"
     'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "```emacs-lisp" text))
        (should (string-match-p "(message \"hi\")" text)))))

  :doc "assistant prose file line reference is buttonized"
  (let* ((root (make-temp-file "niwa-view-response-line-" t))
         (file (file-name-concat root "niwa-session-persistence.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "response-line"
                     :root root :name "response-line"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (niwa-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (with-current-buffer view-buf
              (setq-local niwa--session session))
            (niwa-view-stream-test--insert-data
             data-buf
             "See niwa-session-persistence.el:187.\n"
             'response)
            (with-current-buffer data-buf
              (niwa-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "niwa-session-persistence.el:187")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'niwa-view-path)))
                (should (= 187 (button-get button 'niwa-view-line)))))))
      (delete-directory root t)))

  :doc "assistant inline code file line reference is buttonized"
  (let* ((root (make-temp-file "niwa-view-response-inline-line-" t))
         (file (file-name-concat root "file.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "response-inline-line"
                     :root root :name "response-inline-line"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "inline\n"))
          (niwa-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (with-current-buffer view-buf
              (setq-local niwa--session session))
            (niwa-view-stream-test--insert-data
             data-buf
             "See `file.el:42`.\n"
             'response)
            (with-current-buffer data-buf
              (niwa-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "file.el:42")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'niwa-view-path)))
                (should (= 42 (button-get button 'niwa-view-line)))))))
      (delete-directory root t)))

  :doc "assistant source block file line reference is not buttonized"
  (let* ((root (make-temp-file "niwa-view-response-src-line-" t))
         (file (file-name-concat root "file.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "response-src-line"
                     :root root :name "response-src-line"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "src\n"))
          (niwa-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (with-current-buffer view-buf
              (setq-local niwa--session session))
            (niwa-view-stream-test--insert-data
             data-buf
             "```emacs-lisp\nfile.el:42\n```\n"
             'response)
            (with-current-buffer data-buf
              (niwa-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "file.el:42")
              (should-not (button-at (match-beginning 0))))))
      (delete-directory root t)))

  :doc "expanded assistant response preserves file line buttons"
  (let* ((root (make-temp-file "niwa-view-response-expand-line-" t))
         (file (file-name-concat root "file.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "response-expand-line"
                     :root root :name "response-expand-line"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "expand\n"))
          (niwa-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (with-current-buffer view-buf
              (setq-local niwa--session session))
            (niwa-view-stream-test--insert-data
             data-buf
             "See file.el:42.\n"
             'response)
            (with-current-buffer data-buf
              (niwa-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "file.el:42")
              (goto-char (match-beginning 0))
              (niwa-view-toggle-section)
              (goto-char (point-min))
              (search-forward "See")
              (goto-char (match-beginning 0))
              (niwa-view-toggle-section)
              (goto-char (point-min))
              (search-forward "file.el:42")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'niwa-view-path)))
                (should (= 42 (button-get button 'niwa-view-line)))))))
      (delete-directory root t)))

  :doc "renders bracket indexing literally inside response code blocks"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data
     data-buf
     "```r\neval(f[[3]], df)\n```\n"
     'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "r ⧉" text))
        (should (string-match-p "eval(f\\[\\[3\\]\\], df)" text))
        (should-not (string-match-p "```r" text))
        (should-not (string-match-p "eval(f3, df)" text)))
      (let ((pos (save-excursion
                   (goto-char (point-min))
                   (when (search-forward "[[3]]" (niwa-view-transcript-tail) t)
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
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data
     data-buf
     "\n\n#+begin_tool (Read :file_path \"/tmp/test.el\")\n"
     nil)
    (niwa-view-stream-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nfile content\n"
     '(tool . "call_1"))
    (niwa-view-stream-test--insert-data data-buf "\n#+end_tool\n" nil)
    (niwa-view-stream-test--insert-data data-buf "Here is the file.\n" 'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let* ((text (buffer-substring-no-properties (point-min) (niwa-view-transcript-tail)))
             (you-count (cl-count-if (lambda (line) (string= line "You"))
                                     (split-string text "\n"))))
        (should (= 0 you-count))
        (should-not (string-match-p "#\\+begin_tool" text))
        (should (string-match-p "Read.*test\\.el" text))
        (should (string-match-p "Here is the file" text)))))

  :doc "permission retry keeps consecutive Bash handles distinct"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data
     data-buf
     "#+begin_tool (Bash :command \"rg TODO ..\")\n"
     nil)
    (niwa-view-stream-test--insert-data
     data-buf
     (concat "(:name \"Bash\" :args (:command \"rg TODO ..\"))\n\n"
             "Error: Filesystem authority required\n")
     '(tool . "call-1"))
    (niwa-view-stream-test--insert-data data-buf "#+end_tool\n" nil)
    (niwa-view-stream-test--insert-data
     data-buf
     "#+begin_tool (Bash :command \"pwd\")\n"
     nil)
    (niwa-view-stream-test--insert-data
     data-buf
     "(:name \"Bash\" :args (:command \"pwd\"))\n\nsuccess output\n"
     '(tool . "call-2"))
    (niwa-view-stream-test--insert-data data-buf "#+end_tool\n" nil)
    (niwa-view-stream-test--insert-data data-buf "Done.\n" 'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (= 2 (niwa-view-stream-test--count-substring
                      "Bash:" text))))
      (goto-char (point-min))
      (search-forward "Bash: pwd")
      (goto-char (match-beginning 0))
      (niwa-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "success output" text))
        (should-not (string-match-p "Filesystem authority required" text))
        (should-not (string-match-p "#\\+begin_tool\\|#\\+end_tool"
                                    text)))))

  :doc "complete same-id Bash blocks render as separate handles"
  (niwa-view-stream-test--with-buffers
    ;; Each block is its own row; three of them stay below the fold.
    (let ((niwa-view-tool-group-collapse-threshold 3))
      (dolist (call '(("test 1 = 2" . "<bash-execution exit_code=\"1\" outcome=\"false\"/>")
                      ("rg missing niwa.el" . "<bash-execution exit_code=\"1\" outcome=\"no-match\"/>")
                      ("diff /dev/null niwa.el" . "Error: Filesystem authority required")))
        (niwa-view-stream-test--insert-data
         data-buf
         (format "#+begin_tool (Bash :command %S)\n" (car call))
         nil)
        (niwa-view-stream-test--insert-data
         data-buf
         (format "(:name \"Bash\" :args (:command %S))\n\n%s\n"
                 (car call) (cdr call))
         '(tool . "duplicate-call-id"))
        (niwa-view-stream-test--insert-data data-buf "#+end_tool\n" nil))
      (with-current-buffer data-buf
        (niwa-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should (= 3 (niwa-view-stream-test--count-substring
                        "Bash:" text)))
          (dolist (command '("test 1 = 2"
                             "rg missing niwa.el"
                             "diff /dev/null niwa.el"))
            (should (string-match-p (regexp-quote command) text)))))))

  :doc "restored stale tool bounds render and expand without garbled fragments"
  (niwa-view-stream-test--with-buffers
    (niwa-tool-register
     (niwa-tool--create
      :name "RecoverRead"
      :category "niwa"
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
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "RecoverRead: /tmp/f" text))
        (should (string-match-p
                 "Fixed the byte-compilation warning" text))
        (should-not (string-match-p "#\\+begin_tool\\|#\\+end_tool\\|n_tool" text))
        (should-not (string-match-p "^You$\\|Thinking" text)))
      (goto-char (point-min))
      (search-forward "RecoverRead: /tmp/f")
      (goto-char (match-beginning 0))
      (niwa-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "file body" text))
        (should-not (string-match-p
                     "#\\+begin_tool\\|#\\+end_tool\\|n_tool" text)))))

  :doc "renders repeated read calls as individual tool rows"
  (let ((niwa-view-tool-group-collapse-threshold 0))
    (niwa-view-stream-test--with-buffers
      (dotimes (i 4)
        (niwa-view-stream-test--insert-data
         data-buf
         (format "(:name \"Read\" :args (:file_path \"/tmp/file%d.el\"))\n\ncontent %d\n"
                 i i)
         `(tool . ,(format "call_%d" i))))
      (with-current-buffer data-buf
        (niwa-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should-not (string-match-p "Reading 4 files" text))
          (dolist (file '("file0.el" "file1.el" "file2.el" "file3.el"))
            (should (string-match-p
                     (format "Read: .*%s" (regexp-quote file))
                     text)))))))

  :doc "renders user turn when in-flight marker outlives echoed user block"
  (niwa-view-stream-test--with-buffers
    (let (start end)
      (niwa-view-stream-test--insert-data data-buf "First\n" nil)
      (niwa-view-stream-test--insert-data data-buf "First response.\n" 'response)
      (with-current-buffer data-buf
        (niwa-view-stream-render-response (point-min) (point-max))
        (setq start (point-max)))
      (niwa-view-stream-test--insert-data data-buf "Second\n" nil)
      (niwa-view-stream-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer data-buf
        (setq end (point-max)))
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker) nil))
        (setq niwa-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (niwa-view-stream-render-response start end))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should (string-match-p "Second" text))
          (should (string-match-p "Second response" text))))))

  :doc "does not duplicate visible send-path user echo after flag is consumed"
  (niwa-view-stream-test--with-buffers
    (let (start end)
      (with-current-buffer data-buf
        (setq start (point-max)))
      (niwa-view-stream-test--insert-data data-buf "Second\n" nil)
      (niwa-view-stream-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer data-buf
        (setq end (point-max)))
      (with-current-buffer view-buf
        (niwa-view--insert-user-message "Second")
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker) nil))
        (setq niwa-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (niwa-view-stream-render-response start end))
      (with-current-buffer view-buf
        (let* ((text (buffer-substring-no-properties
                      (point-min) (niwa-view-transcript-tail)))
               (you-count (cl-count-if (lambda (line) (string= line "You"))
                                       (split-string text "\n"))))
          (should (= 1 you-count))
          (should (string-match-p "Second response" text))))))

  :doc "final response includes reasoning before hook start"
  (niwa-view-stream-test--with-buffers
    (let (data-turn-start response-start response-end)
      (niwa-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point) nil)))
      (niwa-view-stream-test--insert-data
       data-buf
       "(:name \"Bash\" :args (:command \"true\"))\n\nok\n"
       '(tool . "call_1"))
      (niwa-view-stream-test--insert-data
       data-buf
       "#+begin_reasoning\nroot cause thought\n#+end_reasoning\n"
       'ignore)
      (with-current-buffer data-buf
        (setq response-start (point)))
      (niwa-view-stream-test--insert-data data-buf "Final answer.\n" 'response)
      (with-current-buffer data-buf
        (setq response-end (point)))
      (with-current-buffer view-buf
        (niwa-view--insert-user-message "Prompt")
        (setq niwa-view--data-turn-start data-turn-start)
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker) nil))
        (setq niwa-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (niwa-view-stream-render-response response-start response-end))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should (string-match-p "Bash" text))
          (should (string-match-p "Thinking" text))
          (should (string-match-p "Final answer" text))
          (should (string-match-p "root cause thought" text)))
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (niwa-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should-not (string-match-p "root cause thought" text))))))

  :doc "final response widening does not duplicate visible user echo"
  (niwa-view-stream-test--with-buffers
    (let (data-turn-start response-start response-end)
      (niwa-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (niwa-view-stream-test--insert-data
       data-buf
       "#+begin_reasoning\nlate thought\n#+end_reasoning\n"
       'ignore)
      (with-current-buffer data-buf
        (setq response-start (point)))
      (niwa-view-stream-test--insert-data data-buf "Final answer.\n" 'response)
      (with-current-buffer data-buf
        (setq response-end (point)))
      (with-current-buffer view-buf
        (niwa-view--insert-user-message "Prompt")
        (setq niwa-view--data-turn-start data-turn-start)
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker) nil))
        (setq niwa-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (niwa-view-stream-render-response response-start response-end))
      (with-current-buffer view-buf
        (let* ((text (buffer-substring-no-properties
                      (point-min) (niwa-view-transcript-tail)))
               (you-count (cl-count-if (lambda (line) (string= line "You"))
                                       (split-string text "\n"))))
          (should (= 1 you-count))
          (should (string-match-p "Thinking" text))
          (should (string-match-p "Final answer" text))))))

  :doc "second-turn incremental render stays above interaction zone without duplication"
  (niwa-view-stream-test--with-buffers
    (let (data-turn-start)
      (niwa-view-stream-test--insert-data data-buf "First\n" nil)
      (niwa-view-stream-test--insert-data data-buf "First response.\n" 'response)
      (with-current-buffer data-buf
        (niwa-view-stream-render-response (point-min) (point-max))
        (goto-char (point-max))
        (insert "\n\nSecond\n")
        (setq data-turn-start (copy-marker (point) nil)))
      (niwa-view-stream-test--insert-data data-buf "Partial response.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view--interaction-register
         (list :kind 'permission
               :id 'permission
               :count 1
               :body "\npermission\n"
               :keymap (make-sparse-keymap)
               :help-echo "Permission"
               :entry 'permission-entry
               :activate #'ignore))
        (setq niwa-view--data-turn-start data-turn-start)
        (setq niwa-view--in-flight-turn-start
              (niwa-view--insert-user-message "Second"))
        (niwa-view--start-spinner "Thinking...")
        (niwa-view-render-live-update data-buf)
        (niwa-view-render-live-update data-buf)
        (let* ((text (buffer-substring-no-properties
                      (point-min) (niwa-view-transcript-tail)))
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
  (niwa-view-stream-test--with-buffers
    (let (data-turn-start
          (changes 0))
      (niwa-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (niwa-view-stream-test--insert-data data-buf "Partial response.\n" 'response)
      (with-current-buffer view-buf
        (setq niwa-view--data-turn-start data-turn-start)
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view--history-insertion-marker) nil))
        (setq niwa-view--pending-tool-calls
              (list (list 'read "Read" nil)))
        (niwa-view-stream-test--draw-running-rows)
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (niwa-view-render-live-update data-buf)
        (should (= 0 changes))
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should (string-match-p "Partial response" text))
          ;; The redraw replaced the running row instead of adding one.
          (should (= 1 (niwa-view-stream-test--count-substring
                        "● Read" text)))))))

  :doc "does not duplicate the original user turn after mailbox insertion"
  (niwa-view-stream-test--with-buffers
    (let (data-turn-start)
      (niwa-view-stream-test--insert-data data-buf "Prompt\n" nil)
      (with-current-buffer data-buf
        ;; Simulate the real send-path marker landing inside the nil
        ;; user-property run, which `--extract-segments' expands
        ;; backward to the beginning of the prompt.
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (niwa-view-stream-test--insert-data data-buf "Thinking\n" 'ignore)
      (niwa-view-stream-test--insert-data data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view--insert-user-message "Prompt")
        (let ((inhibit-read-only t))
          (goto-char (niwa-view-transcript-tail-marker))
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
          (unwind-protect
              (progn
                (insert "✉ from explorer\nhi\n\n")
                (setq niwa-view--in-flight-turn-start
                      (copy-marker (point) nil))
                (insert "Assistant\nold live tail\n")
                (set-marker niwa-view--status-marker (point))
                (set-marker niwa-view--interaction-marker (point))
                (set-marker (niwa-view-transcript-tail-marker) (point)))
            (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil)))
        (setq niwa-view--data-turn-start data-turn-start)
        (setq niwa-view--user-pre-rendered nil)
        (niwa-view-render-live-update data-buf)
        (let* ((text (buffer-substring-no-properties
                      (point-min) (niwa-view-transcript-tail)))
               (you-count (cl-count-if (lambda (line) (string= line "You"))
                                       (split-string text "\n"))))
          (should (= 1 you-count))
          (should (string-match-p "Assistant text" text))))))

  :doc "renders thinking blocks expanded with a labelled summary"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "line 1\nline 2\nline 3\n" 'ignore)
    (niwa-view-stream-test--insert-data data-buf "The answer is 42.\n" 'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "Thinking" text))
        (should (string-match-p "line 1" text))
        (should (string-match-p "42" text)))))

  :doc "trims contaminated thinking source to structural reasoning block"
  (niwa-view-stream-test--with-buffers
    (let (reasoning-start)
      (niwa-view-stream-test--insert-data
       data-buf
       "(:name \"Bash\" :args (:command \"true\"))\n\nok\n"
       '(tool . "call_1"))
      (niwa-view-stream-test--insert-data
       data-buf
       "**Output observed:**\n  `ok`\n\nVERDICT: PASS\n</agent-result>\n"
       nil)
      (with-current-buffer data-buf
        (setq reasoning-start (point)))
      (niwa-view-stream-test--insert-data
       data-buf
       "#+begin_reasoning\nreal thought\n#+end_reasoning\n"
       'ignore)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (let ((source (get-text-property (point) 'niwa-view-source))
              (line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (should (= (marker-position (car source)) reasoning-start))
          (should (string-match-p "Thinking\\.\\.\\. (1 lines)" line)))
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should (string-match-p "real thought" text))
          (should-not (string-match-p "VERDICT: PASS" text))
          (should-not (string-match-p "</agent-result>" text)))
        (niwa-view-toggle-section)
        (should-not (string-search "real thought" (buffer-string))))))

  :doc "hides complete proposed-plan protocol outside Goal planning"
  (niwa-view-stream-test--with-buffers
    (let ((session (niwa-session--create
                    :name "test"
                    :workspace nil
                    :permission-mode 'ask)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session))
      (niwa-view-stream-test--insert-data
       data-buf
       "Normal\n<proposed_plan>\n# Plan\n</proposed_plan>\nAfter\n"
       'response)
      (with-current-buffer data-buf
        (niwa-view-stream-render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should-not (string-match-p "<proposed_plan>" text))
          (should-not (string-match-p "# Plan" text))
          (should (string-match-p "Normal" text))
          (should (string-match-p "After" text))))))

  :doc "keeps historical proposed-plan protocol hidden after Goal planning"
  (let ((tmp (make-temp-file "niwa-view-plan-" t)))
    (unwind-protect
        (niwa-view-stream-test--with-buffers
          (let* ((plan-path (file-name-concat tmp "local" "plans" "current.md"))
                 (session (niwa-session--create
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
              (setq-local niwa--session session))
            (with-current-buffer view-buf
              (setq-local niwa--session session))
            (niwa-view-stream-test--insert-data
             data-buf
             "Normal\n<proposed_plan>\n# Old plan\n</proposed_plan>\nAfter\n"
             'response)
            (with-current-buffer data-buf
              (niwa-view-stream-render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (let ((text (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))
                (should-not (string-match-p "<proposed_plan>" text))
                (should-not (string-match-p "# Old plan" text))
                (should (string-match-p "Normal" text))
                (should (string-match-p "After" text))))))
      ;; Killing the view buffers records telemetry under the session's
      ;; save path, so the directory goes only after them.
      (delete-directory tmp t)))

  :doc "renders an ignored PROMPT drawer as a collapsed user section"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "Change alpha\n" nil)
    (niwa-view-stream-test--insert-data
     data-buf
     ":PROMPT:\n## TASK\nFull hidden prompt.\n:END:\n"
     'ignore)
    (niwa-view-stream-test--insert-data data-buf "Done.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "Change alpha" text))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "Full hidden prompt" text))
        (should (string-match-p "Done" text)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (niwa-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "Full hidden prompt" expanded)))))

  :doc "renders inline skills as compact invocation with collapsed prompt"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data
     data-buf
     "You are helping with this user request:\n\nSay hi!\n"
     nil)
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert
         (niwa-tool-render-data-format
          '(:kind inline-skill
                  :name "emacs-context-snapshot"
                  :arguments "Say hi!"
                  :display-text
                  "$emacs-context-snapshot\nSay hi!"
                  :prompt
                  "You are helping with this user request:\n\nSay hi!\n")))
        (put-text-property start (point) 'gptel 'ignore)))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p
                 "\\$emacs-context-snapshot\nSay hi!"
                 text))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "You are helping with this user request"
                                    text)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (niwa-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "You are helping with this user request"
                                expanded)))))

  :doc "renders nil-property inline skill metadata as compact invocation"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data
     data-buf
     (concat
      "# Green Loop\n\nRun the loop.\n\nARGUMENTS: current changes"
      (niwa-tool-render-data-format
       '(:kind inline-skill
               :name "green-loop"
               :arguments "current changes"
               :display-text "$green-loop current changes"
               :prompt
               "# Green Loop\n\nRun the loop.\n\nARGUMENTS: current changes")))
     nil)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "\\$green-loop current changes" text))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "# Green Loop" text))
        (should-not (string-match-p "Run the loop" text))
        (should-not (string-match-p "niwa-render-data" text)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (niwa-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "# Green Loop" expanded))
        (should (string-match-p "Run the loop" expanded)))))

  :doc "request summary before inline skill does not absorb user turn"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data data-buf "Previous answer.\n" 'response)
    (niwa-view-stream-test--insert-data
     data-buf
     (niwa-tool-render-data-format
      '(:kind request-summary :elapsed-seconds 120))
     'ignore)
    (niwa-view-stream-test--insert-data
     data-buf
     "# Green Loop\n\nRun the loop.\n\nARGUMENTS: current changes"
     nil)
    (niwa-view-stream-test--insert-data
     data-buf
     (niwa-tool-render-data-format
      '(:kind inline-skill
              :name "green-loop"
              :arguments "current changes"
              :display-text "$green-loop current changes"))
     'ignore)
    (niwa-view-stream-test--insert-data data-buf "Restarting checks.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "Previous answer" text))
        (should (string-match-p "Worked for 2m" text))
        (should (string-match-p "You\n\\$green-loop current changes" text))
        (should (string-match-p "Restarting checks" text))
        (should-not (string-match-p "# Green Loop" text))
        (should-not (string-match-p "niwa-render-data" text)))))

  :doc "expanded inline skill prompt omits saved org property drawer"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--insert-data
     data-buf
     ":PROPERTIES:\n:GPTEL_PRESET: niwa-implement\n:GPTEL_MODEL: gpt-5.5\n:GPTEL_BOUNDS: ((ignore (1 2)))\n:END:\n\nSkill prompt body\n\nVisible model prompt.\n"
     nil)
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert
         (niwa-tool-render-data-format
          '(:kind inline-skill
                  :name "green-loop"
                  :arguments "commits a b"
                  :display-text
                  "$green-loop\ncommits a b"
                  :prompt
                  "Skill prompt body\n\nVisible model prompt.\n")))
        (put-text-property start (point) 'gptel 'ignore)))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "Prompt")
      (niwa-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) (niwa-view-transcript-tail))))
        (should (string-match-p "Visible model prompt" expanded))
        (should-not (string-match-p ":PROPERTIES:" expanded))
        (should-not (string-match-p "GPTEL_BOUNDS" expanded)))))

  :doc "target-native response link opens only through its session target"
  (let* ((host "view-link-target")
         (other-host "view-link-other")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-view-link-target-" t)))
         (remote-root (format "/niwamock:%s:%s" host local-root))
         (native-file (file-name-concat local-root "src" "main.el"))
         (remote-file (concat (file-remote-p remote-root)
                              native-file))
         (client-file (make-temp-file
                       "niwa-view-link-client-" nil ".el" "client\n"))
         (client-path (concat "/:" client-file))
         (other-path (format "/niwamock:%s:%s" other-host client-file))
         opened)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host other-host)
          (make-directory (file-name-directory remote-file) t)
          (write-region "target\n" nil remote-file nil 'silent)
          (let* ((workspace
                  (niwa-workspace--create
                   :type 'project :id "view-link-target"
                   :root remote-root :name "view-link-target"))
                 (session (niwa-session-create "main" workspace)))
            (niwa-view-stream-test--with-buffers
              (dolist (buffer (list data-buf view-buf))
                (with-current-buffer buffer
                  (setq-local niwa--session session)))
              (niwa-view-stream-test--insert-data
               data-buf "Show paths\n" nil)
              (niwa-view-stream-test--insert-data
               data-buf
               (format "Open %s\nIgnore %s\nIgnore %s\n"
                       native-file other-path client-path)
               'response)
              (with-current-buffer data-buf
                (niwa-view-stream-render-response
                 (point-min) (point-max)))
              ;; Decoration runs from a redraw timer and so never stats the
              ;; target; a remote path is linked only once the verification
              ;; pass has recorded it.  Drive that pass and redraw here
              ;; rather than waiting on its idle timer.
              (with-current-buffer view-buf
                (cl-letf (((symbol-function 'niwa-transport-run-when-idle)
                           (lambda (_key _path thunk &optional _on-cancel)
                             (funcall thunk)
                             t)))
                  (niwa-view--verify-paths view-buf)))
              (with-current-buffer data-buf
                (niwa-view-stream-render-response
                 (point-min) (point-max)))
              (with-current-buffer view-buf
                (goto-char (point-min))
                (search-forward native-file)
                (let ((button (button-at (match-beginning 0))))
                  (should button)
                  (should (equal remote-file
                                 (button-get button 'niwa-view-path)))
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
  (niwa-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (insert "ask\n\nreply\n"))
    (niwa-view-stream-test--with-visible-view
      (niwa-view-stream-begin-turn
       (point-min)
       (with-current-buffer data-buf (copy-marker (point-min) nil)))
      (setq niwa-view--pending-tool-calls
            (list (list :id "call-1" :name "Read")))
      (should (timerp niwa-view--spinner-timer))
      (let (warning later-ran)
        (niwa-test--with-captured-diagnostics warning
          (cl-letf (((symbol-function 'niwa-view--render-live-region)
                     (lambda (&rest _) (error "Projection failed"))))
            (with-current-buffer data-buf
              ;; Through the hook, because the point of not signalling is
              ;; that the observers after this one still run.
              (let ((gptel-post-response-functions
                     (list #'niwa-view-stream-render-response
                           (lambda (&rest _) (setq later-ran t)))))
                (run-hook-with-args 'gptel-post-response-functions
                                    (point-min) (point-max))))))
        (should (string-match-p "Projection failed" warning))
        (should later-ran))
      (should-not niwa-view--pending-tool-calls)
      (should-not (timerp niwa-view--spinner-timer))
      (should-not niwa-view--in-flight-turn-start)
      (should-not niwa-view--data-turn-start)
      (should (eq 'full niwa-view--pending-render-kind))))

  :doc "releases the turn when stopping the progress row fails"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (insert "ask\n\nreply\n"))
    (niwa-view-stream-test--with-visible-view
      (niwa-view-stream-begin-turn
       (point-min)
       (with-current-buffer data-buf (copy-marker (point-min) nil)))
      (setq niwa-view--pending-tool-calls
            (list (list :id "call-1" :name "Read")))
      (should (timerp niwa-view--spinner-timer))
      (let (warning)
        (niwa-test--with-captured-diagnostics warning
          (cl-letf (((symbol-function 'niwa-view--stop-request-progress)
                     (lambda (&rest _) (error "Zone reconcile failed"))))
            (with-current-buffer data-buf
              (niwa-view-stream-render-response
               (point-min) (point-max)))))
        (should (string-match-p "Zone reconcile failed" warning)))
      (should-not niwa-view--pending-tool-calls)
      (should-not (timerp niwa-view--spinner-timer))
      (should-not niwa-view--in-flight-turn-start)
      (should-not niwa-view--data-turn-start))))

(niwa-deftest niwa-view-animation-frozen-metadata
  (:doc "Elapsed metadata redraws never move disabled or zero-fps glyphs.")
  (dolist (setting '(disabled zero-fps))
    (niwa-view-stream-test--with-buffers
      (niwa-view-stream-test--with-visible-view
        (let ((niwa-view-spinner-style 'ascii)
              (niwa-view-tool-spinner-style 'ascii)
              (niwa-view-spinner-animate (eq setting 'zero-fps))
              (niwa-view-spinner-power-policy
               (if (eq setting 'zero-fps) 'save 'full))
              (niwa-view-spinner-battery-framerate 0)
              (seconds 0))
          (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                     (lambda () seconds)))
            (niwa-view--start-spinner "Working...")
            (let ((initial (niwa-view-stream-test--rail-activity)))
              (should (= niwa-view--spinner-timer-period 1.0))
              (dolist (elapsed '(3 6))
                (setq seconds elapsed
                      niwa-view--spinner-start-time
                      (time-subtract (current-time) (seconds-to-time elapsed))
                      niwa-view--spinner-last-second nil)
                (niwa-view--spinner-tick)
                (should (equal initial
                               (niwa-view-stream-test--rail-activity)))
                (should (string-match-p
                         (format " · %ds" elapsed) (niwa-view-test--rail-text)))))))))))

(niwa-deftest niwa-view-animation-freeze-resume-phase
  (:doc "Zero fps holds the last displayed sample, not the next clock phase.")
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full)
            (niwa-view-spinner-battery-framerate 0)
            (seconds 0.119))
        (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                   (lambda () seconds)))
          (niwa-view--start-spinner "Working...")
          (let ((phase niwa-view--spinner-phase-start)
                (frame (niwa-view-stream-test--rail-activity)))
            (setq seconds 0.241)
            ;; A focus/scroll rearm at the new clock phase does not render a
            ;; new sample and must not overwrite the last displayed one.
            (niwa-view--start-spinner-timer)
            (should (equal frame
                           (niwa-view-stream-test--rail-activity)))
            (setq niwa-view-spinner-power-policy 'save)
            (niwa-view--refresh-animation-options)
            (should (equal frame
                           (niwa-view-stream-test--rail-activity)))
            (setq seconds 3.48
                  niwa-view--spinner-start-time
                  (time-subtract (current-time) (seconds-to-time 3))
                  niwa-view--spinner-last-second nil)
            (niwa-view--spinner-tick)
            (should (equal frame
                           (niwa-view-stream-test--rail-activity)))
            (setq niwa-view-spinner-power-policy 'full)
            (niwa-view--refresh-animation-options)
            (should (equal (niwa-view-animation-frame
                            'ascii "Working..." seconds 'niwa-view-spinner
                            (selected-frame))
                           (niwa-view-stream-test--rail-activity)))
            ;; A real rendered frame supersedes the old frozen sample before
            ;; the next transition, without a metadata/second redraw.
            (niwa-view--spinner-tick)
            (let ((resumed (niwa-view-stream-test--rail-activity)))
              (setq niwa-view-spinner-power-policy 'save
                    seconds 3.601)
              (niwa-view--start-spinner-timer)
              (setq niwa-view--spinner-last-second nil)
              (niwa-view--spinner-tick)
              (should (equal resumed (niwa-view-stream-test--rail-activity))))
            (should (= phase niwa-view--spinner-phase-start))))))))

;; A freeze mid-sweep used to keep the faded band for the whole pause.
(niwa-deftest niwa-view-animation-freeze-rests
  (:doc "Freezing a color label or tool row mid-motion repaints it at rest.")
  (dolist (case '((shimmer) (breathe) (shimmer . native)))
    (let ((style (car case))
          (niwa-view-animation--cache nil))
      (cl-letf (((symbol-function 'niwa-view-animation--colors)
                 (lambda (_face _frame) (cons "#ffffff" "#000000"))))
        (niwa-view-stream-test--with-buffers
          (niwa-view-stream-test--with-visible-view
            (let ((niwa-view-spinner-style style)
                  (niwa-view-tool-spinner-style 'shimmer)
                  (niwa-view-native-enabled nil)
                  (niwa-view-spinner-power-policy 'full)
                  (niwa-view-spinner-battery-framerate 0)
                  (niwa-view--pending-tool-calls
                   '(("call-1" "Read" nil)))
                  ;; Inside the first shimmer sweep and the breathe fade.
                  (seconds 1.0))
              (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                         (lambda () seconds)))
                (niwa-view--start-spinner "Working...")
                (niwa-view-stream-test--draw-running-rows)
                (niwa-view--spinner-tick)
                (let* ((tool (marker-position
                              (caar niwa-view--spinner-tool-targets)))
                       (moving-label (niwa-view-stream-test--rail-activity))
                       (moving-tool (get-text-property tool 'display))
                       (rest-label (niwa-view-animation-frame
                                    style "Working..."
                                    (niwa-view-animation-rest-seconds style 1.0)
                                    'niwa-view-spinner (selected-frame)))
                       (rest-tool (niwa-view-animation-frame
                                   'shimmer (niwa-view--tool-animation-label
                                             (car niwa-view--spinner-tool-targets))
                                   (niwa-view-animation-rest-seconds 'shimmer 1.0)
                                   'niwa-view-ephemeral (selected-frame))))
                  (should-not (equal-including-properties moving-label rest-label))
                  (should-not (equal-including-properties moving-tool rest-tool))
                  ;; A natively presented row keeps sampling mid-sweep; the
                  ;; freeze must not take that sample over its rest.
                  (when (eq (cdr case) 'native)
                    (setq niwa-view--native-animation-targets
                          (list (car niwa-view--spinner-tool-targets))))
                  (cl-letf (((symbol-function 'niwa-view-native-sample)
                             (lambda (_target) 1.0)))
                    (setq niwa-view-spinner-power-policy 'save)
                    (niwa-view--refresh-animation-options))
                  (should (equal-including-properties
                           rest-label (niwa-view-stream-test--rail-activity)))
                  (should (equal-including-properties
                           rest-tool (get-text-property tool 'display)))
                  ;; Resuming continues from the clock, not from rest.
                  (setq niwa-view-spinner-power-policy 'full)
                  (niwa-view--refresh-animation-options)
                  (niwa-view--spinner-tick)
                  (should (equal-including-properties
                           moving-label (niwa-view-stream-test--rail-activity)))
                  (should (equal-including-properties
                           moving-tool (get-text-property tool 'display))))))))))))

(niwa-deftest niwa-view--animation-rest-seconds
  (:doc "Rests color samples only, including before insertion and after release.")
  (with-temp-buffer
    (insert "Working...")
    (let ((target (cons (copy-marker (point-min)) (copy-marker (point-max))))
          (dead (cons (make-marker) (make-marker))))
      (dolist (color '(nil t))
        (put-text-property (point-min) (point-max) 'display
                           (if color (propertize "Working..." 'face '(:foreground "white"))
                             "- Working..."))
        (cl-letf (((symbol-function 'niwa-view-animation-color-available-p)
                   (lambda (_style _label _face _frame) color)))
          (dolist (span (list target nil dead))
            (dolist (style '(shimmer breathe ascii))
              (should (= (niwa-view--animation-rest-seconds
                          style 1.0 span 'niwa-view-spinner)
                         (if color (niwa-view-animation-rest-seconds style 1.0)
                           1.0)))))))
      ;; Moving or hiding the window does not change what was last drawn.
      (put-text-property (point-min) (point-max) 'display "- Working...")
      (cl-letf (((symbol-function 'niwa-view-animation-color-available-p)
                 (lambda (&rest _) (ert-fail "Probed an unrelated frame"))))
        (should (= 1.0 (niwa-view--animation-rest-seconds
                        'shimmer 1.0 target 'niwa-view-spinner))))
      (let ((niwa-view--native-animation-targets (list target)))
        (should (= (niwa-view-animation-rest-seconds 'shimmer 1.0)
                   (niwa-view--animation-rest-seconds
                    'shimmer 1.0 target 'niwa-view-spinner)))))))

(niwa-deftest niwa-view-animation-freeze-color-fallback
  (:doc "Freezing a color style keeps its displayed fallback glyph and draft.")
  (dolist (style '(shimmer breathe))
    (let ((niwa-view-animation--cache nil))
      (cl-letf (((symbol-function 'niwa-view-animation--colors)
                 (lambda (_face _frame) nil)))
        (niwa-view-stream-test--with-buffers
          (niwa-view-stream-test--with-visible-view
            (let ((niwa-view-spinner-style style)
                  (niwa-view-tool-spinner-style 'shimmer)
                  (niwa-view-native-enabled nil)
                  (niwa-view-spinner-power-policy 'full)
                  (niwa-view-spinner-battery-framerate 0)
                  (niwa-view--pending-tool-calls
                   '(("call-1" "Read" nil))))
              (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                         (lambda () 1.0)))
                (niwa-view-test--insert-composer-draft
                 "> first line\nsecond line" 4)
                (niwa-view--start-spinner "Working...")
                (niwa-view-stream-test--draw-running-rows)
                (niwa-view--spinner-tick)
                (let* ((tool (marker-position
                              (caar niwa-view--spinner-tool-targets)))
                       (label-glyph (niwa-view-stream-test--rail-activity))
                       (tool-glyph (get-text-property tool 'display)))
                  (setq niwa-view-spinner-power-policy 'save)
                  (niwa-view--refresh-animation-options)
                  (should (equal label-glyph (niwa-view-stream-test--rail-activity)))
                  (should (equal tool-glyph (get-text-property tool 'display)))
                  (should (equal "> first line\nsecond line"
                                 (niwa-view-composer-draft-text)))
                  (should (= 4 (niwa-view-stream-test--draft-offset))))))))))))

(niwa-deftest niwa-view-animation-freeze-tool-row
  (:doc "Power changes keep the last displayed tool glyph, not its initial frame.")
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-tool-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full)
            (niwa-view-spinner-battery-framerate 0)
            (niwa-view--pending-tool-calls
             '(("call-1" "Read" nil)))
            (seconds 0.72))
        (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                   (lambda () seconds)))
          (niwa-view--start-spinner "Working...")
          (niwa-view-stream-test--draw-running-rows)
          (niwa-view--spinner-tick)
          (let* ((target (car niwa-view--spinner-tool-targets))
                 (display (get-text-property
                           (marker-position (car target)) 'display)))
            (should (equal display "/ "))
            (setq seconds 0.961
                  niwa-view-spinner-power-policy 'save)
            (niwa-view--refresh-animation-options)
            (should (equal display (get-text-property
                                    (marker-position (car target)) 'display)))
            (setq seconds 3.6
                  niwa-view--spinner-start-time
                  (time-subtract (current-time) (seconds-to-time 3))
                  niwa-view--spinner-last-second nil)
            (niwa-view--spinner-tick)
            (should (equal display (get-text-property
                                    (marker-position (car target)) 'display)))
            (setq niwa-view--pending-tool-calls
                  '(("call-1" "Read" nil)
                    ("call-2" "Grep" nil)))
            (niwa-view-stream-test--draw-running-rows)
            (should (equal display
                           (get-text-property
                            (marker-position
                             (caar niwa-view--spinner-tool-targets))
                            'display)))
            (let ((second-display
                   (get-text-property
                    (marker-position
                     (car (cadr niwa-view--spinner-tool-targets)))
                    'display)))
              (setq niwa-view--pending-tool-calls
                    '(("call-2" "Grep" nil)))
              (niwa-view-stream-test--draw-running-rows)
              (should (equal second-display
                             (get-text-property
                              (marker-position
                               (caar niwa-view--spinner-tool-targets))
                              'display))))
            ;; A new style redraws the live turn's running rows.
            (setq niwa-view--in-flight-turn-start
                  (copy-marker (niwa-view--history-insertion-marker))
                  niwa-view--data-turn-start
                  (with-current-buffer data-buf (copy-marker (point-max))))
            (setq niwa-view-tool-spinner-style 'dots)
            (niwa-view--refresh-animation-options)
            (should (eq 'incremental niwa-view--pending-render-kind))
            (niwa-view--flush-scheduled-render view-buf t)
            (should (equal "*... "
                           (get-text-property
                            (marker-position
                             (caar niwa-view--spinner-tool-targets))
                            'display)))))))))

(niwa-deftest niwa-view-animation-freeze-independent-tool-phase
  (:doc "A paused tool keeps its own displayed phase on rearm and glyph fallback.")
  (dolist (main '(none static))
    (dolist (style '(braille dots))
      (dolist (freeze '(global battery))
        (let ((seconds 0.65)
              (supported t)
              (niwa-view-animation--dots-cache nil))
          (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                     (lambda () seconds))
                    ((symbol-function 'char-displayable-p)
                     (lambda (char &optional _frame)
                       (or supported (not (memq char '(?⠋ ?● ?·)))))))
            (niwa-view-stream-test--with-buffers
              (niwa-view-stream-test--with-visible-view
                (let ((niwa-view-spinner-style 'static)
                      (niwa-view-tool-spinner-style style)
                      (niwa-view-spinner-power-policy 'full)
                      (niwa-view-spinner-battery-framerate 0)
                      (niwa-view-spinner-animate t)
                      (niwa-view--pending-tool-calls
                       '(("call-1" "Read" nil))))
                  (when (eq main 'static)
                    (niwa-view--start-spinner "Working..."))
                  (niwa-view-stream-test--draw-running-rows)
                  (niwa-view--spinner-tick)
                  (let* ((target (car niwa-view--spinner-tool-targets))
                         (position (marker-position (car target)))
                         (display (get-text-property position 'display)))
                    (should (equal-including-properties
                             display
                             (niwa-view-animation-frame
                              style "" 0.65 'niwa-view-ephemeral
                              (niwa-view--animation-target-frame
                               target (eq style 'dots)))))
                    (setq seconds 1.1)
                    (if (eq freeze 'global)
                        (setq niwa-view-spinner-animate nil)
                      (setq niwa-view-spinner-power-policy 'save))
                    (niwa-view--start-spinner-timer)
                    (when (eq main 'none)
                      (should-not niwa-view--spinner-timer))
                    (niwa-view--resume-on-window-change (selected-window))
                    (should (equal-including-properties
                             display (get-text-property position 'display)))
                    (setq supported nil)
                    (run-hook-with-args 'enable-theme-functions
                                        'niwa-test-theme)
                    (should (equal-including-properties
                             (niwa-view-animation-frame
                              style "" 0.65 'niwa-view-ephemeral
                              (niwa-view--animation-target-frame
                               target (eq style 'dots)))
                             (get-text-property position 'display)))
                    (setq supported t)
                    (niwa-view--resume-on-window-change (selected-window))
                    (should (equal-including-properties
                             display (get-text-property position 'display)))
                    (setq niwa-view--pending-tool-calls
                          '(("call-1" "Read" nil)
                            ("call-2" "Grep" nil)))
                    (niwa-view-stream-test--draw-running-rows)
                    (let* ((first (car niwa-view--spinner-tool-targets))
                           (second (cadr niwa-view--spinner-tool-targets)))
                      (should (equal-including-properties
                               display (get-text-property
                                        (marker-position (car first)) 'display)))
                      (niwa-view--resume-on-window-change
                       (selected-window))
                      (should (equal-including-properties
                               display (get-text-property
                                        (marker-position (car first)) 'display)))
                      (should (equal-including-properties
                               (niwa-view-animation-frame
                                style "" 0 'niwa-view-ephemeral
                                (niwa-view--animation-target-frame
                                 second (eq style 'dots)))
                               (get-text-property
                                (marker-position (car second)) 'display))))
                    (when (eq main 'none)
                      (should-not niwa-view--spinner-timer))))))))))))

(niwa-deftest niwa-view-animation-incremental-tool-phase
  (:doc "An incremental transcript projection retains a frozen tool sample.")
  (dolist (style '(ascii braille dots))
    (niwa-view-stream-test--with-buffers
      (niwa-view-stream-test--with-visible-view
        (let ((seconds 0.72)
              (niwa-view-spinner-style 'static)
              (niwa-view-tool-spinner-style style)
              (niwa-view-spinner-power-policy 'full)
              (niwa-view-spinner-battery-framerate 0)
              (niwa-view--pending-tool-calls
               '(("call-1" "Read" nil))))
          (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                     (lambda () seconds)))
            (setq niwa-view--in-flight-turn-start
                  (copy-marker (niwa-view--history-insertion-marker))
                  niwa-view--data-turn-start
                  (with-current-buffer data-buf (copy-marker (point-max))))
            (niwa-view--start-spinner "Working...")
            (niwa-view-stream-test--draw-running-rows)
            (niwa-view--spinner-tick)
            (let ((display
                   (get-text-property
                    (marker-position (caar niwa-view--spinner-tool-targets))
                    'display)))
              (setq seconds 0.961
                    niwa-view-spinner-power-policy 'save)
              (niwa-view--start-spinner-timer)
              (niwa-view--render-live-region data-buf nil)
              (let ((position
                     (marker-position
                      (caar niwa-view--spinner-tool-targets))))
                (should (equal-including-properties
                         display (get-text-property position 'display)))
                (should (= 0.72 (niwa-view--tool-sample-seconds position)))
                (niwa-view--resume-on-window-change (selected-window))
                (should (equal-including-properties
                         display (get-text-property position 'display)))))))))))

(niwa-deftest niwa-view-animation-full-rerender-tool-phase
  (:doc "A full projection retains each surviving frozen tool's displayed phase.")
  (dolist (style '(ascii braille dots))
    (dolist (freeze '(global battery))
      (niwa-view-stream-test--with-buffers
        (niwa-view-stream-test--with-visible-view
          (let ((seconds 0.72)
                (niwa-view-spinner-style 'static)
                (niwa-view-tool-spinner-style style)
                (niwa-view-spinner-power-policy 'full)
                (niwa-view-spinner-battery-framerate 0)
                (niwa-view-spinner-animate t)
                (niwa-view--pending-tool-calls
                 '(("call-1" "Read" nil))))
            (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                       (lambda () seconds)))
              (setq niwa-view--in-flight-turn-start
                    (copy-marker (niwa-view--history-insertion-marker))
                    niwa-view--data-turn-start
                    (with-current-buffer data-buf (copy-marker (point-max))))
              (niwa-view--start-spinner "Working...")
              (niwa-view-stream-test--draw-running-rows)
              (niwa-view--spinner-tick)
              (let ((display
                     (get-text-property
                      (marker-position (caar niwa-view--spinner-tool-targets))
                      'display)))
                (setq seconds 0.961)
                (if (eq freeze 'global)
                    (setq niwa-view-spinner-animate nil)
                  (setq niwa-view-spinner-power-policy 'save))
                (niwa-view--start-spinner-timer)
                (niwa-view--full-rerender)
                ;; The live render queued behind the projection redraws the
                ;; running rows before redisplay, restoring their phase.
                (should (text-property-any
                         (point-min) (point-max)
                         'niwa-view-pending-tool-live t))
                (let ((position
                       (marker-position
                        (caar niwa-view--spinner-tool-targets))))
                  (should (equal-including-properties
                           display (get-text-property position 'display)))
                  (should (= 0.72 (niwa-view--tool-sample-seconds position)))
                  (niwa-view--resume-on-window-change (selected-window))
                  (should (equal-including-properties
                           display (get-text-property position 'display))))))))))))

(niwa-deftest niwa-view-animation-resume-lifecycle
  (:doc "Scroll/focus only rearm frames; mode change releases power and timers.")
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'auto)
            (reconciles 0))
        (niwa-view--start-spinner "Working...")
        (should (timerp niwa-view--spinner-timer))
        (should (gethash view-buf niwa-view-power--watchers))
        (let ((timer niwa-view--spinner-timer))
          (cl-letf (((symbol-function 'niwa-view-zone-reconcile)
                     (lambda (&rest _) (cl-incf reconciles)))
                    ((symbol-function 'niwa-view--schedule-tool-boundary-render)
                     (lambda (&rest _) (cl-incf reconciles)))
                    ((symbol-function 'niwa-view-render-live-update)
                     (lambda (&rest _) (cl-incf reconciles))))
            (niwa-view--resume-on-window-scroll (selected-window) (point-min))
            (niwa-view--resume-render-if-attended view-buf)
            (should (= 0 reconciles)))
          (fundamental-mode)
          (should-not (niwa--timer-pending-p timer))
          (should-not (gethash view-buf niwa-view-power--watchers)))))))

(niwa-deftest niwa-view--resume-on-window-scroll
  (:doc "Scrolling defers the decision until the new range is drawn.")
  ,test
  (test)

  :doc "the animation decision waits for the completed redisplay"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full))
        (unwind-protect
            (progn
              (niwa-view--start-spinner "Working...")
              (let ((timer niwa-view--spinner-timer))
                ;; Inside the hook the old range would read as hidden.
                (cl-letf (((symbol-function 'niwa-view--animation-visible-p)
                           #'ignore))
                  (niwa-view--resume-on-window-scroll (selected-window)
                                                         (point-min)))
                (should (niwa--ui-timer-pending-p timer))
                (should (niwa--ui-timer-pending-p
                         niwa-view--spinner-probe-timer))
                (should (memq #'niwa-view--resume-on-window-scroll
                              window-scroll-functions))))
          (niwa-view--stop-spinner)))))

  :doc "a stale rail is not rewritten inside redisplay, keeping the draft cursor"
  ;; Redisplay restores point numerically after `window-scroll-functions'.
  ;; The elapsed label gaining a digit there moved the cursor one character
  ;; back, so text typed after RET at the window bottom joined the previous
  ;; line.  Run the view's own hook as redisplay does.
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'static))
        (unwind-protect
            (progn
              (niwa-view-test--insert-composer-draft "> draft\nsecond" 9)
              (niwa-view--start-spinner "Working...")
              (setq niwa-view--spinner-start-time
                    (time-subtract nil 9.5))
              (niwa-view--render-request-progress)
              (should (string-suffix-p "· 9s" (niwa-view-test--rail-text)))
              ;; The once-a-second tick has not run yet when 10 s pass.
              (setq niwa-view--spinner-start-time
                    (time-subtract nil 10.5))
              (let ((tick (buffer-modified-tick))
                    (before (niwa-view-test--rail-text)))
                (run-hook-with-args 'window-scroll-functions
                                    (selected-window) (window-start))
                (should (= tick (buffer-modified-tick)))
                (should (equal before (niwa-view-test--rail-text))))
              (should (equal "> draft\nsecond" (niwa-view-composer-draft-text)))
              (should (= 9 (niwa-view-stream-test--draft-offset))))
          (niwa-view--stop-spinner))))))

(niwa-deftest niwa-view--resume-on-window-resize
  (:doc "Resizing refits the rail and rearms animation from the size hook.")
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full))
        (unwind-protect
            (progn
              (should (memq #'niwa-view--resume-on-window-resize
                            window-size-change-functions))
              (should-not (memq #'niwa-view--resume-on-window-resize
                                window-scroll-functions))
              (niwa-view-pair-hide view-buf)
              (let ((narrow (split-window-right)))
                (set-window-buffer narrow view-buf)
                (should (niwa-view-composer-display (selected-window)))
                (let ((composer (niwa-view-composer-display narrow))
                      (width (window-total-width narrow)))
                  (should composer)
                  (niwa-view-test--insert-composer-draft "a draft" 2)
                  (niwa-view--start-spinner "Working...")
                  (window-resize narrow (- 12 width) t)
                  (should (<= (window-body-width composer) 12))
                  (run-hook-with-args 'window-size-change-functions
                                      (selected-window))
                  (should-not (string-search "Working" (niwa-view-test--rail-text)))
                  (window-resize narrow (- width 12) t)))
              (run-hook-with-args 'window-size-change-functions
                                  (selected-window))
              (should (string-search "Working" (niwa-view-test--rail-text)))
              (should (niwa--ui-timer-pending-p
                       niwa-view--spinner-probe-timer))
              (should (equal "a draft" (niwa-view-composer-draft-text)))
              (should (= 2 (niwa-view-stream-test--draft-offset))))
          (niwa-view--stop-spinner))))))

(niwa-deftest niwa-view-animation-window-departure
  (:doc "Hiding the last frozen tool view releases power monitoring immediately.")
  (let ((queries 0)
        (battery-status-function (lambda () (cl-incf queries) nil))
        (niwa-view-power--watchers (make-hash-table :test #'eq))
        (niwa-view-power--timer nil)
        (niwa-view-power--state 'unknown)
        (niwa-view-power--sample-time nil)
        (niwa-view-power--last-query nil)
        (battery-update-functions
         (copy-sequence (bound-and-true-p battery-update-functions)))
        (window-state-change-functions
         (copy-sequence window-state-change-functions))
        (niwa-view-spinner-power-policy 'auto)
        (niwa-view-spinner-battery-framerate 0))
    (unwind-protect
        (niwa-view-stream-test--without-battery-load
          (niwa-view-stream-test--with-buffers
            (save-window-excursion
              (delete-other-windows)
              (switch-to-buffer view-buf)
              (redisplay t)
              (with-current-buffer view-buf
                (setq niwa-view--pending-tool-calls
                      '(("call-1" "Read" nil)))
                (niwa-view-stream-test--draw-running-rows)
                (should (gethash view-buf niwa-view-power--watchers))
                (should (timerp niwa-view-power--timer))
                (should-not niwa-view--spinner-timer))
              ;; The first departure must not unsubscribe while a second window
              ;; still displays the target.
              (let ((other (split-window-right))
                    (last-power niwa-view-power--timer))
                (set-window-buffer other view-buf)
                (redisplay t)
                (switch-to-buffer data-buf)
                (redisplay t)
                ;; Batch Emacs has no live redisplay loop; deliver the departing
                ;; buffer's local callback with that buffer current.
                (with-current-buffer view-buf
                  (should (memq #'niwa-view--resume-on-window-change
                                window-buffer-change-functions))
                  (run-hook-with-args 'window-buffer-change-functions
                                      (selected-window)))
                (should (gethash view-buf niwa-view-power--watchers))
                (set-window-buffer other data-buf)
                (redisplay t)
                (with-current-buffer view-buf
                  (run-hook-with-args 'window-buffer-change-functions other))
                (should-not (gethash view-buf niwa-view-power--watchers))
                (should-not niwa-view-power--timer)
                (should-not (memq #'niwa-view-power--sample
                                  battery-update-functions))
                (niwa-view-power--poll last-power)
                (niwa-view-power--poll last-power)
                (should (zerop queries))
                (should-not niwa-view-power--timer)
                ;; Reopening the same view restores its observer without
                ;; restarting or changing the pending tool call.
                (set-window-buffer other view-buf)
                (redisplay t)
                (with-current-buffer view-buf
                  (run-hook-with-args 'window-buffer-change-functions other))
                (should (gethash view-buf niwa-view-power--watchers))
                (should (equal (with-current-buffer view-buf
                                 niwa-view--pending-tool-calls)
                               '(("call-1" "Read" nil))))
                ;; Deleting the last view window has no departing buffer-local
                ;; callback.  A shared window-state observer must do the cleanup.
                (delete-window other)
                (run-hook-with-args 'window-state-change-functions
                                    (selected-frame))
                (should-not (gethash view-buf niwa-view-power--watchers))
                (should-not niwa-view-power--timer)
                (should-not (memq #'niwa-view-power--sample
                                  battery-update-functions))
                (should-not (memq #'niwa-view-power--on-window-state-change
                                  window-state-change-functions))))))
        (when niwa-view-power--timer
        (cancel-timer niwa-view-power--timer)))))

(niwa-deftest niwa-view-animation-power-cadence
  (:doc "The actual view timer changes cadence without restarting the phase.")
  (let ((niwa-view-animation--cache nil))
    (cl-letf (((symbol-function 'niwa-view-animation--colors)
               (lambda (_face _frame) '("#ff0000" . "#ffffff"))))
      (niwa-view-stream-test--with-buffers
        (niwa-view-stream-test--with-visible-view
          (let ((niwa-view-spinner-style 'shimmer)
                (niwa-view-spinner-framerate 60)
                (niwa-view-spinner-battery-framerate 30)
                (niwa-view-spinner-power-policy 'full))
            (niwa-view--start-spinner "Working...")
            (should (< (abs (- niwa-view--spinner-timer-period (/ 1.0 60)))
                       1e-7))
            (let ((phase niwa-view--spinner-phase-start)
                  (full-timer niwa-view--spinner-timer))
              (setq niwa-view-spinner-power-policy 'save)
              (niwa-view--start-spinner-timer)
              (should (< (abs (- niwa-view--spinner-timer-period (/ 1.0 30)))
                         1e-7))
              (should-not (niwa--timer-pending-p full-timer))
              (should (= phase niwa-view--spinner-phase-start))
              (setq niwa-view-spinner-battery-framerate 0)
              (niwa-view--start-spinner-timer)
              (should (= 1.0 niwa-view--spinner-timer-period))
              (should (= phase niwa-view--spinner-phase-start))
              (setq niwa-view-spinner-style 'static)
              (niwa-view--start-spinner-timer)
              (should (= 1.0 niwa-view--spinner-timer-period)))))))))

(niwa-deftest niwa-view-animation-customize-live
  (:doc "Customize replaces the active timer and display without resetting a turn.")
  (let ((niwa-view-animation--cache nil)
        (original-style (default-value 'niwa-view-spinner-style))
        (original-policy (default-value 'niwa-view-spinner-power-policy))
        (original-rate (default-value 'niwa-view-spinner-framerate)))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-view-animation--colors)
                   (lambda (_face _frame) '("#ff0000" . "#ffffff"))))
          (customize-set-variable 'niwa-view-spinner-power-policy 'full)
          (customize-set-variable 'niwa-view-spinner-style 'ascii)
          (customize-set-variable 'niwa-view-spinner-framerate 60)
          (niwa-view-stream-test--with-buffers
            (niwa-view-stream-test--with-visible-view
              (niwa-view--start-spinner "Working...")
              (should (= 0.24 niwa-view--spinner-timer-period))
              (let ((phase niwa-view--spinner-phase-start)
                    (old-timer niwa-view--spinner-timer))
                (customize-set-variable 'niwa-view-spinner-style 'shimmer)
                (should (< (abs (- niwa-view--spinner-timer-period
                                   (/ 1.0 60))) 1e-7))
                (should-not (niwa--timer-pending-p old-timer))
                (should (= phase niwa-view--spinner-phase-start))
                (let* ((sample (niwa-view-stream-test--rail-activity)))
                  (should (get-text-property 0 'face sample)))
                (customize-set-variable 'niwa-view-spinner-framerate 30)
                (should (< (abs (- niwa-view--spinner-timer-period
                                   (/ 1.0 30))) 1e-7))
                (should (= phase niwa-view--spinner-phase-start))
                (customize-set-variable 'niwa-view-spinner-style 'static)
                (should (= 1.0 niwa-view--spinner-timer-period))
                (should (= phase niwa-view--spinner-phase-start))))))
      (set-default 'niwa-view-spinner-style original-style)
      (set-default 'niwa-view-spinner-power-policy original-policy)
      (set-default 'niwa-view-spinner-framerate original-rate))))

(niwa-deftest niwa-view-animation-timer-suspension
  (:doc "Suspended timers rearm from their old phase without replaying missed frames.")
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full)
            (now 1000.0))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional _) now)))
          (niwa-view--start-spinner "Working...")
          (should (= 0.24 niwa-view--spinner-timer-period))
          (let ((old-timer niwa-view--spinner-timer)
                (phase niwa-view--spinner-phase-start))
            ;; TRAMP can remove timers from timer-list while their buffer-local
            ;; references still point to timer objects.  A focus event rearms.
            (cancel-timer old-timer)
            (setq now 1234.37)
            (niwa-view--resume-render-if-attended view-buf)
            (should (timerp niwa-view--spinner-timer))
            (should-not (eq old-timer niwa-view--spinner-timer))
            (should (= phase niwa-view--spinner-phase-start))
            (niwa-view--spinner-tick)
            (should (equal (niwa-view-stream-test--rail-activity)
                           (niwa-view-animation-frame
                            'ascii "Working..." (- now phase)
                            'niwa-view-spinner (selected-frame))))))))))

(niwa-deftest niwa-view-animation-tramp-timer-ownership
  (:doc "TRAMP suspension preserves one view timer and cancels it on stop.")
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full))
        (niwa-view--start-spinner "Working...")
        (let ((original niwa-view--spinner-timer)
              (phase niwa-view--spinner-phase-start))
          (should (memq original timer-list))
          (with-tramp-suspended-timers
            (dotimes (_ 5) (niwa-view--start-spinner-timer))
            (should (eq original niwa-view--spinner-timer))
            (should (niwa--ui-timer-pending-p original))
            (should-not (memq original timer-list)))
          (should (memq original timer-list))
          (with-tramp-suspended-timers
            ;; A cadence change must replace rather than strand the old poll.
            (setq niwa-view-spinner-style 'static)
            (niwa-view--start-spinner-timer)
            (let ((replacement niwa-view--spinner-timer))
              (should-not (eq original replacement))
              (should-not (memq original
                                (default-toplevel-value 'timer-list)))
              (should (niwa--ui-timer-pending-p replacement))
              (should (memq niwa--coalesced-timer
                            (default-toplevel-value 'timer-list)))
              (should (= phase niwa-view--spinner-phase-start))
              (setq niwa-view-spinner-style 'ascii)
              (niwa-view--start-spinner-timer)
              (should-not (niwa--ui-timer-pending-p replacement))))
          (let ((active niwa-view--spinner-timer))
            (should (memq active timer-list))
            (with-tramp-suspended-timers
              (niwa-view-stream-stop)
              (should-not (memq active
                                    (default-toplevel-value 'timer-list))))
            (should-not (memq active timer-list))
            (should-not niwa-view--spinner-timer)
            ;; Stale delivery must not restart a stopped view.
            (funcall (timer--function active))
            (should-not niwa-view--spinner-timer)))))))

(niwa-deftest niwa-view-animation-auto-power-timer-suspension
  (:doc "Resuming a visible view restores the shared auto-power poll too.")
  (let* ((niwa-view-power--watchers (make-hash-table :test #'eq))
         (niwa-view-power--state 'unknown)
         (niwa-view-power--sample-time nil)
         (niwa-view-power--last-query nil)
         (niwa-view-power--timer nil)
         (battery-update-functions
          (copy-sequence (bound-and-true-p battery-update-functions)))
         (window-state-change-functions
          (copy-sequence window-state-change-functions))
         (queries 0)
         (battery-status-function
          (lambda ()
            (cl-incf queries)
            '((?L . "AC") (?B . "high")))))
    (niwa-view-stream-test--without-battery-load
      (niwa-view-stream-test--with-buffers
        (niwa-view-stream-test--with-visible-view
          (let ((niwa-view-spinner-style 'ascii)
                (niwa-view-spinner-power-policy 'auto)
                (niwa-view-spinner-battery-framerate 0))
            ;; Both UI-host timers survive TRAMP's disposable binding.
            (let ((timer-list nil) (timer-idle-list nil))
              (niwa-view--start-spinner "Working...")
              (should (= 1.0 niwa-view--spinner-timer-period))
              (should (niwa--ui-timer-pending-p niwa-view--spinner-timer))
              (should (memq niwa--coalesced-timer
                            (default-toplevel-value 'timer-list)))
              (should (memq niwa-view-power--timer
                            (default-toplevel-value 'timer-list))))
            (let ((original niwa-view-power--timer)
                  (phase niwa-view--spinner-phase-start))
              (should (niwa--timer-pending-p original))
              (niwa-view--resume-render-if-attended view-buf)
              (should (niwa--timer-pending-p niwa-view-power--timer))
              (should (eq original niwa-view-power--timer))
              (should (= 0 queries))
              (should (= 1.0 niwa-view--spinner-timer-period))
              (niwa--ui-timer-cancel original)
              (niwa-view-power--poll original)
              (should (= 1 queries))
              (should (= 60 (niwa-view-power-framerate 60 0 'auto t)))
              (should (= 0.24 niwa-view--spinner-timer-period))
              (should (= phase niwa-view--spinner-phase-start))
              (niwa-view-stream-stop)
              (should (zerop (hash-table-count niwa-view-power--watchers)))
              (should-not niwa-view-power--timer))))))))

(niwa-deftest niwa-view-animation-overdue-timer
  (:doc "An overdue one-shot does not replay missed callbacks or allocate timers.")
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full)
            (ticks 0))
        (niwa-view--start-spinner "Working...")
        (let ((timer niwa-view--spinner-timer)
              (phase niwa-view--spinner-phase-start))
          (should-not (timer--repeat-delay timer))
          ;; Deliver one deadline after a simulated long stall.  Emacs's
          ;; repeating timer would requeue overdue callbacks in a burst.
          (timer-set-time timer
                          (time-subtract (current-time) (seconds-to-time 3)))
          (cl-letf (((symbol-function 'niwa-view--spinner-tick)
                     (lambda () (cl-incf ticks))))
            (timer-event-handler timer)
            (should (= ticks 1))
            (should (eq timer niwa-view--spinner-timer))
            (should (= phase niwa-view--spinner-phase-start))
            (should (niwa--timer-pending-p timer))
            (should (< (timer-until timer nil) -0.06))
            (should-not (timer--repeat-delay timer))))))))

(niwa-deftest niwa-view-animation-auto-transition
  ()
  ,test
  (test)
  :doc "Battery notifications rearm the view, preserving phase and color bank."
  (cl-letf (((symbol-function 'niwa-view-animation--colors)
             (lambda (_face _frame) '("#ff0000" . "#ffffff"))))
    (let ((niwa-view-power--state 'unknown)
          (niwa-view-power--sample-time nil)
          (niwa-view-animation--cache nil))
      (niwa-view-stream-test--with-buffers
        (niwa-view-stream-test--with-visible-view
          (let ((niwa-view-spinner-style 'shimmer)
                (niwa-view-spinner-power-policy 'auto)
                (niwa-view-spinner-framerate 60)
                (niwa-view-spinner-battery-framerate 30))
            (niwa-view--start-spinner "Working...")
            (should (gethash view-buf niwa-view-power--watchers))
            (should (< (abs (- niwa-view--spinner-timer-period (/ 1.0 30)))
                       1e-7))
            (let ((phase niwa-view--spinner-phase-start)
                  (bank niwa-view-animation--cache))
              (niwa-view-power--sample '((?L . "AC") (?B . "high")))
              (should (< (abs (- niwa-view--spinner-timer-period (/ 1.0 60)))
                         1e-7))
              (should (= phase niwa-view--spinner-phase-start))
              (should (eq bank niwa-view-animation--cache))
              (niwa-view-power--sample '((?L . "BAT") (?B . "low")))
              (should (< (abs (- niwa-view--spinner-timer-period (/ 1.0 30)))
                         1e-7))
              (should (= phase niwa-view--spinner-phase-start))
              (should (eq bank niwa-view-animation--cache))))))))

  :doc "An expired AC sample followed by unknown notification slows the real timer"
  (let ((niwa-view-power--watchers (make-hash-table :test #'eq))
        (niwa-view-power--timer nil)
        (niwa-view-power--state 'unknown)
        (niwa-view-power--sample-time nil)
        (niwa-view-power--last-query nil)
        (battery-update-functions
         (copy-sequence (bound-and-true-p battery-update-functions)))
        (window-state-change-functions
         (copy-sequence window-state-change-functions))
        (now 1000.5)
        (queries 0))
    (let ((battery-status-function (lambda () (cl-incf queries) nil)))
      (niwa-view-stream-test--without-battery-load
        (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
          (niwa-view-stream-test--with-buffers
            (niwa-view-stream-test--with-visible-view
              (let ((niwa-view-spinner-style 'ascii)
                    (niwa-view-spinner-power-policy 'auto)
                    (niwa-view-spinner-framerate 60)
                    (niwa-view-spinner-battery-framerate 1))
                (niwa-view--start-spinner "Working...")
                (niwa-view-power--sample '((?L . "AC") (?B . "high")))
                (should (= 0.24 niwa-view--spinner-timer-period))
                (let ((fast niwa-view--spinner-timer)
                      (phase niwa-view--spinner-phase-start))
                  (setq now 1060.5)
                  (should (eq 'unknown (niwa-view-power--current-state)))
                  (should (niwa--ui-timer-pending-p fast))
                  (run-hook-with-args 'battery-update-functions
                                      '((?L . "N/A") (?B . "N/A") (?p . "N/A")))
                  (should (= 1.0 niwa-view--spinner-timer-period))
                  (should-not (eq fast niwa-view--spinner-timer))
                  (should-not (niwa--ui-timer-pending-p fast))
                  (should (niwa--ui-timer-pending-p
                           niwa-view--spinner-timer))
                  (should (= phase niwa-view--spinner-phase-start))
                  (should (= 0 queries)))))))))))

(niwa-deftest niwa-view-animation-expired-external
  (:doc "An expired AC reading with a failed backend drops the actual timer rate.")
  (cl-letf (((symbol-function 'niwa-view-animation--colors)
             (lambda (_face _frame) '("#ff0000" . "#ffffff"))))
    (let ((niwa-view-animation--cache nil)
          (now 1000.0)
          (niwa-view-power--state 'unknown)
          (niwa-view-power--sample-time nil)
          (niwa-view-power--last-query nil)
          (battery-status-function (lambda () (error "Battery backend unavailable"))))
      (niwa-view-stream-test--without-battery-load
        (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
          (niwa-view-stream-test--with-buffers
            (niwa-view-stream-test--with-visible-view
              (let ((niwa-view-spinner-style 'shimmer)
                    (niwa-view-spinner-power-policy 'auto)
                    (niwa-view-spinner-framerate 60)
                    (niwa-view-spinner-battery-framerate 0))
                (niwa-view--start-spinner "Working...")
                (niwa-view-power--sample '((?L . "AC") (?B . "high")))
                (should (< (abs (- niwa-view--spinner-timer-period
                                   (/ 1.0 60))) 1e-7))
                (let ((fast niwa-view--spinner-timer))
                  (setq now 1060.0)
                  (when niwa-view-power--timer
                    (niwa--ui-timer-cancel niwa-view-power--timer))
                  (niwa-view-power--poll niwa-view-power--timer)
                  (should-not (niwa--timer-pending-p fast))
                  (should (= 1.0 niwa-view--spinner-timer-period)))))))))))

(niwa-deftest niwa-view-animation-tool-display-frame
  (:doc "Tool frames use portable initial glyphs and their own target frame.")
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-tool-spinner-style 'braille)
            (niwa-view-spinner-power-policy 'full)
            (niwa-view--pending-tool-calls
             '(("call-1" "Read" nil)))
            (original (symbol-function 'niwa-view-animation-frame))
            frames)
        (cl-letf (((symbol-function 'char-displayable-p)
                   (lambda (_character) t))
                  ((symbol-function 'niwa-view-animation-frame)
                   (lambda (style label seconds face &optional frame)
                     (when (eq face 'niwa-view-ephemeral)
                       (push frame frames))
                     (funcall original style label seconds face frame))))
          (niwa-view-stream-test--draw-running-rows)
          (should (eq (car frames) :multiple))
          (let ((pos (text-property-any
                      (point-min) (point-max)
                      'niwa-view-inline-spinner-frame t)))
            (should (equal "- " (get-text-property pos 'display)))
            (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                       (lambda () 0.24)))
              (niwa-view--spinner-tick))
            (should (eq (car frames) (selected-frame)))
            (should (equal "⠙ " (get-text-property pos 'display)))))))))

(niwa-deftest niwa-view-animation-main-initial-frame
  (:doc "An undisplayed status uses a portable frame, not the selected frame.")
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((niwa-view-spinner-style 'braille))
        (cl-letf (((symbol-function 'char-displayable-p)
                   (lambda (_character) t)))
          (let ((line (niwa-view--format-spinner-line "Working...")))
            (should (equal "- Working..."
                           (get-text-property 0 'display line)))))))))

(niwa-deftest niwa-view-animation-multiple-frame-glyph-style
  (:doc "A multi-frame status retains dots, ellipsis, and ASCII styles.")
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (dolist (style '(dots ellipsis ascii))
        (let ((niwa-view-spinner-style style)
              (niwa-view-tool-spinner-style 'static)
              (niwa-view-spinner-power-policy 'full))
          (niwa-view--start-spinner "Working...")
          (let* ((initial (niwa-view-stream-test--rail-activity)))
            (setq niwa-view--spinner-last-second (floor (float-time)))
            (cl-letf (((symbol-function 'niwa-view--animation-target-frame)
                       (lambda (_target &optional _all) :multiple))
                      ((symbol-function 'niwa-view--animation-span-in-window-p)
                       (lambda (_start _end _window) t))
                      ((symbol-function 'niwa-view--animation-seconds)
                       (lambda () 0.96)))
              (niwa-view--spinner-tick))
            (should (equal
                     (niwa-view-animation-frame
                      style "Working..." 0.96 'niwa-view-spinner :multiple)
                     (niwa-view-stream-test--rail-activity)))
            (should (= (string-width initial)
                       (string-width (niwa-view-stream-test--rail-activity)))))
          (niwa-view--stop-spinner))))))

(niwa-deftest niwa-view-animation-dots-visible-target-frames
  (:doc "Dots ignore other windows not actually displaying the indicator.")
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (insert "Working...")
      (let* ((target (cons (copy-marker (point-min))
                           (copy-marker (point-max))))
             (visible-frame (selected-frame))
             (hidden-frame 'hidden)
             (scrolled-frame 'scrolled)
             (niwa-view-animation--dots-cache nil))
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
                  ((symbol-function 'niwa-view--animation-span-in-window-p)
                   (lambda (_start _end window)
                     (eq window 'visible)))
                  ((symbol-function 'niwa-view-animation--dots-frame-supported-p)
                   (lambda (frame) (eq frame visible-frame))))
          (let ((frames (niwa-view--animation-target-frame target t)))
            (should (equal frames (list visible-frame)))
            (should (equal
                     (niwa-view-animation-frame
                      'dots "Working" 0 'default frames)
                     "●··· Working"))))))))

(niwa-deftest niwa-view-animation-colorless-glyph-cadence
  (:doc "Colorless views use the glyph timer cadence without frame-time queries.")
  (let ((niwa-view-animation--cache nil)
        (color-queries 0))
    (cl-letf (((symbol-function 'niwa-view-animation--colors)
               (lambda (_face _frame)
                 (cl-incf color-queries)
                 nil)))
      (niwa-view-stream-test--with-buffers
        (niwa-view-stream-test--with-visible-view
          (let ((niwa-view-spinner-style 'shimmer)
                (niwa-view-tool-spinner-style 'static)
                (niwa-view-spinner-power-policy 'full))
            (niwa-view--start-spinner "Working...")
            (should (= 0.24 niwa-view--spinner-timer-period))
            (let ((queries color-queries))
              (setq niwa-view--spinner-last-second (floor (float-time)))
              (niwa-view--spinner-tick)
              (should (= queries color-queries))
              (should (= 0.24 niwa-view--spinner-timer-period)))))))))

(niwa-deftest niwa-view-animation-attended-target-window
  (:doc "A focused offscreen window cannot animate an unfocused visible target.")
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full)
            (auto-hscroll-mode nil))
        (setq-local truncate-lines t)
        (niwa-view--start-spinner "Working...")
        (let* ((foreground (niwa-view-composer-window))
               (background (split-window-right))
               (focused (window-frame foreground))
               (background-frame 'background-animation-test-frame)
               (original-window-frame (symbol-function 'window-frame))
               (initial (niwa-view-stream-test--rail-activity)))
          (set-window-buffer background view-buf)
          (setq background (niwa-view-composer-display background))
          (should background)
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
                    ((symbol-function 'niwa-view--animation-span-in-window-p)
                     (lambda (_start _end window)
                       (eq window background))))
            (should-not (niwa-view--animation-visible-p))
            (should-not (niwa-view--spinner-metadata-visible-p))
            (niwa-view--start-spinner-timer)
            (should-not niwa-view--spinner-timer-period)
            (should-not niwa-view--spinner-timer)
            (let ((niwa-view--spinner-last-second (floor (float-time))))
              (niwa-view--spinner-tick))
            (should (equal-including-properties
                     initial (niwa-view-stream-test--rail-activity)))
            ;; The focus-change hook rearms the window showing the label.
            (cl-letf (((symbol-function 'frame-focus-state)
                       (lambda (frame) (eq frame background-frame))))
              (should (niwa-view--animation-visible-p))
              (niwa-view--resume-attended-views)
              (should (= 0.24 niwa-view--spinner-timer-period))
              (cl-letf (((symbol-function 'niwa-view--animation-seconds)
                         (lambda () 0.48)))
                (setq niwa-view--spinner-last-second (floor (float-time)))
                (niwa-view--spinner-tick))
              (should-not (equal-including-properties
                           initial (niwa-view-stream-test--rail-activity))))))))))

(niwa-deftest niwa-view-animation-clipped-row
  (:doc "Clipped rows keep progress current without glyph layout or draft edits.")
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-power-policy 'full)
            (auto-hscroll-mode nil)
            (draft "> quoted\nsecond line"))
        (with-current-buffer (niwa-view-composer-buffer)
          (setq-local truncate-lines t))
        (niwa-view-test--insert-composer-draft draft 4)
        (dolist (style '(ascii ellipsis shimmer static))
          (let ((niwa-view-spinner-style style))
            (niwa-view--start-spinner "Working...")
            (set-window-hscroll (niwa-view-composer-window) 100)
            (let ((cursor (niwa-view-stream-test--draft-offset)))
              (cl-letf (((symbol-function 'posn-at-point)
                         (lambda (&rest _) (ert-fail "Animation queried glyphs")))
                        ((symbol-function 'posn-at-x-y)
                         (lambda (&rest _) (ert-fail "Animation queried pixels"))))
                (niwa-view--start-spinner-timer)
                (should niwa-view--spinner-timer-period)
                (niwa-view--spinner-tick))
              (should (equal draft (niwa-view-composer-draft-text)))
              (should (= cursor (niwa-view-stream-test--draft-offset))))))))))

(niwa-deftest niwa-view-animation-metadata-cell-visible
  (:doc "Elapsed time updates when only its cell is visible.")
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii))
        (niwa-view--start-spinner "Working...")
        (let* ((start (marker-position
                       (car niwa-view--spinner-metadata-target)))
               (end (marker-position
                     (cdr niwa-view--spinner-metadata-target))))
          (cl-letf (((symbol-function 'window-start)
                     (lambda (_window) start))
                    ((symbol-function 'window-end)
                     (lambda (_window &optional _update) end)))
            (should-not (niwa-view--animation-target-visible-p
                         niwa-view--spinner-label-target 'niwa-view-spinner-frame))
            (should (niwa-view--spinner-metadata-visible-p))
            (niwa-view--start-spinner-timer)
            (should (= 1.0 niwa-view--spinner-timer-period))
            (setq niwa-view--spinner-start-time
                  (time-subtract (current-time) (seconds-to-time 3))
                  niwa-view--spinner-last-second nil)
            (niwa-view--spinner-tick)
            (should (string-match-p "Working\\.\\.\\. · [3-9]s"
                                    (niwa-view-test--rail-text)))))))))

(niwa-deftest niwa-view-animation-low-color-terminal-cadence
  (:doc "A resolved 8-color terminal never schedules color-rate callbacks.")
  (let ((niwa-view-animation--cache nil))
    (cl-letf (((symbol-function 'display-color-p) (lambda (&optional _frame) t))
              ((symbol-function 'display-graphic-p) (lambda (&optional _frame) nil))
              ((symbol-function 'display-color-cells)
               (lambda (&optional _frame) 8))
              ((symbol-function 'face-foreground)
               (lambda (&rest _) "#ffffff"))
              ((symbol-function 'face-background)
               (lambda (&rest _) "#000000")))
      (niwa-view-stream-test--with-buffers
        (niwa-view-stream-test--with-visible-view
          (dolist (style '(shimmer breathe bounce))
            (let ((niwa-view-spinner-style style)
                  (niwa-view-tool-spinner-style 'static)
                  (niwa-view-spinner-power-policy 'full))
              (niwa-view--start-spinner "Working...")
              (should-not niwa-view--spinner-main-color-p)
              (should (= 0.24 niwa-view--spinner-timer-period))
              (should-not (get-text-property
                           0 'face
                           (niwa-view-stream-test--rail-activity))))))))))

(niwa-deftest niwa-view-animation-theme-prepares-off-frame-path
  (:doc "Theme invalidation defers color preparation to semantic maintenance.")
  (let ((niwa-view-animation--cache nil)
        (color-queries 0))
    (cl-letf (((symbol-function 'niwa-view-animation--colors)
               (lambda (_face _frame)
                 (cl-incf color-queries)
                 '("#ff0000" . "#ffffff"))))
      (niwa-view-stream-test--with-buffers
        (niwa-view-stream-test--with-visible-view
          (let ((niwa-view-spinner-style 'shimmer)
                (niwa-view-tool-spinner-style 'static)
                (niwa-view-spinner-power-policy 'full))
            (niwa-view--start-spinner "Working...")
            (should (> color-queries 0))
            (niwa-view-animation-invalidate)
            (let ((queries color-queries))
              (setq niwa-view--spinner-last-second (floor (float-time)))
              (niwa-view--spinner-tick)
              (should (= queries color-queries))
              (should-not niwa-view-animation--cache)
              (setq niwa-view--spinner-last-second nil)
              (niwa-view--spinner-tick)
              (should (> color-queries queries))
              (should niwa-view-animation--cache))))))))

(niwa-deftest niwa-view-animation-theme-refreshes-frozen-status
  (:doc "Theme changes repaint a paused zero-fps color sample without a timer.")
  (let ((niwa-view-animation--cache nil)
        (foreground "#ff0000"))
    (cl-letf (((symbol-function 'niwa-view-animation--colors)
               (lambda (_face _frame) (cons foreground "#000000"))))
      (niwa-view-stream-test--with-buffers
        (with-current-buffer data-buf
          (setq-local niwa--current-request
                      (niwa-request--create :started-at (current-time)))
          (niwa-request-set-active-work-paused
           niwa--current-request t))
        (niwa-view-stream-test--with-visible-view
          (let ((niwa-view-spinner-style 'shimmer)
                (niwa-view-spinner-power-policy 'save)
                (niwa-view-spinner-battery-framerate 0))
            (niwa-view--start-spinner "Working...")
            (let* ((before (niwa-view-stream-test--rail-activity))
                   (phase niwa-view--spinner-phase-start)
                   (frozen niwa-view--spinner-frozen-seconds)
                   (render-state niwa-view--spinner-rendered-state)
                   (was-modified (buffer-modified-p))
                   (undo buffer-undo-list)
                   (here (point)))
              (should (get-text-property 0 'face before))
              (should-not niwa-view--spinner-timer)
              (dolist (hook '(enable-theme-functions disable-theme-functions))
                (setq foreground (if (eq hook 'enable-theme-functions)
                                     "#00ff00" "#0000ff"))
                (run-hook-with-args hook 'niwa-test-theme)
                (should-not (equal-including-properties
                             before (niwa-view-stream-test--rail-activity)))
                (should (equal-including-properties
                         (niwa-view-animation-frame
                          'shimmer "Waiting for input" frozen
                          'niwa-view-spinner (selected-frame))
                         (niwa-view-stream-test--rail-activity)))
                (should-not niwa-view--spinner-timer)
                (should (= phase niwa-view--spinner-phase-start))
                (should (= frozen niwa-view--spinner-frozen-seconds))
                (should (equal render-state niwa-view--spinner-rendered-state))
                (should (eq was-modified (buffer-modified-p)))
                (should (eq undo buffer-undo-list))
                (should (= here (point)))
                (setq before (niwa-view-stream-test--rail-activity)))
              (niwa-view--spinner-tick)
              (should (equal-including-properties
                       before (niwa-view-stream-test--rail-activity)))
              ;; The same event must not paint an undisplayed buffer for an
              ;; unrelated frame; its frozen phase repaints on visibility.
              (niwa-view-pair-hide view-buf)
              (set-window-buffer (selected-window) data-buf)
              (setq foreground "#ffff00")
              (run-hook-with-args 'enable-theme-functions 'niwa-test-theme)
              (should niwa-view--spinner-theme-stale-p)
              (should (equal-including-properties
                       before (niwa-view-stream-test--rail-activity)))
              (should-not niwa-view--spinner-timer)
              (set-window-buffer (selected-window) view-buf)
              (should (niwa-view-composer-display (selected-window)))
              (niwa-view--resume-on-window-change (selected-window))
              (should-not niwa-view--spinner-theme-stale-p)
              (should (equal-including-properties
                       (niwa-view-animation-frame
                        'shimmer "Waiting for input" frozen
                        'niwa-view-spinner (selected-frame))
                       (niwa-view-stream-test--rail-activity)))
              (should (= frozen niwa-view--spinner-frozen-seconds))
              (should-not niwa-view--spinner-timer))))))))

(niwa-deftest niwa-view-animation-frame-refreshes-frozen-status
  (:doc "A paused zero-fps label changes palettes when its display frame changes.")
  (let ((display-frame :first))
    (cl-letf (((symbol-function 'niwa-view--animation-buffer-frame)
               (lambda (&optional _buffer) display-frame))
              ((symbol-function 'niwa-view--animation-target-frame)
               (lambda (&rest _) display-frame))
              ((symbol-function 'niwa-view-animation-color-available-p)
               (lambda (&rest _) t))
              ((symbol-function 'niwa-view-animation-frame)
               (lambda (_style label _seconds _face frame)
                 (propertize label 'face
                             (list :foreground
                                   (if (eq frame :first)
                                       "#ff0000" "#00ff00"))))))
      (niwa-view-stream-test--with-buffers
        (with-current-buffer data-buf
          (setq-local niwa--current-request
                      (niwa-request--create :started-at (current-time)))
          (niwa-request-set-active-work-paused
           niwa--current-request t))
        (niwa-view-stream-test--with-visible-view
          (let ((niwa-view-spinner-style 'shimmer)
                (niwa-view-spinner-power-policy 'save)
                (niwa-view-spinner-battery-framerate 0))
            (niwa-view--start-spinner "Working...")
            (let* ((before (niwa-view-stream-test--rail-activity))
                   (phase niwa-view--spinner-phase-start)
                   (frozen niwa-view--spinner-frozen-seconds)
                   (render-state niwa-view--spinner-rendered-state)
                   (modified (buffer-modified-p))
                   (undo buffer-undo-list)
                   (here (point)))
              (should (eq niwa-view--spinner-sample-frame :first))
              (should-not niwa-view--spinner-timer)
              (setq display-frame :second)
              (niwa-view--resume-on-window-change (selected-window))
              (should-not (equal-including-properties
                           before (niwa-view-stream-test--rail-activity)))
              (should (equal (plist-get
                              (get-text-property 0 'face
                                                 (niwa-view-stream-test--rail-activity))
                              :foreground)
                             "#00ff00"))
              (should (eq niwa-view--spinner-sample-frame :second))
              (should-not niwa-view--spinner-theme-stale-p)
              (should-not niwa-view--spinner-timer)
              (should (= phase niwa-view--spinner-phase-start))
              (should (= frozen niwa-view--spinner-frozen-seconds))
              (should (equal render-state niwa-view--spinner-rendered-state))
              (should (eq modified (buffer-modified-p)))
              (should (eq undo buffer-undo-list))
              (should (= here (point)))
              (setq before (niwa-view-stream-test--rail-activity))
              (niwa-view--resume-on-window-change (selected-window))
              (should (equal-including-properties
                       before (niwa-view-stream-test--rail-activity)))
              (should-not niwa-view--spinner-timer)
              ;; The old sample must stay untouched while its window is
              ;; hidden; returning to an attended frame repairs it.
              (niwa-view-pair-hide view-buf)
              (set-window-buffer (selected-window) data-buf)
              (setq display-frame :first)
              (niwa-view--start-spinner-timer)
              (should (eq niwa-view--spinner-sample-frame :second))
              (should (equal-including-properties
                       before (niwa-view-stream-test--rail-activity)))
              (set-window-buffer (selected-window) view-buf)
              (should (niwa-view-composer-display (selected-window)))
              (niwa-view--resume-on-window-change (selected-window))
              (should (eq niwa-view--spinner-sample-frame :first))
              (should (equal-including-properties
                       (niwa-view-stream-test--rail-activity)
                       (propertize "Waiting for input" 'face
                                   '(:foreground "#ff0000"))))
              (should (= phase niwa-view--spinner-phase-start))
              (should (= frozen niwa-view--spinner-frozen-seconds))
              (should-not niwa-view--spinner-timer))))))))

(niwa-deftest niwa-view-animation-glyph-fallback-refreshes-frozen-status
  (:doc "Theme/capability changes repaint frozen main and tool glyphs without timers.")
  (dolist (style '(braille dots))
    (dolist (freeze '(global battery))
      (let ((supported t)
            (niwa-view-animation--dots-cache nil))
        (cl-letf (((symbol-function 'char-displayable-p)
                   (lambda (char &optional _frame)
                     (or supported (not (memq char '(?⠋ ?● ?·))))))
                  ;; The first frame, however long a loaded runner stalls.
                  ((symbol-function 'niwa-view--animation-seconds)
                   (lambda () 0.0)))
          (niwa-view-stream-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--current-request
                          (niwa-request--create :started-at (current-time))))
            (niwa-view-stream-test--with-visible-view
              (let ((niwa-view-spinner-style style)
                    (niwa-view-tool-spinner-style style)
                    (niwa-view-spinner-power-policy 'full)
                    (niwa-view-spinner-battery-framerate 30)
                    (niwa-view-spinner-animate t)
                    (niwa-view--pending-tool-calls
                     '(("call-1" "Read" nil))))
                (niwa-view--start-spinner "Working...")
                (niwa-view-stream-test--draw-running-rows)
                (niwa-view--spinner-tick)
                (niwa-view--interaction-register
                 '(:kind ask :id ask :origin "/root" :body "Question"))
                (let* ((tool (caar niwa-view--spinner-tool-targets))
                       (before-main (niwa-view-stream-test--rail-activity))
                       (before-tool (get-text-property (marker-position tool)
                                                       'display)))
                  (should (niwa-view--animation-visible-p))
                  (should (if (eq style 'braille)
                              (string-prefix-p "⠋ " before-main)
                            (string-prefix-p "●" before-main)))
                  (should (if (eq style 'braille)
                              (string-prefix-p "⠋ " before-tool)
                            (string-prefix-p "●" before-tool)))
                  (if (eq freeze 'global)
                      (setq niwa-view-spinner-animate nil)
                    (setq niwa-view-spinner-power-policy 'save
                          niwa-view-spinner-battery-framerate 0))
                  (niwa-view--start-spinner-timer)
                  (let ((phase niwa-view--spinner-phase-start)
                        (frozen niwa-view--spinner-frozen-seconds)
                        (modified (buffer-modified-p))
                        (undo buffer-undo-list)
                        (here (point)))
                    (should-not niwa-view--spinner-timer)
                    (setq supported nil)
                    (run-hook-with-args 'enable-theme-functions 'niwa-test-theme)
                    (let ((expected-main
                           (niwa-view-animation-frame
                            style "Waiting for input" frozen
                            'niwa-view-spinner
                            (niwa-view--animation-target-frame
                             niwa-view--spinner-label-target
                             (eq style 'dots))))
                          (expected-tool
                           (niwa-view-animation-frame
                            style "" frozen 'niwa-view-ephemeral
                            (niwa-view--animation-target-frame
                             (car niwa-view--spinner-tool-targets)
                             (eq style 'dots)))))
                      (should-not (equal-including-properties
                                   before-main (niwa-view-stream-test--rail-activity)))
                      (should-not (equal-including-properties
                                   before-tool (get-text-property
                                                (marker-position tool) 'display)))
                      (should (equal-including-properties
                               expected-main (niwa-view-stream-test--rail-activity)))
                      (should (equal-including-properties
                               expected-tool (get-text-property
                                              (marker-position tool) 'display))))
                    (setq niwa-view--spinner-last-second nil)
                    (niwa-view--spinner-tick)
                    (should-not niwa-view--spinner-timer)
                    (should (= phase niwa-view--spinner-phase-start))
                    (should (= frozen niwa-view--spinner-frozen-seconds))
                    (should (eq modified (buffer-modified-p)))
                    (should (eq undo buffer-undo-list))
                    (should (= here (point)))
                    ;; A font capability change need not run a theme hook.
                    ;; Visibility rearming retries the cached dots support.
                    (setq supported t)
                    (niwa-view--resume-on-window-change (selected-window))
                    (should (equal-including-properties
                             (niwa-view-animation-frame
                              style "Waiting for input" frozen
                              'niwa-view-spinner
                              (niwa-view--animation-target-frame
                               niwa-view--spinner-label-target
                               (eq style 'dots)))
                             (niwa-view-stream-test--rail-activity)))
                    (should (equal-including-properties
                             (niwa-view-animation-frame
                              style "" frozen 'niwa-view-ephemeral
                              (niwa-view--animation-target-frame
                               (car niwa-view--spinner-tool-targets)
                               (eq style 'dots)))
                             (get-text-property (marker-position tool)
                                                'display)))
                    (let ((visible-main (niwa-view-stream-test--rail-activity))
                          (visible-tool (get-text-property
                                         (marker-position tool) 'display)))
                      (niwa-view-pair-hide view-buf)
                      (set-window-buffer (selected-window) data-buf)
                      (setq supported nil)
                      (run-hook-with-args 'enable-theme-functions
                                          'niwa-test-theme)
                      (should niwa-view--spinner-theme-stale-p)
                      (should (equal-including-properties
                               visible-main (niwa-view-stream-test--rail-activity)))
                      (should (equal-including-properties
                               visible-tool (get-text-property
                                             (marker-position tool) 'display)))
                      (set-window-buffer (selected-window) view-buf)
                      (should (niwa-view-composer-display (selected-window)))
                      (niwa-view--resume-on-window-change
                       (selected-window))
                      (should-not niwa-view--spinner-theme-stale-p)
                      (should-not (equal-including-properties
                                   visible-main (niwa-view-stream-test--rail-activity)))
                      (should-not (equal-including-properties
                                   visible-tool (get-text-property
                                                 (marker-position tool)
                                                 'display))))
                    (should-not niwa-view--spinner-timer)))))))))))

(niwa-deftest niwa-view-animation-face-refreshes-frozen-status
  (:doc "Customize and direct face edits repaint paused color without a timer.")
  (let ((niwa-view-animation--cache nil)
        (original (face-attribute 'niwa-view-spinner :foreground nil))
        (already-installed
         (advice-member-p #'niwa-view--refresh-animation-on-face
                          'set-face-attribute)))
    (unwind-protect
        (progn
          (advice-add 'set-face-attribute :after
                      #'niwa-view--refresh-animation-on-face)
          (set-face-attribute 'niwa-view-spinner nil :foreground "#ff0000")
          (cl-letf (((symbol-function 'niwa-view-animation--colors)
                     (lambda (_face _frame)
                       (cons (face-foreground 'niwa-view-spinner nil t)
                             "#000000"))))
            (niwa-view-stream-test--with-buffers
              (with-current-buffer data-buf
                (setq-local niwa--current-request
                            (niwa-request--create :started-at (current-time)))
                (niwa-request-set-active-work-paused
                 niwa--current-request t))
              (niwa-view-stream-test--with-visible-view
                (let ((niwa-view-spinner-style 'shimmer)
                      (niwa-view-spinner-power-policy 'save)
                      (niwa-view-spinner-battery-framerate 0))
                  (niwa-view--start-spinner "Working...")
                  (let* ((before (niwa-view-stream-test--rail-activity))
                         (phase niwa-view--spinner-phase-start)
                         (frozen niwa-view--spinner-frozen-seconds)
                         (modified (buffer-modified-p))
                         (undo buffer-undo-list)
                         (here (point)))
                    (should-not niwa-view--spinner-timer)
                    (set-face-attribute 'niwa-view-spinner nil
                                        :foreground "#00ff00")
                    (should-not (equal-including-properties
                                 before (niwa-view-stream-test--rail-activity)))
                    (should (equal-including-properties
                             (niwa-view-animation-frame
                              'shimmer "Waiting for input" frozen
                              'niwa-view-spinner (selected-frame))
                             (niwa-view-stream-test--rail-activity)))
                    (should-not niwa-view--spinner-timer)
                    (should (= phase niwa-view--spinner-phase-start))
                    (should (= frozen niwa-view--spinner-frozen-seconds))
                    (should (eq modified (buffer-modified-p)))
                    (should (eq undo buffer-undo-list))
                    (should (= here (point)))
                    (let ((visible (niwa-view-stream-test--rail-activity)))
                      (niwa-view-pair-hide view-buf)
                      (set-window-buffer (selected-window) data-buf)
                      (set-face-attribute 'niwa-view-spinner nil
                                          :foreground "#0000ff")
                      (should niwa-view--spinner-theme-stale-p)
                      (should (equal-including-properties
                               visible (niwa-view-stream-test--rail-activity)))
                      (should-not niwa-view--spinner-timer)
                      (set-window-buffer (selected-window) view-buf)
                      (should (niwa-view-composer-display (selected-window)))
                      (niwa-view--resume-on-window-change (selected-window))
                      (should-not niwa-view--spinner-theme-stale-p)
                      (should-not (equal-including-properties
                                   visible (niwa-view-stream-test--rail-activity)))
                      (should-not niwa-view--spinner-timer))))))))
      (set-face-attribute 'niwa-view-spinner nil :foreground original)
      (unless already-installed
        (advice-remove 'set-face-attribute
                       #'niwa-view--refresh-animation-on-face)))))

(niwa-deftest niwa-view-animation-inherited-face-refreshes-frozen-status
  (:doc "Changing an inherited face repaints a timer-free paused label.")
  (let ((original-spinner
         (face-attribute 'niwa-view-spinner :foreground nil))
        (original-inherit
         (face-attribute 'niwa-view-spinner :inherit nil))
        (original-parent
         (face-attribute 'font-lock-comment-face :foreground nil))
        (alias 'niwa-view-test-spinner-comment-alias)
        (old-alias (get 'niwa-view-test-spinner-comment-alias 'face-alias))
        (installed (advice-member-p #'niwa-view--refresh-animation-on-face
                                    'set-face-attribute)))
    (unwind-protect
        (progn
          (advice-add 'set-face-attribute :after
                      #'niwa-view--refresh-animation-on-face)
          (set-face-attribute 'niwa-view-spinner nil
                              :foreground 'unspecified)
          (put alias 'face-alias 'font-lock-comment-face)
          (dolist (variant (list (cons 'font-lock-comment-face
                                       'font-lock-comment-face)
                                 (cons alias 'font-lock-comment-face)
                                 (cons 'font-lock-comment-face alias)))
            (set-face-attribute 'niwa-view-spinner nil
                                :inherit (list 'bold (car variant)))
            (set-face-attribute 'font-lock-comment-face nil
                                :foreground "#ff0000")
            (should (niwa-view--spinner-inherits-face-p
                     'font-lock-comment-face (selected-frame)))
            (cl-letf (((symbol-function 'niwa-view-animation--colors)
                       (lambda (_face _frame)
                         (cons (face-foreground 'niwa-view-spinner nil t)
                               "#000000"))))
              (niwa-view-stream-test--with-buffers
                (with-current-buffer data-buf
                  (setq-local niwa--current-request
                              (niwa-request--create :started-at (current-time)))
                  (niwa-request-set-active-work-paused
                   niwa--current-request t))
                (niwa-view-stream-test--with-visible-view
                  (let ((niwa-view-spinner-style 'shimmer)
                        (niwa-view-spinner-power-policy 'save)
                        (niwa-view-spinner-battery-framerate 0))
                    (niwa-view--start-spinner "Working...")
                    (let* ((before (niwa-view-stream-test--rail-activity))
                           (phase niwa-view--spinner-phase-start)
                           (frozen niwa-view--spinner-frozen-seconds))
                      (should-not niwa-view--spinner-timer)
                      (set-face-attribute (cdr variant) nil
                                          :foreground "#00ff00")
                      (should-not (equal-including-properties
                                   before (niwa-view-stream-test--rail-activity)))
                      (should (equal-including-properties
                               (niwa-view-animation-frame
                                'shimmer "Waiting for input" frozen
                                'niwa-view-spinner (selected-frame))
                               (niwa-view-stream-test--rail-activity)))
                      (should (= phase niwa-view--spinner-phase-start))
                      (should (= frozen niwa-view--spinner-frozen-seconds))
                      (should-not niwa-view--spinner-timer)))))))
      (set-face-attribute 'font-lock-comment-face nil
                          :foreground original-parent)
      (set-face-attribute 'niwa-view-spinner nil
                          :foreground original-spinner :inherit original-inherit)
      (put alias 'face-alias old-alias)
      (unless installed
        (advice-remove 'set-face-attribute
                       #'niwa-view--refresh-animation-on-face))))))

(niwa-deftest niwa-view-animation-shimmer-overflow
  (:doc "Frozen shimmer retains phase but displays the current running count.")
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((niwa-view-spinner-animate nil)
            (niwa-view-tool-spinner-style 'shimmer)
            (niwa-view-tool-group-collapse-threshold 3))
        (niwa-view-test--insert-composer-draft "> draft\nsecond line")
        (setq niwa-view--pending-tool-calls
              '(("1" "Read" "a") ("2" "Bash" "sleep") ("3" "Grep" "x")
                ("4" "Read" "b")))
        (niwa-view-stream-test--draw-running-rows)
        (should (= 1 (length niwa-view--spinner-tool-targets)))
        (push '("5" "Grep" "y") niwa-view--pending-tool-calls)
        (niwa-view-stream-test--draw-running-rows)
        (let* ((target (car (last niwa-view--spinner-tool-targets)))
               (start (marker-position (car target))))
          (should (string-match-p
                   "5 running"
                   (get-text-property start 'display))))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))))))

(niwa-deftest niwa-view-animation-shimmer-tool-banks
  (:doc "All distinct shimmer rows survive cache eviction and display changes.")
  (let ((niwa-view-animation--cache nil))
    (niwa-view-stream-test--with-buffers
      (niwa-view-stream-test--with-visible-view
        (let ((niwa-view-spinner-style 'static)
              (niwa-view-tool-spinner-style 'shimmer)
              (niwa-view-spinner-power-policy 'full)
              (niwa-view-tool-group-collapse-threshold 3))
          (cl-letf (((symbol-function 'niwa-view-animation--colors)
                     (lambda (_face frame)
                       (unless (eq frame :multiple)
                         '("#ffffff" . "#000000"))))
                    ((symbol-function 'niwa-view--animation-span-in-window-p)
                     (lambda (&rest _) t)))
            (setq niwa-view--pending-tool-calls
                  ;; The most separate rows a batch draws before it groups.
                  (cl-loop for i below 3
                           collect (list (number-to-string i)
                                         (format "Tool%d" i) "argument")))
            (niwa-view-stream-test--draw-running-rows)
            (should (= 3 (length niwa-view--spinner-tool-targets)))
            ;; A new display needs different banks.  The semantic scheduler
            ;; must prepare every row, even with no main color indicator.
            (cl-letf (((symbol-function 'niwa-view--animation-target-frame)
                       (lambda (&rest _) :multiple)))
              (niwa-view--start-spinner-timer t)
              ;; Another view may evict the entire shared reuse cache.
              (setq niwa-view-animation--cache nil)
              (dolist (target niwa-view--spinner-tool-targets)
                (should (niwa-view-animation-color-ready-p
                         'shimmer (niwa-view--tool-animation-label target)
                         (niwa-view--tool-animation-face target)
                         :multiple))))))))))

(niwa-deftest niwa-view--arm-spinner-timer
  (:doc "coalesces metadata while retaining the ordinary animation cadence")
  (let ((niwa--coalesced-timers nil)
        (niwa--coalesced-timer nil)
        (metadata (timer-create))
        (animation (timer-create)))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-view--animation-seconds) (lambda () 1.0)))
          (timer-set-function metadata #'ignore)
          (timer-set-function animation #'ignore)
          (niwa-view--arm-spinner-timer metadata '((metadata . 1.0)))
          (let ((due (float-time (timer--time metadata))))
            (should (= due (floor due))))
          (should (niwa--ui-timer-pending-p metadata))
          (should-not (memq metadata timer-list))
          (niwa-view--arm-spinner-timer animation '((shimmer . 0.04)))
          (should (memq animation (default-toplevel-value 'timer-list)))
          (should (< (- (float-time (timer--time animation)) (float-time)) 0.05))
          (should-not (timer--repeat-delay metadata))
          (should-not (timer--repeat-delay animation)))
      (niwa--ui-timer-cancel metadata)
      (niwa--ui-timer-cancel animation))))

(niwa-deftest niwa-view--spinner-next-delay
  (:doc "Combines sweep, portable glyph and elapsed cadences without polling rest.")
  ,test
  (test)
  (should (= 2.6 (niwa-view--spinner-next-delay '((shimmer . 0.04)) 2.0)))
  (should (= 1.0 (niwa-view--spinner-next-delay
                  '((shimmer . 0.04) (metadata . 1.0)) 2.0)))
  (should (= 0.24 (niwa-view--spinner-next-delay
                   '((shimmer . 0.04) (glyph . 0.24)) 2.0)))
  (should (= 0.04 (niwa-view--spinner-next-delay
                   '((shimmer . 0.04) (glyph . 0.24)) 1.0))))

(niwa-deftest niwa-view--spinner-visual-period
  (:doc "Text breathe and bounce leave 8 fps once PGTK presents only changes.")
  (let ((niwa-view-spinner-framerate 30)
        (niwa-view-spinner-power-policy 'full)
        (niwa-view-spinner-animate t)
        (niwa-present--state nil))
    (should (= 0.125 (niwa-view--spinner-visual-period 'bounce)))
    (setq niwa-present--state 'enabled)
    (should (= (/ 1.0 30) (niwa-view--spinner-visual-period 'bounce)))
    (should (= (/ 1.0 30) (niwa-view--spinner-visual-period 'breathe)))
    ;; Glyphs keep their own cadence, and so does a glyph fallback.
    (should (= 0.24 (niwa-view--spinner-visual-period 'braille)))
    (should (= 0.24 (niwa-view--spinner-visual-period 'bounce t)))
    (let ((niwa-view-spinner-power-policy 'save)
          (niwa-view-spinner-battery-framerate 15))
      (should (= (/ 1.0 15) (niwa-view--spinner-visual-period 'bounce))))))

(niwa-deftest niwa-view-animation-shimmer-mixed-displays
  (:doc "A portable tool glyph continues during another tool's color rest.")
  (let ((niwa-view-animation--cache nil))
    (niwa-view-stream-test--with-buffers
      (niwa-view-stream-test--with-visible-view
        (let ((niwa-view-spinner-style 'static)
              (niwa-view-tool-spinner-style 'shimmer)
              (niwa-view-spinner-power-policy 'full))
          (cl-letf (((symbol-function 'niwa-view-animation--colors)
                     (lambda (_face frame)
                       (unless (eq frame :multiple)
                         '("#ffffff" . "#000000"))))
                    ((symbol-function 'niwa-view--animation-span-in-window-p)
                     (lambda (&rest _) t)))
            (setq niwa-view--pending-tool-calls
                  '(("1" "Read" "a") ("2" "Bash" "sleep")))
            (niwa-view-stream-test--draw-running-rows)
            (cl-letf (((symbol-function 'niwa-view--animation-target-frame)
                       (lambda (target &optional _all)
                         (if (eq target (car niwa-view--spinner-tool-targets))
                             (selected-frame) :multiple))))
              (niwa-view--start-spinner-timer t)
              (should (assq 'shimmer niwa-view--spinner-timer-plan))
              (should (equal '(glyph . 0.24)
                             (assq 'glyph niwa-view--spinner-timer-plan)))
              (should (= 0.24 (niwa-view--spinner-next-delay
                               niwa-view--spinner-timer-plan 2.0))))))))))

(niwa-deftest niwa-view-animation-shimmer-frozen-tool-display
  (:doc "A frozen tool-only row adopts the destination display without a timer.")
  (let ((niwa-view-animation--cache nil))
    (niwa-view-stream-test--with-buffers
      (niwa-view-stream-test--with-visible-view
        (let ((niwa-view-spinner-style 'static)
              (niwa-view-tool-spinner-style 'shimmer)
              (niwa-view-spinner-animate nil))
          (cl-letf (((symbol-function 'niwa-view-animation--colors)
                     (lambda (_face frame)
                       (unless (eq frame :multiple)
                         '("#ffffff" . "#000000"))))
                    ((symbol-function 'niwa-view--animation-span-in-window-p)
                     (lambda (&rest _) t)))
            (setq niwa-view--pending-tool-calls '(("1" "Read" "a")))
            (niwa-view-stream-test--draw-running-rows)
            (let ((start (marker-position
                          (caar niwa-view--spinner-tool-targets))))
              (should (equal "Read" (get-text-property start 'display)))
              (cl-letf (((symbol-function 'niwa-view--animation-target-frame)
                         (lambda (&rest _) :multiple)))
                (niwa-view--start-spinner-timer t)
                (should (equal "- Read"
                               (get-text-property start 'display)))
                (should-not niwa-view--spinner-timer)))))))))

(niwa-deftest niwa-view-animation-interaction-pause
  (:doc "Registering and answering an Ask stops and resumes the real scheduler.")
  (niwa-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (setq-local niwa--current-request
                  (niwa-request--create :started-at (current-time))))
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'ascii)
            (niwa-view-spinner-power-policy 'full))
        (cl-letf (((symbol-function 'niwa-view--animation-span-in-window-p)
                   (lambda (&rest _) t)))
          (niwa-view-test--insert-composer-draft "> draft\nsecond line")
          (niwa-view--start-spinner "Working...")
          (should (timerp niwa-view--spinner-timer))
          (niwa-view--interaction-register
           '(:kind ask :id ask :origin "/root" :body "Question"))
          (should-not niwa-view--spinner-timer)
          (niwa-view--interaction-unregister 'ask)
          (should (timerp niwa-view--spinner-timer))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))))))

(niwa-deftest niwa-view-animation-paused-shimmer-rearm
  (:doc "Visibility and theme refresh retain paused tool phases at ordinary fps.")
  (let ((niwa-view-animation--cache nil))
    (niwa-view-stream-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--current-request
                    (niwa-request--create :started-at (current-time))))
      (niwa-view-stream-test--with-visible-view
        (let ((niwa-view-spinner-style 'shimmer)
              (niwa-view-tool-spinner-style 'shimmer)
              (niwa-view-spinner-power-policy 'full)
              (seconds 0.9))
          (cl-letf (((symbol-function 'niwa-view-animation--colors)
                     (lambda (&rest _) '("#ffffff" . "#000000")))
                    ((symbol-function 'niwa-view--animation-span-in-window-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'niwa-view--animation-seconds)
                     (lambda () seconds)))
            (niwa-view--start-spinner "Working...")
            (setq niwa-view--pending-tool-calls '(("1" "Read" "a")))
            (niwa-view-stream-test--draw-running-rows)
            (niwa-view--spinner-tick)
            (niwa-view--interaction-register
             '(:kind ask :id ask :origin "/root" :body "Question"))
            (let* ((start (marker-position
                           (caar niwa-view--spinner-tool-targets)))
                   (display (get-text-property start 'display))
                   (phase (niwa-view--tool-sample-seconds start)))
              (setq seconds 1.3)
              (niwa-view--start-spinner-timer t)
              (should (equal-including-properties
                       display (get-text-property start 'display)))
              (should (= phase (niwa-view--tool-sample-seconds start)))
              (should-not niwa-view--spinner-timer))))))))

(niwa-deftest niwa-view-composer-rail
  (:doc "The permanent rail preserves the draft and reserves no idle timer.")
  ,test
  (test)

  :doc "idle, active and settled rails occupy one composer line and preserve input"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "> draft\nsecond" 4)
      (let ((lines (count-lines (point-min) (niwa-view-transcript-tail))))
        (should (equal "[ask]" (niwa-view-test--rail-text)))
        (with-current-buffer (niwa-view-composer-buffer view-buf)
          (should (= 2 (line-number-at-pos (niwa-view-composer-draft-start view-buf)))))
        (should-not (niwa-view--spinner-active-p))
        (should-not niwa-view--spinner-timer)
        (niwa-view--start-spinner "Working...")
        (should (= lines (count-lines (point-min) (niwa-view-transcript-tail))))
        (should (string-search "Working" (niwa-view-test--rail-text)))
        (should-not (string-search "Working" (buffer-string)))
        (niwa-view--stop-spinner)
        (should (= lines (count-lines (point-min) (niwa-view-transcript-tail))))
        (should-not (niwa-view--spinner-active-p))
        (should-not niwa-view--spinner-timer)
        (should (equal "> draft\nsecond" (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-stream-test--draft-offset)))
        (let* ((composer (niwa-view-composer-buffer))
               (before (with-current-buffer composer (buffer-string)))
               (tick (with-current-buffer composer (buffer-modified-tick))))
          (niwa-view--render-request-progress)
          (with-current-buffer composer
            (should (= tick (buffer-modified-tick)))
            (should (equal-including-properties before (buffer-string))))))))

  :doc "pending attention is real actionable text and does not take focus"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (niwa-view-test--insert-composer-draft "keep typing" 3)
      (niwa-view--interaction-register
       '(:kind permission :id approval :count 1 :body "Approve" :activate ignore))
      (should (= 3 (niwa-view-stream-test--draft-offset)))
      (let ((reader (selected-window)))
        (should (string-search "⚠ 1 permission · C-c C-j" (niwa-view-test--rail-text)))
        (should (eq reader (selected-window)))
        (select-window (niwa-view-composer-window reader))
        (execute-kbd-macro (kbd "C-c C-j"))
        (should (eq reader (selected-window)))
        (should (eq 'approval (get-text-property (point) 'niwa-view-zone-id))))
      (niwa-view--interaction-unregister 'approval)
      (should-not (string-search "⚠" (niwa-view-test--rail-text)))
      (should (equal "keep typing" (niwa-view-composer-draft-text)))))

  :doc "two windows use the narrower width and expanding restores activity"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (niwa-view--interaction-register
       '(:kind permission :id approval :count 1 :body "Approve" :activate ignore))
      (niwa-view-pair-hide view-buf)
      (let* ((wide (selected-window))
             (narrow (split-window wide -24 'right))
             (wide-composer (niwa-view-composer-display wide)))
        (set-window-buffer narrow view-buf)
        (should wide-composer)
        (let ((composer (niwa-view-composer-display narrow)))
          (should composer)
          (should (<= (window-body-width composer) 24))
          (niwa-view-test--insert-composer-draft "a draft\nsecond line" 4)
          (set-window-point wide-composer (+ (niwa-view-composer-draft-start) 4))
          (niwa-view--start-spinner "Working...")
          (niwa-view--resume-on-window-resize wide)
          (let ((rail (niwa-view-test--rail-text)))
            (should (< (string-width rail) (window-body-width composer)))
            (should (string-search "⚠" rail))
            (should-not (string-search "Working" rail))))
        (delete-window narrow)
        (niwa-view--resume-on-window-resize wide)
        (let ((rail (niwa-view-test--rail-text)))
          (should (string-search "C-c C-j" rail))
          (should (string-search "Working" rail)))
        (should (equal "a draft\nsecond line" (niwa-view-composer-draft-text)))
        (should (= (+ (niwa-view-composer-draft-start) 4)
                   (window-point wide-composer)))))))

(niwa-deftest niwa-view--format-composer-rail
  (:doc "Narrow rails sacrifice activity before mode and pending attention.")
  (let ((niwa-view-spinner-style 'static))
    (dolist (width '(1 3 5 10 16 24 40 80))
      (let ((rail (niwa-view--format-composer-rail
                   "[full-auto]" "2 permissions" width
                   "Reading a very long file name\nwith details"
                   "Reading a very long file name\nwith details · 1m 20s")))
        (should (= 1 (cl-count ?\n rail)))
        (should (<= (string-width (string-trim-right rail)) width))))
    (let ((rail (niwa-view--format-composer-rail
                 "[edits]" "2 permissions" 40 "Working..." "Working... · 2s")))
      (should (string-prefix-p "[edits] · ⚠ 2 permissions · C-c C-j" rail))
      (should-not (string-match-p "Working" rail)))
    (let ((rail (niwa-view--format-composer-rail
                 "[edits]" nil 80 "Working..." "Working... · 2s")))
      (should (eq (get-text-property (string-search "·" rail) 'face rail)
                  (get-text-property (string-search "·" rail 10) 'face rail))))))

(niwa-deftest niwa-view--spinner-framerate ()
  (let ((niwa-view-spinner-framerate 30)
        (niwa-view-spinner-battery-framerate 5)
        (niwa-view-spinner-power-policy 'full)
        (niwa-view-spinner-animate t))
    ,test)
  (test)
  :doc "uses the full frame rate under the full power policy"
  (should (= 30 (niwa-view--spinner-framerate)))
  :doc "uses the battery ceiling under the save power policy"
  (let ((niwa-view-spinner-power-policy 'save))
    (should (= 5 (niwa-view--spinner-framerate))))
  :doc "is zero while animation is off"
  (let ((niwa-view-spinner-animate nil))
    (should (zerop (niwa-view--spinner-framerate)))))

(niwa-deftest niwa-view--animation-frozen-p
  (:doc "Input pause and reduced motion independently freeze presentation.")
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((niwa-view-spinner-animate t)
            (niwa-view-spinner-power-policy 'full)
            (request (niwa-request--create :started-at (current-time))))
        (with-current-buffer data-buf (setq niwa--current-request request))
        (should-not (niwa-view--animation-frozen-p))
        (niwa-request-set-active-work-paused request t)
        (should (niwa-view--animation-frozen-p))
        (niwa-request-set-active-work-paused request nil)
        (should-not (niwa-view--animation-frozen-p))
        (let ((niwa-view-spinner-animate nil))
          (should (niwa-view--animation-frozen-p)))))))

(niwa-deftest niwa-view--capture-native-animation
  (:doc "Freezing uses the native submitted phase instead of stale Lisp display text.")
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((niwa-view-spinner-style 'ascii))
        (niwa-view--start-spinner "Working...")
        (setq niwa-view--native-animation-targets
              (list niwa-view--spinner-label-target)
              niwa-view--spinner-last-sample-seconds 0.0)
        (cl-letf (((symbol-function 'niwa-view-native-sample) (lambda (_) 0.72)))
          (niwa-view--capture-native-animation)
          (should (= 0.72 niwa-view--spinner-last-sample-seconds))
          (let ((niwa-view-spinner-animate nil))
            (should (= 0.72 (niwa-view--animation-display-seconds)))))))))

(niwa-deftest niwa-view--sync-native-animation
  (:doc "Offloaded labels leave only metadata wakeups and preserve the composer.")
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-test--with-visible-view
      (let ((niwa-view-spinner-style 'bounce)
            (niwa-view-spinner-power-policy 'full)
            (niwa-view-spinner-framerate 30))
        (niwa-view--start-spinner "Working...")
        (niwa-view-composer-insert "> retained\nsecond line")
        (let ((draft (niwa-view-composer-draft-text))
              (composer (niwa-view-composer-buffer))
              captured)
          (cl-letf (((symbol-function 'niwa-view-native-sync)
                     (lambda (specs _seconds _rearm)
                       (when specs
                         (should (eq composer (current-buffer)))
                         (setq captured specs))
                       (mapcar #'car specs)))
                    ((symbol-function 'niwa-view-native-sample) (lambda (_) 0.5)))
            (niwa-view--start-spinner-timer)
            (should (eq (cadar captured) 'bounce))
            (should (= (/ 1.0 30) (nth 4 (car captured))))
            (should (equal niwa-view--spinner-timer-plan '((metadata . 1.0))))
            (let* ((sample (niwa-view-stream-test--rail-activity)))
              (niwa-view--spinner-tick)
              (should (equal-including-properties sample (niwa-view-stream-test--rail-activity))))
            (should (equal draft (niwa-view-composer-draft-text)))))))))

(niwa-deftest niwa-view--adopt-tool-animation-targets ()
  ,test
  (test)
  :doc "animates only the running spans a render drew in its range"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((niwa-view-tool-spinner-style 'ascii)
            (inhibit-read-only t)
            outside)
        (niwa-view-test--insert-composer-draft "> draft\nline" 3)
        (save-excursion
          (goto-char (niwa-view--history-insertion-marker))
          (niwa-view--with-render-boundaries-advancing
            (setq outside (point))
            (niwa-view--insert-pending-tool-row '("old" "Read" "a"))))
        (let ((start (marker-position (niwa-view--history-insertion-marker))))
          (save-excursion
            (goto-char start)
            (niwa-view--with-render-boundaries-advancing
              (niwa-view--insert-pending-run
               '(("k" "Grep" "x") ("j" "Grep" "y")))))
          (niwa-view--adopt-tool-animation-targets
           start (marker-position (niwa-view--history-insertion-marker))
           '(("k" "* " 0.48 ""))))
        (should (= 2 (length niwa-view--spinner-tool-targets)))
        (should (cl-every (lambda (target) (> (car target) outside))
                          niwa-view--spinner-tool-targets))
        ;; A row that survives a redraw keeps the frame it displayed.
        (should (equal "* " (get-text-property
                             (car (car niwa-view--spinner-tool-targets))
                             'display)))
        (should (eq 'ascii niwa-view--spinner-rendered-tool-style))
        (should (equal "> draft\nline" (niwa-view-composer-draft-text)))
        (niwa-view--stop-spinner)))))

(niwa-deftest niwa-view--retire-pending-tool-rows ()
  ,test
  (test)
  :doc "removes running rows and their animation without touching the draft"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (let ((niwa-view-tool-spinner-style 'ascii))
        (niwa-view-test--insert-composer-draft "> draft\nline" 3)
        (setq niwa-view--pending-tool-calls '(("k" "Read" "a")))
        (niwa-view-stream-test--draw-running-rows)
        (should niwa-view--spinner-tool-targets)
        (niwa-view--retire-pending-tool-rows)
        (should-not niwa-view--spinner-tool-targets)
        (should-not (string-search "Read: a" (buffer-string)))
        (should (equal "> draft\nline" (niwa-view-composer-draft-text)))
        (should (= 3 (niwa-view-stream-test--draft-offset)))
        (niwa-view--stop-spinner)))))

(provide 'test-niwa-view-stream)
(niwa-deftest niwa-view-stream-progress-verbs ()
  ,test
  (test)
  :doc "show: sets label and owner, defaults the owner to request, and un-suppresses"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf (niwa-view--stop-request-progress))
    (niwa-view-stream-show-progress view-buf "Thinking...")
    (with-current-buffer view-buf
      (should (equal "Thinking..." niwa-view--spinner-status))
      (should (eq 'request niwa-view--spinner-owner))
      (should-not niwa-view--request-progress-suppressed))
    (niwa-view-stream-show-progress view-buf "Compacting..." 'compaction)
    (with-current-buffer view-buf
      (should (equal "Compacting..." niwa-view--spinner-status))
      (should (eq 'compaction niwa-view--spinner-owner))))

  :doc "show: records one status transition from the previous owner to the explicit owner"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer data-buf
      (setq-local niwa--session (niwa-session--create)))
    (let ((session (buffer-local-value 'niwa--session data-buf))
          records)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-telemetry-record)
                     (lambda (&rest args) (push args records))))
            (niwa-view-stream-show-progress view-buf "Thinking...")
            (niwa-view-stream-show-progress
             view-buf "Compacting..." 'compaction))
        ;; Teardown stops the view and must not see the fake session.
        (with-current-buffer data-buf (setq-local niwa--session nil)))
      (should (equal `(,session status-transition
                       :previous-owner request :previous-status "Thinking..."
                       :owner compaction :status "Compacting...")
                     (car records)))
      (should (= 2 (length records)))))

  :doc "ensure: redraws without changing the label or un-suppressing a hidden row"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-show-progress view-buf "Thinking..." 'compaction)
    (niwa-view-stream-ensure-progress view-buf data-buf)
    (with-current-buffer view-buf
      (should (equal "Thinking..." niwa-view--spinner-status))
      (should (eq 'compaction niwa-view--spinner-owner)))
    (niwa-view-stream-end-progress view-buf)
    (niwa-view-stream-ensure-progress view-buf data-buf)
    (with-current-buffer view-buf
      (should niwa-view--request-progress-suppressed)
      (should-not niwa-view--spinner-status)))

  :doc "end: goes idle and suppresses the row until the next show"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-show-progress view-buf "Thinking...")
    (niwa-view-stream-end-progress view-buf)
    (with-current-buffer view-buf
      (should niwa-view--request-progress-suppressed)
      (should-not niwa-view--spinner-status)
      (should-not niwa-view--spinner-owner)
      (should-not niwa-view--spinner-start-time))
    (niwa-view-stream-show-progress view-buf "Again...")
    (with-current-buffer view-buf
      (should-not niwa-view--request-progress-suppressed)))

  :doc "verbs: a nil or dead view is a silent no-op with no telemetry"
  (let ((dead (generate-new-buffer " *dead-view*"))
        records)
    (kill-buffer dead)
    (cl-letf (((symbol-function 'niwa-telemetry-record)
               (lambda (&rest args) (push args records))))
      (dolist (view (list nil dead))
        (should-not (niwa-view-stream-show-progress view "x"))
        (should-not (niwa-view-stream-ensure-progress view nil))
        (should-not (niwa-view-stream-end-progress view))
        (should-not (niwa-view-stream-claim-progress view "x" 'o))
        (should-not (niwa-view-stream-release-progress view '(:owner o)))))
    (should-not records))

  :doc "claim and release: release restores the replaced status while the owner holds"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-show-progress view-buf "Thinking...")
    (let ((token (niwa-view-stream-claim-progress
                  view-buf "Retrying..." 'provider-retry)))
      (with-current-buffer view-buf
        (should (eq 'provider-retry niwa-view--spinner-owner)))
      (niwa-view-stream-release-progress view-buf token)
      (with-current-buffer view-buf
        (should (equal "Thinking..." niwa-view--spinner-status))
        (should (eq 'request niwa-view--spinner-owner)))))

  :doc "claim and release: a token with nothing replaced returns to idle, not suppressed"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf (niwa-view--stop-spinner))
    (let ((token (niwa-view-stream-claim-progress
                  view-buf "Retrying..." 'provider-retry)))
      (niwa-view-stream-release-progress view-buf token)
      (with-current-buffer view-buf
        (should-not niwa-view--spinner-status)
        (should-not niwa-view--spinner-owner)
        (should-not niwa-view--request-progress-suppressed))))

  :doc "claim and release: a newer owner is left alone with or without a snapshot"
  (niwa-view-stream-test--with-buffers
    (dolist (before '("Thinking..." nil))
      (with-current-buffer view-buf (niwa-view--stop-spinner))
      (when before (niwa-view-stream-show-progress view-buf before))
      (let ((token (niwa-view-stream-claim-progress
                    view-buf "Retrying..." 'provider-retry)))
        (niwa-view-stream-show-progress view-buf "Hook running" 'hook)
        (niwa-view-stream-release-progress view-buf token)
        (with-current-buffer view-buf
          (should (equal "Hook running" niwa-view--spinner-status))
          (should (eq 'hook niwa-view--spinner-owner))))))

  :doc "claim and release: release touches the view the caller resolves now, not the claimed one"
  (niwa-view-stream-test--with-buffers
    (let ((view2 (generate-new-buffer " *niwa-stream-view2*")))
      (unwind-protect
          (progn
            (niwa-view--setup view2 data-buf)
            (niwa-view-stream-show-progress view-buf "Thinking...")
            (let ((token (niwa-view-stream-claim-progress
                          view-buf "Retrying..." 'provider-retry)))
              ;; The replacement view does not hold the claim: untouched.
              (niwa-view-stream-show-progress view2 "Other" 'hook)
              (niwa-view-stream-release-progress view2 token)
              (with-current-buffer view2
                (should (equal "Other" niwa-view--spinner-status))
                (should (eq 'hook niwa-view--spinner-owner)))
              ;; The original view is still live and still holds the claim,
              ;; yet release into the replacement leaves it alone.
              (with-current-buffer view-buf
                (should (eq 'provider-retry niwa-view--spinner-owner)))
              ;; A replacement that holds the owner is the one restored.
              (niwa-view-stream-show-progress view2 "Retrying..." 'provider-retry)
              (niwa-view-stream-release-progress view2 token)
              (with-current-buffer view2
                (should (equal "Thinking..." niwa-view--spinner-status))
                (should (eq 'request niwa-view--spinner-owner)))
              (with-current-buffer view-buf
                (should (eq 'provider-retry niwa-view--spinner-owner)))
              ;; Finally the original, resolved again, is restored.
              (niwa-view-stream-release-progress view-buf token)
              (with-current-buffer view-buf
                (should (eq 'request niwa-view--spinner-owner)))))
        (when (buffer-live-p view2)
          (with-current-buffer view2 (niwa-view-stream-stop))
          (kill-buffer view2)))))

  :doc "progress changes keep point and the composer draft in place"
  (niwa-view-stream-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-stream-begin-turn
       niwa-view--status-marker
       (with-current-buffer data-buf (copy-marker (point-min))))
      (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4))
    (dolist (step (list (lambda () (niwa-view-stream-show-progress
                                    view-buf "Compacting..." 'compaction))
                        (lambda () (niwa-view-stream-ensure-progress
                                    view-buf data-buf))
                        (lambda () (niwa-view-stream-end-progress view-buf))))
      (funcall step)
      (with-current-buffer view-buf
        (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-stream-test--draft-offset))))))

  :doc "cold load: each caller module ends progress with no view in a fresh Emacs"
  (with-temp-buffer
    (let ((status
           (call-process
            (expand-file-name invocation-name invocation-directory)
            nil t nil "--batch" "-Q" "--eval"
            (prin1-to-string
             `(progn
                (setq load-path ',load-path load-prefer-newer t)
                (require 'ert)
                (require 'cl-lib)
                (dolist (feature '(niwa-compact niwa-compact-run
                                   niwa-compact-target niwa-plan-handoff
                                   niwa-review niwa-gptel-retry
                                   niwa-hooks))
                  (require feature)
                  (should-not (featurep 'niwa-view-composer)))
                (dolist (verb '(niwa-view-stream-show-progress
                                niwa-view-stream-end-progress
                                niwa-view-stream-claim-progress
                                niwa-view-stream-release-progress
                                niwa-view-stream-ensure-progress
                                niwa-view-stream-lease-progress
                                niwa-view-stream-claim-leased-progress))
                  (should (fboundp verb)))
                (should-not (niwa-view-stream-end-progress nil))
                (should-not (niwa-view-stream-lease-progress nil))
                (should-not (niwa-hooks--progress-snapshot))
                (should-not (niwa-view-stream-show-progress nil "x"))
                (should-not (cl-remove-if-not
                             (lambda (b) (buffer-local-value 'niwa-view--spinner-status b))
                             (buffer-list))))))))
      (ert-info ((buffer-string))
        (should (equal 0 status))))))

(niwa-deftest niwa-view-stream-lease-progress ()
  ,test
  (test)
  :doc "lease: only active request-owned progress can be leased"
  (niwa-view-stream-test--with-buffers
    (should-not (niwa-view-stream-lease-progress nil))
    (should-not (niwa-view-stream-lease-progress view-buf))
    (niwa-view-stream-show-progress view-buf "Thinking...")
    (should (equal "Thinking..."
                   (plist-get (niwa-view-stream-lease-progress view-buf)
                              :status)))
    (niwa-view-stream-show-progress view-buf "Other" 'other)
    (should-not (niwa-view-stream-lease-progress view-buf)))

  :doc "claim-leased: claims a current lease and release restores it"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-show-progress view-buf "Thinking...")
    (let* ((lease (niwa-view-stream-lease-progress view-buf))
           (token (niwa-view-stream-claim-leased-progress
                   view-buf lease "Hook..." 'hook)))
      (should token)
      (with-current-buffer view-buf
        (should (equal "Hook..." niwa-view--spinner-status))
        (should (eq 'hook niwa-view--spinner-owner)))
      (should (niwa-view-stream-release-progress view-buf token))
      (with-current-buffer view-buf
        (should (equal "Thinking..." niwa-view--spinner-status))
        (should (eq 'request niwa-view--spinner-owner)))))

  :doc "claim-leased: refuses a stale lease, a nil lease and a dead view"
  (niwa-view-stream-test--with-buffers
    (niwa-view-stream-show-progress view-buf "Thinking...")
    (let ((lease (niwa-view-stream-lease-progress view-buf)))
      (should-not (niwa-view-stream-claim-leased-progress
                   view-buf nil "Hook..." 'hook))
      (should-not (niwa-view-stream-claim-leased-progress
                   nil lease "Hook..." 'hook))
      (niwa-view-stream-show-progress view-buf "Newer")
      (should-not (niwa-view-stream-claim-leased-progress
                   view-buf lease "Hook..." 'hook))
      (with-current-buffer view-buf
        (should (equal "Newer" niwa-view--spinner-status))))))

;;; test-niwa-view-stream.el ends here
