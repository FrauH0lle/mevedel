;;; test-niwa-execution-transcript.el -- Execution transcript tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests durable execution render data, compaction archives, and terminal
;; reconciliation independently from live View streaming.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-engine-test-support"))
(require 'gptel)
(require 'niwa-agents)
(require 'niwa-agent-conversation)
(require 'niwa-execution-target)
(require 'niwa-execution)
(require 'niwa-execution-transcript)
(require 'niwa-gptel-stream-bridge)
(require 'niwa-pipeline)
(require 'niwa-session-artifacts)
(require 'niwa-session-durability)
(require 'niwa-session-persistence)
(require 'niwa-session-publication)
(require 'niwa-structs)
(require 'niwa-tools)
(require 'niwa-transcript-audit)
(require 'niwa-transcript-restore)
(require 'niwa-tool-render-data)
(require 'niwa-workspace)
(require 'niwa-workspace-identity)

;;
;;; Archive projection and settlement

(defun niwa-execution-transcript-test--persisted-audit-transcript (text)
  "Return a persisted Org transcript containing trusted audit TEXT."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n" text)
    (niwa-session-artifacts-stabilize-gptel-bounds)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun niwa-execution-transcript-test--restored-audit-records (text type)
  "Return trusted audit records of TYPE restored from persisted TEXT."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (org-mode))
    (niwa-transcript-restore-properties)
    (niwa-transcript-audit-records (buffer-string) type)))

(defun niwa-execution-transcript-test--audit-records-in-file (path type)
  "Return trusted audit records of TYPE restored from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (niwa-execution-transcript-test--restored-audit-records
     (buffer-string) type)))

(defun niwa-execution-transcript-test--publish (session entries)
  "Make ENTRIES SESSION's verified publication.
ENTRIES is an alist of logical names and content strings.  Archives resolve
only through a publication, never a plain file under the save path."
  (setf (niwa-session-publication session)
        (list :artifacts
              (cl-loop for (logical . content) in entries
                       for bytes = (encode-coding-string content 'utf-8-unix)
                       for published = (make-temp-file
                                        (file-name-concat
                                         (niwa-session-save-path session)
                                         "published-"))
                       do (let ((coding-system-for-write 'no-conversion))
                            (write-region bytes nil published nil 'silent))
                       collect (list logical :published published
                                     :sha256 (secure-hash 'sha256 bytes))))))

(niwa-deftest niwa-execution-transcript--update-archived-render-data ()
  ,test
  (test)
  :doc "retained Agent summary reaches its own archived row through refresh"
  (dolist (nested '(nil t))
    (let* ((directory (make-temp-file "niwa-summary-archive-" t))
           (parent (generate-new-buffer " *summary-parent*"))
           (session (niwa-session--create :save-path directory))
           (archive "agents/parent.compact-0001.chat.org")
           (id (if nested "outer/1" "outer"))
           (summary '(:attempt-count 3 :started-count 3 :refused-count 0
                      :sandbox off :filesystem unrestricted :network unrestricted))
           (invocation (niwa-agent-invocation--create
                        :agent-id "retained" :path "/root/retained"
                        :parent-data-buffer parent :parent-tool-use-id id
                        :sandbox-summary-cell (list summary))))
      (unwind-protect
          (progn
            (niwa-execution-transcript-test--publish
             session
             (list
              (cons archive
                    (with-temp-buffer
                      (niwa--transcript-org-mode)
                      (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
                      (insert "#+begin_tool (Agent)\n")
                      (insert (propertize
                               (concat "(:name \"Agent\" :args nil)\n\nstarted\n"
                                       (niwa-tool-render-data-format
                                        (if nested
                                            '(:kind ptc :calls
                                              ((:id "outer/1" :tool "Agent"
                                                :render-data (:kind collaboration-event :agent-id "retained"))))
                                          '(:kind collaboration-event :agent-id "retained"))
                                        "outer"))
                               'gptel '(tool . "outer")))
                      (insert "#+end_tool\n")
                      (niwa-session-artifacts-stabilize-gptel-bounds)
                      (buffer-substring-no-properties (point-min) (point-max))))))
            (with-current-buffer parent
              (setq-local niwa--session session
                          niwa--agent-invocation invocation
                          buffer-file-name (file-name-concat directory "agents/parent.chat.org"))
              (insert "unsent draft must stay here\n"))
            (cl-letf (((symbol-function 'niwa-agent-conversation--sync-entry) #'ignore)
                      ((symbol-function 'niwa-agent-conversation--render-data-bounds)
                       (lambda (&rest _) nil)))
              (let (diagnostics)
                (niwa-test--with-captured-diagnostics diagnostics
                  (cl-letf (((symbol-function 'niwa-session-artifacts-publish-transcript-state)
                             (lambda (&rest _) (error "Injected archive failure"))))
                    (niwa-agent-conversation-refresh invocation)))
                (should (string-match-p "Injected archive failure" diagnostics)))
              (with-current-buffer parent
                (should (gethash id niwa-execution-transcript--pending-terminals)))
              ;; The publication contract is tested elsewhere; adopt its bytes.
              (cl-letf (((symbol-function 'niwa-session-artifacts-publish-transcript-state)
                         (lambda (session _root path content &rest _)
                           (niwa-execution-transcript-test--publish
                            session
                            (list (cons (file-relative-name
                                         path (niwa-session-save-path session))
                                        content))))))
                (niwa-agent-conversation-refresh invocation)
                ;; Identical refreshes must not parse/rewrite the archived file.
                (cl-letf (((symbol-function 'niwa-session-artifacts-read-transcript-segment)
                           (lambda (&rest _) (error "Redundant archive read"))))
                  (niwa-agent-conversation-refresh invocation))))
            (with-current-buffer parent
              (should (= 0 (hash-table-count niwa-execution-transcript--pending-terminals)))
              (should (equal "unsent draft must stay here\n" (buffer-string))))
            (with-temp-buffer
              (insert (decode-coding-string
                       (niwa-session-artifacts-read-artifact session archive)
                       'utf-8-unix))
              (niwa--transcript-org-mode)
              (niwa-transcript-restore-properties)
              (should (equal summary (plist-get
                                      (niwa-tool-render-data-for-tool (current-buffer) id)
                                      :sandbox-summary)))
              (should-not (string-search "unsent draft" (buffer-string)))))
        (when (buffer-live-p parent)
          (with-current-buffer parent (set-buffer-modified-p nil))
          (kill-buffer parent))
        (delete-directory directory t)))))

(niwa-deftest niwa-execution-transcript-terminal-render-data ()
  ,test
  (test)
  :doc "preserves reviewed-command provenance in terminal transcript metadata"
  (let* ((approvals '((:source reviewer :outcome allow-once :reason "Reviewed")))
         (render-data
          (niwa-execution-transcript-terminal-render-data
           (list :facts '(:outcome failure)
                 :observation (list :permission-approvals approvals))))
         (block (niwa-tool-render-data-format render-data "approved-call")))
    (should (eq 'error (plist-get render-data :status)))
    (should (equal approvals
                   (plist-get render-data :permission-approvals)))
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_tool (Bash :command \"false\")\n")
      (let ((start (point)))
        (insert "(:name \"Bash\" :args (:command \"false\"))\n\nfailed" block)
        (put-text-property start (point) 'gptel '(tool . "approved-call")))
      (insert "#+end_tool\n")
      (let ((persisted
             (progn
               (niwa-session-artifacts-stabilize-gptel-bounds)
               (buffer-substring-no-properties (point-min) (point-max)))))
        (erase-buffer)
        (insert persisted)
        (niwa-transcript-restore-properties)
        (should
         (equal approvals
                (plist-get
                 (niwa-tool-render-data-for-tool (current-buffer) "approved-call")
                 :permission-approvals))))))
  :doc "omits the default sandbox boundary"
  (should-not
   (plist-member
    (niwa-execution-transcript-terminal-render-data
     '(:facts (:outcome success)
              :observation
              (:sandbox-summary
               (:attempt-count 1 :started-count 1 :refused-count 0
                               :sandbox bubblewrap :filesystem workspace-write
                               :network isolated :proc fresh
                               :additional-read-count 0 :additional-write-count 0))))
    :sandbox-summary))
  :doc "omits additional read-only access"
  (should-not
   (plist-member
    (niwa-execution-transcript-terminal-render-data
     '(:facts (:outcome success)
              :observation
              (:sandbox-summary
               (:attempt-count 1 :started-count 1 :refused-count 0
                               :sandbox bubblewrap :filesystem workspace-write
                               :network isolated :proc fresh
                               :additional-read-count 2 :additional-write-count 0))))
    :sandbox-summary))
  :doc "keeps a material sandbox boundary"
  (should
   (plist-get
    (niwa-execution-transcript-terminal-render-data
     '(:facts (:outcome success)
              :observation
              (:sandbox-summary
               (:attempt-count 1 :started-count 1 :refused-count 0
                               :sandbox bubblewrap :filesystem workspace-write
                               :network isolated :proc fresh
                               :additional-read-count 0 :additional-write-count 1))))
    :sandbox-summary))
  :doc "reports cumulative preview truncation after individually complete polls"
  (let* ((niwa-execution-inline-output-limit 2000)
         (record (niwa-execution--record-create)))
    (dotimes (i 3)
      (niwa-execution--retain-output record (make-string 1000 (+ ?A i)))
      (let ((range (niwa-execution--unread-preview record 1000)))
        (should (= 0 (plist-get range :omitted)))
        (niwa-execution--consume-unread-range
         record (list :end (* 1000 (1+ i))
                      :omitted (plist-get range :omitted)))))
    (should (= 0 (niwa-execution--record-omitted-output-bytes record)))
    (let ((render-data
           (niwa-execution-transcript-terminal-render-data
            (list :facts (list :state 'completed :outcome 'success
                               :omitted-output-bytes 0)
                  :whole-output (niwa-execution--whole-preview record)
                  :whole-output-truncated-p
                  (> (niwa-execution--record-output-chars record)
                     niwa-execution-inline-output-limit)))))
      (should (plist-get render-data :output-preview-truncated-p))
      (should (string-match-p "omitted 1000 chars"
                              (plist-get render-data :execution-output)))))
  :doc "keeps a launch failure diagnostic separate from canonical stdout"
  (let ((render-data
         (niwa-execution-transcript-terminal-render-data
          '(:facts (:state completed :outcome failure :termination spawn-failed)
            :whole-output "actual output"
            :observation (:error "spawning child process: no such file: bash")))))
    (should (equal "actual output" (plist-get render-data :execution-output)))
    (should (equal "spawning child process: no such file: bash"
                   (plist-get render-data :execution-error)))
    (should (eq 'error (plist-get render-data :status)))))
(niwa-deftest niwa-execution-transcript-handle-event ()
  ,test
  (test)
  :doc "persists terminal output without live View streaming"
  (with-temp-buffer
    (insert "#+begin_tool (Bash :command \"true\")\n")
    (let ((start (point)))
      (insert
       "(:name \"Bash\" :args (:command \"true\"))\n\nrunning"
       (niwa-tool-render-data-format
        '(:execution-id "exec-terminal" :state running
                        :live-execution-p t)
        "terminal-call"))
      (put-text-property start (point) 'gptel '(tool . "terminal-call")))
    (insert "#+end_tool\n")
    (should-not
     (niwa-execution-transcript-handle-event
      (list :type 'terminal :data-buffer (current-buffer)
            :tool-use-id "terminal-call" :whole-output "finished"
            :facts '(:execution-id "exec-terminal" :state completed
                                   :outcome success))))
    (let ((render-data
           (niwa-tool-render-data-for-tool
            (current-buffer) "terminal-call")))
      (should (equal "finished"
                     (plist-get render-data :execution-output)))
      (should-not (plist-get render-data :live-execution-p))))
  :doc "records exactly one yielded completion, not foreground settlement"
  (with-temp-buffer
    (let ((event (list :type 'terminal :data-buffer (current-buffer)
                       :tool-use-id "tool-one" :owner "/root"
                       :whole-output "ok"
                       :facts '(:execution-id "exec-one" :command "true"
                               :state completed :outcome success))))
      (niwa-execution-transcript-handle-event event)
      (niwa-execution-transcript-handle-event event)
      (should (= 1 (length (niwa-transcript-audit-records
                            (buffer-string) 'execution-breadcrumb))))
      (should (equal "exec-one"
                     (plist-get
                      (car (niwa-transcript-audit-records
                            (buffer-string) 'execution-breadcrumb))
                      :execution-id)))
      (niwa-execution-transcript-handle-event
       (list :type 'terminal :data-buffer (current-buffer)
             :tool-use-id "tool-foreground" :owner "/root"
             :facts '(:execution-id nil :command "true" :state completed
                     :outcome success)))
      (should (= 1 (length (niwa-transcript-audit-records
                            (buffer-string) 'execution-breadcrumb))))))
  :doc "distinguishes simultaneous identical commands by execution identity"
  (with-temp-buffer
    (set-buffer-modified-p nil)
    (dolist (id '("exec-a" "exec-b"))
      (niwa-execution-transcript-handle-event
       (list :type 'terminal :data-buffer (current-buffer)
             :tool-use-id id :owner "/root"
             :facts (list :execution-id id :command "sleep 1"
                          :state 'completed :outcome 'success))))
    (should (= 2 (length (niwa-transcript-audit-records
                          (buffer-string) 'execution-breadcrumb))))
    (should (buffer-modified-p)))
  :doc "retains a new completion after reload when the execution counter restarts"
  (let (saved old-id new-id)
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
      (setq old-id (niwa-execution--next-id
                    (niwa-execution--new-state)))
      (niwa-execution-transcript-handle-event
       (list :type 'terminal :data-buffer (current-buffer)
             :owner "/root" :tool-use-id "before-reload"
             :facts (list :execution-id old-id :command "echo BEFORE"
                          :state 'completed :outcome 'success)))
      (niwa-session-artifacts-stabilize-gptel-bounds)
      (setq saved (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (org-mode)
      (insert saved)
      (niwa-transcript-restore-properties)
      (setq new-id (niwa-execution--next-id
                    (niwa-execution--new-state)))
      (niwa-execution-transcript-handle-event
       (list :type 'terminal :data-buffer (current-buffer)
             :owner "/root" :tool-use-id "after-reload"
             :facts (list :execution-id new-id :command "echo AFTER"
                          :state 'completed :outcome 'success)))
      (should-not (equal old-id new-id))
      (should (equal '("before-reload" "after-reload")
                     (mapcar (lambda (record) (plist-get record :tool-use-id))
                             (niwa-transcript-audit-records
                              (buffer-string) 'execution-breadcrumb)))))))

(niwa-deftest niwa-execution-transcript--record-breadcrumb ()
  ,test
  (test)
  :doc "defers project completions to one queued checkpoint of the live transcript"
  (niwa-test--with-captured-diagnostics nil
    (niwa-engine-test--with-session
      (insert "Saved input\n")
      (niwa-session-artifacts-save session buffer)
      (should-not (file-exists-p buffer-file-name))
      (let* ((logical (file-relative-name
                      buffer-file-name (niwa-session-save-path session)))
            (event (list :type 'terminal :data-buffer buffer :owner "/root"
                         :tool-use-id "project-call"
                         :facts '(:execution-id "project-exec" :command "true"
                                  :state completed :outcome success)))
            (published
             (lambda ()
               (niwa-execution-transcript-test--restored-audit-records
                (decode-coding-string
                 (niwa-session-artifacts-read-artifact session logical t)
                 'utf-8-unix)
                'execution-breadcrumb)))
            scheduled)
        (cl-letf (((symbol-function 'niwa-session-persistence-autosave-soon)
                   (lambda (buffer) (push buffer scheduled)))
                  ;; The fixture's admitted request runs checkpoints.
                  ((symbol-function 'niwa-session-persistence-checkpoints-active-p)
                   #'ignore))
          (dotimes (_ 2)
            (niwa-execution-transcript--record-breadcrumb event)))
        ;; Nothing is republished inline; one checkpoint is requested.
        (should-not (funcall published))
        (should (equal (list buffer) scheduled))
        (should (buffer-modified-p))
        (should (= 1 (length (niwa-transcript-audit-records
                              (buffer-string) 'execution-breadcrumb))))
        (niwa-session-persistence-autosave-buffer buffer)
        (should (= 1 (length (funcall published))))
        (should-not (file-exists-p buffer-file-name)))))
  :doc "relies on in-flight checkpoints instead of queueing another"
  (niwa-test--with-captured-diagnostics nil
    (niwa-engine-test--with-session
      (insert "Saved input\n")
      (niwa-session-artifacts-save session buffer)
      (let (scheduled)
        (cl-letf (((symbol-function 'niwa-session-persistence-autosave-soon)
                   (lambda (buffer) (push buffer scheduled)))
                  ((symbol-function 'niwa-session-persistence-checkpoints-active-p)
                   #'always))
          (niwa-execution-transcript--record-breadcrumb
           (list :type 'terminal :data-buffer buffer :owner "/root"
                 :tool-use-id "running-call"
                 :facts '(:execution-id "running-exec" :command "true"
                          :state completed :outcome success))))
        (should-not scheduled)
        (should (buffer-modified-p)))))
  :doc "archived enrichment never checkpoints live fields or consumed mail"
  (niwa-test--with-captured-diagnostics nil
      (niwa-engine-test--with-session
        (let* ((plan '(:live (("old-call" :execution-id "exec-old"
                              :state running :live-execution-p t))))
               (event (list :data-buffer buffer :owner "/root" :tool-use-id "old-call"
                            :facts '(:execution-id "exec-old" :command "true"
                                     :state completed :outcome success)))
               logical id)
          (insert "Saved input\n")
          (insert (niwa-execution-transcript-archive-text plan))
          (niwa-session-artifacts-save session buffer)
          (setq logical (file-relative-name buffer-file-name (niwa-session-save-path session)))
          (niwa-execution-transcript-commit-archive buffer plan)
          (niwa-agent-control-send-message session "/root" "pending delivery")
          (setq id (plist-get (car (niwa-session-messages session)) :id))
          (niwa-tools--handle-message-inject
           (gptel-make-fsm :info (list :buffer buffer :backend nil :data (list :messages []))))
          (should-not (niwa-session-messages session))
          (setf (niwa-session-turn-count session) 42)
          (goto-char (point-max))
          (insert "Unsent draft\n")
          (niwa-execution-transcript--record-archived-terminal
           buffer event (niwa-execution-transcript-terminal-render-data event))
          (let ((published (decode-coding-string
                            (niwa-session-artifacts-read-artifact session logical t)
                            'utf-8-unix))
                (sidecar (car (read-from-string
                               (decode-coding-string
                                (niwa-session-artifacts-read-artifact session "session.meta.el" t)
                                'utf-8-unix)))))
            (should-not (string-search "pending delivery" published))
            (should-not (string-search "Unsent draft" published))
            (should (= 0 (plist-get sidecar :total-turn-count)))
            (should (equal (list id) (mapcar (lambda (mail) (plist-get mail :id))
                                             (plist-get sidecar :messages))))
            (should (= 1 (length (niwa-execution-transcript-test--restored-audit-records
                                  published 'execution-completion))))))))

  :doc "records one completion in memory without writing the file"
  (let* ((path (make-temp-file "niwa-execution-breadcrumb-"))
         (buffer (find-file-noselect path))
         (session (niwa-session--create :name "breadcrumb")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (setq-local niwa--session session)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime)
            (insert "draft not yet sent\n"))
          (let ((event
                 (list :type 'terminal :data-buffer buffer :owner "/root"
                       :tool-use-id "call-b" :emitted-at 17.0
                       :facts '(:execution-id "exec-b" :command "exit 1"
                               :state completed :outcome failure :exit-code 1))))
            (niwa-execution-transcript--record-breadcrumb event)
            (niwa-execution-transcript--record-breadcrumb event))
          (with-current-buffer buffer
            (should (buffer-modified-p))
            (should (= 1 (length (niwa-transcript-audit-records
                                  (buffer-string) 'execution-breadcrumb)))))
          (should-not (niwa-execution-transcript-test--audit-records-in-file
                       path 'execution-breadcrumb))
          (with-temp-buffer
            (insert-file-contents path)
            (should-not (string-search "draft not yet sent" (buffer-string)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file path)))
  :doc "keeps flushed stream text eligible for checkpoint after a completion"
  (let* ((path (make-temp-file "niwa-execution-breadcrumb-batch-"))
         (buffer (find-file-noselect path))
         (session (niwa-session--create :name "breadcrumb-batch"))
         (niwa-gptel-stream-bridge-insert-batch-delay 60)
         (advice #'niwa-gptel-stream-bridge--gptel-stream-insert-response-advice)
         (installed (advice-member-p advice 'gptel-curl--stream-insert-response))
         info)
    (unwind-protect
        (with-current-buffer buffer
          (delay-mode-hooks (org-mode))
          (setq-local niwa--session session)
          (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
          (setq info (list :buffer buffer :position (point-marker)))
          (setq-local niwa--current-request
                      (niwa-request--create :fsm (gptel-make-fsm :info info)))
          (unless installed
            (advice-add 'gptel-curl--stream-insert-response :around advice))
          (gptel-curl--stream-insert-response "Checkpointed." info)
          (niwa-gptel-stream-bridge--flush-gptel-stream-insert-batch info)
          (niwa-session-artifacts-stabilize-gptel-bounds)
          (write-region (point-min) (point-max) path nil 'silent)
          (set-buffer-modified-p nil)
          (set-visited-file-modtime)
          (gptel-curl--stream-insert-response " Unsaved batch." info)
          (should-not (buffer-modified-p))
          (niwa-execution-transcript--record-breadcrumb
           (list :data-buffer buffer :owner "/root" :tool-use-id "call-batch"
                 :facts '(:execution-id "exec-batch" :command "true"
                          :state completed :outcome success)))
          (should (string-search "Unsaved batch." (buffer-string)))
          (with-temp-buffer
            (insert-file-contents path)
            (should-not (string-search "Unsaved batch." (buffer-string))))
          (should (buffer-modified-p))
          ;; The next checkpoint publishes the flushed response and the
          ;; completion at its in-memory placement.
          (niwa-session-artifacts-stabilize-gptel-bounds)
          (niwa-session-artifacts-save-buffer-silently)
          (should-not (buffer-modified-p))
          (with-temp-buffer
            (insert-file-contents path)
            (delay-mode-hooks (org-mode))
            (niwa-transcript-restore-properties)
            (let ((spans (niwa-transcript-audit-buffer-spans
                          'execution-breadcrumb)))
              (should (= 1 (length spans)))
              (should (equal "exec-batch"
                             (plist-get (plist-get (car spans) :record)
                                        :execution-id)))
              (goto-char (point-min))
              (should (< (search-forward "Checkpointed.")
                         (search-forward " Unsaved batch.")
                         (plist-get (car spans) :start)))
              (should (eq 'response
                          (get-text-property (match-beginning 0) 'gptel)))
              ;; gptel's trailing newline remains after the record; the
              ;; completion is at the active response, not the disk tail.
              (should (< (niwa-transcript--audit-block-end
                          (current-buffer) (plist-get (car spans) :end))
                         (point-max))))))
      (when info
        (niwa-gptel-stream-bridge--flush-gptel-stream-insert-batch info))
      (unless installed
        (advice-remove 'gptel-curl--stream-insert-response advice))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file path)))
  :doc "never republishes the transcript for a completion"
  (with-temp-buffer
    (set-buffer-modified-p nil)
    (cl-letf (((symbol-function 'niwa-execution-transcript--persist-terminal-record)
               (lambda (&rest _) (ert-fail "Completion republished the transcript"))))
      (niwa-execution-transcript--record-breadcrumb
       (list :data-buffer (current-buffer) :owner "/root" :tool-use-id "call-x"
             :facts '(:execution-id "exec-x" :command "true"
                      :state completed :outcome success))))
    (should (buffer-modified-p))
    (should (= 1 (length (niwa-transcript-audit-records
                          (buffer-string) 'execution-breadcrumb)))))
  :doc "places a mid-turn completion where it finished, before later output"
  (with-temp-buffer
    (org-mode)
    (let ((info (list :buffer (current-buffer) :position (point-marker)))
          (event (list :data-buffer (current-buffer) :owner "/root"
                       :tool-use-id "call-mid"
                       :facts '(:execution-id "exec-mid" :command "sleep 15"
                                :state completed :outcome success))))
      (setq-local niwa--current-request
                  (niwa-request--create :fsm (gptel-make-fsm :info info)))
      (gptel-curl--stream-insert-response "Earlier text." info)
      (niwa-execution-transcript--record-breadcrumb event)
      (niwa-execution-transcript--record-breadcrumb event)
      (gptel-curl--stream-insert-response " Later text." info)
      (let ((spans (niwa-transcript-audit-buffer-spans
                    'execution-breadcrumb)))
        (should (= 1 (length spans)))
        (goto-char (point-min))
        (should (< (search-forward "Earlier text.")
                   (plist-get (car spans) :start)
                   (search-forward " Later text."))))))
  :doc "places a forwarded child completion at the parent's running response"
  (with-temp-buffer
    (org-mode)
    (let ((info (list :buffer (current-buffer) :position (point-marker))))
      (setq-local niwa--current-request
                  (niwa-request--create :fsm (gptel-make-fsm :info info)))
      (gptel-curl--stream-insert-response "Parent text." info)
      (niwa-execution-transcript--record-breadcrumb
       (list :data-buffer (current-buffer) :owner "/root/worker"
             :tool-use-id "call-child"
             :facts '(:execution-id "exec-child" :command "make"
                      :state completed :outcome success)))
      (gptel-curl--stream-insert-response " Parent later." info)
      (let ((span (car (niwa-transcript-audit-buffer-spans
                        'execution-breadcrumb))))
        (should (equal "/root/worker" (plist-get (plist-get span :record) :owner)))
        (goto-char (point-min))
        (should (< (search-forward "Parent text.")
                   (plist-get span :start)
                   (search-forward " Parent later."))))))
  :doc "places an agent's mid-turn completion at the agent's response"
  (with-temp-buffer
    (org-mode)
    (insert "Agent text.")
    (let ((position (point-marker)))
      (insert "\nunrelated tail\n")
      (setq-local niwa--current-request nil)
      (setq-local niwa--agent-invocation
                  (niwa-agent-invocation--create
                   :runtime-context (list :buffer (current-buffer)
                                          :position position)))
      (niwa-execution-transcript--record-breadcrumb
       (list :data-buffer (current-buffer) :owner "/root/explorer"
             :tool-use-id "call-agent"
             :facts '(:execution-id "exec-agent" :command "make"
                      :state completed :outcome success)))
      (let ((span (car (niwa-transcript-audit-buffer-spans
                        'execution-breadcrumb))))
        (goto-char (point-min))
        (should (< (search-forward "Agent text.")
                   (plist-get span :start)
                   (search-forward "unrelated tail"))))))
  :doc "checkpoints a mid-turn completion at the response marker"
  (let* ((path (make-temp-file "niwa-execution-breadcrumb-mid-"))
         (buffer (find-file-noselect path))
         (session (niwa-session--create :name "breadcrumb-mid")))
    (unwind-protect
        (with-current-buffer buffer
          (delay-mode-hooks (org-mode))
          (setq-local niwa--session session)
          (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
          (let ((info (list :buffer buffer :position (point-marker))))
            (setq-local niwa--current-request
                        (niwa-request--create
                         :fsm (gptel-make-fsm :info info)))
            (gptel-curl--stream-insert-response "Running." info)
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime)
            (niwa-execution-transcript--record-breadcrumb
             (list :data-buffer buffer :owner "/root" :tool-use-id "call-pub"
                   :facts '(:execution-id "exec-pub" :command "true"
                            :state completed :outcome success)))
            (should (buffer-modified-p))
            (let ((span (car (niwa-transcript-audit-buffer-spans
                              'execution-breadcrumb))))
              (should (= (niwa-transcript--audit-block-end
                          buffer (plist-get span :end))
                         (niwa--active-response-marker info buffer)))
              (should (< (plist-get span :end) (point-max))))
            ;; The next ordinary save writes the record before later output.
            (gptel-curl--stream-insert-response " Later." info)
            (write-region (point-min) (point-max) path nil 'silent)
            (with-temp-buffer
              (insert-file-contents path)
              (let ((record (string-search "niwa-hook-audit" (buffer-string))))
                (should (< (string-search "Running." (buffer-string))
                           record
                           (string-search " Later." (buffer-string))))))
            (set-buffer-modified-p nil)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file path))))

(niwa-deftest niwa-execution-transcript--breadcrumb-archived-retry ()
  ,test
  (test)
  :doc "a retry after compaction does not append a second completion breadcrumb"
  (let* ((root (make-temp-file "niwa-breadcrumb-archives-" t))
         (session (niwa-session--create :save-path root
                                           :current-segment 3)))
    (unwind-protect
        (with-temp-buffer
          (let* ((current (current-buffer))
                 (event (list :type 'terminal :data-buffer current
                              :owner "/root" :tool-use-id "original"
                              :facts '(:execution-id "exec-1" :command "true"
                                      :state completed :outcome success))))
            (niwa-execution-transcript--record-breadcrumb event)
            (let ((text (niwa-execution-transcript-test--persisted-audit-transcript
                         (buffer-string))))
              (niwa-execution-transcript-test--publish
               session (list (cons "segment-0001.chat.org" text))))
            (erase-buffer)
            (setq buffer-file-name
                  (niwa-session-artifacts-segment-path root 3))
            (setq-local niwa--session session)
            (should (equal '(readable missing readable)
                           (mapcar (lambda (descriptor)
                                     (plist-get descriptor :status))
                                   (niwa-session-artifacts-transcript-segments
                                    session current))))
            (let ((before (buffer-list)))
              (cl-letf (((symbol-function
                          'niwa-execution-transcript--persist-terminal-record)
                         (lambda (&rest _) (ert-fail "Duplicate completion"))))
                (niwa-execution-transcript--record-breadcrumb event))
              (should-not (cl-set-difference (buffer-list) before)))
            (should-not (niwa-transcript-audit-records
                         (buffer-string) 'execution-breadcrumb))))
      (delete-directory root t))))

(niwa-deftest niwa-execution-transcript--segment-breadcrumb-keys ()
  ,test
  (test)
  :doc "is nil without a readable file"
  (let* ((root (make-temp-file "niwa-breadcrumb-missing-" t))
         (session (niwa-session--create :save-path root))
         (niwa-execution-transcript--segment-breadcrumbs
          (make-hash-table :test #'equal)))
    (unwind-protect
        (should-not (niwa-execution-transcript--segment-breadcrumb-keys
                     session (list :number 1 :status 'readable)))
      (delete-directory root t)))
  :doc "reads an archived segment once and rereads it only after a rewrite"
  (let* ((root (make-temp-file "niwa-breadcrumb-keys-" t))
         (archive (niwa-session-artifacts-segment-path root 1))
         (session (niwa-session--create :save-path root :current-segment 2))
         (descriptor (list :number 1 :path archive :status 'readable))
         (niwa-execution-transcript--segment-breadcrumbs (make-hash-table :test #'equal))
         (write (lambda (id)
                  (let ((text (with-temp-buffer
                                (niwa-execution-transcript--record-breadcrumb
                                 (list :type 'terminal :data-buffer (current-buffer)
                                       :owner "/root" :tool-use-id "call"
                                       :facts (list :execution-id id :command "true"
                                                    :state 'completed :outcome 'success)))
                                (niwa-execution-transcript-test--persisted-audit-transcript
                                 (buffer-string)))))
                    (niwa-execution-transcript-test--publish
                     session (list (cons "segment-0001.chat.org" text)))))))
    (unwind-protect
        (progn
          (funcall write "exec-1")
          (should (equal '(("/root" . "exec-1"))
                         (niwa-execution-transcript--segment-breadcrumb-keys
                          session descriptor)))
          (cl-letf (((symbol-function 'niwa-session-artifacts-read-transcript-segment)
                     (lambda (&rest _) (ert-fail "Archived segment reread"))))
            (should (equal '(("/root" . "exec-1"))
                           (niwa-execution-transcript--segment-breadcrumb-keys
                            session descriptor))))
          ;; A rewind can rewrite a segment; a different published hash rereads it.
          (funcall write "exec-two")
          (should (equal '(("/root" . "exec-two"))
                         (niwa-execution-transcript--segment-breadcrumb-keys
                          session descriptor))))
      (delete-directory root t))))

(niwa-deftest niwa-execution-transcript--breadcrumb-keys ()
  ,test
  (test)
  :doc "lists owner and execution identity of each completion breadcrumb"
  (with-temp-buffer
    (dolist (id '("exec-a" "exec-b"))
      (niwa-execution-transcript--record-breadcrumb
       (list :type 'terminal :data-buffer (current-buffer) :owner "/root"
             :tool-use-id id
             :facts (list :execution-id id :command "true"
                          :state 'completed :outcome 'success))))
    (should (equal '(("/root" . "exec-a") ("/root" . "exec-b"))
                   (niwa-execution-transcript--breadcrumb-keys (current-buffer))))))

(niwa-deftest niwa-execution-transcript--record-nested-terminal ()
  ,test
  (test)
  :doc "retains a ToolCall child result independently of the ephemeral pending table"
  (let* ((path (make-temp-file "niwa-execution-nested-"))
         (buffer (find-file-noselect path))
         (session (niwa-session--create :name "nested completion"))
         (event (list :type 'terminal :data-buffer buffer :owner "/root"
                      :tool-use-id "outer/1" :whole-output "whole output"
                      :facts '(:execution-id "exec-nested" :command "printf ok"
                              :state completed :outcome success))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (setq-local niwa--session session)
            (niwa-transcript-enable-gptel-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime))
          (niwa-execution-transcript-handle-event event)
          (niwa-execution-transcript-handle-event event)
          (with-current-buffer buffer
            (should (equal "whole output"
                           (plist-get
                            (niwa-execution-transcript-pending-render-data
                             buffer "outer/1")
                            :execution-output)))
            (should (= 1 (length (niwa-transcript-audit-records
                                  (buffer-string) 'execution-completion))))
            ;; A workspace-free transcript persists through its ordinary save.
            (should (buffer-modified-p))
            (save-buffer))
          (should (= 1 (length
                        (niwa-execution-transcript-test--audit-records-in-file
                         path 'execution-completion))))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)
          (setq buffer (find-file-noselect path))
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (niwa-transcript-restore-properties)
            (let ((niwa-execution-transcript--pending-terminals nil))
              (should (equal "whole output"
                             (plist-get
                              (niwa-execution-transcript-pending-render-data
                               buffer "outer/1")
                              :execution-output))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file path))))

(niwa-deftest niwa-execution-transcript--persist-terminal-record ()
  ,test
  (test)
  :doc "publishes the terminal Bash row and breadcrumb together, not an unsent draft"
  (let* ((path (make-temp-file "niwa-execution-row-"))
         (buffer (find-file-noselect path))
         (session (niwa-session--create :name "terminal row")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (setq-local niwa--session session)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"
                    "#+begin_tool (Bash :command \"printf ok\")\n")
            (let ((start (point)))
              (insert "(:name \"Bash\" :args (:command \"printf ok\"))\n\nrunning"
                      (niwa-tool-render-data-format
                       '(:execution-id "exec-row" :state running
                         :live-execution-p t) "call-row"))
              (put-text-property start (point) 'gptel '(tool . "call-row")))
            (insert "#+end_tool\n")
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime)
            (should (eq 'running
                        (plist-get
                         (niwa-tool-render-data-for-tool buffer "call-row")
                         :state)))
            (insert "unsent composer draft\n"))
          (niwa-execution-transcript-handle-event
           (list :type 'terminal :data-buffer buffer :owner "/root"
                 :tool-use-id "call-row" :whole-output "ok"
                 :facts '(:execution-id "exec-row" :command "printf ok"
                         :state completed :outcome success :exit-code 0)))
          ;; The live row settles now; the file follows at the next checkpoint.
          (with-current-buffer buffer
            (let ((data (niwa-tool-render-data-for-tool buffer "call-row")))
              (should (eq 'completed (plist-get data :state)))
              (should (equal "ok" (plist-get data :execution-output)))
              (should-not (plist-get data :live-execution-p)))
            (should (buffer-modified-p))
            (should (= 1 (length (niwa-transcript-audit-records
                                  (buffer-string) 'execution-breadcrumb)))))
          (with-temp-buffer
            (insert-file-contents path)
            (should-not (string-search "unsent composer draft" (buffer-string)))
            (delay-mode-hooks (org-mode))
            (niwa-transcript-restore-properties)
            (should (eq 'running
                        (plist-get (niwa-tool-render-data-for-tool
                                    (current-buffer) "call-row")
                                   :state)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file path)))
  :doc "parses the saved copy without running the user's Org hooks"
  (let* ((root (make-temp-file "niwa-execution-bare-" t))
         (path (file-name-concat root "segment-0001.chat.org"))
         (header ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
         (buffer (find-file-noselect path))
         ;; Explicit portable authority over a storage-only fixture.
         (session (niwa-session--create :name "bare copy" :save-path root
                                           :authority-mode 'portable))
         (hook-runs 0)
         published)
    (unwind-protect
        (progn
          (niwa-execution-transcript-test--publish
           session (list (cons "segment-0001.chat.org" header)))
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (setq-local niwa--session session)
            (insert header)
            (set-buffer-modified-p nil))
          (let ((org-mode-hook (list (lambda () (cl-incf hook-runs)))))
            (with-current-buffer buffer
              (cl-letf (((symbol-function
                          'niwa-session-artifacts-publish-transcript-state)
                         (lambda (_session _root _path content &rest _)
                           (setq published content))))
                (should (niwa-execution-transcript--persist-terminal-record
                         (list :type 'execution-breadcrumb :execution-id "exec-bare"
                               :tool-use-id "call-bare" :owner "/root" :command "true"))))))
          (should (= 0 hook-runs))
          (with-temp-buffer
            (insert published)
            (delay-mode-hooks (org-mode))
            (niwa-transcript-restore-properties)
            (should (= 1 (length (niwa-transcript-audit-records
                                  (buffer-string) 'execution-breadcrumb))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t)))

  :doc "publishes nested completion and archive after the intermediate conversation is closed"
  (niwa-test--with-captured-diagnostics nil
    (niwa-engine-test--with-session
      (insert "Saved root input\n")
      (niwa-session-artifacts-save session buffer)
      (let* ((child (generate-new-buffer " *closed exec parent*"))
             (grandchild (generate-new-buffer " *retained exec descendant*"))
             (logical "agents/descendant.chat.org")
             (path (file-name-concat (niwa-session-save-path session) logical)))
        (unwind-protect
            (progn
              (with-current-buffer child
                (setq-local niwa--agent-invocation
                            (niwa-agent-invocation--create
                             :parent-data-buffer buffer)))
              (with-current-buffer grandchild
                (org-mode)
                (setq-local niwa--session session
                            niwa--agent-invocation
                            (niwa-agent-invocation--create
                             :parent-data-buffer child))
                (insert "Saved child input\n"
                        (niwa--format-hook-audit-record
                         '(:type execution-archive :tool-use-id "archived-call"
                           :owner "/root/parent/child")))
                (niwa-session-artifacts-stabilize-gptel-bounds)
                (setq buffer-file-name path)
                (niwa-session-artifacts-publish-text
                 session path (buffer-string) 'utf-8-unix)
                (kill-buffer child)
                (insert "Unsent child draft\n")
                (should
                 (niwa-execution-transcript--persist-terminal-record
                  '(:type execution-breadcrumb :execution-id "nested-exec"
                    :tool-use-id "nested-call" :owner "/root/parent/child")))
                ;; Even malformed ancestry cannot loop or change authority.
                (setf (niwa-agent-invocation-parent-data-buffer
                       niwa--agent-invocation) grandchild)
                (niwa-execution-transcript--record-archived-terminal
                 grandchild '(:tool-use-id "archived-call" :owner "/root/parent/child")
                 '(:state completed :outcome success))
                (should (string-search "Unsent child draft" (buffer-string))))
              (let ((published
                     (decode-coding-string
                      (niwa-session-artifacts-read-artifact session logical t)
                      'utf-8-unix)))
                (should-not (string-search "Unsent child draft" published))
                (should (= 1 (length
                              (niwa-execution-transcript-test--restored-audit-records
                               published 'execution-breadcrumb))))
                (should (= 1 (length
                              (niwa-execution-transcript-test--restored-audit-records
                               published 'execution-completion))))
                (should-not
                 (niwa-execution-transcript-test--restored-audit-records
                  published 'execution-archive))
                ;; A stale root must fail before committing any new bytes.
                (setf (niwa-session-root-buffer session) child)
                (unwind-protect
                    (with-current-buffer grandchild
                      (should-error
                       (niwa-execution-transcript--persist-terminal-record
                        '(:type execution-breadcrumb :execution-id "retired-root"
                          :tool-use-id "late-call" :owner "/root/parent/child")))
                      (should (equal published
                                     (decode-coding-string
                                      (niwa-session-artifacts-read-artifact
                                       session logical t)
                                      'utf-8-unix))))
                  (setf (niwa-session-root-buffer session) buffer))))
          (when (buffer-live-p grandchild)
            (with-current-buffer grandchild
              (setq buffer-file-name nil)
              (set-buffer-modified-p nil))
            (kill-buffer grandchild))
          (when (buffer-live-p child) (kill-buffer child)))))))

(niwa-deftest niwa-execution-transcript-prepare-archive ()
  ,test
  (test)
  :doc "separates active public states from completed rows without a view"
  (dolist (state '(queued running stopping))
  (with-temp-buffer
    (insert
     (propertize
      (niwa-tool-render-data-format
       `(:execution-id "exec-live" :state ,state)
       "live-call")
      'gptel '(tool . "live-call")))
    (insert
     (propertize
      (niwa-tool-render-data-format
       '(:execution-id "exec-done" :state completed
                       :live-execution-p nil)
       "done-call")
      'gptel '(tool . "done-call")))
    (let ((plan
           (niwa-execution-transcript-prepare-archive
            (current-buffer) '("live-call" "done-call" "missing-call"))))
      (should (equal "live-call" (caar (plist-get plan :live))))
      (should (eq state
                  (plist-get (cdar (plist-get plan :live)) :state)))
      (should (equal "done-call"
                     (caar (plist-get plan :completed)))))))

  :doc "resolves multiple archive misses with one transcript inspection"
  (with-temp-buffer
    (insert
     (niwa--format-hook-audit-record
      '(:type execution-archive :tool-use-id "archived"
              :render-data (:execution-id "exec-archived" :state running
                            :live-execution-p t))))
    (let ((read-records (symbol-function 'niwa-transcript-audit-records))
          (reads 0)
          (before (buffer-string)))
      (cl-letf (((symbol-function 'niwa-transcript-audit-records)
                 (lambda (&rest args)
                   (cl-incf reads)
                   (apply read-records args))))
        (should
         (equal
          '(:live (("archived" :execution-id "exec-archived" :state running
                     :live-execution-p t)) :completed nil)
          (niwa-execution-transcript-prepare-archive
           (current-buffer) '("missing-first" "archived" "missing-last")))))
      (should (equal-including-properties before (buffer-string)))
      (should (= 1 reads))))

  :doc "keeps live precedence and observes later archive changes without narrowing edits"
  (with-temp-buffer
    (insert
     (niwa--format-hook-audit-record
      '(:type execution-archive :tool-use-id "row"
              :render-data (:execution-id "old" :state running
                            :live-execution-p t)))
     (niwa--format-hook-audit-record
      '(:type execution-archive :tool-use-id "row"
              :render-data (:execution-id "later" :state running
                            :live-execution-p t))))
    (save-restriction
      (narrow-to-region (point-max) (point-max))
      (let ((begin (point-min)))
        (should (equal "old"
                       (plist-get
                        (cdar (plist-get
                               (niwa-execution-transcript-prepare-archive
                                (current-buffer) '("row")) :live))
                        :execution-id)))
        (should (= begin (point-min) (point-max)))))
    (erase-buffer)
    (insert
     (niwa--format-hook-audit-record
      '(:type execution-archive :tool-use-id "row"
              :render-data (:execution-id "new" :state completed))))
    (should (equal "new"
                   (plist-get
                    (cdar (plist-get
                           (niwa-execution-transcript-prepare-archive
                            (current-buffer) '("row")) :completed))
                    :execution-id)))
    (insert
     (propertize
      (niwa-tool-render-data-format
       '(:execution-id "live" :state running :live-execution-p t) "row")
      'gptel '(tool . "row")))
    (cl-letf (((symbol-function 'niwa-transcript-audit-records)
               (lambda (&rest _) (ert-fail "Live rows should not inspect archives"))))
      (should (equal "live"
                     (plist-get
                      (cdar (plist-get
                             (niwa-execution-transcript-prepare-archive
                              (current-buffer) '("row")) :live))
                      :execution-id))))))

(niwa-deftest niwa-execution-transcript-commit-archive ()
  ,test
  (test)
  :doc "marks live rows and persists already-completed rows after compaction"
  (with-temp-buffer
    (let ((plan
           '(:live (("live-call" :execution-id "exec-live"
                     :state running :live-execution-p t))
                   :completed (("done-call" :execution-id "exec-done"
                                :state completed)))))
      (insert (niwa-execution-transcript-archive-text plan))
      (niwa-execution-transcript-commit-archive
       (current-buffer) plan))
    (should (gethash "live-call"
                     niwa-execution-transcript--archived-rows))
    (should (= 1
               (length
                (niwa-transcript-audit-records
                 (buffer-string) 'execution-completion))))
    (should (= 1
               (length
                (niwa-transcript-audit-records
                 (buffer-string) 'execution-archive))))))

(niwa-deftest niwa-execution-transcript-pending-render-data ()
  ,test
  (test)
  :doc "returns a copy of pending terminal render data"
  (with-temp-buffer
    (niwa-execution-transcript-store-pending-terminal
     (current-buffer) '(:tool-use-id "pending-1")
     '(:execution-output "done"))
    (let ((render-data
           (niwa-execution-transcript-pending-render-data
            (current-buffer) "pending-1")))
      (should (equal '(:execution-output "done") render-data))
      (plist-put render-data :execution-output "changed")
      (should
       (equal
        "done"
        (plist-get
         (niwa-execution-transcript-pending-render-data
          (current-buffer) "pending-1")
         :execution-output)))))
  :doc "restores archived execution output without a live pending table"
  (with-temp-buffer
    (insert (niwa--format-hook-audit-record
             '(:type execution-completion :tool-use-id "archived-call"
               :render-data (:state completed :execution-output "archive output"))))
    (should (equal "archive output"
                   (plist-get
                    (niwa-execution-transcript-pending-render-data
                     (current-buffer) "archived-call")
                    :execution-output))))
  :doc "parses the transcript once per change and the newest record wins"
  (with-temp-buffer
    (let ((completion
           (lambda (id output)
             (niwa--format-hook-audit-record
              `(:type execution-completion :tool-use-id ,id
                :render-data (:execution-output ,output)))))
          (parses 0)
          (parse (symbol-function 'niwa-transcript-audit-records)))
      (insert (funcall completion "a" "old a") (funcall completion "b" "b"))
      (cl-letf (((symbol-function 'niwa-transcript-audit-records)
                 (lambda (&rest args) (cl-incf parses) (apply parse args))))
        (dolist (id '("a" "b" "a" "missing"))
          (niwa-execution-transcript-pending-render-data (current-buffer) id))
        (should (= 1 parses))
        (goto-char (point-max))
        (insert (funcall completion "a" "new a"))
        (should (equal "new a"
                       (plist-get (niwa-execution-transcript-pending-render-data
                                   (current-buffer) "a")
                                  :execution-output)))
        (should (= 2 parses))
        (let ((copy (niwa-execution-transcript-pending-render-data
                     (current-buffer) "b")))
          (plist-put copy :execution-output "changed"))
        (should (equal "b" (plist-get (niwa-execution-transcript-pending-render-data
                                       (current-buffer) "b")
                                      :execution-output)))))))

(niwa-deftest niwa-execution-transcript--record-archived-terminal ()
  ,test
  (test)
  :doc "keeps a workspace-free archived completion for ordinary save and reload"
  (let* ((path (make-temp-file "niwa-execution-archive-fallback-"))
         (buffer (find-file-noselect path))
         (session (niwa-session--create :name "archive fallback")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (setq-local niwa--session session)
            (insert (niwa-execution-transcript-test--persisted-audit-transcript
                     (niwa-execution-transcript-archive-text
                      '(:live (("failed-call" :execution-id "exec-failed"
                                :state running :live-execution-p t))))))
            (niwa-transcript-restore-properties)
            (niwa-transcript-enable-gptel-mode)
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime))
          (niwa-execution-transcript-commit-archive
           buffer '(:live (("failed-call" :execution-id "exec-failed"
                            :state running :live-execution-p t))))
          (niwa-execution-transcript-handle-event
           (list :type 'terminal :session session :data-buffer buffer
                 :owner "/root" :tool-use-id "failed-call"
                 :facts '(:execution-id "exec-failed" :command "false"
                         :state completed :outcome failure :exit-code 1)
                 :whole-output "failed output"))
          (with-current-buffer buffer
            (should (buffer-modified-p))
            (should (= 1 (length (niwa-transcript-audit-records
                                  (buffer-string) 'execution-completion))))
            (save-buffer)
            (should (= 1 (length (niwa-execution-transcript-test--audit-records-in-file
                                  path 'execution-completion))))
            (kill-buffer buffer))
          (setq buffer (find-file-noselect path))
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (niwa-transcript-restore-properties)
            (should (= 1 (length (niwa-transcript-audit-records
                                  (buffer-string) 'execution-completion))))
            (should (= 1 (length (niwa-transcript-audit-records
                                  (buffer-string) 'execution-breadcrumb))))
            (let ((data (cdar (plist-get
                               (niwa-execution-transcript-prepare-archive
                                buffer '("failed-call")) :completed))))
              (should (eq 'completed (plist-get data :state)))
              (should (equal "failed output"
                             (plist-get data :execution-output))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p path) (delete-file path))))
  :doc "reroutes a terminal queued before its archive marker commits"
  (let* ((path (make-temp-file "niwa-execution-reroute-"))
         (buffer (find-file-noselect path))
         (session (niwa-session--create :name "reroute"))
         (event
          (list :type 'terminal :session session :data-buffer buffer
                :owner "main" :tool-use-id "reroute-call"
                :facts '(:state completed :outcome success :exit-code 0)
                :whole-output "done")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local niwa--session session)
            (delay-mode-hooks (org-mode)))
          (niwa-execution-transcript-handle-event event)
          (with-current-buffer buffer
            (should
             (gethash "reroute-call"
                      niwa-execution-transcript--pending-terminals))
            (insert
             (niwa-execution-transcript-test--persisted-audit-transcript
              (niwa-execution-transcript-archive-text
               '(:live (("reroute-call" :execution-id "exec-reroute"
                         :state running :live-execution-p t))))))
            (niwa-transcript-restore-properties)
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime))
          (niwa-execution-transcript-commit-archive
           buffer '(:live (("reroute-call" :execution-id "exec-reroute"
                            :state running :live-execution-p t))))
          (niwa-execution-transcript-retry-pending-terminals buffer)
          (with-current-buffer buffer
            (should-not
             (gethash "reroute-call"
                      niwa-execution-transcript--pending-terminals))
            (should (= 1
                       (length
                        (niwa-transcript-audit-records
                         (buffer-string) 'execution-completion))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p path) (delete-file path))))
  :doc "updates committed project transcripts through local and remote access"
  ;; PARTIAL commits the publication but fails the live replacement, then
  ;; retries from a narrowed live buffer.
  (pcase-dolist (`(,remote ,partial) '((nil nil) (t nil) (nil t)))
    (let* ((host "archived-terminal-publication")
           (local-root
            (file-name-as-directory
             (make-temp-file "niwa-archived-terminal-" t)))
           (remote-root
            (if remote
                (format "/niwamock:%s:%s/"
                        host (directory-file-name local-root))
              local-root))
           (session-dir (concat remote-root "session/"))
           (root-segment (concat session-dir "segment-0001.chat.org"))
           (transcript (concat session-dir "agents/remote-call.chat.org"))
           (sidecar (concat session-dir "session.meta.el"))
           (archive
            (niwa-execution-transcript-test--persisted-audit-transcript
             (niwa-execution-transcript-archive-text
              '(:live (("remote-call" :execution-id "exec-remote"
                        :state running :live-execution-p t))))))
           (render-data
            '(:execution-id "exec-remote" :state completed
                            :status success :live-execution-p nil))
           (niwa-session-durability--client-id (make-string 64 ?a))
           (niwa-session-durability--disclosed-targets
            (make-hash-table :test #'equal))
           buffer event root-buffer session)
      (unwind-protect
          (niwa-test--with-local-shell-tramp (list host)
                                                (let ((workspace
                                                       (niwa-workspace--create
                                                        :type 'project :id remote-root :root remote-root
                                                        :name "remote")))
                                                  (setq session
                                                        (niwa-session-create "main" workspace remote-root)))
                                                (setq event
                                                      (list :type 'terminal :session session :owner "main"
                                                            :tool-use-id "remote-call"))
                                                (niwa-workspace-identity-ensure remote-root)
                                                (setf (niwa-execution-target-incarnation
                                                       (niwa-session-execution-target session))
                                                      "mock-incarnation")
                                                (setf (niwa-session-session-id session) "archived-terminal"
                                                      (niwa-session-save-path session) session-dir
                                                      (niwa-session-current-segment session) 1)
                                                (make-directory session-dir t)
                                                (puthash
                                                 (niwa-execution-target-identity
                                                  (niwa-session-execution-target session))
                                                 t niwa-session-durability--disclosed-targets)
                                                (should
                                                 (niwa-session-durability-lease-acquire
                                                  session-dir "*archived terminal*" session))
                                                (setq root-buffer (generate-new-buffer " *archived root*"))
                                                (niwa-session-set-root-buffer session root-buffer)
                                                (with-current-buffer root-buffer
                                                  (setq-local niwa--session session)
                                                  (setq buffer-file-name root-segment)
                                                  (insert "* Root\n"))
                                                (setq buffer (generate-new-buffer " *archived terminal*"))
                                                (with-current-buffer buffer
                                                  (setq-local niwa--session session)
                                                  (setq-local
                                                   niwa--agent-invocation
                                                   (niwa-agent-invocation--create
                                                    :parent-data-buffer root-buffer))
                                                  (setq buffer-file-name transcript)
                                                  (insert archive)
                                                  (delay-mode-hooks (org-mode))
                                                  (niwa-transcript-restore-properties))
                                                (should
                                                 (niwa-session-publication-publish
                                                  session
                                                  (list
                                                   (list :path transcript :content archive)
                                                   (list
                                                    :path sidecar
                                                    :content
                                                    (niwa-session-artifacts-printed-value
                                                     (niwa-session-artifacts-build-sidecar
                                                      session root-buffer))
                                                    :commit-marker t))))
                                                (make-directory (file-name-directory transcript) t)
                                                (write-region "poisoned fixed cache" nil transcript nil 'silent)
                                                (with-current-buffer buffer
                                                  (goto-char (point-max))
                                                  (insert "pending prompt\n")
                                                  (set-buffer-modified-p nil))
                                                (niwa-execution-transcript-commit-archive
                                                 buffer
                                                 '(:live (("remote-call" :execution-id "exec-remote"
                                                           :state running :live-execution-p t))))
                                                (let ((before
                                                       (with-current-buffer buffer
                                                         (buffer-string)))
                                                      (committed
                                                       (niwa-session-artifacts-read-artifact
                                                        session "agents/remote-call.chat.org" t)))
                                                  (cl-letf
                                                   (((symbol-function
                                                      'niwa-session-artifacts-publish-transcript-state)
                                                     (lambda (&rest _)
                                                       (error "Publication failed"))))
                                                   (should
                                                    (equal
                                                     '(error "Publication failed")
                                                     (should-error
                                                      (niwa-execution-transcript--record-archived-terminal
                                                       buffer event render-data)))))
                                                  (with-current-buffer buffer
                                                    (should (equal-including-properties
                                                             before (buffer-string)))
                                                    (should-not (buffer-modified-p))
                                                    (should (gethash
                                                             "remote-call"
                                                             niwa-execution-transcript--archived-rows)))
                                                  (should (equal
                                                           committed
                                                           (niwa-session-artifacts-read-artifact
                                                            session "agents/remote-call.chat.org" t))))
                                                (when partial
                                                  (let ((replace
                                                         (symbol-function
                                                          'niwa-execution-transcript--replace-archived-record)))
                                                    (cl-letf (((symbol-function
                                                                'niwa-execution-transcript--replace-archived-record)
                                                               (lambda (&rest args)
                                                                 (if (eq (current-buffer) buffer)
                                                                     (error "Live replacement failed")
                                                                   (apply replace args)))))
                                                      (should-error
                                                       (niwa-execution-transcript--record-archived-terminal
                                                        buffer event render-data))))
                                                  (should (= 1
                                                             (length
                                                              (niwa-execution-transcript-test--restored-audit-records
                                                               (decode-coding-string
                                                                (niwa-session-artifacts-read-artifact
                                                                 session "agents/remote-call.chat.org" t)
                                                                'utf-8-unix)
                                                               'execution-completion))))
                                                  (with-current-buffer buffer
                                                    (should (gethash
                                                             "remote-call"
                                                             niwa-execution-transcript--archived-rows))
                                                    (narrow-to-region (point-max) (point-max))))
                                                (dotimes (_ 2)
                                                  (niwa-execution-transcript--record-archived-terminal
                                                   buffer event render-data))
                                                (with-current-buffer buffer
                                                  (widen)
                                                  (should-not (gethash
                                                               "remote-call"
                                                               niwa-execution-transcript--archived-rows)))
                                                (let ((published
                                                       (decode-coding-string
                                                        (niwa-session-artifacts-read-artifact
                                                         session "agents/remote-call.chat.org" t)
                                                        'utf-8-unix)))
                                                  (should-not (string-search "poisoned" published))
                                                  (should-not (string-search "pending prompt" published))
                                                  (should (= 1
                                                             (length
                                                              (niwa-execution-transcript-test--restored-audit-records
                                                               published 'execution-completion)))))
                                                (with-current-buffer buffer
                                                  (should (string-search "pending prompt" (buffer-string)))
                                                  (should (= 1
                                                             (length
                                                              (niwa-transcript-audit-records
                                                               (buffer-string) 'execution-completion)))))
                                                ;; A late Agent summary must update its published
                                                ;; compaction archive, not the poisoned fixed cache.
                                                (let* ((logical "agents/remote-call.compact-0001.chat.org")
                                                       (path (file-name-concat session-dir logical))
                                                       (text
                                                        (with-temp-buffer
                                                          (niwa--transcript-org-mode)
                                                          (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
                                                          (insert "#+begin_tool (Agent)\n")
                                                          (insert (propertize
                                                                   (concat "(:name \"Agent\")\nstarted\n"
                                                                           (niwa-tool-render-data-format
                                                                            '(:kind collaboration-event :agent-id "child")
                                                                            "agent-call"))
                                                                   'gptel '(tool . "agent-call")))
                                                          (insert "#+end_tool\n")
                                                          (niwa-session-artifacts-stabilize-gptel-bounds)
                                                          (buffer-string)))
                                                       (updates '(:sandbox-summary
                                                                  (:attempt-count 1 :started-count 1
                                                                   :sandbox off))))
                                                  (niwa-session-artifacts-publish-transcript-state
                                                   session root-buffer path text nil)
                                                  (write-region "poisoned archive cache" nil path nil 'silent)
                                                  (niwa-execution-transcript-store-pending-terminal
                                                   buffer '(:tool-use-id "agent-call") updates)
                                                  (niwa-execution-transcript-retry-pending-terminals buffer)
                                                  (with-current-buffer buffer
                                                    (should-not (gethash "agent-call"
                                                                         niwa-execution-transcript--pending-terminals)))
                                                  (with-temp-buffer
                                                    (insert (decode-coding-string
                                                             (niwa-session-artifacts-read-artifact session logical t)
                                                             'utf-8-unix))
                                                    (niwa--transcript-org-mode)
                                                    (niwa-transcript-restore-properties)
                                                    (should (equal (plist-get updates :sandbox-summary)
                                                                   (plist-get
                                                                    (niwa-tool-render-data-for-tool
                                                                     (current-buffer) "agent-call")
                                                                    :sandbox-summary)))
                                                    (should-not (string-search "poisoned" (buffer-string)))
                                                    (should-not (string-search "pending prompt" (buffer-string))))))
        (when (and session (niwa-session-lease session))
          (ignore-errors
            (niwa-session-durability-lease-release session-dir session)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (when (buffer-live-p root-buffer)
          (with-current-buffer root-buffer (set-buffer-modified-p nil))
          (kill-buffer root-buffer))
        (when (file-directory-p local-root)
          (delete-directory local-root t))))))

(provide 'test-niwa-execution-transcript)

;;; test-niwa-execution-transcript.el ends here
