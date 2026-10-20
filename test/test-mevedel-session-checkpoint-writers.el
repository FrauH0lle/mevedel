;;; test-mevedel-session-checkpoint-writers.el --- Frozen writer regressions -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise real root and retained-agent writers with mailbox delivery reentering
;; after transcript freeze.  The older save must not mark the newer text clean,
;; and the next save must durably include that text and its delivery receipt.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-agent-runtime)

(defun test-mevedel-session-checkpoint-writers--configuration ()
  "Return a frozen agent configuration independent of the tool registry."
  (mevedel-agent-configuration--create
   :agent (mevedel-agent--create :name "default" :description "Checkpoint writer"
                                 :frozen-p t)
   :request-locals
   (mapcar (lambda (symbol)
             (cons symbol
                   (pcase symbol
                     ('gptel-backend (test-mevedel-session-persistence--agent-backend))
                     ('gptel-model 'test-model))))
           mevedel-agent-request-local-symbols)))

(defun test-mevedel-session-checkpoint-writers--save (writer session root invocation)
  "Run WRITER for SESSION, ROOT, and optional agent INVOCATION."
  (pcase writer
    ('root (mevedel-session-artifacts-save session root))
    ('agent (mevedel-agent-conversation-save invocation))
    ('agent-terminal
     (mevedel-session-artifacts-publish-agent-terminal-state invocation))))

(defun test-mevedel-session-checkpoint-writers--late-mail (writer)
  "Verify that WRITER preserves a mailbox delivery after transcript freeze."
  (let* ((directory (make-temp-file "mevedel-writer-late-mail-" t))
         (workspace (test-mevedel-session-persistence--make-workspace directory))
         (session (mevedel-session-create "main" workspace))
         (root (generate-new-buffer " *checkpoint-writer-root*"))
         (path (if (eq writer 'root) "/root" "/root/worker"))
         (payload (format "Late mail survives %s checkpoint" writer))
         (commit (symbol-function 'mevedel-session-durability-commit-publication-head))
         agent invocation record receiver logical session-directory
         entered restored restored-agent)
    (unwind-protect
        (progn
          (with-current-buffer root
            (mevedel-chat-prepare-transcript-buffer)
            (setq-local mevedel--session session mevedel--workspace workspace)
            (insert "Question\n\n" (propertize "Answer\n" 'gptel 'response))
            (should (mevedel-session-artifacts-save session root)))
          (setq session-directory (mevedel-session-save-path session)
                receiver root
                logical "segment-0001.chat.org")
          (unless (eq writer 'root)
            (let ((configuration
                   (test-mevedel-session-checkpoint-writers--configuration)))
              (setq agent (generate-new-buffer " *checkpoint-writer-agent*")
                    receiver agent
                    logical "agents/worker.chat.org"
                    invocation
                    (mevedel-agent-invocation--create
                     :agent (mevedel-agent-configuration-agent configuration)
                     :agent-id "worker" :path path :turn-count 1
                     :buffer agent :parent-session session :parent-data-buffer root
                     :frozen-configuration configuration
                     :transcript-relative-path logical :transcript-status 'completed)
                    record
                    (mevedel-agent-record--create
                     :id "worker" :path path :parent-path "/root" :role "default"
                     :configuration configuration :activity 'idle :invocation invocation
                     :conversation-location logical :conversation-buffer agent)))
            (setf (mevedel-session-agent-registry session) (list (cons path record))
                  (mevedel-session-agent-transcripts session)
                  `(("worker" :agent-type "default" :agent-path ,path
                     :description "Late delivery" :path ,logical :status completed
                     :created-at "created" :updated-at "updated" :parent-turn 0)))
            (with-current-buffer agent
              (mevedel-chat-prepare-transcript-buffer)
              (setq-local mevedel--session session
                          mevedel--workspace workspace
                          mevedel--agent-invocation invocation)
              (set-visited-file-name (file-name-concat session-directory logical) t t)
              (insert "Agent task\n\n" (propertize "Agent answer\n" 'gptel 'response)))
            (should (mevedel-agent-conversation-save invocation)))
          ;; Queue without starting the idle agent: this case delivers the
          ;; mail at a simulated WAIT boundary.
          (cl-letf (((symbol-function 'mevedel-agent-control--capacity-p) #'ignore)
                    ((symbol-function 'mevedel-agent-control--schedule-agent-wakes)
                     #'ignore))
            (mevedel-agent-control-send-message session path payload))
          (with-current-buffer receiver
            (goto-char (point-max))
            (insert (propertize "Partial tail before freeze\n" 'gptel 'response)))
          (cl-letf (((symbol-function 'mevedel-session-durability-commit-publication-head)
                     (lambda (&rest args)
                       (unless entered
                         (setq entered t)
                         (mevedel-tools--handle-message-inject
                          (gptel-make-fsm
                           :info (list :buffer receiver :backend nil
                                       :data (list :messages (vector))
                                       :mevedel-agent-invocation invocation)))
                         (should-not (if record
                                         (mevedel-agent-record-mailbox record)
                                       (mevedel-session-messages session)))
                         (with-current-buffer receiver
                           (should (mevedel-transcript-audit-buffer-spans 'mailbox-receipt))))
                       (apply commit args))))
            (should (test-mevedel-session-checkpoint-writers--save
                     writer session root invocation)))
          (should entered)
          ;; This distinguishes frozen-byte bookkeeping from merely retaining
          ;; an unread mailbox copy: the newer transcript still needs saving.
          (should (buffer-modified-p receiver))
          (should-not
           (string-search payload
                          (decode-coding-string
                           (mevedel-session-artifacts-read-artifact session logical t)
                           'utf-8-unix)))
          (should (test-mevedel-session-checkpoint-writers--save
                   writer session root invocation))
          (should-not (buffer-modified-p receiver))
          (let* ((text (decode-coding-string
                        (mevedel-session-artifacts-read-artifact session logical t)
                        'utf-8-unix))
                 (sidecar (mevedel-session-codec-read
                           (mevedel-session-artifacts-sidecar-path session-directory)))
                 (mailbox (if record
                              (plist-get (car (plist-get sidecar :agent-registry)) :mailbox)
                            (plist-get sidecar :messages))))
            (should (string-search payload text))
            (should-not mailbox))
          ;; Close the original receiver before root teardown so no later
          ;; buffer cleanup can accidentally rescue the missing text.
          (when (buffer-live-p agent)
            (with-current-buffer agent
              (setq kill-buffer-hook nil)
              (set-buffer-modified-p nil))
            (kill-buffer agent)
            (setq agent nil))
          (test-mevedel-session-persistence--release-and-kill root session)
          (setq root nil
                restored (mevedel-session-persistence-restore session-directory))
          (with-current-buffer restored
            (let* ((restored-session mevedel--session)
                   (restored-record
                    (cdr (assoc path (mevedel-session-agent-registry restored-session))))
                   (conversation
                    (if restored-record
                        (setq restored-agent
                              (mevedel-agent-persistence-ensure-conversation
                               restored-session restored-record restored))
                      restored)))
              (should (buffer-live-p conversation))
              (with-current-buffer conversation
                (should (string-search payload (buffer-string)))
                (should (= 1 (length
                              (mevedel-transcript-audit-buffer-spans 'mailbox-receipt)))))
              (should-not (if restored-record
                              (mevedel-agent-record-mailbox restored-record)
                            (mevedel-session-messages restored-session))))))
      (dolist (buffer (list agent restored-agent))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq kill-buffer-hook nil)
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (test-mevedel-session-persistence--release-and-kill root session)
      (test-mevedel-session-persistence--release-and-kill
       restored (and (buffer-live-p restored)
                     (buffer-local-value 'mevedel--session restored)))
      (mevedel-workspace-clear-registry)
      (delete-directory directory t))))

(mevedel-deftest mevedel-session-checkpoint-writers-late-mail (:quiet t)
  ,test
  (test)
  :doc "a root save keeps post-freeze mail dirty until the next committed save"
  (test-mevedel-session-checkpoint-writers--late-mail 'root)
  :doc "an ordinary agent save keeps post-freeze mail dirty and restores it once"
  (test-mevedel-session-checkpoint-writers--late-mail 'agent)
  :doc "a terminal agent save keeps post-freeze mail dirty and restores it once"
  (test-mevedel-session-checkpoint-writers--late-mail 'agent-terminal))

(mevedel-deftest mevedel-session-checkpoint-writers-terminal-result (:quiet t)
  ,test
  (test)
  :doc "a failed terminal commit never exposes its tentative RESULT to another turn"
  (let* ((directory (make-temp-file "mevedel-writer-terminal-result-" t))
         (workspace (test-mevedel-session-persistence--make-workspace directory))
         (session (mevedel-session-create "main" workspace))
         (root (generate-new-buffer " *checkpoint-terminal-root*"))
         (child (generate-new-buffer " *checkpoint-terminal-agent*"))
         (configuration (test-mevedel-session-checkpoint-writers--configuration))
         (invocation
          (mevedel-agent-invocation--create
           :agent (mevedel-agent-configuration-agent configuration)
           :agent-id "terminal-agent" :path "/root/worker"
           :buffer child :parent-session session :parent-data-buffer root
           :frozen-configuration configuration :transcript-status 'running
           :transcript-relative-path "agents/worker.chat.org"))
         (record
          (mevedel-agent-record--create
           :id "terminal-agent" :path "/root/worker" :parent-path "/root"
           :role "default" :configuration configuration :activity 'running
           :invocation invocation :conversation-location "agents/worker.chat.org"))
         (request-data (list :messages (vector)))
         entered old-text session-directory)
    (unwind-protect
        (progn
          (with-current-buffer root
            (mevedel-chat-prepare-transcript-buffer)
            (setq-local mevedel--session session mevedel--workspace workspace)
            (insert "Root prompt\n")
            (should (mevedel-session-artifacts-save session root)))
          (setq session-directory (mevedel-session-save-path session))
          (setf (mevedel-session-agent-registry session) (list (cons "/root/worker" record))
                (mevedel-session-agent-transcripts session)
                '(("terminal-agent" :agent-type "default" :agent-path "/root/worker"
                   :description "Test agent" :path "agents/worker.chat.org"
                   :status running :created-at "created" :updated-at "updated" :parent-turn 0))
                (mevedel-agent-invocation-runtime-settle-callback invocation)
                (lambda (inv response event &optional _token)
                  (mevedel-agent-control--settle session record inv response event)))
          (with-current-buffer child
            (mevedel-chat-prepare-transcript-buffer)
            (setq-local mevedel--session session mevedel--workspace workspace
                        mevedel--agent-invocation invocation)
            (set-visited-file-name
             (file-name-concat session-directory "agents/worker.chat.org") t t)
            (insert "Agent prompt\nPartial answer\n"))
          (should (mevedel-agent-conversation-save invocation))
          (setq old-text
                (mevedel-session-artifacts-read-artifact session "agents/worker.chat.org" t))
          (with-current-buffer child
            (goto-char (point-max))
            (insert "Terminal-only answer tail\n")
            (setq-local
             before-save-hook
             (list (lambda ()
                     (setq entered t)
                     (mevedel-tools--handle-message-inject
                      (gptel-make-fsm
                       :info (list :buffer root :backend nil :data request-data)))
                     (error "Injected terminal checkpoint failure")))))
          (let ((failure
                 (should-error
                  (mevedel-agent-runtime--settle invocation "Tentative result") :type 'error)))
            (should (equal "Injected terminal checkpoint failure"
                           (error-message-string failure))))
          (should entered)
          (should (= 0 (length (plist-get request-data :messages))))
          (should (eq 'running (mevedel-agent-record-activity record)))
          (should-not (mevedel-session-messages session))
          (should (equal old-text
                         (mevedel-session-artifacts-read-artifact
                          session "agents/worker.chat.org" t)))
          (with-current-buffer root
            (should-not (string-search "Tentative result" (buffer-string)))
            (should-not (mevedel-transcript-audit-buffer-spans 'mailbox-receipt))
            (should (mevedel-session-artifacts-save session root)))
          (should-not
           (string-search
            "Tentative result"
            (mevedel-session-artifacts-read-artifact session "segment-0001.chat.org" t)))
          (let* ((sidecar (mevedel-session-codec-read
                           (mevedel-session-artifacts-sidecar-path session-directory)))
                 (persisted-record (car (plist-get sidecar :agent-registry))))
            (should-not (plist-get sidecar :messages))
            (should (eq 'running (plist-get persisted-record :activity)))
            (should-not (plist-get persisted-record :settled-result))))
      (when (buffer-live-p child)
        (with-current-buffer child
          (setq before-save-hook nil kill-buffer-hook nil)
          (set-buffer-modified-p nil))
        (kill-buffer child))
      (test-mevedel-session-persistence--release-and-kill root session)
      (mevedel-workspace-clear-registry)
      (delete-directory directory t))))

(defun test-mevedel-session-checkpoint-writers--root-freeze (phase)
  "Verify root fields and bytes remain coherent when completion enters at PHASE."
  (let* ((directory (make-temp-file "mevedel-writer-root-freeze-" t))
         (workspace (test-mevedel-session-persistence--make-workspace directory))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *checkpoint-root-freeze*"))
         (seam (if (eq phase 'preparation)
                   'mevedel-session-artifacts--instruction-artifacts
                 'mevedel-session-durability-commit-publication-head))
         (original (symbol-function seam))
         entered restored session-directory)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (mevedel-chat-prepare-transcript-buffer)
            (setq-local mevedel--session session mevedel--workspace workspace)
            (insert "Question\n\n" (propertize "Answer\n" 'gptel 'response))
            (should (mevedel-session-artifacts-save session buffer))
            (setq session-directory (mevedel-session-save-path session))
            (goto-char (point-max))
            (insert (propertize "Before freeze\n" 'gptel 'response)))
          (cl-letf (((symbol-function seam)
                     (lambda (&rest args)
                       (unless entered
                         (setq entered t)
                         (setf (mevedel-session-turn-count session) 1)
                         (with-current-buffer buffer
                           (goto-char (point-max))
                           (insert (propertize "Completion during I/O\n" 'gptel 'response))))
                       (apply original args))))
            (should (mevedel-session-artifacts-save session buffer)))
          (should entered)
          ;; Do not save a second time: it could rescue mismatched bytes and
          ;; metadata, concealing which snapshot the first commit published.
          (test-mevedel-session-persistence--release-and-kill buffer session)
          (setq buffer nil
                restored (mevedel-session-persistence-restore session-directory))
          (with-current-buffer restored
            (should (string-search "Before freeze" (buffer-string)))
            (if (eq phase 'preparation)
                (progn
                  (should (string-search "Completion during I/O" (buffer-string)))
                  (should (= 1 (mevedel-session-turn-count mevedel--session))))
              (should-not (string-search "Completion during I/O" (buffer-string)))
              (should (= 0 (mevedel-session-turn-count mevedel--session))))))
      (test-mevedel-session-persistence--release-and-kill buffer session)
      (test-mevedel-session-persistence--release-and-kill
       restored (and (buffer-live-p restored)
                     (buffer-local-value 'mevedel--session restored)))
      (mevedel-workspace-clear-registry)
      (delete-directory directory t))))

(mevedel-deftest mevedel-session-checkpoint-writers-root-fields (:quiet t)
  ,test
  (test)
  :doc "completion during preparation freezes its new bytes and matching root turn"
  (test-mevedel-session-checkpoint-writers--root-freeze 'preparation)
  :doc "completion during head I/O cannot advance the already frozen root turn"
  (test-mevedel-session-checkpoint-writers--root-freeze 'head))

(provide 'test-mevedel-session-checkpoint-writers)
;;; test-mevedel-session-checkpoint-writers.el ends here
