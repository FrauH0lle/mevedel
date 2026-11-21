;;; test-niwa-session-checkpoint-writers.el --- Frozen writer regressions -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise real root and retained-agent writers with mailbox delivery reentering
;; after transcript freeze.  The older save must not mark the newer text clean,
;; and the next save must durably include that text and its delivery receipt.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-agent-runtime)

(defun test-niwa-session-checkpoint-writers--configuration ()
  "Return a frozen agent configuration independent of the tool registry."
  (niwa-agent-configuration--create
   :agent (niwa-agent--create :name "default" :description "Checkpoint writer"
                                 :frozen-p t)
   :request-locals
   (mapcar (lambda (symbol)
             (cons symbol
                   (pcase symbol
                     ('gptel-backend (test-niwa-session-persistence--agent-backend))
                     ('gptel-model 'test-model))))
           niwa-agent-request-local-symbols)))

(defun test-niwa-session-checkpoint-writers--save (writer session root invocation)
  "Run WRITER for SESSION, ROOT, and optional agent INVOCATION."
  (pcase writer
    ('root (niwa-session-artifacts-save session root))
    ('agent (niwa-agent-conversation-save invocation))
    ('agent-terminal
     (niwa-session-artifacts-publish-agent-terminal-state invocation))))

(defun test-niwa-session-checkpoint-writers--late-mail (writer)
  "Verify that WRITER preserves a mailbox delivery after transcript freeze."
  (let* ((directory (make-temp-file "niwa-writer-late-mail-" t))
         (workspace (test-niwa-session-persistence--make-workspace directory))
         (session (niwa-session-create "main" workspace))
         (root (generate-new-buffer " *checkpoint-writer-root*"))
         (path (if (eq writer 'root) "/root" "/root/worker"))
         (payload (format "Late mail survives %s checkpoint" writer))
         (commit (symbol-function 'niwa-session-durability-commit-publication-head))
         agent invocation record receiver logical session-directory
         entered restored restored-agent)
    (unwind-protect
        (progn
          (with-current-buffer root
            (niwa-chat-prepare-transcript-buffer)
            (setq-local niwa--session session niwa--workspace workspace)
            (insert "Question\n\n" (propertize "Answer\n" 'gptel 'response))
            (should (niwa-session-artifacts-save session root)))
          (setq session-directory (niwa-session-save-path session)
                receiver root
                logical "segment-0001.chat.org")
          (unless (eq writer 'root)
            (let ((configuration
                   (test-niwa-session-checkpoint-writers--configuration)))
              (setq agent (generate-new-buffer " *checkpoint-writer-agent*")
                    receiver agent
                    logical "agents/worker.chat.org"
                    invocation
                    (niwa-agent-invocation--create
                     :agent (niwa-agent-configuration-agent configuration)
                     :agent-id "worker" :path path :turn-count 1
                     :buffer agent :parent-session session :parent-data-buffer root
                     :frozen-configuration configuration
                     :transcript-relative-path logical :transcript-status 'completed)
                    record
                    (niwa-agent-record--create
                     :id "worker" :path path :parent-path "/root" :role "default"
                     :configuration configuration :activity 'idle :invocation invocation
                     :conversation-location logical :conversation-buffer agent)))
            (setf (niwa-session-agent-registry session) (list (cons path record))
                  (niwa-session-agent-transcripts session)
                  `(("worker" :agent-type "default" :agent-path ,path
                     :description "Late delivery" :path ,logical :status completed
                     :created-at "created" :updated-at "updated" :parent-turn 0)))
            (with-current-buffer agent
              (niwa-chat-prepare-transcript-buffer)
              (setq-local niwa--session session
                          niwa--workspace workspace
                          niwa--agent-invocation invocation)
              (set-visited-file-name (file-name-concat session-directory logical) t t)
              (insert "Agent task\n\n" (propertize "Agent answer\n" 'gptel 'response)))
            (should (niwa-agent-conversation-save invocation)))
          ;; Queue without starting the idle agent: this case delivers the
          ;; mail at a simulated WAIT boundary.
          (cl-letf (((symbol-function 'niwa-agent-control--capacity-p) #'ignore)
                    ((symbol-function 'niwa-agent-control--schedule-agent-wakes)
                     #'ignore))
            (niwa-agent-control-send-message session path payload))
          (with-current-buffer receiver
            (goto-char (point-max))
            (insert (propertize "Partial tail before freeze\n" 'gptel 'response)))
          (cl-letf (((symbol-function 'niwa-session-durability-commit-publication-head)
                     (lambda (&rest args)
                       (unless entered
                         (setq entered t)
                         (niwa-tools--handle-message-inject
                          (gptel-make-fsm
                           :info (list :buffer receiver :backend nil
                                       :data (list :messages (vector))
                                       :niwa-agent-invocation invocation)))
                         (should-not (if record
                                         (niwa-agent-record-mailbox record)
                                       (niwa-session-messages session)))
                         (with-current-buffer receiver
                           (should (niwa-transcript-audit-buffer-spans 'mailbox-receipt))))
                       (apply commit args))))
            (should (test-niwa-session-checkpoint-writers--save
                     writer session root invocation)))
          (should entered)
          ;; This distinguishes frozen-byte bookkeeping from merely retaining
          ;; an unread mailbox copy: the newer transcript still needs saving.
          (should (buffer-modified-p receiver))
          (should-not
           (string-search payload
                          (decode-coding-string
                           (niwa-session-artifacts-read-artifact session logical t)
                           'utf-8-unix)))
          (should (test-niwa-session-checkpoint-writers--save
                   writer session root invocation))
          (should-not (buffer-modified-p receiver))
          (let* ((text (decode-coding-string
                        (niwa-session-artifacts-read-artifact session logical t)
                        'utf-8-unix))
                 (sidecar (niwa-session-codec-read
                           (niwa-session-artifacts-sidecar-path session-directory)))
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
          (test-niwa-session-persistence--release-and-kill root session)
          (setq root nil
                restored (niwa-session-persistence-restore session-directory))
          (with-current-buffer restored
            (let* ((restored-session niwa--session)
                   (restored-record
                    (cdr (assoc path (niwa-session-agent-registry restored-session))))
                   (conversation
                    (if restored-record
                        (setq restored-agent
                              (niwa-agent-persistence-ensure-conversation
                               restored-session restored-record restored))
                      restored)))
              (should (buffer-live-p conversation))
              (with-current-buffer conversation
                (should (string-search payload (buffer-string)))
                (should (= 1 (length
                              (niwa-transcript-audit-buffer-spans 'mailbox-receipt)))))
              (should-not (if restored-record
                              (niwa-agent-record-mailbox restored-record)
                            (niwa-session-messages restored-session))))))
      (dolist (buffer (list agent restored-agent))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq kill-buffer-hook nil)
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (test-niwa-session-persistence--release-and-kill root session)
      (test-niwa-session-persistence--release-and-kill
       restored (and (buffer-live-p restored)
                     (buffer-local-value 'niwa--session restored)))
      (niwa-workspace-clear-registry)
      (delete-directory directory t))))

(niwa-deftest niwa-session-checkpoint-writers-late-mail (:quiet t)
  ,test
  (test)
  :doc "a root save keeps post-freeze mail dirty until the next committed save"
  (test-niwa-session-checkpoint-writers--late-mail 'root)
  :doc "an ordinary agent save keeps post-freeze mail dirty and restores it once"
  (test-niwa-session-checkpoint-writers--late-mail 'agent)
  :doc "a terminal agent save keeps post-freeze mail dirty and restores it once"
  (test-niwa-session-checkpoint-writers--late-mail 'agent-terminal))

(niwa-deftest niwa-session-checkpoint-writers-terminal-result (:quiet t)
  ,test
  (test)
  :doc "a failed terminal commit never exposes its tentative RESULT to another turn"
  (let* ((directory (make-temp-file "niwa-writer-terminal-result-" t))
         (workspace (test-niwa-session-persistence--make-workspace directory))
         (session (niwa-session-create "main" workspace))
         (root (generate-new-buffer " *checkpoint-terminal-root*"))
         (child (generate-new-buffer " *checkpoint-terminal-agent*"))
         (configuration (test-niwa-session-checkpoint-writers--configuration))
         (invocation
          (niwa-agent-invocation--create
           :agent (niwa-agent-configuration-agent configuration)
           :agent-id "terminal-agent" :path "/root/worker"
           :buffer child :parent-session session :parent-data-buffer root
           :frozen-configuration configuration :transcript-status 'running
           :transcript-relative-path "agents/worker.chat.org"))
         (record
          (niwa-agent-record--create
           :id "terminal-agent" :path "/root/worker" :parent-path "/root"
           :role "default" :configuration configuration :activity 'running
           :invocation invocation :conversation-location "agents/worker.chat.org"))
         (request-data (list :messages (vector)))
         entered old-text session-directory)
    (unwind-protect
        (progn
          (with-current-buffer root
            (niwa-chat-prepare-transcript-buffer)
            (setq-local niwa--session session niwa--workspace workspace)
            (insert "Root prompt\n")
            (should (niwa-session-artifacts-save session root)))
          (setq session-directory (niwa-session-save-path session))
          (setf (niwa-session-agent-registry session) (list (cons "/root/worker" record))
                (niwa-session-agent-transcripts session)
                '(("terminal-agent" :agent-type "default" :agent-path "/root/worker"
                   :description "Test agent" :path "agents/worker.chat.org"
                   :status running :created-at "created" :updated-at "updated" :parent-turn 0))
                (niwa-agent-invocation-runtime-settle-callback invocation)
                (lambda (inv response event &optional _token)
                  (niwa-agent-control--settle session record inv response event)))
          (with-current-buffer child
            (niwa-chat-prepare-transcript-buffer)
            (setq-local niwa--session session niwa--workspace workspace
                        niwa--agent-invocation invocation)
            (set-visited-file-name
             (file-name-concat session-directory "agents/worker.chat.org") t t)
            (insert "Agent prompt\nPartial answer\n"))
          (should (niwa-agent-conversation-save invocation))
          (setq old-text
                (niwa-session-artifacts-read-artifact session "agents/worker.chat.org" t))
          (with-current-buffer child
            (goto-char (point-max))
            (insert "Terminal-only answer tail\n")
            (setq-local
             before-save-hook
             (list (lambda ()
                     (setq entered t)
                     (niwa-tools--handle-message-inject
                      (gptel-make-fsm
                       :info (list :buffer root :backend nil :data request-data)))
                     (error "Injected terminal checkpoint failure")))))
          (let ((failure
                 (should-error
                  (niwa-agent-runtime--settle invocation "Tentative result") :type 'error)))
            (should (equal "Injected terminal checkpoint failure"
                           (error-message-string failure))))
          (should entered)
          (should (= 0 (length (plist-get request-data :messages))))
          (should (eq 'running (niwa-agent-record-activity record)))
          (should-not (niwa-session-messages session))
          (should (equal old-text
                         (niwa-session-artifacts-read-artifact
                          session "agents/worker.chat.org" t)))
          (with-current-buffer root
            (should-not (string-search "Tentative result" (buffer-string)))
            (should-not (niwa-transcript-audit-buffer-spans 'mailbox-receipt))
            (should (niwa-session-artifacts-save session root)))
          (should-not
           (string-search
            "Tentative result"
            (niwa-session-artifacts-read-artifact session "segment-0001.chat.org" t)))
          (let* ((sidecar (niwa-session-codec-read
                           (niwa-session-artifacts-sidecar-path session-directory)))
                 (persisted-record (car (plist-get sidecar :agent-registry))))
            (should-not (plist-get sidecar :messages))
            (should (eq 'running (plist-get persisted-record :activity)))
            (should-not (plist-get persisted-record :settled-result))))
      (when (buffer-live-p child)
        (with-current-buffer child
          (setq before-save-hook nil kill-buffer-hook nil)
          (set-buffer-modified-p nil))
        (kill-buffer child))
      (test-niwa-session-persistence--release-and-kill root session)
      (niwa-workspace-clear-registry)
      (delete-directory directory t))))

(defun test-niwa-session-checkpoint-writers--root-freeze (phase)
  "Verify root fields and bytes remain coherent when completion enters at PHASE."
  (let* ((directory (make-temp-file "niwa-writer-root-freeze-" t))
         (workspace (test-niwa-session-persistence--make-workspace directory))
         (session (niwa-session-create "main" workspace))
         (buffer (generate-new-buffer " *checkpoint-root-freeze*"))
         (seam (if (eq phase 'preparation)
                   'niwa-session-artifacts-ensure-files
                 'niwa-session-durability-commit-publication-head))
         (original (symbol-function seam))
         entered restored session-directory)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (niwa-chat-prepare-transcript-buffer)
            (setq-local niwa--session session niwa--workspace workspace)
            (insert "Question\n\n" (propertize "Answer\n" 'gptel 'response))
            (should (niwa-session-artifacts-save session buffer))
            (setq session-directory (niwa-session-save-path session))
            (goto-char (point-max))
            (insert (propertize "Before freeze\n" 'gptel 'response)))
          (cl-letf (((symbol-function seam)
                     (lambda (&rest args)
                       (unless entered
                         (setq entered t)
                         (setf (niwa-session-turn-count session) 1)
                         (with-current-buffer buffer
                           (goto-char (point-max))
                           (insert (propertize "Completion during I/O\n" 'gptel 'response))))
                       (apply original args))))
            (should (niwa-session-artifacts-save session buffer)))
          (should entered)
          ;; Do not save a second time: it could rescue mismatched bytes and
          ;; metadata, concealing which snapshot the first commit published.
          (test-niwa-session-persistence--release-and-kill buffer session)
          (setq buffer nil
                restored (niwa-session-persistence-restore session-directory))
          (with-current-buffer restored
            (should (string-search "Before freeze" (buffer-string)))
            (if (eq phase 'preparation)
                (progn
                  (should (string-search "Completion during I/O" (buffer-string)))
                  (should (= 1 (niwa-session-turn-count niwa--session))))
              (should-not (string-search "Completion during I/O" (buffer-string)))
              (should (= 0 (niwa-session-turn-count niwa--session))))))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (test-niwa-session-persistence--release-and-kill
       restored (and (buffer-live-p restored)
                     (buffer-local-value 'niwa--session restored)))
      (niwa-workspace-clear-registry)
      (delete-directory directory t))))

(niwa-deftest niwa-session-checkpoint-writers-root-fields (:quiet t)
  ,test
  (test)
  :doc "completion during preparation freezes its new bytes and matching root turn"
  (test-niwa-session-checkpoint-writers--root-freeze 'preparation)
  :doc "completion during head I/O cannot advance the already frozen root turn"
  (test-niwa-session-checkpoint-writers--root-freeze 'head))

(provide 'test-niwa-session-checkpoint-writers)
;;; test-niwa-session-checkpoint-writers.el ends here
