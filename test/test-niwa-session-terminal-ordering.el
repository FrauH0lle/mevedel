;;; test-niwa-session-terminal-ordering.el --- Terminal checkpoint ordering -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise terminal runtime rollback and restore across nested save hooks.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "niwa-session-test-support"))
(require 'niwa-agent-runtime)
(require 'niwa-agent-control)
(require 'niwa-agent-conversation)

(niwa-deftest niwa-agent-runtime--settle/checkpoint-ordering
  (:doc "a nested hook cannot publish tentative terminal state before its transcript")
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "niwa-terminal-order-" t)))
         (workspace (test-niwa-session-persistence--make-workspace tempdir))
         (session (niwa-session-create "main" workspace))
         (root (generate-new-buffer " *terminal-order-root*"))
         (child (generate-new-buffer " *terminal-order-agent*"))
         (agent (niwa-agent--create :name "default" :description "Test agent"))
         (configuration
          (niwa-agent-configuration--create
           :agent agent
           :request-locals
           (mapcar (lambda (symbol)
                     (cons symbol (pcase symbol
                                    ('gptel-backend gptel-backend)
                                    ('gptel-model gptel-model))))
                   niwa-agent-request-local-symbols)))
         (invocation
          (niwa-agent-invocation--create
           :agent agent :agent-id "terminal-agent" :path "/root/worker"
           :buffer child :parent-session session :parent-data-buffer root
           :transcript-status 'running
           :transcript-relative-path "agents/worker.chat.org"))
         (record
          (niwa-agent-record--create
           :id "terminal-agent" :path "/root/worker" :parent-path "/root"
           :role "default" :configuration configuration :activity 'running
           :invocation invocation :conversation-location "agents/worker.chat.org"))
         restored directory old-head old-text entered)
    (unwind-protect
        (progn
          (with-current-buffer root
            (niwa-chat-prepare-transcript-buffer)
            (setq-local niwa--session session niwa--workspace workspace)
            (insert "Root prompt\n")
            (niwa-session-artifacts-save session root))
          (setq directory (niwa-session-save-path session))
          (setf (niwa-session-agent-registry session)
                (list (cons "/root/worker" record))
                (niwa-session-agent-transcripts session)
                '(("terminal-agent" :agent-type "default" :agent-path "/root/worker"
                   :description "Test agent" :path "agents/worker.chat.org"
                   :status running :created-at "created" :updated-at "updated"
                   :parent-turn 0))
                (niwa-agent-invocation-runtime-settle-callback invocation)
                (lambda (inv response event &optional token)
                  (niwa-agent-control--settle session record inv response event token)))
          (with-current-buffer child
            (niwa-chat-prepare-transcript-buffer)
            (setq-local niwa--session session niwa--workspace workspace
                        niwa--agent-invocation invocation)
            (set-visited-file-name
             (file-name-concat directory "agents/worker.chat.org") t t)
            (insert "Agent prompt\nPartial answer\n"))
          (should (niwa-agent-conversation-save invocation))
          (should (niwa-session-artifacts-save session root))
          (setq old-head (copy-tree (plist-get (niwa-session-publication session) :head))
                old-text (niwa-session-artifacts-read-artifact
                          session "agents/worker.chat.org" t))
          (with-current-buffer child
            (goto-char (point-max))
            (insert "Terminal-only answer tail\n")
            (setq-local before-save-hook
                        (list (lambda ()
                                (setq entered t)
                                (niwa-agent-control-send-message
                                 session "/root" "Nested terminal mail B")
                                (error "Injected failure after nested terminal commit")))))
          ;; Real runtime finalization stages the registry and RESULT before
          ;; this hook. Its rollback must not leave that state committed.
          (let ((failure (should-error
                          (niwa-agent-runtime--settle invocation "Finished result")
                          :type 'user-error)))
            (should (equal "Session checkpoint is being prepared"
                           (error-message-string failure))))
          (should entered)
          (should-not (niwa-agent-invocation-runtime-settled-p invocation))
          (should (eq 'running (niwa-agent-record-activity record)))
          (should (eq invocation (niwa-agent-record-invocation record)))
          (should-not (niwa-agent-record-settled-result record))
          (should-not (niwa-session-messages session))
          (should (equal old-head (plist-get (niwa-session-publication session) :head)))
          (should (equal old-text (niwa-session-artifacts-read-artifact
                                  session "agents/worker.chat.org" t)))
          ;; Crash-style teardown: neither live buffer gets a final save.
          (with-current-buffer child
            (setq-local before-save-hook nil kill-buffer-hook nil)
            (set-buffer-modified-p nil))
          (kill-buffer child)
          (setq child nil)
          (test-niwa-session-persistence--release-and-kill root session)
          (setq root nil restored (niwa-session-persistence-restore directory))
          (with-current-buffer restored
            (let ((restored-record
                   (cdr (assoc "/root/worker"
                               (niwa-session-agent-registry niwa--session)))))
              (should restored-record)
              (should-not (equal "Finished result"
                                 (niwa-agent-record-settled-result restored-record)))
              (should-not (seq-some
                           (lambda (mail)
                             (member (plist-get mail :payload)
                                     '("Finished result" "Nested terminal mail B")))
                           (niwa-session-messages niwa--session)))
              (should (equal old-text (niwa-session-artifacts-read-artifact
                                      niwa--session "agents/worker.chat.org" t))))))
      (when (buffer-live-p child)
        (with-current-buffer child
          (setq-local before-save-hook nil kill-buffer-hook nil)
          (set-buffer-modified-p nil))
        (kill-buffer child))
      (test-niwa-session-persistence--release-and-kill root session)
      (test-niwa-session-persistence--release-and-kill
       restored (and restored (buffer-local-value 'niwa--session restored)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-session-terminal-ordering)
;;; test-niwa-session-terminal-ordering.el ends here
