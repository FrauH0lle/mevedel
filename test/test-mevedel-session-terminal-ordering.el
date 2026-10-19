;;; test-mevedel-session-terminal-ordering.el --- Terminal checkpoint ordering -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise terminal runtime rollback and restore across nested save hooks.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "mevedel-session-test-support"))
(require 'mevedel-agent-runtime)
(require 'mevedel-agent-control)
(require 'mevedel-agent-conversation)

(mevedel-deftest mevedel-agent-runtime--settle/checkpoint-ordering
  (:doc "a nested hook cannot publish tentative terminal state before its transcript")
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "mevedel-terminal-order-" t)))
         (workspace (test-mevedel-session-persistence--make-workspace tempdir))
         (session (mevedel-session-create "main" workspace))
         (root (generate-new-buffer " *terminal-order-root*"))
         (child (generate-new-buffer " *terminal-order-agent*"))
         (agent (mevedel-agent--create :name "default" :description "Test agent"))
         (configuration
          (mevedel-agent-configuration--create
           :agent agent
           :request-locals
           (mapcar (lambda (symbol)
                     (cons symbol (pcase symbol
                                    ('gptel-backend gptel-backend)
                                    ('gptel-model gptel-model))))
                   mevedel-agent-request-local-symbols)))
         (invocation
          (mevedel-agent-invocation--create
           :agent agent :agent-id "terminal-agent" :path "/root/worker"
           :buffer child :parent-session session :parent-data-buffer root
           :transcript-status 'running
           :transcript-relative-path "agents/worker.chat.org"))
         (record
          (mevedel-agent-record--create
           :id "terminal-agent" :path "/root/worker" :parent-path "/root"
           :role "default" :configuration configuration :activity 'running
           :invocation invocation :conversation-location "agents/worker.chat.org"))
         restored directory old-head old-text entered)
    (unwind-protect
        (progn
          (with-current-buffer root
            (mevedel-chat-prepare-transcript-buffer)
            (setq-local mevedel--session session mevedel--workspace workspace)
            (insert "Root prompt\n")
            (mevedel-session-artifacts-save session root))
          (setq directory (mevedel-session-save-path session))
          (setf (mevedel-session-agent-registry session)
                (list (cons "/root/worker" record))
                (mevedel-session-agent-transcripts session)
                '(("terminal-agent" :agent-type "default" :agent-path "/root/worker"
                   :description "Test agent" :path "agents/worker.chat.org"
                   :status running :created-at "created" :updated-at "updated"
                   :parent-turn 0))
                (mevedel-agent-invocation-runtime-settle-callback invocation)
                (lambda (inv response event)
                  (mevedel-agent-control--settle session record inv response event)))
          (with-current-buffer child
            (mevedel-chat-prepare-transcript-buffer)
            (setq-local mevedel--session session mevedel--workspace workspace
                        mevedel--agent-invocation invocation)
            (set-visited-file-name
             (file-name-concat directory "agents/worker.chat.org") t t)
            (insert "Agent prompt\nPartial answer\n"))
          (should (mevedel-agent-conversation-save invocation))
          (should (mevedel-session-artifacts-save session root))
          (setq old-head (copy-tree (plist-get (mevedel-session-publication session) :head))
                old-text (mevedel-session-artifacts-read-artifact
                          session "agents/worker.chat.org" t))
          (with-current-buffer child
            (goto-char (point-max))
            (insert "Terminal-only answer tail\n")
            (setq-local before-save-hook
                        (list (lambda ()
                                (setq entered t)
                                (mevedel-agent-control-send-message
                                 session "/root" "Nested terminal mail B")
                                (error "Injected failure after nested terminal commit")))))
          ;; Real runtime finalization stages the registry and RESULT before
          ;; this hook. Its rollback must not leave that state committed.
          (let ((failure (should-error
                          (mevedel-agent-runtime--settle invocation "Finished result")
                          :type 'user-error)))
            (should (equal "Session checkpoint is being prepared"
                           (error-message-string failure))))
          (should entered)
          (should-not (mevedel-agent-invocation-runtime-settled-p invocation))
          (should (eq 'running (mevedel-agent-record-activity record)))
          (should (eq invocation (mevedel-agent-record-invocation record)))
          (should-not (mevedel-agent-record-settled-result record))
          (should-not (mevedel-session-messages session))
          (should (equal old-head (plist-get (mevedel-session-publication session) :head)))
          (should (equal old-text (mevedel-session-artifacts-read-artifact
                                  session "agents/worker.chat.org" t)))
          ;; Crash-style teardown: neither live buffer gets a final save.
          (with-current-buffer child
            (setq-local before-save-hook nil kill-buffer-hook nil)
            (set-buffer-modified-p nil))
          (kill-buffer child)
          (setq child nil)
          (test-mevedel-session-persistence--release-and-kill root session)
          (setq root nil restored (mevedel-session-persistence-restore directory))
          (with-current-buffer restored
            (let ((restored-record
                   (cdr (assoc "/root/worker"
                               (mevedel-session-agent-registry mevedel--session)))))
              (should restored-record)
              (should-not (equal "Finished result"
                                 (mevedel-agent-record-settled-result restored-record)))
              (should-not (seq-some
                           (lambda (mail)
                             (member (plist-get mail :payload)
                                     '("Finished result" "Nested terminal mail B")))
                           (mevedel-session-messages mevedel--session)))
              (should (equal old-text (mevedel-session-artifacts-read-artifact
                                      mevedel--session "agents/worker.chat.org" t))))))
      (when (buffer-live-p child)
        (with-current-buffer child
          (setq-local before-save-hook nil kill-buffer-hook nil)
          (set-buffer-modified-p nil))
        (kill-buffer child))
      (test-mevedel-session-persistence--release-and-kill root session)
      (test-mevedel-session-persistence--release-and-kill
       restored (and restored (buffer-local-value 'mevedel--session restored)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-session-terminal-ordering)
;;; test-mevedel-session-terminal-ordering.el ends here
