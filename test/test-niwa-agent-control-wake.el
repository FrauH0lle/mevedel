;;; test-niwa-agent-control-wake.el --- Mail wake request tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise settlement mail through retained dispatch and the native request.

;;; Code:

(require 'gptel-openai-oauth)
(require 'niwa-agent-control)
(require 'niwa-agent-conversation)
(require 'niwa-agent-exec)
(require 'niwa-agent-runtime)
(require 'niwa-session-artifacts)
(require 'niwa-tools)
(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(niwa-deftest niwa-agent-control--offer-agent-wakes/request
  (:quiet t)
  ,test
  (test)
  :doc "mail sent during settlement reaches the next Codex request exactly once"
  (let* ((gptel--known-backends nil)
         (backend (gptel-make-openai-oauth "Wake Codex"))
         (gptel--openai-oauth-token-file (make-temp-file "wake-token-"))
         (root (generate-new-buffer " *wake-root*"))
         (child (generate-new-buffer " *wake-child*"))
         (session (niwa-session-create
                   "wake" (niwa-workspace--create
                           :type 'file :root default-directory :name "wake")))
         (agent (niwa-agent--create :name "default" :frozen-p t))
         (old (niwa-agent-invocation--create
               :agent agent :path "/root/work" :agent-id "wake-agent"
               :parent-session session :buffer child :transcript-status 'completed))
         (record (niwa-agent-record--create
                  :id "wake-agent" :path "/root/work" :parent-path "/root"
                  :activity 'running :invocation old :conversation-buffer child
                  :conversation-location "agents/wake.chat.org"))
         sent)
    (unwind-protect
        (progn
          (with-temp-file gptel--openai-oauth-token-file
            (prin1 (list :access_token "current" :expires_at (+ (float-time) 3600))
                   (current-buffer)))
          (setf (niwa-session-root-buffer session) root
                (niwa-session-agent-registry session) (list (cons "/root/work" record)))
          (with-current-buffer root
            (setq-local niwa--session session)
            (let ((gptel-backend backend) (gptel-model 'gpt-5.5)
                  (gptel-use-tools nil) (gptel-stream nil))
              (setf (niwa-agent-record-configuration record)
                    (niwa-agent-exec-freeze-configuration "default" old))))
          (with-current-buffer child
            (org-mode)
            (setq-local niwa--session session niwa--agent-invocation old)
            (insert "* Agent Task: work\n\nFinished.\n"))
          (cl-letf (((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
                     #'ignore)
                    ((symbol-function 'niwa-agent-conversation-save)
                     (lambda (&rest _) t))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (fsm)
                       (push (copy-tree (plist-get (gptel-fsm-info fsm) :data)) sent))))
            (let* ((niwa-agent-control-settling (list old))
                   (transaction (niwa-agent-control--settle session record old "Finished")))
              (with-current-buffer root
                (niwa-agent-control-send-message session "/root/work" "settlement-notch"))
              (funcall (cdr transaction)))
            (niwa-agent-control--offer-agent-wakes session)
            (ert-info (niwa-test--captured)
              (should (= 1 (length sent))))
            (let* ((input (plist-get (car sent) :input))
                   (mail (cl-remove-if-not
                          (lambda (item)
                            (string-search "settlement-notch" (format "%S" item)))
                          (append input nil))))
              (should (= 1 (length mail)))
              (should (string-search "<agent-message type=\"MAIL\""
                                     (plist-get (car mail) :content)))
              (should (equal (car mail) (aref input (1- (length input)))))
              (should (string-search niwa-agent-control--wake-prompt
                                     (format "%S" (aref input (- (length input) 2))))))
            (should-not (niwa-agent-record-mailbox record))
            (niwa-tools--handle-message-inject
             (niwa-agent-invocation-runtime-fsm (niwa-agent-record-invocation record)))
            (with-current-buffer child
              (should (= 1 (length (niwa-transcript-audit-buffer-spans 'mailbox-receipt)))))))
      (niwa-transport-cancel-pending (list 'agent-wake session))
      (delete-file gptel--openai-oauth-token-file)
      (kill-buffer child)
      (kill-buffer root))))

(provide 'test-niwa-agent-control-wake)
;;; test-niwa-agent-control-wake.el ends here
