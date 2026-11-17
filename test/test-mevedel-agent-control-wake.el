;;; test-mevedel-agent-control-wake.el --- Mail wake request tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise settlement mail through retained dispatch and the native request.

;;; Code:

(require 'gptel-openai-oauth)
(require 'mevedel-agent-control)
(require 'mevedel-agent-conversation)
(require 'mevedel-agent-exec)
(require 'mevedel-agent-runtime)
(require 'mevedel-session-artifacts)
(require 'mevedel-tools)
(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(mevedel-deftest mevedel-agent-control--offer-agent-wakes/request
  (:quiet t)
  ,test
  (test)
  :doc "mail sent during settlement reaches the next Codex request exactly once"
  (let* ((gptel--known-backends nil)
         (backend (gptel-make-openai-oauth "Wake Codex"))
         (gptel--openai-oauth-token-file (make-temp-file "wake-token-"))
         (root (generate-new-buffer " *wake-root*"))
         (child (generate-new-buffer " *wake-child*"))
         (session (mevedel-session-create
                   "wake" (mevedel-workspace--create
                           :type 'file :root default-directory :name "wake")))
         (agent (mevedel-agent--create :name "default" :frozen-p t))
         (old (mevedel-agent-invocation--create
               :agent agent :path "/root/work" :agent-id "wake-agent"
               :parent-session session :buffer child :transcript-status 'completed))
         (record (mevedel-agent-record--create
                  :id "wake-agent" :path "/root/work" :parent-path "/root"
                  :activity 'running :invocation old :conversation-buffer child
                  :conversation-location "agents/wake.chat.org"))
         sent)
    (unwind-protect
        (progn
          (with-temp-file gptel--openai-oauth-token-file
            (prin1 (list :access_token "current" :expires_at (+ (float-time) 3600))
                   (current-buffer)))
          (setf (mevedel-session-root-buffer session) root
                (mevedel-session-agent-registry session) (list (cons "/root/work" record)))
          (with-current-buffer root
            (setq-local mevedel--session session)
            (let ((gptel-backend backend) (gptel-model 'gpt-5.5)
                  (gptel-use-tools nil) (gptel-stream nil))
              (setf (mevedel-agent-record-configuration record)
                    (mevedel-agent-exec-freeze-configuration "default" old))))
          (with-current-buffer child
            (org-mode)
            (setq-local mevedel--session session mevedel--agent-invocation old)
            (insert "* Agent Task: work\n\nFinished.\n"))
          (cl-letf (((symbol-function 'mevedel-session-artifacts-assert-mutation-authority)
                     #'ignore)
                    ((symbol-function 'mevedel-agent-conversation-save)
                     (lambda (&rest _) t))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (fsm)
                       (push (copy-tree (plist-get (gptel-fsm-info fsm) :data)) sent))))
            (let* ((mevedel-agent-control-settling (list old))
                   (transaction (mevedel-agent-control--settle session record old "Finished")))
              (with-current-buffer root
                (mevedel-agent-control-send-message session "/root/work" "settlement-notch"))
              (funcall (cdr transaction)))
            (mevedel-agent-control--offer-agent-wakes session)
            (ert-info (mevedel-test--captured)
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
              (should (string-search mevedel-agent-control--wake-prompt
                                     (format "%S" (aref input (- (length input) 2))))))
            (should-not (mevedel-agent-record-mailbox record))
            (mevedel-tools--handle-message-inject
             (mevedel-agent-invocation-runtime-fsm (mevedel-agent-record-invocation record)))
            (with-current-buffer child
              (should (= 1 (length (mevedel-transcript-audit-buffer-spans 'mailbox-receipt)))))))
      (mevedel-transport-cancel-pending (list 'agent-wake session))
      (delete-file gptel--openai-oauth-token-file)
      (kill-buffer child)
      (kill-buffer root))))

(provide 'test-mevedel-agent-control-wake)
;;; test-mevedel-agent-control-wake.el ends here
