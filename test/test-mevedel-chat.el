;;; test-mevedel-chat.el --- Tests for chat buffer management -*- lexical-binding: t -*-

;;; Commentary:

;; Focused coverage for chat-buffer setup helpers.

;;; Code:

(require 'mevedel-chat)
(require 'mevedel-permission-queue)
(require 'mevedel-tool-plan)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Org element cache handling

(mevedel-deftest mevedel--chat-buffer-disable-org-element-cache ()
  ,test
  (test)

  :doc "disables Org's element cache buffer-locally"
  (with-temp-buffer
    (org-mode)
    (setq-local org-element-use-cache t)
    (setq-local org-element-cache-persistent t)
    (mevedel--chat-buffer-disable-org-element-cache)
    (should-not org-element-use-cache)
    (should-not org-element-cache-persistent)))


;;
;;; Abort

(mevedel-deftest mevedel-abort
  (:doc "aborts active chat request state")
  ,test
  (test)

  :doc "flushes permission and plan queues"
  (with-temp-buffer
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "/tmp/mevedel-chat-abort/"
                       :root "/tmp/mevedel-chat-abort/"
                       :name "abort"))
           (session (mevedel-session-create "main" workspace))
           (outcomes nil))
      (setq-local mevedel--session session)
      (mevedel-request-begin session)
      (setf (mevedel-session-permission-queue session)
            (list (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :callback
                        (lambda (outcome)
                          (push (cons 'permission outcome) outcomes)))))
      (setf (mevedel-session-plan-queue session)
            (list (list :body "# Plan"
                        :chat-buffer (current-buffer)
                        :session session
                        :callback
                        (lambda (outcome)
                          (push (cons 'plan outcome) outcomes)))))
      (mevedel-abort (current-buffer))
      (should (null (mevedel-session-permission-queue session)))
      (should (null (mevedel-session-plan-queue session)))
      (should (null mevedel--current-request))
      (should (equal '((plan . aborted) (permission . aborted))
                     outcomes)))))


(provide 'test-mevedel-chat)
;;; test-mevedel-chat.el ends here
