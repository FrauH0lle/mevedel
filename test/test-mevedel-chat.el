;;; test-mevedel-chat.el --- Tests for chat buffer management -*- lexical-binding: t -*-

;;; Commentary:

;; Focused coverage for chat-buffer setup helpers.

;;; Code:

(require 'mevedel-chat)
(require 'mevedel)
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

(mevedel-deftest mevedel-load-order ()
  ,test
  (test)

  :doc "`mevedel' loads preset definitions used by `mevedel-install'"
  (should (fboundp 'mevedel--define-presets)))


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
;;; Directive processing

(mevedel-deftest mevedel--process-directive
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "writes directive transcript, starts view turn, and sends full prompt directly"
  (let* ((tmpdir (file-name-as-directory
                  (make-temp-file "mevedel-directive-" t)))
         (file (file-name-concat tmpdir "sample.txt"))
         (buf (find-file-noselect file))
         captured-prompt captured-args captured-fsm captured-chat
         callback-result)
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "alpha\nbeta\n")
          (write-region (point-min) (point-max) file nil 'silent)
          (set-buffer-modified-p nil)
          (goto-char (point-min))
          (let ((directive (mevedel--create-directive-in
                            buf (point-min) (line-end-position)
                            nil "Change alpha.")))
            (overlay-put directive 'mevedel-directive-action 'implement)
            (cl-letf (((symbol-function 'save-some-buffers)
                       (lambda (&rest _) nil))
                      ((symbol-function 'display-buffer)
                       (lambda (&rest _) nil))
                      ((symbol-function 'gptel--apply-preset)
                       (lambda (&rest _) nil))
                      ((symbol-function 'gptel-request)
                       (lambda (prompt &rest args)
                         (setq captured-prompt prompt
                               captured-args args
                               captured-chat (plist-get args :buffer))
                         (let ((fsm (plist-get args :fsm)))
                           (setf (gptel-fsm-info fsm)
                                 (list :buffer captured-chat
                                       :position (plist-get args :position)
                                       :callback (lambda (&rest _) nil)))
                           (setq captured-fsm fsm)
                           fsm))))
              (mevedel--process-directive
               directive '(:system "test")
               #'mevedel--implement-directive-prompt
               (lambda (err fsm)
                 (setq callback-result
                       (list err (eq fsm captured-fsm)))))
              (should (string-match-p "IMPLEMENTATION REQUEST"
                                      captured-prompt))
              (should (string-match-p "Change alpha" captured-prompt))
              (should (eq captured-chat (plist-get captured-args :buffer)))
              (should (markerp (plist-get captured-args :position)))
              (with-current-buffer captured-chat
                (should (eq 'processing
                            (overlay-get directive
                                         'mevedel-directive-status)))
                (should (equal (overlay-get directive 'mevedel-uuid)
                               mevedel--current-directive-uuid))
                (save-excursion
                  (goto-char (point-min))
                  (should (search-forward ":PROMPT:" nil t))
                  (should (eq 'ignore (get-text-property (point) 'gptel)))))
              (let ((view-buf (buffer-local-value 'mevedel--view-buffer
                                                  captured-chat)))
                (should (buffer-live-p view-buf))
                (with-current-buffer view-buf
                  (let ((view-text (buffer-substring-no-properties
                                    (point-min) mevedel-view--input-marker)))
                    (should (string-match-p "Implement: Change alpha"
                                            view-text))
                    (should (string-match-p "Prompt" view-text)))
                  (goto-char (point-min))
                  (search-forward "Implement:")
                  (should (eq 'mevedel-view-directive-action
                              (get-text-property (match-beginning 0)
                                                 'font-lock-face)))
                  (should (markerp mevedel-view--in-flight-turn-start))
                  (should (markerp mevedel-view--data-turn-start))
                  (should mevedel-view--spinner-overlay)))
              (let ((response-start
                     (overlay-get directive
                                  'mevedel-directive-response-start)))
                (should (markerp response-start))
                (with-current-buffer captured-chat
                  (goto-char response-start)
                  (insert (propertize "Answer text.\n" 'gptel 'response)))
                (let ((view-buf (buffer-local-value 'mevedel--view-buffer
                                                    captured-chat)))
                  (with-current-buffer view-buf
                    (mevedel-view--render-incremental captured-chat)))
                (overlay-put directive 'mevedel-directive-response-start nil)
                (cl-letf (((symbol-function
                            'mevedel--ov-actions--find-directive-response-start)
                           (lambda (&rest _) nil)))
                  (let ((gptel--fsm-last nil))
                    (mevedel--ov-actions-show-answer directive)))
                (let ((view-buf (buffer-local-value 'mevedel--view-buffer
                                                    captured-chat)))
                  (with-current-buffer view-buf
                    (should (looking-at-p "Answer text")))))
              (funcall (plist-get (gptel-fsm-info captured-fsm)
                                  :mevedel-request-callback)
                       nil captured-fsm)
              (should (equal '(nil t) callback-result))
              (should (eq 'succeeded
                          (overlay-get directive
                                       'mevedel-directive-status)))
              (with-current-buffer captured-chat
                (should-not mevedel--current-directive-uuid)))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (and captured-chat (buffer-live-p captured-chat))
        (let ((view-buf (buffer-local-value 'mevedel--view-buffer
                                            captured-chat)))
          (when (buffer-live-p view-buf)
            (kill-buffer view-buf)))
        (kill-buffer captured-chat))
      (delete-directory tmpdir t))))


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
