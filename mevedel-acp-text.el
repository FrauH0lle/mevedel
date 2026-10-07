;;; mevedel-acp-text.el --- Isolated ACP text requests -*- lexical-binding: t -*-

;;; Commentary:
;; Background requests share the ACP lifecycle without acquiring a session
;; turn or transcript.  Callers own any supplied tool scope, output policy and
;; workload validation.  The buffer owns cancellation and process cleanup.

;;; Code:

(require 'mevedel-acp)

(defun mevedel-acp-text-request (launch prompt callback &optional stream)
  "Generate isolated text for PROMPT using ACP LAUNCH; return a canceller.
CALLBACK receives RESPONSE and an info plist, using gptel's text callback
convention: streamed strings followed by t, one collected string otherwise,
nil on failure or `abort' on cancellation.  INFO contains :stream, available
:tokens and :error.  The current buffer owns the request; killing it cancels.
LAUNCH may specify :response-timeout in seconds after startup.
LAUNCH must have no retained session.  Supplied MCP servers remain caller-owned."
  (when (plist-get launch :session-id)
    (error "Isolated text requests cannot use retained history"))
  (unless (and (stringp prompt) (functionp callback))
    (error "Isolated text requests require text and a callback"))
  (let ((buffer (current-buffer)) connection finished chunks timer)
    (cl-labels
        ((finish (outcome)
           (unless finished
             (setq finished t)
             (when timer (cancel-timer timer) (setq timer nil))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (remove-hook 'kill-buffer-hook #'cancel t)))
             (when connection (mevedel-acp-close connection))
             (let ((response
                    (pcase (plist-get outcome :status)
                      ('success (if stream t (apply #'concat (nreverse chunks))))
                      ('interrupted 'abort)))
                   (info (list :buffer buffer :stream stream
                               :tokens (plist-get outcome :tokens)
                               :error (plist-get outcome :message))))
               (setq chunks nil)
               (with-current-buffer (if (buffer-live-p buffer) buffer (current-buffer))
                 (funcall callback response info)))))
         (cancel () (finish '(:status interrupted)))
         (event (notification)
           (unless finished
             (condition-case err
                 (let* ((update (alist-get 'update (alist-get 'params notification)))
                        (content (alist-get 'content update)))
                   (when (and (member (alist-get 'sessionUpdate update)
                                      '("agent_message_chunk" "agent_thought_chunk"))
                              (equal "text" (alist-get 'type content))
                              (stringp (alist-get 'text content)))
                     (cond
                      ((equal "agent_thought_chunk" (alist-get 'sessionUpdate update))
                       (when stream
                         (with-current-buffer buffer
                           (funcall callback (cons 'reasoning (alist-get 'text content))
                                    (list :buffer buffer :stream t)))))
                      (stream
                         (with-current-buffer buffer
                           (funcall callback (alist-get 'text content)
                                    (list :buffer buffer :stream t))))
                      (t (push (alist-get 'text content) chunks)))))
               (error (finish (list :status 'error :message (error-message-string err))))))))
      (add-hook 'kill-buffer-hook #'cancel nil t)
      (condition-case err
          (setq connection
                (mevedel-acp-open
                 launch
                 (lambda (active)
                   (when-let* ((timeout (plist-get launch :response-timeout)))
                     (setq timer
                           (mevedel-transport-run-at-time
                            timeout
                            (lambda ()
                              (finish '(:status error :message "ACP inspection response timed out"))))))
                   (mevedel-acp-prompt
                    active (vector `((type . "text") (text . ,prompt)))
                    #'event #'finish))
                 (lambda (message) (finish (list :status 'error :message message)))))
        (error (finish (list :status 'error :message (error-message-string err))))
        (quit (cancel) (signal (car err) (cdr err))))
      #'cancel)))

(provide 'mevedel-acp-text)
;;; mevedel-acp-text.el ends here
