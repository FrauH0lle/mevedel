;;; mevedel-acp-workload.el --- Scoped external workload tools -*- lexical-binding: t -*-

;;; Commentary:
;; Bounded sessionless workloads retain their existing tool functions and
;; authority checks.  This module owns their private MCP scope and closes it
;; with the ACP request.  It never borrows root-session pipeline authority.

;;; Code:

(require 'mevedel-acp-text)
(require 'mevedel-mcp)
(require 'mevedel-mcp-tools)

(defun mevedel-acp-workload-request (launch prompt tools callback before-tool boundary)
  "Run isolated PROMPT with captured gptel TOOLS and streamed CALLBACK.
LAUNCH receives the generated MCP vector and hook command, returning ACP config.
CALLBACK follows gptel's text convention, with reasoning and tool-result events;
t means the entire external turn completed.  BEFORE-TOOL receives TOOL and ARGS
before execution.  BOUNDARY runs after each native tool batch, before more model
work, and may return reminder text.  Either guard may signal to end the workload
as an error.  Tool functions retain scope validation and helper cancellation.
Return a canceller; killing the current buffer also retires this scope."
  (let ((buffer (current-buffer)) (calls (make-hash-table :test #'equal))
        server configuration cancel-text finished)
    (cl-labels
        ((live () (and (not finished) (buffer-live-p buffer)))
         (finish (response info)
           (unless finished
             (setq finished t)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (remove-hook 'kill-buffer-hook #'cancel t)))
             (when server (mevedel-mcp-stop server))
             (when cancel-text (funcall cancel-text))
             (funcall callback response info)))
         (cancel () (finish 'abort (list :buffer buffer :stream t)))
         (fail (err)
           (finish nil (list :buffer buffer :stream t :error (error-message-string err))))
         (receive (response info)
           (when (live)
             (if (memq response '(t nil abort)) (finish response info)
               (funcall callback response info))))
         (dispatch (name args metadata complete)
           (condition-case err
               (progn
                 (unless (live) (error "Workload has ended"))
                 (let* ((tool (cl-find name tools :test #'equal :key #'gptel-tool-name))
                        (id (plist-get metadata (plist-get configuration :tool-id-field)))
                        done cancel-tool)
                   (unless (and tool (stringp id) (not (string-empty-p id)))
                     (error "Workload tool call lacks its admitted tool or native identity"))
                   (when (gethash id calls) (error "Tool call identity was already admitted"))
                   (puthash id t calls)
                   (cl-labels
                       ((result (value &optional errorp)
                          (unless done
                            (setq done t)
                            (when (live)
                              (with-current-buffer buffer
                                (let ((text (gptel--to-string value)))
                                  (receive (list 'tool-result (list tool args text))
                                           (list :buffer buffer :stream t))
                                  (funcall complete
                                           (list :isError (if errorp t :json-false)
                                                 :content (vector (list :type "text" :text text)))))))))
                        (cancel-call ()
                          (setq done t)
                          (when (functionp cancel-tool) (funcall cancel-tool))))
                     (with-current-buffer buffer
                       (when before-tool (funcall before-tool tool args))
                       (when (live)
                         (condition-case err
                             (let ((values (gptel--map-tool-args tool args)))
                               (if (gptel-tool-async tool)
                                   (setq cancel-tool (apply (gptel-tool-function tool) #'result values))
                                 (result (apply (gptel-tool-function tool) values))))
                           (error (result (error-message-string err) t)))))
                     #'cancel-call)))
             (error (fail err)))))
      (add-hook 'kill-buffer-hook #'cancel nil t)
      (condition-case err
          (progn
            (setq server
                  (mevedel-mcp-start
                   (lambda () (mevedel-mcp-tools-schemas tools)) #'dispatch
                   (lambda (event complete)
                     (unless (live) (error "Workload has ended"))
                     (condition-case err
                         (with-current-buffer buffer
                           (funcall complete (funcall (plist-get configuration :control) boundary event)))
                       (error (fail err) (signal (car err) (cdr err)))))
                   (lambda (message) (fail (list 'error message)))))
            (setq configuration (funcall launch (vector (mevedel-mcp-configuration server))
                                         (mevedel-mcp-hook-command server)))
            (when (live)
              (setq cancel-text (mevedel-acp-text-request configuration prompt #'receive t)))
            (when (and finished cancel-text) (funcall cancel-text)))
        (error (fail err))
        (quit (cancel) (signal (car err) (cdr err))))
      #'cancel)))

(provide 'mevedel-acp-workload)
;;; mevedel-acp-workload.el ends here
