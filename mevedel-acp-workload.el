;;; mevedel-acp-workload.el --- Scoped external workload tools -*- lexical-binding: t -*-

;;; Commentary:
;; Bounded sessionless workloads retain their existing tool functions and
;; authority checks.  This module owns their private MCP scope and closes it
;; with the ACP request.  It never borrows root-session pipeline authority.

;;; Code:

(require 'mevedel-acp-text)
(require 'mevedel-mcp)
(require 'mevedel-mcp-tools)

;; `mevedel-utilities'
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))
(autoload 'mevedel--warn-once "mevedel-utilities")

(defun mevedel-acp-workload-request (launch prompt tools callback before-tool boundary)
  "Run isolated PROMPT with captured gptel TOOLS and streamed CALLBACK.
LAUNCH receives the generated MCP vector and hook command, returning ACP config.
CALLBACK follows gptel's text convention, with reasoning and tool-result events;
t means the entire external turn completed.  BEFORE-TOOL receives TOOL and ARGS
before execution.  BOUNDARY runs after each native tool batch, before more model
work, and may return reminder text.  Either guard may signal to end the workload
as an error.  Tool functions retain scope validation and helper cancellation.
A tool result or hook reply whose MCP client closed first stays done locally;
the terminal info then lists those replies as :mcp-lost, (:kind KIND :id ID).
Unexpected delivery errors retire the owner as an error.  A quit or nonlocal
exit from workload callbacks retires it with `abort', then preserves that exit.
Teardown errors warn once per owner; errors and quits do not replace the
outcome or skip its callback.
Return a canceller; killing the current buffer also retires this scope."
  (let ((buffer (current-buffer)) (calls (make-hash-table :test #'equal))
        server configuration cancel-text finished lost)
    (cl-labels
        ((live () (and (not finished) (buffer-live-p buffer)))
         (warn-cleanup (owner err)
           ;; Reporting must not interrupt the remaining cleanup or settlement.
           (condition-case nil
               (mevedel--warn-once
                (list 'acp-workload-cleanup owner)
                "ACP workload %s cleanup failed: %s" owner (error-message-string err))
             ((error quit) nil)))
         (finish (response info)
           (unless finished
             (setq finished t)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (remove-hook 'kill-buffer-hook #'cancel t)))
             ;; Teardown failures must neither skip the other owner nor replace
             ;; the outcome being delivered.  In particular, CANCEL can run
             ;; while unwinding a quit from a tool or its result callback.
             (let ((inhibit-quit t))
               (condition-case err
                   (when server (mevedel-mcp-stop server))
                 (error (warn-cleanup 'MCP err))
                 (quit nil))
               (condition-case err
                   (when cancel-text (funcall cancel-text))
                 (error (warn-cleanup 'ACP err))
                 (quit nil)))
             ;; Replies whose client closed are diagnostics; the agent's own
             ;; RESPONSE settles the workload.
             (funcall callback response
                      (if lost (plist-put (copy-sequence info) :mcp-lost lost) info))))
         (cancel () (finish 'abort (list :buffer buffer :stream t)))
         (fail (err)
           (finish nil (list :buffer buffer :stream t :error (error-message-string err))))
         (guard (function)
           ;; Ordinary errors are handled at their existing boundaries.  Any
           ;; other unwind must retire the scope and preserve the original exit.
           (let (returned)
             (unwind-protect
                 (prog1 (funcall function) (setq returned t))
               (unless returned
                 (let ((inhibit-quit t))
                   (condition-case nil (cancel) ((error quit) nil)))))))
         (receive (response info)
           (guard
            (lambda ()
              (condition-case err
                  (when (live)
                    (if (memq response '(t nil abort)) (finish response info)
                      (funcall callback response info)))
                (error (fail err))))))
         (dispatch (name args metadata complete)
           (guard
            (lambda ()
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
                               ;; This callback also runs after DISPATCH returns.
                               ;; A delivery failure cannot retry an already-done
                               ;; result; settle its owner instead of stranding it.
                               (guard
                                (lambda ()
                                  (condition-case err
                                      (when (live)
                                        (with-current-buffer buffer
                                          (let ((text (gptel--to-string value)))
                                            ;; Answer before publishing the display.
                                            (funcall complete
                                                     (list :isError (if errorp t :json-false)
                                                           :content (vector (list :type "text" :text text))))
                                            (receive (list 'tool-result (list tool args text))
                                                     (list :buffer buffer :stream t)))))
                                    (error (fail err)))))))
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
                (error (fail err)))))))
      (add-hook 'kill-buffer-hook #'cancel nil t)
      (guard
       (lambda ()
         (condition-case err
             (progn
               (setq server
                     (mevedel-mcp-start
                      (lambda () (mevedel-mcp-tools-schemas tools)) #'dispatch
                      (lambda (event complete)
                        (guard
                         (lambda ()
                           (unless (live) (error "Workload has ended"))
                           (condition-case err
                               (with-current-buffer buffer
                                 (funcall complete (funcall (plist-get configuration :control) boundary event)))
                             (error (fail err) (signal (car err) (cdr err)))))))
                      (lambda (message) (fail (list 'error message)))
                      (lambda (diagnostic)
                        (when (< (length lost) 8)
                          (setq lost (append lost (list diagnostic)))))))
               (setq configuration (funcall launch (vector (mevedel-mcp-configuration server))
                                            (mevedel-mcp-hook-command server)))
               (when (live)
                 (setq cancel-text (mevedel-acp-text-request configuration prompt #'receive t)))
               (when (and finished cancel-text) (funcall cancel-text)))
           (error (fail err)))))
      #'cancel)))

(provide 'mevedel-acp-workload)
;;; mevedel-acp-workload.el ends here
