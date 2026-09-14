;;; export-provider.el -- Export an isolated evaluation policy -*- lexical-binding: t -*-

;;; Commentary:

;; Evaluate the first form in the user's existing Emacs. Return only the temporary pathname.
;; This does not load or replace any mevedel code in that process.

;;; Code:
(let* ((policy (if-let* ((name (getenv "MEVEDEL_QUALITY_BACKEND")))
                   (let ((backend (cdr (assoc name gptel--known-backends))))
                     (unless backend (error "Unknown configured backend: %s" name))
                     (list :backend backend :model (car (gptel-backend-models backend))))
                 (mevedel-model-resolve-workload
                  (intern (or (getenv "MEVEDEL_QUALITY_WORKLOAD") "journal")))))
       (backend (plist-get policy :backend))
       (model (plist-get policy :model))
       (gptel-backend backend)
       (authentication
        (pcase (type-of backend)
          ('gptel-deepseek (list :key (gptel--get-api-key)))
          ('gptel-openai-oauth (list :header (gptel--openai-oauth-header nil)))
          (_ (error "Unsupported quality backend: %s" (type-of backend)))))
       (config (list :name (gptel-backend-name backend) :type (type-of backend)
                     :model model :model-properties (symbol-plist model)
                     :models (mapcar (lambda (entry) (cons entry (symbol-plist entry)))
                                     (gptel-backend-models backend))
                     :effort (plist-get policy :effort) :stream gptel-stream
                     :backend-options
                     (append (list :host (gptel-backend-host backend)
                           :protocol (gptel-backend-protocol backend)
                           :endpoint (gptel-backend-endpoint backend)
                           :stream (gptel-backend-stream backend)
                           :request-params (gptel-backend-request-params backend)
                           :curl-args (gptel-backend-curl-args backend))
                             authentication)))
       (path (make-temp-file "/tmp/mevedel-memory-quality-provider-" nil ".el")))
  (set-file-modes path #o600)
  (with-temp-file path
    (let ((print-length nil) (print-level nil)) (prin1 config (current-buffer))))
  path)

(provide 'mevedel-memory-quality-export-provider)
;;; export-provider.el ends here
