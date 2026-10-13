;;; gptel-retry-helpers.el --- Local native HTTP fixtures -*- lexical-binding: t -*-

;;; Commentary:
;; Shared disposable curl fixtures; loading registers no ERT cases.

;;; Code:
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-gptel-retry)
(require 'gptel-openai-oauth)
(require 'mevedel-gptel-stream-bridge)
(require 'mevedel-telemetry-usage)
(require 'mevedel-agent-exec)

(defconst mevedel-retry-test--success
  (concat "event: response.output_text.delta\n"
          "data: {\"delta\":\"Recovered answer\"}\n\n"
          "event: response.completed\n"
          "data: {\"response\":{\"status\":\"completed\",\"error\":null,\"usage\":{\"input_tokens\":12,\"output_tokens\":3}}}\n\n"))

(defconst mevedel-retry-test--failure
  (concat "event: response.output_text.delta\n"
          "data: {\"delta\":\"Abandoned output\"}\n\n"
          "event: response.failed\n"
          "data: {\"response\":{\"status\":\"failed\",\"error\":{\"code\":\"server_error\",\"message\":\"Model at capacity\"},\"usage\":{\"input_tokens\":7,\"output_tokens\":2}}}\n\n"))

(defmacro mevedel-retry-test--with-http (responses &rest body)
  "Evaluate BODY against local HTTP RESPONSES, with all state isolated."
  (declare (indent 1) (debug t))
  `(let* ((data-buf (generate-new-buffer " *retry-data*"))
          (session (mevedel-session--create :name "retry-test"))
          (request (mevedel-request--create :id "retry-test-request" :session session))
          (remaining ,responses)
          (requests nil) (clients nil) (scheduled nil) (chunk-timers nil) (transports nil)
          (finished 0) (failed 0) (aborted 0) (waits 0)
          (gptel--request-alist nil)
          (mevedel-gptel-retry--states nil)
          (gptel-use-curl t)
          (gptel-log-level nil)
          (gptel-post-request-hook
           (list (lambda ()
                   (dolist (entry gptel--request-alist)
                     (unless (assq (car entry) transports)
                       (push (cons (car entry)
                                   (plist-get (gptel-fsm-info (cadr entry)) :mevedel-retry-attempt))
                             transports))))))
          (gptel-org-convert-response nil)
          (gptel-post-response-functions nil)
          (gptel-pre-response-hook nil)
          (gptel-post-stream-hook nil)
          (server
           (make-network-process
            :name "retry-http" :server t :host "127.0.0.1" :service t
            :family 'ipv4 :noquery t :coding 'utf-8-unix
            :log (lambda (_server client _message) (push client clients))
            :filter
            (lambda (client text)
              (let ((input (concat (process-get client 'input) text)))
                (process-put client 'input input)
                (when (and (not (process-get client 'answered))
                           (string-match "\r\n\r\n" input)
                           (let ((header-end (match-end 0)))
                             (and (string-match "Content-Length: \\([0-9]+\\)" input)
                                  (>= (- (string-bytes input) header-end)
                                      (string-to-number (match-string 1 input))))))
                  (process-put client 'answered t)
                  (push (substring input (+ 4 (string-match "\r\n\r\n" input))) requests)
                  (let ((response (or (pop remaining) mevedel-retry-test--success)))
                    (let ((body (if (consp response) (apply #'concat response) response)))
                      (process-send-string
                       client (format "HTTP/1.1 200 OK\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                                      (if (string-prefix-p "{" body)
                                          "application/json" "text/event-stream")
                                      (string-bytes body)
                                      (if (consp response) (car response) body)))
                      (when (consp response)
                        (push (run-at-time 0.05 nil
                                           (lambda ()
                                             (when (process-live-p client)
                                               (process-send-string client (apply #'concat (cdr response))))))
                              chunk-timers)))))))))
          (backend (gptel--make-openai-oauth
                    :name "Codex test" :header nil :stream t :models '(test-model)
                    ;; Pass the real auth guard without reading or refreshing
                    ;; credentials.  Headerless requests stay on localhost.
                    :token (list :access_token "synthetic-local-only"
                                 :expires_at (+ (float-time) 3600))
                    :url (format "http://127.0.0.1:%d/responses" (process-contact server :service))))
          (fsm (gptel-make-fsm
                :info (list :buffer data-buf :backend backend :model 'test-model :stream t
                            :position (with-current-buffer data-buf (point-max-marker))
                            :data '(:model "test-model" :input [] :stream t)
                            :callback #'gptel-curl--stream-insert-response)
                :handlers
                `((WAIT ,(lambda (machine)
                           (cl-incf waits)
                           (mevedel-gptel-retry-dispatch machine)))
                  (TOOL gptel--handle-tool-use)
                  (TRET gptel--handle-tool-result)
                  (DONE ,(lambda (_machine) (cl-incf finished)))
                  (ERRS ,(lambda (_machine) (cl-incf failed)))
                  (ABRT ,(lambda (_machine) (cl-incf aborted)))))))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (setq-local mevedel--current-request request)
             (setq-local mevedel--session session)
             (setq-local gptel-backend backend)
             (setq-local gptel-model 'test-model))
           (setf (mevedel-request-fsm request) fsm)
           (mevedel-gptel-retry-install)
           (cl-letf (((symbol-function 'mevedel-transport-run-at-time)
                      (lambda (_seconds function &rest args)
                        (let ((timer (run-at-time 3600 nil #'ignore)))
                          (push (list timer function args) scheduled)
                          timer))))
             ,@body))
       (dolist (entry scheduled) (cancel-timer (car entry)))
       (dolist (timer chunk-timers) (cancel-timer timer))
       (mevedel-gptel-retry-uninstall)
       (dolist (entry gptel--request-alist)
         (when (processp (car entry))
           (set-process-sentinel (car entry) #'ignore)
           (when (buffer-live-p (process-buffer (car entry)))
             (kill-buffer (process-buffer (car entry))))
           (delete-process (car entry))))
       (dolist (client clients) (delete-process client))
       (delete-process server)
       (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

;; A lexical queue is consumed by a local loop rather than symbol-value.
(defmacro mevedel-retry-test--drain-native (&optional until-wait)
  "Wait for native completion, optionally stopping at retry backoff."
  `(let ((deadline (+ (float-time) 10)))
     (while (and (< (float-time) deadline)
                 (not (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT)))
                 (not (and ,until-wait (eq (gptel-fsm-state fsm) 'MRETRY))))
       (accept-process-output nil 0.01)
       (unless ,until-wait
         (when-let* ((entry (pop scheduled)))
           (cancel-timer (car entry))
           (apply (nth 1 entry) (nth 2 entry)))))
     (should (< (float-time) deadline))))

(provide 'gptel-retry-helpers)
;;; gptel-retry-helpers.el ends here
