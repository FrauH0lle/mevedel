;;; collaboration-transport-spike.el --- web-server transport spike -*- lexical-binding: t; -*-

;;; Commentary:

;; A repository-owned feasibility check for GNU ELPA `web-server'.  One
;; loopback listener serves the bundled viewer assets and a WebSocket route.
;; The shell runner and Node client perform the real socket assertions.

;;; Code:

(require 'web-server)

(defvar mevedel-transport-spike--server nil)
(defvar mevedel-transport-spike--state-file (getenv "MEVEDEL_SPIKE_STATE"))
(defvar mevedel-transport-spike--stop-file (getenv "MEVEDEL_SPIKE_STOP"))

(defun mevedel-transport-spike--request-path (request)
  "Return the exact GET path in REQUEST, or nil."
  (cdr (assq :GET (slot-value request 'headers))))

(defun mevedel-transport-spike--header (request name)
  "Return request header NAME from REQUEST."
  (cdr (assq (intern (concat ":" (upcase name)))
             (slot-value request 'headers))))

(defun mevedel-transport-spike--asset (name)
  "Return the bundled asset named NAME as an unibyte string."
  (with-temp-buffer
    (insert-file-contents-literally
     (expand-file-name name
                       (file-name-concat
                        (or (getenv "MEVEDEL_SPIKE_ROOT") default-directory)
                        "collaboration")))
    (buffer-string)))

(defun mevedel-transport-spike--send-http (process code type body)
  "Send an HTTP CODE response with TYPE and BODY to PROCESS."
  (ws-response-header
   process code
   (cons "Content-Type" type)
   (cons "Content-Length" (string-bytes body))
   (cons "Content-Security-Policy"
         "default-src 'none'; script-src 'self'; style-src 'self'; connect-src 'self'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'")
   (cons "X-Content-Type-Options" "nosniff")
   (cons "Cache-Control" "no-store"))
  (process-send-string process body))

(defun mevedel-transport-spike--handler (request)
  "Handle one transport-spike REQUEST."
  (let* ((process (slot-value request 'process))
         (path (mevedel-transport-spike--request-path request))
         (origin (mevedel-transport-spike--header request "origin"))
         (server-process (process mevedel-transport-spike--server))
         (port (process-contact server-process :service))
         (allowed-origin (format "http://127.0.0.1:%s" port)))
    (cond
     ((member path '("/" "/index.html"))
      (mevedel-transport-spike--send-http
       process 200 "text/html; charset=utf-8"
       (mevedel-transport-spike--asset "index.html")))
     ((equal path "/viewer.css")
      (mevedel-transport-spike--send-http
       process 200 "text/css; charset=utf-8"
       (mevedel-transport-spike--asset "viewer.css")))
     ((equal path "/viewer.js")
      (mevedel-transport-spike--send-http
       process 200 "text/javascript; charset=utf-8"
       (mevedel-transport-spike--asset "viewer.js")))
     ((equal path "/ws")
      (if (not (equal origin allowed-origin))
          (mevedel-transport-spike--send-http
           process 403 "text/plain; charset=utf-8" "Origin rejected")
        (when (ws-web-socket-connect
               request
               (lambda (socket data)
                 (when (equal data "ping")
                   (process-send-string
                    socket (ws-web-socket-frame "pong")))))
          :keep-alive)))
     (t
      (mevedel-transport-spike--send-http
       process 404 "text/plain; charset=utf-8" "Not found")))))

(defun mevedel-transport-spike--stop ()
  "Stop the spike listener."
  (when mevedel-transport-spike--server
    (ws-stop mevedel-transport-spike--server)
    (setq mevedel-transport-spike--server nil)))

(add-hook 'kill-emacs-hook #'mevedel-transport-spike--stop)

(setq mevedel-transport-spike--server
      (ws-start #'mevedel-transport-spike--handler
                t nil :host "127.0.0.1"))
(let* ((process (process mevedel-transport-spike--server))
       (port (process-contact process :service))
       (host (or (process-contact process :host) "127.0.0.1")))
  (unless (equal host "127.0.0.1")
    (error "Spike listener bound to unexpected host %s" host))
  (with-temp-file mevedel-transport-spike--state-file
    (insert (format "started:%s:%s\n" host port)))
  (message "transport-spike listening on %s:%s" host port)
  (while (not (and mevedel-transport-spike--stop-file
                   (file-exists-p mevedel-transport-spike--stop-file)))
    (accept-process-output nil 1)))

(provide 'collaboration-transport-spike)
;;; collaboration-transport-spike.el ends here
