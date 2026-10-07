;;; mevedel-auth.el --- Asynchronous provider authentication -*- lexical-binding: t -*-
;;; Commentary:
;; Provider credentials stay on the host.  One operation per credential store
;; exposes only an owner-facing URL/code challenge and safe status messages.
;;; Code:
(require 'gptel-openai-oauth)
(require 'url-http)
(require 'json)
(require 'mevedel-recovery)
(require 'mevedel-claude-code-backend)

;; `mevedel-claude-code'
(declare-function mevedel-claude-code--prepare-launch "mevedel-claude-code" (checks cli environment ready failure))
(defvar mevedel-claude-code--api-environment)
(defvar mevedel-claude-code-executable)

;; `mevedel-transport'
(autoload 'mevedel-transport-run-at-time "mevedel-transport")
(declare-function mevedel-transport-run-at-time "mevedel-transport" (seconds function &rest args))

;; `url-http'
(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defvar-local mevedel-auth--waiting-backend nil
  "Effective provider whose renewal must wake this buffer's retained input.")

(defvar-local mevedel-auth--managed nil
  "Non-nil when provider requests in this buffer use asynchronous renewal.")

(defvar mevedel-auth--operations (make-hash-table :test #'equal))
(defvar mevedel-auth-changed-hook nil
  "Functions called with BACKEND and safe STATE after an authentication change.")
(autoload 'mevedel-readiness-changed "mevedel-readiness")
(add-hook 'mevedel-auth-changed-hook #'mevedel-readiness-changed)

(defun mevedel-auth--key (backend)
  "Return the credential-store identity for BACKEND."
  (cond ((gptel-openai-oauth-p backend) (expand-file-name gptel--openai-oauth-token-file))
        ((mevedel-claude-code-backend-p backend)
         (concat "claude:" (or (getenv "CLAUDE_CONFIG_DIR") (expand-file-name "~/.claude"))))
        (t (user-error "This provider has no browser login"))))

(defun mevedel-auth-state (backend)
  "Return BACKEND's ephemeral owner-visible state, without credentials."
  (when-let* ((operation (gethash (mevedel-auth--key backend) mevedel-auth--operations)))
    (cl-loop for key in '(:id :status :message :url :code)
             append (list key (plist-get operation key)))))

(defun mevedel-auth--publish (backend status message &optional url code)
  "Publish BACKEND's STATUS and MESSAGE with optional login URL and CODE."
  (let* ((key (mevedel-auth--key backend))
         (op (or (gethash key mevedel-auth--operations)
                 (list :id (format "%s-%s" (emacs-pid) (float-time))))))
    (setq op (plist-put op :status status)
          op (plist-put op :message message)
          op (plist-put op :url url)
          op (plist-put op :code code))
    (unless (member status '("login" "refreshing"))
      (when-let* ((lock (plist-get op :lock)))
        (unlock-file lock)
        (setq op (plist-put op :lock nil))))
    (puthash key op mevedel-auth--operations)
    (run-hook-wrapped 'mevedel-auth-changed-hook
                      (lambda (observer)
                        (condition-case nil
                            (funcall observer backend (mevedel-auth-state backend))
                          (error nil))
                        nil))))

(defun mevedel-auth--http (url data form callback &optional headers parse)
  "Request URL asynchronously; CALLBACK receives payload and HTTP code.
POST DATA, form-encoded when FORM, or GET when DATA is nil.  HEADERS are
extra request headers.  PARSE, called at the response body, returns the
payload and defaults to a JSON plist; a failed parse yields nil.  Code 0
means no response within 30 seconds or no connection.  Return a canceller.
Never expose HTTP bodies in errors."
  (let (buffers timer done)
    (cl-labels ((clean ()
                  (when timer (cancel-timer timer))
                  ;; url-http retries a 401 in a new buffer; kill each one.
                  (dolist (buffer buffers)
                    (when (buffer-live-p buffer)
                      (when-let* ((process (get-buffer-process buffer))) (delete-process process))
                      (kill-buffer buffer))))
                (finish (payload code)
                  (unless done (setq done t) (clean) (funcall callback payload code)))
                (cancel () (setq done t) (clean)))
      (condition-case nil
          (let ((url-request-method (if data "POST" "GET"))
                (url-request-data
                 (and data (encode-coding-string
                            (if form (url-build-query-string data) (json-serialize data)) 'utf-8)))
                (url-request-extra-headers
                 (append (and data `(("Content-Type" . ,(if form "application/x-www-form-urlencoded"
                                                          "application/json"))))
                         headers))
                (url-show-status nil) (url-request-noninteractive t) (url-max-redirections 0))
            (setq timer (mevedel-transport-run-at-time 30 (lambda () (finish nil 0))))
            (push (url-retrieve url
                                (lambda (_status)
                                  (push (current-buffer) buffers)
                                  (let ((code url-http-response-status)
                                        (payload (condition-case nil
                                                     (progn (goto-char url-http-end-of-headers)
                                                            (if parse (funcall parse)
                                                              (json-parse-buffer :object-type 'plist :null-object nil
                                                                                 :false-object nil)))
                                                   (error nil))))
                                    (finish payload code))) nil t t)
                  buffers)
            (when done (clean)))
        (error (finish nil 0))
        (quit (cancel) (signal 'quit nil)))
      #'cancel)))

(defun mevedel-auth-cancel (backend)
  "Cancel BACKEND's current authentication operation and discard its challenge."
  (let ((op (gethash (mevedel-auth--key backend) mevedel-auth--operations)))
    (when-let* ((cancel (plist-get op :cancel))) (funcall cancel))
    (when-let* ((lock (plist-get op :lock))) (unlock-file lock))
    (remhash (mevedel-auth--key backend) mevedel-auth--operations)
    (run-hook-wrapped 'mevedel-auth-changed-hook
                      (lambda (observer)
                        (condition-case nil (funcall observer backend nil) (error nil)) nil))))

(defun mevedel-auth--current-p (token)
  "Return whether TOKEN holds an access token valid for 30 more seconds."
  (and (proper-list-p token)
       (stringp (plist-get token :access_token))
       (numberp (plist-get token :expires_at))
       (> (plist-get token :expires_at) (+ (float-time) 30))))

(defun mevedel-auth-codex-ready-p (backend)
  "Reload BACKEND credentials and return whether its access token is current."
  (condition-case nil
      (let ((token (gptel-oauth--read-token gptel--openai-oauth-token-file)))
        (unless (and (proper-list-p token) (cl-evenp (length token)))
          (setq token nil))
        (setf (gptel-openai-oauth-token backend) token)
        (mevedel-auth--current-p token))
    (error (setf (gptel-openai-oauth-token backend) nil) nil)))

(defun mevedel-auth--codex-persist (backend payload)
  "Store validated PAYLOAD through gptel and refresh every sharing BACKEND."
  (unless (and (stringp (plist-get payload :access_token))
               (stringp (plist-get payload :refresh_token))
               (numberp (plist-get payload :expires_in)))
    (error "Invalid authentication response"))
  (gptel--openai-oauth-persist backend payload)
  (set-file-modes gptel--openai-oauth-token-file #o600)
  (dolist (entry gptel--known-backends)
    (when (gptel-openai-oauth-p (cdr entry))
      (setf (gptel-openai-oauth-token (cdr entry)) (gptel-openai-oauth-token backend)))))


(defun mevedel-auth--lock (backend)
  "Acquire BACKEND's local credential lock for an asynchronous operation."
  (let ((lock (concat (mevedel-auth--key backend) ".mevedel-auth")))
    ;; Claude owns its own credential serialization inside its CLI.
    (when (gptel-openai-oauth-p backend)
      (make-directory (file-name-directory lock) t)
      (when (file-locked-p lock)
        (mevedel-auth--publish backend "failed" "Another host process is renewing Codex login; retry shortly")
        (user-error "Another host process is renewing Codex login; retry shortly"))
      ;; `lock-file' does nothing while `create-lockfiles' is nil.
      (let ((create-lockfiles t)) (lock-file lock))
      (plist-put (gethash (mevedel-auth--key backend) mevedel-auth--operations) :lock lock))))

(defun mevedel-auth-assert-ready (backend)
  "Reject stale Codex BACKEND credentials before gptel can prompt or block.
Begin asynchronous renewal; callers retain their ordinary failure lifecycle."
  (setq-local mevedel-auth--managed t)
  (when (and (gptel-openai-oauth-p backend)
             (not (mevedel-auth-codex-ready-p backend)))
    (setq mevedel-auth--waiting-backend backend)
    (mevedel-auth-refresh backend)
    (user-error "Codex login needs renewal; sign in or retry after refresh")))

(defun mevedel-auth--dispatch (original fsm)
  "Guard every managed FSM sample before ORIGINAL constructs OAuth headers."
  (let* ((info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer))
         (backend (plist-get info :backend)))
    (if (not (and (gptel-openai-oauth-p backend) (buffer-live-p buffer)
                  (or (buffer-local-value 'mevedel-auth--managed buffer)
                      (buffer-local-value 'mevedel--session buffer))
                  ;; Every request and tool round trip passes here; reread the
                  ;; credential file only once the loaded token is stale.
                  (not (mevedel-auth--current-p (gptel-openai-oauth-token backend)))
                  (not (mevedel-auth-codex-ready-p backend))))
        (funcall original fsm)
      (condition-case nil (mevedel-auth-refresh backend) (error nil))
      (setf (gptel-fsm-info fsm)
            (plist-put info :error '(:code 401 :message "Codex login needs renewal; review the interrupted turn before continuing")))
      (gptel--fsm-transition fsm 'ERRS))))
(advice-add 'gptel--handle-wait :around #'mevedel-auth--dispatch)

(defun mevedel-auth-refresh (backend)
  "Refresh BACKEND asynchronously once per credential store."
  (unless (member (plist-get (mevedel-auth-state backend) :status) '("refreshing" "login"))
    (let ((refresh (plist-get (gptel-openai-oauth-token backend) :refresh_token)))
      (if (not refresh)
          (mevedel-auth--publish backend "required" "Sign in to Codex to continue")
        (mevedel-auth--publish backend "refreshing" "Refreshing Codex login")
        (mevedel-auth--lock backend)
        (let* ((op (gethash (mevedel-auth--key backend) mevedel-auth--operations))
               (cancel
                (mevedel-auth--http
                 (concat gptel--openai-oauth-url "/oauth/token")
                 `(("grant_type" "refresh_token") ("refresh_token" ,refresh)
                   ("client_id" ,gptel--openai-oauth-client-id)) t
                 (lambda (payload code)
                   (when (eq op (gethash (mevedel-auth--key backend) mevedel-auth--operations))
                     (condition-case nil
                         (if (eql code 200)
                             (progn (mevedel-auth--codex-persist backend payload)
                                    (mevedel-auth--publish backend "ready" "Codex login ready"))
                           (mevedel-auth--publish backend (if (memq code '(400 401 403)) "required" "failed")
                                                  (if (memq code '(400 401 403)) "Codex login expired; sign in again"
                                                    "Codex refresh failed; check connectivity and retry")))
                       (error (mevedel-auth--publish backend "required" "Codex login response was invalid; sign in again"))))))))
          (plist-put op :cancel cancel))))))

(defun mevedel-auth--codex-login (backend)
  "Start BACKEND's asynchronous device-code login using gptel's protocol."
  (let ((op (gethash (mevedel-auth--key backend) mevedel-auth--operations))
        (deadline (+ (float-time) 300)) cancel timer stopped)
    (cl-labels
        ((cleanup () (when cancel (funcall cancel)) (when timer (cancel-timer timer)))
         (stop () (setq stopped t) (cleanup))
         (fail () (stop) (mevedel-auth--publish backend "failed" "Codex login failed or expired; start again"))
         (exchange (payload)
           (setq cancel (mevedel-auth--http
                         (concat gptel--openai-oauth-url "/oauth/token")
                         `(("grant_type" "authorization_code")
                           ("code" ,(plist-get payload :authorization_code))
                           ("code_verifier" ,(plist-get payload :code_verifier))
                           ("client_id" ,gptel--openai-oauth-client-id)
                           ("redirect_uri" "https://auth.openai.com/deviceauth/callback")) t
                         (lambda (tokens code)
                           (unless stopped
                             (condition-case nil
                                 (if (not (eql code 200)) (fail)
                                   (mevedel-auth--codex-persist backend tokens)
                                   (stop)
                                   (mevedel-auth--publish backend "ready" "Codex login ready"))
                               (error (fail))))))))
         (poll (id user-code)
           (unless stopped
             (if (> (float-time) deadline) (fail)
               (setq cancel
                     (mevedel-auth--http
                      (concat gptel--openai-oauth-url "/api/accounts/deviceauth/token")
                      (list :device_auth_id id :user_code user-code) nil
                      (lambda (payload code)
                        (unless stopped
                          (cond ((plist-get payload :authorization_code) (exchange payload))
                                ((or (memq code '(403 404))
                                     (and (eql code 200) (not (plist-get payload :error))))
                                 (setq timer (mevedel-transport-run-at-time 2 #'poll id user-code)))
                                (t (fail)))))))))))
      (plist-put op :cancel #'stop)
      (setq cancel
            (mevedel-auth--http
             (concat gptel--openai-oauth-url "/api/accounts/deviceauth/usercode")
             (list :client_id gptel--openai-oauth-client-id) nil
             (lambda (payload code)
               (unless stopped
                 (if (not (and (eql code 200) (stringp (plist-get payload :device_auth_id))
                               (stringp (plist-get payload :user_code)))) (fail)
                   (mevedel-auth--publish backend "login" "Authorize Codex in your browser"
                                          (concat gptel--openai-oauth-url "/codex/device")
                                          (plist-get payload :user_code))
                   (poll (plist-get payload :device_auth_id) (plist-get payload :user_code))))))))))

(defun mevedel-auth--claude-login (backend)
  "Run Claude's supported login subprocess for BACKEND, keeping credentials local."
  (require 'mevedel-claude-code)
  (let ((op (gethash (mevedel-auth--key backend) mevedel-auth--operations))
        (process-environment (copy-sequence process-environment))
        (default-directory temporary-file-directory)
        (output "") process timer stopped environment
        (cli mevedel-claude-code-executable))
    (dolist (name (append mevedel-claude-code--api-environment
                          '("CLAUDE_CODE_OAUTH_REFRESH_TOKEN" "CLAUDE_CODE_OAUTH_SCOPES")))
      (setenv name nil))
    (setenv "BROWSER" "true")
    (setq environment process-environment)
    (cl-labels ((stop ()
                  (setq stopped t output "")
                  (when timer (cancel-timer timer))
                  (when process (set-process-filter process #'ignore)
			(set-process-sentinel process #'ignore)
			(when (process-live-p process) (delete-process process))))
		(fail () (stop) (mevedel-auth--publish backend "failed" "Claude login failed or expired; start again")))
      (plist-put op :cancel #'stop)
      (condition-case nil
          (progn
            (setq timer (mevedel-transport-run-at-time 300 #'fail))
            (setq process
                  (make-process
                   :name "mevedel-claude-login" :command (list mevedel-claude-code-executable "auth" "login" "--claudeai")
                   :connection-type 'pipe :noquery t :coding 'utf-8-unix
                   :filter (lambda (_process text)
                             (unless stopped
                               (setq output (concat output text))
                               (if (> (length output) 32768) (fail)
                                 (when (string-match "https://\\(?:claude\\.ai\\|platform\\.claude\\.com\\)/oauth/authorize[^[:space:]\"\033]*" output)
                                   (mevedel-auth--publish backend "login" "Sign in to Claude, then paste the full returned code"
                                                          (match-string 0 output))))))
                   :sentinel (lambda (child _event)
                               (when (and (not stopped) (memq (process-status child) '(exit signal)))
                                 (if (and (eq (process-status child) 'exit) (zerop (process-exit-status child)))
                                     (progn
                                       (stop)
                                       (plist-put op :cancel
                                                  (mevedel-claude-code--prepare-launch
                                                   nil cli environment
                                                   (lambda () (mevedel-auth--publish backend "ready" "Claude subscription login ready"))
                                                   (lambda (_message) (mevedel-auth--publish backend "required" "Claude subscription login is required; check the selected account")))))
                                   (fail))))))
            (plist-put op :process process))
        (error (fail))))))

(defun mevedel-auth-start (backend)
  "Start an owner-requested login for BACKEND, or return its active operation."
  (unless (member (plist-get (mevedel-auth-state backend) :status) '("refreshing" "login"))
    (mevedel-auth-cancel backend)
    (mevedel-auth--publish backend "login" "Starting provider login")
    (mevedel-auth--lock backend)
    (if (gptel-openai-oauth-p backend) (mevedel-auth--codex-login backend)
      (mevedel-auth--claude-login backend)))
  (mevedel-auth-state backend))

(defun mevedel-auth-submit-code (backend id code)
  "Submit a single Claude authorization CODE to BACKEND's operation ID."
  (let ((op (gethash (mevedel-auth--key backend) mevedel-auth--operations)))
    (unless (and (equal id (plist-get op :id))
                 (equal "login" (plist-get op :status))
                 (stringp code) (< (length code) 8192)
                 (string-match-p "\\`[A-Za-z0-9_-]+#[A-Za-z0-9_-]+\\'" code)
                 (process-live-p (plist-get op :process)))
      (user-error "Login code or operation is no longer valid"))
    (process-send-string (plist-get op :process) (concat code "\n"))))

(defun mevedel-auth-stop-all ()
  "Cancel all owned login processes and HTTP operations on shutdown."
  (maphash (lambda (_key op) (when-let* ((cancel (plist-get op :cancel))) (funcall cancel))
             (when-let* ((lock (plist-get op :lock))) (unlock-file lock)))
           mevedel-auth--operations)
  (clrhash mevedel-auth--operations))
(add-hook 'kill-emacs-hook #'mevedel-auth-stop-all)

(provide 'mevedel-auth)
;;; mevedel-auth.el ends here
