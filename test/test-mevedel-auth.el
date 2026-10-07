;;; test-mevedel-auth.el --- Provider authentication lifecycle -*- lexical-binding: t -*-
;;; Commentary:
;; Real HTTP and subprocess fixtures exercise expiry, login and cancellation.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-auth)
(require 'mevedel-claude-code)

(defconst mevedel-auth-test--fixtures
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "fixtures"))

(defmacro mevedel-auth-test--with-provider (status &rest body)
  "Run BODY with isolated credentials and a local HTTP server returning STATUS."
  (declare (indent 1))
  `(let* ((directory (make-temp-file "mevedel-auth-test-" t))
          (port-file (file-name-concat directory "port"))
          (gptel--openai-oauth-token-file (file-name-concat directory "token.json"))
          (gptel--known-backends nil)
          (mevedel-auth--operations (make-hash-table :test #'equal))
          (mevedel-auth-changed-hook nil)
          (backend (gptel-make-openai-oauth "Test Codex"))
          (server (make-process :name "mevedel-auth-fixture" :noquery t
				:command (list (executable-find "python3")
                                               (file-name-concat mevedel-auth-test--fixtures "provider-auth.py")
                                               port-file (number-to-string ,status)))))
     (unwind-protect
         (cl-labels ((await (predicate)
                       (with-timeout (5 (ert-fail "Authentication fixture timed out"))
                         (while (not (funcall predicate)) (accept-process-output nil 0.01)))))
           (await (lambda () (file-exists-p port-file)))
           (let ((gptel--openai-oauth-url
                  (concat "http://127.0.0.1:" (with-temp-buffer (insert-file-contents port-file) (buffer-string)))))
             ,@body))
       (mevedel-auth-stop-all)
       (delete-process server)
       (delete-directory directory t))))

(mevedel-deftest mevedel-auth-refresh (:quiet t)
  ,test
  (test)
  :doc "expired credentials renew once and every backend observes the new token"
  (mevedel-auth-test--with-provider 200
				    (gptel-oauth--write-token gptel--openai-oauth-token-file
							      '(:access_token "old" :refresh_token "old-refresh" :expires_at 0))
				    (should-not (mevedel-auth-codex-ready-p backend))
				    (should-error (mevedel-auth-assert-ready backend) :type 'user-error)
				    (let ((id (plist-get (mevedel-auth-state backend) :id)))
				      (mevedel-auth-refresh backend)
				      (should (equal id (plist-get (mevedel-auth-state backend) :id))))
				    (await (lambda () (equal "ready" (plist-get (mevedel-auth-state backend) :status))))
				    (should (mevedel-auth-codex-ready-p backend))
				    (should (= #o600 (file-modes gptel--openai-oauth-token-file)))
				    (should-not (plist-get (mevedel-auth-state backend) :access_token))
				    (should-not (file-locked-p (concat gptel--openai-oauth-token-file ".mevedel-auth")))
				    (delete-file gptel--openai-oauth-token-file)
				    (should-not (mevedel-auth-codex-ready-p backend)))
  :doc "rejected renewal requires login without forwarding HTTP secrets"
  (mevedel-auth-test--with-provider 401
				    (setf (gptel-openai-oauth-token backend) '(:refresh_token "old-refresh"))
				    (mevedel-auth-refresh backend)
				    (await (lambda () (equal "required" (plist-get (mevedel-auth-state backend) :status))))
				    (should-not (string-search "fixture-access" (prin1-to-string (mevedel-auth-state backend))))))

(mevedel-deftest mevedel-auth-start (:quiet t)
  ,test
  (test)
  :doc "Codex device authorization completes through the token exchange"
  (mevedel-auth-test--with-provider 200
				    (mevedel-auth-start backend)
				    (await (lambda () (equal "ready" (plist-get (mevedel-auth-state backend) :status))))
				    (should (mevedel-auth-codex-ready-p backend))
				    (should-not (plist-get (mevedel-auth-state backend) :code))
				    (mevedel-auth-cancel backend)
				    (should-not (mevedel-auth-state backend)))
  :doc "Claude verifies subscription after the fixed full-code login process"
  (mevedel-auth-test--with-provider 200
				    (let ((backend (mevedel-claude-code-register))
					  (mevedel-claude-code-executable (file-name-concat directory "claude")))
				      (copy-file (file-name-concat mevedel-auth-test--fixtures "claude-status.py") mevedel-claude-code-executable)
				      (set-file-modes mevedel-claude-code-executable #o700)
				      (mevedel-auth-start backend)
				      (await (lambda () (plist-get (mevedel-auth-state backend) :url)))
				      (should-error (mevedel-auth-submit-code backend "stale" "fixture-code#fixture-state") :type 'user-error)
				      (mevedel-auth-submit-code backend (plist-get (mevedel-auth-state backend) :id) "fixture-code#fixture-state")
				      (await (lambda () (equal "ready" (plist-get (mevedel-auth-state backend) :status))))
				      (should-not (plist-get (mevedel-auth-state backend) :url)))))

(mevedel-deftest mevedel-auth-cancel (:quiet t)
  (mevedel-auth-test--with-provider 200
				    (mevedel-auth-start backend)
				    (mevedel-auth-cancel backend)
				    (accept-process-output nil 0.05)
				    (should-not (mevedel-auth-state backend))
				    (should-not (file-exists-p gptel--openai-oauth-token-file))))

(mevedel-deftest mevedel-auth--dispatch (:quiet t)
  (mevedel-auth-test--with-provider 200
				    (with-temp-buffer
				      (setq-local mevedel-auth--managed t)
				      (gptel-oauth--write-token gptel--openai-oauth-token-file
								'(:access_token "expired" :refresh_token "old-refresh" :expires_at 0))
				      (let* (sent failed
						  (fsm (gptel-make-fsm :state 'WAIT
								       :info (list :buffer (current-buffer) :backend backend)
								       :handlers (list (list 'ERRS (lambda (_) (setq failed t)))))))
					(mevedel-auth--dispatch (lambda (_) (setq sent t)) fsm)
					(should-not sent)
					(should failed)
					(should (eq 'ERRS (gptel-fsm-state fsm)))
					(await (lambda () (equal "ready" (plist-get (mevedel-auth-state backend) :status))))
					;; Renewal does not replay the failed sample.
					(should-not sent)))))

(mevedel-deftest mevedel-auth-codex-ready-p (:quiet t)
  (mevedel-auth-test--with-provider 200
    (dolist (content '("\"not a token\"" "(:access_token 7 :expires_at \"bad\")" "(:access_token"))
      (with-temp-file gptel--openai-oauth-token-file (insert content))
      (should-not (mevedel-auth-codex-ready-p backend)))
    (delete-file gptel--openai-oauth-token-file)
    (make-directory gptel--openai-oauth-token-file)
    (should-not (mevedel-auth-codex-ready-p backend))))

(mevedel-deftest mevedel-auth--http ()
  ,test
  (test)
  :doc "a missing response buffer fails immediately and retires its timeout"
  (let (result timer)
    (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) nil))
	      ((symbol-function 'mevedel-transport-run-at-time)
	       (lambda (seconds function &rest args)
		 (setq timer (apply #'run-at-time seconds nil function args)))))
      (unwind-protect
	  (progn
	    (funcall (mevedel-auth--http "https://example.invalid" nil nil
					 (lambda (payload code) (setq result (list payload code)))))
	    (should (equal result '(nil 0)))
	    (should-not (memq timer timer-list)))
	(when timer (cancel-timer timer)))))
  :doc "a late cancelled HTTP callback cleans its response without publishing"
  (let (callback cancel results response)
    (unwind-protect
	(cl-letf (((symbol-function 'url-retrieve)
		   (lambda (_url complete &rest _)
		     (setq callback complete response (generate-new-buffer " *auth-http*")))))
	  (setq cancel (mevedel-auth--http "https://example.invalid" nil nil
					   (lambda (&rest result) (push result results))))
	  (funcall cancel)
	  (should-not (buffer-live-p response))
	  (setq response (generate-new-buffer " *auth-http-late*"))
	  (with-current-buffer response (funcall callback nil))
	  (should-not (buffer-live-p response))
	  (should-not results))
      (when cancel (funcall cancel))
      (when (buffer-live-p response) (kill-buffer response)))))

(mevedel-deftest mevedel-auth--lock (:quiet t)
  ,test
  (test)
  :doc "credential path errors terminate login so the owner can retry"
  (let* ((directory (make-temp-file "auth-lock-" t))
         (gptel--openai-oauth-token-file (file-name-concat directory "blocked" "token"))
         (gptel--known-backends nil)
         (backend (gptel-make-openai-oauth "Lock test"))
         (mevedel-auth--operations (make-hash-table :test #'equal))
         (mevedel-auth-changed-hook nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat directory "blocked") (insert "Not a directory"))
          (should-error (mevedel-auth-start backend) :type 'user-error)
          (should (equal "failed" (plist-get (mevedel-auth-state backend) :status)))
          (should-error (mevedel-auth-start backend) :type 'user-error))
      (mevedel-auth-stop-all)
      (delete-directory directory t)))
  :doc "credential renewal owns its lock even with editor file locks disabled"
  (mevedel-auth-test--with-provider 200
                                    (let ((create-lockfiles nil))
                                      (mevedel-auth-start backend)
                                      (should (eq t (file-locked-p (concat gptel--openai-oauth-token-file ".mevedel-auth"))))
                                      (mevedel-auth-cancel backend)
                                      (should-not (file-locked-p (concat gptel--openai-oauth-token-file ".mevedel-auth"))))))

(provide 'test-mevedel-auth)
;;; test-mevedel-auth.el ends here
