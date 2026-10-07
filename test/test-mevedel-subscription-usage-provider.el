;;; test-mevedel-subscription-usage-provider.el --- Quota retrieval tests -*- lexical-binding: t -*-

;;; Commentary:
;; Verify HTTP and ACP boundaries without reaching a configured account.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-subscription-usage-provider)

(defconst mevedel-usage-test--native
  "## Usage\n\n> Claude max subscription usage\n\n### Limits\n\n**5-hour limit** — **12%** · Resets Oct 7, 8:00 PM UTC\n\n`██░░░░░░░░░░░░░░░░░░`\n\n**Weekly · all models** — **0%**\n\n`░░░░░░░░░░░░░░░░░░░░`\n\n---\n\n### This session\n\nCost: $0\n\n---\n\n### What’s using your limits?\n\nprivate behavior")

(defconst mevedel-usage-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "fixtures" "acp-agent.py"))

(mevedel-deftest mevedel-subscription-usage-provider--value ()
  (should (equal ,expected (mevedel-subscription-usage-provider--value ,value)))
  (value expected)
  :doc "missing is unavailable" nil "Unavailable"
  :doc "zero remains zero" 0 "0"
  :doc "false remains no" :false "No"
  :doc "true remains yes" t "Yes")

(mevedel-deftest mevedel-subscription-usage-provider--window ()
  ,test
  (test)
  :doc "duration and reset are taken from the provider, not assumed"
  (let ((text (mevedel-subscription-usage-provider--window
               "Primary" '((used_percent . 0) (limit_window_seconds . 900) (reset_at . 1800000000)))))
    (should (string-match-p "0% used; window: 15 minutes" text))
    (should (string-match-p (regexp-quote (format-time-string "%Y-%m-%d %H:%M:%S %Z" (seconds-to-time 1800000000))) text)))
  :doc "null fields never become zero"
  (should (equal "Primary: Unavailable used; window: Unavailable; resets: Unavailable\n"
                 (mevedel-subscription-usage-provider--window "Primary" nil)))
  :doc "malformed values are rejected"
  (should-error (mevedel-subscription-usage-provider--window "Primary" '((used_percent . "secret")))))

(mevedel-deftest mevedel-subscription-usage-provider--codex-text ()
  ,test
  (test)
  :doc "native HTTP schema includes primary, secondary, extra buckets, plan and credits"
  (let ((text
         (mevedel-subscription-usage-provider--codex-text
          (json-parse-string
           "{\"plan_type\":\"plus\",\"rate_limit\":{\"primary_window\":{\"used_percent\":0,\"limit_window_seconds\":18000},\"secondary_window\":{\"used_percent\":30,\"limit_window_seconds\":604800}},\"additional_rate_limits\":[{\"limit_name\":\"Spark\",\"rate_limit\":{\"primary_window\":{\"used_percent\":18,\"limit_window_seconds\":300}}}],\"credits\":{\"has_credits\":true,\"balance\":\"20.5\",\"unlimited\":false}}"
           :object-type 'alist :array-type 'list :null-object nil :false-object :false)
          "selected-account")))
    (dolist (part '("Account: selected-account" "Plan: plus" "5 hours" "7 days"
                    "Spark" "5 minutes" "Credits: Yes" "Credit balance: 20.5" "Unlimited credits: No"))
      (should (string-match-p (regexp-quote part) text))))
  :doc "absent and explicit nulls remain unavailable"
  (let ((text (mevedel-subscription-usage-provider--codex-text
               '((plan_type) (rate_limit) (credits) (additional_rate_limits)) nil)))
    (should (string-match-p "Plan: Unavailable" text))
    (should-not (string-match-p "0%" text)))
  :doc "unknown or malformed responses fail closed"
  (dolist (payload '(nil ((error . "secret")) ((rate_limit . "bad")) ((additional_rate_limits . 42))))
    (should-error (mevedel-subscription-usage-provider--codex-text payload nil))))

(mevedel-deftest mevedel-subscription-usage-provider--claude-text ()
  ,test
  (test)
  :doc "preserves native limits but excludes session and behavior totals"
  (let ((text (mevedel-subscription-usage-provider--claude-text mevedel-usage-test--native)))
    (should (string-match-p "Claude max subscription usage" text))
    (should (string-match-p (regexp-quote "**5-hour limit** — **12%**") text))
    (should-not (string-match-p "This session\\|private behavior\\|Cost:" text)))
  :doc "adapter omissions are explicitly unavailable"
  (should (string-match-p "Subscription quotas: Unavailable"
                          (mevedel-subscription-usage-provider--claude-text
                           "## Usage\n\n> Claude pro subscription usage\n\n---\n\n### This session\nzero")))
  :doc "arbitrary fallback and malformed quota sections are rejected"
  (dolist (text '("You have plenty of quota" "## Usage\n\n### This session\n$0"
                  "## Usage\n\n### Limits\n\nmade up"))
    (should-error (mevedel-subscription-usage-provider--claude-text text))))

(mevedel-deftest mevedel-subscription-usage-provider-fetch ()
  ,test
  (test)
  :doc "concrete backend type determines provider, not its name"
  (let ((backend (gptel--make-openai :name "Codex")) result)
    (funcall (mevedel-subscription-usage-provider-fetch backend
              (lambda (text error) (setq result (list text error)))))
    (should-not (car result))
    (should (string-match-p "Select one" (cadr result))))
  :doc "authentication failures are actionable and never echo credentials"
  (let (result)
    (cl-letf (((symbol-function 'gptel--openai-oauth-header)
               (lambda (_) (error "Bearer SECRET"))))
      (funcall (mevedel-subscription-usage-provider-fetch
                (gptel--make-openai-oauth :name "Other name")
                (lambda (text error) (setq result (list text error))))))
    (should-not (car result))
    (should (string-match-p "gptel-openai-oauth-login" (cadr result)))
    (should-not (string-match-p "SECRET" (cadr result)))))

(mevedel-deftest mevedel-subscription-usage-provider--codex (:quiet t)
  (let* ((original-retrieve (symbol-function 'url-retrieve))
         (original-timer (symbol-function 'run-at-time))
         (backend (gptel--make-openai-oauth :name "Selected"))
         (server (make-network-process
                  :name "usage-http-test" :server t :host "127.0.0.1" :service t :noquery t
                  :filter
                  (lambda (process text)
                    (process-put process 'request (concat (process-get process 'request) text))
                    (when (and ,status (string-match-p "\r\n\r\n" (process-get process 'request)))
                      (process-send-string process
                                           (format "HTTP/1.1 %s Test\r\nConnection: close\r\nContent-Length: %s\r\n\r\n%s"
                                                   ,status (string-bytes ,body) ,body))))))
         (url (format "http://127.0.0.1:%s/usage" (process-contact server :service)))
         timer response cancel results)
    (unwind-protect
        (cl-letf (((symbol-function 'gptel--openai-oauth-header)
                   (lambda (_) (should (eq backend gptel-backend))
                     '(("Authorization" . "Bearer test") ("ChatGPT-Account-Id" . "chosen"))))
                  ((symbol-function 'url-retrieve)
                   (lambda (requested callback &rest args)
                     (should (equal requested "https://chatgpt.com/backend-api/wham/usage"))
                     (should (equal "Bearer test" (cdr (assoc "Authorization" url-request-extra-headers))))
                     (setq response (apply original-retrieve url callback args))))
                  ((symbol-function 'run-at-time)
                   (lambda (seconds repeat function &rest args)
                     (if (eql seconds 30)
                         (setq timer (apply original-timer 0.2 repeat function args))
                       (apply original-timer seconds repeat function args)))))
          (setq cancel (mevedel-subscription-usage-provider-fetch backend
                         (lambda (text error) (push (list text error) results))))
          (if ,cancel (funcall cancel)
            (with-timeout (5 (ert-fail "HTTP quota request did not settle"))
              (while (not results) (accept-process-output nil 0.01))))
          (if ,cancel (should-not results)
            (should (= 1 (length results)))
            (should (string-match-p ,expected (or (caar results) (cadar results)))))
          (funcall cancel)
          (should-not (buffer-live-p response))
          (should-not (memq timer timer-list)))
      (when cancel (funcall cancel))
      (dolist (process (process-list))
        (when (or (eq process server) (eq (process-contact process :server) server))
          (delete-process process)))))
  (status body expected cancel)
  :doc "real HTTP success cleans response and timeout"
  200 "{\"plan_type\":\"plus\",\"rate_limit\":null}" "Account: chosen" nil
  :doc "HTTP authentication error never exposes response body"
  401 "SECRET" "access denied" nil
  :doc "HTTP service failure remains actionable"
  503 "SECRET" "status 503" nil
  :doc "malformed HTTP body is rejected"
  200 "SECRET" "Unrecognized" nil
  :doc "bounded HTTP request expires and cleans resources"
  nil "" "timed out" nil
  :doc "cancellation cleans resources without a terminal callback"
  nil "" "" t)

(mevedel-deftest mevedel-subscription-usage-provider--claude (:quiet t)
  (with-temp-buffer
    (let* ((directory (make-temp-file "usage-acp-" t))
           (log (file-name-concat directory "prompts.json"))
           (gptel-model 'sonnet)
           (original-open (symbol-function 'mevedel-acp-open))
           (mevedel-acp--control-timeout 0.3)
           (original-timer (symbol-function 'run-at-time))
           connection timer response-timer cancel results)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (lambda (_system mcp _model _effort &rest _)
                       (should (equal [] mcp))
                       (list :command (executable-find "python3") :args (list mevedel-usage-test--peer)
                             :cwd directory :mcp mcp
                             :meta `((inspection . ((advertise . ,,advertise) (missing . ,,missing)
                                                   (wait . ,,wait) (promptLog . ,log)))
                                     (responseText . ,,output)
                                     (claudeCode . ((options . ((extraArgs . ((disable-slash-commands . "")))))))))))
                    ((symbol-function 'mevedel-acp-open)
                     (lambda (launch ready failure)
                       (should (equal "usage" (plist-get launch :required-command)))
                       (should-not (alist-get 'extraArgs (alist-get 'options (alist-get 'claudeCode (plist-get launch :meta)))))
                       (setq connection (funcall original-open launch ready failure)
                             timer (mevedel-acp-timer connection))
                       connection))
                    ((symbol-function 'run-at-time)
                     (lambda (seconds repeat function &rest args)
                       (if (eql seconds 30)
                           (setq response-timer (apply original-timer 0.15 repeat function args))
                         (apply original-timer seconds repeat function args)))))
            (setq cancel (mevedel-subscription-usage-provider-fetch
                          (mevedel-claude-code--make-backend :name "Selected Claude")
                          (lambda (text error) (push (list text error) results))))
            (if ,cancel
                (progn (funcall cancel) (should-not results))
              (with-timeout (5 (ert-fail "ACP inspection did not settle"))
                (while (not results) (accept-process-output nil 0.01)))
              (should (= 1 (length results)))
              (should (string-match-p ,expected (or (caar results) (cadar results)))))
            (if ,prompt
                (should (file-exists-p log))
              (should-not (file-exists-p log)))
            (funcall cancel)
            (should (eq 'closed (mevedel-acp-state connection)))
            (should-not (process-live-p (alist-get :process (mevedel-acp-client connection))))
            (should-not (memq timer timer-list))
            (should-not (mevedel-acp-timer connection))
            (should-not (memq response-timer timer-list)))
        (when cancel (funcall cancel))
        (delete-directory directory t))))
  (advertise missing wait output expected prompt cancel)
  :doc "pre-session command advertisements permit only exact native usage"
  "before" :false :false mevedel-usage-test--native "Claude max" t nil
  :doc "post-session command advertisements also gate startup"
  "after" :false :false mevedel-usage-test--native "Claude max" t nil
  :doc "missing usage never submits a prompt"
  "before" t :false "arbitrary" "does not advertise /usage" nil nil
  :doc "advertisement timeout never submits a prompt"
  "never" :false :false "arbitrary" "startup timed out" nil nil
  :doc "unrecognized native output is an error"
  "after" :false :false "arbitrary" "Unrecognized Claude" t nil
  :doc "inspection response timeout closes the subprocess"
  "before" :false t "arbitrary" "response timed out" t nil
  :doc "startup cancellation closes the subprocess without prompting"
  "never" :false :false "arbitrary" "" nil t)

(provide 'test-mevedel-subscription-usage-provider)
;;; test-mevedel-subscription-usage-provider.el ends here
