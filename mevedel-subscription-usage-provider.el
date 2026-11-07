;;; mevedel-subscription-usage-provider.el --- Subscription inspection -*- lexical-binding: t -*-

;;; Commentary:
;; Fetch display-ready account quotas without entering the conversation engine.
;; Codex uses gptel OAuth and HTTP; Claude uses an isolated advertised ACP command.
;; `-fetch' yields display text and `-readings' yields plain quota windows; both
;; share one transport and validation.  Callbacks receive (RESULT ERROR), exactly
;; once unless cancelled.  Cancellation owns transport cleanup; callers own
;; presentation and freshness.

;;; Code:

(require 'gptel-openai-oauth)
(require 'mevedel-auth)
(require 'mevedel-claude-code)
(require 'mevedel-acp-text)
(require 'json)

(defun mevedel-subscription-usage-provider--value (value)
  "Format a JSON scalar VALUE, keeping missing values distinct from zero."
  (cond ((null value) "Unavailable")
        ((eq value :false) "No")
        ((eq value t) "Yes")
        ((or (stringp value) (numberp value)) (format "%s" value))
        (t (error "Invalid quota value"))))

(defun mevedel-subscription-usage-provider--duration (seconds)
  "Format a window length of SECONDS in its largest whole unit."
  (setq seconds (round seconds))
  (cond ((zerop (% seconds 86400)) (format "%s days" (/ seconds 86400)))
        ((zerop (% seconds 3600)) (format "%s hours" (/ seconds 3600)))
        ((zerop (% seconds 60)) (format "%s minutes" (/ seconds 60)))
        (t (format "%s seconds" seconds))))

(defun mevedel-subscription-usage-provider--window-fields (window)
  "Return validated (USED SECONDS RESET) of Codex WINDOW; nil means missing."
  (let ((used (alist-get 'used_percent window))
        (seconds (alist-get 'limit_window_seconds window))
        (reset (alist-get 'reset_at window)))
    (dolist (value (list used seconds reset))
      (unless (or (null value) (and (numberp value) (>= value 0)))
        (error "Invalid quota window")))
    (list used seconds reset)))

(defun mevedel-subscription-usage-provider--window (label window)
  "Format Codex WINDOW with LABEL, a usage bar, duration and reset time."
  (pcase-let ((`(,used ,seconds ,reset)
               (mevedel-subscription-usage-provider--window-fields window)))
    (format "%s: %s used; window: %s; resets: %s\n%s"
            label
            (if used (format "%s%%" used) "Unavailable")
            (if seconds (mevedel-subscription-usage-provider--duration seconds)
              "Unavailable")
            (if reset (format-time-string "%Y-%m-%d %H:%M:%S %Z" (seconds-to-time reset))
              "Unavailable")
            (if used
                (let ((filled (if (zerop used) 0
                                (min 20 (max 1 (round (/ used 5.0)))))))
                  (concat "\n" (make-string filled ?\u2588)
                          (make-string (- 20 filled) ?\u2591) "\n\n"))
              ""))))

(defun mevedel-subscription-usage-provider--codex-buckets (payload)
  "Return the named rate-limit buckets of Codex PAYLOAD, or signal."
  (unless (and (listp payload)
               (seq-some (lambda (key) (assq key payload))
                         '(plan_type rate_limit additional_rate_limits credits)))
    (error "Unrecognized Codex usage response"))
  (cons (cons "Codex" (alist-get 'rate_limit payload))
        (mapcar
         (lambda (bucket)
           (cons (or (alist-get 'limit_name bucket)
                     (alist-get 'metered_feature bucket)
                     "Additional quota")
                 (alist-get 'rate_limit bucket)))
         (alist-get 'additional_rate_limits payload))))

(defun mevedel-subscription-usage-provider--codex-text (payload account)
  "Normalize the Codex HTTP PAYLOAD and selected ACCOUNT for display."
  (let ((buckets (mevedel-subscription-usage-provider--codex-buckets payload))
        (credits (alist-get 'credits payload)))
    (concat
     (format "Account: %s\nPlan: %s\n\n"
             (mevedel-subscription-usage-provider--value account)
             (mevedel-subscription-usage-provider--value (alist-get 'plan_type payload)))
     (mapconcat
      (lambda (bucket)
        (concat (car bucket) "\n"
                (mevedel-subscription-usage-provider--window
                 "Primary" (alist-get 'primary_window (cdr bucket)))
                (mevedel-subscription-usage-provider--window
                 "Secondary" (alist-get 'secondary_window (cdr bucket)))))
      buckets "\n")
     (format "\nCredits: %s\nCredit balance: %s\nUnlimited credits: %s\n"
             (mevedel-subscription-usage-provider--value (alist-get 'has_credits credits))
             (mevedel-subscription-usage-provider--value (alist-get 'balance credits))
             (mevedel-subscription-usage-provider--value (alist-get 'unlimited credits))))))

(defun mevedel-subscription-usage-provider--codex-readings (payload _account)
  "Return (ok . WINDOWS), the plain quota windows of Codex PAYLOAD.
The success tag keeps an empty list distinct from a failed parse.  The
account is never included."
  (let (readings)
    (pcase-dolist (`(,name . ,limit)
                   (mevedel-subscription-usage-provider--codex-buckets payload))
      (dolist (key '(primary_window secondary_window))
        (when-let* ((window (alist-get key limit)))
          (pcase-let ((`(,used ,seconds ,reset)
                       (mevedel-subscription-usage-provider--window-fields window)))
            (push (list :quota name
                        :window (and seconds (mevedel-subscription-usage-provider--duration seconds))
                        :used-percent used
                        :resets (and reset (format-time-string
                                            "%FT%T%z" (seconds-to-time reset))))
                  readings)))))
    (cons 'ok (nreverse readings))))

(defun mevedel-subscription-usage-provider--codex (backend parse callback)
  "Fetch BACKEND's Codex quotas, delivering CALLBACK; return a canceller.
PARSE receives the decoded payload and selected account.
Keep gptel's private authentication API here, renewing before HTTP dispatch.
Neither authentication errors nor HTTP bodies are included in diagnostics."
  (let (headers failure)
    (condition-case nil
        (let ((gptel-backend backend))
          (mevedel-auth-assert-ready backend)
          (setq headers (gptel--openai-oauth-header nil)))
      (error
       ;; Readiness starts an asynchronous renewal before it refuses.
       (setq failure
             (if (equal "refreshing" (plist-get (mevedel-auth-state backend) :status))
                 "Codex login is renewing; refresh shortly."
               "Codex login is required. Run M-x gptel-openai-oauth-login and refresh."))))
    (if failure
        (progn (funcall callback nil failure) #'ignore)
      (mevedel-auth--http
       "https://chatgpt.com/backend-api/wham/usage" nil nil
       (lambda (text code)
         (if (and text (eql code 200))
             (funcall callback text nil)
           (funcall callback nil
                    (cond ((memq code '(401 403))
                           "Codex usage access denied. Run M-x gptel-openai-oauth-login for the selected account and refresh.")
                          ((eql code 200)
                           "Unrecognized Codex usage response; check gptel/provider support and retry.")
                          ((memq code '(0 nil))
                           "Codex usage request failed or timed out; check connectivity and refresh.")
                          (t (format "Codex usage HTTP request failed (status %s); refresh to retry."
                                     code))))))
       headers
       (lambda ()
         (funcall
          parse
          (json-parse-buffer :object-type 'alist :array-type 'list
                             :null-object nil :false-object :false)
          (cdr (assoc "ChatGPT-Account-Id" headers))))))))

(defun mevedel-subscription-usage-provider--claude-text (text)
  "Extract only the recognized subscription header and Limits from TEXT.
Preserve native Markdown formatting.  Session totals and behavior are omitted."
  (unless (and (stringp text) (string-prefix-p "## Usage\n" text))
    (error "Unrecognized Claude usage output; update the Claude CLI and ACP adapter"))
  (let* ((header (and (string-match "^> Claude .+ subscription usage$" text)
                      (match-string 0 text)))
         (start (and (string-match "^### Limits\n" text) (match-beginning 0)))
         (end (and start (string-match "^\\(?:---\\|### \\)" text (+ start 11))))
         (limits (and start (string-trim-right (substring text start end)))))
    (when limits
      (dolist (line (cdr (split-string limits "\n")))
        (unless (or (string-empty-p line)
                    (string-match-p "\\`\\*\\*\\(?:5-hour limit\\|Weekly \u00b7 .+\\)\\*\\* \u2014 \\*\\*[0-9]+\\(?:\\.[0-9]+\\)?%\\*\\*\\(?: \u00b7 Resets .+\\)?\\'" line)
                    (string-match-p "\\``[\u2588\u2591]+`\\'" line))
          (error "Unrecognized Claude quota section; update the Claude CLI and ACP adapter"))))
    (unless (or header limits)
      (error "Claude did not return subscription quotas; check subscription login and adapter support"))
    (concat (or header "Subscription plan: Unavailable") "\n\n"
            (or limits "Subscription quotas: Unavailable")
            "\n\nAccount identity and fields omitted by the adapter (including extra-usage credits): Unavailable\n")))

(defun mevedel-subscription-usage-provider--claude-readings (text)
  "Return (ok . WINDOWS) from validated native Claude usage TEXT."
  (let ((validated (mevedel-subscription-usage-provider--claude-text text))
        (start 0) readings)
    (while (string-match "^\\*\\*\\(5-hour limit\\|Weekly \u00b7 [^*\n]+\\)\\*\\* \u2014 \\*\\*\\([0-9.]+\\)%\\*\\*\\(?: \u00b7 Resets \\(.+\\)\\)?$"
                         validated start)
      (let ((label (match-string 1 validated)))
        (push (list :quota label
                    :window (if (string-prefix-p "5-hour" label) "5 hours" "7 days")
                    :used-percent (string-to-number (match-string 2 validated))
                    :resets (match-string 3 validated))
              readings))
      (setq start (match-end 0)))
    (cons 'ok (nreverse readings))))

(defun mevedel-subscription-usage-provider--claude (parse callback)
  "Inspect native Claude usage for CALLBACK via PARSE; return a canceller."
  (let ((launch (mevedel-claude-code-usage-launch (gptel--model-name gptel-model))))
    (mevedel-acp-text-request
     launch "/usage"
     (lambda (response info)
       (unless (eq response 'abort)
         (if (not (stringp response))
             (funcall callback nil (or (plist-get info :error) "Claude usage inspection failed"))
           (let (text failure)
             (condition-case err
                 (setq text (funcall parse response))
               (error (setq failure (error-message-string err))))
             (funcall callback text failure))))))))

(defun mevedel-subscription-usage-provider--dispatch (backend codex claude callback)
  "Fetch BACKEND quotas, parsing with CODEX or CLAUDE, for CALLBACK."
  (condition-case err
      (cond ((gptel-openai-oauth-p backend)
             (mevedel-subscription-usage-provider--codex backend codex callback))
            ((mevedel-claude-code-backend-p backend)
             (mevedel-subscription-usage-provider--claude claude callback))
            (t (funcall callback nil "Subscription usage is available for Codex OAuth and Claude Code backends. Select one in the session and refresh.")
               #'ignore))
    (error (funcall callback nil (error-message-string err)) #'ignore)))

(defun mevedel-subscription-usage-provider-fetch (backend callback)
  "Retrieve BACKEND subscription information and return a canceller.
CALLBACK receives display-ready text and nil, or nil and an actionable error.
The caller binds its selected `gptel-model'.  No conversation is admitted."
  (mevedel-subscription-usage-provider--dispatch
   backend #'mevedel-subscription-usage-provider--codex-text
   #'mevedel-subscription-usage-provider--claude-text callback))

(defun mevedel-subscription-usage-provider-readings (backend callback)
  "Retrieve BACKEND quota windows as plain data and return a canceller.
CALLBACK receives a list of plists and nil, or nil and an actionable error.
Each plist has :quota, :window, :used-percent and :resets; values may be nil.
Readings carry no account, plan, credit or credential field.  The caller
binds its selected `gptel-model'."
  (mevedel-subscription-usage-provider--dispatch
   backend #'mevedel-subscription-usage-provider--codex-readings
   #'mevedel-subscription-usage-provider--claude-readings
   (lambda (result error) (funcall callback (cdr result) error))))

(provide 'mevedel-subscription-usage-provider)
;;; mevedel-subscription-usage-provider.el ends here
