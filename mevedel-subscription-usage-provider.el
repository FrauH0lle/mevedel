;;; mevedel-subscription-usage-provider.el --- Subscription inspection -*- lexical-binding: t -*-

;;; Commentary:
;; Fetch display-ready account quotas without entering the conversation engine.
;; Codex uses gptel OAuth and HTTP; Claude uses an isolated advertised ACP command.
;; Callbacks receive (TEXT ERROR), exactly once unless cancelled.  Cancellation
;; owns transport cleanup; callers own presentation and freshness.

;;; Code:

(require 'gptel-openai-oauth)
(require 'mevedel-claude-code)
(require 'mevedel-acp-text)
(require 'url-http)
(require 'json)

;; `url-http'
(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defun mevedel-subscription-usage-provider--value (value)
  "Format a JSON scalar VALUE, keeping missing values distinct from zero."
  (cond ((null value) "Unavailable")
        ((eq value :false) "No")
        ((eq value t) "Yes")
        ((or (stringp value) (numberp value)) (format "%s" value))
        (t (error "Invalid quota value"))))

(defun mevedel-subscription-usage-provider--window (label window)
  "Format Codex WINDOW with LABEL, a usage bar, duration and reset time."
  (let ((used (alist-get 'used_percent window))
        (seconds (alist-get 'limit_window_seconds window))
        (reset (alist-get 'reset_at window)))
    (dolist (value (list used seconds reset))
      (unless (or (null value) (and (numberp value) (>= value 0)))
        (error "Invalid quota window")))
    (format "%s: %s used; window: %s; resets: %s\n%s"
            label
            (if used (format "%s%%" used) "Unavailable")
            (if seconds
                (cond ((zerop (% seconds 86400)) (format "%s days" (/ seconds 86400)))
                      ((zerop (% seconds 3600)) (format "%s hours" (/ seconds 3600)))
                      ((zerop (% seconds 60)) (format "%s minutes" (/ seconds 60)))
                      (t (format "%s seconds" seconds)))
              "Unavailable")
            (if reset (format-time-string "%Y-%m-%d %H:%M:%S %Z" (seconds-to-time reset))
              "Unavailable")
            (if used
                (let ((filled (if (zerop used) 0
                                (min 20 (max 1 (round (/ used 5.0)))))))
                  (concat "\n" (make-string filled ?\u2588)
                          (make-string (- 20 filled) ?\u2591) "\n\n"))
              ""))))

(defun mevedel-subscription-usage-provider--codex-text (payload account)
  "Normalize the Codex HTTP PAYLOAD and selected ACCOUNT for display."
  (unless (and (listp payload)
               (seq-some (lambda (key) (assq key payload))
                         '(plan_type rate_limit additional_rate_limits credits)))
    (error "Unrecognized Codex usage response"))
  (let ((buckets (cons (cons "Codex" (alist-get 'rate_limit payload))
                       (mapcar
                        (lambda (bucket)
                          (cons (or (alist-get 'limit_name bucket)
                                    (alist-get 'metered_feature bucket)
                                    "Additional quota")
                                (alist-get 'rate_limit bucket)))
                        (alist-get 'additional_rate_limits payload))))
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

(defun mevedel-subscription-usage-provider--codex (backend callback)
  "Fetch BACKEND's Codex quotas, delivering CALLBACK; return a canceller.
Keep gptel's private authentication API here, including synchronous renewal.
Neither authentication errors nor HTTP bodies are included in diagnostics."
  (let (response timer done headers)
    (cl-labels
        ((cleanup ()
           (when timer (cancel-timer timer) (setq timer nil))
           (when (buffer-live-p response)
             (when-let* ((process (get-buffer-process response)))
               (delete-process process))
             (kill-buffer response)))
         (finish (text error)
           (unless done
             (setq done t)
             (cleanup)
             (funcall callback text error)))
         (cancel () (setq done t) (cleanup)))
      (condition-case nil
          (let ((gptel-backend backend))
            (setq headers (gptel--openai-oauth-header nil)))
        (error (finish nil "Codex authentication failed. Run M-x gptel-openai-oauth-login and refresh.")))
      (unless done
        (condition-case nil
            (let ((url-request-method "GET")
                  (url-request-extra-headers headers)
                  (url-request-data nil)
                  (url-show-status nil)
                  (url-max-redirections 0))
              (setq timer (run-at-time 30 nil (lambda () (finish nil "Codex usage request timed out; refresh to retry."))))
              (setq response
                    (url-retrieve
                     "https://chatgpt.com/backend-api/wham/usage"
                     (lambda (status)
                       (setq response (current-buffer))
                       (unless done
                         (let ((code url-http-response-status))
                           (cond
                            ((memq code '(401 403))
                             (finish nil "Codex usage access denied. Run M-x gptel-openai-oauth-login for the selected account and refresh."))
                            ((or (plist-get status :error) (not (eql code 200)))
                             (finish nil (format "Codex usage HTTP request failed (status %s); refresh to retry."
                                                 (if (integerp code) code "unavailable"))))
                            (t
                             (let ((text
                                    (condition-case nil
                                        (progn
                                          (goto-char url-http-end-of-headers)
                                          (mevedel-subscription-usage-provider--codex-text
                                           (json-parse-buffer :object-type 'alist :array-type 'list
                                                              :null-object nil :false-object :false)
                                           (cdr (assoc "ChatGPT-Account-Id" headers))))
                                      (error nil))))
                               (if text (finish text nil)
                                 (finish nil "Unrecognized Codex usage response; check gptel/provider support and retry.")))))))
                       (cleanup))
                     nil t t))
              (if done (cleanup)
                (unless (buffer-live-p response)
                  (finish nil "Codex usage connection could not be opened; refresh to retry."))))
          (error (finish nil "Codex usage connection failed; check connectivity and refresh."))
          (quit (cancel) (signal 'quit nil))))
      #'cancel)))

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

(defun mevedel-subscription-usage-provider--claude (callback)
  "Inspect native Claude subscription usage through CALLBACK; return a canceller."
  (let ((launch (mevedel-claude-code-usage-launch (gptel--model-name gptel-model))))
    (mevedel-acp-text-request
     launch "/usage"
     (lambda (response info)
       (unless (eq response 'abort)
         (if (not (stringp response))
             (funcall callback nil (or (plist-get info :error) "Claude usage inspection failed"))
           (let (text failure)
             (condition-case err
                 (setq text (mevedel-subscription-usage-provider--claude-text response))
               (error (setq failure (error-message-string err))))
             (funcall callback text failure))))))))

(defun mevedel-subscription-usage-provider-fetch (backend callback)
  "Retrieve BACKEND subscription information and return a canceller.
CALLBACK receives display-ready text and nil, or nil and an actionable error.
The caller binds its selected `gptel-model'.  No conversation is admitted."
  (condition-case err
      (cond ((gptel-openai-oauth-p backend)
             (mevedel-subscription-usage-provider--codex backend callback))
            ((mevedel-claude-code-backend-p backend)
             (mevedel-subscription-usage-provider--claude callback))
            (t (funcall callback nil "Subscription usage is available for Codex OAuth and Claude Code backends. Select one in the session and refresh.")
               #'ignore))
    (error (funcall callback nil (error-message-string err)) #'ignore)))

(provide 'mevedel-subscription-usage-provider)
;;; mevedel-subscription-usage-provider.el ends here
