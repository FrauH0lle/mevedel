;;; test-mevedel-tool-usage.el --- SubscriptionUsage tool tests -*- lexical-binding: t -*-

;;; Commentary:
;; Call SubscriptionUsage through its registered tool function.  Transports are
;; replaced at the HTTP and ACP boundaries; the provider's parsers stay real.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-tool-registry)
(require 'mevedel-auth)
(require 'mevedel-acp-text)
(require 'mevedel-claude-code)
(require 'mevedel-pipeline)
(require 'mevedel-subscription-usage-provider)

(defconst mevedel-tool-usage-test--codex
  "{\"plan_type\":\"plus\",\"email\":\"SECRET-EMAIL\",\"user_id\":\"SECRET-USER\",\"rate_limit\":{\"primary_window\":{\"used_percent\":12,\"limit_window_seconds\":18000,\"reset_at\":1800000000},\"secondary_window\":{\"used_percent\":30,\"limit_window_seconds\":604800}},\"credits\":{\"has_credits\":true,\"balance\":\"SECRET-BALANCE\",\"unlimited\":false}}"
  "Codex payload with account-bearing fields the tool must drop.")

(defconst mevedel-tool-usage-test--claude
  "## Usage\n\n> Claude max subscription usage\n\n### Limits\n\n**5-hour limit** \u2014 **12%** \u00b7 Resets Oct 7, 8:00 PM UTC\n\n`\u2588\u2588\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591`\n\n**Weekly \u00b7 all models** \u2014 **40.5%**\n\n`\u2588\u2588\u2588\u2588\u2588\u2588\u2588\u2588\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591`\n\n---\n\n### This session\n\nCost: $0\n")

(defvar mevedel-tool-usage-test--codex-body nil
  "Codex HTTP body to answer with; nil means `mevedel-tool-usage-test--codex'.")

(defvar mevedel-tool-usage-test--claude-response nil
  "Claude ACP (RESPONSE . INFO); nil means the native fixture.")

(defvar mevedel-tool-usage-test--auth-error nil
  "Non-nil makes Codex readiness signal this message.")

(defvar mevedel-tool-usage-test--cancelled nil
  "Transports cancelled during the current test, most recent first.")

(defun mevedel-tool-usage-test--with-transports (deferred fn)
  "Call FN with HTTP and ACP transports replaced.
With DEFERRED, transports answer from a timer that their canceller cancels."
  (let ((deliver
         (lambda (name answer)
           (let ((timer (and deferred (run-at-time 0 nil answer))))
             (unless deferred (funcall answer))
             (lambda ()
               (push name mevedel-tool-usage-test--cancelled)
               (when timer (cancel-timer timer)))))))
    (cl-letf (((symbol-function 'mevedel-auth-assert-ready)
               (lambda (_)
                 (when mevedel-tool-usage-test--auth-error
                   (user-error "%s" mevedel-tool-usage-test--auth-error))))
              ((symbol-function 'gptel--openai-oauth-header)
               (lambda (_) '(("Authorization" . "Bearer SECRET-TOKEN")
                             ("ChatGPT-Account-Id" . "SECRET-ACCOUNT"))))
              ((symbol-function 'mevedel-auth--http)
               (lambda (_url _data _form callback headers parse)
                 (should (equal "Bearer SECRET-TOKEN" (cdr (assoc "Authorization" headers))))
                 (funcall deliver 'codex
                          (lambda ()
                            (with-temp-buffer
                              (insert (or mevedel-tool-usage-test--codex-body
                                          mevedel-tool-usage-test--codex))
                              (goto-char (point-min))
                              (funcall callback (funcall parse) 200))))))
              ((symbol-function 'mevedel-claude-code-usage-launch) (lambda (_) nil))
              ((symbol-function 'mevedel-acp-text-request)
               (lambda (_launch prompt callback)
                 (should (equal "/usage" prompt))
                 (funcall deliver 'claude
                          (lambda ()
                            (let ((answer (or mevedel-tool-usage-test--claude-response
                                              (cons mevedel-tool-usage-test--claude nil))))
                              (funcall callback (car answer) (cdr answer))))))))
      (funcall fn))))

(defun mevedel-tool-usage-test--call (backends current &optional deferred)
  "Call SubscriptionUsage with BACKENDS known and CURRENT selected.
With DEFERRED, transports answer from a timer.  Return (IMMEDIATE . RESULT)."
  (let ((tool (mevedel-tool-gptel-tool (mevedel-tool-ensure "SubscriptionUsage")))
        (gptel--known-backends (mapcar (lambda (b) (cons (gptel-backend-name b) b)) backends))
        result immediate)
    (mevedel-tool-usage-test--with-transports
     deferred
     (lambda ()
       (with-temp-buffer
         (setq-local gptel-backend current gptel-model 'test-model)
         (funcall (gptel-tool-function tool) (lambda (value) (setq result value)))
         (setq immediate result)
         (when deferred
           (mevedel-test--await 5 "SubscriptionUsage did not answer" result)))))
    (cons immediate result)))

(defun mevedel-tool-usage-test--entries (text)
  "Parse SubscriptionUsage TEXT into alists, with null and false as nil."
  (json-parse-string text :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(mevedel-deftest mevedel-tool-usage--handle
    (:vars ((codex (gptel--make-openai-oauth :name "Codex OAuth"))
            (claude (mevedel-claude-code--make-backend :name "Claude Code"))
            (mevedel-tool-usage-test--codex-body nil)
            (mevedel-tool-usage-test--claude-response nil)
            (mevedel-tool-usage-test--cancelled nil)))
  ,test
  (test)
  :doc "current and other subscription backends yield plain quota windows"
  (let* ((other (gptel--make-openai :name "API key"))
         (call (mevedel-tool-usage-test--call (list other codex claude) claude t))
         (entries (mevedel-tool-usage-test--entries (cdr call))))
    (should-not (car call))
    (should (equal '("Claude Code" "Codex OAuth")
                   (mapcar (lambda (entry) (alist-get 'backend entry)) entries)))
    (should (equal '(t nil) (mapcar (lambda (entry) (alist-get 'current entry)) entries)))
    (should (equal '(((quota . "5-hour limit") (window . "5 hours")
                      (used_percent . 12) (resets . "Oct 7, 8:00 PM UTC"))
                     ((quota . "Weekly \u00b7 all models") (window . "7 days")
                      (used_percent . 40.5) (resets)))
                   (alist-get 'readings (car entries))))
    (should (equal `(((quota . "Codex") (window . "5 hours") (used_percent . 12)
                      (resets . ,(format-time-string "%FT%T%z" (seconds-to-time 1800000000))))
                     ((quota . "Codex") (window . "7 days") (used_percent . 30) (resets)))
                   (alist-get 'readings (cadr entries))))
    (should-not mevedel-tool-usage-test--cancelled))
  :doc "readings carry only allowlisted fields and no account, plan, session or credential"
  (let ((text (cdr (mevedel-tool-usage-test--call (list codex claude) codex))))
    (should-not (string-match-p "SECRET\\|plus\\|max\\|Cost\\|session\\|credit\\|account\\|Bearer" text))
    (let ((entries (json-parse-string text :object-type 'alist :array-type 'list)))
      (should (= 2 (length entries)))
      (dolist (entry entries)
        (should (equal '(backend current readings) (mapcar #'car entry)))
        (should (alist-get 'readings entry))
        (dolist (reading (alist-get 'readings entry))
          (should (equal '(quota window used_percent resets) (mapcar #'car reading)))))))
  :doc "a recognized response without windows is an empty success"
  (let* ((mevedel-tool-usage-test--codex-body "{\"plan_type\":\"plus\",\"rate_limit\":{}}")
         (mevedel-tool-usage-test--claude-response
          (cons "## Usage\n\n> Claude pro subscription usage\n\n---\n\n### This session\nzero" nil))
         (entries (mevedel-tool-usage-test--entries
                   (cdr (mevedel-tool-usage-test--call (list codex claude) codex)))))
    (should (equal '(((backend . "Codex OAuth") (current . t) (readings))
                     ((backend . "Claude Code") (current) (readings)))
                   entries)))
  :doc "provider failures are reported per backend"
  (let* ((mevedel-tool-usage-test--claude-response
          (cons nil '(:error "Claude usage response timed out")))
         (mevedel-tool-usage-test--auth-error "Bearer SECRET-TOKEN expired")
         (text (cdr (mevedel-tool-usage-test--call (list codex claude) codex))))
    (let ((entries (mevedel-tool-usage-test--entries text)))
      (should (string-match-p "gptel-openai-oauth-login" (alist-get 'error (car entries))))
      (should (equal "Claude usage response timed out" (alist-get 'error (cadr entries))))
      (dolist (entry entries)
        (should (equal '(backend current error) (mapcar #'car entry)))))
    (should-not (string-match-p "SECRET" text)))
  :doc "unrecognized Claude output is an error, not readings"
  (let* ((mevedel-tool-usage-test--claude-response (cons "You have plenty of quota" nil))
         (entry (car (mevedel-tool-usage-test--entries
                      (cdr (mevedel-tool-usage-test--call (list claude) claude))))))
    (should (string-match-p "Unrecognized Claude usage output" (alist-get 'error entry)))
    (should-not (assq 'readings entry)))
  :doc "without subscription backends the tool says so"
  (let ((other (gptel--make-openai :name "API key")))
    (should (string-match-p "No Codex OAuth or Claude Code backend"
                            (cdr (mevedel-tool-usage-test--call (list other) other))))))

(mevedel-deftest mevedel-tool-usage--handle/cancel
    (:vars ((mevedel-tool-usage-test--cancelled nil)))
  ,test
  (test)
  :doc "pipeline cancellation cancels every pending retrieval and suppresses delivery"
  (let* ((codex (gptel--make-openai-oauth :name "Codex OAuth"))
         (claude (mevedel-claude-code--make-backend :name "Claude Code"))
         (gptel--known-backends (list (cons "Codex OAuth" codex) (cons "Claude Code" claude)))
         (pipeline-cancelled nil)
         (cell (list (lambda () (setq pipeline-cancelled t))))
         delivered)
    (mevedel-tool-usage-test--with-transports
     t
     (lambda ()
       (with-temp-buffer
         (setq-local gptel-backend claude gptel-model 'test-model)
         (mevedel-pipeline--step-handler
          (list :tool (mevedel-tool-ensure "SubscriptionUsage") :args nil :cancel-cell cell)
          (lambda (context) (setq delivered context))
          #'ignore)
         (should-not delivered)
         (funcall (car cell))
         (should pipeline-cancelled)
         (should (equal '(claude codex)
                        (sort (copy-sequence mevedel-tool-usage-test--cancelled)
                              (lambda (a b) (string< (symbol-name a) (symbol-name b))))))
         (accept-process-output nil 0.05)
         (should-not delivered))))))

(provide 'test-mevedel-tool-usage)
;;; test-mevedel-tool-usage.el ends here
