;;; test-mevedel-subscription-usage.el --- Usage report lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:
;; Drive both user entry points and asynchronous results through the report.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-subscription-usage)
(require 'mevedel-skills-ui)
(require 'mevedel-menu)
(require 'mevedel-view)

(mevedel-deftest mevedel-subscription-usage-show ()
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (let* ((backend (gptel--make-openai-oauth :name "Selected Codex"))
             (other (mevedel-claude-code--make-backend :name "Selected Claude"))
             (session (mevedel-session--create :session-id "usage-test"
                                              :goal (mevedel-goal--create :id "g" :status 'active :tokens-used 42)))
             callbacks selections cancellations report retrieved)
        (with-current-buffer data-buf
          (setq-local mevedel--session session gptel-backend backend gptel-model 'gpt-5.5)
          (insert "Retained transcript\n"))
        (with-current-buffer view-buf
          (mevedel-view-test--insert-composer-draft "> quoted\nsecond line" 4))
        (let ((transcript (with-current-buffer data-buf (buffer-string)))
              (state (copy-tree (with-current-buffer data-buf gptel--token-usage)))
              (goal (copy-sequence (mevedel-session-goal session))))
          (unwind-protect
              (cl-letf (((symbol-function 'mevedel-subscription-usage-provider-fetch)
                         (lambda (selected callback)
                           (push selected selections)
                           (setq callbacks (append callbacks (list callback)))
                           (let ((id (length callbacks)))
                             (lambda () (push id cancellations))))))
                (with-current-buffer view-buf
                  (if ,slash
                      (mevedel-view-run-invocation "usage" "")
                    (call-interactively (plist-get (cdr (transient-get-suffix 'mevedel-menu--top "U")) :command))))
                (setq report (buffer-local-value 'mevedel-subscription-usage--buffer data-buf))
                (with-current-buffer report
                  (should (derived-mode-p 'mevedel-report-mode))
                  (should buffer-read-only)
                  (should (eq backend (car selections)))
                  (should (string-match-p "Loading" (buffer-string)))
                  (funcall (nth 0 callbacks) "Quota: 11%" nil)
                  (setq retrieved mevedel-subscription-usage--retrieved)
                  (should retrieved)
                  (call-interactively (key-binding "g"))
                  (should (string-match-p "STALE" (buffer-string)))
                  (funcall (nth 1 callbacks) nil "Offline")
                  (should (equal retrieved mevedel-subscription-usage--retrieved))
                  (should (string-match-p "STALE" (buffer-string)))
                  (should (string-match-p "Quota: 11%" (buffer-string)))
                  (mevedel-subscription-usage-refresh)
                  (mevedel-subscription-usage-refresh)
                  (should (memq 3 cancellations))
                  (funcall (nth 2 callbacks) "Superseded result" nil)
                  (should-not (string-match-p "Superseded result" (buffer-string)))
                  (with-current-buffer data-buf (setq gptel-backend other))
                  (mevedel-subscription-usage-refresh)
                  (should (eq other (car selections)))
                  (should-not mevedel-subscription-usage--result)
                  (should-not mevedel-subscription-usage--retrieved)
                  (should-not (string-match-p "Quota: 11%" (buffer-string)))
                  (funcall (nth 3 callbacks) "Wrong backend" nil)
                  (should-not (string-match-p "Wrong backend" (buffer-string)))
                  (funcall (nth 4 callbacks) "Native Claude quota" nil))
                (with-current-buffer view-buf (should (eq report (mevedel-subscription-usage-show))))
                (with-current-buffer report (call-interactively (key-binding "q")))
                (should (eq (window-buffer (selected-window)) view-buf))
                (should-not (buffer-live-p report))
                (should (memq 6 cancellations))
                (funcall (nth 5 callbacks) "Closed buffer callback" nil)
                (should-not (buffer-live-p report))
                (with-current-buffer view-buf
                  (should (equal "> quoted\nsecond line" (mevedel-view--input-text))))
                (with-current-buffer data-buf
                  (should (equal transcript (buffer-string)))
                  (should (equal state gptel--token-usage)))
                (should (equal goal (mevedel-session-goal session))))
            (when (buffer-live-p report) (kill-buffer report)))))))
  (slash)
  :doc "slash entry preserves draft, transcript and Goal through refresh, cancellation and quit" t
  :doc "transient U uses the same report lifecycle without conversation mutation" nil)

(mevedel-deftest mevedel-subscription-usage-refresh ()
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (let (report cancelled callback)
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-subscription-usage-provider-fetch)
                       (lambda (_backend complete)
                         (setq callback complete)
                         (lambda () (setq cancelled t)))))
              (with-current-buffer view-buf (setq report (mevedel-subscription-usage-show)))
              (kill-buffer report)
              (should cancelled)
              (funcall callback "late" nil)
              (should-not (buffer-live-p report)))
          (when (buffer-live-p report) (kill-buffer report)))))))

(mevedel-deftest mevedel-subscription-usage--render ()
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (let (report)
        (unwind-protect
            (progn
              (with-current-buffer data-buf
                (setq-local gptel-backend (gptel--make-openai :name "API backend")))
              (with-current-buffer view-buf (setq report (mevedel-subscription-usage-show)))
              (with-current-buffer report
                (should (string-match-p "Provider: API backend" (buffer-string)))
                (should (string-match-p "Last successful retrieval: Unavailable" (buffer-string)))
                (should-not (string-match-p "STALE\\|Loading" (buffer-string)))
                (should-not mevedel-subscription-usage--cancel)))
          (when (buffer-live-p report) (kill-buffer report)))))))

(mevedel-deftest mevedel-subscription-usage-show/codex-http (:quiet t)
  ;; Report, provider, credential check and HTTP run unmocked; only the
  ;; fixed quota endpoint is redirected to a local server.
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (let* ((directory (make-temp-file "usage-codex-" t))
             (gptel--openai-oauth-token-file (file-name-concat directory "token.el"))
             (mevedel-auth--operations (make-hash-table :test #'equal))
             (backend (gptel--make-openai-oauth :name "Selected Codex"))
             (original-retrieve (symbol-function 'url-retrieve))
             request report)
        (gptel-oauth--write-token
         gptel--openai-oauth-token-file
         (list :access_token "current" :refresh_token "refresh"
               :expires_at (+ (float-time) 3600)
               :id_token '(:https://api.openai.com/auth (:chatgpt_account_id "chosen"))))
        (unwind-protect
            (mevedel-test-http
             (lambda (line) (setq request line)
               '("200 OK" "" "{\"plan_type\":\"plus\",\"rate_limit\":{\"primary_window\":{\"used_percent\":40}}}"))
             (lambda (url)
               (cl-letf (((symbol-function 'url-retrieve)
                          (lambda (_requested callback &rest args)
                            (apply original-retrieve (concat url "/wham/usage") callback args))))
                 (with-current-buffer data-buf
                   (setq-local gptel-backend backend gptel-model 'gpt-5.5))
                 (with-current-buffer view-buf (mevedel-view-run-invocation "usage" ""))
                 (setq report (buffer-local-value 'mevedel-subscription-usage--buffer data-buf))
                 (with-timeout (5 (ert-fail "Usage report did not settle"))
                   (while (with-current-buffer report mevedel-subscription-usage--cancel)
                     (accept-process-output nil 0.01)))
                 (with-current-buffer report
                   (should (string-match-p "Account: chosen" (buffer-string)))
                   (should (string-match-p "Plan: plus" (buffer-string)))
                   (should (string-match-p "40% used" (buffer-string))))
                 (should (string-prefix-p "GET /wham/usage" request)))))
          (when (buffer-live-p report) (kill-buffer report))
          (delete-directory directory t))))))

(mevedel-deftest mevedel-cmd--usage ()
  (progn
   (should (assq 'mevedel-cmd--usage (mapcar (lambda (entry) (cons (cdr entry) (car entry))) mevedel-slash-commands)))
  (should (member "usage" (all-completions "us" (mevedel-skills--slash-completion-table mevedel-slash-commands))))))

(mevedel-deftest mevedel-subscription-usage-show/cold-load ()
  (let* ((root (file-name-directory (locate-library "mevedel-subscription-usage")))
         (compiled (make-temp-file "usage-cold-" t)))
    (unwind-protect
        (progn
          (dolist (name '("mevedel-subscription-usage" "mevedel-subscription-usage-provider"))
            (let ((target (file-name-concat compiled (concat name ".el"))))
              (copy-file (file-name-concat root (concat name ".el")) target)
              (let ((byte-compile-verbose nil)) (byte-compile-file target))))
          (with-temp-buffer
            (let ((status (call-process
                   (expand-file-name invocation-name invocation-directory) nil t nil
                   "--batch" "-Q" "--eval"
                   (prin1-to-string
                    `(progn
                       (setq load-path ',(append (list compiled (file-name-concat root "test")) load-path))
                       (require 'gptel-openai)
                       (require 'helpers)
                       (require 'mevedel-view)
                       (require 'mevedel-skills-ui)
                       (when (featurep 'mevedel-subscription-usage)
                         (error "Report eagerly loaded before first use"))
                       (save-window-excursion
                         (mevedel-view-test--with-buffers
                           (with-current-buffer data-buf
                             (setq-local gptel-backend (gptel--make-openai :name "API")))
                           (let (report)
                             (unwind-protect
                                 (progn
                                   (with-current-buffer view-buf
                                     (mevedel-view-run-invocation "usage" ""))
                                   (setq report (buffer-local-value 'mevedel-subscription-usage--buffer data-buf))
                                   (unless (buffer-live-p report) (error "Report did not open")))
                               (when (buffer-live-p report) (kill-buffer report)))))))))))
              (ert-info ((buffer-string)) (should (= 0 status))))
            (should (string-empty-p (string-trim (buffer-string))))))
      (delete-directory compiled t))))

(provide 'test-mevedel-subscription-usage)
;;; test-mevedel-subscription-usage.el ends here
