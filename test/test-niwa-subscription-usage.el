;;; test-niwa-subscription-usage.el --- Usage report lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:
;; Drive both user entry points and asynchronous results through the report.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'niwa-subscription-usage)
(require 'niwa-skills-ui)
(require 'niwa-menu)
(require 'niwa-view)

(niwa-deftest niwa-subscription-usage-show ()
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let* ((backend (gptel--make-openai-oauth :name "Selected Codex"))
             (other (niwa-claude-code--make-backend :name "Selected Claude"))
             (session (niwa-session--create :session-id "usage-test"
                                              :goal (niwa-goal--create :id "g" :status 'active :tokens-used 42)))
             (gptel--known-backends nil)
             callbacks selections cancellations report retrieved)
        (with-current-buffer data-buf
          (setq-local niwa--session session gptel-backend backend gptel-model 'gpt-5.5)
          (insert "Retained transcript\n"))
        (with-current-buffer view-buf
          (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4))
        (switch-to-buffer view-buf)
        (let ((transcript (with-current-buffer data-buf (buffer-string)))
              (state (copy-tree (with-current-buffer data-buf gptel--token-usage)))
              (goal (copy-sequence (niwa-session-goal session))))
          (unwind-protect
              (cl-letf (((symbol-function 'niwa-subscription-usage-provider-fetch)
                         (lambda (selected callback)
                           (push selected selections)
                           (setq callbacks (append callbacks (list callback)))
                           (let ((id (length callbacks)))
                             (lambda () (push id cancellations))))))
                (with-current-buffer view-buf
                  (if ,slash
                      (niwa-view-run-invocation "usage" "")
                    ;; The top menu builds its utility row from this list.
                    (call-interactively (nth 2 (assoc "U" niwa-menu--utilities)))))
                (setq report (buffer-local-value 'niwa-subscription-usage--buffer data-buf))
                (with-current-buffer report
                  (should (derived-mode-p 'niwa-report-mode))
                  (should buffer-read-only)
                  (should (eq backend (car selections)))
                  (should (string-match-p "Loading" (buffer-string)))
                  (funcall (nth 0 callbacks) "Quota: 11%" nil)
                  (should (string-match "Last successful retrieval: [0-9].*" (buffer-string)))
                  (setq retrieved (match-string 0 (buffer-string)))
                  (call-interactively (key-binding "g"))
                  (should (string-match-p "STALE" (buffer-string)))
                  (funcall (nth 1 callbacks) nil "Offline")
                  (should (string-search retrieved (buffer-string)))
                  (should (string-match-p "STALE" (buffer-string)))
                  (should (string-match-p "Quota: 11%" (buffer-string)))
                  (niwa-subscription-usage-refresh)
                  (niwa-subscription-usage-refresh)
                  (should (memq 3 cancellations))
                  (funcall (nth 2 callbacks) "Superseded result" nil)
                  (should-not (string-match-p "Superseded result" (buffer-string)))
                  (with-current-buffer data-buf (setq gptel-backend other))
                  (niwa-subscription-usage-refresh)
                  (should (eq other (car selections)))
                  (should (string-search "Last successful retrieval: Unavailable" (buffer-string)))
                  (should-not (string-match-p "Quota: 11%" (buffer-string)))
                  (funcall (nth 3 callbacks) "Wrong backend" nil)
                  (should-not (string-match-p "Wrong backend" (buffer-string)))
                  (funcall (nth 4 callbacks) "Native Claude quota" nil))
                (with-current-buffer view-buf (should (eq report (niwa-subscription-usage-show))))
                (with-current-buffer report (call-interactively (key-binding "q")))
                (should (eq (window-buffer (selected-window)) view-buf))
                (should-not (buffer-live-p report))
                (should (memq 6 cancellations))
                (funcall (nth 5 callbacks) "Closed buffer callback" nil)
                (should-not (buffer-live-p report))
                (with-current-buffer view-buf
                  (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text))))
                (with-current-buffer data-buf
                  (should (equal transcript (buffer-string)))
                  (should (equal state gptel--token-usage)))
                (should (equal goal (niwa-session-goal session))))
            (when (buffer-live-p report) (kill-buffer report)))))))
  (slash)
  :doc "slash entry preserves draft, transcript and Goal through refresh, cancellation and quit" t
  :doc "transient U uses the same report lifecycle without conversation mutation" nil)

(niwa-deftest niwa-subscription-usage-refresh ()
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let* ((codex (gptel--make-openai-oauth :name "Quota"))
             (claude (niwa-claude-code--make-backend :name "Quota" :models '(sonnet)))
             (gptel--known-backends `(("Quota" . ,claude)))
             callbacks cancelled report)
        (with-current-buffer data-buf
          (setq-local gptel-backend codex gptel-model 'selected))
        (unwind-protect
            (cl-letf (((symbol-function 'niwa-subscription-usage-provider-fetch)
                       (lambda (backend complete)
                         (push (list backend gptel-model complete) callbacks)
                         (lambda () (push backend cancelled)))))
              (with-current-buffer view-buf (setq report (niwa-subscription-usage-show)))
              (with-current-buffer report
                (goto-char (point-min))
                (should (search-forward "** Quota (current session)" nil t))
                (push-button (1- (point)))
                (should (search-forward "** Quota\n" nil t))
                (should (equal (mapcar #'cadr callbacks) '(sonnet selected)))
                (funcall (caddar callbacks) "Claude quotas" nil)
                (goto-char (point-min))
                (should (search-forward "Claude quotas" nil t))
                (should-not (invisible-p (1- (point))))
                (should (string-match-p "Loading" (buffer-string)))
                (should (string-match-p "Claude quotas" (buffer-string)))
                (funcall (caddr (cadr callbacks)) nil "Codex offline")
                (should (string-match-p "Codex offline" (buffer-string)))
                (should (string-match-p "Claude quotas" (buffer-string)))
                (niwa-subscription-usage-refresh)
                (let ((late (caddar callbacks)))
                  (niwa-subscription-usage-refresh)
                  (funcall late "Superseded Claude" nil)
                  (should-not (string-match-p "Superseded Claude" (buffer-string))))
                (should (equal cancelled (list claude codex)))
                (setq cancelled nil))
              (kill-buffer report)
              (should (equal cancelled (list claude codex)))
              (funcall (caddar callbacks) "late" nil)
              (should-not (buffer-live-p report)))
          (when (buffer-live-p report) (kill-buffer report)))))))

(niwa-deftest niwa-subscription-usage--render ()
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let (gptel--known-backends report)
        (unwind-protect
            (progn
              (with-current-buffer data-buf
                (setq-local gptel-backend (gptel--make-openai :name "API backend")))
              (with-current-buffer view-buf (setq report (niwa-subscription-usage-show)))
              (with-current-buffer report
                (should (string-search "No Codex OAuth or Claude Code backend is configured." (buffer-string)))
                (should-not (string-match-p "STALE\\|Loading" (buffer-string)))))
          (when (buffer-live-p report) (kill-buffer report)))))))

(niwa-deftest niwa-subscription-usage-show/codex-http (:quiet t)
  ;; Report, provider, credential check and HTTP run unmocked; only the
  ;; fixed quota endpoint is redirected to a local server.
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let* ((directory (make-temp-file "usage-codex-" t))
             (gptel--openai-oauth-token-file (file-name-concat directory "token.el"))
             (niwa-auth--operations (make-hash-table :test #'equal))
             (backend (gptel--make-openai-oauth :name "Selected Codex"))
             (original-retrieve (symbol-function 'url-retrieve))
             gptel--known-backends request report)
        (gptel-oauth--write-token
         gptel--openai-oauth-token-file
         (list :access_token "current" :refresh_token "refresh"
               :expires_at (+ (float-time) 3600)
               :id_token '(:https://api.openai.com/auth (:chatgpt_account_id "chosen"))))
        (unwind-protect
            (niwa-test-http
             (lambda (line) (setq request line)
               '("200 OK" "" "{\"plan_type\":\"plus\",\"rate_limit\":{\"primary_window\":{\"used_percent\":40}}}"))
             (lambda (url)
               (cl-letf (((symbol-function 'url-retrieve)
                          (lambda (_requested callback &rest args)
                            (apply original-retrieve (concat url "/wham/usage") callback args))))
                 (with-current-buffer data-buf
                   (setq-local gptel-backend backend gptel-model 'gpt-5.5))
                 (with-current-buffer view-buf (niwa-view-run-invocation "usage" ""))
                 (setq report (buffer-local-value 'niwa-subscription-usage--buffer data-buf))
                 (niwa-test--await 5 "Usage report did not settle"
                   (with-current-buffer report (not (string-search "Loading" (buffer-string)))))
                 (with-current-buffer report
                   (should (string-match-p "Account: chosen" (buffer-string)))
                   (should (string-match-p "Plan: plus" (buffer-string)))
                   (should (string-match-p "40% used" (buffer-string))))
                 (should (string-prefix-p "GET /wham/usage" request)))))
          (when (buffer-live-p report) (kill-buffer report))
          (delete-directory directory t))))))

(niwa-deftest niwa-cmd--usage ()
  (progn
   (should (assq 'niwa-cmd--usage (mapcar (lambda (entry) (cons (cdr entry) (car entry))) niwa-slash-commands)))
  (should (member "usage" (all-completions "us" (niwa-skills--slash-completion-table niwa-slash-commands))))))

(niwa-deftest niwa-subscription-usage-show/cold-load ()
  (let* ((root (file-name-directory (locate-library "niwa-subscription-usage")))
         (compiled (make-temp-file "usage-cold-" t)))
    (unwind-protect
        (progn
          (dolist (name '("niwa-subscription-usage" "niwa-subscription-usage-provider"))
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
                       (require 'niwa-view)
                       (require 'niwa-skills-ui)
                       (when (featurep 'niwa-subscription-usage)
                         (error "Report eagerly loaded before first use"))
                       (save-window-excursion
                         (niwa-view-test--with-buffers
                           (with-current-buffer data-buf
                             (setq-local gptel-backend (gptel--make-openai :name "API")))
                           (let (report)
                             (unwind-protect
                                 (progn
                                   (with-current-buffer view-buf
                                     (niwa-view-run-invocation "usage" ""))
                                   (setq report (buffer-local-value 'niwa-subscription-usage--buffer data-buf))
                                   (unless (buffer-live-p report) (error "Report did not open")))
                               (when (buffer-live-p report) (kill-buffer report)))))))))))
              (ert-info ((buffer-string)) (should (= 0 status))))
            (should (string-empty-p (string-trim (buffer-string))))))
      (delete-directory compiled t))))

(provide 'test-niwa-subscription-usage)
;;; test-niwa-subscription-usage.el ends here
