;;; loading.el --- Cold startup scenarios -*- lexical-binding: t -*-

;;; Commentary:
;; Runs only in Eask-isolated fresh subprocesses, with no test helpers.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'gptel-openai)

(defconst mevedel-test-loading--root
  (expand-file-name "../.." (file-name-directory load-file-name)))
(defconst mevedel-test-loading--deferred
  '(mevedel-tool-exec mevedel-tool-exec-permission mevedel-tool-patch
    mevedel-tool-web mevedel-tool-fs-read mevedel-tool-fs-search
    mevedel-shared-editing mevedel-shared-library mevedel-view
    mevedel-collaboration-guest mevedel-collaboration-files
    mevedel-collaboration-artifact mevedel-collaboration-editing
    mevedel-collaboration-transport mevedel-claude-code mevedel-acp acp
    mevedel-mcp mevedel-claude-code-usage))

(defun mevedel-test-loading (scenario)
  "Exercise cold SCENARIO through installed entry points."
  (let* ((root (make-temp-file "mevedel-loading-state-" t))
         (default-directory (file-name-as-directory root))
         (user-emacs-directory (file-name-concat root "emacs/"))
         (inhibit-message t))
    (unwind-protect
        (progn
          (require 'mevedel)
          (setq mevedel-user-dir (file-name-concat root "user/"))
          (pcase scenario
            ('installation
             (mevedel-uninstall)
             (dotimes (_ 2)
               (mevedel-install)
               (should (functionp #'mevedel-create-directive))
               (should (commandp 'mevedel-compact))
               (should (commandp 'mevedel-buddy-mode))
               (should (commandp 'mevedel-buddy-global-mode))
               (should-not (featurep 'mevedel-buddy))
               (should (commandp 'mevedel-claude-code-setup))
               (should (memq #'mevedel-journal-idle-session-opened mevedel-session-start-hook))
               (dolist (tool (mevedel-tool-all))
                 (should (functionp (mevedel-tool-handler tool)))
                 (should (stringp (mevedel-tool-prompt tool))))
               (should (> (length (mevedel-tool-all)) 35))
               (when after-focus-change-function (funcall after-focus-change-function))
               (set-face-attribute 'default nil :foreground (face-attribute 'default :foreground))
               (run-hook-with-args 'enable-theme-functions 'user)
               (dolist (feature mevedel-test-loading--deferred)
                 (ert-info ((format "Unexpected eager feature: %s" feature))
                   (should-not (featurep feature))))
               (mevedel-uninstall)
               (dolist (feature mevedel-test-loading--deferred)
                 (should-not (featurep feature)))))
            ((or 'directive-preview-implement 'directive-preview-discuss
                 'directive-answer)
             (mevedel-install)
             (should-not (featurep 'mevedel-directive-request))
             (with-temp-buffer
               (insert "cold directive source")
               (let* ((workspace (mevedel-workspace))
                      (directive
                       (mevedel--create-directive-in
                        (current-buffer) (point-min) (point-max) nil
                        "Explain the cold directive")))
                 (goto-char (point-min))
                 (if (eq scenario 'directive-answer)
                     ;; No prior session or answer: load the session owner,
                     ;; create its view, then report the absent answer.
                     (should
                      (equal '(user-error "Directive answer is not in the live transcript")
                             (should-error
                              (mevedel--ov-actions-show-answer directive)
                              :type 'user-error)))
                   (cl-letf (((symbol-function 'completing-read)
                              (lambda (&rest _)
                                (if (eq scenario 'directive-preview-discuss)
                                    "discuss" "implement"))))
                     (mevedel-preview-directive-prompt))
                   (with-current-buffer "*mevedel-directive-preview*"
                     (should (string-search "Explain the cold directive"
                                            (buffer-string))))
                   (kill-buffer "*mevedel-directive-preview*"))
                 (should (featurep 'mevedel-directive-request))
                 (should (mevedel-workspace-directives workspace)))))
            ('gptel
             (mevedel-install)
             (with-temp-buffer
               (setq-local gptel-backend (gptel-make-openai "Cold native" :key "unused")
                           gptel-model 'gpt-4o-mini)
               (let ((fsm (gptel-request "hello" :dry-run t)))
                 (should (plist-get (gptel-fsm-info fsm) :data))))
             (dolist (feature '(acp mevedel-acp mevedel-mcp mevedel-claude-code))
               (should-not (featurep feature))))
            ((or 'chat 'chat-in-directory)
             (mevedel-install)
             (if (eq scenario 'chat-in-directory)
                 (mevedel-in-directory default-directory)
               (mevedel))
             (should (featurep 'mevedel-view))
             (should (cl-some (lambda (buffer)
                                (with-current-buffer buffer
                                  (derived-mode-p 'mevedel-view-mode)))
                              (buffer-list)))
             (should (featurep 'mevedel-journal-idle))
             (require 'mevedel-readiness)
             (dolist (feature '(acp mevedel-acp mevedel-mcp mevedel-claude-code
                               mevedel-claude-code-usage))
               (should-not (featurep feature))))
            ('tools
             (mevedel-install)
             ;; Persisted references recover the catalog before implementations.
             (mevedel-tool-clear-registry)
             (dolist (name '("Bash" "Eval" "Read" "Glob" "ApplyPatch" "WebFetch"))
               (should (mevedel-tool-ensure name)))
             (dolist (feature mevedel-test-loading--deferred)
               (should-not (featurep feature)))
             (require 'mevedel-permission-mode)
             (require 'mevedel-permission-rules)
             (let ((mevedel-permission-mode 'full-auto)
                   (mevedel-permission-rules nil) outcome)
               (funcall (mevedel-tool-check-permission-async (mevedel-tool-get "Eval"))
                        nil '(:expression "(+ 1 2)")
                        (lambda (value) (setq outcome value)))
               (should (eq 'allow outcome)))
             (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "cold"))
                   result)
               (with-temp-buffer
                 (setq-local mevedel--session session)
                 (funcall (gptel-tool-function
                           (mevedel-tool-gptel-tool (mevedel-tool-ensure "ListExecutions")))
                          (lambda (value) (setq result value))))
               (should (stringp result))
               (should-not (string-prefix-p "Error:" result)))
             (should (featurep 'mevedel-tool-exec))
             (let ((file (file-name-concat root "sample.txt")))
               (write-region "cold read evidence\n" nil file nil 'silent)
               (let (result)
                 (funcall (mevedel-tool-handler (mevedel-tool-ensure "Read"))
                          (lambda (value) (setq result value)) (list :file_path file))
                 (should (string-match-p "cold read evidence" (plist-get result :result)))))
             (let ((tool (mevedel-tool-ensure "ApplyPatch")))
               (should (listp (funcall (mevedel-tool-get-paths tool)
                                      '(:patch "*** Begin Patch\n*** Add File: cold.txt\n+hello\n*** End Patch")))))
             (let ((tool (mevedel-tool-ensure "WebFetch")))
               (should (equal "example.com"
                              (funcall (mevedel-tool-get-domain tool)
                                       '(:url "https://example.com/path"))))))
            ('renderers
             (mevedel-install)
             (dolist (row '(("Read" (:file_path "sample.txt") nil)
                            ("Glob" (:pattern "*.el") nil)
                            ("Bash" (:command "printf hello") nil)
                            ("ApplyPatch" nil (:kind patch :files nil))
                            ("WebFetch" (:url "https://example.com") nil)))
               (let* ((tool (mevedel-tool-ensure (car row)))
                      (renderer (mevedel-tool-renderer tool)))
                 (when (listp renderer) (setq renderer (cdr (assq 'success renderer))))
                 (should (plist-get (funcall renderer (car row) (cadr row) "hello" (caddr row))
                                    :header)))))
            ('claude
             (let* ((backend (mevedel-claude-code-register))
                    (gptel-backend backend) (gptel-model 'sonnet)
                    (gptel-tools nil) response)
               (should (mevedel-engine-external-p backend))
               (should-not (featurep 'acp))
               ;; Substitute the paid launcher only; real generic dispatch,
               ;; transport loading, ACP protocol, and callbacks run unchanged.
               (autoload 'mevedel-claude-code-launch "mevedel-claude-code")
               (require 'mevedel-claude-code-backend)
               (advice-add
                'mevedel-claude-code-launch :override
                (lambda (&rest _)
                  (list :command (executable-find "python3")
                        :args (list (file-name-concat mevedel-test-loading--root
                                                     "test/fixtures/acp-agent.py"))
                        :cwd root)))
               (mevedel-engine-request-text
                backend "hello" "system" (lambda (text _info) (setq response text)))
               (with-timeout (5 (ert-fail "Claude fixture timed out"))
                 (while (not response) (accept-process-output nil 0.01)))
               (should (equal "answer:hello" response))
               (should (featurep 'acp))
               (let (done text)
                 (mevedel-engine-request-workload
                  backend "workload" "system" nil
                  (lambda (value _info)
                    (if (eq value t) (setq done t)
                      (when (stringp value) (push value text)))) nil nil)
                 (with-timeout (5 (ert-fail "Claude workload fixture timed out"))
                   (while (not done) (accept-process-output nil 0.01)))
                 (should (equal "answer:workload" (apply #'concat (nreverse text)))))))
            ('room
             (mevedel-install)
             (mevedel)
             (require 'mevedel-collaboration)
             (require 'mevedel-collaboration-transport)
             (should-not (featurep 'mevedel-collaboration-task))
             (let ((data (mevedel-collaboration--current-data-buffer)))
               (should (buffer-live-p data))
               (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                         ((symbol-function 'mevedel-collaboration--transport-open)
                          (lambda (&rest _) (list :state 'down)))
                         ((symbol-function 'mevedel-collaboration--transport-stop) #'ignore))
                 (let ((room (car (mevedel-collaboration--room-or-start data))))
                   (unwind-protect
                       (progn
                         (mevedel-collaboration--publish-timer data)
                         (should (featurep 'mevedel-collaboration-task))
                         (should (equal "tasks" (plist-get (plist-get room :tasks) :t)))
                         (should (= 0 (plist-get (plist-get room :tasks) :total))))
                     (mevedel-collaboration--stop-internal room 'user-stop))))))
            ('lobby
             (require 'mevedel-collaboration-lobby)
             (mevedel-collaboration-lobby-restore)
             (make-directory mevedel-user-dir t)
             (dolist (text '("" "nil" "(" "42"))
               (with-temp-file (mevedel-collaboration-lobby--intent-path) (insert text))
               (mevedel-collaboration-lobby-restore)
               (should-not (featurep 'mevedel-collaboration-guest)))
             (mevedel-collaboration-lobby--set-intended default-directory t)
             (autoload 'mevedel-collaboration--transport-open "mevedel-collaboration-transport")
             (advice-add 'mevedel-collaboration--transport-open :override
                         (lambda (&rest _) (error "Fixture relay unavailable")))
             (let (warnings)
               (cl-letf (((symbol-function 'display-warning)
                          (lambda (&rest args) (push args warnings))))
                 (mevedel-collaboration-lobby-restore))
               (should (featurep 'mevedel-collaboration-guest))
               (should warnings)
               (should (equal (list default-directory) (mevedel-collaboration-lobby--intended))))
             (let ((opened 0))
               (cl-letf (((symbol-function 'mevedel-collaboration--transport-open)
                          (lambda (&rest _) (cl-incf opened) nil)))
                 (mevedel-collaboration-lobby-restore)
                 (mevedel-collaboration-lobby-restore)
                 (should (= opened 1))
                 (mevedel-collaboration-lobby-stop)
                 (should-not (mevedel-collaboration-lobby--intended)))))))
      (dolist (buffer (buffer-list))
        (when (buffer-local-value 'mevedel--session buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (mevedel-uninstall)
      (delete-directory root t))))
