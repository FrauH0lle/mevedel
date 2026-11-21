;;; test-niwa-claude-code-maintenance.el --- Runtime maintenance -*- lexical-binding: t -*-
;;; Commentary:
;; Exercise the real installation pipeline with inert CLI and npm fixtures.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'niwa-claude-code)
(require 'niwa-claude-code-maintenance)
(defconst niwa-maintenance-test--fixture
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "fixtures" "claude-status.py"))

(defmacro niwa-maintenance-test--with-installation (&rest body)
  "Run BODY in an isolated native installation with a managed adapter."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "niwa-runtime-test-" t))
          (native (file-name-concat directory "claude" "versions" "2.1.293"))
          (bin (file-name-concat directory "bin"))
          (niwa-claude-code-directory (file-name-concat directory "managed"))
          (niwa-claude-code-executable (file-name-concat bin "claude"))
          (niwa-claude-code-adapter-executable nil)
          (niwa-claude-code-auto-update nil)
          (niwa-claude-code-maintenance--jobs (make-hash-table :test #'equal))
          (niwa-claude-code-maintenance-changed-hook nil)
          (niwa-claude-code-maintenance--timer nil)
          (process-environment (copy-sequence process-environment))
          (exec-path (cons bin exec-path)))
     (unwind-protect
         (progn
           (make-directory (file-name-directory native) t)
           (make-directory bin t)
           (copy-file niwa-maintenance-test--fixture native)
           (set-file-modes native #o700)
           (make-symbolic-link native niwa-claude-code-executable)
           (copy-file native (file-name-concat bin "npm"))
           (cl-labels ((await ()
                         (niwa-test--await 8 "Maintenance fixture timed out"
                           (not (> (hash-table-count niwa-claude-code-maintenance--jobs) 0)))))
             ,@body))
       (niwa-claude-code-maintenance-stop)
       (delete-directory directory t))))

(niwa-deftest niwa-claude-code-maintenance-check (:quiet t)
  ,test
  (test)
  :doc "checked stable runtime is published only after a real ACP handshake"
  (niwa-maintenance-test--with-installation
   (let ((create-lockfiles nil) messages)
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest args) (push (apply #'format-message format args) messages))))
       (niwa-claude-code-maintenance-check t)
       ;; The installation lock holds even when the user disables lock files.
       (should (file-locked-p (file-name-concat niwa-claude-code-directory "runtime-update")))
       (should (= 1 (hash-table-count niwa-claude-code-maintenance--jobs)))
       (niwa-claude-code-maintenance-check t)
       (should (= 1 (hash-table-count niwa-claude-code-maintenance--jobs)))
       (await))
     (should (equal '("niwa: Claude runtime is up to date"
                      "niwa: a Claude runtime update check is already running"
                      "niwa: checking Claude runtime updates...")
                    messages)))
   (let* ((state (niwa-claude-code-maintenance-state))
	  (cli (plist-get state :cli)) (adapter (plist-get state :adapter)))
     (should (equal "ready" (plist-get state :status)))
     (should (file-executable-p cli))
     (should (file-executable-p adapter))
     (let ((niwa-claude-code-executable "another-claude"))
       (should-not (niwa-claude-code-maintenance-state)))
     (delete-file native)
     (should (file-executable-p cli))
     (copy-file cli native)
     (setenv "NIWA_TEST_INSTALL_FAIL" "1")
     (niwa-claude-code-maintenance-check t)
     (await)
     (setq state (niwa-claude-code-maintenance-state))
     (should (equal "failed" (plist-get state :status)))
     (should (equal cli (plist-get state :cli)))
     (should (equal adapter (plist-get state :adapter)))
     (should-not (file-locked-p (file-name-concat niwa-claude-code-directory "runtime-update")))))
  :doc "the native CLI updates on its configured channel, latest by default"
  (niwa-maintenance-test--with-installation
   (let ((config (file-name-concat directory "config"))
         (installed (file-name-concat directory "installed-channel")))
     (make-directory config)
     (setenv "CLAUDE_CONFIG_DIR" config)
     (setenv "NIWA_TEST_INSTALL_CHANNEL" installed)
     (niwa-claude-code-maintenance-check t)
     (await)
     (should (equal "latest" (with-temp-buffer (insert-file-contents installed) (buffer-string))))
     (write-region "{\"autoUpdatesChannel\": \"stable\"}" nil
                   (file-name-concat config "settings.json") nil 'silent)
     (niwa-claude-code-maintenance-check t)
     (await)
     (should (equal "stable" (with-temp-buffer (insert-file-contents installed) (buffer-string))))))
  :doc "external installations remain untouched and return actionable status"
  (niwa-maintenance-test--with-installation
   (let ((niwa-claude-code-executable (file-name-concat bin "external-claude")))
     (copy-file native niwa-claude-code-executable)
     (niwa-claude-code-maintenance-check t)
     (await)
     (should (equal "ready" (plist-get (niwa-claude-code-maintenance-state) :status)))
     (should (file-exists-p (file-name-concat bin "npm")))))
  :doc "runtime updates retain serialization when editor file locks are disabled"
  (niwa-maintenance-test--with-installation
   (let ((create-lockfiles nil)
         (lock (file-name-concat niwa-claude-code-directory "runtime-update")))
     (niwa-claude-code-maintenance-check t)
     (should (eq t (file-locked-p lock)))
     (niwa-claude-code-maintenance-stop)
     (should-not (file-locked-p lock)))))

(niwa-deftest niwa-claude-code-maintenance--prune ()
  ,test
  (test)
  :doc "pruning keeps current, previous, rejected and running versions"
  (let* ((niwa-claude-code-directory (make-temp-file "niwa-prune-test-" t))
         (runtime (lambda (version)
                    (file-name-concat niwa-claude-code-directory "runtimes" version)))
         (adapter (lambda (version)
                    (file-name-concat (funcall runtime version) "node_modules" ".bin" "claude-agent-acp")))
         (cli (lambda (version)
                (file-name-concat niwa-claude-code-directory "executables" version)))
         (process (make-process :name "niwa-prune-test" :command '("cat") :noquery t)))
    (unwind-protect
        (progn
          (dolist (version '("1.0.0" "1.1.0" "1.2.0" "2.0.0" "3.0.0"))
            (make-directory (file-name-directory (funcall adapter version)) t)
            (write-region "" nil (funcall adapter version) nil 'silent))
          (make-directory (funcall cli "") t)
          (dolist (version '("2.1.290" "2.1.291" "2.1.292" "2.1.293"))
            (write-region "" nil (funcall cli version) nil 'silent))
          (process-put process 'niwa-acp-launch
                       (list :command (funcall adapter "1.0.0")
                             :environment (list (concat "CLAUDE_CODE_EXECUTABLE=" (funcall cli "2.1.290")))))
          (niwa-claude-code-maintenance--prune
           (list :cli (funcall cli "2.1.293") :adapter (funcall adapter "2.0.0")
                 :previousCli (funcall cli "2.1.292") :previousAdapter (funcall adapter "1.1.0")
                 :rejectedAdapter (funcall adapter "3.0.0")))
          (should (equal '("1.0.0" "1.1.0" "2.0.0" "3.0.0")
                         (directory-files (funcall runtime "") nil "\\`[0-9]")))
          (should (equal '("2.1.290" "2.1.292" "2.1.293")
                         (directory-files (funcall cli "") nil "\\`[0-9]"))))
      (delete-process process)
      (delete-directory niwa-claude-code-directory t))))

(niwa-deftest niwa-claude-code-maintenance-state ()
  ,test
  (test)
  :doc "a fresh niwa reads runtime state without loading Claude or touching disk"
  (let ((home (make-temp-file "niwa-maintenance-cold-" t)))
    (unwind-protect
        (with-temp-buffer
          (let ((status (call-process
                         (expand-file-name invocation-name invocation-directory) nil t nil
                         "--batch" "-Q" "--eval"
                         (prin1-to-string
                          `(progn
                             (setq load-path ',load-path
                                   user-emacs-directory ,(file-name-as-directory home))
                             (require 'niwa)
                             (require 'niwa-claude-code-maintenance)
                             (let ((files (directory-files-recursively ,home "" t)))
                               (when (niwa-claude-code-maintenance-state)
                                 (error "Unexpected runtime state"))
                               (when (featurep 'niwa-claude-code)
                                 (error "Runtime state loaded the Claude adapter"))
                               (unless (equal files (directory-files-recursively ,home "" t))
                                 (error "Runtime state wrote files"))))))))
            (ert-info ((buffer-string))
              (should (equal 0 status)))))
      (delete-directory home t))))

(niwa-deftest niwa-claude-code-maintenance-stop (:quiet t)
  (niwa-maintenance-test--with-installation
   (let ((niwa-claude-code-auto-update t))
     (niwa-claude-code-maintenance-check)
     (should (timerp niwa-claude-code-maintenance--timer))
     (niwa-claude-code-maintenance-stop)
     (should-not niwa-claude-code-maintenance--timer)
     (should (zerop (hash-table-count niwa-claude-code-maintenance--jobs)))
     (should-not (file-locked-p (file-name-concat niwa-claude-code-directory "runtime-update"))))))

(provide 'test-niwa-claude-code-maintenance)
;;; test-niwa-claude-code-maintenance.el ends here
