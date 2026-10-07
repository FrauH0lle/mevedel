;;; test-mevedel-claude-code-maintenance.el --- Runtime maintenance -*- lexical-binding: t -*-
;;; Commentary:
;; Exercise the real installation pipeline with inert CLI and npm fixtures.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-claude-code)
(require 'mevedel-claude-code-maintenance)
(defconst mevedel-maintenance-test--fixture
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "fixtures" "claude-status.py"))

(defmacro mevedel-maintenance-test--with-installation (&rest body)
  "Run BODY in an isolated native installation with a managed adapter."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "mevedel-runtime-test-" t))
          (native (file-name-concat directory "claude" "versions" "2.1.290"))
          (bin (file-name-concat directory "bin"))
          (mevedel-claude-code-directory (file-name-concat directory "managed"))
          (mevedel-claude-code-executable (file-name-concat bin "claude"))
          (mevedel-claude-code-adapter-executable nil)
          (mevedel-claude-code-auto-update nil)
          (mevedel-claude-code-maintenance--jobs (make-hash-table :test #'equal))
          (mevedel-claude-code-maintenance-changed-hook nil)
          (mevedel-claude-code-maintenance--timer nil)
          (process-environment (copy-sequence process-environment))
          (exec-path (cons bin exec-path)))
     (unwind-protect
         (progn
           (make-directory (file-name-directory native) t)
           (make-directory bin t)
           (copy-file mevedel-maintenance-test--fixture native)
           (set-file-modes native #o700)
           (make-symbolic-link native mevedel-claude-code-executable)
           (copy-file native (file-name-concat bin "npm"))
           (cl-labels ((await ()
                         (with-timeout (8 (ert-fail "Maintenance fixture timed out"))
                           (while (> (hash-table-count mevedel-claude-code-maintenance--jobs) 0)
                             (accept-process-output nil 0.01)))))
             ,@body))
       (mevedel-claude-code-maintenance-stop)
       (delete-directory directory t))))

(mevedel-deftest mevedel-claude-code-maintenance-check (:quiet t)
  ,test
  (test)
  :doc "checked stable runtime is published only after a real ACP handshake"
  (mevedel-maintenance-test--with-installation
   (let ((create-lockfiles nil) messages)
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest args) (push (apply #'format-message format args) messages))))
       (mevedel-claude-code-maintenance-check t)
       ;; The installation lock holds even when the user disables lock files.
       (should (file-locked-p (file-name-concat mevedel-claude-code-directory "runtime-update")))
       (should (= 1 (hash-table-count mevedel-claude-code-maintenance--jobs)))
       (mevedel-claude-code-maintenance-check t)
       (should (= 1 (hash-table-count mevedel-claude-code-maintenance--jobs)))
       (await))
     (should (equal '("mevedel: Claude runtime is up to date"
                      "mevedel: a Claude runtime update check is already running"
                      "mevedel: checking Claude runtime updates...")
                    messages)))
   (let* ((state (mevedel-claude-code-maintenance-state))
	  (cli (plist-get state :cli)) (adapter (plist-get state :adapter)))
     (should (equal "ready" (plist-get state :status)))
     (should (file-executable-p cli))
     (should (file-executable-p adapter))
     (let ((mevedel-claude-code-executable "another-claude"))
       (should-not (mevedel-claude-code-maintenance-state)))
     (delete-file native)
     (should (file-executable-p cli))
     (copy-file cli native)
     (setenv "MEVEDEL_TEST_INSTALL_FAIL" "1")
     (mevedel-claude-code-maintenance-check t)
     (await)
     (setq state (mevedel-claude-code-maintenance-state))
     (should (equal "failed" (plist-get state :status)))
     (should (equal cli (plist-get state :cli)))
     (should (equal adapter (plist-get state :adapter)))
     (should-not (file-locked-p (file-name-concat mevedel-claude-code-directory "runtime-update")))))
  :doc "external installations remain untouched and return actionable status"
  (mevedel-maintenance-test--with-installation
   (let ((mevedel-claude-code-executable (file-name-concat bin "external-claude")))
     (copy-file native mevedel-claude-code-executable)
     (mevedel-claude-code-maintenance-check t)
     (await)
     (should (equal "ready" (plist-get (mevedel-claude-code-maintenance-state) :status)))
     (should (file-exists-p (file-name-concat bin "npm"))))))

(mevedel-deftest mevedel-claude-code-maintenance--prune ()
  ,test
  (test)
  :doc "pruning keeps current, previous, rejected and running versions"
  (let* ((mevedel-claude-code-directory (make-temp-file "mevedel-prune-test-" t))
         (runtime (lambda (version)
                    (file-name-concat mevedel-claude-code-directory "runtimes" version)))
         (adapter (lambda (version)
                    (file-name-concat (funcall runtime version) "node_modules" ".bin" "claude-agent-acp")))
         (cli (lambda (version)
                (file-name-concat mevedel-claude-code-directory "executables" version)))
         (process (make-process :name "mevedel-prune-test" :command '("cat") :noquery t)))
    (unwind-protect
        (progn
          (dolist (version '("1.0.0" "1.1.0" "1.2.0" "2.0.0" "3.0.0"))
            (make-directory (file-name-directory (funcall adapter version)) t)
            (write-region "" nil (funcall adapter version) nil 'silent))
          (make-directory (funcall cli "") t)
          (dolist (version '("2.1.290" "2.1.291" "2.1.292" "2.1.293"))
            (write-region "" nil (funcall cli version) nil 'silent))
          (process-put process 'mevedel-acp-launch
                       (list :command (funcall adapter "1.0.0")
                             :environment (list (concat "CLAUDE_CODE_EXECUTABLE=" (funcall cli "2.1.290")))))
          (mevedel-claude-code-maintenance--prune
           (list :cli (funcall cli "2.1.293") :adapter (funcall adapter "2.0.0")
                 :previousCli (funcall cli "2.1.292") :previousAdapter (funcall adapter "1.1.0")
                 :rejectedAdapter (funcall adapter "3.0.0")))
          (should (equal '("1.0.0" "1.1.0" "2.0.0" "3.0.0")
                         (directory-files (funcall runtime "") nil "\\`[0-9]")))
          (should (equal '("2.1.290" "2.1.292" "2.1.293")
                         (directory-files (funcall cli "") nil "\\`[0-9]"))))
      (delete-process process)
      (delete-directory mevedel-claude-code-directory t))))

(mevedel-deftest mevedel-claude-code-maintenance-state ()
  ,test
  (test)
  :doc "a fresh mevedel reads runtime state without loading Claude or touching disk"
  (let ((home (make-temp-file "mevedel-maintenance-cold-" t)))
    (unwind-protect
        (with-temp-buffer
          (let ((status (call-process
                         (expand-file-name invocation-name invocation-directory) nil t nil
                         "--batch" "-Q" "--eval"
                         (prin1-to-string
                          `(progn
                             (setq load-path ',load-path
                                   user-emacs-directory ,(file-name-as-directory home))
                             (require 'mevedel)
                             (require 'mevedel-claude-code-maintenance)
                             (let ((files (directory-files-recursively ,home "" t)))
                               (when (mevedel-claude-code-maintenance-state)
                                 (error "Unexpected runtime state"))
                               (when (featurep 'mevedel-claude-code)
                                 (error "Runtime state loaded the Claude adapter"))
                               (unless (equal files (directory-files-recursively ,home "" t))
                                 (error "Runtime state wrote files"))))))))
            (ert-info ((buffer-string))
              (should (equal 0 status)))))
      (delete-directory home t))))

(mevedel-deftest mevedel-claude-code-maintenance-stop (:quiet t)
  (mevedel-maintenance-test--with-installation
   (let ((mevedel-claude-code-auto-update t))
     (mevedel-claude-code-maintenance-check)
     (should (timerp mevedel-claude-code-maintenance--timer))
     (mevedel-claude-code-maintenance-stop)
     (should-not mevedel-claude-code-maintenance--timer)
     (should (zerop (hash-table-count mevedel-claude-code-maintenance--jobs)))
     (should-not (file-locked-p (file-name-concat mevedel-claude-code-directory "runtime-update"))))))

(provide 'test-mevedel-claude-code-maintenance)
;;; test-mevedel-claude-code-maintenance.el ends here
