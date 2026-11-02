;;; test-mevedel-permission-origin-enforcement.el -- Bound grants -*- lexical-binding: t -*-

;;; Commentary:
;; Persistent relative authority cannot follow a changed workspace path.

;;; Code:

(require 'mevedel-execution-grants)
(require 'mevedel-permission-persistence)
(require 'mevedel-sandbox)
(require 'mevedel-structs)
(require 'mevedel-tool-permission)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-execution-grants-covering-relative
  (:doc "rejects a replaced relative grant while preserving explicit absolute authority")
  (let* ((root (make-temp-file "mevedel-relative-cover-" t))
         (outside (make-temp-file "mevedel-relative-outside-" t))
         (workspace (mevedel-workspace--create :type 'project :root root))
         (file (mevedel-permission-persistence-file workspace 'workspace))
         (vault (file-name-concat root "vault"))
         (mevedel-protected-paths nil))
    (unwind-protect
        (progn
          (make-directory vault)
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (prin1 '(:rules nil :resource-grants
                     ((:path "vault" :access write :recursive t)))
                   (current-buffer)))
          (let ((grants (plist-get (mevedel-permission-persistent-authority
                                   workspace 'workspace) :resource-grants)))
            (should (mevedel-execution-grants-covering-grant vault 'write grants t))
            (delete-directory vault)
            (make-symbolic-link outside vault)
            (should-not (mevedel-execution-grants-covering-grant vault 'write grants t))
            (should (mevedel-execution-grants-covering-grant
                     vault 'write `((:path ,vault :access write :recursive t)) t))))
      (delete-directory root t)
      (delete-directory outside t))))

(mevedel-deftest mevedel-sandbox-prepare-relative
  (:doc "refuses a stale relative profile before resolving it into an outside mount")
  (let* ((root (make-temp-file "mevedel-relative-launch-" t))
         (outside (make-temp-file "mevedel-relative-launch-outside-" t))
         (workspace (mevedel-workspace--create :type 'project :root root))
         (file (mevedel-permission-persistence-file workspace 'workspace))
         (vault (file-name-concat root "vault"))
         (mevedel-protected-paths nil))
    (unwind-protect
        (progn
          (skip-unless (plist-get (mevedel-sandbox-probe) :available))
          (make-directory vault)
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (prin1 '(:rules nil :resource-grants
                     ((:path "vault" :access write :recursive t)))
                   (current-buffer)))
          (let ((grants (plist-get (mevedel-permission-persistent-authority
                                   workspace 'workspace) :resource-grants)))
            (delete-directory vault)
            (make-symbolic-link outside vault)
            (let ((prepared (mevedel-sandbox-prepare
                             '("true") root (list root)
                             (list :file-system grants) nil 'required root)))
              (should (eq 'refused (plist-get prepared :state)))
              (should (string-match-p "Workspace-relative" (plist-get prepared :error))))))
      (delete-directory root t)
      (delete-directory outside t))))

(mevedel-deftest mevedel-sandbox-prepare-relative-during-resolution
  (:doc "refuses a path swapped after initial origin validation but before canonical capture")
  (let* ((root (make-temp-file "mevedel-relative-capture-" t))
         (outside (make-temp-file "mevedel-relative-capture-outside-" t))
         (workspace (mevedel-workspace--create :type 'project :root root))
         (file (mevedel-permission-persistence-file workspace 'workspace))
         (vault (file-name-concat root "vault"))
         (mevedel-protected-paths nil)
         swapped advice)
    (unwind-protect
        (progn
          (skip-unless (plist-get (mevedel-sandbox-probe) :available))
          (make-directory vault)
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (prin1 '(:rules nil :resource-grants
                     ((:path "vault" :access write :recursive t)))
                   (current-buffer)))
          (let ((grants (plist-get (mevedel-permission-persistent-authority
                                   workspace 'workspace) :resource-grants)))
            (setq advice
                  (lambda (path &rest _)
                    (when (and (equal path vault) (not swapped))
                      (setq swapped t)
                      (delete-directory vault)
                      (make-symbolic-link outside vault))))
            (advice-add 'file-truename :before advice)
            (let ((prepared (mevedel-sandbox-prepare
                             '("true") root (list root)
                             (list :file-system grants) nil 'required root)))
              (should swapped)
              (should (eq 'refused (plist-get prepared :state)))
              (should (string-match-p "changed during resolution" (plist-get prepared :error))))))
      (when advice (advice-remove 'file-truename advice))
      (delete-directory root t)
      (delete-directory outside t))))

(mevedel-deftest mevedel-tool-permission-step-relative-consumption
  (:doc "rechecks relative native authority after a tool checker yields filesystem changes")
  (dolist (scenario '(bounded-only bounded-rule absolute-rule unprotected-rule))
    (ert-info ((format "Native bound scenario: %s" scenario))
      (let* ((root (make-temp-file "mevedel-relative-native-" t))
         (outside (make-temp-file "mevedel-relative-native-outside-" t))
         (mevedel-user-dir (file-name-concat root "user"))
         (workspace (mevedel-workspace--create :type 'project :root root))
         (session (mevedel-session-create "native-bound" workspace))
         (file (mevedel-permission-persistence-file workspace 'workspace))
         (vault (file-name-concat root "vault"))
         (mevedel-protected-paths
          (unless (eq scenario 'unprotected-rule)
            `((,(file-name-concat root "vault/**") . inaccessible))))
         allowed refused swapped advice (probes 0))
    (unwind-protect
        (progn
          (make-directory vault)
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (prin1 `(:rules nil :resource-grants
                     ((:path "vault" :access read :recursive t)
                      ,@(and (eq scenario 'absolute-rule)
                             `((:path ,vault :access read :recursive t)))))
                   (current-buffer)))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (setf (mevedel-session-permission-mode session) 'ask)
            (unless (eq scenario 'bounded-only)
              (setf (mevedel-session-permission-rules session)
                    `(("Read" :path ,vault :action allow))))
            (let ((tool (mevedel-tool--create
                         :name "Read" :read-only-p t
                         :get-path (lambda (args) (plist-get args :file_path))
                         :check-permission
                         (and (eq scenario 'bounded-only)
                              (lambda (_tool _args)
                           (setq swapped t)
                           (delete-directory vault)
                           (make-symbolic-link outside vault)
                           nil)))))
              ;; With no checker slot, direct allow rules use the native tail
              ;; rather than the command/resource composition path.  Replace
              ;; the directory just after both initial source/path probes.
              (unless (eq scenario 'bounded-only)
                (setq advice
                      (lambda (path &rest _)
                        (when (equal path vault)
                          (cl-incf probes)
                          (when (and (= probes 2) (not swapped))
                            (setq swapped t)
                            (delete-directory vault)
                            (make-symbolic-link outside vault)))))
                (advice-add 'file-symlink-p :after advice))
              (mevedel-tool-permission-step
               (list :tool tool :args (list :file_path vault) :buffer (current-buffer)
                     :session session :workspace workspace :default-directory root)
               (lambda (_) (setq allowed t))
               (lambda (reason &rest _) (setq refused reason)))))
          (should swapped)
          (if (memq scenario '(absolute-rule unprotected-rule))
              (progn (should allowed) (should-not refused))
            (should-not allowed)
            (should (string-match-p "Workspace-relative" refused))))
      (when advice (advice-remove 'file-symlink-p advice))
      (delete-directory root t)
      (delete-directory outside t))))))

(provide 'test-mevedel-permission-origin-enforcement)
;;; test-mevedel-permission-origin-enforcement.el ends here
