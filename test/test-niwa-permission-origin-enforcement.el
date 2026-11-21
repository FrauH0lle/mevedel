;;; test-niwa-permission-origin-enforcement.el -- Bound grants -*- lexical-binding: t -*-

;;; Commentary:
;; Persistent relative authority cannot follow a changed workspace path.

;;; Code:

(require 'niwa-execution-grants)
(require 'niwa-permission-persistence)
(require 'niwa-sandbox)
(require 'niwa-structs)
(require 'niwa-tool-permission)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-execution-grants-covering-relative
  (:doc "rejects a replaced relative grant while preserving explicit absolute authority")
  (let* ((root (make-temp-file "niwa-relative-cover-" t))
         (outside (make-temp-file "niwa-relative-outside-" t))
         (workspace (niwa-workspace--create :type 'project :root root))
         (file (niwa-permission-persistence-file workspace 'workspace))
         (vault (file-name-concat root "vault"))
         (niwa-protected-paths nil))
    (unwind-protect
        (progn
          (make-directory vault)
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (prin1 '(:rules nil :resource-grants
                     ((:path "vault" :access write :recursive t)))
                   (current-buffer)))
          (let ((grants (plist-get (niwa-permission-persistent-authority
                                   workspace 'workspace) :resource-grants)))
            (should (niwa-execution-grants-covering-grant vault 'write grants t))
            (delete-directory vault)
            (make-symbolic-link outside vault)
            (should-not (niwa-execution-grants-covering-grant vault 'write grants t))
            (should (niwa-execution-grants-covering-grant
                     vault 'write `((:path ,vault :access write :recursive t)) t))))
      (delete-directory root t)
      (delete-directory outside t))))

(niwa-deftest niwa-sandbox-prepare-relative
  (:doc "refuses a stale relative profile before resolving it into an outside mount")
  (let* ((root (make-temp-file "niwa-relative-launch-" t))
         (outside (make-temp-file "niwa-relative-launch-outside-" t))
         (workspace (niwa-workspace--create :type 'project :root root))
         (file (niwa-permission-persistence-file workspace 'workspace))
         (vault (file-name-concat root "vault"))
         (niwa-protected-paths nil))
    (unwind-protect
        (progn
          (skip-unless (plist-get (niwa-sandbox-probe) :available))
          (make-directory vault)
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (prin1 '(:rules nil :resource-grants
                     ((:path "vault" :access write :recursive t)))
                   (current-buffer)))
          (let ((grants (plist-get (niwa-permission-persistent-authority
                                   workspace 'workspace) :resource-grants)))
            (delete-directory vault)
            (make-symbolic-link outside vault)
            (let ((prepared (niwa-sandbox-prepare
                             '("true") root (list root)
                             (list :file-system grants) nil 'required root)))
              (should (eq 'refused (plist-get prepared :state)))
              (should (string-match-p "Workspace-relative" (plist-get prepared :error))))))
      (delete-directory root t)
      (delete-directory outside t))))

(niwa-deftest niwa-sandbox-prepare-relative-during-resolution
  (:doc "refuses a path swapped after initial origin validation but before canonical capture")
  (let* ((root (make-temp-file "niwa-relative-capture-" t))
         (outside (make-temp-file "niwa-relative-capture-outside-" t))
         (workspace (niwa-workspace--create :type 'project :root root))
         (file (niwa-permission-persistence-file workspace 'workspace))
         (vault (file-name-concat root "vault"))
         (niwa-protected-paths nil)
         swapped advice)
    (unwind-protect
        (progn
          (skip-unless (plist-get (niwa-sandbox-probe) :available))
          (make-directory vault)
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (prin1 '(:rules nil :resource-grants
                     ((:path "vault" :access write :recursive t)))
                   (current-buffer)))
          (let ((grants (plist-get (niwa-permission-persistent-authority
                                   workspace 'workspace) :resource-grants)))
            (setq advice
                  (lambda (path &rest _)
                    (when (and (equal path vault) (not swapped))
                      (setq swapped t)
                      (delete-directory vault)
                      (make-symbolic-link outside vault))))
            (advice-add 'file-truename :before advice)
            (let ((prepared (niwa-sandbox-prepare
                             '("true") root (list root)
                             (list :file-system grants) nil 'required root)))
              (should swapped)
              (should (eq 'refused (plist-get prepared :state)))
              (should (string-match-p "changed during resolution" (plist-get prepared :error))))))
      (when advice (advice-remove 'file-truename advice))
      (delete-directory root t)
      (delete-directory outside t))))

(niwa-deftest niwa-tool-permission-step-relative-consumption
  (:doc "rechecks relative native authority after a tool checker yields filesystem changes")
  (dolist (scenario '(bounded-only bounded-rule absolute-rule unprotected-rule))
    (ert-info ((format "Native bound scenario: %s" scenario))
      (let* ((root (make-temp-file "niwa-relative-native-" t))
         (outside (make-temp-file "niwa-relative-native-outside-" t))
         (niwa-user-dir (file-name-concat root "user"))
         (workspace (niwa-workspace--create :type 'project :root root))
         (session (niwa-session-create "native-bound" workspace))
         (file (niwa-permission-persistence-file workspace 'workspace))
         (vault (file-name-concat root "vault"))
         (niwa-protected-paths
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
            (setq-local niwa--session session)
            (setf (niwa-session-permission-mode session) 'ask)
            (unless (eq scenario 'bounded-only)
              (setf (niwa-session-permission-rules session)
                    `(("Read" :path ,vault :action allow))))
            (let ((tool (niwa-tool--create
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
              (niwa-tool-permission-step
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

(provide 'test-niwa-permission-origin-enforcement)
;;; test-niwa-permission-origin-enforcement.el ends here
