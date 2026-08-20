;;; test-mevedel-session-save-as.el --- Portable Save As tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `mevedel-session-save-as'.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))


(mevedel-deftest mevedel-session-save-as-run ()
  ,test
  (test)
  :doc "cleans pre-commit recovery and keeps every committed child discoverable"
  (dolist (failure-point
           '(pre-commit publish-pre-commit post-commit move-post-commit))
    (let* ((host (format "save-as-%s" failure-point))
           (local-root
            (file-name-as-directory
             (make-temp-file "mevedel-save-as-failure-" t)))
           (owner-id (make-string 64 ?a))
           buffer
           view-buffer
           session
           old-save-path
           new-save-path
           staging-path)
      (unwind-protect
          (mevedel-test--with-local-shell-tramp (list host)
            (cl-destructuring-bind
                (_workspace fixture-session session-dir segment)
                (test-mevedel-session-persistence--make-remote-restore-fixture
                 host local-root "Parent transcript\n")
              (setq session fixture-session
                    old-save-path session-dir
                    new-save-path
                    (file-name-as-directory
                     (file-name-concat
                      (file-name-directory (directory-file-name session-dir))
                      (format "clone-%s" failure-point)))
                    buffer (generate-new-buffer " *save-as-failure*")
                    view-buffer (generate-new-buffer " *save-as-failure:view*"))
              (let* ((mevedel-session-durability--client-id owner-id)
                     (mevedel-session-durability--disclosed-targets
                      (make-hash-table :test #'equal))
                     (materialize-function
                      (symbol-function
                       'mevedel-session-rewind-materialize-publication))
                     (publish-function
                      (symbol-function 'mevedel-session-publication-publish))
                     (rename-function (symbol-function 'rename-file))
                     recovery)
                (puthash
                 (mevedel-execution-target-identity
                  (mevedel-session-execution-target session))
                 t mevedel-session-durability--disclosed-targets)
                (should
                 (mevedel-session-durability-lease-acquire
                  old-save-path "save-as-owner" session))
                (with-current-buffer buffer
                  (org-mode)
                  (setq-local mevedel--session session
                              mevedel--view-buffer view-buffer
                              buffer-file-name segment)
                  (insert "Parent transcript\n"))
                (cl-letf
                    (((symbol-function
                       'mevedel-session-rewind-materialize-publication)
                      (lambda (actual-session publication destination)
                        (setq staging-path destination)
                        (if (eq failure-point 'pre-commit)
                            (error "Injected pre-commit Save As failure")
                          (funcall materialize-function
                                   actual-session publication destination))))
                     ((symbol-function 'mevedel-session-publication-publish)
                      (lambda (actual-session artifacts)
                        (if (eq failure-point 'publish-pre-commit)
                            (let* ((directory
                                    (make-temp-file
                                     "mevedel-save-as-recovery-" t))
                                   (source
                                    (file-name-concat directory "artifact")))
                              (write-region "recovery" nil source nil 'silent)
                              (setq recovery directory)
                              (setf
                               (mevedel-session-pending-publication
                                actual-session)
                               (list
                                :batches
                                (list
                                 (list :directory directory
                                       :artifacts
                                       (list (list :source source))))))
                              (error "Injected publication failure"))
                          (prog1
                              (funcall publish-function
                                       actual-session artifacts)
                            (when (eq failure-point 'post-commit)
                              (error "Injected post-commit Save As failure"))))))
                     ((symbol-function 'rename-file)
                      (lambda (file newname &optional ok-if-exists)
                        (if (and (eq failure-point 'move-post-commit)
                                 staging-path
                                 (equal file
                                        (directory-file-name staging-path)))
                            (error "Injected Save As discovery move failure")
                          (funcall rename-function
                                   file newname ok-if-exists)))))
                  (let ((error
                         (should-error
                          (mevedel-session-save-as-run
                           session buffer "clone"
                           (file-name-nondirectory
                            (directory-file-name new-save-path))
                           new-save-path))))
                    (if (memq failure-point
                              '(pre-commit publish-pre-commit))
                        (progn
                          (should-not (file-directory-p staging-path))
                          (should-not (file-directory-p new-save-path))
                          (should
                           (equal old-save-path
                                  (mevedel-session-save-path session)))
                          (should
                           (mevedel-session-durability-lease-owned-p session)))
                      (should
                       (string-match-p
                        "committed a child"
                        (error-message-string error)))
                      (if (eq failure-point 'move-post-commit)
                          (progn
                            (should-not (file-directory-p new-save-path))
                            (should (file-directory-p staging-path))
                            (should
                             (equal staging-path
                                    (mevedel-session-save-path session)))
                            (should
                             (cl-find staging-path
                                      (mevedel-session-persistence-list-sessions
                                       (mevedel-session-workspace session))
                                      :key (lambda (entry)
                                             (plist-get entry :save-path))
                                      :test #'equal)))
                        (should (file-directory-p new-save-path)))
                      (should
                       (mevedel-session-publication-read
                        (mevedel-session-save-path session)))
                      (should
                       (string-match-p "clone" (buffer-name buffer)))
                      (should
                       (string-match-p "clone" (buffer-name view-buffer)))
                      (should
                       (mevedel-session-durability-lease-owned-p session))))
                  (when recovery
                    (should-not (file-exists-p recovery)))))))
        (mevedel-test--with-local-shell-tramp (list host)
          (when (and session (mevedel-session-save-path session))
            (ignore-errors
              (mevedel-session-persistence-lock-release
               (mevedel-session-save-path session) session)))
          (when old-save-path
            (ignore-errors
              (mevedel-session-persistence-lock-release old-save-path)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (set-buffer-modified-p nil)
              (set-visited-file-name nil t)
              (setq-local kill-buffer-hook nil))
            (kill-buffer buffer)))
        (when (buffer-live-p view-buffer)
          (kill-buffer view-buffer))
        (when (file-directory-p local-root)
          (delete-directory local-root t))
        (mevedel-workspace-clear-registry)))))

(provide 'test-mevedel-session-save-as)
;;; test-mevedel-session-save-as.el ends here
