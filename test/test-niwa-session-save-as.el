;;; test-niwa-session-save-as.el --- Portable Save As tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `niwa-session-save-as'.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))


(niwa-deftest niwa-session-save-as-run ()
  ,test
  (test)
  :doc "cleans early exits and retains a child committed before throw or quit"
  (dolist (scenario '(clone-error materialize-throw materialize-quit
                     publish-throw publish-quit))
    (let* ((root (file-name-as-directory (make-temp-file "save-as-exit-" t)))
           (workspace (test-niwa-session-persistence--make-workspace root))
           (session (niwa-session-create "parent" workspace root))
           (buffer (generate-new-buffer " *save-as-exit*"))
           (clone (symbol-function 'niwa-session-fork-clone-session))
           (materialize (symbol-function 'niwa-session-rewind-materialize-publication))
           (publish (symbol-function 'niwa-session-publication-publish))
           staging child)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (org-mode)
              (setq-local niwa--session session)
              (insert "Parent transcript\n")
              (niwa-session-artifacts-save session buffer))
            (let* ((parent-path (niwa-session-save-path session))
                   (parent-id (niwa-session-session-id session))
                   (parent-sidecar (niwa-session-artifacts-read-file-raw
                                    (niwa-session-artifacts-sidecar-path parent-path)))
                   (destination (file-name-concat
                                 (file-name-directory (directory-file-name parent-path))
                                 "child"))
                   (committed (memq scenario '(publish-throw publish-quit)))
                   (outcome
                    (cl-letf (((symbol-function 'niwa-session-fork-clone-session)
                               (lambda (source policy &rest keys)
                                 (setq staging (plist-get keys :save-path))
                                 (if (eq scenario 'clone-error)
                                     (error "Injected clone failure")
                                   (setq child (apply clone source policy keys)))))
                              ((symbol-function 'niwa-session-rewind-materialize-publication)
                               (lambda (&rest args)
                                 (pcase scenario
                                   ('materialize-throw (throw 'save-as-escape 'escaped))
                                   ('materialize-quit (signal 'quit nil))
                                   (_ (apply materialize args)))))
                              ((symbol-function 'niwa-session-publication-publish)
                               (lambda (&rest args)
                                 (prog1 (apply publish args)
                                   (pcase scenario
                                     ('publish-throw (throw 'save-as-escape 'escaped))
                                     ('publish-quit (signal 'quit nil)))))))
                      (condition-case failure
                          (catch 'save-as-escape
                            (niwa-session-save-as-run
                             session buffer "child" "child" destination))
                        (error failure)
                        (quit 'quit)))))
              (ert-info ((format "Save As exit %s" scenario))
                (should (equal outcome
                               (pcase scenario
                                 ('clone-error '(error "Injected clone failure"))
                                 ((or 'materialize-throw 'publish-throw) 'escaped)
                                 (_ 'quit))))
                (should staging)
                (should-not (file-directory-p destination))
                (should (equal parent-sidecar
                               (niwa-session-artifacts-read-file-raw
                                (niwa-session-artifacts-sidecar-path parent-path))))
                (if committed
                    (progn
                      (should (file-directory-p staging))
                      (should (niwa-session-publication-read staging))
                      (should (equal "child" (niwa-session-session-id session)))
                      (should (equal staging (niwa-session-save-path session)))
                      (should (cl-find staging
                                       (niwa-session-persistence-list-sessions workspace)
                                       :key (lambda (entry) (plist-get entry :save-path))
                                       :test #'equal)))
                  (should-not (file-directory-p staging))
                  (should (equal parent-path (niwa-session-save-path session)))
                  (should (equal parent-id (niwa-session-session-id session))))
                (should (niwa-session-durability-lease-owned-p session))
                (when child
                  (should-not (niwa-session-durability-lease-owned-p child))
                  (should-not (niwa-session-lease-renewal-timer child))))))
        (when (and child (niwa-session-lease child))
          (niwa-session-persistence-lock-release
           (niwa-session-save-path child) child))
        (when (niwa-session-save-path session)
          (niwa-session-persistence-lock-release
           (niwa-session-save-path session) session))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil)
            (setq-local kill-buffer-hook nil))
          (kill-buffer buffer))
        (delete-directory root t)
        (niwa-workspace-clear-registry))))
  :doc "cleans pre-commit recovery and keeps every committed child discoverable"
  (dolist (scenario
           '(pre-commit publish-pre-commit post-commit move-post-commit
             adopt-rebind adopt-verify))
    (let* ((host (format "save-as-%s" scenario))
           (local-root
            (file-name-as-directory
             (make-temp-file "niwa-save-as-failure-" t)))
           (owner-id (make-string 64 ?a))
           buffer
           view-buffer
           session
           old-save-path
           new-save-path
           staging-path
           retained-child)
      (unwind-protect
          (niwa-test--with-local-shell-tramp (list host)
            (cl-destructuring-bind
                (_workspace fixture-session session-dir segment)
                (test-niwa-session-persistence--make-remote-restore-fixture
                 host local-root "Parent transcript\n")
              (setq session fixture-session
                    old-save-path session-dir
                    new-save-path
                    (file-name-as-directory
                     (file-name-concat
                      (file-name-directory (directory-file-name session-dir))
                      (format "clone-%s" scenario)))
                    buffer (generate-new-buffer " *save-as-failure*")
                    view-buffer (generate-new-buffer " *save-as-failure:view*"))
              (let* ((niwa-session-durability--client-id owner-id)
                     (niwa-session-durability--disclosed-targets
                      (make-hash-table :test #'equal))
                     (materialize-function
                      (symbol-function
                       'niwa-session-rewind-materialize-publication))
                     (publish-function
                      (symbol-function 'niwa-session-publication-publish))
                     (lock-acquire-function
                      (symbol-function
                       'niwa-session-persistence-lock-acquire))
                     (adopt-function
                      (symbol-function
                       'niwa-session-durability-adopt-owned-lease))
                     (rename-function (symbol-function 'rename-file))
                     (lock-acquires 0)
                     old-buffer-file
                     old-buffer-name
                     old-publication
                     old-renewal-timer
                     old-session-id
                     old-session-name
                     old-view-name
                     recovery)
                (puthash
                 (niwa-execution-target-identity
                  (niwa-session-execution-target session))
                 t niwa-session-durability--disclosed-targets)
                (should
                 (niwa-session-durability-lease-acquire
                  old-save-path "save-as-owner" session))
                (with-current-buffer buffer
                  (org-mode)
                  (setq-local niwa--session session
                              niwa--view-buffer view-buffer
                              buffer-file-name segment)
                  (insert "Parent transcript\n"))
                (setq old-buffer-file
                      (buffer-local-value 'buffer-file-name buffer)
                      old-buffer-name (buffer-name buffer)
                      old-publication (niwa-session-publication session)
                      old-renewal-timer
                      (niwa-session-lease-renewal-timer session)
                      old-session-id (niwa-session-session-id session)
                      old-session-name (niwa-session-name session)
                      old-view-name (buffer-name view-buffer))
                (cl-letf
                    (((symbol-function
                       'niwa-session-rewind-materialize-publication)
                      (lambda (actual-session publication destination)
                        (setq staging-path destination)
                        (if (eq scenario 'pre-commit)
                            (error "Injected pre-commit Save As failure")
                          (funcall materialize-function
                                   actual-session publication destination))))
                     ((symbol-function 'niwa-session-publication-publish)
                      (lambda (actual-session artifacts)
                        (if (eq scenario 'publish-pre-commit)
                            (let* ((directory
                                    (make-temp-file
                                     "niwa-save-as-recovery-" t))
                                   (source
                                    (file-name-concat directory "artifact")))
                              (write-region "recovery" nil source nil 'silent)
                              (setq recovery directory)
                              (setf
                               (niwa-session-pending-publication
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
                            (when (eq scenario 'post-commit)
                              (error "Injected post-commit Save As failure"))))))
                     ((symbol-function
                       'niwa-session-persistence-lock-acquire)
                      (lambda (path buffer-name &optional owner)
                        (cl-incf lock-acquires)
                        (if (and (eq scenario 'adopt-rebind)
                                 (> lock-acquires 1))
                            nil
                          (funcall lock-acquire-function
                                   path buffer-name owner))))
                     ((symbol-function
                       'niwa-session-durability-adopt-owned-lease)
                      (lambda (destination source)
                        (if (eq scenario 'adopt-verify)
                            (progn
                              (setq retained-child source)
                              (error "Injected adoption verification failure"))
                          (funcall adopt-function destination source))))
                     ((symbol-function 'rename-file)
                      (lambda (file newname &optional ok-if-exists)
                        (if (and (eq scenario 'move-post-commit)
                                 staging-path
                                 (equal file
                                        (directory-file-name staging-path)))
                            (error "Injected Save As discovery move failure")
                          (funcall rename-function
                                   file newname ok-if-exists)))))
                  (let ((outcome
                         (condition-case error
                             (niwa-session-save-as-run
                              session buffer "clone"
                              (file-name-nondirectory
                               (directory-file-name new-save-path))
                              new-save-path)
                           (error error))))
                    (if (memq scenario '(pre-commit publish-pre-commit))
                        (progn
                          (should (eq (car outcome) 'error))
                          (should-not (file-directory-p staging-path))
                          (should-not (file-directory-p new-save-path))
                          (should
                           (equal old-save-path
                                  (niwa-session-save-path session)))
                          (should
                           (niwa-session-durability-lease-owned-p session)))
                      (if (eq scenario 'adopt-rebind)
                          (progn
                            (should (eq (plist-get outcome :status)
                                        'committed))
                            (should (= lock-acquires 1)))
                        (should
                         (string-match-p
                          "committed a child"
                          (error-message-string outcome))))
                      (cond
                       ((eq scenario 'adopt-verify)
                        (should retained-child)
                        (should (equal old-save-path
                                       (niwa-session-save-path session)))
                        (should (equal old-session-id
                                       (niwa-session-session-id session)))
                        (should (equal old-session-name
                                       (niwa-session-name session)))
                        (should (eq old-publication
                                    (niwa-session-publication session)))
                        (should (eq old-renewal-timer
                                    (niwa-session-lease-renewal-timer
                                     session)))
                        (should
                         (equal old-buffer-file
                                (buffer-local-value 'buffer-file-name buffer)))
                        (should (equal old-buffer-name (buffer-name buffer)))
                        (should (equal old-view-name
                                       (buffer-name view-buffer)))
                        (should (niwa-session-durability-lease-owned-p
                                 session))
                        (should (niwa-session-durability-lease-owned-p
                                 retained-child))
                        (should (timerp
                                 (niwa-session-lease-renewal-timer
                                  retained-child)))
                        (should (equal new-save-path
                                       (niwa-session-save-path
                                        retained-child)))
                        (should
                         (cl-find new-save-path
                                  (niwa-session-persistence-list-sessions
                                   (niwa-session-workspace session))
                                  :key (lambda (entry)
                                         (plist-get entry :save-path))
                                  :test #'equal)))
                       ((eq scenario 'move-post-commit)
                        (should-not (file-directory-p new-save-path))
                        (should (file-directory-p staging-path))
                        (should
                         (equal staging-path
                                (niwa-session-save-path session)))
                        (should
                         (cl-find staging-path
                                  (niwa-session-persistence-list-sessions
                                   (niwa-session-workspace session))
                                  :key (lambda (entry)
                                         (plist-get entry :save-path))
                                  :test #'equal)))
                       (t
                        (should (file-directory-p new-save-path))))
                      (unless (eq scenario 'adopt-verify)
                        (should
                         (niwa-session-publication-read
                          (niwa-session-save-path session)))
                        (should
                         (string-match-p "clone" (buffer-name buffer)))
                        (should
                         (string-match-p "clone" (buffer-name view-buffer)))
                        (should
                         (niwa-session-durability-lease-owned-p session)))))
                  (when recovery
                    (should-not (file-exists-p recovery)))))))
        (niwa-test--with-local-shell-tramp (list host)
          (when (and session (niwa-session-save-path session))
            (ignore-errors
              (niwa-session-persistence-lock-release
               (niwa-session-save-path session) session)))
          (when old-save-path
            (ignore-errors
              (niwa-session-persistence-lock-release old-save-path)))
          (when retained-child
            (ignore-errors
              (niwa-session-persistence-lock-release
               (niwa-session-save-path retained-child) retained-child)))
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
        (niwa-workspace-clear-registry)))))

(provide 'test-niwa-session-save-as)
;;; test-niwa-session-save-as.el ends here
