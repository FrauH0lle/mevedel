;;; test-niwa-loading.el --- Fresh feature boundary tests -*- lexical-binding: t -*-

;;; Commentary:
;; Each source and compiled probe starts without umbrella-preloaded fixtures.

;;; Code:
(require 'ert)
(require 'cl-lib)

(defun niwa-loading--emacs (form)
  "Evaluate FORM in a fresh batch Emacs; return its exit status and output.
A child that outlives two minutes is killed and fails the test."
  (with-temp-buffer
    (let ((process
           (make-process
            :name "niwa-loading" :buffer (current-buffer) :noquery t
            :connection-type 'pipe :sentinel #'ignore
            :command (list (expand-file-name invocation-name invocation-directory)
                           "--batch" "-Q" "--eval" (prin1-to-string form)))))
      (with-timeout (120 (delete-process process)
                         (ert-fail (format "Child Emacs timed out: %s" (buffer-string))))
        (while (process-live-p process) (accept-process-output process 0.05)))
      (accept-process-output process 0)
      (list (process-exit-status process) (buffer-string)))))

(ert-deftest niwa-loading/features ()
  (should (string-match-p "niwa-test-home-" (getenv "HOME")))
  (let* ((root (file-name-directory (locate-library "niwa")))
         (fixture (file-name-concat root "test/fixtures/loading.el"))
         (compiled (make-temp-file "niwa-loading-compiled-" t))
         (state (make-temp-file "niwa-loading-session-" t)))
    (cl-flet ((run (entry scenario &optional state)
                (pcase-let ((`(,status ,output)
                             (niwa-loading--emacs
                              `(progn
                                 (setq load-path ',(cons entry load-path))
                                 (load ,fixture nil t)
                                 (niwa-test-loading ',scenario ,state)))))
                  (ert-info ((format "%s %s: %s" entry scenario output))
                    (should (equal 0 status))
                    (should (equal "" output))))))
      (unwind-protect
          (progn
            ;; Compile every file in one process, as package managers do, so
            ;; earlier files' definitions are known while later ones compile.
            ;; Source siblings preserve ordinary prompt and resource resolution.
            (dolist (file (directory-files root t "\\`niwa.*\\.el\\'"))
              (make-symbolic-link file (file-name-concat compiled (file-name-nondirectory file))))
            (pcase-let ((`(,status ,output)
                         (niwa-loading--emacs
                          `(progn
                             (setq load-path ',(cons compiled load-path))
                             (require 'bytecomp)
                             ;; Terminal-only Emacs builds lack this primitive.
                             (fmakunbound 'font-info)
                             (let ((byte-compile-error-on-warn t)
                                   (byte-compile-verbose nil))
                               (dolist (file (directory-files ,compiled t "\\.el\\'"))
                                 (unless (byte-compile-file file)
                                   (error "Compilation failed: %s" file))))))))
              (ert-info (output)
                (should (equal 0 status))
                (should (equal "" output))))
            (dolist (entry (list root compiled))
              (dolist (scenario '(commands stop-shares installation handoff-progress request-progress
                                  artifacts gptel chat chat-in-directory tools renderers
                                  live-execution claude worker-teardown native-btw lobby room
                                  notes note-lifecycle))
                (run entry scenario))
              ;; The first send saves a session that a second Emacs resumes.
              (run entry 'send state)
              (run entry 'resume state)
              ;; A restored lobby opens that saved session for a guest.
              (run entry 'lobby-open state)
              (delete-directory state t)
              (make-directory state)))
        (delete-directory compiled t)
        (delete-directory state t)))))

(ert-deftest niwa-loading/deferred-tables ()
  "Deferred table cases initialize their view without earlier composer cases."
  (pcase-let ((`(,status ,output)
               (niwa-loading--emacs
                `(progn
                   (setq load-path ',load-path)
                   (load ,(file-name-concat
                           (file-name-directory (locate-library "niwa"))
                           "test/test-niwa-view-table-deferred.el") nil t)
                   (ert-run-tests-batch-and-exit
                    "^niwa-view-table-rerender/deferred/test@")))))
    (ert-info (output)
      (should (equal 0 status))
      (should (string-match-p "5 results as expected, 0 unexpected" output)))))

(ert-deftest niwa-loading/completion-verb ()
  "The completion classifier loads on first use from the notice, breadcrumb and projection."
  (pcase-let ((`(,status ,output)
               (niwa-loading--emacs
                `(progn
                   (setq load-path ',load-path)
                   (require 'niwa-collaboration-projection)
                   (require 'niwa-view-audit)
                   (when (featurep 'niwa-tool-exec)
                     (error "Classifier module loaded before first use"))
                   (let ((notice (niwa-view--mail-notice
                                  "<agent-message type=\"MONITOR_TERMINAL\" description=\"w\" termination=\"exited\">"
                                  'agent-message "/root" ""))
                         (record (niwa-collaboration--forwarded-execution-record
                                  '(:owner "/root" :execution-id "e"
                                    :facts (:kind monitor :termination exited)))))
                     (unless (and (string-prefix-p "Monitor \"w\" ended" (car notice))
                                  (equal "ended" (plist-get record :verb))
                                  (equal "Ended" (niwa-view-audit-breadcrumb-status
                                                  '(:facts (:termination exited)))))
                       (error "Unexpected verbs: %S %S" notice record)))
                   (princ "ok")))))
    (ert-info (output)
      (should (equal 0 status))
      (should (equal "ok" output)))))

(ert-deftest niwa-loading/viewport-entries ()
  (dolist (entry '(niwa-view-zone niwa-view-viewport niwa-view-composer))
    (pcase-let ((`(,status ,output)
                 (niwa-loading--emacs
                  `(progn
                     (setq load-path ',load-path)
                     (require ',entry)
                     (with-temp-buffer
                       (switch-to-buffer (current-buffer))
                       (pcase ',entry
                         ('niwa-view-zone
                          (cl-assert (not (featurep 'niwa-view-viewport)))
                          (let ((start (point-marker)))
                            (niwa-view-zone-reconcile
                             'status start start '((:namespace status :id row :body "Status"))))
                          (cl-assert (string-search "Status" (buffer-string))))
                         ('niwa-view-viewport
                          (niwa-view-viewport-apply
                           (niwa-view-viewport-capture nil)
                           (list (list (selected-window) 1 :lost-p nil))))
                         ('niwa-view-composer
                          (niwa-view-call-preserving-user-view-state
                           (lambda () (insert "written")))
                          (cl-assert (equal "written" (buffer-string))))))
                     (cl-assert (featurep 'niwa-view-viewport))
                     ,(unless (eq entry 'niwa-view-composer)
                        '(cl-assert (not (featurep 'niwa-view-composer))))))))
      (ert-info ((format "Cold %s: %s" entry output))
        (should (zerop status))
        (should (equal output ""))))))

(provide 'test-niwa-loading)
;;; test-niwa-loading.el ends here
