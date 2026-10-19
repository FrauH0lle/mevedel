;;; test-mevedel-session-artifacts-admission.el --- Admission fences -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise combined control observations through the mutation admission gate,
;; with real leases, transfer records, recovery markers and reentrant grants.

;;; Code:

(require 'mevedel-session-artifacts)
(require 'mevedel-session-transfer)
(require 'mevedel-session-recovery)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-session-artifacts-assert-new-mutation-authority ()
  ,test
  (test)
  :doc "admits owned work but fences grants, releases, recovery and mixed controls"
  (dolist (scenario '(fresh fresh-recovery fresh-mixed fresh-redirected-recovery
                     quiet request grant grant-during-check release-during-check
                     plain-release-during-check
                     grant-during-observation recovery mixed redirected-recovery))
    (ert-info ((format "Admission scenario: %s" scenario))
      (let* ((root (make-temp-file "mevedel-admission-" t))
             (session-dir (file-name-concat root "session"))
             (workspace (mevedel-workspace--create
                         :type 'project :id root :root root :name "admission"))
             (session (mevedel-session-create "main" workspace))
             (mevedel-session-durability--client-id (make-string 64 ?a))
             (check (symbol-function
                     'mevedel-session-artifacts-check-target-incarnation))
             (read-record (symbol-function 'mevedel-session-durability--read-plist))
             injected)
        (unwind-protect
            (with-temp-buffer
              (setq-local mevedel--session session)
              (make-directory session-dir t)
              (setf (mevedel-session-save-path session) session-dir
                    (mevedel-session-session-id session) "admission-session")
              (unless (memq scenario '(fresh fresh-recovery fresh-mixed
                                      fresh-redirected-recovery))
                (should (mevedel-session-durability-lease-acquire
                         session-dir (buffer-name) session)))
              (when (memq scenario '(request grant grant-during-check
                                             release-during-check
                                             grant-during-observation))
                (let ((mevedel-session-durability--client-id (make-string 64 ?b)))
                  (should (mevedel-session-transfer-request session)))
                (when (eq scenario 'grant)
                  (mevedel-session-transfer-decide session 'grant)
                  ;; A reopened owner must hydrate the durable grant, not rely
                  ;; solely on a transient quiescing flag.
                  (setf (mevedel-session-control-transfer session) nil)))
              (pcase scenario
                ((or 'recovery 'fresh-recovery)
                 (make-directory (file-name-concat session-dir ".recovery/undo") t)
                 (with-temp-file (file-name-concat session-dir
                                                  ".recovery/recovery-ab.el")
                   (prin1 '(:version 1 :kind rewind :directory ".recovery/undo"
                            :reason "incomplete rewind") (current-buffer))))
                ((or 'mixed 'fresh-mixed)
                 (with-temp-file (file-name-concat session-dir ".lock")
                   (insert "mixed authority")))
                ((or 'redirected-recovery 'fresh-redirected-recovery)
                 (make-symbolic-link root (file-name-concat session-dir ".recovery"))))
              (cl-letf (((symbol-function
                          'mevedel-session-artifacts-check-target-incarnation)
                         (lambda (&rest args)
                           (apply check args)
                           (when (and (not injected)
                                      (memq scenario '(grant-during-check
                                                       release-during-check
                                                       plain-release-during-check)))
                             (setq injected t)
                             (if (eq scenario 'plain-release-during-check)
                                 (mevedel-session-durability-lease-release session-dir session)
                               (mevedel-session-transfer-decide session 'grant)
                               (when (eq scenario 'release-during-check)
                                 (mevedel-session-transfer-release session))))))
                        ((symbol-function 'mevedel-session-durability--read-plist)
                         (lambda (path)
                           (prog1 (funcall read-record path)
                             ;; Return the older absent decision after another
                             ;; callback committed a grant for that request.
                             (when (and (eq scenario 'grant-during-observation)
                                        (not injected)
                                        (string-prefix-p "decision-"
                                                         (file-name-nondirectory path)))
                               (setq injected t)
                               (mevedel-session-transfer-decide session 'grant))))))
                (if (memq scenario '(fresh quiet request))
                    (should (mevedel-session-artifacts-assert-new-mutation-authority
                             session))
                  (let ((err (should-error
                              (mevedel-session-artifacts-assert-new-mutation-authority
                               session))))
                    ;; An unexecuted batched probe is not evidence of absence.
                    ;; First admission must still run each mandatory proof.
                    (pcase scenario
                      ('fresh-recovery
                       (should (string-match-p "pending publication"
                                               (error-message-string err))))
                      ('fresh-mixed
                       (should (string-match-p "has a PID lock"
                                               (error-message-string err))))
                      ('fresh-redirected-recovery
                       (should-not (string-match-p "skipped"
                                                   (error-message-string err))))))))
              (when (eq scenario 'fresh)
                (should (mevedel-session-durability-lease-owned-p session)))
              (when (memq scenario '(grant-during-check release-during-check
                                    grant-during-observation plain-release-during-check))
                (should injected))
              (when (eq scenario 'plain-release-during-check)
                (should-not (mevedel-session-lease session)))
              (when (memq scenario '(grant grant-during-check grant-during-observation))
                (should (eq 'quiescing
                            (plist-get (mevedel-session-control-transfer session) :state)))
                ;; Admission observes the grant but must not settle/release it.
                (should (mevedel-session-durability-lease-owned-p session)))
              (when (eq scenario 'release-during-check)
                (should (eq 'released
                            (plist-get (mevedel-session-control-transfer session) :state))))
              (when (memq scenario '(recovery fresh-recovery))
                (should (plist-get (mevedel-session-pending-publication session)
                                   :manual-recovery-marker))))
          (mevedel-session-durability--cancel-renewal session)
          (delete-directory root t))))))

(mevedel-deftest mevedel-session-artifacts-assert-new-mutation-authority/nested-refresh ()
  ,test
  (test)
  :doc "a nested requested refresh cannot hide the outer poll's timeout grant"
  (let* ((root (make-temp-file "mevedel-admission-refresh-" t))
         (session-dir (file-name-concat root "session"))
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "admission"))
         (session (mevedel-session-create "main" workspace))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (read-record (symbol-function 'mevedel-session-durability--read-plist))
         injected)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (make-directory session-dir t)
          (setf (mevedel-session-save-path session) session-dir
                (mevedel-session-session-id session) "admission-session")
          (should (mevedel-session-durability-lease-acquire
                   session-dir (buffer-name) session))
          (let ((mevedel-session-durability--client-id (make-string 64 ?b)))
            (should (mevedel-session-transfer-request session)))
          (mevedel-session-transfer-poll session)
          (let* ((transfer (mevedel-session-control-transfer session))
                 (now (+ 2 (plist-get transfer :observed-at)))
                 (mevedel-session-transfer-prompt-timeout 1))
            (should (eq 'requested (plist-get transfer :state)))
            (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                       (lambda (&rest _) now))
                      ((symbol-function 'mevedel-session-durability--read-plist)
                       (lambda (path)
                         (prog1 (funcall read-record path)
                           (when (and (not injected)
                                      (string-prefix-p "decision-"
                                                       (file-name-nondirectory path)))
                             (setq injected t)
                             ;; This callback observes the same request before
                             ;; its deadline; the outer callback is timed out.
                             (let ((mevedel-session-transfer-prompt-timeout 60))
                               (mevedel-session-transfer-poll session))
                             (should (eq 'requested
                                         (plist-get (mevedel-session-control-transfer
                                                     session) :state))))))))
              (should-error
               (mevedel-session-artifacts-assert-new-mutation-authority session)
               :type 'user-error))
            (should injected)
            (let ((current (mevedel-session-control-transfer session)))
              (should (eq 'quiescing (plist-get current :state)))
              (should (eq 'grant (plist-get (plist-get current :decision) :decision)))
              (should (equal
                       (plist-get current :decision)
                       (funcall read-record
                                (mevedel-session-transfer--decision-path
                                 (mevedel-session-durability--lease-path session-dir)
                                 (plist-get (plist-get current :request) :generation))))))))
      (mevedel-session-durability--cancel-renewal session)
      (delete-directory root t))))

(provide 'test-mevedel-session-artifacts-admission)
;;; test-mevedel-session-artifacts-admission.el ends here
