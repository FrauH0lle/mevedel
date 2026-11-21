;;; test-niwa-session-discovery-race.el --- Discovery interleavings -*- lexical-binding: t; -*-

;;; Commentary:

;; Read portable publications while another client advances the lease head.

;;; Code:

(require 'niwa-session-artifacts)
(require 'niwa-session-durability)
(require 'niwa-session-control-fs)
(require 'niwa-session-persistence)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-session-persistence-list-sessions/lease-successor ()
  ,test
  (test)
  :doc "discovers the publication when a successor prunes the listed lease"
  (dolist (consumer '(batch enumeration))
    (dolist (scenario '(successor malformed redirected churn))
      (ert-info ((format "Discovery %s successor: %s" consumer scenario))
        (let* ((root (make-temp-file "niwa-discovery-race-" t))
               (workspace (niwa-workspace--create
                           :type 'project :id root :root root :name "discovery"))
               (session (niwa-session-create "main" workspace))
               (listing (symbol-function 'niwa-session-persistence--lease-listings))
               (read-records (symbol-function 'niwa-session-durability--read-records))
               (lease-reads 0)
               injected in-discovery)
          (unwind-protect
              (with-temp-buffer
                (org-mode)
                (setq-local niwa--session session)
                (insert "Published transcript\n")
                (niwa-session-artifacts-save session (current-buffer))
                (cl-labels
                    ((successor ()
                       (let ((directory (niwa-session-save-path session))
                             (was-in-discovery in-discovery))
                         (setq in-discovery nil)
                         (unwind-protect
                             (progn
                               (let ((niwa-session-durability--client-id
                                      (plist-get (niwa-session-lease session) :client-id)))
                                 (niwa-session-durability-lease-release directory session))
                               (let ((niwa-session-durability--client-id (make-string 64 ?b)))
                                 (should (niwa-session-durability-lease-acquire
                                          directory "successor" session)))
                               (niwa-session-durability--generation-path
                                (niwa-session-durability--lease-path directory)
                                (plist-get (niwa-session-lease session) :generation)))
                           (setq in-discovery was-in-discovery)))))
                  (cl-letf
                      (((symbol-function 'niwa-session-persistence--lease-listings)
                        (lambda (entries)
                          (prog1 (funcall listing entries)
                            (unless injected
                              (setq injected t)
                              ;; Commit a successor after real names were read.
                              (let ((path (successor)))
                                (pcase scenario
                                  ('malformed
                                   (with-temp-file path (insert "(:corrupted t)")))
                                  ('redirected (make-symbolic-link root path t))))))))
                       ((symbol-function 'niwa-session-durability--read-records)
                        (lambda (paths)
                          (when (and in-discovery paths
                                     (string-match-p "/\\.lease/" (car paths)))
                            (cl-incf lease-reads)
                            (when (and (eq scenario 'churn) (= lease-reads 2))
                              ;; The one refreshed set also disappears.  This
                              ;; must report instability, not retry forever.
                              (successor)))
                          (funcall read-records paths))))
                    (setq in-discovery t)
                    (let* ((directory (niwa-session-save-path session))
                           (result
                            (if (eq consumer 'batch)
                                (cdar (niwa-session-publication-read-batch
                                       (list directory)
                                       (niwa-session-persistence--lease-listings
                                        (list directory))))
                              (niwa-session-persistence--enumerate-sessions workspace)))
                           (diagnostic
                            (if (eq consumer 'batch)
                                (when-let* ((err (plist-get result :error)))
                                  (error-message-string err))
                              (let ((bad (car (plist-get result :incompatible))))
                                (when bad
                                  (should (eq 'undiscoverable (plist-get bad :status)))
                                  (plist-get bad :diagnostic))))))
                      (should injected)
                      (if (eq scenario 'successor)
                          (progn
                            (should-not diagnostic)
                            (should
                             (equal
                              (niwa-session-session-id session)
                              (if (eq consumer 'batch)
                                  (plist-get (plist-get result :sidecar-data) :session-id)
                                (plist-get (plist-get (car (plist-get result :sessions))
                                                      :summary) :session-id)))))
                        (when (eq consumer 'enumeration)
                          (should-not (plist-get result :sessions)))
                        (should
                         (string-match-p
                          (pcase scenario
                            ('malformed "Invalid portable session lease")
                            ('redirected "Portable control operation failed")
                            ('churn "Portable session lease changed during discovery"))
                          diagnostic)))
                      (when (eq scenario 'churn) (should (= lease-reads 2)))))))
            (niwa-session-durability--cancel-renewal session)
            (delete-directory root t)))))))

(niwa-deftest niwa-session-publication-read/lease-successor ()
  ,test
  (test)
  :doc "reads the same publication after a successor prunes the listed lease"
  (let* ((root (make-temp-file "niwa-scalar-discovery-race-" t))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "discovery"))
         (session (niwa-session-create "main" workspace))
         (read-records (symbol-function 'niwa-session-durability--read-records))
         listed-paths result)
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local niwa--session session)
          (insert "Published transcript\n")
          (niwa-session-artifacts-save session (current-buffer))
          (let* ((directory (niwa-session-save-path session))
                 (lease-directory (niwa-session-durability--lease-path directory))
                 (before (niwa-session-publication-read directory))
                 (head (plist-get before :head))
                 (generation (plist-get (niwa-session-lease session) :generation)))
            (should head)
            (cl-letf
                (((symbol-function 'niwa-session-durability--read-records)
                  (lambda (paths &optional strict)
                    (when (and (not listed-paths) paths
                               (equal (file-name-as-directory lease-directory)
                                      (file-name-directory (car paths))))
                      ;; Capture real paths before rotation; also guard reentry.
                      (setq listed-paths (copy-sequence paths))
                      (dolist (path listed-paths)
                        (should (file-regular-p path)))
                      (let ((niwa-session-durability--client-id
                             (plist-get (niwa-session-lease session) :client-id)))
                        (niwa-session-durability-lease-release directory session))
                      (let ((niwa-session-durability--client-id (make-string 64 ?b)))
                        (should (niwa-session-durability-lease-acquire
                                 directory "successor" session)))
                      (should (> (plist-get (niwa-session-lease session) :generation)
                                 generation))
                      (should (equal (make-string 64 ?b)
                                     (plist-get (niwa-session-lease session) :client-id)))
                      (should (equal head (plist-get (niwa-session-lease session)
                                                     :publication-head)))
                      (dolist (path listed-paths)
                        (should-not (file-exists-p path))))
                    (funcall read-records paths strict))))
              (setq result (niwa-session-publication-read directory)))
            (should listed-paths)
            ;; Check the uninjected control before asserting the raced result.
            (let ((fresh (niwa-session-publication-read directory)))
              (should fresh)
              (should (equal head (plist-get fresh :head)))
              (ert-info ((format "Pruned %d listed generations; fresh read retains head %s"
                                 (length listed-paths) (plist-get fresh :head)))
                (should result)
                (should (equal head (plist-get result :head)))))))
      (niwa-session-durability--cancel-renewal session)
      (delete-directory root t))))

(niwa-deftest niwa-session-durability--resolve-lease-head/interleavings ()
  ,test
  (test)
  :doc "refreshes once across scalar, shared resolver and asynchronous readers"
  (dolist (consumer '(scalar resolver async))
    (dolist (scenario '(normal successor churn redirected malformed unchanged-malformed older))
      (ert-info ((format "Head reader %s: %s" consumer scenario))
        (let* ((root (make-temp-file "niwa-head-reader-" t))
               (workspace (niwa-workspace--create
                           :type 'project :id root :root root :name "discovery"))
               (session (niwa-session-create "main" workspace))
               (async-program (symbol-function 'niwa-session-durability--async-program))
               (run-program (symbol-function 'niwa-session-control-fs-run-program))
               (run-async (symbol-function 'niwa-session-control-fs-run-program-async))
               (reads 0) (listings 0) (callbacks 0)
               rotating result failure done job context)
          (unwind-protect
              (with-temp-buffer
                (org-mode)
                (setq-local niwa--session session)
                (insert "Published transcript\n")
                (niwa-session-artifacts-save session (current-buffer))
                (niwa-session-durability--cancel-renewal session)
                (let* ((directory (niwa-session-save-path session))
                       (lease-directory (niwa-session-durability--lease-path directory))
                       (head (plist-get (niwa-session-publication-read directory) :head))
                       (paths (niwa-session-durability--generation-paths lease-directory)))
                  (should head)
                  (pcase scenario
                    ('unchanged-malformed (with-temp-file (car paths) (insert "(")))
                    ('older
                     (with-temp-file
                         (niwa-session-durability--generation-path lease-directory 6000)
                       (insert "("))))
                  (cl-labels
                      ((successor ()
                         (setq rotating t)
                         (unwind-protect
                             (progn
                               (let ((niwa-session-durability--client-id
                                      (plist-get (niwa-session-lease session) :client-id)))
                                 (niwa-session-durability-lease-release directory session))
                               (let ((niwa-session-durability--client-id (make-string 64 ?b)))
                                 (should (niwa-session-durability-lease-acquire
                                          directory "successor" session)))
                               (niwa-session-durability--cancel-renewal session)
                               (should (equal head (plist-get (niwa-session-lease session)
                                                              :publication-head)))
                               (niwa-session-durability--generation-path
                                lease-directory (plist-get (niwa-session-lease session) :generation)))
                           (setq rotating nil)))
                       (before-read (observed)
                         (when (and (not rotating) observed
                                    (equal (file-name-as-directory lease-directory)
                                           (file-name-directory (car observed))))
                           (cl-incf reads)
                           (when (or (and (= reads 1)
                                          (memq scenario '(successor churn redirected malformed)))
                                     (and (= reads 2) (eq scenario 'churn)))
                             (dolist (path observed) (should (file-regular-p path)))
                             (let ((path (successor)))
                               (pcase scenario
                                 ('redirected (make-symbolic-link root path t))
                                 ('malformed (with-temp-file path (insert "(")))))
                             (dolist (path observed) (should-not (file-exists-p path))))))
                       (before-program (operations)
                         (unless rotating
                           (pcase (plist-get (car operations) :op)
                             ('list-directory
                              (when (equal lease-directory (plist-get (car operations) :path))
                                (cl-incf listings)))
                             ('read (before-read (mapcar (lambda (op) (plist-get op :path))
                                                         operations)))))))
                    (cl-letf
                        (((symbol-function 'niwa-session-control-fs-run-program)
                          (lambda (&rest args)
                            (when (and (eq consumer 'async) (not rotating))
                              (ert-fail "Synchronous target program in an async reader"))
                            (before-program (car args))
                            (apply run-program args)))
                         ((symbol-function 'niwa-session-control-fs-run-program-async)
                          (lambda (&rest args)
                            (before-program (car args))
                            (let ((process (apply run-async args)))
                              (should (processp process))
                              process)))
                         ((symbol-function 'niwa-session-durability--async-program)
                          (lambda (path operations actual-job actual-context continue &optional lock)
                            (should (eq job actual-job))
                            (should (eq context actual-context))
                            (funcall async-program path operations actual-job actual-context
                                     continue lock))))
                      (condition-case err
                          (pcase consumer
                            ('scalar (setq result (niwa-session-publication-read directory)))
                            ('resolver
                             (let ((observed (niwa-session-durability--generation-paths
                                              lease-directory)))
                               (setq result (niwa-session-durability--resolve-lease-head
                                             lease-directory observed
                                             (niwa-session-durability--read-records observed)))))
                            ('async
                             (setq context (niwa-session-durability-fresh-context nil)
                                   job (niwa-session-durability--async-job
                                        (lambda (value error)
                                          (cl-incf callbacks)
                                          (setq result value failure error done t))))
                             (niwa-session-durability--lease-head-async
                              lease-directory nil job context
                              (lambda (value) (funcall (car job) value nil)))
                             (let ((steps 0))
                               (while (and (not done) (< (cl-incf steps) 500))
                                 (accept-process-output nil 0.01)))
                             (should done)
                             (accept-process-output nil 0.02)
                             (should (= callbacks 1))))
                        (error (setq failure err))))
                    (should (= reads (if (memq scenario '(successor churn malformed)) 2 1)))
                    (should (= listings (if (memq scenario '(normal older)) 1 2)))
                    (pcase scenario
                      ((or 'normal 'successor 'older)
                       (should-not failure)
                       (should (equal head (plist-get result (if (eq consumer 'scalar)
                                                                :head :publication-head)))))
                      ('unchanged-malformed
                       (should-not failure)
                       (should-not result))
                      (_
                       (should failure)
                       (should (string-match-p
                                (if (eq scenario 'redirected)
                                    "Portable control operation failed"
                                  "Portable session lease changed during discovery")
                                (error-message-string failure))))))))
            (niwa-session-durability--cancel-renewal session)
            (delete-directory root t)))))))

(provide 'test-niwa-session-discovery-race)
;;; test-niwa-session-discovery-race.el ends here
