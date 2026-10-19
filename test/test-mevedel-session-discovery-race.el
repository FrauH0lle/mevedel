;;; test-mevedel-session-discovery-race.el --- Discovery interleavings -*- lexical-binding: t; -*-

;;; Commentary:

;; Read portable publications while another client advances the lease head.

;;; Code:

(require 'mevedel-session-artifacts)
(require 'mevedel-session-durability)
(require 'mevedel-session-persistence)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-session-persistence-list-sessions/lease-successor ()
  ,test
  (test)
  :doc "discovers the publication when a successor prunes the listed lease"
  (dolist (consumer '(batch enumeration))
    (dolist (scenario '(successor malformed redirected churn))
      (ert-info ((format "Discovery %s successor: %s" consumer scenario))
        (let* ((root (make-temp-file "mevedel-discovery-race-" t))
               (workspace (mevedel-workspace--create
                           :type 'project :id root :root root :name "discovery"))
               (session (mevedel-session-create "main" workspace))
               (listing (symbol-function 'mevedel-session-persistence--lease-listings))
               (read-records (symbol-function 'mevedel-session-durability--read-records))
               (lease-reads 0)
               injected in-discovery)
          (unwind-protect
              (with-temp-buffer
                (org-mode)
                (setq-local mevedel--session session)
                (insert "Published transcript\n")
                (mevedel-session-artifacts-save session (current-buffer))
                (cl-labels
                    ((successor ()
                       (let ((directory (mevedel-session-save-path session))
                             (was-in-discovery in-discovery))
                         (setq in-discovery nil)
                         (unwind-protect
                             (progn
                               (let ((mevedel-session-durability--client-id
                                      (plist-get (mevedel-session-lease session) :client-id)))
                                 (mevedel-session-durability-lease-release directory session))
                               (let ((mevedel-session-durability--client-id (make-string 64 ?b)))
                                 (should (mevedel-session-durability-lease-acquire
                                          directory "successor" session)))
                               (mevedel-session-durability--generation-path
                                (mevedel-session-durability--lease-path directory)
                                (plist-get (mevedel-session-lease session) :generation)))
                           (setq in-discovery was-in-discovery)))))
                  (cl-letf
                      (((symbol-function 'mevedel-session-persistence--lease-listings)
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
                       ((symbol-function 'mevedel-session-durability--read-records)
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
                    (let* ((directory (mevedel-session-save-path session))
                           (result
                            (if (eq consumer 'batch)
                                (cdar (mevedel-session-publication-read-batch
                                       (list directory)
                                       (mevedel-session-persistence--lease-listings
                                        (list directory))))
                              (mevedel-session-persistence--enumerate-sessions workspace)))
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
                              (mevedel-session-session-id session)
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
            (mevedel-session-durability--cancel-renewal session)
            (delete-directory root t)))))))

(provide 'test-mevedel-session-discovery-race)
;;; test-mevedel-session-discovery-race.el ends here
