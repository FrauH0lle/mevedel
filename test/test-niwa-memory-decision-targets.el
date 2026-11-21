;;; test-niwa-memory-decision-targets.el -- Memory target lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise actual TRAMP writes and recovery with target and client-local roots.

;;; Code:

(require 'niwa-workspace-identity)
(require 'niwa-session-control-fs)
(require 'niwa-memory-decision)
(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-memory-pass)
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "niwa-execution-test-helpers"))
(require 'niwa-system)

(niwa-deftest niwa-memory-decision-apply/targets
    (:vars* ((directory (make-temp-file "niwa-memory-target-" t))
             (provisioned (cond ((getenv "NIWA_TEST_PODMAN_ROOT")
                                 (test-niwa-execution-remote--real-root "NIWA_TEST_PODMAN_ROOT" 'podman))
                                ((getenv "NIWA_TEST_SSH_ROOT")
                                 (test-niwa-execution-remote--real-root "NIWA_TEST_SSH_ROOT" 'ssh))))
             (remote-directory (when provisioned (test-niwa-execution-remote--real-temp-directory provisioned "memory-decisions-")))
             memory
             (workspace nil) (scope nil) (accepted nil) (item nil)
             (pass nil) (claim nil) (niwa-memory-dirs nil))
     :after-each ((when remote-directory
                    (unwind-protect (delete-directory remote-directory t)
                      (tramp-cleanup-connection (tramp-dissect-file-name remote-directory) nil t)))
                  (delete-directory directory t)))
  (niwa-test--with-local-shell-tramp '("memory-decisions")
    (let ((remote (or remote-directory (format "/niwamock:memory-decisions:%s/" directory))))
      (setq workspace (niwa-workspace--create :root remote))
      (niwa-workspace-identity-ensure remote)
      (cl-labels
          ((prepare (&optional client-local)
             (setq memory (file-name-concat (if client-local directory remote) "memory"))
             (make-directory memory)
             (write-region "Original topic.\n" nil (file-name-concat memory "topic.md") nil 'silent)
             (write-region "- [Topic](topic.md) - old\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
             (setq niwa-memory-dirs (if client-local (list memory) '("memory"))
                   scope (niwa-memory-scope-capture workspace)
                   claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
             (let* ((prepared (niwa-memory-store-prepare workspace claim scope nil ""))
                    (reply (format (concat "## Promote\n- none\n## Update\n```proposal\nroot: %S\nfile: \"topic.md\"\n"
                                           "type: \"project\"\ntitle: \"Topic\"\nhook: \"current\"\nreason: \"Evidence\"\n"
                                           "evidence: []\n---\nNew topic.\n```\n## Merge\n- none\n## Remove\n- none\n"
                                           "## Instructions\n- none\n## No action\n- none") (caar (plist-get scope :roots)))))
               (setq pass (plist-get prepared :id)
                     accepted (niwa-memory-store-accept workspace prepared reply nil "test:model" nil)
                     item (car (plist-get accepted :proposals)))
               (niwa-memory-store-publish workspace pass)))
           (write-pending ()
             (let (reached)
               (cl-letf (((symbol-function 'niwa-memory-decision--elect)
                          (lambda (_ws owner &rest _)
                            (should (equal (file-remote-p remote) (file-remote-p (process-get owner 'lock))))
                            (setq reached t) (error "Interrupted before remote election"))))
                 (should-error (niwa-memory-decision-apply workspace pass (plist-get item :id))))
               (should reached)
               (should (string-suffix-p "New topic.\n" (niwa-session-control-fs-read-file (file-name-concat memory "topic.md"))))
               (should-not (niwa-memory-decision-status workspace (plist-get item :id))))))
        (unwind-protect ,test
          (when claim (niwa-journal-claim-settle claim 'cancelled ""))))))
  (test)
  :doc "remote topic and index writes reconcile after interruption and reverse on the same target"
  (progn
    (prepare)
    (should-not (plist-get (cdar (plist-get scope :roots)) :client))
    (write-pending)
    (cl-letf (((symbol-function 'niwa-workspace-identity-client) (lambda () (make-string 64 ?f))))
      (let ((decision (plist-get (niwa-memory-decision-apply workspace pass (plist-get item :id)) :decision)))
        (should (eq 'applied (plist-get decision :status)))
        (should (equal decision (plist-get (niwa-memory-decision-apply workspace pass (plist-get item :id)) :decision))))
      (should (eq 'reversed (plist-get (niwa-memory-decision-reverse workspace pass (plist-get item :id)) :status))))
    (should (equal "Original topic.\n" (niwa-session-control-fs-read-file (file-name-concat memory "topic.md"))))
    (should (equal "- [Topic](topic.md) - old\n" (niwa-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))
    (should (= 3 (length (niwa-journal-store-entries remote)))))
  :doc "a remote journal cannot rebind its client-local memory writes to another client"
  (progn
    (prepare t)
    (write-pending)
    (cl-letf (((symbol-function 'niwa-workspace-identity-client) (lambda () (make-string 64 ?f))))
      (should-error (niwa-memory-decision-apply workspace pass (plist-get item :id)) :type 'niwa-memory-scope-unavailable)
      (should-error (niwa-memory-decision-reject workspace pass (plist-get item :id)) :type 'niwa-memory-scope-unavailable))
    (should-not (niwa-memory-decision-status workspace (plist-get item :id)))
    (should (eq 'rejected (plist-get (niwa-memory-decision-reject workspace pass (plist-get item :id)) :status)))
    (should (equal "Original topic.\n" (niwa-session-control-fs-read-file (file-name-concat memory "topic.md")))))
  :doc "remote completed history expires after reversal without modifying the restored files"
  (progn
    (prepare)
    (should (eq 'applied (plist-get (niwa-memory-decision-apply workspace pass (plist-get item :id)) :status)))
    (should (eq 'reversed (plist-get (niwa-memory-decision-reverse workspace pass (plist-get item :id)) :status)))
    (let ((niwa-memory-history-max-age-days 0)
          (now (niwa-session-control-fs-target-time remote)))
      (cl-letf (((symbol-function 'niwa-session-control-fs-target-time) (lambda (_) (+ now 1))))
        (should (= 3 (plist-get (niwa-journal-cleanup-expired workspace) :count)))))
    (should-not (niwa-journal-store-entries remote))
    (should-not (niwa-memory-decision--slots workspace (plist-get item :id)))
    (niwa-memory-decision-recover workspace)
    (should (equal "Original topic.\n" (niwa-session-control-fs-read-file (file-name-concat memory "topic.md"))))))

(provide 'test-niwa-memory-decision-targets)
;;; test-niwa-memory-decision-targets.el ends here
