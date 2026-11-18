;;; test-mevedel-memory-decision-targets.el -- Memory target lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise actual TRAMP writes and recovery with target and client-local roots.

;;; Code:

(require 'mevedel-workspace-identity)
(require 'mevedel-session-control-fs)
(require 'mevedel-memory-decision)
(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-pass)
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "mevedel-execution-test-helpers"))
(require 'mevedel-system)

(mevedel-deftest mevedel-memory-decision-apply/targets
    (:vars* ((directory (make-temp-file "mevedel-memory-target-" t))
             (provisioned (cond ((getenv "MEVEDEL_TEST_PODMAN_ROOT")
                                 (test-mevedel-execution-remote--real-root "MEVEDEL_TEST_PODMAN_ROOT" 'podman))
                                ((getenv "MEVEDEL_TEST_SSH_ROOT")
                                 (test-mevedel-execution-remote--real-root "MEVEDEL_TEST_SSH_ROOT" 'ssh))))
             (remote-directory (when provisioned (test-mevedel-execution-remote--real-temp-directory provisioned "memory-decisions-")))
             memory
             (workspace nil) (scope nil) (accepted nil) (item nil)
             (pass nil) (claim nil) (mevedel-memory-dirs nil))
     :after-each ((when remote-directory
                    (unwind-protect (delete-directory remote-directory t)
                      (tramp-cleanup-connection (tramp-dissect-file-name remote-directory) nil t)))
                  (delete-directory directory t)))
  (mevedel-test--with-local-shell-tramp '("memory-decisions")
    (let ((remote (or remote-directory (format "/mevedelmock:memory-decisions:%s/" directory))))
      (setq workspace (mevedel-workspace--create :root remote))
      (mevedel-workspace-identity-ensure remote)
      (cl-labels
          ((prepare (&optional client-local)
             (setq memory (file-name-concat (if client-local directory remote) "memory"))
             (make-directory memory)
             (write-region "Original topic.\n" nil (file-name-concat memory "topic.md") nil 'silent)
             (write-region "- [Topic](topic.md) - old\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
             (setq mevedel-memory-dirs (if client-local (list memory) '("memory"))
                   scope (mevedel-memory-scope-capture workspace)
                   claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
             (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
                    (reply (format (concat "## Promote\n- none\n## Update\n```proposal\nroot: %S\nfile: \"topic.md\"\n"
                                           "type: \"project\"\ntitle: \"Topic\"\nhook: \"current\"\nreason: \"Evidence\"\n"
                                           "evidence: []\n---\nNew topic.\n```\n## Merge\n- none\n## Remove\n- none\n"
                                           "## Instructions\n- none\n## No action\n- none") (caar (plist-get scope :roots)))))
               (setq pass (plist-get prepared :id)
                     accepted (mevedel-memory-store-accept workspace prepared reply nil "test:model" nil)
                     item (car (plist-get accepted :proposals)))
               (mevedel-memory-store-publish workspace pass)))
           (write-pending ()
             (let (reached)
               (cl-letf (((symbol-function 'mevedel-memory-decision--elect)
                          (lambda (_ws owner &rest _)
                            (should (equal (file-remote-p remote) (file-remote-p (process-get owner 'lock))))
                            (setq reached t) (error "Interrupted before remote election"))))
                 (should-error (mevedel-memory-decision-apply workspace pass (plist-get item :id))))
               (should reached)
               (should (string-suffix-p "New topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
               (should-not (mevedel-memory-decision-status workspace (plist-get item :id))))))
        (unwind-protect ,test
          (when claim (mevedel-journal-claim-settle claim 'cancelled ""))))))
  (test)
  :doc "remote topic and index writes reconcile after interruption and reverse on the same target"
  (progn
    (prepare)
    (should-not (plist-get (cdar (plist-get scope :roots)) :client))
    (write-pending)
    (cl-letf (((symbol-function 'mevedel-workspace-identity-client) (lambda () (make-string 64 ?f))))
      (let ((decision (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision)))
        (should (eq 'applied (plist-get decision :status)))
        (should (equal decision (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision))))
      (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :status))))
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
    (should (equal "- [Topic](topic.md) - old\n" (mevedel-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))
    (should (= 3 (length (mevedel-journal-store-entries remote)))))
  :doc "a remote journal cannot rebind its client-local memory writes to another client"
  (progn
    (prepare t)
    (write-pending)
    (cl-letf (((symbol-function 'mevedel-workspace-identity-client) (lambda () (make-string 64 ?f))))
      (should-error (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :type 'mevedel-memory-scope-unavailable)
      (should-error (mevedel-memory-decision-reject workspace pass (plist-get item :id)) :type 'mevedel-memory-scope-unavailable))
    (should-not (mevedel-memory-decision-status workspace (plist-get item :id)))
    (should (eq 'rejected (plist-get (mevedel-memory-decision-reject workspace pass (plist-get item :id)) :status)))
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md")))))
  :doc "remote completed history expires after reversal without modifying the restored files"
  (progn
    (prepare)
    (should (eq 'applied (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :status)))
    (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :status)))
    (let ((mevedel-memory-history-max-age-days 0)
          (now (mevedel-session-control-fs-target-time remote)))
      (cl-letf (((symbol-function 'mevedel-session-control-fs-target-time) (lambda (_) (+ now 1))))
        (should (= 3 (plist-get (mevedel-journal-cleanup-expired workspace) :count)))))
    (should-not (mevedel-journal-store-entries remote))
    (should-not (mevedel-memory-decision--slots workspace (plist-get item :id)))
    (mevedel-memory-decision-recover workspace)
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))))

(provide 'test-mevedel-memory-decision-targets)
;;; test-mevedel-memory-decision-targets.el ends here
