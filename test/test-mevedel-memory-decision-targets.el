;;; test-mevedel-memory-decision-targets.el -- Memory target lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise actual TRAMP writes and recovery with target and client-local roots.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-pass)
(require 'mevedel-system)

(mevedel-deftest mevedel-memory-decision-recover-write
    (:vars* ((directory (make-temp-file "mevedel-memory-target-" t))
             (memory (file-name-concat directory "memory"))
             (workspace nil) (scope nil) (accepted nil) (item nil)
             (pass nil) (claim nil) (intent nil) (mevedel-memory-dirs nil))
     :after-each ((delete-directory directory t)))
  (mevedel-test--with-local-shell-tramp '("memory-decisions")
    (let ((remote (format "/mevedelmock:memory-decisions:%s/" directory)))
      (setq workspace (mevedel-workspace--create :root remote))
      (mevedel-workspace-identity-ensure remote)
      (make-directory memory)
      (write-region "Original topic.\n" nil (file-name-concat memory "topic.md") nil 'silent)
      (write-region "- [Topic](topic.md) - old\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
      (cl-labels
          ((prepare (&optional client-local)
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
             (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
             (mevedel-memory-write-call
              scope (plist-get item :root)
              (lambda (target)
                (setq intent (mevedel-memory-write-prepare workspace claim target accepted item))
                (mevedel-memory-write-run workspace claim target intent)))
             (mevedel-journal-claim-settle claim 'cancelled "")))
        (unwind-protect ,test
          (when claim (mevedel-journal-claim-settle claim 'cancelled ""))))))
  (test)
  :doc "remote topic and index writes reconcile after interruption and reverse on the same target"
  (progn
    (prepare)
    (should-not (plist-get (cdar (plist-get scope :roots)) :client))
    (write-pending)
    (should (file-exists-p (mevedel-memory-write--pin intent)))
    (cl-letf (((symbol-function 'mevedel-workspace-identity-client) (lambda () (make-string 64 ?f))))
      (let ((decision (mevedel-memory-decision-recover-write workspace (plist-get intent :id) (plist-get intent :hash))))
        (should (eq 'applied (plist-get decision :status)))
        (should (equal decision (mevedel-memory-decision-apply workspace pass (plist-get item :id)))))
      (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :status))))
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
    (should (equal "- [Topic](topic.md) - old\n" (mevedel-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))
    (should-not (file-exists-p (mevedel-memory-write--pin intent)))
    (should (= 3 (length (mevedel-journal-store-entries remote)))))
  :doc "a remote journal cannot rebind its client-local memory writes to another client"
  (progn
    (prepare t)
    (write-pending)
    (let ((pin (mevedel-memory-write--pin intent)))
      (cl-letf (((symbol-function 'mevedel-workspace-identity-client) (lambda () (make-string 64 ?f))))
        (should-error (mevedel-memory-decision-apply workspace pass (plist-get item :id))
                      :type 'mevedel-memory-scope-unavailable)
        (should-error (mevedel-memory-decision-recover-write workspace (plist-get intent :id) (plist-get intent :hash))
                      :type 'mevedel-memory-scope-unavailable)
        (should (file-exists-p pin))))
    (should (eq 'applied (plist-get (mevedel-memory-decision-recover-write workspace (plist-get intent :id) (plist-get intent :hash)) :status)))
    (should-not (file-exists-p (mevedel-memory-write--pin intent))))
  :doc "remote completed history expires after reversal without modifying the restored files"
  (progn
    (prepare)
    (should (eq 'applied (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :status)))
    (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :status)))
    (let ((mevedel-memory-history-max-age-days 0)
          (now (mevedel-session-control-fs-target-time remote)))
      (cl-letf (((symbol-function 'mevedel-session-control-fs-target-time) (lambda (_) (+ now 1))))
        (should (= 3 (mevedel-journal-cleanup-expired workspace)))))
    (should-not (mevedel-journal-store-entries remote))
    (should-not (mevedel-memory-write-list workspace))
    (mevedel-memory-decision-recover-pending workspace)
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))))

(provide 'test-mevedel-memory-decision-targets)
;;; test-mevedel-memory-decision-targets.el ends here
