;;; test-niwa-memory-cycle-retention.el --- Cycle proof retention -*- lexical-binding: t -*-

;;; Commentary:

;; Real cleanup must retain a reserved attempt even before preparation exists.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-journal-cleanup)
(require 'niwa-journal-gc)
(require 'niwa-journal-expiry-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-expiry-test-support"))
(require 'niwa-journal-store)
(require 'niwa-memory-cycle)
(require 'niwa-memory-decision)
(require 'niwa-memory-notes)
(require 'niwa-memory-scope)
(require 'niwa-memory-store)
(require 'niwa-session-control-fs)
(require 'niwa-workspace-identity)

(niwa-deftest niwa-journal-gc/cycle-proof
    (:vars* ((root (make-temp-file "niwa-cycle-gc-" t))
             (workspace (niwa-workspace--create :root root))
             (niwa-memory-dirs nil)
             (niwa-journal-cleanup--inhibit-scheduling t)
             (key (niwa-memory-store--claim-directory workspace))
             (path (file-name-concat root ".niwa" "shared" "finding.md"))
             reserved unreferenced successor)
     :before-each ((niwa-workspace-identity-ensure root)
                   (make-directory (niwa-journal-store-directory root) t)
                   (make-directory (file-name-directory path) t)
                   (write-region "A temporary finding.\n" nil path nil 'silent)
                   (set-file-times path (seconds-to-time (- (float-time) (* 15 86400)))))
     :after-each ((dolist (claim (list reserved unreferenced successor))
                    (when claim (niwa-journal-claim-settle claim 'cancelled "")))
                  (delete-directory root t)))
  (cl-labels
      ((reserve ()
         ;; Prepare metadata while the authentic reservation is still live.
         (let ((cohort (niwa-memory-notes-cohort workspace nil)))
           (setq reserved (niwa-journal-claim-acquire key 120))
           (should reserved)
           (niwa-memory-cycle-admit workspace reserved (lambda () cohort))
           (should (equal reserved (niwa-memory-cycle-inflight-claim workspace)))
           (should-not (file-exists-p
                        (file-name-concat (niwa-memory-store--directory workspace (plist-get reserved :owner))
                                          "prepared.el")))
           (should (niwa-journal-claim-settle reserved 'cancelled ""))
           (setq unreferenced (niwa-journal-claim-acquire key 120))
           (should unreferenced)
           (should (niwa-journal-claim-settle unreferenced 'cancelled ""))
           ;; The same cleanup without clock advancement must retain both.
           (niwa-journal-cleanup-expired workspace t)
           (should (niwa-journal-claim-outcome reserved))
           (should (niwa-journal-claim-outcome unreferenced))))
       (collect ()
         (niwa-journal-cleanup-expired workspace t)
         ;; This control proves that cleanup reached actual expired-claim GC.
         (should-not (niwa-journal-claim-outcome unreferenced)))
       (recover ()
         (setq successor (niwa-journal-claim-acquire key 120))
         (should successor)
         (niwa-memory-cycle-recover workspace successor))
       (accept-cycle ()
         (setq reserved (niwa-journal-claim-acquire key 180))
         (should reserved)
         (let* ((cycle (niwa-memory-cycle-admit
                        workspace reserved (lambda () (niwa-memory-notes-cohort workspace nil))))
                (capture (niwa-memory-notes-capture workspace nil :cohort cycle))
                (scope (niwa-memory-scope-capture workspace)))
           (plist-put scope :roots (append (plist-get scope :roots) (list (plist-get capture :root))))
           (setq scope (plist-put scope :note-cycle
                                  (list :id (plist-get cycle :id) :cohort-hash (plist-get cycle :cohort-hash))))
           (niwa-memory-cycle-stage workspace reserved cycle :capture capture)
           (let* ((prepared (niwa-memory-store-prepare workspace reserved scope nil ""))
                  (notes (mapcar (lambda (document) (plist-get document :id))
                                 (niwa-memory-notes-documents scope)))
                  (reply (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n"
                                 "## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes.")))
             (cl-letf (((symbol-function 'niwa-journal-store-timestamp) (lambda (_) "2000-01-01T00:00:00Z")))
               (let* ((accepted (niwa-memory-store-accept workspace prepared reply nil "test:model" nil notes))
                      (proposal (car (plist-get accepted :proposals))))
                 (should (= 1 (length (plist-get accepted :admitted-notes))))
                 (should (= 1 (length (plist-get accepted :proposals))))
                 (should (eq 'note-delete (plist-get proposal :action)))
                 (prog1 (niwa-memory-store-publish workspace (plist-get prepared :id))
                   ;; Resolve disposal so only the unreconciled cycle pins history.
                   (should (eq 'rejected (plist-get (niwa-memory-decision-reject
                                                    workspace (plist-get prepared :id) (plist-get proposal :id)
                                                    :reason "Retain the fixture note")
                                                   :status))))))))))
    ,test)
  (test)
  :doc "a pre-preparation reservation survives ordinary GC, then releases its sole proof"
  (progn
    (reserve)
    (niwa-test-journal-with-expired-claims (list reserved unreferenced)
      (collect))
    (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome reserved) :status)))
    (let ((cycle (recover)))
      (should (eq 'paused (plist-get cycle :status)))
      (should (eq 'cancelled (plist-get cycle :error)))
      (should-not (plist-get cycle :inflight)))
    (should (niwa-journal-claim-settle successor 'completed ""))
    (niwa-test-journal-with-expired-claims (list reserved)
      (niwa-journal-cleanup-expired workspace t))
    (should-not (niwa-journal-claim-outcome reserved))
    (should (equal "A temporary finding.\n" (niwa-session-control-fs-read-file path))))
  :doc "without the cycle reference, GC destroys the predecessor and recovery refuses it"
  (progn
    (reserve)
    (let ((entered nil))
      (niwa-test-journal-with-expired-claims (list reserved unreferenced)
        (cl-letf (((symbol-function 'niwa-memory-cycle-inflight-claim)
                   (lambda (_) (setq entered t) nil)))
          (collect)))
      (should entered))
    (should-not (niwa-journal-claim-outcome reserved))
    (let ((failure (should-error (recover))))
      (should (string-match-p "predecessor is not fenced" (error-message-string failure))))
    (should (equal reserved (niwa-memory-cycle-inflight-claim workspace)))
    (should (equal "A temporary finding.\n" (niwa-session-control-fs-read-file path))))
  :doc "accepted no-action input stays pinned after disposal rejection until reconciliation"
  (let* ((entry (accept-cycle))
         (pass (plist-get entry :pass-id))
         (niwa-memory-history-max-age-days 0))
    (should (niwa-memory-cycle-pins-pass-p workspace pass))
    (should (= 0 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
    (should-not (niwa-journal-cleanup-pass-retired-p root pass))
    (should (= 1 (length (plist-get (niwa-memory-store-accepted workspace pass) :admitted-notes))))
    (let ((cycle (recover)))
      (should (eq 'complete (plist-get cycle :status)))
      (should (= 1 (length (plist-get cycle :attempted))))
      (should-not (plist-get cycle :inflight)))
    (should (niwa-journal-claim-settle successor 'completed ""))
    ;; Retirement deletes the review and its terminal rejection decision.
    (should (= 2 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
    (should (niwa-journal-cleanup-pass-retired-p root pass))
    (should-error (niwa-memory-store-accepted workspace pass))
    (should (equal "A temporary finding.\n" (niwa-session-control-fs-read-file path)))))

(provide 'test-niwa-memory-cycle-retention)
;;; test-niwa-memory-cycle-retention.el ends here
