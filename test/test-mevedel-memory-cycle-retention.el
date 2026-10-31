;;; test-mevedel-memory-cycle-retention.el --- Cycle proof retention -*- lexical-binding: t -*-

;;; Commentary:

;; Real cleanup must retain a reserved attempt even before preparation exists.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-journal-cleanup)
(require 'mevedel-journal-gc)
(require 'mevedel-journal-store)
(require 'mevedel-memory-cycle)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-scope)
(require 'mevedel-memory-store)
(require 'mevedel-session-control-fs)
(require 'mevedel-workspace-identity)

(mevedel-deftest mevedel-journal-gc/cycle-proof
    (:vars* ((root (make-temp-file "mevedel-cycle-gc-" t))
             (workspace (mevedel-workspace--create :root root))
             (mevedel-memory-dirs nil)
             (mevedel-journal-cleanup--inhibit-scheduling t)
             (key (mevedel-memory-store--claim-directory workspace))
             (path (file-name-concat root ".mevedel" "shared" "finding.md"))
             reserved unreferenced successor)
     :before-each ((mevedel-workspace-identity-ensure root)
                   (make-directory (mevedel-journal-store-directory root) t)
                   (make-directory (file-name-directory path) t)
                   (write-region "A temporary finding.\n" nil path nil 'silent)
                   (set-file-times path (seconds-to-time (- (float-time) (* 15 86400)))))
     :after-each ((dolist (claim (list reserved unreferenced successor))
                    (when claim (mevedel-journal-claim-settle claim 'cancelled "")))
                  (delete-directory root t)))
  (cl-labels
      ((reserve ()
         ;; Prepare the metadata before the intentionally short-lived claim.
         (let ((cohort (mevedel-memory-notes-cohort workspace nil)))
           (setq reserved (mevedel-journal-claim-acquire key 3))
           (should reserved)
           (mevedel-memory-cycle-admit workspace reserved (lambda () cohort))
           (should (equal reserved (mevedel-memory-cycle-inflight-claim workspace)))
           (should-not (file-exists-p
                        (file-name-concat (mevedel-memory-store--directory workspace (plist-get reserved :owner))
                                          "prepared.el")))
           (should (mevedel-journal-claim-settle reserved 'cancelled ""))
           (setq unreferenced (mevedel-journal-claim-acquire key 3))
           (should unreferenced)
           (should (mevedel-journal-claim-settle unreferenced 'cancelled ""))
           ;; Local fixture clocks and the target native clock share the host.
           (sleep-for (max 0 (- (+ (plist-get unreferenced :expires-at) 0.05) (float-time))))))
       (collect ()
         (mevedel-journal-cleanup-expired workspace t)
         ;; This control proves that cleanup reached actual expired-claim GC.
         (should-not (mevedel-journal-claim-outcome unreferenced)))
       (recover ()
         (setq successor (mevedel-journal-claim-acquire key 120))
         (should successor)
         (mevedel-memory-cycle-recover workspace successor))
       (accept-cycle ()
         (setq reserved (mevedel-journal-claim-acquire key 180))
         (should reserved)
         (let* ((cycle (mevedel-memory-cycle-admit
                        workspace reserved (lambda () (mevedel-memory-notes-cohort workspace nil))))
                (capture (mevedel-memory-notes-capture workspace nil :cohort cycle))
                (scope (mevedel-memory-scope-capture workspace)))
           (plist-put scope :roots (append (plist-get scope :roots) (list (plist-get capture :root))))
           (setq scope (plist-put scope :note-cycle
                                  (list :id (plist-get cycle :id) :cohort-hash (plist-get cycle :cohort-hash))))
           (mevedel-memory-cycle-stage workspace reserved cycle :capture capture)
           (let* ((prepared (mevedel-memory-store-prepare workspace reserved scope nil ""))
                  (notes (mapcar (lambda (document) (plist-get document :id))
                                 (mevedel-memory-notes-documents scope)))
                  (reply (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n"
                                 "## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes.")))
             (cl-letf (((symbol-function 'mevedel-journal-store-timestamp) (lambda (_) "2000-01-01T00:00:00Z")))
               (let* ((accepted (mevedel-memory-store-accept workspace prepared reply nil "test:model" nil notes))
                      (proposal (car (plist-get accepted :proposals))))
                 (should (= 1 (length (plist-get accepted :admitted-notes))))
                 (should (= 1 (length (plist-get accepted :proposals))))
                 (should (eq 'note-delete (plist-get proposal :action)))
                 (prog1 (mevedel-memory-store-publish workspace (plist-get prepared :id))
                   ;; Resolve disposal so only the unreconciled cycle pins history.
                   (should (eq 'rejected (plist-get (mevedel-memory-decision-reject
                                                    workspace (plist-get prepared :id) (plist-get proposal :id)
                                                    :reason "Retain the fixture note")
                                                   :status))))))))))
    ,test)
  (test)
  :doc "a pre-preparation reservation survives ordinary GC, then releases its sole proof"
  (progn
    (reserve)
    (collect)
    (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome reserved) :status)))
    (let ((cycle (recover)))
      (should (eq 'paused (plist-get cycle :status)))
      (should (eq 'cancelled (plist-get cycle :error)))
      (should-not (plist-get cycle :inflight)))
    (should (mevedel-journal-claim-settle successor 'completed ""))
    (mevedel-journal-cleanup-expired workspace t)
    (should-not (mevedel-journal-claim-outcome reserved))
    (should (equal "A temporary finding.\n" (mevedel-session-control-fs-read-file path))))
  :doc "without the cycle reference, GC destroys the predecessor and recovery refuses it"
  (progn
    (reserve)
    (cl-letf (((symbol-function 'mevedel-memory-cycle-inflight-claim) (lambda (_) nil)))
      (collect))
    (should-not (mevedel-journal-claim-outcome reserved))
    (let ((failure (should-error (recover))))
      (should (string-match-p "predecessor is not fenced" (error-message-string failure))))
    (should (equal reserved (mevedel-memory-cycle-inflight-claim workspace)))
    (should (equal "A temporary finding.\n" (mevedel-session-control-fs-read-file path))))
  :doc "accepted no-action input stays pinned after disposal rejection until reconciliation"
  (let* ((entry (accept-cycle))
         (pass (plist-get entry :pass-id))
         (mevedel-memory-history-max-age-days 0))
    (should (mevedel-memory-cycle-pins-pass-p workspace pass))
    (should (= 0 (mevedel-journal-cleanup-expired workspace t)))
    (should-not (mevedel-journal-cleanup-pass-retired-p root pass))
    (should (= 1 (length (plist-get (mevedel-memory-store-accepted workspace pass) :admitted-notes))))
    (let ((cycle (recover)))
      (should (eq 'complete (plist-get cycle :status)))
      (should (= 1 (length (plist-get cycle :attempted))))
      (should-not (plist-get cycle :inflight)))
    (should (mevedel-journal-claim-settle successor 'completed ""))
    ;; Retirement deletes the review and its terminal rejection decision.
    (should (= 2 (mevedel-journal-cleanup-expired workspace t)))
    (should (mevedel-journal-cleanup-pass-retired-p root pass))
    (should-error (mevedel-memory-store-accepted workspace pass))
    (should (equal "A temporary finding.\n" (mevedel-session-control-fs-read-file path)))))

(provide 'test-mevedel-memory-cycle-retention)
;;; test-mevedel-memory-cycle-retention.el ends here
