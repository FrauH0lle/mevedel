;;; test-mevedel-journal-gc.el --- Bounded coordination retention -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise cleanup through the workspace lifecycle with real control files.

;;; Code:
(require 'helpers
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "helpers"))
(require 'mevedel-journal-cleanup)
(require 'mevedel-journal-expiry-test-support
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "mevedel-journal-expiry-test-support"))
(require 'mevedel-memory-store)

(mevedel-deftest mevedel-journal-cleanup-coordination ()
  ,test
  (test)
  :doc "drains two full batches and a partial batch of obsolete pairs without recall expiry"
  (let* ((root (make-temp-file "mevedel-coordination-gc-" t))
         (workspace (mevedel-workspace--create :root root))
         (directory (mevedel-journal-store-claim-directory (mevedel-journal-store-directory root) 'mutation))
         (mevedel-journal-max-age-days nil)
         (mevedel-journal-cleanup--more nil))
    (unwind-protect
        (progn
          (make-directory directory t)
          (dotimes (index 401)
            (let ((record (list :generation (1+ index) :owner (format "%064x" (1+ index)) :expires-at 1)))
              (write-region (json-serialize record) nil
                            (file-name-concat directory (format "%020d.claim" (1+ index))) nil 'silent)
              (write-region (json-serialize (append record '(:status "completed" :payload ""))) nil
                            (file-name-concat directory (format "%020d.outcome" (1+ index))) nil 'silent)))
          ;; Every cleanup adds one live claim.  The first two remove exactly
          ;; 200 expired pairs each; the last removes the sole remainder.
          (cl-loop for remaining in '(202 3 3)
                   for generation from 402
                   for more in '(t t nil)
                   do (setq mevedel-journal-cleanup--more nil)
                   (mevedel-journal-cleanup-expired workspace t)
                   (should (eq more mevedel-journal-cleanup--more))
                   (should (= remaining (length (directory-files directory nil "\\.claim\\'"))))
                   (should (= remaining (length (directory-files directory nil "\\.outcome\\'"))))
                   (should (= generation (plist-get (mevedel-journal-claim-current directory) :generation)))
                   ;; Earlier cleanup owners are unexpired negative controls.
                   (should (file-exists-p (file-name-concat directory "00000000000000000402.claim"))))
          (setq mevedel-journal-cleanup--more nil)
          (mevedel-journal-cleanup-expired workspace t)
          (should-not mevedel-journal-cleanup--more))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-cleanup-coordination-continuation ()
  ,test
  (test)
  :doc "collecting previous cleanup claims does not force another full scan"
  (let* ((root (make-temp-file "mevedel-cleanup-continuation-" t))
         (workspace (mevedel-workspace--create :root root))
         (journal (mevedel-journal-store-directory root))
         (mevedel-journal-cleanup--more nil))
    (unwind-protect
        (progn
          (dolist (scope '(mutation digest-run consolidation))
            (let* ((directory (mevedel-journal-store-claim-directory journal scope))
                   (record (list :generation 1 :owner (make-string 64 ?a) :expires-at 1)))
              (make-directory directory t)
              (write-region (json-serialize record) nil
                            (file-name-concat directory "00000000000000000001.claim") nil 'silent)
              (write-region (json-serialize (append record '(:status "completed" :payload ""))) nil
                            (file-name-concat directory "00000000000000000001.outcome") nil 'silent)))
          (should (= 0 (mevedel-journal-cleanup-expired workspace t)))
          (dolist (scope '(mutation digest-run consolidation))
            (let ((directory (mevedel-journal-store-claim-directory journal scope)))
              (should-not (file-exists-p (file-name-concat directory "00000000000000000001.claim")))
              (should (= 2 (plist-get (mevedel-journal-claim-current directory) :generation)))))
          (should-not mevedel-journal-cleanup--more))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-cleanup-private-coordination
    (:vars* ((root (make-temp-file "mevedel-private-gc-" t))
             (workspace (mevedel-workspace--create :root root))
             (mevedel-memory-dirs nil))
     :after-each ((delete-directory root t)))
  (progn (mevedel-workspace-identity-ensure root) ,test)
  (test)
  :doc "accepted history retains its expired proof while obsolete admission records disappear"
  (let* ((claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 120))
         (prepared (mevedel-memory-store-prepare workspace claim (mevedel-memory-scope-capture workspace) nil "")))
    (mevedel-memory-store-accept-proposals workspace prepared '(:proposals nil :no-action "- none") nil "test:model" nil)
    (mevedel-memory-store-publish workspace (plist-get prepared :id))
    (mevedel-test-journal-with-expired-claims (list claim)
      (should (= 0 (mevedel-journal-cleanup-expired workspace t)))
      (should (mevedel-memory-store-accepted workspace (plist-get prepared :id)))
      (should (mevedel-journal-claim-outcome claim))))
  :doc "failed preparations lose their payload and proof after the diagnostic window"
  (let* ((mevedel-memory-history-max-age-days 0)
         (claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 120))
         (prepared (mevedel-memory-store-prepare workspace claim (mevedel-memory-scope-capture workspace) nil "")))
    (mevedel-journal-claim-settle claim 'failed "No result")
    (should-not (file-exists-p (mevedel-journal-store-directory root)))
    ;; Without advancing the native clock, the diagnostic payload survives.
    (mevedel-journal-cleanup-expired workspace t)
    (should (file-exists-p (mevedel-memory-store--directory workspace (plist-get prepared :id))))
    (should (mevedel-journal-claim-outcome claim))
    (mevedel-test-journal-with-expired-claims (list claim)
      (mevedel-journal-cleanup-expired workspace t)
      (should-not (file-exists-p (mevedel-memory-store--directory workspace (plist-get prepared :id))))
      (should-not (mevedel-journal-claim-outcome claim))))
  :doc "finished expiry manifests are collected without reviving retired history"
  (let* ((mevedel-memory-history-max-age-days 0)
         (claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 120))
         (prepared (mevedel-memory-store-prepare workspace claim (mevedel-memory-scope-capture workspace) nil ""))
         (journal (mevedel-journal-store-directory root)))
    (mevedel-memory-store-accept-proposals workspace prepared '(:proposals nil :no-action "- none") nil "test:model" nil)
    (mevedel-memory-store-publish workspace (plist-get prepared :id))
    (should (= 1 (mevedel-journal-cleanup-expired workspace t)))
    (let ((mutation (mevedel-journal-claim-current (mevedel-journal-store-claim-directory journal 'mutation))))
      (mevedel-journal-cleanup-expired workspace t)
      (should (mevedel-session-control-fs-list-directory (file-name-concat (mevedel-journal-store-state-directory journal) "expiry") "\\.json"))
      (mevedel-test-journal-with-expired-claims (list mutation)
        (mevedel-journal-cleanup-expired workspace t)))
    (should-not (mevedel-session-control-fs-list-directory (file-name-concat (mevedel-journal-store-state-directory journal) "expiry") "\\.json"))
    (should (mevedel-journal-cleanup-pass-retired-p root (plist-get prepared :id)))
    (mevedel-memory-store-recover workspace)
    (should-not (mevedel-journal-store-entries root))))

(provide 'test-mevedel-journal-gc)
;;; test-mevedel-journal-gc.el ends here
