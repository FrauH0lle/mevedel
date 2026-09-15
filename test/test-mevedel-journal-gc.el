;;; test-mevedel-journal-gc.el --- Bounded coordination retention -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise cleanup through the workspace lifecycle with real control files.

;;; Code:
(require 'helpers
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "helpers"))
(require 'mevedel-journal-cleanup)
(require 'mevedel-memory-store)

(mevedel-deftest mevedel-journal-cleanup-coordination ()
  ,test
  (test)
  :doc "drains thousands of obsolete pairs in bounded batches without recall expiry"
  (let* ((root (make-temp-file "mevedel-coordination-gc-" t))
         (workspace (mevedel-workspace--create :root root))
         (directory (mevedel-journal-store-claim-directory (mevedel-journal-store-directory root) 'mutation))
         (mevedel-journal-max-age-days nil))
    (unwind-protect
        (progn
          (make-directory directory t)
          (dotimes (index 1000)
            (let ((record (list :generation (1+ index) :owner (format "%064x" (1+ index)) :expires-at 1)))
              (write-region (json-serialize record) nil
                            (file-name-concat directory (format "%020d.claim" (1+ index))) nil 'silent)
              (write-region (json-serialize (append record '(:status "completed" :payload ""))) nil
                            (file-name-concat directory (format "%020d.outcome" (1+ index))) nil 'silent)))
          (mevedel-journal-cleanup-expired workspace t)
          (should (= 801 (length (directory-files directory nil "\\.claim\\'"))))
          (dotimes (_ 4) (mevedel-journal-cleanup-expired workspace t))
          (should (= 5 (length (directory-files directory nil "\\.claim\\'"))))
          (should (= 1005 (plist-get (mevedel-journal-claim-current directory) :generation))))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-cleanup-private-coordination
    (:vars* ((root (make-temp-file "mevedel-private-gc-" t))
             (workspace (mevedel-workspace--create :root root))
             (mevedel-memory-dirs nil))
     :after-each ((delete-directory root t)))
  (progn (mevedel-workspace-identity-ensure root) ,test)
  (test)
  :doc "accepted history retains its expired proof while obsolete admission records disappear"
  (let* ((claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 2))
         (prepared (mevedel-memory-store-prepare workspace claim (mevedel-memory-scope-capture workspace) nil "")))
    (mevedel-memory-store-accept-proposals workspace prepared '(:proposals nil :no-action "- none") nil "test:model" nil)
    (mevedel-memory-store-publish workspace (plist-get prepared :id))
    (while (< (mevedel-session-control-fs-target-time root) (plist-get claim :expires-at)) (sleep-for 0.02))
    (should (= 0 (mevedel-journal-cleanup-expired workspace t)))
    (should (mevedel-memory-store-accepted workspace (plist-get prepared :id)))
    (should (mevedel-journal-claim-outcome claim)))
  :doc "failed preparations lose their payload and proof after the diagnostic window"
  (let* ((mevedel-memory-history-max-age-days 0)
         (claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 2))
         (prepared (mevedel-memory-store-prepare workspace claim (mevedel-memory-scope-capture workspace) nil "")))
    (mevedel-journal-claim-settle claim 'failed "No result")
    (while (< (mevedel-session-control-fs-target-time root) (plist-get claim :expires-at)) (sleep-for 0.02))
    (mevedel-journal-cleanup-expired workspace t)
    (should-not (file-exists-p (mevedel-memory-store--directory workspace (plist-get prepared :id))))
    (should-not (mevedel-journal-claim-outcome claim)))
  :doc "finished expiry manifests are collected without reviving retired history"
  (let* ((mevedel-memory-history-max-age-days 0)
         (claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 120))
         (prepared (mevedel-memory-store-prepare workspace claim (mevedel-memory-scope-capture workspace) nil ""))
         (acquire (symbol-function 'mevedel-journal-claim-acquire))
         (journal (mevedel-journal-store-directory root)))
    (mevedel-memory-store-accept-proposals workspace prepared '(:proposals nil :no-action "- none") nil "test:model" nil)
    (mevedel-memory-store-publish workspace (plist-get prepared :id))
    (cl-letf (((symbol-function 'mevedel-journal-claim-acquire)
               (lambda (directory _seconds &optional deadline) (funcall acquire directory 3 deadline))))
      (should (= 1 (mevedel-journal-cleanup-expired workspace t))))
    (let ((mutation (mevedel-journal-claim-current (mevedel-journal-store-claim-directory journal 'mutation))))
      (while (< (mevedel-session-control-fs-target-time root) (plist-get mutation :expires-at)) (sleep-for 0.02)))
    (mevedel-journal-cleanup-expired workspace t)
    (should-not (mevedel-session-control-fs-list-directory (file-name-concat journal "state" "expiry") "\\.json"))
    (should (mevedel-journal-cleanup-pass-retired-p root (plist-get prepared :id)))
    (mevedel-memory-store-recover workspace)
    (should-not (mevedel-journal-store-entries root))))

(provide 'test-mevedel-journal-gc)
;;; test-mevedel-journal-gc.el ends here
