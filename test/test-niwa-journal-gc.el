;;; test-niwa-journal-gc.el --- Bounded coordination retention -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise cleanup through the workspace lifecycle with real control files.

;;; Code:
(require 'helpers
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "helpers"))
(require 'niwa-journal-cleanup)
(require 'niwa-journal-gc)
(require 'niwa-journal-worker)
(require 'niwa-journal-expiry-test-support
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "niwa-journal-expiry-test-support"))
(require 'niwa-memory-store)

(defun niwa-test-gc--write-expired-pairs (directory count)
  "Write COUNT expired, settled claim and outcome pairs into DIRECTORY."
  (make-directory directory t)
  (dotimes (index count)
    (let ((record (list :generation (1+ index) :owner (format "%064x" (1+ index)) :expires-at 1)))
      (write-region (json-serialize record) nil
                    (file-name-concat directory (format "%020d.claim" (1+ index))) nil 'silent)
      (write-region (json-serialize (append record '(:status "completed" :payload ""))) nil
                    (file-name-concat directory (format "%020d.outcome" (1+ index))) nil 'silent))))

(defun niwa-test-gc--drive (root pending)
  "Fire idle cleanup timers for ROOT in PENDING until no job remains."
  (let ((deadline (+ (float-time) 15)))
    (while (and (gethash root pending) (< (float-time) deadline))
      (let ((timer (car (gethash root pending))))
        (when (memq timer timer-idle-list)
          (cancel-timer timer)
          (apply (timer--function timer) (timer--args timer))))
      (sleep-for .002))
    (should-not (gethash root pending))))

(niwa-deftest niwa-journal-cleanup-coordination ()
  ,test
  (test)
  :doc "drains two full batches and a partial batch of obsolete pairs without recall expiry"
  (let* ((root (make-temp-file "niwa-coordination-gc-" t))
         (workspace (niwa-workspace--create :root root))
         (directory (niwa-journal-store-claim-directory (niwa-journal-store-directory root) 'mutation))
         (niwa-journal-max-age-days nil)
         (niwa-journal-gc--claim-pair-limit 2))
    (unwind-protect
        (progn
          (make-directory directory t)
          (niwa-test-gc--write-expired-pairs directory 5)
          ;; Every cleanup adds one live claim.  The first two remove exactly
          ;; the bound of expired pairs each; the last removes the sole remainder.
          (cl-loop for remaining in '(4 3 3)
                   for generation from 6
                   for more in '(t t nil)
                   for result = (niwa-journal-cleanup-expired workspace t)
                   do (should (equal (list :count 0 :more more) result))
                   (should (= remaining (length (directory-files directory nil "\\.claim\\'"))))
                   (should (= remaining (length (directory-files directory nil "\\.outcome\\'"))))
                   (should (= generation (plist-get (niwa-journal-claim-current directory) :generation)))
                   ;; Earlier cleanup owners are unexpired negative controls.
                   (should (file-exists-p (file-name-concat directory "00000000000000000006.claim"))))
          ;; Nothing remains to collect, yet the batch ran: a result, not nil.
          (should (equal '(:count 0 :more nil) (niwa-journal-cleanup-expired workspace t))))
      (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup-schedule/continuation ()
  (let ((niwa-journal-worker--child-p t)) ,test)
  (test)
  :doc "an in-process batch with work left reschedules itself until a batch reports none"
  (let* ((root (make-temp-file "niwa-cleanup-reschedule-" t))
         (workspace (niwa-workspace--create :root root))
         (directory (niwa-journal-store-claim-directory (niwa-journal-store-directory root) 'mutation))
         (niwa-journal-max-age-days nil)
         (niwa-journal-gc--claim-pair-limit 2)
         (niwa-journal-cleanup--inhibit-scheduling nil)
         (niwa-journal-cleanup--pending (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (niwa-test-gc--write-expired-pairs directory 5)
          (niwa-journal-cleanup-schedule workspace t)
          (niwa-test-gc--drive root niwa-journal-cleanup--pending)
          ;; Three batches ran: two with continuations and a short last one.
          (should (= 8 (plist-get (niwa-journal-claim-current directory) :generation)))
          (should (= 3 (length (directory-files directory nil "\\.claim\\'")))))
      (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup-schedule/worker-continuation ()
  ,test
  (test)
  :doc "a worker child's continuation makes the parent run a forced successor"
  (let* ((root (make-temp-file "niwa-cleanup-worker-more-" t))
         (workspace (niwa-workspace--create :root root))
         (directory (niwa-journal-store-claim-directory (niwa-journal-store-directory root) 'mutation))
         (niwa-journal-max-age-days nil)
         (niwa-journal-gc--claim-pair-limit 2)
         (niwa-journal-worker--child-p nil)
         (niwa-journal-cleanup--inhibit-scheduling nil)
         (niwa-journal-cleanup--pending (make-hash-table :test #'equal))
         results payloads)
    (unwind-protect
        (progn
          (niwa-test-gc--write-expired-pairs directory 5)
          (should (niwa-journal-worker-local-p workspace))
          ;; The child entry point runs for real; only process spawning is
          ;; replaced, delivering its result to the parent's own callback.
          ;; A child has its own activity table and schedules nothing.
          (cl-letf (((symbol-function 'niwa-journal-worker-start)
                     (lambda (ws _operation payload callback &rest _)
                       (push payload payloads)
                       (let ((result (let ((niwa-journal-worker--child-p t)
                                           (niwa-journal-cleanup--active (make-hash-table :test #'equal))
                                           (niwa-journal-cleanup--inhibit-scheduling t))
                                       (niwa-journal-cleanup-child ws payload))))
                         (push result results)
                         (funcall callback result))
                       nil)))
            (niwa-journal-cleanup-schedule workspace t)
            (niwa-test-gc--drive root niwa-journal-cleanup--pending))
          (should (equal '(t t nil) (nreverse (mapcar (lambda (r) (plist-get r :more)) results))))
          (should (equal '((:force t) (:force t) (:force t)) payloads))
          (should (= 8 (plist-get (niwa-journal-claim-current directory) :generation))))
      (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup-child ()
  ,test
  (test)
  :doc "reports a batch's count and continuation, and a non-run as no count"
  (let* ((root (make-temp-file "niwa-cleanup-child-" t))
         (workspace (niwa-workspace--create :root root))
         (directory (niwa-journal-store-claim-directory (niwa-journal-store-directory root) 'mutation))
         (niwa-journal-max-age-days nil)
         (niwa-journal-gc--claim-pair-limit 2)
         (niwa-journal-worker--child-p t))
    (unwind-protect
        (progn
          (niwa-test-gc--write-expired-pairs directory 5)
          (let ((ran (niwa-journal-cleanup-child workspace '(:force t))))
            (should (plist-get ran :ok))
            (should (= 0 (plist-get ran :count)))
            (should (eq t (plist-get ran :more)))
            (should (numberp (plist-get ran :cleanup-at))))
          ;; Within the hourly gate and unforced, no batch runs at all.
          (let ((throttled (niwa-journal-cleanup-child workspace nil)))
            (should (plist-get throttled :ok))
            (should-not (plist-get throttled :count))
            (should-not (plist-get throttled :more))))
      (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup-coordination-continuation ()
  ,test
  (test)
  :doc "collecting previous cleanup claims does not force another full scan"
  (let* ((root (make-temp-file "niwa-cleanup-continuation-" t))
         (workspace (niwa-workspace--create :root root))
         (journal (niwa-journal-store-directory root)))
    (unwind-protect
        (progn
          (dolist (scope '(mutation digest-run consolidation))
            (let* ((directory (niwa-journal-store-claim-directory journal scope))
                   (record (list :generation 1 :owner (make-string 64 ?a) :expires-at 1)))
              (make-directory directory t)
              (write-region (json-serialize record) nil
                            (file-name-concat directory "00000000000000000001.claim") nil 'silent)
              (write-region (json-serialize (append record '(:status "completed" :payload ""))) nil
                            (file-name-concat directory "00000000000000000001.outcome") nil 'silent)))
          (should (equal '(:count 0 :more nil) (niwa-journal-cleanup-expired workspace t)))
          (dolist (scope '(mutation digest-run consolidation))
            (let ((directory (niwa-journal-store-claim-directory journal scope)))
              (should-not (file-exists-p (file-name-concat directory "00000000000000000001.claim")))
              (should (= 2 (plist-get (niwa-journal-claim-current directory) :generation))))))
      (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup-private-coordination
    (:vars* ((root (make-temp-file "niwa-private-gc-" t))
             (workspace (niwa-workspace--create :root root))
             (niwa-memory-dirs nil))
     :after-each ((delete-directory root t)))
  (progn (niwa-workspace-identity-ensure root) ,test)
  (test)
  :doc "accepted history retains its expired proof while obsolete admission records disappear"
  (let* ((claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 120))
         (prepared (niwa-memory-store-prepare workspace claim (niwa-memory-scope-capture workspace) nil "")))
    (niwa-memory-store-accept-proposals workspace prepared '(:proposals nil :no-action "- none") nil "test:model" nil)
    (niwa-memory-store-publish workspace (plist-get prepared :id))
    (niwa-test-journal-with-expired-claims (list claim)
      (should (= 0 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
      (should (niwa-memory-store-accepted workspace (plist-get prepared :id)))
      (should (niwa-journal-claim-outcome claim))))
  :doc "failed preparations lose their payload and proof after the diagnostic window"
  (let* ((niwa-memory-history-max-age-days 0)
         (claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 120))
         (prepared (niwa-memory-store-prepare workspace claim (niwa-memory-scope-capture workspace) nil "")))
    (niwa-journal-claim-settle claim 'failed "No result")
    (should-not (file-exists-p (niwa-journal-store-directory root)))
    ;; Without advancing the native clock, the diagnostic payload survives.
    (niwa-journal-cleanup-expired workspace t)
    (should (file-exists-p (niwa-memory-store--directory workspace (plist-get prepared :id))))
    (should (niwa-journal-claim-outcome claim))
    (niwa-test-journal-with-expired-claims (list claim)
      (niwa-journal-cleanup-expired workspace t)
      (should-not (file-exists-p (niwa-memory-store--directory workspace (plist-get prepared :id))))
      (should-not (niwa-journal-claim-outcome claim))))
  :doc "finished expiry manifests are collected without reviving retired history"
  (let* ((niwa-memory-history-max-age-days 0)
         (claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 120))
         (prepared (niwa-memory-store-prepare workspace claim (niwa-memory-scope-capture workspace) nil ""))
         (journal (niwa-journal-store-directory root)))
    (niwa-memory-store-accept-proposals workspace prepared '(:proposals nil :no-action "- none") nil "test:model" nil)
    (niwa-memory-store-publish workspace (plist-get prepared :id))
    (should (= 1 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
    (let ((mutation (niwa-journal-claim-current (niwa-journal-store-claim-directory journal 'mutation))))
      (niwa-journal-cleanup-expired workspace t)
      (should (niwa-session-control-fs-list-directory (file-name-concat (niwa-journal-store-state-directory journal) "expiry") "\\.json"))
      (niwa-test-journal-with-expired-claims (list mutation)
        (niwa-journal-cleanup-expired workspace t)))
    (should-not (niwa-session-control-fs-list-directory (file-name-concat (niwa-journal-store-state-directory journal) "expiry") "\\.json"))
    (should (niwa-journal-cleanup-pass-retired-p root (plist-get prepared :id)))
    (niwa-memory-store-recover workspace)
    (should-not (niwa-journal-store-entries root))))

(provide 'test-niwa-journal-gc)
;;; test-niwa-journal-gc.el ends here
