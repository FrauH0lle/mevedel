;;; test-mevedel-journal-gc.el --- Bounded coordination retention -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise cleanup through the workspace lifecycle with real control files.

;;; Code:
(require 'helpers
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "helpers"))
(require 'mevedel-journal-cleanup)
(require 'mevedel-journal-gc)
(require 'mevedel-journal-worker)
(require 'mevedel-journal-expiry-test-support
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "mevedel-journal-expiry-test-support"))
(require 'mevedel-memory-store)

(defun mevedel-test-gc--write-expired-pairs (directory count)
  "Write COUNT expired, settled claim and outcome pairs into DIRECTORY."
  (make-directory directory t)
  (dotimes (index count)
    (let ((record (list :generation (1+ index) :owner (format "%064x" (1+ index)) :expires-at 1)))
      (write-region (json-serialize record) nil
                    (file-name-concat directory (format "%020d.claim" (1+ index))) nil 'silent)
      (write-region (json-serialize (append record '(:status "completed" :payload ""))) nil
                    (file-name-concat directory (format "%020d.outcome" (1+ index))) nil 'silent))))

(defun mevedel-test-gc--drive (root pending)
  "Fire idle cleanup timers for ROOT in PENDING until no job remains."
  (let ((deadline (+ (float-time) 15)))
    (while (and (gethash root pending) (< (float-time) deadline))
      (let ((timer (car (gethash root pending))))
        (when (memq timer timer-idle-list)
          (cancel-timer timer)
          (apply (timer--function timer) (timer--args timer))))
      (sleep-for .002))
    (should-not (gethash root pending))))

(mevedel-deftest mevedel-journal-cleanup-coordination ()
  ,test
  (test)
  :doc "drains two full batches and a partial batch of obsolete pairs without recall expiry"
  (let* ((root (make-temp-file "mevedel-coordination-gc-" t))
         (workspace (mevedel-workspace--create :root root))
         (directory (mevedel-journal-store-claim-directory (mevedel-journal-store-directory root) 'mutation))
         (mevedel-journal-max-age-days nil)
         (mevedel-journal-gc--claim-pair-limit 2))
    (unwind-protect
        (progn
          (make-directory directory t)
          (mevedel-test-gc--write-expired-pairs directory 5)
          ;; Every cleanup adds one live claim.  The first two remove exactly
          ;; the bound of expired pairs each; the last removes the sole remainder.
          (cl-loop for remaining in '(4 3 3)
                   for generation from 6
                   for more in '(t t nil)
                   for result = (mevedel-journal-cleanup-expired workspace t)
                   do (should (equal (list :count 0 :more more) result))
                   (should (= remaining (length (directory-files directory nil "\\.claim\\'"))))
                   (should (= remaining (length (directory-files directory nil "\\.outcome\\'"))))
                   (should (= generation (plist-get (mevedel-journal-claim-current directory) :generation)))
                   ;; Earlier cleanup owners are unexpired negative controls.
                   (should (file-exists-p (file-name-concat directory "00000000000000000006.claim"))))
          ;; Nothing remains to collect, yet the batch ran: a result, not nil.
          (should (equal '(:count 0 :more nil) (mevedel-journal-cleanup-expired workspace t))))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-cleanup-schedule/continuation ()
  (let ((mevedel-journal-worker--child-p t)) ,test)
  (test)
  :doc "an in-process batch with work left reschedules itself until a batch reports none"
  (let* ((root (make-temp-file "mevedel-cleanup-reschedule-" t))
         (workspace (mevedel-workspace--create :root root))
         (directory (mevedel-journal-store-claim-directory (mevedel-journal-store-directory root) 'mutation))
         (mevedel-journal-max-age-days nil)
         (mevedel-journal-gc--claim-pair-limit 2)
         (mevedel-journal-cleanup--inhibit-scheduling nil)
         (mevedel-journal-cleanup--pending (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (mevedel-test-gc--write-expired-pairs directory 5)
          (mevedel-journal-cleanup-schedule workspace t)
          (mevedel-test-gc--drive root mevedel-journal-cleanup--pending)
          ;; Three batches ran: two with continuations and a short last one.
          (should (= 8 (plist-get (mevedel-journal-claim-current directory) :generation)))
          (should (= 3 (length (directory-files directory nil "\\.claim\\'")))))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-cleanup-schedule/worker-continuation ()
  ,test
  (test)
  :doc "a worker child's continuation makes the parent run a forced successor"
  (let* ((root (make-temp-file "mevedel-cleanup-worker-more-" t))
         (workspace (mevedel-workspace--create :root root))
         (directory (mevedel-journal-store-claim-directory (mevedel-journal-store-directory root) 'mutation))
         (mevedel-journal-max-age-days nil)
         (mevedel-journal-gc--claim-pair-limit 2)
         (mevedel-journal-worker--child-p nil)
         (mevedel-journal-cleanup--inhibit-scheduling nil)
         (mevedel-journal-cleanup--pending (make-hash-table :test #'equal))
         results payloads)
    (unwind-protect
        (progn
          (mevedel-test-gc--write-expired-pairs directory 5)
          (should (mevedel-journal-worker-local-p workspace))
          ;; The child entry point runs for real; only process spawning is
          ;; replaced, delivering its result to the parent's own callback.
          ;; A child has its own activity table and schedules nothing.
          (cl-letf (((symbol-function 'mevedel-journal-worker-start)
                     (lambda (ws _operation payload callback &rest _)
                       (push payload payloads)
                       (let ((result (let ((mevedel-journal-worker--child-p t)
                                           (mevedel-journal-cleanup--active (make-hash-table :test #'equal))
                                           (mevedel-journal-cleanup--inhibit-scheduling t))
                                       (mevedel-journal-cleanup-child ws payload))))
                         (push result results)
                         (funcall callback result))
                       nil)))
            (mevedel-journal-cleanup-schedule workspace t)
            (mevedel-test-gc--drive root mevedel-journal-cleanup--pending))
          (should (equal '(t t nil) (nreverse (mapcar (lambda (r) (plist-get r :more)) results))))
          (should (equal '((:force t) (:force t) (:force t)) payloads))
          (should (= 8 (plist-get (mevedel-journal-claim-current directory) :generation))))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-cleanup-child ()
  ,test
  (test)
  :doc "reports a batch's count and continuation, and a non-run as no count"
  (let* ((root (make-temp-file "mevedel-cleanup-child-" t))
         (workspace (mevedel-workspace--create :root root))
         (directory (mevedel-journal-store-claim-directory (mevedel-journal-store-directory root) 'mutation))
         (mevedel-journal-max-age-days nil)
         (mevedel-journal-gc--claim-pair-limit 2)
         (mevedel-journal-worker--child-p t))
    (unwind-protect
        (progn
          (mevedel-test-gc--write-expired-pairs directory 5)
          (let ((ran (mevedel-journal-cleanup-child workspace '(:force t))))
            (should (plist-get ran :ok))
            (should (= 0 (plist-get ran :count)))
            (should (eq t (plist-get ran :more)))
            (should (numberp (plist-get ran :cleanup-at))))
          ;; Within the hourly gate and unforced, no batch runs at all.
          (let ((throttled (mevedel-journal-cleanup-child workspace nil)))
            (should (plist-get throttled :ok))
            (should-not (plist-get throttled :count))
            (should-not (plist-get throttled :more))))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-cleanup-coordination-continuation ()
  ,test
  (test)
  :doc "collecting previous cleanup claims does not force another full scan"
  (let* ((root (make-temp-file "mevedel-cleanup-continuation-" t))
         (workspace (mevedel-workspace--create :root root))
         (journal (mevedel-journal-store-directory root)))
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
          (should (equal '(:count 0 :more nil) (mevedel-journal-cleanup-expired workspace t)))
          (dolist (scope '(mutation digest-run consolidation))
            (let ((directory (mevedel-journal-store-claim-directory journal scope)))
              (should-not (file-exists-p (file-name-concat directory "00000000000000000001.claim")))
              (should (= 2 (plist-get (mevedel-journal-claim-current directory) :generation))))))
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
      (should (= 0 (plist-get (mevedel-journal-cleanup-expired workspace t) :count)))
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
    (should (= 1 (plist-get (mevedel-journal-cleanup-expired workspace t) :count)))
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
