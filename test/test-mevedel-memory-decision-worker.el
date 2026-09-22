;;; test-mevedel-memory-decision-worker.el --- Recovery child lifecycle -*- lexical-binding: t -*-
;;; Commentary:
;; Publication recovery runs outside the editor; live checks and ownership stay local.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-decision)

(mevedel-deftest mevedel-memory-decision--start-recovery ()
  (let* ((root (make-temp-file "mevedel-recovery-child-" t))
         (workspace (mevedel-workspace--create :root root))
         (origin (current-buffer))
         job completed tick timer)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-memory-decision-recover)
                   (lambda (&rest _) (ert-fail "Publication recovery ran in editor")))
                  ((symbol-function 'mevedel-memory-store-recover)
                   (lambda (&rest _) (ert-fail "Store recovery ran in editor")))
                  ((symbol-function 'mevedel-memory-decision--recover-writes)
                   (lambda (&rest _) (ert-fail "Empty store inspected a write"))))
          (setq job (mevedel-memory-decision--start-recovery
                     workspace (lambda ()
                                 (should (eq origin (current-buffer)))
                                 (setq completed t)) origin)
                timer (run-at-time 0 nil (lambda () (setq tick t))))
          (should (processp (plist-get job :worker)))
          (should-not completed)
          (with-temp-buffer
            (let ((deadline (+ (float-time) 10)))
              (while (and (not completed) (< (float-time) deadline))
                (accept-process-output nil .01))))
          (should completed)
          (should tick)
          (should-not (memq (plist-get job :timer) timer-list))
          (should-not (process-live-p (plist-get job :worker)))
          (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status)))
          (should (= 0 (hash-table-count mevedel-memory-decision--recovery-workers))))
      (when timer (cancel-timer timer))
      (mevedel-memory-decision-stop-recovery)
      (delete-directory root t))))

(mevedel-deftest mevedel-memory-decision--recover-next-write ()
  ,test
  (test)
  :doc "inspects one fresh write per callback, yields between records, and preserves origin"
  (let* ((root (make-temp-file "mevedel-recovery-yield-" t))
         (workspace (mevedel-workspace--create :root root))
         (directory (mevedel-memory-write--directory workspace))
         (inspect (symbol-function 'mevedel-memory-write-list))
         (origin (current-buffer)) groups ticks completed job timer)
    (unwind-protect
        (progn
          (make-directory directory t)
          (dotimes (n 4)
            (write-region "(:invalid t)" nil (file-name-concat directory (concat (secure-hash 'sha256 (number-to-string n)) ".el")) nil 'silent))
          (cl-letf (((symbol-function 'mevedel-memory-write-list)
                     (lambda (&rest args)
                       (push (cadr args) groups)
                       (push (and timer (not (memq timer timer-list))) ticks)
                       (setq timer (run-at-time 0 nil #'ignore))
                       (apply inspect args))))
            (setq job (mevedel-memory-decision--start-recovery
                       workspace (lambda () (should (eq origin (current-buffer))) (setq completed t)) origin))
            (with-temp-buffer
              (let ((deadline (+ (float-time) 10)))
                (while (and (not completed) (< (float-time) deadline)) (accept-process-output nil .005))))
            (should completed)
            (should (= 4 (length groups)))
            (should (cl-every (lambda (ids) (= 1 (length ids))) groups))
            (should (equal '(t t t nil) ticks)))
          (should-not (gethash (plist-get (plist-get job :claim) :directory) mevedel-memory-decision--recovery-workers)))
      (when timer (cancel-timer timer))
      (mevedel-memory-decision-stop-recovery)
      (delete-directory root t)))
  :doc "cancellation during a record check cannot rearm the job or run its callback"
  (let* ((root (make-temp-file "mevedel-recovery-check-stop-" t))
         (workspace (mevedel-workspace--create :root root))
         (key (mevedel-memory-store--claim-directory workspace))
         (claim (mevedel-journal-claim-acquire key 180))
         (job (list :claim claim :released t :origin (current-buffer)
                    :writes (list (make-string 64 ?a) (make-string 64 ?b))
                    :callback (lambda () (ert-fail "Cancelled checks admitted review")))))
    (unwind-protect
        (progn
          (mevedel-journal-claim-settle claim 'cancelled "")
          (puthash key job mevedel-memory-decision--recovery-workers)
          (cl-letf (((symbol-function 'mevedel-memory-decision--recover-writes)
                     (lambda (actual ids)
                       (should (eq actual workspace))
                       (should (equal (list (make-string 64 ?a)) ids))
                       (mevedel-memory-decision-stop-recovery))))
            (mevedel-memory-decision--recover-next-write workspace job))
          (should-not (gethash key mevedel-memory-decision--recovery-workers))
          (should-not (plist-get job :timer))
          (mevedel-memory-decision--recover-next-write workspace job))
      (mevedel-memory-decision-stop-recovery)
      (delete-directory root t)))
  :doc "deadline and quit release the inspection registry without admitting a review"
  (dolist (failure '(deadline quit))
    (let* ((root (make-temp-file "mevedel-recovery-check-failure-" t))
           (workspace (mevedel-workspace--create :root root))
           (key (mevedel-memory-store--claim-directory workspace))
           (claim (mevedel-journal-claim-acquire key 180))
           (job (list :claim (copy-sequence claim) :released t :origin (current-buffer)
                      :writes (list (make-string 64 ?a))
                      :callback (lambda () (ert-fail "Failed checks admitted review")))) diagnostics)
      (unwind-protect
          (progn
            (mevedel-journal-claim-settle claim 'cancelled "")
            (puthash key job mevedel-memory-decision--recovery-workers)
            (if (eq failure 'deadline)
                (progn
                  (plist-put (plist-get job :claim) :expires-at (1- (float-time)))
                  (mevedel-test--with-captured-diagnostics diagnostics
                    (mevedel-memory-decision--recover-next-write workspace job))
                  (should (string-match-p "Memory recovery timed out" diagnostics)))
              (cl-letf (((symbol-function 'mevedel-memory-decision--recover-writes)
                         (lambda (&rest _) (signal 'quit nil))))
                (should (eq 'quit (condition-case nil
                                      (mevedel-memory-decision--recover-next-write workspace job)
                                    (quit 'quit))))))
            (should-not (gethash key mevedel-memory-decision--recovery-workers))
            (should-not (plist-get job :timer)))
        (mevedel-memory-decision-stop-recovery)
        (delete-directory root t)))))

(mevedel-deftest mevedel-memory-decision--recovered ()
  ,test
  (test)
  :doc "deadline fences the worker, reports failure once and ignores its late reply"
  (let* ((root (make-temp-file "mevedel-recovery-timeout-" t))
         (workspace (mevedel-workspace--create :root root)) job diagnostics)
    (unwind-protect
        (progn
          (setq job (mevedel-memory-decision--start-recovery
                     workspace (lambda () (ert-fail "Failed recovery admitted review")) (current-buffer)))
          (mevedel-test--with-captured-diagnostics diagnostics
            (let ((timer (plist-get job :timer)))
              (apply (timer--function timer) (timer--args timer)))
            (mevedel-memory-decision--recovered workspace job '(:ok t)))
          (should (string-match-p "Memory recovery timed out" diagnostics))
          (should-not (process-live-p (plist-get job :worker)))
          (should (= 0 (hash-table-count mevedel-memory-decision--recovery-workers)))
          (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status))))
      (mevedel-memory-decision-stop-recovery)
      (delete-directory root t)))
  :doc "shutdown cancels the active child and cannot accept a late success"
  (let* ((root (make-temp-file "mevedel-recovery-stop-" t))
         (workspace (mevedel-workspace--create :root root)) job)
    (unwind-protect
        (progn
          (setq job (mevedel-memory-decision--start-recovery
                     workspace (lambda () (ert-fail "Stopped recovery admitted review")) (current-buffer)))
          (mevedel-memory-decision-stop-recovery)
          (mevedel-memory-decision--recovered workspace job '(:ok t))
          (should-not (memq (plist-get job :timer) timer-list))
          (should-not (process-live-p (plist-get job :worker)))
          (should (= 0 (hash-table-count mevedel-memory-decision--recovery-workers)))
          (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status))))
      (mevedel-memory-decision-stop-recovery)
      (delete-directory root t))))

(provide 'test-mevedel-memory-decision-worker)
;;; test-mevedel-memory-decision-worker.el ends here
