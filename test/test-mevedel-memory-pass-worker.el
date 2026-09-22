;;; test-mevedel-memory-pass-worker.el --- Async preparation tests -*- lexical-binding: t -*-

;;; Commentary:
;; Real storage children retain configured roots and cancellation boundaries.

;;; Code:
(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-memory-pass)
(require 'gptel-openai)

(mevedel-deftest mevedel-memory-pass--prepare ()
  (let* ((directory (make-temp-file "mevedel-pass-worker-" t))
         (workspace (mevedel-workspace--create :root directory))
         (memory (file-name-concat directory "configured-memory"))
         (mevedel-memory-dirs (list memory))
         (kill-emacs-hook kill-emacs-hook)
         (origin (current-buffer))
         (gptel--known-backends nil)
         (backend (gptel-make-openai "worker-test" :key "test-only" :models '(test-model)))
         state observed result tick timer review-callback)
    (unwind-protect
        (progn
          (make-directory memory)
          (with-temp-file (file-name-concat memory "topic.md") (insert "Original configured fact.\n"))
          (cl-letf (((symbol-function 'mevedel-memory-decision-recover)
                     (lambda (&rest _) (ert-fail "Recovery ran in the editor")))
                    ((symbol-function 'mevedel-memory-store-recover)
                     (lambda (&rest _) (ert-fail "Store recovery ran in the editor")))
                    ((symbol-function 'mevedel-memory-review-request)
                     (lambda (scope _entries callback &rest _)
                       (should (eq origin (current-buffer)))
                       (setq observed scope review-callback callback)
                       (list :cancel #'ignore))))
            (setq state (mevedel-memory-pass-start
                         workspace (lambda (value) (setq result value)) :memory-only t))
            (should (processp (plist-get state :worker)))
            (should-not observed)
            (setq mevedel-memory-dirs nil
                  timer (run-at-time 0 nil (lambda () (setq tick t))))
            (with-temp-buffer
             (let ((deadline (+ (float-time) 15)))
              (while (and (not observed) (not result) (< (float-time) deadline))
                (accept-process-output nil .01))))
            (should tick)
            (should-not result)
            (let* ((root (car (plist-get observed :roots)))
                   (snapshot (mevedel-memory-scope-before observed (car root) "topic.md")))
              (should (equal "Original configured fact.\n" (plist-get snapshot :bytes)))
              (should (equal (mevedel-workspace-identity-client) (plist-get (cdr root) :client))))
            (should (plist-get state :prepared))
            (funcall review-callback
                     (list :outcome 'success :entries (plist-get (plist-get state :prepared) :entries)
                           :policy (list :backend backend :model 'test-model)
                           :reply "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes."))
            (should (processp (plist-get state :worker)))
            (should-not result)
            (let ((deadline (+ (float-time) 15)))
              (while (and (not result) (< (float-time) deadline))
                (accept-process-output nil .01)))
            (should (eq 'success (plist-get result :outcome)))
            (should (= 1 (length (mevedel-journal-store-entries directory))))))
      (when timer (cancel-timer timer))
      (mevedel-memory-pass-cancel workspace)
      (delete-directory directory t))))

(mevedel-deftest mevedel-memory-pass--prepared ()
  (let* ((directory (make-temp-file "mevedel-pass-cancel-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs nil)
         (kill-emacs-hook kill-emacs-hook)
         (settlements 0) state result worker)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-memory-review-request)
                   (lambda (&rest _) (ert-fail "Cancelled preparation started inference"))))
          (setq state (mevedel-memory-pass-start
                       workspace (lambda (value) (cl-incf settlements) (setq result value))
                       :memory-only t)
                worker (plist-get state :worker))
          (should (processp worker))
          (mevedel-memory-pass-cancel workspace)
          (accept-process-output nil .02)
          (mevedel-memory-pass--prepared state '(:error "Late child") "" t)
          (should (= settlements 1))
          (should (eq 'aborted (plist-get result :outcome)))
          (should-not (process-live-p worker))
          (should-not (mevedel-memory-pass-running workspace))
          (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome
                                            (plist-get state :claim)) :status))))
      (mevedel-memory-pass-cancel workspace)
      (delete-directory directory t))))

(mevedel-deftest mevedel-memory-pass--publish ()
  (let* ((directory (make-temp-file "mevedel-pass-publication-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs nil)
         (none "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes.")
         claim)
    (unwind-protect
        (progn
          (mevedel-workspace-identity-ensure directory)
          (setq claim (mevedel-journal-claim-acquire
                       (mevedel-memory-store--claim-directory workspace) 180))
          (let* ((prepared (mevedel-memory-store-prepare
                            workspace claim (mevedel-memory-scope-capture workspace) nil ""))
                 (payload (list :prepared prepared :reply none :entries nil :model "test:fixture"))
                 (result
                  (cl-letf (((symbol-function 'mevedel-memory-store-publish)
                             (lambda (&rest _) (error "Injected publication failure"))))
                    (mevedel-memory-pass--publish workspace payload))))
            (should (plist-get result :error))
            (should (plist-get result :accepted))
            (should (eq 'completed (plist-get (mevedel-journal-claim-outcome claim) :status)))
            (should (mevedel-memory-store-publish workspace (plist-get prepared :id)))
            (should (mevedel-memory-store-accepted workspace (plist-get prepared :id)))))
      (when claim (mevedel-journal-claim-settle claim 'cancelled ""))
      (delete-directory directory t))))

(mevedel-deftest mevedel-memory-pass-start/automatic-worker ()
  (let* ((directory (make-temp-file "mevedel-pass-gate-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs nil)
         (mevedel-memory-pass--inhibit-scheduling nil)
         (mevedel-memory-consolidation-mode 'propose)
         (mevedel-memory-consolidation-min-hours 9)
         (mevedel-memory-consolidation-min-digests 7)
         state)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-memory-review-request)
                   (lambda (&rest _) (ert-fail "Skipped admission started inference"))))
          (setq state (mevedel-memory-pass-start
                       workspace (lambda (_) (ert-fail "Skipped admission called completion")) :automatic t))
          (should (processp (plist-get state :worker)))
          (setq mevedel-memory-consolidation-min-hours 0
                mevedel-memory-consolidation-min-digests 1)
          (let ((until (+ (float-time) 10)))
            (while (and (not (plist-get state :settled)) (< (float-time) until))
              (accept-process-output nil .01)))
          (should (plist-get state :skipped))
          (should-not (mevedel-memory-pass-running workspace))
          (should (= 9 (plist-get (mevedel-workspace-memory-schedule workspace) :hours)))
          (should (= 7 (plist-get (mevedel-workspace-memory-schedule workspace) :digests)))
          (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get state :claim)) :status))))
      (mevedel-memory-pass-cancel workspace)
      (delete-directory directory t))))

(provide 'test-mevedel-memory-pass-worker)
;;; test-mevedel-memory-pass-worker.el ends here
