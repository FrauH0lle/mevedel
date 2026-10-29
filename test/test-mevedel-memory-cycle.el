;;; test-mevedel-memory-cycle.el -- Finite note offer recovery -*- lexical-binding: t -*-

;;; Commentary:

;; Real claims, metadata inventories and accepted bundles exercise scheduling
;; receipts.  No provider calls, editor hotloading or destructive rollout.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-journal-claim)
(require 'mevedel-memory-cycle)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-store)
(require 'mevedel-session-control-fs)

(mevedel-deftest mevedel-memory-cycle
    (:vars* ((root (make-temp-file "memory-cycle-" t))
             (workspace (mevedel-workspace--create :root root))
             (identity (mevedel-workspace-identity-ensure root))
             (shared (file-name-concat root ".mevedel" "shared"))
             (mevedel-memory-dirs nil)
             (now (floor (float-time))) claim cycle)
     :before-each ((make-directory shared t))
     :after-each ((when claim (mevedel-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (cl-letf (((symbol-function 'mevedel-session-control-fs-target-time) (lambda (_) now))
            ((symbol-function 'mevedel-session-control-fs-observation-time) (lambda (_) now)))
    (cl-labels
        ((notes (count)
           (dotimes (n count)
             (let ((path (file-name-concat shared (format "note-%03d.md" n))))
               (write-region "Disposable factual working note.\n" nil path nil 'silent)
               (set-file-times path (seconds-to-time (- now (* 15 86400)))))))
         (acquire ()
           (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
           (should claim))
         (settle () (mevedel-journal-claim-settle claim 'completed "") (setq claim nil))
         (freeze (&optional force)
           (setq cycle (mevedel-memory-cycle-admit
                        workspace claim (lambda () (mevedel-memory-notes-cohort workspace nil :mode 'propose)) force)))
         (recover () (setq cycle (mevedel-memory-cycle-recover workspace claim)))
         (capture ()
           (mevedel-memory-notes-capture workspace nil :mode 'propose
                                         :cohort (mevedel-memory-cycle-cohort cycle)))
         (batch (&optional no-admission)
           (let* ((captured (capture))
                  (_ (setq cycle (mevedel-memory-cycle-stage workspace claim cycle :capture captured)))
                  (scope (mevedel-memory-scope-capture workspace))
                  (_ (setq scope (plist-put scope :roots (append (plist-get scope :roots) (list (plist-get captured :root)))))))
             (plist-put scope :note-cycle (list :id (plist-get cycle :id) :cohort-hash (plist-get cycle :cohort-hash)))
             (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
                    (admitted (unless no-admission (mapcar #'car (mevedel-memory-notes-index scope)))))
               (should (mevedel-memory-store-accept-proposals
                        workspace prepared (list :proposals nil :no-action "No supported extractions.")
                        nil "test:model" nil admitted))
               (setq claim nil)
               (length admitted)))))
      ,test))
  (test)
  :doc "32-note cohort survives a fresh owner and record reread after every batch"
  (progn
    (notes 32) (acquire) (freeze)
    (should (= 32 (length (plist-get cycle :members))))
    (should (= 16 (batch)))
    (acquire) (recover)
    (should (= 16 (length (plist-get cycle :attempted))))
    (should (= 16 (batch)))
    (acquire) (recover)
    (should (eq 'complete (plist-get cycle :status)))
    (should (= 32 (length (plist-get cycle :attempted))))
    (should-not (mevedel-memory-cycle-pending cycle)))
  :doc "50-note finite cohort has no dependence on deletion progress or an in-memory attempt set"
  (progn
    (notes 50) (acquire) (freeze)
    (let (counts)
      (while (mevedel-memory-cycle-pending cycle)
        (push (batch) counts)
        (acquire) (recover))
      (should (equal '(2 16 16 16) counts)))
    (should (= 50 (length (plist-get cycle :attempted))))
    (should (= 50 (length (directory-files shared nil "\\.md\\'")))))
  :doc "100-note cohort is finite with bounded body-free metadata and accepted receipts"
  (progn
    (notes 100) (acquire) (freeze)
    (while (mevedel-memory-cycle-pending cycle)
      (batch) (acquire) (recover))
    (should (= 100 (length (plist-get cycle :attempted))))
    (let ((text (cdr (mevedel-memory-cycle--read workspace))))
      (should (< (string-bytes text) (* 128 1024)))
      (should-not (string-search "Disposable factual working note" text))))
  :doc "accepted actual input counts once independently of pending disposal application"
  (progn
    (notes 17) (acquire) (freeze) (batch)
    (acquire) (recover)
    (should (= 16 (length (plist-get cycle :attempted))))
    (should (= 1 (length (mevedel-memory-cycle-pending cycle))))
    (should-not (plist-get cycle :deferred)))
  :doc "captured but unadmitted input is terminal deferred, not attempted or service"
  (progn
    (notes 1) (acquire) (freeze) (batch t)
    (acquire) (recover)
    (should-not (plist-get cycle :attempted))
    (should (eq 'not-admitted (plist-get (car (plist-get cycle :deferred)) :reason)))
    (should (eq 'complete (plist-get cycle :status))))
  :doc "settled tokens cannot write the cycle; only the fresh owner reconciles acceptance"
  (progn
    (notes 1) (acquire) (freeze)
    (let ((old claim))
      (batch)
      (should-error (mevedel-memory-cycle--write
                     workspace old (mevedel-memory-cycle--read workspace) cycle))
      (should (mevedel-memory-cycle-pins-pass-p workspace (plist-get old :owner)))
      (should (equal old (mevedel-memory-cycle-inflight-claim workspace)))
      (acquire) (recover)
      (should-not (mevedel-memory-cycle-pins-pass-p workspace (plist-get old :owner)))
      (should-not (mevedel-memory-cycle-inflight-claim workspace))))
  :doc "failure before preparation pauses without inventing attempted or accepted evidence"
  (progn
    (notes 2) (acquire) (freeze)
    (setq cycle (mevedel-memory-cycle-stage workspace claim cycle :capture (capture)))
    (mevedel-journal-claim-settle claim 'failed "")
    (acquire) (recover)
    (should (eq 'paused (plist-get cycle :status)))
    (should (= 2 (length (mevedel-memory-cycle-pending cycle))))
    (should-not (freeze))
    (should (eq 'active (plist-get (freeze t) :status))))
  :doc "daily admission is inclusive at exactly 86400 target seconds"
  (progn
    (acquire) (freeze)
    (should (eq 'complete (plist-get cycle :status)))
    (let ((id (plist-get cycle :id)))
      (settle) (acquire)
      (cl-incf now 86399)
      (should-not (freeze))
      (cl-incf now)
      (should (freeze))
      (should-not (equal id (plist-get cycle :id)))))
  :doc "paused failure never loops today but admits a new finite cohort at next-day equality"
  (progn
    (notes 1) (acquire) (freeze)
    (setq cycle (mevedel-memory-cycle-stage workspace claim cycle :stop 'failed))
    (let ((id (plist-get cycle :id)))
      (settle) (acquire)
      (cl-incf now 86399)
      (should-not (freeze))
      (cl-incf now)
      (should (freeze))
      (should-not (equal id (plist-get cycle :id)))
      (should (eq 'active (plist-get cycle :status)))))
  :doc "later arrivals never expand the frozen cohort and changed versions defer"
  (progn
    (notes 1) (acquire) (freeze)
    (notes 2)
    (let ((capture (capture)))
      (should-not (plist-get capture :root))
      (should (= 1 (length (plist-get capture :deferred))))
      (setq cycle (mevedel-memory-cycle-stage workspace claim cycle :capture capture)))
    (should (= 1 (length (plist-get cycle :members))))
    (should (eq 'complete (plist-get cycle :status)))
    (should-not (plist-get cycle :attempted)))
  :doc "malformed and oversized state fail closed rather than permitting admission or cleanup"
  (progn
    (acquire) (freeze)
    (let ((path (mevedel-memory-cycle--path workspace)))
      (write-region "(:status complete)" nil path nil 'silent)
      (should-error (mevedel-memory-cycle-pins-pass-p workspace (make-string 64 ?a)))
      (should-error (freeze))
      (let ((mevedel-memory-cycle--max-bytes 8))
        (should-error (mevedel-memory-cycle--read workspace)))))
  :doc "incomplete cohort fails admission without persisting partial coverage"
  (progn
    (acquire)
    (should-error (mevedel-memory-cycle-admit workspace claim (lambda () (list :complete nil :workspace identity))))
    (should-not (mevedel-memory-cycle--read workspace)))
  :doc "two coordinators cannot admit concurrent daily cycles under the consolidation claim"
  (progn
    (notes 1) (acquire) (freeze)
    (should-not (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (let ((id (plist-get cycle :id)))
      (mevedel-journal-claim-settle claim 'cancelled "")
      (acquire) (recover)
      (should-not (freeze))
      (should (equal id (plist-get (freeze t) :id)))))
  :doc "CAS rejects a changed record without overwriting another coordinator's metadata"
  (progn
    (notes 1) (acquire) (freeze)
    (let* ((previous (mevedel-memory-cycle--read workspace))
           (path (mevedel-memory-cycle--path workspace)))
      (write-region (concat (cdr previous) "\n") nil path nil 'silent)
      (should-error (mevedel-memory-cycle--write workspace claim previous cycle))
      (should (string-suffix-p "\n" (mevedel-session-control-fs-read-file path)))))
  :doc "foreign predecessor directories fail before outcome IO and leave exact cycle bytes untouched"
  (progn
    (notes 1) (acquire) (freeze)
    (let* ((path (mevedel-memory-cycle--path workspace))
           (foreign (file-name-concat root "foreign-claims"))
           (record (copy-tree cycle))
           (read-outcome (symbol-function 'mevedel-journal-claim-outcome))
           outcome-io)
      (make-directory foreign)
      (plist-put (plist-get (plist-get record :inflight) :claim) :directory foreign)
      (write-region (mevedel-memory-cycle--print record) nil path nil 'silent)
      (mevedel-journal-claim-settle claim 'cancelled "")
      (acquire)
      (let ((before (mevedel-session-control-fs-read-file path)))
        (cl-letf (((symbol-function 'mevedel-journal-claim-outcome)
                   (lambda (&rest args) (setq outcome-io t) (apply read-outcome args))))
          (should-error (recover)))
        (should-not outcome-io)
        (should (equal before (mevedel-session-control-fs-read-file path))))))
  :doc "same or future predecessor generations fail before outcome IO without cycle mutation"
  (progn
    (notes 1) (acquire) (freeze)
    (let ((original (copy-tree cycle))
          (path (mevedel-memory-cycle--path workspace))
          (read-outcome (symbol-function 'mevedel-journal-claim-outcome)))
      (mevedel-journal-claim-settle claim 'cancelled "")
      (acquire)
      (dolist (generation (list (plist-get claim :generation) (1+ (plist-get claim :generation))))
        (let ((record (copy-tree original)) outcome-io)
          (plist-put (plist-get (plist-get record :inflight) :claim) :generation generation)
          (write-region (mevedel-memory-cycle--print record) nil path nil 'silent)
          (let ((before (mevedel-session-control-fs-read-file path)))
            (cl-letf (((symbol-function 'mevedel-journal-claim-outcome)
                       (lambda (&rest args) (setq outcome-io t) (apply read-outcome args))))
              (should-error (recover)))
            (should-not outcome-io)
            (should (equal before (mevedel-session-control-fs-read-file path))))))))
  :doc "claim generations and deadlines use the journal decoder's positive integer schema"
  (progn
    (notes 1) (acquire) (freeze)
    (dolist (key '(:generation :expires-at))
      (dolist (value '(0 -1 "1"))
        (let ((record (copy-tree cycle)))
          (plist-put (plist-get (plist-get record :inflight) :claim) key value)
          (write-region (mevedel-memory-cycle--print record) nil
                        (mevedel-memory-cycle--path workspace) nil 'silent)
          (should-error (mevedel-memory-cycle--read workspace))
          (should-error (mevedel-memory-cycle-inflight-claim workspace))))))
  :doc "remote method strings match the target API schema without claiming remote transport verification"
  (progn
    (acquire) (freeze)
    (let ((record (copy-tree cycle)))
      (plist-put record :target '(:method "ssh" :user "test" :host "target.example"))
      (should (mevedel-memory-cycle--valid-p record))
      (dolist (method '(nil 0 ssh ""))
        (plist-put (plist-get record :target) :method method)
        (should-not (mevedel-memory-cycle--valid-p record)))))
  :doc "active namespace mismatch refuses reservation without inventing alias equivalence or changing the cycle"
  (progn
    (notes 1) (acquire) (freeze)
    (let* ((path (mevedel-memory-cycle--path workspace))
           (alternate (file-name-concat root "replacement" ".mevedel" "shared"))
           (record (copy-tree cycle))
           cohort-called)
      ;; Model the ordinary between-batch active state after reconciliation.
      (plist-put record :inflight nil)
      (write-region (mevedel-memory-cycle--print record) nil path nil 'silent)
      (make-directory alternate t)
      (let ((before (mevedel-session-control-fs-read-file path)))
        (cl-letf (((symbol-function 'mevedel-memory-notes-directory) (lambda (_) alternate)))
          (let ((failure (should-error
                          (mevedel-memory-cycle-admit
                           workspace claim (lambda () (setq cohort-called t))))))
            (should (string-match-p "original authority is required" (error-message-string failure)))))
        (should-not cohort-called)
        (should (equal before (mevedel-session-control-fs-read-file path))))
      ;; A different target is not an alias for the current local namespace.
      (plist-put record :target '(:method "ssh" :host "other.example"))
      (write-region (mevedel-memory-cycle--print record) nil path nil 'silent)
      (let ((before (mevedel-session-control-fs-read-file path)))
        (should-error (mevedel-memory-cycle-admit workspace claim #'ignore))
        (should (equal before (mevedel-session-control-fs-read-file path))))))
  :doc "inflight namespace mismatch needs original authority before outcome IO or scheduling mutation"
  (progn
    (notes 1) (acquire) (freeze)
    (let* ((path (mevedel-memory-cycle--path workspace))
           (alternate (file-name-concat root "replacement" ".mevedel" "shared"))
           (read-outcome (symbol-function 'mevedel-journal-claim-outcome))
           outcome-io)
      (make-directory alternate t)
      (mevedel-journal-claim-settle claim 'cancelled "")
      (acquire)
      (let ((before (mevedel-session-control-fs-read-file path)))
        (cl-letf (((symbol-function 'mevedel-memory-notes-directory) (lambda (_) alternate))
                  ((symbol-function 'mevedel-journal-claim-outcome)
                   (lambda (&rest args) (setq outcome-io t) (apply read-outcome args))))
          (let ((failure (should-error (recover))))
            (should (string-match-p "original authority is required" (error-message-string failure)))))
        (should-not outcome-io)
        (should (equal before (mevedel-session-control-fs-read-file path))))
      (recover)
      (should (eq 'paused (plist-get cycle :status)))))
  :doc "a completed old namespace is replaced only when a fresh daily admission is due"
  (progn
    (acquire) (freeze)
    (should (eq 'complete (plist-get cycle :status)))
    (let ((old-id (plist-get cycle :id))
          (alternate (file-name-concat root "replacement" ".mevedel" "shared"))
          (before (cdr (mevedel-memory-cycle--read workspace))))
      (make-directory alternate t)
      (settle) (acquire)
      (cl-incf now 86399)
      (cl-letf (((symbol-function 'mevedel-memory-notes-directory) (lambda (_) alternate)))
        (should-not (freeze))
        (should (equal before (cdr (mevedel-memory-cycle--read workspace))))
        (cl-incf now)
        (should (freeze))
        (should (eq 'complete (plist-get cycle :status)))
        (should-not (equal old-id (plist-get cycle :id)))
        (should (equal (file-name-as-directory (file-truename alternate)) (plist-get cycle :dir))))))
  :doc "namespace changes during yielding capture cannot mutate the reserved cycle at staging"
  (progn
    (notes 1) (acquire) (freeze)
    (let ((captured (capture))
          (before (cdr (mevedel-memory-cycle--read workspace)))
          (alternate (file-name-concat root "replacement" ".mevedel" "shared")))
      (should (plist-get captured :root))
      (make-directory alternate t)
      (cl-letf (((symbol-function 'mevedel-memory-notes-directory) (lambda (_) alternate)))
        (let ((failure (should-error
                        (mevedel-memory-cycle-stage workspace claim cycle :capture captured))))
          (should (string-match-p "original authority is required" (error-message-string failure))))
        (should-error (mevedel-memory-cycle-stage workspace claim cycle :stop 'cancelled)))
      (should (equal before (cdr (mevedel-memory-cycle--read workspace))))
      (should (equal claim (mevedel-memory-cycle-inflight-claim workspace)))))
  :doc "empty newly constructed envelopes still require the exact actual namespace before persistence"
  (progn
    (acquire)
    (let ((cohort (mevedel-memory-notes-cohort workspace nil))
          (alternate (file-name-concat root "replacement" ".mevedel" "shared")))
      (should-not (plist-get cohort :members))
      (make-directory alternate t)
      (dolist (fault (list (cons :dir (file-name-as-directory alternate))
                          (cons :target '(:method "ssh" :host "other.example"))))
        (let ((wrong (copy-tree cohort)))
          (plist-put wrong (car fault) (cdr fault))
          (let ((failure (should-error
                          (mevedel-memory-cycle-admit workspace claim (lambda () wrong)))))
            (should (string-match-p "original authority is required" (error-message-string failure))))
          (should-not (mevedel-memory-cycle--read workspace)))))))

(provide 'test-mevedel-memory-cycle)
;;; test-mevedel-memory-cycle.el ends here
