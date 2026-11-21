;;; test-niwa-memory-decision-history.el -- Bounded history observations -*- lexical-binding: t -*-

;;; Commentary:

;; Real accepted passes and slot-backed decisions exercise recovery and pending
;; selection over retained history.  Counters observe real readers, not stubs
;; replacing authentication or application.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-memory-decision)
(require 'niwa-memory-list)
(require 'niwa-memory-notes)
(require 'niwa-memory-pass)
(require 'niwa-memory-store)
(require 'niwa-memory-write)
(require 'niwa-journal-store)
(require 'niwa-journal-cleanup)
(require 'niwa-session-control-fs)

(require 'memory-history-fixtures
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "memory-history-fixtures"))

(niwa-deftest niwa-memory-decision--record-batch
    (:vars* ((root (niwa-test-memory-history-root 'decision--record-batch))
             (workspace (niwa-workspace--create :root root))
             (niwa-memory-dirs nil)
             (niwa-journal-cleanup--inhibit-scheduling t)
             (identity (niwa-workspace-identity-ensure root))
             (bundles (niwa-test-memory-history-build
                       'decision--record-batch root
                       (lambda () (niwa-test-memory-history-create workspace 1 2 100))))
             (paths (niwa-session-control-fs-list-directory
                     (niwa-memory-decision--directory workspace) niwa-memory-decision--file-regexp)))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "authenticates the same bounded metadata using one program per batch, not per decision"
  (let ((program (symbol-function 'niwa-session-control-fs-run-program)) (calls 0) ordinary batched)
    (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
               (lambda (&rest args) (cl-incf calls) (apply program args))))
      (setq ordinary (mapcar (lambda (path) (cons path (niwa-memory-decision--record workspace path))) paths))
      (should (= 2 calls))
      (setq calls 0 batched (niwa-memory-decision--record-batch workspace paths))
      (should (= 1 calls))
      (should (equal ordinary batched)))
    (dolist (row batched)
      (should (equal (plist-get (cdr row) :hash) (secure-hash 'sha256 (niwa-session-control-fs-read-file (car row)))))
      (should-not (plist-get (cdr row) :accepted))))
  :doc "a fresh batch rejects damaged slot bytes without discarding the evidence"
  (let* ((path (car paths)) (text (niwa-session-control-fs-read-file path)))
    (should (= 2 (length (niwa-memory-decision--record-batch workspace paths))))
    (write-region (concat text "\n") nil path nil 'silent)
    (should (equal "Accepted memory decision changed" (error-message-string (should-error (niwa-memory-decision--record-batch workspace paths)))))
    (should (equal (concat text "\n") (niwa-session-control-fs-read-file path))))
  :doc "publication discards metadata proofs before inspecting the next decision"
  (let* ((first (niwa-memory-decision--record workspace (cadr paths)))
         (metadata (plist-get first :metadata))
         (public (file-name-concat (niwa-journal-store-directory root)
                                   (niwa-journal-store--filename metadata 'decision)))
         (other (car paths))
         (text (niwa-session-control-fs-read-file other))
         (listing (symbol-function 'niwa-session-control-fs-list-directory))
         (publish (symbol-function 'niwa-memory-decision--publish)) injected)
    (delete-file public)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-control-fs-list-directory)
                   (lambda (directory &rest args)
                     (if (equal directory (niwa-memory-decision--directory workspace)) paths
                       (apply listing directory args))))
                  ((symbol-function 'niwa-memory-decision--publish)
                   (lambda (&rest args)
                     (prog1 (apply publish args)
                       (unless injected
                         (setq injected t)
                         (write-region (concat text "\n") nil other nil 'silent))))))
          (niwa-test-memory-history-owned workspace
            (should (equal "Accepted memory decision changed"
                           (error-message-string (should-error (niwa-memory-decision-recover workspace))))))
          (should injected))
      (write-region text nil other nil 'silent)))
  :doc "empty batches stay empty and oversized batches are refused before IO"
  (progn
    (should-not (niwa-memory-decision--record-batch workspace nil))
    (should-error (niwa-memory-decision--record-batch workspace (make-list 9 (car paths))))))

(niwa-deftest niwa-memory-decision-recover/history
    (:vars* ((root (niwa-test-memory-history-root 'decision-recover/history))
             (workspace (niwa-workspace--create :root root))
             (niwa-memory-dirs nil)
             (niwa-memory-consolidation-mode 'auto)
             (niwa-journal-cleanup--inhibit-scheduling t)
             (identity (niwa-workspace-identity-ensure root))
             ;; Nine interleaved decisions cross both recovery bounds: one
             ;; 8-record batch, and a pass group spanning two batches.  A
             ;; single-slot bundle cache would read nine bundles, not three.
             (bundles (niwa-test-memory-history-build
                       'decision-recover/history root
                       (lambda () (niwa-test-memory-history-create workspace 3 3 6000)))))
            :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "recovery authenticates one bounded bundle per pass despite interleaved slot-backed decisions"
  (let ((reader (symbol-function 'niwa-memory-store-accepted))
        (listing (symbol-function 'niwa-session-control-fs-list-directory))
        (reads 0) paths)
    ;; Force an adversarial listing: one decision from each pass in turn.
    (dolist (accepted bundles)
      (push (mapcar (lambda (proposal)
                      (let ((decision (niwa-memory-decision-status workspace (plist-get proposal :id))))
                        (file-name-concat (niwa-memory-decision--directory workspace)
                                          (concat (plist-get decision :decision-id) ".el"))))
                    (plist-get accepted :proposals)) paths))
    (setq paths (apply #'cl-mapcan #'list paths))
    (cl-letf (((symbol-function 'niwa-memory-store-accepted)
               (lambda (&rest args) (cl-incf reads) (apply reader args)))
              ((symbol-function 'niwa-session-control-fs-list-directory)
               (lambda (directory &rest args)
                 (if (equal directory (niwa-memory-decision--directory workspace)) paths
                   (apply listing directory args)))))
      (niwa-test-memory-history-owned workspace
        ;; Fully published history is a no-op recovery.
        (should-not (niwa-memory-decision-recover workspace))))
    (should (= 3 reads)))
  :doc "pending selection reads the public journal once and never reapplies terminal history"
  (let ((reader (symbol-function 'niwa-journal-store-entries))
        (calls 0) notified)
    (cl-letf (((symbol-function 'niwa-journal-store-entries)
               (lambda (&rest args) (cl-incf calls) (apply reader args))))
      (should (equal '(0 . 0) (niwa-memory-pass--apply-pending workspace))))
    (should (= 1 calls))
    (let* ((accepted (car bundles))
           (pass (plist-get (plist-get accepted :prepared) :id)))
      (should (eq 'applied (plist-get (niwa-memory-decision-apply
                                       workspace pass (plist-get (car (plist-get accepted :proposals)) :id)
                                       :on-applied (lambda (_) (setq notified t))) :status))))
    (should-not notified))
  :doc "settled store recovery authenticates bundles without redundant publication transactions"
  (let ((publisher (symbol-function 'niwa-memory-store-publish)) (calls 0))
    (cl-letf (((symbol-function 'niwa-memory-store-publish)
               (lambda (&rest args) (cl-incf calls) (apply publisher args))))
      (niwa-test-memory-history-owned workspace
        (should-not (niwa-memory-store-recover workspace))))
    (should (= 0 calls)))
  :doc "each new observation rejects tampered accepted bytes, claim outcomes and decision slots"
  (let* ((accepted (car bundles))
         (prepared (plist-get accepted :prepared))
         (proposal (car (plist-get accepted :proposals)))
         (decision (niwa-memory-decision-status workspace (plist-get proposal :id)))
         (decision-path (file-name-concat (niwa-memory-decision--directory workspace)
                                          (concat (plist-get decision :decision-id) ".el")))
         (paths (list (file-name-concat (niwa-memory-store--directory workspace (plist-get prepared :id)) "accepted.el")
                      (niwa-journal-claim--path (plist-get prepared :claim) t)
                      decision-path)))
    (dolist (path paths)
      (let ((text (niwa-session-control-fs-read-file path))
            (expected (cond ((equal path decision-path) "Accepted memory decision changed")
                            (t "Accepted memory bundle changed"))))
        (unwind-protect
            (progn
              (write-region (cond ((equal path (niwa-journal-claim--path (plist-get prepared :claim) t))
                                   (string-replace (plist-get accepted :hash) (make-string 64 ?f) text))
                                  ((equal path decision-path)
                                   ;; A noncanonical slot must not pass a fresh observation.
                                   (concat text "\n"))
                                  (t (concat text "\n")))
                            nil path nil 'silent)
              (niwa-test-memory-history-owned workspace
                (should (equal expected (error-message-string
                                         (should-error (niwa-memory-decision-recover workspace))))))
              (should (equal expected (error-message-string
                                       (should-error (plist-get (niwa-memory-decision-apply
                                                      workspace (plist-get prepared :id) (plist-get proposal :id)) :decision))))))
          (write-region text nil path nil 'silent))
        (niwa-test-memory-history-owned workspace
          (niwa-memory-decision-recover workspace)))))
  :doc "pending selection refreshes after application publishes another terminal proposal"
  (let* ((accepted (car (niwa-test-memory-history-create workspace 1 2 100 t)))
         (pass (plist-get (plist-get accepted :prepared) :id))
         (items (plist-get accepted :proposals))
         (apply-decision (symbol-function 'niwa-memory-decision-apply))
         (calls 0))
    (should (= 2 (length items)))
    (cl-letf (((symbol-function 'niwa-memory-decision-apply)
               (lambda (ws id proposal &rest args)
                 (cl-incf calls)
                 (prog1 (apply apply-decision ws id proposal args)
                   ;; A separate real application publishes the next candidate
                   ;; before the pending selector continues its iteration.
                   (should (eq 'applied
                               (plist-get (funcall apply-decision ws pass (plist-get (cadr items) :id)) :status)))))))
      (should (equal '(1 . 0) (niwa-memory-pass--apply-pending workspace))))
    ;; The selection was taken once; the automatic verb rechecks status under
    ;; ownership and skips the proposal decided meanwhile, writing nothing.
    (should (= 2 calls))
    (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get (cadr items) :id)) :status)))
    (should (= 1 (seq-count (lambda (entry) (and (eq (plist-get entry :kind) 'decision)
                                                 (equal (plist-get entry :proposal-id) (plist-get (cadr items) :id))))
                            (niwa-journal-store-entries root)))))
  :doc "publication reauthenticates after acceptance: a bundle changed between them is refused"
  (let* ((accepted (car (niwa-test-memory-history-create workspace 1 1 100 t)))
         (pass (plist-get (plist-get accepted :prepared) :id))
         (proposal (car (plist-get accepted :proposals)))
         (path (file-name-concat (niwa-memory-store--directory workspace pass) "accepted.el"))
         (text (niwa-session-control-fs-read-file path))
         (publish (symbol-function 'niwa-memory-decision--publish-accepted)) injected)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-memory-decision--publish-accepted)
                   (lambda (&rest args)
                     (setq injected t)
                     (write-region (concat text "\n") nil path nil 'silent)
                     (apply publish args))))
          (should (equal "Accepted memory bundle changed"
                         (error-message-string
                          (should-error (niwa-memory-decision-apply workspace pass (plist-get proposal :id)))))))
      (write-region text nil path nil 'silent))
    (should injected)
    (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status)))
    (should-not (seq-find (lambda (entry) (equal (plist-get entry :proposal-id) (plist-get proposal :id)))
                          (niwa-journal-store-entries root)))
    (niwa-test-memory-history-owned workspace (should (niwa-memory-decision-recover workspace)))
    (should (seq-find (lambda (entry) (and (equal (plist-get entry :proposal-id) (plist-get proposal :id))
                                           (eq 'applied (plist-get entry :status))))
                      (niwa-journal-store-entries root))))
  :doc "a later pass bundle changed after selection fails closed at its own owned application"
  (let* ((created (niwa-test-memory-history-create workspace 2 1 100 t))
         (pending (car created))
         (later (cadr created))
         (later-id (plist-get (plist-get later :prepared) :id))
         (later-proposal (plist-get (car (plist-get later :proposals)) :id))
         (path (file-name-concat (niwa-memory-store--directory workspace later-id) "accepted.el"))
         (text (niwa-session-control-fs-read-file path))
         (enumerate (symbol-function 'niwa-memory-notes-passes))
         (apply-decision (symbol-function 'niwa-memory-decision-apply)) injected)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-memory-notes-passes)
                   (lambda (&rest args)
                     (let ((all (apply enumerate args)))
                       (list (seq-find (lambda (bundle) (equal bundle pending)) all)
                             (seq-find (lambda (bundle) (equal bundle later)) all)))))
                  ((symbol-function 'niwa-memory-decision-apply)
                   (lambda (&rest args)
                     (prog1 (apply apply-decision args)
                       (unless injected
                         (setq injected t)
                         (write-region (concat text "\n") nil path nil 'silent))))))
          ;; Selection authenticated both bundles before the change; the later
          ;; application authenticates afresh and refuses, deciding nothing.
          (let ((outcome (niwa-memory-pass--apply-pending workspace)))
            (should (= 1 (car outcome)))
            (should (= (length (plist-get later :proposals)) (cdr outcome))))
          (should (equal "Accepted memory bundle changed"
                         (error-message-string
                          (should-error (funcall apply-decision workspace later-id later-proposal))))))
      (write-region text nil path nil 'silent))
    (should injected)
    (dolist (proposal (plist-get later :proposals))
      (should-not (niwa-memory-decision-status workspace (plist-get proposal :id))))))

(provide 'test-niwa-memory-decision-history)
;;; test-niwa-memory-decision-history.el ends here
