;;; test-mevedel-memory-decision-history.el -- Bounded history observations -*- lexical-binding: t -*-

;;; Commentary:

;; Real accepted passes and write-backed decisions exercise recovery and pending
;; selection over retained history.  Counters observe real readers, not stubs
;; replacing authentication or application.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-decision)
(require 'mevedel-memory-list)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-pass)
(require 'mevedel-memory-store)
(require 'mevedel-memory-write)
(require 'mevedel-journal-store)
(require 'mevedel-journal-cleanup)
(require 'mevedel-session-control-fs)

(require 'memory-history-fixtures
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "memory-history-fixtures"))

(mevedel-deftest mevedel-memory-write--list/history
    (:vars* ((root (make-temp-file "mevedel-memory-write-history-" t))
             (workspace (mevedel-workspace--create :root root))
             (mevedel-memory-dirs nil)
             (mevedel-journal-cleanup--inhibit-scheduling t)
             (identity (mevedel-workspace-identity-ensure root))
             (bundles (mevedel-test-memory-history-create workspace 2 3 100)))
            :after-each ((remhash workspace mevedel-memory-write--inspected)
                         (delete-directory root t)))
  ,test
  (test)
  :doc "authenticates one source bundle per pass and preserves all write rows"
  (let ((reader (symbol-function 'mevedel-memory-store-accepted)) (calls 0))
    (cl-letf (((symbol-function 'mevedel-memory-store-accepted)
               (lambda (&rest args) (cl-incf calls) (apply reader args))))
      (let ((rows (mevedel-memory-write--list workspace)))
        (should (= 6 (length rows)))
        (dolist (row rows)
          (should (plist-get row :intent))
          (should-not (plist-get row :error))
          (should-not (plist-get row :marked)))))
    (should (= 2 calls)))
  :doc "the read-only collector authenticates each write once, not once per decision"
  (let ((reader (symbol-function 'mevedel-memory-write--read)) (calls 0))
    (cl-letf (((symbol-function 'mevedel-memory-write--read)
               (lambda (&rest args) (cl-incf calls) (apply reader args))))
      (let ((rows (mevedel-memory-list--collect (mevedel-cockpit-workspace-context workspace))))
        (should (= 6 (length rows)))
        (dolist (row rows)
          (should (eq 'applied (plist-get row :status)))
          (should-not (plist-get row :error)))))
    (should (= 6 calls)))
  :doc "new observations reject changed source and intent bytes after a successful listing"
  (let* ((rows (mevedel-memory-write--list workspace))
         (row (car rows))
         (intent (plist-get row :intent))
         (source (file-name-concat (mevedel-memory-store--directory workspace (plist-get intent :pass)) "accepted.el"))
         (write (file-name-concat (mevedel-memory-write--directory workspace) (concat (plist-get intent :id) ".el"))))
    (dolist (path (list source write))
      (let ((text (mevedel-session-control-fs-read-file path)))
        (unwind-protect
            (progn
              (write-region (concat text "\n") nil path nil 'silent)
              (let* ((changed (mevedel-memory-list--collect (mevedel-cockpit-workspace-context workspace)))
                     (item (seq-find (lambda (item) (equal (plist-get intent :pass) (plist-get item :id))) changed)))
                (should (eq 'unavailable (plist-get item :status)))
                (should (equal (if (equal path source) "Accepted memory bundle changed" "Memory write intent changed")
                               (plist-get item :error)))))
          (write-region text nil path nil 'silent))))))

(mevedel-deftest mevedel-memory-decision--record-batch
    (:vars* ((root (make-temp-file "mevedel-memory-record-batch-" t))
             (workspace (mevedel-workspace--create :root root))
             (mevedel-memory-dirs nil)
             (mevedel-journal-cleanup--inhibit-scheduling t)
             (identity (mevedel-workspace-identity-ensure root))
             (bundles (mevedel-test-memory-history-create workspace 1 2 100))
             (paths (mevedel-session-control-fs-list-directory
                     (mevedel-memory-decision--directory workspace) mevedel-memory-write-intent-file-regexp)))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "authenticates the same bounded metadata using two programs per batch, not per decision"
  (let ((program (symbol-function 'mevedel-session-control-fs-run-program)) (calls 0) ordinary batched)
    (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
               (lambda (&rest args) (cl-incf calls) (apply program args))))
      (setq ordinary (mapcar (lambda (path) (cons path (mevedel-memory-decision--record workspace path))) paths))
      (should (= 4 calls))
      (setq calls 0 batched (mevedel-memory-decision--record-batch workspace paths))
      (should (= 2 calls))
      (should (equal ordinary batched)))
    (dolist (row batched)
      (should (= 8 (length (cdr row))))
      (should-not (plist-get (cdr row) :accepted))))
  :doc "rejects a decision changed between its byte read and batched authentication"
  (let* ((path (car paths))
         (text (mevedel-session-control-fs-read-file path))
         (program (symbol-function 'mevedel-session-control-fs-run-program)) injected)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                   (lambda (operations &rest args)
                     (prog1 (apply program operations args)
                       (when (and (not injected) (equal paths (mapcar (lambda (op) (plist-get op :path)) operations)))
                         (setq injected t)
                         (write-region (concat text "\n") nil path nil 'silent))))))
          (let ((failure (should-error (mevedel-memory-decision--record-batch workspace paths)
                                       :type 'file-error)))
            (should (equal "Portable control operation failed" (cadr failure)))
            (should (equal path (caddr failure))))
          (should injected))
      (write-region text nil path nil 'silent))
    (should (= 2 (length (mevedel-memory-decision--record-batch workspace paths)))))
  :doc "publication discards metadata proofs before inspecting the next decision"
  (let* ((first (mevedel-memory-decision--record workspace (cadr paths)))
         (metadata (plist-get first :metadata))
         (public (file-name-concat (mevedel-journal-store-directory root)
                                   (mevedel-journal-store--filename metadata 'decision)))
         (other (car paths))
         (text (mevedel-session-control-fs-read-file other))
         (listing (symbol-function 'mevedel-session-control-fs-list-directory))
         (publish (symbol-function 'mevedel-memory-decision--publish)) injected)
    (delete-file public)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-control-fs-list-directory)
                   (lambda (directory &rest args)
                     (if (equal directory (mevedel-memory-decision--directory workspace)) paths
                       (apply listing directory args))))
                  ((symbol-function 'mevedel-memory-decision--publish)
                   (lambda (&rest args)
                     (prog1 (apply publish args)
                       (unless injected
                         (setq injected t)
                         (write-region (concat text "\n") nil other nil 'silent))))))
          (mevedel-test-memory-history-owned workspace
            (should (equal "Accepted memory decision changed"
                           (error-message-string (should-error (mevedel-memory-decision-recover workspace))))))
          (should injected))
      (write-region text nil other nil 'silent)))
  :doc "empty batches stay empty and oversized batches are refused before IO"
  (progn
    (should-not (mevedel-memory-decision--record-batch workspace nil))
    (should-error (mevedel-memory-decision--record-batch workspace (make-list 9 (car paths))))))

(mevedel-deftest mevedel-memory-decision-recover/history
    (:vars* ((root (make-temp-file "mevedel-memory-history-" t))
             (workspace (mevedel-workspace--create :root root))
             (mevedel-memory-dirs nil)
             (mevedel-memory-consolidation-mode 'auto)
             (mevedel-journal-cleanup--inhibit-scheduling t)
             (identity (mevedel-workspace-identity-ensure root))
             (bundles (mevedel-test-memory-history-create workspace 3 4 6000)))
            :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "recovery authenticates one bounded bundle per pass despite interleaved write-backed decisions"
  (let ((reader (symbol-function 'mevedel-memory-store-accepted))
        (listing (symbol-function 'mevedel-session-control-fs-list-directory))
        (reads 0) paths)
    ;; Force an adversarial listing: one decision from each pass in turn.
    (dolist (accepted bundles)
      (push (mapcar (lambda (proposal)
                      (let ((decision (mevedel-memory-decision-status workspace (plist-get proposal :id))))
                        (file-name-concat (mevedel-memory-decision--directory workspace)
                                          (concat (plist-get decision :decision-id) ".el"))))
                    (plist-get accepted :proposals)) paths))
    (setq paths (apply #'cl-mapcan #'list paths))
    (cl-letf (((symbol-function 'mevedel-memory-store-accepted)
               (lambda (&rest args) (cl-incf reads) (apply reader args)))
              ((symbol-function 'mevedel-session-control-fs-list-directory)
               (lambda (directory &rest args)
                 (if (equal directory (mevedel-memory-decision--directory workspace)) paths
                   (apply listing directory args)))))
      (mevedel-test-memory-history-owned workspace
        ;; Fully published history is a no-op recovery.
        (should-not (mevedel-memory-decision-recover workspace))))
    (should (= 3 reads)))
  :doc "write evidence reuses only the exact coordinator and source proofs from this read"
  (let* ((accepted (car bundles))
         (proposal (car (plist-get accepted :proposals)))
         (decision (mevedel-memory-decision-status workspace (plist-get proposal :id)))
         (path (file-name-concat (mevedel-memory-decision--directory workspace)
                                 (concat (plist-get decision :decision-id) ".el")))
         (private (mevedel-memory-decision--read workspace path))
         (intent (plist-get private :intent))
         (coordinator (plist-get private :claim))
         (program (symbol-function 'mevedel-session-control-fs-run-program))
         (calls 0) ordinary reused ordinary-calls)
    (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
               (lambda (&rest args) (cl-incf calls) (apply program args))))
      (setq ordinary (mevedel-memory-write--read workspace (plist-get intent :id) (plist-get intent :hash) accepted)
            ordinary-calls calls calls 0
            reused (mevedel-memory-write--read workspace (plist-get intent :id) (plist-get intent :hash)
                                               accepted nil coordinator))
      (should (equal ordinary reused))
      (should (= 1 (- ordinary-calls calls))))
    ;; A different decision coordinator is legitimate after write recovery.
    ;; Its proof must not replace the original coordinator's authentication.
    (dolist (other (list (plist-put (copy-sequence coordinator) :owner (make-string 64 ?f))
                        (plist-put (copy-sequence coordinator) :directory root)))
      (setq calls 0)
      (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                 (lambda (&rest args) (cl-incf calls) (apply program args))))
        (should (equal ordinary
                       (mevedel-memory-write--read workspace (plist-get intent :id) (plist-get intent :hash)
                                                  accepted nil other))))
      (should (= ordinary-calls calls)))
    (should (equal "Invalid memory write intent"
                   (error-message-string
                    (should-error
                     (mevedel-memory-write--read workspace (plist-get intent :id) (plist-get intent :hash)
                                                (cadr bundles) nil coordinator))))))
  :doc "pending selection reads the public journal once and never reapplies terminal history"
  (let ((reader (symbol-function 'mevedel-journal-store-entries))
        (calls 0) notified)
    (cl-letf (((symbol-function 'mevedel-journal-store-entries)
               (lambda (&rest args) (cl-incf calls) (apply reader args))))
      (should (equal '(0 . 0) (mevedel-memory-pass--apply-pending workspace))))
    (should (= 1 calls))
    (let* ((accepted (car bundles))
           (pass (plist-get (plist-get accepted :prepared) :id)))
      (should (eq 'applied (plist-get (mevedel-memory-decision-apply
                                       workspace pass (plist-get (car (plist-get accepted :proposals)) :id)
                                       :on-applied (lambda (_) (setq notified t))) :status))))
    (should-not notified))
  :doc "settled store recovery authenticates bundles without redundant publication transactions"
  (let ((publisher (symbol-function 'mevedel-memory-store-publish)) (calls 0))
    (cl-letf (((symbol-function 'mevedel-memory-store-publish)
               (lambda (&rest args) (cl-incf calls) (apply publisher args))))
      (mevedel-test-memory-history-owned workspace
        (should-not (mevedel-memory-store-recover workspace))))
    (should (= 0 calls)))
  :doc "each new observation rejects tampered accepted bytes, claim outcomes and write evidence"
  (let* ((accepted (car bundles))
         (prepared (plist-get accepted :prepared))
         (proposal (car (plist-get accepted :proposals)))
         (decision (mevedel-memory-decision-status workspace (plist-get proposal :id)))
         (private (mevedel-memory-decision--published workspace decision))
         (intent (plist-get private :intent))
         (decision-path (file-name-concat (mevedel-memory-decision--directory workspace)
                                          (concat (plist-get decision :decision-id) ".el")))
         (paths (list (file-name-concat (mevedel-memory-store--directory workspace (plist-get prepared :id)) "accepted.el")
                      (mevedel-journal-claim--path (plist-get prepared :claim) t)
                      decision-path
                      (file-name-concat (mevedel-memory-write--directory workspace) (concat (plist-get intent :id) ".el")))))
    (should (equal "Invalid memory write intent"
                   (error-message-string
                    (should-error (mevedel-memory-write--read workspace (plist-get intent :id)
                                                             (plist-get intent :hash) (cadr bundles))))))
    (dolist (path paths)
      (let ((text (mevedel-session-control-fs-read-file path))
            (expected (cond ((equal path decision-path) "Accepted memory decision changed")
                            ((equal path (car (last paths))) "Memory write intent changed")
                            (t "Accepted memory bundle changed"))))
        (unwind-protect
            (progn
              (write-region (cond ((equal path (mevedel-journal-claim--path (plist-get prepared :claim) t))
                                   (string-replace (plist-get accepted :hash) (make-string 64 ?f) text))
                                  ((equal path decision-path)
                                   ;; A forged grouping hint must not bypass authentication.
                                   (string-replace (plist-get prepared :id) (make-string 64 ?f) text))
                                  (t (concat text "\n")))
                            nil path nil 'silent)
              (mevedel-test-memory-history-owned workspace
                (should (equal expected (error-message-string
                                         (should-error (mevedel-memory-decision-recover workspace))))))
              (should (equal expected (error-message-string
                                       (should-error (plist-get (mevedel-memory-decision-apply
                                                      workspace (plist-get prepared :id) (plist-get proposal :id)) :decision))))))
          (write-region text nil path nil 'silent))
        (mevedel-test-memory-history-owned workspace
          (mevedel-memory-decision-recover workspace)))))
  :doc "pending selection refreshes after application publishes another terminal proposal"
  (let* ((accepted (car (mevedel-test-memory-history-create workspace 1 2 100 t)))
         (pass (plist-get (plist-get accepted :prepared) :id))
         (items (plist-get accepted :proposals))
         (apply-decision (symbol-function 'mevedel-memory-decision-apply))
         (calls 0))
    (should (= 2 (length items)))
    (cl-letf (((symbol-function 'mevedel-memory-decision-apply)
               (lambda (ws id proposal &rest args)
                 (cl-incf calls)
                 (prog1 (apply apply-decision ws id proposal args)
                   ;; A separate real application publishes the next candidate
                   ;; before the pending selector continues its iteration.
                   (should (eq 'applied
                               (plist-get (funcall apply-decision ws pass (plist-get (cadr items) :id)) :status)))))))
      (should (equal '(1 . 0) (mevedel-memory-pass--apply-pending workspace))))
    ;; The selection was taken once; the automatic verb rechecks status under
    ;; ownership and skips the proposal decided meanwhile, writing nothing.
    (should (= 2 calls))
    (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get (cadr items) :id)) :status)))
    (should (= 1 (seq-count (lambda (entry) (and (eq (plist-get entry :kind) 'decision)
                                                 (equal (plist-get entry :proposal-id) (plist-get (cadr items) :id))))
                            (mevedel-journal-store-entries root)))))
  :doc "publication reauthenticates after acceptance: a bundle changed between them is refused"
  (let* ((accepted (car (mevedel-test-memory-history-create workspace 1 1 100 t)))
         (pass (plist-get (plist-get accepted :prepared) :id))
         (proposal (car (plist-get accepted :proposals)))
         (path (file-name-concat (mevedel-memory-store--directory workspace pass) "accepted.el"))
         (text (mevedel-session-control-fs-read-file path))
         (retire (symbol-function 'mevedel-memory-write--retire)) injected)
    (unwind-protect
        ;; Retirement runs after durable acceptance settled the owned phase
        ;; and before publication: change the bundle in that gap.
        (cl-letf (((symbol-function 'mevedel-memory-write--retire)
                   (lambda (&rest args)
                     (prog1 (apply retire args)
                       (setq injected t)
                       (write-region (concat text "\n") nil path nil 'silent)))))
          (should (equal "Accepted memory bundle changed"
                         (error-message-string
                          (should-error (mevedel-memory-decision-apply workspace pass (plist-get proposal :id)))))))
      (write-region text nil path nil 'silent))
    (should injected)
    ;; The accepted decision exists privately but was never published.
    (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id)))
    (should-not (plist-get (mevedel-memory-decision-inspect workspace :pass pass :proposal (plist-get proposal :id)) :writes))
    (mevedel-memory-decision-recover-pending workspace)
    (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status))))
  :doc "a later pass bundle changed after selection fails closed at its own owned application"
  (let* ((created (mevedel-test-memory-history-create workspace 2 1 100 t))
         (pending (car created))
         (later (cadr created))
         (later-id (plist-get (plist-get later :prepared) :id))
         (later-proposal (plist-get (car (plist-get later :proposals)) :id))
         (path (file-name-concat (mevedel-memory-store--directory workspace later-id) "accepted.el"))
         (text (mevedel-session-control-fs-read-file path))
         (enumerate (symbol-function 'mevedel-memory-notes-passes))
         (apply-decision (symbol-function 'mevedel-memory-decision-apply)) injected)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-memory-notes-passes)
                   (lambda (&rest args)
                     (let ((all (apply enumerate args)))
                       (list (seq-find (lambda (bundle) (equal bundle pending)) all)
                             (seq-find (lambda (bundle) (equal bundle later)) all)))))
                  ((symbol-function 'mevedel-memory-decision-apply)
                   (lambda (&rest args)
                     (prog1 (apply apply-decision args)
                       (unless injected
                         (setq injected t)
                         (write-region (concat text "\n") nil path nil 'silent))))))
          ;; Selection authenticated both bundles before the change; the later
          ;; application authenticates afresh and refuses, deciding nothing.
          (let ((outcome (mevedel-memory-pass--apply-pending workspace)))
            (should (= 1 (car outcome)))
            (should (= (length (plist-get later :proposals)) (cdr outcome))))
          (should (equal "Accepted memory bundle changed"
                         (error-message-string
                          (should-error (funcall apply-decision workspace later-id later-proposal))))))
      (write-region text nil path nil 'silent))
    (should injected)
    (dolist (proposal (plist-get later :proposals))
      (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id))))))

(provide 'test-mevedel-memory-decision-history)
;;; test-mevedel-memory-decision-history.el ends here
