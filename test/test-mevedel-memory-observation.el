;;; test-mevedel-memory-observation.el --- Batched evidence observations -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise bounded batch failures and authentication reuse over real history.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "helpers"))
(require 'mevedel-memory-decision)
(require 'mevedel-memory-list)
(require 'mevedel-memory-store)
(require 'mevedel-memory-write)
(require 'mevedel-session-control-fs)
(require 'memory-history-fixtures
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "memory-history-fixtures"))

(mevedel-deftest mevedel-memory-write--read-batch
    (:vars* ((root (make-temp-file "mevedel-memory-read-batch-" t))
             (workspace (mevedel-workspace--create :root root))
             (id (make-string 64 ?a))
             (directory (mevedel-memory-write--directory workspace))
             (path (file-name-concat directory (concat id ".el"))))
            :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "returns real bounded observations including absent records in request order"
  (progn
    (make-directory directory t)
    (write-region "(:record t)" nil path nil 'silent)
    (let ((reads (mevedel-memory-write--read-batch workspace (list id (make-string 64 ?b)))))
      (should (= 2 (length reads)))
      (should (equal path (plist-get (car reads) :path)))
      (should (equal "(:record t)" (mevedel-session-control-fs-program-value (car reads))))
      (should (eq 'absent (plist-get (cadr reads) :status))))))

(mevedel-deftest mevedel-memory-write-list/batches
    (:vars* ((root (make-temp-file "mevedel-memory-batches-" t))
             (workspace (mevedel-workspace--create :root root))
             (mevedel-memory-dirs nil)
             (mevedel-journal-cleanup--inhibit-scheduling t)
             (identity (mevedel-workspace-identity-ensure root))
             (bundles (mevedel-test-memory-history-create workspace 2 5 100))
             (rows (mevedel-memory-write-list workspace))
             (ids (mapcar (lambda (row) (plist-get row :id)) rows)))
            :after-each ((remhash workspace mevedel-memory-write--inspected)
                         (delete-directory root t)))
  ,test
  (test)
  :doc "preserves ten ordered identities across corrupt, missing and oversized records"
  (let ((paths (mapcar (lambda (id) (file-name-concat (mevedel-memory-write--directory workspace) (concat id ".el"))) ids)))
    (should (= 10 (length ids)))
    (write-region "(broken" nil (nth 0 paths) nil 'silent)
    (delete-file (nth 7 paths))
    (write-region (make-string (+ 2 mevedel-memory-store--max-bytes) ?x) nil (nth 8 paths) nil 'silent)
    (let ((observed (mevedel-memory-write-list workspace ids)))
      (should (equal ids (mapcar (lambda (row) (plist-get row :id)) observed)))
      (cl-loop for row in observed for position from 0
               do (if (memq position '(0 7 8))
                      (should (plist-get row :error))
                    (should (plist-get row :intent))))))
  :doc "signalled and incomplete responses preserve errors and independent later batches"
  (let ((runner (symbol-function 'mevedel-session-control-fs-run-program)))
    (dolist (failure '(signal incomplete))
      (let ((injected nil))
        (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                   (lambda (ops &rest args)
                     (if (and (not injected) (= (length ops) 8)
                              (string-prefix-p (mevedel-memory-write--directory workspace) (plist-get (car ops) :path)))
                         (progn (setq injected t)
                                (if (eq failure 'signal) (error "Injected write batch transport failure")
                                  (butlast (apply runner ops args))))
                       (apply runner ops args)))))
          (let ((observed (mevedel-memory-write-list workspace ids)))
            (should injected)
            (should (equal ids (mapcar (lambda (row) (plist-get row :id)) observed)))
            (dolist (row (seq-take observed 8)) (should (plist-get row :error)))
            (dolist (row (nthcdr 8 observed)) (should (plist-get row :intent)))))))))

(mevedel-deftest mevedel-memory-write-list/authenticated-batches
    (:vars* ((root (make-temp-file "mevedel-memory-auth-batches-" t))
             (workspace (mevedel-workspace--create :root root))
             (mevedel-memory-dirs nil)
             (mevedel-journal-cleanup--inhibit-scheduling t)
             (identity (mevedel-workspace-identity-ensure root))
             (bundle (car (mevedel-test-memory-history-create workspace 1 16 100)))
             (rows (mevedel-memory-write-list workspace))
             (ids (mapcar (lambda (row) (plist-get row :id)) rows)))
            :after-each ((remhash workspace mevedel-memory-write--inspected)
                         (delete-directory root t)))
  ,test
  (test)
  :doc "a later authenticated-group batch failure preserves earlier rows and all identities"
  (let ((runner (symbol-function 'mevedel-session-control-fs-run-program)))
    (should (= 16 (length ids)))
    (dolist (failure '(signal incomplete))
      (let ((batches 0) failed)
        (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                   (lambda (ops &rest args)
                     (when (and (= (length ops) 8)
                                (string-prefix-p (mevedel-memory-write--directory workspace) (plist-get (car ops) :path)))
                       (cl-incf batches))
                     ;; Two hint batches precede the two authenticated batches.
                     (if (and (= batches 4) (= (length ops) 8))
                         (progn
                           (setq failed (mapcar (lambda (op) (file-name-sans-extension (file-name-nondirectory (plist-get op :path)))) ops))
                           (if (eq failure 'signal) (error "Injected authenticated batch failure")
                             (butlast (apply runner ops args))))
                       (apply runner ops args)))))
          (let ((observed (mevedel-memory-write-list workspace ids)))
            (should (= 4 batches))
            (should (= 8 (length failed)))
            (should (equal ids (mapcar (lambda (row) (plist-get row :id)) observed)))
            (dolist (row observed)
              (if (member (plist-get row :id) failed)
                  (should (plist-get row :error))
                (should (plist-get row :intent))))))))))

(mevedel-deftest mevedel-memory-decision--read/reuse
    (:vars* ((root (make-temp-file "mevedel-memory-reuse-" t))
             (workspace (mevedel-workspace--create :root root))
             (mevedel-memory-dirs nil)
             (mevedel-journal-cleanup--inhibit-scheduling t)
             (identity (mevedel-workspace-identity-ensure root))
             (bundle (car (mevedel-test-memory-history-create workspace 1 1 100)))
             (intent (plist-get (car (mevedel-memory-write-list workspace)) :intent))
             (entry (mevedel-memory-decision-status workspace (plist-get intent :proposal)))
             (path (file-name-concat (mevedel-memory-decision--directory workspace) (concat (plist-get entry :decision-id) ".el"))))
            :after-each ((remhash workspace mevedel-memory-write--inspected)
                         (delete-directory root t)))
  ,test
  (test)
  :doc "only exact identity, intent hash and source hash reuse the authenticated observation"
  (let ((reader (symbol-function 'mevedel-memory-write-read)))
    (dolist (mismatch '(nil identity intent-hash source-hash))
      (let ((writes (make-hash-table :test #'equal)) (passes (make-hash-table :test #'equal))
            (observed (copy-tree intent)) (calls 0))
        (pcase mismatch
          ('identity (plist-put observed :id (make-string 64 ?f)))
          ('intent-hash (plist-put observed :hash (make-string 64 ?f)))
          ('source-hash (plist-put (plist-get observed :accepted) :hash (make-string 64 ?f))))
        (puthash (plist-get intent :id) observed writes)
        (puthash (plist-get intent :pass) bundle passes)
        (cl-letf (((symbol-function 'mevedel-memory-write-read)
                   (lambda (&rest args) (cl-incf calls) (apply reader args))))
          (should (equal (plist-get intent :id)
                         (plist-get (plist-get (mevedel-memory-decision--read workspace path passes writes) :intent) :id))))
        (should (= (if mismatch 1 0) calls)))))
  :doc "reusing writes never conceals changed decision bytes or claim and outcome evidence"
  (let* ((accepted (mevedel-memory-decision--published workspace entry))
         (claim (plist-get accepted :claim))
         (writes (make-hash-table :test #'equal)))
    (puthash (plist-get intent :id) intent writes)
    (dolist (changed (list path (mevedel-journal-claim--path claim nil) (mevedel-journal-claim--path claim t)))
      (let ((text (mevedel-session-control-fs-read-file changed)))
        (unwind-protect
            (progn
              (write-region (if (equal changed path) (concat text "\n") "{}") nil changed nil 'silent)
              (should-error (mevedel-memory-decision--read workspace path nil writes)))
          (write-region text nil changed nil 'silent))))))

(provide 'test-mevedel-memory-observation)
;;; test-mevedel-memory-observation.el ends here
