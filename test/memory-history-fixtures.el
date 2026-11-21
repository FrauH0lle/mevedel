;;; memory-history-fixtures.el --- Real retained memory history -*- lexical-binding: t -*-

;;; Commentary:
;; Shared fixture construction without registering ERT cases on import.
;;
;; Building authentic history costs about a hundred control programs per
;; applied decision.  Multi-case suites build it once per worker with
;; `niwa-test-memory-history-root' and `niwa-test-memory-history-build',
;; then give each case a private copy of the immutable result.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-journal-claim)
(require 'niwa-memory-decision)
(require 'niwa-memory-notes)
(require 'niwa-memory-scope)
(require 'niwa-memory-store)

(defmacro niwa-test-memory-history-owned (workspace &rest body)
  "Run BODY under a fresh successor consolidation claim for WORKSPACE."
  (declare (indent 1))
  (let ((claim (make-symbol "claim")))
    `(let ((,claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory ,workspace) 180)))
       (should ,claim)
       (unwind-protect (progn ,@body)
         (niwa-journal-claim-settle ,claim 'cancelled "")))))

(defun niwa-test-memory-history-create (workspace batches items bytes &optional pending)
  "Create BATCHES accepted passes in WORKSPACE with ITEMS decisions each.
Each shared note contains BYTES bytes.  Return the accepted bundles.
PENDING leaves their proposals undecided; otherwise apply each normally."
  (let* ((root (niwa-workspace-root workspace))
         (shared (file-name-concat root ".niwa" "shared")) bundles)
    (make-directory shared t)
    (dotimes (batch batches (nreverse bundles))
      (dotimes (item items)
        (let ((path (file-name-concat shared (format "note-%d-%d.md" batch item))))
          (write-region (make-string bytes ?x) nil path nil 'silent)
          (set-file-times path (seconds-to-time (- (float-time) (* 15 86400))))))
      (let* ((capture (niwa-memory-notes-capture workspace nil :mode 'auto))
             (scope (niwa-memory-scope-capture workspace))
             (claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180)))
        (unwind-protect
            (let* ((scope (plist-put scope :roots (append (plist-get scope :roots) (list (plist-get capture :root)))))
                   (prepared (niwa-memory-store-prepare workspace claim scope nil ""))
                   (notes (mapcar (lambda (document) (plist-get document :id))
                                  (niwa-memory-notes-documents scope)))
                   (reply (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n"
                                  "## Remove\n- none\n## Instructions\n- none\n## No action\n- none"))
                   (accepted (niwa-memory-store-accept workspace prepared reply nil "test:model" nil notes)))
              (niwa-memory-store-publish workspace (plist-get prepared :id))
              (unless pending
                (dolist (proposal (plist-get accepted :proposals))
                  (should (eq 'applied (plist-get (niwa-memory-decision-apply
                                                   workspace (plist-get prepared :id) (plist-get proposal :id)) :status)))))
              (push accepted bundles))
          (niwa-journal-claim-settle claim 'cancelled ""))))))

(defvar niwa-test-memory-history--snapshots (make-hash-table :test #'equal)
  "Built histories by key: (DIRECTORY . VALUE), private to this worker.")

(defun niwa-test-memory-history--directory (key)
  "Return KEY's worker-private fixture directory."
  (file-name-concat niwa-test--worker-home "memory-history" (md5 (prin1-to-string key))))

(defun niwa-test-memory-history--copy (value)
  "Return a copy of plain-data VALUE sharing no strings or conses with it."
  (let (print-length print-level)
    (car (read-from-string (prin1-to-string value)))))

(defun niwa-test-memory-history--forget (root)
  "Drop process-wide `niwa-' hash entries whose key names ROOT.
Each case at a reused fixed path then starts from the caches a never-seen
root would have, so no topic rows, identities or proofs carry over."
  (let ((names (delete-dups (list (directory-file-name (expand-file-name root))
                                  (directory-file-name (file-truename root))))))
    (mapatoms
     (lambda (symbol)
       (when (and (boundp symbol) (string-prefix-p "niwa-" (symbol-name symbol))
                  (hash-table-p (symbol-value symbol)))
         (let ((table (symbol-value symbol)) stale)
           (maphash (lambda (key _)
                      (let ((text (prin1-to-string key)))
                        (when (seq-some (lambda (name) (string-search name text)) names)
                          (push key stale))))
                    table)
           ;; Live ownership is a teardown leak, not a disposable read cache.
           (when (memq symbol '(niwa-memory-decision--operations
                                niwa-memory-pass--running
                                niwa-memory-pass--deferred))
             (should-not stale))
           (dolist (key stale) (remhash key table))))))))

(defun niwa-test-memory-history-root (key)
  "Return a fresh workspace root for KEY, holding its built history if any.
Retained history embeds and authenticates its absolute root, so a copy is
valid only where it was built: every case of KEY uses one fixed path in the
worker's home, emptied and refilled from the immutable snapshot each time.
Process caches naming that path are dropped first."
  (let ((root (file-name-concat (niwa-test-memory-history--directory key) "root"))
        (snapshot (car (gethash key niwa-test-memory-history--snapshots))))
    (when (file-exists-p root) (delete-directory root t))
    (niwa-test-memory-history--forget root)
    (make-directory (file-name-directory root) t)
    (if snapshot (copy-directory snapshot root t) (make-directory root))
    root))

(defun niwa-test-memory-history-build (key root build)
  "Return KEY's history value, calling BUILD in ROOT only on first use.
ROOT comes from `niwa-test-memory-history-root'.  The first call snapshots
ROOT after BUILD; later calls return a copy of BUILD's value, matching the
files the root call restored."
  (if-let* ((snapshot (gethash key niwa-test-memory-history--snapshots)))
      (niwa-test-memory-history--copy (cdr snapshot))
    (unless (equal root (file-name-concat (niwa-test-memory-history--directory key) "root"))
      (error "History fixture %S built outside its fixed root" key))
    (let ((value (funcall build))
          (snapshot (file-name-concat (niwa-test-memory-history--directory key) "snapshot")))
      (when (file-exists-p snapshot) (delete-directory snapshot t))
      (copy-directory root snapshot t)
      (let ((copy (niwa-test-memory-history--copy value)))
        (unless (equal copy value) (error "History fixture %S value is not plain data" key))
        (puthash key (cons snapshot copy) niwa-test-memory-history--snapshots))
      value)))

(provide 'memory-history-fixtures)
;;; memory-history-fixtures.el ends here
