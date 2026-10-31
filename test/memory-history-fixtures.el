;;; memory-history-fixtures.el --- Real retained memory history -*- lexical-binding: t -*-

;;; Commentary:
;; Shared fixture construction without registering ERT cases on import.
;;
;; Building authentic history costs about a hundred control programs per
;; applied decision.  Multi-case suites build it once per worker with
;; `mevedel-test-memory-history-root' and `mevedel-test-memory-history-build',
;; then give each case a private copy of the immutable result.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-journal-claim)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-scope)
(require 'mevedel-memory-store)

(defmacro mevedel-test-memory-history-owned (workspace &rest body)
  "Run BODY under a fresh successor consolidation claim for WORKSPACE."
  (declare (indent 1))
  (let ((claim (make-symbol "claim")))
    `(let ((,claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory ,workspace) 180)))
       (should ,claim)
       (unwind-protect (progn ,@body)
         (mevedel-journal-claim-settle ,claim 'cancelled "")))))

(defun mevedel-test-memory-history-create (workspace batches items bytes &optional pending)
  "Create BATCHES accepted passes in WORKSPACE with ITEMS decisions each.
Each shared note contains BYTES bytes.  Return the accepted bundles.
PENDING leaves their proposals undecided.  Applications use the native owned
path under a fresh workspace claim each; they skip the per-call recovery of
`mevedel-memory-decision-apply', a no-op on this fully published history."
  (let* ((root (mevedel-workspace-root workspace))
         (shared (file-name-concat root ".mevedel" "shared")) bundles)
    (make-directory shared t)
    (dotimes (batch batches (nreverse bundles))
      (dotimes (item items)
        (let ((path (file-name-concat shared (format "note-%d-%d.md" batch item))))
          (write-region (make-string bytes ?x) nil path nil 'silent)
          (set-file-times path (seconds-to-time (- (float-time) (* 15 86400))))))
      (let* ((capture (mevedel-memory-notes-capture workspace nil :mode 'auto))
             (scope (mevedel-memory-scope-capture workspace))
             (claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)))
        (unwind-protect
            (let* ((scope (plist-put scope :roots (append (plist-get scope :roots) (list (plist-get capture :root)))))
                   (prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
                   (notes (mapcar (lambda (document) (plist-get document :id))
                                  (mevedel-memory-notes-documents scope)))
                   (reply (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n"
                                  "## Remove\n- none\n## Instructions\n- none\n## No action\n- none"))
                   (accepted (mevedel-memory-store-accept workspace prepared reply nil "test:model" nil notes)))
              (mevedel-memory-store-publish workspace (plist-get prepared :id))
              (unless pending
                (dolist (proposal (plist-get accepted :proposals))
                  (let ((owner (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)))
                    (should owner)
                    (unwind-protect
                        (should (eq 'applied
                                    (plist-get (mevedel-memory-decision--apply-owned
                                                workspace (list :claim owner) (plist-get prepared :id) (plist-get proposal :id))
                                               :status)))
                      (mevedel-journal-claim-settle owner 'cancelled "")))))
              (push accepted bundles))
          (mevedel-journal-claim-settle claim 'cancelled ""))))))

(defvar mevedel-test-memory-history--snapshots (make-hash-table :test #'equal)
  "Built histories by key: (DIRECTORY . VALUE), private to this worker.")

(defun mevedel-test-memory-history--directory (key)
  "Return KEY's worker-private fixture directory."
  (file-name-concat mevedel-test--worker-home "memory-history" (md5 (prin1-to-string key))))

(defun mevedel-test-memory-history--copy (value)
  "Return a copy of plain-data VALUE sharing no strings or conses with it."
  (let (print-length print-level)
    (car (read-from-string (prin1-to-string value)))))

(defun mevedel-test-memory-history--forget (root)
  "Drop process-wide `mevedel-' hash entries whose key names ROOT.
Each case at a reused fixed path then starts from the caches a never-seen
root would have, so no topic rows, identities or proofs carry over."
  (let ((names (delete-dups (list (directory-file-name (expand-file-name root))
                                  (directory-file-name (file-truename root))))))
    (mapatoms
     (lambda (symbol)
       (when (and (boundp symbol) (string-prefix-p "mevedel-" (symbol-name symbol))
                  (hash-table-p (symbol-value symbol)))
         (let ((table (symbol-value symbol)) stale)
           (maphash (lambda (key _)
                      (let ((text (prin1-to-string key)))
                        (when (seq-some (lambda (name) (string-search name text)) names)
                          (push key stale))))
                    table)
           (dolist (key stale) (remhash key table))))))))

(defun mevedel-test-memory-history-root (key)
  "Return a fresh workspace root for KEY, holding its built history if any.
Retained history embeds and authenticates its absolute root, so a copy is
valid only where it was built: every case of KEY uses one fixed path in the
worker's home, emptied and refilled from the immutable snapshot each time.
Process caches naming that path are dropped first."
  (let ((root (file-name-concat (mevedel-test-memory-history--directory key) "root"))
        (snapshot (car (gethash key mevedel-test-memory-history--snapshots))))
    (when (file-exists-p root) (delete-directory root t))
    (mevedel-test-memory-history--forget root)
    (make-directory (file-name-directory root) t)
    (if snapshot (copy-directory snapshot root t) (make-directory root))
    root))

(defun mevedel-test-memory-history-build (key root build)
  "Return KEY's history value, calling BUILD in ROOT only on first use.
ROOT comes from `mevedel-test-memory-history-root'.  The first call snapshots
ROOT after BUILD; later calls return a copy of BUILD's value, matching the
files the root call restored."
  (if-let* ((snapshot (gethash key mevedel-test-memory-history--snapshots)))
      (mevedel-test-memory-history--copy (cdr snapshot))
    (unless (equal root (file-name-concat (mevedel-test-memory-history--directory key) "root"))
      (error "History fixture %S built outside its fixed root" key))
    (let ((value (funcall build))
          (snapshot (file-name-concat (mevedel-test-memory-history--directory key) "snapshot")))
      (when (file-exists-p snapshot) (delete-directory snapshot t))
      (copy-directory root snapshot t)
      (let ((copy (mevedel-test-memory-history--copy value)))
        (unless (equal copy value) (error "History fixture %S value is not plain data" key))
        (puthash key (cons snapshot copy) mevedel-test-memory-history--snapshots))
      value)))

(provide 'memory-history-fixtures)
;;; memory-history-fixtures.el ends here
