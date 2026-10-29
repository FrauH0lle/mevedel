;;; memory-history-fixtures.el --- Real retained memory history -*- lexical-binding: t -*-

;;; Commentary:
;; Shared fixture construction without registering ERT cases on import.

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
PENDING leaves their proposals undecided."
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
                  (should (eq 'applied (plist-get (mevedel-memory-decision-apply
                                                   workspace (plist-get prepared :id) (plist-get proposal :id)) :status)))))
              (push accepted bundles))
          (mevedel-journal-claim-settle claim 'cancelled ""))))))

(provide 'memory-history-fixtures)
;;; memory-history-fixtures.el ends here
