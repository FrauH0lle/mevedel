;;; test-mevedel-memory-notes-cohort-apply.el --- Exact cohort disposal -*- lexical-binding: t -*-

;;; Commentary:

;; Closed-cohort capture, parsed acceptance and real checked decisions prove
;; that same-byte recreation after review cannot inherit disposal authority.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-scope)
(require 'mevedel-memory-store)
(require 'mevedel-memory-write)
(require 'mevedel-session-control-fs)

(mevedel-deftest mevedel-memory-notes-cohort-apply
    (:vars* ((root (file-name-as-directory (make-temp-file "cohort-apply-" t)))
             (workspace (mevedel-workspace--create :root root))
             (identity (mevedel-workspace-identity-ensure root))
             (shared (file-name-concat root ".mevedel" "shared"))
             (path (file-name-concat shared "note.md"))
             (body "# Observation\nThe temporary finding has no qualifying durable extraction.\n")
             (mevedel-memory-dirs nil)
             before note accepted pass claim)
     :before-each ((make-directory shared t)
                   (write-region body nil path nil 'silent)
                   (set-file-modes path #o640)
                   (set-file-times path (seconds-to-time (- (float-time) (* 15 86400)))))
     :after-each ((when claim (mevedel-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (cl-labels
      ((prepare (&optional ordinary)
         (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
         (should claim)
         (let* ((cohort (unless ordinary (mevedel-memory-notes-cohort workspace nil)))
                (captured (if ordinary (mevedel-memory-notes-capture workspace nil :mode 'propose)
                            (mevedel-memory-notes-capture workspace nil :mode 'propose :cohort cohort)))
                (row (plist-get captured :root))
                (scope (mevedel-memory-scope-capture workspace))
                (reply (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n"
                               "## Remove\n- none\n## Instructions\n- none\n## No action\n- none")))
           (should row)
           (setq before (cdr (assoc "note.md" (plist-get (cdr row) :before))))
           (if ordinary (should-not (plist-get before :cohort-version))
             (should (plist-get before :cohort-version)))
           (setf (plist-get scope :roots) (append (plist-get scope :roots) (list row)))
           (let ((prepared (mevedel-memory-store-prepare workspace claim scope nil "")))
             (setq pass (plist-get prepared :id)
                   accepted (mevedel-memory-store-accept workspace prepared reply nil "test:model" nil '("work://shared/note.md"))
                   note (car (plist-get accepted :proposals)))
             (should (eq 'note-delete (plist-get note :action)))
             (mevedel-memory-store-publish workspace pass))))
       (recreate ()
         (let ((modified (file-attribute-modification-time (file-attributes path)))
               (mode (file-modes path))
               (old-inode (file-attribute-inode-number (file-attributes path))))
           ;; Keep the original alive, outside the shared tree, to ensure the
           ;; replacement cannot coincidentally reuse its filesystem inode.
           (rename-file path (file-name-concat root "original.md"))
           (write-region body nil path nil 'silent)
           (set-file-modes path mode)
           (set-file-times path modified)
           (should (equal modified (file-attribute-modification-time (file-attributes path))))
           (should (= mode (file-modes path)))
           (should-not (equal old-inode (file-attribute-inode-number (file-attributes path))))
           (should (mevedel-memory-notes-current-p before))))
       (apply-note () (mevedel-memory-decision-apply workspace pass (plist-get note :id)))
       (unchanged ()
         (should (equal body (mevedel-session-control-fs-read-file path)))
         (should (= #o640 (file-modes path)))
         (should-not (mevedel-memory-decision-status workspace (plist-get note :id)))))
    ,test)
  (test)
  :doc "unchanged frozen input passes the real decision boundary and checked deletion"
  (progn
    (prepare)
    (should (eq 'fresh (plist-get (mevedel-memory-decision--application-input accepted note workspace) :status)))
    (should (eq 'applied (plist-get (apply-note) :status)))
    (should-not (file-exists-p path)))
  :doc "same-byte recreation with restored old mtime and mode is a nonterminal hold"
  (progn
    (prepare)
    (recreate)
    (let ((input (mevedel-memory-decision--application-input accepted note workspace)))
      (should (eq 'waiting (plist-get input :status))))
    (should-error (apply-note) :type 'user-error)
    (unchanged)
    (should-not (mevedel-memory-write-recovery-candidates workspace)))
  :doc "late recreation at final write admission is held immediately before mutation"
  (progn
    (prepare)
    (let ((commit (symbol-function 'mevedel-memory-notes-commit)) injected)
      (cl-letf (((symbol-function 'mevedel-memory-notes-commit)
                 (lambda (&rest args)
                   (unless injected (setq injected t) (recreate))
                   (apply commit args))))
        (should-error (apply-note) :type 'mevedel-memory-decision-held))
      (should injected))
    (unchanged)
    (let ((writes (mevedel-memory-write-list workspace)))
      (should (= 1 (length writes)))
      (should-not (plist-get (car writes) :error))
      (should-not (plist-get (car writes) :marked)))
    (should-not (mevedel-memory-write-recovery-candidates workspace)))
  :doc "ordinary noncohort capture retains byte-mode-age semantics rather than gaining a new identity lock"
  (progn
    (prepare t)
    (recreate)
    (should (eq 'applied (plist-get (apply-note) :status)))
    (should-not (file-exists-p path))))

(provide 'test-mevedel-memory-notes-cohort-apply)
;;; test-mevedel-memory-notes-cohort-apply.el ends here
