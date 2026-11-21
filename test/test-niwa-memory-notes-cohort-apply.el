;;; test-niwa-memory-notes-cohort-apply.el --- Exact cohort disposal -*- lexical-binding: t -*-

;;; Commentary:

;; Closed-cohort capture, parsed acceptance and real checked decisions prove
;; that same-byte recreation after review cannot inherit disposal authority.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-memory-decision)
(require 'niwa-memory-notes)
(require 'niwa-memory-scope)
(require 'niwa-memory-store)
(require 'niwa-memory-write)
(require 'niwa-session-control-fs)

(niwa-deftest niwa-memory-notes-cohort-apply
    (:vars* ((root (file-name-as-directory (make-temp-file "cohort-apply-" t)))
             (workspace (niwa-workspace--create :root root))
             (identity (niwa-workspace-identity-ensure root))
             (shared (file-name-concat root ".niwa" "shared"))
             (path (file-name-concat shared "note.md"))
             (body "# Observation\nThe temporary finding has no qualifying durable extraction.\n")
             (niwa-memory-dirs nil)
             before note accepted pass claim)
     :before-each ((make-directory shared t)
                   (write-region body nil path nil 'silent)
                   (set-file-modes path #o640)
                   (set-file-times path (seconds-to-time (- (float-time) (* 15 86400)))))
     :after-each ((when claim (niwa-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (cl-labels
      ((prepare (&optional ordinary)
         (setq claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
         (should claim)
         (let* ((cohort (unless ordinary (niwa-memory-notes-cohort workspace nil)))
                (captured (if ordinary (niwa-memory-notes-capture workspace nil :mode 'propose)
                            (niwa-memory-notes-capture workspace nil :mode 'propose :cohort cohort)))
                (row (plist-get captured :root))
                (scope (niwa-memory-scope-capture workspace))
                (reply (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n"
                               "## Remove\n- none\n## Instructions\n- none\n## No action\n- none")))
           (should row)
           (setq before (cdr (assoc "note.md" (plist-get (cdr row) :before))))
           (if ordinary (should-not (plist-get before :cohort-version))
             (should (plist-get before :cohort-version)))
           (setf (plist-get scope :roots) (append (plist-get scope :roots) (list row)))
           (let ((prepared (niwa-memory-store-prepare workspace claim scope nil "")))
             (setq pass (plist-get prepared :id)
                   accepted (niwa-memory-store-accept workspace prepared reply nil "test:model" nil '("work://shared/note.md"))
                   note (car (plist-get accepted :proposals)))
             (should (eq 'note-delete (plist-get note :action)))
             (niwa-memory-store-publish workspace pass))))
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
           (should (niwa-memory-notes-current-p before))))
       (apply-note () (niwa-memory-decision-apply workspace pass (plist-get note :id)))
       (unchanged ()
         (should (equal body (niwa-session-control-fs-read-file path)))
         (should (= #o640 (file-modes path)))
         (should-not (niwa-memory-decision-status workspace (plist-get note :id)))))
    ,test)
  (test)
  :doc "unchanged frozen input passes the real decision boundary and checked deletion"
  (progn
    (prepare)
    (should (eq 'fresh (plist-get (plist-get (niwa-memory-decision-inspect workspace :pass pass :proposal (plist-get note :id))
                                             :check)
                                  :status)))
    (should (eq 'applied (plist-get (apply-note) :status)))
    (should-not (file-exists-p path)))
  :doc "same-byte recreation with restored old mtime and mode is a nonterminal hold"
  (progn
    (prepare)
    (recreate)
    (let ((input (plist-get (niwa-memory-decision-inspect workspace :pass pass :proposal (plist-get note :id)) :check)))
      (should (eq 'waiting (plist-get input :status))))
    ;; The program-backed hold runs at the deletion boundary, before election.
    (should (eq 'held (plist-get (apply-note) :status)))
    (unchanged))
  :doc "late recreation at final write admission is held immediately before mutation"
  (progn
    (prepare)
    (let ((commit (symbol-function 'niwa-memory-notes-commit)) injected)
      (cl-letf (((symbol-function 'niwa-memory-notes-commit)
                 (lambda (&rest args)
                   (unless injected (setq injected t) (recreate))
                   (apply commit args))))
        (should (eq 'held (plist-get (apply-note) :status))))
      (should injected))
    (unchanged))
  :doc "ordinary noncohort capture retains byte-mode-age semantics rather than gaining a new identity lock"
  (progn
    (prepare t)
    (recreate)
    (should (eq 'applied (plist-get (apply-note) :status)))
    (should-not (file-exists-p path))))

(provide 'test-niwa-memory-notes-cohort-apply)
;;; test-niwa-memory-notes-cohort-apply.el ends here
