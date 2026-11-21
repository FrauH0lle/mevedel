;;; test-niwa-memory-decision-note-dependencies.el --- Note dependency transactions -*- lexical-binding: t -*-

;;; Commentary:

;; Parsed extraction and instruction repairs precede checked note disposal.
;; Fresh permanent citations and modification age remain live mutation gates.

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
(require 'niwa-system)

(niwa-deftest niwa-memory-decision-note-dependencies
    (:vars* ((root (make-temp-file "niwa-note-dependencies-" t))
             (memory (file-name-concat root ".niwa" "memory"))
             (shared (file-name-concat root ".niwa" "shared"))
             (workspace (niwa-workspace--create :root root))
             (identity (niwa-workspace-identity-ensure root))
             (niwa-memory-dirs (list memory))
             (note-path (file-name-concat shared "task.md"))
             (topic-path (file-name-concat memory "context.md"))
             (instructions-path (file-name-concat root "AGENTS.md"))
             (old-note "# Handoff\nTODO: follow up; owner still has open work.\nCounts\t17\t23\n")
             (old-instructions "Keep unrelated guidance.\nSee work://shared/task.md for temporary evidence.\n")
             (new-instructions "Keep unrelated guidance.\n")
             scope accepted note extraction instruction pass claim)
     :before-each ((make-directory memory t)
                   (make-directory shared t)
                   (write-region old-note nil note-path nil 'silent)
                   (set-file-modes note-path #o640)
                   (set-file-times note-path (seconds-to-time (- (float-time) (* 15 86400))))
                   (write-region "- [Context](context.md) - project context\n" nil
                                 (file-name-concat memory "MEMORY.md") nil 'silent)
                   (write-region "Project context; evidence: work://shared/task.md\n" nil topic-path nil 'silent)
                   (write-region old-instructions nil instructions-path nil 'silent))
     :after-each ((when claim (niwa-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (cl-labels
      ((read-file (path) (niwa-session-control-fs-read-file path))
       (uncite ()
         (write-region "Project context without temporary citations.\n" nil topic-path nil 'silent)
         (write-region new-instructions nil instructions-path nil 'silent))
       (prepare (&optional repairs absent-instructions)
         (setq scope (niwa-memory-scope-capture workspace))
         (let* ((capture (niwa-memory-notes-capture workspace nil :mode 'auto :citation-scope scope))
                (memory-id (car (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'memory)) (plist-get scope :roots))))
                (instruction-id (car (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'instructions)) (plist-get scope :roots))))
                (note-id "work://shared/task.md"))
           (should (plist-get capture :root))
           (setf (plist-get scope :roots) (append (plist-get scope :roots) (list (plist-get capture :root))))
           (setq claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
           (should claim)
           (let* ((prepared (niwa-memory-store-prepare workspace claim scope nil ""))
                  (reply
                   (concat
                    "## Promote\n- none\n## Update\n"
                    (if repairs
                        (format (concat "```proposal\nroot: %S\nfile: \"context.md\"\ntype: \"project\"\n"
                                        "title: \"Context\"\nhook: \"project context\"\nreason: \"Repair temporary citation\"\n"
                                        "evidence: [%S]\n---\nDurable project context.\n```\n") memory-id note-id)
                      "- none\n")
                    "## Merge\n- none\n## Remove\n- none\n## Instructions\n"
                    (if (or repairs absent-instructions)
                        (format (concat "```proposal\nroot: %S\nfile: %S\ntype: \"project\"\n"
                                        "title: \"Workspace guidance\"\nhook: \"workspace conventions\"\nreason: \"Repair temporary citation\"\n"
                                        "evidence: [%S]\n---\n%s\n```\n")
                                instruction-id (if absent-instructions "AGENTS.local.md" "AGENTS.md") note-id
                                (if absent-instructions "" new-instructions))
                      "- none\n")
                    "## No action\n- none")))
             (setq pass (plist-get prepared :id)
                   accepted (niwa-memory-store-accept workspace prepared reply nil "test:model" nil (list note-id))
                   note (seq-find (lambda (item) (eq (plist-get item :action) 'note-delete)) (plist-get accepted :proposals))
                   extraction (seq-find (lambda (item) (eq (plist-get item :action) 'update)) (plist-get accepted :proposals))
                   instruction (seq-find (lambda (item) (eq (plist-get item :action) 'instructions)) (plist-get accepted :proposals)))
             (should note)
             (niwa-memory-store-publish workspace pass))))
       (apply-item (item)
         (niwa-memory-decision-apply workspace pass (plist-get item :id)))
       (input () (plist-get (niwa-memory-decision-inspect workspace :pass pass :proposal (plist-get note :id)) :check)))
    ,test)
  (test)
  :doc "all citing repairs apply before delete and extraction reversal never restores its note"
  (progn
    (prepare t)
    (should (equal (sort (copy-sequence (plist-get note :requires)) #'string<)
                   (sort (list (plist-get extraction :id) (plist-get instruction :id)) #'string<)))
    (should (eq (plist-get (input) :status) 'waiting))
    (should (eq (plist-get (apply-item extraction) :status) 'applied))
    (should (eq 'waiting (plist-get (apply-item note) :status)))
    (should-not (niwa-memory-decision-status workspace (plist-get note :id)))
    (should (equal old-instructions (read-file instructions-path)))
    (should (eq (plist-get (apply-item instruction) :status) 'applied))
    (should (equal new-instructions (read-file instructions-path)))
    (should (eq (plist-get (apply-item note) :status) 'applied))
    (should-not (file-exists-p note-path))
    (should (eq 'reversed (plist-get (niwa-memory-decision-reverse workspace pass (plist-get instruction :id)) :status)))
    (should-not (file-exists-p note-path))
    (should (equal old-instructions (read-file instructions-path)))
    (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get note :id)) :status))))
  :doc "pre-existing citations dynamically hold delete without manufacturing a keep"
  (progn
    (prepare)
    (should-not (plist-get note :requires))
    (should (eq 'waiting (plist-get (input) :status)))
    (should (eq 'held (plist-get (apply-item note) :status)))
    (should (equal old-note (read-file note-path)))
    (should-not (niwa-memory-decision-status workspace (plist-get note :id)))
    (uncite)
    (should (eq 'fresh (plist-get (input) :status)))
    (should (eq 'applied (plist-get (apply-item note) :status)))
    (should-not (file-exists-p note-path)))
  :doc "new permanent files after capture hold deletion until their citation is repaired"
  (progn
    (uncite)
    (prepare)
    (should (eq 'fresh (plist-get (input) :status)))
    (let ((new-topic (file-name-concat memory "later.md")))
      (write-region "See work://shared/task.md\n" nil new-topic nil 'silent)
      (should (eq 'waiting (plist-get (input) :status)))
      (should (eq 'held (plist-get (apply-item note) :status)))
      (should-not (niwa-memory-decision-status workspace (plist-get note :id)))
      (write-region "Durable context without that citation.\n" nil new-topic nil 'silent)
      (should (eq 'applied (plist-get (apply-item note) :status)))
      (should-not (file-exists-p note-path))))
  :doc "a citation introduced at final write admission holds without a terminal decision or pending marker"
  (progn
    (uncite)
    (prepare)
    (should (eq 'fresh (plist-get (input) :status)))
    (let ((original (symbol-function 'niwa-memory-notes-commit)) injected)
      (cl-letf (((symbol-function 'niwa-memory-notes-commit)
                 (lambda (&rest arguments)
                   (setq injected t)
                   (write-region "Late citation: work://shared/task.md\n" nil topic-path nil 'silent)
                   (apply original arguments))))
        (should (eq 'held (plist-get (apply-item note) :status))))
      (should injected))
    (should (equal old-note (read-file note-path)))
    (should-not (niwa-memory-decision-status workspace (plist-get note :id)))
    (uncite)
    (should (eq 'applied (plist-get (apply-item note) :status))))
  :doc "same-byte touch after approval capture resets age and remains a nonterminal hold"
  (progn
    (uncite)
    (prepare)
    (should (eq 'fresh (plist-get (input) :status)))
    (set-file-times note-path)
    (should (eq 'waiting (plist-get (input) :status)))
    (should (eq 'held (plist-get (apply-item note) :status)))
    (should-not (niwa-memory-decision-status workspace (plist-get note :id)))
    (should (equal old-note (read-file note-path)))
    (set-file-times note-path (seconds-to-time (- (float-time) (* 15 86400))))
    (should (eq 'applied (plist-get (apply-item note) :status))))
  :doc "empty full instructions create an admitted absent file and reverse without note restoration"
  (progn
    (uncite)
    (prepare nil t)
    (let ((local (file-name-concat root "AGENTS.local.md")))
      (should-not (file-exists-p local))
      (should (eq 'applied (plist-get (apply-item instruction) :status)))
      (should (file-exists-p local))
      (should (= 0 (file-attribute-size (file-attributes local))))
      (should (eq 'applied (plist-get (apply-item note) :status)))
      (should (eq 'reversed (plist-get (niwa-memory-decision-reverse workspace pass (plist-get instruction :id)) :status)))
      (should-not (file-exists-p local))
      (should-not (file-exists-p note-path))))
  :doc "a stale instruction repair never deletes the note or overwrites intervening guidance"
  (progn
    (prepare t)
    (should (eq 'applied (plist-get (apply-item extraction) :status)))
    (write-region "Foreign guidance.\n" nil instructions-path nil 'silent)
    (should (eq 'stale (plist-get (apply-item instruction) :status)))
    (should (eq 'stale (plist-get (apply-item note) :status)))
    (should (equal "Foreign guidance.\n" (read-file instructions-path)))
    (should (equal old-note (read-file note-path))))
  :doc "an unchanged full instruction body performs no write and cannot erase a surviving citation"
  (progn
    (setq new-instructions old-instructions)
    (prepare t)
    (should (eq 'applied (plist-get (apply-item extraction) :status)))
    (let ((modified (file-attribute-modification-time (file-attributes instructions-path))))
      (should (eq 'applied (plist-get (apply-item instruction) :status)))
      (should (equal modified (file-attribute-modification-time (file-attributes instructions-path)))))
    (should (equal old-instructions (read-file instructions-path)))
    (should (eq 'waiting (plist-get (input) :status)))
    (should-not (niwa-memory-decision-status workspace (plist-get note :id)))
    (should (equal old-note (read-file note-path))))
  :doc "a rejected citing extraction cannot authorize deletion of its source"
  (progn
    (prepare t)
    (should (eq 'rejected (plist-get (niwa-memory-decision-reject workspace pass (plist-get extraction :id)) :status)))
    (should (eq 'stale (plist-get (apply-item note) :status)))
    (should (equal old-note (read-file note-path)))
    (should (equal old-instructions (read-file instructions-path))))
  :doc "an unsaved full instruction repair leaves disk and dependent note unchanged"
  (progn
    (prepare t)
    (should (eq 'applied (plist-get (apply-item extraction) :status)))
    (let ((buffer (find-file-noselect instructions-path)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "Unsaved user guidance.\n")
            (should-error (apply-item instruction))
            (should (buffer-modified-p))
            (should (string-suffix-p "Unsaved user guidance.\n" (buffer-string)))
            (should (eq 'waiting (plist-get (input) :status)))
            (should (equal old-note (read-file note-path)))
            (should (equal old-instructions (read-file instructions-path))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(provide 'test-niwa-memory-decision-note-dependencies)
;;; test-niwa-memory-decision-note-dependencies.el ends here
