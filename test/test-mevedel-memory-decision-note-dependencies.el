;;; test-mevedel-memory-decision-note-dependencies.el --- Note dependency transactions -*- lexical-binding: t -*-

;;; Commentary:

;; Parsed extraction and instruction repairs precede checked note disposal.
;; Fresh permanent citations and modification age remain live mutation gates.

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
(require 'mevedel-system)

(mevedel-deftest mevedel-memory-decision-note-dependencies
    (:vars* ((root (make-temp-file "mevedel-note-dependencies-" t))
             (memory (file-name-concat root ".mevedel" "memory"))
             (shared (file-name-concat root ".mevedel" "shared"))
             (workspace (mevedel-workspace--create :root root))
             (identity (mevedel-workspace-identity-ensure root))
             (mevedel-memory-dirs (list memory))
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
     :after-each ((when claim (mevedel-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (cl-labels
      ((read-file (path) (mevedel-session-control-fs-read-file path))
       (uncite ()
         (write-region "Project context without temporary citations.\n" nil topic-path nil 'silent)
         (write-region new-instructions nil instructions-path nil 'silent))
       (prepare (&optional repairs absent-instructions)
         (setq scope (mevedel-memory-scope-capture workspace))
         (let* ((capture (mevedel-memory-notes-capture workspace nil :mode 'auto :citation-scope scope))
                (memory-id (car (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'memory)) (plist-get scope :roots))))
                (instruction-id (car (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'instructions)) (plist-get scope :roots))))
                (note-id "work://shared/task.md"))
           (should (plist-get capture :root))
           (setf (plist-get scope :roots) (append (plist-get scope :roots) (list (plist-get capture :root))))
           (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
           (should claim)
           (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
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
                    (format (concat "## Notes\n```proposal\nnote: %S\n---\n```\n"
                                    "## No action\n- none") note-id))))
             (setq pass (plist-get prepared :id)
                   accepted (mevedel-memory-store-accept workspace prepared reply nil "test:model" nil (list note-id))
                   note (seq-find (lambda (item) (eq (plist-get item :action) 'note-delete)) (plist-get accepted :proposals))
                   extraction (seq-find (lambda (item) (eq (plist-get item :action) 'update)) (plist-get accepted :proposals))
                   instruction (seq-find (lambda (item) (eq (plist-get item :action) 'instructions)) (plist-get accepted :proposals)))
             (should note)
             (mevedel-memory-store-publish workspace pass))))
       (apply-item (item)
         (mevedel-memory-decision-apply workspace pass (plist-get item :id)))
       (input () (mevedel-memory-decision--application-input accepted note workspace)))
    ,test)
  (test)
  :doc "all citing repairs apply before delete and extraction reversal restores its note first"
  (progn
    (prepare t)
    (should (equal (sort (copy-sequence (plist-get note :requires)) #'string<)
                   (sort (list (plist-get extraction :id) (plist-get instruction :id)) #'string<)))
    (should (eq (plist-get (input) :status) 'waiting))
    (should (eq (plist-get (apply-item extraction) :status) 'applied))
    (should-error (apply-item note) :type 'user-error)
    (should-not (mevedel-memory-decision-status workspace (plist-get note :id)))
    (should (equal old-instructions (read-file instructions-path)))
    (should (eq (plist-get (apply-item instruction) :status) 'applied))
    (should (equal new-instructions (read-file instructions-path)))
    (should (eq (plist-get (apply-item note) :status) 'applied))
    (should-not (file-exists-p note-path))
    (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get instruction :id)) :status)))
    (should (equal old-note (read-file note-path)))
    (should (= #o640 (file-modes note-path)))
    (should (equal old-instructions (read-file instructions-path)))
    (should (eq 'reversed (plist-get (mevedel-memory-decision-status workspace (plist-get note :id)) :status))))
  :doc "pre-existing citations dynamically hold delete without manufacturing a keep"
  (progn
    (prepare)
    (should-not (plist-get note :requires))
    (should (eq 'waiting (plist-get (input) :status)))
    (should-error (apply-item note) :type 'user-error)
    (should (equal old-note (read-file note-path)))
    (should-not (mevedel-memory-decision-status workspace (plist-get note :id)))
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
      (should-error (apply-item note) :type 'user-error)
      (should-not (mevedel-memory-decision-status workspace (plist-get note :id)))
      (write-region "Durable context without that citation.\n" nil new-topic nil 'silent)
      (should (eq 'applied (plist-get (apply-item note) :status)))
      (should-not (file-exists-p note-path))))
  :doc "a citation introduced during quota IO holds without a terminal decision or pending marker"
  (progn
    (uncite)
    (prepare)
    (should (eq 'fresh (plist-get (input) :status)))
    (let ((original (symbol-function 'mevedel-memory-notes--quota)) injected)
      (cl-letf (((symbol-function 'mevedel-memory-notes--quota)
                 (lambda (&rest arguments)
                   (prog1 (apply original arguments)
                     (setq injected t)
                     (write-region "Late citation: work://shared/task.md\n" nil topic-path nil 'silent)))))
        (should-error (apply-item note) :type 'mevedel-memory-decision-held))
      (should injected))
    (should (equal old-note (read-file note-path)))
    (should-not (mevedel-memory-decision-status workspace (plist-get note :id)))
    ;; Uninspected immutable intents are candidates even when unmarked.  Prove
    ;; marker retirement through authenticated inspection, not that hint list.
    (let ((writes (mevedel-memory-write-list workspace)))
      (should (= 1 (length writes)))
      (should-not (plist-get (car writes) :error))
      (should-not (plist-get (car writes) :marked))
      (should-not (file-exists-p (mevedel-memory-write--pin (plist-get (car writes) :intent)))))
    (should-not (mevedel-memory-write-recovery-candidates workspace))
    (uncite)
    (should (eq 'applied (plist-get (apply-item note) :status))))
  :doc "same-byte touch after approval capture resets age and remains a nonterminal hold"
  (progn
    (uncite)
    (prepare)
    (should (eq 'fresh (plist-get (input) :status)))
    (set-file-times note-path)
    (should (eq 'waiting (plist-get (input) :status)))
    (should-error (apply-item note) :type 'user-error)
    (should-not (mevedel-memory-decision-status workspace (plist-get note :id)))
    (should (equal old-note (read-file note-path)))
    (set-file-times note-path (seconds-to-time (- (float-time) (* 15 86400))))
    (should (eq 'applied (plist-get (apply-item note) :status))))
  :doc "empty full instructions create an admitted absent file and reverse to absence after note restore"
  (progn
    (uncite)
    (prepare nil t)
    (let ((local (file-name-concat root "AGENTS.local.md")))
      (should-not (file-exists-p local))
      (should (eq 'applied (plist-get (apply-item instruction) :status)))
      (should (file-exists-p local))
      (should (= 0 (file-attribute-size (file-attributes local))))
      (should (eq 'applied (plist-get (apply-item note) :status)))
      (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get instruction :id)) :status)))
      (should-not (file-exists-p local))
      (should (equal old-note (read-file note-path)))))
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
    (should-not (mevedel-memory-decision-status workspace (plist-get note :id)))
    (should (equal old-note (read-file note-path))))
  :doc "a rejected citing extraction cannot authorize deletion of its source"
  (progn
    (prepare t)
    (should (eq 'rejected (plist-get (mevedel-memory-decision-reject workspace pass (plist-get extraction :id)) :status)))
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

(provide 'test-mevedel-memory-decision-note-dependencies)
;;; test-mevedel-memory-decision-note-dependencies.el ends here
