;;; test-mevedel-memory-store-notes.el -- Checked delete acceptance -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise accepted delete acknowledgements, extraction dependencies and exact
;; preimages through the ordinary checked decision/write lifecycle.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-journal-claim)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-scope)
(require 'mevedel-memory-store)
(require 'mevedel-memory-write)
(require 'mevedel-session-control-fs)
(require 'mevedel-workspace-identity)

(mevedel-deftest mevedel-memory-store-accept/notes
    (:vars* ((root (file-name-as-directory (file-truename (make-temp-file "mevedel-delete-store-" t))))
             (workspace (mevedel-workspace--create :root root))
             (shared (file-name-concat root ".mevedel" "shared"))
             (memory (file-name-concat root ".agents" "memory"))
             (path (file-name-concat shared "handoff.md"))
             (mevedel-memory-dirs (list memory))
             claim accepted item pass scope)
     :before-each ((mevedel-workspace-identity-ensure root)
                   (make-directory shared t)
                   (make-directory memory t)
                   (write-region "" nil (file-name-concat memory "MEMORY.md") nil 'silent)
                   (write-region "Keep unrelated contributor guidance.\n" nil (file-name-concat root "AGENTS.md") nil 'silent))
     :after-each ((when claim (mevedel-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (cl-labels
      ((bytes () (mevedel-session-control-fs-read-file path 'no-conversion))
       (capture () (mevedel-memory-notes-capture workspace (mevedel-journal-store-entries root)))
       (accept (original &optional extraction-sections)
         (let ((coding-system-for-write 'utf-8-unix)) (write-region original nil path nil 'silent))
         (set-file-modes path #o640)
         (set-file-times path (seconds-to-time (- (float-time) (* 15 86400))))
         (setq scope (mevedel-memory-scope-capture workspace))
         (let ((notes (capture)))
           (should (= 1 (plist-get notes :notes)))
           (push (plist-get notes :root) (plist-get scope :roots)))
         (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
         (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
                (reply (concat (if extraction-sections (funcall extraction-sections scope)
                                 "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n")
                               "## Notes\n````proposal\nnote: \"work://shared/handoff.md\"\n---\n````\n## No action\n- none")))
           (setq accepted (mevedel-memory-store-accept workspace prepared reply nil "test:model" nil
                                                      '("work://shared/handoff.md"))
                 item (car (last (plist-get accepted :proposals)))
                 pass (plist-get prepared :id))
           (mevedel-memory-store-publish workspace pass)))
       (intent (decision)
         (plist-get (mevedel-memory-decision--published workspace decision) :intent)))
    ,test)
  (test)
  :doc "unresolved numeric handoff with no memory extraction is genuinely deleted and undo is exact"
  (let* ((original "TODO: verify the remote case.\n\nScenario: 584 reads, 67 retained, 2.491 ms.\nOwner: another agent.\n")
         notifications)
    (accept original)
    (should (eq 'note-delete (plist-get item :action)))
    (should-not (plist-get item :requires))
    (should (equal "" (plist-get item :body)))
    (should (= 0 (plist-get (capture) :notes)))
    (let* ((notify (lambda (files) (push files notifications)))
           (decision (mevedel-memory-decision-apply workspace pass (plist-get item :id) notify))
           (write (intent decision)))
      (should (eq 'applied (plist-get decision :status)))
      (should-not (file-exists-p path))
      (should (equal (list (list path)) notifications))
      (should (= 1 (length (plist-get write :changes))))
      (should (eq 'after (mevedel-memory-write-state write)))
      (should-not (file-exists-p (mevedel-memory-write--pin write)))
      (should-not (mevedel-memory-write-pending workspace (plist-get item :id)))
      (should-not (mevedel-memory-write-recovery-candidates workspace))
      (should (equal decision (mevedel-memory-decision-apply workspace pass (plist-get item :id) notify)))
      (should (equal (list (list path)) notifications))
      (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :status)))
      (should (equal (encode-coding-string original 'utf-8-unix) (bytes)))
      (should (= #o640 (file-modes path)))))
  :doc "untouched delete recovery reports observed unchanged targets without inventing an interruption"
  (let ((original "Original reviewed handoff.\n"))
    (accept original)
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (let (write)
      (mevedel-memory-write-call
       scope (plist-get item :root)
       (lambda (target)
         (setq write (mevedel-memory-write-prepare workspace claim target accepted item))))
      (mevedel-journal-claim-settle claim 'cancelled "")
      (let ((decision (mevedel-memory-decision-recover-write workspace (plist-get write :id) (plist-get write :hash))))
        (should (eq 'unavailable (plist-get decision :status)))
        (should (equal "Application left targets unchanged." (plist-get decision :reason)))
        (should (equal original (decode-coding-string (bytes) 'utf-8-unix)))
        (should (= #o640 (file-modes path)))
        (should (= 1 (plist-get (capture) :notes)))
        (should-not (file-exists-p (mevedel-memory-write--pin write))))))
  :doc "large escaped preimages remain readable and restore byte-exactly after actual deletion"
  (let* ((original (concat "TODO: unresolved observation.\nScenario: 584 results.\n"
                           (make-string (* 255 1024) #x00e9) "\n"))
         (encoded (encode-coding-string original 'utf-8-unix)))
    (accept original)
    (should (eq 'note-delete (plist-get item :action)))
    (should (equal accepted (mevedel-memory-store-accepted workspace pass)))
    (let* ((decision (mevedel-memory-decision-apply workspace pass (plist-get item :id)))
           (write (intent decision)))
      (should (eq 'applied (plist-get decision :status)))
      (should-not (file-exists-p path))
      (should (equal encoded (plist-get (cdr (assoc "handoff.md" (plist-get item :before))) :bytes)))
      (should (> (string-bytes (mevedel-memory-store--print write)) (* 1024 1024)))
      (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get item :id)) :status)))
      (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :status)))
      (should (equal encoded (bytes)))
      (should (= #o640 (file-modes path)))))
  :doc "every citing memory and instruction proposal becomes an extraction dependency"
  (progn
   (accept
   "TODO: two durable lessons and an instruction repair.\n"
   (lambda (captured)
     (let ((memory-id (car (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'memory))
                                    (plist-get captured :roots))))
           (instructions-id (car (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'instructions))
                                          (plist-get captured :roots)))))
       (concat
        "## Promote\n"
        (mapconcat
         (lambda (file)
           (format "````proposal\nroot: %S\nfile: %S\ntype: \"project\"\ntitle: \"Lesson\"\nhook: \"When useful\"\nreason: \"Verified lesson\"\nevidence: [\"work://shared/handoff.md\"]\n---\nSelf-contained durable lesson.\n````\n"
                   memory-id file)) '("first.md" "second.md") "")
        "## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n"
        (format "````proposal\nroot: %S\nfile: \"AGENTS.md\"\ntype: \"project\"\ntitle: \"Guidance\"\nhook: \"Contributors\"\nreason: \"Repair temporary source\"\nevidence: [\"work://shared/handoff.md\"]\n---\nKeep unrelated contributor guidance.\n````\n"
                instructions-id)))))
  (let ((extractions (butlast (plist-get accepted :proposals))))
    (should (equal '(promote promote instructions) (mapcar (lambda (proposal) (plist-get proposal :action)) extractions)))
    (should (equal (mapcar (lambda (proposal) (plist-get proposal :id)) extractions)
                   (plist-get item :requires)))
    (should (eq 'note-delete (plist-get item :action)))
    (should (equal "TODO: two durable lessons and an instruction repair.\n" (decode-coding-string (bytes) 'utf-8-unix))))))

(provide 'test-mevedel-memory-store-notes)
;;; test-mevedel-memory-store-notes.el ends here
