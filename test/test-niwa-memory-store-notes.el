;;; test-niwa-memory-store-notes.el -- Checked delete acceptance -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise host-generated disposals, extraction dependencies and exact
;; preimages through the ordinary checked decision/write lifecycle.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-journal-claim)
(require 'niwa-memory-decision)
(require 'niwa-memory-notes)
(require 'niwa-memory-scope)
(require 'niwa-memory-store)
(require 'niwa-memory-write)
(require 'niwa-session-control-fs)
(require 'niwa-workspace-identity)

(niwa-deftest niwa-memory-store-accept/notes
    (:vars* ((root (file-name-as-directory (file-truename (make-temp-file "niwa-delete-store-" t))))
             (workspace (niwa-workspace--create :root root))
             (shared (file-name-concat root ".niwa" "shared"))
             (memory (file-name-concat root ".agents" "memory"))
             (path (file-name-concat shared "handoff.md"))
             (niwa-memory-dirs (list memory))
             claim accepted item pass scope)
     :before-each ((niwa-workspace-identity-ensure root)
                   (make-directory shared t)
                   (make-directory memory t)
                   (write-region "" nil (file-name-concat memory "MEMORY.md") nil 'silent)
                   (write-region "Keep unrelated contributor guidance.\n" nil (file-name-concat root "AGENTS.md") nil 'silent))
     :after-each ((when claim (niwa-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (cl-labels
      ((bytes () (niwa-session-control-fs-read-file path 'no-conversion))
       (capture () (niwa-memory-notes-capture workspace (niwa-journal-store-entries root)))
       (accept (original &optional extraction-sections)
         (let ((coding-system-for-write 'utf-8-unix)) (write-region original nil path nil 'silent))
         (set-file-modes path #o640)
         (set-file-times path (seconds-to-time (- (float-time) (* 15 86400))))
         (setq scope (niwa-memory-scope-capture workspace))
         (let ((notes (capture)))
           (should (= 1 (plist-get notes :notes)))
           (push (plist-get notes :root) (plist-get scope :roots)))
         (setq claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
         (let* ((prepared (niwa-memory-store-prepare workspace claim scope nil ""))
                (reply (concat (if extraction-sections (funcall extraction-sections scope)
                                 "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n")
                               "## No action\n- none")))
           (setq accepted (niwa-memory-store-accept workspace prepared reply nil "test:model" nil
                                                      '("work://shared/handoff.md"))
                 item (car (last (plist-get accepted :proposals)))
                 pass (plist-get prepared :id))
           (niwa-memory-store-publish workspace pass)))
       (slot (decision) (niwa-memory-decision--published workspace decision)))
    ,test)
  (test)
  :doc "unresolved numeric handoff with no memory extraction is permanently deleted"
  (let* ((original "TODO: verify the remote case.\n\nScenario: 584 reads, 67 retained, 2.491 ms.\nOwner: another agent.\n")
         notifications)
    (accept original)
    (should (eq 'note-delete (plist-get item :action)))
    (should-not (plist-get item :requires))
    (should (equal "" (plist-get item :body)))
    (should (= 0 (plist-get (capture) :notes)))
    (let* ((notify (lambda (files) (push files notifications)))
           (decision (plist-get (niwa-memory-decision-apply workspace pass (plist-get item :id) :on-applied notify) :decision))
           (record (slot decision)))
      (should (eq 'applied (plist-get decision :status)))
      (should-not (file-exists-p path))
      (should (equal (list (list path)) notifications))
      (should (equal (plist-get item :before) (plist-get record :before)))
      (should (equal (list (cons "handoff.md" (list :path path :exists nil))) (plist-get record :after)))
      (should (equal decision (plist-get (niwa-memory-decision-apply workspace pass (plist-get item :id) :on-applied notify) :decision)))
      (should (equal (list (list path)) notifications))
      (should-error (plist-get (niwa-memory-decision-reverse workspace pass (plist-get item :id)) :decision) :type 'user-error)
      (should-not (file-exists-p path))))
  :doc "a delete failing before mutation records an unavailable attempt and leaves the reviewed file intact"
  (let ((original "Original reviewed handoff.\n"))
    (accept original)
    (cl-letf (((symbol-function 'niwa-memory-notes-commit) (lambda (&rest _) (error "Delete failed"))))
      (let ((decision (niwa-memory-decision-apply workspace pass (plist-get item :id))))
        (should (eq 'unavailable (plist-get decision :status)))
        (should (string-search "Delete failed" (plist-get decision :reason)))))
    (should (equal original (decode-coding-string (bytes) 'utf-8-unix)))
    (should (= #o640 (file-modes path)))
    (should (= 1 (plist-get (capture) :notes))))
  :doc "large escaped review evidence remains readable without creating a restoration path"
  (let* ((original (concat "TODO: unresolved observation.\nScenario: 584 results.\n"
                           (make-string (* 255 1024) #x00e9) "\n"))
         (encoded (encode-coding-string original 'utf-8-unix)))
    (accept original)
    (should (eq 'note-delete (plist-get item :action)))
    (should (equal accepted (niwa-memory-store-accepted workspace pass)))
    (let* ((decision (plist-get (niwa-memory-decision-apply workspace pass (plist-get item :id)) :decision))
           (record (slot decision)))
      (should (eq 'applied (plist-get decision :status)))
      (should-not (file-exists-p path))
      (should (equal encoded (plist-get (cdr (assoc "handoff.md" (plist-get item :before))) :bytes)))
      (should (> (string-bytes (niwa-memory-store--print (plist-get record :before))) (* 1024 1024)))
      (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get item :id)) :status)))
      (should-error (plist-get (niwa-memory-decision-reverse workspace pass (plist-get item :id)) :decision) :type 'user-error)
      (should-not (file-exists-p path))))
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

(provide 'test-niwa-memory-store-notes)
;;; test-niwa-memory-store-notes.el ends here
