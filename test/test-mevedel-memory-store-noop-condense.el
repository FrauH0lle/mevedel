;;; test-mevedel-memory-store-noop-condense.el -- Unchanged note condensation -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise parsed condense replies through immutable acceptance, checked
;; application, publication and fresh note selection, without provider calls.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-store)
(require 'mevedel-session-control-fs)
(require 'mevedel-workspace-identity)

(mevedel-deftest mevedel-memory-store-accept/noop-condense
    (:vars* ((root (file-name-as-directory (file-truename (make-temp-file "mevedel-noop-condense-" t))))
             (workspace (mevedel-workspace--create :root root))
             (shared (file-name-concat root ".mevedel" "shared"))
             (path (file-name-concat shared "handoff.md"))
             (mevedel-memory-dirs nil)
             (mevedel-memory-notes-review-days 7)
             (now 1800000000)
             claim accepted item pass scope)
     :before-each ((mevedel-workspace-identity-ensure root) (make-directory shared t))
     :after-each ((when claim (mevedel-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (cl-letf (((symbol-function 'mevedel-session-control-fs-target-time) (lambda (_) now)))
    (cl-labels
        ((bytes () (mevedel-session-control-fs-read-file path 'no-conversion))
         (capture () (mevedel-memory-notes-capture workspace (mevedel-journal-store-entries root)))
         (accept (original body)
           (let ((coding-system-for-write 'utf-8-unix))
             (write-region original nil path nil 'silent))
           (set-file-modes path #o640)
           (set-file-times path (seconds-to-time (- now (* 48 3600))))
           (setq scope (mevedel-memory-scope-capture workspace))
           (setf (plist-get scope :roots) (list (plist-get (capture) :root)))
           (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
           (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
                  (reply (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n"
                                 "## Remove\n- none\n## Instructions\n- none\n## Notes\n"
                                 "````proposal\nnote: \"work://shared/handoff.md\"\ndisposition: \"condense\"\n"
                                 "reason: \"Retain the unresolved check\"\nopen-items: [\"Verify the remote case\"]\n"
                                 "---\n" body "\n````\n## No action\n- none")))
             (should (eq 'note-condense
                         (plist-get (car (plist-get (mevedel-memory-proposal-parse
                                                    reply (plist-get scope :roots) nil
                                                    (mevedel-memory-notes-index scope))
                                                   :proposals)) :action)))
             (setq accepted (mevedel-memory-store-accept
                             workspace prepared reply nil "test:model" nil '("work://shared/handoff.md"))
                   item (car (plist-get accepted :proposals))
                   pass (plist-get prepared :id))
             (mevedel-memory-store-publish workspace pass)))
         (intent (decision)
           (plist-get (mevedel-memory-decision--published workspace decision) :intent)))
      ,test))
  (test)
  :doc "canonical newline equality becomes an applied unchanged keep with ordinary version suppression"
  (let* ((body "Open: verify the remote case with caf\u00e9 input.")
         (original (encode-coding-string (concat body "\n") 'utf-8-unix))
         notifications)
    (accept (concat body "\n") body)
    (should (= 0 (plist-get (capture) :notes)))
    (let* ((modified (file-attribute-modification-time (file-attributes path)))
           (notify (lambda (files) (push files notifications)))
           (decision (mevedel-memory-decision-apply workspace pass (plist-get item :id) notify))
           (write (intent decision)))
      ;; Assert the consumer-visible failure first: the old path returned unavailable.
      (should (eq 'applied (plist-get decision :status)))
      (should (eq 'note-keep (plist-get item :action)))
      (should (plist-get item :guard-converted))
      (should (string-match-p "byte-identical" (plist-get item :reason)))
      (should-not (plist-get item :result))
      (should-not (plist-get write :changes))
      (should (equal (plist-get write :before) (plist-get write :after)))
      (should (equal original (bytes)))
      (should (= #o640 (file-modes path)))
      (should (equal modified (file-attribute-modification-time (file-attributes path))))
      (should (equal '(nil) notifications))
      (should (equal decision (mevedel-memory-decision-apply workspace pass (plist-get item :id) notify)))
      (should (equal '(nil) notifications))
      (should-not (file-exists-p (mevedel-memory-write--pin write)))
      (should-not (mevedel-memory-write-pending workspace (plist-get item :id)))
      (should-not (mevedel-memory-write-recovery-candidates workspace))
      (should (= 0 (plist-get (capture) :notes)))
      (cl-incf now (* 8 86400))
      (should (= 1 (plist-get (capture) :notes)))
      (cl-decf now (* 8 86400))
      ;; A changed version bypasses retention once it is quiet.
      (write-region "Open: verify a different remote case.\n" nil path nil 'silent)
      (set-file-times path (seconds-to-time (- now (* 48 3600))))
      (should (= 1 (plist-get (capture) :notes)))))
  :doc "a genuinely shorter condensation still writes exact bytes and retains checked undo"
  (let ((original "Finished background.\n\nOpen: verify the remote case.\n") notifications)
    (accept original "Open: verify the remote case.")
    (should (eq 'note-condense (plist-get item :action)))
    (should-not (plist-get item :guard-converted))
    (let* ((decision (mevedel-memory-decision-apply
                      workspace pass (plist-get item :id) (lambda (files) (push files notifications))))
           (write (intent decision)))
      (should (eq 'applied (plist-get decision :status)))
      (should (equal "Open: verify the remote case.\n" (bytes)))
      (should (equal (secure-hash 'sha256 (bytes)) (plist-get item :result)))
      (should (= #o640 (file-modes path)))
      (should (equal (list (list path)) notifications))
      (should (= 1 (length (plist-get write :changes))))
      (should (eq 'after (mevedel-memory-write-state write)))
      (should-not (file-exists-p (mevedel-memory-write--pin write)))
      (set-file-times path (seconds-to-time (- now (* 48 3600))))
      (should (= 0 (plist-get (capture) :notes)))
      (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :status)))
      (should (equal original (bytes)))))
  :doc "a missing original newline is a real byte change, not an unchanged keep"
  (progn
    (accept "Open: verify the remote case." "Open: verify the remote case.")
    (should (eq 'note-condense (plist-get item :action)))
    (let* ((decision (mevedel-memory-decision-apply workspace pass (plist-get item :id)))
           (write (intent decision)))
      (should (eq 'applied (plist-get decision :status)))
      (should (equal "Open: verify the remote case.\n" (bytes)))
      (should (= 1 (length (plist-get write :changes))))
      (should (eq 'after (mevedel-memory-write-state write)))))
  :doc "untouched write recovery reports observed unchanged targets without inventing an interruption"
  (progn
    (accept "Finished background.\n\nOpen: verify the remote case.\n" "Open: verify the remote case.")
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
        (should (= 1 (plist-get (capture) :notes)))
        (should-not (file-exists-p (mevedel-memory-write--pin write)))))))

(provide 'test-mevedel-memory-store-noop-condense)
;;; test-mevedel-memory-store-noop-condense.el ends here
