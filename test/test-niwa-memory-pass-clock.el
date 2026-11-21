;;; test-niwa-memory-pass-clock.el --- Independent journal review clock -*- lexical-binding: t -*-

;;; Commentary:

;; Real accepted publications and expiry preserve only journal-bearing clocks.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-journal-cleanup)
(require 'niwa-journal-store)
(require 'niwa-memory-decision)
(require 'niwa-memory-notes)
(require 'niwa-memory-pass)
(require 'niwa-memory-scope)
(require 'niwa-memory-store)
(require 'niwa-session-control-fs)
(require 'niwa-workspace-identity)

(niwa-deftest niwa-memory-pass--general-times
    (:vars* ((root (make-temp-file "niwa-journal-clock-" t))
             (workspace (niwa-workspace--create :root root))
             (identity (niwa-workspace-identity-ensure root))
             (niwa-memory-dirs nil)
             (niwa-journal-max-age-days 0)
             (niwa-memory-history-max-age-days 0)
             (date "2000-01-01T00:00:00Z")
             (timestamp (float-time (date-to-time date)))
             (none (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n"
                           "## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes.")))
     :after-each ((delete-directory root t)))
  (cl-labels
      ((digest ()
         (niwa-journal-store-publish-digest
          root (list :capture-id (make-string 64 ?a) :session "closed" :session-name "Closed"
                     :workspace identity :trigger 'session-end :segment 1
                     :source-revision (make-string 64 ?b) :turns '(1)
                     :turn-ids (list (make-string 64 ?c)) :created date :model "test:model")
          "## Done\n- Observed: Tests passed.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none"))
       (note ()
         (let ((path (file-name-concat root ".niwa" "shared" "finding.md")))
           (make-directory (file-name-directory path) t)
           (write-region "An uncertain finding, not durable guidance.\n" nil path nil 'silent)
           (set-file-times path (date-to-time date))))
       (review (&optional entries focus)
         (let* ((scope (niwa-memory-scope-capture workspace))
                (capture (niwa-memory-notes-capture workspace entries))
                (claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180)))
           (should claim)
           (unwind-protect
               (progn
                 (when-let* ((row (plist-get capture :root)))
                   (plist-put scope :roots (append (plist-get scope :roots) (list row))))
                 (cl-letf (((symbol-function 'niwa-journal-store-timestamp) (lambda (_) date)))
                   (let* ((prepared (niwa-memory-store-prepare workspace claim scope entries (or focus "")))
                          (notes (mapcar (lambda (document) (plist-get document :id))
                                         (niwa-memory-notes-documents scope))))
                     (niwa-memory-store-accept workspace prepared none entries "test:model" nil notes)
                     (niwa-memory-store-publish workspace (plist-get prepared :id)))))
             (niwa-journal-claim-settle claim 'cancelled ""))))
       (times ()
         (niwa-memory-pass--general-times workspace (niwa-journal-store-entries root)))
       (retire (entry)
         ;; A covered digest expires before its dependent general review.
         ;; Stop once that review is retired, rather than creating and
         ;; collecting another batch of otherwise unrelated cleanup claims.
         (should-not (niwa-journal-cleanup-pass-retired-p root (plist-get entry :pass-id)))
         (cl-loop repeat 3
                  until (niwa-journal-cleanup-pass-retired-p root (plist-get entry :pass-id))
                  do (niwa-journal-cleanup-expired workspace t))
         (should (niwa-journal-cleanup-pass-retired-p root (plist-get entry :pass-id)))
         (json-parse-string
          (niwa-session-control-fs-read-file
           (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory root))
                             "retired-passes" (plist-get entry :pass-id))))))
    ,test)
  (test)
  :doc "a real admitted note without journal evidence does not advance the journal clock"
  (progn
    (note)
    (let* ((entry (review))
           (accepted (niwa-memory-store-accepted workspace (plist-get entry :pass-id))))
      (should (= 1 (length (plist-get accepted :admitted-notes))))
      (should-not (plist-get entry :digests))
      (should-not (times))))
  :doc "journal evidence advances only a general review, not a focused one"
  (let ((entry (digest)))
    (review (list entry) "One source condition")
    (should-not (times))
    (review (list entry))
    (should (equal (list timestamp) (times))))
  :doc "retiring a note-only review retains a false journal marker and no clock"
  (progn
    (note)
    (let* ((entry (review))
           (pass (plist-get entry :pass-id))
           (accepted (niwa-memory-store-accepted workspace pass))
           (proposal (car (plist-get accepted :proposals))))
      (should (= 1 (length (plist-get accepted :proposals))))
      (should (eq 'note-delete (plist-get proposal :action)))
      ;; Pending disposal independently prevents expiry even without a cycle.
      (should (= 0 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
      (should-not (niwa-journal-cleanup-pass-retired-p root pass))
      (cl-letf (((symbol-function 'niwa-journal-store-timestamp) (lambda (_) date)))
        (should (eq 'rejected (plist-get (niwa-memory-decision-reject
                                         workspace pass (plist-get proposal :id) :reason "Retain the fixture note")
                                        :status))))
      (let ((record (retire entry)))
        (should (eq :false (gethash "journal-bearing" record)))
        (should (equal "general" (gethash "scope" record)))
        (should-not (times)))
      (niwa-journal-cleanup-expired workspace t)
      (should (equal "An uncertain finding, not durable guidance.\n"
                     (niwa-session-control-fs-read-file
                      (file-name-concat root ".niwa" "shared" "finding.md"))))))
  :doc "retiring a journal-bearing general review preserves its independent clock"
  (let* ((entry (review (list (digest))))
         (record (retire entry)))
    (should (eq t (gethash "journal-bearing" record)))
    (should (equal (list timestamp) (times)))
    (should-not (file-exists-p
                 (file-name-concat (niwa-memory-store--directory workspace (plist-get entry :pass-id))
                                   "accepted.el"))))
  :doc "retiring a focused journal review never starts the general clock"
  (let ((record (retire (review (list (digest)) "Specific claim"))))
    (should (eq t (gethash "journal-bearing" record)))
    (should (equal "focused" (gethash "scope" record)))
    (should-not (times)))
  :doc "missing, null, numeric or invented retirement classification fails closed"
  (dolist (classification '(missing :null 0 "yes"))
    (let* ((directory (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory root))
                                      "retired-passes"))
           (path (file-name-concat directory (make-string 64 ?f)))
           (record (list :sha256 (make-string 64 ?e) :scope "general" :created date)))
      (unless (eq classification 'missing)
        (setq record (append record (list :journal-bearing classification))))
      (make-directory directory t)
      (write-region (json-serialize record) nil path nil 'silent)
      (should-error (times) :type 'error))))

(provide 'test-niwa-memory-pass-clock)
;;; test-niwa-memory-pass-clock.el ends here
