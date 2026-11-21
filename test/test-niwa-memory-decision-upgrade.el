;;; test-niwa-memory-decision-upgrade.el --- Decision format boundary -*- lexical-binding: t -*-

;;; Commentary:
;; An old accepted bundle lacking slots must not make a past rejection pending.

;;; Code:

(require 'niwa-memory-store)
(require 'memory-history-fixtures
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "memory-history-fixtures"))
(require 'niwa-memory-decision)
(require 'niwa-workspace-identity)

(niwa-deftest niwa-memory-decision-format-upgrade
    (:vars* ((root (make-temp-file "niwa-format-upgrade-" t))
             (workspace (niwa-workspace--create :root root))
             (identity (niwa-workspace-identity-ensure root))
             (niwa-memory-dirs nil)
             (niwa-journal-cleanup--inhibit-scheduling t))
     :after-each ((delete-directory root t)))
  (let* ((kind ,format)
         (printer (symbol-function 'niwa-memory-store--print))
         (accepted
          (cl-letf (((symbol-function 'niwa-memory-store--print)
                     (lambda (data)
                       ;; Emit the old prepared bytes *before* hashing and accepting.
                       (when (and (not (eq kind 'current)) (plist-get data :scope))
                         (setq data (copy-tree data))
                         (cl-remf (plist-get data :scope) :decision-format))
                       (funcall printer data))))
            (car (niwa-test-memory-history-create workspace 1 1 80 t))))
         (pass (plist-get (plist-get accepted :prepared) :id))
         (item (car (plist-get accepted :proposals)))
         (id (plist-get item :id))
         (path (plist-get (cdar (plist-get item :before)) :path)))
    (should (file-exists-p path))
    (if (eq kind 'current)
        (progn
          (should (member id (mapcar (lambda (row) (plist-get row :proposal))
                                     (niwa-memory-decision--pending-items workspace nil))))
          (should (eq 'applied (plist-get (niwa-memory-decision-apply workspace pass id) :status)))
          (should-not (file-exists-p path)))
      (when (eq kind 'rejected)
        (let ((decision (list :workspace identity :pass-id pass :proposal-id id
                              :decision-id (secure-hash 'sha256 "pre-upgrade decision owner")
                              :state-hash (secure-hash 'sha256 "old immutable acceptance")
                              :status 'rejected :reason "Keep the note" :created (niwa-journal-store-timestamp (current-time)))))
          (should (eq 'rejected (plist-get (niwa-journal-store-publish-decision root decision) :status)))))
      (should-not (niwa-memory-decision--pending-items workspace nil))
      (let ((rows (plist-get (niwa-memory-decision-inspect workspace) :rows)))
        (should (= 1 (length rows)))
        (should (eq 'unavailable (plist-get (car rows) :status)))
        (should (string-match-p "before the decision-format upgrade" (plist-get (car rows) :error))))
      (dolist (operation '(niwa-memory-decision-apply niwa-memory-decision-reject niwa-memory-decision-reverse))
        (let ((err (should-error (funcall operation workspace pass id) :type 'user-error)))
          (should (string-match-p "before the decision-format upgrade; the next memory review will propose it again" (error-message-string err)))))
      (should (file-exists-p path))
      (should-not (niwa-memory-decision--slots workspace id))))
  (format)
  :doc "never selects, applies or mutates an expressly rejected pre-slot proposal" 'rejected
  :doc "never treats an old pending proposal as a new slot-format proposal" 'pending
  :doc "selects and applies a newly accepted proposal" 'current)

(niwa-deftest niwa-memory-decision-old-expiry
    (:vars* ((root (make-temp-file "niwa-old-expiry-" t))
             (workspace (niwa-workspace--create :root root))
             (identity (niwa-workspace-identity-ensure root))
             (journal (niwa-journal-store-directory root))
             (state (niwa-journal-store-state-directory journal))
             (mutation (niwa-journal-claim-acquire (niwa-journal-store-claim-directory journal 'mutation) 180))
             (pass (secure-hash 'sha256 "old pass"))
             (decision (secure-hash 'sha256 "old decision"))
             (write (secure-hash 'sha256 "old write"))
             (hash (secure-hash 'sha256 "old private state"))
             (path (file-name-concat state "expiry" (concat (plist-get mutation :owner) ".json")))
             (stamp "2000-01-01T00-00-00Z")
             (filename (lambda (data kind)
                         (niwa-journal-store--filename (append (list :created "2000-01-01T00:00:00Z") data) kind)))
             (private (list (format "passes/%s/prepared.el" pass) (format "passes/%s/accepted.el" pass)
                            (format "decisions/%s.el" decision) (format "writes/%s.el" write)))
             claim)
     :after-each ((when claim (niwa-journal-claim-settle claim 'cancelled "")) (delete-directory root t)))
  (let* ((bad ,malformed)
         (files (if bad (cons "../foreign" (cdr private)) private))
         (entry (list :id pass :file (funcall filename (list :pass-id pass) 'consolidation)
                      :kind "consolidation" :scope "general" :journal-bearing :false :sha256 hash
                      :related (vector (list :id decision :file (funcall filename (list :decision-id decision) 'decision) :sha256 hash))
                      :private (vconcat (mapcar (lambda (file) (list :file file :sha256 hash)) files))))
         (text (json-serialize (append (niwa-journal-claim--record mutation) (list :entries (vector entry))))))
    (dolist (file private)
      (let ((actual (file-name-concat state file)))
        (make-directory (file-name-directory actual) t)
        (write-region "old private state" nil actual nil 'silent)))
    (make-directory (file-name-directory path) t)
    (write-region text nil path nil 'silent)
    (should (niwa-journal-claim-settle mutation 'completed (secure-hash 'sha256 text)))
    (setq claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
    (if bad
        (progn
          (should (string-match-p (regexp-quote path)
                                  (error-message-string (should-error (niwa-memory-store-prepare workspace claim
                                                                                                    (niwa-memory-scope-capture workspace) nil "")))))
          (should (cl-every (lambda (file) (file-exists-p (file-name-concat state file))) private)))
      (should (niwa-memory-store-prepare workspace claim (niwa-memory-scope-capture workspace) nil ""))
      (should (file-exists-p (concat path ".done")))
      (should-not (seq-some (lambda (file) (file-exists-p (file-name-concat state file))) private))
      (let ((sentinel (file-name-concat state (car (last private)))))
        (write-region "later occupant" nil sentinel nil 'silent)
        (niwa-journal-claim-settle claim 'cancelled "")
        (setq claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
        (should (niwa-memory-store-prepare workspace claim (niwa-memory-scope-capture workspace) nil ""))
        (should (equal "later occupant" (niwa-session-control-fs-read-file sentinel))))))
  (malformed)
  :doc "a prior accepted expiry finishes once; a finished old manifest permits the next review" nil
  :doc "a malformed old manifest refuses preparation, names its file and leaves every target in place" t)

(provide 'test-niwa-memory-decision-upgrade)
;;; test-niwa-memory-decision-upgrade.el ends here
