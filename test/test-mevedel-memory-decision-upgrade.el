;;; test-mevedel-memory-decision-upgrade.el --- Decision format boundary -*- lexical-binding: t -*-

;;; Commentary:
;; An old accepted bundle lacking slots must not make a past rejection pending.

;;; Code:

(require 'mevedel-memory-store)
(require 'memory-history-fixtures
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "memory-history-fixtures"))
(require 'mevedel-memory-decision)
(require 'mevedel-workspace-identity)

(mevedel-deftest mevedel-memory-decision-format-upgrade
    (:vars* ((root (make-temp-file "mevedel-format-upgrade-" t))
             (workspace (mevedel-workspace--create :root root))
             (identity (mevedel-workspace-identity-ensure root))
             (mevedel-memory-dirs nil)
             (mevedel-journal-cleanup--inhibit-scheduling t))
     :after-each ((delete-directory root t)))
  (let* ((kind ,format)
         (printer (symbol-function 'mevedel-memory-store--print))
         (accepted
          (cl-letf (((symbol-function 'mevedel-memory-store--print)
                     (lambda (data)
                       ;; Emit the old prepared bytes *before* hashing and accepting.
                       (when (and (not (eq kind 'current)) (plist-get data :scope))
                         (setq data (copy-tree data))
                         (cl-remf (plist-get data :scope) :decision-format))
                       (funcall printer data))))
            (car (mevedel-test-memory-history-create workspace 1 1 80 t))))
         (pass (plist-get (plist-get accepted :prepared) :id))
         (item (car (plist-get accepted :proposals)))
         (id (plist-get item :id))
         (path (plist-get (cdar (plist-get item :before)) :path)))
    (should (file-exists-p path))
    (if (eq kind 'current)
        (progn
          (should (member id (mapcar (lambda (row) (plist-get row :proposal))
                                     (mevedel-memory-decision--pending-items workspace nil))))
          (should (eq 'applied (plist-get (mevedel-memory-decision-apply workspace pass id) :status)))
          (should-not (file-exists-p path)))
      (when (eq kind 'rejected)
        (let ((decision (list :workspace identity :pass-id pass :proposal-id id
                              :decision-id (secure-hash 'sha256 "pre-upgrade decision owner")
                              :state-hash (secure-hash 'sha256 "old immutable acceptance")
                              :status 'rejected :reason "Keep the note" :created (mevedel-journal-store-timestamp (current-time)))))
          (should (eq 'rejected (plist-get (mevedel-journal-store-publish-decision root decision) :status)))))
      (should-not (mevedel-memory-decision--pending-items workspace nil))
      (let ((rows (plist-get (mevedel-memory-decision-inspect workspace) :rows)))
        (should (= 1 (length rows)))
        (should (eq 'unavailable (plist-get (car rows) :status)))
        (should (string-match-p "before the decision-format upgrade" (plist-get (car rows) :error))))
      (dolist (operation '(mevedel-memory-decision-apply mevedel-memory-decision-reject mevedel-memory-decision-reverse))
        (let ((err (should-error (funcall operation workspace pass id) :type 'user-error)))
          (should (string-match-p "before the decision-format upgrade; the next memory review will propose it again" (error-message-string err)))))
      (should (file-exists-p path))
      (should-not (mevedel-memory-decision--slots workspace id))))
  (format)
  :doc "never selects, applies or mutates an expressly rejected pre-slot proposal" 'rejected
  :doc "never treats an old pending proposal as a new slot-format proposal" 'pending
  :doc "selects and applies a newly accepted proposal" 'current)

(mevedel-deftest mevedel-memory-decision-old-expiry
    (:vars* ((root (make-temp-file "mevedel-old-expiry-" t))
             (workspace (mevedel-workspace--create :root root))
             (identity (mevedel-workspace-identity-ensure root))
             (journal (mevedel-journal-store-directory root))
             (state (mevedel-journal-store-state-directory journal))
             (mutation (mevedel-journal-claim-acquire (mevedel-journal-store-claim-directory journal 'mutation) 180))
             (pass (secure-hash 'sha256 "old pass"))
             (decision (secure-hash 'sha256 "old decision"))
             (write (secure-hash 'sha256 "old write"))
             (hash (secure-hash 'sha256 "old private state"))
             (path (file-name-concat state "expiry" (concat (plist-get mutation :owner) ".json")))
             (stamp "2000-01-01T00-00-00Z")
             (filename (lambda (data kind)
                         (mevedel-journal-store--filename (append (list :created "2000-01-01T00:00:00Z") data) kind)))
             (private (list (format "passes/%s/prepared.el" pass) (format "passes/%s/accepted.el" pass)
                            (format "decisions/%s.el" decision) (format "writes/%s.el" write)))
             claim)
     :after-each ((when claim (mevedel-journal-claim-settle claim 'cancelled "")) (delete-directory root t)))
  (let* ((bad ,malformed)
         (files (if bad (cons "../foreign" (cdr private)) private))
         (entry (list :id pass :file (funcall filename (list :pass-id pass) 'consolidation)
                      :kind "consolidation" :scope "general" :journal-bearing :false :sha256 hash
                      :related (vector (list :id decision :file (funcall filename (list :decision-id decision) 'decision) :sha256 hash))
                      :private (vconcat (mapcar (lambda (file) (list :file file :sha256 hash)) files))))
         (text (json-serialize (append (mevedel-journal-claim--record mutation) (list :entries (vector entry))))))
    (dolist (file private)
      (let ((actual (file-name-concat state file)))
        (make-directory (file-name-directory actual) t)
        (write-region "old private state" nil actual nil 'silent)))
    (make-directory (file-name-directory path) t)
    (write-region text nil path nil 'silent)
    (should (mevedel-journal-claim-settle mutation 'completed (secure-hash 'sha256 text)))
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (if bad
        (progn
          (should (string-match-p (regexp-quote path)
                                  (error-message-string (should-error (mevedel-memory-store-prepare workspace claim
                                                                                                    (mevedel-memory-scope-capture workspace) nil "")))))
          (should (cl-every (lambda (file) (file-exists-p (file-name-concat state file))) private)))
      (should (mevedel-memory-store-prepare workspace claim (mevedel-memory-scope-capture workspace) nil ""))
      (should (file-exists-p (concat path ".done")))
      (should-not (seq-some (lambda (file) (file-exists-p (file-name-concat state file))) private))
      (let ((sentinel (file-name-concat state (car (last private)))))
        (write-region "later occupant" nil sentinel nil 'silent)
        (mevedel-journal-claim-settle claim 'cancelled "")
        (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
        (should (mevedel-memory-store-prepare workspace claim (mevedel-memory-scope-capture workspace) nil ""))
        (should (equal "later occupant" (mevedel-session-control-fs-read-file sentinel))))))
  (malformed)
  :doc "a prior accepted expiry finishes once; a finished old manifest permits the next review" nil
  :doc "a malformed old manifest refuses preparation, names its file and leaves every target in place" t)

(provide 'test-mevedel-memory-decision-upgrade)
;;; test-mevedel-memory-decision-upgrade.el ends here
