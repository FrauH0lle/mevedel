;;; test-niwa-memory-observation.el --- Batched evidence observations -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise bounded batch failures and authentication reuse over real history.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "helpers"))
(require 'niwa-memory-decision)
(require 'niwa-memory-list)
(require 'niwa-memory-store)
(require 'niwa-memory-write)
(require 'niwa-session-control-fs)
(require 'memory-history-fixtures
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "memory-history-fixtures"))

(niwa-deftest niwa-memory-decision-inspect/slot-batches
    (:vars* ((root (niwa-test-memory-history-root 'inspect/slot-batches))
             (workspace (niwa-workspace--create :root root))
             (niwa-memory-dirs nil)
             (niwa-journal-cleanup--inhibit-scheduling t)
             (_identity (niwa-workspace-identity-ensure root))
             (bundle (car (niwa-test-memory-history-build
                           'inspect/slot-batches root (lambda () (niwa-test-memory-history-create workspace 1 10 100)))))
             (ids (mapcar (lambda (p) (plist-get p :id)) (plist-get bundle :proposals)))
             (paths (mapcar (lambda (id) (file-name-concat (niwa-memory-decision--directory workspace) (concat id ".el"))) ids)))
            :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "corrupt, missing and oversized slots preserve unrelated decisions and each affected identity"
  (progn
    (write-region "(broken" nil (nth 0 paths) nil 'silent)
    (delete-file (nth 7 paths))
    (write-region (make-string (+ 2 niwa-memory-store--max-bytes) ?x) nil (nth 8 paths) nil 'silent)
    (let ((rows (plist-get (niwa-memory-decision-inspect workspace) :rows)))
      (should (= 10 (length rows)))
      (cl-loop for id in ids for pos from 0
               for row = (seq-find (lambda (row) (equal id (plist-get row :id))) rows)
               do (should row)
               (if (memq pos '(0 7 8))
                   (progn (should (eq 'unavailable (plist-get row :status))) (should (plist-get row :error)))
                 (should (eq 'applied (plist-get row :status)))))))
  :doc "an incomplete carrier reply cannot authorize a partial observation"
  (let ((run (symbol-function 'niwa-session-control-fs-run-program)))
    (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
               (lambda (&rest args) (butlast (apply run args)))))
      (should (equal "Incomplete decision slot observation"
                     (error-message-string (should-error (niwa-memory-decision--record-batch workspace (seq-take paths 8)))))))))

(provide 'test-niwa-memory-observation)
;;; test-niwa-memory-observation.el ends here
