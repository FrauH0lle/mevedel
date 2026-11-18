;;; test-mevedel-memory-observation.el --- Batched evidence observations -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise bounded batch failures and authentication reuse over real history.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "helpers"))
(require 'mevedel-memory-decision)
(require 'mevedel-memory-list)
(require 'mevedel-memory-store)
(require 'mevedel-memory-write)
(require 'mevedel-session-control-fs)
(require 'memory-history-fixtures
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "memory-history-fixtures"))

(mevedel-deftest mevedel-memory-decision-inspect/slot-batches
    (:vars* ((root (mevedel-test-memory-history-root 'inspect/slot-batches))
             (workspace (mevedel-workspace--create :root root))
             (mevedel-memory-dirs nil)
             (mevedel-journal-cleanup--inhibit-scheduling t)
             (_identity (mevedel-workspace-identity-ensure root))
             (bundle (car (mevedel-test-memory-history-build
                           'inspect/slot-batches root (lambda () (mevedel-test-memory-history-create workspace 1 10 100)))))
             (ids (mapcar (lambda (p) (plist-get p :id)) (plist-get bundle :proposals)))
             (paths (mapcar (lambda (id) (file-name-concat (mevedel-memory-decision--directory workspace) (concat id ".el"))) ids)))
            :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "corrupt, missing and oversized slots preserve unrelated decisions and each affected identity"
  (progn
    (write-region "(broken" nil (nth 0 paths) nil 'silent)
    (delete-file (nth 7 paths))
    (write-region (make-string (+ 2 mevedel-memory-store--max-bytes) ?x) nil (nth 8 paths) nil 'silent)
    (let ((rows (plist-get (mevedel-memory-decision-inspect workspace) :rows)))
      (should (= 10 (length rows)))
      (cl-loop for id in ids for pos from 0
               for row = (seq-find (lambda (row) (equal id (plist-get row :id))) rows)
               do (should row)
               (if (memq pos '(0 7 8))
                   (progn (should (eq 'unavailable (plist-get row :status))) (should (plist-get row :error)))
                 (should (eq 'applied (plist-get row :status)))))))
  :doc "an incomplete carrier reply cannot authorize a partial observation"
  (let ((run (symbol-function 'mevedel-session-control-fs-run-program)))
    (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
               (lambda (&rest args) (butlast (apply run args)))))
      (should (equal "Incomplete decision slot observation"
                     (error-message-string (should-error (mevedel-memory-decision--record-batch workspace (seq-take paths 8)))))))))

(provide 'test-mevedel-memory-observation)
;;; test-mevedel-memory-observation.el ends here
