;;; test-niwa-memory-investigation-reach.el -- Fair source search -*- lexical-binding: t -*-

;;; Commentary:

;; Wide roots must not hide all documentation from the review tools.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-memory-investigation)
(require 'niwa-memory-scope)
(require 'niwa-sandbox)

(defun niwa-test-investigation-reach--documents (state root path contents)
  "Collect STATE's ROOT/PATH document iterator with CONTENTS."
  (let ((iterator (niwa-memory-investigation--documents state root path contents))
        result ready)
    (unwind-protect
        (while (not ready)
          (let ((value (iter-next iterator)))
            (when (plist-member value :ready)
              (setq ready t result (plist-get value :ready)))))
      (iter-close iterator))
    result))

(niwa-deftest niwa-memory-investigation--documents ()
  ,test
  (test)
  :doc "root tools reach sibling and nested directories despite 253 root entries"
  (let* ((directory (make-temp-file "niwa-investigation-reach-" t))
         (niwa-memory-dirs '("memory"))
         (niwa-sandbox-mode 'off)
         (workspace (niwa-workspace--create :root directory))
         state)
    (unwind-protect
        (progn
          ;; Fit the root listing so filesystem order cannot omit a directory.
          ;; Pending siblings still exhaust the descendant visit allowance.
          (dotimes (n 249)
            (with-temp-file (file-name-concat directory (format "zroot%03d.el" n))
              (insert ";; Root only.\n")))
          (dolist (name '("docs" "src" "memory" ".niwa"))
            (make-directory (file-name-concat directory name)))
          (dolist (name '("docs/completion.md" "src/completion.el"
                          "docs/nested/completion.md" "memory/secret.md"
                          ".niwa/secret.md"))
            (let ((file (file-name-concat directory name)))
              (make-directory (file-name-directory file) t)
              (with-temp-file file (insert "Completion needle.\n"))))
          (setq state (niwa-memory-investigation-create
                       (niwa-memory-scope-capture workspace) nil
                       (lambda () t) (lambda (_) (ert-fail "Unexpected exhaustion"))))
          (dolist (operation '(glob grep))
            (let* ((tool (cl-find (capitalize (symbol-name operation))
                                  (niwa-memory-investigation-tools state)
                                  :key #'gptel-tool-name :test #'equal))
                   result)
              ;; Invoke the actual request-local tool adapter and real rg helper.
              (funcall (gptel-tool-function tool) (lambda (text) (setq result text))
                       "workspace" "." (if (eq operation 'glob) "**/completion.*" "Completion needle"))
              ;; Match the 1 ms step timer instead of delaying every slice by 10 ms.
              (with-timeout (10 (ert-fail "Search did not settle"))
                (while (not result) (accept-process-output nil 0.001)))
              (should (string-prefix-p "[Partial source snapshot" result))
              (should (string-match-p "docs/completion.md" result))
              (should (string-match-p "docs/nested/completion.md" result))
              (should (string-match-p "src/completion.el" result))
              (should-not (string-match-p "secret\\|/tmp/\\|niwa-memory-search-" result))))
          (let ((grep (cl-find "Grep" (niwa-memory-investigation-tools state)
                               :key #'gptel-tool-name :test #'equal))
                result)
            (funcall (gptel-tool-function grep) (lambda (text) (setq result text))
                     "workspace" "." "Never present in any source")
            (with-timeout (10 (ert-fail "Missing-pattern search did not settle"))
              (while (not result) (accept-process-output nil 0.001)))
            (should (string-prefix-p "[Partial source snapshot" result))
            (should-not (string-match-p "docs/completion.md\\|src/completion.el" result)))
          ;; The visit bound includes directories and rejected private entries,
          ;; not just the number of documents returned.
          (let* ((scope (niwa-memory-investigation-scope state))
                 (original (symbol-function 'niwa-memory-scope-source-path))
                 (visits 0))
            (cl-letf (((symbol-function 'niwa-memory-scope-source-path)
                       (lambda (&rest args)
                         (cl-incf visits)
                         (apply original args))))
              (let ((snapshot (niwa-test-investigation-reach--documents state "workspace" "." nil)))
                (should (cdr snapshot))
                (should (<= (length (car snapshot)) 256))
                ;; One resolution initializes the queue; each visit revalidates.
                (should (= visits 257))
                (should-not (assoc "zroot248.el" (car snapshot)))))
            ;; Negative controls: a missing match remains absent; targeted
            ;; exclusion cannot become readable through the fair traversal.
            (should-error (niwa-test-investigation-reach--documents state "workspace" "memory" t))
            (let ((snapshot (niwa-test-investigation-reach--documents state "workspace" "docs" t)))
              (should-not (cdr snapshot))
              (should (= 2 (length (car snapshot))))
              (should-not (assoc "src/completion.el" (car snapshot))))))
      (when state (niwa-memory-investigation-stop state))
      (niwa-execution-teardown-all)
      (delete-directory directory t))))

(provide 'test-niwa-memory-investigation-reach)
;;; test-niwa-memory-investigation-reach.el ends here
