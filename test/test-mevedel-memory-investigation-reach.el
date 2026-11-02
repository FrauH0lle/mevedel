;;; test-mevedel-memory-investigation-reach.el -- Fair source search -*- lexical-binding: t -*-

;;; Commentary:

;; Wide roots must not hide all documentation from the review tools.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-memory-investigation)
(require 'mevedel-memory-scope)
(require 'mevedel-sandbox)

(defun mevedel-test-investigation-reach--documents (state root path contents)
  "Collect STATE's ROOT/PATH document iterator with CONTENTS."
  (let ((iterator (mevedel-memory-investigation--documents state root path contents))
        result ready)
    (unwind-protect
        (while (not ready)
          (let ((value (iter-next iterator)))
            (when (plist-member value :ready)
              (setq ready t result (plist-get value :ready)))))
      (iter-close iterator))
    result))

(mevedel-deftest mevedel-memory-investigation--documents ()
  ,test
  (test)
  :doc "root tools reach sibling and nested directories despite 253 root entries"
  (let* ((directory (make-temp-file "mevedel-investigation-reach-" t))
         (mevedel-memory-dirs '("memory"))
         (mevedel-sandbox-mode 'off)
         (workspace (mevedel-workspace--create :root directory))
         state)
    (unwind-protect
        (progn
          ;; Fit the root listing so filesystem order cannot omit a directory.
          ;; Pending siblings still exhaust the descendant visit allowance.
          (dotimes (n 249)
            (with-temp-file (file-name-concat directory (format "zroot%03d.el" n))
              (insert ";; Root only.\n")))
          (dolist (name '("docs" "src" "memory" ".mevedel"))
            (make-directory (file-name-concat directory name)))
          (dolist (name '("docs/completion.md" "src/completion.el"
                          "docs/nested/completion.md" "memory/secret.md"
                          ".mevedel/secret.md"))
            (let ((file (file-name-concat directory name)))
              (make-directory (file-name-directory file) t)
              (with-temp-file file (insert "Completion needle.\n"))))
          (setq state (mevedel-memory-investigation-create
                       (mevedel-memory-scope-capture workspace) nil
                       (lambda () t) (lambda (_) (ert-fail "Unexpected exhaustion"))))
          (dolist (operation '(glob grep))
            (let* ((tool (cl-find (capitalize (symbol-name operation))
                                  (mevedel-memory-investigation-tools state)
                                  :key #'gptel-tool-name :test #'equal))
                   result)
              ;; Invoke the actual request-local tool adapter and real rg helper.
              (funcall (gptel-tool-function tool) (lambda (text) (setq result text))
                       "workspace" "." (if (eq operation 'glob) "**/completion.*" "Completion needle"))
              (with-timeout (10 (ert-fail "Search did not settle"))
                (while (not result) (accept-process-output nil 0.01)))
              (should (string-prefix-p "[Partial source snapshot" result))
              (should (string-match-p "docs/completion.md" result))
              (should (string-match-p "docs/nested/completion.md" result))
              (should (string-match-p "src/completion.el" result))
              (should-not (string-match-p "secret\\|/tmp/\\|mevedel-memory-search-" result))))
          (let ((grep (cl-find "Grep" (mevedel-memory-investigation-tools state)
                               :key #'gptel-tool-name :test #'equal))
                result)
            (funcall (gptel-tool-function grep) (lambda (text) (setq result text))
                     "workspace" "." "Never present in any source")
            (with-timeout (10 (ert-fail "Missing-pattern search did not settle"))
              (while (not result) (accept-process-output nil 0.01)))
            (should (string-prefix-p "[Partial source snapshot" result))
            (should-not (string-match-p "docs/completion.md\\|src/completion.el" result)))
          ;; The visit bound includes directories and rejected private entries,
          ;; not just the number of documents returned.
          (let* ((scope (mevedel-memory-investigation-scope state))
                 (original (symbol-function 'mevedel-memory-scope-source-path))
                 (visits 0))
            (cl-letf (((symbol-function 'mevedel-memory-scope-source-path)
                       (lambda (&rest args)
                         (cl-incf visits)
                         (apply original args))))
              (let ((snapshot (mevedel-test-investigation-reach--documents state "workspace" "." nil)))
                (should (cdr snapshot))
                (should (<= (length (car snapshot)) 256))
                ;; One resolution initializes the queue; each visit revalidates.
                (should (= visits 257))
                (should-not (assoc "zroot248.el" (car snapshot)))))
            ;; Negative controls: a missing match remains absent; targeted
            ;; exclusion cannot become readable through the fair traversal.
            (should-error (mevedel-test-investigation-reach--documents state "workspace" "memory" t))
            (let ((snapshot (mevedel-test-investigation-reach--documents state "workspace" "docs" t)))
              (should-not (cdr snapshot))
              (should (= 2 (length (car snapshot))))
              (should-not (assoc "src/completion.el" (car snapshot))))))
      (when state (mevedel-memory-investigation-stop state))
      (mevedel-execution-teardown-all)
      (delete-directory directory t))))

(provide 'test-mevedel-memory-investigation-reach)
;;; test-mevedel-memory-investigation-reach.el ends here
