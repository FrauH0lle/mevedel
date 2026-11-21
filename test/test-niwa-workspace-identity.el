;;; test-niwa-workspace-identity.el --- Workspace identity tests -*- lexical-binding: t -*-

;;; Commentary:

;; Covers project-owned workspace identity creation and lookup.

;;; Code:

(require 'niwa-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Workspace identity

(niwa-deftest niwa-workspace-identity-read
  (:vars* ((niwa-workspace-identity--cache (make-hash-table :test #'equal))
           (root (file-name-as-directory
                  (make-temp-file "niwa-workspace-identity-" t))))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "returns nil before the workspace identity exists"
  (should-not (niwa-workspace-identity-read root))

  :doc "reads the project-owned identity"
  (let ((path (file-name-concat root ".niwa" "workspace-id"))
        (identity (make-string 64 ?a)))
    (make-directory (file-name-directory path) t)
    (write-region (concat identity "\n") nil path nil 'silent)
    (should (equal identity (niwa-workspace-identity-read root))))

  :doc "rejects malformed identity files"
  (let ((path (file-name-concat root ".niwa" "workspace-id")))
    (make-directory (file-name-directory path) t)
    (write-region "not-an-identity\n" nil path nil 'silent)
    (should-error (niwa-workspace-identity-read root)
                  :type 'error))

  :doc "caches a read identity and never caches absence"
  (let ((path (file-name-concat root ".niwa" "workspace-id"))
        (identity (make-string 64 ?b)))
    ;; Absence is answered live, so a later creator is observed.
    (should-not (niwa-workspace-identity-read root))
    (make-directory (file-name-directory path) t)
    (write-region (concat identity "\n") nil path nil 'silent)
    (should (equal identity (niwa-workspace-identity-read root)))
    ;; One successful read is authoritative for the process.
    (delete-file path)
    (should (equal identity (niwa-workspace-identity-read root)))
    (clrhash niwa-workspace-identity--cache)
    (should-not (niwa-workspace-identity-read root))))

(niwa-deftest niwa-workspace-identity-ensure
  (:vars* ((niwa-workspace-identity--cache (make-hash-table :test #'equal))
           (root (file-name-as-directory
                  (make-temp-file "niwa-workspace-identity-" t))))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "creates one bounded opaque identity and reuses it"
  (let* ((identity (niwa-workspace-identity-ensure root))
         (path (file-name-concat root ".niwa" "workspace-id")))
    (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" identity))
    (should (equal (concat identity "\n")
                   (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
    (should (equal identity (niwa-workspace-identity-ensure root))))

  :doc "survives equivalent TRAMP spellings of the same project"
  (niwa-test--with-local-shell-tramp '("first" "second")
    (let ((first (format "/niwamock:first:%s" root))
          (second (format "/niwamock:second:%s" root)))
      (should (file-remote-p first))
      (should (find-file-name-handler first 'write-region))
      (should
       (equal (niwa-workspace-identity-ensure first)
              (niwa-workspace-identity-read second))))))

(provide 'test-niwa-workspace-identity)
;;; test-niwa-workspace-identity.el ends here
