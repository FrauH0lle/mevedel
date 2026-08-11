;;; test-mevedel-workspace-identity.el --- Workspace identity tests -*- lexical-binding: t -*-

;;; Commentary:

;; Covers project-owned workspace identity creation and lookup.

;;; Code:

(require 'ert)
(require 'mevedel-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Workspace identity

(mevedel-deftest mevedel-workspace-identity-read
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "mevedel-workspace-identity-" t))))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "returns nil before the workspace identity exists"
  (should-not (mevedel-workspace-identity-read root))

  :doc "reads the project-owned identity"
  (let ((path (file-name-concat root ".mevedel" "workspace-id"))
        (identity (make-string 64 ?a)))
    (make-directory (file-name-directory path) t)
    (write-region (concat identity "\n") nil path nil 'silent)
    (should (equal identity (mevedel-workspace-identity-read root))))

  :doc "rejects malformed identity files"
  (let ((path (file-name-concat root ".mevedel" "workspace-id")))
    (make-directory (file-name-directory path) t)
    (write-region "not-an-identity\n" nil path nil 'silent)
    (should-error (mevedel-workspace-identity-read root)
                  :type 'error)))

(mevedel-deftest mevedel-workspace-identity-ensure
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "mevedel-workspace-identity-" t))))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "creates one bounded opaque identity and reuses it"
  (let* ((identity (mevedel-workspace-identity-ensure root))
         (path (file-name-concat root ".mevedel" "workspace-id")))
    (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" identity))
    (should (equal (concat identity "\n")
                   (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
    (should (equal identity (mevedel-workspace-identity-ensure root))))

  :doc "survives equivalent TRAMP spellings of the same project"
  (mevedel-test--with-local-shell-tramp '("first" "second")
    (let ((first (format "/mevedelmock:first:%s" root))
          (second (format "/mevedelmock:second:%s" root)))
      (should (file-remote-p first))
      (should (find-file-name-handler first 'write-region))
      (should
       (equal (mevedel-workspace-identity-ensure first)
              (mevedel-workspace-identity-read second))))))

(provide 'test-mevedel-workspace-identity)
;;; test-mevedel-workspace-identity.el ends here
