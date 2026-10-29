;;; test-mevedel-tool-patch-shared.el --- Ordinary shared-file edits -*- lexical-binding: t -*-

;;; Commentary:

;; ApplyPatch must preserve authored shared-note bytes without a note schema.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-tool-patch-registration)
(require 'mevedel-tool-patch)
(require 'mevedel-resource)

(mevedel-deftest mevedel-tool-patch-handler/shared
    (:vars* ((root (file-name-as-directory (make-temp-file "plain-note-patch-" t)))
             (workspace (mevedel-workspace--create
                         :type 'file :id root :root root :name "notes"
                         :file-cache (mevedel-test-file-cache-create)))
             (session (mevedel-session--create :workspace workspace :working-directory root :permission-mode 'edits))
             (buffer (generate-new-buffer " *plain-note-patch*")))
     :before-each ((with-current-buffer buffer
                     (setq-local default-directory root mevedel--workspace workspace mevedel--session session)))
     :after-each ((kill-buffer buffer) (delete-directory root t)))
  ,test
  (test)
  :doc "shared addresses and physical paths preserve plain and arbitrary header-like text"
  (with-current-buffer buffer
    (dolist (operand '("work://shared/note.md" ".mevedel/shared/physical.MD" "outside.md"))
      (let ((path (if (string-prefix-p "work://" operand)
                      (file-name-concat (mevedel-resource-work-shared-directory workspace) "note.md")
                    (file-name-concat root operand)))
            result)
        (mevedel-tool-patch-handler
         (lambda (value) (setq result value))
         (list :patch (format "*** Begin Patch\n*** Add File: %s\n+# Body\n*** End Patch" operand)))
        (should-not (plist-get result :status))
        (should (equal "# Body\n" (mevedel-test--read-file path)))
        ;; No metadata parser may reject or rewrite this ordinary authored text.
        (mevedel-tool-patch-handler
         (lambda (value) (setq result value))
         (list :patch (format "*** Begin Patch\n*** Update File: %s\n@@\n-# Body\n+---\n+kind: invalid\n+# Edited\n*** End Patch" operand)))
        (should-not (plist-get result :status))
        (should (equal "---\nkind: invalid\n# Edited\n" (mevedel-test--read-file path)))))))

(provide 'test-mevedel-tool-patch-shared)
;;; test-mevedel-tool-patch-shared.el ends here
