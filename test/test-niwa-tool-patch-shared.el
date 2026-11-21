;;; test-niwa-tool-patch-shared.el --- Ordinary shared-file edits -*- lexical-binding: t -*-

;;; Commentary:

;; ApplyPatch must preserve authored shared-note bytes without a note schema.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-tool-patch-registration)
(require 'niwa-tool-patch)
(require 'niwa-resource)

(niwa-deftest niwa-tool-patch-handler/shared
    (:vars* ((root (file-name-as-directory (make-temp-file "plain-note-patch-" t)))
             (workspace (niwa-workspace--create
                         :type 'file :id root :root root :name "notes"
                         :file-cache (niwa-test-file-cache-create)))
             (session (niwa-session--create :workspace workspace :working-directory root :permission-mode 'edits))
             (buffer (generate-new-buffer " *plain-note-patch*")))
     :before-each ((with-current-buffer buffer
                     (setq-local default-directory root niwa--workspace workspace niwa--session session)))
     :after-each ((kill-buffer buffer) (delete-directory root t)))
  ,test
  (test)
  :doc "shared addresses and physical paths preserve plain and arbitrary header-like text"
  (with-current-buffer buffer
    (dolist (operand '("work://shared/note.md" ".niwa/shared/physical.MD" "outside.md"))
      (let ((path (if (string-prefix-p "work://" operand)
                      (file-name-concat (niwa-resource-work-shared-directory workspace) "note.md")
                    (file-name-concat root operand)))
            result)
        (niwa-tool-patch-handler
         (lambda (value) (setq result value))
         (list :patch (format "*** Begin Patch\n*** Add File: %s\n+# Body\n*** End Patch" operand)))
        (should-not (plist-get result :status))
        (should (equal "# Body\n" (niwa-test--read-file path)))
        ;; No metadata parser may reject or rewrite this ordinary authored text.
        (niwa-tool-patch-handler
         (lambda (value) (setq result value))
         (list :patch (format "*** Begin Patch\n*** Update File: %s\n@@\n-# Body\n+---\n+kind: invalid\n+# Edited\n*** End Patch" operand)))
        (should-not (plist-get result :status))
        (should (equal "---\nkind: invalid\n# Edited\n" (niwa-test--read-file path)))))))

(provide 'test-niwa-tool-patch-shared)
;;; test-niwa-tool-patch-shared.el ends here
