;;; test-niwa-overlays.el -- Overlay command diagnostic tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for workspace-scoped instruction command diagnostics.

;;; Code:

(require 'niwa-overlays)
(require 'niwa-instruction-registry)
(require 'niwa-note-source)
(require 'niwa-overlay-ui)
(require 'niwa-persistence)
(require 'niwa-structs)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-delete-instructions/source-diagnostics ()
  (let* ((root (file-name-as-directory (make-temp-file "niwa-source-scope-" t)))
         (nested (file-name-concat root "nested/"))
         (file (file-name-concat nested "source.txt"))
         (niwa-workspace--registry (make-hash-table :test #'equal))
         (niwa--instruction-states (make-hash-table :test #'equal))
         (niwa--instruction-current-state-key :global)
         buffer workspace)
    (unwind-protect
        (progn
          (make-directory nested)
          (dolist (directory (list root nested))
            (should (zerop (process-file "git" nil nil nil "init" "--quiet" directory))))
          (niwa-workspace-get-or-create 'project root root "Outer")
          (setq workspace (niwa-workspace-get-or-create 'project nested nested "Inner"))
          (with-temp-file file (insert "blank source\n"))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (should-not niwa--workspace)
            (should-not niwa--session)
            (should (eq workspace (niwa--instruction-buffer-workspace buffer)))
            (dolist (case '((niwa-delete-instructions "No instruction to delete at point")))
              (should (equal (format "niwa [%s]: %s" nested (cadr case))
                             (cadr (should-error (call-interactively (car case))
                                                 :type 'user-error)))))
            (let (messages)
              (niwa-test--with-captured-messages messages
                (call-interactively #'niwa-next-instruction))
              (should (equal (format "niwa [%s]: No next instruction found\n" nested)
                             messages))
              (niwa-test--with-captured-messages messages
                (call-interactively #'niwa-cycle-instructions-at-point))
              (should (equal (format "niwa [%s]: No instructions at point\n" nested)
                             messages)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(provide 'test-niwa-overlays)
;;; test-niwa-overlays.el ends here
