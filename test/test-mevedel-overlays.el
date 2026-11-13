;;; test-mevedel-overlays.el -- Overlay command diagnostic tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for workspace-scoped instruction command diagnostics.

;;; Code:

(require 'mevedel-overlays)
(require 'mevedel-instruction-registry)
(require 'mevedel-note-source)
(require 'mevedel-overlay-ui)
(require 'mevedel-persistence)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-delete-instructions/source-diagnostics ()
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-source-scope-" t)))
         (nested (file-name-concat root "nested/"))
         (file (file-name-concat nested "source.txt"))
         (mevedel-workspace--registry (make-hash-table :test #'equal))
         (mevedel--instruction-states (make-hash-table :test #'equal))
         (mevedel--instruction-current-state-key :global)
         buffer workspace)
    (unwind-protect
        (progn
          (make-directory nested)
          (dolist (directory (list root nested))
            (should (zerop (process-file "git" nil nil nil "init" "--quiet" directory))))
          (mevedel-workspace-get-or-create 'project root root "Outer")
          (setq workspace (mevedel-workspace-get-or-create 'project nested nested "Inner"))
          (with-temp-file file (insert "blank source\n"))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (should-not mevedel--workspace)
            (should-not mevedel--session)
            (should (eq workspace (mevedel--instruction-buffer-workspace buffer)))
            (dolist (case '((mevedel-delete-instructions "No instruction to delete at point")))
              (should (equal (format "mevedel [%s]: %s" nested (cadr case))
                             (cadr (should-error (call-interactively (car case))
                                                 :type 'user-error)))))
            (let (messages)
              (mevedel-test--with-captured-messages messages
                (call-interactively #'mevedel-next-instruction))
              (should (equal (format "mevedel [%s]: No next instruction found\n" nested)
                             messages))
              (mevedel-test--with-captured-messages messages
                (call-interactively #'mevedel-cycle-instructions-at-point))
              (should (equal (format "mevedel [%s]: No instructions at point\n" nested)
                             messages)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(provide 'test-mevedel-overlays)
;;; test-mevedel-overlays.el ends here
