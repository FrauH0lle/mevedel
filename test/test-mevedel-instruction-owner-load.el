;;; test-mevedel-instruction-owner-load.el -- Cold instruction owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the split instruction owners without loading the mevedel umbrella.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-instruction-owners/cold-load ()
  ,test
  (test)
  :doc "loads compiled owners and exercises their cross-owner entrypoints"
  (let ((root
         (file-name-as-directory
          (file-name-directory (locate-library "mevedel"))))
        (compiled-root (make-temp-file "mevedel-instruction-owners-" t))
        (emacs (expand-file-name invocation-name invocation-directory))
        (owners '("mevedel-instruction-registry.el"
                  "mevedel-directive-source.el"
                  "mevedel-overlays.el"
                  "mevedel-overlay-ui.el"))
        (cases
         '((registry
            (progn
              (require 'mevedel-instruction-registry)
              (unless (plist-member (mevedel--instruction-state) :instructions)
                (error "Registry did not construct instruction state"))))
           (source
            (progn
              (require 'mevedel-structs)
              (require 'mevedel-directive-source)
              (let* ((record
                      (mevedel-directive--create
                       :id "cold" :request "request"
                       :anchor '(:state source-missing :file "missing")
                       :attempts '(attempt)))
                     (workspace
                      (mevedel-workspace--create
                       :type 'file :id "cold" :root default-directory
                       :name "cold" :directives (list record))))
                (unless (eq record
                            (mevedel-archive-directive record workspace))
                  (error "Source owner did not archive its record")))))
           (core
            (progn
              (require 'mevedel-overlays)
              (mevedel-delete-all-instructions)
              (unless (null (mevedel--filter-references nil))
                (error "Empty core registry returned references"))
              (with-temp-buffer
                (insert "source")
                (let* ((workspace
                        (mevedel-workspace--create
                         :type 'file :id "core" :root default-directory
                         :name "core"))
                       (instruction
                        (make-overlay (point-min) (point-max))))
                  (setq-local mevedel--workspace workspace)
                  (overlay-put instruction 'mevedel-instruction t)
                  (overlay-put instruction 'mevedel-instruction-type
                               'reference)
                  (overlay-put instruction 'mevedel-id 1)
                  (overlay-put instruction 'mevedel-uuid "cold-reference")
                  (mevedel--instruction-activate-buffer (current-buffer))
                  (mevedel--set-instruction-alist-value
                   (list (list (current-buffer) instruction)))
                  (goto-char (point-min))
                  (let ((inhibit-message t))
                    (mevedel-delete-instructions))
                  (when (overlay-buffer instruction)
                    (error "Core command did not delete the instruction"))))))
           (ui
            (progn
              (require 'mevedel-overlay-ui)
              (condition-case nil
                  (mevedel--ov-actions-abort)
                (error nil))
              (unless (featurep 'mevedel-chat)
                (error "UI action did not load its target owner")))))))
    (unwind-protect
        (progn
          (dolist (owner owners)
            (copy-file (file-name-concat root owner)
                       (file-name-concat compiled-root owner)))
          (let ((byte-compile-verbose nil))
            (dolist (owner owners)
              (byte-compile-file (file-name-concat compiled-root owner))))
          (dolist (case cases)
            (with-temp-buffer
              (ert-info ((format "cold owner: %s" (car case)))
                (should
                 (= 0
                    (call-process
                     emacs nil t nil
                     "--batch" "-Q" "-L" compiled-root "-L" root
                     "--eval"
                     (prin1-to-string
                      `(progn
                         ,(cadr case)
                         (when (featurep 'mevedel)
                           (error "Owner loaded the mevedel umbrella")))))))
                (should (string-empty-p (string-trim (buffer-string))))))))
      (delete-directory compiled-root t))))

(provide 'test-mevedel-instruction-owner-load)
;;; test-mevedel-instruction-owner-load.el ends here
