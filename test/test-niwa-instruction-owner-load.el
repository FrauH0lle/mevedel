;;; test-niwa-instruction-owner-load.el -- Cold instruction owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the split instruction owners without loading the niwa umbrella.

;;; Code:

(require 'bytecomp)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-instruction-owners/cold-load ()
  ,test
  (test)
  :doc "loads compiled owners and exercises their cross-owner entrypoints"
  (let ((root
         (file-name-as-directory
          (file-name-directory (locate-library "niwa"))))
        (compiled-root (make-temp-file "niwa-instruction-owners-" t))
        (emacs (expand-file-name invocation-name invocation-directory))
        (owners '("niwa-instruction-registry.el"
                  "niwa-note-source.el"
                  "niwa-overlays.el"
                  "niwa-overlay-ui.el"))
        (cases
         '((registry
            (progn
              (require 'niwa-instruction-registry)
              (unless (plist-member (niwa--instruction-state) :instructions)
                (error "Registry did not construct instruction state"))))
           (source
            (progn
              (require 'niwa-note-source)
              (with-temp-buffer
                (insert "source")
                (let ((overlay (niwa--create-instruction-overlay-in
                                (current-buffer) 1 7 'char)))
                  (unless (and (eq (current-buffer) (overlay-buffer overlay))
                               (integerp (overlay-get overlay 'niwa-id)))
                    (error "Source owner did not register its instruction"))))))
           (core
            (progn
              (require 'niwa-overlays)
              (niwa-delete-all-instructions)
              (unless (null (niwa--all-instructions))
                (error "Empty core registry returned instructions"))
              (with-temp-buffer
                (insert "source")
                (let* ((workspace
                        (niwa-workspace--create
                         :type 'file :id "core" :root default-directory
                         :name "core"))
                       (instruction
                        (make-overlay (point-min) (point-max))))
                  (setq-local niwa--workspace workspace)
                  (overlay-put instruction 'niwa-instruction t)
                  (overlay-put instruction 'niwa-instruction-type
                               'note)
                  (overlay-put instruction 'niwa-id 1)
                  (overlay-put instruction 'niwa-uuid "cold-note")
                  (niwa--instruction-activate-buffer (current-buffer))
                  (niwa--set-instruction-alist-value
                   (list (list (current-buffer) instruction)))
                  (goto-char (point-min))
                  (let ((inhibit-message t))
                    (niwa-delete-instructions))
                  (when (overlay-buffer instruction)
                    (error "Core command did not delete the instruction"))))))
           (ui
            (progn
              (require 'niwa-overlay-ui)
              (condition-case nil
                  (niwa--ov-actions-dispatch)
                (error nil))
              (unless (featurep 'niwa-notes)
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
                         (when (featurep 'niwa)
                           (error "Owner loaded the niwa umbrella")))))))
                (should (string-empty-p (string-trim (buffer-string))))))))
      (delete-directory compiled-root t))))

(provide 'test-niwa-instruction-owner-load)
;;; test-niwa-instruction-owner-load.el ends here
