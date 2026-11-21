;;; test-niwa-overlays-delete.el -- Bulk instruction deletion tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise workspace scope and durable cleanup through interactive commands.

;;; Code:

(require 'niwa-overlays)
(require 'niwa-notes)
(require 'niwa-note-source)
(require 'niwa-instruction-registry)
(require 'niwa-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-instruction-test-support"))

(niwa-deftest niwa--delete-all-instructions-of-type
  (:vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "deletion isolates workspaces and counts every note buffer"
  (let* ((workspace-a (niwa-workspace--create :type 'file :id "a"))
         (workspace-b (niwa-workspace--create :type 'file :id "b"))
         (first (niwa-instruction-test--make-note "first\n" workspace-a))
         (second (niwa-instruction-test--make-note "second\n" workspace-a))
         (other (niwa-instruction-test--make-note "other\n" workspace-b))
         prompts messages)
    (unwind-protect
        (progn
          (with-current-buffer (car first)
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (prompt) (push prompt prompts) t)))
              (niwa-test--with-captured-messages messages
                (call-interactively #'niwa-delete-all-instructions))))
          (should (= 1 (length prompts)))
          (should (string-match-p "2 instructions in 2 buffers" (car prompts)))
          (should (string-search "Deleted 2 instructions in 2 buffers" messages))
          (should-not (overlay-buffer (cdr first)))
          (should-not (overlay-buffer (cdr second)))
          (should (overlay-buffer (cdr other))))
      (mapc #'niwa-instruction-test--discard (list first second other))))

  :doc "all no-workspace buffers share the global bucket, not workspace buckets"
  (let* ((workspace (niwa-workspace--create :type 'file :id "project"))
         (project (niwa-instruction-test--make-note "project\n" workspace))
         (first (generate-new-buffer " *global-first*"))
         (second (generate-new-buffer " *global-second*"))
         overlays)
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (insert "global")
              (push (niwa-instruction-test--create-note-in buffer 1 7) overlays)))
          (with-current-buffer first
            (niwa-test--with-captured-messages nil
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
                (call-interactively #'niwa-delete-all-instructions))))
          (should-not (seq-some #'overlay-buffer overlays))
          (should (overlay-buffer (cdr project))))
      (kill-buffer first)
      (kill-buffer second)
      (niwa-instruction-test--discard project))))

(provide 'test-niwa-overlays-delete)
;;; test-niwa-overlays-delete.el ends here
