;;; test-mevedel-overlays-delete.el -- Bulk instruction deletion tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise workspace scope and durable cleanup through interactive commands.

;;; Code:

(require 'mevedel-overlays)
(require 'mevedel-notes)
(require 'mevedel-directive-source)
(require 'mevedel-instruction-registry)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-instruction-test-support"))

(mevedel-deftest mevedel--delete-all-instructions-of-type
  (:vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "deletion isolates workspaces and counts every note buffer"
  (let* ((workspace-a (mevedel-workspace--create :type 'file :id "a"))
         (workspace-b (mevedel-workspace--create :type 'file :id "b"))
         (first (mevedel-instruction-test--make-note "first\n" workspace-a))
         (second (mevedel-instruction-test--make-note "second\n" workspace-a))
         (other (mevedel-instruction-test--make-note "other\n" workspace-b))
         prompts messages)
    (unwind-protect
        (progn
          (with-current-buffer (car first)
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (prompt) (push prompt prompts) t)))
              (mevedel-test--with-captured-messages messages
                (call-interactively #'mevedel-delete-all-instructions))))
          (should (= 1 (length prompts)))
          (should (string-match-p "2 instructions in 2 buffers" (car prompts)))
          (should (string-search "Deleted 2 instructions in 2 buffers" messages))
          (should-not (overlay-buffer (cdr first)))
          (should-not (overlay-buffer (cdr second)))
          (should (overlay-buffer (cdr other))))
      (mapc #'mevedel-instruction-test--discard (list first second other))))

  :doc "all no-workspace buffers share the global bucket, not workspace buckets"
  (let* ((workspace (mevedel-workspace--create :type 'file :id "project"))
         (project (mevedel-instruction-test--make-note "project\n" workspace))
         (first (generate-new-buffer " *global-first*"))
         (second (generate-new-buffer " *global-second*"))
         overlays)
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (insert "global")
              (push (mevedel-instruction-test--create-note-in buffer 1 7) overlays)))
          (with-current-buffer first
            (mevedel-test--with-captured-messages nil
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
                (call-interactively #'mevedel-delete-all-instructions))))
          (should-not (seq-some #'overlay-buffer overlays))
          (should (overlay-buffer (cdr project))))
      (kill-buffer first)
      (kill-buffer second)
      (mevedel-instruction-test--discard project))))

(provide 'test-mevedel-overlays-delete)
;;; test-mevedel-overlays-delete.el ends here
