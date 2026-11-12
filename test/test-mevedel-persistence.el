;;; test-mevedel-persistence.el -- Tests for mevedel-persistence.el -*- lexical-binding: t -*-

;;; Commentary:

;; Integration tests for instruction snapshot persistence.

;;; Code:

(require 'mevedel-notes)

(require 'mevedel)
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

(mevedel-deftest mevedel--load-instructions-file
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "patches outdated content without an external diff program"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-outdated-persistence-" t)))
         (source (file-name-concat root "source.el"))
         (snapshot (file-name-concat root "instructions.el"))
         (workspace (mevedel-workspace--create
                     :type 'file :id root :root root :name "test"))
         (original "alpha\nreference\nmiddle\nsecond\nomega\n")
         (current "prefix\nalpha\nreference\nmiddle inserted\nsecond\nomega\nsuffix\n")
         (mevedel-patch-outdated-instructions t)
         source-buffer reference-id second-id diagnostics)
    (unwind-protect
        (progn
          (with-temp-file source (insert original))
          (setq source-buffer (find-file-noselect source))
          (with-current-buffer source-buffer
            (setq-local mevedel--workspace workspace)
            (goto-char (point-min))
            (search-forward "reference")
            (setq reference-id
                  (overlay-get
                   (mevedel-instruction-test--create-note-in
                    source-buffer (match-beginning 0) (match-end 0))
                   'mevedel-uuid))
            (search-forward "second")
            (setq second-id
                  (overlay-get
                   (mevedel-instruction-test--create-note-in
                    source-buffer (match-beginning 0) (match-end 0))
                   'mevedel-uuid))
            (mevedel--write-instructions-file snapshot root t t t)
            (mevedel--clear-instruction-state workspace)
            (erase-buffer)
            (insert current)
            (save-buffer))
          (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil)))
            (mevedel-test--with-captured-diagnostics diagnostics
              (mevedel--load-instructions-file
               snapshot root nil t workspace)))
          (should mevedel-patch-outdated-instructions)
          (should-not (string-match-p "requires.*diff" diagnostics))
          (with-current-buffer source-buffer
            (should (equal current (buffer-string)))
            (dolist (pair `((,reference-id . "reference")
                            (,second-id . "second")))
              (let ((overlay (mevedel--instruction-with-uuid
                              (car pair) workspace)))
                (should (overlayp overlay))
                (should (equal (cdr pair)
                               (buffer-substring-no-properties
                                (overlay-start overlay)
                                (overlay-end overlay))))))))
      (mevedel--clear-instruction-state workspace)
      (when (buffer-live-p source-buffer)
        (with-current-buffer source-buffer
          (setq-local kill-buffer-hook nil)
          (set-buffer-modified-p nil))
        (kill-buffer source-buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel--stash-instructions-on-kill
  ()
  ,test
  (test)
  :doc "leaves a buffer with no file alone, registry untouched"
  (let ((activations 0))
    (cl-letf (((symbol-function 'mevedel--instruction-activate-buffer)
               (lambda (&rest _) (setq activations (1+ activations)))))
      (with-temp-buffer
        (mevedel--stash-instructions-on-kill))
      ;; The global hook sees every buffer Emacs kills, internal ones
      ;; included; none of them can carry a stashable instruction.
      (should (zerop activations)))))


(mevedel-deftest mevedel--setup-buffer-hooks
  ()
  ,test
  (test)
  :doc "keeps the global kill hook at one entry and the rest buffer-local"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-buffer-hooks-" t)))
         (preinstalled (memq #'mevedel--stash-instructions-on-kill
                             (default-value 'kill-buffer-hook)))
         buffers)
    (unwind-protect
        (progn
          (dotimes (i 3)
            (let ((file (file-name-concat root (format "source-%d.el" i))))
              (with-temp-file file (insert ";; hook target\n"))
              (push (find-file-noselect file) buffers)))
          (let ((baseline (length (default-value 'kill-buffer-hook))))
            (dolist (buffer buffers)
              (mevedel--setup-buffer-hooks buffer)
              (mevedel--setup-buffer-hooks buffer))
            ;; Six setups may add at most the one named function; a
            ;; per-buffer closure on the global hook is the leak this
            ;; test exists to catch.
            (should (<= (length (default-value 'kill-buffer-hook))
                        (1+ baseline)))
            (should (= 1 (cl-count #'mevedel--stash-instructions-on-kill
                                   (default-value 'kill-buffer-hook)))))
          (dolist (buffer buffers)
            (with-current-buffer buffer
              (should mevedel--buffer-hooks-setup)
              (should (local-variable-p 'post-command-hook))
              (should (local-variable-p 'before-revert-hook))
              (should (local-variable-p 'after-revert-hook)))))
      (unless preinstalled
        (remove-hook 'kill-buffer-hook #'mevedel--stash-instructions-on-kill))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local kill-buffer-hook nil)
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory root t))))

(mevedel-deftest mevedel-persistence--write-save-file ()
  ,test
  (test)
  :doc "a save that dies mid-write leaves the previous snapshot readable"
  ;; The file is the durable record of every workspace instruction; the
  ;; old in-place `with-temp-file' truncated it before writing, so a
  ;; crash lost all instructions at once.
  (let* ((root (make-temp-file "mevedel-save-atomic-" t))
         (path (file-name-concat root "instructions.eld")))
    (unwind-protect
        (progn
          (mevedel-persistence--write-save-file path '(:version 1 :files (a)))
          (cl-letf (((symbol-function 'write-region)
                     (lambda (&rest _) (error "Disk full"))))
            (should-error
             (mevedel-persistence--write-save-file
              path '(:version 2 :files (b)))))
          (should (equal '(:version 1 :files (a))
                         (with-temp-buffer
                           (insert-file-contents path)
                           (read (current-buffer)))))
          (should-not (directory-files root nil "mevedel-write")))
      (delete-directory root t))))

(provide 'test-mevedel-persistence)
;;; test-mevedel-persistence.el ends here
