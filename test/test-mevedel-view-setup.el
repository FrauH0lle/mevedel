;;; test-mevedel-view-setup.el --- View setup tests -*- lexical-binding: t -*-

;;; Commentary:

;; Focused contract tests for view buffer setup and creation.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-menu)
(require 'mevedel-skills-core)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'mevedel-workspace)

(mevedel-deftest mevedel-view--setup ()
  ,test
  (test)
  :doc "fontifies known dollar skill mentions"
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-session-create
                     "main"
                     (mevedel-workspace--create
                      :type 'file :id "skills" :root "/tmp/skills"
                      :name "skills")))
           (skill (mevedel-skill--create :name "review" :body "Review")))
      (setf (mevedel-session-skills session) (list skill))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (font-lock-mode 1)
        (goto-char (mevedel-view--input-start))
        (insert "Run $review but keep $PATH literal")
        (font-lock-flush (mevedel-view--input-start) (point-max))
        (font-lock-ensure (mevedel-view--input-start) (point-max))
        (goto-char (mevedel-view--input-start))
        (search-forward "$review")
        (should (memq 'font-lock-keyword-face
                      (ensure-list
                       (get-text-property (match-beginning 0) 'face))))
        (search-forward "$PATH")
        (should-not (get-text-property (match-beginning 0) 'face)))))

  :doc "wires buffers together correctly"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (should (eq mevedel--data-buffer data-buf))
      (should mevedel-view--input-marker)
      (should (derived-mode-p 'mevedel-surface-mode))
      (should (derived-mode-p 'mevedel-view-mode))
      (should-not buffer-read-only))
    (with-current-buffer data-buf
      (should (eq mevedel--view-buffer view-buf))
      (should (eq (local-key-binding (kbd "C-c C-o"))
                  #'mevedel-menu))
      (should-not (eq (local-key-binding (kbd "C-c C-m"))
                      #'mevedel-menu))
      (let ((data-map (current-local-map)))
        (with-temp-buffer
          (org-mode)
          (should-not (eq data-map (current-local-map)))
          (should-not (eq (lookup-key (current-local-map) (kbd "C-c C-o"))
                          #'mevedel-menu))))))

  :doc "view buffers are ephemeral and never offered for saving"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (should-not buffer-file-name)
      (should-not buffer-offer-save)
      (should-not buffer-auto-save-file-name)
      (goto-char (point-max))
      (insert "draft input")
      (should-not buffer-file-name)
      (should-not buffer-offer-save)
      (should-not (buffer-modified-p))
      (should-not (memq view-buf (files--buffers-needing-to-be-saved t))))
    (let ((prompted nil))
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _)
                   (setq prompted t)
                   (error "View buffer requested save filename")))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _)
                   (setq prompted t)
                   (error "View buffer requested save confirmation"))))
        (save-some-buffers t (lambda () (eq (current-buffer) view-buf))))
      (should-not prompted)))

  :doc "view buffers stay out of save prompts even if a file name leaks in"
  (mevedel-view-test--with-buffers
    (let ((fake-file (make-temp-file "mevedel-view-leaked-file-")))
      (unwind-protect
          (with-current-buffer view-buf
            (setq buffer-file-name fake-file
                  buffer-file-truename (file-truename fake-file))
            (set-buffer-modified-p t)
            (goto-char (point-max))
            (insert "draft input")
            (should-not buffer-file-name)
            (should-not buffer-file-truename)
            (should-not (buffer-modified-p))
            (should-not (memq view-buf
                              (files--buffers-needing-to-be-saved t))))
        (when (file-exists-p fake-file)
          (delete-file fake-file))))))

(mevedel-deftest mevedel-view--ensure ()
  ,test
  (test)

  :doc "derived view names do not inherit an internal buffer prefix"
  (let ((data-buf (generate-new-buffer " *mevedel-view-hidden-data*"))
        view-buf)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode))
          (setq view-buf (mevedel-view--ensure data-buf))
          (should-not (string-prefix-p " " (buffer-name view-buf)))
          (should (eq data-buf
                      (buffer-local-value 'mevedel--data-buffer view-buf)))
          (should (eq view-buf
                      (buffer-local-value 'mevedel--view-buffer data-buf))))
      (when (buffer-live-p view-buf)
        (kill-buffer view-buf))
      (when (buffer-live-p data-buf)
        (kill-buffer data-buf))))

  :doc "setup failure removes only the newly created partial view"
  (let ((data-buf (generate-new-buffer " *mevedel-view-ensure-data*"))
        (view-name " *mevedel-view-ensure-partial*"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-view-composer-initialize)
                     (lambda () (error "Injected view setup failure"))))
            (should (string-match-p
                     "Injected view setup failure"
                     (error-message-string
                      (should-error
                       (mevedel-view--ensure data-buf view-name))))))
          (should (buffer-live-p data-buf))
          (should-not (get-buffer view-name))
          (should-not (buffer-local-value 'mevedel--view-buffer data-buf)))
      (when-let* ((view-buf (get-buffer view-name)))
        (kill-buffer view-buf))
      (when (buffer-live-p data-buf)
        (kill-buffer data-buf)))))

(provide 'test-mevedel-view-setup)
;;; test-mevedel-view-setup.el ends here
