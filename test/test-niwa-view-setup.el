;;; test-niwa-view-setup.el --- View setup tests -*- lexical-binding: t -*-

;;; Commentary:

;; Focused contract tests for view buffer setup and creation.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-menu)
(require 'niwa-skills-core)
(require 'niwa-structs)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-workspace)

(niwa-deftest niwa-surface--enforce-ephemeral
  (:doc "cursor stays by wrapped hidden markup with both header rows visible")
  (let ((process-environment (copy-sequence process-environment)))
    (skip-unless (niwa-test--wayland-available-p))
    (setenv "NIWA_TEST_LOAD_PATH" (prin1-to-string load-path))
    (pcase-let ((`(,observed ,status . ,_trace)
                 (niwa-test--run-wayland-fixture "view-cursor-wayland.el" 20)))
      (ert-info ((format "Observed: %S" observed))
        (should (= status 0))
        (should-not (plist-get observed :modified))
        (let* ((rows (plist-get observed :rows))
               (origin (aref (plist-get (car rows) :cursor) 2)))
          (should (= (length rows) 5))
          (should (equal (mapcar (lambda (row) (plist-get row :hidden)) rows)
                         '(nil t nil t nil)))
          (dolist (row rows)
            (should (< (abs (- (aref (plist-get row :cursor) 2) origin))
                       (* 2 (plist-get observed :line-height))))))))))

(niwa-deftest niwa-view--setup ()
  ,test
  (test)
  :doc "fontifies known dollar skill mentions"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session-create
                     "main"
                     (niwa-workspace--create
                      :type 'file :id "skills" :root "/tmp/skills"
                      :name "skills")))
           (skill (niwa-skill--create :name "review" :body "Review")))
      (setf (niwa-session-skills session) (list skill))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (niwa-view--setup view-buf data-buf)
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft
         "Run $review but keep $PATH literal")
        (with-current-buffer (niwa-view-composer-buffer view-buf)
          (font-lock-mode 1)
          (let ((start (niwa-view-composer-draft-start view-buf))
                (end (niwa-view-composer-draft-end view-buf)))
            (font-lock-flush start end)
            (font-lock-ensure start end)
            (goto-char start))
          (search-forward "$review")
          (should (memq 'font-lock-keyword-face
                        (ensure-list
                         (get-text-property (match-beginning 0) 'face))))
          (search-forward "$PATH")
          (should-not (get-text-property (match-beginning 0) 'face))))))

  :doc "wires buffers together correctly"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (should (eq niwa--data-buffer data-buf))
      (should (niwa-view-transcript-tail-marker))
      (should (derived-mode-p 'niwa-surface-mode))
      (should (derived-mode-p 'niwa-view-mode))
      (should-not buffer-read-only))
    (with-current-buffer data-buf
      (should (eq niwa--view-buffer view-buf))
      (should (eq (local-key-binding (kbd "C-c C-o"))
                  #'niwa-menu))
      (should-not (eq (local-key-binding (kbd "C-c C-m"))
                      #'niwa-menu))
      (let ((data-map (current-local-map)))
        (with-temp-buffer
          (org-mode)
          (should-not (eq data-map (current-local-map)))
          (should-not (eq (lookup-key (current-local-map) (kbd "C-c C-o"))
                          #'niwa-menu))))))

  :doc "view buffers are ephemeral and never offered for saving"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (should-not buffer-file-name)
      (should-not buffer-offer-save)
      (should-not buffer-auto-save-file-name)
      (niwa-view--insert-user-message "transcript text")
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
  (niwa-view-test--with-buffers
    (let ((fake-file (make-temp-file "niwa-view-leaked-file-")))
      (unwind-protect
          (with-current-buffer view-buf
            (setq buffer-file-name fake-file
                  buffer-file-truename (file-truename fake-file))
            (set-buffer-modified-p t)
            (niwa-view--insert-user-message "transcript text")
            (should-not buffer-file-name)
            (should-not buffer-file-truename)
            (should-not (buffer-modified-p))
            (should-not (memq view-buf
                              (files--buffers-needing-to-be-saved t))))
        (when (file-exists-p fake-file)
          (delete-file fake-file))))))

(niwa-deftest niwa-view--ensure ()
  ,test
  (test)

  :doc "derived view names do not inherit an internal buffer prefix"
  (let ((data-buf (generate-new-buffer " *niwa-view-hidden-data*"))
        view-buf)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode))
          (setq view-buf (niwa-view--ensure data-buf))
          (should-not (string-prefix-p " " (buffer-name view-buf)))
          (should (eq data-buf
                      (buffer-local-value 'niwa--data-buffer view-buf)))
          (should (eq view-buf
                      (buffer-local-value 'niwa--view-buffer data-buf))))
      (when (buffer-live-p view-buf)
        (kill-buffer view-buf))
      (when (buffer-live-p data-buf)
        (kill-buffer data-buf))))

  :doc "setup failure removes only the newly created partial view"
  (let ((data-buf (generate-new-buffer " *niwa-view-ensure-data*"))
        (view-name " *niwa-view-ensure-partial*"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'niwa-view-composer-initialize)
                     (lambda () (error "Injected view setup failure"))))
            (should (string-match-p
                     "Injected view setup failure"
                     (error-message-string
                      (should-error
                       (niwa-view--ensure data-buf view-name))))))
          (should (buffer-live-p data-buf))
          (should-not (get-buffer view-name))
          (should-not (buffer-local-value 'niwa--view-buffer data-buf)))
      (when-let* ((view-buf (get-buffer view-name)))
        (kill-buffer view-buf))
      (when (buffer-live-p data-buf)
        (kill-buffer data-buf)))))

(provide 'test-niwa-view-setup)
;;; test-niwa-view-setup.el ends here
