;;; test-mevedel-artifacts-list.el --- Session artifacts cockpit tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the artifacts cockpit projection, open, and delete-as-unpublish.

;;; Code:

(require 'mevedel-artifacts-list)
(require 'mevedel-cockpit)
(require 'mevedel-session-artifacts)
(require 'mevedel-structs)
(require 'tabulated-list)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-artifacts-list-test--context (session view data)
  "Return a cockpit context for SESSION, VIEW, and DATA."
  (list :session session
        :view-buffer view
        :data-buffer data
        :origin-buffer view))

(mevedel-deftest mevedel-artifacts-list-open ()
  ,test
  (test)
  :doc "lists the artifacts folder without a room and shows details"
  (let* ((save-path (make-temp-file "mevedel-artifacts-cockpit-" t))
         (dir (mevedel-session-artifacts-artifacts-dir save-path))
         (session (mevedel-session--create :name "main"
                                           :save-path save-path))
         (view (generate-new-buffer " *artifacts-list-view*"))
         (data (generate-new-buffer " *artifacts-list-data*")))
    (unwind-protect
        (progn
          (make-directory (file-name-concat dir "sub") t)
          (write-region "<h1>hi</h1>" nil
                        (file-name-concat dir "mockup.html") nil 'silent)
          (write-region "notes" nil
                        (file-name-concat dir "sub" "notes.md") nil 'silent)
          (let ((buffer (mevedel-artifacts-list-open
                         (mevedel-artifacts-list-test--context
                          session view data))))
            (with-current-buffer buffer
              (should (eq major-mode 'mevedel-artifacts-list-mode))
              (should (= 2 (length tabulated-list-entries)))
              (should (string-match-p
                       "mevedel: artifacts.*main.*2 files"
                       (substring-no-properties
                        (mevedel-cockpit-surface-header-line))))
              ;; Nested files list under their relative name.
              (should (cl-find-if
                       (lambda (entry)
                         (equal "sub/notes.md" (aref (cadr entry) 0)))
                       tabulated-list-entries))
              (let ((item (mevedel-cockpit-surface-selected)))
                (should (string-match-p
                         "Path: .*artifacts"
                         (mevedel-artifacts-list--details item nil)))))))
      (when-let* ((buffer (get-buffer mevedel-artifacts-list-buffer-name)))
        (kill-buffer buffer))
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data))
      (delete-directory save-path t))))

(mevedel-deftest mevedel-artifacts-list--files ()
  ,test
  (test)
  :doc "returns nothing without a save path or an artifacts directory"
  (progn
    (should-not (mevedel-artifacts-list--files
                 (mevedel-session--create :name "no-save-path")))
    (let ((save-path (make-temp-file "mevedel-artifacts-empty-" t)))
      (unwind-protect
          (should-not (mevedel-artifacts-list--files
                       (mevedel-session--create :name "empty"
                                                :save-path save-path)))
        (delete-directory save-path t)))))

(mevedel-deftest mevedel-artifacts-list-open-browser ()
  ,test
  (test)
  :doc "opens local files in the browser and rejects a stale selection"
  (let ((path (make-temp-file "mevedel-artifact-open-" nil ".html"))
        opened item)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-cockpit-surface-selected)
                   (lambda (&optional _) item))
                  ((symbol-function 'mevedel-cockpit-surface-refresh)
                   (lambda (&optional _) nil))
                  ((symbol-function 'browse-url-of-file)
                   (lambda (seen) (setq opened seen))))
          (setq item (list :name "open.html" :path path))
          (mevedel-artifacts-list-open-browser)
          (should (equal path opened))
          (setq item (list :name "gone.html"
                           :path "/missing/mevedel-artifact.html"))
          (should-error (mevedel-artifacts-list-open-browser)
                        :type 'user-error))
      (when (file-exists-p path) (delete-file path)))))

(mevedel-deftest mevedel-artifacts-list-delete (:quiet t)
  ,test
  (test)
  :doc "deletes the file, which unpublishes it, and tells the room"
  (let* ((save-path (make-temp-file "mevedel-artifacts-delete-" t))
         (dir (mevedel-session-artifacts-artifacts-dir save-path))
         (session (mevedel-session--create :name "main"
                                           :save-path save-path))
         (view (generate-new-buffer " *artifacts-delete-view*"))
         (data (generate-new-buffer " *artifacts-delete-data*"))
         (path (file-name-concat dir "mockup.html"))
         notified)
    (unwind-protect
        (progn
          (make-directory dir t)
          (write-region "<h1>hi</h1>" nil path nil 'silent)
          (let ((buffer (mevedel-artifacts-list-open
                         (mevedel-artifacts-list-test--context
                          session view data))))
            (with-current-buffer buffer
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                        ((symbol-function
                          'mevedel-collaboration-notify-artifacts-changed)
                         (lambda (seen) (setq notified seen))))
                (mevedel-artifacts-list-delete))
              (should-not (file-exists-p path))
              (should (eq session notified))
              (should-not tabulated-list-entries)
              ;; A declined confirmation deletes nothing.
              (write-region "<h1>hi</h1>" nil path nil 'silent)
              (mevedel-cockpit-surface-refresh)
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
                (mevedel-artifacts-list-delete))
              (should (file-exists-p path)))))
      (when-let* ((buffer (get-buffer mevedel-artifacts-list-buffer-name)))
        (kill-buffer buffer))
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data))
      (delete-directory save-path t))))

(mevedel-deftest mevedel-artifacts-list-quit ()
  ,test
  (test)
  :doc "returns to the owning session cockpit"
  (let (label)
    (cl-letf (((symbol-function 'mevedel-cockpit-quit)
               (lambda (&optional seen) (setq label seen))))
      (mevedel-artifacts-list-quit))
    (should (equal "artifacts cockpit" label))))

(provide 'test-mevedel-artifacts-list)
;;; test-mevedel-artifacts-list.el ends here
