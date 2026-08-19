;;; test-mevedel-view-input-files.el -- View file-input tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests local file drops and clipboard-image insertion.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-input-files)
(require 'mevedel-workspace)

(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)
(defvar mevedel-plugin-extra-roots)

(mevedel-deftest mevedel-view--insert-dropped-file-mentions ()
  ,test
  (test)
  :doc "inserts @file mentions and records exact grants"
  (let* ((dir (make-temp-file "mevedel dnd-" t))
         (path (expand-file-name "image file.png" dir))
         (data-buf (generate-new-buffer " *test-data-dnd*"))
         (view-buf (generate-new-buffer " *test-view-dnd*"))
         (ws (mevedel-workspace--create :type 'project :id "dnd"
                                        :root dir :name "dnd"))
         (session (mevedel-session-create "main" ws)))
    (with-temp-file path (insert "fake image\n"))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace ws))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (goto-char (point-max))
            (mevedel-view--insert-dropped-file-mentions (list path))
            (should (equal (format "@file:{%s}" path)
                           (mevedel-view--input-text)))
            (let* ((input (mevedel-view--input-text))
                   (binding (get-text-property
                             0 'mevedel-mention-binding input)))
              (should (equal (list :kind 'file
                                   :token (format "@file:{%s}" path)
                                   :path path)
                             binding)))
            (should (equal (list path)
                           (mevedel-session-dropped-file-grants session)))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (delete-directory dir t))))

(mevedel-deftest mevedel-view--dnd-handle-files ()
  ,test
  (test)
  :doc "accepts the single-URI protocol-handler shape"
  (let* ((dir (make-temp-file "mevedel-dnd-" t))
         (path (expand-file-name "single.txt" dir))
         (uri (concat "file://" path))
         (data-buf (generate-new-buffer " *test-data-dnd-single*"))
         (view-buf (generate-new-buffer " *test-view-dnd-single*"))
         (ws (mevedel-workspace--create :type 'project :id "dnd-single"
                                        :root dir :name "dnd-single"))
         (session (mevedel-session-create "main" ws)))
    (with-temp-file path (insert "single\n"))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace ws))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (goto-char (point-max))
            (should (eq 'copy
                        (mevedel-view--dnd-handle-files uri 'copy)))
            (should (equal (format "@file:%s" path)
                           (mevedel-view--input-text)))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (delete-directory dir t))))

(mevedel-deftest mevedel-view-yank-dwim ()
  ,test
  (test)
  :doc "saves clipboard images to workspace media and inserts @file"
  (let* ((dir (make-temp-file "mevedel-clipboard-" t))
         (data-buf (generate-new-buffer " *test-data-clipboard*"))
         (view-buf (generate-new-buffer " *test-view-clipboard*"))
         (ws (mevedel-workspace--create :type 'project :id "clipboard"
                                        :root dir :name "clipboard"))
         (session (mevedel-session-create "main" ws))
         (expected (file-name-concat
                    dir ".mevedel" "media"
                    "clipboard-20260620-121314.png"))
         (mevedel-view-clipboard-image-handlers
          `(((:command . "fake-clipboard")
             (:save . ,(lambda (file-path)
                         (with-temp-file file-path
                           (set-buffer-multibyte nil)
                           (insert "png"))))))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace ws))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (goto-char (point-max))
            (cl-letf (((symbol-function 'window-system)
                       (lambda (&optional _frame) 'x))
                      ((symbol-function 'executable-find)
                       (lambda (command)
                         (and (equal command "fake-clipboard") command)))
                      ((symbol-function 'format-time-string)
                       (lambda (&rest _) "20260620-121314")))
              (mevedel-view-yank-dwim))
            (should (file-exists-p expected))
            (should (equal (format "@file:%s" expected)
                           (mevedel-view--input-text)))
            (should (equal (list expected)
                           (mevedel-session-dropped-file-grants session)))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (delete-directory dir t)))

  :doc "falls back to text yank when image saving has no session"
  (with-temp-buffer
    (let ((kill-ring '("plain text"))
          (kill-ring-yank-pointer nil)
          (mevedel-view-clipboard-image-handlers
           '(((:command . "fake-clipboard") (:save . ignore)))))
      (cl-letf (((symbol-function 'window-system)
                 (lambda (&optional _frame) 'x))
                ((symbol-function 'executable-find)
                 (lambda (command)
                   (and (equal command "fake-clipboard") command))))
        (mevedel-view-yank-dwim))
      (should (equal "plain text" (buffer-string))))))

(provide 'test-mevedel-view-input-files)
;;; test-mevedel-view-input-files.el ends here
