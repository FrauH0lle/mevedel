;;; test-niwa-view-input-files.el -- View file-input tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests local file drops and clipboard-image insertion.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-structs)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-input-files)
(require 'niwa-workspace)
(require 'niwa-session-persistence)

(defvar gptel-response-separator)
(defvar niwa-plugin-extra-roots)

(niwa-deftest niwa-view--insert-dropped-file-mentions ()
  ,test
  (test)
  :doc "inserts @file mentions and records exact grants"
  (let* ((dir (make-temp-file "niwa dnd-" t))
         (path (expand-file-name "image file.png" dir))
         (data-buf (generate-new-buffer " *test-data-dnd*"))
         (view-buf (generate-new-buffer " *test-view-dnd*"))
         (ws (niwa-workspace--create :type 'project :id "dnd"
                                        :root dir :name "dnd"))
         (session (niwa-session-create "main" ws)))
    (with-temp-file path (insert "fake image\n"))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (niwa--transcript-org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local niwa--session session)
            (setq-local niwa--workspace ws))
          (niwa-view--setup view-buf data-buf)
          (with-current-buffer (niwa-view-composer-buffer view-buf)
            (let ((buffer-read-only t))
              (should-error (niwa-view--insert-dropped-file-mentions (list path))
                            :type 'buffer-read-only))
            (should-not (niwa-session-dropped-file-grants session))
            (niwa-view--insert-dropped-file-mentions (list path))
            (should (equal (format "@file:{%s}" path)
                           (niwa-view-composer-draft-text)))
            (let* ((input (niwa-view-composer-draft-text))
                   (binding (get-text-property
                             0 'niwa-mention-binding input)))
              (should (equal (list :kind 'file
                                   :token (format "@file:{%s}" path)
                                   :path path)
                             binding)))
            (should (equal (list path)
                           (niwa-session-dropped-file-grants session)))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (delete-directory dir t))))

(niwa-deftest niwa-view--dnd-handle-files ()
  ,test
  (test)
  :doc "a single-URI drop appends from the view and inserts at point in the composer"
  (let* ((dir (make-temp-file "niwa-dnd-" t))
         (path (expand-file-name "single.txt" dir))
         (uri (concat "file://" path))
         (data-buf (generate-new-buffer " *test-data-dnd-single*"))
         (view-buf (generate-new-buffer " *test-view-dnd-single*"))
         (ws (niwa-workspace--create :type 'project :id "dnd-single"
                                        :root dir :name "dnd-single"))
         (session (niwa-session-create "main" ws)))
    (with-temp-file path (insert "single\n"))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (niwa--transcript-org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local niwa--session session)
            (setq-local niwa--workspace ws))
          (niwa-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (goto-char (point-min))
            (let ((transcript (buffer-string))
                  (pos (point)))
              (niwa-view-composer-replace "look at")
              (should (eq 'copy (niwa-view--dnd-handle-files uri 'copy)))
              (should (equal (format "look at @file:%s" path)
                             (niwa-view-composer-draft-text nil t)))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-composer-replace "look here")
                (goto-char (+ 5 (niwa-view-composer-draft-start)))
                (should (eq 'copy
                            (niwa-view--dnd-handle-files uri 'copy)))
                (should (equal (format "look @file:%s here" path)
                               (niwa-view-composer-draft-text nil t))))
              (should (= pos (point)))
              (should (equal transcript (buffer-string))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (delete-directory dir t))))

(niwa-deftest niwa-view-yank-dwim ()
  ,test
  (test)
  :doc "saves clipboard images to workspace media and inserts @file"
  (let* ((dir (make-temp-file "niwa-clipboard-" t))
         (data-buf (generate-new-buffer " *test-data-clipboard*"))
         (view-buf (generate-new-buffer " *test-view-clipboard*"))
         (ws (niwa-workspace--create :type 'project :id "clipboard"
                                        :root dir :name "clipboard"))
         (session (niwa-session-create "main" ws))
         (expected (file-name-concat
                    dir ".niwa" "state" "media"
                    "clipboard-20260620-121314.png"))
         (niwa-view-clipboard-image-handlers
          `(((:command . "fake-clipboard")
             (:save . ,(lambda (file-path)
                         (with-temp-file file-path
                           (set-buffer-multibyte nil)
                           (insert "png"))))))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (niwa--transcript-org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local niwa--session session)
            (setq-local niwa--workspace ws))
          (niwa-view--setup view-buf data-buf)
          (with-current-buffer (niwa-view-composer-buffer view-buf)
            (goto-char (niwa-view-composer-draft-end view-buf))
            (cl-letf (((symbol-function 'window-system)
                       (lambda (&optional _frame) 'x))
                      ((symbol-function 'executable-find)
                       (lambda (command)
                         (and (equal command "fake-clipboard") command)))
                      ((symbol-function 'format-time-string)
                       (lambda (&rest _) "20260620-121314")))
              (niwa-view-yank-dwim)))
          (with-current-buffer view-buf
            (should (file-exists-p expected))
            (should (equal (format "@file:%s" expected)
                           (niwa-view-composer-draft-text)))
            (should (equal (list expected)
                           (niwa-session-dropped-file-grants session)))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (delete-directory dir t)))

  :doc "falls back to text yank when image saving has no session"
  (with-temp-buffer
    (let ((kill-ring '("plain text"))
          (kill-ring-yank-pointer nil)
          (niwa-view-clipboard-image-handlers
           '(((:command . "fake-clipboard") (:save . ignore)))))
      (cl-letf (((symbol-function 'window-system)
                 (lambda (&optional _frame) 'x))
                ((symbol-function 'executable-find)
                 (lambda (command)
                   (and (equal command "fake-clipboard") command))))
        (niwa-view-yank-dwim))
      (should (equal "plain text" (buffer-string))))))

(niwa-deftest niwa-view--install-dnd ()
  ,test
  (test)
  :doc "installs one local handler while preserving other protocols and defaults"
  (let* ((original '(("^https:" . browse-url)
                     ("^file:" . niwa-view--dnd-handle-files)
                     ("^ftp:" . browse-url)
                     ("^old-file:" . niwa-view--dnd-handle-files)))
         (dnd-protocol-alist (copy-tree original)))
    (with-temp-buffer
      (niwa-view--install-dnd)
      (niwa-view--install-dnd)
      (should (local-variable-p 'dnd-protocol-alist))
      (should (equal '(("^file:" . niwa-view--dnd-handle-files)
                       ("^https:" . browse-url)
                       ("^ftp:" . browse-url))
                     dnd-protocol-alist)))
    (should (equal original dnd-protocol-alist))))

(provide 'test-niwa-view-input-files)
;;; test-niwa-view-input-files.el ends here
