;;; test-mevedel-view-history.el --- Tests for view input history -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-history)
(require 'mevedel-structs)
(require 'mevedel-mentions)
(require 'mevedel-session-persistence)


;;
;;; Test helpers

(defmacro mevedel-view-history-test--with-view (&rest body)
  "Run BODY with a minimal data/view buffer pair.
Binds `data-buf' and `view-buf'."
  (declare (indent 0) (debug t))
  `(let ((mevedel-session-persistence nil)
         (data-buf (generate-new-buffer " *test-data*"))
         (view-buf (generate-new-buffer " *test-view*")))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (org-mode)
             (setq-local gptel-response-separator "\n\n")
             (setq-local gptel-prompt-prefix-alist '((org-mode . "*** "))))
           (mevedel-view--setup view-buf data-buf)
           ,@body)
       (when (buffer-live-p view-buf) (kill-buffer view-buf))
       (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(defmacro mevedel-view-history-test--with-temp-dir (dir &rest body)
  "Bind DIR to a temporary directory while running BODY."
  (declare (indent 1) (debug t))
  `(let ((,dir (file-name-as-directory
                (make-temp-file "mevedel-view-history-" t))))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,dir)
         (delete-directory ,dir t)))))


;;
;;; Ring and commands

(mevedel-deftest mevedel-view-history--ring
  (:doc "adds newest-first entries, trims input, skips empties and duplicate heads")
  (let ((mevedel-view-input-history-size 3))
    (with-temp-buffer
      (mevedel-view-history-add "  first  ")
      (mevedel-view-history-add "first")
      (mevedel-view-history-add "")
      (mevedel-view-history-add "second")
      (mevedel-view-history-add "third")
      (mevedel-view-history-add "fourth")
      (should (equal '("fourth" "third" "second")
                     (mevedel-view-history--entries))))))

(mevedel-deftest mevedel-view-history--navigation
  (:doc "M-p / M-n navigate history and restore incomplete input")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "draft")
      (mevedel-view-history-add "first")
      (mevedel-view-history-add "second")
      (mevedel-view-history-previous)
      (should (equal "second" (mevedel-view-history--input-text)))
      (mevedel-view-history-previous)
      (should (equal "first" (mevedel-view-history--input-text)))
      (mevedel-view-history-next)
      (should (equal "second" (mevedel-view-history--input-text)))
      (mevedel-view-history-next)
      (should (equal "draft" (mevedel-view-history--input-text))))))

(mevedel-deftest mevedel-view-history--bol-clear-search-browse
  (:doc "C-a, C-c C-u, M-r, and C-c C-l operate on the input region")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "abc")
      (mevedel-view-history-beginning-of-line)
      (should (= (point) (mevedel-view--input-start)))
      (goto-char (point-max))
      (mevedel-view-history-clear-input)
      (should (equal "" (mevedel-view-history--input-text)))
      (mevedel-view-history-add "alpha")
      (mevedel-view-history-add "beta match")
      (cl-letf (((symbol-function 'read-regexp)
                 (lambda (&rest _) "match")))
        (mevedel-view-history-search))
      (should (equal "beta match" (mevedel-view-history--input-text)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "alpha")))
        (mevedel-view-history-browse))
      (should (equal "alpha" (mevedel-view-history--input-text))))))


;;
;;; Persistence

(mevedel-deftest mevedel-view-history--persistence
  (:doc "round-trips input-history.el using the session sidecar schema")
  (mevedel-view-history-test--with-temp-dir dir
    (let ((session (mevedel-session--create
                    :name "main"
                    :save-path dir))
          (mevedel-session-persistence t))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (mevedel-view-history-add "first")
        (mevedel-view-history-add "second")
        (mevedel-view-history-save (current-buffer)))
      (should (file-exists-p (file-name-concat dir "input-history.el")))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (mevedel-view-history-load session)
        (should (equal '("second" "first")
                       (mevedel-view-history--entries)))))))

(mevedel-deftest mevedel-view-history--persistence-corrupt
  (:doc "renames corrupt input-history.el to .bad and starts empty")
  (mevedel-view-history-test--with-temp-dir dir
    (let* ((path (file-name-concat dir "input-history.el"))
           (session (mevedel-session--create
                     :name "main"
                     :save-path dir))
           (mevedel-session-persistence t))
      (with-temp-file path
        (insert "not a plist"))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (mevedel-view-history-load session)
        (should (equal nil (mevedel-view-history--entries))))
      (should (file-exists-p (concat path ".bad"))))))

(mevedel-deftest mevedel-view-history--persistence-disabled
  (:doc "does not write input-history.el when persistence is disabled")
  (mevedel-view-history-test--with-temp-dir dir
    (let ((session (mevedel-session--create
                    :name "main"
                    :save-path dir))
          (mevedel-session-persistence nil))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (mevedel-view-history-add "first")
        (mevedel-view-history-save (current-buffer)))
      (should-not (file-exists-p
                   (file-name-concat dir "input-history.el"))))))

(mevedel-deftest mevedel-view-history--read-only-attach
  (:doc "does not write input-history.el when attached data buffer is read-only")
  (mevedel-view-history-test--with-temp-dir dir
    (let ((session (mevedel-session--create
                    :name "main"
                    :save-path dir))
          (data-buf (generate-new-buffer " *test-data*"))
          (view-buf (generate-new-buffer " *test-view*"))
          (mevedel-session-persistence t))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local mevedel-session--read-only-mode t))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (setq-local mevedel--data-buffer data-buf)
              (mevedel-view-history-add "first")
              (mevedel-view-history-save (current-buffer)))
            (should-not (file-exists-p
                         (file-name-concat dir "input-history.el"))))
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))))))

(mevedel-deftest mevedel-view-history--fork-copy
  (:doc "copies input-history.el from parent session directory to fork directory")
  (mevedel-view-history-test--with-temp-dir parent
    (mevedel-view-history-test--with-temp-dir child
      (let ((src (file-name-concat parent "input-history.el"))
            (dst (file-name-concat child "input-history.el")))
        (with-temp-file src
          (prin1 '(:version 1 :entries ("second" "first"))
                 (current-buffer)))
        (mevedel-view-history-copy-file parent child)
        (should (file-exists-p dst))
        (should (equal (with-temp-buffer
                         (insert-file-contents src)
                         (buffer-string))
                       (with-temp-buffer
                         (insert-file-contents dst)
                         (buffer-string))))))))

(provide 'test-mevedel-view-history)
;;; test-mevedel-view-history.el ends here
