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

(defun mevedel-view-history-test--workspace (dir)
  "Return a minimal workspace rooted at DIR."
  (mevedel-workspace--create
   :type 'project
   :id dir
   :root dir
   :name "test"
   :file-cache (mevedel-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil
                :total-bytes 0)))

(defun mevedel-view-history-test--session (dir)
  "Return a test session with workspace and save path DIR."
  (mevedel-session--create
   :name "main"
   :workspace (mevedel-view-history-test--workspace dir)
   :save-path dir))

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

(mevedel-deftest mevedel-view-history--outside-input-fallback
  (:doc "M-p / M-n outside the input region delegate to global bindings")
  (mevedel-view-history-test--with-view
    (let ((prev nil)
          (next nil)
          (old-prev (lookup-key global-map (kbd "M-p")))
          (old-next (lookup-key global-map (kbd "M-n"))))
      (unwind-protect
          (progn
            (define-key global-map (kbd "M-p")
                        (lambda () (interactive) (setq prev t)))
            (define-key global-map (kbd "M-n")
                        (lambda () (interactive) (setq next t)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (mevedel-view-history-previous)
              (mevedel-view-history-next))
            (should prev)
            (should next))
        (define-key global-map (kbd "M-p") old-prev)
        (define-key global-map (kbd "M-n") old-next)))))

(mevedel-deftest mevedel-view-history--restore-incomplete
  (:doc "M-n restores typed-but-unsent input after history browsing")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "draft")
      (mevedel-view-history-add "sent")
      (mevedel-view-history-previous)
      (should (equal "sent" (mevedel-view-history--input-text)))
      (mevedel-view-history-next)
      (should (equal "draft" (mevedel-view-history--input-text))))))

(mevedel-deftest mevedel-view-history--bol
  (:doc "C-a moves to input start on the prompt line")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "abc")
      (mevedel-view-history-beginning-of-line)
      (should (= (point) (mevedel-view--input-start))))))

(mevedel-deftest mevedel-view-history--clear-input
  (:doc "C-c C-u clears input without touching history")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "abc")
      (mevedel-view-history-clear-input)
      (should (equal "" (mevedel-view-history--input-text))))))

(mevedel-deftest mevedel-view-history--isearch
  (:doc "M-r incrementally searches, cycles matches, and restores on C-g")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "draft")
      (mevedel-view-history-add "match older")
      (mevedel-view-history-add "match newer")
      (let ((unread-command-events
             (append (list ?m ?a ?\M-r ?\r) unread-command-events)))
        (mevedel-view-history-search))
      (should (equal "match older" (mevedel-view-history--input-text)))
      (mevedel-view-history--replace-input "draft")
      (let ((unread-command-events
             (append (list ?m ?\C-g) unread-command-events)))
        (should (condition-case nil
                    (progn
                      (mevedel-view-history-search)
                      nil)
                  (quit t))))
      (should (equal "draft" (mevedel-view-history--input-text)))
      (let ((aborted nil)
            (unread-command-events
             (append (list ?m ?\C-c ?\C-k) unread-command-events)))
        (cl-letf (((symbol-function 'mevedel-view-abort)
                   (lambda () (interactive) (setq aborted t))))
          (mevedel-view-history-search))
        (should aborted))
      (should (equal "draft" (mevedel-view-history--input-text))))))

(mevedel-deftest mevedel-view-history--browse-picker
  (:doc "C-c C-l inserts a completing-read history choice")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (mevedel-view-history-add "alpha")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "alpha")))
        (mevedel-view-history-browse))
      (should (equal "alpha" (mevedel-view-history--input-text))))))

(mevedel-deftest mevedel-view-history--ret-dispatch
  (:doc "plain RET inserts input newline; display and descriptors keep RET actions")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (should-not (lookup-key mevedel-view-mode-map (kbd "RET")))
      (goto-char (point-min))
      (should (eq (lookup-key (get-text-property (point) 'keymap)
                              (kbd "RET"))
                  #'mevedel-view-open-agent-transcript-at-point))
      (let ((map (make-sparse-keymap))
            (activated nil))
        (define-key map (kbd "RET")
                    (lambda () (interactive) (setq activated t)))
        (mevedel-view--interaction-register
         (list :id "test-ret"
               :kind 'ask
               :body "Descriptor\n"
               :keymap map))
        (goto-char (mevedel-view--interaction-anchor))
        (call-interactively (key-binding (kbd "RET")))
        (should activated)))))


;;
;;; Persistence

(mevedel-deftest mevedel-view-history--persistence
  (:doc "round-trips input-history.el using the session sidecar schema")
  (mevedel-view-history-test--with-temp-dir dir
    (let ((session (mevedel-view-history-test--session dir))
          (mevedel-session-persistence t))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (mevedel-view-history-add "first")
        (mevedel-view-history-add "second")
        (mevedel-view-history-add "multi\nline \"quoted\"")
        (mevedel-view-history-add (concat "unicode " (string #x03bb)))
        (mevedel-view-history-save (current-buffer)))
      (should (file-exists-p (file-name-concat dir "input-history.el")))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (mevedel-view-history-load session)
        (should (equal (list (concat "unicode " (string #x03bb))
                             "multi\nline \"quoted\""
                             "second"
                             "first")
                       (mevedel-view-history--entries)))))))

(mevedel-deftest mevedel-view-history--persistence-corrupt
  (:doc "renames corrupt input-history.el to .bad and starts empty")
  (mevedel-view-history-test--with-temp-dir dir
    (let* ((path (file-name-concat dir "input-history.el"))
           (session (mevedel-view-history-test--session dir))
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
    (let ((session (mevedel-view-history-test--session dir))
          (mevedel-session-persistence nil))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (mevedel-view-history-add "first")
        (mevedel-view-history-save (current-buffer)))
      (should-not (file-exists-p
                   (file-name-concat dir "input-history.el"))))))

(mevedel-deftest mevedel-view-history--no-workspace
  (:doc "keeps history in memory only when no workspace is available")
  (mevedel-view-history-test--with-temp-dir dir
    (let ((session (mevedel-session--create
                    :name "main"
                    :save-path dir))
          (mevedel-session-persistence t))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (mevedel-view-history-add "first")
        (mevedel-view-history-save (current-buffer)))
      (should-not (file-exists-p
                   (file-name-concat dir "input-history.el"))))))

(mevedel-deftest mevedel-view-history--read-only-attach
  (:doc "does not write input-history.el when attached data buffer is read-only")
  (mevedel-view-history-test--with-temp-dir dir
    (let ((session (mevedel-view-history-test--session dir))
          (data-buf (generate-new-buffer " *test-data*"))
          (view-buf (generate-new-buffer " *test-view*"))
          (mevedel-session-persistence t))
      (unwind-protect
          (progn
            (with-temp-file (file-name-concat dir "input-history.el")
              (prin1 '(:version 1 :entries ("old")) (current-buffer)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (setq-local mevedel-session--read-only-mode t))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (setq-local mevedel--data-buffer data-buf)
              (mevedel-view-history-load session)
              (should (equal '("old") (mevedel-view-history--entries)))
              (mevedel-view-history-add "first")
              (mevedel-view-history-save (current-buffer)))
            (should (equal (with-temp-buffer
                             (insert-file-contents
                              (file-name-concat dir "input-history.el"))
                             (read (current-buffer)))
                           '(:version 1 :entries ("old")))))
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))))))

(mevedel-deftest mevedel-view-history--agent-transcript-no-save
  (:doc "agent transcript views never overwrite the parent input history")
  (mevedel-view-history-test--with-temp-dir dir
    (let ((session (mevedel-view-history-test--session dir))
          (data-buf (generate-new-buffer " *test-data*"))
          (view-buf (generate-new-buffer " *test-view*"))
          (path (file-name-concat dir "input-history.el"))
          (mevedel-session-persistence t))
      (unwind-protect
          (progn
            (with-temp-file path
              (prin1 '(:version 1 :entries ("parent")) (current-buffer)))
            (with-current-buffer data-buf
              (org-mode)
              (setq-local mevedel--session session)
              (setq-local gptel-response-separator "\n\n")
              (setq-local gptel-prompt-prefix-alist '((org-mode . "*** "))))
            (mevedel-view--setup
             view-buf data-buf
             (list :agent-transcript-p t
                   :agent-id "agent--1"
                   :transcript-info nil
                   :parent-view nil))
            (with-current-buffer view-buf
              (mevedel-view-history-add "transcript")
              (mevedel-view-history-save (current-buffer)))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (read (current-buffer)))
                           '(:version 1 :entries ("parent")))))
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))))))

(mevedel-deftest mevedel-view-history--rewind-keeps-ring
  (:doc "history ring is session-level state and survives view text rewrites")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (mevedel-view-history-add "before rewind")
      (let ((inhibit-read-only t))
        (delete-region (point-min) (mevedel-view--input-start))
        (goto-char (point-min))
        (insert "rewound transcript\n"))
      (should (equal '("before rewind")
                     (mevedel-view-history--entries))))))

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
