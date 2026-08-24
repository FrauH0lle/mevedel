;;; test-mevedel-view-markdown.el --- Markdown view tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-session-publication)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-execution-target)
(require 'mevedel-session-persistence)
(require 'mevedel-view)
(require 'mevedel-view-markdown)
(require 'mevedel-view-render)
(require 'mevedel-structs)
(require 'mevedel-workspace)

(mevedel-deftest mevedel-view--resolve-path
  (:doc "`mevedel-view--resolve-path' resolves paths in the session target")
  ,test
  (test)
  :doc "remote target-native absolute and relative paths are re-qualified"
  (let* ((root "/ssh:builder@example.test:/srv/project/")
         (workspace (mevedel-workspace--create
                     :type 'project :id "remote-link-root"
                     :root root :name "remote-link-root"))
         (session (mevedel-session-create "main" workspace)))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (should (equal "/ssh:builder@example.test:/srv/project/src/main.el"
                     (mevedel-view--resolve-path "/srv/project/src/main.el")))
      (should (equal "/ssh:builder@example.test:/srv/project/test/main.el"
                     (mevedel-view--resolve-path "test/main.el")))
      (should-not
       (mevedel-view--resolve-path
        "/ssh:other@example.test:/srv/project/src/main.el")))))

(mevedel-deftest mevedel-view--decorate-code-blocks-in-range
  (:doc "`mevedel-view--decorate-code-blocks-in-range' adds copy buttons to fenced blocks")
  ,test
  (test)
  (with-temp-buffer
    (insert "before\n```elisp\n(+ 1 2)\n```\nafter\n")
    (mevedel-view--decorate-code-blocks-in-range (point-min) (point-max))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (equal "before\nelisp ⧉\n\n(+ 1 2)\n\nafter\n" text))
      (should-not (string-match-p "```" text))
      (should (string-match-p "elisp ⧉" text))
      (should (string-match-p (regexp-quote "(+ 1 2)") text)))
    (goto-char (point-min))
    (search-forward "elisp ⧉")
    (let ((label-start (match-beginning 0))
          (button (button-at (match-beginning 0)))
          copied)
      (should button)
      (should-not (get-text-property label-start 'line-prefix))
      (should-not (get-text-property label-start 'wrap-prefix))
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &optional _replace)
                   (setq copied text))))
        (button-activate button))
      (should (equal "(+ 1 2)" copied)))
    (goto-char (point-min))
    (search-forward "(+ 1 2)")
    (let ((body-start (match-beginning 0)))
      (should (get-text-property body-start
                                 'mevedel-view-code-block-body))
      (should-not (get-text-property body-start 'line-prefix))
      (should-not (get-text-property body-start 'wrap-prefix))))

  :doc "leaves incomplete fenced blocks unrendered"
  (let ((text "before\n```elisp\n(+ 1 2)\n"))
    (with-temp-buffer
      (insert text)
      (mevedel-view--decorate-code-blocks-in-range (point-min) (point-max))
      (should (equal text (buffer-string)))))

  :doc "labels a tilde-fenced source panel"
  (with-temp-buffer
    (insert "~~~elisp\n(+ 1 2)\n~~~\n")
    (mevedel-view--decorate-code-blocks-in-range (point-min) (point-max))
    (should (equal "elisp ⧉\n\n(+ 1 2)\n\n"
                   (buffer-substring-no-properties
                    (point-min) (point-max)))))

  :doc "labels spaced language info for both fence delimiters"
  (dolist (case '(("``` elisp\n(+ 1 2)\n```\n" . "elisp ⧉")
                  ("~~~ python\nprint(1)\n~~~\n" . "python ⧉")))
    (with-temp-buffer
      (insert (car case))
      (mevedel-view--decorate-code-blocks-in-range (point-min) (point-max))
      (should (string-prefix-p
               (cdr case)
               (buffer-substring-no-properties (point-min) (point-max))))))

  :doc "empty rendered code block copies an empty string"
  (with-temp-buffer
    (insert "```text\n```\n```text\nnext\n```\n")
    (mevedel-view--decorate-code-blocks-in-range (point-min) (point-max))
    (should (string-prefix-p "text ⧉\n\n\ntext ⧉"
                             (buffer-substring-no-properties
                              (point-min) (point-max))))
    (goto-char (point-min))
    (search-forward "text ⧉")
    (let ((button (button-at (match-beginning 0)))
          copied)
      (should button)
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &optional _replace)
                   (setq copied text))))
        (button-activate button))
      (should (equal "" copied))))

  :doc "decorated code bodies stay hidden from later Markdown affordances"
  (let ((text "```md\n| A | B |\n|---|---|\n| x | yy |\n```\n"))
    (with-temp-buffer
      (insert text)
      (mevedel-view--decorate-markdown-in-range (point-min) (point-max))
      (let ((rendered (buffer-substring-no-properties
                       (point-min) (point-max))))
        (should-not (string-match-p "```" rendered))
        (should (string-match-p "| A | B |" rendered))))))

(mevedel-deftest mevedel-view--decorate-markdown-url-links-in-range
  (:doc "`mevedel-view--decorate-markdown-in-range' renders Markdown links")
  ,test
  (test)
  (with-temp-buffer
    (insert "[Engineer](http://x.com)\n")
    (add-text-properties (point-min) (point-max)
                         '(keymap stale-map
                           follow-link t
                           help-echo "stale markdown link"))
    (mevedel-view--decorate-markdown-in-range (point-min) (point-max))
    (should (equal "Engineer\n" (buffer-string)))
    (goto-char (point-min))
    (search-forward "Engineer")
    (let ((button (button-at (match-beginning 0))))
      (should button)
      (should (equal "http://x.com"
                     (button-get button 'mevedel-view-url))))))

(mevedel-deftest mevedel-view--decorate-local-images-in-range
  (:doc "`mevedel-view--decorate-local-images-in-range' displays local image references")
  ,test
  (test)
  :doc "renders Markdown image links"
  (let ((file (make-temp-file "mevedel-image-link-" nil ".png")))
    (unwind-protect
        (with-temp-buffer
          (insert (format "![shot](%s)\n" file))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (path &rest _)
                       (list 'image :file path))))
            (mevedel-view--decorate-local-images-in-range
             (point-min) (point-max)))
          (goto-char (point-min))
          (search-forward "![shot]")
          (let ((display (get-text-property (match-beginning 0) 'display)))
            (should (equal (list 'image :file file) display))))
      (delete-file file)))

  :doc "renders bare local image paths"
  (let ((file (make-temp-file "mevedel-image-bare-" nil ".png")))
    (unwind-protect
        (with-temp-buffer
          (insert (format "Image: %s\n" file))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (path &rest _)
                       (list 'image :file path))))
            (mevedel-view--decorate-local-images-in-range
             (point-min) (point-max)))
          (goto-char (point-min))
          (search-forward file)
          (let ((display (get-text-property (match-beginning 0) 'display)))
            (should (equal (list 'image :file file) display))))
      (delete-file file)))

  :doc "renders remote session images from verified publication bytes"
  (let* ((host "view-image-artifact")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-view-image-artifact-" t)))
         (remote-root (format "/mevedelmock:%s:%s" host local-root))
         (save-path (concat remote-root "session/"))
         (logical "tool-results/image.png")
         (fixed (file-name-concat save-path logical))
         (native-fixed (file-name-concat local-root "session" logical))
         (published (file-name-concat
                     save-path ".publications" "generation" "000001.data"))
         (staged (file-name-concat local-root "staged-image"))
         (content "verified image bytes")
         (target (mevedel-execution-target-create remote-root))
         (session (mevedel-session--create
                   :authority-mode 'portable
                   :name "remote-view" :execution-target target
                   :save-path save-path))
         created)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (make-directory (file-name-directory fixed) t)
          (write-region "poisoned fixed cache" nil fixed nil 'silent)
          (make-directory (file-name-directory published) t)
          (write-region content nil published nil 'silent)
          (write-region "uncommitted image bytes" nil staged nil 'silent)
          (setf (mevedel-session-publication session)
                (list :head ".publications/generation/manifest.el"
                      :sidecar nil
                      :artifacts
                      (list (list logical :published published
                                  :sha256 (secure-hash 'sha256 content))))
                (mevedel-session-publication-uncommitted-batches session)
                (list (list :directory local-root
                            :artifacts
                            (list (list :logical logical :source staged)))))
          (cl-letf (((symbol-function
                      'mevedel-session-durability-lease-owned-p)
                     (lambda (_session) t))
                    ((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (source &optional _type data-p &rest _)
                       (setq created (list source data-p))
                       'image)))
            (with-temp-buffer
              (setq-local mevedel--session session)
              (insert (format "![shot](%s)\n" native-fixed))
              (mevedel-view--decorate-local-images-in-range
               (point-min) (point-max))
              (should (equal (list content t) created)))
            (setf (mevedel-session-publication session)
                  (list :head ".publications/generation/manifest.el"
                        :sidecar nil :artifacts nil))
            (setq created nil)
            (with-temp-buffer
              (setq-local mevedel--session session)
              (insert (format "![shot](%s)\n" native-fixed))
              (mevedel-view--decorate-local-images-in-range
               (point-min) (point-max))
              (should-not created))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-view--decorate-markdown-in-range
  (:doc "`mevedel-view--decorate-markdown-in-range' renders pipe tables")
  ,test
  (test)
  :doc "renders pipe tables and preserves view-owned properties"
  (let ((text "| Name | Role |\n|---|---|\n| Ada | Developer |\n"))
    (with-temp-buffer
      (insert text)
      (add-text-properties (point-min) (point-max)
                           '(mevedel-view-source task-table))
      (mevedel-view--decorate-markdown-in-range (point-min) (point-max))
      (let ((rendered (buffer-substring-no-properties
                       (point-min) (point-max))))
        (should (string-match-p "│ Name │ Role      │" rendered))
        (should-not (string-match-p "^| Name" rendered)))
      (should (equal (string-trim-right text "\n")
                     (substring-no-properties
                      (get-text-property (point-min)
                                         'mevedel-view-table-source))))
      (should-not
       (text-property-not-all
        (point-min) (point-max) 'mevedel-view-source 'task-table))))

  :doc "pipe rows inside fenced code blocks stay raw"
  (dolist (text '("```\n| a | b |\n| c | d |\n```\n"
                  "````\n| a | b |\n| c | d |\n````\n"
                  "~~~\n| a | b |\n| c | d |\n~~~\n"))
    (with-temp-buffer
      (insert text)
      (mevedel-view--decorate-markdown-in-range (point-min) (point-max))
      (let ((rendered (buffer-substring-no-properties
                       (point-min) (point-max))))
        (should (string-match-p "| a | b |" rendered))
        (should-not (string-match-p "│" rendered)))))

  :doc "pipe rows inside an incomplete streaming fence stay raw"
  (dolist (text '("```\n| a | b |\n| c | d |\n"
                  "````\n| a | b |\n| c | d |\n"
                  "~~~\n| a | b |\n| c | d |\n"))
    (with-temp-buffer
      (insert text)
      (mevedel-view--decorate-markdown-in-range (point-min) (point-max))
      (let ((rendered (buffer-substring-no-properties
                       (point-min) (point-max))))
        (should (string-match-p "| a | b |" rendered))
        (should-not (string-match-p "│" rendered))))))


(mevedel-deftest mevedel-view--linkify-paths-in-range ()
  ,test
  (test)
  :doc "slashless root filename is buttonized when it exists"
  (let* ((root (make-temp-file "mevedel-view-linkify-" t))
         (file (file-name-concat root "mevedel-skills.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-root"
                     :root root :name "linkify-root"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "Read: mevedel-skills.el\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "mevedel-skills.el")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path))))))
      (delete-directory root t)))

  :doc "linkify-exempt text stays plain even when the path exists"
  (let* ((root (make-temp-file "mevedel-view-linkify-exempt-" t))
         (file (file-name-concat root "mevedel-skills.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-exempt"
                     :root root :name "linkify-exempt"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert (propertize "+;;; mevedel-skills.el --- functions\n"
                                'mevedel-view-no-linkify t))
            (insert "Read: mevedel-skills.el\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "mevedel-skills.el")
            (should-not (button-at (match-beginning 0)))
            (search-forward "mevedel-skills.el")
            (should (button-at (match-beginning 0)))))
      (delete-directory root t)))

  :doc "missing slashless filename stays plain text"
  (let* ((root (make-temp-file "mevedel-view-linkify-missing-" t))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-missing"
                     :root root :name "linkify-missing"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (insert "Read: missing-file.el\n")
          (mevedel-view--linkify-paths-in-range (point-min) (point-max))
          (goto-char (point-min))
          (search-forward "missing-file.el")
          (should-not (button-at (match-beginning 0))))
      (delete-directory root t)))

  :doc "remote session artifact opens committed bytes through its logical path"
  (let* ((host "view-artifact")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-view-artifact-" t)))
         (remote-root (format "/mevedelmock:%s:%s" host local-root))
         (save-path (concat remote-root "session/"))
         (logical "tool-results/result.txt")
         (fixed (file-name-concat save-path logical))
         (native-fixed (file-name-concat local-root "session" logical))
         (published (file-name-concat
                     save-path ".publications" "generation" "000001.data"))
         (content "committed\nsecond\nthird\n")
         (target (mevedel-execution-target-create remote-root))
         (session (mevedel-session--create
                   :authority-mode 'portable
                   :name "remote-view" :execution-target target
                   :save-path save-path))
         opened)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (make-directory (file-name-directory fixed) t)
          (write-region "poisoned fixed cache" nil fixed nil 'silent)
          (make-directory (file-name-directory published) t)
          (write-region content nil published nil 'silent)
          (setf (mevedel-session-publication session)
                (list :head ".publications/generation/manifest.el"
                      :sidecar nil
                      :artifacts
                      (list (list logical :published published
                                  :sha256 (secure-hash 'sha256 content)))))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "Read " native-fixed ":2\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward (concat native-fixed ":2"))
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal logical
                             (button-get
                              button 'mevedel-view-session-artifact)))
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _)
                           (setq opened buffer)
                           buffer)))
                (button-activate button))))
          (should (buffer-live-p opened))
          (with-current-buffer opened
            (should (equal buffer-file-name fixed))
            (should (equal content (buffer-string)))
            (should (= 2 (line-number-at-pos)))
            (should (looking-at "second"))
            (should buffer-read-only)))
      (when (buffer-live-p opened)
        (with-current-buffer opened
          (setq buffer-read-only nil)
          (set-buffer-modified-p nil))
        (kill-buffer opened))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "unpublished remote session cache stays plain text"
  (let* ((host "view-unpublished-artifact")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-view-unpublished-" t)))
         (remote-root (format "/mevedelmock:%s:%s" host local-root))
         (save-path (concat remote-root "session/"))
         (logical "tool-results/result.txt")
         (fixed (file-name-concat save-path logical))
         (native-fixed (file-name-concat local-root "session" logical))
         (target (mevedel-execution-target-create remote-root))
         (session (mevedel-session--create
                   :authority-mode 'portable
                   :name "remote-view" :execution-target target
                   :save-path save-path)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (make-directory (file-name-directory fixed) t)
          (write-region "unpublished fixed cache" nil fixed nil 'silent)
          (setf (mevedel-session-publication session)
                (list :head ".publications/generation/manifest.el"
                      :sidecar nil :artifacts nil))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "Read " native-fixed "\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward native-fixed)
            (should-not (button-at (match-beginning 0)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "slash-containing relative path is still buttonized"
  (let* ((root (make-temp-file "mevedel-view-linkify-rel-" t))
         (file (file-name-concat root "test/test-mevedel-skills.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-rel"
                     :root root :name "linkify-rel"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert "subdir\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "Edit: test/test-mevedel-skills.el\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "test/test-mevedel-skills.el")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path))))))
      (delete-directory root t)))

  :doc "URL-like text is not buttonized"
  (let* ((root (make-temp-file "mevedel-view-linkify-url-" t))
         (file (file-name-concat root "example.com"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-url"
                     :root root :name "linkify-url"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "not a link target here\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See https://example.com\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "example.com")
            (should-not (button-at (match-beginning 0)))))
      (delete-directory root t)))

  :doc "relative file line reference stores path and line"
  (let* ((root (make-temp-file "mevedel-view-linkify-line-" t))
         (file (file-name-concat root "mevedel-session-persistence.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-line"
                     :root root :name "linkify-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See mevedel-session-persistence.el:187\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "mevedel-session-persistence.el:187")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 187 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "relative file line range stores first line and spans full range"
  (let* ((root (make-temp-file "mevedel-view-linkify-range-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-range"
                     :root root :name "linkify-range"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See file.el:100-102\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:100-102")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 100 (button-get button 'mevedel-view-line)))
              (let ((end-button (button-at (1- (match-end 0)))))
                (should end-button)
                (should (equal file
                               (button-get end-button 'mevedel-view-path)))
                (should (= 100
                           (button-get end-button 'mevedel-view-line)))))))
      (delete-directory root t)))

  :doc "relative file L-prefixed line range stores first line"
  (let* ((root (make-temp-file "mevedel-view-linkify-l-range-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-l-range"
                     :root root :name "linkify-l-range"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See file.el:L1400-L1422\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:L1400-L1422")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 1400 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "colon line list creates separate buttons"
  (let* ((root (make-temp-file "mevedel-view-linkify-list-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-list"
                     :root root :name "linkify-list"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See file.el:L24,L120-L143\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:L24")
            (let ((first (button-at (match-beginning 0))))
              (should first)
              (should (= 24 (button-get first 'mevedel-view-line))))
            (search-forward ",")
            (should-not (button-at (1- (point))))
            (search-forward "L120-L143")
            (let ((second (button-at (match-beginning 0))))
              (should second)
              (should (equal file
                             (button-get second 'mevedel-view-path)))
              (should (= 120 (button-get second 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "hash line list creates separate buttons"
  (let* ((root (make-temp-file "mevedel-view-linkify-hash-list-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-hash-list"
                     :root root :name "linkify-hash-list"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See file.el:#L24,#L120-#L143\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:#L24")
            (let ((first (button-at (match-beginning 0))))
              (should first)
              (should (= 24 (button-get first 'mevedel-view-line))))
            (search-forward ",")
            (should-not (button-at (1- (point))))
            (search-forward "#L120-#L143")
            (let ((second (button-at (match-beginning 0))))
              (should second)
              (should (equal file
                             (button-get second 'mevedel-view-path)))
              (should (= 120 (button-get second 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "direct #L fragment stores first line"
  (let* ((root (make-temp-file "mevedel-view-linkify-hash-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-hash"
                     :root root :name "linkify-hash"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See file.el#L24-L30\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el#L24-L30")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 24 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "@file mention with line reference stores path and line"
  (let* ((root (make-temp-file "mevedel-view-linkify-file-mention-" t))
         (file (file-name-concat root "with space.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-file-mention"
                     :root root :name "linkify-file-mention"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert (format "See @file:{%s}#L7\n" file))
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "@file:")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 7 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "Markdown local link #L range stores first line"
  (let* ((root (make-temp-file "mevedel-view-linkify-md-range-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-md-range"
                     :root root :name "linkify-md-range"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "[spot](file.el#L24-L30)\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "spot")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 24 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "Markdown local link colon line suffix stores first line"
  (let* ((root (make-temp-file "mevedel-view-linkify-md-colon-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-md-colon"
                     :root root :name "linkify-md-colon"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "[plain](file.el:24)\n[prefixed](file.el:L25-L30)\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "plain")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 24 (button-get button 'mevedel-view-line))))
            (search-forward "prefixed")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 25 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "nested relative file line reference resolves from workspace root"
  (let* ((root (make-temp-file "mevedel-view-linkify-nested-line-" t))
         (file (file-name-concat root "test/test-mevedel-agent-exec.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-nested-line"
                     :root root :name "linkify-nested-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert "nested\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See test/test-mevedel-agent-exec.el:803\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "test/test-mevedel-agent-exec.el:803")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 803 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "absolute file line reference stores path and line"
  (let ((file (make-temp-file "mevedel-view-linkify-abs-" nil ".el")))
    (unwind-protect
        (with-temp-buffer
          (insert "See " file ":42\n")
          (mevedel-view--linkify-paths-in-range (point-min) (point-max))
          (goto-char (point-min))
          (search-forward (concat file ":42"))
          (let ((button (button-at (match-beginning 0))))
            (should button)
            (should (equal file
                           (button-get button 'mevedel-view-path)))
            (should (= 42 (button-get button 'mevedel-view-line)))))
      (delete-file file)))

  :doc "activating file line reference jumps to the requested line"
  (let ((file (make-temp-file "mevedel-view-linkify-action-" nil ".el"))
        opened)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "one\ntwo\nthree\nfour\n"))
          (with-temp-buffer
            (insert "See " file ":3\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward (concat file ":3"))
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (cl-letf (((symbol-function 'find-file-other-window)
                         (lambda (path)
                           (setq opened (find-file-noselect path)))))
                (button-activate button))))
          (should (buffer-live-p opened))
          (with-current-buffer opened
            (should (= 3 (line-number-at-pos)))
            (should (looking-at "three"))))
      (when (buffer-live-p opened)
        (kill-buffer opened))
      (delete-file file)))

  :doc "missing file line reference stays plain text"
  (let* ((root (make-temp-file "mevedel-view-linkify-missing-line-" t))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-missing-line"
                     :root root :name "linkify-missing-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (insert "See missing-file.el:10\n")
          (mevedel-view--linkify-paths-in-range (point-min) (point-max))
          (goto-char (point-min))
          (search-forward "missing-file.el:10")
          (should-not (button-at (match-beginning 0))))
      (delete-directory root t)))

  :doc "URL-like text with port is not buttonized"
  (with-temp-buffer
    (insert "See https://example.com:443\n")
    (mevedel-view--linkify-paths-in-range (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "example.com:443")
    (should-not (button-at (match-beginning 0))))

  :doc "trailing punctuation is not part of a line reference button"
  (let* ((root (make-temp-file "mevedel-view-linkify-punct-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-punct"
                     :root root :name "linkify-punct"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "punct\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See file.el:42.\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:42")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (= 42 (button-get button 'mevedel-view-line)))
              (should-not (button-at (point))))))
      (delete-directory root t))))


(mevedel-deftest mevedel-view--buffer-substring-filter ()
  ,test
  (test)
  :doc "a region overlapping a rendered table yields canonical Markdown"
  (with-temp-buffer
    (require 'mevedel-view-table)
    (let ((source "| a | b |\n|---|---|\n| 1 | 2 |"))
      (insert "before\n" source "\nafter\n")
      (mevedel-view-table-decorate (point-min) (point-max) nil)
      (should (equal (concat "before\n" source "\nafter\n")
                     (substring-no-properties
                      (mevedel-view--buffer-substring-filter
                       (point-min) (point-max)))))))

  :doc "a partial table region yields the complete table source"
  (with-temp-buffer
    (require 'mevedel-view-table)
    (let ((source "| a | b |\n|---|---|\n| 1 | 2 |"))
      (insert source "\n")
      (mevedel-view-table-decorate (point-min) (point-max) nil)
      (goto-char (point-min))
      (search-forward "│ 1 │")
      (should (equal source
                     (substring-no-properties
                      (mevedel-view--buffer-substring-filter
                       (match-beginning 0) (match-end 0)))))))

  :doc "table links keep canonical source and activation across rerenders"
  (mevedel-test--with-displayed-buffer
    (require 'mevedel-view-table)
    (let ((source (concat "| Person | Site |\n"
                          "|---|---|\n"
                          "| Ada | [Home](https://example.test) |")))
      (insert source "\n")
      (mevedel-view--decorate-markdown-in-range (point-min) (point-max))
      (dotimes (pass 2)
        (goto-char (point-min))
        (search-forward "Home")
        (should (equal "https://example.test"
                       (button-get (button-at (match-beginning 0))
                                   'mevedel-view-url)))
        (should (equal source
                       (substring-no-properties
                        (get-text-property
                         (point-min) 'mevedel-view-table-source))))
        (should (equal (concat source "\n")
                       (substring-no-properties
                        (mevedel-view--buffer-substring-filter
                         (point-min) (point-max)))))
        (when (zerop pass)
          (put-text-property
           (point-min)
           (next-single-property-change
            (point-min) 'mevedel-view-table-source nil (point-max))
           'mevedel-view-table-width nil)
          (mevedel-view-table-rerender)))))

  :doc "regions without tables copy verbatim and honor deletion"
  (with-temp-buffer
    (insert "plain text\n")
    (should (equal "plain"
                   (substring-no-properties
                    (mevedel-view--buffer-substring-filter 1 6))))
    (should (equal "plain"
                   (substring-no-properties
                    (mevedel-view--buffer-substring-filter 1 6 t))))
    (should (equal " text\n" (buffer-string)))))

  :doc "regions without tables retain the stock substring filter"
  (with-temp-buffer
    (insert "plain text\n")
    (cl-letf (((symbol-function 'buffer-substring--filter)
               (lambda (_beg _end _delete) "stock")))
      (should (equal "stock"
                     (mevedel-view--buffer-substring-filter 1 6)))))

(mevedel-deftest mevedel-view--last-live-response-boundary ()
  ,test
  (test)
  :doc "uses blank lines outside fenced code as stable response boundaries"
  (with-temp-buffer
    (insert "First.\n\n```elisp\n(code)\n\nstill code\n```\n\nTail")
    (let ((boundary (mevedel-view--last-live-response-boundary
                     (current-buffer) (point-min) (point-max))))
      (should (equal "Tail" (buffer-substring-no-properties
                             boundary (point-max))))))

  :doc "keeps an incomplete fenced block in the mutable tail"
  (with-temp-buffer
    (insert "First.\n\n```elisp\n(code)\n\nstill code")
    (let ((boundary (mevedel-view--last-live-response-boundary
                     (current-buffer) (point-min) (point-max))))
      (should (string-prefix-p
               "```elisp" (buffer-substring-no-properties
                            boundary (point-max))))))

  :doc "does not close a long fence with a shorter delimiter plus text"
  (with-temp-buffer
    (insert
     "First.\n\n````elisp\n```not a close\n\nstill code\n````\n\nTail")
    (let ((boundary (mevedel-view--last-live-response-boundary
                     (current-buffer) (point-min) (point-max))))
      (should (equal "Tail" (buffer-substring-no-properties
                             boundary (point-max))))))

  :doc "does not treat a four-space-indented delimiter as a closing fence"
  (with-temp-buffer
    (insert
     "First.\n\n```elisp\ncode\n    ```\n\nstill code\n```\n\nTail")
    (let ((boundary (mevedel-view--last-live-response-boundary
                     (current-buffer) (point-min) (point-max))))
      (should (equal "Tail" (buffer-substring-no-properties
                             boundary (point-max)))))))


(provide 'test-mevedel-view-markdown)

;;; test-mevedel-view-markdown.el ends here
