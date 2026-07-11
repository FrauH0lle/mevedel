;;; test-mevedel-view-markdown.el --- Markdown view tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-markdown)
(require 'mevedel-view-render)
(require 'mevedel-structs)
(require 'mevedel-workspace)

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
    (let ((button (button-at (match-beginning 0)))
          copied)
      (should button)
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &optional _replace)
                   (setq copied text))))
        (button-activate button))
      (should (equal "(+ 1 2)" copied)))
    (goto-char (point-min))
    (search-forward "(+ 1 2)")
    (should (get-text-property (match-beginning 0)
                               'mevedel-view-code-block-body)))

  :doc "leaves incomplete fenced blocks unrendered"
  (let ((text "before\n```elisp\n(+ 1 2)\n"))
    (with-temp-buffer
      (insert text)
      (mevedel-view--decorate-code-blocks-in-range (point-min) (point-max))
      (should (equal text (buffer-string)))))

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
      (mevedel-view--decorate-code-blocks-in-range (point-min) (point-max))
      (mevedel-view--prettify-markdown-tables-in-range
       (point-min) (point-max))
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
      (delete-file file))))

(mevedel-deftest mevedel-view--prettify-markdown-tables-in-range
  (:doc "`mevedel-view--prettify-markdown-tables-in-range' aligns pipe tables")
  ,test
  (test)
  :doc "pads cells and separator rows"
  (with-temp-buffer
    (insert "| Name | Role |\n")
    (insert "|------|------|\n")
    (insert "| Alice | Engineer |\n")
    (mevedel-view--prettify-markdown-tables-in-range
     (point-min) (point-max))
    (should (equal "| Name  | Role     |\n|-------|----------|\n| Alice | Engineer |\n"
                   (buffer-string))))

  :doc "uses visible Markdown text width for simple emphasis and links"
  (with-temp-buffer
    (insert "| Name | Role |\n")
    (insert "|------|------|\n")
    (insert "| **Alice** | [Engineer](http://x.com) |\n")
    (mevedel-view--prettify-markdown-tables-in-range
     (point-min) (point-max))
    (should (equal "| Name  | Role     |\n|-------|----------|\n| **Alice** | [Engineer](http://x.com) |\n"
                   (buffer-string))))

  :doc "keeps inline pipes and unmatched literal backticks from breaking table detection"
  (let ((tick (make-string 1 ?`)))
    (with-temp-buffer
      (insert "| Item | Description | Example |\n")
      (insert "|---|---|---|\n")
      (insert (concat "| Markdown table | Uses pipes and dashes | "
                      tick "| A | B |" tick " |\n"))
      (insert (concat "| Code block | Uses triple backticks | "
                      (make-string 3 ?`) "python |\n"))
      (insert (concat "| Inline code | Uses single backticks | "
                      tick "example" tick " |\n"))
      (add-text-properties (point-min) (point-max)
                           '(font-lock-face markdown-table-face))
      (save-excursion
        (goto-char (point-min))
        (search-forward "```python")
        (add-text-properties (match-beginning 0) (match-end 0)
                             '(font-lock-face markdown-code-face
                               invisible t
                               display "``python")))
      (mevedel-view--prettify-markdown-tables-in-range
       (point-min) (point-max))
      (should
       (equal
        (concat "| Item           | Description           | Example     |\n"
                "|----------------|-----------------------|-------------|\n"
                "| Markdown table | Uses pipes and dashes | `| A | B |` |\n"
                "| Code block     | Uses triple backticks | ```python   |\n"
                "| Inline code    | Uses single backticks | `example`   |\n")
        (buffer-string)))
      (goto-char (point-min))
      (search-forward "```python")
      (should-not
       (text-property-any (match-beginning 0) (match-end 0) 'invisible t))
      (should-not
       (text-property-not-all
        (match-beginning 0) (match-end 0)
        'font-lock-face 'markdown-table-face))))

  :doc "copies table face onto inserted padding"
  (with-temp-buffer
    (insert "| Name | Role |\n")
    (insert "|------|------|\n")
    (insert "| Ada | Developer |\n")
    (add-text-properties (point-min) (point-max)
                         '(font-lock-face markdown-table-face))
    (mevedel-view--prettify-markdown-tables-in-range
     (point-min) (point-max))
    (should-not
     (text-property-not-all
      (point-min) (point-max) 'font-lock-face 'markdown-table-face)))

  :doc "skips tables inside fenced code blocks"
  (let ((text "```md\n| A | B |\n|---|---|\n| x | yy |\n```\n"))
    (with-temp-buffer
      (insert text)
      (mevedel-view--prettify-markdown-tables-in-range
       (point-min) (point-max))
      (should (equal text (buffer-string)))))

  :doc "preserves caller point"
  (with-temp-buffer
    (insert "| Name | Role |\n")
    (insert "|------|------|\n")
    (insert "| Alice | Engineer |\n")
    (goto-char (point-max))
    (mevedel-view--prettify-markdown-tables-in-range
     (point-min) (point-max))
    (should (= (point) (point-max)))))


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


(provide 'test-mevedel-view-markdown)

;;; test-mevedel-view-markdown.el ends here
