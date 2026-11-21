;;; test-niwa-view-markdown.el --- Markdown view tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-session-publication)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-execution-target)
(require 'niwa-mentions)
(require 'niwa-collaboration)
(require 'niwa-resource)
(require 'niwa-session-persistence)
(require 'niwa-view)
(require 'niwa-view-agent)
(require 'niwa-view-markdown)
(require 'niwa-view-render)
(require 'niwa-structs)
(require 'niwa-workspace)

(niwa-deftest niwa-view-markdown/cold-compile ()
  ,test
  (test)
  :doc "compiles alone without preloaded session structs and uses publication data"
  (let* ((root (file-name-directory (locate-library "niwa-view-markdown")))
         (directory (make-temp-file "niwa-markdown-compile-" t))
         (source (file-name-concat directory "niwa-view-markdown.el")))
    (unwind-protect
        (progn
          (copy-file (file-name-concat root "niwa-view-markdown.el") source)
          (with-temp-buffer
            (let ((status
                   (call-process
                    (expand-file-name invocation-name invocation-directory)
                    nil t nil "--batch" "-Q" "--eval"
                    (prin1-to-string
                     `(progn
                        (setq load-path ',(cons directory load-path))
                        (require 'bytecomp)
                        (when (featurep 'niwa-structs)
                          (error "Session structs must start unloaded"))
                        (let ((byte-compile-error-on-warn t)
                              (byte-compile-verbose nil))
                          (unless (byte-compile-file ,source)
                            (error "Standalone Markdown compilation failed")))
                        (load ,(concat source "c") nil t)
                        (require 'niwa-structs)
                        (let ((session (niwa-session--create
                                        :publication
                                        '(:artifacts (("known.txt" :size 1))))))
                          (unless (and (niwa-view--artifact-present-p session "known.txt")
                                       (not (niwa-view--artifact-present-p session "absent.txt")))
                            (error "Compiled publication lookup failed"))))))))
              (ert-info ((buffer-string))
                (should (equal 0 status))
                (should (equal "" (buffer-string)))))))
      (delete-directory directory t))))

(niwa-deftest niwa-view--resolve-path
  (:doc "`niwa-view--resolve-path' resolves paths in the session target")
  ,test
  (test)
  :doc "remote target-native absolute and relative paths are re-qualified"
  (let* ((root "/ssh:builder@example.test:/srv/project/")
         (workspace (niwa-workspace--create
                     :type 'project :id "remote-link-root"
                     :root root :name "remote-link-root"))
         (session (niwa-session-create "main" workspace)))
    (with-temp-buffer
      (setq-local niwa--session session)
      (should (equal "/ssh:builder@example.test:/srv/project/src/main.el"
                     (niwa-view--resolve-path "/srv/project/src/main.el")))
      (should (equal "/ssh:builder@example.test:/srv/project/test/main.el"
                     (niwa-view--resolve-path "test/main.el")))
      (should-not
       (niwa-view--resolve-path
        "/ssh:other@example.test:/srv/project/src/main.el")))))

(niwa-deftest niwa-view--decorate-code-blocks-in-range
  (:doc "`niwa-view--decorate-code-blocks-in-range' adds copy buttons to fenced blocks"
   :quiet t)
  ,test
  (test)
  (with-temp-buffer
    (insert "before\n```elisp\n(+ 1 2)\n```\nafter\n")
    (niwa-view--decorate-code-blocks-in-range (point-min) (point-max))
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
                                 'niwa-view-code-block-body))
      (should-not (get-text-property body-start 'line-prefix))
      (should-not (get-text-property body-start 'wrap-prefix))))

  :doc "leaves incomplete fenced blocks unrendered"
  (let ((text "before\n```elisp\n(+ 1 2)\n"))
    (with-temp-buffer
      (insert text)
      (niwa-view--decorate-code-blocks-in-range (point-min) (point-max))
      (should (equal text (buffer-string)))))

  :doc "labels a tilde-fenced source panel"
  (with-temp-buffer
    (insert "~~~elisp\n(+ 1 2)\n~~~\n")
    (niwa-view--decorate-code-blocks-in-range (point-min) (point-max))
    (should (equal "elisp ⧉\n\n(+ 1 2)\n\n"
                   (buffer-substring-no-properties
                    (point-min) (point-max)))))

  :doc "labels spaced language info for both fence delimiters"
  (dolist (case '(("``` elisp\n(+ 1 2)\n```\n" . "elisp ⧉")
                  ("~~~ python\nprint(1)\n~~~\n" . "python ⧉")))
    (with-temp-buffer
      (insert (car case))
      (niwa-view--decorate-code-blocks-in-range (point-min) (point-max))
      (should (string-prefix-p
               (cdr case)
               (buffer-substring-no-properties (point-min) (point-max))))))

  :doc "copy buttons survive position shifts before their bodies"
  (with-temp-buffer
    (insert "```text\nfirst body\n```\n"
            "middle prose\n"
            "```text\nskill://local/review\nskill://global/review\n```\n"
            "after\n")
    (niwa-view--decorate-code-blocks-in-range (point-min) (point-max))
    (let (copied)
      (goto-char (point-min))
      (search-forward "text ⧉")
      (search-forward "text ⧉")
      (let ((button (button-at (match-beginning 0))))
        (goto-char (button-end button))
        (insert "extra header\n")
        (goto-char (car (button-get button 'niwa-view-code-block-range)))
        (insert "inserted body\n"))
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &optional _replace)
                   (push text copied))))
        (goto-char (point-min))
        (while (search-forward "text ⧉" nil t)
          (button-activate (button-at (match-beginning 0)))))
      (should (equal '("first body"
                       "inserted body\nskill://local/review\nskill://global/review")
                     (nreverse copied)))))

  :doc "empty rendered code block copies an empty string"
  (with-temp-buffer
    (insert "```text\n```\n```text\nnext\n```\n")
    (niwa-view--decorate-code-blocks-in-range (point-min) (point-max))
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
      (niwa-view--decorate-markdown-in-range (point-min) (point-max))
      (let ((rendered (buffer-substring-no-properties
                       (point-min) (point-max))))
        (should-not (string-match-p "```" rendered))
        (should (string-match-p "| A | B |" rendered))))))

(niwa-deftest niwa-view--decorate-markdown-url-links-in-range
  (:doc "`niwa-view--decorate-markdown-in-range' renders Markdown links")
  ,test
  (test)
  :doc "a web link becomes a button visiting its URL"
  (with-temp-buffer
    (insert "[Engineer](http://x.com)\n")
    (add-text-properties (point-min) (point-max)
                         '(keymap stale-map
                           follow-link t
                           help-echo "stale markdown link"))
    (niwa-view--decorate-markdown-in-range (point-min) (point-max))
    (should (equal "Engineer\n" (buffer-string)))
    (goto-char (point-min))
    (search-forward "Engineer")
    (let ((button (button-at (match-beginning 0))))
      (should button)
      (should (equal "http://x.com"
                     (button-get button 'niwa-view-url)))))
  :doc "a shared:// item link opens the item in the session's room"
  (with-temp-buffer
    (insert "[Plan](shared://ab12)\n")
    (setq-local niwa--data-buffer (current-buffer))
    (niwa-view--decorate-markdown-in-range (point-min) (point-max))
    (should (equal "Plan\n" (buffer-string)))
    (let (opened)
      (cl-letf (((symbol-function 'niwa-collaboration-open-shared-item)
                 (lambda (buffer id) (setq opened (list buffer id)))))
        (button-activate (button-at (point-min))))
      (should (equal (list (current-buffer) "ab12") opened))))
  :doc "resource clicks decode the same identity as tool addresses"
  (dolist (entry '(("agent://root/%72eview#/result" . "/root/review")
                   ("history://root/%72eview/" . "/root/review")
                   ("shared://%61b12/view.png" . "ab12")
                   ;; Browser-made items are UUIDs.
                   ("shared://4f6e1c2a-77b0-4c1e-9d61-0a6b5e2f9c11" .
                    "4f6e1c2a-77b0-4c1e-9d61-0a6b5e2f9c11")))
    (with-temp-buffer
      (insert (format "[Open](%s)" (car entry)))
      (niwa-view--decorate-markdown-in-range (point-min) (point-max))
      (let (opened)
        (cl-letf (((symbol-function 'niwa-view-open-agent-transcript)
                   (lambda (path) (setq opened path)))
                  ((symbol-function 'niwa-collaboration-open-shared-item)
                   (lambda (_buffer id) (setq opened id))))
          (button-activate (button-at (point-min))))
        (should (equal (cdr entry) opened)))))
  :doc "malformed logical resource links fail before opening a destination"
  (dolist (address '("agent://root/review/../other"
                     "agent://root/review#/bad~2escape"
                     "shared://ab12/../other"))
    (with-temp-buffer
      (insert (format "[Open](%s)" address))
      (niwa-view--decorate-markdown-in-range (point-min) (point-max))
      (let (opened)
        (cl-letf (((symbol-function 'niwa-view-open-agent-transcript)
                   (lambda (&rest _) (setq opened t)))
                  ((symbol-function 'niwa-collaboration-open-shared-item)
                   (lambda (&rest _) (setq opened t))))
          (should-error (button-activate (button-at (point-min)))
                        :type 'user-error))
        (should-not opened))))
  :doc "bare resource addresses open their file, agent or item on click"
  (let ((file (make-temp-file "niwa-resource-link-")))
    (unwind-protect
        (with-temp-buffer
          (insert "See `work://notes.md`, agent://root/review#/result and shared://ab12/view.png.\n"
                  "```\nwork://code.md\n```\n")
          (setq-local niwa--data-buffer (current-buffer))
          (niwa-view--decorate-markdown-in-range (point-min) (point-max))
          (let (visited agent item)
            (cl-letf (((symbol-function 'niwa-resource-visit-path)
                       (lambda (address &rest _) (and (equal address "work://notes.md") file)))
                      ((symbol-function 'find-file-other-window) (lambda (path) (setq visited path)))
                      ((symbol-function 'niwa-view-open-agent-transcript)
                       (lambda (path) (setq agent path)))
                      ((symbol-function 'niwa-collaboration-open-shared-item)
                       (lambda (_ id) (setq item id))))
              (dolist (address '("work://notes.md" "agent://root/review#/result" "shared://ab12/view.png"))
                (goto-char (point-min))
                (search-forward address)
                (let ((button (button-at (match-beginning 0))))
                  (should button)
                  (should (= (match-end 0) (button-end button)))
                  (button-activate button)))
              (should (equal file visited))
              (should (equal "/root/review" agent))
              (should (equal "ab12" item))
              (goto-char (point-min))
              (search-forward "work://code.md")
              (should-not (button-at (match-beginning 0))))))
      (delete-file file))))

(niwa-deftest niwa-view--decorate-local-images-in-range
  (:doc "`niwa-view--decorate-local-images-in-range' displays local image references")
  ,test
  (test)
  :doc "renders Markdown image links"
  (let ((file (make-temp-file "niwa-image-link-" nil ".png")))
    (unwind-protect
        (with-temp-buffer
          (insert (format "![shot](%s)\n" file))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (path &rest _)
                       (list 'image :file path))))
            (niwa-view--decorate-local-images-in-range
             (point-min) (point-max)))
          (goto-char (point-min))
          (search-forward "![shot]")
          (let ((display (get-text-property (match-beginning 0) 'display)))
            (should (equal (list 'image :file file) display))))
      (delete-file file)))

  :doc "bare non-image paths are never resolved"
  (with-temp-buffer
    (insert "See lisp/file.el and docs/readme.md\n")
    (cl-letf (((symbol-function 'niwa-view--resolve-path)
               (lambda (_) (ert-fail "Resolved a path that cannot be an image"))))
      (niwa-view--decorate-local-images-in-range (point-min) (point-max))))

  :doc "renders bare local image paths"
  (let ((file (make-temp-file "niwa-image-bare-" nil ".png")))
    (unwind-protect
        (with-temp-buffer
          (insert (format "Image: %s\n" file))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (path &rest _)
                       (list 'image :file path))))
            (niwa-view--decorate-local-images-in-range
             (point-min) (point-max)))
          (goto-char (point-min))
          (search-forward file)
          (let ((display (get-text-property (match-beginning 0) 'display)))
            (should (equal (list 'image :file file) display))))
      (delete-file file)))

  :doc "renders remote session images from verified publication bytes"
  (let* ((host "view-image-artifact")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-view-image-artifact-" t)))
         (remote-root (format "/niwamock:%s:%s" host local-root))
         (save-path (concat remote-root "session/"))
         (logical "tool-results/image.png")
         (fixed (file-name-concat save-path logical))
         (native-fixed (file-name-concat local-root "session" logical))
         (published (file-name-concat
                     save-path ".publications" "generation" "000001.data"))
         (staged (file-name-concat local-root "staged-image"))
         (content "verified image bytes")
         (target (niwa-execution-target-create remote-root))
         (session (niwa-session--create
                   :authority-mode 'portable
                   :name "remote-view" :execution-target target
                   :save-path save-path))
         created)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (make-directory (file-name-directory fixed) t)
          (write-region "poisoned fixed cache" nil fixed nil 'silent)
          (make-directory (file-name-directory published) t)
          (write-region content nil published nil 'silent)
          (write-region "uncommitted image bytes" nil staged nil 'silent)
          (setf (niwa-session-publication session)
                (list :head ".publications/generation/manifest.el"
                      :sidecar nil
                      :artifacts
                      (list (list logical :published published
                                  :sha256 (secure-hash 'sha256 content))))
                (niwa-session-publication-uncommitted-batches session)
                (list (list :directory local-root
                            :artifacts
                            (list (list :logical logical :source staged)))))
          (cl-letf (((symbol-function
                      'niwa-session-durability-lease-owned-p)
                     (lambda (_session) t))
                    ((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (source &optional _type data-p &rest _)
                       (setq created (list source data-p))
                       'image)))
            (with-temp-buffer
              (setq-local niwa--session session)
              (insert (format "![shot](%s)\n" native-fixed))
              (niwa-view--decorate-local-images-in-range
               (point-min) (point-max))
              (should (equal (list content t) created))
              (let ((auto-mode-alist
                     (cons '("\\.png\\'" . fundamental-mode) auto-mode-alist))
                    opened)
                (unwind-protect
                    (save-window-excursion
                      (button-activate (button-at (point-min)))
                      (setq opened (window-buffer (selected-window)))
                      (with-current-buffer opened
                        (should (equal content (buffer-string)))
                        (should buffer-read-only)
                        (should (equal fixed buffer-file-name))))
                  (when (buffer-live-p opened)
                    (kill-buffer opened)))))
            (setf (niwa-session-publication session)
                  (list :head ".publications/generation/manifest.el"
                        :sidecar nil :artifacts nil))
            (setq created nil)
            (with-temp-buffer
              (setq-local niwa--session session)
              (insert (format "![shot](%s)\n" native-fixed))
              (niwa-view--decorate-local-images-in-range
               (point-min) (point-max))
              (should-not created))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-view--decorate-markdown-in-range
  (:doc "`niwa-view--decorate-markdown-in-range' renders Markdown affordances")
  ,test
  (test)
  :doc "displayed image paths retain their image-opening button"
  (let ((file (make-temp-file "niwa-image-decoration-" nil ".png")))
    (unwind-protect
        (with-temp-buffer
          (insert (format "path: %s\n" file))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (path &rest _)
                       (list 'image :file path))))
            (niwa-view--decorate-markdown-in-range
             (point-min) (point-max)))
          (goto-char (point-min))
          (search-forward file)
          (let ((start (match-beginning 0)))
            (should (equal (list 'image :file file)
                           (get-text-property start 'display)))
            (should (button-at start))
            (should (equal file (button-get (button-at start)
                                           'niwa-view-path)))))
      (delete-file file)))

  :doc "renders pipe tables and preserves view-owned properties"
  (let ((text "| Name | Role |\n|---|---|\n| Ada | Developer |\n"))
    (with-temp-buffer
      (insert text)
      (add-text-properties (point-min) (point-max)
                           '(niwa-view-source task-table))
      (niwa-view--decorate-markdown-in-range (point-min) (point-max))
      (let ((rendered (buffer-substring-no-properties
                       (point-min) (point-max))))
        (should (string-match-p "│ Name │ Role      │" rendered))
        (should-not (string-match-p "^| Name" rendered)))
      (should (equal (string-trim-right text "\n")
                     (substring-no-properties
                      (get-text-property (point-min)
                                         'niwa-view-table-source))))
      (should-not
       (text-property-not-all
        (point-min) (point-max) 'niwa-view-source 'task-table))))

  :doc "pipe rows inside fenced code blocks stay raw"
  (dolist (text '("```\n| a | b |\n| c | d |\n```\n"
                  "````\n| a | b |\n| c | d |\n````\n"
                  "~~~\n| a | b |\n| c | d |\n~~~\n"))
    (with-temp-buffer
      (insert text)
      (niwa-view--decorate-markdown-in-range (point-min) (point-max))
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
      (niwa-view--decorate-markdown-in-range (point-min) (point-max))
      (let ((rendered (buffer-substring-no-properties
                       (point-min) (point-max))))
        (should (string-match-p "| a | b |" rendered))
        (should-not (string-match-p "│" rendered))))))


(niwa-deftest niwa-view--linkify-paths-in-range ()
  ,test
  (test)
  :doc "one projection resolves a repeated path once and links each use"
  (let* ((root (make-temp-file "niwa-view-linkify-memo-" t))
         (file (file-name-concat root "memo.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-memo"
                     :root root :name "linkify-memo"))
         (session (niwa-session-create "main" workspace))
         (resolve (symbol-function 'niwa-view--resolve-path))
         (resolutions 0))
    (unwind-protect
        (progn
          (with-temp-file file (insert "memo\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "Read: memo.el\nAgain: memo.el\nGone: absent.el\n"
                    "Gone again: absent.el\n")
            (cl-letf (((symbol-function 'niwa-view--resolve-path)
                       (lambda (raw) (cl-incf resolutions) (funcall resolve raw))))
              (let ((niwa-view--path-link-memo (make-hash-table :test #'equal)))
                (niwa-view--linkify-paths-in-range (point-min) (point-max))))
            (should (= 2 resolutions))
            (goto-char (point-min))
            (dotimes (_ 2)
              (search-forward "memo.el")
              (should (button-at (match-beginning 0))))
            (search-forward "absent.el")
            (should-not (button-at (match-beginning 0)))))
      (delete-directory root t)))

  :doc "slashless root filename is buttonized when it exists"
  (let* ((root (make-temp-file "niwa-view-linkify-" t))
         (file (file-name-concat root "niwa-skills.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-root"
                     :root root :name "linkify-root"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "Read: niwa-skills.el\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "niwa-skills.el")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'niwa-view-path))))))
      (delete-directory root t)))

  :doc "linkify-exempt text stays plain even when the path exists"
  (let* ((root (make-temp-file "niwa-view-linkify-exempt-" t))
         (file (file-name-concat root "niwa-skills.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-exempt"
                     :root root :name "linkify-exempt"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert (propertize "+;;; niwa-skills.el --- functions\n"
                                'niwa-view-no-linkify t))
            (insert "Read: niwa-skills.el\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "niwa-skills.el")
            (should-not (button-at (match-beginning 0)))
            (search-forward "niwa-skills.el")
            (should (button-at (match-beginning 0)))))
      (delete-directory root t)))

  :doc "missing slashless filename stays plain text"
  (let* ((root (make-temp-file "niwa-view-linkify-missing-" t))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-missing"
                     :root root :name "linkify-missing"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (insert "Read: missing-file.el\n")
          (niwa-view--linkify-paths-in-range (point-min) (point-max))
          (goto-char (point-min))
          (search-forward "missing-file.el")
          (should-not (button-at (match-beginning 0))))
      (delete-directory root t)))

  :doc "remote session artifact opens committed bytes through its logical path"
  (let* ((host "view-artifact")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-view-artifact-" t)))
         (remote-root (format "/niwamock:%s:%s" host local-root))
         (save-path (concat remote-root "session/"))
         (logical "tool-results/result.txt")
         (fixed (file-name-concat save-path logical))
         (native-fixed (file-name-concat local-root "session" logical))
         (published (file-name-concat
                     save-path ".publications" "generation" "000001.data"))
         (content "committed\nsecond\nthird\n")
         (target (niwa-execution-target-create remote-root))
         (session (niwa-session--create
                   :authority-mode 'portable
                   :name "remote-view" :execution-target target
                   :save-path save-path))
         opened)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (make-directory (file-name-directory fixed) t)
          (write-region "poisoned fixed cache" nil fixed nil 'silent)
          (make-directory (file-name-directory published) t)
          (write-region content nil published nil 'silent)
          (setf (niwa-session-publication session)
                (list :head ".publications/generation/manifest.el"
                      :sidecar nil
                      :artifacts
                      (list (list logical :published published
                                  :sha256 (secure-hash 'sha256 content)))))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "Read " native-fixed ":2\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward (concat native-fixed ":2"))
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal logical
                             (button-get
                              button 'niwa-view-session-artifact)))
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
                      (make-temp-file "niwa-view-unpublished-" t)))
         (remote-root (format "/niwamock:%s:%s" host local-root))
         (save-path (concat remote-root "session/"))
         (logical "tool-results/result.txt")
         (fixed (file-name-concat save-path logical))
         (native-fixed (file-name-concat local-root "session" logical))
         (target (niwa-execution-target-create remote-root))
         (session (niwa-session--create
                   :authority-mode 'portable
                   :name "remote-view" :execution-target target
                   :save-path save-path)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (make-directory (file-name-directory fixed) t)
          (write-region "unpublished fixed cache" nil fixed nil 'silent)
          (setf (niwa-session-publication session)
                (list :head ".publications/generation/manifest.el"
                      :sidecar nil :artifacts nil))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "Read " native-fixed "\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward native-fixed)
            (should-not (button-at (match-beginning 0)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "slash-containing relative path is still buttonized"
  (let* ((root (make-temp-file "niwa-view-linkify-rel-" t))
         (file (file-name-concat root "test/test-niwa-skills.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-rel"
                     :root root :name "linkify-rel"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert "subdir\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "Edit: test/test-niwa-skills.el\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "test/test-niwa-skills.el")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'niwa-view-path))))))
      (delete-directory root t)))

  :doc "URL-like text is not buttonized"
  (let* ((root (make-temp-file "niwa-view-linkify-url-" t))
         (file (file-name-concat root "example.com"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-url"
                     :root root :name "linkify-url"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "not a link target here\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "See https://example.com\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "example.com")
            (should-not (button-at (match-beginning 0)))))
      (delete-directory root t)))

  :doc "relative file line reference stores path and line"
  (let* ((root (make-temp-file "niwa-view-linkify-line-" t))
         (file (file-name-concat root "niwa-session-persistence.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-line"
                     :root root :name "linkify-line"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "See niwa-session-persistence.el:187\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "niwa-session-persistence.el:187")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'niwa-view-path)))
              (should (= 187 (button-get button 'niwa-view-line))))))
      (delete-directory root t)))

  :doc "relative file line range stores first line and spans full range"
  (let* ((root (make-temp-file "niwa-view-linkify-range-" t))
         (file (file-name-concat root "file.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-range"
                     :root root :name "linkify-range"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "See file.el:100-102\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:100-102")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'niwa-view-path)))
              (should (= 100 (button-get button 'niwa-view-line)))
              (let ((end-button (button-at (1- (match-end 0)))))
                (should end-button)
                (should (equal file
                               (button-get end-button 'niwa-view-path)))
                (should (= 100
                           (button-get end-button 'niwa-view-line)))))))
      (delete-directory root t)))

  :doc "relative file L-prefixed line range stores first line"
  (let* ((root (make-temp-file "niwa-view-linkify-l-range-" t))
         (file (file-name-concat root "file.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-l-range"
                     :root root :name "linkify-l-range"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "See file.el:L1400-L1422\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:L1400-L1422")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'niwa-view-path)))
              (should (= 1400 (button-get button 'niwa-view-line))))))
      (delete-directory root t)))

  :doc "colon line list creates separate buttons"
  (let* ((root (make-temp-file "niwa-view-linkify-list-" t))
         (file (file-name-concat root "file.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-list"
                     :root root :name "linkify-list"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "See file.el:L24,L120-L143\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:L24")
            (let ((first (button-at (match-beginning 0))))
              (should first)
              (should (= 24 (button-get first 'niwa-view-line))))
            (search-forward ",")
            (should-not (button-at (1- (point))))
            (search-forward "L120-L143")
            (let ((second (button-at (match-beginning 0))))
              (should second)
              (should (equal file
                             (button-get second 'niwa-view-path)))
              (should (= 120 (button-get second 'niwa-view-line))))))
      (delete-directory root t)))

  :doc "hash line list creates separate buttons"
  (let* ((root (make-temp-file "niwa-view-linkify-hash-list-" t))
         (file (file-name-concat root "file.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-hash-list"
                     :root root :name "linkify-hash-list"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "See file.el:#L24,#L120-#L143\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:#L24")
            (let ((first (button-at (match-beginning 0))))
              (should first)
              (should (= 24 (button-get first 'niwa-view-line))))
            (search-forward ",")
            (should-not (button-at (1- (point))))
            (search-forward "#L120-#L143")
            (let ((second (button-at (match-beginning 0))))
              (should second)
              (should (equal file
                             (button-get second 'niwa-view-path)))
              (should (= 120 (button-get second 'niwa-view-line))))))
      (delete-directory root t)))

  :doc "direct #L fragment stores first line"
  (let* ((root (make-temp-file "niwa-view-linkify-hash-" t))
         (file (file-name-concat root "file.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-hash"
                     :root root :name "linkify-hash"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "See file.el#L24-L30\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el#L24-L30")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'niwa-view-path)))
              (should (= 24 (button-get button 'niwa-view-line))))))
      (delete-directory root t)))

  :doc "@file mention with line reference stores path and line"
  (let* ((root (make-temp-file "niwa-view-linkify-file-mention-" t))
         (file (file-name-concat root "with space.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-file-mention"
                     :root root :name "linkify-file-mention"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert (format "See @file:{%s}#L7\n" file))
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "@file:")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'niwa-view-path)))
              (should (= 7 (button-get button 'niwa-view-line))))))
      (delete-directory root t)))

  :doc "escaped file tokens retain pathname and range through mentions and rendering"
  (let ((root (make-temp-file "niwa-file-token-" t)))
    (unwind-protect
        (dolist (name '("plain.el" "space #hash}.el" "open{brace.el"
                        "tab\tname.el"))
          (let* ((file (file-name-concat root name))
                 (token (niwa-mentions-file-token file))
                 (text (concat "See " token "#L2-3")))
            (with-temp-file file (insert "one\ntwo\nthree\n"))
            (should (equal (list file)
                           (niwa-mentions-file-paths-in-text text)))
            (with-temp-buffer
              (insert text)
              (niwa-view--linkify-paths-in-range (point-min) (point-max))
              (let ((button (button-at (+ (point-min) 4))))
                (should button)
                (should (equal file (button-get button 'niwa-view-path)))
                (should (= 2 (button-get button 'niwa-view-line)))
                (should (= (point-max) (button-end button)))))))
      (delete-directory root t)))

  :doc "Markdown local link #L range stores first line"
  (let* ((root (make-temp-file "niwa-view-linkify-md-range-" t))
         (file (file-name-concat root "file.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-md-range"
                     :root root :name "linkify-md-range"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "[spot](file.el#L24-L30)\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "spot")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'niwa-view-path)))
              (should (= 24 (button-get button 'niwa-view-line))))))
      (delete-directory root t)))

  :doc "Markdown local link colon line suffix stores first line"
  (let* ((root (make-temp-file "niwa-view-linkify-md-colon-" t))
         (file (file-name-concat root "file.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-md-colon"
                     :root root :name "linkify-md-colon"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "[plain](file.el:24)\n[prefixed](file.el:L25-L30)\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "plain")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'niwa-view-path)))
              (should (= 24 (button-get button 'niwa-view-line))))
            (search-forward "prefixed")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'niwa-view-path)))
              (should (= 25 (button-get button 'niwa-view-line))))))
      (delete-directory root t)))

  :doc "nested relative file line reference resolves from workspace root"
  (let* ((root (make-temp-file "niwa-view-linkify-nested-line-" t))
         (file (file-name-concat root "test/test-niwa-agent-exec.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-nested-line"
                     :root root :name "linkify-nested-line"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert "nested\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "See test/test-niwa-agent-exec.el:803\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "test/test-niwa-agent-exec.el:803")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'niwa-view-path)))
              (should (= 803 (button-get button 'niwa-view-line))))))
      (delete-directory root t)))

  :doc "absolute file line reference stores path and line"
  (let ((file (make-temp-file "niwa-view-linkify-abs-" nil ".el")))
    (unwind-protect
        (with-temp-buffer
          (insert "See " file ":42\n")
          (niwa-view--linkify-paths-in-range (point-min) (point-max))
          (goto-char (point-min))
          (search-forward (concat file ":42"))
          (let ((button (button-at (match-beginning 0))))
            (should button)
            (should (equal file
                           (button-get button 'niwa-view-path)))
            (should (= 42 (button-get button 'niwa-view-line)))))
      (delete-file file)))

  :doc "activating file line reference jumps to the requested line"
  (let ((file (make-temp-file "niwa-view-linkify-action-" nil ".el"))
        opened)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "one\ntwo\nthree\nfour\n"))
          (with-temp-buffer
            (insert "See " file ":3\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
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
  (let* ((root (make-temp-file "niwa-view-linkify-missing-line-" t))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-missing-line"
                     :root root :name "linkify-missing-line"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (insert "See missing-file.el:10\n")
          (niwa-view--linkify-paths-in-range (point-min) (point-max))
          (goto-char (point-min))
          (search-forward "missing-file.el:10")
          (should-not (button-at (match-beginning 0))))
      (delete-directory root t)))

  :doc "URL-like text with port is not buttonized"
  (with-temp-buffer
    (insert "See https://example.com:443\n")
    (niwa-view--linkify-paths-in-range (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "example.com:443")
    (should-not (button-at (match-beginning 0))))

  :doc "trailing punctuation is not part of a line reference button"
  (let* ((root (make-temp-file "niwa-view-linkify-punct-" t))
         (file (file-name-concat root "file.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "linkify-punct"
                     :root root :name "linkify-punct"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "punct\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "See file.el:42.\n")
            (niwa-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:42")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (= 42 (button-get button 'niwa-view-line)))
              (should-not (button-at (point))))))
      (delete-directory root t))))


(niwa-deftest niwa-view--buffer-substring-filter ()
  ,test
  (test)
  :doc "a region overlapping a rendered table yields canonical Markdown"
  (with-temp-buffer
    (let ((source "| a | b |\n|---|---|\n| 1 | 2 |"))
      (insert "before\n" source "\nafter\n")
      (niwa-view-table-decorate (point-min) (point-max) nil)
      (should (equal (concat "before\n" source "\nafter\n")
                     (substring-no-properties
                      (niwa-view--buffer-substring-filter
                       (point-min) (point-max)))))))

  :doc "a partial table region yields its cell text without borders"
  (with-temp-buffer
    (let ((source "| a | b |\n|---|---|\n| 1 | 2 |"))
      (insert source "\n")
      (niwa-view-table-decorate (point-min) (point-max) nil)
      (goto-char (point-min))
      (search-forward "│ 1 │")
      (should (equal "1"
                     (substring-no-properties
                      (niwa-view--buffer-substring-filter
                       (match-beginning 0) (match-end 0)))))))

  :doc "partial rows join cells with tabs, drop separators, keep empty cells"
  (with-temp-buffer
    (insert "| a | b | c |\n|---|---|---|\n| 1 |  | 3 |\n| x | y | z |\n")
    (niwa-view-table-decorate (point-min) (point-max) nil)
    (goto-char (point-min))
    (search-forward "│ x")
    (should (equal "a\tb\tc\n1\t\t3\nx"
                   (substring-no-properties
                    (niwa-view--buffer-substring-filter
                     (point-min) (point))))))

  :doc "a box-drawing character inside a cell is content, not a border"
  (with-temp-buffer
    (insert "| a | b |\n|---|---|\n| 1│2 | 3 |\n")
    (niwa-view-table-decorate (point-min) (point-max) nil)
    (goto-char (point-min))
    (search-forward "1│2")
    (should (equal "1│2"
                   (substring-no-properties
                    (niwa-view--buffer-substring-filter
                     (match-beginning 0) (match-end 0))))))

  :doc "a region spanning prose and part of a table strips only the table"
  (with-temp-buffer
    (insert "before\n| a | b |\n|---|---|\n| 1 | 2 |\n")
    (niwa-view-table-decorate (point-min) (point-max) nil)
    (goto-char (point-min))
    (search-forward "│ a │ b │")
    (should (equal "before\na\tb"
                   (substring-no-properties
                    (niwa-view--buffer-substring-filter
                     (point-min) (point))))))

  :doc "table links keep canonical source and activation across rerenders"
  (niwa-test--with-displayed-buffer
    (let ((source (concat "| Person | Site |\n"
                          "|---|---|\n"
                          "| Ada | [Home](https://example.test) |")))
      (insert source "\n")
      (niwa-view--decorate-markdown-in-range (point-min) (point-max))
      (dotimes (pass 2)
        (goto-char (point-min))
        (search-forward "Home")
        (should (equal "https://example.test"
                       (button-get (button-at (match-beginning 0))
                                   'niwa-view-url)))
        (should (equal source
                       (substring-no-properties
                        (get-text-property
                         (point-min) 'niwa-view-table-source))))
        (should (equal (concat source "\n")
                       (substring-no-properties
                        (niwa-view--buffer-substring-filter
                         (point-min) (point-max)))))
        (when (zerop pass)
          (put-text-property
           (point-min)
           (next-single-property-change
            (point-min) 'niwa-view-table-source nil (point-max))
           'niwa-view-table-layout nil)
          (niwa-view-table-rerender)))))

  :doc "regions without tables copy verbatim and honor deletion"
  (with-temp-buffer
    (insert "plain text\n")
    (should (equal "plain"
                   (substring-no-properties
                    (niwa-view--buffer-substring-filter 1 6))))
    (should (equal "plain"
                   (substring-no-properties
                    (niwa-view--buffer-substring-filter 1 6 t))))
    (should (equal " text\n" (buffer-string))))

  :doc "regions without tables retain the stock substring filter"
  (with-temp-buffer
    (insert "plain text\n")
    (cl-letf (((symbol-function 'buffer-substring--filter)
               (lambda (_beg _end _delete) "stock")))
      (should (equal "stock"
                     (niwa-view--buffer-substring-filter 1 6))))))

(niwa-deftest niwa-view--position-in-ranges-p ()
  ,test
  (test)
  :doc "range membership includes starts, excludes ends and preserves input"
  (dolist (case '((nil nil)
                  (((3 . 6)) (3 4 5))
                  (((8 . 10) (2 . 5) (4 . 7)) (2 3 4 5 6 8 9))
                  (((3 . 3)) nil)
                  (((6 . 2)) nil)))
    (let* ((ranges (car case))
           (before (copy-tree ranges)))
      (dotimes (position 12)
        (should
         (eq (and (member position (cadr case)) t)
             (niwa-view--position-in-ranges-p position ranges))))
      (should (equal before ranges)))))

(niwa-deftest niwa-view--last-live-response-boundary ()
  ,test
  (test)
  :doc "uses blank lines outside fenced code as stable response boundaries"
  (with-temp-buffer
    (insert "First.\n\n```elisp\n(code)\n\nstill code\n```\n\nTail")
    (let ((boundary (niwa-view--last-live-response-boundary
                     (current-buffer) (point-min) (point-max))))
      (should (equal "Tail" (buffer-substring-no-properties
                             boundary (point-max))))))

  :doc "keeps an incomplete fenced block in the mutable tail"
  (with-temp-buffer
    (insert "First.\n\n```elisp\n(code)\n\nstill code")
    (let ((boundary (niwa-view--last-live-response-boundary
                     (current-buffer) (point-min) (point-max))))
      (should (string-prefix-p
               "```elisp" (buffer-substring-no-properties
                            boundary (point-max))))))

  :doc "does not close a long fence with a shorter delimiter plus text"
  (with-temp-buffer
    (insert
     "First.\n\n````elisp\n```not a close\n\nstill code\n````\n\nTail")
    (let ((boundary (niwa-view--last-live-response-boundary
                     (current-buffer) (point-min) (point-max))))
      (should (equal "Tail" (buffer-substring-no-properties
                             boundary (point-max))))))

  :doc "does not treat a four-space-indented delimiter as a closing fence"
  (with-temp-buffer
    (insert
     "First.\n\n```elisp\ncode\n    ```\n\nstill code\n```\n\nTail")
    (let ((boundary (niwa-view--last-live-response-boundary
                     (current-buffer) (point-min) (point-max))))
      (should (equal "Tail" (buffer-substring-no-properties
                             boundary (point-max)))))))


(provide 'test-niwa-view-markdown)

;;; test-niwa-view-markdown.el ends here
