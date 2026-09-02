;;; test-mevedel-skills-syntax.el --- Shared skill syntax tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests the low-level authored Markdown and dependency syntax helpers.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-skills-preparation)
(require 'mevedel-skills-syntax)

(mevedel-deftest mevedel-skills-syntax--author-ranges-p
  (:doc "distinguishes authored text from generated text")
  (let ((text (copy-sequence "abc")))
    (add-text-properties 1 2 '(mevedel-skills-non-author-text t) text)
    (should (mevedel-skills-syntax--author-ranges-p text 0 1 2 3))
    (should-not (mevedel-skills-syntax--author-ranges-p text 0 2))))

(mevedel-deftest mevedel-skills-syntax--ranges-overlap-p
  (:doc "detects only actual half-open range overlap")
  (progn
    (should (mevedel-skills-syntax--ranges-overlap-p '((2 . 4)) 3 5))
    (should-not (mevedel-skills-syntax--ranges-overlap-p '((2 . 4)) 4 5))))

(mevedel-deftest mevedel-skills-syntax--injection-fence-opener-p
  (:doc "recognizes only supported injection fences")
  (progn
    (should (mevedel-skills-syntax--injection-fence-opener-p "```!" "```"))
    (should (mevedel-skills-syntax--injection-fence-opener-p
             "```!el " "```"))
    (should-not
     (mevedel-skills-syntax--injection-fence-opener-p "```md" "```"))))

(mevedel-deftest mevedel-skills-syntax--authored-fence-close-end
  (:doc "skips generated closing fences")
  (let ((text (copy-sequence "```\n```\n")))
    (add-text-properties 0 4 '(mevedel-skills-non-author-text t) text)
    (should (= 8 (mevedel-skills-syntax--authored-fence-close-end
                  text "\\(^\\|\n\\)```\\(\n\\|\\'\\)" 0)))))

(mevedel-deftest mevedel-skills-syntax--code-fence-ranges
  (:doc "excludes injection fences unless requested")
  (let ((text "```!\necho hi\n```\n"))
    (should-not (mevedel-skills-syntax--code-fence-ranges text))
    (should (mevedel-skills-syntax--code-fence-ranges text t))))

(mevedel-deftest mevedel-skills-syntax--injection-inline-marker-start
  (:doc "finds shell and Elisp inline injection markers")
  (progn
    (should (= 0 (mevedel-skills-syntax--injection-inline-marker-start
                  "!`x`" 1)))
    (should (= 0 (mevedel-skills-syntax--injection-inline-marker-start
                  "!el`x`" 3)))
    (should-not
     (mevedel-skills-syntax--injection-inline-marker-start "`x`" 0))))

(mevedel-deftest mevedel-skills-syntax--injection-inline-span-end
  (:doc "finds the authored closing backtick")
  (progn
    (should (= 4 (mevedel-skills-syntax--injection-inline-span-end
                  "!`x`" 1 4)))
    (should-not
     (mevedel-skills-syntax--injection-inline-span-end "`x`" 0 3))))

(mevedel-deftest mevedel-skills-syntax--inline-code-ranges
  (:doc "finds Markdown spans outside fences")
  (progn
    (should (equal '((0 . 3))
                   (mevedel-skills-syntax--inline-code-ranges "`x`" nil)))
    (should-not
     (mevedel-skills-syntax--inline-code-ranges "`x`" '((0 . 3))))))

(mevedel-deftest mevedel-skills-syntax-markdown-code-ranges
  (:doc "combines fenced and inline Markdown ranges")
  (should (= 2 (length (mevedel-skills-syntax-markdown-code-ranges
                        "`x`\n```\ny\n```\n")))))

(mevedel-deftest mevedel-skills-syntax--dependency-name-char-p
  (:doc "covers every character legal in a visible dependency name")
  (progn
    (dolist (ch '(?a ?Z ?0 ?- ?: ?_))
      (should (mevedel-skills-syntax--dependency-name-char-p ch)))
    (dolist (ch '(?. ?/))
      (should-not (mevedel-skills-syntax--dependency-name-char-p ch)))))

(mevedel-deftest mevedel-skills-syntax--dependency-escaped-p
  (:doc "uses odd authored backslash parity")
  (progn
    (should (mevedel-skills-syntax--dependency-escaped-p "\\!$x" 1))
    (should-not
     (mevedel-skills-syntax--dependency-escaped-p "\\\\!$x" 2))))

(mevedel-deftest mevedel-skills-syntax-parse-dependencies ()
  ,test
  (test)

  :doc "parses inline, full-line, multiple, and qualified declarations"
  (let* ((parsed
          (mevedel-skills-syntax-parse-dependencies
           "Use !$child and !$Plugin.Name_1:review.\n!$templated -- raw args\nDone"))
         (dependencies (plist-get parsed :dependencies)))
    (should (equal '("child" "Plugin.Name_1:review" "templated")
                   (mapcar (lambda (dependency)
                             (plist-get dependency :name))
                           dependencies)))
    (should (equal '(nil nil "raw args")
                   (mapcar (lambda (dependency)
                             (plist-get dependency :argument-template))
                           dependencies)))
    (should
     (equal
      (concat "Use [skill:child -- attached] and "
              "[skill:Plugin.Name_1:review -- attached].\n"
              "[skill:templated -- attached]\nDone")
      (plist-get parsed :body))))

  :doc "keeps escaped and Markdown or injection code examples inert"
  (let* ((text
          (concat "\\!$escaped `!$inline`\n"
                  "```md\n!$fenced\n```\n"
                  "!`echo !$shell`\n"
                  "```!\necho !$fenced-shell\n```\n"
                  "!$live"))
         (parsed (mevedel-skills-syntax-parse-dependencies text)))
    (should (equal '("live")
                   (mapcar (lambda (dependency)
                             (plist-get dependency :name))
                           (plist-get parsed :dependencies))))
    (should (equal (concat (substring text 0 (- (length text) 6))
                           "[skill:live -- attached]")
                   (plist-get parsed :body))))

  :doc "substitution-generated declarations remain inert argument data"
  (let* ((skill (mevedel-skill--create :name "parent"))
         (body (mevedel-skills-preparation-substitute
                "generated=$ARGUMENTS\n!$child -- $ARGUMENTS"
                "!$generated -- unsafe" nil skill))
         (parsed (mevedel-skills-syntax-parse-dependencies body))
         (dependency (car (plist-get parsed :dependencies)))
         (template (plist-get dependency :argument-template)))
    (should (equal "child" (plist-get dependency :name)))
    (should (equal "!$generated -- unsafe" template))
    (should (mevedel-skills-preparation--non-author-range-p
             template 0 (length template)))
    (should
     (equal
      (concat "generated=!$generated -- unsafe\n"
              "[skill:child -- attached]")
      (substring-no-properties (plist-get parsed :body)))))

  :doc "generated line boundaries cannot activate full-line arguments"
  (let* ((skill (mevedel-skill--create :name "parent"))
         (body (mevedel-skills-preparation-substitute
                "prefix$ARGUMENTS!$child -- authored" "\n" nil skill))
         (parsed (mevedel-skills-syntax-parse-dependencies body))
         (dependency (car (plist-get parsed :dependencies))))
    (should (equal "child" (plist-get dependency :name)))
    (should-not (plist-get dependency :argument-template))
    (should
     (equal "prefix\n[skill:child -- attached] -- authored"
            (substring-no-properties (plist-get parsed :body))))))

(provide 'test-mevedel-skills-syntax)
;;; test-mevedel-skills-syntax.el ends here
