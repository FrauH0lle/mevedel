;;; test-niwa-skills-syntax.el --- Shared skill syntax tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests the low-level authored Markdown and dependency syntax helpers.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-skills-core)
(require 'niwa-skills-preparation)
(require 'niwa-skills-syntax)

(niwa-deftest niwa-skills-syntax--author-ranges-p
  (:doc "distinguishes authored text from generated text")
  (let ((text (copy-sequence "abc")))
    (add-text-properties 1 2 '(niwa-skills-non-author-text t) text)
    (should (niwa-skills-syntax--author-ranges-p text 0 1 2 3))
    (should-not (niwa-skills-syntax--author-ranges-p text 0 2))))

(niwa-deftest niwa-skills-syntax--ranges-overlap-p
  (:doc "detects only actual half-open range overlap")
  (progn
    (should (niwa-skills-syntax--ranges-overlap-p '((2 . 4)) 3 5))
    (should-not (niwa-skills-syntax--ranges-overlap-p '((2 . 4)) 4 5))))

(niwa-deftest niwa-skills-syntax--injection-fence-opener-p
  (:doc "recognizes only supported injection fences")
  (progn
    (should (niwa-skills-syntax--injection-fence-opener-p "```!" "```"))
    (should (niwa-skills-syntax--injection-fence-opener-p
             "```!el " "```"))
    (should-not
     (niwa-skills-syntax--injection-fence-opener-p "```md" "```"))))

(niwa-deftest niwa-skills-syntax--authored-fence-close-end
  (:doc "skips generated closing fences")
  (let ((text (copy-sequence "```\n```\n")))
    (add-text-properties 0 4 '(niwa-skills-non-author-text t) text)
    (should (= 8 (niwa-skills-syntax--authored-fence-close-end
                  text "\\(^\\|\n\\)```\\(\n\\|\\'\\)" 0)))))

(niwa-deftest niwa-skills-syntax--code-fence-ranges
  (:doc "excludes injection fences unless requested")
  (let ((text "```!\necho hi\n```\n"))
    (should-not (niwa-skills-syntax--code-fence-ranges text))
    (should (niwa-skills-syntax--code-fence-ranges text t))))

(niwa-deftest niwa-skills-syntax--injection-inline-marker-start
  (:doc "finds shell and Elisp inline injection markers")
  (progn
    (should (= 0 (niwa-skills-syntax--injection-inline-marker-start
                  "!`x`" 1)))
    (should (= 0 (niwa-skills-syntax--injection-inline-marker-start
                  "!el`x`" 3)))
    (should-not
     (niwa-skills-syntax--injection-inline-marker-start "`x`" 0))))

(niwa-deftest niwa-skills-syntax--injection-inline-span-end
  (:doc "finds the authored closing backtick")
  (progn
    (should (= 4 (niwa-skills-syntax--injection-inline-span-end
                  "!`x`" 1 4)))
    (should-not
     (niwa-skills-syntax--injection-inline-span-end "`x`" 0 3))))

(niwa-deftest niwa-skills-syntax--inline-code-ranges
  (:doc "finds Markdown spans outside fences")
  (progn
    (should (equal '((0 . 3))
                   (niwa-skills-syntax--inline-code-ranges "`x`" nil)))
    (should-not
     (niwa-skills-syntax--inline-code-ranges "`x`" '((0 . 3))))))

(niwa-deftest niwa-skills-syntax-markdown-code-ranges
  (:doc "combines fenced and inline Markdown ranges")
  (should (= 2 (length (niwa-skills-syntax-markdown-code-ranges
                        "`x`\n```\ny\n```\n")))))

(niwa-deftest niwa-skills-syntax--dependency-name-char-p
  (:doc "covers every character legal in a visible dependency name")
  (progn
    (dolist (ch '(?a ?Z ?0 ?- ?: ?_))
      (should (niwa-skills-syntax--dependency-name-char-p ch)))
    (dolist (ch '(?. ?/))
      (should-not (niwa-skills-syntax--dependency-name-char-p ch)))))

(niwa-deftest niwa-skills-syntax--dependency-escaped-p
  (:doc "uses odd authored backslash parity")
  (progn
    (should (niwa-skills-syntax--dependency-escaped-p "\\!$x" 1))
    (should-not
     (niwa-skills-syntax--dependency-escaped-p "\\\\!$x" 2))))

(niwa-deftest niwa-skills-syntax-parse-dependencies ()
  ,test
  (test)

  :doc "parses inline, full-line, multiple, and qualified declarations"
  (let* ((parsed
          (niwa-skills-syntax-parse-dependencies
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
         (parsed (niwa-skills-syntax-parse-dependencies text)))
    (should (equal '("live")
                   (mapcar (lambda (dependency)
                             (plist-get dependency :name))
                           (plist-get parsed :dependencies))))
    (should (equal (concat (substring text 0 (- (length text) 6))
                           "[skill:live -- attached]")
                   (plist-get parsed :body))))

  :doc "substitution-generated declarations remain inert argument data"
  (let* ((skill (niwa-skill--create :name "parent"))
         (body (niwa-skills-preparation-substitute
                "generated=$ARGUMENTS\n!$child -- $ARGUMENTS"
                "!$generated -- unsafe" nil skill))
         (parsed (niwa-skills-syntax-parse-dependencies body))
         (dependency (car (plist-get parsed :dependencies)))
         (template (plist-get dependency :argument-template)))
    (should (equal "child" (plist-get dependency :name)))
    (should (equal "!$generated -- unsafe" template))
    (should (niwa-skills-preparation--non-author-range-p
             template 0 (length template)))
    (should
     (equal
      (concat "generated=!$generated -- unsafe\n"
              "[skill:child -- attached]")
      (substring-no-properties (plist-get parsed :body)))))

  :doc "generated line boundaries cannot activate full-line arguments"
  (let* ((skill (niwa-skill--create :name "parent"))
         (body (niwa-skills-preparation-substitute
                "prefix$ARGUMENTS!$child -- authored" "\n" nil skill))
         (parsed (niwa-skills-syntax-parse-dependencies body))
         (dependency (car (plist-get parsed :dependencies))))
    (should (equal "child" (plist-get dependency :name)))
    (should-not (plist-get dependency :argument-template))
    (should
     (equal "prefix\n[skill:child -- attached] -- authored"
            (substring-no-properties (plist-get parsed :body))))))

(provide 'test-niwa-skills-syntax)
;;; test-niwa-skills-syntax.el ends here
