;;; test-mevedel-compact.el --- Tests for mevedel-compact.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-compact)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel--file-local-variables-start ()
  ,test
  (test)
  :doc "returns nil when no file-local variables"
  (with-temp-buffer
    (insert "Hello world\n")
    (should (null (mevedel--file-local-variables-start))))

  :doc "detects elisp-style file-local variables"
  (with-temp-buffer
    (insert "Content here\n\n")
    (let ((start (point)))
      (insert ";; Local Variables:\n")
      (insert ";; gptel-model: \"test\"\n")
      (insert ";; End:\n")
      (should (= (mevedel--file-local-variables-start) start))))

  :doc "detects markdown-style file-local variables"
  (with-temp-buffer
    (insert "# Markdown content\n\n")
    (let ((start (point)))
      (insert "<!-- Local Variables: -->\n")
      (insert "<!-- gptel-model: \"test\" -->\n")
      (insert "<!-- End: -->\n")
      (should (= (mevedel--file-local-variables-start) start))))

  :doc "finds first Local Variables block when multiple exist"
  (with-temp-buffer
    (insert "Content here\n\n")
    (let ((first-start (point)))
      (insert ";; Local Variables:\n")
      (insert ";; gptel-model: \"test\"\n")
      (insert ";; End:\n\n")
      (insert ";; Local Variables:\n")
      (insert ";; gptel-model: \"test2\"\n")
      (insert ";; End:\n")
      (should (= (mevedel--file-local-variables-start) first-start)))))

(mevedel-deftest mevedel--estimate-tokens ()
  ,test
  (test)
  :doc "counts tokens without file-local variables"
  (with-temp-buffer
    (insert "Hello world")  ; 11 chars / 4 = 2 tokens
    (should (= (mevedel--estimate-tokens) 2)))

  :doc "excludes elisp-style file-local variables from count"
  (with-temp-buffer
    (insert "Hello world\n\n")  ; 13 chars / 4 = 3 tokens
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"claude-sonnet-4\"\n")
    (insert ";; End:\n")
    (should (= (mevedel--estimate-tokens) 3)))

  :doc "excludes markdown-style file-local variables from count"
  (with-temp-buffer
    (insert "# Title\n\nContent\n\n")  ; 18 chars / 4 = 4 tokens
    (insert "<!-- Local Variables: -->\n")
    (insert "<!-- gptel-model: \"test\" -->\n")
    (insert "<!-- End: -->\n")
    (should (= (mevedel--estimate-tokens) 4)))

  :doc "respects gptel ignore property"
  (with-temp-buffer
    (insert "Hello ")
    (let ((start (point)))
      (insert "ignored ")
      (put-text-property start (point) 'gptel 'ignore))
    (insert "world")
    (should (= (mevedel--estimate-tokens) 2)))  ; "Hello world" = 11 chars / 4 = 2 tokens

  :doc "combines gptel ignore property and file-local variables exclusion"
  (with-temp-buffer
    (insert "Hello ")
    (let ((start (point)))
      (insert "ignored ")
      (put-text-property start (point) 'gptel 'ignore))
    (insert "world\n\n")
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"test\"\n")
    (insert ";; End:\n")
    (should (= (mevedel--estimate-tokens) 3)))  ; "Hello world\n\n" = 13 chars / 4 = 3 tokens

  :doc "excludes all file-local variables when multiple blocks exist"
  (with-temp-buffer
    (insert "Hello world\n\n")  ; 13 chars / 4 = 3 tokens
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"test\"\n")
    (insert ";; End:\n\n")
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"test2\"\n")
    (insert ";; End:\n")
    (should (= (mevedel--estimate-tokens) 3))))

(provide 'test-mevedel-compact)
;;; test-mevedel-compact.el ends here
