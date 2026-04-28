;;; test-mevedel-compact.el --- Tests for mevedel-compact.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-compact)
(require 'mevedel-structs)
(require 'mevedel-workspace)
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

(mevedel-deftest mevedel--compact-invoked-skills-appendix ()
  ,test
  (test)
  :doc "returns nil when no session"
  (should (null (mevedel--compact-invoked-skills-appendix nil)))

  :doc "returns nil when session has no invoked-skills records"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "c1" :root "/tmp/c1" :name "c1"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (should (null (mevedel--compact-invoked-skills-appendix session))))

  :doc "lists invoked skills with name, args, trigger, turn"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "c2" :root "/tmp/c2" :name "c2"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (rec1 (mevedel-skill-invocation-record--create
                :name "grill-me" :args "spec 22"
                :trigger 'user-slash :turn 3
                :source-path "/skills/grill-me/SKILL.md"
                :prepared-body "Body 1"))
         (rec2 (mevedel-skill-invocation-record--create
                :name "review-spec" :args nil
                :trigger 'model-skill :turn 7
                :source-path "/skills/review-spec/SKILL.md"
                :prepared-body "Body 2")))
    (setf (mevedel-session-invoked-skills session) (list rec1 rec2))
    (let ((appendix (mevedel--compact-invoked-skills-appendix session)))
      (should appendix)
      (should (string-match-p "Skills invoked" appendix))
      (should (string-match-p "/grill-me spec 22" appendix))
      (should (string-match-p "user-slash" appendix))
      (should (string-match-p "turn: 3" appendix))
      (should (string-match-p "/review-spec" appendix))
      (should (string-match-p "model-skill" appendix)))))

(provide 'test-mevedel-compact)
;;; test-mevedel-compact.el ends here
