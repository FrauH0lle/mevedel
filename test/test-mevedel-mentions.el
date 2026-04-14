;;; test-mevedel-mentions.el --- Tests for mevedel-mentions.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-overlays)
(require 'mevedel-persistence)
(require 'mevedel-mentions)
(require 'mevedel-structs)
(require 'mevedel-workspace)

;; Declared in gptel; declared here so `let' binds it dynamically in
;; tests that do not load gptel.
(defvar gptel-default-mode)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Parsing

(mevedel-deftest mevedel--parse-ref-mentions
  (:doc "`mevedel--parse-ref-mentions' extracts id and tag mentions in order")
  ,test
  (test)
  :doc "returns empty list when no mentions present"
  (should (null (mevedel--parse-ref-mentions "plain text with no refs")))

  :doc "parses a single id mention"
  (let ((result (mevedel--parse-ref-mentions "see @ref:42 please")))
    (should (= 1 (length result)))
    (should (eq 'id (plist-get (car result) :type)))
    (should (= 42 (plist-get (car result) :value)))
    (should (equal "@ref:42" (plist-get (car result) :match-text))))

  :doc "parses a single tag-query mention"
  (let ((result (mevedel--parse-ref-mentions "check @ref{core or utils}")))
    (should (= 1 (length result)))
    (should (eq 'tag (plist-get (car result) :type)))
    (should (equal "core or utils" (plist-get (car result) :value))))

  :doc "parses multiple mentions in order"
  (let* ((text "first @ref:1 then @ref{foo} and @ref:2")
         (result (mevedel--parse-ref-mentions text)))
    (should (= 3 (length result)))
    (should (eq 'id (plist-get (nth 0 result) :type)))
    (should (= 1 (plist-get (nth 0 result) :value)))
    (should (eq 'tag (plist-get (nth 1 result) :type)))
    (should (equal "foo" (plist-get (nth 1 result) :value)))
    (should (eq 'id (plist-get (nth 2 result) :type)))
    (should (= 2 (plist-get (nth 2 result) :value)))))


;;
;;; Expansion

(defun mevedel-test--make-ref-buffer (content text)
  "Create a live file-visiting buffer with CONTENT and a reference on TEXT.
Returns (buffer . overlay)."
  (let* ((tmp (make-temp-file "mevedel-mention-" nil ".txt" content))
         (buf (find-file-noselect tmp))
         ov)
    (with-current-buffer buf
      (fundamental-mode)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (re-search-forward (regexp-quote text))
      (setq ov (mevedel--create-reference-in buf
                                             (match-beginning 0)
                                             (match-end 0))))
    (cons buf ov)))

(mevedel-deftest mevedel--expand-ref-mentions-in-string
  (:before-each
   (setq mevedel--instructions nil)
   (mevedel-workspace-clear-registry)
   :after-each
   (dolist (entry mevedel--instructions)
     (when (buffer-live-p (car entry))
       (with-current-buffer (car entry)
         (set-buffer-modified-p nil))
       (let ((file (buffer-file-name (car entry))))
         (kill-buffer (car entry))
         (when (and file (file-exists-p file))
           (delete-file file)))))
   (setq mevedel--instructions nil)
   (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "returns text unchanged when no references present"
  (should (equal "no mentions here"
                 (mevedel--expand-ref-mentions-in-string "no mentions here")))

  :doc "replaces valid @ref:ID with `Reference #N' and prepends section"
  (let* ((cell (mevedel-test--make-ref-buffer "foo bar baz\n" "bar"))
         (buf (car cell))
         (ov (cdr cell))
         (id (mevedel--instruction-id ov))
         (gptel-default-mode 'text-mode))
    (unwind-protect
        (let ((result (mevedel--expand-ref-mentions-in-string
                       (format "please look at @ref:%d now" id))))
          (should (string-match-p (format "Reference #%d" id) result))
          (should (string-match-p "### Reference" result)))
      (let ((file (buffer-file-name buf)))
        (kill-buffer buf)
        (when (and file (file-exists-p file))
          (delete-file file)))))

  :doc "invalid @ref:ID becomes [invalid @ref:N] with no section"
  (let ((result (mevedel--expand-ref-mentions-in-string
                 "look at @ref:99999 please")))
    (should (string-match-p "\\[invalid @ref:99999\\]" result))
    (should-not (string-match-p "### Reference" result))))


;;
;;; Resolution

(mevedel-deftest mevedel--resolve-ref-by-id
  (:before-each
   (setq mevedel--instructions nil)
   :after-each
   (dolist (entry mevedel--instructions)
     (when (buffer-live-p (car entry))
       (let ((file (buffer-file-name (car entry))))
         (with-current-buffer (car entry)
           (set-buffer-modified-p nil))
         (kill-buffer (car entry))
         (when (and file (file-exists-p file))
           (delete-file file)))))
   (setq mevedel--instructions nil))
  ,test
  (test)
  :doc "returns nil for unknown id"
  (should (null (mevedel--resolve-ref-by-id 999999)))

  :doc "returns overlay for known reference id"
  (let* ((cell (mevedel-test--make-ref-buffer "hello world\n" "hello"))
         (buf (car cell))
         (ov (cdr cell))
         (id (mevedel--instruction-id ov)))
    (unwind-protect
        (should (eq ov (mevedel--resolve-ref-by-id id)))
      (let ((file (buffer-file-name buf)))
        (kill-buffer buf)
        (when (and file (file-exists-p file))
          (delete-file file))))))


;;
;;; Font-lock matchers

(mevedel-deftest mevedel--fontify-ref-id-keyword
  (:doc "`mevedel--fontify-ref-id-keyword' matches @ref:ID at BOB or after whitespace")
  ,test
  (test)
  :doc "matches mention at beginning of buffer"
  (with-temp-buffer
    (insert "@ref:7 text")
    (goto-char (point-min))
    (should (mevedel--fontify-ref-id-keyword (point-max)))
    (should (equal "7" (match-string 1))))

  :doc "matches mention after whitespace"
  (with-temp-buffer
    (insert "some text @ref:42 here")
    (goto-char (point-min))
    (should (mevedel--fontify-ref-id-keyword (point-max)))
    (should (equal "42" (match-string 1)))))

(mevedel-deftest mevedel--fontify-ref-tag-keyword
  (:doc "`mevedel--fontify-ref-tag-keyword' matches @ref{query}")
  ,test
  (test)
  :doc "matches tag query at beginning of buffer"
  (with-temp-buffer
    (insert "@ref{core} extra")
    (goto-char (point-min))
    (should (mevedel--fontify-ref-tag-keyword (point-max)))
    (should (equal "core" (match-string 1))))

  :doc "matches tag query after whitespace"
  (with-temp-buffer
    (insert "pre @ref{core or utils} post")
    (goto-char (point-min))
    (should (mevedel--fontify-ref-tag-keyword (point-max)))
    (should (equal "core or utils" (match-string 1)))))

(mevedel-deftest mevedel--fontify-file-keyword
  (:doc "`mevedel--fontify-file-keyword' matches @file:path tokens")
  ,test
  (test)
  :doc "matches file mention with slashes"
  (with-temp-buffer
    (insert "see @file:/tmp/foo.el here")
    (goto-char (point-min))
    (should (mevedel--fontify-file-keyword (point-max)))
    (should (equal "/tmp/foo.el" (match-string 1))))

  :doc "captures path after @file:"
  (with-temp-buffer
    (insert "before @file:lib/core.el after")
    (goto-char (point-min))
    (should (mevedel--fontify-file-keyword (point-max)))
    (should (equal "lib/core.el" (match-string 1)))))

(provide 'test-mevedel-mentions)
;;; test-mevedel-mentions.el ends here
