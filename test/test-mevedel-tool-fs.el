;;; test-mevedel-tool-fs.el --- Tests for file system tools -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-file-state)
(require 'mevedel-tool-registry)
(require 'mevedel-view)
(require 'mevedel-tool-fs)
(require 'mevedel-preview-mode)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar gptel-backend)


;;
;;; Binary extension detection

(mevedel-deftest mevedel-tool-fs--binary-extension-p ()
  ,test
  (test)
  :doc "detects common binary extensions"
  (should (mevedel-tool-fs--binary-extension-p "image.png"))
  (should (mevedel-tool-fs--binary-extension-p "archive.zip"))
  (should (mevedel-tool-fs--binary-extension-p "binary.exe"))
  (should (mevedel-tool-fs--binary-extension-p "doc.pdf"))
  :doc "rejects text file extensions"
  (should-not (mevedel-tool-fs--binary-extension-p "code.el"))
  (should-not (mevedel-tool-fs--binary-extension-p "readme.md"))
  (should-not (mevedel-tool-fs--binary-extension-p "config.json"))
  (should-not (mevedel-tool-fs--binary-extension-p "style.css"))
  :doc "case insensitive"
  (should (mevedel-tool-fs--binary-extension-p "IMAGE.PNG"))
  (should (mevedel-tool-fs--binary-extension-p "file.JPG"))
  :doc "handles files without extension"
  (should-not (mevedel-tool-fs--binary-extension-p "Makefile"))
  (should-not (mevedel-tool-fs--binary-extension-p ".gitignore")))


;;
;;; Media helpers

(defconst test-mevedel-tool-fs--png-bytes
  (unibyte-string #x89 ?P ?N ?G ?\r ?\n #x1a ?\n)
  "Minimal PNG bytes used by Read media tests.")

(defun test-mevedel-tool-fs--write-bytes (path bytes)
  "Write unibyte BYTES to PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (let ((coding-system-for-write 'binary))
      (write-region nil nil path nil 'silent))))

(mevedel-deftest mevedel-tool-fs--media-mime-type ()
  ,test
  (test)
  :doc "detects supported media MIME types"
  (should (equal "image/png" (mevedel-tool-fs--media-mime-type "a.png")))
  (should (equal "image/jpeg" (mevedel-tool-fs--media-mime-type "a.jpg")))
  (should (equal "image/jpeg" (mevedel-tool-fs--media-mime-type "a.JPEG")))
  (should (equal "image/gif" (mevedel-tool-fs--media-mime-type "a.gif")))
  (should (equal "image/webp" (mevedel-tool-fs--media-mime-type "a.webp")))
  (should (equal "application/pdf"
                 (mevedel-tool-fs--media-mime-type "doc.pdf")))
  :doc "returns nil for unsupported binary and text files"
  (should-not (mevedel-tool-fs--media-mime-type "archive.zip"))
  (should-not (mevedel-tool-fs--media-mime-type "readme.md")))

(mevedel-deftest mevedel-tool-fs--native-media-backend-p ()
  ,test
  (test)
  :doc "accepts gptel struct backends with native tool-result media serializers"
  (let ((gptel-backend 'backend))
    (cl-letf (((symbol-function 'gptel-anthropic-p)
               (lambda (backend) (eq backend 'backend)))
              ((symbol-function 'gptel-bedrock-p)
               (lambda (_backend) nil)))
      (should (mevedel-tool-fs--native-media-backend-p))))
  :doc "rejects backends without native tool-result media serializers"
  (let ((gptel-backend 'backend))
    (cl-letf (((symbol-function 'gptel-anthropic-p)
               (lambda (_backend) nil))
              ((symbol-function 'gptel-bedrock-p)
               (lambda (_backend) nil)))
      (should-not (mevedel-tool-fs--native-media-backend-p)))))

(mevedel-deftest mevedel-tool-fs--parse-pages ()
  ,test
  (test)
  :doc "parses single page"
  (should (equal '(3 . 3) (mevedel-tool-fs--parse-pages "3")))
  :doc "parses closed range"
  (should (equal '(1 . 5) (mevedel-tool-fs--parse-pages "1-5")))
  :doc "parses open range with 20-page cap"
  (should (equal '(3 . 22) (mevedel-tool-fs--parse-pages "3-")))
  :doc "rejects invalid syntax"
  (should-error (mevedel-tool-fs--parse-pages "x-y") :type 'error)
  :doc "rejects descending ranges"
  (should-error (mevedel-tool-fs--parse-pages "5-2") :type 'error)
  :doc "rejects ranges over 20 pages"
  (should-error (mevedel-tool-fs--parse-pages "1-21") :type 'error))

(mevedel-deftest mevedel-tool-fs--bounded-pdf-page-range ()
  ,test
  (test)
  :doc "limits open-ended ranges to the actual PDF page count"
  (cl-letf (((symbol-function 'mevedel-tool-fs--pdf-page-count)
             (lambda (_path) 1)))
    (should (equal '(1 . 1)
                   (mevedel-tool-fs--bounded-pdf-page-range
                    "/tmp/doc.pdf" "1-"))))
  :doc "limits closed ranges to the actual PDF page count"
  (cl-letf (((symbol-function 'mevedel-tool-fs--pdf-page-count)
             (lambda (_path) 1)))
    (should (equal '(1 . 1)
                   (mevedel-tool-fs--bounded-pdf-page-range
                    "/tmp/doc.pdf" "1-5"))))
  :doc "falls back to the 20-page cap when the page count is unavailable"
  (cl-letf (((symbol-function 'mevedel-tool-fs--pdf-page-count)
             (lambda (_path) nil)))
    (should (equal '(3 . 22)
                   (mevedel-tool-fs--bounded-pdf-page-range
                    "/tmp/doc.pdf" "3-"))))
  :doc "rejects ranges that start after the last page"
  (cl-letf (((symbol-function 'mevedel-tool-fs--pdf-page-count)
             (lambda (_path) 2)))
    (let ((err (should-error
                (mevedel-tool-fs--bounded-pdf-page-range
                 "/tmp/doc.pdf" "3-4")
                :type 'error)))
      (should (string-match-p "starts after last page" (cadr err))))))

(mevedel-deftest mevedel-tool-fs--normalize-read-args ()
  ,test
  (test)
  :doc "treats empty pages and zero optional integers as absent"
  (let ((normalized (mevedel-tool-fs--normalize-read-args
                     '(:file_path "~/a.png"
                       :offset 0
                       :limit 0
                       :pages ""
                       :max_width 1600
                       :max_height 0
                       :max_tokens 0))))
    (should (equal (expand-file-name "~/a.png")
                   (plist-get normalized :file_path)))
    (should (null (plist-get normalized :offset)))
    (should (null (plist-get normalized :limit)))
    (should (null (plist-get normalized :pages)))
    (should (equal 1600 (plist-get normalized :max_width)))
    (should (null (plist-get normalized :max_height)))
    (should (null (plist-get normalized :max_tokens))))
  :doc "preserves positive optional integers"
  (let ((normalized (mevedel-tool-fs--normalize-read-args
                     '(:file_path "relative.png"
                       :offset 3 :limit 12 :max_height 900
                       :max_tokens 2000))))
    (should (equal (expand-file-name "relative.png")
                   (plist-get normalized :file_path)))
    (should (equal 3 (plist-get normalized :offset)))
    (should (equal 12 (plist-get normalized :limit)))
    (should (equal 900 (plist-get normalized :max_height)))
    (should (equal 2000 (plist-get normalized :max_tokens)))))


;;
;;; Blocked device detection

(mevedel-deftest mevedel-tool-fs--blocked-device-p ()
  ,test
  (test)
  :doc "blocks dangerous device paths"
  (should (mevedel-tool-fs--blocked-device-p "/dev/zero"))
  (should (mevedel-tool-fs--blocked-device-p "/dev/random"))
  (should (mevedel-tool-fs--blocked-device-p "/dev/stdin"))
  (should (mevedel-tool-fs--blocked-device-p "/dev/fd/0"))
  :doc "blocks proc fd aliases"
  (should (mevedel-tool-fs--blocked-device-p "/proc/self/fd/0"))
  (should (mevedel-tool-fs--blocked-device-p "/proc/1234/fd/1"))
  :doc "allows safe devices and regular paths"
  (should-not (mevedel-tool-fs--blocked-device-p "/dev/null"))
  (should-not (mevedel-tool-fs--blocked-device-p "/tmp/file.txt"))
  (should-not (mevedel-tool-fs--blocked-device-p "/proc/self/status")))


;;
;;; Read handler

(mevedel-deftest mevedel-tool-fs--read-file ()
  ,test
  (test)
  :doc "reads full file with line numbers"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "line one\nline two\nline three\n"))
          (let ((result (mevedel-tool-fs--read-file (list :file_path tmp))))
            (should (string-match-p "1\tline one" result))
            (should (string-match-p "2\tline two" result))
            (should (string-match-p "3\tline three" result))))
      (delete-file tmp)))
  :doc "reads with offset"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "a\nb\nc\nd\ne\n"))
          (let ((result (mevedel-tool-fs--read-file
                         (list :file_path tmp :offset 3 :limit 2))))
            (should (string-match-p "3\tc" result))
            (should (string-match-p "4\td" result))
            (should-not (string-match-p "\\b1\ta" result))
            (should-not (string-match-p "5\te" result))))
      (delete-file tmp)))
  :doc "reads with limit only"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "a\nb\nc\nd\ne\n"))
          (let ((result (mevedel-tool-fs--read-file
                         (list :file_path tmp :offset 1 :limit 2))))
            (should (string-match-p "1\ta" result))
            (should (string-match-p "2\tb" result))
            (should-not (string-match-p "3\tc" result))))
      (delete-file tmp)))
  :doc "errors on non-existent file"
  (should-error
   (mevedel-tool-fs--read-file (list :file_path "/nonexistent/file.txt"))
   :type 'error)
  :doc "errors on directory"
  (should-error
   (mevedel-tool-fs--read-file (list :file_path "/tmp"))
   :type 'error)
  :doc "errors on binary extension"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".zip")))
    (unwind-protect
        (should-error
         (mevedel-tool-fs--read-file (list :file_path tmp))
         :type 'error)
      (delete-file tmp)))
  :doc "reads supported image media with base64 envelope when media-capable"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".png")))
    (unwind-protect
        (progn
          (test-mevedel-tool-fs--write-bytes
           tmp test-mevedel-tool-fs--png-bytes)
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (mime &optional _model)
                       (equal mime "image/png")))
                    ((symbol-function
                      'mevedel-tool-fs--native-media-backend-p)
                     (lambda () t)))
            (let ((result (mevedel-tool-fs--read-file (list :file_path tmp))))
              (should (listp result))
              (should (string-match-p "<media-file>"
                                      (plist-get result :result)))
              (should (string-match-p "mime_type: image/png"
                                      (plist-get result :result)))
              (should (string-match-p "encoding: base64"
                                      (plist-get result :result)))
              (should (equal (plist-get (car (plist-get result :media)) :mime)
                             "image/png"))
              (should (plist-get (car (plist-get result :media)) :data)))))
      (delete-file tmp)))
  :doc "treats default offset and limit values as absent for media reads"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".png")))
    (unwind-protect
        (progn
          (test-mevedel-tool-fs--write-bytes
           tmp test-mevedel-tool-fs--png-bytes)
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (mime &optional _model)
                       (equal mime "image/png")))
                    ((symbol-function
                      'mevedel-tool-fs--native-media-backend-p)
                     (lambda () t)))
            (let ((result (mevedel-tool-fs--read-file
                           (list :file_path tmp :offset 0 :limit 2000
                                 :pages ""))))
              (should (listp result))
              (should (equal (plist-get (car (plist-get result :media)) :mime)
                             "image/png")))))
      (delete-file tmp)))
  :doc "rejects nonzero text ranges for media reads"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".png")))
    (unwind-protect
        (progn
          (test-mevedel-tool-fs--write-bytes
           tmp test-mevedel-tool-fs--png-bytes)
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (mime &optional _model)
                       (equal mime "image/png"))))
            (let ((err (should-error
                        (mevedel-tool-fs--read-file
                         (list :file_path tmp :offset 1))
                        :type 'error)))
              (should (string-match-p "offset and limit"
                                      (cadr err))))))
      (delete-file tmp)))
  :doc "rejects media reads when current model cannot accept media"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".jpg" "jpg-bytes")))
    (unwind-protect
        (cl-letf (((symbol-function 'gptel--model-capable-p)
                   (lambda (&rest _) nil)))
          (let ((err (should-error
                      (mevedel-tool-fs--read-file (list :file_path tmp))
                      :type 'error)))
            (should (string-match-p "does not support media"
                                    (cadr err)))))
      (delete-file tmp)))
  :doc "rejects media reads when current model cannot accept MIME type"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (cl-letf (((symbol-function 'gptel--model-capable-p)
                   (lambda (cap &optional _model) (eq cap 'media)))
                  ((symbol-function 'gptel--model-mime-capable-p)
                   (lambda (_mime &optional _model) nil)))
          (let ((err (should-error
                      (mevedel-tool-fs--read-file (list :file_path tmp))
                      :type 'error)))
            (should (string-match-p "does not support media type application/pdf"
                                    (cadr err)))))
      (delete-file tmp)))
  :doc "falls back to textual media envelope when backend cannot serialize media"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".png")))
    (unwind-protect
        (progn
          (test-mevedel-tool-fs--write-bytes
           tmp test-mevedel-tool-fs--png-bytes)
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (_mime &optional _model) t))
                    ((symbol-function
                      'mevedel-tool-fs--native-media-backend-p)
                     (lambda () nil)))
            (let ((result (mevedel-tool-fs--read-file (list :file_path tmp))))
              (should (listp result))
              (should (string-match-p "<media-file>"
                                      (plist-get result :result)))
              (should (string-match-p "mime_type: image/png"
                                      (plist-get result :result)))
              (should (string-match-p "encoding: base64"
                                      (plist-get result :result)))
              (should (equal (plist-get (car (plist-get result :media)) :mime)
                             "image/png")))))
      (delete-file tmp)))
  :doc "checks transformed image MIME instead of source MIME"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".webp"))
        (prepared (make-temp-file "mevedel-test-prepared-" nil ".jpg")))
    (unwind-protect
        (progn
          (test-mevedel-tool-fs--write-bytes
           tmp (unibyte-string ?R ?I ?F ?F 0 0 0 0 ?W ?E ?B ?P))
          (test-mevedel-tool-fs--write-bytes
           prepared (unibyte-string #xff #xd8 #xff))
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (mime &optional _model)
                       (equal mime "image/jpeg")))
                    ((symbol-function
                      'mevedel-tool-fs--native-media-backend-p)
                     (lambda () t))
                    ((symbol-function 'mevedel-tool-fs--maybe-transform-media)
                     (lambda (_path _args)
                       (cons prepared "image/jpeg"))))
            (let ((result (mevedel-tool-fs--read-file
                           (list :file_path tmp :max_tokens 512))))
              (should (listp result))
              (should (string-match-p "mime_type: image/jpeg"
                                      (plist-get result :result)))
              (should (equal (plist-get (car (plist-get result :media)) :mime)
                             "image/jpeg")))))
      (delete-file tmp)
      (delete-file prepared)))
  :doc "rejects media reads when contents do not match extension"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".png" "not-a-png")))
    (unwind-protect
        (cl-letf (((symbol-function 'gptel--model-capable-p)
                   (lambda (cap &optional _model) (eq cap 'media)))
                  ((symbol-function 'gptel--model-mime-capable-p)
                   (lambda (_mime &optional _model) t))
                  ((symbol-function
                    'mevedel-tool-fs--native-media-backend-p)
                   (lambda () t)))
          (let ((err (should-error
                      (mevedel-tool-fs--read-file (list :file_path tmp))
                      :type 'error)))
            (should (string-match-p "contents do not match media type"
                                    (cadr err)))))
      (delete-file tmp)))
  :doc "rejects image transform arguments on full PDF reads"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (cl-letf (((symbol-function 'gptel--model-capable-p)
                   (lambda (cap &optional _model) (eq cap 'media)))
                  ((symbol-function 'gptel--model-mime-capable-p)
                   (lambda (_mime &optional _model) t)))
          (let ((err (should-error
                      (mevedel-tool-fs--read-file
                       (list :file_path tmp :max_width 1000))
                      :type 'error)))
            (should (string-match-p "PDF page images" (cadr err)))))
      (delete-file tmp)))
  :doc "errors when PDF page extraction needs missing pdftoppm"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (let ((orig-executable-find (symbol-function 'executable-find)))
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (_mime &optional _model) t))
                    ((symbol-function
                      'mevedel-tool-fs--native-media-backend-p)
                     (lambda () t))
                    ((symbol-function 'executable-find)
                     (lambda (cmd)
                       (and (not (equal cmd "pdftoppm"))
                            (funcall orig-executable-find cmd)))))
            (let ((err (should-error
                        (mevedel-tool-fs--read-file
                         (list :file_path tmp :pages "1"))
                        :type 'error)))
              (should (string-match-p "poppler-utils" (cadr err))))))
      (delete-file tmp)))
  :doc "caps aggregate base64 payload for rendered PDF pages"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (let ((mevedel-tool-fs--pdf-pages-max-base64-chars 8))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (cmd) (and (equal cmd "pdftoppm") t)))
                    ((symbol-function 'call-process)
                     (lambda (_program &rest args)
                       (let ((prefix (car (last args))))
                         (test-mevedel-tool-fs--write-bytes
                          (concat prefix ".png")
                          test-mevedel-tool-fs--png-bytes))
                       0))
                    ((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (_mime &optional _model) t))
                    ((symbol-function
                      'mevedel-tool-fs--native-media-backend-p)
                     (lambda () t))
                    ((symbol-function 'mevedel-tool-fs--base64-file)
                     (lambda (&rest _) "aaaaaaaaa")))
            (let ((err (should-error
                        (mevedel-tool-fs--read-file
                         (list :file_path tmp :pages "1"))
                        :type 'error)))
              (should (string-match-p "aggregate media size limit"
                                      (cadr err))))))
      (delete-file tmp)))
  :doc "file-not-found suggests same basename with different extension"
  (let* ((tmp-dir (make-temp-file "mevedel-missing-" t))
         (default-directory (file-name-as-directory tmp-dir))
         (actual (file-name-concat tmp-dir "notes.md"))
         (missing (file-name-concat tmp-dir "notes.txt")))
    (unwind-protect
        (progn
          (with-temp-file actual (insert "notes"))
          (let ((err (should-error
                      (mevedel-tool-fs--read-file
                       (list :file_path missing))
                      :type 'error)))
            (should (string-match-p "Did you mean" (cadr err)))
            (should (string-match-p (regexp-quote actual) (cadr err)))))
      (delete-directory tmp-dir t)))
  :doc "file-not-found suggests under-cwd correction"
  (let* ((parent (make-temp-file "mevedel-parent-" t))
         (repo (file-name-concat parent "repo"))
         (default-directory (file-name-as-directory repo))
         (actual (file-name-concat repo "src" "main.el"))
         (missing (file-name-concat parent "src" "main.el")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory actual) t)
          (with-temp-file actual (insert "content"))
          (let ((err (should-error
                      (mevedel-tool-fs--read-file
                       (list :file_path missing))
                      :type 'error)))
            (should (string-match-p "Did you mean" (cadr err)))
            (should (string-match-p (regexp-quote actual) (cadr err)))))
      (delete-directory parent t)))
  :doc "errors on blocked device path"
  (should-error
   (mevedel-tool-fs--read-file (list :file_path "/dev/zero"))
   :type 'error)
  :doc "returns system-reminder for empty file"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (let ((result (mevedel-tool-fs--read-file (list :file_path tmp))))
          (should (string-match-p "system-reminder" result))
          (should (string-match-p "empty" result)))
      (delete-file tmp)))
  :doc "truncates long lines"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (make-string 3000 ?x) "\nshort\n"))
          (let ((result (mevedel-tool-fs--read-file (list :file_path tmp))))
            (should (string-match-p "\\[\\.\\.\\.]" result))
            (should (string-match-p "2\tshort" result))))
      (delete-file tmp)))
  :doc "errors on oversized file without range"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (make-string (* 600 1024) ?x)))
          (should-error
           (mevedel-tool-fs--read-file (list :file_path tmp))
           :type 'error))
      (delete-file tmp)))
  :doc "reads oversized file with offset/limit"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (dotimes (i 100)
              (insert (format "line %d: %s\n" (1+ i) (make-string 6000 ?x)))))
          (let ((result (mevedel-tool-fs--read-file
                         (list :file_path tmp :offset 50 :limit 5))))
            (should (string-match-p "50\tline 50" result))
            (should (string-match-p "54\tline 54" result))))
      (delete-file tmp)))
  :doc "follows symlinks"
  (let ((tmp (make-temp-file "mevedel-test-"))
        (link (make-temp-file "mevedel-link-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "real content\n"))
          (delete-file link)
          (make-symbolic-link tmp link)
          (let ((result (mevedel-tool-fs--read-file (list :file_path link))))
            (should (string-match-p "real content" result))))
      (delete-file tmp)
      (when (file-exists-p link) (delete-file link))))
  :doc "records session interaction and workspace cache entry"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".txt" "hello world\n"))
         (ws (mevedel-workspace--create
              :type 'test :id "read-integration"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 5)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (mevedel-tool-fs--read-file (list :file_path tmp))
          (let ((entry (gethash (expand-file-name tmp)
                                (mevedel-session-touched-files session))))
            (should entry)
            (should (= 5 (mevedel-file-interaction-read-turn entry))))
          (should (mevedel-file-cache-get
                   (mevedel-workspace-file-cache ws) tmp)))
      (delete-file tmp)))
  :doc "returns stub on duplicate media read when mtime unchanged"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".png"))
         (ws (mevedel-workspace--create
              :type 'test :id "read-media-dedup"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (progn
          (test-mevedel-tool-fs--write-bytes
           tmp test-mevedel-tool-fs--png-bytes)
          (with-temp-buffer
            (setq-local mevedel--session session)
            (cl-letf (((symbol-function 'gptel--model-capable-p)
                       (lambda (cap &optional _model) (eq cap 'media)))
                      ((symbol-function 'gptel--model-mime-capable-p)
                       (lambda (_mime &optional _model) t))
                      ((symbol-function
                        'mevedel-tool-fs--native-media-backend-p)
                       (lambda () t)))
              (let ((first (mevedel-tool-fs--read-file
                            (list :file_path tmp))))
                (should (listp first))
                (should (plist-get first :media)))
              (let ((second (mevedel-tool-fs--read-file
                             (list :file_path tmp))))
                (should (string-match-p "unchanged since last read"
                                        second))
                (should-not (string-match-p "<media-file>" second))))))
      (delete-file tmp)))
  :doc "returns stub on duplicate read when mtime unchanged"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".txt" "hello world\n"))
         (ws (mevedel-workspace--create
              :type 'test :id "read-dedup"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (let ((first (mevedel-tool-fs--read-file (list :file_path tmp))))
            (should (string-match-p "hello world" first)))
          (let ((second (mevedel-tool-fs--read-file (list :file_path tmp))))
            (should (string-match-p "unchanged since last read" second))
            (should-not (string-match-p "hello world" second))))
      (delete-file tmp)))
  :doc "sub-agent reads ignore parent-session dedup state"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".txt" "agent content\n"))
         (ws (mevedel-workspace--create
              :type 'test :id "read-agent-dedup"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (let ((first (mevedel-tool-fs--read-file (list :file_path tmp))))
            (should (string-match-p "agent content" first)))
          (let ((second (mevedel-tool-fs--read-file (list :file_path tmp))))
            (should (string-match-p "unchanged since last read" second)))
          (setq-local mevedel--agent-invocation t)
          (let ((agent-read (mevedel-tool-fs--read-file
                             (list :file_path tmp))))
            (should (string-match-p "agent content" agent-read))
            (should-not (string-match-p "unchanged since last read"
                                        agent-read)))
          (setq-local mevedel--agent-invocation nil)
          ;; The parent session still sees its own dedup state; the
          ;; agent read did not replace or clear it.
          (let ((after-agent (mevedel-tool-fs--read-file
                              (list :file_path tmp))))
            (should (string-match-p "unchanged since last read"
                                    after-agent))))
      (delete-file tmp)))
  :doc "does not dedupe after external modification"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".txt" "hello\n"))
         (ws (mevedel-workspace--create
              :type 'test :id "read-dedup-mtime"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (mevedel-tool-fs--read-file (list :file_path tmp))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file tmp (insert "goodbye\n"))
            (set-file-times tmp future))
          (let ((second (mevedel-tool-fs--read-file (list :file_path tmp))))
            (should (string-match-p "goodbye" second))
            (should-not (string-match-p "unchanged since last read" second))))
      (delete-file tmp)))
  :doc "does not dedupe when range differs"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".txt"
                              "a\nb\nc\nd\ne\nf\n"))
         (ws (mevedel-workspace--create
              :type 'test :id "read-dedup-range"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (mevedel-tool-fs--read-file
           (list :file_path tmp :offset 1 :limit 2))
          (let ((second (mevedel-tool-fs--read-file
                         (list :file_path tmp :offset 4 :limit 2))))
            (should (string-match-p "4\td" second))
            (should-not (string-match-p "unchanged since last read" second))))
      (delete-file tmp))))


;;
;;; Line number formatting

(mevedel-deftest mevedel-tool-fs--add-line-numbers ()
  ,test
  (test)
  :doc "adds correct line numbers starting at 1"
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (mevedel-tool-fs--add-line-numbers 1)
    (should (string-match-p "1\talpha" (buffer-string)))
    (should (string-match-p "2\tbeta" (buffer-string)))
    (should (string-match-p "3\tgamma" (buffer-string))))
  :doc "adds correct line numbers with offset"
  (with-temp-buffer
    (insert "alpha\nbeta\n")
    (mevedel-tool-fs--add-line-numbers 42)
    (should (string-match-p "42\talpha" (buffer-string)))
    (should (string-match-p "43\tbeta" (buffer-string))))
  :doc "truncates lines over 2000 characters"
  (with-temp-buffer
    (insert (make-string 3000 ?a) "\n")
    (mevedel-tool-fs--add-line-numbers 1)
    (should (string-match-p "\\[\\.\\.\\.]" (buffer-string)))
    (should (<= (length (buffer-string)) 2100))))


;;
;;; Directory listing helper

(mevedel-deftest mevedel-tool-fs--list-directory ()
  ,test
  (test)
  :doc "lists files in a directory, sorted by path"
  (let ((tmp-dir (make-temp-file "mevedel-list-" t)))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "alpha.txt") (insert "a"))
          (with-temp-file (file-name-concat tmp-dir "beta.txt") (insert "b"))
          (let* ((result (mevedel-tool-fs--list-directory tmp-dir))
                 (entries (car result)))
            (should (= 2 (length entries)))
            (should (member "alpha.txt" entries))
            (should (member "beta.txt" entries))
            (should (null (cdr result)))))
      (delete-directory tmp-dir t)))

  :doc "caps listing at max-entries and reports truncation"
  (let ((tmp-dir (make-temp-file "mevedel-list-" t)))
    (unwind-protect
        (progn
          (dotimes (i 5)
            (with-temp-file (file-name-concat tmp-dir (format "f%d.txt" i))
              (insert "x")))
          (let* ((result (mevedel-tool-fs--list-directory tmp-dir 3))
                 (entries (car result))
                 (truncated (cdr result)))
            (should (= 3 (length entries)))
            (should truncated)))
      (delete-directory tmp-dir t)))

  :doc "returns empty list for empty directory"
  (let ((tmp-dir (make-temp-file "mevedel-list-" t)))
    (unwind-protect
        (let ((result (mevedel-tool-fs--list-directory tmp-dir)))
          (should (null (car result)))
          (should (null (cdr result))))
      (delete-directory tmp-dir t)))

  :doc "errors on non-directory path"
  (let ((tmp (make-temp-file "mevedel-list-" nil ".txt" "x")))
    (unwind-protect
        (should-error (mevedel-tool-fs--list-directory tmp) :type 'error)
      (delete-file tmp))))


;;
;;; Glob handler

(mevedel-deftest mevedel-tool-fs--glob ()
  ,test
  (test)
  :doc "finds files matching pattern"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "foo.el")
            (insert "content"))
          (with-temp-file (file-name-concat tmp-dir "bar.el")
            (insert "content"))
          (with-temp-file (file-name-concat tmp-dir "baz.txt")
            (insert "content"))
          (mevedel-tool-fs--glob (lambda (r) (setq result r))
                                 (list :pattern "*.el" :path tmp-dir))
          (should (string-match-p "foo\\.el" result))
          (should (string-match-p "bar\\.el" result))
          (should-not (string-match-p "baz\\.txt" result)))
      (delete-directory tmp-dir t)))
  :doc "returns message when no files match"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (mevedel-tool-fs--glob (lambda (r) (setq result r))
                                 (list :pattern "*.xyz" :path tmp-dir))
          (should (string-match-p "No files found" result)))
      (delete-directory tmp-dir t)))
  :doc "errors on empty pattern"
  (should-error
   (mevedel-tool-fs--glob #'ignore (list :pattern ""))
   :type 'error)
  :doc "errors on non-existent path"
  (should-error
   (mevedel-tool-fs--glob #'ignore (list :pattern "*.el"
                                          :path "/nonexistent/dir"))
   :type 'error)
  :doc "defaults path to current directory"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (default-directory tmp-dir)
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "test.el")
            (insert "content"))
          (mevedel-tool-fs--glob (lambda (r) (setq result r))
                                 (list :pattern "*.el"))
          (should (string-match-p "test\\.el" result)))
      (delete-directory tmp-dir t)))
  :doc "limits output to 100 entries by default"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (dotimes (i 101)
            (with-temp-file (file-name-concat tmp-dir (format "f%03d.el" i))
              (insert "content")))
          (mevedel-tool-fs--glob (lambda (r) (setq result r))
                                 (list :pattern "*.el" :path tmp-dir))
          (should (= 101 (length (split-string result "\n" t))))
          (should (string-match-p "Results truncated (limit: 100)" result)))
      (delete-directory tmp-dir t))))


;;
;;; Grep handler

(mevedel-deftest mevedel-tool-fs--grep ()
  ,test
  (test)
  :doc "files_with_matches: returns matching file paths"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "match.el")
            (insert "hello world\n"))
          (with-temp-file (file-name-concat tmp-dir "nomatch.el")
            (insert "goodbye\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "hello"
                                        :path tmp-dir))
          (should (string-match-p "match\\.el" result))
          (should-not (string-match-p "nomatch\\.el" result)))
      (delete-directory tmp-dir t)))
  :doc "content mode: returns matching lines with headings"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "code.el")
            (insert "line one\nfind me\nline three\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "find me"
                                        :path tmp-dir
                                        :output_mode "content"))
          (should (string-match-p "2:find me" result)))
      (delete-directory tmp-dir t)))
  :doc "count mode: returns match count"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "data.txt")
            (insert "foo\nbar\nfoo\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "foo"
                                        :path tmp-dir
                                        :output_mode "count"))
          (should (string-match-p ":2" result)))
      (delete-directory tmp-dir t)))
  :doc "returns no-matches message for exit code 1"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "empty.txt")
            (insert "nothing here\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "zzzznotfound"
                                        :path tmp-dir))
          (should (string-match-p "No matches found" result)))
      (delete-directory tmp-dir t)))
  :doc "errors on non-readable path"
  (should-error
   (mevedel-tool-fs--grep #'ignore (list :pattern "test"
                                          :path "/nonexistent/dir"))
   :type 'error)
  :doc "glob filter restricts file types"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "code.el")
            (insert "target\n"))
          (with-temp-file (file-name-concat tmp-dir "notes.txt")
            (insert "target\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "target"
                                        :path tmp-dir
                                        :glob "*.el"))
          (should (string-match-p "code\\.el" result))
          (should-not (string-match-p "notes\\.txt" result)))
      (delete-directory tmp-dir t)))
  :doc "case insensitive search"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "test.txt")
            (insert "Hello World\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "hello"
                                        :path tmp-dir
                                        :-i t))
          (should (string-match-p "test\\.txt" result)))
      (delete-directory tmp-dir t)))
  :doc "head_limit truncates output"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (dotimes (i 10)
            (with-temp-file (file-name-concat tmp-dir (format "f%d.txt" i))
              (insert "match\n")))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "match"
                                        :path tmp-dir
                                        :head_limit 3))
          (should (string-match-p "Results truncated" result))
          ;; Count non-empty, non-truncation lines
          (let ((lines (seq-filter
                        (lambda (l)
                          (and (not (string-empty-p l))
                               (not (string-match-p "truncated" l))))
                        (split-string result "\n"))))
            (should (= 3 (length lines)))))
      (delete-directory tmp-dir t)))
  :doc "offset skips initial results"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result-full nil)
         (result-offset nil))
    (unwind-protect
        (progn
          (dotimes (i 5)
            (with-temp-file (file-name-concat tmp-dir (format "f%d.txt" i))
              (insert "match\n")))
          (mevedel-tool-fs--grep (lambda (r) (setq result-full r))
                                  (list :pattern "match"
                                        :path tmp-dir
                                        :head_limit 0))
          (mevedel-tool-fs--grep (lambda (r) (setq result-offset r))
                                  (list :pattern "match"
                                        :path tmp-dir
                                        :offset 2
                                        :head_limit 0))
          (let ((full-lines (seq-filter (lambda (l) (not (string-empty-p l)))
                                        (split-string result-full "\n")))
                (offset-lines (seq-filter (lambda (l) (not (string-empty-p l)))
                                          (split-string result-offset "\n"))))
            (should (= (- (length full-lines) 2) (length offset-lines)))))
      (delete-directory tmp-dir t)))
  :doc "context lines in content mode"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "ctx.txt")
            (insert "before\ntarget\nafter\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "target"
                                        :path tmp-dir
                                        :output_mode "content"
                                        :context 1))
          (should (string-match-p "before" result))
          (should (string-match-p "target" result))
          (should (string-match-p "after" result)))
      (delete-directory tmp-dir t)))
  :doc "multiline mode matches across lines"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "multi.txt")
            (insert "start\nend\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "start.end"
                                        :path tmp-dir
                                        :multiline t
                                        :output_mode "content"))
          (should (string-match-p "start" result)))
      (delete-directory tmp-dir t)))
  :doc "single file search"
  (let* ((tmp (make-temp-file "mevedel-test-"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "alpha\nbeta\ngamma\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "beta"
                                        :path tmp
                                        :output_mode "content"))
          (should (string-match-p "beta" result))
          (should-not (string-match-p "alpha" result)))
      (delete-file tmp)))

  :doc "empty :glob string is treated as nil, not passed to rg"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "code.el")
            (insert "target\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "target"
                                        :path tmp-dir
                                        :glob ""))
          (should (string-match-p "code\\.el" result))
          (should-not (string-match-p "Error" result)))
      (delete-directory tmp-dir t)))

  :doc "empty :type string is treated as nil, not passed to rg"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "code.el")
            (insert "target\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "target"
                                        :path tmp-dir
                                        :type ""))
          (should (string-match-p "code\\.el" result))
          (should-not (string-match-p "unrecognized file type" result)))
      (delete-directory tmp-dir t)))

  :doc "empty :output_mode falls back to default files_with_matches"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "m.el")
            (insert "target\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "target"
                                        :path tmp-dir
                                        :output_mode ""))
          ;; files_with_matches: prints the path, not the line content.
          (should (string-match-p "m\\.el" result))
          (should-not (string-match-p "target" result)))
      (delete-directory tmp-dir t)))

  :doc "empty :path falls back to default current directory"
  (let* ((default-directory (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat default-directory "p.el")
            (insert "target\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "target"
                                        :path ""))
          (should (string-match-p "p\\.el" result)))
      (delete-directory default-directory t)))

  :doc ":json-false context args are ignored, not passed as -A%d"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "c.el")
            (insert "line one\nfind me\nline three\n"))
          ;; Without the integer guard these would crash `format'.
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "find me"
                                        :path tmp-dir
                                        :output_mode "content"
                                        :-A :json-false
                                        :-B :json-false
                                        :-C :json-false))
          (should (string-match-p "find me" result))
          (should-not (string-match-p "Error" result)))
      (delete-directory tmp-dir t)))

  :doc "non-integer :context is ignored"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "q.el")
            (insert "hit\n"))
          (mevedel-tool-fs--grep (lambda (r) (setq result r))
                                  (list :pattern "hit"
                                        :path tmp-dir
                                        :output_mode "content"
                                        :context "5"))
          (should (string-match-p "hit" result))
          (should-not (string-match-p "Error" result)))
      (delete-directory tmp-dir t))))

;;
;;; Write handler

(mevedel-deftest mevedel-tool-fs--write ()
  ,test
  (test)
  :doc "errors on missing file_path"
  (should-error
   (mevedel-tool-fs--write #'ignore (list :content "hello"))
   :type 'error)
  :doc "errors on missing content"
  (should-error
   (mevedel-tool-fs--write #'ignore (list :file_path "/tmp/test.txt"))
   :type 'error)
  :doc "creates parent directories"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (nested-path (file-name-concat tmp-dir "a" "b" "c" "file.txt")))
    (unwind-protect
        (progn
          ;; The handler will create dirs, then call mevedel-preview-mode-add-preview
          ;; which will error because there's no chat buffer context. That's
          ;; fine -- we just want to verify the directories were created.
          (condition-case _err
              (mevedel-tool-fs--write #'ignore
                                      (list :file_path nested-path
                                            :content "hello"))
            (error nil))
          (should (file-directory-p (file-name-concat tmp-dir "a" "b" "c"))))
      (delete-directory tmp-dir t)))
  :doc "writes temp file with correct content"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (target (file-name-concat tmp-dir "output.txt"))
         (captured-temp nil))
    (unwind-protect
        (progn
          ;; Intercept add-preview to capture the temp file
          (cl-letf (((symbol-function 'mevedel-preview-mode-add-preview)
                     (lambda (&rest args)
                       (setq captured-temp
                             (with-temp-buffer
                               (insert-file-contents (plist-get args :temp-file))
                               (buffer-string))))))
            (mevedel-tool-fs--write #'ignore
                                    (list :file_path target
                                          :content "test content\n")))
          (should (equal captured-temp "test content\n")))
      (delete-directory tmp-dir t)))
  :doc "passes original content for existing file"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (target (file-name-concat tmp-dir "existing.txt"))
         (captured-original nil))
    (unwind-protect
        (progn
          (with-temp-file target (insert "old content\n"))
          (cl-letf (((symbol-function 'mevedel-preview-mode-add-preview)
                     (lambda (&rest args)
                       (setq captured-original (plist-get args :original-content)))))
            (mevedel-tool-fs--write #'ignore
                                    (list :file_path target
                                          :content "new content\n")))
          (should (equal captured-original "old content\n")))
      (delete-directory tmp-dir t)))
  :doc "passes nil original for new file"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (target (file-name-concat tmp-dir "new-file.txt"))
         (captured-original 'sentinel))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-preview-mode-add-preview)
                     (lambda (&rest args)
                       (setq captured-original (plist-get args :original-content)))))
            (mevedel-tool-fs--write #'ignore
                                    (list :file_path target
                                          :content "fresh\n")))
          (should (null captured-original)))
      (delete-directory tmp-dir t))))


;;
;;; MkDir handler

(mevedel-deftest mevedel-tool-fs--mkdir ()
  ,test
  (test)
  :doc "errors on missing path"
  (should-error
   (mevedel-tool-fs--mkdir #'ignore (list))
   :type 'error)
  :doc "creates a directory"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (target (file-name-concat tmp-dir "new-dir"))
         (result nil))
    (unwind-protect
        (progn
          (mevedel-tool-fs--mkdir (lambda (r) (setq result r))
                                  (list :path target))
          (should (file-directory-p target))
          (should (string-match-p "created" (plist-get result :result)))
          (should (eq 'mkdir (plist-get (plist-get result :render-data) :kind)))
          (should (eq t (plist-get (plist-get result :render-data)
                                    :created)))
          (should (equal target (plist-get (plist-get result :render-data)
                                            :path))))
      (delete-directory tmp-dir t)))
  :doc "creates nested directories"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (target (file-name-concat tmp-dir "a" "b" "c"))
         (result nil))
    (unwind-protect
        (progn
          (mevedel-tool-fs--mkdir (lambda (r) (setq result r))
                                  (list :path target))
          (should (file-directory-p target))
          (should (string-match-p "created" (plist-get result :result)))
          (should (eq t (plist-get (plist-get result :render-data)
                                    :created))))
      (delete-directory tmp-dir t)))
  :doc "succeeds on existing directory (idempotent)"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (mevedel-tool-fs--mkdir (lambda (r) (setq result r))
                                  (list :path tmp-dir))
          (should (file-directory-p tmp-dir))
          (should (string-match-p "already exists" (plist-get result :result)))
          (should (null (plist-get (plist-get result :render-data)
                                    :created))))
      (delete-directory tmp-dir t)))
  :doc "records workspace-relative path when workspace is available"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (target (file-name-concat tmp-dir "src" "new-dir"))
         (workspace
          (mevedel-workspace--create
           :type 'test :id tmp-dir :root tmp-dir :name "mkdir-test"))
         (result nil))
    (unwind-protect
        (cl-progv '(mevedel--workspace) (list workspace)
          (mevedel-tool-fs--mkdir (lambda (r) (setq result r))
                                  (list :path target))
          (should (equal "src/new-dir"
                         (plist-get (plist-get result :render-data)
                                    :rel-path))))
      (delete-directory tmp-dir t))))


;;
;;; Edit handler

(mevedel-deftest mevedel-tool-fs--edit ()
  ,test
  (test)
  :doc "errors on missing file_path"
  (should-error
   (mevedel-tool-fs--edit #'ignore (list :old_string "a" :new_string "b"))
   :type 'error)
  :doc "errors on missing old_string"
  (should-error
   (mevedel-tool-fs--edit #'ignore (list :file_path "/tmp/x" :new_string "b"))
   :type 'error)
  :doc "errors on missing new_string"
  (should-error
   (mevedel-tool-fs--edit #'ignore (list :file_path "/tmp/x" :old_string "a"))
   :type 'error)
  :doc "errors on non-existent file"
  (should-error
   (mevedel-tool-fs--edit #'ignore (list :file_path "/nonexistent/f.txt"
                                          :old_string "a" :new_string "b"))
   :type 'error)
  :doc "errors on directory path"
  (should-error
   (mevedel-tool-fs--edit #'ignore (list :file_path "/tmp"
                                          :old_string "a" :new_string "b"))
   :type 'error)
  :doc "errors when old_string equals new_string"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "content\n"))
          (should-error
           (mevedel-tool-fs--edit #'ignore
                                   (list :file_path tmp
                                         :old_string "content"
                                         :new_string "content"))
           :type 'error))
      (delete-file tmp)))
  :doc "delegates to mevedel-preview-mode-add-preview on success"
  (let* ((tmp (make-temp-file "mevedel-test-"))
         (captured-temp nil)
         (captured-original nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "hello world\n"))
          (cl-letf (((symbol-function 'mevedel-preview-mode-add-preview)
                     (lambda (&rest args)
                       (setq captured-temp
                             (with-temp-buffer
                               (insert-file-contents (plist-get args :temp-file))
                               (buffer-string)))
                       (setq captured-original (plist-get args :original-content)))))
            (mevedel-tool-fs--edit #'ignore
                                    (list :file_path tmp
                                          :old_string "hello"
                                          :new_string "goodbye")))
          (should (string-match-p "goodbye world" captured-temp))
          (should (equal captured-original "hello world\n")))
      (delete-file tmp))))


;;
;;; String replacement

(mevedel-deftest mevedel-tool-fs--apply-string-replacement ()
  ,test
  (test)
  :doc "replaces unique match"
  (let ((tmp (make-temp-file "mevedel-test-"))
        (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "alpha beta gamma\n"))
          (mevedel-tool-fs--apply-string-replacement
           tmp "beta" "BETA" nil
           (lambda (r) (setq result r)))
          (should (eq result t))
          (should (equal (with-temp-buffer
                           (insert-file-contents tmp)
                           (buffer-string))
                         "alpha BETA gamma\n")))
      (delete-file tmp)))
  :doc "errors on non-unique match"
  (let ((tmp (make-temp-file "mevedel-test-"))
        (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "aaa bbb aaa\n"))
          (mevedel-tool-fs--apply-string-replacement
           tmp "aaa" "xxx" nil
           (lambda (r) (setq result r)))
          (should (stringp result))
          (should (string-match-p "not unique" result)))
      (delete-file tmp)))
  :doc "errors on missing match"
  (let ((tmp (make-temp-file "mevedel-test-"))
        (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "hello\n"))
          (mevedel-tool-fs--apply-string-replacement
           tmp "world" "earth" nil
           (lambda (r) (setq result r)))
          (should (stringp result))
          (should (string-match-p "Could not find" result)))
      (delete-file tmp)))
  :doc "replace_all replaces all occurrences"
  (let ((tmp (make-temp-file "mevedel-test-"))
        (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "foo bar foo baz foo\n"))
          (mevedel-tool-fs--apply-string-replacement
           tmp "foo" "qux" t
           (lambda (r) (setq result r)))
          (should (eq result t))
          (should (equal (with-temp-buffer
                           (insert-file-contents tmp)
                           (buffer-string))
                         "qux bar qux baz qux\n")))
      (delete-file tmp)))
  :doc "replace_all errors when string not found"
  (let ((tmp (make-temp-file "mevedel-test-"))
        (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "hello\n"))
          (mevedel-tool-fs--apply-string-replacement
           tmp "zzz" "yyy" t
           (lambda (r) (setq result r)))
          (should (stringp result))
          (should (string-match-p "Could not find" result)))
      (delete-file tmp)))
  :doc "multiline replacement"
  (let ((tmp (make-temp-file "mevedel-test-"))
        (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "line1\nline2\nline3\n"))
          (mevedel-tool-fs--apply-string-replacement
           tmp "line1\nline2" "replaced" nil
           (lambda (r) (setq result r)))
          (should (eq result t))
          (should (equal (with-temp-buffer
                           (insert-file-contents tmp)
                           (buffer-string))
                         "replaced\nline3\n")))
      (delete-file tmp)))
  :doc "preserves backslashes in new_string"
  (let ((tmp (make-temp-file "mevedel-test-"))
        (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "old\n"))
          (mevedel-tool-fs--apply-string-replacement
           tmp "old" "path\\to\\file" nil
           (lambda (r) (setq result r)))
          (should (eq result t))
          (should (equal (with-temp-buffer
                           (insert-file-contents tmp)
                           (buffer-string))
                         "path\\to\\file\n")))
      (delete-file tmp)))
  :doc "empty new_string strips trailing newline"
  (let ((tmp (make-temp-file "mevedel-test-"))
        (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "keep\nremove\nkeep\n"))
          (mevedel-tool-fs--apply-string-replacement
           tmp "remove" "" nil
           (lambda (r) (setq result r)))
          (should (eq result t))
          (should (equal (with-temp-buffer
                           (insert-file-contents tmp)
                           (buffer-string))
                         "keep\nkeep\n")))
      (delete-file tmp)))
  :doc ":json-false is treated as not replace_all"
  (let ((tmp (make-temp-file "mevedel-test-"))
        (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "aaa bbb aaa\n"))
          (mevedel-tool-fs--apply-string-replacement
           tmp "aaa" "xxx" :json-false
           (lambda (r) (setq result r)))
          ;; Should fail because not unique and replace_all is :json-false
          (should (stringp result))
          (should (string-match-p "not unique" result)))
      (delete-file tmp))))


;;
;;; Diff renderer

(mevedel-deftest mevedel-tool-fs--count-diff-changes ()
  ,test
  (test)
  :doc "counts added and removed lines, skipping unified-diff headers"
  (let* ((patch "--- a/foo\n+++ b/foo\n@@ -1,1 +1,1 @@\n-old line\n+new line\n")
         (counts (mevedel-tool-fs--count-diff-changes patch)))
    (should (equal 1 (car counts)))
    (should (equal 1 (cdr counts))))
  :doc "counts multiple additions and removals"
  (let* ((patch "@@ @@\n-a\n-b\n+c\n+d\n+e\n")
         (counts (mevedel-tool-fs--count-diff-changes patch)))
    (should (equal 3 (car counts)))
    (should (equal 2 (cdr counts))))
  :doc "non-string patch yields (0 . 0)"
  (should (equal '(0 . 0) (mevedel-tool-fs--count-diff-changes nil)))
  :doc "empty patch yields (0 . 0)"
  (should (equal '(0 . 0) (mevedel-tool-fs--count-diff-changes ""))))

(mevedel-deftest mevedel-tool-fs--render-diff-summary ()
  ,test
  (test)
  :doc "returns nil when render-data is absent"
  (should (null (mevedel-tool-fs--render-diff-summary
                 "Edit" '(:file_path "/x") "ok" nil)))
  :doc "returns nil when render-data is malformed (missing :kind diff)"
  (should (null (mevedel-tool-fs--render-diff-summary
                 "Edit" nil "ok" '(:kind something :patch "p"))))
  :doc "returns nil when :patch is not a string"
  (should (null (mevedel-tool-fs--render-diff-summary
                 "Edit" nil "ok" '(:kind diff :patch nil))))
  :doc "returns a valid rendering plist for Edit"
  (let* ((data '(:kind diff
                 :patch "--- a\n+++ b\n@@ @@\n-old\n+new\n"
                 :path "/tmp/foo.el"
                 :rel-path "foo.el"))
         (plist (mevedel-tool-fs--render-diff-summary
                 "Edit" '(:file_path "/tmp/foo.el") "ok" data)))
    (should (stringp (plist-get plist :header)))
    (should (string-match-p "\\`Edit: foo.el " (plist-get plist :header)))
    (should (string-match-p "+1" (plist-get plist :header)))
    (should (string-match-p "-1" (plist-get plist :header)))
    (should (equal (plist-get data :patch) (plist-get plist :body)))
    (should (eq 'diff-mode (plist-get plist :body-mode))))
  :doc "falls back to args :file_path basename when :rel-path absent"
  (let* ((data '(:kind diff :patch "@@ @@\n+a\n" :path nil :rel-path nil))
         (plist (mevedel-tool-fs--render-diff-summary
                 "Write" '(:file_path "/tmp/other.el") "ok" data)))
    (should (stringp (plist-get plist :header)))
    (should (string-match-p "Write: other.el " (plist-get plist :header))))
  :doc "uses tool name in header"
  (let* ((data '(:kind diff :patch "@@ @@\n+a\n"
                 :path "/tmp/x" :rel-path "x"))
         (plist (mevedel-tool-fs--render-diff-summary
                 "Write" nil "ok" data)))
    (should (string-prefix-p "Write: " (plist-get plist :header))))

  :doc "default permission mode: summary starts collapsed (user already saw the preview)"
  (let* ((mevedel-permission-mode 'default)
         (data '(:kind diff :patch "@@ @@\n+a\n"
                 :path "/tmp/x" :rel-path "x"))
         (plist (mevedel-tool-fs--render-diff-summary
                 "Edit" nil "ok" data)))
    (should (eq t (plist-get plist :initially-collapsed-p))))

  :doc "plan permission mode: summary starts collapsed"
  (let* ((mevedel-permission-mode 'plan)
         (data '(:kind diff :patch "@@ @@\n+a\n"
                 :path "/tmp/x" :rel-path "x"))
         (plist (mevedel-tool-fs--render-diff-summary
                 "Edit" nil "ok" data)))
    (should (eq t (plist-get plist :initially-collapsed-p))))

  :doc "accept-edits: summary starts collapsed"
  (let* ((mevedel-permission-mode 'accept-edits)
         (data '(:kind diff :patch "@@ @@\n+a\n"
                 :path "/tmp/x" :rel-path "x"))
         (plist (mevedel-tool-fs--render-diff-summary
                 "Edit" nil "ok" data)))
    (should (eq t (plist-get plist :initially-collapsed-p))))

  :doc "trust-all: summary starts collapsed"
  (let* ((mevedel-permission-mode 'trust-all)
         (data '(:kind diff :patch "@@ @@\n+a\n"
                 :path "/tmp/x" :rel-path "x"))
         (plist (mevedel-tool-fs--render-diff-summary
                 "Edit" nil "ok" data)))
    (should (eq t (plist-get plist :initially-collapsed-p)))))

(mevedel-deftest mevedel-tool-fs--render-read ()
  ,test
  (test)
  :doc "returns nil for non-string result"
  (should (null (mevedel-tool-fs--render-read "Read" '(:file_path "/x.el") nil nil)))

  :doc "header shows basename and line count; body-mode from extension"
  (let* ((body "1->foo\n2->bar\n3->baz\n")
         (plist (mevedel-tool-fs--render-read
                 "Read" '(:file_path "/home/roland/proj/hello.el") body nil)))
    (should (string-match-p "\\`Read: hello\\.el " (plist-get plist :header)))
    (should (equal body (plist-get plist :body)))
    (should (eq 'emacs-lisp-mode (plist-get plist :body-mode))))

  :doc "header shows workspace-relative root file"
  (let* ((root (make-temp-file "mevedel-read-root-" t))
         (file (file-name-concat root "mevedel-skills.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "read-root"
                     :root root :name "read-root"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (let ((plist (mevedel-tool-fs--render-read
                          "Read" `(:file_path ,file) "1->root\n" nil)))
              (should (string-match-p "\\`Read: mevedel-skills\\.el "
                                      (plist-get plist :header))))))
      (delete-directory root t)))

  :doc "header shows workspace-relative subdirectory file"
  (let* ((root (make-temp-file "mevedel-read-subdir-" t))
         (file (file-name-concat root "test/test-mevedel-skills.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "read-subdir"
                     :root root :name "read-subdir"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert "subdir\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (let ((plist (mevedel-tool-fs--render-read
                          "Read" `(:file_path ,file) "1->subdir\n" nil)))
              (should (string-match-p
                       "\\`Read: test/test-mevedel-skills\\.el "
                       (plist-get plist :header))))))
      (delete-directory root t)))

  :doc "body-mode is nil when file has no recognized extension"
  (let* ((plist (mevedel-tool-fs--render-read
                 "Read" '(:file_path "/tmp/no-extension-here") "x\n" nil)))
    (should (null (plist-get plist :body-mode))))

  :doc "body-mode is nil for media files"
  (let* ((plist (mevedel-tool-fs--render-read
                 "Read" '(:file_path "/tmp/screenshot.png")
                 "<media-file>\n...\n</media-file>" nil)))
    (should (null (plist-get plist :body-mode)))))

(mevedel-deftest mevedel-tool-fs--render-grep ()
  ,test
  (test)
  :doc "returns nil for non-string result"
  (should (null (mevedel-tool-fs--render-grep "Grep" '(:pattern "p") nil nil)))

  :doc "header includes pattern and match count; body-mode is grep-mode"
  (let* ((body "file1.el:10:match1\nfile1.el:22:match2\nfile2.el:3:match3\n")
         (plist (mevedel-tool-fs--render-grep
                 "Grep" '(:pattern "foo") body nil)))
    (should (string-match-p "\\`Grep: foo " (plist-get plist :header)))
    (should (string-match-p "3 matches" (plist-get plist :header)))
    (should (eq 'grep-mode (plist-get plist :body-mode))))

  :doc "no matches sentinel shows 0 matches, not 1"
  (let* ((plist (mevedel-tool-fs--render-grep
                 "Grep" '(:pattern "foo") "No matches found" nil)))
    (should (string-match-p "0 matches" (plist-get plist :header))))

  :doc "error message shows 0 matches, not 1"
  (let* ((plist (mevedel-tool-fs--render-grep
                 "Grep" '(:pattern "foo") "Error: search failed (exit code 2)\n\n" nil)))
    (should (string-match-p "0 matches" (plist-get plist :header)))))

(mevedel-deftest mevedel-tool-fs--render-glob ()
  ,test
  (test)
  :doc "returns nil for non-string result"
  (should (null (mevedel-tool-fs--render-glob "Glob" '(:pattern "*.el") nil nil)))

  :doc "header includes pattern and file count"
  (let* ((body "a.el\nb.el\nc.el\n")
         (plist (mevedel-tool-fs--render-glob
                 "Glob" '(:pattern "*.el") body nil)))
    (should (string-match-p "\\`Glob: \\*\\.el " (plist-get plist :header)))
    (should (string-match-p "3 files" (plist-get plist :header))))

  :doc "no files sentinel shows 0 files, not 1"
  (let* ((plist (mevedel-tool-fs--render-glob
                 "Glob" '(:pattern "*.el") "No files found matching pattern" nil)))
    (should (string-match-p "0 files" (plist-get plist :header))))

  :doc "error message shows 0 files, not 1"
  (let* ((plist (mevedel-tool-fs--render-glob
                 "Glob" '(:pattern "*.el") "Error: glob failed (exit code 2)\n\n" nil)))
    (should (string-match-p "0 files" (plist-get plist :header))))

  :doc "truncation marker is not counted as a file"
  (let* ((plist (mevedel-tool-fs--render-glob
                 "Glob" '(:pattern "*.el")
                 "a.el\nb.el\n... Results truncated (limit: 2)" nil)))
    (should (string-match-p "2 files" (plist-get plist :header)))))

(mevedel-deftest mevedel-tool-fs--render-mkdir ()
  ,test
  (test)
  :doc "returns nil for non-string result"
  (should (null (mevedel-tool-fs--render-mkdir
                 "MkDir" '(:path "foo") nil nil)))

  :doc "header shows created for newly created directory"
  (let* ((plist (mevedel-tool-fs--render-mkdir
                 "MkDir" '(:path "/tmp/proj/foo")
                 "Directory created: /tmp/proj/foo"
                 '(:kind mkdir :created t
                   :path "/tmp/proj/foo" :rel-path "foo"))))
    (should (equal "MkDir: ./foo/ (created)" (plist-get plist :header)))
    (should (null (plist-get plist :body)))
    (should (eq t (plist-get plist :initially-collapsed-p))))

  :doc "header shows exists for idempotent directory"
  (let* ((plist (mevedel-tool-fs--render-mkdir
                 "MkDir" '(:path "/tmp/proj/foo")
                 "Directory already exists: /tmp/proj/foo"
                 '(:kind mkdir :created nil
                   :path "/tmp/proj/foo" :rel-path "foo"))))
    (should (equal "MkDir: ./foo/ (exists)" (plist-get plist :header))))

  :doc "header shows error for error result"
  (let* ((plist (mevedel-tool-fs--render-mkdir
                 "MkDir" '(:path "/tmp/proj/foo")
                 "Error creating directory /tmp/proj/foo: denied"
                 nil)))
    (should (equal "MkDir: /tmp/proj/foo/ (error)"
                   (plist-get plist :header))))

  :doc "uses :rel-path when present"
  (let* ((plist (mevedel-tool-fs--render-mkdir
                 "MkDir" '(:path "/tmp/proj/foo")
                 "Directory created: /tmp/proj/foo"
                 '(:kind mkdir :created t
                   :path "/tmp/proj/foo" :rel-path "src/foo"))))
    (should (equal "MkDir: src/foo/ (created)"
                   (plist-get plist :header))))

  :doc "detects exists from legacy result without render-data"
  (let* ((plist (mevedel-tool-fs--render-mkdir
                 "MkDir" '(:path "foo")
                 "Directory already exists: /tmp/proj/foo"
                 nil)))
    (should (equal "MkDir: foo/ (exists)" (plist-get plist :header)))))

(provide 'test-mevedel-tool-fs)
;;; test-mevedel-tool-fs.el ends here
