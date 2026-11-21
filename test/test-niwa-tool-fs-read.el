;;; test-niwa-tool-fs-read.el -- Tests for Read file-system tools -*- lexical-binding: t -*-

;;; Commentary:

;; Behavioral coverage for the Read text/media owner.

;;; Code:

(require 'gptel-request)
(require 'niwa-structs)
(require 'niwa-tool-fs-read)
(require 'niwa-pin)
(require 'niwa-agents)
(require 'niwa-execution)
(require 'niwa-sandbox)
(require 'niwa-reminders)
(require 'niwa-system)
(require 'niwa-view)
(require 'niwa-resource)
(require 'niwa-workspace)
(require 'niwa-file-state)
(require 'niwa-agent-control)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defun test-niwa-tool-fs-read--as-start-helper (run)
  "Return a `niwa-execution-start-helper' stub delivering RUN's result.
RUN takes the arguments of `niwa-execution-run-helper'."
  (lambda (callback &rest args)
    (funcall callback (apply run args))
    #'ignore))

(defun test-niwa-tool-fs-read--settle (body)
  "Run Read BODY, which takes a continuation, and return its settled result.
An error result is signalled, as a Read that failed before waiting does."
  (let (settled)
    (niwa-tool-fs-read--run body (lambda (value) (setq settled (list value))))
    (with-timeout (30 (ert-fail "Read never settled"))
      (while (not settled) (accept-process-output nil 0.02)))
    (let ((value (car settled)))
      (if (and (listp value) (eq 'error (plist-get value :status)))
          (error "%s" (string-remove-prefix "Error: " (plist-get value :result)))
        value))))

(defun test-niwa-tool-fs-read--file (args)
  "Return the settled result of reading ARGS."
  (test-niwa-tool-fs-read--settle
   (lambda (k) (niwa-tool-fs-read--file args k))))

(defun test-niwa-tool-fs-read--list (path &optional max-entries)
  "Return the settled listing of directory PATH, capped at MAX-ENTRIES."
  (test-niwa-tool-fs-read--settle
   (lambda (k) (niwa-tool-fs-read-list-directory path k max-entries))))

(defun test-niwa-tool-fs-read--eof-retry-args (text)
  "Extract the last-page Read arguments from EOF diagnostic TEXT."
  (should (string-match
           "Read(file_path=\\(\".*\"\\), offset=\\([0-9]+\\), limit=\\([0-9]+\\))"
           text))
  (list :file_path (car (read-from-string (match-string 1 text)))
        :offset (string-to-number (match-string 2 text))
        :limit (string-to-number (match-string 3 text))))

(defun test-niwa-tool-fs-read--assert-eof (path last-line last-text)
  "Assert PATH's observed LAST-LINE and follow its retry to LAST-TEXT."
  (dolist (offset (list (1+ last-line) 999))
    (let* ((failure (should-error
                     (niwa-test--read (list :file_path path :offset offset :limit 1))))
           (text (error-message-string failure))
           (retry (test-niwa-tool-fs-read--eof-retry-args text)))
      (should (string-search (format "Available lines in this read: 1–%d." last-line) text))
      (should (equal retry (list :file_path path :offset last-line :limit 1)))
      (should (string-search
               (format "%d\t%s" last-line last-text)
               (plist-get (niwa-test--read retry) :result))))))

(defmacro test-niwa-tool-fs-read--with-file (suffix content &rest body)
  "Run BODY with tmp bound to a temporary file with SUFFIX and CONTENT."
  (declare (indent 2) (debug t))
  `(let ((tmp (make-temp-file "niwa-test-" nil ,suffix ,content)))
     (unwind-protect (progn ,@body)
       (delete-file tmp))))

(niwa-deftest niwa-tool-fs-read--with-local-media-source ()
  ,test
  (test)
  :doc "keeps local converter inputs unchanged"
  (let ((source (make-temp-file "niwa-media-local-" nil ".png")))
    (unwind-protect
        (should (equal source
                       (niwa-tool-fs-read--with-local-media-source
                        source #'identity)))
      (delete-file source)))

  :doc "copies remote converter inputs locally and deletes the copy"
  (let ((source (make-temp-file "niwa-media-remote-" nil ".png"
                                "remote-media"))
        copied)
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("media")
                                              (let ((remote (format "/niwamock:media:%s" source)))
                                                (setq copied
                                                      (niwa-tool-fs-read--with-local-media-source
                                                       remote
                                                       (lambda (local)
                                                         (should-not (file-remote-p local))
                                                         (should (equal "remote-media"
                                                                        (with-temp-buffer
                                                                          (insert-file-contents local)
                                                                          (buffer-string))))
                                                         local)))
                                                (should-not (file-exists-p copied))))
      (delete-file source)))

  :doc "rejects oversized remote media before invoking the converter"
  (let ((source (make-temp-file "niwa-media-oversized-" nil ".pdf"
                                "oversized"))
        (niwa-tool-fs-read--remote-media-copy-max-bytes 1)
        called)
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("media")
                                              (should-error
                                               (niwa-tool-fs-read--with-local-media-source
                                                (format "/niwamock:media:%s" source)
                                                (lambda (_local) (setq called t))))
                                              (should-not called))
      (delete-file source))))

;;
;;; Binary extension detection

(niwa-deftest niwa-tool-fs-read--binary-extension-p ()
  ,test
  (test)
  :doc "detects common binary extensions"
  (should (niwa-tool-fs-read--binary-extension-p "image.png"))
  (should (niwa-tool-fs-read--binary-extension-p "archive.zip"))
  (should (niwa-tool-fs-read--binary-extension-p "binary.exe"))
  (should (niwa-tool-fs-read--binary-extension-p "doc.pdf"))
  :doc "rejects text file extensions"
  (should-not (niwa-tool-fs-read--binary-extension-p "code.el"))
  (should-not (niwa-tool-fs-read--binary-extension-p "readme.md"))
  (should-not (niwa-tool-fs-read--binary-extension-p "config.json"))
  (should-not (niwa-tool-fs-read--binary-extension-p "style.css"))
  :doc "case insensitive"
  (should (niwa-tool-fs-read--binary-extension-p "IMAGE.PNG"))
  (should (niwa-tool-fs-read--binary-extension-p "file.JPG"))
  :doc "handles files without extension"
  (should-not (niwa-tool-fs-read--binary-extension-p "Makefile"))
  (should-not (niwa-tool-fs-read--binary-extension-p ".gitignore")))

;;
;;; Media helpers

(defconst test-niwa-tool-fs-read--png-bytes
  (unibyte-string #x89 ?P ?N ?G ?\r ?\n #x1a ?\n)
  "Minimal PNG bytes used by Read media tests.")

(defun test-niwa-tool-fs-read--write-bytes (path bytes)
  "Write unibyte BYTES to PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (let ((coding-system-for-write 'binary))
      (write-region nil nil path nil 'silent))))

(niwa-deftest niwa-tool-fs-read--validate-media-file ()
  ,test
  (test)
  :doc "validates every supported signature and rejects empty or mismatched bytes"
  (dolist (entry `(("application/pdf" . "%PDF-")
                   ("image/png" . ,test-niwa-tool-fs-read--png-bytes)
                   ("image/jpeg" . ,(unibyte-string #xff #xd8 #xff))
                   ("image/gif" . "GIF87a")
                   ("image/gif" . "GIF89a")
                   ("image/webp" . "RIFFxxxxWEBP")))
    (test-niwa-tool-fs-read--with-file nil nil
      (test-niwa-tool-fs-read--write-bytes tmp (cdr entry))
      (should-not (niwa-tool-fs-read--validate-media-file tmp (car entry)))
      (dolist (bytes '("" "x" "RIFFxxxxNOPE" "NOPExxxxWEBP"))
        (test-niwa-tool-fs-read--write-bytes tmp bytes)
        (should-error
         (niwa-tool-fs-read--validate-media-file tmp (car entry)))))))

(niwa-deftest niwa-tool-fs-read-media-mime-type ()
  ,test
  (test)
  :doc "detects supported media MIME types"
  (should (equal "image/png" (niwa-tool-fs-read-media-mime-type "a.png")))
  (should (equal "image/jpeg" (niwa-tool-fs-read-media-mime-type "a.jpg")))
  (should (equal "image/jpeg" (niwa-tool-fs-read-media-mime-type "a.JPEG")))
  (should (equal "image/gif" (niwa-tool-fs-read-media-mime-type "a.gif")))
  (should (equal "image/webp" (niwa-tool-fs-read-media-mime-type "a.webp")))
  (should (equal "application/pdf"
                 (niwa-tool-fs-read-media-mime-type "doc.pdf")))
  :doc "returns nil for unsupported binary and text files"
  (should-not (niwa-tool-fs-read-media-mime-type "archive.zip"))
  (should-not (niwa-tool-fs-read-media-mime-type "readme.md")))

(niwa-deftest niwa-tool-fs-read--pdf-page-images-only-p ()
  ,test
  (test)
  :doc "applies to a model that takes images but not PDF documents"
  (cl-letf (((symbol-function 'gptel--model-mime-capable-p)
             (lambda (mime &optional _model) (equal mime "image/png"))))
    (should (niwa-tool-fs-read--pdf-page-images-only-p)))
  :doc "does not apply when the model takes PDF documents"
  (cl-letf (((symbol-function 'gptel--model-mime-capable-p)
             (lambda (_mime &optional _model) t)))
    (should-not (niwa-tool-fs-read--pdf-page-images-only-p)))
  :doc "does not apply when the model takes no images either"
  (cl-letf (((symbol-function 'gptel--model-mime-capable-p)
             (lambda (_mime &optional _model) nil)))
    (should-not (niwa-tool-fs-read--pdf-page-images-only-p))))

(niwa-deftest niwa-tool-fs-read--parse-pages ()
  ,test
  (test)
  :doc "parses single page"
  (should (equal '(3 . 3) (niwa-tool-fs-read--parse-pages "3")))
  :doc "parses closed range"
  (should (equal '(1 . 5) (niwa-tool-fs-read--parse-pages "1-5")))
  :doc "parses open range with 20-page cap"
  (should (equal '(3 . 22) (niwa-tool-fs-read--parse-pages "3-")))
  :doc "rejects invalid syntax"
  (should-error (niwa-tool-fs-read--parse-pages "x-y") :type 'error)
  :doc "rejects descending ranges"
  (should-error (niwa-tool-fs-read--parse-pages "5-2") :type 'error)
  :doc "rejects ranges over 20 pages"
  (should-error (niwa-tool-fs-read--parse-pages "1-21") :type 'error))

(niwa-deftest niwa-tool-fs-read--run ()
  ,test
  (test)
  :doc "settles once with an error raised after a helper, releasing copies"
  (let (delivered released pending)
    (cl-letf (((symbol-function 'niwa-execution-start-helper)
               (lambda (callback &rest _) (setq pending callback) #'ignore)))
      (niwa-tool-fs-read--run
       (lambda (_k)
         (push (lambda () (setq released t)) (car niwa-tool-fs-read--cleanups))
         (niwa-tool-fs-read--helper
          (lambda (_exit _output) (error "Converter exploded"))
          "helper" '("helper") nil))
       (lambda (value) (push value delivered))))
    (should-not delivered)
    (should-not released)
    (funcall pending '(:exit-code 0 :output ""))
    (should released)
    (should (equal '((:result "Error: Converter exploded" :status error))
                   delivered)))

  :doc "lets an error before any helper propagate to the pipeline"
  (let (released)
    (should-error
     (niwa-tool-fs-read--run
      (lambda (_k)
        (push (lambda () (setq released t)) (car niwa-tool-fs-read--cleanups))
        (error "Not a readable file"))
      (lambda (_) (ert-fail "No result for a synchronous error"))))
    (should released))

  :doc "continues in the Read's context once the helper settles"
  (let (pending seen)
    (cl-letf (((symbol-function 'niwa-execution-start-helper)
               (lambda (callback &rest _) (setq pending callback) #'ignore)))
      (with-temp-buffer
        (let ((buffer (current-buffer)))
          (niwa-tool-fs-read--run
           (lambda (k)
             (let ((niwa-tool-fs-read--resource-address "memory://notes"))
               (niwa-tool-fs-read--helper
                (lambda (exit output)
                  (setq seen (list exit output
                                   niwa-tool-fs-read--resource-address
                                   (eq buffer (current-buffer))))
                  (funcall k "done"))
                "helper" '("helper") nil)))
           #'ignore)
          (with-temp-buffer
            (funcall pending '(:exit-code 3 :output "out"))))))
    (should (equal '(3 "out" "memory://notes" t) seen))))

(niwa-deftest niwa-tool-fs-read--bounded-pdf-page-range ()
  ,test
  (test)
  :doc "limits open-ended ranges to the actual PDF page count"
  (should (equal '(1 . 1) (niwa-tool-fs-read--bounded-pdf-page-range "1-" 1)))
  :doc "limits closed ranges to the actual PDF page count"
  (should (equal '(1 . 1) (niwa-tool-fs-read--bounded-pdf-page-range "1-5" 1)))
  :doc "falls back to the 20-page cap when the page count is unavailable"
  (should (equal '(3 . 22) (niwa-tool-fs-read--bounded-pdf-page-range "3-" nil)))
  :doc "rejects ranges that start after the last page"
  (let ((err (should-error
              (niwa-tool-fs-read--bounded-pdf-page-range "3-4" 2)
              :type 'error)))
    (should (string-match-p "starts after last page" (cadr err)))))

(niwa-deftest niwa-tool-fs-read-large-pdf-p ()
  ,test
  (test)
  :doc "large when page count exceeds one Read page chunk"
  (let ((tmp (make-temp-file "niwa-large-pdf-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (should (niwa-tool-fs-read-large-pdf-p
                 tmp (1+ niwa-tool-fs-read--max-pages)))
      (delete-file tmp)))
  :doc "large when page count is unavailable but attachment size is high"
  (let ((tmp (make-temp-file "niwa-large-pdf-" nil ".pdf" "%PDF-1.4\npayload")))
    (unwind-protect
        (let ((niwa-tool-fs-read--large-attachment-reminder-bytes 1))
          (should (niwa-tool-fs-read-large-pdf-p tmp nil)))
      (delete-file tmp)))
  :doc "small PDFs do not trigger bounded-page guidance"
  (let ((tmp (make-temp-file "niwa-large-pdf-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (let ((niwa-tool-fs-read--large-attachment-reminder-bytes 1024))
          (should-not (niwa-tool-fs-read-large-pdf-p tmp 3)))
      (delete-file tmp))))

(niwa-deftest niwa-tool-fs-read--normalize-read-args ()
  ,test
  (test)
  :doc "treats empty pages and zero optional integers as absent"
  (let ((normalized (niwa-tool-fs-read--normalize-read-args
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
  (let ((normalized (niwa-tool-fs-read--normalize-read-args
                     '(:file_path "relative.png"
                       :offset 3 :limit 12 :max_height 900
                       :max_tokens 2000))))
    (should (equal (expand-file-name "relative.png")
                   (plist-get normalized :file_path)))
    (should (equal 3 (plist-get normalized :offset)))
    (should (equal 12 (plist-get normalized :limit)))
    (should (equal 900 (plist-get normalized :max_height)))
    (should (equal 2000 (plist-get normalized :max_tokens)))))

(niwa-deftest niwa-tool-fs-read--maybe-transform-media ()
  ,test
  (test)
  :doc "missing ImageMagick explains recovery and permits an original-image retry"
  (let ((path (make-temp-file "niwa-image-retry-" nil ".png")))
    (unwind-protect
        (progn
          (test-niwa-tool-fs-read--write-bytes
           path test-niwa-tool-fs-read--png-bytes)
          (let ((exec-path nil)
                (text-quoting-style 'straight))
            (dolist (args '((:max_width 64) (:max_height 64) (:max_tokens 100)))
              (let* ((called nil)
                     (err (should-error
                           (niwa-tool-fs-read--maybe-transform-media
                            path args (lambda (_) (setq called t)))
                           :type 'error)))
                (should-not called)
                (should
                 (equal (error-message-string err)
                        "Cannot resize/compress this image because ImageMagick is unavailable. Install 'magick' or 'convert', or retry Read without max_width, max_height, and max_tokens to read the original image"))))
            (should (equal (niwa-tool-fs-read--maybe-transform-media
                            path nil #'identity)
                           (cons path "image/png")))))
      (delete-file path))))

(niwa-deftest niwa-tool-fs-read--media-file ()
  ,test
  (test)
  :doc "remote image conversion uses temporary local input and output paths"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-remote-media-root-" t)))
        converter-input converter-output)
    (unwind-protect
        (progn
          (test-niwa-tool-fs-read--write-bytes
           (file-name-concat root "image.png")
           test-niwa-tool-fs-read--png-bytes)
          (niwa-test--with-local-shell-tramp '("media")
            (let* ((remote-root (format "/niwamock:media:%s" root))
                   (remote-path (file-name-concat remote-root "image.png"))
                   (workspace (niwa-workspace--create
                               :type 'project :id "remote-media"
                               :root remote-root :name "remote-media"))
                   (session (niwa-session-create "main" workspace))
                   (niwa--session session))
              (cl-letf
                  (((symbol-function 'gptel--model-capable-p)
                    (lambda (cap &optional _model) (eq cap 'media)))
                   ((symbol-function 'gptel--model-mime-capable-p)
                    (lambda (mime &optional _model)
                      (equal mime "image/png")))

                   ((symbol-function 'niwa-tool-fs-read--imagemagick-command)
                    (lambda () "magick"))
                   ((symbol-function 'niwa-execution-start-helper)
                    (test-niwa-tool-fs-read--as-start-helper
                     (lambda (_name command _read-paths _writable-roots
                                    &rest keys)
                       (setq converter-input (nth 1 command)
                             converter-output (car (last command)))
                       (should-not (file-remote-p converter-input))
                       (should-not (file-remote-p converter-output))
                       (should-not (plist-get keys :session))
                       (test-niwa-tool-fs-read--write-bytes
                        converter-output test-niwa-tool-fs-read--png-bytes)
                       '(:exit-code 0 :output "")))))
                (let* ((result (test-niwa-tool-fs-read--settle
                                (lambda (k)
                                  (niwa-tool-fs-read--media-file
                                   remote-path '(:max_width 64) k))))
                       (body (plist-get result :result))
                       (media (car (plist-get result :media))))
                  (should (string-match-p
                           (regexp-quote
                            (file-name-concat root "image.png"))
                           body))
                  (should-not (string-match-p "/niwamock:" body))
                  (should-not (string-match-p "niwa-media-" body))
                  (should (equal (file-name-concat root "image.png")
                                 (plist-get media :path)))))))
          (should-not (file-exists-p converter-input))
          (should-not (file-exists-p converter-output)))
      (delete-directory root t))))

(niwa-deftest niwa-tool-fs-read--pdf-pages ()
  ,test
  (test)
  :doc "remote PDF conversion uses local Poppler input and removes artifacts"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-remote-pdf-root-" t)))
        transient-paths)
    (unwind-protect
        (progn
          (write-region "%PDF-1.4\n" nil
                        (file-name-concat root "document.pdf") nil 'silent)
          (niwa-test--with-local-shell-tramp '("pdf")
            (let* ((remote-root (format "/niwamock:pdf:%s" root))
                   (remote-path
                    (file-name-concat remote-root "document.pdf"))
                   (workspace (niwa-workspace--create
                               :type 'project :id "remote-pdf"
                               :root remote-root :name "remote-pdf"))
                   (session (niwa-session-create "main" workspace))
                   (niwa--session session))
              (cl-letf
                  (((symbol-function 'executable-find)
                    (lambda (name &optional _remote)
                      (and (member name '("pdfinfo" "pdftoppm")) name)))
                   ((symbol-function 'gptel--model-capable-p)
                    (lambda (cap &optional _model) (eq cap 'media)))
                   ((symbol-function 'gptel--model-mime-capable-p)
                    (lambda (mime &optional _model)
                      (equal mime "image/png")))

                   ((symbol-function 'niwa-execution-start-helper)
                    (test-niwa-tool-fs-read--as-start-helper
                     (lambda (_name command _read-paths _writable-roots
                                    &rest keys)
                       (should-not (plist-get keys :session))
                       (pcase (car command)
                         ("pdfinfo"
                          (push (nth 1 command) transient-paths)
                          '(:exit-code 0 :output "Pages: 1\n"))
                         ("pdftoppm"
                          (let ((input (nth 7 command))
                                (output (concat (car (last command)) ".png")))
                            (should-not (file-remote-p input))
                            (push input transient-paths)
                            (push output transient-paths)
                            (test-niwa-tool-fs-read--write-bytes
                             output test-niwa-tool-fs-read--png-bytes)
                            '(:exit-code 0 :output "")))
                         (_ '(:exit-code 1 :output "unexpected")))))))
                (let* ((result (test-niwa-tool-fs-read--settle
                                (lambda (k)
                                  (niwa-tool-fs-read--pdf-pages
                                   remote-path "1" nil k))))
                       (body (plist-get result :result))
                       (media (car (plist-get result :media))))
                  (should (string-match-p
                           (regexp-quote
                            (file-name-concat root "document.pdf"))
                           body))
                  (should-not (string-match-p "/niwamock:" body))
                  (should (equal (file-name-concat root "document.pdf")
                                 (plist-get media :path)))))))
          (dolist (path transient-paths)
            (should-not (file-exists-p path))))
      (delete-directory root t)))
  :doc "tells the model how to continue when pages remain after the range"
  (test-niwa-tool-fs-read--with-file ".pdf" "%PDF-1.4\n"
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (and (member name '("pdfinfo" "pdftoppm")) name)))
              ((symbol-function 'gptel--model-capable-p)
               (lambda (cap &optional _model) (eq cap 'media)))
              ((symbol-function 'gptel--model-mime-capable-p)
               (lambda (mime &optional _model) (equal mime "image/png")))
              ((symbol-function 'niwa-execution-start-helper)
               (test-niwa-tool-fs-read--as-start-helper
                (lambda (_name command &rest _)
                  (pcase (car command)
                    ("pdfinfo" '(:exit-code 0 :output "Pages: 25\n"))
                    ("pdftoppm"
                     (test-niwa-tool-fs-read--write-bytes
                      (concat (car (last command)) ".png")
                      test-niwa-tool-fs-read--png-bytes)
                     '(:exit-code 0 :output ""))
                    (_ '(:exit-code 1 :output "unexpected")))))))
      (let ((body (plist-get (test-niwa-tool-fs-read--settle
                              (lambda (k)
                                (niwa-tool-fs-read--pdf-pages tmp "1-2" nil k)))
                             :result)))
        (should (string-match-p "Rendered pages 1-2 of 25" body))
        (should (string-match-p (regexp-quote "pages=\"3-\"") body))))))

(niwa-deftest niwa-tool-fs-read-pdf-page-images-now ()
  ,test
  (test)
  :doc "renders at most the page cap and removes its output directory"
  (test-niwa-tool-fs-read--with-file ".pdf" "%PDF-1.4\n"
    (let (pages directories)
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (name &optional _remote) (equal name "pdftoppm")))
                ((symbol-function 'niwa-execution-run-helper)
                 (lambda (_name command read-paths writable-roots &rest _)
                   (should (equal (list tmp) read-paths))
                   (push (car writable-roots) directories)
                   (push (string-to-number (nth 2 command)) pages)
                   (test-niwa-tool-fs-read--write-bytes
                    (concat (car (last command)) ".png")
                    test-niwa-tool-fs-read--png-bytes)
                   '(:exit-code 0 :output ""))))
        (let ((images (niwa-tool-fs-read-pdf-page-images-now tmp 25)))
          (should (= niwa-tool-fs-read--max-pages (length images)))
          (should (equal test-niwa-tool-fs-read--png-bytes (car images)))
          (should (equal (number-sequence 1 niwa-tool-fs-read--max-pages)
                         (nreverse pages)))
          (should-not (file-exists-p (car directories)))))))
  :doc "errors without a page count"
  (should-error (niwa-tool-fs-read-pdf-page-images-now "/tmp/x.pdf" nil)
                :type 'error)
  :doc "errors when rendered pages exceed the media size limit"
  (test-niwa-tool-fs-read--with-file ".pdf" "%PDF-1.4\n"
    (let ((niwa-tool-fs-read-media-max-bytes 1))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (name &optional _remote) (equal name "pdftoppm")))
                ((symbol-function 'niwa-execution-run-helper)
                 (lambda (_name command &rest _)
                   (test-niwa-tool-fs-read--write-bytes
                    (concat (car (last command)) ".png")
                    test-niwa-tool-fs-read--png-bytes)
                   '(:exit-code 0 :output ""))))
        (should-error (niwa-tool-fs-read-pdf-page-images-now tmp 2)
                      :type 'error)))))

;;
;;; Blocked device detection

(niwa-deftest niwa-tool-fs-read--blocked-device-p ()
  ,test
  (test)
  :doc "blocks dangerous device paths"
  (should (niwa-tool-fs-read--blocked-device-p "/dev/zero"))
  (should (niwa-tool-fs-read--blocked-device-p "/dev/random"))
  (should (niwa-tool-fs-read--blocked-device-p "/dev/stdin"))
  (should (niwa-tool-fs-read--blocked-device-p "/dev/fd/0"))
  :doc "blocks proc fd aliases"
  (should (niwa-tool-fs-read--blocked-device-p "/proc/self/fd/0"))
  (should (niwa-tool-fs-read--blocked-device-p "/proc/1234/fd/1"))
  :doc "allows safe devices and regular paths"
  (should-not (niwa-tool-fs-read--blocked-device-p "/dev/null"))
  (should-not (niwa-tool-fs-read--blocked-device-p "/tmp/file.txt"))
  (should-not (niwa-tool-fs-read--blocked-device-p "/proc/self/status")))

;;
;;; Read handler

(niwa-deftest niwa-tool-fs-read
  ()
  ,test
  (test)
  :doc "reads local and artifact resources without exposing backing paths"
  (let* ((save-path (make-temp-file "niwa-fs-resource-" t))
         (session (niwa-session--create
                   :save-path save-path))
         (local-root (file-name-concat save-path "local" "src"))
         (artifact-root (file-name-concat save-path "tool-results" "part one"))
         (local-file (file-name-concat local-root "notes.md"))
         (artifact-file (file-name-concat artifact-root "result.txt"))
         (local-address "work://src/notes.md")
         (artifact-address "artifact://part%20one/result.txt"))
    (unwind-protect
        (progn
          (make-directory local-root t)
          (make-directory artifact-root t)
          (with-temp-file local-file (insert "local note\n"))
          (with-temp-file artifact-file (insert "artifact result\n"))
          (cl-labels
              ((read-resource (address operation path)
                 (let* ((attempt (niwa-resource-prepare
                                  operation address (list :session session)))
                        (niwa-resource-current-attempts
                         (list (cons address attempt)))
                        (niwa--session session)
                        (result (plist-get
                                 (niwa-test--read
                                  (list :file_path address))
                                 :result)))
                   (should (string-match-p path result))
                   (should-not (string-match-p
                                (regexp-quote save-path) result))
                   result)))
            (dotimes (_ 2)
              (read-resource local-address 'read "local note")
              (read-resource artifact-address 'read "artifact result"))
            (should-not (niwa-session-touched-files session))
            (let* ((record (niwa-agent-record--create
                            :path "/root/reviewer" :role "reviewer"
                            :activity 'idle
                            :settled-result "alpha\nbeta\ngamma"
                            :settled-outcome 'completed))
                   (virtual-address "agent://root/reviewer")
                   virtual-attempt)
              (niwa-session--set-agent-registry
               session (list (cons "/root/reviewer" record)))
              (setq virtual-attempt
                    (niwa-resource-prepare
                     'read virtual-address (list :session session)))
              (let ((niwa-resource-current-attempts
                     (list (cons virtual-address virtual-attempt))))
                (let ((result
                       (plist-get
                        (niwa-test--read
                         (list :file_path virtual-address :offset 2 :limit 1))
                        :result)))
                  (should (string-match-p "2\11beta" result))
                  (should-not (string-match-p "alpha" result))
                  (should-not (string-match-p (regexp-quote save-path)
                                              result)))))))
      (delete-directory save-path t)))
  :doc "reads current installed documentation without a session or source path"
  (let* ((root (make-temp-file "niwa-read-installed-" t))
         (docs (file-name-concat root "docs"))
         (file (file-name-concat docs "guide.md"))
         (address "niwa://guide.md")
         (niwa-resource--source-dir root))
    (unwind-protect
        (progn
          (make-directory docs t)
          (with-temp-file (file-name-concat root "niwa-resource.el")
            (insert ";; Adjacent package source.\n"))
          (with-temp-file file (insert "old documentation\n"))
          (let* ((attempt (niwa-resource-prepare 'read address nil))
                 (niwa-resource-current-attempts
                  (list (cons address attempt))))
            (with-temp-file file (insert "current documentation\n"))
            (let ((result (plist-get
                           (niwa-test--read (list :file_path address))
                           :result)))
              (should (string-match-p "1\11current documentation" result))
              (should-not (string-match-p "old documentation" result))
              (should-not (string-match-p (regexp-quote root) result)))))
      (delete-directory root t)))
  :doc "retrieves installed tool manuals and fails when one is unavailable"
  (let* ((source niwa-resource--source-dir)
         (root (make-temp-file "niwa-read-manuals-" t))
         (docs (file-name-concat root "docs"))
         (niwa-resource--source-dir root))
    (unwind-protect
        (progn
          (make-directory (file-name-concat docs "tools") t)
          (with-temp-file (file-name-concat root "niwa-resource.el")
            (insert ";; Installed package.\n"))
          (dolist (name '("ptc-dialect.md" "tools/files.md"
                          "tools/applypatch.md" "tools/execution.md"))
            (let* ((file (file-name-concat docs name))
                   (address (concat "niwa://" name)))
              (copy-file (file-name-concat source "docs" name) file)
              (let* ((attempt (niwa-resource-prepare 'read address nil))
                     (niwa-resource-current-attempts
                      (list (cons address attempt)))
                     (text (plist-get
                            (niwa-test--read (list :file_path address))
                            :result)))
                (should (string-match-p "1\11# " text))
                (should-not (string-match-p (regexp-quote root) text)))
              ;; The source checkout still has the same manual. An installed
              ;; copy missing it must not silently read that other version.
              (delete-file file)
              (let* ((attempt (niwa-resource-prepare 'read address nil))
                     (niwa-resource-current-attempts
                      (list (cons address attempt))))
                (should-error (niwa-test--read (list :file_path address)))))))
      (delete-directory root t)))
  :doc "rejects binary and media reads through memory addresses"
  (let* ((workspace-root (make-temp-file "niwa-memory-media-" t))
         (memory-root (file-name-concat workspace-root "memory"))
         (workspace (niwa-workspace--create
                     :type 'test :id workspace-root :root workspace-root
                     :name "memory-media"))
         (session (niwa-session--create
                   :workspace workspace :working-directory workspace-root))
         (niwa-memory-dirs (list memory-root)))
    (unwind-protect
        (progn
          (make-directory memory-root t)
          (with-temp-file (file-name-concat memory-root "picture.png")
            (insert "not really an image"))
          (let* ((root (car (niwa-system--memory-roots workspace)))
                 (address (format "memory://%s/picture.png"
                                  (niwa-resource-memory-root-key root)))
                 (attempt (niwa-resource-prepare
                           'read address (list :session session)))
                 (niwa-resource-current-attempts
                  (list (cons address attempt)))
                 (niwa--session session)
                 (err (should-error
                       (niwa-test--read (list :file_path address)))))
            (should (string-match-p "Memory resources only support text reads"
                                    (error-message-string err)))))
      (delete-directory workspace-root t)))
  :doc "successful Read queues changed deeper workspace instructions once per conversation"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-read-instructions-" t)))
           (nested (file-name-concat root "lib"))
           (deeper (file-name-concat nested "private"))
           (sibling (file-name-concat nested "other"))
           (file (file-name-concat deeper "source.el"))
           (sibling-file (file-name-concat sibling "source2.el"))
           (workspace (niwa-workspace-get-or-create
                       'project root root "read-instructions"))
           (session (niwa-session-create "main" workspace root))
           (request (niwa-request--create))
           (draft "> quoted\nsecond line")
           (gptel--known-backends nil)
           (backend (gptel-make-openai
                     "read-instructions" :key "test" :host "example.test"
                     :models '(test)))
           (data (list :messages [(:role "user" :content "task")]))
           (fsm (gptel-make-fsm
                 :info (list :buffer data-buf :backend backend :data data
                             :position (with-current-buffer data-buf (point-marker))))))
      (unwind-protect
          (progn
            (make-directory deeper t)
            (make-directory sibling t)
            (write-region "root" nil (file-name-concat root "AGENTS.md")
                          nil 'silent)
            (write-region "shared" nil (file-name-concat nested "AGENTS.md")
                          nil 'silent)
            (write-region "local" nil
                          (file-name-concat nested "AGENTS.local.md")
                          nil 'silent)
            (write-region "code" nil file nil 'silent)
            (write-region "code" nil sibling-file nil 'silent)
            (with-current-buffer view-buf
              (niwa-view-test--insert-composer-draft draft 4))
            (with-current-buffer data-buf
              (setq-local niwa--session session)
              (setq-local niwa--workspace workspace)
              (setq-local niwa--current-request request)
              (niwa-test--read (list :file_path file))
              (let* ((bodies
                      (cl-loop for entry in (plist-get
                                             niwa-reminders--turn-events
                                             :items)
                               when (eq (car-safe (car entry))
                                        'workspace-instructions)
                               collect (plist-get (cdr entry) :body)))
                     (body (string-join bodies "\n")))
                (should (= 2 (length bodies)))
                (should-not
                 (string-search (file-name-concat root "AGENTS.md") body))
                (should (< (string-match "shared" body)
                           (string-match "local" body)))
                ;; A same-turn sibling read coalesces the shared ancestor
                ;; instructions instead of queuing them again.
                (niwa-test--read (list :file_path sibling-file))
                (should (= 2 (cl-count-if
                              (lambda (entry)
                                (eq (car-safe (car entry))
                                    'workspace-instructions))
                              (plist-get niwa-reminders--turn-events
                                         :items)))))
              (should-not
               (niwa-session-workspace-instruction-hashes session))
              ;; An abort before injection must not suppress the next delivery.
              (setq-local niwa-reminders--turn-events nil)
              (niwa-test--read (list :file_path file))
              (should niwa-reminders--turn-events)
              (niwa-reminders--handle-inject fsm)
              (should (niwa-session-workspace-instruction-hashes session))
              (niwa-test--read (list :file_path file))
              (should-not niwa-reminders--turn-events)
              (write-region "shared changed" nil
                            (file-name-concat nested "AGENTS.md") nil 'silent)
              (niwa-test--read (list :file_path file))
              (should
               (string-search
                "shared changed"
                (plist-get
                 (cdr (cl-find-if
                       (lambda (entry)
                         (eq (caar entry) 'workspace-instructions))
                       (plist-get niwa-reminders--turn-events :items)))
                 :body)))
              (setq-local niwa-reminders--turn-events nil)
              (setq-local niwa--agent-invocation
                          (niwa-agent-invocation--create
                           :path "/root/worker"))
              (niwa-test--read (list :file_path file))
              (should (cl-find-if
                       (lambda (entry)
                         (eq (caar entry) 'workspace-instructions))
                       (plist-get niwa-reminders--turn-events :items))))
            (with-current-buffer view-buf
              (should (string= draft (niwa-view-composer-draft-text)))
              (should (= 4 (niwa-view-test--composer-point-offset)))))
        (delete-directory root t)))))

(niwa-deftest niwa-tool-fs-read--virtual-text ()
  ,test
  (test)
  :doc "public file and virtual Read preserve line semantics and follow capped EOF retries"
  (test-niwa-tool-fs-read--with-file ".txt" nil
    (let* ((record (niwa-agent-record--create
                    :path "/root/reader" :role "reader" :activity 'idle
                    :settled-outcome 'completed))
           (session (niwa-session--create))
           (niwa-tool-fs-read--default-limit 2))
      (niwa-session--set-agent-registry session (list (cons "/root/reader" record)))
      (cl-labels ((read-page (args)
                   (let* ((address (plist-get args :file_path))
                          (attempt (and (string-prefix-p "agent://" address)
                                        (niwa-resource-prepare
                                         'read address (list :session session))))
                          (niwa-resource-current-attempts
                           (and attempt (list (cons address attempt)))))
                     (condition-case err
                         (niwa-test--read args)
                       (error (list :status 'error :result (error-message-string err)))))))
        (dolist (entry '(("a\nb\nc\n" 3) ("a\nb\nc" 3)
                         ("\n\n" 2) ("a\n\nc" 3) ("a" 1)))
          (with-temp-file tmp (insert (car entry)))
          (setf (niwa-agent-record-settled-result record) (car entry))
          (dolist (address (list tmp "agent://root/reader"))
            (dolist (offset (list (1+ (cadr entry)) 99))
              (dolist (limit '(nil 1 20))
                (let* ((result (read-page (list :file_path address :offset offset :limit limit)))
                       (text (plist-get result :result))
                       (retry (test-niwa-tool-fs-read--eof-retry-args text))
                       (last (cadr entry))
                       (start (max 1 (1+ (- last (min (or limit 2) 2))))))
                  (should (eq 'error (plist-get result :status)))
                  (should (string-search (format "Available lines in this read: 1–%d." last) text))
                  (should (equal retry (list :file_path address :offset start
                                            :limit (1+ (- last start)))))
                  (let ((page (read-page retry)))
                    (should-not (eq 'error (plist-get page :status)))
                    (should (string-match-p (format "%d\t" last) (plist-get page :result)))))))))
        ;; A valid last-page retry still observes the normal character cap.
        (let ((niwa-tool-fs-read--default-limit 4)
              (niwa-tool-fs-read--max-output-chars 12)
              (text "aaa\nbbb\nccc\nddd\neee\nfff\n"))
          (with-temp-file tmp (insert text))
          (setf (niwa-agent-record-settled-result record) text)
          (dolist (address (list tmp "agent://root/reader"))
            (let* ((failure (read-page (list :file_path address :offset 99 :limit 20)))
                   (retry (test-niwa-tool-fs-read--eof-retry-args
                           (plist-get failure :result)))
                   (page (read-page retry))
                   (next (test-niwa-tool-fs-read--eof-retry-args
                          (plist-get page :result))))
              (should (equal retry (list :file_path address :offset 3 :limit 4)))
              (should (string-search "Read output truncated" (plist-get page :result)))
              (should (equal next (list :file_path address :offset 5 :limit 4)))
              (should (string-search "6\tfff" (plist-get (read-page next) :result))))))
        ;; Empty files and virtual payloads remain successful at any offset.
        (with-temp-file tmp)
        (setf (niwa-agent-record-settled-result record) "")
        (dolist (address (list tmp "agent://root/reader"))
          (let ((result (read-page (list :file_path address :offset 99 :limit 1))))
            (should-not (eq 'error (plist-get result :status)))
            (if (equal address tmp)
                (should (string-search "empty (0 bytes)" (plist-get result :result)))
              (should (equal "" (plist-get result :result))))))))))

(niwa-deftest niwa-tool-fs-read--file (:quiet t)
  ,test
  (test)
  :doc "reads full file with line numbers"
  (test-niwa-tool-fs-read--with-file nil nil
    (with-temp-file tmp (insert "line one\nline two\nline three\n"))
    (let ((result (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file (list :file_path tmp) k)))))
      (should (string-match-p "1\tline one" result))
      (should (string-match-p "2\tline two" result))
      (should (string-match-p "3\tline three" result))))
  :doc "reads with offset"
  (test-niwa-tool-fs-read--with-file nil nil
    (with-temp-file tmp (insert "a\nb\nc\nd\ne\n"))
    (let ((result (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file
                      (list :file_path tmp :offset 3 :limit 2) k)))))
      (should (string-match-p "3\tc" result))
      (should (string-match-p "4\td" result))
      (should-not (string-match-p "\\b1\ta" result))
      (should-not (string-match-p "5\te" result))))
  :doc "reads with limit only"
  (test-niwa-tool-fs-read--with-file nil nil
    (with-temp-file tmp (insert "a\nb\nc\nd\ne\n"))
    (let ((result (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file
                      (list :file_path tmp :offset 1 :limit 2) k)))))
      (should (string-match-p "1\ta" result))
      (should (string-match-p "2\tb" result))
      (should-not (string-match-p "3\tc" result))))
  :doc "rejects a negative text range"
  ;; A negative limit read as an empty success, and a negative offset
  ;; was clamped to line 1 -- content the label does not describe.
  (test-niwa-tool-fs-read--with-file nil nil
    (with-temp-file tmp (insert "a\nb\nc\n"))
    (should-error
     (test-niwa-tool-fs-read--settle
      (lambda (k)
        (niwa-tool-fs-read--file (list :file_path tmp :limit -5) k)))
      :type 'error)
    (should-error
     (test-niwa-tool-fs-read--settle
      (lambda (k)
        (niwa-tool-fs-read--file (list :file_path tmp :offset -3) k)))
      :type 'error)
    ;; A negative float must not slip past an integer-only check.
    (should-error
     (test-niwa-tool-fs-read--settle
      (lambda (k)
        (niwa-tool-fs-read--file (list :file_path tmp :offset -3.0) k)))
      :type 'error))
  :doc "reports a range that starts after the last line"
  ;; An empty success is indistinguishable from an empty file, and this
  ;; tool explicitly annotates the genuinely-empty case.  Line four of a
  ;; three-line file is the exact offset a stale continuation hint hands
  ;; the model.
  (test-niwa-tool-fs-read--with-file nil nil
    (with-temp-file tmp (insert "a\nb\nc\n"))
    (dolist (offset '(4 99))
      (let ((failure (should-error
                      (test-niwa-tool-fs-read--file
                       (list :file_path tmp :offset offset))
                      :type 'error)))
        (should (equal
                 (format (concat "Read offset %d starts after the last line of %s. "
                                 "Available lines in this read: 1–3. "
                                 "Read the last page with Read(file_path=%S, offset=1, limit=3).")
                         offset tmp tmp)
                 (cadr failure)))))
    (should (equal "1\ta\n2\tb\n3\tc\n"
                   (test-niwa-tool-fs-read--file
                    (list :file_path tmp :offset 1 :limit 3))))
    ;; The last real line still reads.
    (should (string-match-p
             "3\tc"
             (test-niwa-tool-fs-read--settle
              (lambda (k)
                (niwa-tool-fs-read--file
                 (list :file_path tmp :offset 3) k))))))
  :doc "errors on non-existent file"
  (should-error
   (test-niwa-tool-fs-read--settle
    (lambda (k)
      (niwa-tool-fs-read--file (list :file_path "/nonexistent/file.txt") k)))
    :type 'error)
  :doc "errors on directory"
  (should-error
   (test-niwa-tool-fs-read--settle
    (lambda (k)
      (niwa-tool-fs-read--file (list :file_path "/tmp") k)))
    :type 'error)
  :doc "errors on binary extension"
  (test-niwa-tool-fs-read--with-file ".zip" nil
    (should-error
     (test-niwa-tool-fs-read--settle
      (lambda (k)
        (niwa-tool-fs-read--file (list :file_path tmp) k)))
      :type 'error))
  :doc "reads supported image media with base64 envelope when media-capable"
  (test-niwa-tool-fs-read--with-file ".png" nil
    (test-niwa-tool-fs-read--write-bytes
     tmp test-niwa-tool-fs-read--png-bytes)
    (cl-letf (((symbol-function 'gptel--model-capable-p)
               (lambda (cap &optional _model) (eq cap 'media)))
              ((symbol-function 'gptel--model-mime-capable-p)
               (lambda (mime &optional _model)
                 (equal mime "image/png")))
              )
      (let ((result (test-niwa-tool-fs-read--settle
                     (lambda (k)
                       (niwa-tool-fs-read--file (list :file_path tmp) k)))))
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
  :doc "ignores text ranges for media reads"
  (test-niwa-tool-fs-read--with-file ".png" nil
    (test-niwa-tool-fs-read--write-bytes
     tmp test-niwa-tool-fs-read--png-bytes)
    (cl-letf (((symbol-function 'gptel--model-capable-p)
               (lambda (cap &optional _model) (eq cap 'media)))
              ((symbol-function 'gptel--model-mime-capable-p)
               (lambda (mime &optional _model)
                 (equal mime "image/png"))))
      (dolist (range '((:offset 0 :limit 2000) (:offset 1 :limit 1)
                       (:offset 50 :limit 5)))
        (let ((result (test-niwa-tool-fs-read--settle
                       (lambda (k)
                         (niwa-tool-fs-read--file
                          (append (list :file_path tmp :pages "") range) k)))))
          (should (equal (plist-get (car (plist-get result :media)) :mime)
                         "image/png"))))))
  :doc "rejects media reads when current model cannot accept media"
  (test-niwa-tool-fs-read--with-file ".jpg" "jpg-bytes"
    (cl-letf (((symbol-function 'gptel--model-capable-p)
               (lambda (&rest _) nil)))
      (let ((err (should-error
                  (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file (list :file_path tmp) k)))
                   :type 'error)))
        (should (string-match-p "does not support media"
                                (cadr err))))))
  :doc "rejects media reads when current model cannot accept MIME type"
  (test-niwa-tool-fs-read--with-file ".pdf" "%PDF-1.4\n"
    (cl-letf (((symbol-function 'gptel--model-capable-p)
               (lambda (cap &optional _model) (eq cap 'media)))
              ((symbol-function 'gptel--model-mime-capable-p)
               (lambda (_mime &optional _model) nil)))
      (let ((err (should-error
                  (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file (list :file_path tmp) k)))
                   :type 'error)))
        (should (string-match-p "does not support media type application/pdf"
                                (cadr err))))))
  :doc "reads a whole PDF as page images when the model lacks document input"
  (test-niwa-tool-fs-read--with-file ".pdf" "%PDF-1.4\n"
    (let (rendered)
      (cl-letf (((symbol-function 'gptel--model-capable-p)
                 (lambda (cap &optional _model) (eq cap 'media)))
                ((symbol-function 'gptel--model-mime-capable-p)
                 (lambda (mime &optional _model) (equal mime "image/png")))
                ((symbol-function 'executable-find)
                 (lambda (name &optional _remote)
                   (and (member name '("pdfinfo" "pdftoppm")) name)))
                ((symbol-function 'niwa-execution-start-helper)
                 (test-niwa-tool-fs-read--as-start-helper
                  (lambda (_name command &rest _)
                    (pcase (car command)
                      ("pdfinfo" '(:exit-code 0 :output "Pages: 2\n"))
                      ("pdftoppm"
                       (push (string-to-number (nth 2 command)) rendered)
                       (test-niwa-tool-fs-read--write-bytes
                        (concat (car (last command)) ".png")
                        test-niwa-tool-fs-read--png-bytes)
                       '(:exit-code 0 :output ""))
                      (_ '(:exit-code 1 :output "unexpected")))))))
        (let ((result (test-niwa-tool-fs-read--settle
                       (lambda (k)
                         (niwa-tool-fs-read--file (list :file_path tmp) k)))))
          (should (equal '(1 2) (nreverse rendered)))
          (should (equal '("image/png" "image/png")
                         (mapcar (lambda (media) (plist-get media :mime))
                                 (plist-get result :media))))
          (should-not (string-match-p "Rendered pages"
                                      (plist-get result :result)))))))
  :doc "adds bounded-page reminder when reading a large PDF without pages"
  (test-niwa-tool-fs-read--with-file ".pdf" "%PDF-1.4\n"
    (cl-letf (((symbol-function 'gptel--model-capable-p)
               (lambda (cap &optional _model) (eq cap 'media)))
              ((symbol-function 'gptel--model-mime-capable-p)
               (lambda (_mime &optional _model) t))

              ((symbol-function 'niwa-tool-fs-read-large-pdf-p)
               (lambda (_path _page-count) t))
              ((symbol-function 'niwa-tool-fs-read--pdf-page-count)
               (lambda (_path k) (funcall k 42)))
              ((symbol-function 'niwa-tool-fs-read--base64-file)
               (lambda (&rest _) "JVBERg==")))
      (let* ((result (test-niwa-tool-fs-read--settle
                      (lambda (k)
                        (niwa-tool-fs-read--file (list :file_path tmp) k))))
             (body (plist-get result :result)))
        (should (listp result))
        (should (plist-get result :media))
        (should (string-match-p "<system-reminder>" body))
        (should (string-match-p "pages=\"START-END\"" body))
        (should (string-match-p "42 pages" body)))))
  :doc "downloads a remote PDF only once across attachment and guidance"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-read-remote-pdf-" t)))
         (local (file-name-concat root "document.pdf"))
         (remote (format "/niwamock:pdf-once:%s" local))
         (copy-file-function (symbol-function 'copy-file))
         (copies 0))
    (unwind-protect
        (progn
          (write-region "%PDF-1.4\npayload" nil local nil 'silent)
          (niwa-test--with-local-shell-tramp '("pdf-once")
            (cl-letf (((symbol-function 'gptel--model-capable-p)
                       (lambda (cap &optional _model) (eq cap 'media)))
                      ((symbol-function 'gptel--model-mime-capable-p)
                       (lambda (_mime &optional _model) t))

                      ((symbol-function 'executable-find)
                       (lambda (name &optional _remote)
                         (and (equal name "pdfinfo") name)))
                      ((symbol-function 'niwa-execution-start-helper)
                       (test-niwa-tool-fs-read--as-start-helper
                        (lambda (&rest _) '(:exit-code 0 :output "Pages: 42\n"))))
                      ((symbol-function 'copy-file)
                       (lambda (file newname &optional ok-if-exists
                                     keep-time preserve-uid-gid
                                     preserve-extended-attributes)
                         (when (file-remote-p file) (cl-incf copies))
                         (funcall copy-file-function file newname ok-if-exists
                                  keep-time preserve-uid-gid
                                  preserve-extended-attributes))))
              (let ((result
                     (test-niwa-tool-fs-read--settle
                      (lambda (k)
                        (niwa-tool-fs-read--file (list :file_path remote) k)))))
                (should (string-match-p "42 pages"
                                        (plist-get result :result)))
                (should (= 1 copies))))))
      (delete-directory root t)))
  :doc "adds bounded-page reminder when direct PDF attachment is too large"
  (test-niwa-tool-fs-read--with-file ".pdf" "%PDF-1.4\n"
    (let ((niwa-tool-fs-read-media-max-bytes 1))
      (cl-letf (((symbol-function 'gptel--model-capable-p)
                 (lambda (cap &optional _model) (eq cap 'media)))
                ((symbol-function 'gptel--model-mime-capable-p)
                 (lambda (_mime &optional _model) t))
                ((symbol-function 'niwa-tool-fs-read--pdf-page-count)
                 (lambda (_path k) (funcall k 42))))
        (let ((err (should-error
                    (test-niwa-tool-fs-read--settle
                     (lambda (k)
                       (niwa-tool-fs-read--file
                        (list :file_path tmp) k)))
                     :type 'error)))
          (should (string-match-p "Media file is too large"
                                  (cadr err)))
          (should (string-match-p "<system-reminder>"
                                  (cadr err)))
          (should (string-match-p "pages=\"START-END\""
                                  (cadr err)))))))
  :doc "falls back to textual media envelope when backend cannot serialize media"
  (test-niwa-tool-fs-read--with-file ".png" nil
    (test-niwa-tool-fs-read--write-bytes
     tmp test-niwa-tool-fs-read--png-bytes)
    (cl-letf (((symbol-function 'gptel--model-capable-p)
               (lambda (cap &optional _model) (eq cap 'media)))
              ((symbol-function 'gptel--model-mime-capable-p)
               (lambda (_mime &optional _model) t))
              )
      (let ((result (test-niwa-tool-fs-read--settle
                     (lambda (k)
                       (niwa-tool-fs-read--file (list :file_path tmp) k)))))
        (should (listp result))
        (should (string-match-p "<media-file>"
                                (plist-get result :result)))
        (should (string-match-p "mime_type: image/png"
                                (plist-get result :result)))
        (should (string-match-p "encoding: base64"
                                (plist-get result :result)))
        (should (equal (plist-get (car (plist-get result :media)) :mime)
                       "image/png")))))
  :doc "checks transformed image MIME instead of source MIME"
  (let ((tmp (make-temp-file "niwa-test-" nil ".webp"))
        (prepared (make-temp-file "niwa-test-prepared-" nil ".jpg")))
    (unwind-protect
        (progn
          (test-niwa-tool-fs-read--write-bytes
           tmp (unibyte-string ?R ?I ?F ?F 0 0 0 0 ?W ?E ?B ?P))
          (test-niwa-tool-fs-read--write-bytes
           prepared (unibyte-string #xff #xd8 #xff))
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (mime &optional _model)
                       (equal mime "image/jpeg")))

                    ((symbol-function 'niwa-tool-fs-read--maybe-transform-media)
                     (lambda (_path _args k)
                       (funcall k (cons prepared "image/jpeg")))))
            (let ((result (test-niwa-tool-fs-read--settle
                           (lambda (k)
                             (niwa-tool-fs-read--file
                              (list :file_path tmp :max_tokens 512) k)))))
              (should (listp result))
              (should (string-match-p "mime_type: image/jpeg"
                                      (plist-get result :result)))
              (should (equal (plist-get (car (plist-get result :media)) :mime)
                             "image/jpeg")))))
      (delete-file tmp)
      (delete-file prepared)))
  :doc "rejects media reads when contents do not match extension"
  (test-niwa-tool-fs-read--with-file ".png" "not-a-png"
    (cl-letf (((symbol-function 'gptel--model-capable-p)
               (lambda (cap &optional _model) (eq cap 'media)))
              ((symbol-function 'gptel--model-mime-capable-p)
               (lambda (_mime &optional _model) t))
              )
      (let ((err (should-error
                  (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file (list :file_path tmp) k)))
                   :type 'error)))
        (should (string-match-p "contents do not match media type"
                                (cadr err))))))
  :doc "rejects image transform arguments on full PDF reads"
  (test-niwa-tool-fs-read--with-file ".pdf" "%PDF-1.4\n"
    (cl-letf (((symbol-function 'gptel--model-capable-p)
               (lambda (cap &optional _model) (eq cap 'media)))
              ((symbol-function 'gptel--model-mime-capable-p)
               (lambda (_mime &optional _model) t)))
      (let ((err (should-error
                  (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file
                      (list :file_path tmp :max_width 1000) k)))
                   :type 'error)))
        (should (string-match-p "PDF page images" (cadr err))))))
  :doc "errors when PDF page extraction needs missing pdftoppm"
  (test-niwa-tool-fs-read--with-file ".pdf" "%PDF-1.4\n"
    (let ((orig-executable-find (symbol-function 'executable-find))
          (niwa-sandbox--probe-cache nil))
      (cl-letf (((symbol-function 'gptel--model-capable-p)
                 (lambda (cap &optional _model) (eq cap 'media)))
                ((symbol-function 'gptel--model-mime-capable-p)
                 (lambda (_mime &optional _model) t))

                ((symbol-function 'executable-find)
                 (lambda (cmd &optional remote)
                   (and (not (equal cmd "pdftoppm"))
                        (funcall orig-executable-find cmd remote)))))
        (let ((err (should-error
                    (test-niwa-tool-fs-read--settle
                     (lambda (k)
                       (niwa-tool-fs-read--file
                        (list :file_path tmp :pages "1") k)))
                     :type 'error)))
          (should (string-match-p "poppler-utils" (cadr err)))))))
  :doc "caps aggregate base64 payload for rendered PDF pages"
  (test-niwa-tool-fs-read--with-file ".pdf" "%PDF-1.4\n"
    (let ((niwa-tool-fs-read--pdf-pages-max-base64-chars 8))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (cmd) (and (equal cmd "pdftoppm") t)))
                ((symbol-function 'niwa-execution-start-helper)
                 (test-niwa-tool-fs-read--as-start-helper
                  (lambda (_name command _read-paths _writable-roots
                                 &rest _)
                    (if (equal (car command) "pdftoppm")
                        (progn
                          (test-niwa-tool-fs-read--write-bytes
                           (concat (car (last command)) ".png")
                           test-niwa-tool-fs-read--png-bytes)
                          '(:exit-code 0 :output ""))
                      '(:exit-code 1 :output "")))))
                ((symbol-function 'gptel--model-capable-p)
                 (lambda (cap &optional _model) (eq cap 'media)))
                ((symbol-function 'gptel--model-mime-capable-p)
                 (lambda (_mime &optional _model) t))

                ((symbol-function 'niwa-tool-fs-read--base64-file)
                 (lambda (&rest _) "aaaaaaaaa")))
        (let ((err (should-error
                    (test-niwa-tool-fs-read--settle
                     (lambda (k)
                       (niwa-tool-fs-read--file
                        (list :file_path tmp :pages "1") k)))
                     :type 'error)))
          (should (string-match-p "aggregate media size limit"
                                  (cadr err)))))))
  :doc "file-not-found suggests same basename with different extension"
  (let* ((tmp-dir (make-temp-file "niwa-missing-" t))
         (default-directory (file-name-as-directory tmp-dir))
         (actual (file-name-concat tmp-dir "notes.md"))
         (missing (file-name-concat tmp-dir "notes.txt")))
    (unwind-protect
        (progn
          (with-temp-file actual (insert "notes"))
          (let ((err (should-error
                      (test-niwa-tool-fs-read--settle
                       (lambda (k)
                         (niwa-tool-fs-read--file
                          (list :file_path missing) k)))
                       :type 'error)))
            (should (string-match-p "Did you mean" (cadr err)))
            (should (string-match-p (regexp-quote actual) (cadr err)))))
      (delete-directory tmp-dir t)))
  :doc "file-not-found suggests under-cwd correction"
  (let* ((parent (make-temp-file "niwa-parent-" t))
         (repo (file-name-concat parent "repo"))
         (default-directory (file-name-as-directory repo))
         (actual (file-name-concat repo "src" "main.el"))
         (missing (file-name-concat parent "src" "main.el")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory actual) t)
          (with-temp-file actual (insert "content"))
          (let ((err (should-error
                      (test-niwa-tool-fs-read--settle
                       (lambda (k)
                         (niwa-tool-fs-read--file
                          (list :file_path missing) k)))
                       :type 'error)))
            (should (string-match-p "Did you mean" (cadr err)))
            (should (string-match-p (regexp-quote actual) (cadr err)))))
      (delete-directory parent t)))
  :doc "errors on blocked device path"
  (should-error
   (test-niwa-tool-fs-read--settle
    (lambda (k)
      (niwa-tool-fs-read--file (list :file_path "/dev/zero") k)))
    :type 'error)
  :doc "returns system-reminder for empty file"
  (test-niwa-tool-fs-read--with-file nil nil
    (let ((result (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file (list :file_path tmp) k)))))
      (should (string-match-p "system-reminder" result))
      (should (string-match-p "empty" result))))
  :doc "truncates long lines"
  (test-niwa-tool-fs-read--with-file nil nil
    (with-temp-file tmp
      (insert (make-string 3000 ?x) "\nshort\n"))
    (let ((result (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file (list :file_path tmp) k)))))
      (should (string-search "char_offset=2000" result))
      (should (string-match-p "2\tshort" result))))
  :doc "caps default text reads with continuation guidance"
  (test-niwa-tool-fs-read--with-file nil nil
    (let ((niwa-tool-fs-read--default-limit 3)
          (niwa-tool-fs-read--max-output-chars 10000))
      (with-temp-file tmp
        (dotimes (i 5)
          (insert (format "line %d\n" (1+ i)))))
      (let ((result (test-niwa-tool-fs-read--settle
                     (lambda (k)
                       (niwa-tool-fs-read--file (list :file_path tmp) k)))))
        (should (string-match-p "1\tline 1" result))
        (should (string-match-p "3\tline 3" result))
        (should-not (string-match-p "4\tline 4" result))
        (should (string-match-p "Read output truncated" result))
        (should (string-match-p "offset=4" result)))))
  :doc "remote continuation guidance uses the target-native path"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-remote-read-root-" t))))
    (unwind-protect
        (progn
          (write-region "one\ntwo\nthree\n" nil
                        (file-name-concat root "notes.txt") nil 'silent)
          (niwa-test--with-local-shell-tramp '("read")
            (let* ((remote-root (format "/niwamock:read:%s" root))
                   (remote-path (file-name-concat remote-root "notes.txt"))
                   (workspace (niwa-workspace--create
                               :type 'project :id "remote-read"
                               :root remote-root :name "remote-read"
                               :file-cache (niwa-test-file-cache-create)))
                   (niwa--session
                    (niwa-session-create "main" workspace))
                   (niwa-tool-fs-read--default-limit 2)
                   (result (test-niwa-tool-fs-read--settle
                            (lambda (k)
                              (niwa-tool-fs-read--file
                               (list :file_path remote-path) k)))))
              (should (string-match-p
                       (regexp-quote (file-name-concat root "notes.txt"))
                       result))
              (should-not (string-match-p "/niwamock:" result))
              (let* ((failure (should-error
                               (niwa-test--read
                                (list :file_path remote-path :offset 99))))
                     (text (error-message-string failure))
                     (retry (test-niwa-tool-fs-read--eof-retry-args text)))
                (should-not (string-search "/niwamock:" text))
                (should (string-search "Available lines in this read: 1–3." text))
                (should (equal retry (list :file_path (file-name-concat root "notes.txt")
                                          :offset 2 :limit 2)))
                (plist-put retry :file_path
                           (niwa-execution-target-expand-path
                            (niwa-session-execution-target niwa--session)
                            (plist-get retry :file_path)))
                (should (string-search "3\tthree"
                                       (plist-get (niwa-test--read retry) :result)))))))
      (delete-directory root t)))
  :doc "caps aggregate text read output with continuation guidance"
  (test-niwa-tool-fs-read--with-file nil nil
    (let ((niwa-tool-fs-read--default-limit 10)
          (niwa-tool-fs-read--max-output-chars 25))
      (with-temp-file tmp
        (insert "one\ntwo\nthree\nfour\nfive\n"))
      (let ((result (test-niwa-tool-fs-read--settle
                     (lambda (k)
                       (niwa-tool-fs-read--file (list :file_path tmp) k)))))
        (should (string-match-p "1\tone" result))
        (should-not (string-match-p "5\tfive" result))
        (should (string-match-p "Read output truncated" result))
        (should (string-match-p "offset=4" result)))))
  :doc "errors on oversized file without range"
  (test-niwa-tool-fs-read--with-file nil nil
    (with-temp-file tmp
      (insert (make-string (* 600 1024) ?x)))
    (should-error
     (test-niwa-tool-fs-read--settle
      (lambda (k)
        (niwa-tool-fs-read--file (list :file_path tmp) k)))
      :type 'error))
  :doc "reads oversized file with offset/limit"
  (test-niwa-tool-fs-read--with-file nil nil
    (with-temp-file tmp
      (dotimes (i 100)
        (insert (format "line %d: %s\n" (1+ i) (make-string 6000 ?x)))))
    (let ((result (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file
                      (list :file_path tmp :offset 50 :limit 5) k)))))
      (should (string-match-p "50\tline 50" result))
      (should (string-match-p "54\tline 54" result)))
    (test-niwa-tool-fs-read--assert-eof tmp 100 "line 100"))
  :doc "preserves a UTF-8 character split across input chunks"
  (test-niwa-tool-fs-read--with-file nil nil
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file tmp
        (insert (make-string (- (* 512 1024) 4) ?x)
                "\nab\u00e9\n")))
    (let ((result (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file
                      (list :file_path tmp :offset 2 :limit 1) k)))))
      (should (string-match-p
               (regexp-quote "2\tab\u00e9") result)))
    (test-niwa-tool-fs-read--assert-eof tmp 2 "ab\u00e9"))
  :doc "preserves a UTF-16 surrogate pair split across input chunks"
  (test-niwa-tool-fs-read--with-file nil nil
    (let ((coding-system-for-write 'utf-16le-with-signature))
      (write-region
       (concat (make-string (/ (- (* 512 1024) 8) 2) ?x)
               "\na\U0001f600\n")
       nil tmp nil 'silent))
    (let ((result (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file
                      (list :file_path tmp :offset 2 :limit 1) k)))))
      (should (string-match-p
               (regexp-quote "2\ta\U0001f600") result)))
    (test-niwa-tool-fs-read--assert-eof tmp 2 "a\U0001f600"))
  :doc "ignores newline bytes spanning UTF-16 code units"
  (test-niwa-tool-fs-read--with-file nil nil
    (let ((coding-system-for-write 'utf-16le-with-signature))
      (write-region "\u0a41\u0100\nsecond\n"
                    nil tmp nil 'silent))
    (let ((result (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file
                      (list :file_path tmp :offset 2 :limit 1) k)))))
      (should (string-match-p "2\tsecond" result)))
    (test-niwa-tool-fs-read--assert-eof tmp 2 "second"))
  :doc "counts a bare LF after a DOS-coded input chunk"
  (test-niwa-tool-fs-read--with-file nil nil
    (let ((coding-system-for-write 'no-conversion))
      (write-region
       (concat "one\r\n" (make-string (* 512 1024) ?x)
               "\nthree\r\n")
       nil tmp nil 'silent))
    (let ((result (test-niwa-tool-fs-read--settle
                   (lambda (k)
                     (niwa-tool-fs-read--file
                      (list :file_path tmp :offset 3 :limit 1) k)))))
      (should (string-match-p "3\tthree" result)))
    (test-niwa-tool-fs-read--assert-eof tmp 3 "three"))
  :doc "EOF counting preserves DOS, Mac, UTF-8 and signed UTF-16 line endings"
  (test-niwa-tool-fs-read--with-file ".txt" nil
    (dolist (coding '(utf-8-unix utf-8-dos utf-8-mac
                      utf-16le-with-signature utf-16be-with-signature))
      (dolist (text '("one\n\nlast\n" "one\n\nlast"))
        (let ((coding-system-for-write coding))
          (write-region text nil tmp nil 'silent))
        (test-niwa-tool-fs-read--assert-eof tmp 3 "last"))))
  :doc "EOF uses captured bytes despite append during traversal and does not rescan"
  (test-niwa-tool-fs-read--with-file ".txt" nil
    (write-region (concat (make-string (1- (* 512 1024)) ?x)
                          "\nsecond\nthird") nil tmp nil 'silent)
    (let ((size (file-attribute-size (file-attributes tmp)))
          (original-raw (symbol-function 'insert-file-contents-literally))
          (original-decoded (symbol-function 'insert-file-contents))
          (original-pinned (symbol-function 'niwa-pin-insert-file-contents))
          ranges (probes 0) appended probing)
      (let ((failure
             (cl-letf (((symbol-function 'insert-file-contents)
                        (lambda (&rest args)
                          ;; Literal reads delegate here with no conversion.
                          (unless (eq coding-system-for-read 'no-conversion)
                            (cl-incf probes))
                          (apply original-decoded args)))
                       ;; The decoded probe reads literally through the pin.
                       ((symbol-function 'niwa-pin-insert-file-contents)
                        (lambda (&rest args)
                          (cl-incf probes)
                          (setq probing t)
                          (unwind-protect (apply original-pinned args)
                            (setq probing nil))))
                       ((symbol-function 'insert-file-contents-literally)
                        (lambda (&rest args)
                          (unless probing
                            (push (list (nth 2 args) (nth 3 args)) ranges))
                          (prog1 (apply original-raw args)
                            (unless (or appended probing)
                              (setq appended t)
                              (write-region "\nfour\nfive\n" nil tmp t 'silent))))))
               (should-error
                (niwa-test--read (list :file_path tmp :offset 99 :limit 2))))))
        (should (= probes 1))
        (should (equal (nreverse ranges) (list (list 0 (* 512 1024))
                                             (list (* 512 1024) size))))
        (should (string-search "Available lines in this read: 1–3."
                               (error-message-string failure)))
        (let ((retry (test-niwa-tool-fs-read--eof-retry-args
                      (error-message-string failure))))
          (should (equal retry (list :file_path tmp :offset 2 :limit 2)))
          (should (string-search "3\tthird" (plist-get (niwa-test--read retry) :result))))
        ;; A later read observes growth; the failed read made no completion claim.
        (test-niwa-tool-fs-read--assert-eof tmp 5 "five"))))
  :doc "EOF at an exact chunk boundary still recognizes a loaded next line"
  (test-niwa-tool-fs-read--with-file ".txt" nil
    (write-region (concat (make-string (1- (* 512 1024)) ?x) "\nlast\n")
                  nil tmp nil 'silent)
    (should (string-search "2\tlast"
                           (plist-get (niwa-test--read
                                       (list :file_path tmp :offset 2 :limit 1)) :result)))
    (test-niwa-tool-fs-read--assert-eof tmp 2 "last"))
  :doc "reads the canonical target and refuses a symlink spelling"
  (let ((tmp (make-temp-file "niwa-test-"))
        (link (make-temp-file "niwa-link-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "real content\n"))
          (delete-file link)
          (make-symbolic-link tmp link)
          ;; The pipeline hands the handler the canonical target.
          (should (string-match-p
                   "real content"
                   (test-niwa-tool-fs-read--settle
                    (lambda (k)
                      (niwa-tool-fs-read--file
                       (list :file_path (file-truename link)) k)))))
          ;; A link where the authorized path names a file is a swap.
          (should-error (niwa-tool-fs-read--file (list :file_path link) #'ignore)
                        :type 'niwa-pin-refused))
      ;; Remove the link while its target exists, or it tests as missing.
      (when (file-exists-p link) (delete-file link))
      (delete-file tmp)))
  :doc "does not re-resolve a path already canonicalized by the pipeline"
  (test-niwa-tool-fs-read--with-file ".txt" "content\n"
    (cl-letf (((symbol-function 'file-truename)
               (lambda (&rest _)
                 (error "Read handler re-resolved its authorized path"))))
      (should (string-match-p
               "content"
               (test-niwa-tool-fs-read--settle
                (lambda (k)
                  (niwa-tool-fs-read--file (list :file_path tmp) k)))))))
  :doc "records session interaction and workspace cache entry"
  (let* ((tmp (make-temp-file "niwa-test-" nil ".txt" "hello world\n"))
         (ws (niwa-workspace--create
              :type 'file :id "read-integration"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (niwa-test-file-cache-create)))
         (session (niwa-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 5)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (test-niwa-tool-fs-read--settle
           (lambda (k)
             (niwa-tool-fs-read--file (list :file_path tmp) k)))
           (let ((entry (gethash (expand-file-name tmp)
                                 (niwa-session-touched-files session))))
             (should entry)
             (should (= 5 (niwa-file-interaction-read-turn entry))))
           (should (niwa-file-cache-get
                    (niwa-workspace-file-cache ws) tmp)))
      (delete-file tmp)))
  :doc "returns stub on duplicate media read when mtime unchanged"
  (let* ((tmp (make-temp-file "niwa-test-" nil ".png"))
         (ws (niwa-workspace--create
              :type 'file :id "read-media-dedup"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (niwa-test-file-cache-create)))
         (session (niwa-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (progn
          (test-niwa-tool-fs-read--write-bytes
           tmp test-niwa-tool-fs-read--png-bytes)
          (with-temp-buffer
            (setq-local niwa--session session)
            (cl-letf (((symbol-function 'gptel--model-capable-p)
                       (lambda (cap &optional _model) (eq cap 'media)))
                      ((symbol-function 'gptel--model-mime-capable-p)
                       (lambda (_mime &optional _model) t))
                      )
              (let ((first (test-niwa-tool-fs-read--settle
                            (lambda (k)
                              (niwa-tool-fs-read--file
                               (list :file_path tmp) k)))))
                (should (listp first))
                (should (plist-get first :media)))
              (let ((second (test-niwa-tool-fs-read--settle
                             (lambda (k)
                               (niwa-tool-fs-read--file
                                (list :file_path tmp) k)))))
                (should (string-match-p "unchanged since last read"
                                        second))
                (should-not (string-match-p "<media-file>" second))))))
      (delete-file tmp)))
  :doc "returns stub on duplicate read when mtime unchanged"
  (let* ((tmp (make-temp-file "niwa-test-" nil ".txt" "hello world\n"))
         (ws (niwa-workspace--create
              :type 'file :id "read-dedup"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (niwa-test-file-cache-create)))
         (session (niwa-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (let ((first (test-niwa-tool-fs-read--settle
                        (lambda (k)
                          (niwa-tool-fs-read--file (list :file_path tmp) k)))))
            (should (string-match-p "hello world" first)))
          (let ((second (test-niwa-tool-fs-read--settle
                         (lambda (k)
                           (niwa-tool-fs-read--file (list :file_path tmp) k)))))
            (should (string-match-p "unchanged since last read" second))
            (should-not (string-match-p "hello world" second))))
      (delete-file tmp)))
  :doc "sub-agent reads ignore parent-session dedup state"
  (let* ((tmp (make-temp-file "niwa-test-" nil ".txt" "agent content\n"))
         (ws (niwa-workspace--create
              :type 'file :id "read-agent-dedup"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (niwa-test-file-cache-create)))
         (session (niwa-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (let ((first (test-niwa-tool-fs-read--settle
                        (lambda (k)
                          (niwa-tool-fs-read--file (list :file_path tmp) k)))))
            (should (string-match-p "agent content" first)))
          (let ((second (test-niwa-tool-fs-read--settle
                         (lambda (k)
                           (niwa-tool-fs-read--file (list :file_path tmp) k)))))
            (should (string-match-p "unchanged since last read" second)))
          (setq-local niwa--agent-invocation t)
          (let ((agent-read (test-niwa-tool-fs-read--settle
                             (lambda (k)
                               (niwa-tool-fs-read--file
                                (list :file_path tmp) k)))))
            (should (string-match-p "agent content" agent-read))
            (should-not (string-match-p "unchanged since last read"
                                        agent-read)))
          (setq-local niwa--agent-invocation nil)
          ;; The parent session still sees its own dedup state; the
          ;; agent read did not replace or clear it.
          (let ((after-agent (test-niwa-tool-fs-read--settle
                              (lambda (k)
                                (niwa-tool-fs-read--file
                                 (list :file_path tmp) k)))))
            (should (string-match-p "unchanged since last read"
                                    after-agent))))
      (delete-file tmp)))
  :doc "ToolCall nested reads neither consult nor record dedup state"
  (let* ((tmp (make-temp-file "niwa-test-" nil ".txt" "ptc content\n"))
         (ws (niwa-workspace--create
              :type 'file :id "read-ptc-dedup"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (niwa-test-file-cache-create)))
         (session (niwa-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          ;; A script sees only its own tool results, so a reuse stub is
          ;; unusable there even when the conversation read the file first.
          (let ((first (test-niwa-tool-fs-read--settle
                        (lambda (k)
                          (niwa-tool-fs-read--file (list :file_path tmp) k)))))
            (should (string-match-p "ptc content" first)))
          (let ((niwa-pipeline--active-call-source 'ptc))
            (dotimes (_ 2)
              (let ((nested (test-niwa-tool-fs-read--settle
                             (lambda (k)
                               (niwa-tool-fs-read--file
                                (list :file_path tmp) k)))))
                (should (string-match-p "ptc content" nested))
                (should-not (string-match-p "unchanged since last read"
                                            nested)))))
          ;; The nested reads recorded nothing: the conversation's own
          ;; dedup state is exactly what its first read left behind.
          (let ((after (test-niwa-tool-fs-read--settle
                        (lambda (k)
                          (niwa-tool-fs-read--file (list :file_path tmp) k)))))
            (should (string-match-p "unchanged since last read" after))))
      (delete-file tmp)))
  :doc "does not dedupe after external modification"
  (let* ((tmp (make-temp-file "niwa-test-" nil ".txt" "hello\n"))
         (ws (niwa-workspace--create
              :type 'file :id "read-dedup-mtime"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (niwa-test-file-cache-create)))
         (session (niwa-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (test-niwa-tool-fs-read--settle
           (lambda (k)
             (niwa-tool-fs-read--file (list :file_path tmp) k)))
           (let ((future (time-add (current-time) 2)))
             (with-temp-file tmp (insert "goodbye\n"))
             (set-file-times tmp future))
           (let ((second (test-niwa-tool-fs-read--settle (lambda (k) (niwa-tool-fs-read--file (list :file_path tmp) k)))))
                 (should (string-match-p "goodbye" second))
                 (should-not (string-match-p "unchanged since last read" second))))
           (delete-file tmp)))
  :doc "does not dedupe when range differs"
  (let* ((tmp (make-temp-file "niwa-test-" nil ".txt"
                              "a\nb\nc\nd\ne\nf\n"))
         (ws (niwa-workspace--create
              :type 'file :id "read-dedup-range"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (niwa-test-file-cache-create)))
         (session (niwa-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (test-niwa-tool-fs-read--settle
           (lambda (k)
             (niwa-tool-fs-read--file
              (list :file_path tmp :offset 1 :limit 2) k)))
           (let ((second (test-niwa-tool-fs-read--file
                          (list :file_path tmp :offset 4 :limit 2))))
             (should (string-match-p "4\td" second))
             (should-not (string-match-p "unchanged since last read" second))))
      (delete-file tmp)))
  :doc "remote session artifacts ignore poisoned fixed caches and file-state dedup"
  (let* ((host "read-artifact")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-read-artifact-" t)))
         (remote-root (format "/niwamock:%s:%s/" host local-root))
         (workspace (niwa-workspace--create
                     :type 'project :id "read-artifact"
                     :root remote-root :name "read-artifact"))
         (session (niwa-session-create "main" workspace))
         (save-path (file-name-concat remote-root ".niwa" "sessions"
                                     "session"))
         (logical "plans/current.md")
         (fixed (file-name-concat save-path logical))
         (published (file-name-concat
                     save-path ".publications" "generation" "000001.data"))
         (committed "committed plan\nsecond line\n")
         dedup-called)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (make-directory (file-name-directory fixed) t)
          (write-region "poisoned fixed cache\n" nil fixed nil 'silent)
          (make-directory (file-name-directory published) t)
          (write-region committed nil published nil 'silent)
          (setf (niwa-session-save-path session)
                (file-name-as-directory save-path)
                (niwa-session-publication session)
                (list :head ".publications/generation/manifest.el"
                      :sidecar published
                      :artifacts
                      (list (list logical
                                  :published published
                                  :sha256 (secure-hash 'sha256 committed)))))
          (with-temp-buffer
            (setq-local niwa--session session)
            (cl-letf (((symbol-function 'niwa-session-read-is-duplicate-p)
                       (lambda (&rest _)
                         (setq dedup-called t)))
                      ((symbol-function 'niwa-session-record-file-access)
                       (lambda (&rest _)
                         (setq dedup-called t))))
              (let ((result
                     (test-niwa-tool-fs-read--settle
                      (lambda (k)
                        (niwa-tool-fs-read--file
                         (list :file_path fixed :offset 2 :limit 1) k)))))
                (should (string-match-p "2\tsecond line" result))
                (should-not (string-match-p "poisoned" result))
                (should-not dedup-called))
              (let* ((failure (should-error
                               (niwa-test--read
                                (list :file_path fixed :offset 99 :limit 1))))
                     (text (error-message-string failure))
                     (retry (test-niwa-tool-fs-read--eof-retry-args text)))
                (should (string-search "Available lines in this read: 1–2." text))
                (dolist (private '("niwa-session-artifact-" "/niwamock:" ".publications"))
                  (should-not (string-search private text)))
                (should (equal (plist-get retry :file_path)
                               (file-name-concat local-root ".niwa" "sessions"
                                                 "session" logical)))
                (plist-put retry :file_path
                           (niwa-execution-target-expand-path
                            (niwa-session-execution-target session)
                            (plist-get retry :file_path)))
                (should (string-search "2\tsecond line"
                                       (plist-get (niwa-test--read retry) :result)))
                (should-not dedup-called)))
            (delete-file fixed)
            (should (string-match-p
                     "1\tcommitted plan"
                     (test-niwa-tool-fs-read--settle
                      (lambda (k)
                        (niwa-tool-fs-read--file
                         (list :file_path fixed :offset 1 :limit 1) k)))))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))
  :doc "remote session artifacts never fall back when the manifest omits them"
  (let* ((host "read-unpublished-artifact")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-read-unpublished-" t)))
         (remote-root (format "/niwamock:%s:%s/" host local-root))
         (workspace (niwa-workspace--create
                     :type 'project :id "read-unpublished-artifact"
                     :root remote-root :name "read-unpublished-artifact"))
         (session (niwa-session-create "main" workspace))
         (save-path (file-name-concat remote-root ".niwa" "sessions"
                                     "session"))
         (fixed (file-name-concat save-path "tool-results" "result.txt")))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (make-directory (file-name-directory fixed) t)
          (write-region "fixed cache only\n" nil fixed nil 'silent)
          (setf (niwa-session-save-path session)
                (file-name-as-directory save-path)
                (niwa-session-publication session)
                (list :head ".publications/generation/manifest.el"
                      :sidecar nil :artifacts nil))
          (with-temp-buffer
            (setq-local niwa--session session)
            (should-error
             (test-niwa-tool-fs-read--settle
              (lambda (k)
                (niwa-tool-fs-read--file (list :file_path fixed) k)))
              :type 'error)))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

;;
;;; Line number formatting

(niwa-deftest niwa-tool-fs-read--add-line-numbers ()
  ,test
  (test)
  :doc "adds correct line numbers starting at 1"
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (niwa-tool-fs-read--add-line-numbers 1)
    (should (string-match-p "1\talpha" (buffer-string)))
    (should (string-match-p "2\tbeta" (buffer-string)))
    (should (string-match-p "3\tgamma" (buffer-string))))
  :doc "adds correct line numbers with offset"
  (with-temp-buffer
    (insert "alpha\nbeta\n")
    (niwa-tool-fs-read--add-line-numbers 42)
    (should (string-match-p "42\talpha" (buffer-string)))
    (should (string-match-p "43\tbeta" (buffer-string))))
  :doc "truncates lines over 2000 characters"
  (with-temp-buffer
    (insert (make-string 3000 ?a) "\n")
    (niwa-tool-fs-read--add-line-numbers 1 "long.txt")
    (should (string-prefix-p (concat "1\t" (make-string 2000 ?a))
                            (buffer-string)))
    (should (string-search
             "Read(file_path=\"long.txt\", offset=1, limit=1, char_offset=2000)"
             (buffer-string)))
    (should (<= (length (buffer-string)) 2200))))

;;
;;; Directory listing helper

(niwa-deftest niwa-tool-fs-read-list-directory ()
  ,test
  (test)
  :doc "lists files in a directory, sorted by path"
  (let ((tmp-dir (make-temp-file "niwa-list-" t)))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "alpha.txt") (insert "a"))
          (with-temp-file (file-name-concat tmp-dir "beta.txt") (insert "b"))
          (let* ((result (test-niwa-tool-fs-read--list tmp-dir))
                 (entries (car result)))
            (should (= 2 (length entries)))
            (should (member "alpha.txt" entries))
            (should (member "beta.txt" entries))
            (should (null (cdr result)))))
      (delete-directory tmp-dir t)))

  :doc "caps listing at max-entries and reports truncation"
  (let ((tmp-dir (make-temp-file "niwa-list-" t)))
    (unwind-protect
        (progn
          (dotimes (i 5)
            (with-temp-file (file-name-concat tmp-dir (format "f%d.txt" i))
              (insert "x")))
          (let* ((result (test-niwa-tool-fs-read--list tmp-dir 3))
                 (entries (car result))
                 (truncated (cdr result)))
            (should (= 3 (length entries)))
            (should truncated)))
      (delete-directory tmp-dir t)))

  :doc "does not traverse descendant symlinks outside the selected root"
  (let* ((base (file-name-as-directory
                (make-temp-file "niwa-list-symlink-" t)))
         (root (file-name-as-directory (file-name-concat base "root")))
         (outside (file-name-as-directory
                   (file-name-concat base "outside"))))
    (unwind-protect
        (progn
          (make-directory root t)
          (make-directory outside t)
          (with-temp-file (file-name-concat root "inside.txt")
            (insert "inside"))
          (with-temp-file (file-name-concat outside "secret.txt")
            (insert "secret"))
          (make-symbolic-link outside (file-name-concat root "linked"))
          (let ((entries (car (test-niwa-tool-fs-read--list root))))
            (should (member "inside.txt" entries))
            (should-not (member "linked/secret.txt" entries))))
      (delete-directory base t)))

  :doc "returns empty list for empty directory"
  (let ((tmp-dir (make-temp-file "niwa-list-" t)))
    (unwind-protect
        (let ((result (test-niwa-tool-fs-read--list tmp-dir)))
          (should (null (car result)))
          (should (null (cdr result))))
      (delete-directory tmp-dir t)))

  :doc "errors on non-directory path"
  (let ((tmp (make-temp-file "niwa-list-" nil ".txt" "x")))
    (unwind-protect
        (should-error (test-niwa-tool-fs-read--list tmp) :type 'error)
      (delete-file tmp))))

(niwa-deftest niwa-tool-fs-read-render ()
  ,test
  (test)
  :doc "returns nil for non-string result"
  (should (null (niwa-tool-fs-read-render "Read" '(:file_path "/x.el") nil nil)))

  :doc "returns nil for error results"
  (should (null
           (niwa-tool-fs-read-render
            "Read" '(:file_path "/missing.el")
            "Error: File /missing.el does not exist" nil)))

  :doc "header shows basename and line count; body-mode from extension"
  (let* ((body "1->foo\n2->bar\n3->baz\n")
         (plist (niwa-tool-fs-read-render
                 "Read" '(:file_path "/home/roland/proj/hello.el") body nil)))
    (should (string-match-p "\\`Read: hello\\.el " (plist-get plist :header)))
    (should (equal body (plist-get plist :body)))
    (should (eq 'emacs-lisp-mode (plist-get plist :body-mode))))

  :doc "header shows workspace-relative root file"
  (let* ((root (make-temp-file "niwa-read-root-" t))
         (file (file-name-concat root "niwa-skills.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "read-root"
                     :root root :name "read-root"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (let ((plist (niwa-tool-fs-read-render
                          "Read" `(:file_path ,file) "1->root\n" nil)))
              (should (string-match-p "\\`Read: niwa-skills\\.el "
                                      (plist-get plist :header))))))
      (delete-directory root t)))

  :doc "header shows workspace-relative subdirectory file"
  (let* ((root (make-temp-file "niwa-read-subdir-" t))
         (file (file-name-concat root "test/test-niwa-skills.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "read-subdir"
                     :root root :name "read-subdir"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert "subdir\n"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (let ((plist (niwa-tool-fs-read-render
                          "Read" `(:file_path ,file) "1->subdir\n" nil)))
              (should (string-match-p
                       "\\`Read: test/test-niwa-skills\\.el "
                       (plist-get plist :header))))))
      (delete-directory root t)))

  :doc "body-mode is nil when file has no recognized extension"
  (let* ((plist (niwa-tool-fs-read-render
                 "Read" '(:file_path "/tmp/no-extension-here") "x\n" nil)))
    (should (null (plist-get plist :body-mode))))

  :doc "body-mode is nil for media files"
  (let* ((plist (niwa-tool-fs-read-render
                 "Read" '(:file_path "/tmp/screenshot.png")
                 "<media-file>\n...\n</media-file>" nil)))
    (should (null (plist-get plist :body-mode))))

  :doc "header line count ignores appended system reminders"
  (let* ((body "1->foo\n2->bar\n\n<system-reminder>\nuse Imenu\n</system-reminder>")
         (plist (niwa-tool-fs-read-render
                 "Read" '(:file_path "/tmp/a.el") body nil)))
    (should (string-match-p "2 lines" (plist-get plist :header))))

  :doc "header line count preserves literal system reminder content"
  (let* ((body "1->before\n2-><system-reminder>\n3->sample\n4-></system-reminder>\n5->after")
         (plist (niwa-tool-fs-read-render
                 "Read" '(:file_path "/tmp/a.el") body nil)))
    (should (string-match-p "5 lines" (plist-get plist :header)))))

(niwa-deftest niwa-tool-fs-read--tool-results-dir ()
  ,test
  (test)
  :doc "media read outside a session goes to a private directory deleted at exit"
  (let ((niwa-tool-fs-read--sessionless-results nil)
        (kill-emacs-hook nil)
        (niwa--session nil))
    (with-temp-buffer
      (let ((dir (niwa-tool-fs-read--tool-results-dir)))
        (unwind-protect
            (progn
              (should (file-directory-p dir))
              (should-not (equal dir (file-name-concat temporary-file-directory
                                                       "niwa-tool-results")))
              (should (equal dir (niwa-tool-fs-read--tool-results-dir)))
              (should (memq #'niwa-tool-fs-read--delete-sessionless-results kill-emacs-hook))
              (run-hooks 'kill-emacs-hook)
              (should-not (file-exists-p dir)))
          (ignore-errors (delete-directory dir t)))))))

(provide 'test-niwa-tool-fs-read)
;;; test-niwa-tool-fs-read.el ends here
